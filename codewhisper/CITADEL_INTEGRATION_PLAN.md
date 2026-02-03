# Citadel Integration Plan for CodeWhisper

This document outlines the architecture for integrating Citadel code analysis tools into the CodeWhisper agent, enabling multi-step planning and multi-turn responses with memory.

## Executive Summary

The integration adds 13 new tools from the Citadel SDK to CodeWhisper, transforming it from a simple skill-lookup chatbot into a powerful code analysis agent capable of:

- Analyzing code structure and extracting artifacts
- Finding dependencies and call chains across files
- Generating visual diagrams (flow, sequence)
- Detecting dead code
- Extracting specific function bodies for detailed analysis

## Current Architecture

```
CodeWhisper Agent (LangGraph)
    |
    +-- Tools Module (tools.py)
    |       - search_skills
    |       - load_skill
    |       - search_code
    |       - read_file
    |
    +-- State (state.py)
    |       - messages (conversation history)
    |       - loaded_skills
    |       - search_results
    |
    +-- Graph (graph.py)
            - START -> agent -> tools -> agent -> END
            - Simple ReAct pattern
```

## Target Architecture

```
CodeWhisper Agent (LangGraph)
    |
    +-- Tools Module (tools.py) - Existing skills/code tools
    |
    +-- Citadel Tools Module (citadel_tools.py) - NEW
    |       - analyze_file
    |       - get_functions
    |       - get_callouts
    |       - get_includes
    |       - get_function_body
    |       - get_function_bodies
    |       - get_file_stats
    |       - get_callers
    |       - get_sequence_diagrams
    |       - get_dead_code
    |       - get_flow_diagram
    |       - get_file_summary
    |       - get_analysis_patterns
    |
    +-- State (state.py) - Enhanced
    |       - messages
    |       - loaded_skills
    |       - analysis_cache (NEW)
    |       - current_plan (NEW)
    |       - plan_step (NEW)
    |
    +-- Graph (graph.py) - Enhanced with Planning
            - START -> planner -> agent -> tools -> agent -> END
            - Multi-step planning support
            - Plan execution tracking
```

---

## Phase 1: Citadel Tools Module

### File: `src/codewhisper/agent/citadel_tools.py`

Create a new module that wraps Citadel SDK functions as LangChain tools.

```python
"""Citadel code analysis tools for the CodeWhisper agent.

This module wraps Citadel SDK functions as LangChain tools, enabling
the agent to perform deep code analysis on mainframe codebases.

Tools:
    - citadel_analyze_file: Full structural analysis of a source file
    - citadel_get_functions: Extract all functions with their callouts
    - citadel_get_callouts: Get all references from a file/directory
    - citadel_get_includes: Get preprocessor includes (COPY, etc.)
    - citadel_get_function_body: Extract body of a specific function
    - citadel_get_function_bodies: Batch extract multiple function bodies
    - citadel_get_file_stats: Get structural statistics
    - citadel_get_callers: Find all callers of a function
    - citadel_get_sequence_diagrams: Generate call chain diagrams
    - citadel_get_dead_code: Find unreferenced artifacts
    - citadel_get_flow_diagram: Generate control flow diagram
    - citadel_get_file_summary: Compact file summary
    - citadel_get_analysis_patterns: Extract code patterns
"""

from __future__ import annotations

import logging
from pathlib import Path
from typing import TYPE_CHECKING, Any

from langchain_core.tools import tool
from pydantic import BaseModel, Field

if TYPE_CHECKING:
    from codewhisper.config import AgentConfig

logger = logging.getLogger(__name__)

# Module-level Citadel instance (lazy initialized)
_citadel: Any = None
_config: AgentConfig | None = None


def configure_citadel_tools(config: AgentConfig) -> None:
    """Configure citadel tools with runtime dependencies."""
    global _citadel, _config
    _config = config
    _citadel = None  # Lazy init on first use


def _get_citadel() -> Any:
    """Get or create the Citadel SDK instance."""
    global _citadel
    if _citadel is None:
        from citadel.sdk import Citadel
        _citadel = Citadel()
    return _citadel


def _resolve_path(file_path: str) -> Path:
    """Resolve file path relative to code directory."""
    if _config is None:
        raise RuntimeError("Citadel tools not configured")

    path = Path(file_path)
    if not path.is_absolute():
        path = _config.code_dir / path
    return path.resolve()
```

### Tool Definitions

Each tool should have:
1. Clear docstring explaining when to use it
2. Pydantic input schema for validation
3. Formatted output suitable for LLM consumption

#### Tool 1: `citadel_analyze_file`

```python
class AnalyzeFileInput(BaseModel):
    """Input for analyze_file tool."""
    file_path: str = Field(
        ...,
        description="Path to source file (relative to code directory or absolute)"
    )


@tool(args_schema=AnalyzeFileInput)
def citadel_analyze_file(file_path: str) -> str:
    """Perform full structural analysis of a source file.

    Use this tool when you need to understand the complete structure
    of a source file, including all artifacts (programs, paragraphs,
    functions) and their relationships.

    Returns:
        - Language detected
        - All artifacts with line numbers
        - Callouts (calls, includes, reads, writes)
        - Preprocessor includes

    Example use cases:
        - "What's the structure of PROGRAM.cbl?"
        - "Show me all the paragraphs in this file"
        - "What does this file call?"
    """
    citadel = _get_citadel()
    full_path = _resolve_path(file_path)

    result = citadel.analyze_file(full_path)

    if result.error:
        return f"Error analyzing {file_path}: {result.error}"

    lines = [
        f"# Analysis of {result.file_path}",
        f"**Language:** {result.language}",
        "",
        "## Artifacts",
    ]

    for artifact in result.artifacts:
        lines.append(f"### {artifact.name} ({artifact.type})")
        if artifact.line_start:
            lines.append(f"Lines {artifact.line_start}-{artifact.line_end or '?'}")
        if artifact.callouts:
            lines.append("**Calls:**")
            for c in artifact.callouts[:10]:  # Limit to avoid overwhelming
                lines.append(f"  - {c.target} ({c.relationship})")
            if len(artifact.callouts) > 10:
                lines.append(f"  ... and {len(artifact.callouts) - 10} more")
        lines.append("")

    if result.file_level_callouts:
        lines.append("## File-Level References")
        for c in result.file_level_callouts:
            lines.append(f"- {c.target} ({c.relationship})")

    return "\n".join(lines)
```

#### Tool 2: `citadel_get_functions`

```python
class GetFunctionsInput(BaseModel):
    """Input for get_functions tool."""
    file_path: str = Field(
        ...,
        description="Path to source file"
    )


@tool(args_schema=GetFunctionsInput)
def citadel_get_functions(file_path: str) -> str:
    """Get all functions/paragraphs in a file with their callouts.

    Use this tool for a quick overview of what functions exist in
    a file and what each function calls. More compact than full
    analyze_file when you only need function-level info.

    Returns:
        List of functions with names, types, line numbers, and calls.
    """
    citadel = _get_citadel()
    full_path = _resolve_path(file_path)

    functions = citadel.get_functions(full_path)

    if not functions:
        return f"No functions found in {file_path}"

    if "error" in functions[0]:
        return f"Error: {functions[0]['error']}"

    lines = [f"# Functions in {file_path}", ""]
    for func in functions:
        lines.append(f"## {func['name']} ({func['type']})")
        lines.append(f"Lines: {func.get('line', '?')}-{func.get('line_end', '?')}")
        if func.get('calls'):
            lines.append("Calls:")
            for call in func['calls'][:5]:
                lines.append(f"  - {call['target']} ({call['type']})")
            if len(func['calls']) > 5:
                lines.append(f"  ... and {len(func['calls']) - 5} more")
        lines.append("")

    return "\n".join(lines)
```

#### Tool 3: `citadel_get_function_body`

```python
class GetFunctionBodyInput(BaseModel):
    """Input for get_function_body tool."""
    file_path: str = Field(..., description="Path to source file")
    function_name: str = Field(..., description="Name of the function/paragraph")


@tool(args_schema=GetFunctionBodyInput)
def citadel_get_function_body(file_path: str, function_name: str) -> str:
    """Extract the source code body of a specific function.

    Use this tool when you need to see the actual implementation
    of a specific paragraph or function. Essential for understanding
    the business logic within a function.

    Returns:
        The complete source code of the function body.
    """
    citadel = _get_citadel()
    full_path = _resolve_path(file_path)

    body = citadel.get_function_body(full_path, function_name)

    if body is None:
        return f"Function '{function_name}' not found in {file_path}"

    return f"# {function_name} in {file_path}\n\n```\n{body}\n```"
```

#### Tool 4: `citadel_get_callers`

```python
class GetCallersInput(BaseModel):
    """Input for get_callers tool."""
    file_path: str = Field(..., description="Path to file containing the target function")
    function_name: str = Field(..., description="Name of the function to find callers for")


@tool(args_schema=GetCallersInput)
def citadel_get_callers(file_path: str, function_name: str) -> str:
    """Find all callers of a specific function across the codebase.

    Use this tool for reverse dependency analysis - finding all
    places that call or reference a particular function. Essential
    for impact analysis and understanding code flow.

    Returns:
        List of callers with file, function, line, and call type.
    """
    citadel = _get_citadel()
    full_path = _resolve_path(file_path)

    callers = citadel.get_callers(full_path, function_name)

    if not callers:
        return f"No callers found for '{function_name}'"

    lines = [f"# Callers of {function_name}", ""]
    for caller in callers:
        func = caller.get('function') or '(file level)'
        lines.append(f"- {Path(caller['file']).name}:{caller.get('line', '?')} - {func} ({caller['type']})")

    return "\n".join(lines)
```

#### Tool 5: `citadel_get_flow_diagram`

```python
class GetFlowDiagramInput(BaseModel):
    """Input for get_flow_diagram tool."""
    file_path: str = Field(..., description="Path to COBOL source file")
    paragraph: str | None = Field(
        default=None,
        description="Optional starting paragraph (shows entire file if not specified)"
    )


@tool(args_schema=GetFlowDiagramInput)
def citadel_get_flow_diagram(file_path: str, paragraph: str | None = None) -> str:
    """Generate a Mermaid flow diagram showing internal control flow.

    Use this tool to visualize how paragraphs call each other within
    a file. Produces a Mermaid flowchart that shows PERFORM relationships
    and external calls as leaf nodes.

    Returns:
        Mermaid flowchart diagram string.
    """
    citadel = _get_citadel()
    full_path = _resolve_path(file_path)

    diagram = citadel.get_flow_diagram(full_path, paragraph=paragraph)

    return f"# Flow Diagram for {file_path}\n\n```mermaid\n{diagram}\n```"
```

#### Tool 6: `citadel_get_sequence_diagrams`

```python
class GetSequenceDiagramsInput(BaseModel):
    """Input for get_sequence_diagrams tool."""
    path: str = Field(..., description="Source directory or graph JSON file")
    max_diagrams: int = Field(default=3, description="Maximum diagrams to generate")


@tool(args_schema=GetSequenceDiagramsInput)
def citadel_get_sequence_diagrams(path: str, max_diagrams: int = 3) -> str:
    """Generate Mermaid sequence diagrams showing call chains.

    Use this tool to visualize the longest call chains across
    multiple files. Shows how programs call each other in sequence.

    Note: This analyzes the entire directory which may take time.

    Returns:
        List of Mermaid sequence diagrams.
    """
    citadel = _get_citadel()
    full_path = _resolve_path(path)

    diagrams = citadel.get_sequence_diagrams(full_path, max_diagrams=max_diagrams)

    if not diagrams:
        return "No significant call chains found"

    lines = [f"# Sequence Diagrams ({len(diagrams)} call chains)", ""]
    for i, diagram in enumerate(diagrams, 1):
        lines.append(f"## Call Chain {i}")
        lines.append(f"```mermaid\n{diagram}\n```")
        lines.append("")

    return "\n".join(lines)
```

#### Tool 7: `citadel_get_dead_code`

```python
class GetDeadCodeInput(BaseModel):
    """Input for get_dead_code tool."""
    path: str = Field(..., description="Source directory or graph JSON file")


@tool(args_schema=GetDeadCodeInput)
def citadel_get_dead_code(path: str) -> str:
    """Find dead code (unreferenced artifacts) in the codebase.

    Use this tool to identify paragraphs, copybooks, or programs
    that are never called or referenced. Useful for cleanup and
    understanding which code is actually used.

    Note: Entry points are excluded (they're expected to have no callers).

    Returns:
        List of unreferenced artifacts with reasons.
    """
    citadel = _get_citadel()
    full_path = _resolve_path(path)

    dead = citadel.get_dead_code(full_path)

    if not dead:
        return "No dead code detected"

    lines = [f"# Dead Code Analysis ({len(dead)} items)", ""]
    for item in dead[:20]:
        lines.append(f"- **{item['name']}** ({item['type']})")
        lines.append(f"  File: {Path(item.get('file', '?')).name}, Line: {item.get('line', '?')}")
        lines.append(f"  Reason: {item.get('reason', 'unreferenced')}")
        lines.append("")

    if len(dead) > 20:
        lines.append(f"... and {len(dead) - 20} more unreferenced items")

    return "\n".join(lines)
```

### Additional Tools (Simplified)

```python
@tool
def citadel_get_callouts(path: str) -> str:
    """Get all callouts/references from a file or directory."""
    # Implementation similar to above patterns
    pass


@tool
def citadel_get_includes(file_path: str) -> str:
    """Get all preprocessor includes (COPY statements, etc.)."""
    pass


@tool
def citadel_get_function_bodies(file_path: str, function_names: list[str]) -> str:
    """Batch extract multiple function bodies efficiently."""
    pass


@tool
def citadel_get_file_stats(file_path: str) -> str:
    """Get structural statistics for planning documentation strategy."""
    pass


@tool
def citadel_get_file_summary(file_path: str) -> str:
    """Get compact file summary for quick overview."""
    pass


@tool
def citadel_get_analysis_patterns(file_path: str) -> str:
    """Extract code patterns (data flow, control flow, error handling)."""
    pass
```

### Tool Registration

```python
def get_citadel_tools() -> list:
    """Get all Citadel tools for the agent."""
    return [
        citadel_analyze_file,
        citadel_get_functions,
        citadel_get_callouts,
        citadel_get_includes,
        citadel_get_function_body,
        citadel_get_function_bodies,
        citadel_get_file_stats,
        citadel_get_callers,
        citadel_get_sequence_diagrams,
        citadel_get_dead_code,
        citadel_get_flow_diagram,
        citadel_get_file_summary,
        citadel_get_analysis_patterns,
    ]
```

---

## Phase 2: Enhanced Agent State

### File: `src/codewhisper/agent/state.py`

Add planning and caching support to the state.

```python
@dataclass
class PlanStep:
    """A single step in a multi-step plan."""
    step_number: int
    description: str
    tool_hint: str | None = None  # Suggested tool to use
    completed: bool = False
    result_summary: str | None = None


@dataclass
class AgentPlan:
    """Multi-step plan for complex queries."""
    goal: str
    steps: list[PlanStep]
    current_step: int = 0

    def get_current_step(self) -> PlanStep | None:
        if self.current_step < len(self.steps):
            return self.steps[self.current_step]
        return None

    def advance(self, result_summary: str) -> None:
        if self.current_step < len(self.steps):
            self.steps[self.current_step].completed = True
            self.steps[self.current_step].result_summary = result_summary
            self.current_step += 1

    def is_complete(self) -> bool:
        return self.current_step >= len(self.steps)

    def format_status(self) -> str:
        lines = [f"**Goal:** {self.goal}", "", "**Progress:**"]
        for step in self.steps:
            status = "[x]" if step.completed else "[ ]"
            marker = " <-- current" if step.step_number == self.current_step + 1 and not step.completed else ""
            lines.append(f"{status} Step {step.step_number}: {step.description}{marker}")
        return "\n".join(lines)


@dataclass
class AgentState:
    """Enhanced state for the CodeWhisper agent."""

    # Existing fields
    messages: Annotated[Sequence[Any], add_messages] = field(default_factory=list)
    loaded_skills: list[str] = field(default_factory=list)
    skill_contexts: list[SkillContext] = field(default_factory=list)
    search_results: list[CodeSearchResult] = field(default_factory=list)
    file_contents: list[FileContent] = field(default_factory=list)
    current_query: str = ""
    needs_more_info: bool = False

    # NEW: Planning support
    current_plan: AgentPlan | None = None

    # NEW: Analysis cache (avoid re-analyzing same files)
    analysis_cache: dict[str, Any] = field(default_factory=dict)

    # NEW: Thinking/reasoning output for transparency
    last_reasoning: str = ""
```

---

## Phase 3: Enhanced Agent Graph with Planning

### File: `src/codewhisper/agent/graph.py`

Transform the simple ReAct loop into a planning-capable agent.

### New System Prompt

```python
SYSTEM_PROMPT = """You are CodeWhisper, an expert assistant for exploring and understanding mainframe codebases. You have access to two categories of tools:

## Knowledge Tools (Skills)
- search_skills: Find documentation by keyword
- load_skill: Load specific skill content
- search_code: Search source code with regex
- read_file: Read raw source files

## Analysis Tools (Citadel)
- citadel_analyze_file: Full structural analysis of a file
- citadel_get_functions: List all functions/paragraphs with calls
- citadel_get_callouts: Get all references from file or directory
- citadel_get_includes: Get preprocessor includes (COPY statements)
- citadel_get_function_body: Extract specific function's source code
- citadel_get_function_bodies: Batch extract multiple functions
- citadel_get_file_stats: Get structural statistics
- citadel_get_callers: Find all callers of a function
- citadel_get_sequence_diagrams: Generate call chain diagrams
- citadel_get_dead_code: Find unreferenced artifacts
- citadel_get_flow_diagram: Generate control flow diagram
- citadel_get_file_summary: Compact file overview
- citadel_get_analysis_patterns: Extract code patterns

## Approach Strategy

For simple questions:
- Directly use the appropriate tool and answer

For complex questions (multiple files, dependencies, impact analysis):
1. PLAN: Break down into steps
2. EXECUTE: Work through each step using tools
3. SYNTHESIZE: Combine findings into a coherent answer

## Planning Guidelines

When planning, consider:
- What specific files/functions are involved?
- Do I need structure (analyze_file) or content (get_function_body)?
- Should I trace dependencies (get_callers, get_callouts)?
- Would a diagram help (get_flow_diagram, get_sequence_diagrams)?

## Response Guidelines

- Cite your sources (files, line numbers, skills consulted)
- Use diagrams when they add clarity
- Be explicit about limitations or uncertainties
- For multi-step work, show your reasoning process

The codebase contains mainframe programs (COBOL, JCL, PL/I, Assembler, REXX) for a financial authorization system.
"""
```

### Graph Architecture

```python
def _build_graph(self) -> CompiledStateGraph:
    """Build the enhanced StateGraph with planning support."""

    graph_builder = StateGraph(AgentState)

    # Get all tools (existing + citadel)
    from codewhisper.agent.citadel_tools import get_citadel_tools
    all_tools = get_all_tools() + get_citadel_tools()
    llm_with_tools = self.llm.bind_tools(all_tools)

    # Planner node: decides if planning is needed
    async def planner_node(state: AgentState) -> dict[str, Any]:
        """Analyze query complexity and create plan if needed."""
        # Simple heuristic: queries with certain keywords need planning
        complexity_indicators = [
            "all", "every", "across", "dependencies",
            "impact", "trace", "flow", "chain", "diagram",
            "dead code", "unused", "compare"
        ]

        query = state.current_query.lower()
        needs_planning = any(ind in query for ind in complexity_indicators)

        if needs_planning and state.current_plan is None:
            # Ask LLM to create a plan
            plan_prompt = f"""Create a step-by-step plan for: {state.current_query}

Return a JSON object with:
- goal: The objective in one sentence
- steps: Array of step objects with step_number, description, tool_hint

Example:
{{
  "goal": "Find all callers of CALCULATE-TAX",
  "steps": [
    {{"step_number": 1, "description": "Find the file containing CALCULATE-TAX", "tool_hint": "search_code"}},
    {{"step_number": 2, "description": "Get callers of the function", "tool_hint": "citadel_get_callers"}}
  ]
}}"""

            # Get plan from LLM
            plan_response = await self.llm.ainvoke([
                SystemMessage(content="You are a planning assistant. Output only valid JSON."),
                HumanMessage(content=plan_prompt)
            ])

            # Parse and create plan
            try:
                import json
                plan_data = json.loads(plan_response.content)
                plan = AgentPlan(
                    goal=plan_data["goal"],
                    steps=[PlanStep(**s) for s in plan_data["steps"]]
                )
                return {"current_plan": plan}
            except:
                pass  # Fall through to direct execution

        return {}  # No planning needed

    # Agent node: execute with plan awareness
    async def agent_node(state: AgentState) -> dict[str, Any]:
        """Call LLM with plan context."""
        messages = [SystemMessage(content=SYSTEM_PROMPT)]

        # Add plan context if exists
        if state.current_plan and not state.current_plan.is_complete():
            plan_status = state.current_plan.format_status()
            current_step = state.current_plan.get_current_step()
            messages.append(SystemMessage(content=f"""
CURRENT PLAN:
{plan_status}

CURRENT STEP: {current_step.description}
SUGGESTED TOOL: {current_step.tool_hint or 'your choice'}

Execute this step, then I'll update the plan.
"""))

        # Add conversation history
        for msg in state.messages:
            messages.append(self._convert_message(msg))

        response = await llm_with_tools.ainvoke(messages)

        return {"messages": [response]}

    # Plan updater: advance plan after tool execution
    async def plan_updater_node(state: AgentState) -> dict[str, Any]:
        """Update plan after tool execution."""
        if state.current_plan and not state.current_plan.is_complete():
            # Extract result summary from last tool message
            last_msg = state.messages[-1] if state.messages else None
            summary = str(last_msg.content)[:200] if last_msg else ""

            state.current_plan.advance(summary)

            if state.current_plan.is_complete():
                return {"current_plan": None}  # Clear completed plan

        return {}

    # Add nodes
    graph_builder.add_node("planner", planner_node)
    graph_builder.add_node("agent", agent_node)
    graph_builder.add_node("tools", ToolNode(all_tools))
    graph_builder.add_node("plan_updater", plan_updater_node)

    # Add edges
    graph_builder.add_edge(START, "planner")
    graph_builder.add_edge("planner", "agent")
    graph_builder.add_conditional_edges(
        "agent",
        self._should_continue,
        {"tools": "tools", "end": END}
    )
    graph_builder.add_edge("tools", "plan_updater")
    graph_builder.add_edge("plan_updater", "agent")

    return graph_builder.compile()
```

---

## Phase 4: CLI Enhancements

### File: `src/codewhisper/cli.py`

Add better interactivity and streaming support.

```python
# New commands
COMMANDS = {
    "/help": "Show help",
    "/clear": "Clear screen",
    "/quit": "Exit",
    "/reset": "Reset conversation",
    "/skills": "List skills",
    "/load <skill>": "Load a skill",
    "/search <query>": "Search skills",
    # NEW commands
    "/plan": "Show current plan (if any)",
    "/analyze <file>": "Quick analyze a file",
    "/functions <file>": "List functions in file",
    "/callers <file> <function>": "Find callers",
    "/flow <file>": "Generate flow diagram",
    "/cache": "Show analysis cache stats",
    "/cache clear": "Clear analysis cache",
}


async def run_interactive(config: AgentConfig) -> None:
    """Enhanced interactive REPL with streaming."""

    # ... existing setup ...

    while True:
        try:
            user_input = Prompt.ask("\n[bold green]You[/bold green]")
            cmd = user_input.strip().lower()

            # NEW: Plan command
            if cmd == "/plan":
                if agent._conversation_state and agent._conversation_state.current_plan:
                    console.print(Panel(
                        agent._conversation_state.current_plan.format_status(),
                        title="Current Plan"
                    ))
                else:
                    console.print("[dim]No active plan[/dim]")
                continue

            # NEW: Quick analyze
            if cmd.startswith("/analyze "):
                file_path = user_input.strip()[9:].strip()
                with console.status("[bold]Analyzing...[/bold]"):
                    from citadel.sdk import Citadel
                    result = Citadel().analyze_file(config.code_dir / file_path)
                _display_analysis_result(result)
                continue

            # NEW: Quick functions
            if cmd.startswith("/functions "):
                file_path = user_input.strip()[11:].strip()
                # ... similar pattern
                continue

            # NEW: Streaming response display
            console.print("\n[bold blue]CodeWhisper[/bold blue]")

            with Live(Markdown(""), refresh_per_second=4, console=console) as live:
                full_response = ""
                async for chunk in agent.chat_stream(user_input):
                    full_response += chunk
                    live.update(Markdown(full_response))

        except KeyboardInterrupt:
            console.print("\n[dim]Use /quit to exit[/dim]")
```

### Streaming Support

Add to `CodeWhisperAgent`:

```python
async def chat_stream(self, user_message: str):
    """Stream a response for better interactivity."""
    # For models that support streaming
    async for event in self.graph.astream_events(
        initial_state,
        version="v1"
    ):
        if event["event"] == "on_chat_model_stream":
            chunk = event["data"]["chunk"]
            if hasattr(chunk, "content") and chunk.content:
                yield chunk.content
```

---

## Phase 5: Configuration Updates

### File: `src/codewhisper/config.py`

Add Citadel-specific configuration.

```python
class AgentConfig(BaseSettings):
    # ... existing fields ...

    # NEW: Citadel integration
    enable_citadel: bool = Field(
        default=True,
        description="Enable Citadel code analysis tools"
    )

    citadel_cache_size: int = Field(
        default=50,
        description="Maximum files to cache in analysis cache"
    )

    enable_planning: bool = Field(
        default=True,
        description="Enable multi-step planning for complex queries"
    )

    stream_responses: bool = Field(
        default=True,
        description="Stream responses for better interactivity"
    )
```

---

## Implementation Order

### Week 1: Foundation
1. Create `citadel_tools.py` with basic tools:
   - `citadel_analyze_file`
   - `citadel_get_functions`
   - `citadel_get_function_body`
   - `citadel_get_callouts`

2. Update `graph.py` to include Citadel tools
3. Update `tools.py` configure function
4. Basic testing

### Week 2: Advanced Tools
1. Add remaining tools:
   - `citadel_get_callers`
   - `citadel_get_flow_diagram`
   - `citadel_get_sequence_diagrams`
   - `citadel_get_dead_code`
   - `citadel_get_file_stats`
   - `citadel_get_analysis_patterns`

2. Add configuration options
3. Tool-specific testing

### Week 3: Planning & State
1. Enhance `state.py` with planning support
2. Implement planner node in graph
3. Add plan-aware agent behavior
4. Integration testing

### Week 4: CLI & Polish
1. Add CLI commands
2. Implement streaming
3. Add caching
4. Documentation
5. End-to-end testing

---

## Testing Strategy

### Unit Tests

```python
# tests/agent/test_citadel_tools.py

def test_citadel_analyze_file(tmp_path):
    """Test analyze_file tool with sample COBOL."""
    # Create sample file
    cobol_file = tmp_path / "TEST.cbl"
    cobol_file.write_text(SAMPLE_COBOL)

    # Configure tools
    config = AgentConfig(code_dir=tmp_path)
    configure_citadel_tools(config)

    # Run tool
    result = citadel_analyze_file(str(cobol_file))

    assert "MAIN-PARAGRAPH" in result
    assert "COBOL" in result.upper()


def test_citadel_get_function_body(tmp_path):
    """Test function body extraction."""
    # ... similar pattern
```

### Integration Tests

```python
# tests/agent/test_graph_with_citadel.py

async def test_agent_uses_citadel_for_analysis():
    """Test that agent uses Citadel tools for code analysis queries."""
    agent = create_agent(config)

    response = await agent.chat("Analyze the structure of PROGRAM.cbl")

    # Should have used citadel_analyze_file
    assert "artifacts" in response.lower() or "paragraph" in response.lower()


async def test_agent_planning():
    """Test multi-step planning for complex queries."""
    agent = create_agent(config)

    response = await agent.chat("Find all callers of CALCULATE-TAX across the codebase")

    # Should have created and executed a plan
    assert agent._conversation_state.current_plan is None  # Plan completed
```

---

## Dependencies

Add to `pyproject.toml`:

```toml
[project]
dependencies = [
    # ... existing ...
]

[project.optional-dependencies]
citadel = [
    "citadel @ file:../citadel",  # Local path for development
]
```

---

## Future Enhancements

1. **Checkpointing**: Save/restore conversation state with plans
2. **Tool usage analytics**: Track which tools are most useful
3. **Adaptive planning**: Learn from successful plans
4. **Parallel tool execution**: Run independent tools concurrently
5. **Context window management**: Smarter truncation of analysis results
6. **Diagram rendering**: Inline Mermaid rendering in terminal
