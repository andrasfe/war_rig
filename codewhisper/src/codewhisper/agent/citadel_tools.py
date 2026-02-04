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

Example:
    from codewhisper.agent.citadel_tools import (
        configure_citadel_tools,
        get_citadel_tools,
    )

    # Configure with agent config
    configure_citadel_tools(config)

    # Get all tools for the agent
    tools = get_citadel_tools()
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
    """Configure citadel tools with runtime dependencies.

    This function must be called before citadel tools can be used.

    Args:
        config: Agent configuration containing code_dir.
    """
    global _citadel, _config
    _config = config
    _citadel = None  # Reset to force lazy re-init


def _get_citadel() -> Any:
    """Get or create the Citadel SDK instance.

    Returns:
        Citadel SDK instance.

    Raises:
        RuntimeError: If citadel tools are not configured.
    """
    global _citadel
    if _citadel is None:
        from citadel import Citadel

        _citadel = Citadel()
    return _citadel


def _resolve_path(file_path: str) -> Path:
    """Resolve file path relative to code directory.

    Args:
        file_path: Path to file (relative to code_dir or absolute).

    Returns:
        Resolved absolute path.

    Raises:
        RuntimeError: If citadel tools are not configured.
    """
    if _config is None:
        raise RuntimeError(
            "Citadel tools not configured. Call configure_citadel_tools first."
        )

    path = Path(file_path)
    if not path.is_absolute():
        path = _config.code_dir / path
    return path.resolve()


# -----------------------------------------------------------------------------
# Input Schemas
# -----------------------------------------------------------------------------


class AnalyzeFileInput(BaseModel):
    """Input for analyze_file tool."""

    file_path: str = Field(
        ...,
        description="Path to source file (relative to code directory or absolute)",
    )


class GetFunctionsInput(BaseModel):
    """Input for get_functions tool."""

    file_path: str = Field(
        ...,
        description="Path to source file (relative to code directory or absolute)",
    )


class GetCalloutsInput(BaseModel):
    """Input for get_callouts tool."""

    path: str = Field(
        ...,
        description="Path to source file or directory (relative to code directory or absolute)",
    )


class GetIncludesInput(BaseModel):
    """Input for get_includes tool."""

    file_path: str = Field(
        ...,
        description="Path to source file (relative to code directory or absolute)",
    )


class GetFunctionBodyInput(BaseModel):
    """Input for get_function_body tool."""

    file_path: str = Field(
        ...,
        description="Path to source file (relative to code directory or absolute)",
    )
    function_name: str = Field(
        ...,
        description="Name of the function/paragraph to extract (case-insensitive)",
    )


class GetFunctionBodiesInput(BaseModel):
    """Input for get_function_bodies tool."""

    file_path: str = Field(
        ...,
        description="Path to source file (relative to code directory or absolute)",
    )
    function_names: list[str] = Field(
        ...,
        description="List of function/paragraph names to extract",
    )


class GetFileStatsInput(BaseModel):
    """Input for get_file_stats tool."""

    file_path: str = Field(
        ...,
        description="Path to source file (relative to code directory or absolute)",
    )


class GetCallersInput(BaseModel):
    """Input for get_callers tool."""

    file_path: str = Field(
        ...,
        description="Path to file containing the target function",
    )
    function_name: str = Field(
        ...,
        description="Name of the function to find callers for",
    )


class GetSequenceDiagramsInput(BaseModel):
    """Input for get_sequence_diagrams tool."""

    path: str = Field(
        ...,
        description="Source directory or graph JSON file path",
    )
    max_diagrams: int = Field(
        default=3,
        ge=1,
        le=10,
        description="Maximum number of diagrams to generate",
    )


class GetDeadCodeInput(BaseModel):
    """Input for get_dead_code tool."""

    path: str = Field(
        ...,
        description="Source directory or graph JSON file path",
    )


class GetFlowDiagramInput(BaseModel):
    """Input for get_flow_diagram tool."""

    file_path: str = Field(
        ...,
        description="Path to source file (typically COBOL)",
    )
    paragraph: str | None = Field(
        default=None,
        description="Optional starting paragraph (shows entire file if not specified)",
    )


class GetFileSummaryInput(BaseModel):
    """Input for get_file_summary tool."""

    file_path: str = Field(
        ...,
        description="Path to source file (relative to code directory or absolute)",
    )


class GetAnalysisPatternsInput(BaseModel):
    """Input for get_analysis_patterns tool."""

    file_path: str = Field(
        ...,
        description="Path to source file (relative to code directory or absolute)",
    )


# -----------------------------------------------------------------------------
# Tool Definitions
# -----------------------------------------------------------------------------


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
    try:
        citadel = _get_citadel()
        full_path = _resolve_path(file_path)

        result = citadel.analyze_file(full_path)

        if result.error:
            return f"Error analyzing {file_path}: {result.error}"

        lines = [
            f"# Analysis of {Path(result.file_path).name}",
            f"**Language:** {result.language}",
            "",
            "## Artifacts",
        ]

        for artifact in result.artifacts:
            lines.append(f"### {artifact.name} ({artifact.type})")
            if artifact.line_start:
                line_range = f"Lines {artifact.line_start}"
                if artifact.line_end:
                    line_range += f"-{artifact.line_end}"
                lines.append(line_range)
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

        if result.preprocessor_includes:
            lines.append("")
            lines.append("## Preprocessor Includes")
            for inc in result.preprocessor_includes:
                lines.append(f"- {inc}")

        return "\n".join(lines)

    except Exception as e:
        logger.exception(f"Error in citadel_analyze_file: {e}")
        return f"Error analyzing file: {e}"


@tool(args_schema=GetFunctionsInput)
def citadel_get_functions(file_path: str) -> str:
    """Get all functions/paragraphs in a file with their callouts.

    Use this tool for a quick overview of what functions exist in
    a file and what each function calls. More compact than full
    analyze_file when you only need function-level info.

    Returns:
        List of functions with names, types, line numbers, and calls.

    Example use cases:
        - "What paragraphs are in this COBOL program?"
        - "List the functions in UTILS.cbl"
        - "What does each paragraph call?"
    """
    try:
        citadel = _get_citadel()
        full_path = _resolve_path(file_path)

        functions = citadel.get_functions(full_path)

        if not functions:
            return f"No functions found in {file_path}"

        if functions and "error" in functions[0]:
            return f"Error: {functions[0]['error']}"

        lines = [f"# Functions in {Path(file_path).name}", ""]
        for func in functions:
            lines.append(f"## {func['name']} ({func['type']})")
            line_info = f"Lines: {func.get('line', '?')}"
            if func.get("line_end"):
                line_info += f"-{func['line_end']}"
            lines.append(line_info)
            if func.get("calls"):
                lines.append("Calls:")
                for call in func["calls"][:5]:
                    lines.append(f"  - {call['target']} ({call['type']})")
                if len(func["calls"]) > 5:
                    lines.append(f"  ... and {len(func['calls']) - 5} more")
            lines.append("")

        return "\n".join(lines)

    except Exception as e:
        logger.exception(f"Error in citadel_get_functions: {e}")
        return f"Error getting functions: {e}"


@tool(args_schema=GetCalloutsInput)
def citadel_get_callouts(path: str) -> str:
    """Get all callouts/references from a file or directory.

    Use this tool to find all external references (calls, includes,
    reads, writes) from a file or across a directory. Useful for
    understanding dependencies and data flow.

    When given a directory, shows which references are resolved
    (target exists in codebase) vs unresolved.

    Returns:
        List of callouts with source, target, and type.

    Example use cases:
        - "What does PROGRAM.cbl call?"
        - "Find all external references in the src directory"
        - "What copybooks are used across the codebase?"
    """
    try:
        citadel = _get_citadel()
        full_path = _resolve_path(path)

        callouts = citadel.get_callouts(full_path)

        if not callouts:
            return f"No callouts found in {path}"

        if callouts and "error" in callouts[0]:
            return f"Error: {callouts[0]['error']}"

        lines = [f"# Callouts from {Path(path).name}", ""]

        # Group by type for readability
        by_type: dict[str, list[dict[str, Any]]] = {}
        for c in callouts:
            call_type = c.get("type", "unknown")
            if call_type not in by_type:
                by_type[call_type] = []
            by_type[call_type].append(c)

        for call_type, type_callouts in sorted(by_type.items()):
            lines.append(f"## {call_type.title()} ({len(type_callouts)})")
            for c in type_callouts[:15]:
                resolved = ""
                if "resolved" in c:
                    resolved = " [resolved]" if c["resolved"] else " [unresolved]"
                lines.append(f"- {c['from']} -> {c['to']}{resolved}")
            if len(type_callouts) > 15:
                lines.append(f"... and {len(type_callouts) - 15} more")
            lines.append("")

        return "\n".join(lines)

    except Exception as e:
        logger.exception(f"Error in citadel_get_callouts: {e}")
        return f"Error getting callouts: {e}"


@tool(args_schema=GetIncludesInput)
def citadel_get_includes(file_path: str) -> str:
    """Get all preprocessor includes (COPY statements, etc.).

    Use this tool to find all files included via preprocessor
    directives like COBOL COPY statements, C #include, etc.

    Returns:
        List of included file names.

    Example use cases:
        - "What copybooks does PROGRAM.cbl use?"
        - "List all COPY statements in this file"
    """
    try:
        citadel = _get_citadel()
        full_path = _resolve_path(file_path)

        includes = citadel.get_includes(full_path)

        if not includes:
            return f"No preprocessor includes found in {file_path}"

        lines = [f"# Preprocessor Includes in {Path(file_path).name}", ""]
        for inc in includes:
            lines.append(f"- {inc}")

        return "\n".join(lines)

    except Exception as e:
        logger.exception(f"Error in citadel_get_includes: {e}")
        return f"Error getting includes: {e}"


@tool(args_schema=GetFunctionBodyInput)
def citadel_get_function_body(file_path: str, function_name: str) -> str:
    """Extract the source code body of a specific function.

    Use this tool when you need to see the actual implementation
    of a specific paragraph or function. Essential for understanding
    the business logic within a function.

    Returns:
        The complete source code of the function body.

    Example use cases:
        - "Show me the code for CALCULATE-TAX paragraph"
        - "What does the MAIN-PROCESS function do?"
        - "Let me see the implementation of VALIDATE-INPUT"
    """
    try:
        citadel = _get_citadel()
        full_path = _resolve_path(file_path)

        body = citadel.get_function_body(full_path, function_name)

        if body is None:
            return f"Function '{function_name}' not found in {file_path}"

        return f"# {function_name} in {Path(file_path).name}\n\n```\n{body}\n```"

    except Exception as e:
        logger.exception(f"Error in citadel_get_function_body: {e}")
        return f"Error extracting function body: {e}"


@tool(args_schema=GetFunctionBodiesInput)
def citadel_get_function_bodies(file_path: str, function_names: list[str]) -> str:
    """Batch extract multiple function bodies efficiently.

    Use this tool when you need to examine several functions
    from the same file. More efficient than calling
    get_function_body multiple times because the file is
    parsed only once.

    Returns:
        Source code for each requested function.

    Example use cases:
        - "Show me PARA-1, PARA-2, and PARA-3 from PROGRAM.cbl"
        - "Extract all the validation paragraphs"
    """
    try:
        citadel = _get_citadel()
        full_path = _resolve_path(file_path)

        bodies = citadel.get_function_bodies(full_path, function_names)

        lines = [f"# Function Bodies from {Path(file_path).name}", ""]

        found_count = 0
        for name, body in bodies.items():
            if body is not None:
                found_count += 1
                lines.append(f"## {name}")
                lines.append(f"```\n{body}\n```")
                lines.append("")
            else:
                lines.append(f"## {name}")
                lines.append("*Not found*")
                lines.append("")

        lines.insert(1, f"Found {found_count} of {len(function_names)} functions")

        return "\n".join(lines)

    except Exception as e:
        logger.exception(f"Error in citadel_get_function_bodies: {e}")
        return f"Error extracting function bodies: {e}"


@tool(args_schema=GetFileStatsInput)
def citadel_get_file_stats(file_path: str) -> str:
    """Get structural statistics for planning documentation strategy.

    Use this tool to understand the size and structure of a file
    before deciding how to document it. Returns line counts,
    paragraph counts, and paragraph details.

    Returns:
        - Total lines
        - Paragraph count
        - Language
        - List of paragraphs with line ranges

    Example use cases:
        - "How big is PROGRAM.cbl?"
        - "How many paragraphs are in this file?"
        - "What's the structure of this program?"
    """
    try:
        citadel = _get_citadel()
        full_path = _resolve_path(file_path)

        stats = citadel.get_file_stats(full_path)

        if stats.get("error"):
            return f"Error: {stats['error']}"

        lines = [
            f"# File Statistics: {Path(file_path).name}",
            "",
            f"**Language:** {stats['language']}",
            f"**Total Lines:** {stats['total_lines']}",
            f"**Paragraph Count:** {stats['paragraph_count']}",
            "",
        ]

        if stats.get("paragraphs"):
            lines.append("## Paragraphs")
            for para in stats["paragraphs"]:
                line_range = (
                    f"{para.get('line_start', '?')}-{para.get('line_end', '?')}"
                )
                line_count = para.get("line_count", 0)
                lines.append(
                    f"- **{para['name']}**: lines {line_range} ({line_count} lines)"
                )

        return "\n".join(lines)

    except Exception as e:
        logger.exception(f"Error in citadel_get_file_stats: {e}")
        return f"Error getting file stats: {e}"


@tool(args_schema=GetCallersInput)
def citadel_get_callers(file_path: str, function_name: str) -> str:
    """Find all callers of a specific function across the codebase.

    Use this tool for reverse dependency analysis - finding all
    places that call or reference a particular function. Essential
    for impact analysis and understanding code flow.

    Searches the directory containing the file and its subdirectories.

    Returns:
        List of callers with file, function, line, and call type.

    Example use cases:
        - "Who calls CALCULATE-TAX?"
        - "What programs use the VALIDATE-INPUT paragraph?"
        - "Find all references to PROCESS-RECORD"
    """
    try:
        citadel = _get_citadel()
        full_path = _resolve_path(file_path)

        callers = citadel.get_callers(full_path, function_name)

        if not callers:
            return f"No callers found for '{function_name}'"

        lines = [f"# Callers of {function_name}", ""]
        for caller in callers:
            func = caller.get("function") or "(file level)"
            caller_file = Path(caller["file"]).name
            line_num = caller.get("line", "?")
            call_type = caller.get("type", "unknown")
            lines.append(f"- {caller_file}:{line_num} - {func} ({call_type})")

        lines.append("")
        lines.append(f"Total: {len(callers)} callers")

        return "\n".join(lines)

    except Exception as e:
        logger.exception(f"Error in citadel_get_callers: {e}")
        return f"Error finding callers: {e}"


@tool(args_schema=GetSequenceDiagramsInput)
def citadel_get_sequence_diagrams(path: str, max_diagrams: int = 3) -> str:
    """Generate Mermaid sequence diagrams showing call chains.

    Use this tool to visualize the longest call chains across
    multiple files. Shows how programs call each other in sequence.

    Note: This analyzes the entire directory which may take time.

    Returns:
        List of Mermaid sequence diagrams.

    Example use cases:
        - "Show me the call flow in the src directory"
        - "Generate sequence diagrams for the authorization system"
        - "Visualize the longest call chains"
    """
    try:
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

    except Exception as e:
        logger.exception(f"Error in citadel_get_sequence_diagrams: {e}")
        return f"Error generating sequence diagrams: {e}"


@tool(args_schema=GetDeadCodeInput)
def citadel_get_dead_code(path: str) -> str:
    """Find dead code (unreferenced artifacts) in the codebase.

    Use this tool to identify paragraphs, copybooks, or programs
    that are never called or referenced. Useful for cleanup and
    understanding which code is actually used.

    Note: Entry points are excluded (they're expected to have no callers).

    Returns:
        List of unreferenced artifacts with reasons.

    Example use cases:
        - "Find unused paragraphs in the codebase"
        - "What copybooks are never referenced?"
        - "Identify dead code for cleanup"
    """
    try:
        citadel = _get_citadel()
        full_path = _resolve_path(path)

        dead = citadel.get_dead_code(full_path)

        if not dead:
            return "No dead code detected"

        lines = [f"# Dead Code Analysis ({len(dead)} items)", ""]
        for item in dead[:20]:
            item_file = Path(item.get("file", "?")).name if item.get("file") else "?"
            lines.append(f"- **{item['name']}** ({item['type']})")
            lines.append(f"  File: {item_file}, Line: {item.get('line', '?')}")
            lines.append(f"  Reason: {item.get('reason', 'unreferenced')}")
            lines.append("")

        if len(dead) > 20:
            lines.append(f"... and {len(dead) - 20} more unreferenced items")

        return "\n".join(lines)

    except Exception as e:
        logger.exception(f"Error in citadel_get_dead_code: {e}")
        return f"Error finding dead code: {e}"


@tool(args_schema=GetFlowDiagramInput)
def citadel_get_flow_diagram(file_path: str, paragraph: str | None = None) -> str:
    """Generate a Mermaid flow diagram showing internal control flow.

    Use this tool to visualize how paragraphs call each other within
    a file. Produces a Mermaid flowchart that shows PERFORM relationships
    and external calls as leaf nodes.

    Returns:
        Mermaid flowchart diagram string.

    Example use cases:
        - "Show the control flow of PROGRAM.cbl"
        - "Generate a flow diagram starting from MAIN-PROCESS"
        - "Visualize the paragraph relationships"
    """
    try:
        citadel = _get_citadel()
        full_path = _resolve_path(file_path)

        diagram = citadel.get_flow_diagram(full_path, paragraph=paragraph)

        return (
            f"# Flow Diagram for {Path(file_path).name}\n\n```mermaid\n{diagram}\n```"
        )

    except Exception as e:
        logger.exception(f"Error in citadel_get_flow_diagram: {e}")
        return f"Error generating flow diagram: {e}"


@tool(args_schema=GetFileSummaryInput)
def citadel_get_file_summary(file_path: str) -> str:
    """Get a compact summary of a source file.

    Use this tool for a quick overview of a file without detailed
    analysis. Returns essential metadata, paragraph count, entry
    points, and main calls.

    Returns:
        Compact summary with file name, language, lines, paragraphs,
        entry points, and main calls.

    Example use cases:
        - "Give me a quick overview of PROGRAM.cbl"
        - "Summarize this file briefly"
        - "What are the entry points and main calls?"
    """
    try:
        citadel = _get_citadel()
        full_path = _resolve_path(file_path)

        summary = citadel.get_file_summary(full_path)

        if summary.get("error"):
            return f"Error: {summary['error']}"

        lines = [
            f"# Summary: {summary['file_name']}",
            "",
            f"**Language:** {summary['language']}",
            f"**Total Lines:** {summary['total_lines']}",
            f"**Paragraph Count:** {summary['paragraph_count']}",
        ]

        if summary.get("entry_points"):
            lines.append(f"**Entry Points:** {', '.join(summary['entry_points'])}")

        if summary.get("main_calls"):
            lines.append(f"**Main Calls:** {', '.join(summary['main_calls'])}")

        return "\n".join(lines)

    except Exception as e:
        logger.exception(f"Error in citadel_get_file_summary: {e}")
        return f"Error getting file summary: {e}"


@tool(args_schema=GetAnalysisPatternsInput)
def citadel_get_analysis_patterns(file_path: str) -> str:
    """Extract code patterns (data flow, control flow, error handling).

    Use this tool to understand how data moves through the code,
    control flow structures used, and error handling approaches.

    Categories:
    - data_flow: MOVE, COMPUTE, SET, etc.
    - control_flow: IF, EVALUATE, PERFORM loops, GO TO, etc.
    - error_handling: ON EXCEPTION, file status checks, etc.

    Returns:
        Pattern matches organized by category with counts.

    Example use cases:
        - "What data flow patterns are in this program?"
        - "How is error handling implemented?"
        - "Show me the control flow patterns used"
    """
    try:
        citadel = _get_citadel()
        full_path = _resolve_path(file_path)

        result = citadel.get_analysis_patterns(full_path)

        if result.error:
            return f"Error: {result.error}"

        lines = [
            f"# Analysis Patterns: {Path(file_path).name}",
            "",
            f"**Language:** {result.language}",
            f"**Total Matches:** {result.total_matches}",
            f"**Pattern Coverage:** {result.coverage_pct:.1f}%",
            "",
        ]

        for category, cat_result in result.categories.items():
            lines.append(
                f"## {category.replace('_', ' ').title()} ({cat_result.match_count} matches)"
            )
            if cat_result.patterns_matched:
                for pattern_name, count in sorted(
                    cat_result.patterns_matched.items(), key=lambda x: -x[1]
                ):
                    lines.append(f"  - {pattern_name}: {count}")
            lines.append("")

        if result.required_missing:
            lines.append("## Missing Required Patterns")
            for pattern in result.required_missing:
                lines.append(f"  - {pattern}")

        return "\n".join(lines)

    except Exception as e:
        logger.exception(f"Error in citadel_get_analysis_patterns: {e}")
        return f"Error extracting analysis patterns: {e}"


# -----------------------------------------------------------------------------
# Tool Registration
# -----------------------------------------------------------------------------


def get_citadel_tools() -> list[Any]:
    """Get all Citadel tools for the agent.

    Returns:
        List of tool functions.
    """
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
