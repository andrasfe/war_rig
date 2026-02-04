"""ReAct agent loop for CodeWhisper.

This module implements a simple ReAct (Reason + Act) pattern agent loop
that uses llm_providers directly instead of LangChain/LangGraph.

The agent:
1. Receives a user message
2. Calls the LLM with available tools
3. If the LLM requests tool calls, executes them and loops back
4. If the LLM provides a final response, returns it

Example:
    from codewhisper.agent.react_loop import ReActAgent
    from codewhisper.agent.tools import create_tool_registry

    registry = create_tool_registry(skills_index, config)
    agent = ReActAgent(config, registry)

    response = await agent.chat("What does CBPAUP0C do?")
"""

from __future__ import annotations

import json
import logging
import uuid
from typing import TYPE_CHECKING, Any

from llm_providers import Message, get_provider_from_env

from codewhisper.agent.message import Conversation, ConversationMessage
from codewhisper.agent.protocol import ToolCall, ToolResult

if TYPE_CHECKING:
    from llm_providers import CompletionResponse, LLMProvider

    from codewhisper.agent.minion import MinionProcessor
    from codewhisper.agent.tools.registry import ToolRegistry
    from codewhisper.config import AgentConfig
    from codewhisper.skills.index import SkillsIndex

logger = logging.getLogger(__name__)

# System prompt for the agent
SYSTEM_PROMPT = """You are CodeWhisper, an expert assistant for exploring and understanding mainframe codebases. You have access to two categories of tools:

## Knowledge Tools (Skills)
- **search_skills**: Find documentation by keyword (e.g., "authorization", "MQ")
- **load_skill**: Load specific skill content for detailed program documentation
- **search_code**: Search source code with regex patterns
- **read_file**: Read raw source files to examine implementation

## Analysis Tools (Citadel)
- **citadel_analyze_file**: Full structural analysis of a file - artifacts, callouts, includes
- **citadel_get_functions**: List all functions/paragraphs with their calls
- **citadel_get_callouts**: Get all references from file or directory (calls, includes, reads)
- **citadel_get_includes**: Get preprocessor includes (COPY statements, etc.)
- **citadel_get_function_body**: Extract specific function's source code
- **citadel_get_function_bodies**: Batch extract multiple functions efficiently
- **citadel_get_file_stats**: Get structural statistics (lines, paragraph count, ranges)
- **citadel_get_callers**: Find all callers of a function across the codebase
- **citadel_get_sequence_diagrams**: Generate Mermaid call chain diagrams
- **citadel_get_dead_code**: Find unreferenced artifacts (paragraphs, copybooks)
- **citadel_get_flow_diagram**: Generate Mermaid control flow diagram
- **citadel_get_file_summary**: Compact file overview (entry points, main calls)
- **citadel_get_analysis_patterns**: Extract code patterns (data flow, control flow, error handling)

## Approach Strategy

For **simple questions** (what is X, quick lookup):
- Search skills or use appropriate analysis tool and answer directly

For **complex questions** (dependencies, impact analysis, multi-file exploration):
Think step-by-step:
1. **UNDERSTAND**: What specifically is being asked?
2. **PLAN**: What information do I need? Which tools will help?
3. **EXECUTE**: Gather information systematically
4. **SYNTHESIZE**: Combine findings into a coherent answer

## Tool Selection Guidelines

| Need | Tool(s) to Use |
|------|----------------|
| Program documentation | search_skills -> load_skill |
| File structure overview | citadel_analyze_file or citadel_get_file_summary |
| List functions/paragraphs | citadel_get_functions |
| See function code | citadel_get_function_body (single) or citadel_get_function_bodies (batch) |
| What does X call? | citadel_get_callouts |
| Who calls X? | citadel_get_callers |
| Impact analysis | citadel_get_callers + trace call chains |
| Visualize flow | citadel_get_flow_diagram (single file) or citadel_get_sequence_diagrams (cross-file) |
| Find unused code | citadel_get_dead_code |
| Understand patterns | citadel_get_analysis_patterns |
| Find specific text | search_code |
| Read raw code | read_file |

## Response Guidelines

- **Cite sources**: Mention which files, skills, or analysis results you used
- **Use diagrams**: Include Mermaid diagrams when they clarify flow or relationships
- **Be explicit about limitations**: If you cannot find something or are uncertain, say so
- **Show your reasoning**: For multi-step analysis, briefly explain your approach

## Domain Context

The codebase contains mainframe programs (COBOL, JCL, PL/I, Assembler, REXX) for a financial authorization system. Key concepts include:
- IMS databases for hierarchical data storage
- CICS for online transaction processing
- IBM MQ for messaging
- Batch jobs for scheduled processing
- COBOL paragraphs as the primary unit of program structure
- COPY statements for including shared copybooks
"""


class ReActAgent:
    """ReAct-style agent using llm_providers directly.

    This agent implements a simple reason-act loop:
    1. Send messages to LLM with tool definitions
    2. If LLM requests tools, execute them and loop
    3. If LLM provides response, return it

    Attributes:
        config: Agent configuration.
        registry: Tool registry with all available tools.
        provider: LLM provider instance.
        conversation: Current conversation state.

    Example:
        agent = ReActAgent(config, registry)
        response = await agent.chat("What does CBPAUP0C do?")
        await agent.reset()  # Clear conversation
    """

    def __init__(
        self,
        config: "AgentConfig",
        registry: "ToolRegistry",
        provider: "LLMProvider | None" = None,
        minion_processor: "MinionProcessor | None" = None,
        skills_index: "SkillsIndex | None" = None,
    ):
        """Initialize the ReAct agent.

        Args:
            config: Agent configuration.
            registry: Tool registry with available tools.
            provider: Optional LLM provider. If None, creates from environment.
            minion_processor: Optional minion for summarizing large results.
            skills_index: Optional skills index for CLI display.
        """
        self.config = config
        self.registry = registry
        self._minion = minion_processor
        self._provider: LLMProvider | None = provider
        self.skills_index = skills_index

        # Initialize conversation with system prompt
        self.conversation = Conversation(max_history=config.max_history * 2)
        self.conversation.add_system(SYSTEM_PROMPT)

        logger.info(
            f"ReActAgent initialized: model={config.model}, "
            f"tools={len(registry)}, minions={'enabled' if minion_processor else 'disabled'}"
        )

    @property
    def provider(self) -> "LLMProvider":
        """Get the LLM provider, creating if needed.

        Returns:
            Configured LLM provider.
        """
        if self._provider is None:
            self._provider = get_provider_from_env()
        return self._provider

    async def chat(self, user_message: str) -> str:
        """Process a user message and return a response.

        Implements the ReAct loop:
        1. Add user message to conversation
        2. Call LLM with tools
        3. If tool calls, execute and loop
        4. Return final response

        Args:
            user_message: The user's query.

        Returns:
            The assistant's response text.
        """
        logger.info(f"Chat: {user_message[:100]}...")

        # Add user message
        self.conversation.add_user(user_message)

        iteration = 0
        max_iterations = getattr(self.config, "max_iterations", 10)

        while iteration < max_iterations:
            iteration += 1
            logger.debug(f"ReAct iteration {iteration}/{max_iterations}")

            # Call LLM
            response = await self._call_llm()

            # Check for tool calls in response
            tool_calls = self._extract_tool_calls(response)

            if tool_calls:
                # Execute tools and continue loop
                logger.info(f"Executing {len(tool_calls)} tool call(s)")
                await self._execute_tools(tool_calls)
            else:
                # No tool calls - return the response
                content = response.content
                self.conversation.add_assistant(content=content)
                logger.info(f"Final response: {len(content)} chars")
                return content

        # Max iterations reached
        logger.warning(f"Max iterations ({max_iterations}) reached")
        return (
            "I apologize, but I've reached the maximum number of tool calls for this query. "
            "Here's what I found so far based on the tools I've used. "
            "Please refine your question if you need more specific information."
        )

    async def _call_llm(self) -> "CompletionResponse":
        """Call the LLM with current conversation and tools.

        Returns:
            Completion response from the LLM.
        """
        # Convert conversation to provider messages
        # Note: We need to handle tool messages specially since
        # llm_providers.Message only supports system/user/assistant

        messages = self._build_llm_messages()

        # Get tools schema
        tools_schema = self.registry.to_openai_schema()

        logger.debug(f"Calling LLM with {len(messages)} messages, {len(tools_schema)} tools")

        # Call the provider
        # Note: Tool calling is passed via kwargs - provider implementations
        # should handle this appropriately
        response = await self.provider.complete(
            messages=messages,
            model=self.config.model,
            temperature=self.config.temperature,
            max_tokens=self.config.max_tokens,
            tools=tools_schema,
        )

        logger.debug(f"LLM response: {len(response.content)} chars, {response.tokens_used} tokens")
        return response

    def _build_llm_messages(self) -> list[Message]:
        """Build messages list for LLM, handling tool messages.

        Returns:
            List of Message objects for the provider.
        """
        from llm_providers import Message

        messages: list[Message] = []

        for msg in self.conversation.messages:
            if msg.role == "system":
                messages.append(Message(role="system", content=msg.content or ""))
            elif msg.role == "user":
                messages.append(Message(role="user", content=msg.content or ""))
            elif msg.role == "assistant":
                # For assistant messages with tool calls, we need to format appropriately
                if msg.tool_calls:
                    # Format tool calls as part of assistant message
                    tool_calls_text = "\n".join(
                        f"[Tool Call: {tc.name}({json.dumps(tc.arguments)})]"
                        for tc in msg.tool_calls
                    )
                    content = msg.content or ""
                    if tool_calls_text:
                        content = f"{content}\n{tool_calls_text}" if content else tool_calls_text
                    messages.append(Message(role="assistant", content=content))
                else:
                    messages.append(Message(role="assistant", content=msg.content or ""))
            elif msg.role == "tool":
                # Convert tool result to user message format
                # This is a simplification - some providers handle tool messages natively
                tool_result_text = f"[Tool Result ({msg.name})]:\n{msg.content or ''}"
                messages.append(Message(role="user", content=tool_result_text))

        return messages

    def _extract_tool_calls(self, response: "CompletionResponse") -> list[ToolCall]:
        """Extract tool calls from LLM response.

        Args:
            response: The completion response.

        Returns:
            List of tool calls, empty if none.
        """
        # Check raw_response for tool_calls (OpenAI format)
        if response.raw_response:
            raw_tool_calls = response.raw_response.get("tool_calls", [])
            if raw_tool_calls:
                return [ToolCall.from_openai_format(tc) for tc in raw_tool_calls]

            # Also check message format
            message = response.raw_response.get("message", {})
            msg_tool_calls = message.get("tool_calls", [])
            if msg_tool_calls:
                return [ToolCall.from_openai_format(tc) for tc in msg_tool_calls]

        # Fall back to parsing from content (for providers that embed tool calls in text)
        # This is a simplified heuristic - may need refinement
        content = response.content
        if "[Tool Call:" in content:
            # Parse embedded tool calls from our own format
            return self._parse_embedded_tool_calls(content)

        return []

    def _parse_embedded_tool_calls(self, content: str) -> list[ToolCall]:
        """Parse tool calls embedded in response text.

        This is a fallback for providers that don't support native tool calling.

        Args:
            content: Response text that may contain embedded tool calls.

        Returns:
            Parsed tool calls.
        """
        import re

        tool_calls: list[ToolCall] = []

        # Pattern: [Tool Call: name({"arg": "value"})]
        pattern = r'\[Tool Call: (\w+)\((.*?)\)\]'
        matches = re.findall(pattern, content, re.DOTALL)

        for name, args_str in matches:
            try:
                arguments = json.loads(args_str) if args_str.strip() else {}
                tool_calls.append(
                    ToolCall(
                        id=f"call_{uuid.uuid4().hex[:12]}",
                        name=name,
                        arguments=arguments,
                    )
                )
            except json.JSONDecodeError:
                logger.warning(f"Failed to parse tool call arguments: {args_str}")

        return tool_calls

    async def _execute_tools(self, tool_calls: list[ToolCall]) -> None:
        """Execute tool calls and add results to conversation.

        Args:
            tool_calls: List of tool calls to execute.
        """
        # Add assistant message with tool calls
        self.conversation.add_assistant(tool_calls=tool_calls)

        # Execute tools (in parallel for efficiency)
        results = await self.registry.execute_batch(tool_calls)

        # Process results through minion if enabled and result is large
        for result in results:
            if self._minion and len(result.content) > self._minion.threshold:
                logger.debug(
                    f"Summarizing {result.name} result: {len(result.content)} chars"
                )
                result.content = await self._minion.summarize_result(
                    result.name, result.content
                )

            # Add tool result to conversation
            self.conversation.add_tool_result(result)

    async def reset(self) -> None:
        """Reset the conversation state.

        Clears all messages except the system prompt.
        """
        logger.info("Resetting conversation")
        self.conversation.clear()


def create_agent(config: "AgentConfig") -> ReActAgent:
    """Create a CodeWhisper agent from configuration.

    This is the main entry point for creating an agent instance.
    Sets up the tool registry with skills and code directory access.

    Args:
        config: Agent configuration.

    Returns:
        Initialized ReActAgent.

    Raises:
        ValueError: If required directories don't exist.

    Example:
        config = AgentConfig(
            skills_dir="./skills",
            code_dir="./src",
        )
        agent = create_agent(config)
    """
    from codewhisper.agent.minion import MinionProcessor
    from codewhisper.agent.tools import create_tool_registry
    from codewhisper.skills.index import SkillsIndex
    from codewhisper.skills.loader import SkillsLoader

    # Validate directories
    if not config.skills_dir.exists():
        raise ValueError(f"Skills directory does not exist: {config.skills_dir}")

    if not config.code_dir.exists():
        raise ValueError(f"Code directory does not exist: {config.code_dir}")

    # Load skills
    logger.info(f"Loading skills from {config.skills_dir}")
    loader = SkillsLoader(config.skills_dir)
    skills_index = SkillsIndex.from_loader(loader)
    logger.info(f"Loaded {len(skills_index)} skill(s)")

    # Create tool registry
    registry = create_tool_registry(skills_index, config)
    logger.info(f"Registered {len(registry)} tools")

    # Create minion processor if enabled
    minion_processor = None
    if config.use_minions:
        minion_processor = MinionProcessor()
        logger.info("Minion processor enabled")

    # Create and return agent
    return ReActAgent(
        config=config,
        registry=registry,
        minion_processor=minion_processor,
        skills_index=skills_index,
    )
