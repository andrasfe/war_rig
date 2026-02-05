"""ReAct loop implementation for CodeWhisper SDK.

This module provides a simple ReAct (Reason + Act) agent loop that replaces
LangGraph's StateGraph with a straightforward while loop. The loop handles
LLM interactions, tool calling, and optional result summarization.

The ReAct pattern follows this cycle:
1. Receive user input
2. Send to LLM with available tools
3. If LLM returns tool calls, execute them
4. Add tool results to conversation
5. Repeat until LLM returns a final response (no tool calls)

Example:
    from codewhisper.core.react_loop import ReActLoop, ReActConfig
    from codewhisper.tools.registry import ToolRegistry

    loop = ReActLoop(
        llm_provider=my_provider,
        tool_registry=registry,
        system_prompt="You are a helpful assistant.",
    )

    response, history, iterations, tool_calls = await loop.run(
        user_message="What does main.py do?",
        history=[],
    )
"""

from __future__ import annotations

import json
import logging
from dataclasses import dataclass
from typing import TYPE_CHECKING, Any, Protocol, runtime_checkable

from codewhisper.core.message import Message, ToolCall, ToolResult

if TYPE_CHECKING:
    from codewhisper.tools.registry import ToolRegistry

logger = logging.getLogger(__name__)


@runtime_checkable
class LLMProvider(Protocol):
    """Protocol for LLM providers with tool support.

    Any class implementing this method can be used as an LLM provider
    for the ReAct loop.
    """

    async def complete(
        self,
        messages: list[dict[str, Any]],
        tools: list[dict[str, Any]] | None = None,
        **kwargs: Any,
    ) -> Any:
        """Send messages and get a completion response."""
        ...


@runtime_checkable
class MinionProcessor(Protocol):
    """Protocol for minion processors that summarize large results.

    The minion processor is optional. When provided, it summarizes
    tool results that exceed a size threshold to reduce token usage.
    """

    async def maybe_summarize(self, tool_name: str, result: str) -> str:
        """Summarize a tool result if it exceeds the threshold.

        Args:
            tool_name: Name of the tool that produced the result.
            result: The tool's output to potentially summarize.

        Returns:
            Either the original result or a summarized version.
        """
        ...


@dataclass
class ReActConfig:
    """Configuration for the ReAct loop.

    Attributes:
        max_iterations: Maximum number of LLM call iterations before stopping.
            Prevents infinite loops if the LLM keeps making tool calls.
        temperature: Sampling temperature for LLM responses. Lower values
            (0.0-0.3) are more deterministic.
        max_tokens: Maximum tokens in each LLM response.
        parallel_tool_execution: Whether to execute multiple tool calls
            in parallel. Set to False for sequential execution.

    Example:
        config = ReActConfig(
            max_iterations=15,
            temperature=0.2,
            max_tokens=8192,
        )
    """

    max_iterations: int = 10
    temperature: float = 0.3
    max_tokens: int = 4096
    parallel_tool_execution: bool = True


@dataclass
class ReActResult:
    """Result from a ReAct loop execution.

    Attributes:
        response: The final text response from the assistant.
        history: Updated conversation history including all messages.
        iterations: Number of LLM call iterations completed.
        tool_calls_made: Total number of tool calls executed.
        reached_max_iterations: True if stopped due to max_iterations limit.

    Example:
        result = await loop.run("Explain main.py", [])
        if result.reached_max_iterations:
            print("Warning: Max iterations reached")
        print(f"Response: {result.response}")
    """

    response: str
    history: list[Message]
    iterations: int
    tool_calls_made: int
    reached_max_iterations: bool = False


class ReActLoop:
    """Simple ReAct (Reason + Act) agent loop.

    This class implements the core ReAct pattern without LangGraph dependencies.
    It manages the conversation loop between the LLM and tools, handling:

    - Message formatting for the LLM API
    - Tool call detection and execution
    - Optional result summarization via minion processor
    - History management and iteration limits

    Attributes:
        _llm: The LLM provider for generating responses.
        _tools: Registry of available tools.
        _system_prompt: System instructions for the LLM.
        _config: Configuration options.
        _minion: Optional minion processor for summarizing large results.

    Example:
        loop = ReActLoop(
            llm_provider=provider,
            tool_registry=registry,
            system_prompt=SYSTEM_PROMPT,
            config=ReActConfig(max_iterations=15),
        )

        result = await loop.run(
            user_message="What is the main function?",
            history=[],
        )
        print(result.response)
    """

    def __init__(
        self,
        llm_provider: LLMProvider,
        tool_registry: ToolRegistry,
        system_prompt: str,
        config: ReActConfig | None = None,
        minion_processor: MinionProcessor | None = None,
    ) -> None:
        """Initialize the ReAct loop.

        Args:
            llm_provider: The LLM provider for completions. Must implement
                the LLMProvider protocol with tool support.
            tool_registry: Registry of tools available to the LLM.
            system_prompt: System instructions sent with each LLM call.
            config: Optional configuration. Defaults to ReActConfig().
            minion_processor: Optional processor for summarizing large tool
                results. If None, tool results are passed through unchanged.
        """
        self._llm = llm_provider
        self._tools = tool_registry
        self._system_prompt = system_prompt
        self._config = config or ReActConfig()
        self._minion = minion_processor

        logger.debug(
            "ReActLoop initialized with %d tools, max_iterations=%d",
            len(tool_registry),
            self._config.max_iterations,
        )

    async def run(
        self,
        user_message: str,
        history: list[Message],
    ) -> ReActResult:
        """Execute the ReAct loop.

        Processes the user message through the LLM, executing any tool calls
        and continuing until the LLM provides a final response (no tool calls)
        or the maximum iterations are reached.

        Args:
            user_message: The user's input message/question.
            history: Previous conversation history as Message objects.

        Returns:
            ReActResult containing the response, updated history, and metrics.

        Example:
            result = await loop.run(
                user_message="Explain the authentication flow",
                history=previous_messages,
            )

            print(f"Response: {result.response}")
            print(f"Used {result.tool_calls_made} tool calls")
        """
        # Build initial messages list
        messages = self._build_messages(history, user_message)

        # Add user message to our history tracking
        working_history = list(history)
        working_history.append(Message(role="user", content=user_message))

        iterations = 0
        total_tool_calls = 0
        reached_max = False

        # Get tool schemas once
        tool_schemas = self._tools.to_openai_schema() if len(self._tools) > 0 else None

        while iterations < self._config.max_iterations:
            iterations += 1
            logger.debug(
                "ReAct iteration %d/%d", iterations, self._config.max_iterations
            )

            # Call the LLM
            try:
                # Convert messages to provider format (war_rig expects Message objects)
                provider_messages = self._convert_for_provider(messages)
                response = await self._llm.complete(
                    messages=provider_messages,
                    tools=tool_schemas,
                    temperature=self._config.temperature,
                    max_tokens=self._config.max_tokens,
                )
            except Exception as e:
                logger.exception("LLM call failed in ReAct loop")
                error_msg = f"Error during LLM call: {e}"
                return ReActResult(
                    response=error_msg,
                    history=working_history,
                    iterations=iterations,
                    tool_calls_made=total_tool_calls,
                )

            # Parse the response
            assistant_message, tool_calls = self._parse_response(response)

            # Check for model outputting tool call text instead of actual tool calls
            # Some models (like Gemini) output "[Calling tools: xyz]" as text
            # instead of actually invoking the tool
            if not tool_calls and assistant_message.content:
                content_lower = assistant_message.content.lower().strip()
                tool_text_patterns = [
                    "[calling tools:",
                    "[calling tool:",
                    "i will call",
                    "let me call",
                    "i'll call",
                    "calling the",
                    "i will use the",
                    "let me use the",
                ]
                if any(pattern in content_lower for pattern in tool_text_patterns):
                    logger.warning(
                        "Model output tool-calling text instead of making tool call: %s",
                        assistant_message.content[:100],
                    )
                    # Don't add this message to history - it's useless
                    # Instead, add a correction and re-prompt
                    messages.append({
                        "role": "user",
                        "content": (
                            "You described calling a tool but didn't actually invoke it. "
                            "Please make the actual tool call now using the tool calling mechanism - "
                            "don't describe it, just call it directly."
                        ),
                    })
                    # Continue the loop to re-call the LLM
                    continue

            # Add assistant message to tracking
            working_history.append(assistant_message)
            messages.append(assistant_message.to_dict())

            # If no tool calls, we're done
            if not tool_calls:
                logger.debug(
                    "ReAct loop complete: %d iterations, %d tool calls",
                    iterations,
                    total_tool_calls,
                )
                return ReActResult(
                    response=assistant_message.content,
                    history=working_history,
                    iterations=iterations,
                    tool_calls_made=total_tool_calls,
                )

            # Execute tool calls
            total_tool_calls += len(tool_calls)
            logger.debug("Executing %d tool call(s)", len(tool_calls))

            tool_results = await self._execute_tools(tool_calls)

            # Add tool results to messages and history
            for result in tool_results:
                result_message = result.to_message()
                working_history.append(result_message)
                messages.append(result_message.to_dict())

        # Reached max iterations
        reached_max = True
        logger.warning(
            "ReAct loop reached max iterations (%d) with %d tool calls",
            self._config.max_iterations,
            total_tool_calls,
        )

        # Generate a summary response for max iterations case
        summary = self._generate_max_iterations_summary(working_history)
        return ReActResult(
            response=summary,
            history=working_history,
            iterations=iterations,
            tool_calls_made=total_tool_calls,
            reached_max_iterations=reached_max,
        )

    def _build_messages(
        self,
        history: list[Message],
        user_message: str,
    ) -> list[dict[str, Any]]:
        """Build the messages list for the LLM API.

        Constructs the message array with:
        1. System prompt
        2. Conversation history
        3. New user message

        Args:
            history: Previous conversation messages.
            user_message: The new user input.

        Returns:
            List of message dictionaries in OpenAI format.
        """
        messages: list[dict[str, Any]] = []

        # System prompt first
        messages.append({"role": "system", "content": self._system_prompt})

        # Add conversation history
        for msg in history:
            messages.append(msg.to_dict())

        # Add new user message
        messages.append({"role": "user", "content": user_message})

        return messages

    def _convert_for_provider(
        self, messages: list[dict[str, Any]]
    ) -> list[Any]:
        """Convert dict messages to war_rig.providers.Message objects.

        The war_rig providers expect Message objects, not dicts. This method
        handles the conversion, including special handling for tool-related
        messages that war_rig doesn't support (they're converted to assistant
        messages with the tool result in the content).

        Args:
            messages: List of message dicts in OpenAI format.

        Returns:
            List of war_rig.providers.Message objects (or dicts if import fails).
        """
        try:
            from war_rig.providers import Message as ProviderMessage
        except ImportError:
            # Fall back to dicts if war_rig not available
            return messages

        converted: list[Any] = []
        for msg in messages:
            role = msg.get("role", "user")
            content = msg.get("content", "")

            # Skip tool messages - war_rig doesn't support them
            # Instead, the tool result should already be in context
            if role == "tool":
                # Convert tool result to user message for context
                tool_name = msg.get("name", "tool")
                converted.append(
                    ProviderMessage(
                        role="user",
                        content=f"[Tool Result from {tool_name}]: {content}",
                    )
                )
                continue

            # Handle assistant messages with tool calls
            if role == "assistant" and msg.get("tool_calls"):
                tool_calls = msg.get("tool_calls", [])
                tool_desc = ", ".join(
                    tc.get("function", {}).get("name", "unknown")
                    for tc in tool_calls
                )
                # Include both content and tool call info
                full_content = content or ""
                if tool_desc:
                    full_content = f"{full_content}\n[Calling tools: {tool_desc}]".strip()
                converted.append(
                    ProviderMessage(role="assistant", content=full_content or "...")
                )
                continue

            # Standard message
            if role in ("system", "user", "assistant"):
                converted.append(
                    ProviderMessage(role=role, content=content or "")
                )

        return converted

    def _parse_response(self, response: Any) -> tuple[Message, list[ToolCall]]:
        """Parse the LLM response into a Message and tool calls.

        Handles various response formats from different providers:
        - OpenAI-style responses with choices[0].message
        - Dict responses with direct message fields
        - Object responses with attributes

        Args:
            response: The raw response from the LLM provider.

        Returns:
            Tuple of (assistant_message, tool_calls).
        """
        content = ""
        tool_calls: list[ToolCall] = []

        # Extract message from response
        # Handle different response formats

        # Check for war_rig.providers.CompletionResponse (has content and tool_calls directly)
        if hasattr(response, "has_tool_calls") and hasattr(response, "content"):
            # war_rig.providers.CompletionResponse
            content = response.content or ""

            if response.has_tool_calls and response.tool_calls:
                for tc in response.tool_calls:
                    # ProviderToolCall has id, type, function (ToolCallFunction)
                    args_str = tc.function.arguments
                    try:
                        args = json.loads(args_str) if args_str else {}
                    except json.JSONDecodeError:
                        logger.warning(
                            "Failed to parse tool arguments as JSON: %s",
                            args_str[:100] if args_str else "",
                        )
                        args = {}

                    tool_calls.append(
                        ToolCall(
                            id=tc.id,
                            name=tc.function.name,
                            arguments=args,
                        )
                    )

        elif hasattr(response, "choices") and response.choices:
            # OpenAI-style response
            message = response.choices[0].message
            content = message.content or ""

            if hasattr(message, "tool_calls") and message.tool_calls:
                for tc in message.tool_calls:
                    # Parse arguments - may be string or dict
                    args = tc.function.arguments
                    if isinstance(args, str):
                        try:
                            args = json.loads(args)
                        except json.JSONDecodeError:
                            logger.warning(
                                "Failed to parse tool arguments as JSON: %s",
                                args[:100],
                            )
                            args = {}

                    tool_calls.append(
                        ToolCall(
                            id=tc.id,
                            name=tc.function.name,
                            arguments=args,
                        )
                    )

        elif isinstance(response, dict):
            # Dict-style response
            content = response.get("content", "")
            raw_tool_calls = response.get("tool_calls", [])

            for tc in raw_tool_calls:
                # Handle both nested and flat formats
                if "function" in tc:
                    args = tc["function"].get("arguments", {})
                    if isinstance(args, str):
                        try:
                            args = json.loads(args)
                        except json.JSONDecodeError:
                            args = {}
                    tool_calls.append(
                        ToolCall(
                            id=tc.get("id", ""),
                            name=tc["function"]["name"],
                            arguments=args,
                        )
                    )
                else:
                    args = tc.get("arguments", {})
                    if isinstance(args, str):
                        try:
                            args = json.loads(args)
                        except json.JSONDecodeError:
                            args = {}
                    tool_calls.append(
                        ToolCall(
                            id=tc.get("id", ""),
                            name=tc.get("name", ""),
                            arguments=args,
                        )
                    )

        else:
            # Try attribute access as fallback
            content = getattr(response, "content", str(response))
            if hasattr(response, "tool_calls") and response.tool_calls:
                for tc in response.tool_calls:
                    args = getattr(tc, "arguments", {})
                    if isinstance(args, str):
                        try:
                            args = json.loads(args)
                        except json.JSONDecodeError:
                            args = {}
                    tool_calls.append(
                        ToolCall(
                            id=getattr(tc, "id", ""),
                            name=getattr(tc, "name", ""),
                            arguments=args,
                        )
                    )

        # Build assistant message
        assistant_message = Message(
            role="assistant",
            content=content,
            tool_calls=tool_calls if tool_calls else None,
        )

        return assistant_message, tool_calls

    async def _execute_tools(self, tool_calls: list[ToolCall]) -> list[ToolResult]:
        """Execute tool calls and optionally summarize large results.

        Executes tools either in parallel or sequentially based on config.
        If a minion processor is configured, large results are summarized.

        Args:
            tool_calls: List of tool calls to execute.

        Returns:
            List of ToolResult objects in the same order as input.
        """
        results = await self._tools.execute_all(
            tool_calls,
            parallel=self._config.parallel_tool_execution,
        )

        # Apply minion summarization if configured
        if self._minion is not None:
            summarized_results: list[ToolResult] = []
            for result in results:
                if not result.is_error:
                    summarized_content = await self._minion.maybe_summarize(
                        result.name,
                        result.content,
                    )
                    # Create new ToolResult with summarized content
                    summarized_results.append(
                        ToolResult(
                            tool_call_id=result.tool_call_id,
                            name=result.name,
                            content=summarized_content,
                        )
                    )
                else:
                    summarized_results.append(result)
            return summarized_results

        return results

    def _generate_max_iterations_summary(self, history: list[Message]) -> str:
        """Generate a summary when max iterations is reached.

        Creates a helpful message indicating the loop was stopped and
        summarizes what information was gathered.

        Args:
            history: The conversation history up to this point.

        Returns:
            Summary string for the user.
        """
        # Count tool calls and results
        tool_calls_count = 0
        tool_results: list[str] = []

        for msg in history:
            if msg.has_tool_calls and msg.tool_calls:
                tool_calls_count += len(msg.tool_calls)
            if msg.role == "tool" and msg.name:
                tool_results.append(msg.name)

        # Build summary
        summary_parts = [
            f"I reached the maximum number of iterations ({self._config.max_iterations}) "
            "while processing your request.",
        ]

        if tool_calls_count > 0:
            unique_tools = set(tool_results)
            summary_parts.append(
                f"\n\nDuring my analysis, I made {tool_calls_count} tool calls "
                f"using: {', '.join(sorted(unique_tools))}."
            )

        # Try to extract the last substantive assistant message
        for msg in reversed(history):
            if msg.role == "assistant" and msg.content and len(msg.content) > 50:
                summary_parts.append(
                    f"\n\nHere's what I learned so far:\n\n{msg.content}"
                )
                break

        summary_parts.append(
            "\n\nYou may want to ask a more specific follow-up question "
            "to continue the analysis."
        )

        return "".join(summary_parts)
