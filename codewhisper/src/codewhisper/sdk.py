"""CodeWhisper SDK - Main entry point for programmatic usage.

This module provides the main CodeWhisper class for interacting with
codebases through an LLM-powered agent. It abstracts away the complexity
of tool management, conversation history, and LLM interactions.

The SDK is designed to be provider-agnostic - any LLM provider that
implements the LLMProvider protocol can be used.

Example:
    from pathlib import Path
    from codewhisper.sdk import CodeWhisper, CodeWhisperConfig
    from war_rig.providers import get_provider_from_env

    # Create provider and SDK instance
    provider = get_provider_from_env()
    sdk = CodeWhisper(
        llm_provider=provider,
        code_dir=Path("./src"),
        documents_dir=Path("./docs"),
    )

    # Ask questions about the codebase
    result = await sdk.complete("What does the main function do?")
    print(result.content)

    # Continue the conversation
    result = await sdk.complete("What parameters does it accept?")
    print(result.content)

    # Reset and start fresh
    sdk.reset()
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from pathlib import Path
from typing import TYPE_CHECKING, Any, Protocol, runtime_checkable

from codewhisper.core.message import Message
from codewhisper.core.react_loop import ReActConfig, ReActLoop
from codewhisper.tools.registry import ToolRegistry

if TYPE_CHECKING:
    from codewhisper.minion import MinionProcessor
    from codewhisper.skills.index import SkillsIndex

logger = logging.getLogger(__name__)

# System prompt imported from agent/graph.py - this is the canonical version
# We import it here to keep SDK self-contained while maintaining compatibility
SYSTEM_PROMPT = """You are CodeWhisper, an expert assistant for exploring and understanding mainframe codebases. Your job is to help users understand THIS codebase - when they ask about "the system", "this code", or "how it works", they mean the codebase you have access to. Never ask for clarification about which system they mean.

## CRITICAL: Always Search First

Before answering ANY question about the system, architecture, or functionality:
1. **ALWAYS search_skills first** - This is your primary knowledge source
2. Then load_skill for relevant results
3. Only then use code analysis tools if needed

Do NOT ask clarifying questions when you can search for answers instead. Be proactive.

## Knowledge Tools (Skills) - USE THESE FIRST
- **search_skills**: Find documentation by keyword - ALWAYS start here for conceptual questions
- **load_skill**: Load specific skill content after finding relevant skills
- **search_code**: Search source code with regex patterns
- **read_file**: Read raw source files to examine implementation

## Analysis Tools (Citadel) - For Code Deep-Dives
- **citadel_analyze_file**: Full structural analysis of a file
- **citadel_get_functions**: List all functions/paragraphs with their calls
- **citadel_get_callouts**: Get all references from file or directory
- **citadel_get_includes**: Get preprocessor includes (COPY statements)
- **citadel_get_function_body**: Extract specific function's source code
- **citadel_get_function_bodies**: Batch extract multiple functions
- **citadel_get_file_stats**: Get structural statistics
- **citadel_get_callers**: Find all callers of a function
- **citadel_get_sequence_diagrams**: Generate Mermaid call chain diagrams
- **citadel_get_dead_code**: Find unreferenced artifacts
- **citadel_get_flow_diagram**: Generate Mermaid control flow diagram
- **citadel_get_file_summary**: Compact file overview
- **citadel_get_analysis_patterns**: Extract code patterns

## Approach Strategy

For ANY question about the system:
1. **search_skills** with relevant keywords (e.g., "authorization", "system", "overview")
2. **load_skill** for the most relevant results
3. If skills don't have the answer, use citadel tools to analyze code
4. Synthesize findings into a coherent answer

NEVER respond with "which system do you mean?" or similar clarifying questions. This IS the system.

## Response Guidelines

- **Act immediately**: Search skills and gather information before responding
- **Cite sources**: Mention which skills or files you used
- **Use diagrams**: Include Mermaid diagrams when helpful
- **Be proactive**: If you need more information, search for it rather than asking

## Domain Context

The codebase contains mainframe programs (COBOL, JCL, PL/I, Assembler, REXX) for a financial authorization system with:
- IMS databases for hierarchical data storage
- CICS for online transaction processing
- IBM MQ for messaging
- Batch jobs for scheduled processing
- COBOL paragraphs as the primary unit of program structure
"""


def _create_minion_processor(
    provider: LLMProvider,
    threshold: int,
) -> MinionProcessor:
    """Create a MinionProcessor instance.

    Factory function that creates a MinionProcessor with the specified
    provider and threshold configuration.

    Args:
        provider: LLM provider for making summarization calls.
        threshold: Character count threshold for summarization.

    Returns:
        Configured MinionProcessor instance.
    """
    from codewhisper.minion import MinionConfig, MinionProcessor

    config = MinionConfig(threshold=threshold)
    return MinionProcessor(llm_provider=provider, config=config)


@runtime_checkable
class LLMProvider(Protocol):
    """Protocol for LLM providers compatible with war_rig.providers.

    Any class implementing these methods can be used as an LLM provider
    for CodeWhisper. This uses structural subtyping (duck typing) via
    Python's Protocol, so no inheritance is required.

    The protocol is intentionally minimal and matches war_rig.providers.LLMProvider,
    but with an extended signature to support tool calling.

    Example implementation:
        class MyProvider:
            async def complete(
                self,
                messages: list[dict],
                tools: list[dict] | None = None,
                **kwargs,
            ) -> Any:
                # Make API call and return response
                ...
    """

    async def complete(
        self,
        messages: list[dict[str, Any]],
        tools: list[dict[str, Any]] | None = None,
        **kwargs: Any,
    ) -> Any:
        """Send messages and get a completion response.

        Args:
            messages: List of messages in OpenAI format (role, content, etc.).
            tools: Optional list of tool definitions in OpenAI format.
            **kwargs: Provider-specific parameters (temperature, max_tokens, etc.).

        Returns:
            Response object from the provider. The exact type depends on
            the provider implementation.
        """
        ...


@dataclass
class CodeWhisperConfig:
    """Configuration for the CodeWhisper SDK.

    Attributes:
        max_iterations: Maximum number of tool-calling iterations per request.
            This prevents infinite loops in case the LLM keeps calling tools.
        max_history: Maximum number of messages to keep in conversation history.
            Older messages are trimmed to fit within context limits.
        temperature: Sampling temperature for LLM responses. Lower values
            (0.0-0.3) are more deterministic, higher values more creative.
        max_tokens: Maximum tokens in each LLM response.
        use_minion: Whether to use a smaller "minion" model to summarize
            large tool outputs before passing to the main model.
        minion_threshold: Character count threshold above which tool outputs
            are summarized by the minion model.

    Example:
        config = CodeWhisperConfig(
            max_iterations=15,
            temperature=0.2,
            use_minion=True,
            minion_threshold=16000,
        )
    """

    max_iterations: int = 10
    max_history: int = 20
    temperature: float = 0.3
    max_tokens: int = 4096
    use_minion: bool = True
    minion_threshold: int = 32000


@dataclass
class CompletionResult:
    """Result from a CodeWhisper completion request.

    Attributes:
        content: The final text response from the assistant.
        tool_calls_made: Total number of tool calls made during this request.
        iterations: Number of LLM iterations (request-response cycles).
        messages: The full conversation history after this completion.

    Example:
        result = await sdk.complete("Explain main.py")
        print(f"Response: {result.content}")
        print(f"Made {result.tool_calls_made} tool calls in {result.iterations} iterations")
    """

    content: str
    tool_calls_made: int = 0
    iterations: int = 0
    messages: list[Message] = field(default_factory=list)


class CodeWhisper:
    """Main SDK class for CodeWhisper.

    CodeWhisper provides an LLM-powered interface for exploring and
    understanding codebases. It manages:

    - Tool registration and execution (file reading, code search, etc.)
    - Conversation history with automatic context management
    - Skills/documentation lookup
    - Optional minion model for summarizing large outputs

    The SDK follows a ReAct (Reasoning + Acting) pattern: the LLM
    reasons about what information it needs, calls tools to gather
    that information, and then formulates a response.

    Attributes:
        llm_provider: The LLM provider for generating responses.
        code_dir: Directory containing the source code to explore.
        documents_dir: Optional directory containing documentation/skills.
        config: Configuration options for the SDK.

    Example:
        from codewhisper.sdk import CodeWhisper, CodeWhisperConfig
        from war_rig.providers import get_provider_from_env

        provider = get_provider_from_env()
        sdk = CodeWhisper(
            llm_provider=provider,
            code_dir=Path("./src"),
            documents_dir=Path("./docs/skills"),
            config=CodeWhisperConfig(temperature=0.2),
        )

        # Single query
        result = await sdk.complete("What is the purpose of utils.py?")

        # Multi-turn conversation
        await sdk.complete("List the main functions")
        result = await sdk.complete("Tell me more about the first one")
    """

    def __init__(
        self,
        llm_provider: LLMProvider,
        code_dir: Path,
        documents_dir: Path | None = None,
        skills: list[str] | None = None,
        config: CodeWhisperConfig | None = None,
        minion_provider: LLMProvider | None = None,
    ) -> None:
        """Initialize the CodeWhisper SDK.

        Args:
            llm_provider: The LLM provider for main completions. Must implement
                the LLMProvider protocol.
            code_dir: Path to the source code directory to explore. All file
                operations are restricted to this directory.
            documents_dir: Optional path to documentation/skills directory.
                If provided, skills will be indexed for lookup.
            skills: Optional list of specific skill names to load. If None,
                all skills in documents_dir are available.
            config: Configuration options. Defaults to CodeWhisperConfig().
            minion_provider: Optional separate provider for the minion model.
                If None and use_minion is True, uses llm_provider.

        Example:
            sdk = CodeWhisper(
                llm_provider=my_provider,
                code_dir=Path("./src"),
                documents_dir=Path("./skills"),
                skills=["cobol", "jcl"],  # Only load specific skills
                config=CodeWhisperConfig(max_iterations=15),
            )
        """
        self._llm_provider = llm_provider
        self._code_dir = code_dir.resolve()
        self._documents_dir = documents_dir.resolve() if documents_dir else None
        self._requested_skills = skills
        self._config = config or CodeWhisperConfig()
        self._minion_provider = minion_provider

        # Initialize skills index (lazy - loaded when needed)
        self._skills_index: SkillsIndex | None = None

        # Initialize tool registry with all tools
        from codewhisper.tools.factory import create_tool_registry

        # Get skills index if documents_dir is provided
        skills_index = self._get_skills_index() if documents_dir else None

        # Create registry with enabled tools filter if specified
        enabled_tools = set(skills) if skills else None
        self._tool_registry = create_tool_registry(
            code_dir=self._code_dir,
            skills_index=skills_index,
            enabled_tools=enabled_tools,
        )

        # Conversation history
        self._conversation_history: list[Message] = []

        logger.info(
            "CodeWhisper initialized with code_dir=%s, documents_dir=%s",
            self._code_dir,
            self._documents_dir,
        )

    @property
    def code_dir(self) -> Path:
        """Get the code directory path.

        Returns:
            Resolved absolute path to the code directory.
        """
        return self._code_dir

    @property
    def documents_dir(self) -> Path | None:
        """Get the documents/skills directory path.

        Returns:
            Resolved absolute path to documents directory, or None if not set.
        """
        return self._documents_dir

    @property
    def config(self) -> CodeWhisperConfig:
        """Get the current configuration.

        Returns:
            The CodeWhisperConfig instance.
        """
        return self._config

    @property
    def tool_registry(self) -> ToolRegistry:
        """Get the tool registry.

        Returns:
            The ToolRegistry instance for registering custom tools.
        """
        return self._tool_registry

    @property
    def conversation_history(self) -> list[Message]:
        """Get the current conversation history.

        Returns:
            List of Message objects in the conversation.
        """
        return list(self._conversation_history)

    def _get_skills_index(self) -> SkillsIndex | None:
        """Get or create the skills index.

        Lazily initializes the skills index on first access.

        Returns:
            The SkillsIndex if documents_dir is configured, None otherwise.
        """
        if self._skills_index is not None:
            return self._skills_index

        if self._documents_dir is None:
            return None

        # Lazy import to avoid circular dependencies
        from codewhisper.skills.index import SkillsIndex
        from codewhisper.skills.loader import SkillsLoader

        try:
            loader = SkillsLoader(self._documents_dir)
            self._skills_index = SkillsIndex.from_loader(loader)
            logger.info(
                "Skills index initialized with %d skills", len(self._skills_index)
            )
        except Exception as e:
            logger.warning("Failed to initialize skills index: %s", e)
            return None

        return self._skills_index

    async def complete(
        self,
        prompt: str,
        context: str | None = None,
        reset_conversation: bool = False,
    ) -> CompletionResult:
        """Send a prompt and get a completion response.

        This is the main entry point for interacting with CodeWhisper.
        The method handles the full ReAct loop: sending the prompt to the
        LLM, executing any tool calls, and returning the final response.

        Args:
            prompt: The user's question or request.
            context: Optional additional context to include with the prompt.
            reset_conversation: If True, clears conversation history before
                processing. Useful for starting a new topic.

        Returns:
            CompletionResult containing the response and metadata.

        Example:
            # Simple question
            result = await sdk.complete("What does main.py do?")

            # With context
            result = await sdk.complete(
                "Explain this error",
                context="TypeError: 'NoneType' object is not callable",
            )

            # Start fresh conversation
            result = await sdk.complete(
                "New topic: how is logging configured?",
                reset_conversation=True,
            )
        """
        if reset_conversation:
            self.reset()

        # Build the user message
        content = prompt
        if context:
            content = f"{context}\n\n{prompt}"

        logger.info("Processing prompt: %s...", prompt[:50])

        # Create minion processor if configured
        minion_processor: MinionProcessor | None = None
        if self._config.use_minion:
            # Use separate minion provider if available, otherwise use main provider
            minion_provider = self._minion_provider or self._llm_provider
            minion_processor = _create_minion_processor(
                provider=minion_provider,
                threshold=self._config.minion_threshold,
            )

        # Create and configure the ReAct loop
        react_config = ReActConfig(
            max_iterations=self._config.max_iterations,
            temperature=self._config.temperature,
            max_tokens=self._config.max_tokens,
        )

        react_loop = ReActLoop(
            llm_provider=self._llm_provider,
            tool_registry=self._tool_registry,
            system_prompt=SYSTEM_PROMPT,
            config=react_config,
            minion_processor=minion_processor,
        )

        # Run the ReAct loop
        # Note: We pass the history WITHOUT the new user message since
        # the ReAct loop adds it internally
        react_result = await react_loop.run(
            user_message=content,
            history=list(self._conversation_history),
        )

        # Update our conversation history from the result
        self._conversation_history = react_result.history

        # Trim history if needed
        self._trim_history()

        return CompletionResult(
            content=react_result.response,
            tool_calls_made=react_result.tool_calls_made,
            iterations=react_result.iterations,
            messages=list(self._conversation_history),
        )

    def reset(self) -> None:
        """Reset the conversation history.

        Clears all messages from the conversation, starting fresh.
        Does not affect tool registrations or configuration.

        Example:
            sdk.reset()
            result = await sdk.complete("Start a new conversation")
        """
        logger.debug("Resetting conversation history")
        self._conversation_history = []

    def add_system_message(self, content: str) -> None:
        """Add a system message to the conversation.

        System messages provide instructions to the LLM about its behavior,
        persona, or constraints.

        Args:
            content: The system message content.

        Example:
            sdk.add_system_message(
                "You are a COBOL expert. Explain code in business terms."
            )
        """
        message = Message(role="system", content=content)
        self._conversation_history.append(message)
        logger.debug("Added system message (%d chars)", len(content))

    def _trim_history(self) -> None:
        """Trim conversation history to max_history messages.

        Preserves system messages and the most recent messages.
        """
        if len(self._conversation_history) <= self._config.max_history:
            return

        # Separate system messages (keep all) and other messages
        system_messages = [m for m in self._conversation_history if m.role == "system"]
        other_messages = [m for m in self._conversation_history if m.role != "system"]

        # Keep most recent non-system messages
        keep_count = self._config.max_history - len(system_messages)
        if keep_count > 0:
            other_messages = other_messages[-keep_count:]
        else:
            other_messages = []

        self._conversation_history = system_messages + other_messages
        logger.debug("Trimmed history to %d messages", len(self._conversation_history))

    def _messages_to_dicts(self) -> list[dict[str, Any]]:
        """Convert conversation history to API-compatible format.

        Returns:
            List of message dictionaries for the LLM API.
        """
        return [m.to_dict() for m in self._conversation_history]
