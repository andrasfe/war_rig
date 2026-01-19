"""LLM Provider protocol for War Rig.

This module defines a provider-agnostic interface for LLM providers using
Python's Protocol (structural subtyping). Any LLM provider (OpenRouter,
Azure, local models, custom APIs) can implement this interface without
inheriting from a base class.

The protocol is intentionally minimal and dependency-free - it does NOT
depend on LangChain, OpenAI SDK, or any specific LLM library.

Example:
    class MyProvider:
        @property
        def default_model(self) -> str:
            return "my-model"

        async def complete(
            self,
            messages: list[Message],
            model: str | None = None,
            temperature: float = 0.7,
            max_tokens: int | None = None,
            **kwargs,
        ) -> CompletionResponse:
            # Implementation here
            ...

    # MyProvider now satisfies the LLMProvider protocol
    provider: LLMProvider = MyProvider()
"""

from dataclasses import dataclass, field
from typing import Any, Protocol, runtime_checkable


@dataclass(frozen=True)
class Message:
    """A single message in a conversation.

    Attributes:
        role: The role of the message sender. Must be one of:
            - "system": System instructions for the model
            - "user": User input
            - "assistant": Model response
        content: The text content of the message.
    """

    role: str
    content: str

    def __post_init__(self) -> None:
        """Validate the message role."""
        valid_roles = {"system", "user", "assistant"}
        if self.role not in valid_roles:
            raise ValueError(
                f"Invalid role '{self.role}'. Must be one of: {valid_roles}"
            )


@dataclass
class CompletionResponse:
    """Response from an LLM completion request.

    Attributes:
        content: The generated text content.
        model: The model that generated the response.
        tokens_used: Total tokens consumed (prompt + completion).
        raw_response: Optional raw response from the provider for debugging
            or accessing provider-specific fields.
    """

    content: str
    model: str
    tokens_used: int
    raw_response: dict[str, Any] | None = field(default=None)

    @property
    def has_content(self) -> bool:
        """Check if the response contains non-empty content.

        Returns:
            True if content is non-empty after stripping whitespace.
        """
        return bool(self.content and self.content.strip())


@runtime_checkable
class LLMProvider(Protocol):
    """Protocol for LLM providers.

    Any class implementing these methods can be used as an LLM provider
    in War Rig. This uses structural subtyping (duck typing) via Python's
    Protocol, so no inheritance is required.

    The @runtime_checkable decorator allows isinstance() checks:
        if isinstance(my_provider, LLMProvider):
            result = await my_provider.complete(messages)

    Example implementation:
        class OpenRouterProvider:
            def __init__(self, api_key: str):
                self.api_key = api_key
                self._default_model = "anthropic/claude-3-sonnet"

            @property
            def default_model(self) -> str:
                return self._default_model

            async def complete(
                self,
                messages: list[Message],
                model: str | None = None,
                temperature: float = 0.7,
                max_tokens: int | None = None,
                **kwargs,
            ) -> CompletionResponse:
                # Make API call and return response
                ...
    """

    @property
    def default_model(self) -> str:
        """The default model for this provider.

        Returns:
            The model identifier string to use when no model is specified
            in the complete() call.
        """
        ...

    async def complete(
        self,
        messages: list[Message],
        model: str | None = None,
        temperature: float = 0.7,
        max_tokens: int | None = None,
        **kwargs: Any,
    ) -> CompletionResponse:
        """Send messages and get a completion response.

        Args:
            messages: List of messages forming the conversation history.
                Should typically include at least a system message and
                a user message.
            model: Optional model override. If None, uses default_model.
            temperature: Sampling temperature (0.0 = deterministic,
                1.0+ = more random). Default is 0.7.
            max_tokens: Maximum tokens to generate. If None, provider
                decides the limit.
            **kwargs: Provider-specific parameters (e.g., top_p, stop
                sequences, etc.).

        Returns:
            CompletionResponse containing the generated content and metadata.

        Raises:
            Provider-specific exceptions for API errors, rate limits, etc.
        """
        ...
