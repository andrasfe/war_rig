"""LLM Provider protocol for War Rig.

This module re-exports the base protocol types from ``llm_providers`` and adds
war_rig-specific extensions for tool calling.

Base types (from llm_providers):
    - Message: A single message in a conversation (system/user/assistant)
    - LLMProvider: Protocol defining the interface all providers must implement

War Rig extensions:
    - ProviderToolCall: Tool call from an LLM response
    - ToolCallFunction: Function details within a tool call
    - CompletionResponse: Extended with optional tool_calls field

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
            **kwargs,  # tools passed here for tool-calling providers
        ) -> CompletionResponse:
            # Implementation here
            ...

    # MyProvider now satisfies the LLMProvider protocol
    provider: LLMProvider = MyProvider()
"""

from dataclasses import dataclass, field
from typing import Any

# Re-export base types from llm_providers
from llm_providers.protocol import LLMProvider, Message

__all__ = [
    "CompletionResponse",
    "LLMProvider",
    "Message",
    "ProviderToolCall",
    "ToolCallFunction",
]


@dataclass(frozen=True)
class ToolCallFunction:
    """Function call details within a tool call.

    Attributes:
        name: The name of the function to call.
        arguments: JSON string of the function arguments.
    """

    name: str
    arguments: str


@dataclass(frozen=True)
class ProviderToolCall:
    """A tool call from the LLM response.

    Attributes:
        id: Unique identifier for this tool call.
        type: The type of tool call (always "function" for now).
        function: The function call details.
    """

    id: str
    type: str
    function: ToolCallFunction


@dataclass
class CompletionResponse:
    """Response from an LLM completion request.

    Extends the base llm_providers CompletionResponse with optional
    tool_calls for providers that support tool calling.

    Attributes:
        content: The generated text content.
        model: The model that generated the response.
        tokens_used: Total tokens consumed (prompt + completion).
        raw_response: Optional raw response from the provider for debugging
            or accessing provider-specific fields.
        tool_calls: Optional list of tool calls requested by the LLM.
    """

    content: str
    model: str
    tokens_used: int
    raw_response: dict[str, Any] | None = field(default=None)
    tool_calls: list[ProviderToolCall] | None = field(default=None)

    @property
    def has_content(self) -> bool:
        """Check if the response contains non-empty content.

        Returns:
            True if content is non-empty after stripping whitespace.
        """
        return bool(self.content and self.content.strip())

    @property
    def has_tool_calls(self) -> bool:
        """Check if the response contains tool calls.

        Returns:
            True if tool_calls is non-empty.
        """
        return bool(self.tool_calls)
