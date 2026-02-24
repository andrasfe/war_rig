"""LLM Provider abstractions for War Rig.

This package is a thin extension layer over ``llm_providers``, adding:
- **Tool calling**: ``ProviderToolCall``, ``ToolCallFunction``, and an extended
  ``CompletionResponse`` with ``tool_calls`` / ``has_tool_calls``.
- **Circuit breaker**: ``CircuitBreakerProvider`` wraps every provider returned
  by ``get_provider_from_env()`` for fault tolerance.
- **SCRIBE_MODEL fallback**: ``get_provider_from_env("openrouter")`` reads
  ``SCRIBE_MODEL`` as a model default for backward compatibility.

Base types re-exported from ``llm_providers``:
    Message, LLMProvider, ProviderConfig, OpenRouterConfig, AnthropicConfig,
    OpenAIConfig, AnthropicProvider, OpenAIProvider.

War Rig-specific:
    CompletionResponse (extended), ProviderToolCall, ToolCallFunction,
    OpenRouterProvider (tool-calling variant), OpenRouterProviderError,
    CircuitBreakerProvider.

Example:
    from war_rig.providers import get_provider_from_env, Message

    # Create provider from LLM_PROVIDER environment variable
    provider = get_provider_from_env()

    messages = [
        Message(role="system", content="You are a helpful assistant."),
        Message(role="user", content="Hello!"),
    ]
    response = await provider.complete(messages)
    print(response.content)

    # Check for tool calls
    if response.has_tool_calls:
        for tc in response.tool_calls:
            print(f"Tool: {tc.function.name}, Args: {tc.function.arguments}")
"""

from llm_providers.providers.anthropic import AnthropicProvider
from llm_providers.providers.openai import OpenAIProvider

from war_rig.providers.config import (
    AnthropicConfig,
    OpenAIConfig,
    OpenRouterConfig,
    ProviderConfig,
)
from war_rig.providers.factory import (
    create_provider,
    get_available_providers,
    get_provider_from_env,
    register_provider,
)
from war_rig.providers.openrouter import (
    OpenRouterProvider,
    OpenRouterProviderError,
)
from war_rig.providers.protocol import (
    CompletionResponse,
    LLMProvider,
    Message,
    ProviderToolCall,
    ToolCallFunction,
)

__all__ = [
    # Base types (from llm_providers via protocol.py)
    "CompletionResponse",
    "LLMProvider",
    "Message",
    # Tool calling (war_rig extensions)
    "ProviderToolCall",
    "ToolCallFunction",
    # Configuration (from llm_providers via config.py)
    "AnthropicConfig",
    "OpenAIConfig",
    "OpenRouterConfig",
    "ProviderConfig",
    # Provider implementations
    "AnthropicProvider",
    "OpenAIProvider",
    "OpenRouterProvider",
    "OpenRouterProviderError",
    # Factory functions
    "create_provider",
    "get_available_providers",
    "get_provider_from_env",
    "register_provider",
]
