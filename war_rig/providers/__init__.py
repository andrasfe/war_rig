"""LLM Provider abstractions for War Rig.

This package provides a **provider-agnostic** interface for LLM providers,
allowing War Rig to work with any LLM backend (OpenRouter, Azure, local
models, custom APIs) without coupling to specific SDKs.

**Provider-Agnostic Design:**
    War Rig does not assume any specific LLM provider. The provider is
    determined by the LLM_PROVIDER environment variable, and each provider
    handles its own configuration (API keys, base URLs, etc.).

The core abstractions are:
- Message: A single message in a conversation (system/user/assistant)
- CompletionResponse: Response from an LLM completion request
- LLMProvider: Protocol defining the interface all providers must implement
- ProviderToolCall: Tool call from an LLM response (for tool-calling providers)
- ToolCallFunction: Function details within a tool call

Configuration:
- ProviderConfig: Base configuration for any LLM provider
- OpenRouterConfig: Configuration specific to OpenRouter (built-in provider)

Factory functions:
- create_provider: Create a provider by name with explicit configuration
- register_provider: Register a custom provider class
- get_provider_from_env: Create a provider from environment variables
- get_available_providers: List all registered providers (built-in + plugins)

Plugin Discovery:
    External packages can register providers without modifying war_rig by
    using Python entry points. Add to your pyproject.toml:

        [project.entry-points."war_rig.providers"]
        myprovider = "mypackage.provider:MyProviderClass"

    The provider will be auto-discovered when war_rig loads.

Tool Calling:
    Providers can optionally support tool calling. See TOOL_CALLING.md for
    implementation details.

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

from war_rig.providers.config import (
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
    "CompletionResponse",
    "LLMProvider",
    "Message",
    "ProviderToolCall",
    "ToolCallFunction",
    "OpenRouterConfig",
    "OpenRouterProvider",
    "OpenRouterProviderError",
    "ProviderConfig",
    "create_provider",
    "get_available_providers",
    "get_provider_from_env",
    "register_provider",
]
