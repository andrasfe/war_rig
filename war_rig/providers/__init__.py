"""LLM Provider abstractions for War Rig.

This package provides a provider-agnostic interface for LLM providers,
allowing War Rig to work with any LLM backend (OpenRouter, Azure, local
models, custom APIs) without coupling to specific SDKs.

The core abstractions are:
- Message: A single message in a conversation (system/user/assistant)
- CompletionResponse: Response from an LLM completion request
- LLMProvider: Protocol defining the interface all providers must implement

Configuration:
- ProviderConfig: Base configuration for any LLM provider
- OpenRouterConfig: Configuration specific to OpenRouter

Implementations:
- OpenRouterProvider: OpenRouter implementation using the OpenAI SDK

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

Example:
    from war_rig.providers import OpenRouterProvider, OpenRouterConfig, Message

    config = OpenRouterConfig(
        api_key="sk-or-...",
        default_model="anthropic/claude-sonnet-4-20250514",
    )
    provider = OpenRouterProvider(api_key=config.api_key)

    messages = [
        Message(role="system", content="You are a helpful assistant."),
        Message(role="user", content="Hello!"),
    ]
    response = await provider.complete(messages)
    print(response.content)

    # Or use the factory
    from war_rig.providers import get_provider_from_env
    provider = get_provider_from_env()
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
