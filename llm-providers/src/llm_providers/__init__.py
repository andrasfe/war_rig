"""LLM Providers - Provider-agnostic LLM abstraction layer.

This package provides a unified interface for interacting with various LLM
providers (OpenRouter, Anthropic, OpenAI) through a common Protocol-based
abstraction.

Core Abstractions:
    - Message: A single message in a conversation (system/user/assistant)
    - CompletionResponse: Response from an LLM completion request
    - LLMProvider: Protocol defining the interface all providers must implement

Configuration:
    - ProviderConfig: Base configuration for any LLM provider
    - OpenRouterConfig: Configuration specific to OpenRouter
    - AnthropicConfig: Configuration specific to Anthropic direct API
    - OpenAIConfig: Configuration specific to OpenAI direct API

Implementations:
    - OpenRouterProvider: OpenRouter implementation using the OpenAI SDK
    - AnthropicProvider: Direct Anthropic API implementation
    - OpenAIProvider: Direct OpenAI API implementation

Factory Functions:
    - create_provider: Create a provider by name with explicit configuration
    - register_provider: Register a custom provider class
    - get_provider_from_env: Create a provider from environment variables
    - get_available_providers: List all registered providers

Example:
    from llm_providers import create_provider, Message

    provider = create_provider(
        "openrouter",
        api_key="sk-or-...",
        default_model="anthropic/claude-sonnet-4-20250514",
    )

    messages = [
        Message(role="system", content="You are a helpful assistant."),
        Message(role="user", content="Hello!"),
    ]
    response = await provider.complete(messages)
    print(response.content)
"""

from llm_providers.config import (
    AnthropicConfig,
    OpenAIConfig,
    OpenRouterConfig,
    ProviderConfig,
)
from llm_providers.factory import (
    create_provider,
    get_available_providers,
    get_provider_from_env,
    register_provider,
)
from llm_providers.protocol import (
    CompletionResponse,
    LLMProvider,
    Message,
)
from llm_providers.providers.anthropic import AnthropicProvider
from llm_providers.providers.openai import OpenAIProvider
from llm_providers.providers.openrouter import OpenRouterProvider

__all__ = [
    # Protocol and data types
    "CompletionResponse",
    "LLMProvider",
    "Message",
    # Configuration
    "AnthropicConfig",
    "OpenAIConfig",
    "OpenRouterConfig",
    "ProviderConfig",
    # Providers
    "AnthropicProvider",
    "OpenAIProvider",
    "OpenRouterProvider",
    # Factory functions
    "create_provider",
    "get_available_providers",
    "get_provider_from_env",
    "register_provider",
]

__version__ = "0.1.0"
