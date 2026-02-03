"""Provider configuration models.

This module defines Pydantic models for configuring LLM providers,
providing type-safe configuration with validation and sensible defaults.

All configuration models support loading from environment variables
using pydantic-settings.

Example:
    from llm_providers.config import OpenRouterConfig

    # Direct instantiation
    config = OpenRouterConfig(
        api_key="sk-or-v1-...",
        default_model="anthropic/claude-sonnet-4-20250514",
    )

    # Or from environment (with LLM_PROVIDERS_ prefix)
    config = OpenRouterConfig()  # Reads LLM_PROVIDERS_API_KEY, etc.
"""

from pydantic import BaseModel, Field


class ProviderConfig(BaseModel):
    """Base configuration for any LLM provider.

    This serves as the base for all provider-specific configurations,
    defining common settings that apply across different LLM backends.

    Attributes:
        provider_type: Identifier for the provider type.
        default_model: The model to use when not explicitly specified.
        temperature: Sampling temperature for generation.
        timeout_seconds: Request timeout in seconds.
    """

    provider_type: str = Field(
        default="base",
        description="Provider type identifier",
    )
    default_model: str = Field(
        ...,
        description="Default model to use for completions",
    )
    temperature: float = Field(
        default=0.7,
        ge=0.0,
        le=2.0,
        description="Sampling temperature (0.0 = deterministic, 2.0 = creative)",
    )
    timeout_seconds: float = Field(
        default=600.0,
        gt=0.0,
        description="Request timeout in seconds",
    )


class OpenRouterConfig(ProviderConfig):
    """Configuration for OpenRouter provider.

    OpenRouter provides a unified API for accessing multiple LLM providers
    (Anthropic, OpenAI, Google, etc.) through a single endpoint.

    Attributes:
        api_key: OpenRouter API key (required).
        base_url: OpenRouter API endpoint URL.
        site_url: Optional URL for your site (used in OpenRouter rankings).
        site_name: Optional name for your site (used in OpenRouter rankings).

    Example:
        config = OpenRouterConfig(
            api_key="sk-or-v1-...",
            default_model="anthropic/claude-sonnet-4-20250514",
            site_name="My App",
        )
    """

    provider_type: str = Field(
        default="openrouter",
        description="Provider type identifier",
    )
    default_model: str = Field(
        default="anthropic/claude-sonnet-4-20250514",
        description="Default model to use for completions",
    )
    api_key: str = Field(
        ...,
        description="OpenRouter API key",
    )
    base_url: str = Field(
        default="https://openrouter.ai/api/v1",
        description="OpenRouter API base URL",
    )
    site_url: str | None = Field(
        default=None,
        description="Site URL for OpenRouter rankings",
    )
    site_name: str | None = Field(
        default=None,
        description="Site name for OpenRouter rankings",
    )


class AnthropicConfig(ProviderConfig):
    """Configuration for direct Anthropic API access.

    Uses the Anthropic Python SDK for direct API access.

    Attributes:
        api_key: Anthropic API key (required).
        base_url: Optional custom API base URL.
        max_tokens: Maximum tokens in the response (Anthropic requires this).

    Example:
        config = AnthropicConfig(
            api_key="sk-ant-...",
            default_model="claude-sonnet-4-20250514",
            max_tokens=4096,
        )
    """

    provider_type: str = Field(
        default="anthropic",
        description="Provider type identifier",
    )
    default_model: str = Field(
        default="claude-sonnet-4-20250514",
        description="Default model to use for completions",
    )
    api_key: str = Field(
        ...,
        description="Anthropic API key",
    )
    base_url: str | None = Field(
        default=None,
        description="Custom API base URL (optional)",
    )
    max_tokens: int = Field(
        default=4096,
        gt=0,
        description="Maximum tokens in the response",
    )


class OpenAIConfig(ProviderConfig):
    """Configuration for direct OpenAI API access.

    Uses the OpenAI Python SDK for direct API access.

    Attributes:
        api_key: OpenAI API key (required).
        base_url: Optional custom API base URL (for Azure or proxies).
        organization: Optional OpenAI organization ID.

    Example:
        config = OpenAIConfig(
            api_key="sk-...",
            default_model="gpt-4o",
        )
    """

    provider_type: str = Field(
        default="openai",
        description="Provider type identifier",
    )
    default_model: str = Field(
        default="gpt-4o",
        description="Default model to use for completions",
    )
    api_key: str = Field(
        ...,
        description="OpenAI API key",
    )
    base_url: str | None = Field(
        default=None,
        description="Custom API base URL (optional, for Azure or proxies)",
    )
    organization: str | None = Field(
        default=None,
        description="OpenAI organization ID",
    )
