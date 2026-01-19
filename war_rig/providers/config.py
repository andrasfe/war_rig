"""Provider configuration schemas for War Rig LLM providers.

This module defines Pydantic models for configuring LLM providers,
providing type-safe configuration with validation and sensible defaults.

Typical usage:
    from war_rig.providers.config import OpenRouterConfig

    config = OpenRouterConfig(
        api_key="sk-or-v1-...",
        default_model="anthropic/claude-sonnet-4-20250514",
    )
"""

from pydantic import BaseModel, Field


class ProviderConfig(BaseModel):
    """Base configuration for any LLM provider.

    This serves as the abstract base for all provider-specific configurations,
    defining common settings that apply across different LLM backends.

    Attributes:
        provider_type: Identifier for the provider (e.g., "openrouter", "anthropic").
        default_model: The model to use when not explicitly specified.
        temperature: Sampling temperature for generation (0.0 = deterministic, 2.0 = creative).
        max_tokens: Maximum tokens in response, None for provider default.
    """

    provider_type: str = Field(
        default="openrouter",
        description="Provider name",
    )
    default_model: str = Field(
        ...,
        description="Default model to use",
    )
    temperature: float = Field(
        default=0.7,
        ge=0.0,
        le=2.0,
        description="Sampling temperature for generation",
    )
    max_tokens: int | None = Field(
        default=None,
        description="Maximum tokens in response",
    )


class OpenRouterConfig(ProviderConfig):
    """Configuration for OpenRouter provider.

    OpenRouter provides a unified API for accessing multiple LLM providers
    (Anthropic, OpenAI, Meta, Google, etc.) through a single endpoint.

    Attributes:
        provider_type: Always "openrouter" for this config.
        api_key: OpenRouter API key (required).
        base_url: OpenRouter API endpoint URL.
        site_url: Optional URL for your site (used in OpenRouter rankings).
        site_name: Optional name for your site (used in OpenRouter rankings).

    Example:
        config = OpenRouterConfig(
            api_key="sk-or-v1-...",
            default_model="anthropic/claude-sonnet-4-20250514",
            site_name="War Rig",
        )
    """

    provider_type: str = Field(
        default="openrouter",
        description="Provider name",
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
