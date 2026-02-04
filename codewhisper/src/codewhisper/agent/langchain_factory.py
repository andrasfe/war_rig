"""LangChain model factory for CodeWhisper.

This module provides factory functions for creating LangChain chat models
using the war_rig.providers package for environment-based configuration.

It delegates ALL provider and API key resolution to war_rig.providers, ensuring
100% identical behavior with war_rig's provider system. The war_rig.providers
package handles:
- Reading LLM_PROVIDER from environment (default: "openrouter")
- Looking up the correct API key for that provider
- Creating properly configured providers
- Plugin discovery for custom providers

This module extracts the configuration from war_rig.providers and creates
corresponding LangChain models.

IMPORTANT: This module uses EXACTLY the same provider logic as war_rig:
- Built-in provider: "openrouter" (uses OPENROUTER_API_KEY)
- Custom providers: Register via war_rig.providers plugin system
- See war_rig/CUSTOM_LLM_PROVIDER.md for adding custom providers

If you set LLM_PROVIDER to something other than "openrouter", you must
have a corresponding provider plugin installed.

Example:
    from codewhisper.agent.langchain_factory import get_langchain_model

    # Create model from environment (uses LLM_PROVIDER, defaults to openrouter)
    llm = get_langchain_model()

    # Override model while using configured provider
    llm = get_langchain_model(model="anthropic/claude-3-opus-20240229")
"""

from __future__ import annotations

import logging
import os
from typing import TYPE_CHECKING, Any

from dotenv import load_dotenv

# Load .env file from current directory or parent directories
load_dotenv()

if TYPE_CHECKING:
    from langchain_core.language_models import BaseChatModel

logger = logging.getLogger(__name__)


def _get_provider_info(
    provider_name: str | None = None,
) -> tuple[str, str, str | None, dict[str, Any]]:
    """Get provider info from war_rig.providers.

    This function uses EXACTLY the same logic as war_rig.providers.get_provider_from_env().
    It does NOT have any fallback handling - if war_rig doesn't know the provider,
    an error is raised (consistent with war_rig behavior).

    Provider support:
    - "openrouter" (default): Built-in, uses OPENROUTER_API_KEY
    - Custom providers: Must be registered via war_rig.providers plugin system

    Args:
        provider_name: Optional provider name override. If None, reads from
            LLM_PROVIDER environment variable (default: "openrouter").

    Returns:
        Tuple of (provider_type, api_key, base_url, extra_config).

    Raises:
        KeyError: If required API key environment variable is not set.
        ValueError: If provider is not registered in war_rig.providers.
            To add custom providers, see war_rig/CUSTOM_LLM_PROVIDER.md.
    """
    from war_rig.providers import get_provider_from_env

    # Temporarily set LLM_PROVIDER if overridden
    original_provider = os.environ.get("LLM_PROVIDER")
    if provider_name:
        os.environ["LLM_PROVIDER"] = provider_name

    try:
        # Delegate ENTIRELY to war_rig.providers - same logic as war_rig uses
        provider = get_provider_from_env()

        # Extract info from war_rig provider
        api_key = provider._api_key
        provider_class_name = type(provider).__name__.lower()

        base_url: str | None = None
        if hasattr(provider, "_base_url"):
            base_url = provider._base_url

        default_model: str | None = None
        if hasattr(provider, "_default_model"):
            default_model = provider._default_model

        extra_config: dict[str, Any] = {}
        if default_model:
            extra_config["default_model"] = default_model

        # Determine LangChain model type based on provider class
        # This mapping converts war_rig providers to LangChain model types
        if "anthropic" in provider_class_name:
            provider_type = "anthropic"
        elif "openrouter" in provider_class_name:
            provider_type = "openai_compatible"
            extra_config["base_url"] = base_url or "https://openrouter.ai/api/v1"
        elif "openai" in provider_class_name:
            if base_url and "api.openai.com" not in base_url:
                provider_type = "openai_compatible"
                extra_config["base_url"] = base_url
            else:
                provider_type = "openai"
        else:
            # For any other provider (plugins), assume OpenAI-compatible API
            # This is the most common pattern for custom LLM providers
            provider_type = "openai_compatible"
            if base_url:
                extra_config["base_url"] = base_url

        return provider_type, api_key, base_url, extra_config

    finally:
        # Restore original LLM_PROVIDER
        if provider_name:
            if original_provider is not None:
                os.environ["LLM_PROVIDER"] = original_provider
            else:
                os.environ.pop("LLM_PROVIDER", None)


def _create_anthropic_model(
    api_key: str,
    model: str | None = None,
    temperature: float = 0.3,
    max_tokens: int = 4096,
    default_model: str | None = None,
    **kwargs: Any,
) -> BaseChatModel:
    """Create an Anthropic-backed LangChain model.

    Args:
        api_key: Anthropic API key.
        model: Model identifier (e.g., "claude-3-opus-20240229")
        temperature: Sampling temperature
        max_tokens: Maximum tokens in response
        default_model: Default model from war_rig.providers config
        **kwargs: Additional arguments passed to ChatAnthropic

    Returns:
        Configured ChatAnthropic instance
    """
    from langchain_anthropic import ChatAnthropic

    # Model precedence: explicit param > IMPERATOR_MODEL > LLM_DEFAULT_MODEL > default
    resolved_model = model or os.environ.get(
        "IMPERATOR_MODEL",
        os.environ.get(
            "LLM_DEFAULT_MODEL", default_model or "claude-sonnet-4-20250514"
        ),
    )

    return ChatAnthropic(  # type: ignore[call-arg, arg-type]
        model=resolved_model,
        temperature=temperature,
        max_tokens=max_tokens,
        api_key=api_key,
        **kwargs,
    )


def _create_openai_model(
    api_key: str,
    model: str | None = None,
    temperature: float = 0.3,
    max_tokens: int = 4096,
    default_model: str | None = None,
    **kwargs: Any,
) -> BaseChatModel:
    """Create an OpenAI-backed LangChain model.

    Args:
        api_key: OpenAI API key.
        model: Model identifier (e.g., "gpt-4o")
        temperature: Sampling temperature
        max_tokens: Maximum tokens in response
        default_model: Default model from war_rig.providers config
        **kwargs: Additional arguments passed to ChatOpenAI

    Returns:
        Configured ChatOpenAI instance
    """
    from langchain_openai import ChatOpenAI

    # Model precedence: explicit param > IMPERATOR_MODEL > LLM_DEFAULT_MODEL > default
    resolved_model = model or os.environ.get(
        "IMPERATOR_MODEL",
        os.environ.get("LLM_DEFAULT_MODEL", default_model or "gpt-4o"),
    )

    return ChatOpenAI(  # type: ignore[call-arg, arg-type]
        model=resolved_model,
        temperature=temperature,
        max_tokens=max_tokens,
        api_key=api_key,
        **kwargs,
    )


def _create_openai_compatible_model(
    api_key: str,
    base_url: str,
    model: str | None = None,
    temperature: float = 0.3,
    max_tokens: int = 4096,
    default_model: str | None = None,
    **kwargs: Any,
) -> BaseChatModel:
    """Create an OpenAI-compatible LangChain model.

    This is used for OpenRouter, and any other OpenAI-compatible API.

    Args:
        api_key: API key for the service.
        base_url: Base URL for the API endpoint.
        model: Model identifier
        temperature: Sampling temperature
        max_tokens: Maximum tokens in response
        default_model: Default model from war_rig.providers config
        **kwargs: Additional arguments passed to ChatOpenAI

    Returns:
        Configured ChatOpenAI instance
    """
    from langchain_openai import ChatOpenAI

    # Model precedence: explicit param > IMPERATOR_MODEL > LLM_DEFAULT_MODEL > default
    resolved_model = model or os.environ.get(
        "IMPERATOR_MODEL",
        os.environ.get(
            "LLM_DEFAULT_MODEL", default_model or "anthropic/claude-sonnet-4-20250514"
        ),
    )

    return ChatOpenAI(  # type: ignore[call-arg, arg-type]
        model=resolved_model,
        temperature=temperature,
        max_tokens=max_tokens,
        base_url=base_url,
        api_key=api_key,
        **kwargs,
    )


def get_available_providers() -> list[str]:
    """Get list of all available provider names.

    Delegates to war_rig.providers for the authoritative list.

    Returns:
        List of provider names.
    """
    from war_rig.providers import get_available_providers as llm_get_providers

    return list(llm_get_providers())


def get_langchain_model(
    provider: str | None = None,
    model: str | None = None,
    temperature: float = 0.3,
    max_tokens: int = 4096,
    **kwargs: Any,
) -> BaseChatModel:
    """Create a LangChain model based on provider configuration.

    Uses EXACTLY the same provider logic as war_rig:
    - Delegates ALL provider resolution to war_rig.providers.get_provider_from_env()
    - Built-in provider: "openrouter" (default, uses OPENROUTER_API_KEY)
    - Custom providers: Must be registered via war_rig.providers plugin system

    This ensures 100% identical behavior with war_rig's provider handling.
    If you need a custom provider, see war_rig/CUSTOM_LLM_PROVIDER.md for
    instructions on creating and registering provider plugins.

    Environment variables:
    - LLM_PROVIDER: Provider name (default: "openrouter")
    - OPENROUTER_API_KEY: Required when using openrouter provider
    - LLM_DEFAULT_MODEL: Default model override
    - IMPERATOR_MODEL: Model override (takes precedence over LLM_DEFAULT_MODEL)

    Args:
        provider: Provider name override. If None, reads from LLM_PROVIDER env var.
            Must be a provider registered in war_rig.providers (default: "openrouter").
        model: Model identifier override. If None, uses provider-specific default.
        temperature: Sampling temperature.
        max_tokens: Maximum tokens in response.
        **kwargs: Additional provider-specific arguments.

    Returns:
        Configured BaseChatModel instance.

    Raises:
        KeyError: If required API key environment variable is not set.
        ValueError: If provider is not registered in war_rig.providers.
    """
    # Delegate to war_rig.providers for provider resolution and API key lookup
    # This is the ONLY place we get API keys from - never directly from env
    provider_type, api_key, base_url, extra_config = _get_provider_info(
        provider_name=provider
    )

    logger.debug(f"Creating LangChain model: type={provider_type}, base_url={base_url}")

    # Create the appropriate LangChain model
    if provider_type == "anthropic":
        return _create_anthropic_model(
            api_key=api_key,
            model=model,
            temperature=temperature,
            max_tokens=max_tokens,
            default_model=extra_config.get("default_model"),
            **kwargs,
        )
    elif provider_type == "openai":
        return _create_openai_model(
            api_key=api_key,
            model=model,
            temperature=temperature,
            max_tokens=max_tokens,
            default_model=extra_config.get("default_model"),
            **kwargs,
        )
    else:  # openai_compatible (OpenRouter, plugins, custom endpoints)
        return _create_openai_compatible_model(
            api_key=api_key,
            base_url=extra_config.get(
                "base_url", base_url or "https://openrouter.ai/api/v1"
            ),
            model=model,
            temperature=temperature,
            max_tokens=max_tokens,
            default_model=extra_config.get("default_model"),
            **kwargs,
        )
