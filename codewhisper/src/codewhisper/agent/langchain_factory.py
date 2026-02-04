"""LangChain model factory for CodeWhisper.

This module provides factory functions for creating LangChain chat models
using the war_rig.providers package for environment-based configuration.

It delegates ALL provider and API key resolution to war_rig.providers, avoiding
duplication of configuration logic. The war_rig.providers package handles:
- Reading LLM_PROVIDER from environment
- Looking up the correct API key for that provider
- Creating properly configured providers

This module extracts the configuration from war_rig.providers and creates
corresponding LangChain models.

IMPORTANT: This module NEVER checks environment variables for API keys
directly. It always goes through war_rig.providers.get_provider_from_env(),
which handles all provider configuration including API key lookup.

Example:
    from codewhisper.agent.langchain_factory import get_langchain_model

    # Create model from environment (uses LLM_PROVIDER)
    llm = get_langchain_model()

    # Create with specific provider
    llm = get_langchain_model(provider="anthropic", model="claude-3-opus-20240229")
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


def _get_provider_info_from_war_rig(
    provider_name: str | None = None,
) -> tuple[str, str, str | None, dict[str, Any]]:
    """Get provider info by delegating entirely to war_rig.providers.

    This function uses war_rig.providers.get_provider_from_env() to handle all
    provider/API key resolution, then extracts the configuration needed
    to create a corresponding LangChain model.

    CRITICAL: We NEVER check environment variables for API keys directly.
    war_rig.providers handles everything - LLM_PROVIDER, API keys, base URLs, etc.

    Args:
        provider_name: Optional provider name override. If provided, temporarily
            sets LLM_PROVIDER env var before calling get_provider_from_env().

    Returns:
        Tuple of (provider_type, api_key, base_url, extra_config).
        - provider_type: "anthropic", "openai", or "openai_compatible"
        - api_key: The API key from the provider
        - base_url: Base URL if set (for OpenRouter or custom endpoints)
        - extra_config: Additional provider-specific config

    Raises:
        KeyError: If required API key environment variable is not set
            (raised by war_rig.providers, not us).
        ValueError: If provider is not supported by war_rig.providers.
    """
    from war_rig.providers import get_provider_from_env

    # If provider_name is specified, temporarily override LLM_PROVIDER
    original_provider = os.environ.get("LLM_PROVIDER")
    if provider_name:
        os.environ["LLM_PROVIDER"] = provider_name

    try:
        # Get provider from war_rig.providers - it handles all env resolution
        # If provider is unknown, fall back to openrouter (war_rig's default)
        try:
            provider = get_provider_from_env()
        except ValueError as e:
            if "Unknown provider" in str(e):
                # Unknown provider - fall back to openrouter
                logger.info(f"Unknown provider, falling back to openrouter: {e}")
                os.environ["LLM_PROVIDER"] = "openrouter"
                provider = get_provider_from_env()
            else:
                raise
    finally:
        # Restore original LLM_PROVIDER
        if provider_name or "LLM_PROVIDER" in os.environ:
            if original_provider is not None:
                os.environ["LLM_PROVIDER"] = original_provider
            elif provider_name:
                os.environ.pop("LLM_PROVIDER", None)

    # Extract API key from the provider instance
    api_key = provider._api_key

    # Determine the provider type from the class name
    provider_class_name = type(provider).__name__.lower()

    # Get base URL if available
    base_url: str | None = None
    if hasattr(provider, "_base_url"):
        base_url = provider._base_url
    elif hasattr(provider, "_client") and hasattr(provider._client, "base_url"):
        base_url = str(provider._client.base_url)

    # Get default model if available
    default_model: str | None = None
    if hasattr(provider, "_default_model"):
        default_model = provider._default_model

    extra_config: dict[str, Any] = {}
    if default_model:
        extra_config["default_model"] = default_model

    # Determine the LangChain model type to use
    if "anthropic" in provider_class_name:
        # Direct Anthropic API
        provider_type = "anthropic"
        if hasattr(provider, "_max_tokens"):
            extra_config["max_tokens"] = provider._max_tokens
    elif "openrouter" in provider_class_name:
        # OpenRouter uses OpenAI-compatible API
        provider_type = "openai_compatible"
        extra_config["base_url"] = base_url or "https://openrouter.ai/api/v1"
    elif "openai" in provider_class_name:
        # Direct OpenAI API or OpenAI-compatible
        if base_url and "api.openai.com" not in base_url:
            provider_type = "openai_compatible"
            extra_config["base_url"] = base_url
        else:
            provider_type = "openai"
    else:
        # For any unknown provider (plugins), assume OpenAI-compatible API
        # This allows ANY provider to work as long as it's OpenAI-compatible
        provider_type = "openai_compatible"
        if base_url:
            extra_config["base_url"] = base_url

    return provider_type, api_key, base_url, extra_config


def _create_anthropic_model(
    api_key: str,
    model: str | None = None,
    temperature: float = 0.3,
    max_tokens: int = 4096,
    default_model: str | None = None,
    **kwargs: Any,
) -> "BaseChatModel":
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
) -> "BaseChatModel":
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
) -> "BaseChatModel":
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
) -> "BaseChatModel":
    """Create a LangChain model based on provider configuration.

    Delegates ALL provider and API key resolution to war_rig.providers package.
    This ensures consistent behavior with other war_rig components.

    IMPORTANT: This function NEVER checks environment variables for API keys
    directly. If LLM_PROVIDER is set, war_rig.providers handles everything.
    You will NEVER see "OPENROUTER_API_KEY not found" errors when using
    a different provider via LLM_PROVIDER.

    The war_rig.providers package handles:
    - Reading LLM_PROVIDER to determine which provider to use
    - Using the appropriate API key for that provider
    - Applying provider-specific defaults and configuration

    Args:
        provider: Provider name override. If None, reads from LLM_PROVIDER env var.
        model: Model identifier override. If None, uses provider-specific default.
        temperature: Sampling temperature.
        max_tokens: Maximum tokens in response.
        **kwargs: Additional provider-specific arguments.

    Returns:
        Configured BaseChatModel instance.

    Raises:
        KeyError: If required API key environment variable is not set.
        ValueError: If provider is not supported.
    """
    # Delegate to war_rig.providers for provider resolution and API key lookup
    # This is the ONLY place we get API keys from - never directly from env
    provider_type, api_key, base_url, extra_config = _get_provider_info_from_war_rig(
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
            base_url=extra_config.get("base_url", base_url or "https://openrouter.ai/api/v1"),
            model=model,
            temperature=temperature,
            max_tokens=max_tokens,
            default_model=extra_config.get("default_model"),
            **kwargs,
        )
