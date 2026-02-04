"""LangChain model factory for CodeWhisper.

This module provides factory functions for creating LangChain chat models
using the llm_providers package for environment-based configuration.

It delegates ALL provider and API key resolution to llm_providers, avoiding
duplication of configuration logic. The llm_providers package handles:
- Reading LLM_PROVIDER from environment
- Looking up the correct API key (ANTHROPIC_API_KEY, OPENAI_API_KEY, etc.)
- Creating properly configured providers

This module extracts the configuration from llm_providers and creates
corresponding LangChain models.

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


def _get_provider_info_from_llm_providers(
    provider_name: str | None = None,
) -> tuple[str, str, dict[str, Any]]:
    """Get provider name, API key, and config by delegating to llm_providers.

    This function uses llm_providers.get_provider_from_env() to handle all
    provider/API key resolution, then extracts the configuration needed
    to create a corresponding LangChain model.

    Args:
        provider_name: Optional provider name override. If None, reads from
            LLM_PROVIDER environment variable (via llm_providers).

    Returns:
        Tuple of (provider_name, api_key, extra_config).

    Raises:
        KeyError: If required API key environment variable is not set.
        ValueError: If provider is not supported by llm_providers.
    """
    from llm_providers import get_provider_from_env

    # Get provider from llm_providers - it handles all env resolution
    provider = get_provider_from_env(provider_name=provider_name)

    # Determine the provider type from the class name
    provider_class_name = type(provider).__name__.lower()

    if "openrouter" in provider_class_name:
        resolved_name = "openrouter"
        api_key = provider._api_key
        extra_config = {
            "base_url": provider._base_url,
            "default_model": provider._default_model,
        }
    elif "anthropic" in provider_class_name:
        resolved_name = "anthropic"
        api_key = provider._api_key
        extra_config = {
            "default_model": provider._default_model,
            "max_tokens": provider._max_tokens,
        }
    elif "openai" in provider_class_name:
        resolved_name = "openai"
        api_key = provider._api_key
        extra_config = {
            "default_model": provider._default_model,
        }
        # Include base_url if set (for custom OpenAI-compatible endpoints)
        if hasattr(provider, "_client") and provider._client.base_url:
            base_url_str = str(provider._client.base_url)
            # Only set if it's not the default OpenAI URL
            if "api.openai.com" not in base_url_str:
                extra_config["base_url"] = base_url_str
    else:
        # Unknown provider type - this shouldn't happen with registered providers
        raise ValueError(
            f"Unknown provider type: {provider_class_name}. "
            "Cannot extract configuration for LangChain model."
        )

    return resolved_name, api_key, extra_config


def _create_openrouter_model(
    api_key: str,
    model: str | None = None,
    temperature: float = 0.3,
    max_tokens: int = 4096,
    base_url: str | None = None,
    default_model: str | None = None,
    **kwargs: Any,
) -> BaseChatModel:
    """Create an OpenRouter-backed LangChain model.

    Args:
        api_key: OpenRouter API key.
        model: Model identifier (e.g., "anthropic/claude-sonnet-4-20250514")
        temperature: Sampling temperature
        max_tokens: Maximum tokens in response
        base_url: OpenRouter base URL
        default_model: Default model from llm_providers config
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

    resolved_base_url = base_url or os.environ.get(
        "OPENROUTER_BASE_URL", "https://openrouter.ai/api/v1"
    )

    return ChatOpenAI(  # type: ignore[call-arg, arg-type]
        model=resolved_model,
        temperature=temperature,
        max_tokens=max_tokens,
        base_url=resolved_base_url,
        api_key=api_key,
        **kwargs,
    )


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
        default_model: Default model from llm_providers config
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
    base_url: str | None = None,
    default_model: str | None = None,
    **kwargs: Any,
) -> BaseChatModel:
    """Create an OpenAI-backed LangChain model.

    Args:
        api_key: OpenAI API key.
        model: Model identifier (e.g., "gpt-4o")
        temperature: Sampling temperature
        max_tokens: Maximum tokens in response
        base_url: Optional custom base URL (for OpenAI-compatible APIs)
        default_model: Default model from llm_providers config
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

    # Only pass base_url if explicitly set (not None)
    chat_kwargs: dict[str, Any] = {
        "model": resolved_model,
        "temperature": temperature,
        "max_tokens": max_tokens,
        "api_key": api_key,
        **kwargs,
    }
    if base_url:
        chat_kwargs["base_url"] = base_url

    return ChatOpenAI(**chat_kwargs)  # type: ignore[call-arg, arg-type]


def get_available_providers() -> list[str]:
    """Get list of all available provider names.

    Delegates to llm_providers for the authoritative list.

    Returns:
        List of provider names.
    """
    from llm_providers import get_available_providers as llm_get_providers

    return list(llm_get_providers())


def get_langchain_model(
    provider: str | None = None,
    model: str | None = None,
    temperature: float = 0.3,
    max_tokens: int = 4096,
    **kwargs: Any,
) -> BaseChatModel:
    """Create a LangChain model based on provider configuration.

    Delegates ALL provider and API key resolution to llm_providers package.
    This ensures consistent behavior with other war_rig components.

    The llm_providers package handles:
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
    # Delegate to llm_providers for provider resolution and API key lookup
    resolved_provider, api_key, extra_config = _get_provider_info_from_llm_providers(
        provider_name=provider
    )

    logger.debug(f"Creating LangChain model for provider: {resolved_provider}")

    # Create the appropriate LangChain model
    if resolved_provider == "openrouter":
        return _create_openrouter_model(
            api_key=api_key,
            model=model,
            temperature=temperature,
            max_tokens=max_tokens,
            base_url=extra_config.get("base_url"),
            default_model=extra_config.get("default_model"),
            **kwargs,
        )
    elif resolved_provider == "anthropic":
        return _create_anthropic_model(
            api_key=api_key,
            model=model,
            temperature=temperature,
            max_tokens=max_tokens,
            default_model=extra_config.get("default_model"),
            **kwargs,
        )
    elif resolved_provider == "openai":
        return _create_openai_model(
            api_key=api_key,
            model=model,
            temperature=temperature,
            max_tokens=max_tokens,
            base_url=extra_config.get("base_url"),
            default_model=extra_config.get("default_model"),
            **kwargs,
        )
    else:
        # For other providers registered via llm_providers plugins,
        # try to use them as OpenAI-compatible
        logger.info(f"Using OpenAI-compatible API for provider: {resolved_provider}")
        return _create_openai_model(
            api_key=api_key,
            model=model,
            temperature=temperature,
            max_tokens=max_tokens,
            base_url=extra_config.get("base_url"),
            default_model=extra_config.get("default_model"),
            **kwargs,
        )
