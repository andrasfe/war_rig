"""LangChain model factory for CodeWhisper.

This module provides factory functions for creating LangChain chat models
using war_rig.providers for environment-based configuration.

**Provider-Agnostic Design:**
    This module delegates ALL provider and API key resolution to
    war_rig.providers, avoiding duplication of configuration logic.
    The provider package handles:
    - Reading LLM_PROVIDER from environment
    - Looking up the correct API key for that provider
    - Creating properly configured providers

    This module extracts configuration from the provider and creates
    corresponding LangChain models.

IMPORTANT: This module NEVER checks environment variables for API keys
directly. It always goes through war_rig.providers.get_provider_from_env(),
which handles all provider configuration including API key lookup.

Example:
    from codewhisper.agent.langchain_factory import get_langchain_model

    # Create model from environment (uses LLM_PROVIDER)
    llm = get_langchain_model()

    # Create with specific provider override
    llm = get_langchain_model(provider="myprovider", model="my-model")
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
) -> tuple[str, str, str | None, dict[str, Any]]:
    """Get provider info by delegating entirely to war_rig.providers.

    This function uses war_rig.providers.get_provider_from_env() to handle all
    provider/API key resolution, then extracts the configuration needed
    to create a corresponding LangChain model.

    **Provider-Agnostic:** This function works with ANY registered provider.
    It inspects the provider instance to determine how to create a LangChain
    model, without hardcoding provider-specific logic.

    CRITICAL: We NEVER check environment variables for API keys directly.
    war_rig.providers handles everything - LLM_PROVIDER, API keys, base URLs, etc.

    Args:
        provider_name: Optional provider name override. If None, reads from
            LLM_PROVIDER environment variable (via war_rig.providers).

    Returns:
        Tuple of (provider_type, api_key, base_url, extra_config).
        - provider_type: "langchain_native", "anthropic", "openai", or "openai_compatible"
        - api_key: The API key from the provider (empty for langchain_native)
        - base_url: Base URL if set (for custom endpoints)
        - extra_config: Additional provider-specific config

    Raises:
        KeyError: If required API key environment variable is not set
            (raised by war_rig.providers, not us).
        ValueError: If provider is not supported.
    """
    from war_rig.providers import get_provider_from_env

    # Get provider from llm_providers - it handles all env resolution
    # This is the ONLY place we interact with llm_providers for config
    provider = get_provider_from_env(provider_name=provider_name)

    # Check if the provider has a get_langchain_model method (plugin providers)
    # This allows any provider to supply its own LangChain model without API keys
    if hasattr(provider, "get_langchain_model"):
        default_model: str | None = None
        if hasattr(provider, "_default_model"):
            default_model = provider._default_model
        extra_config: dict[str, Any] = {"provider_instance": provider}
        if default_model:
            extra_config["default_model"] = default_model
        return "langchain_native", "", None, extra_config

    # Extract API key from the provider instance (for API-key-based providers)
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
    default_model = None
    if hasattr(provider, "_default_model"):
        default_model = provider._default_model

    extra_config = {}
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
        if base_url:
            extra_config["base_url"] = base_url
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
        default_model: Default model from provider config
        **kwargs: Additional arguments passed to ChatAnthropic

    Returns:
        Configured ChatAnthropic instance
    """
    from langchain_anthropic import ChatAnthropic

    # Model precedence: explicit param > IMPERATOR_MODEL > LLM_DEFAULT_MODEL > default
    resolved_model = model or os.environ.get(
        "IMPERATOR_MODEL",
        os.environ.get("LLM_DEFAULT_MODEL", default_model),
    )
    if not resolved_model:
        raise ValueError(
            "No model specified. Set LLM_DEFAULT_MODEL or IMPERATOR_MODEL environment variable, "
            "or pass model parameter explicitly."
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
        default_model: Default model from provider config
        **kwargs: Additional arguments passed to ChatOpenAI

    Returns:
        Configured ChatOpenAI instance
    """
    from langchain_openai import ChatOpenAI

    # Model precedence: explicit param > IMPERATOR_MODEL > LLM_DEFAULT_MODEL > default
    resolved_model = model or os.environ.get(
        "IMPERATOR_MODEL",
        os.environ.get("LLM_DEFAULT_MODEL", default_model),
    )
    if not resolved_model:
        raise ValueError(
            "No model specified. Set LLM_DEFAULT_MODEL or IMPERATOR_MODEL environment variable, "
            "or pass model parameter explicitly."
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

    This is used for any provider with an OpenAI-compatible API.

    Args:
        api_key: API key for the service.
        base_url: Base URL for the API endpoint.
        model: Model identifier
        temperature: Sampling temperature
        max_tokens: Maximum tokens in response
        default_model: Default model from provider config
        **kwargs: Additional arguments passed to ChatOpenAI

    Returns:
        Configured ChatOpenAI instance
    """
    from langchain_openai import ChatOpenAI

    # Model precedence: explicit param > IMPERATOR_MODEL > LLM_DEFAULT_MODEL > default
    resolved_model = model or os.environ.get(
        "IMPERATOR_MODEL",
        os.environ.get("LLM_DEFAULT_MODEL", default_model),
    )
    if not resolved_model:
        raise ValueError(
            "No model specified. Set LLM_DEFAULT_MODEL or IMPERATOR_MODEL environment variable, "
            "or pass model parameter explicitly."
        )

    return ChatOpenAI(  # type: ignore[call-arg, arg-type]
        model=resolved_model,
        temperature=temperature,
        max_tokens=max_tokens,
        base_url=base_url,
        api_key=api_key,
        **kwargs,
    )


def _create_langchain_native_model(
    provider_instance: Any,
    model: str | None = None,
    default_model: str | None = None,
    **kwargs: Any,
) -> "BaseChatModel":
    """Create a LangChain model from a provider that supports get_langchain_model.

    This is used for plugin providers that supply their own LangChain models
    (e.g., providers using custom authentication like IDAAS).

    Args:
        provider_instance: The provider instance with get_langchain_model method
        model: Model identifier override
        default_model: Default model from provider config
        **kwargs: Additional arguments (currently unused)

    Returns:
        LangChain model instance from the provider
    """
    # Model precedence: explicit param > IMPERATOR_MODEL > LLM_DEFAULT_MODEL > default
    resolved_model = model or os.environ.get(
        "IMPERATOR_MODEL",
        os.environ.get("LLM_DEFAULT_MODEL", default_model),
    )

    logger.debug(f"Creating LangChain model from provider: {resolved_model}")

    return provider_instance.get_langchain_model(resolved_model)


def get_available_providers() -> list[str]:
    """Get list of all available provider names.

    Delegates to war_rig.providers for the authoritative list.
    Includes both built-in providers and discovered plugins.

    Returns:
        List of provider names.
    """
    from war_rig.providers import get_available_providers as wr_get_providers

    return list(wr_get_providers())


def get_langchain_model(
    provider: str | None = None,
    model: str | None = None,
    temperature: float = 0.3,
    max_tokens: int = 4096,
    **kwargs: Any,
) -> "BaseChatModel":
    """Create a LangChain model based on provider configuration.

    **Provider-Agnostic:** Delegates ALL provider and API key resolution to
    war_rig.providers package. This ensures consistent behavior with other
    war_rig components and works with ANY registered provider.

    IMPORTANT: This function NEVER checks environment variables for API keys
    directly. The provider (determined by LLM_PROVIDER) handles its own
    API key lookup.

    The war_rig.providers package handles:
    - Reading LLM_PROVIDER to determine which provider to use
    - Using the appropriate API key for that provider
    - Applying provider-specific defaults and configuration

    Args:
        provider: Provider name override. If None, reads from LLM_PROVIDER env var.
        model: Model identifier override. If None, uses LLM_DEFAULT_MODEL env var.
        temperature: Sampling temperature.
        max_tokens: Maximum tokens in response.
        **kwargs: Additional provider-specific arguments.

    Returns:
        Configured BaseChatModel instance.

    Raises:
        KeyError: If required API key environment variable is not set.
        ValueError: If provider is not supported or model is not specified.
    """
    # Delegate to llm_providers for provider resolution and API key lookup
    # This is the ONLY place we get API keys from - never directly from env
    provider_type, api_key, base_url, extra_config = _get_provider_info_from_llm_providers(
        provider_name=provider
    )

    logger.debug(f"Creating LangChain model: type={provider_type}, base_url={base_url}")

    # Create the appropriate LangChain model
    if provider_type == "langchain_native":
        # Provider supplies its own LangChain model (e.g., via get_langchain_model)
        return _create_langchain_native_model(
            provider_instance=extra_config.get("provider_instance"),
            model=model,
            default_model=extra_config.get("default_model"),
            **kwargs,
        )
    elif provider_type == "anthropic":
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
    else:  # openai_compatible (plugins, custom endpoints)
        resolved_base_url = extra_config.get("base_url") or base_url
        if not resolved_base_url:
            raise ValueError(
                f"Provider type '{provider_type}' requires a base_url. "
                "Ensure your provider sets _base_url or _client.base_url."
            )
        return _create_openai_compatible_model(
            api_key=api_key,
            base_url=resolved_base_url,
            model=model,
            temperature=temperature,
            max_tokens=max_tokens,
            default_model=extra_config.get("default_model"),
            **kwargs,
        )
