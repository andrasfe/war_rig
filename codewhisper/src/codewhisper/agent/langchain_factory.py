"""LangChain model factory for CodeWhisper.

This module provides factory functions for creating LangChain chat models
using the llm_providers package for environment-based configuration.

It reads LLM_PROVIDER from environment (via llm_providers) and creates
appropriate LangChain models with the correct API keys. Supports the same
provider discovery pattern as war_rig.

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


def _get_provider_config() -> tuple[str, str]:
    """Get provider name and API key using llm_providers logic.

    This function uses the same environment variable resolution as
    llm_providers.get_provider_from_env() to determine which provider
    and API key to use.

    Returns:
        Tuple of (provider_name, api_key).

    Raises:
        KeyError: If required API key environment variable is not set.
    """
    # Get provider name - same logic as llm_providers.factory
    provider = os.getenv("LLM_PROVIDER", "openrouter").lower()

    # Get API key based on provider
    if provider == "openrouter":
        api_key = os.environ.get("OPENROUTER_API_KEY", "")
        if not api_key:
            raise KeyError(
                "OPENROUTER_API_KEY environment variable not set. "
                "Required for openrouter provider (LLM_PROVIDER=openrouter or not set)."
            )
    elif provider == "anthropic":
        api_key = os.environ.get("ANTHROPIC_API_KEY", "")
        if not api_key:
            raise KeyError(
                "ANTHROPIC_API_KEY environment variable not set. "
                "Required for anthropic provider (LLM_PROVIDER=anthropic)."
            )
    elif provider == "openai":
        api_key = os.environ.get("OPENAI_API_KEY", "")
        if not api_key:
            raise KeyError(
                "OPENAI_API_KEY environment variable not set. "
                "Required for openai provider (LLM_PROVIDER=openai)."
            )
    else:
        # For unknown providers, try to find an API key with the provider name
        # e.g., LLM_PROVIDER=azure -> look for AZURE_API_KEY
        env_key = f"{provider.upper()}_API_KEY"
        api_key = os.environ.get(env_key, "")
        if not api_key:
            # Fall back to OPENROUTER_API_KEY for routing through OpenRouter
            api_key = os.environ.get("OPENROUTER_API_KEY", "")
            if api_key:
                logger.info(f"Unknown provider '{provider}', using OpenRouter")
                return "openrouter", api_key
            raise KeyError(
                f"No API key found for provider '{provider}'. "
                f"Set {env_key} or OPENROUTER_API_KEY environment variable."
            )

    return provider, api_key


def _create_openrouter_model(
    api_key: str,
    model: str | None = None,
    temperature: float = 0.3,
    max_tokens: int = 4096,
    **kwargs: Any,
) -> BaseChatModel:
    """Create an OpenRouter-backed LangChain model.

    Args:
        api_key: OpenRouter API key.
        model: Model identifier (e.g., "anthropic/claude-sonnet-4-20250514")
        temperature: Sampling temperature
        max_tokens: Maximum tokens in response
        **kwargs: Additional arguments passed to ChatOpenAI

    Returns:
        Configured ChatOpenAI instance
    """
    from langchain_openai import ChatOpenAI

    model = model or os.environ.get(
        "IMPERATOR_MODEL",
        os.environ.get("LLM_DEFAULT_MODEL", "anthropic/claude-sonnet-4-20250514"),
    )

    return ChatOpenAI(  # type: ignore[call-arg, arg-type]
        model=model,
        temperature=temperature,
        max_tokens=max_tokens,
        base_url=os.environ.get("OPENROUTER_BASE_URL", "https://openrouter.ai/api/v1"),
        api_key=api_key,
        **kwargs,
    )


def _create_anthropic_model(
    api_key: str,
    model: str | None = None,
    temperature: float = 0.3,
    max_tokens: int = 4096,
    **kwargs: Any,
) -> BaseChatModel:
    """Create an Anthropic-backed LangChain model.

    Args:
        api_key: Anthropic API key.
        model: Model identifier (e.g., "claude-3-opus-20240229")
        temperature: Sampling temperature
        max_tokens: Maximum tokens in response
        **kwargs: Additional arguments passed to ChatAnthropic

    Returns:
        Configured ChatAnthropic instance
    """
    from langchain_anthropic import ChatAnthropic

    model = model or os.environ.get(
        "IMPERATOR_MODEL",
        os.environ.get("LLM_DEFAULT_MODEL", "claude-sonnet-4-20250514"),
    )

    return ChatAnthropic(  # type: ignore[call-arg, arg-type]
        model=model,
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
    **kwargs: Any,
) -> BaseChatModel:
    """Create an OpenAI-backed LangChain model.

    Args:
        api_key: OpenAI API key.
        model: Model identifier (e.g., "gpt-4o")
        temperature: Sampling temperature
        max_tokens: Maximum tokens in response
        **kwargs: Additional arguments passed to ChatOpenAI

    Returns:
        Configured ChatOpenAI instance
    """
    from langchain_openai import ChatOpenAI

    model = model or os.environ.get(
        "IMPERATOR_MODEL",
        os.environ.get("LLM_DEFAULT_MODEL", "gpt-4o"),
    )

    return ChatOpenAI(  # type: ignore[call-arg, arg-type]
        model=model,
        temperature=temperature,
        max_tokens=max_tokens,
        api_key=api_key,
        **kwargs,
    )


def get_available_providers() -> list[str]:
    """Get list of all available provider names.

    Returns:
        List of provider names.
    """
    return ["openrouter", "anthropic", "openai"]


def get_langchain_model(
    provider: str | None = None,
    model: str | None = None,
    temperature: float = 0.3,
    max_tokens: int = 4096,
    **kwargs: Any,
) -> BaseChatModel:
    """Create a LangChain model based on provider configuration.

    Uses the same environment-based provider discovery as llm_providers:
    - Reads LLM_PROVIDER to determine which provider to use
    - Uses the appropriate API key for that provider
    - Falls back to openrouter if LLM_PROVIDER is not set

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
    # If provider is explicitly specified, use it; otherwise get from env
    if provider:
        resolved_provider = provider.lower()
        # Get the API key for the explicitly specified provider
        if resolved_provider == "openrouter":
            api_key = os.environ.get("OPENROUTER_API_KEY", "")
            if not api_key:
                raise KeyError(
                    "OPENROUTER_API_KEY environment variable not set. "
                    "Required for openrouter provider."
                )
        elif resolved_provider == "anthropic":
            api_key = os.environ.get("ANTHROPIC_API_KEY", "")
            if not api_key:
                raise KeyError(
                    "ANTHROPIC_API_KEY environment variable not set. "
                    "Required for anthropic provider."
                )
        elif resolved_provider == "openai":
            api_key = os.environ.get("OPENAI_API_KEY", "")
            if not api_key:
                raise KeyError(
                    "OPENAI_API_KEY environment variable not set. "
                    "Required for openai provider."
                )
        else:
            # For unknown providers, try provider-specific key or fall back to OpenRouter
            env_key = f"{resolved_provider.upper()}_API_KEY"
            api_key = os.environ.get(env_key, "")
            if not api_key:
                api_key = os.environ.get("OPENROUTER_API_KEY", "")
                if api_key:
                    logger.info(f"Unknown provider '{provider}', using OpenRouter")
                    resolved_provider = "openrouter"
                else:
                    raise KeyError(
                        f"No API key found for provider '{provider}'. "
                        f"Set {env_key} or OPENROUTER_API_KEY environment variable."
                    )
    else:
        # Use llm_providers-style resolution from LLM_PROVIDER env var
        resolved_provider, api_key = _get_provider_config()

    logger.debug(f"Creating LangChain model for provider: {resolved_provider}")

    # Create the appropriate model
    if resolved_provider == "openrouter":
        return _create_openrouter_model(
            api_key=api_key,
            model=model,
            temperature=temperature,
            max_tokens=max_tokens,
            **kwargs,
        )
    elif resolved_provider == "anthropic":
        return _create_anthropic_model(
            api_key=api_key,
            model=model,
            temperature=temperature,
            max_tokens=max_tokens,
            **kwargs,
        )
    elif resolved_provider == "openai":
        return _create_openai_model(
            api_key=api_key,
            model=model,
            temperature=temperature,
            max_tokens=max_tokens,
            **kwargs,
        )
    else:
        # For other providers with their own API key, try OpenAI-compatible API
        # Many providers (Azure, local LLMs, etc.) use OpenAI-compatible APIs
        logger.info(f"Using OpenAI-compatible API for provider: {resolved_provider}")
        base_url = os.environ.get(f"{resolved_provider.upper()}_BASE_URL")
        return _create_openai_model(
            api_key=api_key,
            model=model,
            temperature=temperature,
            max_tokens=max_tokens,
            base_url=base_url,
            **kwargs,
        )
