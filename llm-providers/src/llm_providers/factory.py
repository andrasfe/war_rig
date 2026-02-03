"""Provider factory and registry.

This module provides a factory pattern for creating LLM providers by name,
along with a registry for custom provider implementations. It supports
environment-based configuration for easy deployment.

Plugin Discovery:
    External packages can register providers using Python entry points.
    Add to your pyproject.toml:

        [project.entry-points."llm_providers.providers"]
        myprovider = "mypackage.provider:MyProviderClass"

Example:
    from llm_providers import create_provider, get_provider_from_env

    # Create a provider explicitly
    provider = create_provider(
        "openrouter",
        api_key="sk-or-...",
        default_model="anthropic/claude-sonnet-4-20250514",
    )

    # Or create from environment variables
    provider = get_provider_from_env()
"""

from __future__ import annotations

import logging
import os
from importlib.metadata import entry_points
from typing import TYPE_CHECKING, Any, cast

if TYPE_CHECKING:
    from llm_providers.protocol import LLMProvider

logger = logging.getLogger(__name__)

# Registry of provider classes by name
_PROVIDERS: dict[str, type] = {}

# Track if built-in providers have been registered
_BUILTINS_REGISTERED = False

# Track if plugins have been loaded
_PLUGINS_LOADED = False


def _register_builtins() -> None:
    """Register built-in provider implementations.

    This is called lazily to avoid circular imports.
    """
    global _BUILTINS_REGISTERED

    if _BUILTINS_REGISTERED:
        return

    _BUILTINS_REGISTERED = True

    # Import here to avoid circular imports
    from llm_providers.providers.anthropic import AnthropicProvider
    from llm_providers.providers.openai import OpenAIProvider
    from llm_providers.providers.openrouter import OpenRouterProvider

    _PROVIDERS["openrouter"] = OpenRouterProvider
    _PROVIDERS["anthropic"] = AnthropicProvider
    _PROVIDERS["openai"] = OpenAIProvider


def _discover_plugins() -> None:
    """Discover and register provider plugins via entry points.

    Looks for entry points in the 'llm_providers.providers' group.
    Each entry point should point to a provider class.

    External packages register by adding to pyproject.toml:
        [project.entry-points."llm_providers.providers"]
        myprovider = "mypackage.provider:MyProviderClass"
    """
    global _PLUGINS_LOADED

    if _PLUGINS_LOADED:
        return

    _PLUGINS_LOADED = True

    try:
        # Python 3.10+ style
        eps = entry_points(group="llm_providers.providers")
    except TypeError:
        # Python 3.9 fallback - returns a dict-like object
        all_eps = entry_points()
        eps = getattr(all_eps, "get", lambda x, d: d)("llm_providers.providers", [])

    for ep in eps:
        try:
            provider_class = ep.load()
            name = ep.name.lower()

            if name in _PROVIDERS:
                logger.debug(f"Provider '{name}' already registered, skipping plugin")
                continue

            _PROVIDERS[name] = provider_class
            logger.info(f"Discovered provider plugin: {name} -> {ep.value}")

        except Exception as e:
            logger.warning(f"Failed to load provider plugin '{ep.name}': {e}")


def get_available_providers() -> list[str]:
    """Get list of all registered provider names.

    Returns:
        List of provider names (includes built-in and discovered plugins).
    """
    _register_builtins()
    _discover_plugins()
    return list(_PROVIDERS.keys())


def register_provider(name: str, provider_class: type) -> None:
    """Register a custom provider class.

    Args:
        name: The name to register the provider under (e.g., "azure", "local").
        provider_class: The provider class to register. Should implement
            the LLMProvider protocol.

    Example:
        from llm_providers import register_provider

        class MyCustomProvider:
            # ... implements LLMProvider protocol ...
            pass

        register_provider("custom", MyCustomProvider)
    """
    _PROVIDERS[name.lower()] = provider_class


def create_provider(
    provider_name: str,
    **kwargs: Any,
) -> LLMProvider:
    """Create an LLM provider by name.

    Automatically discovers provider plugins on first call.

    Args:
        provider_name: Name of the provider (e.g., "openrouter", "anthropic", "openai")
        **kwargs: Provider-specific configuration parameters

    Returns:
        An instance implementing LLMProvider protocol

    Raises:
        ValueError: If provider is not registered

    Example:
        provider = create_provider(
            "openrouter",
            api_key="sk-or-...",
            default_model="anthropic/claude-sonnet-4-20250514",
        )
    """
    _register_builtins()
    _discover_plugins()

    name = provider_name.lower()
    if name not in _PROVIDERS:
        available = ", ".join(_PROVIDERS.keys())
        raise ValueError(f"Unknown provider '{provider_name}'. Available: {available}")

    provider_class = _PROVIDERS[name]
    return cast("LLMProvider", provider_class(**kwargs))


def get_provider_from_env(
    provider_name: str | None = None,
) -> LLMProvider:
    """Create a provider based on environment variables.

    Reads configuration from environment variables with provider-specific
    prefixes. Falls back to sensible defaults where possible.

    Environment Variables:
        General:
            - LLM_PROVIDER: Provider name (default: "openrouter")
            - LLM_DEFAULT_MODEL: Default model to use
            - LLM_TIMEOUT: Request timeout in seconds

        OpenRouter:
            - OPENROUTER_API_KEY: API key (required)
            - OPENROUTER_BASE_URL: Custom base URL
            - OPENROUTER_SITE_NAME: Site name for rankings

        Anthropic:
            - ANTHROPIC_API_KEY: API key (required)
            - ANTHROPIC_MAX_TOKENS: Max response tokens

        OpenAI:
            - OPENAI_API_KEY: API key (required)
            - OPENAI_ORGANIZATION: Organization ID

    Args:
        provider_name: Optional override for provider name. If None,
            reads from LLM_PROVIDER environment variable.

    Returns:
        An instance implementing LLMProvider protocol configured from
        environment variables.

    Raises:
        KeyError: If required environment variables are missing.
        ValueError: If the provider name is not registered.
    """
    _register_builtins()
    _discover_plugins()

    env_provider = provider_name if provider_name else os.getenv("LLM_PROVIDER", "openrouter")
    name = env_provider.lower() if env_provider else "openrouter"

    # Get common settings
    default_model = os.getenv("LLM_DEFAULT_MODEL")
    timeout_str = os.getenv("LLM_TIMEOUT")
    timeout = float(timeout_str) if timeout_str else None

    if name == "openrouter":
        return _create_openrouter_from_env(default_model, timeout)
    elif name == "anthropic":
        return _create_anthropic_from_env(default_model, timeout)
    elif name == "openai":
        return _create_openai_from_env(default_model, timeout)
    else:
        # For plugins, they must handle their own env configuration
        return create_provider(name)


def _create_openrouter_from_env(
    default_model: str | None,
    timeout: float | None,
) -> LLMProvider:
    """Create OpenRouter provider from environment variables."""
    from llm_providers.providers.openrouter import OpenRouterProvider

    api_key = os.environ["OPENROUTER_API_KEY"]
    base_url = os.getenv("OPENROUTER_BASE_URL", "https://openrouter.ai/api/v1")
    site_name = os.getenv("OPENROUTER_SITE_NAME")
    site_url = os.getenv("OPENROUTER_SITE_URL")
    model = default_model if default_model else os.getenv(
        "OPENROUTER_MODEL", "anthropic/claude-sonnet-4-20250514"
    )

    provider = OpenRouterProvider(
        api_key=api_key,
        base_url=base_url,
        default_model=model if model else "anthropic/claude-sonnet-4-20250514",
        site_name=site_name,
        site_url=site_url,
        timeout_seconds=timeout if timeout else 600.0,
    )
    return provider


def _create_anthropic_from_env(
    default_model: str | None,
    timeout: float | None,
) -> LLMProvider:
    """Create Anthropic provider from environment variables."""
    from llm_providers.providers.anthropic import AnthropicProvider

    api_key = os.environ["ANTHROPIC_API_KEY"]
    model = default_model if default_model else os.getenv(
        "ANTHROPIC_MODEL", "claude-sonnet-4-20250514"
    )
    max_tokens_str = os.getenv("ANTHROPIC_MAX_TOKENS", "4096")
    max_tokens = int(max_tokens_str)

    provider = AnthropicProvider(
        api_key=api_key,
        default_model=model if model else "claude-sonnet-4-20250514",
        max_tokens=max_tokens,
        timeout_seconds=timeout if timeout else 600.0,
    )
    return provider


def _create_openai_from_env(
    default_model: str | None,
    timeout: float | None,
) -> LLMProvider:
    """Create OpenAI provider from environment variables."""
    from llm_providers.providers.openai import OpenAIProvider

    api_key = os.environ["OPENAI_API_KEY"]
    model = default_model if default_model else os.getenv("OPENAI_MODEL", "gpt-4o")
    organization = os.getenv("OPENAI_ORGANIZATION")
    base_url = os.getenv("OPENAI_BASE_URL")

    provider = OpenAIProvider(
        api_key=api_key,
        default_model=model if model else "gpt-4o",
        organization=organization,
        base_url=base_url,
        timeout_seconds=timeout if timeout else 600.0,
    )
    return provider
