"""Provider factory and registry for War Rig.

This module provides a factory pattern for creating LLM providers by name,
along with a registry for custom provider implementations. It also supports
environment-based configuration for easy deployment.

**Provider-Agnostic Design:**
    War Rig does not assume any specific LLM provider. The provider is
    determined entirely by the LLM_PROVIDER environment variable. If not
    set, it defaults to "openrouter" for backward compatibility, but this
    can be any registered provider (built-in or plugin).

Plugin Discovery:
    External packages can register providers without modifying war_rig by
    using Python entry points. Add to your pyproject.toml:

        [project.entry-points."war_rig.providers"]
        myprovider = "mypackage.provider:MyProviderClass"

    The provider will be auto-discovered when war_rig loads. Plugin providers
    must handle their own environment variable configuration.

Environment Variables:
    - LLM_PROVIDER: Provider name (default: "openrouter")
    - LLM_DEFAULT_MODEL: Default model for completions
    - LLM_REQUEST_TIMEOUT: Request timeout in seconds
    - Provider-specific keys (e.g., OPENROUTER_API_KEY for openrouter)

Example:
    from war_rig.providers.factory import create_provider, get_provider_from_env

    # Create a provider explicitly
    provider = create_provider(
        "myprovider",
        api_key="...",
        default_model="my-model",
    )

    # Or create from environment variables
    # Uses LLM_PROVIDER to determine which provider to instantiate
    provider = get_provider_from_env()
"""

import logging
from importlib.metadata import entry_points

from war_rig.providers.circuit_breaker import CircuitBreakerProvider
from war_rig.providers.openrouter import OpenRouterProvider
from war_rig.providers.protocol import LLMProvider

logger = logging.getLogger(__name__)

# Registry of provider classes by name
_PROVIDERS: dict[str, type] = {
    "openrouter": OpenRouterProvider,
}

# Track if plugins have been loaded
_PLUGINS_LOADED = False


def _discover_plugins() -> None:
    """Discover and register provider plugins via entry points.

    Looks for entry points in the 'war_rig.providers' group.
    Each entry point should point to a provider class.

    External packages register by adding to pyproject.toml:
        [project.entry-points."war_rig.providers"]
        myprovider = "mypackage.provider:MyProviderClass"
    """
    global _PLUGINS_LOADED

    if _PLUGINS_LOADED:
        return

    _PLUGINS_LOADED = True

    try:
        # Python 3.10+ style
        eps = entry_points(group="war_rig.providers")
    except TypeError:
        # Python 3.9 fallback
        all_eps = entry_points()
        eps = all_eps.get("war_rig.providers", [])

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
    _discover_plugins()
    return list(_PROVIDERS.keys())


def register_provider(name: str, provider_class: type) -> None:
    """Register a custom provider class.

    Args:
        name: The name to register the provider under (e.g., "azure", "local").
        provider_class: The provider class to register. Should implement
            the LLMProvider protocol.

    Example:
        from war_rig.providers.factory import register_provider

        class MyCustomProvider:
            # ... implements LLMProvider protocol ...
            pass

        register_provider("custom", MyCustomProvider)
    """
    _PROVIDERS[name] = provider_class


def create_provider(
    provider_name: str,
    **kwargs,
) -> LLMProvider:
    """Create an LLM provider by name.

    Automatically discovers provider plugins on first call.

    Args:
        provider_name: Name of the provider (e.g., "openrouter")
        **kwargs: Provider-specific configuration

    Returns:
        An instance implementing LLMProvider protocol

    Raises:
        ValueError: If provider is not registered
    """
    _discover_plugins()

    if provider_name not in _PROVIDERS:
        available = ", ".join(_PROVIDERS.keys())
        raise ValueError(f"Unknown provider '{provider_name}'. Available: {available}")

    provider_class = _PROVIDERS[provider_name]
    return provider_class(**kwargs)


def get_provider_from_env(provider_name: str | None = None) -> LLMProvider:
    """Create a provider based on environment variables.

    Automatically discovers provider plugins on first call.

    This function is **provider-agnostic**. The provider is determined by:
    1. The provider_name argument (if provided)
    2. The LLM_PROVIDER environment variable
    3. Default: "openrouter" (for backward compatibility only)

    Each provider handles its own environment configuration. For example:
    - openrouter: reads OPENROUTER_API_KEY, OPENROUTER_BASE_URL
    - anthropic: reads ANTHROPIC_API_KEY
    - custom plugins: read their own env vars

    Args:
        provider_name: Optional provider name override. If None, reads from
            LLM_PROVIDER environment variable.

    Returns:
        An instance implementing LLMProvider protocol configured from
        environment variables.

    Raises:
        KeyError: If required environment variables are missing.
        ValueError: If the provider name is not registered.
    """
    import os

    _discover_plugins()

    # Determine provider: argument > env var > default
    resolved_provider = provider_name or os.getenv("LLM_PROVIDER", "openrouter").lower()

    logger.debug(f"Creating provider: {resolved_provider}")

    # Built-in openrouter provider with env configuration
    if resolved_provider == "openrouter":
        # Build timeout from env if specified (in seconds)
        # LLM_REQUEST_TIMEOUT sets the read timeout for LLM responses
        # Default is 600s (10 min) to accommodate slow models
        timeout = None
        timeout_env = os.getenv("LLM_REQUEST_TIMEOUT")
        if timeout_env:
            try:
                from httpx import Timeout
                read_timeout = float(timeout_env)
                timeout = Timeout(connect=30.0, read=read_timeout, write=60.0, pool=30.0)
                logger.info(f"Using custom LLM request timeout: {read_timeout}s")
            except ValueError:
                logger.warning(f"Invalid LLM_REQUEST_TIMEOUT value: {timeout_env}, using default")

        # Get API key - required for openrouter
        api_key = os.environ.get("OPENROUTER_API_KEY")
        if not api_key:
            raise KeyError(
                "OPENROUTER_API_KEY environment variable is required for openrouter provider. "
                "Set LLM_PROVIDER to use a different provider."
            )

        return CircuitBreakerProvider(
            OpenRouterProvider(
                api_key=api_key,
                base_url=os.getenv("OPENROUTER_BASE_URL", "https://openrouter.ai/api/v1"),
                default_model=os.getenv("LLM_DEFAULT_MODEL", os.getenv("SCRIBE_MODEL", "anthropic/claude-sonnet-4-20250514")),
                timeout=timeout,
            )
        )

    # For all other providers (plugins), they must handle their own env configuration
    # Pass no kwargs - plugin is responsible for reading env vars
    return CircuitBreakerProvider(create_provider(resolved_provider))
