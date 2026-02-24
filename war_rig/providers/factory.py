"""Provider factory and registry for War Rig.

This module wraps ``llm_providers`` factory functions, adding:
- Circuit-breaker wrapping for all providers
- War Rig-specific environment variable handling (e.g. SCRIBE_MODEL)
- Plugin discovery via the ``war_rig.providers`` entry-point group

For providers that llm_providers handles natively (anthropic, openai), the
raw provider is created by llm_providers and then wrapped in a
``CircuitBreakerProvider``.  For openrouter, war_rig's own implementation
is used (it adds tool-calling support and structured error logging).

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
import os
from importlib.metadata import entry_points

from llm_providers import get_provider_from_env as _llm_get_provider_from_env
from llm_providers.providers.anthropic import AnthropicProvider
from llm_providers.providers.openai import OpenAIProvider

from war_rig.providers.circuit_breaker import CircuitBreakerProvider
from war_rig.providers.openrouter import OpenRouterProvider
from war_rig.providers.protocol import LLMProvider

logger = logging.getLogger(__name__)

# Registry of provider classes by name
_PROVIDERS: dict[str, type] = {
    "openrouter": OpenRouterProvider,
    "anthropic": AnthropicProvider,
    "openai": OpenAIProvider,
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

    Wraps every provider in a ``CircuitBreakerProvider`` for fault tolerance.

    For openrouter, war_rig's own implementation is used (it adds tool-calling
    support and reads SCRIBE_MODEL as a fallback for LLM_DEFAULT_MODEL).

    For anthropic, openai, and plugin providers, the raw provider is created
    by ``llm_providers`` and then wrapped.

    Args:
        provider_name: Optional provider name override. If None, reads from
            LLM_PROVIDER environment variable.

    Returns:
        An instance implementing LLMProvider protocol configured from
        environment variables, wrapped in a CircuitBreakerProvider.

    Raises:
        KeyError: If required environment variables are missing.
        ValueError: If the provider name is not registered.
    """
    _discover_plugins()

    # Determine provider: argument > env var > default
    resolved_provider = provider_name or os.getenv("LLM_PROVIDER", "openrouter").lower()

    logger.debug(f"Creating provider: {resolved_provider}")

    # OpenRouter: use war_rig's implementation (tool calling + SCRIBE_MODEL)
    if resolved_provider == "openrouter":
        return _create_openrouter_from_env()

    # Anthropic / OpenAI: delegate to llm_providers (richer env handling)
    if resolved_provider in ("anthropic", "openai"):

        def _make_llm() -> LLMProvider:
            return _llm_get_provider_from_env(resolved_provider)

        return CircuitBreakerProvider(_make_llm(), factory=_make_llm)

    # Custom / plugin providers: use war_rig's own registry
    def _make_plugin() -> LLMProvider:
        return create_provider(resolved_provider)

    return CircuitBreakerProvider(_make_plugin(), factory=_make_plugin)


def _create_openrouter_from_env() -> LLMProvider:
    """Create war_rig's OpenRouterProvider from environment variables.

    Uses war_rig's OpenRouterProvider (with tool-calling support) and wraps
    it in a CircuitBreakerProvider.  Reads SCRIBE_MODEL as a fallback for
    LLM_DEFAULT_MODEL for backward compatibility.
    """
    # Build timeout from env if specified (in seconds)
    timeout = None
    timeout_env = os.getenv("LLM_REQUEST_TIMEOUT")
    if timeout_env:
        try:
            from httpx import Timeout

            read_timeout = float(timeout_env)
            timeout = Timeout(connect=30.0, read=read_timeout, write=60.0, pool=30.0)
            logger.info(f"Using custom LLM request timeout: {read_timeout}s")
        except ValueError:
            logger.warning(
                f"Invalid LLM_REQUEST_TIMEOUT value: {timeout_env}, using default"
            )

    # Get API key - required for openrouter
    api_key = os.environ.get("OPENROUTER_API_KEY")
    if not api_key:
        raise KeyError(
            "OPENROUTER_API_KEY environment variable is required for openrouter provider. "
            "Set LLM_PROVIDER to use a different provider."
        )

    def _make_openrouter() -> LLMProvider:
        return OpenRouterProvider(
            api_key=api_key,
            base_url=os.getenv(
                "OPENROUTER_BASE_URL", "https://openrouter.ai/api/v1"
            ),
            default_model=os.getenv(
                "LLM_DEFAULT_MODEL",
                os.getenv("SCRIBE_MODEL", "anthropic/claude-sonnet-4-20250514"),
            ),
            timeout=timeout,
        )

    return CircuitBreakerProvider(_make_openrouter(), factory=_make_openrouter)
