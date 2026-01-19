"""Provider factory and registry for War Rig.

This module provides a factory pattern for creating LLM providers by name,
along with a registry for custom provider implementations. It also supports
environment-based configuration for easy deployment.

Example:
    from war_rig.providers.factory import create_provider, get_provider_from_env

    # Create a provider explicitly
    provider = create_provider(
        "openrouter",
        api_key="sk-or-...",
        default_model="anthropic/claude-sonnet-4-20250514",
    )

    # Or create from environment variables
    provider = get_provider_from_env()
"""

from war_rig.providers.protocol import LLMProvider
from war_rig.providers.openrouter import OpenRouterProvider

# Registry of provider classes by name
_PROVIDERS: dict[str, type] = {
    "openrouter": OpenRouterProvider,
}


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

    Args:
        provider_name: Name of the provider (e.g., "openrouter")
        **kwargs: Provider-specific configuration

    Returns:
        An instance implementing LLMProvider protocol

    Raises:
        ValueError: If provider is not registered
    """
    if provider_name not in _PROVIDERS:
        available = ", ".join(_PROVIDERS.keys())
        raise ValueError(f"Unknown provider '{provider_name}'. Available: {available}")

    provider_class = _PROVIDERS[provider_name]
    return provider_class(**kwargs)


def get_provider_from_env() -> LLMProvider:
    """Create a provider based on environment variables.

    Reads:
        - LLM_PROVIDER: Provider name (default: "openrouter")
        - Provider-specific vars like OPENROUTER_API_KEY, etc.

    Returns:
        An instance implementing LLMProvider protocol configured from
        environment variables.

    Raises:
        KeyError: If required environment variables are missing.
        ValueError: If the provider name is not registered.
    """
    import os

    provider_name = os.getenv("LLM_PROVIDER", "openrouter").lower()

    if provider_name == "openrouter":
        return OpenRouterProvider(
            api_key=os.environ["OPENROUTER_API_KEY"],
            base_url=os.getenv("OPENROUTER_BASE_URL", "https://openrouter.ai/api/v1"),
            default_model=os.getenv("SCRIBE_MODEL", "anthropic/claude-sonnet-4-20250514"),
        )

    return create_provider(provider_name)
