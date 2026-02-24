"""Provider configuration schemas — re-exported from llm_providers.

All configuration models are now defined in the ``llm_providers`` package.
This module re-exports them for backward compatibility.

Typical usage:
    from war_rig.providers.config import OpenRouterConfig

    config = OpenRouterConfig(
        api_key="sk-or-v1-...",
        default_model="anthropic/claude-sonnet-4-20250514",
    )
"""

from llm_providers.config import (
    AnthropicConfig,
    OpenAIConfig,
    OpenRouterConfig,
    ProviderConfig,
)

__all__ = [
    "AnthropicConfig",
    "OpenAIConfig",
    "OpenRouterConfig",
    "ProviderConfig",
]
