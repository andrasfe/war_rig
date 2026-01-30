"""Tests for the provider factory and registry.

This module tests:
- register_provider for adding custom providers
- create_provider for creating providers by name
- get_provider_from_env for environment-based configuration
- Error handling for unknown providers
"""

import os
from typing import Any
from unittest.mock import patch

import pytest

from war_rig.providers import (
    CompletionResponse,
    LLMProvider,
    Message,
    OpenRouterProvider,
    create_provider,
    get_provider_from_env,
    register_provider,
)
from war_rig.providers.factory import _PROVIDERS


class TestRegisterProvider:
    """Tests for register_provider function."""

    def test_register_custom_provider(self) -> None:
        """Test registering a custom provider class."""
        class CustomProvider:
            def __init__(self, api_key: str) -> None:
                self._api_key = api_key

            @property
            def default_model(self) -> str:
                return "custom-model"

            async def complete(
                self,
                messages: list[Message],
                model: str | None = None,
                temperature: float = 0.7,
                **kwargs: Any,
            ) -> CompletionResponse:
                return CompletionResponse(
                    content="custom response",
                    model=self.default_model,
                    tokens_used=10,
                )

        # Register the custom provider
        register_provider("custom", CustomProvider)

        # Verify it was registered
        assert "custom" in _PROVIDERS
        assert _PROVIDERS["custom"] is CustomProvider

        # Clean up
        del _PROVIDERS["custom"]

    def test_register_provider_overwrites_existing(self) -> None:
        """Test that registering with same name overwrites previous registration."""
        class ProviderV1:
            pass

        class ProviderV2:
            pass

        register_provider("test_overwrite", ProviderV1)
        assert _PROVIDERS["test_overwrite"] is ProviderV1

        register_provider("test_overwrite", ProviderV2)
        assert _PROVIDERS["test_overwrite"] is ProviderV2

        # Clean up
        del _PROVIDERS["test_overwrite"]


class TestCreateProvider:
    """Tests for create_provider function."""

    def test_create_openrouter_provider(self) -> None:
        """Test creating an OpenRouter provider."""
        provider = create_provider("openrouter", api_key="test-key")

        assert isinstance(provider, OpenRouterProvider)
        assert isinstance(provider, LLMProvider)

    def test_create_openrouter_with_custom_model(self) -> None:
        """Test creating OpenRouter provider with custom model."""
        provider = create_provider(
            "openrouter",
            api_key="test-key",
            default_model="openai/gpt-4",
        )

        assert provider.default_model == "openai/gpt-4"

    def test_create_openrouter_with_all_options(self) -> None:
        """Test creating OpenRouter provider with all configuration options."""
        provider = create_provider(
            "openrouter",
            api_key="test-key",
            base_url="https://custom.api.com/v1",
            default_model="meta/llama-3-70b",
            site_url="https://example.com",
            site_name="Test App",
        )

        assert isinstance(provider, OpenRouterProvider)
        assert provider.default_model == "meta/llama-3-70b"
        assert provider._base_url == "https://custom.api.com/v1"
        assert provider._site_url == "https://example.com"
        assert provider._site_name == "Test App"

    def test_create_unknown_provider_raises_value_error(self) -> None:
        """Test that unknown provider name raises ValueError."""
        with pytest.raises(ValueError) as exc_info:
            create_provider("nonexistent", api_key="test-key")

        assert "Unknown provider 'nonexistent'" in str(exc_info.value)
        assert "Available:" in str(exc_info.value)
        assert "openrouter" in str(exc_info.value)

    def test_create_provider_case_sensitive(self) -> None:
        """Test that provider names are case-sensitive."""
        with pytest.raises(ValueError) as exc_info:
            create_provider("OpenRouter", api_key="test-key")

        assert "Unknown provider 'OpenRouter'" in str(exc_info.value)

    def test_create_custom_registered_provider(self) -> None:
        """Test creating a custom registered provider."""
        class MockProvider:
            def __init__(self, api_key: str, custom_param: str = "default") -> None:
                self.api_key = api_key
                self.custom_param = custom_param

            @property
            def default_model(self) -> str:
                return "mock-model"

            async def complete(
                self,
                messages: list[Message],
                model: str | None = None,
                temperature: float = 0.7,
                **kwargs: Any,
            ) -> CompletionResponse:
                return CompletionResponse(
                    content="mock",
                    model=self.default_model,
                    tokens_used=0,
                )

        register_provider("mock", MockProvider)

        try:
            provider = create_provider(
                "mock",
                api_key="test-key",
                custom_param="custom-value",
            )

            assert provider.api_key == "test-key"
            assert provider.custom_param == "custom-value"
        finally:
            del _PROVIDERS["mock"]


class TestGetProviderFromEnv:
    """Tests for get_provider_from_env function."""

    def test_default_provider_is_openrouter(self) -> None:
        """Test that default provider is openrouter when LLM_PROVIDER not set."""
        env_vars = {
            "OPENROUTER_API_KEY": "test-api-key",
        }

        with patch.dict(os.environ, env_vars, clear=True):
            provider = get_provider_from_env()

        assert isinstance(provider, OpenRouterProvider)

    def test_openrouter_provider_from_env(self) -> None:
        """Test creating OpenRouter provider from environment variables."""
        env_vars = {
            "LLM_PROVIDER": "openrouter",
            "OPENROUTER_API_KEY": "sk-or-test-key",
        }

        with patch.dict(os.environ, env_vars, clear=True):
            provider = get_provider_from_env()

        assert isinstance(provider, OpenRouterProvider)
        assert provider._api_key == "sk-or-test-key"

    def test_openrouter_custom_base_url_from_env(self) -> None:
        """Test OpenRouter with custom base URL from environment."""
        env_vars = {
            "LLM_PROVIDER": "openrouter",
            "OPENROUTER_API_KEY": "test-key",
            "OPENROUTER_BASE_URL": "https://custom.openrouter.ai/api/v1",
        }

        with patch.dict(os.environ, env_vars, clear=True):
            provider = get_provider_from_env()

        assert provider._base_url == "https://custom.openrouter.ai/api/v1"

    def test_openrouter_custom_model_from_env(self) -> None:
        """Test OpenRouter with custom model from SCRIBE_MODEL env var."""
        env_vars = {
            "LLM_PROVIDER": "openrouter",
            "OPENROUTER_API_KEY": "test-key",
            "SCRIBE_MODEL": "anthropic/claude-3-opus",
        }

        with patch.dict(os.environ, env_vars, clear=True):
            provider = get_provider_from_env()

        assert provider.default_model == "anthropic/claude-3-opus"

    def test_openrouter_default_model_when_not_set(self) -> None:
        """Test OpenRouter uses default model when SCRIBE_MODEL not set."""
        env_vars = {
            "LLM_PROVIDER": "openrouter",
            "OPENROUTER_API_KEY": "test-key",
        }

        with patch.dict(os.environ, env_vars, clear=True):
            provider = get_provider_from_env()

        assert provider.default_model == "anthropic/claude-sonnet-4-20250514"

    def test_missing_api_key_raises_key_error(self) -> None:
        """Test that missing OPENROUTER_API_KEY raises KeyError."""
        env_vars = {
            "LLM_PROVIDER": "openrouter",
        }

        with patch.dict(os.environ, env_vars, clear=True):
            with pytest.raises(KeyError) as exc_info:
                get_provider_from_env()

        assert "OPENROUTER_API_KEY" in str(exc_info.value)

    def test_provider_name_case_insensitive(self) -> None:
        """Test that LLM_PROVIDER is case-insensitive."""
        env_vars = {
            "LLM_PROVIDER": "OPENROUTER",
            "OPENROUTER_API_KEY": "test-key",
        }

        with patch.dict(os.environ, env_vars, clear=True):
            provider = get_provider_from_env()

        assert isinstance(provider, OpenRouterProvider)

    def test_provider_name_mixed_case(self) -> None:
        """Test that LLM_PROVIDER handles mixed case."""
        env_vars = {
            "LLM_PROVIDER": "OpenRouter",
            "OPENROUTER_API_KEY": "test-key",
        }

        with patch.dict(os.environ, env_vars, clear=True):
            provider = get_provider_from_env()

        assert isinstance(provider, OpenRouterProvider)

    def test_unknown_provider_from_env_raises_value_error(self) -> None:
        """Test that unknown provider in env raises ValueError."""
        env_vars = {
            "LLM_PROVIDER": "unknown_provider",
        }

        with patch.dict(os.environ, env_vars, clear=True):
            with pytest.raises(ValueError) as exc_info:
                get_provider_from_env()

        assert "Unknown provider 'unknown_provider'" in str(exc_info.value)

    def test_custom_registered_provider_from_env(self) -> None:
        """Test that custom registered providers can be created from env."""
        class EnvProvider:
            def __init__(self) -> None:
                pass

            @property
            def default_model(self) -> str:
                return "env-model"

            async def complete(
                self,
                messages: list[Message],
                model: str | None = None,
                temperature: float = 0.7,
                **kwargs: Any,
            ) -> CompletionResponse:
                return CompletionResponse(
                    content="env",
                    model=self.default_model,
                    tokens_used=0,
                )

        register_provider("envprovider", EnvProvider)

        try:
            env_vars = {
                "LLM_PROVIDER": "envprovider",
            }

            with patch.dict(os.environ, env_vars, clear=True):
                provider = get_provider_from_env()

            assert provider.default_model == "env-model"
        finally:
            del _PROVIDERS["envprovider"]


class TestProviderRegistry:
    """Tests for the provider registry state."""

    def test_openrouter_registered_by_default(self) -> None:
        """Test that openrouter is registered by default."""
        assert "openrouter" in _PROVIDERS
        assert _PROVIDERS["openrouter"] is OpenRouterProvider

    def test_registry_isolation(self) -> None:
        """Test that test registrations don't affect production registry."""
        original_providers = set(_PROVIDERS.keys())

        # Register a test provider
        class TempProvider:
            pass

        register_provider("temp_test", TempProvider)
        assert "temp_test" in _PROVIDERS

        # Clean up
        del _PROVIDERS["temp_test"]

        # Verify original state restored
        assert set(_PROVIDERS.keys()) == original_providers
