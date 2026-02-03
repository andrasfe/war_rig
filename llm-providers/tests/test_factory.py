"""Tests for the provider factory and registry.

This module tests:
- register_provider for adding custom providers
- create_provider for creating providers by name
- get_provider_from_env for environment-based configuration
- get_available_providers for listing providers
- Error handling for unknown providers
"""

import os
from typing import Any
from unittest.mock import patch

import pytest

from llm_providers.factory import (
    _PROVIDERS,
    create_provider,
    get_available_providers,
    get_provider_from_env,
    register_provider,
)
from llm_providers.protocol import CompletionResponse, LLMProvider, Message
from llm_providers.providers.anthropic import AnthropicProvider
from llm_providers.providers.openai import OpenAIProvider
from llm_providers.providers.openrouter import OpenRouterProvider


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

        register_provider("custom", CustomProvider)

        try:
            assert "custom" in _PROVIDERS
            assert _PROVIDERS["custom"] is CustomProvider
        finally:
            del _PROVIDERS["custom"]

    def test_register_provider_overwrites_existing(self) -> None:
        """Test that registering with same name overwrites previous."""
        class ProviderV1:
            pass

        class ProviderV2:
            pass

        register_provider("test_overwrite", ProviderV1)
        assert _PROVIDERS["test_overwrite"] is ProviderV1

        register_provider("test_overwrite", ProviderV2)
        assert _PROVIDERS["test_overwrite"] is ProviderV2

        del _PROVIDERS["test_overwrite"]

    def test_register_provider_case_insensitive(self) -> None:
        """Test that provider names are lowercased."""
        class TestProvider:
            pass

        register_provider("TestProvider", TestProvider)

        try:
            assert "testprovider" in _PROVIDERS
            assert "TestProvider" not in _PROVIDERS
        finally:
            del _PROVIDERS["testprovider"]


class TestGetAvailableProviders:
    """Tests for get_available_providers function."""

    def test_builtin_providers_available(self) -> None:
        """Test that builtin providers are listed."""
        providers = get_available_providers()
        assert "openrouter" in providers
        assert "anthropic" in providers
        assert "openai" in providers

    def test_custom_provider_listed_after_registration(self) -> None:
        """Test that registered providers appear in list."""
        class CustomProvider:
            pass

        register_provider("custom_list_test", CustomProvider)

        try:
            providers = get_available_providers()
            assert "custom_list_test" in providers
        finally:
            del _PROVIDERS["custom_list_test"]


class TestCreateProvider:
    """Tests for create_provider function."""

    def test_create_openrouter_provider(self) -> None:
        """Test creating an OpenRouter provider."""
        provider = create_provider(
            "openrouter",
            api_key="test-key",
            default_model="anthropic/claude-sonnet-4-20250514",
        )

        assert isinstance(provider, OpenRouterProvider)
        assert isinstance(provider, LLMProvider)

    def test_create_anthropic_provider(self) -> None:
        """Test creating an Anthropic provider."""
        provider = create_provider(
            "anthropic",
            api_key="sk-ant-test",
            default_model="claude-sonnet-4-20250514",
        )

        assert isinstance(provider, AnthropicProvider)
        assert isinstance(provider, LLMProvider)

    def test_create_openai_provider(self) -> None:
        """Test creating an OpenAI provider."""
        provider = create_provider(
            "openai",
            api_key="sk-test",
            default_model="gpt-4o",
        )

        assert isinstance(provider, OpenAIProvider)
        assert isinstance(provider, LLMProvider)

    def test_create_openrouter_with_custom_model(self) -> None:
        """Test creating OpenRouter provider with custom model."""
        provider = create_provider(
            "openrouter",
            api_key="test-key",
            default_model="openai/gpt-4o",
        )

        assert provider.default_model == "openai/gpt-4o"

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

    def test_create_provider_case_insensitive(self) -> None:
        """Test that provider name lookup is case-insensitive."""
        provider = create_provider(
            "OPENROUTER",
            api_key="test-key",
            default_model="test-model",
        )
        assert isinstance(provider, OpenRouterProvider)

        provider2 = create_provider(
            "OpenRouter",
            api_key="test-key",
            default_model="test-model",
        )
        assert isinstance(provider2, OpenRouterProvider)

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

    def test_anthropic_provider_from_env(self) -> None:
        """Test creating Anthropic provider from environment variables."""
        env_vars = {
            "LLM_PROVIDER": "anthropic",
            "ANTHROPIC_API_KEY": "sk-ant-test-key",
        }

        with patch.dict(os.environ, env_vars, clear=True):
            provider = get_provider_from_env()

        assert isinstance(provider, AnthropicProvider)
        assert provider._api_key == "sk-ant-test-key"

    def test_openai_provider_from_env(self) -> None:
        """Test creating OpenAI provider from environment variables."""
        env_vars = {
            "LLM_PROVIDER": "openai",
            "OPENAI_API_KEY": "sk-test-key",
        }

        with patch.dict(os.environ, env_vars, clear=True):
            provider = get_provider_from_env()

        assert isinstance(provider, OpenAIProvider)
        assert provider._api_key == "sk-test-key"

    def test_custom_model_from_llm_default_model(self) -> None:
        """Test custom model from LLM_DEFAULT_MODEL environment variable."""
        env_vars = {
            "LLM_PROVIDER": "openrouter",
            "OPENROUTER_API_KEY": "test-key",
            "LLM_DEFAULT_MODEL": "custom/model-name",
        }

        with patch.dict(os.environ, env_vars, clear=True):
            provider = get_provider_from_env()

        assert provider.default_model == "custom/model-name"

    def test_custom_timeout_from_env(self) -> None:
        """Test custom timeout from LLM_TIMEOUT environment variable."""
        env_vars = {
            "LLM_PROVIDER": "openrouter",
            "OPENROUTER_API_KEY": "test-key",
            "LLM_TIMEOUT": "120.0",
        }

        with patch.dict(os.environ, env_vars, clear=True):
            provider = get_provider_from_env()

        # Verify timeout is set (check internal state)
        assert provider._timeout.read == 120.0

    def test_openrouter_site_metadata_from_env(self) -> None:
        """Test OpenRouter site metadata from environment."""
        env_vars = {
            "LLM_PROVIDER": "openrouter",
            "OPENROUTER_API_KEY": "test-key",
            "OPENROUTER_SITE_NAME": "Test App",
            "OPENROUTER_SITE_URL": "https://test.com",
        }

        with patch.dict(os.environ, env_vars, clear=True):
            provider = get_provider_from_env()

        assert provider._site_name == "Test App"
        assert provider._site_url == "https://test.com"

    def test_missing_api_key_raises_key_error(self) -> None:
        """Test that missing API key raises KeyError."""
        env_vars = {
            "LLM_PROVIDER": "openrouter",
        }

        with patch.dict(os.environ, env_vars, clear=True):
            with pytest.raises(KeyError) as exc_info:
                get_provider_from_env()

        assert "OPENROUTER_API_KEY" in str(exc_info.value)

    def test_missing_anthropic_api_key_raises(self) -> None:
        """Test that missing ANTHROPIC_API_KEY raises KeyError."""
        env_vars = {
            "LLM_PROVIDER": "anthropic",
        }

        with patch.dict(os.environ, env_vars, clear=True):
            with pytest.raises(KeyError) as exc_info:
                get_provider_from_env()

        assert "ANTHROPIC_API_KEY" in str(exc_info.value)

    def test_missing_openai_api_key_raises(self) -> None:
        """Test that missing OPENAI_API_KEY raises KeyError."""
        env_vars = {
            "LLM_PROVIDER": "openai",
        }

        with patch.dict(os.environ, env_vars, clear=True):
            with pytest.raises(KeyError) as exc_info:
                get_provider_from_env()

        assert "OPENAI_API_KEY" in str(exc_info.value)

    def test_provider_name_override_parameter(self) -> None:
        """Test overriding provider name via parameter."""
        env_vars = {
            "LLM_PROVIDER": "openrouter",  # This should be ignored
            "ANTHROPIC_API_KEY": "sk-ant-test",
        }

        with patch.dict(os.environ, env_vars, clear=True):
            provider = get_provider_from_env(provider_name="anthropic")

        assert isinstance(provider, AnthropicProvider)

    def test_provider_name_case_insensitive(self) -> None:
        """Test that LLM_PROVIDER is case-insensitive."""
        env_vars = {
            "LLM_PROVIDER": "OPENROUTER",
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


class TestProviderRegistry:
    """Tests for the provider registry state."""

    def test_openrouter_registered_by_default(self) -> None:
        """Test that openrouter is registered by default."""
        # Ensure builtins are registered
        get_available_providers()
        assert "openrouter" in _PROVIDERS
        assert _PROVIDERS["openrouter"] is OpenRouterProvider

    def test_anthropic_registered_by_default(self) -> None:
        """Test that anthropic is registered by default."""
        get_available_providers()
        assert "anthropic" in _PROVIDERS
        assert _PROVIDERS["anthropic"] is AnthropicProvider

    def test_openai_registered_by_default(self) -> None:
        """Test that openai is registered by default."""
        get_available_providers()
        assert "openai" in _PROVIDERS
        assert _PROVIDERS["openai"] is OpenAIProvider

    def test_registry_isolation(self) -> None:
        """Test that test registrations can be cleaned up."""

        class TempProvider:
            pass

        register_provider("temp_test", TempProvider)
        assert "temp_test" in _PROVIDERS

        del _PROVIDERS["temp_test"]

        # Verify temp_test was removed
        assert "temp_test" not in _PROVIDERS
