"""Tests for provider configuration models.

This module tests the Pydantic configuration models for all providers.
"""

import pytest
from pydantic import ValidationError

from llm_providers.config import (
    AnthropicConfig,
    OpenAIConfig,
    OpenRouterConfig,
    ProviderConfig,
)


class TestProviderConfig:
    """Tests for the base ProviderConfig model."""

    def test_create_base_config(self) -> None:
        """Test creating a base provider config."""
        config = ProviderConfig(default_model="test-model")
        assert config.provider_type == "base"
        assert config.default_model == "test-model"
        assert config.temperature == 0.7
        assert config.timeout_seconds == 600.0

    def test_temperature_validation_min(self) -> None:
        """Test that temperature has a minimum of 0.0."""
        with pytest.raises(ValidationError) as exc_info:
            ProviderConfig(default_model="test", temperature=-0.1)
        assert "temperature" in str(exc_info.value).lower()

    def test_temperature_validation_max(self) -> None:
        """Test that temperature has a maximum of 2.0."""
        with pytest.raises(ValidationError) as exc_info:
            ProviderConfig(default_model="test", temperature=2.5)
        assert "temperature" in str(exc_info.value).lower()

    def test_temperature_boundary_values(self) -> None:
        """Test temperature at boundary values."""
        config_min = ProviderConfig(default_model="test", temperature=0.0)
        config_max = ProviderConfig(default_model="test", temperature=2.0)
        assert config_min.temperature == 0.0
        assert config_max.temperature == 2.0

    def test_timeout_must_be_positive(self) -> None:
        """Test that timeout_seconds must be positive."""
        with pytest.raises(ValidationError) as exc_info:
            ProviderConfig(default_model="test", timeout_seconds=0.0)
        assert "timeout" in str(exc_info.value).lower()

    def test_default_model_required(self) -> None:
        """Test that default_model is required."""
        with pytest.raises(ValidationError):
            ProviderConfig()  # type: ignore

    def test_custom_timeout(self) -> None:
        """Test setting custom timeout."""
        config = ProviderConfig(default_model="test", timeout_seconds=120.0)
        assert config.timeout_seconds == 120.0


class TestOpenRouterConfig:
    """Tests for OpenRouterConfig model."""

    def test_create_openrouter_config(self) -> None:
        """Test creating OpenRouter config with required fields."""
        config = OpenRouterConfig(
            api_key="sk-or-test-key",
            default_model="anthropic/claude-sonnet-4-20250514",
        )
        assert config.provider_type == "openrouter"
        assert config.api_key == "sk-or-test-key"
        assert config.default_model == "anthropic/claude-sonnet-4-20250514"
        assert config.base_url == "https://openrouter.ai/api/v1"

    def test_openrouter_api_key_required(self) -> None:
        """Test that api_key is required."""
        with pytest.raises(ValidationError):
            OpenRouterConfig()  # type: ignore

    def test_openrouter_default_model_has_default(self) -> None:
        """Test that default_model has a sensible default."""
        config = OpenRouterConfig(api_key="test-key")
        assert config.default_model == "anthropic/claude-sonnet-4-20250514"

    def test_openrouter_custom_base_url(self) -> None:
        """Test setting custom base URL."""
        config = OpenRouterConfig(
            api_key="sk-or-test",
            default_model="test-model",
            base_url="https://custom.api.com/v1",
        )
        assert config.base_url == "https://custom.api.com/v1"

    def test_openrouter_site_metadata(self) -> None:
        """Test setting site URL and name for OpenRouter rankings."""
        config = OpenRouterConfig(
            api_key="sk-or-test",
            default_model="test-model",
            site_url="https://myapp.com",
            site_name="My Application",
        )
        assert config.site_url == "https://myapp.com"
        assert config.site_name == "My Application"

    def test_openrouter_site_metadata_optional(self) -> None:
        """Test that site metadata is optional."""
        config = OpenRouterConfig(
            api_key="sk-or-test",
            default_model="test-model",
        )
        assert config.site_url is None
        assert config.site_name is None

    def test_openrouter_inherits_base_config(self) -> None:
        """Test that OpenRouterConfig inherits temperature and timeout."""
        config = OpenRouterConfig(
            api_key="sk-or-test",
            default_model="test-model",
            temperature=0.5,
            timeout_seconds=300.0,
        )
        assert config.temperature == 0.5
        assert config.timeout_seconds == 300.0


class TestAnthropicConfig:
    """Tests for AnthropicConfig model."""

    def test_create_anthropic_config(self) -> None:
        """Test creating Anthropic config with required fields."""
        config = AnthropicConfig(
            api_key="sk-ant-test-key",
            default_model="claude-sonnet-4-20250514",
        )
        assert config.provider_type == "anthropic"
        assert config.api_key == "sk-ant-test-key"
        assert config.default_model == "claude-sonnet-4-20250514"
        assert config.max_tokens == 4096
        assert config.base_url is None

    def test_anthropic_api_key_required(self) -> None:
        """Test that api_key is required."""
        with pytest.raises(ValidationError):
            AnthropicConfig()  # type: ignore

    def test_anthropic_default_model_has_default(self) -> None:
        """Test that default_model has a sensible default."""
        config = AnthropicConfig(api_key="test-key")
        assert config.default_model == "claude-sonnet-4-20250514"

    def test_anthropic_custom_max_tokens(self) -> None:
        """Test setting custom max_tokens."""
        config = AnthropicConfig(
            api_key="sk-ant-test",
            default_model="test-model",
            max_tokens=8192,
        )
        assert config.max_tokens == 8192

    def test_anthropic_max_tokens_must_be_positive(self) -> None:
        """Test that max_tokens must be positive."""
        with pytest.raises(ValidationError) as exc_info:
            AnthropicConfig(
                api_key="sk-ant-test",
                default_model="test-model",
                max_tokens=0,
            )
        assert "max_tokens" in str(exc_info.value).lower()

    def test_anthropic_custom_base_url(self) -> None:
        """Test setting custom base URL for proxy."""
        config = AnthropicConfig(
            api_key="sk-ant-test",
            default_model="test-model",
            base_url="https://anthropic-proxy.example.com",
        )
        assert config.base_url == "https://anthropic-proxy.example.com"


class TestOpenAIConfig:
    """Tests for OpenAIConfig model."""

    def test_create_openai_config(self) -> None:
        """Test creating OpenAI config with required fields."""
        config = OpenAIConfig(
            api_key="sk-test-key",
            default_model="gpt-4o",
        )
        assert config.provider_type == "openai"
        assert config.api_key == "sk-test-key"
        assert config.default_model == "gpt-4o"
        assert config.base_url is None
        assert config.organization is None

    def test_openai_api_key_required(self) -> None:
        """Test that api_key is required."""
        with pytest.raises(ValidationError):
            OpenAIConfig()  # type: ignore

    def test_openai_default_model_has_default(self) -> None:
        """Test that default_model has a sensible default."""
        config = OpenAIConfig(api_key="test-key")
        assert config.default_model == "gpt-4o"

    def test_openai_custom_base_url(self) -> None:
        """Test setting custom base URL for Azure or proxy."""
        config = OpenAIConfig(
            api_key="sk-test",
            default_model="gpt-4o",
            base_url="https://my-azure.openai.azure.com/v1",
        )
        assert config.base_url == "https://my-azure.openai.azure.com/v1"

    def test_openai_organization(self) -> None:
        """Test setting organization ID."""
        config = OpenAIConfig(
            api_key="sk-test",
            default_model="gpt-4o",
            organization="org-12345",
        )
        assert config.organization == "org-12345"

    def test_openai_organization_optional(self) -> None:
        """Test that organization is optional."""
        config = OpenAIConfig(
            api_key="sk-test",
            default_model="gpt-4o",
        )
        assert config.organization is None


class TestConfigSerialization:
    """Tests for config serialization/deserialization."""

    def test_openrouter_to_dict(self) -> None:
        """Test serializing OpenRouterConfig to dict."""
        config = OpenRouterConfig(
            api_key="sk-or-test",
            default_model="test-model",
            site_name="Test App",
        )
        data = config.model_dump()
        assert data["api_key"] == "sk-or-test"
        assert data["default_model"] == "test-model"
        assert data["site_name"] == "Test App"
        assert data["provider_type"] == "openrouter"

    def test_openrouter_from_dict(self) -> None:
        """Test creating OpenRouterConfig from dict."""
        data = {
            "api_key": "sk-or-test",
            "default_model": "test-model",
            "temperature": 0.5,
        }
        config = OpenRouterConfig.model_validate(data)
        assert config.api_key == "sk-or-test"
        assert config.temperature == 0.5

    def test_config_exclude_unset(self) -> None:
        """Test serializing with exclude_unset."""
        config = OpenRouterConfig(
            api_key="sk-or-test",
            default_model="test-model",
        )
        data = config.model_dump(exclude_unset=True)
        # site_url and site_name should not be in output
        assert "site_url" not in data or data["site_url"] is None
        assert "site_name" not in data or data["site_name"] is None
