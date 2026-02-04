"""Tests for the langchain_factory module.

This module tests:
- Provider configuration resolution from LLM_PROVIDER env var
- LangChain model creation for each provider
- API key requirement validation
- Model selection defaults
"""

from unittest.mock import MagicMock, patch

import pytest

from codewhisper.agent.langchain_factory import (
    get_available_providers,
    get_langchain_model,
)


class TestGetAvailableProviders:
    """Tests for get_available_providers function."""

    def test_returns_all_providers(self) -> None:
        """Test that all built-in providers are returned."""
        providers = get_available_providers()

        assert "openrouter" in providers
        assert "anthropic" in providers
        assert "openai" in providers
        assert len(providers) == 3


class TestGetLangchainModelOpenRouter:
    """Tests for get_langchain_model with openrouter provider."""

    def test_requires_api_key_when_llm_provider_not_set(
        self,
        monkeypatch: pytest.MonkeyPatch,
    ) -> None:
        """Test that OPENROUTER_API_KEY is required when LLM_PROVIDER not set."""
        monkeypatch.delenv("LLM_PROVIDER", raising=False)
        monkeypatch.delenv("OPENROUTER_API_KEY", raising=False)

        with pytest.raises(KeyError, match="OPENROUTER_API_KEY"):
            get_langchain_model()

    def test_requires_api_key_when_openrouter_explicit(
        self,
        monkeypatch: pytest.MonkeyPatch,
    ) -> None:
        """Test that OPENROUTER_API_KEY is required when provider=openrouter."""
        monkeypatch.delenv("OPENROUTER_API_KEY", raising=False)

        with pytest.raises(KeyError, match="OPENROUTER_API_KEY"):
            get_langchain_model(provider="openrouter")

    def test_creates_chatopenai_for_openrouter(
        self,
        monkeypatch: pytest.MonkeyPatch,
    ) -> None:
        """Test that ChatOpenAI is created for openrouter provider."""
        monkeypatch.setenv("LLM_PROVIDER", "openrouter")
        monkeypatch.setenv("OPENROUTER_API_KEY", "test-key")

        with patch("langchain_openai.ChatOpenAI") as mock_chat:
            mock_instance = MagicMock()
            mock_chat.return_value = mock_instance

            get_langchain_model()

            mock_chat.assert_called_once()
            call_kwargs = mock_chat.call_args[1]
            assert call_kwargs["api_key"] == "test-key"
            assert "openrouter.ai" in call_kwargs["base_url"]

    def test_uses_default_model_for_openrouter(
        self,
        monkeypatch: pytest.MonkeyPatch,
    ) -> None:
        """Test default model selection for openrouter."""
        monkeypatch.setenv("OPENROUTER_API_KEY", "test-key")
        monkeypatch.delenv("LLM_PROVIDER", raising=False)
        monkeypatch.delenv("IMPERATOR_MODEL", raising=False)
        monkeypatch.delenv("LLM_DEFAULT_MODEL", raising=False)

        with patch("langchain_openai.ChatOpenAI") as mock_chat:
            mock_instance = MagicMock()
            mock_chat.return_value = mock_instance

            get_langchain_model()

            call_kwargs = mock_chat.call_args[1]
            assert "claude" in call_kwargs["model"].lower()


class TestGetLangchainModelAnthropic:
    """Tests for get_langchain_model with anthropic provider."""

    def test_requires_api_key_when_anthropic(
        self,
        monkeypatch: pytest.MonkeyPatch,
    ) -> None:
        """Test that ANTHROPIC_API_KEY is required when LLM_PROVIDER=anthropic."""
        monkeypatch.setenv("LLM_PROVIDER", "anthropic")
        monkeypatch.delenv("ANTHROPIC_API_KEY", raising=False)

        with pytest.raises(KeyError, match="ANTHROPIC_API_KEY"):
            get_langchain_model()

    def test_requires_api_key_when_anthropic_explicit(
        self,
        monkeypatch: pytest.MonkeyPatch,
    ) -> None:
        """Test that ANTHROPIC_API_KEY is required when provider=anthropic."""
        monkeypatch.delenv("ANTHROPIC_API_KEY", raising=False)

        with pytest.raises(KeyError, match="ANTHROPIC_API_KEY"):
            get_langchain_model(provider="anthropic")

    def test_creates_chatanthropic_for_anthropic(
        self,
        monkeypatch: pytest.MonkeyPatch,
    ) -> None:
        """Test that ChatAnthropic is created for anthropic provider."""
        monkeypatch.setenv("LLM_PROVIDER", "anthropic")
        monkeypatch.setenv("ANTHROPIC_API_KEY", "test-anthropic-key")

        with patch("langchain_anthropic.ChatAnthropic") as mock_chat:
            mock_instance = MagicMock()
            mock_chat.return_value = mock_instance

            get_langchain_model()

            mock_chat.assert_called_once()
            call_kwargs = mock_chat.call_args[1]
            assert call_kwargs["api_key"] == "test-anthropic-key"

    def test_uses_default_model_for_anthropic(
        self,
        monkeypatch: pytest.MonkeyPatch,
    ) -> None:
        """Test default model selection for anthropic."""
        monkeypatch.setenv("LLM_PROVIDER", "anthropic")
        monkeypatch.setenv("ANTHROPIC_API_KEY", "test-key")
        monkeypatch.delenv("IMPERATOR_MODEL", raising=False)
        monkeypatch.delenv("LLM_DEFAULT_MODEL", raising=False)

        with patch("langchain_anthropic.ChatAnthropic") as mock_chat:
            mock_instance = MagicMock()
            mock_chat.return_value = mock_instance

            get_langchain_model()

            call_kwargs = mock_chat.call_args[1]
            assert "claude" in call_kwargs["model"].lower()


class TestGetLangchainModelOpenAI:
    """Tests for get_langchain_model with openai provider."""

    def test_requires_api_key_when_openai(
        self,
        monkeypatch: pytest.MonkeyPatch,
    ) -> None:
        """Test that OPENAI_API_KEY is required when LLM_PROVIDER=openai."""
        monkeypatch.setenv("LLM_PROVIDER", "openai")
        monkeypatch.delenv("OPENAI_API_KEY", raising=False)

        with pytest.raises(KeyError, match="OPENAI_API_KEY"):
            get_langchain_model()

    def test_requires_api_key_when_openai_explicit(
        self,
        monkeypatch: pytest.MonkeyPatch,
    ) -> None:
        """Test that OPENAI_API_KEY is required when provider=openai."""
        monkeypatch.delenv("OPENAI_API_KEY", raising=False)

        with pytest.raises(KeyError, match="OPENAI_API_KEY"):
            get_langchain_model(provider="openai")

    def test_creates_chatopenai_for_openai(
        self,
        monkeypatch: pytest.MonkeyPatch,
    ) -> None:
        """Test that ChatOpenAI is created for openai provider."""
        monkeypatch.setenv("LLM_PROVIDER", "openai")
        monkeypatch.setenv("OPENAI_API_KEY", "test-openai-key")

        with patch("langchain_openai.ChatOpenAI") as mock_chat:
            mock_instance = MagicMock()
            mock_chat.return_value = mock_instance

            get_langchain_model()

            mock_chat.assert_called_once()
            call_kwargs = mock_chat.call_args[1]
            assert call_kwargs["api_key"] == "test-openai-key"
            # OpenAI should NOT have base_url set (unlike openrouter)
            assert "base_url" not in call_kwargs or call_kwargs["base_url"] is None

    def test_uses_default_model_for_openai(
        self,
        monkeypatch: pytest.MonkeyPatch,
    ) -> None:
        """Test default model selection for openai."""
        monkeypatch.setenv("LLM_PROVIDER", "openai")
        monkeypatch.setenv("OPENAI_API_KEY", "test-key")
        monkeypatch.delenv("IMPERATOR_MODEL", raising=False)
        monkeypatch.delenv("LLM_DEFAULT_MODEL", raising=False)

        with patch("langchain_openai.ChatOpenAI") as mock_chat:
            mock_instance = MagicMock()
            mock_chat.return_value = mock_instance

            get_langchain_model()

            call_kwargs = mock_chat.call_args[1]
            assert "gpt" in call_kwargs["model"].lower()


class TestGetLangchainModelProviderOverride:
    """Tests for explicit provider override."""

    def test_explicit_provider_overrides_env(
        self,
        monkeypatch: pytest.MonkeyPatch,
    ) -> None:
        """Test that explicit provider parameter overrides LLM_PROVIDER."""
        monkeypatch.setenv("LLM_PROVIDER", "openrouter")
        monkeypatch.setenv("ANTHROPIC_API_KEY", "test-anthropic-key")

        with patch("langchain_anthropic.ChatAnthropic") as mock_chat:
            mock_instance = MagicMock()
            mock_chat.return_value = mock_instance

            # Explicitly request anthropic even though LLM_PROVIDER=openrouter
            get_langchain_model(provider="anthropic")

            mock_chat.assert_called_once()

    def test_explicit_model_overrides_default(
        self,
        monkeypatch: pytest.MonkeyPatch,
    ) -> None:
        """Test that explicit model parameter overrides default."""
        monkeypatch.setenv("ANTHROPIC_API_KEY", "test-key")

        with patch("langchain_anthropic.ChatAnthropic") as mock_chat:
            mock_instance = MagicMock()
            mock_chat.return_value = mock_instance

            get_langchain_model(provider="anthropic", model="claude-3-opus-20240229")

            call_kwargs = mock_chat.call_args[1]
            assert call_kwargs["model"] == "claude-3-opus-20240229"


class TestGetLangchainModelUnknownProvider:
    """Tests for unknown provider handling."""

    def test_unknown_provider_in_env_raises(
        self,
        monkeypatch: pytest.MonkeyPatch,
    ) -> None:
        """Test that unknown LLM_PROVIDER raises ValueError."""
        monkeypatch.setenv("LLM_PROVIDER", "unknown_provider")

        with pytest.raises(ValueError, match="Unknown provider"):
            get_langchain_model()

    def test_unknown_explicit_provider_raises(
        self,
        monkeypatch: pytest.MonkeyPatch,
    ) -> None:
        """Test that unknown explicit provider raises ValueError."""
        with pytest.raises(ValueError, match="Unknown provider"):
            get_langchain_model(provider="nonexistent")


class TestGetLangchainModelCustomSettings:
    """Tests for custom temperature and max_tokens."""

    def test_custom_temperature(
        self,
        monkeypatch: pytest.MonkeyPatch,
    ) -> None:
        """Test custom temperature is passed through."""
        monkeypatch.setenv("ANTHROPIC_API_KEY", "test-key")

        with patch("langchain_anthropic.ChatAnthropic") as mock_chat:
            mock_instance = MagicMock()
            mock_chat.return_value = mock_instance

            get_langchain_model(provider="anthropic", temperature=0.9)

            call_kwargs = mock_chat.call_args[1]
            assert call_kwargs["temperature"] == 0.9

    def test_custom_max_tokens(
        self,
        monkeypatch: pytest.MonkeyPatch,
    ) -> None:
        """Test custom max_tokens is passed through."""
        monkeypatch.setenv("ANTHROPIC_API_KEY", "test-key")

        with patch("langchain_anthropic.ChatAnthropic") as mock_chat:
            mock_instance = MagicMock()
            mock_chat.return_value = mock_instance

            get_langchain_model(provider="anthropic", max_tokens=8192)

            call_kwargs = mock_chat.call_args[1]
            assert call_kwargs["max_tokens"] == 8192
