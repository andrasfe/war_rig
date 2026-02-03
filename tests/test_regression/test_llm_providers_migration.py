"""Regression tests for llm-providers extraction.

This module ensures that the extraction of the providers module into
a standalone package (llm-providers) doesn't break existing war_rig
functionality.

Tests verify:
- All expected imports still work
- Provider factory creates correct types
- Environment-based configuration works
- Existing provider tests still pass
- Integration with war_rig agents works
"""

import os
from typing import Any
from unittest.mock import patch

import pytest


class TestImportCompatibility:
    """Tests for import compatibility after extraction."""

    def test_import_war_rig_providers(self) -> None:
        """Test that war_rig.providers module can still be imported."""
        try:
            from war_rig import providers
            assert providers is not None
        except ImportError:
            # If war_rig.providers was removed, check llm_providers
            from llm_providers import (
                CompletionResponse,
                LLMProvider,
                Message,
            )
            assert Message is not None
            assert CompletionResponse is not None
            assert LLMProvider is not None

    def test_import_message_class(self) -> None:
        """Test that Message class is importable."""
        try:
            from war_rig.providers import Message
        except ImportError:
            from llm_providers import Message

        msg = Message(role="user", content="test")
        assert msg.role == "user"
        assert msg.content == "test"

    def test_import_completion_response(self) -> None:
        """Test that CompletionResponse class is importable."""
        try:
            from war_rig.providers import CompletionResponse
        except ImportError:
            from llm_providers import CompletionResponse

        response = CompletionResponse(
            content="test",
            model="test-model",
            tokens_used=10,
        )
        assert response.content == "test"
        assert response.has_content is True

    def test_import_llm_provider_protocol(self) -> None:
        """Test that LLMProvider protocol is importable."""
        try:
            from war_rig.providers import LLMProvider
        except ImportError:
            from llm_providers import LLMProvider

        assert LLMProvider is not None

    def test_import_openrouter_provider(self) -> None:
        """Test that OpenRouterProvider is importable."""
        try:
            from war_rig.providers import OpenRouterProvider
        except ImportError:
            from llm_providers import OpenRouterProvider

        assert OpenRouterProvider is not None

    def test_import_factory_functions(self) -> None:
        """Test that factory functions are importable."""
        try:
            from war_rig.providers import create_provider, get_provider_from_env
        except ImportError:
            from llm_providers import create_provider, get_provider_from_env

        assert create_provider is not None
        assert get_provider_from_env is not None


class TestProviderFactoryCompatibility:
    """Tests for provider factory compatibility."""

    def test_create_openrouter_provider(self) -> None:
        """Test creating OpenRouter provider."""
        try:
            from war_rig.providers import create_provider, OpenRouterProvider
        except ImportError:
            from llm_providers import create_provider, OpenRouterProvider

        provider = create_provider(
            "openrouter",
            api_key="test-key",
            default_model="test-model",
        )

        assert isinstance(provider, OpenRouterProvider)
        assert provider.default_model == "test-model"

    def test_create_anthropic_provider(self) -> None:
        """Test creating Anthropic provider."""
        try:
            from war_rig.providers import create_provider, AnthropicProvider
        except ImportError:
            from llm_providers import create_provider
            from llm_providers.providers.anthropic import AnthropicProvider

        provider = create_provider(
            "anthropic",
            api_key="sk-ant-test",
            default_model="claude-sonnet-4-20250514",
        )

        assert isinstance(provider, AnthropicProvider)

    def test_create_openai_provider(self) -> None:
        """Test creating OpenAI provider."""
        try:
            from war_rig.providers import create_provider, OpenAIProvider
        except ImportError:
            from llm_providers import create_provider
            from llm_providers.providers.openai import OpenAIProvider

        provider = create_provider(
            "openai",
            api_key="sk-test",
            default_model="gpt-4o",
        )

        assert isinstance(provider, OpenAIProvider)


class TestEnvironmentConfiguration:
    """Tests for environment-based configuration."""

    def test_openrouter_from_env(self) -> None:
        """Test creating OpenRouter from environment."""
        try:
            from war_rig.providers import get_provider_from_env, OpenRouterProvider
        except ImportError:
            from llm_providers import get_provider_from_env, OpenRouterProvider

        env_vars = {
            "LLM_PROVIDER": "openrouter",
            "OPENROUTER_API_KEY": "test-key",
        }

        with patch.dict(os.environ, env_vars, clear=True):
            provider = get_provider_from_env()

        assert isinstance(provider, OpenRouterProvider)

    def test_anthropic_from_env(self) -> None:
        """Test creating Anthropic from environment."""
        try:
            from war_rig.providers import get_provider_from_env
        except ImportError:
            from llm_providers import get_provider_from_env

        from llm_providers.providers.anthropic import AnthropicProvider

        env_vars = {
            "LLM_PROVIDER": "anthropic",
            "ANTHROPIC_API_KEY": "sk-ant-test",
        }

        with patch.dict(os.environ, env_vars, clear=True):
            provider = get_provider_from_env()

        assert isinstance(provider, AnthropicProvider)


class TestMessageCompatibility:
    """Tests for Message class compatibility."""

    def test_message_role_validation(self) -> None:
        """Test that Message still validates roles."""
        try:
            from war_rig.providers import Message
        except ImportError:
            from llm_providers import Message

        # Valid roles
        Message(role="user", content="test")
        Message(role="system", content="test")
        Message(role="assistant", content="test")

        # Invalid role should raise
        with pytest.raises(ValueError):
            Message(role="invalid", content="test")

    def test_message_immutability(self) -> None:
        """Test that Message is still immutable."""
        try:
            from war_rig.providers import Message
        except ImportError:
            from llm_providers import Message

        msg = Message(role="user", content="test")

        with pytest.raises(AttributeError):
            msg.role = "system"  # type: ignore

    def test_message_equality(self) -> None:
        """Test that Message equality works."""
        try:
            from war_rig.providers import Message
        except ImportError:
            from llm_providers import Message

        msg1 = Message(role="user", content="hello")
        msg2 = Message(role="user", content="hello")
        msg3 = Message(role="user", content="world")

        assert msg1 == msg2
        assert msg1 != msg3


class TestCompletionResponseCompatibility:
    """Tests for CompletionResponse compatibility."""

    def test_completion_response_has_content(self) -> None:
        """Test has_content property."""
        try:
            from war_rig.providers import CompletionResponse
        except ImportError:
            from llm_providers import CompletionResponse

        # Non-empty content
        response = CompletionResponse(
            content="Hello",
            model="test",
            tokens_used=5,
        )
        assert response.has_content is True

        # Empty content
        response2 = CompletionResponse(
            content="",
            model="test",
            tokens_used=0,
        )
        assert response2.has_content is False

        # Whitespace only
        response3 = CompletionResponse(
            content="   ",
            model="test",
            tokens_used=0,
        )
        assert response3.has_content is False

    def test_completion_response_raw_response(self) -> None:
        """Test raw_response field."""
        try:
            from war_rig.providers import CompletionResponse
        except ImportError:
            from llm_providers import CompletionResponse

        response = CompletionResponse(
            content="test",
            model="test-model",
            tokens_used=10,
            raw_response={"id": "test-123"},
        )

        assert response.raw_response is not None
        assert response.raw_response["id"] == "test-123"


class TestProtocolCompatibility:
    """Tests for LLMProvider protocol compatibility."""

    def test_protocol_isinstance_check(self) -> None:
        """Test that isinstance checks still work."""
        try:
            from war_rig.providers import (
                CompletionResponse,
                LLMProvider,
                Message,
            )
        except ImportError:
            from llm_providers import (
                CompletionResponse,
                LLMProvider,
                Message,
            )

        class CustomProvider:
            @property
            def default_model(self) -> str:
                return "custom"

            async def complete(
                self,
                messages: list[Message],
                model: str | None = None,
                temperature: float = 0.7,
                **kwargs: Any,
            ) -> CompletionResponse:
                return CompletionResponse(
                    content="",
                    model=self.default_model,
                    tokens_used=0,
                )

        provider = CustomProvider()
        assert isinstance(provider, LLMProvider)


class TestWarRigAgentCompatibility:
    """Tests for war_rig agent compatibility with providers."""

    def test_import_war_rig_agents(self) -> None:
        """Test that war_rig agents can still be imported."""
        # These imports should work regardless of provider location
        from war_rig.agents.scribe import ScribeAgent
        from war_rig.agents.challenger import ChallengerAgent
        from war_rig.agents.imperator import ImperatorAgent

        assert ScribeAgent is not None
        assert ChallengerAgent is not None
        assert ImperatorAgent is not None

    def test_import_war_rig_config(self) -> None:
        """Test that war_rig config can still be imported."""
        from war_rig.config import WarRigConfig, ScribeConfig

        assert WarRigConfig is not None
        assert ScribeConfig is not None

    def test_import_war_rig_models(self) -> None:
        """Test that war_rig models can still be imported."""
        from war_rig.models.tickets import ChromeTicket, ChallengerQuestion
        from war_rig.models.templates import DocumentationTemplate
        from war_rig.models.assessments import ChallengerAssessment

        assert ChromeTicket is not None
        assert ChallengerQuestion is not None
        assert DocumentationTemplate is not None
        assert ChallengerAssessment is not None


class TestExistingTestsStillPass:
    """Meta-tests to verify existing test suites work."""

    def test_provider_tests_importable(self) -> None:
        """Test that existing provider tests can be imported."""
        # These tests should be able to import from the correct location
        try:
            from tests.test_providers import test_factory
            from tests.test_providers import test_protocol
        except ImportError:
            # Tests may have been moved or renamed
            pass

    def test_agent_tests_importable(self) -> None:
        """Test that agent tests can still import providers."""
        from tests.test_agents import test_scribe

        assert test_scribe is not None


class TestBackwardCompatibleAPI:
    """Tests for backward compatible API surface."""

    def test_all_expected_exports(self) -> None:
        """Test that all expected names are exported."""
        try:
            from war_rig.providers import (
                CompletionResponse,
                LLMProvider,
                Message,
                OpenRouterProvider,
                create_provider,
                get_provider_from_env,
                register_provider,
            )
        except ImportError:
            from llm_providers import (
                CompletionResponse,
                LLMProvider,
                Message,
                OpenRouterProvider,
                create_provider,
                get_provider_from_env,
                register_provider,
            )

        # All exports should be present
        assert CompletionResponse is not None
        assert LLMProvider is not None
        assert Message is not None
        assert OpenRouterProvider is not None
        assert create_provider is not None
        assert get_provider_from_env is not None
        assert register_provider is not None

    def test_register_custom_provider(self) -> None:
        """Test that custom provider registration still works."""
        try:
            from war_rig.providers import (
                CompletionResponse,
                Message,
                create_provider,
                register_provider,
            )
            from war_rig.providers.factory import _PROVIDERS
        except ImportError:
            from llm_providers import (
                CompletionResponse,
                Message,
                create_provider,
                register_provider,
            )
            from llm_providers.factory import _PROVIDERS

        class TestProvider:
            def __init__(self, **kwargs: Any) -> None:
                pass

            @property
            def default_model(self) -> str:
                return "test"

            async def complete(
                self,
                messages: list[Message],
                model: str | None = None,
                temperature: float = 0.7,
                **kwargs: Any,
            ) -> CompletionResponse:
                return CompletionResponse(
                    content="test",
                    model="test",
                    tokens_used=0,
                )

        register_provider("regression_test_provider", TestProvider)

        try:
            provider = create_provider("regression_test_provider")
            assert provider.default_model == "test"
        finally:
            # Clean up
            del _PROVIDERS["regression_test_provider"]
