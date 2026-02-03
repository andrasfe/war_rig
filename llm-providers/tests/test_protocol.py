"""Tests for the LLM Provider protocol and data classes.

This module tests the core protocol definitions including:
- Message dataclass creation and role validation
- CompletionResponse creation and properties
- LLMProvider protocol isinstance() checks
"""

from typing import Any

import pytest

from llm_providers.protocol import (
    CompletionResponse,
    LLMProvider,
    Message,
)


class TestMessage:
    """Tests for the Message dataclass."""

    def test_create_message_user_role(self) -> None:
        """Test creating a message with user role."""
        msg = Message(role="user", content="Hello, world!")
        assert msg.role == "user"
        assert msg.content == "Hello, world!"

    def test_create_message_system_role(self) -> None:
        """Test creating a message with system role."""
        msg = Message(role="system", content="You are a helpful assistant.")
        assert msg.role == "system"
        assert msg.content == "You are a helpful assistant."

    def test_create_message_assistant_role(self) -> None:
        """Test creating a message with assistant role."""
        msg = Message(role="assistant", content="I can help with that.")
        assert msg.role == "assistant"
        assert msg.content == "I can help with that."

    def test_create_message_empty_content(self) -> None:
        """Test creating a message with empty content is allowed."""
        msg = Message(role="user", content="")
        assert msg.role == "user"
        assert msg.content == ""

    def test_invalid_role_raises_value_error(self) -> None:
        """Test that an invalid role raises ValueError."""
        with pytest.raises(ValueError) as exc_info:
            Message(role="invalid", content="Hello")
        assert "Invalid role 'invalid'" in str(exc_info.value)
        assert "system" in str(exc_info.value)
        assert "user" in str(exc_info.value)
        assert "assistant" in str(exc_info.value)

    def test_invalid_role_admin_raises(self) -> None:
        """Test that 'admin' role raises ValueError."""
        with pytest.raises(ValueError) as exc_info:
            Message(role="admin", content="Admin message")
        assert "Invalid role 'admin'" in str(exc_info.value)

    def test_invalid_role_tool_raises(self) -> None:
        """Test that 'tool' role raises ValueError."""
        with pytest.raises(ValueError) as exc_info:
            Message(role="tool", content="Tool output")
        assert "Invalid role 'tool'" in str(exc_info.value)

    def test_message_is_frozen(self) -> None:
        """Test that Message is immutable (frozen dataclass)."""
        msg = Message(role="user", content="Hello")
        with pytest.raises(AttributeError):
            msg.role = "system"  # type: ignore
        with pytest.raises(AttributeError):
            msg.content = "New content"  # type: ignore

    def test_message_equality(self) -> None:
        """Test that messages with same content are equal."""
        msg1 = Message(role="user", content="Hello")
        msg2 = Message(role="user", content="Hello")
        assert msg1 == msg2

    def test_message_inequality(self) -> None:
        """Test that messages with different content are not equal."""
        msg1 = Message(role="user", content="Hello")
        msg2 = Message(role="user", content="Goodbye")
        msg3 = Message(role="system", content="Hello")
        assert msg1 != msg2
        assert msg1 != msg3

    def test_message_hash(self) -> None:
        """Test that Messages can be used in sets and as dict keys."""
        msg1 = Message(role="user", content="Hello")
        msg2 = Message(role="user", content="Hello")
        msg3 = Message(role="user", content="Goodbye")

        msg_set = {msg1, msg2, msg3}
        assert len(msg_set) == 2

        msg_dict = {msg1: "first", msg3: "second"}
        assert msg_dict[msg2] == "first"

    def test_message_repr(self) -> None:
        """Test message string representation."""
        msg = Message(role="user", content="Test")
        repr_str = repr(msg)
        assert "Message" in repr_str
        assert "user" in repr_str
        assert "Test" in repr_str


class TestCompletionResponse:
    """Tests for the CompletionResponse dataclass."""

    def test_create_completion_response(self) -> None:
        """Test creating a basic CompletionResponse."""
        response = CompletionResponse(
            content="The answer is 42.",
            model="anthropic/claude-sonnet-4-20250514",
            tokens_used=150,
        )
        assert response.content == "The answer is 42."
        assert response.model == "anthropic/claude-sonnet-4-20250514"
        assert response.tokens_used == 150
        assert response.raw_response is None

    def test_create_completion_response_with_raw_response(self) -> None:
        """Test creating a CompletionResponse with raw_response."""
        raw = {
            "id": "gen-123",
            "model": "anthropic/claude-sonnet-4-20250514",
            "created": 1704067200,
        }
        response = CompletionResponse(
            content="Hello",
            model="anthropic/claude-sonnet-4-20250514",
            tokens_used=10,
            raw_response=raw,
        )
        assert response.raw_response == raw
        assert response.raw_response["id"] == "gen-123"

    def test_has_content_true_for_non_empty(self) -> None:
        """Test has_content returns True for non-empty content."""
        response = CompletionResponse(
            content="Some content",
            model="test-model",
            tokens_used=10,
        )
        assert response.has_content is True

    def test_has_content_false_for_empty_string(self) -> None:
        """Test has_content returns False for empty string."""
        response = CompletionResponse(
            content="",
            model="test-model",
            tokens_used=0,
        )
        assert response.has_content is False

    def test_has_content_false_for_whitespace_only(self) -> None:
        """Test has_content returns False for whitespace-only content."""
        response = CompletionResponse(
            content="   \n\t  ",
            model="test-model",
            tokens_used=0,
        )
        assert response.has_content is False

    def test_has_content_true_with_leading_trailing_whitespace(self) -> None:
        """Test has_content returns True when content has leading/trailing whitespace."""
        response = CompletionResponse(
            content="  Hello  ",
            model="test-model",
            tokens_used=5,
        )
        assert response.has_content is True

    def test_completion_response_mutable(self) -> None:
        """Test that CompletionResponse is mutable (not frozen)."""
        response = CompletionResponse(
            content="Original",
            model="test-model",
            tokens_used=10,
        )
        response.content = "Modified"
        assert response.content == "Modified"

    def test_zero_tokens_used(self) -> None:
        """Test CompletionResponse with zero tokens."""
        response = CompletionResponse(
            content="",
            model="test-model",
            tokens_used=0,
        )
        assert response.tokens_used == 0

    def test_large_token_count(self) -> None:
        """Test CompletionResponse with large token count."""
        response = CompletionResponse(
            content="Large response",
            model="test-model",
            tokens_used=100000,
        )
        assert response.tokens_used == 100000


class TestLLMProviderProtocol:
    """Tests for the LLMProvider protocol."""

    def test_protocol_is_runtime_checkable(self) -> None:
        """Test that LLMProvider can be used with isinstance()."""
        class ValidProvider:
            @property
            def default_model(self) -> str:
                return "test-model"

            async def complete(
                self,
                messages: list[Message],
                model: str | None = None,
                temperature: float = 0.7,
                **kwargs: Any,
            ) -> CompletionResponse:
                return CompletionResponse(
                    content="test",
                    model=self.default_model,
                    tokens_used=10,
                )

        provider = ValidProvider()
        assert isinstance(provider, LLMProvider)

    def test_class_without_default_model_not_recognized(self) -> None:
        """Test that a class missing default_model is not recognized as LLMProvider."""
        class InvalidProvider:
            async def complete(
                self,
                messages: list[Message],
                model: str | None = None,
                temperature: float = 0.7,
                **kwargs: Any,
            ) -> CompletionResponse:
                return CompletionResponse(
                    content="test",
                    model="test-model",
                    tokens_used=10,
                )

        provider = InvalidProvider()
        assert not isinstance(provider, LLMProvider)

    def test_class_without_complete_not_recognized(self) -> None:
        """Test that a class missing complete() is not recognized as LLMProvider."""
        class InvalidProvider:
            @property
            def default_model(self) -> str:
                return "test-model"

        provider = InvalidProvider()
        assert not isinstance(provider, LLMProvider)

    def test_minimal_valid_provider(self) -> None:
        """Test that a minimal implementation satisfies the protocol."""
        class MinimalProvider:
            @property
            def default_model(self) -> str:
                return "minimal-model"

            async def complete(
                self,
                messages: list[Message],
                model: str | None = None,
                temperature: float = 0.7,
                **kwargs: Any,
            ) -> CompletionResponse:
                return CompletionResponse(
                    content="",
                    model=model or self.default_model,
                    tokens_used=0,
                )

        provider = MinimalProvider()
        assert isinstance(provider, LLMProvider)
        assert provider.default_model == "minimal-model"

    def test_provider_with_extra_methods(self) -> None:
        """Test that providers with extra methods still satisfy protocol."""
        class ExtendedProvider:
            @property
            def default_model(self) -> str:
                return "extended-model"

            async def complete(
                self,
                messages: list[Message],
                model: str | None = None,
                temperature: float = 0.7,
                **kwargs: Any,
            ) -> CompletionResponse:
                return CompletionResponse(
                    content="",
                    model=model or self.default_model,
                    tokens_used=0,
                )

            def extra_method(self) -> str:
                """Additional method that doesn't affect protocol."""
                return "extra"

            @property
            def extra_property(self) -> int:
                """Additional property."""
                return 42

        provider = ExtendedProvider()
        assert isinstance(provider, LLMProvider)
