"""Tests for the Anthropic provider implementation.

This module tests:
- Provider initialization and configuration
- Message conversion with system message handling
- Streaming API call handling with mocked responses
- Error handling for various failure modes
"""

from collections.abc import AsyncIterator
from typing import Any
from unittest.mock import MagicMock, patch

import pytest
from anthropic import APIConnectionError, APIStatusError, RateLimitError

from llm_providers.protocol import CompletionResponse, Message
from llm_providers.providers.anthropic import (
    DEFAULT_MAX_TOKENS,
    DEFAULT_MODEL,
    DEFAULT_TIMEOUT_SECONDS,
    AnthropicProvider,
    AnthropicProviderError,
)


class TestAnthropicProviderInit:
    """Tests for AnthropicProvider initialization."""

    def test_init_with_required_params(self) -> None:
        """Test initialization with only required parameters."""
        provider = AnthropicProvider(api_key="sk-ant-test-key")

        assert provider._api_key == "sk-ant-test-key"
        assert provider._default_model == DEFAULT_MODEL
        assert provider._max_tokens == DEFAULT_MAX_TOKENS
        assert provider._timeout_seconds == DEFAULT_TIMEOUT_SECONDS

    def test_init_with_custom_model(self) -> None:
        """Test initialization with custom default model."""
        provider = AnthropicProvider(
            api_key="sk-ant-test",
            default_model="claude-opus-4-20250514",
        )

        assert provider.default_model == "claude-opus-4-20250514"

    def test_init_with_custom_max_tokens(self) -> None:
        """Test initialization with custom max_tokens."""
        provider = AnthropicProvider(
            api_key="sk-ant-test",
            max_tokens=8192,
        )

        assert provider._max_tokens == 8192

    def test_init_with_custom_base_url(self) -> None:
        """Test initialization with custom base URL."""
        provider = AnthropicProvider(
            api_key="sk-ant-test",
            base_url="https://anthropic-proxy.example.com",
        )

        # Base URL is passed to the client
        assert provider._client._base_url is not None

    def test_init_with_custom_timeout(self) -> None:
        """Test initialization with custom timeout."""
        provider = AnthropicProvider(
            api_key="sk-ant-test",
            timeout_seconds=120.0,
        )

        assert provider._timeout_seconds == 120.0

    def test_default_model_property(self) -> None:
        """Test the default_model property."""
        provider = AnthropicProvider(
            api_key="sk-ant-test",
            default_model="claude-sonnet-4-20250514",
        )

        assert provider.default_model == "claude-sonnet-4-20250514"


class MockStreamContextManager:
    """Mock context manager for Anthropic streaming."""

    def __init__(self, events: list) -> None:
        """Initialize with list of events to yield.

        Args:
            events: List of mock event objects.
        """
        self.events = events

    async def __aenter__(self) -> "MockStreamContextManager":
        """Enter the async context."""
        return self

    async def __aexit__(self, exc_type: Any, exc_val: Any, exc_tb: Any) -> None:
        """Exit the async context."""
        pass

    def __aiter__(self) -> AsyncIterator:
        """Return async iterator over events."""
        return self._generate_events()

    async def _generate_events(self) -> AsyncIterator:
        """Generate events."""
        for event in self.events:
            yield event


def create_anthropic_stream_events(
    text_chunks: list[str],
    input_tokens: int = 20,
    output_tokens: int = 10,
) -> list:
    """Create mock Anthropic streaming events.

    Args:
        text_chunks: List of text content to include in deltas.
        input_tokens: Input token count for message_start.
        output_tokens: Output token count for message_delta.

    Returns:
        List of mock event objects.
    """
    events = []

    # message_start event with input tokens
    message_start = MagicMock()
    message_start.type = "message_start"
    message_start.message = MagicMock()
    message_start.message.usage = MagicMock()
    message_start.message.usage.input_tokens = input_tokens
    events.append(message_start)

    # content_block_delta events with text
    for text in text_chunks:
        delta = MagicMock()
        delta.type = "content_block_delta"
        delta.delta = MagicMock()
        delta.delta.text = text
        events.append(delta)

    # message_delta event with output tokens
    message_delta = MagicMock()
    message_delta.type = "message_delta"
    message_delta.usage = MagicMock()
    message_delta.usage.output_tokens = output_tokens
    events.append(message_delta)

    return events


class TestAnthropicProviderComplete:
    """Tests for the complete() method."""

    @pytest.fixture
    def provider(self) -> AnthropicProvider:
        """Create a provider instance for testing."""
        return AnthropicProvider(
            api_key="sk-ant-test-key",
            default_model="test-model",
        )

    @pytest.fixture
    def sample_messages_with_system(self) -> list[Message]:
        """Sample messages including system message."""
        return [
            Message(role="system", content="You are helpful."),
            Message(role="user", content="Hello!"),
        ]

    @pytest.fixture
    def sample_messages_without_system(self) -> list[Message]:
        """Sample messages without system message."""
        return [
            Message(role="user", content="Hello!"),
            Message(role="assistant", content="Hi there!"),
            Message(role="user", content="How are you?"),
        ]

    async def test_complete_success(
        self,
        provider: AnthropicProvider,
        sample_messages_with_system: list[Message],
    ) -> None:
        """Test successful completion with streaming."""
        events = create_anthropic_stream_events(["Hello! ", "I'm doing well."])

        with patch.object(
            provider._client.messages,
            "stream",
            return_value=MockStreamContextManager(events),
        ):
            response = await provider.complete(sample_messages_with_system)

        assert isinstance(response, CompletionResponse)
        assert response.content == "Hello! I'm doing well."
        assert response.tokens_used == 30  # 20 input + 10 output

    async def test_complete_extracts_system_message(
        self,
        provider: AnthropicProvider,
        sample_messages_with_system: list[Message],
    ) -> None:
        """Test that system message is extracted and passed separately."""
        events = create_anthropic_stream_events(["Response"])

        with patch.object(
            provider._client.messages,
            "stream",
            return_value=MockStreamContextManager(events),
        ) as mock_stream:
            await provider.complete(sample_messages_with_system)

        call_kwargs = mock_stream.call_args[1]
        assert call_kwargs["system"] == "You are helpful."
        # Only user message should remain in messages list
        assert call_kwargs["messages"] == [{"role": "user", "content": "Hello!"}]

    async def test_complete_without_system_message(
        self,
        provider: AnthropicProvider,
        sample_messages_without_system: list[Message],
    ) -> None:
        """Test completion without system message."""
        events = create_anthropic_stream_events(["Response"])

        with patch.object(
            provider._client.messages,
            "stream",
            return_value=MockStreamContextManager(events),
        ) as mock_stream:
            await provider.complete(sample_messages_without_system)

        call_kwargs = mock_stream.call_args[1]
        assert "system" not in call_kwargs
        assert len(call_kwargs["messages"]) == 3

    async def test_complete_uses_default_model(
        self,
        provider: AnthropicProvider,
        sample_messages_with_system: list[Message],
    ) -> None:
        """Test that default model is used when not specified."""
        events = create_anthropic_stream_events(["Response"])

        with patch.object(
            provider._client.messages,
            "stream",
            return_value=MockStreamContextManager(events),
        ) as mock_stream:
            await provider.complete(sample_messages_with_system)

        call_kwargs = mock_stream.call_args[1]
        assert call_kwargs["model"] == "test-model"

    async def test_complete_uses_override_model(
        self,
        provider: AnthropicProvider,
        sample_messages_with_system: list[Message],
    ) -> None:
        """Test that model parameter overrides default."""
        events = create_anthropic_stream_events(["Response"])

        with patch.object(
            provider._client.messages,
            "stream",
            return_value=MockStreamContextManager(events),
        ) as mock_stream:
            await provider.complete(
                sample_messages_with_system,
                model="claude-opus-4-20250514",
            )

        call_kwargs = mock_stream.call_args[1]
        assert call_kwargs["model"] == "claude-opus-4-20250514"

    async def test_complete_uses_default_max_tokens(
        self,
        provider: AnthropicProvider,
        sample_messages_with_system: list[Message],
    ) -> None:
        """Test that default max_tokens is used."""
        events = create_anthropic_stream_events(["Response"])

        with patch.object(
            provider._client.messages,
            "stream",
            return_value=MockStreamContextManager(events),
        ) as mock_stream:
            await provider.complete(sample_messages_with_system)

        call_kwargs = mock_stream.call_args[1]
        assert call_kwargs["max_tokens"] == DEFAULT_MAX_TOKENS

    async def test_complete_override_max_tokens(
        self,
        provider: AnthropicProvider,
        sample_messages_with_system: list[Message],
    ) -> None:
        """Test that max_tokens can be overridden via kwargs."""
        events = create_anthropic_stream_events(["Response"])

        with patch.object(
            provider._client.messages,
            "stream",
            return_value=MockStreamContextManager(events),
        ) as mock_stream:
            await provider.complete(
                sample_messages_with_system,
                max_tokens=1000,
            )

        call_kwargs = mock_stream.call_args[1]
        assert call_kwargs["max_tokens"] == 1000

    async def test_complete_passes_temperature(
        self,
        provider: AnthropicProvider,
        sample_messages_with_system: list[Message],
    ) -> None:
        """Test that temperature is passed correctly."""
        events = create_anthropic_stream_events(["Response"])

        with patch.object(
            provider._client.messages,
            "stream",
            return_value=MockStreamContextManager(events),
        ) as mock_stream:
            await provider.complete(
                sample_messages_with_system,
                temperature=0.5,
            )

        call_kwargs = mock_stream.call_args[1]
        assert call_kwargs["temperature"] == 0.5

    async def test_complete_streaming_enabled(
        self,
        provider: AnthropicProvider,
        sample_messages_with_system: list[Message],
    ) -> None:
        """Test that streaming is enabled in API calls."""
        events = create_anthropic_stream_events(["Response"])

        with patch.object(
            provider._client.messages,
            "stream",
            return_value=MockStreamContextManager(events),
        ) as mock_stream:
            await provider.complete(sample_messages_with_system)

        call_kwargs = mock_stream.call_args[1]
        assert call_kwargs["stream"] is True

    async def test_complete_handles_empty_content(
        self,
        provider: AnthropicProvider,
        sample_messages_with_system: list[Message],
    ) -> None:
        """Test handling of empty response content."""
        # Create events with no content deltas
        events = [
            MagicMock(type="message_start", message=MagicMock(usage=MagicMock(input_tokens=10))),
            MagicMock(type="message_delta", usage=MagicMock(output_tokens=0)),
        ]

        with patch.object(
            provider._client.messages,
            "stream",
            return_value=MockStreamContextManager(events),
        ):
            response = await provider.complete(sample_messages_with_system)

        assert response.content == ""
        assert response.has_content is False


class TestAnthropicProviderErrors:
    """Tests for error handling in AnthropicProvider."""

    @pytest.fixture
    def provider(self) -> AnthropicProvider:
        """Create a provider instance for testing."""
        return AnthropicProvider(api_key="sk-ant-test-key")

    @pytest.fixture
    def sample_messages(self) -> list[Message]:
        """Sample messages for testing."""
        return [Message(role="user", content="Hello!")]

    async def test_rate_limit_error(
        self,
        provider: AnthropicProvider,
        sample_messages: list[Message],
    ) -> None:
        """Test handling of rate limit errors."""
        mock_error = RateLimitError(
            message="Rate limit exceeded",
            response=MagicMock(),
            body=None,
        )

        with patch.object(
            provider._client.messages,
            "stream",
            side_effect=mock_error,
        ):
            with pytest.raises(AnthropicProviderError) as exc_info:
                await provider.complete(sample_messages)

        assert exc_info.value.status_code == 429
        assert "rate limit" in exc_info.value.message.lower()
        assert exc_info.value.original_error is mock_error

    async def test_connection_error(
        self,
        provider: AnthropicProvider,
        sample_messages: list[Message],
    ) -> None:
        """Test handling of connection errors."""
        mock_error = APIConnectionError(request=MagicMock())

        with patch.object(
            provider._client.messages,
            "stream",
            side_effect=mock_error,
        ):
            with pytest.raises(AnthropicProviderError) as exc_info:
                await provider.complete(sample_messages)

        assert "connect" in exc_info.value.message.lower()
        assert exc_info.value.original_error is mock_error

    async def test_api_status_error(
        self,
        provider: AnthropicProvider,
        sample_messages: list[Message],
    ) -> None:
        """Test handling of API status errors."""
        mock_error = APIStatusError(
            message="Internal server error",
            response=MagicMock(),
            body=None,
        )
        mock_error.status_code = 500

        with patch.object(
            provider._client.messages,
            "stream",
            side_effect=mock_error,
        ):
            with pytest.raises(AnthropicProviderError) as exc_info:
                await provider.complete(sample_messages)

        assert exc_info.value.status_code == 500
        assert exc_info.value.original_error is mock_error

    async def test_unexpected_error(
        self,
        provider: AnthropicProvider,
        sample_messages: list[Message],
    ) -> None:
        """Test handling of unexpected errors."""
        mock_error = RuntimeError("Unexpected failure")

        with patch.object(
            provider._client.messages,
            "stream",
            side_effect=mock_error,
        ):
            with pytest.raises(AnthropicProviderError) as exc_info:
                await provider.complete(sample_messages)

        assert "unexpected" in exc_info.value.message.lower()
        assert exc_info.value.original_error is mock_error


class TestAnthropicProviderError:
    """Tests for the AnthropicProviderError exception."""

    def test_error_with_message_only(self) -> None:
        """Test error with just a message."""
        error = AnthropicProviderError(message="Test error")

        assert str(error) == "Test error"
        assert error.message == "Test error"
        assert error.status_code is None
        assert error.original_error is None

    def test_error_with_status_code(self) -> None:
        """Test error with status code."""
        error = AnthropicProviderError(
            message="Rate limited",
            status_code=429,
        )

        assert error.status_code == 429

    def test_error_with_original_error(self) -> None:
        """Test error wrapping another exception."""
        original = ValueError("Original error")
        error = AnthropicProviderError(
            message="Wrapped error",
            original_error=original,
        )

        assert error.original_error is original
