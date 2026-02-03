"""Tests for the OpenRouter provider implementation.

This module tests:
- Provider initialization and configuration
- Message conversion to OpenAI format
- Streaming API call handling with mocked responses
- Error handling for various failure modes
- Reasoning model temperature handling
"""

from collections.abc import AsyncIterator
from unittest.mock import AsyncMock, MagicMock, patch

import pytest
from openai import APIConnectionError, APIError, RateLimitError

from llm_providers.protocol import CompletionResponse, Message
from llm_providers.providers.openrouter import (
    DEFAULT_BASE_URL,
    DEFAULT_MODEL,
    OpenRouterProvider,
    OpenRouterProviderError,
)


class TestOpenRouterProviderInit:
    """Tests for OpenRouterProvider initialization."""

    def test_init_with_required_params(self) -> None:
        """Test initialization with only required parameters."""
        provider = OpenRouterProvider(api_key="sk-or-test-key")

        assert provider._api_key == "sk-or-test-key"
        assert provider._base_url == DEFAULT_BASE_URL
        assert provider._default_model == DEFAULT_MODEL
        assert provider._site_url is None
        assert provider._site_name is None

    def test_init_with_custom_base_url(self) -> None:
        """Test initialization with custom base URL."""
        provider = OpenRouterProvider(
            api_key="sk-or-test",
            base_url="https://custom.api.com/v1",
        )

        assert provider._base_url == "https://custom.api.com/v1"

    def test_init_with_custom_model(self) -> None:
        """Test initialization with custom default model."""
        provider = OpenRouterProvider(
            api_key="sk-or-test",
            default_model="openai/gpt-4o",
        )

        assert provider.default_model == "openai/gpt-4o"

    def test_init_with_site_metadata(self) -> None:
        """Test initialization with site URL and name."""
        provider = OpenRouterProvider(
            api_key="sk-or-test",
            site_url="https://myapp.com",
            site_name="My Application",
        )

        assert provider._site_url == "https://myapp.com"
        assert provider._site_name == "My Application"

    def test_init_with_custom_timeout(self) -> None:
        """Test initialization with custom timeout."""
        provider = OpenRouterProvider(
            api_key="sk-or-test",
            timeout_seconds=120.0,
        )

        assert provider._timeout.read == 120.0

    def test_default_model_property(self) -> None:
        """Test the default_model property."""
        provider = OpenRouterProvider(
            api_key="sk-or-test",
            default_model="meta/llama-3-70b",
        )

        assert provider.default_model == "meta/llama-3-70b"


def create_mock_stream(chunks: list[str]) -> AsyncIterator:
    """Create a mock async iterator yielding stream chunks.

    Args:
        chunks: List of content strings to yield.

    Returns:
        An async iterator that yields mock chunk objects.
    """

    async def stream_generator() -> AsyncIterator:
        for content in chunks:
            mock_chunk = MagicMock()
            mock_chunk.choices = [MagicMock()]
            mock_chunk.choices[0].delta.content = content
            yield mock_chunk

    return stream_generator()


class TestOpenRouterProviderComplete:
    """Tests for the complete() method."""

    @pytest.fixture
    def provider(self) -> OpenRouterProvider:
        """Create a provider instance for testing."""
        return OpenRouterProvider(
            api_key="sk-or-test-key",
            default_model="test-model",
        )

    @pytest.fixture
    def sample_messages(self) -> list[Message]:
        """Sample messages for testing."""
        return [
            Message(role="system", content="You are helpful."),
            Message(role="user", content="Hello!"),
        ]

    async def test_complete_success(
        self,
        provider: OpenRouterProvider,
        sample_messages: list[Message],
    ) -> None:
        """Test successful completion with streaming."""
        with patch.object(
            provider._client.chat.completions,
            "create",
            new_callable=AsyncMock,
            return_value=create_mock_stream(["Hello! ", "How can I help?"]),
        ):
            response = await provider.complete(sample_messages)

        assert isinstance(response, CompletionResponse)
        assert response.content == "Hello! How can I help?"
        assert response.model == "test-model"

    async def test_complete_uses_default_model(
        self,
        provider: OpenRouterProvider,
        sample_messages: list[Message],
    ) -> None:
        """Test that default model is used when not specified."""
        with patch.object(
            provider._client.chat.completions,
            "create",
            new_callable=AsyncMock,
            return_value=create_mock_stream(["Response"]),
        ) as mock_create:
            await provider.complete(sample_messages)

        call_kwargs = mock_create.call_args[1]
        assert call_kwargs["model"] == "test-model"

    async def test_complete_uses_override_model(
        self,
        provider: OpenRouterProvider,
        sample_messages: list[Message],
    ) -> None:
        """Test that model parameter overrides default."""
        with patch.object(
            provider._client.chat.completions,
            "create",
            new_callable=AsyncMock,
            return_value=create_mock_stream(["Response"]),
        ) as mock_create:
            await provider.complete(sample_messages, model="custom-model")

        call_kwargs = mock_create.call_args[1]
        assert call_kwargs["model"] == "custom-model"

    async def test_complete_passes_temperature(
        self,
        provider: OpenRouterProvider,
        sample_messages: list[Message],
    ) -> None:
        """Test that temperature is passed correctly."""
        with patch.object(
            provider._client.chat.completions,
            "create",
            new_callable=AsyncMock,
            return_value=create_mock_stream(["Response"]),
        ) as mock_create:
            await provider.complete(sample_messages, temperature=0.5)

        call_kwargs = mock_create.call_args[1]
        assert call_kwargs["temperature"] == 0.5

    async def test_complete_passes_kwargs(
        self,
        provider: OpenRouterProvider,
        sample_messages: list[Message],
    ) -> None:
        """Test that additional kwargs are passed through."""
        with patch.object(
            provider._client.chat.completions,
            "create",
            new_callable=AsyncMock,
            return_value=create_mock_stream(["Response"]),
        ) as mock_create:
            await provider.complete(
                sample_messages,
                max_tokens=100,
                top_p=0.9,
            )

        call_kwargs = mock_create.call_args[1]
        assert call_kwargs["max_tokens"] == 100
        assert call_kwargs["top_p"] == 0.9

    async def test_complete_converts_messages(
        self,
        provider: OpenRouterProvider,
        sample_messages: list[Message],
    ) -> None:
        """Test that messages are converted to OpenAI format."""
        with patch.object(
            provider._client.chat.completions,
            "create",
            new_callable=AsyncMock,
            return_value=create_mock_stream(["Response"]),
        ) as mock_create:
            await provider.complete(sample_messages)

        call_kwargs = mock_create.call_args[1]
        messages = call_kwargs["messages"]
        assert messages == [
            {"role": "system", "content": "You are helpful."},
            {"role": "user", "content": "Hello!"},
        ]

    async def test_complete_streaming_enabled(
        self,
        provider: OpenRouterProvider,
        sample_messages: list[Message],
    ) -> None:
        """Test that streaming is enabled in API calls."""
        with patch.object(
            provider._client.chat.completions,
            "create",
            new_callable=AsyncMock,
            return_value=create_mock_stream(["Response"]),
        ) as mock_create:
            await provider.complete(sample_messages)

        call_kwargs = mock_create.call_args[1]
        assert call_kwargs["stream"] is True

    async def test_complete_handles_empty_chunks(
        self,
        provider: OpenRouterProvider,
        sample_messages: list[Message],
    ) -> None:
        """Test handling of empty chunks in stream."""

        async def stream_with_empty() -> AsyncIterator:
            # First chunk with content
            mock_chunk1 = MagicMock()
            mock_chunk1.choices = [MagicMock()]
            mock_chunk1.choices[0].delta.content = "Hello"
            yield mock_chunk1

            # Empty chunk (keepalive)
            mock_chunk2 = MagicMock()
            mock_chunk2.choices = [MagicMock()]
            mock_chunk2.choices[0].delta.content = None
            yield mock_chunk2

            # Another content chunk
            mock_chunk3 = MagicMock()
            mock_chunk3.choices = [MagicMock()]
            mock_chunk3.choices[0].delta.content = " World"
            yield mock_chunk3

        with patch.object(
            provider._client.chat.completions,
            "create",
            new_callable=AsyncMock,
            return_value=stream_with_empty(),
        ):
            response = await provider.complete(sample_messages)

        assert response.content == "Hello World"


class TestOpenRouterReasoningModels:
    """Tests for reasoning model temperature handling."""

    @pytest.fixture
    def provider(self) -> OpenRouterProvider:
        """Create a provider instance for testing."""
        return OpenRouterProvider(api_key="sk-or-test-key")

    @pytest.fixture
    def sample_messages(self) -> list[Message]:
        """Sample messages for testing."""
        return [Message(role="user", content="Hello!")]

    async def test_o3_model_forces_temperature_1(
        self,
        provider: OpenRouterProvider,
        sample_messages: list[Message],
    ) -> None:
        """Test that o3 model forces temperature to 1.0."""
        with patch.object(
            provider._client.chat.completions,
            "create",
            new_callable=AsyncMock,
            return_value=create_mock_stream(["Response"]),
        ) as mock_create:
            await provider.complete(
                sample_messages,
                model="openai/o3-mini",
                temperature=0.5,
            )

        call_kwargs = mock_create.call_args[1]
        assert call_kwargs["temperature"] == 1.0

    async def test_o1_model_forces_temperature_1(
        self,
        provider: OpenRouterProvider,
        sample_messages: list[Message],
    ) -> None:
        """Test that o1 model forces temperature to 1.0."""
        with patch.object(
            provider._client.chat.completions,
            "create",
            new_callable=AsyncMock,
            return_value=create_mock_stream(["Response"]),
        ) as mock_create:
            await provider.complete(
                sample_messages,
                model="openai/o1-preview",
                temperature=0.2,
            )

        call_kwargs = mock_create.call_args[1]
        assert call_kwargs["temperature"] == 1.0

    async def test_regular_model_keeps_temperature(
        self,
        provider: OpenRouterProvider,
        sample_messages: list[Message],
    ) -> None:
        """Test that regular models keep specified temperature."""
        with patch.object(
            provider._client.chat.completions,
            "create",
            new_callable=AsyncMock,
            return_value=create_mock_stream(["Response"]),
        ) as mock_create:
            await provider.complete(
                sample_messages,
                model="openai/gpt-4o",
                temperature=0.3,
            )

        call_kwargs = mock_create.call_args[1]
        assert call_kwargs["temperature"] == 0.3


class TestOpenRouterProviderErrors:
    """Tests for error handling in OpenRouterProvider."""

    @pytest.fixture
    def provider(self) -> OpenRouterProvider:
        """Create a provider instance for testing."""
        return OpenRouterProvider(api_key="sk-or-test-key")

    @pytest.fixture
    def sample_messages(self) -> list[Message]:
        """Sample messages for testing."""
        return [Message(role="user", content="Hello!")]

    async def test_rate_limit_error(
        self,
        provider: OpenRouterProvider,
        sample_messages: list[Message],
    ) -> None:
        """Test handling of rate limit errors."""
        mock_error = RateLimitError(
            message="Rate limit exceeded",
            response=MagicMock(),
            body=None,
        )

        with patch.object(
            provider._client.chat.completions,
            "create",
            new_callable=AsyncMock,
            side_effect=mock_error,
        ):
            with pytest.raises(OpenRouterProviderError) as exc_info:
                await provider.complete(sample_messages)

        assert exc_info.value.status_code == 429
        assert "rate limit" in exc_info.value.message.lower()
        assert exc_info.value.original_error is mock_error

    async def test_connection_error(
        self,
        provider: OpenRouterProvider,
        sample_messages: list[Message],
    ) -> None:
        """Test handling of connection errors."""
        mock_error = APIConnectionError(request=MagicMock())

        with patch.object(
            provider._client.chat.completions,
            "create",
            new_callable=AsyncMock,
            side_effect=mock_error,
        ):
            with pytest.raises(OpenRouterProviderError) as exc_info:
                await provider.complete(sample_messages)

        assert "connect" in exc_info.value.message.lower()
        assert exc_info.value.original_error is mock_error

    async def test_api_error(
        self,
        provider: OpenRouterProvider,
        sample_messages: list[Message],
    ) -> None:
        """Test handling of generic API errors."""
        mock_error = APIError(
            message="Internal server error",
            request=MagicMock(),
            body=None,
        )
        mock_error.status_code = 500

        with patch.object(
            provider._client.chat.completions,
            "create",
            new_callable=AsyncMock,
            side_effect=mock_error,
        ):
            with pytest.raises(OpenRouterProviderError) as exc_info:
                await provider.complete(sample_messages)

        assert exc_info.value.status_code == 500
        assert exc_info.value.original_error is mock_error

    async def test_unexpected_error(
        self,
        provider: OpenRouterProvider,
        sample_messages: list[Message],
    ) -> None:
        """Test handling of unexpected errors."""
        mock_error = RuntimeError("Unexpected failure")

        with patch.object(
            provider._client.chat.completions,
            "create",
            new_callable=AsyncMock,
            side_effect=mock_error,
        ):
            with pytest.raises(OpenRouterProviderError) as exc_info:
                await provider.complete(sample_messages)

        assert "unexpected" in exc_info.value.message.lower()
        assert exc_info.value.original_error is mock_error


class TestOpenRouterProviderError:
    """Tests for the OpenRouterProviderError exception."""

    def test_error_with_message_only(self) -> None:
        """Test error with just a message."""
        error = OpenRouterProviderError(message="Test error")

        assert str(error) == "Test error"
        assert error.message == "Test error"
        assert error.status_code is None
        assert error.original_error is None

    def test_error_with_status_code(self) -> None:
        """Test error with status code."""
        error = OpenRouterProviderError(
            message="Rate limited",
            status_code=429,
        )

        assert error.status_code == 429

    def test_error_with_original_error(self) -> None:
        """Test error wrapping another exception."""
        original = ValueError("Original error")
        error = OpenRouterProviderError(
            message="Wrapped error",
            original_error=original,
        )

        assert error.original_error is original
