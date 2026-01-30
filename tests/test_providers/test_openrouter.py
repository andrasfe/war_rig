"""Tests for the OpenRouter LLM Provider implementation.

This module tests the OpenRouterProvider including:
- Initialization and configuration
- Protocol conformance
- complete() method with mocked OpenAI client
- Error handling for various failure scenarios
"""

from unittest.mock import AsyncMock, MagicMock

import pytest
from openai import APIConnectionError, APIError, RateLimitError

from war_rig.providers import (
    CompletionResponse,
    LLMProvider,
    Message,
    OpenRouterProvider,
    OpenRouterProviderError,
)
from war_rig.providers.openrouter import DEFAULT_BASE_URL, DEFAULT_MODEL


class TestOpenRouterProviderInitialization:
    """Tests for OpenRouterProvider initialization."""

    def test_initialization_with_api_key_only(self) -> None:
        """Test initialization with just API key."""
        provider = OpenRouterProvider(api_key="test-key")
        assert provider.default_model == DEFAULT_MODEL
        assert provider._api_key == "test-key"
        assert provider._base_url == DEFAULT_BASE_URL

    def test_initialization_with_custom_model(self) -> None:
        """Test initialization with custom default model."""
        provider = OpenRouterProvider(
            api_key="test-key",
            default_model="openai/gpt-4",
        )
        assert provider.default_model == "openai/gpt-4"

    def test_initialization_with_custom_base_url(self) -> None:
        """Test initialization with custom base URL."""
        custom_url = "https://custom.openrouter.ai/api/v1"
        provider = OpenRouterProvider(
            api_key="test-key",
            base_url=custom_url,
        )
        assert provider._base_url == custom_url

    def test_initialization_with_site_info(self) -> None:
        """Test initialization with site URL and name for OpenRouter analytics."""
        provider = OpenRouterProvider(
            api_key="test-key",
            site_url="https://example.com",
            site_name="Test App",
        )
        assert provider._site_url == "https://example.com"
        assert provider._site_name == "Test App"

    def test_initialization_creates_async_client(self) -> None:
        """Test that initialization creates an AsyncOpenAI client."""
        provider = OpenRouterProvider(api_key="test-key")
        assert provider._client is not None


class TestOpenRouterProviderProtocol:
    """Tests for OpenRouterProvider protocol conformance."""

    def test_implements_llm_provider_protocol(self) -> None:
        """Test that OpenRouterProvider implements LLMProvider protocol."""
        provider = OpenRouterProvider(api_key="test-key")
        assert isinstance(provider, LLMProvider)

    def test_default_model_property_returns_string(self) -> None:
        """Test that default_model property returns a string."""
        provider = OpenRouterProvider(api_key="test-key")
        assert isinstance(provider.default_model, str)
        assert len(provider.default_model) > 0


class TestOpenRouterProviderComplete:
    """Tests for the complete() method."""

    @pytest.fixture
    def mock_stream_response(self) -> list[MagicMock]:
        """Create mock streaming response chunks."""
        chunks = []

        # Chunk 1: "This is "
        chunk1 = MagicMock()
        chunk1.choices = [MagicMock()]
        chunk1.choices[0].delta = MagicMock()
        chunk1.choices[0].delta.content = "This is "
        chunks.append(chunk1)

        # Chunk 2: "the assistant's "
        chunk2 = MagicMock()
        chunk2.choices = [MagicMock()]
        chunk2.choices[0].delta = MagicMock()
        chunk2.choices[0].delta.content = "the assistant's "
        chunks.append(chunk2)

        # Chunk 3: "response."
        chunk3 = MagicMock()
        chunk3.choices = [MagicMock()]
        chunk3.choices[0].delta = MagicMock()
        chunk3.choices[0].delta.content = "response."
        chunks.append(chunk3)

        # Final chunk with no content (end of stream)
        chunk_final = MagicMock()
        chunk_final.choices = [MagicMock()]
        chunk_final.choices[0].delta = MagicMock()
        chunk_final.choices[0].delta.content = None
        chunks.append(chunk_final)

        return chunks

    @pytest.fixture
    def mock_response(self) -> MagicMock:
        """Create a mock OpenAI API response (non-streaming, for compatibility)."""
        response = MagicMock()
        response.id = "chatcmpl-123"
        response.model = "anthropic/claude-sonnet-4-20250514"
        response.created = 1704067200

        choice = MagicMock()
        choice.index = 0
        choice.finish_reason = "stop"
        choice.message = MagicMock()
        choice.message.content = "This is the assistant's response."
        response.choices = [choice]

        response.usage = MagicMock()
        response.usage.prompt_tokens = 50
        response.usage.completion_tokens = 100
        response.usage.total_tokens = 150

        return response

    @pytest.fixture
    def provider_with_mock_client(self) -> OpenRouterProvider:
        """Create an OpenRouterProvider with a mocked client."""
        provider = OpenRouterProvider(api_key="test-key")
        provider._client = MagicMock()
        provider._client.chat = MagicMock()
        provider._client.chat.completions = MagicMock()
        return provider

    def _create_mock_stream(self, chunks: list[MagicMock]):
        """Create an async iterator from chunks for mocking stream response."""
        async def async_generator():
            for chunk in chunks:
                yield chunk
        return async_generator()

    @pytest.mark.asyncio
    async def test_complete_success(
        self,
        provider_with_mock_client: OpenRouterProvider,
        mock_stream_response: list[MagicMock],
    ) -> None:
        """Test successful completion call with streaming."""
        provider_with_mock_client._client.chat.completions.create = AsyncMock(
            return_value=self._create_mock_stream(mock_stream_response)
        )

        messages = [
            Message(role="system", content="You are a helpful assistant."),
            Message(role="user", content="Hello!"),
        ]

        response = await provider_with_mock_client.complete(messages)

        assert isinstance(response, CompletionResponse)
        assert response.content == "This is the assistant's response."
        # Model comes from provider's default since streaming doesn't return it
        assert response.model == provider_with_mock_client.default_model
        # Streaming doesn't return token counts
        assert response.tokens_used == 0

    @pytest.mark.asyncio
    async def test_complete_with_custom_model(
        self,
        provider_with_mock_client: OpenRouterProvider,
        mock_stream_response: list[MagicMock],
    ) -> None:
        """Test completion with custom model override."""
        provider_with_mock_client._client.chat.completions.create = AsyncMock(
            return_value=self._create_mock_stream(mock_stream_response)
        )

        messages = [Message(role="user", content="Hello!")]

        response = await provider_with_mock_client.complete(
            messages,
            model="openai/gpt-4",
        )

        # Model is set to the requested model
        assert response.model == "openai/gpt-4"

        # Verify the call was made with the custom model
        call_kwargs = (
            provider_with_mock_client._client.chat.completions.create.call_args.kwargs
        )
        assert call_kwargs["model"] == "openai/gpt-4"

    @pytest.mark.asyncio
    async def test_complete_with_temperature(
        self,
        provider_with_mock_client: OpenRouterProvider,
        mock_stream_response: list[MagicMock],
    ) -> None:
        """Test completion with custom temperature."""
        provider_with_mock_client._client.chat.completions.create = AsyncMock(
            return_value=self._create_mock_stream(mock_stream_response)
        )

        messages = [Message(role="user", content="Hello!")]

        await provider_with_mock_client.complete(messages, temperature=0.2)

        call_kwargs = (
            provider_with_mock_client._client.chat.completions.create.call_args.kwargs
        )
        assert call_kwargs["temperature"] == 0.2

    @pytest.mark.asyncio
    async def test_complete_with_extra_kwargs(
        self,
        provider_with_mock_client: OpenRouterProvider,
        mock_stream_response: list[MagicMock],
    ) -> None:
        """Test completion with additional kwargs passed through."""
        provider_with_mock_client._client.chat.completions.create = AsyncMock(
            return_value=self._create_mock_stream(mock_stream_response)
        )

        messages = [Message(role="user", content="Hello!")]

        await provider_with_mock_client.complete(
            messages,
            top_p=0.9,
            stop=["END"],
        )

        call_kwargs = (
            provider_with_mock_client._client.chat.completions.create.call_args.kwargs
        )
        assert call_kwargs["top_p"] == 0.9
        assert call_kwargs["stop"] == ["END"]

    @pytest.mark.asyncio
    async def test_complete_raw_response_included(
        self,
        provider_with_mock_client: OpenRouterProvider,
        mock_stream_response: list[MagicMock],
    ) -> None:
        """Test that raw_response is included in CompletionResponse."""
        provider_with_mock_client._client.chat.completions.create = AsyncMock(
            return_value=self._create_mock_stream(mock_stream_response)
        )

        messages = [Message(role="user", content="Hello!")]

        response = await provider_with_mock_client.complete(messages)

        # Streaming returns chunk/elapsed info instead of API response details
        assert response.raw_response is not None
        assert "chunks" in response.raw_response
        assert "elapsed" in response.raw_response

    @pytest.mark.asyncio
    async def test_complete_empty_choices(
        self,
        provider_with_mock_client: OpenRouterProvider,
    ) -> None:
        """Test handling of streaming response with no content chunks."""
        # Create stream with only a final empty chunk
        chunk = MagicMock()
        chunk.choices = []
        chunks = [chunk]

        async def empty_stream():
            for c in chunks:
                yield c

        provider_with_mock_client._client.chat.completions.create = AsyncMock(
            return_value=empty_stream()
        )

        messages = [Message(role="user", content="Hello!")]

        response = await provider_with_mock_client.complete(messages)

        assert response.content == ""
        assert response.has_content is False

    @pytest.mark.asyncio
    async def test_complete_null_message_content(
        self,
        provider_with_mock_client: OpenRouterProvider,
    ) -> None:
        """Test handling of streaming response with null content in chunks."""
        # Create stream where all chunks have null content
        chunk = MagicMock()
        chunk.choices = [MagicMock()]
        chunk.choices[0].delta = MagicMock()
        chunk.choices[0].delta.content = None
        chunks = [chunk]

        async def null_content_stream():
            for c in chunks:
                yield c

        provider_with_mock_client._client.chat.completions.create = AsyncMock(
            return_value=null_content_stream()
        )

        messages = [Message(role="user", content="Hello!")]

        response = await provider_with_mock_client.complete(messages)

        assert response.content == ""


class TestOpenRouterProviderErrorHandling:
    """Tests for error handling in OpenRouterProvider."""

    @pytest.fixture
    def provider_with_mock_client(self) -> OpenRouterProvider:
        """Create an OpenRouterProvider with a mocked client."""
        provider = OpenRouterProvider(api_key="test-key")
        provider._client = MagicMock()
        provider._client.chat = MagicMock()
        provider._client.chat.completions = MagicMock()
        return provider

    @pytest.mark.asyncio
    async def test_rate_limit_error(
        self,
        provider_with_mock_client: OpenRouterProvider,
    ) -> None:
        """Test handling of rate limit errors."""
        rate_limit_error = RateLimitError(
            message="Rate limit exceeded",
            response=MagicMock(status_code=429),
            body=None,
        )
        provider_with_mock_client._client.chat.completions.create = AsyncMock(
            side_effect=rate_limit_error
        )

        messages = [Message(role="user", content="Hello!")]

        with pytest.raises(OpenRouterProviderError) as exc_info:
            await provider_with_mock_client.complete(messages)

        assert exc_info.value.status_code == 429
        assert "Rate limit exceeded" in exc_info.value.message
        assert exc_info.value.original_error is rate_limit_error

    @pytest.mark.asyncio
    async def test_connection_error(
        self,
        provider_with_mock_client: OpenRouterProvider,
    ) -> None:
        """Test handling of connection errors."""
        connection_error = APIConnectionError(request=MagicMock())
        provider_with_mock_client._client.chat.completions.create = AsyncMock(
            side_effect=connection_error
        )

        messages = [Message(role="user", content="Hello!")]

        with pytest.raises(OpenRouterProviderError) as exc_info:
            await provider_with_mock_client.complete(messages)

        assert "Failed to connect" in exc_info.value.message
        assert exc_info.value.original_error is connection_error

    @pytest.mark.asyncio
    async def test_api_error(
        self,
        provider_with_mock_client: OpenRouterProvider,
    ) -> None:
        """Test handling of general API errors."""
        api_error = APIError(
            message="Invalid request",
            request=MagicMock(),
            body=None,
        )
        api_error.status_code = 400
        provider_with_mock_client._client.chat.completions.create = AsyncMock(
            side_effect=api_error
        )

        messages = [Message(role="user", content="Hello!")]

        with pytest.raises(OpenRouterProviderError) as exc_info:
            await provider_with_mock_client.complete(messages)

        assert exc_info.value.status_code == 400
        assert "OpenRouter API error" in exc_info.value.message
        assert exc_info.value.original_error is api_error

    @pytest.mark.asyncio
    async def test_unexpected_error(
        self,
        provider_with_mock_client: OpenRouterProvider,
    ) -> None:
        """Test handling of unexpected errors."""
        unexpected_error = RuntimeError("Unexpected failure")
        provider_with_mock_client._client.chat.completions.create = AsyncMock(
            side_effect=unexpected_error
        )

        messages = [Message(role="user", content="Hello!")]

        with pytest.raises(OpenRouterProviderError) as exc_info:
            await provider_with_mock_client.complete(messages)

        assert "Unexpected error" in exc_info.value.message
        assert exc_info.value.original_error is unexpected_error

    @pytest.mark.asyncio
    async def test_stream_error_during_iteration(
        self,
        provider_with_mock_client: OpenRouterProvider,
    ) -> None:
        """Test handling of errors during stream iteration."""
        # Create a stream that raises an error partway through
        async def error_stream():
            chunk = MagicMock()
            chunk.choices = [MagicMock()]
            chunk.choices[0].delta = MagicMock()
            chunk.choices[0].delta.content = "partial"
            yield chunk
            raise RuntimeError("Stream error")

        provider_with_mock_client._client.chat.completions.create = AsyncMock(
            return_value=error_stream()
        )

        messages = [Message(role="user", content="Hello!")]

        with pytest.raises(OpenRouterProviderError) as exc_info:
            await provider_with_mock_client.complete(messages)

        assert "Unexpected error" in exc_info.value.message


class TestOpenRouterProviderError:
    """Tests for the OpenRouterProviderError exception."""

    def test_error_with_message_only(self) -> None:
        """Test creating error with just a message."""
        error = OpenRouterProviderError("Something went wrong")
        assert error.message == "Something went wrong"
        assert error.status_code is None
        assert error.original_error is None
        assert str(error) == "Something went wrong"

    def test_error_with_status_code(self) -> None:
        """Test creating error with status code."""
        error = OpenRouterProviderError(
            "Rate limited",
            status_code=429,
        )
        assert error.message == "Rate limited"
        assert error.status_code == 429

    def test_error_with_original_error(self) -> None:
        """Test creating error with original exception."""
        original = ValueError("Original error")
        error = OpenRouterProviderError(
            "Wrapped error",
            original_error=original,
        )
        assert error.message == "Wrapped error"
        assert error.original_error is original

    def test_error_with_all_attributes(self) -> None:
        """Test creating error with all attributes."""
        original = RuntimeError("Underlying issue")
        error = OpenRouterProviderError(
            "Full error",
            status_code=500,
            original_error=original,
        )
        assert error.message == "Full error"
        assert error.status_code == 500
        assert error.original_error is original


class TestMessageConversion:
    """Tests for message format conversion."""

    def test_convert_single_message(self) -> None:
        """Test conversion of a single message."""
        provider = OpenRouterProvider(api_key="test-key")
        messages = [Message(role="user", content="Hello!")]

        converted = provider._convert_messages(messages)

        assert len(converted) == 1
        assert converted[0] == {"role": "user", "content": "Hello!"}

    def test_convert_multiple_messages(self) -> None:
        """Test conversion of multiple messages."""
        provider = OpenRouterProvider(api_key="test-key")
        messages = [
            Message(role="system", content="You are helpful."),
            Message(role="user", content="Hi!"),
            Message(role="assistant", content="Hello!"),
            Message(role="user", content="How are you?"),
        ]

        converted = provider._convert_messages(messages)

        assert len(converted) == 4
        assert converted[0] == {"role": "system", "content": "You are helpful."}
        assert converted[1] == {"role": "user", "content": "Hi!"}
        assert converted[2] == {"role": "assistant", "content": "Hello!"}
        assert converted[3] == {"role": "user", "content": "How are you?"}

    def test_convert_empty_messages(self) -> None:
        """Test conversion of empty message list."""
        provider = OpenRouterProvider(api_key="test-key")
        messages: list[Message] = []

        converted = provider._convert_messages(messages)

        assert len(converted) == 0


class TestOpenRouterProviderContentTimeout:
    """Tests for content-based timeout behavior.

    These tests verify that the provider correctly times out when receiving
    chunks but no actual content, which can happen when a provider sends
    keepalive chunks without generating content.
    """

    @pytest.fixture
    def provider_with_mock_client(self) -> OpenRouterProvider:
        """Create an OpenRouterProvider with a mocked client."""
        provider = OpenRouterProvider(api_key="test-key")
        provider._client = MagicMock()
        provider._client.chat = MagicMock()
        provider._client.chat.completions = MagicMock()
        return provider

    @pytest.mark.asyncio
    async def test_consecutive_empty_chunks_triggers_error(
        self,
        provider_with_mock_client: OpenRouterProvider,
    ) -> None:
        """Test that excessive consecutive empty chunks trigger an error.

        This tests the safety net for pathological cases where keepalives arrive
        but never any content. The max_consecutive_empty_chunks threshold (300)
        should trigger an error.
        """
        async def many_empty_chunks_stream():
            """Stream that sends 301 empty chunks (exceeds 300 limit)."""
            for _ in range(301):
                chunk = MagicMock()
                chunk.choices = [MagicMock()]
                chunk.choices[0].delta = MagicMock()
                chunk.choices[0].delta.content = None  # Empty chunk
                yield chunk

        provider_with_mock_client._client.chat.completions.create = AsyncMock(
            return_value=many_empty_chunks_stream()
        )

        messages = [Message(role="user", content="Hello!")]

        with pytest.raises(OpenRouterProviderError) as exc_info:
            await provider_with_mock_client.complete(messages)

        # Should fail due to consecutive empty chunks
        assert "consecutive empty chunks" in exc_info.value.message.lower()

    @pytest.mark.asyncio
    async def test_mixed_content_and_empty_chunks_succeeds(
        self,
        provider_with_mock_client: OpenRouterProvider,
    ) -> None:
        """Test that streams with mixed content and empty chunks succeed.

        Empty chunks interspersed with content chunks should not trigger timeout
        as long as content is being received.
        """
        async def mixed_stream():
            """Stream with both content and empty chunks."""
            # Content chunk
            chunk1 = MagicMock()
            chunk1.choices = [MagicMock()]
            chunk1.choices[0].delta = MagicMock()
            chunk1.choices[0].delta.content = "Hello "
            yield chunk1

            # Empty chunk (keepalive)
            chunk2 = MagicMock()
            chunk2.choices = [MagicMock()]
            chunk2.choices[0].delta = MagicMock()
            chunk2.choices[0].delta.content = None
            yield chunk2

            # Content chunk
            chunk3 = MagicMock()
            chunk3.choices = [MagicMock()]
            chunk3.choices[0].delta = MagicMock()
            chunk3.choices[0].delta.content = "world!"
            yield chunk3

            # Empty chunk (final)
            chunk4 = MagicMock()
            chunk4.choices = [MagicMock()]
            chunk4.choices[0].delta = MagicMock()
            chunk4.choices[0].delta.content = None
            yield chunk4

        provider_with_mock_client._client.chat.completions.create = AsyncMock(
            return_value=mixed_stream()
        )

        messages = [Message(role="user", content="Hello!")]

        response = await provider_with_mock_client.complete(messages)

        assert response.content == "Hello world!"
        # Check that empty chunks are tracked in raw_response
        assert response.raw_response["empty_chunks"] == 2
        assert response.raw_response["content_chunks"] == 2

    @pytest.mark.asyncio
    async def test_raw_response_includes_chunk_statistics(
        self,
        provider_with_mock_client: OpenRouterProvider,
    ) -> None:
        """Test that raw_response includes detailed chunk statistics."""
        async def simple_stream():
            """Simple stream with one content chunk."""
            chunk = MagicMock()
            chunk.choices = [MagicMock()]
            chunk.choices[0].delta = MagicMock()
            chunk.choices[0].delta.content = "Response"
            yield chunk

        provider_with_mock_client._client.chat.completions.create = AsyncMock(
            return_value=simple_stream()
        )

        messages = [Message(role="user", content="Hello!")]

        response = await provider_with_mock_client.complete(messages)

        # Verify all expected keys are present
        assert "chunks" in response.raw_response
        assert "content_chunks" in response.raw_response
        assert "empty_chunks" in response.raw_response
        assert "elapsed" in response.raw_response

        # Verify values
        assert response.raw_response["chunks"] == 1
        assert response.raw_response["content_chunks"] == 1
        assert response.raw_response["empty_chunks"] == 0
