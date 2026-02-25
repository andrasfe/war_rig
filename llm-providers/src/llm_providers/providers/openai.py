"""OpenAI LLM Provider implementation.

This module provides a direct OpenAI API implementation of the LLMProvider
protocol using the official OpenAI Python SDK.

Example:
    from llm_providers import OpenAIProvider, Message

    provider = OpenAIProvider(api_key="sk-...")

    messages = [
        Message(role="system", content="You are a helpful assistant."),
        Message(role="user", content="Hello!"),
    ]

    response = await provider.complete(messages)
    print(response.content)
"""

from __future__ import annotations

import asyncio
import logging
import time
from typing import Any

from httpx import Timeout
from openai import APIConnectionError, APIError, AsyncOpenAI, RateLimitError

from llm_providers.protocol import CompletionResponse, Message

logger = logging.getLogger(__name__)

# Default model
DEFAULT_MODEL = "gpt-4o"

# Default timeout
DEFAULT_TIMEOUT_SECONDS = 600.0

# Streaming timeouts
CONTENT_TIMEOUT_SECONDS = 120.0  # Max time without receiving actual content
CHUNK_TIMEOUT_SECONDS = 180.0  # Max time without receiving any chunk
MAX_CONSECUTIVE_EMPTY_CHUNKS = 300  # Max empty chunks before declaring stall
PROGRESS_LOG_INTERVAL = 30.0  # Seconds between progress log messages

# Models that don't support temperature parameter
# These are reasoning models that use a fixed temperature
REASONING_MODELS = {"o1", "o1-mini", "o1-preview", "o3", "o3-mini", "gpt-5"}


class OpenAIProviderError(Exception):
    """Exception raised when OpenAI API calls fail.

    Attributes:
        message: Description of the error.
        status_code: HTTP status code if available.
        original_error: The underlying exception that caused this error.
    """

    def __init__(
        self,
        message: str,
        status_code: int | None = None,
        original_error: Exception | None = None,
    ):
        """Initialize an OpenAIProviderError.

        Args:
            message: Description of the error.
            status_code: HTTP status code if available.
            original_error: The underlying exception that caused this error.
        """
        self.message = message
        self.status_code = status_code
        self.original_error = original_error
        super().__init__(message)


class OpenAIProvider:
    """Direct OpenAI API implementation of the LLMProvider protocol.

    This provider uses the official OpenAI Python SDK for direct API
    access to OpenAI models (GPT-4, GPT-3.5, etc.) with streaming support.

    Handles reasoning models (o1, o3) that don't support the temperature
    parameter by automatically removing it from the request.

    Attributes:
        default_model: The default model to use for completions.

    Example:
        provider = OpenAIProvider(
            api_key="sk-...",
            default_model="gpt-4o",
        )

        messages = [Message(role="user", content="Hello!")]
        response = await provider.complete(messages)
    """

    def __init__(
        self,
        api_key: str,
        default_model: str = DEFAULT_MODEL,
        base_url: str | None = None,
        organization: str | None = None,
        timeout_seconds: float = DEFAULT_TIMEOUT_SECONDS,
    ):
        """Initialize the OpenAI provider.

        Args:
            api_key: OpenAI API key.
            default_model: Default model to use when none is specified.
            base_url: Optional custom API base URL (for Azure or proxies).
            organization: Optional OpenAI organization ID.
            timeout_seconds: Request timeout in seconds.
        """
        self._api_key = api_key
        self._default_model = default_model
        self._timeout_seconds = timeout_seconds

        # Build timeout configuration
        self._timeout = Timeout(
            connect=30.0,
            read=timeout_seconds,
            write=60.0,
            pool=30.0,
        )

        # Initialize the async OpenAI client
        self._client = AsyncOpenAI(
            api_key=api_key,
            base_url=base_url,
            organization=organization,
            timeout=self._timeout,
        )

        logger.debug(f"OpenAIProvider initialized: default_model={default_model}")

    @property
    def default_model(self) -> str:
        """The default model for this provider."""
        return self._default_model

    def _is_reasoning_model(self, model: str) -> bool:
        """Check if a model is a reasoning model that doesn't support temperature.

        Args:
            model: The model identifier to check.

        Returns:
            True if the model is a reasoning model.
        """
        model_lower = model.lower()
        # Check for exact matches and prefix matches
        for reasoning_model in REASONING_MODELS:
            if model_lower == reasoning_model or model_lower.startswith(
                f"{reasoning_model}-"
            ):
                return True
        return False

    async def complete(
        self,
        messages: list[Message],
        model: str | None = None,
        temperature: float = 0.7,
        **kwargs: Any,
    ) -> CompletionResponse:
        """Send messages and get a completion response using streaming.

        Uses streaming mode for progress visibility and stall detection.
        Automatically handles reasoning models (o1, o3) that don't support
        the temperature parameter.

        Args:
            messages: List of messages forming the conversation history.
            model: Optional model override. If None, uses default_model.
            temperature: Sampling temperature (0.0-2.0). Ignored for reasoning models.
            **kwargs: Additional parameters (max_tokens, stop, etc.).

        Returns:
            CompletionResponse containing the generated content and metadata.

        Raises:
            OpenAIProviderError: If the API call fails.
        """
        resolved_model = model or self._default_model

        # Convert messages to OpenAI format
        openai_messages = [
            {"role": msg.role, "content": msg.content} for msg in messages
        ]

        start_time = time.time()
        logger.info(
            f"OpenAI API call starting: model={resolved_model}, "
            f"temperature={temperature}, messages={len(messages)}"
        )

        try:
            # Build API call parameters
            api_params: dict[str, Any] = {
                "model": resolved_model,
                "messages": openai_messages,
                "stream": True,
            }

            # Handle reasoning models that don't support temperature
            if self._is_reasoning_model(resolved_model):
                logger.info(
                    f"Model {resolved_model} is a reasoning model - "
                    f"temperature parameter will be omitted"
                )
            else:
                api_params["temperature"] = temperature

            api_params.update(kwargs)

            # Progress tracking state
            last_progress_log = start_time
            content_chunks: list[str] = []
            chunk_count = 0
            empty_chunk_count = 0
            consecutive_empty_chunks = 0
            last_content_time = start_time

            # Create streaming response
            stream = await self._client.chat.completions.create(
                **api_params,
                timeout=self._timeout,
            )

            # Process stream with chunk timeout
            async for chunk in self._iter_with_timeout(stream, CHUNK_TIMEOUT_SECONDS):
                chunk_count += 1
                current_time = time.time()

                # Extract content from chunk
                if chunk.choices and chunk.choices[0].delta.content:
                    content = chunk.choices[0].delta.content
                    content_chunks.append(content)
                    last_content_time = current_time
                    consecutive_empty_chunks = 0
                else:
                    # Empty chunk (keepalive or metadata)
                    empty_chunk_count += 1
                    consecutive_empty_chunks += 1

                    # Check for content timeout
                    time_since_content = current_time - last_content_time
                    if time_since_content >= CONTENT_TIMEOUT_SECONDS:
                        chars_so_far = sum(len(c) for c in content_chunks)
                        raise OpenAIProviderError(
                            message=(
                                f"Stream stalled: no content received for "
                                f"{time_since_content:.0f}s "
                                f"(received {consecutive_empty_chunks} empty chunks). "
                                f"Total: {chars_so_far:,} chars from {chunk_count} chunks"
                            ),
                            original_error=None,
                        )

                    # Check for excessive consecutive empty chunks
                    if consecutive_empty_chunks >= MAX_CONSECUTIVE_EMPTY_CHUNKS:
                        chars_so_far = sum(len(c) for c in content_chunks)
                        raise OpenAIProviderError(
                            message=(
                                f"Stream stalled: {consecutive_empty_chunks} "
                                f"consecutive empty chunks without content. "
                                f"Total: {chars_so_far:,} chars from {chunk_count} chunks"
                            ),
                            original_error=None,
                        )

                # Log progress periodically
                if current_time - last_progress_log >= PROGRESS_LOG_INTERVAL:
                    elapsed = current_time - start_time
                    chars_so_far = sum(len(c) for c in content_chunks)
                    time_since_content = current_time - last_content_time
                    logger.info(
                        f"OpenAI streaming: {elapsed:.0f}s elapsed, "
                        f"{chars_so_far:,} chars, {chunk_count} chunks "
                        f"({empty_chunk_count} empty, "
                        f"{time_since_content:.0f}s since last content)"
                    )
                    last_progress_log = current_time

            # Combine all chunks
            content = "".join(content_chunks)

            elapsed = time.time() - start_time
            content_len = len(content)
            content_chunk_count = chunk_count - empty_chunk_count

            logger.info(
                f"OpenAI API call completed: model={resolved_model}, "
                f"elapsed={elapsed:.1f}s, response_chars={content_len}, "
                f"chunks={chunk_count} ({content_chunk_count} with content, "
                f"{empty_chunk_count} empty)"
            )

            # Build response (streaming doesn't return usage stats)
            return CompletionResponse(
                content=content,
                model=resolved_model,
                tokens_used=0,  # Not available with streaming
                raw_response={
                    "chunks": chunk_count,
                    "content_chunks": content_chunk_count,
                    "empty_chunks": empty_chunk_count,
                    "elapsed": elapsed,
                },
            )

        except RateLimitError as e:
            logger.error(
                f"OpenAI rate limit exceeded: {e}",
                extra={"model": resolved_model, "message_count": len(messages)},
            )
            raise OpenAIProviderError(
                message="Rate limit exceeded. Please retry after a delay.",
                status_code=429,
                original_error=e,
            ) from e

        except APIConnectionError as e:
            logger.error(
                f"OpenAI connection error: {e}",
                extra={"model": resolved_model, "message_count": len(messages)},
            )
            raise OpenAIProviderError(
                message="Failed to connect to OpenAI API.",
                original_error=e,
            ) from e

        except APIError as e:
            status_code = getattr(e, "status_code", None)
            logger.error(
                f"OpenAI API error: {e}",
                extra={
                    "model": resolved_model,
                    "status_code": status_code,
                    "message_count": len(messages),
                },
            )
            raise OpenAIProviderError(
                message=f"OpenAI API error: {e.message}",
                status_code=status_code,
                original_error=e,
            ) from e

        except TimeoutError as e:
            elapsed = time.time() - start_time
            logger.error(
                f"OpenAI stream timeout after {elapsed:.1f}s: {e}",
                extra={"model": resolved_model, "elapsed": elapsed},
            )
            raise OpenAIProviderError(
                message=f"Stream timeout after {elapsed:.1f}s - model may be "
                "overloaded or stalled",
                original_error=e,
            ) from e

        except OpenAIProviderError:
            # Re-raise our own errors
            raise

        except Exception as e:
            logger.error(
                f"Unexpected error calling OpenAI: {e}",
                extra={"model": resolved_model, "message_count": len(messages)},
            )
            raise OpenAIProviderError(
                message=f"Unexpected error: {e!s}",
                original_error=e,
            ) from e

    async def _iter_with_timeout(
        self,
        stream: Any,
        timeout_seconds: float,
    ) -> Any:
        """Iterate over stream with timeout between chunks.

        This is a safety net for complete network stalls.

        Args:
            stream: The async stream to iterate over.
            timeout_seconds: Maximum time to wait for each chunk.

        Yields:
            Chunks from the stream.

        Raises:
            OpenAIProviderError: If no chunk is received within timeout.
        """
        iterator = stream.__aiter__()
        while True:
            try:
                chunk = await asyncio.wait_for(
                    iterator.__anext__(),
                    timeout=timeout_seconds,
                )
                yield chunk
            except StopAsyncIteration:
                break
            except TimeoutError as e:
                raise OpenAIProviderError(
                    message=f"Stream stalled: no chunk received for "
                    f"{timeout_seconds}s (network-level timeout)",
                    original_error=e,
                ) from e
