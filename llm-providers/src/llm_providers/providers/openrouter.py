"""OpenRouter LLM Provider implementation.

This module provides an OpenRouter implementation of the LLMProvider protocol.
It uses the OpenAI Python SDK with OpenRouter's base URL to access various
LLM models through a unified API.

OpenRouter (https://openrouter.ai) provides access to multiple LLM providers
(Anthropic, OpenAI, Google, etc.) through a single API endpoint that is
compatible with the OpenAI SDK.

Example:
    from llm_providers import OpenRouterProvider, Message

    provider = OpenRouterProvider(api_key="sk-or-...")

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

# Default OpenRouter base URL
DEFAULT_BASE_URL = "https://openrouter.ai/api/v1"

# Default model - Claude Sonnet 4 via OpenRouter
DEFAULT_MODEL = "anthropic/claude-sonnet-4-20250514"

# Default timeout configuration
DEFAULT_TIMEOUT_SECONDS = 600.0

# Streaming timeouts
CONTENT_TIMEOUT_SECONDS = 120.0  # Max time without receiving actual content
CHUNK_TIMEOUT_SECONDS = 180.0  # Max time without receiving any chunk
MAX_CONSECUTIVE_EMPTY_CHUNKS = 300  # Max empty chunks before declaring stall
PROGRESS_LOG_INTERVAL = 30.0  # Seconds between progress log messages


class OpenRouterProviderError(Exception):
    """Exception raised when OpenRouter API calls fail.

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
        """Initialize an OpenRouterProviderError.

        Args:
            message: Description of the error.
            status_code: HTTP status code if available.
            original_error: The underlying exception that caused this error.
        """
        self.message = message
        self.status_code = status_code
        self.original_error = original_error
        super().__init__(message)


class OpenRouterProvider:
    """OpenRouter implementation of the LLMProvider protocol.

    This provider uses the OpenAI Python SDK with OpenRouter's base URL
    to access various LLM models. It handles message conversion, API calls,
    and response parsing with streaming support for large responses.

    Attributes:
        default_model: The default model to use for completions.

    Example:
        provider = OpenRouterProvider(
            api_key="sk-or-...",
            default_model="anthropic/claude-sonnet-4-20250514",
        )

        messages = [Message(role="user", content="Hello!")]
        response = await provider.complete(messages, temperature=0.5)
    """

    def __init__(
        self,
        api_key: str,
        base_url: str = DEFAULT_BASE_URL,
        default_model: str = DEFAULT_MODEL,
        site_url: str | None = None,
        site_name: str | None = None,
        timeout_seconds: float = DEFAULT_TIMEOUT_SECONDS,
    ):
        """Initialize the OpenRouter provider.

        Args:
            api_key: OpenRouter API key (starts with "sk-or-").
            base_url: OpenRouter API base URL.
            default_model: Default model to use when none is specified.
            site_url: Optional URL of your site for OpenRouter analytics.
            site_name: Optional name of your site/app for OpenRouter analytics.
            timeout_seconds: Request timeout in seconds.
        """
        self._api_key = api_key
        self._base_url = base_url
        self._default_model = default_model
        self._site_url = site_url
        self._site_name = site_name

        # Build timeout configuration
        self._timeout = Timeout(
            connect=30.0,
            read=timeout_seconds,
            write=60.0,
            pool=30.0,
        )

        # Build default headers for OpenRouter
        default_headers: dict[str, str] = {}
        if site_url:
            default_headers["HTTP-Referer"] = site_url
        if site_name:
            default_headers["X-Title"] = site_name

        # Initialize the async OpenAI client with OpenRouter configuration
        self._client = AsyncOpenAI(
            api_key=api_key,
            base_url=base_url,
            default_headers=default_headers if default_headers else None,
            timeout=self._timeout,
        )

        logger.debug(
            f"OpenRouterProvider initialized: base_url={base_url}, "
            f"default_model={default_model}"
        )

    @property
    def default_model(self) -> str:
        """The default model for this provider."""
        return self._default_model

    async def complete(
        self,
        messages: list[Message],
        model: str | None = None,
        temperature: float = 0.7,
        **kwargs: Any,
    ) -> CompletionResponse:
        """Send messages and get a completion response using streaming.

        Uses streaming mode for progress visibility and stall detection.
        This prevents the appearance of "hanging" when the LLM is
        generating a long response.

        Args:
            messages: List of messages forming the conversation history.
            model: Optional model override. If None, uses default_model.
            temperature: Sampling temperature (0.0-2.0).
            **kwargs: Additional parameters (top_p, stop, max_tokens, etc.).

        Returns:
            CompletionResponse containing the generated content and metadata.

        Raises:
            OpenRouterProviderError: If the API call fails or stalls.
        """
        resolved_model = model or self._default_model

        # Some models require specific temperature settings
        # o3 and other reasoning models require temperature=1.0
        model_lower = resolved_model.lower()
        if any(m in model_lower for m in ["o3", "o1-", "o1/", "gpt-5"]):
            if temperature != 1.0:
                logger.info(
                    f"Model {resolved_model} requires temperature=1.0, "
                    f"overriding configured value {temperature}"
                )
                temperature = 1.0

        # Convert messages to OpenAI format
        openai_messages = [
            {"role": msg.role, "content": msg.content} for msg in messages
        ]

        start_time = time.time()
        logger.info(
            f"OpenRouter API call starting: model={resolved_model}, "
            f"temperature={temperature}, message_count={len(messages)}"
        )

        try:
            # Estimate input tokens for monitoring (rough: 1 token ~ 4 chars)
            total_chars = sum(len(m.get("content", "")) for m in openai_messages)
            estimated_input_tokens = total_chars // 4
            if estimated_input_tokens > 30000:
                logger.warning(
                    f"Large prompt detected: ~{estimated_input_tokens:,} tokens "
                    f"for {resolved_model}"
                )

            # Build API call parameters
            api_params: dict[str, Any] = {
                "model": resolved_model,
                "messages": openai_messages,
                "temperature": temperature,
                "stream": True,
            }
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
                        raise OpenRouterProviderError(
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
                        raise OpenRouterProviderError(
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
                        f"OpenRouter streaming: {elapsed:.0f}s elapsed, "
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
                f"OpenRouter API call completed: model={resolved_model}, "
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
                f"OpenRouter rate limit exceeded: {e}",
                extra={"model": resolved_model, "message_count": len(messages)},
            )
            raise OpenRouterProviderError(
                message="Rate limit exceeded. Please retry after a delay.",
                status_code=429,
                original_error=e,
            ) from e

        except APIConnectionError as e:
            logger.error(
                f"OpenRouter connection error: {e}",
                extra={"model": resolved_model, "message_count": len(messages)},
            )
            raise OpenRouterProviderError(
                message="Failed to connect to OpenRouter API.",
                original_error=e,
            ) from e

        except APIError as e:
            status_code = getattr(e, "status_code", None)
            logger.error(
                f"OpenRouter API error: {e}",
                extra={
                    "model": resolved_model,
                    "status_code": status_code,
                    "message_count": len(messages),
                },
            )
            raise OpenRouterProviderError(
                message=f"OpenRouter API error: {e.message}",
                status_code=status_code,
                original_error=e,
            ) from e

        except TimeoutError as e:
            elapsed = time.time() - start_time
            logger.error(
                f"OpenRouter stream timeout after {elapsed:.1f}s: {e}",
                extra={"model": resolved_model, "elapsed": elapsed},
            )
            raise OpenRouterProviderError(
                message=f"Stream timeout after {elapsed:.1f}s - model may be "
                "overloaded or stalled",
                original_error=e,
            ) from e

        except OpenRouterProviderError:
            # Re-raise our own errors (e.g., from chunk timeout)
            raise

        except Exception as e:
            logger.error(
                f"Unexpected error calling OpenRouter: {e}",
                extra={"model": resolved_model, "message_count": len(messages)},
            )
            raise OpenRouterProviderError(
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
            OpenRouterProviderError: If no chunk is received within timeout.
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
                raise OpenRouterProviderError(
                    message=f"Stream stalled: no chunk received for "
                    f"{timeout_seconds}s (network-level timeout)",
                    original_error=e,
                ) from e
