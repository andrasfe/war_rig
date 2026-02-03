"""Anthropic LLM Provider implementation.

This module provides a direct Anthropic API implementation of the LLMProvider
protocol using the official Anthropic Python SDK.

Example:
    from llm_providers import AnthropicProvider, Message

    provider = AnthropicProvider(api_key="sk-ant-...")

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

from anthropic import (
    APIConnectionError,
    APIStatusError,
    AsyncAnthropic,
    RateLimitError,
)

from llm_providers.protocol import CompletionResponse, Message

logger = logging.getLogger(__name__)

# Default model
DEFAULT_MODEL = "claude-sonnet-4-20250514"

# Default max tokens (Anthropic requires this parameter)
DEFAULT_MAX_TOKENS = 4096

# Default timeout
DEFAULT_TIMEOUT_SECONDS = 600.0

# Streaming timeouts
CONTENT_TIMEOUT_SECONDS = 120.0  # Max time without receiving actual content
CHUNK_TIMEOUT_SECONDS = 180.0  # Max time without receiving any chunk
MAX_CONSECUTIVE_EMPTY_EVENTS = 300  # Max empty events before declaring stall
PROGRESS_LOG_INTERVAL = 30.0  # Seconds between progress log messages


class AnthropicProviderError(Exception):
    """Exception raised when Anthropic API calls fail.

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
        """Initialize an AnthropicProviderError.

        Args:
            message: Description of the error.
            status_code: HTTP status code if available.
            original_error: The underlying exception that caused this error.
        """
        self.message = message
        self.status_code = status_code
        self.original_error = original_error
        super().__init__(message)


class AnthropicProvider:
    """Direct Anthropic API implementation of the LLMProvider protocol.

    This provider uses the official Anthropic Python SDK for direct API
    access to Claude models with streaming support for large responses.

    Attributes:
        default_model: The default model to use for completions.

    Example:
        provider = AnthropicProvider(
            api_key="sk-ant-...",
            default_model="claude-sonnet-4-20250514",
        )

        messages = [Message(role="user", content="Hello!")]
        response = await provider.complete(messages)
    """

    def __init__(
        self,
        api_key: str,
        default_model: str = DEFAULT_MODEL,
        max_tokens: int = DEFAULT_MAX_TOKENS,
        base_url: str | None = None,
        timeout_seconds: float = DEFAULT_TIMEOUT_SECONDS,
    ):
        """Initialize the Anthropic provider.

        Args:
            api_key: Anthropic API key.
            default_model: Default model to use when none is specified.
            max_tokens: Maximum tokens in the response (required by Anthropic).
            base_url: Optional custom API base URL.
            timeout_seconds: Request timeout in seconds.
        """
        self._api_key = api_key
        self._default_model = default_model
        self._max_tokens = max_tokens
        self._timeout_seconds = timeout_seconds

        # Initialize the async Anthropic client
        self._client = AsyncAnthropic(
            api_key=api_key,
            base_url=base_url,
            timeout=timeout_seconds,
        )

        logger.debug(
            f"AnthropicProvider initialized: default_model={default_model}, "
            f"max_tokens={max_tokens}"
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

        Args:
            messages: List of messages forming the conversation history.
            model: Optional model override. If None, uses default_model.
            temperature: Sampling temperature (0.0-1.0 for Anthropic).
            **kwargs: Additional parameters (max_tokens, stop_sequences, etc.).

        Returns:
            CompletionResponse containing the generated content and metadata.

        Raises:
            AnthropicProviderError: If the API call fails.
        """
        resolved_model = model or self._default_model

        # Anthropic has a separate system parameter
        system_content: str | None = None
        anthropic_messages: list[dict[str, str]] = []

        for msg in messages:
            if msg.role == "system":
                # Anthropic takes system as a separate parameter
                system_content = msg.content
            else:
                anthropic_messages.append(
                    {"role": msg.role, "content": msg.content}
                )

        start_time = time.time()
        logger.info(
            f"Anthropic API call starting: model={resolved_model}, "
            f"temperature={temperature}, messages={len(anthropic_messages)}"
        )

        try:
            # Get max_tokens from kwargs or use default
            max_tokens = kwargs.pop("max_tokens", self._max_tokens)

            # Build API parameters
            api_params: dict[str, Any] = {
                "model": resolved_model,
                "messages": anthropic_messages,
                "max_tokens": max_tokens,
                "temperature": temperature,
                "stream": True,
            }

            if system_content:
                api_params["system"] = system_content

            api_params.update(kwargs)

            # Progress tracking state
            last_progress_log = start_time
            content_chunks: list[str] = []
            event_count = 0
            empty_event_count = 0
            consecutive_empty_events = 0
            last_content_time = start_time
            input_tokens = 0
            output_tokens = 0

            # Create streaming response using context manager
            async with self._client.messages.stream(**api_params) as stream:
                async for event in self._iter_with_timeout(
                    stream, CHUNK_TIMEOUT_SECONDS
                ):
                    event_count += 1
                    current_time = time.time()

                    # Handle different event types
                    if hasattr(event, "type"):
                        if event.type == "content_block_delta":
                            # Extract text from delta
                            if hasattr(event, "delta") and hasattr(
                                event.delta, "text"
                            ):
                                text = event.delta.text
                                if text:
                                    content_chunks.append(text)
                                    last_content_time = current_time
                                    consecutive_empty_events = 0
                                    continue

                        elif event.type == "message_delta":
                            # Extract usage from final message
                            if hasattr(event, "usage") and event.usage:
                                output_tokens = getattr(
                                    event.usage, "output_tokens", 0
                                )

                        elif event.type == "message_start":
                            # Extract input tokens from message start
                            if hasattr(event, "message") and hasattr(
                                event.message, "usage"
                            ):
                                input_tokens = getattr(
                                    event.message.usage, "input_tokens", 0
                                )

                    # Track empty events
                    empty_event_count += 1
                    consecutive_empty_events += 1

                    # Check for content timeout
                    time_since_content = current_time - last_content_time
                    if time_since_content >= CONTENT_TIMEOUT_SECONDS:
                        chars_so_far = sum(len(c) for c in content_chunks)
                        raise AnthropicProviderError(
                            message=(
                                f"Stream stalled: no content received for "
                                f"{time_since_content:.0f}s "
                                f"(received {consecutive_empty_events} empty events). "
                                f"Total: {chars_so_far:,} chars from {event_count} events"
                            ),
                            original_error=None,
                        )

                    # Check for excessive consecutive empty events
                    if consecutive_empty_events >= MAX_CONSECUTIVE_EMPTY_EVENTS:
                        chars_so_far = sum(len(c) for c in content_chunks)
                        raise AnthropicProviderError(
                            message=(
                                f"Stream stalled: {consecutive_empty_events} "
                                f"consecutive empty events without content. "
                                f"Total: {chars_so_far:,} chars from {event_count} events"
                            ),
                            original_error=None,
                        )

                    # Log progress periodically
                    if current_time - last_progress_log >= PROGRESS_LOG_INTERVAL:
                        elapsed = current_time - start_time
                        chars_so_far = sum(len(c) for c in content_chunks)
                        time_since_content = current_time - last_content_time
                        logger.info(
                            f"Anthropic streaming: {elapsed:.0f}s elapsed, "
                            f"{chars_so_far:,} chars, {event_count} events "
                            f"({empty_event_count} empty, "
                            f"{time_since_content:.0f}s since last content)"
                        )
                        last_progress_log = current_time

            # Combine all chunks
            content = "".join(content_chunks)

            elapsed = time.time() - start_time
            content_len = len(content)
            content_event_count = event_count - empty_event_count
            tokens_used = input_tokens + output_tokens

            logger.info(
                f"Anthropic API call completed: model={resolved_model}, "
                f"elapsed={elapsed:.1f}s, response_chars={content_len}, "
                f"events={event_count} ({content_event_count} with content, "
                f"{empty_event_count} empty), tokens={tokens_used}"
            )

            return CompletionResponse(
                content=content,
                model=resolved_model,
                tokens_used=tokens_used,
                raw_response={
                    "events": event_count,
                    "content_events": content_event_count,
                    "empty_events": empty_event_count,
                    "elapsed": elapsed,
                    "input_tokens": input_tokens,
                    "output_tokens": output_tokens,
                },
            )

        except RateLimitError as e:
            logger.error(
                f"Anthropic rate limit exceeded: {e}",
                extra={"model": resolved_model, "message_count": len(messages)},
            )
            raise AnthropicProviderError(
                message="Rate limit exceeded. Please retry after a delay.",
                status_code=429,
                original_error=e,
            ) from e

        except APIConnectionError as e:
            logger.error(
                f"Anthropic connection error: {e}",
                extra={"model": resolved_model, "message_count": len(messages)},
            )
            raise AnthropicProviderError(
                message="Failed to connect to Anthropic API.",
                original_error=e,
            ) from e

        except APIStatusError as e:
            logger.error(
                f"Anthropic API error: {e}",
                extra={
                    "model": resolved_model,
                    "status_code": e.status_code,
                    "message_count": len(messages),
                },
            )
            raise AnthropicProviderError(
                message=f"Anthropic API error: {e.message}",
                status_code=e.status_code,
                original_error=e,
            ) from e

        except TimeoutError as e:
            elapsed = time.time() - start_time
            logger.error(
                f"Anthropic stream timeout after {elapsed:.1f}s: {e}",
                extra={"model": resolved_model, "elapsed": elapsed},
            )
            raise AnthropicProviderError(
                message=f"Stream timeout after {elapsed:.1f}s - model may be "
                "overloaded or stalled",
                original_error=e,
            ) from e

        except AnthropicProviderError:
            # Re-raise our own errors
            raise

        except Exception as e:
            logger.error(
                f"Unexpected error calling Anthropic: {e}",
                extra={"model": resolved_model, "message_count": len(messages)},
            )
            raise AnthropicProviderError(
                message=f"Unexpected error: {e!s}",
                original_error=e,
            ) from e

    async def _iter_with_timeout(
        self,
        stream: Any,
        timeout_seconds: float,
    ) -> Any:
        """Iterate over stream with timeout between events.

        This is a safety net for complete network stalls.

        Args:
            stream: The async stream to iterate over.
            timeout_seconds: Maximum time to wait for each event.

        Yields:
            Events from the stream.

        Raises:
            AnthropicProviderError: If no event is received within timeout.
        """
        iterator = stream.__aiter__()
        while True:
            try:
                event = await asyncio.wait_for(
                    iterator.__anext__(),
                    timeout=timeout_seconds,
                )
                yield event
            except StopAsyncIteration:
                break
            except TimeoutError as e:
                raise AnthropicProviderError(
                    message=f"Stream stalled: no event received for "
                    f"{timeout_seconds}s (network-level timeout)",
                    original_error=e,
                ) from e
