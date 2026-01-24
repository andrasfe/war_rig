"""OpenRouter LLM Provider implementation.

This module provides an OpenRouter implementation of the LLMProvider protocol.
It uses the OpenAI Python SDK with OpenRouter's base URL to access various
LLM models through a unified API.

OpenRouter (https://openrouter.ai) provides access to multiple LLM providers
(Anthropic, OpenAI, Google, etc.) through a single API endpoint that is
compatible with the OpenAI SDK.

Example:
    from war_rig.providers.openrouter import OpenRouterProvider
    from war_rig.providers import Message

    provider = OpenRouterProvider(api_key="sk-or-...")

    messages = [
        Message(role="system", content="You are a helpful assistant."),
        Message(role="user", content="Hello!"),
    ]

    response = await provider.complete(messages)
    print(response.content)
"""

import logging
from typing import Any

from httpx import Timeout
from openai import AsyncOpenAI, APIError, APIConnectionError, RateLimitError

from war_rig.providers.protocol import CompletionResponse, Message
from war_rig.utils.error_logging import log_error

logger = logging.getLogger(__name__)

# Default OpenRouter base URL
DEFAULT_BASE_URL = "https://openrouter.ai/api/v1"

# Default model - Claude Sonnet 4 via OpenRouter
DEFAULT_MODEL = "anthropic/claude-sonnet-4-20250514"

# Default timeout for LLM requests (in seconds)
# Set high to accommodate slow models with large prompts
# - connect: timeout for establishing connection
# - read: timeout for receiving response after request sent (can be very long for LLMs)
# - write: timeout for sending request
# - pool: timeout for acquiring a connection from the pool
DEFAULT_TIMEOUT = Timeout(connect=30.0, read=600.0, write=60.0, pool=30.0)


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
    and response parsing.

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
        timeout: Timeout | None = None,
    ):
        """Initialize the OpenRouter provider.

        Args:
            api_key: OpenRouter API key (starts with "sk-or-").
            base_url: OpenRouter API base URL. Defaults to the standard
                OpenRouter endpoint.
            default_model: Default model to use when none is specified.
                Defaults to Claude Sonnet 4.
            site_url: Optional URL of your site for OpenRouter analytics.
                Sent as the HTTP-Referer header.
            site_name: Optional name of your site/app for OpenRouter analytics.
                Sent as the X-Title header.
            timeout: Request timeout configuration. Defaults to DEFAULT_TIMEOUT
                which allows 10 minutes for LLM response (suitable for slow models).
        """
        self._api_key = api_key
        self._base_url = base_url
        self._default_model = default_model
        self._site_url = site_url
        self._site_name = site_name
        self._timeout = timeout or DEFAULT_TIMEOUT

        # Build default headers for OpenRouter
        default_headers: dict[str, str] = {}
        if site_url:
            default_headers["HTTP-Referer"] = site_url
        if site_name:
            default_headers["X-Title"] = site_name

        # Initialize the async OpenAI client with OpenRouter configuration
        # Set generous timeout for slow models (default: 10 min read timeout)
        self._client = AsyncOpenAI(
            api_key=api_key,
            base_url=base_url,
            default_headers=default_headers if default_headers else None,
            timeout=self._timeout,
        )

        logger.debug(
            f"OpenRouterProvider initialized with base_url={base_url}, "
            f"default_model={default_model}"
        )

    @property
    def default_model(self) -> str:
        """The default model for this provider.

        Returns:
            The model identifier string to use when no model is specified.
        """
        return self._default_model

    async def complete(
        self,
        messages: list[Message],
        model: str | None = None,
        temperature: float = 0.7,
        **kwargs: Any,
    ) -> CompletionResponse:
        """Send messages and get a completion response.

        Converts the provider-agnostic Message format to OpenAI's message
        format, makes the API call via OpenRouter, and parses the response.

        Args:
            messages: List of messages forming the conversation history.
            model: Optional model override. If None, uses default_model.
            temperature: Sampling temperature (0.0 = deterministic,
                1.0+ = more random). Default is 0.7.
            **kwargs: Additional parameters passed to the OpenAI API
                (e.g., top_p, stop, presence_penalty, frequency_penalty).

        Returns:
            CompletionResponse containing the generated content and metadata.

        Raises:
            OpenRouterProviderError: If the API call fails.
        """
        resolved_model = model or self._default_model

        # Convert our Message format to OpenAI's format
        openai_messages = self._convert_messages(messages)

        import time
        start_time = time.time()
        logger.info(
            f"OpenRouter API call starting: model={resolved_model}, "
            f"temperature={temperature}, message_count={len(messages)}"
        )

        try:
            # Estimate input tokens for monitoring (rough: 1 token ≈ 4 chars)
            total_chars = sum(len(m.get("content", "")) for m in openai_messages)
            estimated_input_tokens = total_chars // 4
            if estimated_input_tokens > 30000:
                logger.warning(
                    f"Large prompt detected: ~{estimated_input_tokens:,} tokens for {resolved_model}"
                )

            # Build the API call parameters
            api_params: dict[str, Any] = {
                "model": resolved_model,
                "messages": openai_messages,
                "temperature": temperature,
            }

            # Add any additional kwargs (top_p, stop, max_tokens, etc.)
            # Note: We do NOT set a default max_tokens - documentation should be verbose
            api_params.update(kwargs)

            # Make the API call with explicit timeout (10 min max for large prompts)
            # Note: HTTP 200 is logged when headers arrive, but body may still be streaming
            response = await self._client.chat.completions.create(
                **api_params,
                timeout=600.0,  # 10 minute timeout for full response
            )

            elapsed = time.time() - start_time
            content = response.choices[0].message.content if response.choices else None
            content_len = len(content) if content else 0

            # Log actual token usage if available
            token_info = ""
            if response.usage:
                prompt_tokens = response.usage.prompt_tokens or 0
                completion_tokens = response.usage.completion_tokens or 0
                token_info = f", tokens={prompt_tokens}+{completion_tokens}={prompt_tokens + completion_tokens}"

            logger.info(
                f"OpenRouter API call completed: model={resolved_model}, "
                f"elapsed={elapsed:.1f}s, response_chars={content_len}{token_info}"
            )

            # Parse the response
            return self._parse_response(response, resolved_model)

        except RateLimitError as e:
            logger.error(f"OpenRouter rate limit exceeded: {e}")
            log_error(
                e,
                context={"provider": "openrouter", "model": resolved_model, "error_type": "rate_limit"},
                request={"model": resolved_model, "message_count": len(messages)},
                response={"body": getattr(e, "body", None), "response": str(getattr(e, "response", None))},
            )
            raise OpenRouterProviderError(
                message="Rate limit exceeded. Please retry after a delay.",
                status_code=429,
                original_error=e,
            ) from e

        except APIConnectionError as e:
            logger.error(f"OpenRouter connection error: {e}")
            log_error(
                e,
                context={"provider": "openrouter", "model": resolved_model, "error_type": "connection"},
                request={"model": resolved_model, "message_count": len(messages)},
            )
            raise OpenRouterProviderError(
                message="Failed to connect to OpenRouter API.",
                original_error=e,
            ) from e

        except APIError as e:
            logger.error(f"OpenRouter API error: {e}")
            log_error(
                e,
                context={"provider": "openrouter", "model": resolved_model, "error_type": "api_error", "status_code": e.status_code},
                request={"model": resolved_model, "message_count": len(messages)},
                response={"body": getattr(e, "body", None), "response": str(getattr(e, "response", None)), "message": getattr(e, "message", None)},
            )
            raise OpenRouterProviderError(
                message=f"OpenRouter API error: {e.message}",
                status_code=e.status_code,
                original_error=e,
            ) from e

        except Exception as e:
            logger.error(f"Unexpected error calling OpenRouter: {e}")
            log_error(
                e,
                context={"provider": "openrouter", "model": resolved_model, "error_type": "unexpected"},
                request={"model": resolved_model, "message_count": len(messages)},
            )
            raise OpenRouterProviderError(
                message=f"Unexpected error: {e!s}",
                original_error=e,
            ) from e

    def _convert_messages(
        self,
        messages: list[Message],
    ) -> list[dict[str, str]]:
        """Convert our Message format to OpenAI's message format.

        Args:
            messages: List of Message objects.

        Returns:
            List of dictionaries in OpenAI's message format.
        """
        return [
            {"role": msg.role, "content": msg.content}
            for msg in messages
        ]

    def _parse_response(
        self,
        response: Any,
        model: str,
    ) -> CompletionResponse:
        """Parse the OpenAI API response into a CompletionResponse.

        Args:
            response: The raw response from the OpenAI API.
            model: The model that was used for the completion.

        Returns:
            CompletionResponse with the extracted content and metadata.

        Raises:
            OpenRouterProviderError: If the response cannot be parsed.
        """
        try:
            # Extract content from the first choice
            content = ""
            if response.choices and len(response.choices) > 0:
                choice = response.choices[0]
                if choice.message and choice.message.content:
                    content = choice.message.content

            # Extract token usage
            tokens_used = 0
            if response.usage:
                tokens_used = (
                    (response.usage.prompt_tokens or 0) +
                    (response.usage.completion_tokens or 0)
                )

            # Build raw response dict for debugging
            raw_response = {
                "id": response.id,
                "model": response.model,
                "created": response.created,
                "choices": [
                    {
                        "index": c.index,
                        "finish_reason": c.finish_reason,
                    }
                    for c in response.choices
                ] if response.choices else [],
                "usage": {
                    "prompt_tokens": response.usage.prompt_tokens if response.usage else 0,
                    "completion_tokens": response.usage.completion_tokens if response.usage else 0,
                    "total_tokens": response.usage.total_tokens if response.usage else 0,
                } if response.usage else None,
            }

            logger.debug(
                f"OpenRouter response: tokens_used={tokens_used}, "
                f"content_length={len(content)}"
            )

            return CompletionResponse(
                content=content,
                model=response.model or model,
                tokens_used=tokens_used,
                raw_response=raw_response,
            )

        except Exception as e:
            logger.error(f"Failed to parse OpenRouter response: {e}")
            raise OpenRouterProviderError(
                message=f"Failed to parse response: {e!s}",
                original_error=e,
            ) from e
