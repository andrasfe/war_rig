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

from openai import AsyncOpenAI, APIError, APIConnectionError, RateLimitError

from war_rig.providers.protocol import CompletionResponse, Message

logger = logging.getLogger(__name__)

# Default OpenRouter base URL
DEFAULT_BASE_URL = "https://openrouter.ai/api/v1"

# Default model - Claude Sonnet 4 via OpenRouter
DEFAULT_MODEL = "anthropic/claude-sonnet-4-20250514"


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
        """
        self._api_key = api_key
        self._base_url = base_url
        self._default_model = default_model
        self._site_url = site_url
        self._site_name = site_name

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
        max_tokens: int | None = None,
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
            max_tokens: Maximum tokens to generate. If None, the model
                decides the limit.
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

        logger.debug(
            f"Calling OpenRouter API: model={resolved_model}, "
            f"temperature={temperature}, max_tokens={max_tokens}, "
            f"message_count={len(messages)}"
        )

        try:
            # Build the API call parameters
            api_params: dict[str, Any] = {
                "model": resolved_model,
                "messages": openai_messages,
                "temperature": temperature,
            }

            if max_tokens is not None:
                api_params["max_tokens"] = max_tokens

            # Add any additional kwargs (top_p, stop, etc.)
            api_params.update(kwargs)

            # Make the API call
            response = await self._client.chat.completions.create(**api_params)

            # Parse the response
            return self._parse_response(response, resolved_model)

        except RateLimitError as e:
            logger.error(f"OpenRouter rate limit exceeded: {e}")
            raise OpenRouterProviderError(
                message="Rate limit exceeded. Please retry after a delay.",
                status_code=429,
                original_error=e,
            ) from e

        except APIConnectionError as e:
            logger.error(f"OpenRouter connection error: {e}")
            raise OpenRouterProviderError(
                message="Failed to connect to OpenRouter API.",
                original_error=e,
            ) from e

        except APIError as e:
            logger.error(f"OpenRouter API error: {e}")
            raise OpenRouterProviderError(
                message=f"OpenRouter API error: {e.message}",
                status_code=e.status_code,
                original_error=e,
            ) from e

        except Exception as e:
            logger.error(f"Unexpected error calling OpenRouter: {e}")
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
