# Custom LLM Provider Integration Guide

This guide explains how to integrate a custom LLM API with War Rig.

## Overview

War Rig uses a provider abstraction layer that allows any LLM backend to be used. The system is designed to be:

- **Protocol-based**: No inheritance required, just implement the interface
- **API-agnostic**: Works with any API format, not just OpenAI-compatible
- **Configuration-driven**: Provider selection via environment variables

## Quick Start

### 1. Implement the LLMProvider Protocol

Create a class that implements two things:
- A `default_model` property
- An async `complete()` method

```python
from war_rig.providers import Message, CompletionResponse, LLMProvider

class MyCustomProvider:
    """Custom LLM provider implementation."""

    def __init__(self, api_key: str, endpoint: str, model: str = "default-model"):
        self._api_key = api_key
        self._endpoint = endpoint
        self._model = model

    @property
    def default_model(self) -> str:
        return self._model

    async def complete(
        self,
        messages: list[Message],
        model: str | None = None,
        temperature: float = 0.7,
        **kwargs,
    ) -> CompletionResponse:
        """Send messages to your LLM and return a response."""

        # Convert messages to your API's format
        formatted_messages = [
            {"role": msg.role, "text": msg.content}
            for msg in messages
        ]

        # Call your API (example using httpx)
        import httpx
        async with httpx.AsyncClient() as client:
            response = await client.post(
                self._endpoint,
                headers={"Authorization": f"Bearer {self._api_key}"},
                json={
                    "messages": formatted_messages,
                    "model": model or self._model,
                    "temperature": temperature,
                },
            )
            data = response.json()

        # Return a CompletionResponse
        return CompletionResponse(
            content=data["output"],
            model=data.get("model", self._model),
            tokens_used=data.get("usage", {}).get("total_tokens", 0),
            raw_response=data,
        )
```

### 2. Register Your Provider

```python
from war_rig.providers import register_provider
from my_provider import MyCustomProvider

register_provider("mycustom", MyCustomProvider)
```

### 3. Configure Environment

Add to your `.env` file:

```bash
LLM_PROVIDER=mycustom
MYCUSTOM_API_KEY=your-api-key
MYCUSTOM_ENDPOINT=https://api.example.com/v1/chat
```

### 4. Update the Factory (Optional)

For automatic environment-based configuration, update `war_rig/providers/factory.py`:

```python
def get_provider_from_env() -> LLMProvider:
    provider_name = os.getenv("LLM_PROVIDER", "openrouter").lower()

    if provider_name == "mycustom":
        from my_provider import MyCustomProvider
        return MyCustomProvider(
            api_key=os.environ["MYCUSTOM_API_KEY"],
            endpoint=os.environ["MYCUSTOM_ENDPOINT"],
            model=os.getenv("MYCUSTOM_MODEL", "default-model"),
        )
    # ... existing providers
```

## Protocol Reference

### Message

```python
@dataclass(frozen=True)
class Message:
    role: str      # "system", "user", or "assistant"
    content: str   # The message content
```

### CompletionResponse

```python
@dataclass
class CompletionResponse:
    content: str                    # The generated text
    model: str                      # Model that generated the response
    tokens_used: int                # Total tokens consumed
    raw_response: dict | None       # Optional: full API response for debugging
```

### LLMProvider Protocol

```python
class LLMProvider(Protocol):
    @property
    def default_model(self) -> str:
        """Return the default model identifier."""
        ...

    async def complete(
        self,
        messages: list[Message],
        model: str | None = None,
        temperature: float = 0.7,
        **kwargs,
    ) -> CompletionResponse:
        """Send messages and return a completion."""
        ...
```

## Examples

### YAML-Configured Provider

For providers that use YAML configuration (like ConnectChain):

```python
import yaml

class YAMLConfiguredProvider:
    def __init__(self, config_path: str = "config.yml"):
        with open(config_path) as f:
            self._config = yaml.safe_load(f)
        self._client = self._initialize_client()

    def _initialize_client(self):
        # Initialize based on YAML config
        pass

    @property
    def default_model(self) -> str:
        return self._config.get("model", "default")

    async def complete(self, messages, **kwargs):
        # Implementation using your client
        pass
```

### Local Model Provider

For local models (Ollama, llama.cpp, etc.):

```python
class LocalModelProvider:
    def __init__(self, model_path: str, context_size: int = 4096):
        self._model_path = model_path
        self._context_size = context_size
        # Load your model

    @property
    def default_model(self) -> str:
        return self._model_path

    async def complete(self, messages, **kwargs):
        # Run inference locally
        pass
```

### Azure OpenAI Provider

```python
from openai import AsyncAzureOpenAI

class AzureOpenAIProvider:
    def __init__(
        self,
        api_key: str,
        endpoint: str,
        deployment: str,
        api_version: str = "2024-02-01",
    ):
        self._deployment = deployment
        self._client = AsyncAzureOpenAI(
            api_key=api_key,
            azure_endpoint=endpoint,
            api_version=api_version,
        )

    @property
    def default_model(self) -> str:
        return self._deployment

    async def complete(self, messages, model=None, temperature=0.7, **kwargs):
        response = await self._client.chat.completions.create(
            model=model or self._deployment,
            messages=[{"role": m.role, "content": m.content} for m in messages],
            temperature=temperature,
        )
        return CompletionResponse(
            content=response.choices[0].message.content or "",
            model=response.model,
            tokens_used=response.usage.total_tokens if response.usage else 0,
            raw_response=response.model_dump(),
        )
```

## Error Handling

Providers should raise descriptive exceptions:

```python
class MyProviderError(Exception):
    def __init__(self, message: str, status_code: int | None = None):
        super().__init__(message)
        self.status_code = status_code

class MyCustomProvider:
    async def complete(self, messages, **kwargs):
        try:
            # API call
            pass
        except SomeAPIError as e:
            raise MyProviderError(f"API error: {e}", status_code=e.status)
```

## Testing Your Provider

```python
import pytest
from war_rig.providers import LLMProvider, Message

class TestMyProvider:
    def test_implements_protocol(self):
        provider = MyCustomProvider(api_key="test", endpoint="http://test")
        assert isinstance(provider, LLMProvider)

    @pytest.mark.asyncio
    async def test_complete_returns_response(self):
        provider = MyCustomProvider(api_key="test", endpoint="http://test")
        # Mock the API call
        response = await provider.complete([
            Message(role="user", content="Hello")
        ])
        assert response.content
        assert response.tokens_used >= 0
```

## File Structure

When adding a new provider, create:

```
war_rig/providers/
├── __init__.py          # Add exports
├── protocol.py          # Don't modify
├── factory.py           # Add to registry
├── openrouter.py        # Existing provider
└── myprovider.py        # Your new provider
```

## Checklist

- [ ] Implement `default_model` property
- [ ] Implement async `complete()` method
- [ ] Return proper `CompletionResponse` with all fields
- [ ] Handle errors gracefully
- [ ] Register provider with `register_provider()`
- [ ] Add environment variable configuration
- [ ] Write tests
- [ ] Update `.env.example` with new variables
