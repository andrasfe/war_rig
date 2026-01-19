# Custom LLM Provider Integration Guide

This guide explains how to integrate a custom LLM API with War Rig.

## Overview

War Rig uses a provider abstraction layer that allows any LLM backend to be used. The system is designed to be:

- **Protocol-based**: No inheritance required, just implement the interface
- **API-agnostic**: Works with any API format, not just OpenAI-compatible
- **Plugin-friendly**: External packages auto-register via entry points (no war_rig changes needed)
- **Configuration-driven**: Provider selection via environment variables

## Quick Start (Zero Code Changes to War Rig)

The easiest way to add a custom provider is via **plugin discovery**. Create a separate Python package with your provider, and War Rig will auto-discover it.

### 1. Create Your Provider Package

```
my-warrig-provider/
├── pyproject.toml
└── my_provider/
    ├── __init__.py
    └── provider.py
```

### 2. Implement the LLMProvider Protocol

In `my_provider/provider.py`:

```python
import os
from war_rig.providers import Message, CompletionResponse

class MyCustomProvider:
    """Custom LLM provider - auto-configured from environment."""

    def __init__(self):
        # Read config from environment (plugin is responsible for this)
        self._api_key = os.environ["MY_PROVIDER_API_KEY"]
        self._endpoint = os.environ["MY_PROVIDER_ENDPOINT"]
        self._model = os.getenv("MY_PROVIDER_MODEL", "default-model")

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
        import httpx

        formatted_messages = [
            {"role": msg.role, "text": msg.content}
            for msg in messages
        ]

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

        return CompletionResponse(
            content=data["output"],
            model=data.get("model", self._model),
            tokens_used=data.get("usage", {}).get("total_tokens", 0),
            raw_response=data,
        )
```

### 3. Register via Entry Points

In `pyproject.toml`:

```toml
[project]
name = "my-warrig-provider"
version = "0.1.0"
dependencies = ["war_rig"]

[project.entry-points."war_rig.providers"]
myprovider = "my_provider.provider:MyCustomProvider"
```

### 4. Install and Configure

```bash
pip install my-warrig-provider
```

Set environment variables:

```bash
export LLM_PROVIDER=myprovider
export MY_PROVIDER_API_KEY=your-api-key
export MY_PROVIDER_ENDPOINT=https://api.example.com/v1/chat
```

That's it! War Rig will auto-discover your provider on startup.

---

## Alternative: Manual Registration

If you don't want to create a separate package, you can register programmatically:

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

### 2. Register Before Use

Call `register_provider()` before using any war_rig agents:

```python
from war_rig.providers import register_provider
from my_provider import MyCustomProvider

# Register at application startup
register_provider("mycustom", MyCustomProvider)
```

### 3. Configure Environment

```bash
export LLM_PROVIDER=mycustom
export MYCUSTOM_API_KEY=your-api-key
export MYCUSTOM_ENDPOINT=https://api.example.com/v1/chat
```

Note: With manual registration, your provider's `__init__` receives kwargs
from `create_provider()`, so you need to handle configuration explicitly.

---

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

## File Structure (Plugin Approach - Recommended)

Create a separate package:

```
my-warrig-provider/
├── pyproject.toml       # Entry point registration
├── my_provider/
│   ├── __init__.py
│   └── provider.py      # Your provider class
└── tests/
    └── test_provider.py
```

Example `pyproject.toml`:

```toml
[project]
name = "my-warrig-provider"
version = "0.1.0"
dependencies = ["war_rig", "httpx"]

[project.entry-points."war_rig.providers"]
myprovider = "my_provider.provider:MyCustomProvider"
```

## Checklist

### For Plugin Approach (No war_rig changes)
- [ ] Implement `default_model` property
- [ ] Implement async `complete()` method
- [ ] Return proper `CompletionResponse` with all fields
- [ ] Handle errors gracefully
- [ ] Read configuration from environment in `__init__` (no args)
- [ ] Add entry point in `pyproject.toml`
- [ ] Write tests
- [ ] `pip install` your package
- [ ] Set `LLM_PROVIDER=yourname` environment variable

### For Manual Registration
- [ ] Implement `default_model` property
- [ ] Implement async `complete()` method
- [ ] Return proper `CompletionResponse` with all fields
- [ ] Handle errors gracefully
- [ ] Call `register_provider()` at application startup
- [ ] Set `LLM_PROVIDER=yourname` environment variable
