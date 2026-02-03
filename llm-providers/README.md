# llm-providers

Provider-agnostic LLM abstraction layer supporting OpenRouter, Anthropic, and OpenAI.

## Overview

This package provides a unified interface for interacting with various LLM providers through a common Protocol-based abstraction. It allows you to write code once and easily switch between different LLM backends without changing your application logic.

## Features

- **Protocol-based design**: Uses Python's `Protocol` for structural subtyping (duck typing)
- **Multiple providers**: Supports OpenRouter, Anthropic, and OpenAI out of the box
- **Type-safe configuration**: Pydantic v2 models with validation
- **Async-first**: Built for modern async Python applications
- **Environment-based configuration**: Easy deployment with environment variables
- **Plugin system**: Register custom providers via entry points

## Installation

```bash
pip install llm-providers
```

Or with uv:

```bash
uv add llm-providers
```

## Quick Start

```python
from llm_providers import create_provider, Message

# Create a provider
provider = create_provider(
    "openrouter",
    api_key="sk-or-...",
    default_model="anthropic/claude-sonnet-4-20250514",
)

# Send messages
messages = [
    Message(role="system", content="You are a helpful assistant."),
    Message(role="user", content="Hello!"),
]

response = await provider.complete(messages)
print(response.content)
```

## Environment-Based Configuration

Create providers from environment variables:

```python
from llm_providers import get_provider_from_env

# Uses LLM_PROVIDER and provider-specific env vars
provider = get_provider_from_env()
```

### Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `LLM_PROVIDER` | Provider name | `openrouter` |
| `LLM_DEFAULT_MODEL` | Default model | Provider-specific |
| `LLM_TIMEOUT` | Request timeout (seconds) | `600` |
| `OPENROUTER_API_KEY` | OpenRouter API key | Required for OpenRouter |
| `ANTHROPIC_API_KEY` | Anthropic API key | Required for Anthropic |
| `OPENAI_API_KEY` | OpenAI API key | Required for OpenAI |

## Providers

### OpenRouter

Access multiple LLM providers through OpenRouter's unified API:

```python
from llm_providers import OpenRouterProvider

provider = OpenRouterProvider(
    api_key="sk-or-...",
    default_model="anthropic/claude-sonnet-4-20250514",
)
```

### Anthropic

Direct access to Claude models:

```python
from llm_providers import AnthropicProvider

provider = AnthropicProvider(
    api_key="sk-ant-...",
    default_model="claude-sonnet-4-20250514",
    max_tokens=4096,
)
```

### OpenAI

Direct access to GPT models:

```python
from llm_providers import OpenAIProvider

provider = OpenAIProvider(
    api_key="sk-...",
    default_model="gpt-4o",
)
```

## Custom Providers

Implement the `LLMProvider` protocol:

```python
from llm_providers import LLMProvider, Message, CompletionResponse

class MyProvider:
    @property
    def default_model(self) -> str:
        return "my-model"

    async def complete(
        self,
        messages: list[Message],
        model: str | None = None,
        temperature: float = 0.7,
        **kwargs,
    ) -> CompletionResponse:
        # Your implementation
        ...

# Register it
from llm_providers import register_provider
register_provider("custom", MyProvider)
```

Or use entry points in your `pyproject.toml`:

```toml
[project.entry-points."llm_providers.providers"]
myprovider = "mypackage.provider:MyProviderClass"
```

## License

MIT
