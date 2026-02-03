# llm-providers Architecture

## Overview

llm-providers is a standalone Python package providing a unified, protocol-based interface for interacting with various LLM providers. It enables applications to switch between different LLM backends (OpenRouter, Anthropic, OpenAI) without changing application code.

## Design Goals

1. **Provider Agnostic**: Write code once, use any supported LLM provider
2. **Type Safe**: Full type hints and Pydantic validation
3. **Minimal Dependencies**: Core protocol requires no external LLM libraries
4. **Async First**: Built for modern async Python applications
5. **Extensible**: Easy to add new providers via plugins

## Architecture Diagram

```
+-------------------+     +------------------+     +------------------+
|   Application     |     |   llm-providers  |     |   LLM APIs       |
|                   |     |                  |     |                  |
| create_provider() +---->+ ProviderFactory  +---->+ OpenRouter       |
|                   |     |        |         |     | Anthropic        |
| await complete()  |     |        v         |     | OpenAI           |
|                   |     | LLMProvider      |     |                  |
+-------------------+     | (Protocol)       |     +------------------+
                          +------------------+
```

## Core Components

### Protocol Layer (`protocol.py`)

The protocol layer defines the contract that all providers must satisfy:

```python
@runtime_checkable
class LLMProvider(Protocol):
    @property
    def default_model(self) -> str: ...

    async def complete(
        self,
        messages: list[Message],
        model: str | None = None,
        temperature: float = 0.7,
        **kwargs,
    ) -> CompletionResponse: ...
```

**Key Design Decisions:**

- Uses Python's `Protocol` for structural subtyping (duck typing)
- `@runtime_checkable` enables `isinstance()` checks at runtime
- No inheritance required - any class with matching methods satisfies the protocol
- Minimal interface - only `default_model` property and `complete` method required

### Data Types

```python
@dataclass(frozen=True)
class Message:
    role: str  # "system", "user", "assistant"
    content: str

@dataclass
class CompletionResponse:
    content: str
    model: str
    tokens_used: int
    raw_response: dict[str, Any] | None
```

### Configuration Layer (`config.py`)

Pydantic models provide type-safe configuration with validation:

```
ProviderConfig (Base)
├── OpenRouterConfig
├── AnthropicConfig
└── OpenAIConfig
```

Each config model includes:
- Required fields (e.g., `api_key`)
- Optional fields with defaults (e.g., `temperature=0.7`)
- Validation constraints (e.g., `temperature: float = Field(ge=0.0, le=2.0)`)

### Factory Layer (`factory.py`)

The factory provides multiple ways to create providers:

```python
# 1. Explicit creation
provider = create_provider("openrouter", api_key="...", default_model="...")

# 2. Environment-based
provider = get_provider_from_env()

# 3. Custom registration
register_provider("custom", MyCustomProvider)
```

**Plugin Discovery:**

External packages can register providers via entry points:

```toml
[project.entry-points."llm_providers.providers"]
myprovider = "mypackage.provider:MyProviderClass"
```

### Provider Implementations

Each provider implementation:

1. Accepts configuration in `__init__`
2. Implements `default_model` property
3. Implements async `complete` method
4. Converts internal `Message` format to provider-specific format
5. Wraps provider SDK calls with error handling
6. Returns standardized `CompletionResponse`

```
providers/
├── openrouter.py  # Uses OpenAI SDK with custom base_url
├── anthropic.py   # Uses Anthropic SDK directly
└── openai.py      # Uses OpenAI SDK directly
```

## Data Flow

```
1. Application creates provider
   |
   v
2. Application builds list of Message objects
   |
   v
3. Application calls provider.complete(messages)
   |
   v
4. Provider converts messages to SDK format
   |
   v
5. Provider makes async API call
   |
   v
6. Provider parses response into CompletionResponse
   |
   v
7. Application receives standardized response
```

## Error Handling Strategy

Each provider defines its own exception class:

```python
class OpenRouterProviderError(Exception):
    message: str
    status_code: int | None
    original_error: Exception | None
```

Errors are categorized:
- **Rate limit errors**: HTTP 429, provider-specific codes
- **Connection errors**: Network failures, timeouts
- **API errors**: Invalid requests, authentication failures
- **Unexpected errors**: Catch-all for unknown issues

## Extension Points

### Adding a New Provider

1. Create a new file in `providers/`
2. Implement the `LLMProvider` protocol
3. Add configuration model in `config.py`
4. Register in `factory.py` or via entry point

### Customizing Behavior

- Override `timeout_seconds` for slow models
- Pass provider-specific kwargs through `complete(**kwargs)`
- Access `raw_response` for provider-specific fields

## Dependencies

**Required:**
- `pydantic>=2.0.0` - Configuration and validation
- `pydantic-settings>=2.0.0` - Environment variable support
- `httpx>=0.25.0` - HTTP client with timeout support

**Provider-specific:**
- `openai>=1.0.0` - OpenRouter and OpenAI providers
- `anthropic>=0.39.0` - Anthropic provider

## Future Considerations

1. **Streaming Support**: Add streaming completions for real-time output
2. **Tool/Function Calling**: Standardized tool calling interface
3. **Token Counting**: Pre-flight token estimation
4. **Retry Logic**: Built-in exponential backoff with jitter
5. **Caching**: Optional response caching layer
6. **Observability**: Hooks for logging, metrics, tracing

## Security Notes

- API keys should be passed via environment variables, not code
- The package never logs API keys
- Timeout defaults are generous (10 min) to handle slow models
- No automatic retries by default (application should handle)
