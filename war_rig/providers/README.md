# war_rig.providers - LLM Provider Interface

This package provides a **provider-agnostic** interface for LLM providers. Implement this interface to add support for any LLM backend.

## Quick Start: Implement Your Own Provider

### Step 1: Create your provider class

```python
import os
from typing import Any
from war_rig.providers.protocol import CompletionResponse, Message

class MyProvider:
    """My custom LLM provider."""

    def __init__(self):
        # Read YOUR OWN environment variables
        self._api_key = os.environ.get("MY_PROVIDER_API_KEY")
        self._default_model = os.environ.get("LLM_DEFAULT_MODEL", "my-default-model")

        # Initialize your client
        self._client = MyAPIClient(api_key=self._api_key)

    @property
    def default_model(self) -> str:
        """Required: Return the default model name."""
        return self._default_model

    async def complete(
        self,
        messages: list[Message],
        model: str | None = None,
        temperature: float = 0.7,
        **kwargs: Any,
    ) -> CompletionResponse:
        """Required: Send messages to LLM and return response."""

        # Convert messages to your API format
        api_messages = [{"role": m.role, "content": m.content} for m in messages]

        # Call your API
        response = await self._client.chat(
            model=model or self._default_model,
            messages=api_messages,
            temperature=temperature,
        )

        # Return CompletionResponse
        return CompletionResponse(
            content=response.text,
            model=model or self._default_model,
            tokens_used=response.usage.total_tokens if response.usage else 0,
        )
```

### Step 2: Register via entry point

Add to your `pyproject.toml`:

```toml
[project.entry-points."war_rig.providers"]
myprovider = "mypackage.provider:MyProvider"
```

### Step 3: Use it

```bash
export LLM_PROVIDER=myprovider
export LLM_DEFAULT_MODEL=my-model
export MY_PROVIDER_API_KEY=sk-xxx

# Now war_rig uses your provider
python -c "from war_rig.providers import get_provider_from_env; print(get_provider_from_env())"
```

---

## Required Interface

Your provider must implement:

| Member | Type | Description |
|--------|------|-------------|
| `default_model` | `property -> str` | Returns the default model name |
| `complete()` | `async method` | Sends messages, returns `CompletionResponse` |

### The `complete()` method signature

```python
async def complete(
    self,
    messages: list[Message],      # Conversation history
    model: str | None = None,     # Model override (use default_model if None)
    temperature: float = 0.7,     # Sampling temperature
    **kwargs: Any,                # Extra params (including 'tools' for tool calling)
) -> CompletionResponse:
```

### Message format

```python
from war_rig.providers.protocol import Message

# Message has two fields:
message = Message(
    role="user",      # One of: "system", "user", "assistant"
    content="Hello",  # The text content
)
```

### CompletionResponse format

```python
from war_rig.providers.protocol import CompletionResponse

response = CompletionResponse(
    content="Hello! How can I help?",  # Required: The LLM's response text
    model="my-model",                   # Required: Model that was used
    tokens_used=150,                    # Required: Total tokens consumed
    raw_response=None,                  # Optional: Raw API response for debugging
    tool_calls=None,                    # Optional: List of tool calls (see below)
)
```

---

## Adding Tool Calling Support (Optional)

If your LLM supports function/tool calling, implement this:

### Step 1: Accept tools from kwargs

```python
async def complete(self, messages, model=None, temperature=0.7, **kwargs):
    # Extract tools if provided
    tools = kwargs.pop("tools", None)

    # Pass to your API
    if tools:
        response = await self._client.chat(messages=..., tools=tools)
    else:
        response = await self._client.chat(messages=...)
```

### Step 2: Parse and return tool calls

```python
from war_rig.providers.protocol import (
    CompletionResponse,
    ProviderToolCall,
    ToolCallFunction,
)

async def complete(self, messages, model=None, temperature=0.7, **kwargs):
    tools = kwargs.pop("tools", None)

    response = await self._client.chat(
        messages=[{"role": m.role, "content": m.content} for m in messages],
        tools=tools,
    )

    # Parse tool calls from response
    tool_calls = None
    if response.tool_calls:
        tool_calls = [
            ProviderToolCall(
                id=tc.id,                    # Unique ID for this call
                type="function",             # Always "function" for now
                function=ToolCallFunction(
                    name=tc.function.name,       # Tool name to call
                    arguments=tc.function.arguments,  # JSON string of args
                ),
            )
            for tc in response.tool_calls
        ]

    return CompletionResponse(
        content=response.content or "",
        model=model or self._default_model,
        tokens_used=response.usage.total_tokens if response.usage else 0,
        tool_calls=tool_calls,
    )
```

### Tool schema format (what you receive in `tools`)

```python
tools = [
    {
        "type": "function",
        "function": {
            "name": "search_code",
            "description": "Search for code patterns",
            "parameters": {
                "type": "object",
                "properties": {
                    "query": {"type": "string", "description": "Search query"}
                },
                "required": ["query"]
            }
        }
    }
]
```

---

## LangChain Integration (Optional)

If your provider should work with LangChain/LangGraph tools (like CodeWhisper), add:

```python
class MyProvider:
    # ... required methods ...

    def get_langchain_model(self, model: str | None = None):
        """Optional: Return a LangChain BaseChatModel."""
        from langchain_openai import ChatOpenAI

        return ChatOpenAI(
            model=model or self._default_model,
            api_key=self._api_key,
            base_url=self._base_url,  # If OpenAI-compatible
        )
```

---

## Complete Example

```python
"""my_provider.py - Complete provider implementation."""

import os
from typing import Any

from war_rig.providers.protocol import (
    CompletionResponse,
    Message,
    ProviderToolCall,
    ToolCallFunction,
)


class MyProvider:
    """Custom LLM provider with tool support."""

    def __init__(self):
        self._api_key = os.environ.get("MY_PROVIDER_API_KEY")
        if not self._api_key:
            raise KeyError("MY_PROVIDER_API_KEY environment variable required")

        self._default_model = os.environ.get("LLM_DEFAULT_MODEL", "my-model-v1")
        self._base_url = os.environ.get("MY_PROVIDER_BASE_URL", "https://api.myprovider.com/v1")

        # Initialize async client
        from openai import AsyncOpenAI
        self._client = AsyncOpenAI(
            api_key=self._api_key,
            base_url=self._base_url,
        )

    @property
    def default_model(self) -> str:
        return self._default_model

    async def complete(
        self,
        messages: list[Message],
        model: str | None = None,
        temperature: float = 0.7,
        **kwargs: Any,
    ) -> CompletionResponse:
        resolved_model = model or self._default_model
        tools = kwargs.pop("tools", None)

        # Build API request
        api_messages = [{"role": m.role, "content": m.content} for m in messages]

        api_params = {
            "model": resolved_model,
            "messages": api_messages,
            "temperature": temperature,
        }
        if tools:
            api_params["tools"] = tools

        # Call API
        response = await self._client.chat.completions.create(**api_params)

        # Extract content
        content = response.choices[0].message.content or ""

        # Extract tool calls if present
        tool_calls = None
        if response.choices[0].message.tool_calls:
            tool_calls = [
                ProviderToolCall(
                    id=tc.id,
                    type=tc.type,
                    function=ToolCallFunction(
                        name=tc.function.name,
                        arguments=tc.function.arguments,
                    ),
                )
                for tc in response.choices[0].message.tool_calls
            ]

        return CompletionResponse(
            content=content,
            model=resolved_model,
            tokens_used=response.usage.total_tokens if response.usage else 0,
            tool_calls=tool_calls,
        )

    def get_langchain_model(self, model: str | None = None):
        """Return LangChain-compatible model for tool binding."""
        from langchain_openai import ChatOpenAI

        return ChatOpenAI(
            model=model or self._default_model,
            api_key=self._api_key,
            base_url=self._base_url,
        )
```

**pyproject.toml:**
```toml
[project.entry-points."war_rig.providers"]
myprovider = "my_provider:MyProvider"
```

**Usage:**
```bash
export LLM_PROVIDER=myprovider
export MY_PROVIDER_API_KEY=sk-xxx
export LLM_DEFAULT_MODEL=my-model-v2
```

---

## Environment Variables

| Variable | Description |
|----------|-------------|
| `LLM_PROVIDER` | Provider name (matches entry point name) |
| `LLM_DEFAULT_MODEL` | Default model for completions |
| `LLM_REQUEST_TIMEOUT` | Request timeout in seconds (optional) |
| Provider-specific | Your provider reads its own keys (e.g., `MY_PROVIDER_API_KEY`) |

---

## Testing Your Provider

```python
import asyncio
from war_rig.providers import Message

async def test():
    from my_provider import MyProvider

    provider = MyProvider()

    messages = [
        Message(role="system", content="You are helpful."),
        Message(role="user", content="Say hello"),
    ]

    response = await provider.complete(messages)
    print(f"Response: {response.content}")
    print(f"Tokens: {response.tokens_used}")

asyncio.run(test())
```

### Test with tools

```python
async def test_tools():
    provider = MyProvider()

    tools = [{
        "type": "function",
        "function": {
            "name": "get_weather",
            "description": "Get weather",
            "parameters": {
                "type": "object",
                "properties": {"location": {"type": "string"}},
                "required": ["location"]
            }
        }
    }]

    messages = [Message(role="user", content="What's the weather in London?")]
    response = await provider.complete(messages, tools=tools)

    if response.tool_calls:
        for tc in response.tool_calls:
            print(f"Tool: {tc.function.name}, Args: {tc.function.arguments}")
    else:
        print(f"Response: {response.content}")

asyncio.run(test_tools())
```

---

## Summary

1. **Create class** with `default_model` property and `complete()` async method
2. **Read your own env vars** for API keys in `__init__`
3. **Return `CompletionResponse`** with content, model, tokens_used
4. **Register via entry point** in pyproject.toml
5. **Set `LLM_PROVIDER=yourname`** to use it

Tool support and LangChain integration are optional.
