# Tool Calling Support for LLM Providers

This document describes how to implement tool calling support in custom LLM providers for war_rig.

## Overview

War Rig's provider protocol supports optional tool calling. Providers that implement tool support can be used with LangGraph agents that require function/tool calling capabilities.

## Protocol Types

The following types are defined in `war_rig.providers.protocol`:

```python
@dataclass(frozen=True)
class ToolCallFunction:
    """Function call details within a tool call.

    Attributes:
        name: The name of the function to call.
        arguments: JSON string of the function arguments.
    """
    name: str
    arguments: str  # JSON string


@dataclass(frozen=True)
class ProviderToolCall:
    """A tool call from the LLM response.

    Attributes:
        id: Unique identifier for this tool call.
        type: The type of tool call (always "function" for now).
        function: The function call details.
    """
    id: str
    type: str  # typically "function"
    function: ToolCallFunction


@dataclass
class CompletionResponse:
    """Response from an LLM completion request.

    Attributes:
        content: The generated text content.
        model: The model that generated the response.
        tokens_used: Total tokens consumed (prompt + completion).
        raw_response: Optional raw response for debugging.
        tool_calls: Optional list of tool calls requested by the LLM.
    """
    content: str
    model: str
    tokens_used: int
    raw_response: dict[str, Any] | None = None
    tool_calls: list[ProviderToolCall] | None = None

    @property
    def has_tool_calls(self) -> bool:
        """Check if the response contains tool calls."""
        return bool(self.tool_calls)
```

## Implementing Tool Support

Tool calling is **optional**. Providers that don't support tools can simply ignore the `tools` parameter and return `tool_calls=None`.

### Step 1: Accept tools from kwargs

The `complete()` method receives tools via `**kwargs`:

```python
async def complete(
    self,
    messages: list[Message],
    model: str | None = None,
    temperature: float = 0.7,
    **kwargs: Any,
) -> CompletionResponse:
    # Extract tools if provided
    tools = kwargs.pop("tools", None)
```

### Step 2: Pass tools to your underlying API

Tools are passed in OpenAI-compatible format:

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

Pass this to your API:

```python
if tools:
    api_params["tools"] = tools
```

### Step 3: Parse tool calls from response

When the LLM returns tool calls, convert them to `ProviderToolCall` objects:

```python
from war_rig.providers.protocol import ProviderToolCall, ToolCallFunction

# Parse from your API's response format
if response.tool_calls:
    tool_calls = [
        ProviderToolCall(
            id=tc.id,              # Unique ID for this call
            type=tc.type,          # Usually "function"
            function=ToolCallFunction(
                name=tc.function.name,
                arguments=tc.function.arguments,  # JSON string
            ),
        )
        for tc in response.tool_calls
    ]
else:
    tool_calls = None
```

### Step 4: Return tool_calls in CompletionResponse

```python
return CompletionResponse(
    content=content,
    model=model,
    tokens_used=tokens_used,
    raw_response=raw_response,
    tool_calls=tool_calls,  # list[ProviderToolCall] | None
)
```

## Complete Example

Here's a minimal provider with tool support:

```python
from typing import Any
from war_rig.providers.protocol import (
    CompletionResponse,
    Message,
    ProviderToolCall,
    ToolCallFunction,
)


class MyProvider:
    """Example provider with tool calling support."""

    def __init__(self, api_key: str, default_model: str = "my-model"):
        self._api_key = api_key
        self._default_model = default_model
        self._client = MyAPIClient(api_key)

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

        # Extract tools from kwargs
        tools = kwargs.pop("tools", None)

        # Build API request
        api_messages = [{"role": m.role, "content": m.content} for m in messages]

        api_params = {
            "model": resolved_model,
            "messages": api_messages,
            "temperature": temperature,
            **kwargs,
        }

        if tools:
            api_params["tools"] = tools

        # Make API call
        response = await self._client.chat_completions(**api_params)

        # Parse tool calls if present
        tool_calls = None
        if hasattr(response, "tool_calls") and response.tool_calls:
            tool_calls = [
                ProviderToolCall(
                    id=tc.id,
                    type=tc.type,
                    function=ToolCallFunction(
                        name=tc.function.name,
                        arguments=tc.function.arguments,
                    ),
                )
                for tc in response.tool_calls
            ]

        return CompletionResponse(
            content=response.content or "",
            model=resolved_model,
            tokens_used=response.usage.total_tokens if response.usage else 0,
            raw_response={"response": response},
            tool_calls=tool_calls,
        )
```

## Registering Your Provider

Register via Python entry points in `pyproject.toml`:

```toml
[project.entry-points."war_rig.providers"]
myprovider = "mypackage.provider:MyProvider"
```

Then use:

```bash
export LLM_PROVIDER=myprovider
```

## LangChain Integration

For providers used with LangChain/LangGraph (e.g., CodeWhisper), you can optionally implement `get_langchain_model()`:

```python
class MyProvider:
    # ... provider implementation ...

    def get_langchain_model(self, model: str | None = None) -> BaseChatModel:
        """Return a LangChain-compatible chat model.

        This is optional. If implemented, CodeWhisper and other LangChain-based
        tools will use this method instead of creating their own model.
        """
        from langchain_openai import ChatOpenAI

        return ChatOpenAI(
            model=model or self._default_model,
            api_key=self._api_key,
            base_url=self._base_url,
        )
```

## Testing Tool Support

Test your provider with tools:

```python
import asyncio
from war_rig.providers import Message

async def test_tool_calling():
    provider = MyProvider(api_key="...")

    tools = [
        {
            "type": "function",
            "function": {
                "name": "get_weather",
                "description": "Get weather for a location",
                "parameters": {
                    "type": "object",
                    "properties": {
                        "location": {"type": "string"}
                    },
                    "required": ["location"]
                }
            }
        }
    ]

    messages = [
        Message(role="user", content="What's the weather in London?")
    ]

    response = await provider.complete(messages, tools=tools)

    if response.has_tool_calls:
        for tc in response.tool_calls:
            print(f"Tool: {tc.function.name}")
            print(f"Args: {tc.function.arguments}")
    else:
        print(f"Content: {response.content}")

asyncio.run(test_tool_calling())
```

## Summary

| Requirement | Status |
|-------------|--------|
| Accept `tools` in kwargs | Required for tool support |
| Pass tools to API | Required for tool support |
| Parse tool calls from response | Required for tool support |
| Return `ProviderToolCall` objects | Required for tool support |
| Implement `get_langchain_model()` | Optional (for LangChain integration) |

Tool support is **fully optional**. Providers without tool support work fine for basic completion tasks - they simply won't work with tool-calling agents.
