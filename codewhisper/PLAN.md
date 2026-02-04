# CodeWhisper LLM Integration Fix Plan

## Executive Summary

This document outlines a plan to fix CodeWhisper's LLM integration. The current implementation incorrectly attempts to use `llm_providers` directly for tool calling, when `llm_providers` is only designed to provide connection configuration (API keys, base URLs, model names). The fix requires restoring LangChain/LangGraph for the agent loop while using `llm_providers` as the configuration source.

---

## Problem Analysis

### What Was Removed (Commit `6dd311c`)

The following files were deleted:

| File | Purpose |
|------|---------|
| `agent/langchain_factory.py` | Factory to create LangChain models using llm_providers config |
| `agent/graph.py` | LangGraph StateGraph agent with ReAct pattern |
| `agent/state.py` | LangGraph state schema with message reducers |
| `agent/tools.py` | LangChain `@tool` decorated functions |
| `agent/citadel_tools.py` | LangChain tool wrappers for Citadel analysis |

Dependencies removed from `pyproject.toml`:
- `langgraph>=0.2.0`
- `langchain-core>=0.3.0`
- `langchain-anthropic>=0.2.0`
- `langchain-openai>=0.2.0`

### What Was Added (Incorrectly)

| File | Problem |
|------|---------|
| `agent/react_loop.py` | Custom ReAct loop using `provider.complete()` directly |
| `agent/protocol.py` | Custom ToolDefinition, ToolCall, ToolResult classes |
| `agent/message.py` | Custom Conversation, ConversationMessage classes |
| `agent/tools/registry.py` | Custom tool registry with OpenAI schema generation |

### Why The Current Approach Is Broken

1. **llm_providers does NOT support tool calling**
   - The `LLMProvider.complete()` method only accepts `messages`, `model`, `temperature`, and generic `**kwargs`
   - The `Message` class only supports `role` and `content` - no tool_calls field
   - The `CompletionResponse` only has `content`, `model`, `tokens_used`, `raw_response`
   - There is no parsing of tool calls from responses

2. **The react_loop.py workaround is fragile**
   - Line 249-257: Passes `tools=tools_schema` to `provider.complete()` as kwargs
   - The provider implementations ignore this parameter entirely
   - Line 309-327: Attempts to extract tool_calls from `raw_response` - this will never work because providers don't return tool calls
   - Line 329-360: Falls back to parsing `[Tool Call: name(args)]` from text - unreliable

3. **Tool message handling is incorrect**
   - Line 291-295: Converts tool results to user messages with `[Tool Result (...)]` prefix
   - This is a hack that doesn't match how LLMs expect tool results

---

## Correct Architecture

### Layer Responsibilities

```
+------------------+     +-----------------+     +------------------+
|  llm_providers   | --> | langchain_      | --> | LangGraph        |
|                  |     | factory.py      |     | Agent            |
+------------------+     +-----------------+     +------------------+
        |                        |                       |
  Provides:               Creates:                 Handles:
  - API key              - ChatAnthropic          - Tool binding
  - Base URL             - ChatOpenAI             - Agent loop
  - Model name           - (using config          - State management
  - Provider type          from llm_providers)    - Structured output
```

### llm_providers Role (Configuration Only)

```python
# What llm_providers provides:
provider = get_provider_from_env()
# - provider._api_key (API key)
# - provider._default_model (model name)
# - provider._base_url (if applicable)
# - type(provider).__name__ (provider type: OpenRouterProvider, AnthropicProvider, etc.)

# What llm_providers does NOT provide:
# - Tool calling support
# - Structured output
# - Agent loops
# - Message history management
```

### LangChain/LangGraph Role (Agent Functionality)

```python
# langchain_factory.py extracts config from llm_providers and creates LangChain models:
from llm_providers import get_provider_from_env

def get_langchain_model(provider=None, model=None, ...):
    # Get config from llm_providers
    llm_provider = get_provider_from_env(provider_name=provider)
    api_key = llm_provider._api_key
    provider_type = type(llm_provider).__name__.lower()

    # Create appropriate LangChain model
    if "anthropic" in provider_type:
        from langchain_anthropic import ChatAnthropic
        return ChatAnthropic(api_key=api_key, model=model, ...)
    elif "openrouter" in provider_type or "openai" in provider_type:
        from langchain_openai import ChatOpenAI
        return ChatOpenAI(api_key=api_key, base_url=..., model=model, ...)
```

```python
# LangGraph provides the agent loop with tool calling:
llm = get_langchain_model()
llm_with_tools = llm.bind_tools(tools)  # LangChain handles tool schema

# LangGraph handles the ReAct loop:
graph_builder = StateGraph(AgentState)
graph_builder.add_node("agent", agent_node)
graph_builder.add_node("tools", ToolNode(tools))
# ... proper message/tool_call handling built-in
```

---

## Implementation Plan

### Phase 1: Restore Dependencies

**File: `pyproject.toml`**

Add back LangChain/LangGraph dependencies:

```toml
dependencies = [
    # Agent framework
    "langgraph>=0.2.0",
    "langchain-core>=0.3.0",
    "langchain-anthropic>=0.2.0",
    "langchain-openai>=0.2.0",
    # LLM configuration (NOT for tool calling)
    "llm-providers",
    # ... rest unchanged
]
```

### Phase 2: Restore langchain_factory.py

**File: `src/codewhisper/agent/langchain_factory.py`**

Restore the deleted file from commit `6dd311c~1`. Key functions:

1. `_get_provider_info_from_llm_providers()` - Extract API key, provider type, and config from llm_providers
2. `_create_openrouter_model()` - Create ChatOpenAI for OpenRouter
3. `_create_anthropic_model()` - Create ChatAnthropic for Anthropic
4. `_create_openai_model()` - Create ChatOpenAI for OpenAI
5. `get_langchain_model()` - Main entry point

The key insight is that this module bridges llm_providers (config) to LangChain (functionality):

```python
# Bridge pattern:
provider = get_provider_from_env()  # llm_providers
api_key = provider._api_key         # Extract config
return ChatAnthropic(api_key=api_key, ...)  # Create LangChain model
```

### Phase 3: Restore LangGraph Agent

**File: `src/codewhisper/agent/graph.py`**

Restore the deleted file. Key components:

1. `CodeWhisperAgent` class
   - Uses `get_langchain_model()` from langchain_factory
   - Builds LangGraph StateGraph
   - Binds tools to LLM via `llm.bind_tools()`
   - Implements proper ReAct pattern

2. Graph structure:
   ```
   START -> agent -> (tools -> minion_processor -> agent)* -> END
   ```

3. Proper tool calling:
   ```python
   llm_with_tools = self.llm.bind_tools(tools)  # LangChain handles schema
   response = await llm_with_tools.ainvoke(messages)
   # response.tool_calls populated by LangChain
   ```

**File: `src/codewhisper/agent/state.py`**

Restore the LangGraph state schema:

1. `ConversationMessage` - Our message type
2. `SkillContext`, `CodeSearchResult`, `FileContent` - Result types
3. `AgentState` - LangGraph state with `add_messages` reducer

### Phase 4: Restore LangChain Tools

**File: `src/codewhisper/agent/tools.py`**

Restore the LangChain tool definitions:

```python
from langchain_core.tools import tool
from pydantic import BaseModel, Field

class SkillSearchInput(BaseModel):
    query: str = Field(..., description="...")
    limit: int = Field(default=5, ...)

@tool(args_schema=SkillSearchInput)
def search_skills(query: str, limit: int = 5) -> str:
    """Search for relevant skills by keyword."""
    # Implementation
```

**File: `src/codewhisper/agent/citadel_tools.py`**

Restore Citadel analysis tools with `@tool` decorators.

### Phase 5: Update Minion Processor

**File: `src/codewhisper/agent/minion.py`**

The minion can continue using llm_providers directly since it only needs simple completion (no tools). However, consider whether to also migrate it to LangChain for consistency.

Option A: Keep llm_providers (current approach works for simple completion)
Option B: Use langchain_factory for consistency

Recommendation: Keep llm_providers for minion since it's simpler and doesn't need tool calling.

### Phase 6: Clean Up Redundant Files

**Files to remove:**

| File | Reason |
|------|--------|
| `agent/react_loop.py` | Replaced by graph.py |
| `agent/protocol.py` | AgentState moved to state.py, ToolCall/ToolResult handled by LangChain |
| `agent/message.py` | Conversation handled by LangGraph state |
| `agent/tools/registry.py` | Tool registration handled by LangChain @tool decorator |

**Files to keep (adapt):**

| File | Notes |
|------|-------|
| `agent/tools/citadel.py` | Convert to use @tool decorators |
| `agent/tools/code.py` | Convert to use @tool decorators |
| `agent/tools/knowledge.py` | Convert to use @tool decorators |
| `agent/tools/__init__.py` | Update exports |

### Phase 7: Update Agent Module Init

**File: `src/codewhisper/agent/__init__.py`**

Update exports:
```python
from codewhisper.agent.graph import CodeWhisperAgent, create_agent
from codewhisper.agent.langchain_factory import get_langchain_model
from codewhisper.agent.state import AgentState
```

### Phase 8: Update Tests

Restore deleted tests:
- `tests/test_agent/test_langchain_factory.py`
- `tests/test_agent/test_graph.py`
- `tests/test_agent/test_state.py`
- `tests/test_agent/test_tools.py`
- `tests/test_agent/test_citadel_tools.py`

Remove tests for deleted code:
- `tests/test_agent/test_protocol.py`
- `tests/test_agent/test_message.py`
- `tests/test_agent/test_registry.py`

---

## Migration Strategy

### Step-by-Step Execution

1. **Create feature branch**
   ```bash
   git checkout -b fix/restore-langchain-integration
   ```

2. **Restore dependencies** (pyproject.toml)

3. **Restore core files** from git history:
   ```bash
   git show 6dd311c~1:codewhisper/src/codewhisper/agent/langchain_factory.py > src/codewhisper/agent/langchain_factory.py
   git show 6dd311c~1:codewhisper/src/codewhisper/agent/graph.py > src/codewhisper/agent/graph.py
   git show 6dd311c~1:codewhisper/src/codewhisper/agent/state.py > src/codewhisper/agent/state.py
   ```

4. **Merge tool implementations**
   - Keep the current `tools/citadel.py`, `tools/code.py`, `tools/knowledge.py` implementations
   - Convert them to use `@tool` decorators from langchain_core
   - Restore `tools.py` structure if needed

5. **Update minion.py** (optional - may work as-is)

6. **Remove redundant files**
   - `react_loop.py`
   - `protocol.py`
   - `message.py`
   - `tools/registry.py`

7. **Restore tests**

8. **Run test suite**
   ```bash
   uv run pytest tests/ -x
   ```

9. **Type check**
   ```bash
   uv run mypy codewhisper/
   ```

---

## Key Design Decisions

### 1. llm_providers as Configuration Layer

llm_providers should ONLY be used for:
- Reading environment variables (LLM_PROVIDER, API keys)
- Creating provider instances that expose configuration
- Simple completions where tool calling is not needed (minion)

llm_providers should NOT be used for:
- Tool calling (not supported)
- Agent loops (use LangGraph)
- Structured output (use LangChain)

### 2. LangChain Model Bridge Pattern

The `langchain_factory.py` module acts as a bridge:

```
Environment Variables
        |
        v
llm_providers.get_provider_from_env()
        |
        v
Extract: api_key, base_url, model, provider_type
        |
        v
langchain_factory.get_langchain_model()
        |
        v
ChatAnthropic / ChatOpenAI (LangChain models)
        |
        v
LangGraph agent with tool binding
```

### 3. Tool Definition Approach

Use LangChain's `@tool` decorator with Pydantic schemas:

```python
@tool(args_schema=MyInputSchema)
def my_tool(arg1: str, arg2: int = 5) -> str:
    """Tool docstring shown to LLM."""
    # Implementation
```

Benefits:
- Automatic JSON schema generation
- Type validation via Pydantic
- Docstring extraction for descriptions
- Compatible with `llm.bind_tools()`

---

## Risks and Mitigations

| Risk | Mitigation |
|------|------------|
| Breaking existing CLI | Maintain same public API in `__init__.py` |
| Model behavior differences | Test with multiple providers |
| Missing tool implementations | Carefully merge current tool code |
| Test failures | Restore tests before removing code |

---

## Success Criteria

1. `uv run codewhisper` CLI works with tool calling
2. All tests pass
3. Type checking passes (mypy)
4. Tools are properly called by the agent
5. Minion summarization continues to work
6. Environment-based provider selection works via llm_providers

---

## File Inventory

### Files to Restore (from git history)

- `src/codewhisper/agent/langchain_factory.py`
- `src/codewhisper/agent/graph.py`
- `src/codewhisper/agent/state.py`
- `tests/test_agent/test_langchain_factory.py`
- `tests/test_agent/test_graph.py`
- `tests/test_agent/test_state.py`

### Files to Remove

- `src/codewhisper/agent/react_loop.py`
- `src/codewhisper/agent/protocol.py`
- `src/codewhisper/agent/message.py`
- `src/codewhisper/agent/tools/registry.py`
- `tests/test_agent/test_protocol.py`
- `tests/test_agent/test_message.py`
- `tests/test_agent/test_registry.py`

### Files to Modify

- `pyproject.toml` (add LangChain dependencies)
- `src/codewhisper/agent/__init__.py` (update exports)
- `src/codewhisper/agent/tools/__init__.py` (update to use @tool)
- `src/codewhisper/agent/tools/citadel.py` (convert to @tool)
- `src/codewhisper/agent/tools/code.py` (convert to @tool)
- `src/codewhisper/agent/tools/knowledge.py` (convert to @tool)

### Files to Keep Unchanged

- `src/codewhisper/agent/minion.py` (llm_providers is fine for simple completion)
- `src/codewhisper/cli.py`
- `src/codewhisper/config.py`
- `src/codewhisper/skills/*`
- `src/codewhisper/search/*`
