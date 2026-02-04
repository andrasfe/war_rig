# CodeWhisper Architecture

## Overview

CodeWhisper is an interactive chatbot CLI for exploring mainframe codebases. This document describes the refactored architecture that uses `llm_providers` directly instead of LangChain/LangGraph, implementing a simple ReAct-style agent loop.

## Design Goals

1. **Simplicity**: Replace LangGraph's complex state machine with a simple ReAct loop
2. **Direct LLM Access**: Use `llm_providers` protocol directly for provider-agnostic LLM calls
3. **Preserved Functionality**: Keep all existing tools and minion support
4. **Testability**: Clean interfaces that are easy to mock and test
5. **Type Safety**: Full type hints and Pydantic models throughout
6. **Offline-Capable**: Skills work without internet (only LLM needs connectivity)

## System Architecture

```
+------------------+     +-------------------+     +------------------+
|                  |     |                   |     |                  |
|    CLI (typer)   |---->|  CodeWhisperAgent |---->|   LLMProvider    |
|                  |     |                   |     |  (llm_providers) |
+------------------+     +-------------------+     +------------------+
                               |       ^
                               |       |
                    tool calls |       | tool results
                               v       |
                         +------------------+
                         |                  |
                         |   Tool Registry  |
                         |                  |
                         +------------------+
                               |
          +--------------------+--------------------+
          |                    |                    |
    +----------+        +------------+       +-----------+
    | Knowledge |        |  Citadel   |       |  Minion   |
    |   Tools   |        |   Tools    |       | Processor |
    +----------+        +------------+       +-----------+
```

## Core Components

### 1. Message Protocol (`agent/message.py`)

Simple message types for conversation history, compatible with `llm_providers.Message`:

```python
@dataclass
class ConversationMessage:
    """A message in the conversation."""
    role: Literal["system", "user", "assistant", "tool"]
    content: str
    name: str | None = None  # Tool name for tool messages
    tool_calls: list[ToolCall] | None = None  # For assistant messages with tool calls
    tool_call_id: str | None = None  # For tool result messages

    def to_provider_message(self) -> Message:
        """Convert to llm_providers Message format."""
        ...
```

### 2. Tool Protocol (`agent/protocol.py`)

Clean tool interface without LangChain dependencies:

```python
@dataclass
class ToolDefinition:
    """Definition of a tool available to the agent."""
    name: str
    description: str
    parameters: dict[str, Any]  # JSON Schema for parameters
    handler: Callable[..., Awaitable[str]]  # Async function to execute

@dataclass
class ToolCall:
    """A tool call requested by the LLM."""
    id: str
    name: str
    arguments: dict[str, Any]

@dataclass
class ToolResult:
    """Result of executing a tool."""
    tool_call_id: str
    name: str
    content: str
    error: bool = False
```

### 3. ReAct Agent Loop (`agent/react_loop.py`)

The core agent implements a simple ReAct (Reason + Act) loop:

```
1. Receive user message
2. Build messages array (system + history + user)
3. Call LLM with tools definition
4. If response has tool_calls:
   a. Execute each tool
   b. Optionally summarize large results (minion)
   c. Append tool results to messages
   d. Go to step 3
5. If response is final text:
   a. Return assistant response
```

Key design points:
- **No State Machine**: Simple while loop instead of LangGraph's StateGraph
- **Direct Provider Calls**: Uses `provider.complete()` with tool schema in kwargs
- **Streaming Support**: Optional streaming for long responses
- **Max Iterations**: Safety limit to prevent infinite loops (default: 10)

### 4. Tool Registry (`agent/tools/registry.py`)

Central registry for all available tools:

```python
class ToolRegistry:
    """Registry of tools available to the agent."""

    def __init__(self):
        self._tools: dict[str, ToolDefinition] = {}

    def register(self, tool: ToolDefinition) -> None: ...
    def get(self, name: str) -> ToolDefinition | None: ...
    def list_tools(self) -> list[ToolDefinition]: ...
    def to_openai_schema(self) -> list[dict]: ...

    async def execute(self, tool_call: ToolCall) -> ToolResult: ...
```

### 5. Minion Processor (`agent/minion.py`)

Summarizes large tool results before passing to main agent:

```python
class MinionProcessor:
    """Uses a smaller model to summarize large tool outputs."""

    def __init__(self, provider: LLMProvider | None = None):
        self._provider = provider or get_provider_from_env()

    async def summarize_result(
        self,
        tool_name: str,
        result: str
    ) -> str:
        """Summarize if result exceeds threshold."""
        ...
```

## Tool Categories

### Knowledge Tools (`agent/tools/knowledge.py`)

| Tool | Purpose |
|------|---------|
| `search_skills` | Find skills by keyword in the skills index |
| `load_skill` | Load full content of a specific skill |

### Code Tools (`agent/tools/code.py`)

| Tool | Purpose |
|------|---------|
| `search_code` | Search source files with regex patterns |
| `read_file` | Read contents of a source file |

### Citadel Analysis Tools (`agent/tools/citadel.py`)

| Tool | Purpose |
|------|---------|
| `citadel_analyze_file` | Full structural analysis |
| `citadel_get_functions` | List functions/paragraphs |
| `citadel_get_callouts` | Get references (calls, includes) |
| `citadel_get_includes` | Get preprocessor includes |
| `citadel_get_function_body` | Extract function source |
| `citadel_get_function_bodies` | Batch extract functions |
| `citadel_get_file_stats` | Structural statistics |
| `citadel_get_callers` | Find callers of a function |
| `citadel_get_sequence_diagrams` | Generate call chain diagrams |
| `citadel_get_dead_code` | Find unreferenced code |
| `citadel_get_flow_diagram` | Generate control flow diagram |
| `citadel_get_file_summary` | Compact file summary |
| `citadel_get_analysis_patterns` | Extract code patterns |

## Data Flow

### Conversation Flow

```
User Query
    |
    v
+-------------------+
| Build Messages    |  System prompt + conversation history
+-------------------+
    |
    v
+-------------------+
| LLM Complete      |  provider.complete(messages, tools=...)
+-------------------+
    |
    +---> No tool calls ---> Return response
    |
    v (has tool calls)
+-------------------+
| Execute Tools     |  Parallel tool execution via registry
+-------------------+
    |
    v
+-------------------+
| Minion Summary    |  Summarize large results (if enabled)
+-------------------+
    |
    v
+-------------------+
| Append Results    |  Add tool results to conversation
+-------------------+
    |
    +---> Loop back to LLM Complete (iteration++)
```

### Tool Execution Flow

```
ToolCall from LLM
    |
    v
+-------------------+
| Lookup Handler    |  registry.get(tool_call.name)
+-------------------+
    |
    v
+-------------------+
| Validate Args     |  Check against JSON schema
+-------------------+
    |
    v
+-------------------+
| Execute Handler   |  await tool.handler(**args)
+-------------------+
    |
    v
+-------------------+
| Check Size        |  len(result) > threshold?
+-------------------+
    |
    +---> No ---> Return ToolResult
    |
    v (yes, and minions enabled)
+-------------------+
| Minion Summarize  |  Compact large results
+-------------------+
    |
    v
Return ToolResult with summarized content
```

## Message Format

### Tool Definition Schema (OpenAI-compatible)

The agent converts tools to OpenAI function calling format for the LLM:

```json
{
  "type": "function",
  "function": {
    "name": "search_skills",
    "description": "Search for skills by keyword",
    "parameters": {
      "type": "object",
      "properties": {
        "query": {"type": "string", "description": "Keywords to search"},
        "limit": {"type": "integer", "default": 5}
      },
      "required": ["query"]
    }
  }
}
```

### Tool Call Response from LLM

```json
{
  "role": "assistant",
  "content": null,
  "tool_calls": [
    {
      "id": "call_abc123",
      "type": "function",
      "function": {
        "name": "search_skills",
        "arguments": "{\"query\": \"CBPAUP0C\"}"
      }
    }
  ]
}
```

### Tool Result to LLM

```json
{
  "role": "tool",
  "tool_call_id": "call_abc123",
  "name": "search_skills",
  "content": "Found 1 skill matching 'CBPAUP0C':\n- **cbpaup0c** ..."
}
```

## Error Handling

### Tool Execution Errors

- Errors are caught and returned as ToolResult with `error=True`
- Agent can reason about errors and try alternatives
- Never crashes the agent loop

### LLM Errors

- Provider errors (rate limits, timeouts) propagate up
- Agent catches and reports to user
- Conversation state is preserved for retry

### Iteration Limits

- Max iterations per turn (default: 10)
- Prevents infinite tool-calling loops
- Returns partial response if limit reached with explanation

## Configuration

```python
class AgentConfig(BaseSettings):
    """Configuration for CodeWhisper agent."""
    skills_dir: Path              # Skills directory
    code_dir: Path                # Source code directory
    model: str                    # LLM model identifier
    provider: str                 # Provider name (from LLM_PROVIDER)
    temperature: float = 0.3     # Sampling temperature
    max_history: int = 20        # Max conversation turns
    max_tokens: int = 4096       # Max response tokens
    max_iterations: int = 10     # Max tool loops per turn
    use_minions: bool = True     # Enable minion summarization
```

### Environment Variables

| Variable | Purpose | Default |
|----------|---------|---------|
| `LLM_PROVIDER` | Provider name | `openrouter` |
| `IMPERATOR_MODEL` | Main model | `anthropic/claude-sonnet-4-20250514` |
| `MINION_SCRIBE_MODEL` | Minion model | `anthropic/claude-3-haiku-20240307` |
| `MINION_CONTEXT_THRESHOLD` | Summarize threshold (tokens) | `8000` |
| `OPENROUTER_API_KEY` | OpenRouter API key | - |
| `ANTHROPIC_API_KEY` | Anthropic API key | - |
| `OPENAI_API_KEY` | OpenAI API key | - |

## Module Structure

```
src/codewhisper/agent/
    __init__.py           # Public API exports
    message.py            # Message and conversation types
    protocol.py           # Tool protocol definitions
    react_loop.py         # Main ReAct agent loop
    minion.py             # Minion processor (uses llm_providers)
    tools/
        __init__.py       # Tool exports and registration
        registry.py       # Tool registry class
        knowledge.py      # search_skills, load_skill
        code.py           # search_code, read_file
        citadel.py        # All citadel_* tools
```

## Migration from LangChain

### Removed Dependencies

- `langgraph` - Replaced by simple ReAct loop
- `langchain-core` - Replaced by custom message types
- `langchain-anthropic` - Uses llm_providers instead
- `langchain-openai` - Uses llm_providers instead

### Kept Dependencies

- `llm-providers` - Direct LLM access via protocol
- `citadel` - Code analysis SDK
- `pydantic` / `pydantic-settings` - Data validation and config
- `typer` / `rich` - CLI interface

### Code Changes Summary

| Old (LangChain) | New (llm_providers) |
|-----------------|---------------------|
| `StateGraph` | While loop in `ReActAgent.chat()` |
| `@tool` decorator | `ToolDefinition` dataclass |
| `ToolNode` | `ToolRegistry.execute()` |
| `AIMessage/HumanMessage` | `ConversationMessage` |
| `ChatAnthropic/ChatOpenAI` | `get_provider_from_env()` |
| `add_messages` reducer | Simple `list.append()` |
| `langchain_factory.py` | Deleted (not needed) |

## Testing Strategy

### Unit Tests

- Test each tool handler independently
- Mock `LLMProvider` for agent loop tests
- Test message serialization/deserialization
- Test tool registry operations

### Integration Tests

- Test full conversation flows with mock provider
- Test tool execution with real Citadel/skills
- Test minion summarization thresholds

### Mock Provider for Testing

```python
class MockProvider:
    """Mock LLMProvider for testing."""

    def __init__(self, responses: list[CompletionResponse]):
        self._responses = responses
        self._call_index = 0

    @property
    def default_model(self) -> str:
        return "mock-model"

    async def complete(
        self,
        messages: list[Message],
        **kwargs
    ) -> CompletionResponse:
        response = self._responses[self._call_index]
        self._call_index += 1
        return response
```

## Implementation Roadmap

### Phase 1: Core Protocol
- `agent/protocol.py` - ToolDefinition, ToolCall, ToolResult
- `agent/message.py` - ConversationMessage, conversation state

### Phase 2: Tool Registry
- `agent/tools/registry.py` - ToolRegistry class
- OpenAI schema conversion
- Tool validation and execution

### Phase 3: Refactor Tools
- Remove `@tool` decorators
- Create ToolDefinition for each tool
- Keep handler logic unchanged
- Split into knowledge.py, code.py, citadel.py

### Phase 4: ReAct Loop
- `agent/react_loop.py` - ReActAgent class
- Implement tool calling loop
- Integrate with ToolRegistry
- Conversation history management

### Phase 5: Minion Update
- Remove LangChain dependency from minion.py
- Use llm_providers directly
- Pydantic structured output via prompt engineering

### Phase 6: Integration
- Update CLI to use new agent
- Remove LangChain imports from `__init__.py`
- Update `pyproject.toml` dependencies
- Update tests

## Security Considerations

- Path traversal protection in `read_file` tool
- Input validation for all tool parameters via JSON schema
- Rate limiting awareness (propagate from provider)
- No credential logging in tool results
- Skills are trusted input (from documentation system)

## Future Considerations

1. **Semantic Search**: Vector-based skill search
2. **Memory**: Persistent conversation history
3. **Multi-turn Context**: Better context window management
4. **Streaming**: Real-time response streaming
5. **Export**: Save conversations as documentation
6. **Parallel Tools**: Execute independent tool calls in parallel
