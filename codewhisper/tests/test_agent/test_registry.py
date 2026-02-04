"""Tests for tool registry.

This module tests:
- ToolRegistry registration and lookup
- Schema conversion for LLM
- Tool execution with error handling
- Batch execution
"""

import pytest

from codewhisper.agent.protocol import ToolCall, ToolDefinition, ToolResult
from codewhisper.agent.tools.registry import ToolRegistry


class TestToolRegistry:
    """Tests for ToolRegistry class."""

    @pytest.fixture
    def sample_tool(self) -> ToolDefinition:
        """Create a sample tool definition."""
        async def handler(query: str, limit: int = 5) -> str:
            return f"Found {limit} results for: {query}"

        return ToolDefinition(
            name="search",
            description="Search for something",
            parameters={
                "type": "object",
                "properties": {
                    "query": {"type": "string"},
                    "limit": {"type": "integer", "default": 5},
                },
                "required": ["query"],
            },
            handler=handler,
        )

    def test_register_tool(self, sample_tool: ToolDefinition) -> None:
        """Test registering a tool."""
        registry = ToolRegistry()
        registry.register(sample_tool)

        assert "search" in registry
        assert len(registry) == 1

    def test_register_duplicate_raises(self, sample_tool: ToolDefinition) -> None:
        """Test registering duplicate tool raises error."""
        registry = ToolRegistry()
        registry.register(sample_tool)

        with pytest.raises(ValueError, match="already registered"):
            registry.register(sample_tool)

    def test_get_tool(self, sample_tool: ToolDefinition) -> None:
        """Test getting a tool by name."""
        registry = ToolRegistry()
        registry.register(sample_tool)

        tool = registry.get("search")

        assert tool is not None
        assert tool.name == "search"

    def test_get_nonexistent_returns_none(self) -> None:
        """Test getting nonexistent tool returns None."""
        registry = ToolRegistry()

        tool = registry.get("nonexistent")

        assert tool is None

    def test_list_tools(self, sample_tool: ToolDefinition) -> None:
        """Test listing all tools."""
        registry = ToolRegistry()
        registry.register(sample_tool)

        tools = registry.list_tools()

        assert len(tools) == 1
        assert tools[0].name == "search"

    def test_list_names(self, sample_tool: ToolDefinition) -> None:
        """Test listing tool names."""
        registry = ToolRegistry()
        registry.register(sample_tool)

        names = registry.list_names()

        assert names == ["search"]

    def test_to_openai_schema(self, sample_tool: ToolDefinition) -> None:
        """Test converting to OpenAI schema."""
        registry = ToolRegistry()
        registry.register(sample_tool)

        schema = registry.to_openai_schema()

        assert len(schema) == 1
        assert schema[0]["type"] == "function"
        assert schema[0]["function"]["name"] == "search"


class TestToolRegistryExecute:
    """Tests for tool execution."""

    @pytest.fixture
    def registry_with_tools(self) -> ToolRegistry:
        """Create a registry with test tools."""
        registry = ToolRegistry()

        async def search_handler(query: str) -> str:
            return f"Results for: {query}"

        async def error_handler(msg: str) -> str:
            raise RuntimeError(f"Tool error: {msg}")

        registry.register(
            ToolDefinition(
                name="search",
                description="Search",
                parameters={"type": "object", "properties": {"query": {"type": "string"}}},
                handler=search_handler,
            )
        )

        registry.register(
            ToolDefinition(
                name="error_tool",
                description="Always errors",
                parameters={"type": "object", "properties": {"msg": {"type": "string"}}},
                handler=error_handler,
            )
        )

        return registry

    async def test_execute_success(
        self,
        registry_with_tools: ToolRegistry,
    ) -> None:
        """Test successful tool execution."""
        call = ToolCall(
            id="call_123",
            name="search",
            arguments={"query": "test"},
        )

        result = await registry_with_tools.execute(call)

        assert result.tool_call_id == "call_123"
        assert result.name == "search"
        assert "Results for: test" in result.content
        assert result.error is False

    async def test_execute_unknown_tool(
        self,
        registry_with_tools: ToolRegistry,
    ) -> None:
        """Test executing unknown tool returns error."""
        call = ToolCall(
            id="call_123",
            name="nonexistent",
            arguments={},
        )

        result = await registry_with_tools.execute(call)

        assert result.error is True
        assert "Unknown tool" in result.content

    async def test_execute_tool_error(
        self,
        registry_with_tools: ToolRegistry,
    ) -> None:
        """Test tool execution error is caught."""
        call = ToolCall(
            id="call_123",
            name="error_tool",
            arguments={"msg": "test error"},
        )

        result = await registry_with_tools.execute(call)

        assert result.error is True
        assert "Error executing" in result.content

    async def test_execute_invalid_arguments(
        self,
        registry_with_tools: ToolRegistry,
    ) -> None:
        """Test invalid arguments are caught."""
        call = ToolCall(
            id="call_123",
            name="search",
            arguments={"wrong_arg": "value"},  # Missing required 'query'
        )

        result = await registry_with_tools.execute(call)

        assert result.error is True
        assert "Invalid arguments" in result.content or "Error executing" in result.content


class TestToolRegistryBatchExecute:
    """Tests for batch tool execution."""

    @pytest.fixture
    def registry(self) -> ToolRegistry:
        """Create a registry with a simple tool."""
        registry = ToolRegistry()

        async def handler(value: str) -> str:
            return f"processed: {value}"

        registry.register(
            ToolDefinition(
                name="process",
                description="Process a value",
                parameters={"type": "object", "properties": {"value": {"type": "string"}}},
                handler=handler,
            )
        )

        return registry

    async def test_execute_batch_parallel(self, registry: ToolRegistry) -> None:
        """Test parallel batch execution."""
        calls = [
            ToolCall(id="1", name="process", arguments={"value": "a"}),
            ToolCall(id="2", name="process", arguments={"value": "b"}),
            ToolCall(id="3", name="process", arguments={"value": "c"}),
        ]

        results = await registry.execute_batch(calls, parallel=True)

        assert len(results) == 3
        assert results[0].content == "processed: a"
        assert results[1].content == "processed: b"
        assert results[2].content == "processed: c"

    async def test_execute_batch_sequential(self, registry: ToolRegistry) -> None:
        """Test sequential batch execution."""
        calls = [
            ToolCall(id="1", name="process", arguments={"value": "a"}),
            ToolCall(id="2", name="process", arguments={"value": "b"}),
        ]

        results = await registry.execute_batch(calls, parallel=False)

        assert len(results) == 2
