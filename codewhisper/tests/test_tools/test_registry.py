"""Tests for ToolRegistry in codewhisper.tools.registry.

This module tests:
- ToolRegistry class initialization
- register() and register_all() methods
- get(), list_names(), __contains__, __len__, __iter__
- filter() creates subset registries
- to_openai_schema() format
- execute() with sync and async handlers
- execute_all() with parallel and sequential execution
- Error handling (ToolNotFoundError, ToolExecutionError)
"""

import asyncio
from typing import Any

import pytest

from codewhisper.core.message import ToolCall, ToolResult
from codewhisper.core.tool_protocol import ToolDefinition
from codewhisper.tools.registry import (
    ToolExecutionError,
    ToolNotFoundError,
    ToolRegistry,
)


class TestToolRegistryCreation:
    """Tests for ToolRegistry initialization."""

    def test_create_empty_registry(self) -> None:
        """Test creating an empty registry."""
        registry = ToolRegistry()

        assert len(registry) == 0
        assert registry.list_names() == []

    def test_registry_is_empty_initially(self) -> None:
        """Test that new registry has no tools."""
        registry = ToolRegistry()

        assert "nonexistent" not in registry
        assert registry.get("anything") is None


class TestToolRegistration:
    """Tests for registering tools."""

    def test_register_single_tool(self) -> None:
        """Test registering a single tool."""
        registry = ToolRegistry()
        tool = ToolDefinition(name="test_tool", description="A test tool")

        registry.register(tool)

        assert len(registry) == 1
        assert "test_tool" in registry
        assert registry.get("test_tool") is tool

    def test_register_multiple_tools(self) -> None:
        """Test registering multiple tools one by one."""
        registry = ToolRegistry()
        tool1 = ToolDefinition(name="tool1", description="First tool")
        tool2 = ToolDefinition(name="tool2", description="Second tool")

        registry.register(tool1)
        registry.register(tool2)

        assert len(registry) == 2
        assert "tool1" in registry
        assert "tool2" in registry

    def test_register_duplicate_name_raises(self) -> None:
        """Test that registering duplicate name raises ValueError."""
        registry = ToolRegistry()
        tool1 = ToolDefinition(name="duplicate", description="First")
        tool2 = ToolDefinition(name="duplicate", description="Second")

        registry.register(tool1)

        with pytest.raises(ValueError) as exc_info:
            registry.register(tool2)

        assert "already registered" in str(exc_info.value)

    def test_register_all(self) -> None:
        """Test registering multiple tools at once."""
        registry = ToolRegistry()
        tools = [
            ToolDefinition(name="tool_a", description="Tool A"),
            ToolDefinition(name="tool_b", description="Tool B"),
            ToolDefinition(name="tool_c", description="Tool C"),
        ]

        registry.register_all(tools)

        assert len(registry) == 3
        assert set(registry.list_names()) == {"tool_a", "tool_b", "tool_c"}

    def test_register_all_with_duplicate_raises(self) -> None:
        """Test register_all fails if duplicate encountered."""
        registry = ToolRegistry()
        registry.register(ToolDefinition(name="existing", description="Already here"))

        tools = [
            ToolDefinition(name="new_tool", description="New"),
            ToolDefinition(name="existing", description="Duplicate"),
        ]

        with pytest.raises(ValueError):
            registry.register_all(tools)

        # First tool should have been registered before failure
        assert "new_tool" in registry


class TestToolRetrieval:
    """Tests for retrieving tools from registry."""

    @pytest.fixture
    def populated_registry(self) -> ToolRegistry:
        """Create a registry with some tools."""
        registry = ToolRegistry()
        registry.register(ToolDefinition(name="read_file", description="Read a file"))
        registry.register(ToolDefinition(name="write_file", description="Write a file"))
        registry.register(ToolDefinition(name="search", description="Search code"))
        return registry

    def test_get_existing_tool(self, populated_registry: ToolRegistry) -> None:
        """Test getting an existing tool."""
        tool = populated_registry.get("read_file")

        assert tool is not None
        assert tool.name == "read_file"

    def test_get_nonexistent_returns_none(self, populated_registry: ToolRegistry) -> None:
        """Test that get() returns None for missing tool."""
        tool = populated_registry.get("nonexistent")

        assert tool is None

    def test_getitem_existing(self, populated_registry: ToolRegistry) -> None:
        """Test dict-style access for existing tool."""
        tool = populated_registry["write_file"]

        assert tool.name == "write_file"

    def test_getitem_nonexistent_raises(self, populated_registry: ToolRegistry) -> None:
        """Test that __getitem__ raises ToolNotFoundError."""
        with pytest.raises(ToolNotFoundError) as exc_info:
            _ = populated_registry["missing"]

        assert exc_info.value.tool_name == "missing"
        assert "missing" in str(exc_info.value)

    def test_contains(self, populated_registry: ToolRegistry) -> None:
        """Test __contains__ for checking tool existence."""
        assert "read_file" in populated_registry
        assert "missing" not in populated_registry

    def test_len(self, populated_registry: ToolRegistry) -> None:
        """Test __len__ returns tool count."""
        assert len(populated_registry) == 3

    def test_list_names(self, populated_registry: ToolRegistry) -> None:
        """Test list_names() returns all tool names."""
        names = populated_registry.list_names()

        assert set(names) == {"read_file", "write_file", "search"}

    def test_iter(self, populated_registry: ToolRegistry) -> None:
        """Test iterating over registry yields ToolDefinitions."""
        tools = list(populated_registry)

        assert len(tools) == 3
        assert all(isinstance(t, ToolDefinition) for t in tools)
        names = {t.name for t in tools}
        assert names == {"read_file", "write_file", "search"}


class TestToolUnregister:
    """Tests for unregistering tools."""

    def test_unregister_existing(self) -> None:
        """Test unregistering an existing tool."""
        registry = ToolRegistry()
        registry.register(ToolDefinition(name="tool", description="Test"))

        result = registry.unregister("tool")

        assert result is True
        assert "tool" not in registry

    def test_unregister_nonexistent(self) -> None:
        """Test unregistering a nonexistent tool returns False."""
        registry = ToolRegistry()

        result = registry.unregister("nonexistent")

        assert result is False


class TestToolFilter:
    """Tests for filter() method."""

    @pytest.fixture
    def full_registry(self) -> ToolRegistry:
        """Create a registry with many tools."""
        registry = ToolRegistry()
        tools = [
            ToolDefinition(name="read_file", description="Read"),
            ToolDefinition(name="write_file", description="Write"),
            ToolDefinition(name="list_files", description="List"),
            ToolDefinition(name="search_code", description="Search"),
            ToolDefinition(name="analyze", description="Analyze"),
        ]
        registry.register_all(tools)
        return registry

    def test_filter_subset(self, full_registry: ToolRegistry) -> None:
        """Test filtering to a subset of tools."""
        filtered = full_registry.filter({"read_file", "write_file"})

        assert len(filtered) == 2
        assert "read_file" in filtered
        assert "write_file" in filtered
        assert "search_code" not in filtered

    def test_filter_none_copies_all(self, full_registry: ToolRegistry) -> None:
        """Test that filter(None) copies all tools."""
        filtered = full_registry.filter(None)

        assert len(filtered) == len(full_registry)
        for name in full_registry.list_names():
            assert name in filtered

    def test_filter_creates_new_registry(self, full_registry: ToolRegistry) -> None:
        """Test that filter creates a new independent registry."""
        filtered = full_registry.filter({"read_file"})

        # Original should be unchanged
        assert len(full_registry) == 5

        # Filtered should be separate
        assert len(filtered) == 1

        # Modifying filtered should not affect original
        filtered.register(ToolDefinition(name="new_tool", description="New"))
        assert "new_tool" in filtered
        assert "new_tool" not in full_registry

    def test_filter_ignores_missing_tools(self, full_registry: ToolRegistry) -> None:
        """Test that filter silently ignores missing tools."""
        filtered = full_registry.filter({"read_file", "nonexistent", "also_missing"})

        # Should only have the one that exists
        assert len(filtered) == 1
        assert "read_file" in filtered

    def test_filter_empty_set(self, full_registry: ToolRegistry) -> None:
        """Test filtering with empty set returns empty registry."""
        filtered = full_registry.filter(set())

        assert len(filtered) == 0


class TestToOpenAISchema:
    """Tests for to_openai_schema() method."""

    def test_empty_registry(self) -> None:
        """Test schema for empty registry."""
        registry = ToolRegistry()

        schema = registry.to_openai_schema()

        assert schema == []

    def test_single_tool_schema(self) -> None:
        """Test schema with single tool."""
        registry = ToolRegistry()
        registry.register(
            ToolDefinition(
                name="test",
                description="A test tool",
                parameters={
                    "type": "object",
                    "properties": {
                        "arg1": {"type": "string"},
                    },
                },
            )
        )

        schema = registry.to_openai_schema()

        assert len(schema) == 1
        assert schema[0]["type"] == "function"
        assert schema[0]["function"]["name"] == "test"

    def test_multiple_tools_schema(self) -> None:
        """Test schema with multiple tools."""
        registry = ToolRegistry()
        registry.register(ToolDefinition(name="tool1", description="First"))
        registry.register(ToolDefinition(name="tool2", description="Second"))
        registry.register(ToolDefinition(name="tool3", description="Third"))

        schema = registry.to_openai_schema()

        assert len(schema) == 3
        names = {s["function"]["name"] for s in schema}
        assert names == {"tool1", "tool2", "tool3"}


class TestToolExecution:
    """Tests for execute() method."""

    @pytest.fixture
    def sync_handler_tool(self) -> ToolDefinition:
        """Create a tool with sync handler."""

        def handler(path: str, max_lines: int = 100) -> str:
            return f"Contents of {path} (max {max_lines} lines)"

        return ToolDefinition(
            name="read_file",
            description="Read a file",
            parameters={
                "type": "object",
                "properties": {
                    "path": {"type": "string"},
                    "max_lines": {"type": "integer"},
                },
                "required": ["path"],
            },
            handler=handler,
        )

    @pytest.fixture
    def async_handler_tool(self) -> ToolDefinition:
        """Create a tool with async handler."""

        async def handler(query: str) -> str:
            await asyncio.sleep(0.01)  # Simulate async work
            return f"Results for: {query}"

        return ToolDefinition(
            name="search",
            description="Search code",
            parameters={
                "type": "object",
                "properties": {
                    "query": {"type": "string"},
                },
                "required": ["query"],
            },
            handler=handler,
        )

    async def test_execute_sync_handler(self, sync_handler_tool: ToolDefinition) -> None:
        """Test executing a tool with sync handler."""
        registry = ToolRegistry()
        registry.register(sync_handler_tool)

        tool_call = ToolCall(
            id="call_1",
            name="read_file",
            arguments={"path": "main.py"},
        )

        result = await registry.execute(tool_call)

        assert isinstance(result, ToolResult)
        assert result.tool_call_id == "call_1"
        assert result.name == "read_file"
        assert "Contents of main.py" in result.content
        assert result.error is None

    async def test_execute_async_handler(self, async_handler_tool: ToolDefinition) -> None:
        """Test executing a tool with async handler."""
        registry = ToolRegistry()
        registry.register(async_handler_tool)

        tool_call = ToolCall(
            id="call_2",
            name="search",
            arguments={"query": "def main"},
        )

        result = await registry.execute(tool_call)

        assert result.tool_call_id == "call_2"
        assert result.name == "search"
        assert "Results for: def main" in result.content

    async def test_execute_nonexistent_tool(self) -> None:
        """Test executing a nonexistent tool returns error result."""
        registry = ToolRegistry()

        tool_call = ToolCall(
            id="call_x",
            name="nonexistent",
            arguments={},
        )

        result = await registry.execute(tool_call)

        assert result.is_error
        assert "Tool not found" in result.error  # type: ignore[operator]

    async def test_execute_tool_without_handler(self) -> None:
        """Test executing a tool with no handler returns error."""
        registry = ToolRegistry()
        registry.register(ToolDefinition(name="no_handler", description="No handler"))

        tool_call = ToolCall(id="call_3", name="no_handler", arguments={})

        result = await registry.execute(tool_call)

        assert result.is_error
        assert "no handler" in result.error  # type: ignore[operator]

    async def test_execute_with_invalid_arguments(
        self, sync_handler_tool: ToolDefinition
    ) -> None:
        """Test executing with invalid arguments returns error."""
        registry = ToolRegistry()
        registry.register(sync_handler_tool)

        # Missing required 'path' argument
        tool_call = ToolCall(
            id="call_4",
            name="read_file",
            arguments={"unknown_arg": "value"},
        )

        result = await registry.execute(tool_call)

        assert result.is_error
        assert "Invalid arguments" in result.error  # type: ignore[operator]

    async def test_execute_handler_raises_exception(self) -> None:
        """Test that handler exceptions are caught and returned as error."""

        def failing_handler() -> str:
            raise RuntimeError("Something went wrong")

        registry = ToolRegistry()
        registry.register(
            ToolDefinition(
                name="failing",
                description="A failing tool",
                handler=failing_handler,
            )
        )

        tool_call = ToolCall(id="call_5", name="failing", arguments={})

        result = await registry.execute(tool_call)

        assert result.is_error
        assert "Execution error" in result.error  # type: ignore[operator]
        assert "Something went wrong" in result.error  # type: ignore[operator]

    async def test_execute_returns_none_as_empty_string(self) -> None:
        """Test that handler returning None gives empty content."""

        def none_handler() -> None:
            return None

        registry = ToolRegistry()
        registry.register(
            ToolDefinition(name="returns_none", description="Returns None", handler=none_handler)
        )

        tool_call = ToolCall(id="call_6", name="returns_none", arguments={})

        result = await registry.execute(tool_call)

        assert not result.is_error
        assert result.content == ""

    async def test_execute_serializes_dict_result(self) -> None:
        """Test that dict results are JSON serialized."""

        def dict_handler() -> dict[str, Any]:
            return {"key": "value", "count": 42}

        registry = ToolRegistry()
        registry.register(
            ToolDefinition(name="dict_tool", description="Returns dict", handler=dict_handler)
        )

        tool_call = ToolCall(id="call_7", name="dict_tool", arguments={})

        result = await registry.execute(tool_call)

        assert not result.is_error
        assert '"key": "value"' in result.content
        assert '"count": 42' in result.content


class TestToolExecuteAll:
    """Tests for execute_all() method."""

    @pytest.fixture
    def registry_with_tools(self) -> ToolRegistry:
        """Create a registry with multiple tools for testing."""
        registry = ToolRegistry()

        def tool_a_handler(x: int) -> str:
            return f"A: {x * 2}"

        def tool_b_handler(y: str) -> str:
            return f"B: {y.upper()}"

        async def tool_c_handler(z: int) -> str:
            await asyncio.sleep(0.01)
            return f"C: {z + 1}"

        registry.register(
            ToolDefinition(
                name="tool_a",
                description="Tool A",
                parameters={
                    "type": "object",
                    "properties": {"x": {"type": "integer"}},
                    "required": ["x"],
                },
                handler=tool_a_handler,
            )
        )
        registry.register(
            ToolDefinition(
                name="tool_b",
                description="Tool B",
                parameters={
                    "type": "object",
                    "properties": {"y": {"type": "string"}},
                    "required": ["y"],
                },
                handler=tool_b_handler,
            )
        )
        registry.register(
            ToolDefinition(
                name="tool_c",
                description="Tool C",
                parameters={
                    "type": "object",
                    "properties": {"z": {"type": "integer"}},
                    "required": ["z"],
                },
                handler=tool_c_handler,
            )
        )
        return registry

    async def test_execute_all_empty(self) -> None:
        """Test execute_all with empty list."""
        registry = ToolRegistry()

        results = await registry.execute_all([])

        assert results == []

    async def test_execute_all_single(
        self, registry_with_tools: ToolRegistry
    ) -> None:
        """Test execute_all with single tool call."""
        tool_calls = [ToolCall(id="c1", name="tool_a", arguments={"x": 5})]

        results = await registry_with_tools.execute_all(tool_calls)

        assert len(results) == 1
        assert results[0].content == "A: 10"

    async def test_execute_all_parallel(
        self, registry_with_tools: ToolRegistry
    ) -> None:
        """Test execute_all runs tools in parallel by default."""
        tool_calls = [
            ToolCall(id="c1", name="tool_a", arguments={"x": 5}),
            ToolCall(id="c2", name="tool_b", arguments={"y": "hello"}),
            ToolCall(id="c3", name="tool_c", arguments={"z": 10}),
        ]

        results = await registry_with_tools.execute_all(tool_calls, parallel=True)

        assert len(results) == 3
        assert results[0].tool_call_id == "c1"
        assert results[0].content == "A: 10"
        assert results[1].tool_call_id == "c2"
        assert results[1].content == "B: HELLO"
        assert results[2].tool_call_id == "c3"
        assert results[2].content == "C: 11"

    async def test_execute_all_sequential(
        self, registry_with_tools: ToolRegistry
    ) -> None:
        """Test execute_all can run tools sequentially."""
        tool_calls = [
            ToolCall(id="c1", name="tool_a", arguments={"x": 3}),
            ToolCall(id="c2", name="tool_b", arguments={"y": "test"}),
        ]

        results = await registry_with_tools.execute_all(tool_calls, parallel=False)

        assert len(results) == 2
        assert results[0].content == "A: 6"
        assert results[1].content == "B: TEST"

    async def test_execute_all_preserves_order(
        self, registry_with_tools: ToolRegistry
    ) -> None:
        """Test that results are in same order as input calls."""
        tool_calls = [
            ToolCall(id="first", name="tool_b", arguments={"y": "first"}),
            ToolCall(id="second", name="tool_a", arguments={"x": 2}),
            ToolCall(id="third", name="tool_c", arguments={"z": 0}),
        ]

        results = await registry_with_tools.execute_all(tool_calls)

        assert [r.tool_call_id for r in results] == ["first", "second", "third"]

    async def test_execute_all_handles_errors(
        self, registry_with_tools: ToolRegistry
    ) -> None:
        """Test that errors in one tool don't affect others."""
        tool_calls = [
            ToolCall(id="c1", name="tool_a", arguments={"x": 1}),
            ToolCall(id="c2", name="nonexistent", arguments={}),
            ToolCall(id="c3", name="tool_b", arguments={"y": "ok"}),
        ]

        results = await registry_with_tools.execute_all(tool_calls)

        assert len(results) == 3
        assert not results[0].is_error
        assert results[1].is_error  # The nonexistent tool
        assert not results[2].is_error


class TestToolRegistryExceptions:
    """Tests for custom exception classes."""

    def test_tool_not_found_error(self) -> None:
        """Test ToolNotFoundError attributes."""
        error = ToolNotFoundError("missing_tool")

        assert error.tool_name == "missing_tool"
        assert "missing_tool" in str(error)
        assert isinstance(error, Exception)

    def test_tool_execution_error(self) -> None:
        """Test ToolExecutionError attributes."""
        cause = ValueError("Invalid value")
        error = ToolExecutionError("my_tool", cause)

        assert error.tool_name == "my_tool"
        assert error.cause is cause
        assert "my_tool" in str(error)
        assert "Invalid value" in str(error)
