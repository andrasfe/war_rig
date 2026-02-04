"""Tests for agent protocol definitions.

This module tests:
- ToolDefinition creation and schema conversion
- ToolCall creation and parsing
- ToolResult creation
- AgentState message management
"""

import pytest

from codewhisper.agent.protocol import (
    AgentState,
    ToolCall,
    ToolDefinition,
    ToolResult,
)


class TestToolDefinition:
    """Tests for ToolDefinition dataclass."""

    @pytest.fixture
    def sample_tool(self) -> ToolDefinition:
        """Create a sample tool definition."""
        async def handler(query: str, limit: int = 5) -> str:
            return f"Results for {query}"

        return ToolDefinition(
            name="search_skills",
            description="Search for skills by keyword",
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

    def test_tool_definition_fields(self, sample_tool: ToolDefinition) -> None:
        """Test tool definition has correct fields."""
        assert sample_tool.name == "search_skills"
        assert "Search" in sample_tool.description
        assert "query" in sample_tool.parameters["properties"]

    def test_to_openai_schema(self, sample_tool: ToolDefinition) -> None:
        """Test conversion to OpenAI schema."""
        schema = sample_tool.to_openai_schema()

        assert schema["type"] == "function"
        assert schema["function"]["name"] == "search_skills"
        assert "description" in schema["function"]
        assert "parameters" in schema["function"]
        assert schema["function"]["parameters"]["properties"]["query"]["type"] == "string"


class TestToolCall:
    """Tests for ToolCall dataclass."""

    def test_create_tool_call(self) -> None:
        """Test creating a tool call."""
        call = ToolCall(
            id="call_123",
            name="search_skills",
            arguments={"query": "authorization"},
        )

        assert call.id == "call_123"
        assert call.name == "search_skills"
        assert call.arguments["query"] == "authorization"

    def test_from_openai_format(self) -> None:
        """Test parsing from OpenAI format."""
        data = {
            "id": "call_abc123",
            "type": "function",
            "function": {
                "name": "search_skills",
                "arguments": '{"query": "test", "limit": 10}',
            },
        }

        call = ToolCall.from_openai_format(data)

        assert call.id == "call_abc123"
        assert call.name == "search_skills"
        assert call.arguments == {"query": "test", "limit": 10}

    def test_from_openai_format_invalid_json(self) -> None:
        """Test parsing with invalid JSON arguments."""
        data = {
            "id": "call_123",
            "type": "function",
            "function": {
                "name": "test_tool",
                "arguments": "invalid json {",
            },
        }

        call = ToolCall.from_openai_format(data)

        assert call.id == "call_123"
        assert call.name == "test_tool"
        assert call.arguments == {}  # Fallback to empty


class TestToolResult:
    """Tests for ToolResult dataclass."""

    def test_create_tool_result(self) -> None:
        """Test creating a tool result."""
        result = ToolResult(
            tool_call_id="call_123",
            name="search_skills",
            content="Found 3 skills.",
            error=False,
        )

        assert result.tool_call_id == "call_123"
        assert result.name == "search_skills"
        assert result.content == "Found 3 skills."
        assert result.error is False

    def test_create_error_result(self) -> None:
        """Test creating an error result."""
        result = ToolResult(
            tool_call_id="call_123",
            name="search_skills",
            content="Error: Tool not found",
            error=True,
        )

        assert result.error is True

    def test_to_message_dict(self) -> None:
        """Test conversion to message dict."""
        result = ToolResult(
            tool_call_id="call_123",
            name="search_skills",
            content="Results here",
        )

        msg_dict = result.to_message_dict()

        assert msg_dict["role"] == "tool"
        assert msg_dict["tool_call_id"] == "call_123"
        assert msg_dict["name"] == "search_skills"
        assert msg_dict["content"] == "Results here"


class TestAgentState:
    """Tests for AgentState dataclass."""

    def test_create_empty_state(self) -> None:
        """Test creating empty state."""
        state = AgentState()

        assert state.messages == []
        assert state.loaded_skills == []
        assert state.current_query == ""
        assert state.iteration_count == 0

    def test_add_user_message(self) -> None:
        """Test adding a user message."""
        state = AgentState()
        state.add_user_message("Hello!")

        assert len(state.messages) == 1
        assert state.messages[0]["role"] == "user"
        assert state.messages[0]["content"] == "Hello!"
        assert state.current_query == "Hello!"
        assert state.iteration_count == 0

    def test_add_assistant_message(self) -> None:
        """Test adding an assistant message."""
        state = AgentState()
        state.add_assistant_message("Hi there!")

        assert len(state.messages) == 1
        assert state.messages[0]["role"] == "assistant"
        assert state.messages[0]["content"] == "Hi there!"

    def test_add_assistant_message_with_tool_calls(self) -> None:
        """Test adding assistant message with tool calls."""
        state = AgentState()
        tool_call = ToolCall(
            id="call_123",
            name="search",
            arguments={"query": "test"},
        )
        state.add_assistant_message(tool_calls=[tool_call])

        msg = state.messages[0]
        assert msg["role"] == "assistant"
        assert len(msg["tool_calls"]) == 1
        assert msg["tool_calls"][0]["id"] == "call_123"

    def test_add_tool_result(self) -> None:
        """Test adding a tool result."""
        state = AgentState()
        result = ToolResult(
            tool_call_id="call_123",
            name="search",
            content="Results",
        )
        state.add_tool_result(result)

        assert len(state.messages) == 1
        assert state.messages[0]["role"] == "tool"
        assert state.messages[0]["content"] == "Results"

    def test_trim_history_keeps_system(self) -> None:
        """Test trimming keeps system message."""
        state = AgentState()
        state.messages = [
            {"role": "system", "content": "System prompt"},
            {"role": "user", "content": "1"},
            {"role": "assistant", "content": "2"},
            {"role": "user", "content": "3"},
            {"role": "assistant", "content": "4"},
        ]

        state.trim_history(3)

        assert len(state.messages) == 3
        assert state.messages[0]["role"] == "system"
        assert state.messages[1]["content"] == "3"

    def test_trim_history_no_system(self) -> None:
        """Test trimming without system message."""
        state = AgentState()
        state.messages = [
            {"role": "user", "content": "1"},
            {"role": "assistant", "content": "2"},
            {"role": "user", "content": "3"},
            {"role": "assistant", "content": "4"},
        ]

        state.trim_history(2)

        assert len(state.messages) == 2
        assert state.messages[0]["content"] == "3"
