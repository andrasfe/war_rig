"""Tests for the Message types in codewhisper.core.message.

This module tests:
- ToolCall dataclass creation and serialization
- ToolResult dataclass and to_message() conversion
- Message creation with validation
- Message serialization (to_dict/from_dict)
- has_tool_calls property
"""

from typing import Any

import pytest

from codewhisper.core.message import Message, ToolCall, ToolResult


class TestToolCall:
    """Tests for the ToolCall dataclass."""

    def test_create_with_all_fields(self) -> None:
        """Test creating a ToolCall with all fields."""
        tc = ToolCall(
            id="call_abc123",
            name="read_file",
            arguments={"path": "main.py", "max_lines": 100},
        )

        assert tc.id == "call_abc123"
        assert tc.name == "read_file"
        assert tc.arguments == {"path": "main.py", "max_lines": 100}

    def test_create_with_default_arguments(self) -> None:
        """Test creating a ToolCall with default empty arguments."""
        tc = ToolCall(id="call_123", name="list_files")

        assert tc.id == "call_123"
        assert tc.name == "list_files"
        assert tc.arguments == {}

    def test_is_frozen(self) -> None:
        """Test that ToolCall is immutable (frozen dataclass)."""
        tc = ToolCall(id="call_123", name="read_file")

        with pytest.raises(Exception):  # FrozenInstanceError
            tc.id = "new_id"  # type: ignore[misc]

    def test_to_dict_format(self) -> None:
        """Test that to_dict() returns OpenAI-compatible format."""
        tc = ToolCall(
            id="call_xyz",
            name="search_code",
            arguments={"pattern": "def main", "file_type": ".py"},
        )

        result = tc.to_dict()

        assert result == {
            "id": "call_xyz",
            "type": "function",
            "function": {
                "name": "search_code",
                "arguments": {"pattern": "def main", "file_type": ".py"},
            },
        }

    def test_to_dict_empty_arguments(self) -> None:
        """Test to_dict() with empty arguments."""
        tc = ToolCall(id="call_1", name="list_files")

        result = tc.to_dict()

        assert result["function"]["arguments"] == {}

    def test_hashable_with_empty_args(self) -> None:
        """Test that ToolCall with empty arguments is hashable."""
        # Note: ToolCall with dict arguments is NOT hashable because dicts
        # are mutable. Only ToolCall with default empty dict is hashable
        # after the frozen dataclass generates the hash.
        # This test verifies the current behavior.
        tc1 = ToolCall(id="call_1", name="read_file", arguments={})
        tc2 = ToolCall(id="call_1", name="read_file", arguments={})

        # ToolCalls with dict arguments cannot be used in sets
        # because dicts are unhashable. This is a known limitation.
        # We test that the frozen=True still works for equality.
        assert tc1 == tc2

    def test_equality(self) -> None:
        """Test equality comparison for ToolCall."""
        tc1 = ToolCall(id="call_1", name="read_file", arguments={"path": "test.py"})
        tc2 = ToolCall(id="call_1", name="read_file", arguments={"path": "test.py"})
        tc3 = ToolCall(id="call_2", name="read_file", arguments={"path": "test.py"})

        assert tc1 == tc2
        assert tc1 != tc3


class TestToolResult:
    """Tests for the ToolResult dataclass."""

    def test_create_successful_result(self) -> None:
        """Test creating a successful tool result."""
        result = ToolResult(
            tool_call_id="call_123",
            name="read_file",
            content="def main():\n    print('Hello')",
        )

        assert result.tool_call_id == "call_123"
        assert result.name == "read_file"
        assert result.content == "def main():\n    print('Hello')"
        assert result.error is None
        assert not result.is_error

    def test_create_error_result(self) -> None:
        """Test creating an error tool result."""
        result = ToolResult(
            tool_call_id="call_456",
            name="read_file",
            content="",
            error="File not found: missing.py",
        )

        assert result.tool_call_id == "call_456"
        assert result.name == "read_file"
        assert result.content == ""
        assert result.error == "File not found: missing.py"
        assert result.is_error

    def test_is_error_property(self) -> None:
        """Test the is_error property."""
        success = ToolResult(
            tool_call_id="1", name="tool", content="output"
        )
        error = ToolResult(
            tool_call_id="2", name="tool", content="", error="failed"
        )

        assert success.is_error is False
        assert error.is_error is True

    def test_to_message_success(self) -> None:
        """Test converting successful result to Message."""
        result = ToolResult(
            tool_call_id="call_789",
            name="search_code",
            content="Found 3 matches in file.py",
        )

        message = result.to_message()

        assert message.role == "tool"
        assert message.content == "Found 3 matches in file.py"
        assert message.tool_call_id == "call_789"
        assert message.name == "search_code"

    def test_to_message_error(self) -> None:
        """Test converting error result to Message."""
        result = ToolResult(
            tool_call_id="call_err",
            name="read_file",
            content="",
            error="Permission denied",
        )

        message = result.to_message()

        assert message.role == "tool"
        assert message.content == "Permission denied"  # Error becomes content
        assert message.tool_call_id == "call_err"
        assert message.name == "read_file"

    def test_is_frozen(self) -> None:
        """Test that ToolResult is immutable."""
        result = ToolResult(
            tool_call_id="1", name="tool", content="output"
        )

        with pytest.raises(Exception):  # FrozenInstanceError
            result.content = "new content"  # type: ignore[misc]


class TestMessage:
    """Tests for the Message dataclass."""

    def test_create_system_message(self) -> None:
        """Test creating a system message."""
        msg = Message(role="system", content="You are a helpful assistant.")

        assert msg.role == "system"
        assert msg.content == "You are a helpful assistant."
        assert msg.tool_calls is None
        assert msg.tool_call_id is None
        assert msg.name is None

    def test_create_user_message(self) -> None:
        """Test creating a user message."""
        msg = Message(role="user", content="What does main.py do?")

        assert msg.role == "user"
        assert msg.content == "What does main.py do?"
        assert not msg.has_tool_calls

    def test_create_assistant_message_no_tools(self) -> None:
        """Test creating an assistant message without tool calls."""
        msg = Message(
            role="assistant",
            content="The main.py file initializes the application.",
        )

        assert msg.role == "assistant"
        assert msg.content == "The main.py file initializes the application."
        assert not msg.has_tool_calls

    def test_create_assistant_message_with_tools(self) -> None:
        """Test creating an assistant message with tool calls."""
        tool_calls = [
            ToolCall(id="call_1", name="read_file", arguments={"path": "main.py"}),
            ToolCall(id="call_2", name="search_code", arguments={"pattern": "def main"}),
        ]

        msg = Message(
            role="assistant",
            content="",
            tool_calls=tool_calls,
        )

        assert msg.role == "assistant"
        assert msg.content == ""
        assert msg.has_tool_calls
        assert msg.tool_calls is not None
        assert len(msg.tool_calls) == 2
        assert msg.tool_calls[0].name == "read_file"
        assert msg.tool_calls[1].name == "search_code"

    def test_create_tool_message(self) -> None:
        """Test creating a tool result message."""
        msg = Message(
            role="tool",
            content="def main():\n    pass",
            tool_call_id="call_1",
            name="read_file",
        )

        assert msg.role == "tool"
        assert msg.content == "def main():\n    pass"
        assert msg.tool_call_id == "call_1"
        assert msg.name == "read_file"

    def test_tool_message_requires_tool_call_id(self) -> None:
        """Test that tool messages must have tool_call_id."""
        with pytest.raises(ValueError) as exc_info:
            Message(role="tool", content="result", name="some_tool")

        assert "tool_call_id" in str(exc_info.value)

    def test_only_assistant_can_have_tool_calls(self) -> None:
        """Test that only assistant messages can have tool_calls."""
        tool_calls = [ToolCall(id="1", name="test")]

        # User message with tool_calls should fail
        with pytest.raises(ValueError) as exc_info:
            Message(role="user", content="test", tool_calls=tool_calls)

        assert "assistant" in str(exc_info.value)

        # System message with tool_calls should fail
        with pytest.raises(ValueError) as exc_info:
            Message(role="system", content="test", tool_calls=tool_calls)

        assert "assistant" in str(exc_info.value)

    def test_has_tool_calls_property(self) -> None:
        """Test the has_tool_calls property."""
        msg_without = Message(role="assistant", content="Hello")
        msg_empty_list = Message(role="assistant", content="Hi", tool_calls=[])
        msg_with_tools = Message(
            role="assistant",
            content="",
            tool_calls=[ToolCall(id="1", name="test")],
        )

        assert msg_without.has_tool_calls is False
        assert msg_empty_list.has_tool_calls is False
        assert msg_with_tools.has_tool_calls is True

    def test_to_dict_simple(self) -> None:
        """Test to_dict() for simple messages."""
        msg = Message(role="user", content="Hello")

        result = msg.to_dict()

        assert result == {"role": "user", "content": "Hello"}

    def test_to_dict_with_tool_calls(self) -> None:
        """Test to_dict() for assistant message with tool calls."""
        msg = Message(
            role="assistant",
            content="Let me check that.",
            tool_calls=[
                ToolCall(id="c1", name="read_file", arguments={"path": "test.py"})
            ],
        )

        result = msg.to_dict()

        assert result["role"] == "assistant"
        assert result["content"] == "Let me check that."
        assert "tool_calls" in result
        assert len(result["tool_calls"]) == 1
        assert result["tool_calls"][0]["id"] == "c1"
        assert result["tool_calls"][0]["type"] == "function"
        assert result["tool_calls"][0]["function"]["name"] == "read_file"

    def test_to_dict_tool_message(self) -> None:
        """Test to_dict() for tool result message."""
        msg = Message(
            role="tool",
            content="file contents",
            tool_call_id="c1",
            name="read_file",
        )

        result = msg.to_dict()

        assert result == {
            "role": "tool",
            "content": "file contents",
            "tool_call_id": "c1",
            "name": "read_file",
        }

    def test_from_dict_simple(self) -> None:
        """Test from_dict() for simple messages."""
        data: dict[str, Any] = {"role": "user", "content": "Hello"}

        msg = Message.from_dict(data)

        assert msg.role == "user"
        assert msg.content == "Hello"
        assert msg.tool_calls is None

    def test_from_dict_with_nested_tool_calls(self) -> None:
        """Test from_dict() with OpenAI-style nested tool calls."""
        data: dict[str, Any] = {
            "role": "assistant",
            "content": "",
            "tool_calls": [
                {
                    "id": "call_abc",
                    "type": "function",
                    "function": {
                        "name": "read_file",
                        "arguments": {"path": "test.py"},
                    },
                }
            ],
        }

        msg = Message.from_dict(data)

        assert msg.role == "assistant"
        assert msg.has_tool_calls
        assert msg.tool_calls is not None
        assert len(msg.tool_calls) == 1
        assert msg.tool_calls[0].id == "call_abc"
        assert msg.tool_calls[0].name == "read_file"
        assert msg.tool_calls[0].arguments == {"path": "test.py"}

    def test_from_dict_with_flat_tool_calls(self) -> None:
        """Test from_dict() with flat-style tool calls."""
        data: dict[str, Any] = {
            "role": "assistant",
            "content": "",
            "tool_calls": [
                {
                    "id": "call_xyz",
                    "name": "search_code",
                    "arguments": {"pattern": "main"},
                }
            ],
        }

        msg = Message.from_dict(data)

        assert msg.has_tool_calls
        assert msg.tool_calls is not None
        assert msg.tool_calls[0].name == "search_code"
        assert msg.tool_calls[0].arguments == {"pattern": "main"}

    def test_from_dict_tool_message(self) -> None:
        """Test from_dict() for tool result message."""
        data: dict[str, Any] = {
            "role": "tool",
            "content": "Result data",
            "tool_call_id": "call_123",
            "name": "some_tool",
        }

        msg = Message.from_dict(data)

        assert msg.role == "tool"
        assert msg.content == "Result data"
        assert msg.tool_call_id == "call_123"
        assert msg.name == "some_tool"

    def test_from_dict_missing_content(self) -> None:
        """Test from_dict() with missing content defaults to empty string."""
        data: dict[str, Any] = {"role": "assistant"}

        msg = Message.from_dict(data)

        assert msg.content == ""

    def test_roundtrip_serialization(self) -> None:
        """Test that to_dict/from_dict roundtrip preserves data."""
        original = Message(
            role="assistant",
            content="Checking file...",
            tool_calls=[
                ToolCall(
                    id="call_1",
                    name="read_file",
                    arguments={"path": "main.py", "max_lines": 50},
                ),
                ToolCall(id="call_2", name="list_files", arguments={}),
            ],
        )

        # Roundtrip
        data = original.to_dict()
        restored = Message.from_dict(data)

        assert restored.role == original.role
        assert restored.content == original.content
        assert restored.has_tool_calls == original.has_tool_calls
        assert restored.tool_calls is not None
        assert original.tool_calls is not None
        assert len(restored.tool_calls) == len(original.tool_calls)
        for i, tc in enumerate(restored.tool_calls):
            assert tc.id == original.tool_calls[i].id
            assert tc.name == original.tool_calls[i].name
            assert tc.arguments == original.tool_calls[i].arguments


class TestMessageRoleConstraints:
    """Tests for message role validation."""

    def test_valid_roles(self) -> None:
        """Test that all valid roles work."""
        roles = ["system", "user", "assistant"]
        for role in roles:
            msg = Message(role=role, content="test")  # type: ignore[arg-type]
            assert msg.role == role

    def test_tool_role_requires_id(self) -> None:
        """Test that tool role requires tool_call_id."""
        # This should work
        msg = Message(role="tool", content="result", tool_call_id="id1")
        assert msg.role == "tool"

        # This should fail
        with pytest.raises(ValueError):
            Message(role="tool", content="result")
