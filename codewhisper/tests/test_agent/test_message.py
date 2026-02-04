"""Tests for conversation message types.

This module tests:
- ConversationMessage creation and conversion
- Conversation class for managing history
- Message serialization/deserialization
"""

import pytest

from codewhisper.agent.message import Conversation, ConversationMessage
from codewhisper.agent.protocol import ToolCall, ToolResult


class TestConversationMessage:
    """Tests for ConversationMessage dataclass."""

    def test_create_user_message(self) -> None:
        """Test creating a user message."""
        msg = ConversationMessage(role="user", content="Hello!")

        assert msg.role == "user"
        assert msg.content == "Hello!"
        assert msg.tool_calls is None
        assert msg.tool_call_id is None

    def test_create_system_message_factory(self) -> None:
        """Test system message factory method."""
        msg = ConversationMessage.system("You are helpful.")

        assert msg.role == "system"
        assert msg.content == "You are helpful."

    def test_create_user_message_factory(self) -> None:
        """Test user message factory method."""
        msg = ConversationMessage.user("What is this?")

        assert msg.role == "user"
        assert msg.content == "What is this?"

    def test_create_assistant_message_factory(self) -> None:
        """Test assistant message factory method."""
        msg = ConversationMessage.assistant("I can help with that.")

        assert msg.role == "assistant"
        assert msg.content == "I can help with that."

    def test_create_assistant_with_tool_calls(self) -> None:
        """Test assistant message with tool calls."""
        tool_call = ToolCall(
            id="call_123",
            name="search",
            arguments={"query": "test"},
        )
        msg = ConversationMessage.assistant(tool_calls=[tool_call])

        assert msg.role == "assistant"
        assert msg.content is None
        assert len(msg.tool_calls) == 1
        assert msg.tool_calls[0].name == "search"

    def test_create_tool_result_factory(self) -> None:
        """Test tool result message factory method."""
        result = ToolResult(
            tool_call_id="call_123",
            name="search",
            content="Results here",
        )
        msg = ConversationMessage.tool_result(result)

        assert msg.role == "tool"
        assert msg.content == "Results here"
        assert msg.name == "search"
        assert msg.tool_call_id == "call_123"

    def test_to_dict_basic(self) -> None:
        """Test to_dict for basic message."""
        msg = ConversationMessage.user("Hello")

        d = msg.to_dict()

        assert d["role"] == "user"
        assert d["content"] == "Hello"
        assert "tool_calls" not in d

    def test_to_dict_with_tool_calls(self) -> None:
        """Test to_dict with tool calls."""
        tool_call = ToolCall(
            id="call_123",
            name="search",
            arguments={"query": "test"},
        )
        msg = ConversationMessage.assistant(tool_calls=[tool_call])

        d = msg.to_dict()

        assert "tool_calls" in d
        assert len(d["tool_calls"]) == 1
        assert d["tool_calls"][0]["id"] == "call_123"
        assert d["tool_calls"][0]["function"]["name"] == "search"

    def test_from_dict_basic(self) -> None:
        """Test from_dict for basic message."""
        d = {"role": "user", "content": "Hello"}

        msg = ConversationMessage.from_dict(d)

        assert msg.role == "user"
        assert msg.content == "Hello"

    def test_from_dict_with_tool_calls(self) -> None:
        """Test from_dict with tool calls."""
        d = {
            "role": "assistant",
            "content": None,
            "tool_calls": [
                {
                    "id": "call_123",
                    "type": "function",
                    "function": {
                        "name": "search",
                        "arguments": '{"query": "test"}',
                    },
                }
            ],
        }

        msg = ConversationMessage.from_dict(d)

        assert msg.role == "assistant"
        assert len(msg.tool_calls) == 1
        assert msg.tool_calls[0].name == "search"


class TestConversation:
    """Tests for Conversation class."""

    def test_create_empty_conversation(self) -> None:
        """Test creating empty conversation."""
        conv = Conversation()

        assert len(conv.messages) == 0
        assert conv.max_history == 0

    def test_add_system_message(self) -> None:
        """Test adding system message."""
        conv = Conversation()
        conv.add_system("You are CodeWhisper.")

        assert len(conv.messages) == 1
        assert conv.messages[0].role == "system"
        assert conv.messages[0].content == "You are CodeWhisper."

    def test_add_system_replaces_existing(self) -> None:
        """Test adding system message replaces existing."""
        conv = Conversation()
        conv.add_system("First system")
        conv.add_system("Second system")

        assert len(conv.messages) == 1
        assert conv.messages[0].content == "Second system"

    def test_add_user_message(self) -> None:
        """Test adding user message."""
        conv = Conversation()
        conv.add_user("Hello!")

        assert len(conv.messages) == 1
        assert conv.messages[0].role == "user"

    def test_add_assistant_message(self) -> None:
        """Test adding assistant message."""
        conv = Conversation()
        conv.add_assistant("Hi there!")

        assert len(conv.messages) == 1
        assert conv.messages[0].role == "assistant"

    def test_add_tool_result(self) -> None:
        """Test adding tool result."""
        conv = Conversation()
        result = ToolResult(
            tool_call_id="call_123",
            name="search",
            content="Results",
        )
        conv.add_tool_result(result)

        assert len(conv.messages) == 1
        assert conv.messages[0].role == "tool"

    def test_to_dicts(self) -> None:
        """Test converting to dicts."""
        conv = Conversation()
        conv.add_system("System")
        conv.add_user("Hello")

        dicts = conv.to_dicts()

        assert len(dicts) == 2
        assert dicts[0]["role"] == "system"
        assert dicts[1]["role"] == "user"

    def test_clear_keeps_system(self) -> None:
        """Test clear keeps system message."""
        conv = Conversation()
        conv.add_system("System prompt")
        conv.add_user("Question 1")
        conv.add_assistant("Answer 1")

        conv.clear()

        assert len(conv.messages) == 1
        assert conv.messages[0].role == "system"

    def test_get_last_assistant_message(self) -> None:
        """Test getting last assistant message."""
        conv = Conversation()
        conv.add_user("Q1")
        conv.add_assistant("A1")
        conv.add_user("Q2")
        conv.add_assistant("A2")

        last = conv.get_last_assistant_message()

        assert last is not None
        assert last.content == "A2"

    def test_get_last_assistant_message_none(self) -> None:
        """Test getting last assistant message when none exists."""
        conv = Conversation()
        conv.add_user("Hello")

        last = conv.get_last_assistant_message()

        assert last is None

    def test_trim_with_max_history(self) -> None:
        """Test automatic trimming with max_history."""
        conv = Conversation(max_history=3)
        conv.add_system("System")
        conv.add_user("Q1")
        conv.add_assistant("A1")
        conv.add_user("Q2")  # This should trigger trimming

        # Should have system + 2 recent
        assert len(conv.messages) == 3
        assert conv.messages[0].role == "system"
