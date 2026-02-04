"""Message types for CodeWhisper agent conversations.

This module provides message types that are compatible with llm_providers.Message
but add support for tool calls and conversation management.

Example:
    from codewhisper.agent.message import ConversationMessage
    from llm_providers import Message

    # Create conversation messages
    system_msg = ConversationMessage.system("You are CodeWhisper...")
    user_msg = ConversationMessage.user("What does CBPAUP0C do?")

    # Convert to llm_providers format
    messages = [m.to_provider_message() for m in [system_msg, user_msg]]
"""

from __future__ import annotations

import json
from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Any, Literal

if TYPE_CHECKING:
    from llm_providers import Message

    from codewhisper.agent.protocol import ToolCall, ToolResult


MessageRole = Literal["system", "user", "assistant", "tool"]


@dataclass
class ConversationMessage:
    """A message in the conversation.

    Supports all message roles including tool messages with results.

    Attributes:
        role: The role of the message sender.
        content: The text content of the message.
        name: Tool name for tool messages.
        tool_calls: Tool calls for assistant messages.
        tool_call_id: ID of the tool call this message responds to.

    Example:
        # User message
        msg = ConversationMessage(role="user", content="Hello")

        # Tool call response
        msg = ConversationMessage(
            role="assistant",
            content=None,
            tool_calls=[ToolCall(id="1", name="search", arguments={})],
        )

        # Tool result
        msg = ConversationMessage(
            role="tool",
            content="Results here",
            tool_call_id="1",
            name="search",
        )
    """

    role: MessageRole
    content: str | None
    name: str | None = None
    tool_calls: list["ToolCall"] | None = None
    tool_call_id: str | None = None

    @classmethod
    def system(cls, content: str) -> "ConversationMessage":
        """Create a system message.

        Args:
            content: System prompt content.

        Returns:
            ConversationMessage with role="system".
        """
        return cls(role="system", content=content)

    @classmethod
    def user(cls, content: str) -> "ConversationMessage":
        """Create a user message.

        Args:
            content: User's message text.

        Returns:
            ConversationMessage with role="user".
        """
        return cls(role="user", content=content)

    @classmethod
    def assistant(
        cls,
        content: str | None = None,
        tool_calls: list["ToolCall"] | None = None,
    ) -> "ConversationMessage":
        """Create an assistant message.

        Args:
            content: Assistant's response text (None if tool calls only).
            tool_calls: Optional list of tool calls.

        Returns:
            ConversationMessage with role="assistant".
        """
        return cls(role="assistant", content=content, tool_calls=tool_calls)

    @classmethod
    def tool_result(cls, result: "ToolResult") -> "ConversationMessage":
        """Create a tool result message.

        Args:
            result: The tool execution result.

        Returns:
            ConversationMessage with role="tool".
        """
        return cls(
            role="tool",
            content=result.content,
            name=result.name,
            tool_call_id=result.tool_call_id,
        )

    def to_provider_message(self) -> "Message":
        """Convert to llm_providers Message format.

        Note: Tool messages are converted to user messages since
        llm_providers.Message only supports system/user/assistant roles.

        Returns:
            Message compatible with llm_providers.
        """
        from llm_providers import Message

        # llm_providers only supports system, user, assistant roles
        if self.role == "tool":
            # Convert tool result to a user message format
            # The content should be formatted to indicate it's a tool result
            return Message(role="user", content=self.content or "")
        elif self.role == "assistant" and not self.content:
            # Assistant message with only tool calls - use empty content
            return Message(role="assistant", content="")
        else:
            return Message(role=self.role, content=self.content or "")

    def to_dict(self) -> dict[str, Any]:
        """Convert to dictionary format for API calls.

        Returns:
            Dictionary representation compatible with OpenAI format.
        """
        result: dict[str, Any] = {"role": self.role}

        if self.content is not None:
            result["content"] = self.content

        if self.name is not None:
            result["name"] = self.name

        if self.tool_call_id is not None:
            result["tool_call_id"] = self.tool_call_id

        if self.tool_calls:
            result["tool_calls"] = [
                {
                    "id": tc.id,
                    "type": "function",
                    "function": {
                        "name": tc.name,
                        "arguments": json.dumps(tc.arguments),
                    },
                }
                for tc in self.tool_calls
            ]

        return result

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> "ConversationMessage":
        """Create from dictionary format.

        Args:
            data: Dictionary with role, content, and optional fields.

        Returns:
            Parsed ConversationMessage.
        """
        from codewhisper.agent.protocol import ToolCall

        tool_calls = None
        if "tool_calls" in data:
            tool_calls = [
                ToolCall.from_openai_format(tc) for tc in data["tool_calls"]
            ]

        return cls(
            role=data.get("role", "user"),
            content=data.get("content"),
            name=data.get("name"),
            tool_calls=tool_calls,
            tool_call_id=data.get("tool_call_id"),
        )


@dataclass
class Conversation:
    """Manages a conversation with message history.

    Provides methods for adding messages and converting to formats
    suitable for LLM API calls.

    Attributes:
        messages: List of conversation messages.
        max_history: Maximum number of messages to keep (0 = unlimited).

    Example:
        conv = Conversation(max_history=40)
        conv.add_system("You are CodeWhisper...")
        conv.add_user("What does CBPAUP0C do?")
        messages = conv.to_provider_messages()
    """

    messages: list[ConversationMessage] = field(default_factory=list)
    max_history: int = 0

    def add_system(self, content: str) -> None:
        """Add or replace the system message.

        The system message is always kept at the beginning.

        Args:
            content: System prompt content.
        """
        # Remove existing system message if any
        self.messages = [m for m in self.messages if m.role != "system"]
        # Add new system message at the beginning
        self.messages.insert(0, ConversationMessage.system(content))

    def add_user(self, content: str) -> None:
        """Add a user message.

        Args:
            content: User's message text.
        """
        self.messages.append(ConversationMessage.user(content))
        self._trim_if_needed()

    def add_assistant(
        self,
        content: str | None = None,
        tool_calls: list["ToolCall"] | None = None,
    ) -> None:
        """Add an assistant message.

        Args:
            content: Assistant's response text.
            tool_calls: Optional tool calls.
        """
        self.messages.append(ConversationMessage.assistant(content, tool_calls))

    def add_tool_result(self, result: "ToolResult") -> None:
        """Add a tool result message.

        Args:
            result: The tool execution result.
        """
        self.messages.append(ConversationMessage.tool_result(result))

    def to_provider_messages(self) -> list["Message"]:
        """Convert all messages to llm_providers format.

        Returns:
            List of Message objects for the provider.
        """
        return [m.to_provider_message() for m in self.messages]

    def to_dicts(self) -> list[dict[str, Any]]:
        """Convert all messages to dictionary format.

        Returns:
            List of message dictionaries.
        """
        return [m.to_dict() for m in self.messages]

    def _trim_if_needed(self) -> None:
        """Trim history if it exceeds max_history.

        Keeps the system message (if first) plus recent messages.
        """
        if self.max_history <= 0:
            return

        if len(self.messages) <= self.max_history:
            return

        # Keep system message if present
        if self.messages and self.messages[0].role == "system":
            system_msg = self.messages[0]
            recent = self.messages[-(self.max_history - 1) :]
            self.messages = [system_msg] + recent
        else:
            self.messages = self.messages[-self.max_history :]

    def clear(self) -> None:
        """Clear all messages except system prompt.
        """
        self.messages = [m for m in self.messages if m.role == "system"]

    def get_last_assistant_message(self) -> ConversationMessage | None:
        """Get the most recent assistant message.

        Returns:
            Last assistant message or None.
        """
        for msg in reversed(self.messages):
            if msg.role == "assistant":
                return msg
        return None
