"""Message types for CodeWhisper conversation.

This module provides simple dataclasses for representing messages in a
conversation, replacing LangChain's AIMessage/HumanMessage with a more
lightweight, provider-agnostic implementation.

Types:
    - Message: A single message in the conversation
    - ToolCall: A tool call request from the LLM
    - ToolResult: The result of executing a tool

Example:
    # Simple user message
    user_msg = Message(role="user", content="What does this code do?")

    # Assistant message with tool calls
    assistant_msg = Message(
        role="assistant",
        content="",
        tool_calls=[
            ToolCall(id="call_1", name="read_file", arguments={"path": "main.py"})
        ],
    )

    # Tool result message
    tool_msg = Message(
        role="tool",
        content="file contents...",
        tool_call_id="call_1",
        name="read_file",
    )
"""

from dataclasses import dataclass, field
from typing import Any, Literal


@dataclass(frozen=True)
class ToolCall:
    """Represents a tool call request from the LLM.

    This is a frozen dataclass to ensure immutability and hashability,
    which is useful for caching and comparison operations.

    Attributes:
        id: Unique identifier for this tool call (from the LLM provider).
        name: The name of the tool to call.
        arguments: Dictionary of arguments to pass to the tool.

    Example:
        tool_call = ToolCall(
            id="call_abc123",
            name="read_file",
            arguments={"path": "src/main.py", "max_lines": 100},
        )
    """

    id: str
    name: str
    arguments: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """Convert to dictionary format for API compatibility.

        Returns:
            Dictionary with id, type, and function fields matching
            OpenAI's tool call format.
        """
        return {
            "id": self.id,
            "type": "function",
            "function": {
                "name": self.name,
                "arguments": self.arguments,
            },
        }


@dataclass(frozen=True)
class ToolResult:
    """Result from executing a tool call.

    This is a frozen dataclass to ensure immutability. Tool results
    are typically converted to Message objects for inclusion in the
    conversation history.

    Attributes:
        tool_call_id: The ID of the tool call this result responds to.
        name: The name of the tool that was called.
        content: The string content returned by the tool.
        error: Optional error message if the tool execution failed.

    Example:
        # Successful result
        result = ToolResult(
            tool_call_id="call_abc123",
            name="read_file",
            content="def main():\\n    print('Hello')",
        )

        # Error result
        error_result = ToolResult(
            tool_call_id="call_abc123",
            name="read_file",
            content="",
            error="File not found: src/main.py",
        )
    """

    tool_call_id: str
    name: str
    content: str
    error: str | None = None

    @property
    def is_error(self) -> bool:
        """Check if this result represents an error.

        Returns:
            True if the error field is set, False otherwise.
        """
        return self.error is not None

    def to_message(self) -> "Message":
        """Convert this tool result to a Message for the conversation.

        Returns:
            A Message with role="tool" containing the result content
            or error message.
        """
        content: str = self.error if self.error is not None else self.content
        return Message(
            role="tool",
            content=content,
            tool_call_id=self.tool_call_id,
            name=self.name,
        )


@dataclass
class Message:
    """A single message in the conversation.

    This class represents any type of message in the conversation:
    system prompts, user inputs, assistant responses, and tool results.

    Unlike LangChain's typed messages (HumanMessage, AIMessage, etc.),
    this uses a single class with a role field, which is more aligned
    with the OpenAI API format and easier to serialize.

    Attributes:
        role: The role of the message sender. Must be one of:
            - "system": System instructions for the model
            - "user": User input
            - "assistant": Model response
            - "tool": Tool execution result
        content: The text content of the message.
        tool_calls: Optional list of tool calls (for assistant messages).
        tool_call_id: ID of the tool call this message responds to (for tool messages).
        name: Name of the tool (for tool messages).

    Example:
        # System message
        system = Message(role="system", content="You are a helpful assistant.")

        # User message
        user = Message(role="user", content="Read main.py")

        # Assistant with tool call
        assistant = Message(
            role="assistant",
            content="",
            tool_calls=[ToolCall(id="1", name="read_file", arguments={"path": "main.py"})],
        )

        # Tool result
        tool = Message(
            role="tool",
            content="file contents...",
            tool_call_id="1",
            name="read_file",
        )
    """

    role: Literal["system", "user", "assistant", "tool"]
    content: str
    tool_calls: list[ToolCall] | None = None
    tool_call_id: str | None = None
    name: str | None = None

    def __post_init__(self) -> None:
        """Validate message structure after initialization."""
        # Tool messages must have tool_call_id
        if self.role == "tool" and self.tool_call_id is None:
            raise ValueError("Tool messages must have a tool_call_id")

        # Only assistant messages can have tool_calls
        if self.tool_calls is not None and self.role != "assistant":
            raise ValueError("Only assistant messages can have tool_calls")

    def to_dict(self) -> dict[str, Any]:
        """Convert to dictionary format for API compatibility.

        Returns:
            Dictionary representation suitable for OpenAI-compatible APIs.
        """
        result: dict[str, Any] = {
            "role": self.role,
            "content": self.content,
        }

        if self.tool_calls:
            result["tool_calls"] = [tc.to_dict() for tc in self.tool_calls]

        if self.tool_call_id is not None:
            result["tool_call_id"] = self.tool_call_id

        if self.name is not None:
            result["name"] = self.name

        return result

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> "Message":
        """Create a Message from a dictionary.

        Args:
            data: Dictionary with role, content, and optional tool fields.

        Returns:
            A new Message instance.

        Raises:
            KeyError: If required fields are missing.
            ValueError: If the role is invalid.
        """
        tool_calls = None
        if "tool_calls" in data and data["tool_calls"]:
            tool_calls = []
            for tc in data["tool_calls"]:
                # Handle both nested and flat formats
                if "function" in tc:
                    tool_calls.append(
                        ToolCall(
                            id=tc["id"],
                            name=tc["function"]["name"],
                            arguments=tc["function"].get("arguments", {}),
                        )
                    )
                else:
                    tool_calls.append(
                        ToolCall(
                            id=tc["id"],
                            name=tc["name"],
                            arguments=tc.get("arguments", {}),
                        )
                    )

        return cls(
            role=data["role"],
            content=data.get("content", ""),
            tool_calls=tool_calls,
            tool_call_id=data.get("tool_call_id"),
            name=data.get("name"),
        )

    @property
    def has_tool_calls(self) -> bool:
        """Check if this message contains tool calls.

        Returns:
            True if tool_calls is non-empty, False otherwise.
        """
        return bool(self.tool_calls)
