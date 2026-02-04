"""Tool protocol definitions for CodeWhisper agent.

This module defines the core tool interface without LangChain dependencies.
Tools are defined as simple dataclasses with async handler functions.

Example:
    from codewhisper.agent.protocol import ToolDefinition, ToolCall, ToolResult

    # Define a tool
    async def my_handler(query: str, limit: int = 5) -> str:
        return f"Results for {query}"

    tool = ToolDefinition(
        name="my_tool",
        description="Searches for something",
        parameters={
            "type": "object",
            "properties": {
                "query": {"type": "string", "description": "Search query"},
                "limit": {"type": "integer", "default": 5},
            },
            "required": ["query"],
        },
        handler=my_handler,
    )
"""

from __future__ import annotations

import json
import logging
from dataclasses import dataclass, field
from typing import Any, Awaitable, Callable

logger = logging.getLogger(__name__)


@dataclass
class ToolDefinition:
    """Definition of a tool available to the agent.

    Attributes:
        name: Unique identifier for the tool.
        description: Human-readable description shown to the LLM.
        parameters: JSON Schema defining the tool's parameters.
        handler: Async function that executes the tool.

    Example:
        tool = ToolDefinition(
            name="search_code",
            description="Search for patterns in source files",
            parameters={
                "type": "object",
                "properties": {
                    "pattern": {"type": "string"},
                },
                "required": ["pattern"],
            },
            handler=search_code_handler,
        )
    """

    name: str
    description: str
    parameters: dict[str, Any]
    handler: Callable[..., Awaitable[str]]

    def to_openai_schema(self) -> dict[str, Any]:
        """Convert to OpenAI function calling schema.

        Returns:
            Dict compatible with OpenAI tools parameter.
        """
        return {
            "type": "function",
            "function": {
                "name": self.name,
                "description": self.description,
                "parameters": self.parameters,
            },
        }


@dataclass
class ToolCall:
    """A tool call requested by the LLM.

    Attributes:
        id: Unique identifier for this call (from LLM).
        name: Name of the tool to execute.
        arguments: Parsed arguments dictionary.

    Example:
        tool_call = ToolCall(
            id="call_abc123",
            name="search_skills",
            arguments={"query": "authorization", "limit": 5},
        )
    """

    id: str
    name: str
    arguments: dict[str, Any]

    @classmethod
    def from_openai_format(cls, data: dict[str, Any]) -> "ToolCall":
        """Parse from OpenAI tool_calls format.

        Args:
            data: Dict with id, type, and function fields.

        Returns:
            Parsed ToolCall instance.
        """
        function_data = data.get("function", {})
        arguments_str = function_data.get("arguments", "{}")

        # Parse JSON arguments
        try:
            arguments = json.loads(arguments_str)
        except json.JSONDecodeError:
            logger.warning(f"Failed to parse tool arguments: {arguments_str}")
            arguments = {}

        return cls(
            id=data.get("id", ""),
            name=function_data.get("name", ""),
            arguments=arguments,
        )


@dataclass
class ToolResult:
    """Result of executing a tool.

    Attributes:
        tool_call_id: ID of the tool call this responds to.
        name: Name of the tool that was executed.
        content: String result from the tool.
        error: Whether the result is an error message.

    Example:
        result = ToolResult(
            tool_call_id="call_abc123",
            name="search_skills",
            content="Found 3 skills matching 'authorization'",
            error=False,
        )
    """

    tool_call_id: str
    name: str
    content: str
    error: bool = False

    def to_message_dict(self) -> dict[str, Any]:
        """Convert to message format for LLM.

        Returns:
            Dict with role="tool" and other fields.
        """
        return {
            "role": "tool",
            "tool_call_id": self.tool_call_id,
            "name": self.name,
            "content": self.content,
        }


@dataclass
class AgentState:
    """State maintained across the agent conversation.

    Attributes:
        messages: Full conversation history.
        loaded_skills: Names of skills currently in context.
        current_query: The current user query being processed.
        iteration_count: Number of tool call iterations for current query.
    """

    messages: list[dict[str, Any]] = field(default_factory=list)
    loaded_skills: list[str] = field(default_factory=list)
    current_query: str = ""
    iteration_count: int = 0

    def add_user_message(self, content: str) -> None:
        """Add a user message to the conversation.

        Args:
            content: User's message text.
        """
        self.messages.append({"role": "user", "content": content})
        self.current_query = content
        self.iteration_count = 0

    def add_assistant_message(
        self,
        content: str | None = None,
        tool_calls: list[ToolCall] | None = None,
    ) -> None:
        """Add an assistant message to the conversation.

        Args:
            content: Assistant's response text (may be None for tool calls).
            tool_calls: List of tool calls if any.
        """
        message: dict[str, Any] = {"role": "assistant"}
        if content is not None:
            message["content"] = content
        if tool_calls:
            message["tool_calls"] = [
                {
                    "id": tc.id,
                    "type": "function",
                    "function": {
                        "name": tc.name,
                        "arguments": json.dumps(tc.arguments),
                    },
                }
                for tc in tool_calls
            ]
        self.messages.append(message)

    def add_tool_result(self, result: ToolResult) -> None:
        """Add a tool result to the conversation.

        Args:
            result: Result from tool execution.
        """
        self.messages.append(result.to_message_dict())

    def trim_history(self, max_messages: int) -> None:
        """Trim conversation history to fit context window.

        Keeps system message (if first) and most recent messages.

        Args:
            max_messages: Maximum number of messages to keep.
        """
        if len(self.messages) <= max_messages:
            return

        # Keep system message if present
        if self.messages and self.messages[0].get("role") == "system":
            system_msg = self.messages[0]
            recent = self.messages[-(max_messages - 1) :]
            self.messages = [system_msg] + recent
        else:
            self.messages = self.messages[-max_messages:]
