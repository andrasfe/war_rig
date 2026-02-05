"""Core types and protocols for CodeWhisper SDK.

This module provides the foundational types for the CodeWhisper SDK,
including message types for conversation, tool abstractions, and the
ReAct loop implementation.

Types:
    - Message: A message in the conversation with role, content, and tool info
    - ToolCall: Represents a tool call request from the LLM
    - ToolResult: Result from executing a tool call
    - ToolDefinition: Definition of a tool with JSON Schema parameters
    - ReActLoop: Simple ReAct agent loop for LLM tool calling
    - ReActConfig: Configuration for the ReAct loop
    - ReActResult: Result from a ReAct loop execution

Example:
    from codewhisper.core import Message, ToolCall, ToolResult, ReActLoop

    message = Message(role="user", content="What does this code do?")
    tool_call = ToolCall(id="call_123", name="read_file", arguments={"path": "main.py"})
"""

from codewhisper.core.message import Message, ToolCall, ToolResult
from codewhisper.core.react_loop import ReActConfig, ReActLoop, ReActResult
from codewhisper.core.tool_protocol import ToolDefinition

__all__ = [
    "Message",
    "ReActConfig",
    "ReActLoop",
    "ReActResult",
    "ToolCall",
    "ToolDefinition",
    "ToolResult",
]
