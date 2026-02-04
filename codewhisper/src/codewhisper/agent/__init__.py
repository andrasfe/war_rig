"""Agent module for CodeWhisper.

This module contains the ReAct-based conversational agent
that powers CodeWhisper's code exploration capabilities.

Components:
    - react_loop: The main ReAct agent implementation
    - protocol: Tool protocol definitions
    - message: Conversation message types
    - minion: Minion processor for large result summarization
    - tools: Tool definitions and registry
"""

from codewhisper.agent.message import Conversation, ConversationMessage
from codewhisper.agent.minion import MinionProcessor
from codewhisper.agent.protocol import (
    AgentState,
    ToolCall,
    ToolDefinition,
    ToolResult,
)
from codewhisper.agent.react_loop import ReActAgent, create_agent
from codewhisper.agent.tools import ToolRegistry, create_tool_registry

__all__ = [
    "AgentState",
    "Conversation",
    "ConversationMessage",
    "MinionProcessor",
    "ReActAgent",
    "ToolCall",
    "ToolDefinition",
    "ToolRegistry",
    "ToolResult",
    "create_agent",
    "create_tool_registry",
]
