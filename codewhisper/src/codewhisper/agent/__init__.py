"""Agent module for CodeWhisper.

This module contains the LangGraph-based conversational agent
that powers CodeWhisper's code exploration capabilities.

Components:
    - graph: LangGraph StateGraph agent with ReAct pattern
    - langchain_factory: Factory for creating LangChain models via llm_providers
    - state: Agent state schema with message reducers
    - tools: LangChain @tool decorated functions
    - citadel_tools: Citadel analysis tools
    - minion: Minion processor for large result summarization
"""

from codewhisper.agent.graph import CodeWhisperAgent, create_agent
from codewhisper.agent.langchain_factory import get_langchain_model
from codewhisper.agent.minion import MinionProcessor
from codewhisper.agent.state import AgentState, ConversationMessage
from codewhisper.agent.tools import configure_tools, get_all_tools

__all__ = [
    "AgentState",
    "CodeWhisperAgent",
    "ConversationMessage",
    "MinionProcessor",
    "configure_tools",
    "create_agent",
    "get_all_tools",
    "get_langchain_model",
]
