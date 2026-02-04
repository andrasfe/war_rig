"""Agent state models for CodeWhisper.

This module defines the state schema used by the LangGraph agent,
including conversation history, loaded skills, and search results.

The state follows LangGraph's conventions for message handling and
supports the reducer pattern for accumulating messages.

Example:
    state = AgentState(
        messages=[
            ConversationMessage(role="user", content="What does CBPAUP0C do?"),
        ],
        loaded_skills=["system-overview"],
    )
"""

from __future__ import annotations

from collections.abc import Sequence
from dataclasses import dataclass, field
from typing import Annotated, Any, Literal

from langgraph.graph.message import add_messages
from pydantic import BaseModel, Field


@dataclass
class ConversationMessage:
    """A single message in the conversation.

    Attributes:
        role: The role of the message sender (user, assistant, system, tool).
        content: The text content of the message.
        tool_calls: Optional list of tool calls made by the assistant.
        tool_call_id: ID of the tool call this message responds to.
        name: Name of the tool that produced this message.
    """

    role: Literal["user", "assistant", "system", "tool"]
    content: str
    tool_calls: list[dict[str, Any]] | None = None
    tool_call_id: str | None = None
    name: str | None = None

    def to_dict(self) -> dict[str, Any]:
        """Convert to dictionary format for LangChain compatibility.

        Returns:
            Dictionary representation of the message.
        """
        result: dict[str, Any] = {
            "role": self.role,
            "content": self.content,
        }
        if self.tool_calls:
            result["tool_calls"] = self.tool_calls
        if self.tool_call_id:
            result["tool_call_id"] = self.tool_call_id
        if self.name:
            result["name"] = self.name
        return result


class SkillContext(BaseModel):
    """Context from a loaded skill.

    Attributes:
        name: Skill name/identifier.
        description: Brief description of the skill.
        content: Full markdown content of the skill.
    """

    name: str = Field(..., description="Skill identifier")
    description: str = Field(default="", description="Skill description")
    content: str = Field(default="", description="Skill content")


class CodeSearchResult(BaseModel):
    """Result from a code search.

    Attributes:
        file_path: Path to the file containing the match.
        line_number: Line number of the match.
        line_content: Content of the matching line.
        context: Surrounding lines for context.
    """

    file_path: str = Field(..., description="Path to the matching file")
    line_number: int = Field(..., description="Line number of match")
    line_content: str = Field(..., description="Content of matching line")
    context: str = Field(default="", description="Surrounding context")


class FileContent(BaseModel):
    """Content of a read file.

    Attributes:
        file_path: Path to the file.
        content: File content (may be truncated).
        truncated: Whether the content was truncated.
        total_lines: Total number of lines in the file.
    """

    file_path: str = Field(..., description="Path to the file")
    content: str = Field(..., description="File content")
    truncated: bool = Field(default=False, description="Whether content was truncated")
    total_lines: int = Field(default=0, description="Total lines in file")


@dataclass
class AgentState:
    """State for the CodeWhisper agent.

    This state is passed through the LangGraph nodes and accumulates
    information as the agent processes user queries.

    The messages field uses the add_messages reducer, which appends
    new messages rather than replacing the list.

    Attributes:
        messages: Conversation history (uses add_messages reducer).
        loaded_skills: List of currently loaded skill names.
        skill_contexts: Full content of loaded skills.
        search_results: Results from code searches.
        file_contents: Contents of read files.
        current_query: The current user query being processed.
        needs_more_info: Flag indicating if agent needs more information.

    Example:
        # Initial state
        state = AgentState(
            messages=[],
            loaded_skills=[],
        )

        # After processing
        state = AgentState(
            messages=[
                ConversationMessage(role="user", content="..."),
                ConversationMessage(role="assistant", content="..."),
            ],
            loaded_skills=["system-overview", "cbpaup0c"],
            skill_contexts=[SkillContext(...)],
        )
    """

    # Conversation messages - uses add_messages reducer
    messages: Annotated[Sequence[Any], add_messages] = field(default_factory=list)

    # Currently loaded skills
    loaded_skills: list[str] = field(default_factory=list)

    # Full skill content for context
    skill_contexts: list[SkillContext] = field(default_factory=list)

    # Code search results
    search_results: list[CodeSearchResult] = field(default_factory=list)

    # Read file contents
    file_contents: list[FileContent] = field(default_factory=list)

    # Current query being processed
    current_query: str = ""

    # Flag for needing more information
    needs_more_info: bool = False


def create_initial_state(system_prompt: str | None = None) -> AgentState:
    """Create an initial agent state with optional system prompt.

    Args:
        system_prompt: Optional system prompt to include.

    Returns:
        Initialized AgentState.
    """
    messages: list[ConversationMessage] = []

    if system_prompt:
        messages.append(ConversationMessage(role="system", content=system_prompt))

    return AgentState(messages=messages)
