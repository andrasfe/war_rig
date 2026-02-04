"""Tool definitions for CodeWhisper agent.

This package contains all tools available to the agent, organized by category:

- knowledge: Skills search and loading
- code: Source code search and file reading
- citadel: Code analysis tools using the Citadel SDK

Example:
    from codewhisper.agent.tools import create_tool_registry

    # Create registry with all tools configured
    registry = create_tool_registry(skills_index, config)

    # Get tools for LLM
    tools_schema = registry.to_openai_schema()

    # Execute a tool call
    result = await registry.execute(tool_call)
"""

from __future__ import annotations

from typing import TYPE_CHECKING

from codewhisper.agent.tools.registry import ToolRegistry

if TYPE_CHECKING:
    from codewhisper.config import AgentConfig
    from codewhisper.skills.index import SkillsIndex

__all__ = [
    "ToolRegistry",
    "create_tool_registry",
]


def create_tool_registry(
    skills_index: "SkillsIndex",
    config: "AgentConfig",
) -> ToolRegistry:
    """Create a fully configured tool registry.

    Creates a registry with all tools registered and configured
    with the provided dependencies.

    Args:
        skills_index: Index of available skills.
        config: Agent configuration with code_dir, etc.

    Returns:
        ToolRegistry with all tools registered.
    """
    from codewhisper.agent.tools.citadel import register_citadel_tools
    from codewhisper.agent.tools.code import register_code_tools
    from codewhisper.agent.tools.knowledge import register_knowledge_tools

    registry = ToolRegistry()

    # Register knowledge tools (skills)
    register_knowledge_tools(registry, skills_index)

    # Register code tools (search, read)
    register_code_tools(registry, config)

    # Register Citadel analysis tools
    register_citadel_tools(registry, config)

    return registry
