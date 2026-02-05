"""Tool factory for CodeWhisper.

This module provides the main factory function for creating a fully
configured ToolRegistry with all available tools.

Example:
    from pathlib import Path
    from codewhisper.tools.factory import create_tool_registry
    from codewhisper.skills.index import SkillsIndex

    registry = create_tool_registry(
        code_dir=Path("./src"),
        skills_index=skills_index,
    )

    # Get OpenAI-compatible schema
    schema = registry.to_openai_schema()

    # Execute tool calls
    result = await registry.execute(tool_call)
"""

from __future__ import annotations

import logging
from pathlib import Path
from typing import TYPE_CHECKING

from codewhisper.tools.registry import ToolRegistry

if TYPE_CHECKING:
    from codewhisper.skills.index import SkillsIndex

logger = logging.getLogger(__name__)


def create_tool_registry(
    code_dir: Path,
    skills_index: SkillsIndex | None = None,
    enabled_tools: set[str] | None = None,
) -> ToolRegistry:
    """Factory to create fully configured tool registry.

    Creates a ToolRegistry populated with all available tools, optionally
    filtered to a subset. Tools are organized into three categories:

    1. Knowledge tools (search_skills, load_skill) - require skills_index
    2. Code tools (search_code, read_file) - require code_dir
    3. Citadel tools (13 analysis tools) - require code_dir

    Args:
        code_dir: Base directory containing source code. Required for
            code search, file reading, and Citadel analysis tools.
        skills_index: Optional skills index for knowledge tools. If not
            provided, knowledge tools are not registered.
        enabled_tools: Optional set of tool names to include. If provided,
            only tools with names in this set will be registered. If None,
            all tools are registered.

    Returns:
        Configured ToolRegistry ready for use with the agent.

    Example:
        # Full registry with all tools
        registry = create_tool_registry(
            code_dir=Path("./src"),
            skills_index=skills_index,
        )

        # Only code analysis tools
        registry = create_tool_registry(
            code_dir=Path("./src"),
            enabled_tools={"search_code", "read_file", "citadel_analyze_file"},
        )

        # Without skills (no knowledge tools)
        registry = create_tool_registry(code_dir=Path("./src"))
    """
    from codewhisper.tools.citadel import create_citadel_tools
    from codewhisper.tools.code import create_code_tools
    from codewhisper.tools.knowledge import create_knowledge_tools

    registry = ToolRegistry()

    # Register knowledge tools if skills index is provided
    if skills_index is not None:
        knowledge_tools = create_knowledge_tools(skills_index)
        logger.debug(f"Registering {len(knowledge_tools)} knowledge tools")
        registry.register_all(knowledge_tools)

    # Register code tools
    code_tools = create_code_tools(code_dir)
    logger.debug(f"Registering {len(code_tools)} code tools")
    registry.register_all(code_tools)

    # Register citadel tools
    citadel_tools = create_citadel_tools(code_dir)
    logger.debug(f"Registering {len(citadel_tools)} citadel tools")
    registry.register_all(citadel_tools)

    logger.info(f"Created tool registry with {len(registry)} tools")

    # Filter to enabled tools if specified
    if enabled_tools is not None:
        registry = registry.filter(enabled_tools)
        logger.info(f"Filtered to {len(registry)} enabled tools")

    return registry
