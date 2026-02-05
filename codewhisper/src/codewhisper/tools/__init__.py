"""Tool registry and factories for CodeWhisper SDK.

This module provides the ToolRegistry for managing and executing tools
available to the CodeWhisper agent, along with factory functions for
creating pre-configured tool sets.

Classes:
    - ToolRegistry: Registry for tool definitions with execution support

Factory Functions:
    - create_tool_registry: Main factory combining all tool categories
    - create_knowledge_tools: Create skills/documentation tools
    - create_code_tools: Create code search and file read tools
    - create_citadel_tools: Create Citadel analysis tools

Example:
    from pathlib import Path
    from codewhisper.tools import create_tool_registry, ToolRegistry
    from codewhisper.skills.index import SkillsIndex

    # Create full registry with all tools
    registry = create_tool_registry(
        code_dir=Path("./src"),
        skills_index=skills_index,
    )

    # Get OpenAI-compatible schema for LLM
    schema = registry.to_openai_schema()

    # Execute a tool call
    result = await registry.execute(tool_call)

    # Create filtered registry with specific tools
    registry = create_tool_registry(
        code_dir=Path("./src"),
        enabled_tools={"search_code", "read_file"},
    )
"""

from codewhisper.tools.citadel import create_citadel_tools
from codewhisper.tools.code import create_code_tools
from codewhisper.tools.factory import create_tool_registry
from codewhisper.tools.knowledge import create_knowledge_tools
from codewhisper.tools.registry import ToolRegistry

__all__ = [
    "ToolRegistry",
    "create_tool_registry",
    "create_knowledge_tools",
    "create_code_tools",
    "create_citadel_tools",
]
