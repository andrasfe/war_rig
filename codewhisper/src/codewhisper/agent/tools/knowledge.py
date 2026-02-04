"""Knowledge tools for CodeWhisper agent.

This module provides tools for searching and loading skills from the
skills index. These tools allow the agent to access pre-generated
documentation about the codebase.

Tools:
    - search_skills: Find skills by keyword search
    - load_skill: Load the full content of a specific skill

Example:
    from codewhisper.agent.tools.knowledge import register_knowledge_tools
    from codewhisper.agent.tools.registry import ToolRegistry

    registry = ToolRegistry()
    register_knowledge_tools(registry, skills_index)
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING

from codewhisper.agent.protocol import ToolDefinition

if TYPE_CHECKING:
    from codewhisper.agent.tools.registry import ToolRegistry
    from codewhisper.skills.index import SkillsIndex

logger = logging.getLogger(__name__)

# Module-level reference to skills index, set by register_knowledge_tools
_skills_index: SkillsIndex | None = None


def register_knowledge_tools(
    registry: "ToolRegistry",
    skills_index: "SkillsIndex",
) -> None:
    """Register knowledge tools with the registry.

    Args:
        registry: The tool registry to register with.
        skills_index: The skills index to use for lookups.
    """
    global _skills_index
    _skills_index = skills_index

    registry.register(
        ToolDefinition(
            name="search_skills",
            description="""Search for relevant skills by keyword.

Use this tool to find skills related to specific programs, concepts,
or functionality. Skills contain documentation about programs,
subsystems, and architectural concepts.

Example queries: "authorization", "CBPAUP0C", "MQ messaging", "batch processing"
""",
            parameters={
                "type": "object",
                "properties": {
                    "query": {
                        "type": "string",
                        "description": "Keywords to search for in skill names and descriptions",
                    },
                    "limit": {
                        "type": "integer",
                        "description": "Maximum number of results to return",
                        "default": 5,
                        "minimum": 1,
                        "maximum": 20,
                    },
                },
                "required": ["query"],
            },
            handler=_search_skills_handler,
        )
    )

    registry.register(
        ToolDefinition(
            name="load_skill",
            description="""Load a specific skill into the conversation context.

Use this tool to get detailed information about a specific program,
subsystem, or concept. The skill content will be available for
answering subsequent questions.

Use search_skills first to find the skill name, then load_skill to get full content.
""",
            parameters={
                "type": "object",
                "properties": {
                    "skill_name": {
                        "type": "string",
                        "description": "Name of the skill to load (e.g., 'cbpaup0c', 'system-overview')",
                    },
                },
                "required": ["skill_name"],
            },
            handler=_load_skill_handler,
        )
    )


async def _search_skills_handler(query: str, limit: int = 5) -> str:
    """Handler for search_skills tool.

    Args:
        query: Keywords to search for.
        limit: Maximum number of results.

    Returns:
        Formatted string with search results.
    """
    if _skills_index is None:
        return "Error: Skills index not configured"

    logger.info(f"Searching skills for: {query}")

    results = _skills_index.search(query, limit)

    if not results:
        return f"No skills found matching '{query}'"

    lines = [f"Found {len(results)} skills matching '{query}':\n"]
    for result in results:
        skill = result.skill
        # Truncate description for display
        desc = (
            skill.description[:150] + "..."
            if len(skill.description) > 150
            else skill.description
        )
        lines.append(f"- **{skill.name}** (score: {result.score:.1f})")
        lines.append(f"  {desc}")
        lines.append("")

    lines.append("\nUse load_skill(skill_name) to load the full content of a skill.")
    return "\n".join(lines)


async def _load_skill_handler(skill_name: str) -> str:
    """Handler for load_skill tool.

    Args:
        skill_name: Name of the skill to load.

    Returns:
        Full skill content or error message.
    """
    if _skills_index is None:
        return "Error: Skills index not configured"

    logger.info(f"Loading skill: {skill_name}")

    skill = _skills_index.get(skill_name)
    if skill is None:
        # Try fuzzy match
        available = _skills_index.list_names()
        suggestions = [n for n in available if skill_name.lower() in n.lower()][:5]
        msg = f"Skill '{skill_name}' not found."
        if suggestions:
            msg += f"\n\nDid you mean: {', '.join(suggestions)}?"
        return msg

    # Return full skill content with metadata header
    return f"""# Skill: {skill.name}

**Description:** {skill.description}

**Source:** {skill.file_path}

---

{skill.content}"""
