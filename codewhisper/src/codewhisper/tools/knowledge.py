"""Knowledge/skills tools for CodeWhisper.

This module provides tools for searching and loading skills from the
skills index. These tools enable the LLM agent to discover and access
documentation about programs, concepts, and architectural patterns.

Tools:
    - search_skills: Find relevant skills by keyword
    - load_skill: Load a specific skill into context

Example:
    from codewhisper.tools.knowledge import create_knowledge_tools
    from codewhisper.skills.index import SkillsIndex

    skills_index = SkillsIndex.from_loader(loader)
    tools = create_knowledge_tools(skills_index)
"""

from __future__ import annotations

import asyncio
import logging
from typing import TYPE_CHECKING

from codewhisper.core.tool_protocol import ToolDefinition

if TYPE_CHECKING:
    from codewhisper.skills.index import SkillsIndex

logger = logging.getLogger(__name__)


def create_knowledge_tools(skills_index: SkillsIndex) -> list[ToolDefinition]:
    """Create knowledge tools with injected skills index.

    Args:
        skills_index: The skills index to use for lookups.

    Returns:
        List of ToolDefinition objects for knowledge tools.
    """

    async def search_skills(query: str, limit: int = 5) -> str:
        """Search for relevant skills by keyword.

        Args:
            query: Keywords to search for (e.g., "authorization", "CBPAUP0C").
            limit: Maximum number of results (default: 5).

        Returns:
            Formatted list of matching skills with names and descriptions.
        """
        logger.info(f"Searching skills for: {query}")

        # Run search in thread pool since it might be CPU-bound
        def _search() -> str:
            results = skills_index.search(query, limit)

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

            lines.append(
                "\nUse load_skill(skill_name) to load the full content of a skill."
            )
            return "\n".join(lines)

        loop = asyncio.get_event_loop()
        return await loop.run_in_executor(None, _search)

    async def load_skill(skill_name: str) -> str:
        """Load a specific skill into the conversation context.

        Args:
            skill_name: Name of the skill (e.g., "cbpaup0c", "system-overview").

        Returns:
            The full content of the skill, or an error message if not found.
        """
        logger.info(f"Loading skill: {skill_name}")

        def _load() -> str:
            skill = skills_index.get(skill_name)
            if skill is None:
                # Try fuzzy match
                available = skills_index.list_names()
                suggestions = [
                    n for n in available if skill_name.lower() in n.lower()
                ][:5]
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

        loop = asyncio.get_event_loop()
        return await loop.run_in_executor(None, _load)

    return [
        ToolDefinition(
            name="search_skills",
            description=(
                "Search for relevant skills by keyword. "
                "Use this tool to find skills related to specific programs, concepts, "
                "or functionality. Skills contain documentation about programs, "
                "subsystems, and architectural concepts."
            ),
            parameters={
                "type": "object",
                "properties": {
                    "query": {
                        "type": "string",
                        "description": (
                            "Keywords to search for in skill names and descriptions "
                            "(e.g., 'authorization', 'CBPAUP0C', 'MQ')"
                        ),
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
            handler=search_skills,
        ),
        ToolDefinition(
            name="load_skill",
            description=(
                "Load a specific skill into the conversation context. "
                "Use this tool to get detailed information about a specific program, "
                "subsystem, or concept. The skill content will be available for "
                "answering subsequent questions."
            ),
            parameters={
                "type": "object",
                "properties": {
                    "skill_name": {
                        "type": "string",
                        "description": (
                            "Name of the skill to load "
                            "(e.g., 'cbpaup0c', 'system-overview')"
                        ),
                    },
                },
                "required": ["skill_name"],
            },
            handler=load_skill,
        ),
    ]
