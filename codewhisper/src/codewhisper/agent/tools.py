"""Tool definitions for the CodeWhisper agent.

This module defines the tools available to the LangGraph agent for
exploring codebases and accessing skill-based knowledge.

Knowledge Tools:
    - search_skills: Find relevant skills by keyword
    - load_skill: Load a specific skill into context
    - search_code: Search for patterns in the codebase
    - read_file: Read the contents of a source file

Analysis Tools (Citadel):
    - citadel_analyze_file: Full structural analysis
    - citadel_get_functions: List functions/paragraphs
    - citadel_get_callouts: Get references/calls
    - citadel_get_includes: Get preprocessor includes
    - citadel_get_function_body: Extract function source
    - citadel_get_function_bodies: Batch extract functions
    - citadel_get_file_stats: Structural statistics
    - citadel_get_callers: Find callers of a function
    - citadel_get_sequence_diagrams: Generate sequence diagrams
    - citadel_get_dead_code: Find dead code
    - citadel_get_flow_diagram: Generate flow diagram
    - citadel_get_file_summary: Compact summary
    - citadel_get_analysis_patterns: Extract patterns

Example:
    # Tools are used by the agent automatically
    # based on user queries and the agent's reasoning
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING

from langchain_core.tools import tool
from pydantic import BaseModel, Field

if TYPE_CHECKING:
    from codewhisper.config import AgentConfig
    from codewhisper.skills.index import SkillsIndex

logger = logging.getLogger(__name__)

# Module-level references set by create_agent
_skills_index: SkillsIndex | None = None
_config: AgentConfig | None = None


def configure_tools(skills_index: SkillsIndex, config: AgentConfig) -> None:
    """Configure tools with runtime dependencies.

    This function must be called before tools can be used.

    Args:
        skills_index: The skills index to use for lookups.
        config: Agent configuration.
    """
    global _skills_index, _config
    _skills_index = skills_index
    _config = config

    # Also configure citadel tools
    from codewhisper.agent.citadel_tools import configure_citadel_tools

    configure_citadel_tools(config)


class SkillSearchInput(BaseModel):
    """Input schema for search_skills tool."""

    query: str = Field(
        ...,
        description="Keywords to search for in skill names and descriptions",
    )
    limit: int = Field(
        default=5,
        ge=1,
        le=20,
        description="Maximum number of results to return",
    )


class SkillLoadInput(BaseModel):
    """Input schema for load_skill tool."""

    skill_name: str = Field(
        ...,
        description="Name of the skill to load (e.g., 'cbpaup0c', 'system-overview')",
    )


class CodeSearchInput(BaseModel):
    """Input schema for search_code tool."""

    pattern: str = Field(
        ...,
        description="Regex pattern to search for in source files",
    )
    file_pattern: str = Field(
        default="*",
        description="Glob pattern to filter files (e.g., '*.cbl', '*.jcl')",
    )
    context_lines: int = Field(
        default=3,
        ge=0,
        le=10,
        description="Number of context lines before and after match",
    )


class FileReadInput(BaseModel):
    """Input schema for read_file tool."""

    file_path: str = Field(
        ...,
        description="Path to the file to read (relative to code directory)",
    )
    max_lines: int = Field(
        default=500,
        ge=1,
        le=2000,
        description="Maximum number of lines to read",
    )


@tool(args_schema=SkillSearchInput)
def search_skills(query: str, limit: int = 5) -> str:
    """Search for relevant skills by keyword.

    Use this tool to find skills related to specific programs, concepts,
    or functionality. Skills contain documentation about programs,
    subsystems, and architectural concepts.

    Args:
        query: Keywords to search for (e.g., "authorization", "CBPAUP0C", "MQ").
        limit: Maximum number of results (default: 5).

    Returns:
        Formatted list of matching skills with names and descriptions.
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


@tool(args_schema=SkillLoadInput)
def load_skill(skill_name: str) -> str:
    """Load a specific skill into the conversation context.

    Use this tool to get detailed information about a specific program,
    subsystem, or concept. The skill content will be available for
    answering subsequent questions.

    Args:
        skill_name: Name of the skill (e.g., "cbpaup0c", "system-overview").

    Returns:
        The full content of the skill, or an error message if not found.
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


@tool(args_schema=CodeSearchInput)
def search_code(
    pattern: str,
    file_pattern: str = "*",
    context_lines: int = 3,
) -> str:
    """Search for patterns in the source code.

    Use this tool to find specific code patterns, variable names,
    program calls, or other text in the codebase. Supports regular
    expressions.

    Args:
        pattern: Regex pattern to search for.
        file_pattern: Glob pattern to filter files (default: "*").
        context_lines: Number of context lines (default: 3).

    Returns:
        Formatted search results with file paths, line numbers, and context.
    """
    if _config is None:
        return "Error: Configuration not set"

    logger.info(f"Searching code for pattern: {pattern}")

    from codewhisper.search.code_search import CodeSearcher

    searcher = CodeSearcher(_config.code_dir)
    results = searcher.search(
        pattern=pattern,
        file_pattern=file_pattern,
        context_lines=context_lines,
        max_results=50,  # Limit to prevent overwhelming context
    )

    if not results:
        return f"No matches found for pattern '{pattern}' in files matching '{file_pattern}'"

    lines = [f"Found {len(results)} matches for '{pattern}':\n"]

    for i, result in enumerate(results[:20], 1):  # Show max 20 in output
        lines.append(f"### Match {i}: {result.file_path}:{result.line_number}")
        lines.append("```")
        lines.append(result.format(include_context=True))
        lines.append("```")
        lines.append("")

    if len(results) > 20:
        lines.append(f"\n... and {len(results) - 20} more matches (truncated)")

    return "\n".join(lines)


@tool(args_schema=FileReadInput)
def read_file(file_path: str, max_lines: int = 500) -> str:
    """Read the contents of a source file.

    Use this tool to examine the full source code of a specific file.
    The content will be returned with line numbers for reference.

    Args:
        file_path: Path to the file (relative to code directory).
        max_lines: Maximum lines to read (default: 500).

    Returns:
        File contents with line numbers, or an error message.
    """
    if _config is None:
        return "Error: Configuration not set"

    logger.info(f"Reading file: {file_path}")

    # Resolve the full path
    full_path = (_config.code_dir / file_path).resolve()

    # Security: Prevent path traversal attacks
    try:
        full_path.relative_to(_config.code_dir.resolve())
    except ValueError:
        logger.warning(f"Path traversal attempt blocked: {file_path}")
        return f"Error: Path '{file_path}' is outside the code directory"

    if not full_path.exists():
        return f"File not found: {file_path}"

    if not full_path.is_file():
        return f"Not a file: {file_path}"

    try:
        # Try multiple encodings for mainframe files
        content = None
        for encoding in ["utf-8", "latin-1", "cp1252"]:
            try:
                content = full_path.read_text(encoding=encoding)
                break
            except UnicodeDecodeError:
                continue

        if content is None:
            # Fall back to binary-safe reading
            content = full_path.read_text(errors="replace")

        lines = content.splitlines()
        total_lines = len(lines)

        if total_lines > max_lines:
            lines = lines[:max_lines]
            truncated = True
        else:
            truncated = False

        # Format with line numbers
        numbered_lines = [f"{i + 1:4d} | {line}" for i, line in enumerate(lines)]

        header = f"# File: {file_path}\n# Lines: {total_lines}\n\n"
        result = header + "\n".join(numbered_lines)

        if truncated:
            result += (
                f"\n\n... (truncated, showing first {max_lines} of {total_lines} lines)"
            )

        return result

    except Exception as e:
        logger.error(f"Error reading file {file_path}: {e}")
        return f"Error reading file: {e}"


def get_all_tools() -> list:
    """Get all available tools for the agent.

    Includes both knowledge tools (skills, code search) and
    Citadel analysis tools.

    Returns:
        List of tool functions.
    """
    from codewhisper.agent.citadel_tools import get_citadel_tools

    # Knowledge tools
    knowledge_tools = [
        search_skills,
        load_skill,
        search_code,
        read_file,
    ]

    # Analysis tools (Citadel)
    citadel_tools = get_citadel_tools()

    return knowledge_tools + citadel_tools
