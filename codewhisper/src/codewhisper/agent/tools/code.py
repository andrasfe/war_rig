"""Code tools for CodeWhisper agent.

This module provides tools for searching and reading source code files.
These tools allow the agent to explore the codebase directly.

Tools:
    - search_code: Search for patterns in source files using regex
    - read_file: Read the contents of a specific source file

Example:
    from codewhisper.agent.tools.code import register_code_tools
    from codewhisper.agent.tools.registry import ToolRegistry

    registry = ToolRegistry()
    register_code_tools(registry, config)
"""

from __future__ import annotations

import logging
from pathlib import Path
from typing import TYPE_CHECKING

from codewhisper.agent.protocol import ToolDefinition

if TYPE_CHECKING:
    from codewhisper.agent.tools.registry import ToolRegistry
    from codewhisper.config import AgentConfig

logger = logging.getLogger(__name__)

# Module-level reference to config, set by register_code_tools
_config: AgentConfig | None = None


def register_code_tools(
    registry: "ToolRegistry",
    config: "AgentConfig",
) -> None:
    """Register code tools with the registry.

    Args:
        registry: The tool registry to register with.
        config: Agent configuration with code_dir path.
    """
    global _config
    _config = config

    registry.register(
        ToolDefinition(
            name="search_code",
            description="""Search for patterns in the source code.

Use this tool to find specific code patterns, variable names,
program calls, or other text in the codebase. Supports regular
expressions.

Example patterns:
- "CALL 'PROGNAME'" - Find calls to a specific program
- "MOVE.*TO.*FIELD" - Find MOVE statements to a field
- "EXEC SQL" - Find SQL statements
""",
            parameters={
                "type": "object",
                "properties": {
                    "pattern": {
                        "type": "string",
                        "description": "Regex pattern to search for in source files",
                    },
                    "file_pattern": {
                        "type": "string",
                        "description": "Glob pattern to filter files (e.g., '*.cbl', '*.jcl')",
                        "default": "*",
                    },
                    "context_lines": {
                        "type": "integer",
                        "description": "Number of context lines before and after match",
                        "default": 3,
                        "minimum": 0,
                        "maximum": 10,
                    },
                },
                "required": ["pattern"],
            },
            handler=_search_code_handler,
        )
    )

    registry.register(
        ToolDefinition(
            name="read_file",
            description="""Read the contents of a source file.

Use this tool to examine the full source code of a specific file.
The content will be returned with line numbers for reference.

File paths should be relative to the code directory.
""",
            parameters={
                "type": "object",
                "properties": {
                    "file_path": {
                        "type": "string",
                        "description": "Path to the file (relative to code directory)",
                    },
                    "max_lines": {
                        "type": "integer",
                        "description": "Maximum number of lines to read",
                        "default": 500,
                        "minimum": 1,
                        "maximum": 2000,
                    },
                },
                "required": ["file_path"],
            },
            handler=_read_file_handler,
        )
    )


async def _search_code_handler(
    pattern: str,
    file_pattern: str = "*",
    context_lines: int = 3,
) -> str:
    """Handler for search_code tool.

    Args:
        pattern: Regex pattern to search for.
        file_pattern: Glob pattern to filter files.
        context_lines: Number of context lines.

    Returns:
        Formatted search results.
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


async def _read_file_handler(file_path: str, max_lines: int = 500) -> str:
    """Handler for read_file tool.

    Args:
        file_path: Path to the file (relative to code_dir).
        max_lines: Maximum lines to read.

    Returns:
        File contents with line numbers.
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
