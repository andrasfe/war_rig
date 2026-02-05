"""Code search and read tools for CodeWhisper.

This module provides tools for searching patterns in source code and
reading file contents. These tools enable the LLM agent to explore
and understand the codebase.

Tools:
    - search_code: Search for patterns in source files using regex
    - read_file: Read the contents of a source file

Example:
    from pathlib import Path
    from codewhisper.tools.code import create_code_tools

    code_dir = Path("./src")
    tools = create_code_tools(code_dir)
"""

from __future__ import annotations

import asyncio
import logging
from pathlib import Path

from codewhisper.core.tool_protocol import ToolDefinition

logger = logging.getLogger(__name__)


def create_code_tools(code_dir: Path) -> list[ToolDefinition]:
    """Create code tools with injected code directory.

    Args:
        code_dir: Base directory containing source code.

    Returns:
        List of ToolDefinition objects for code tools.
    """
    # Ensure code_dir is resolved
    code_dir = code_dir.resolve()

    async def search_code(
        pattern: str,
        file_pattern: str = "*",
        context_lines: int = 3,
    ) -> str:
        """Search for patterns in the source code.

        Args:
            pattern: Regex pattern to search for.
            file_pattern: Glob pattern to filter files (default: "*").
            context_lines: Number of context lines (default: 3).

        Returns:
            Formatted search results with file paths, line numbers, and context.
        """
        logger.info(f"Searching code for pattern: {pattern}")

        def _search() -> str:
            from codewhisper.search.code_search import CodeSearcher

            searcher = CodeSearcher(code_dir)
            results = searcher.search(
                pattern=pattern,
                file_pattern=file_pattern,
                context_lines=context_lines,
                max_results=50,  # Limit to prevent overwhelming context
            )

            if not results:
                return (
                    f"No matches found for pattern '{pattern}' "
                    f"in files matching '{file_pattern}'"
                )

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

        loop = asyncio.get_event_loop()
        return await loop.run_in_executor(None, _search)

    async def read_file(file_path: str, max_lines: int = 500) -> str:
        """Read the contents of a source file.

        Args:
            file_path: Path to the file (relative to code directory).
            max_lines: Maximum lines to read (default: 500).

        Returns:
            File contents with line numbers, or an error message.
        """
        logger.info(f"Reading file: {file_path}")

        def _read() -> str:
            # Resolve the full path
            full_path = (code_dir / file_path).resolve()

            # Security: Prevent path traversal attacks
            try:
                full_path.relative_to(code_dir)
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

                file_lines = content.splitlines()
                total_lines = len(file_lines)

                if total_lines > max_lines:
                    file_lines = file_lines[:max_lines]
                    truncated = True
                else:
                    truncated = False

                # Format with line numbers
                numbered_lines = [
                    f"{i + 1:4d} | {line}" for i, line in enumerate(file_lines)
                ]

                header = f"# File: {file_path}\n# Lines: {total_lines}\n\n"
                result = header + "\n".join(numbered_lines)

                if truncated:
                    result += (
                        f"\n\n... (truncated, showing first {max_lines} "
                        f"of {total_lines} lines)"
                    )

                return result

            except Exception as e:
                logger.error(f"Error reading file {file_path}: {e}")
                return f"Error reading file: {e}"

        loop = asyncio.get_event_loop()
        return await loop.run_in_executor(None, _read)

    return [
        ToolDefinition(
            name="search_code",
            description=(
                "Search for patterns in the source code. "
                "Use this tool to find specific code patterns, variable names, "
                "program calls, or other text in the codebase. "
                "Supports regular expressions."
            ),
            parameters={
                "type": "object",
                "properties": {
                    "pattern": {
                        "type": "string",
                        "description": "Regex pattern to search for in source files",
                    },
                    "file_pattern": {
                        "type": "string",
                        "description": (
                            "Glob pattern to filter files (e.g., '*.cbl', '*.jcl')"
                        ),
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
            handler=search_code,
        ),
        ToolDefinition(
            name="read_file",
            description=(
                "Read the contents of a source file. "
                "Use this tool to examine the full source code of a specific file. "
                "The content will be returned with line numbers for reference."
            ),
            parameters={
                "type": "object",
                "properties": {
                    "file_path": {
                        "type": "string",
                        "description": "Path to the file to read (relative to code directory)",
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
            handler=read_file,
        ),
    ]
