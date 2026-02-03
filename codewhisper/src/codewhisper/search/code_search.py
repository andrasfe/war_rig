"""Code search implementation for CodeWhisper.

This module provides pattern-based search across source files,
using ripgrep when available for performance, with a Python
fallback for environments without ripgrep.

Example:
    from codewhisper.search import CodeSearcher

    searcher = CodeSearcher(Path("./src"))
    results = searcher.search("CALL 'CBLTDLI'", file_pattern="*.cbl")
    for result in results:
        print(f"{result.file_path}:{result.line_number}: {result.line_content}")
"""

from __future__ import annotations

import logging
import re
import shutil
import subprocess
from collections.abc import Iterator
from dataclasses import dataclass
from pathlib import Path

logger = logging.getLogger(__name__)


@dataclass
class CodeSearchResult:
    """Result from a code search.

    Attributes:
        file_path: Path to the file containing the match.
        line_number: Line number (1-indexed).
        line_content: Content of the matching line.
        context_before: Lines before the match.
        context_after: Lines after the match.
    """

    file_path: Path
    line_number: int
    line_content: str
    context_before: list[str]
    context_after: list[str]

    def format(self, include_context: bool = True) -> str:
        """Format the result for display.

        Args:
            include_context: Whether to include context lines.

        Returns:
            Formatted string representation.
        """
        lines = []

        if include_context and self.context_before:
            for i, line in enumerate(self.context_before):
                line_num = self.line_number - len(self.context_before) + i
                lines.append(f"{line_num:4d}   {line}")

        lines.append(f"{self.line_number:4d} > {self.line_content}")

        if include_context and self.context_after:
            for i, line in enumerate(self.context_after):
                line_num = self.line_number + 1 + i
                lines.append(f"{line_num:4d}   {line}")

        return "\n".join(lines)


class CodeSearcher:
    """Search for patterns in source code.

    Uses ripgrep if available for performance, with Python fallback.

    Attributes:
        code_dir: Base directory to search.
        use_ripgrep: Whether ripgrep is available.

    Example:
        searcher = CodeSearcher(Path("./src"))
        results = searcher.search("PERFORM VARYING")
    """

    def __init__(self, code_dir: Path):
        """Initialize the searcher.

        Args:
            code_dir: Directory to search in.
        """
        self.code_dir = Path(code_dir)
        self.use_ripgrep = shutil.which("rg") is not None

        if self.use_ripgrep:
            logger.debug("Using ripgrep for code search")
        else:
            logger.debug("Ripgrep not found, using Python fallback")

    def search(
        self,
        pattern: str,
        file_pattern: str = "*",
        context_lines: int = 3,
        max_results: int = 100,
    ) -> list[CodeSearchResult]:
        """Search for a pattern in the codebase.

        Args:
            pattern: Regex pattern to search for.
            file_pattern: Glob pattern to filter files.
            context_lines: Number of context lines.
            max_results: Maximum results to return.

        Returns:
            List of search results.

        TODO: Implement ripgrep integration
        TODO: Add file type detection and filtering
        TODO: Handle binary files gracefully
        """
        if self.use_ripgrep:
            return self._search_ripgrep(
                pattern, file_pattern, context_lines, max_results
            )
        else:
            return self._search_python(
                pattern, file_pattern, context_lines, max_results
            )

    def _search_ripgrep(
        self,
        pattern: str,
        file_pattern: str,
        context_lines: int,
        max_results: int,
    ) -> list[CodeSearchResult]:
        """Search using ripgrep with JSON output for structured results.

        Args:
            pattern: Regex pattern.
            file_pattern: File glob.
            context_lines: Context lines.
            max_results: Max results.

        Returns:
            Search results.
        """
        import json

        try:
            cmd = [
                "rg",
                "--json",
                "--line-number",
                f"--context={context_lines}",
                "--glob",
                file_pattern,
                pattern,
                str(self.code_dir),
            ]

            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=30,
            )

            if result.returncode not in (0, 1):  # 1 means no matches
                logger.warning(f"ripgrep error: {result.stderr}")
                return self._search_python(
                    pattern, file_pattern, context_lines, max_results
                )

            # Parse JSON output
            results: list[CodeSearchResult] = []
            current_match: dict | None = None
            context_before: list[str] = []
            context_after: list[str] = []

            for line in result.stdout.strip().split("\n"):
                if not line:
                    continue

                try:
                    data = json.loads(line)
                except json.JSONDecodeError:
                    continue

                msg_type = data.get("type")

                if msg_type == "begin":
                    # Reset context for new file
                    context_before = []
                    context_after = []
                    current_match = None

                elif msg_type == "context":
                    # Context line (before or after match)
                    ctx_data = data.get("data", {})
                    line_text = ctx_data.get("lines", {}).get("text", "").rstrip("\n")

                    if current_match is None:
                        # Before match
                        context_before.append(line_text)
                        # Keep only last N context lines
                        if len(context_before) > context_lines:
                            context_before.pop(0)
                    else:
                        # After match
                        context_after.append(line_text)

                elif msg_type == "match":
                    # Save previous match if exists
                    if current_match is not None and len(results) < max_results:
                        results.append(
                            CodeSearchResult(
                                file_path=Path(current_match["path"]),
                                line_number=current_match["line_number"],
                                line_content=current_match["line_content"],
                                context_before=current_match["context_before"],
                                context_after=context_after,
                            )
                        )
                        context_after = []

                    # Extract match data
                    match_data = data.get("data", {})
                    file_path = match_data.get("path", {}).get("text", "")
                    line_number = match_data.get("line_number", 0)
                    line_text = match_data.get("lines", {}).get("text", "").rstrip("\n")

                    # Make path relative
                    try:
                        rel_path = Path(file_path).relative_to(self.code_dir)
                    except ValueError:
                        rel_path = Path(file_path)

                    current_match = {
                        "path": rel_path,
                        "line_number": line_number,
                        "line_content": line_text,
                        "context_before": context_before.copy(),
                    }
                    context_before = []

                elif msg_type == "end":
                    # End of file, save last match
                    if current_match is not None and len(results) < max_results:
                        results.append(
                            CodeSearchResult(
                                file_path=Path(current_match["path"]),
                                line_number=current_match["line_number"],
                                line_content=current_match["line_content"],
                                context_before=current_match["context_before"],
                                context_after=context_after,
                            )
                        )
                    current_match = None
                    context_before = []
                    context_after = []

            return results[:max_results]

        except subprocess.TimeoutExpired:
            logger.warning("ripgrep search timed out")
            return []
        except Exception as e:
            logger.warning(f"ripgrep search failed: {e}, falling back to Python")
            return self._search_python(
                pattern, file_pattern, context_lines, max_results
            )

    def _search_python(
        self,
        pattern: str,
        file_pattern: str,
        context_lines: int,
        max_results: int,
    ) -> list[CodeSearchResult]:
        """Search using pure Python.

        Args:
            pattern: Regex pattern.
            file_pattern: File glob.
            context_lines: Context lines.
            max_results: Max results.

        Returns:
            Search results.
        """
        results: list[CodeSearchResult] = []
        regex = re.compile(pattern, re.IGNORECASE)

        # Find matching files
        files = list(self.code_dir.rglob(file_pattern))
        logger.debug(f"Searching {len(files)} files for pattern: {pattern}")

        for file_path in files:
            if len(results) >= max_results:
                break

            if not file_path.is_file():
                continue

            try:
                file_results = self._search_file(
                    file_path, regex, context_lines
                )
                results.extend(file_results)
            except Exception as e:
                logger.debug(f"Error searching {file_path}: {e}")
                continue

        return results[:max_results]

    def _search_file(
        self,
        file_path: Path,
        regex: re.Pattern,
        context_lines: int,
    ) -> Iterator[CodeSearchResult]:
        """Search a single file for matches.

        Args:
            file_path: File to search.
            regex: Compiled regex pattern.
            context_lines: Number of context lines.

        Yields:
            Search results for this file.

        TODO: Handle encoding detection
        TODO: Skip binary files
        """
        try:
            content = file_path.read_text(errors="replace")
            lines = content.splitlines()
        except Exception as e:
            logger.debug(f"Cannot read {file_path}: {e}")
            return

        for i, line in enumerate(lines):
            if regex.search(line):
                # Get context
                start = max(0, i - context_lines)
                end = min(len(lines), i + context_lines + 1)

                context_before = lines[start:i]
                context_after = lines[i + 1 : end]

                yield CodeSearchResult(
                    file_path=file_path.relative_to(self.code_dir),
                    line_number=i + 1,
                    line_content=line,
                    context_before=context_before,
                    context_after=context_after,
                )


def search_in_directory(
    directory: Path,
    pattern: str,
    file_pattern: str = "*",
    context_lines: int = 3,
    max_results: int = 100,
) -> list[CodeSearchResult]:
    """Convenience function for one-off searches.

    Args:
        directory: Directory to search.
        pattern: Regex pattern.
        file_pattern: File glob.
        context_lines: Context lines.
        max_results: Max results.

    Returns:
        Search results.
    """
    searcher = CodeSearcher(directory)
    return searcher.search(pattern, file_pattern, context_lines, max_results)
