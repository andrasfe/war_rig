"""Citadel code analysis tools for CodeWhisper.

This module wraps Citadel SDK functions as ToolDefinition objects,
enabling the agent to perform deep code analysis on mainframe codebases.

Tools:
    - citadel_analyze_file: Full structural analysis of a source file
    - citadel_get_functions: Extract all functions with their callouts
    - citadel_get_callouts: Get all references from a file/directory
    - citadel_get_includes: Get preprocessor includes (COPY, etc.)
    - citadel_get_function_body: Extract body of a specific function
    - citadel_get_function_bodies: Batch extract multiple function bodies
    - citadel_get_file_stats: Get structural statistics
    - citadel_get_callers: Find all callers of a function
    - citadel_get_sequence_diagrams: Generate call chain diagrams
    - citadel_get_dead_code: Find unreferenced artifacts
    - citadel_get_flow_diagram: Generate control flow diagram
    - citadel_get_file_summary: Compact file summary
    - citadel_get_analysis_patterns: Extract code patterns

Example:
    from pathlib import Path
    from codewhisper.tools.citadel import create_citadel_tools

    code_dir = Path("./src")
    tools = create_citadel_tools(code_dir)
"""

from __future__ import annotations

import asyncio
import logging
from pathlib import Path
from typing import Any

from codewhisper.core.tool_protocol import ToolDefinition

logger = logging.getLogger(__name__)

# Module-level Citadel instance (lazy initialized)
_citadel: Any = None


def _get_citadel() -> Any:
    """Get or create the Citadel SDK instance.

    Returns:
        Citadel SDK instance.
    """
    global _citadel
    if _citadel is None:
        from citadel import Citadel

        _citadel = Citadel()
    return _citadel


def _resolve_path(file_path: str, code_dir: Path) -> Path:
    """Resolve file path relative to code directory.

    Args:
        file_path: Path to file (relative to code_dir or absolute).
        code_dir: Base code directory.

    Returns:
        Resolved absolute path.
    """
    path = Path(file_path)
    if not path.is_absolute():
        path = code_dir / path
    return path.resolve()


def create_citadel_tools(code_dir: Path) -> list[ToolDefinition]:
    """Create all citadel analysis tools.

    Args:
        code_dir: Base directory containing source code.

    Returns:
        List of ToolDefinition objects for citadel tools.
    """
    # Ensure code_dir is resolved
    code_dir = code_dir.resolve()

    # -------------------------------------------------------------------------
    # Tool Handler Functions
    # -------------------------------------------------------------------------

    async def citadel_analyze_file(file_path: str) -> str:
        """Perform full structural analysis of a source file."""

        def _analyze() -> str:
            try:
                citadel = _get_citadel()
                full_path = _resolve_path(file_path, code_dir)

                result = citadel.analyze_file(full_path)

                if result.error:
                    return f"Error analyzing {file_path}: {result.error}"

                lines = [
                    f"# Analysis of {Path(result.file_path).name}",
                    f"**Language:** {result.language}",
                    "",
                    "## Artifacts",
                ]

                for artifact in result.artifacts:
                    lines.append(f"### {artifact.name} ({artifact.type})")
                    if artifact.line_start:
                        line_range = f"Lines {artifact.line_start}"
                        if artifact.line_end:
                            line_range += f"-{artifact.line_end}"
                        lines.append(line_range)
                    if artifact.callouts:
                        lines.append("**Calls:**")
                        for c in artifact.callouts[:10]:
                            lines.append(f"  - {c.target} ({c.relationship})")
                        if len(artifact.callouts) > 10:
                            lines.append(
                                f"  ... and {len(artifact.callouts) - 10} more"
                            )
                    lines.append("")

                if result.file_level_callouts:
                    lines.append("## File-Level References")
                    for c in result.file_level_callouts:
                        lines.append(f"- {c.target} ({c.relationship})")

                if result.preprocessor_includes:
                    lines.append("")
                    lines.append("## Preprocessor Includes")
                    for inc in result.preprocessor_includes:
                        lines.append(f"- {inc}")

                return "\n".join(lines)

            except Exception as e:
                logger.exception(f"Error in citadel_analyze_file: {e}")
                return f"Error analyzing file: {e}"

        loop = asyncio.get_event_loop()
        return await loop.run_in_executor(None, _analyze)

    async def citadel_get_functions(file_path: str) -> str:
        """Get all functions/paragraphs in a file with their callouts."""

        def _get_functions() -> str:
            try:
                citadel = _get_citadel()
                full_path = _resolve_path(file_path, code_dir)

                functions = citadel.get_functions(full_path)

                if not functions:
                    return f"No functions found in {file_path}"

                if functions and "error" in functions[0]:
                    return f"Error: {functions[0]['error']}"

                lines = [f"# Functions in {Path(file_path).name}", ""]
                for func in functions:
                    lines.append(f"## {func['name']} ({func['type']})")
                    line_info = f"Lines: {func.get('line', '?')}"
                    if func.get("line_end"):
                        line_info += f"-{func['line_end']}"
                    lines.append(line_info)
                    if func.get("calls"):
                        lines.append("Calls:")
                        for call in func["calls"][:5]:
                            lines.append(f"  - {call['target']} ({call['type']})")
                        if len(func["calls"]) > 5:
                            lines.append(f"  ... and {len(func['calls']) - 5} more")
                    lines.append("")

                return "\n".join(lines)

            except Exception as e:
                logger.exception(f"Error in citadel_get_functions: {e}")
                return f"Error getting functions: {e}"

        loop = asyncio.get_event_loop()
        return await loop.run_in_executor(None, _get_functions)

    async def citadel_get_callouts(path: str) -> str:
        """Get all callouts/references from a file or directory."""

        def _get_callouts() -> str:
            try:
                citadel = _get_citadel()
                full_path = _resolve_path(path, code_dir)

                callouts = citadel.get_callouts(full_path)

                if not callouts:
                    return f"No callouts found in {path}"

                if callouts and "error" in callouts[0]:
                    return f"Error: {callouts[0]['error']}"

                lines = [f"# Callouts from {Path(path).name}", ""]

                # Group by type for readability
                by_type: dict[str, list[dict[str, Any]]] = {}
                for c in callouts:
                    call_type = c.get("type", "unknown")
                    if call_type not in by_type:
                        by_type[call_type] = []
                    by_type[call_type].append(c)

                for call_type, type_callouts in sorted(by_type.items()):
                    lines.append(f"## {call_type.title()} ({len(type_callouts)})")
                    for c in type_callouts[:15]:
                        resolved = ""
                        if "resolved" in c:
                            resolved = " [resolved]" if c["resolved"] else " [unresolved]"
                        lines.append(f"- {c['from']} -> {c['to']}{resolved}")
                    if len(type_callouts) > 15:
                        lines.append(f"... and {len(type_callouts) - 15} more")
                    lines.append("")

                return "\n".join(lines)

            except Exception as e:
                logger.exception(f"Error in citadel_get_callouts: {e}")
                return f"Error getting callouts: {e}"

        loop = asyncio.get_event_loop()
        return await loop.run_in_executor(None, _get_callouts)

    async def citadel_get_includes(file_path: str) -> str:
        """Get all preprocessor includes (COPY statements, etc.)."""

        def _get_includes() -> str:
            try:
                citadel = _get_citadel()
                full_path = _resolve_path(file_path, code_dir)

                includes = citadel.get_includes(full_path)

                if not includes:
                    return f"No preprocessor includes found in {file_path}"

                lines = [f"# Preprocessor Includes in {Path(file_path).name}", ""]
                for inc in includes:
                    lines.append(f"- {inc}")

                return "\n".join(lines)

            except Exception as e:
                logger.exception(f"Error in citadel_get_includes: {e}")
                return f"Error getting includes: {e}"

        loop = asyncio.get_event_loop()
        return await loop.run_in_executor(None, _get_includes)

    async def citadel_get_function_body(file_path: str, function_name: str) -> str:
        """Extract the source code body of a specific function."""

        def _get_function_body() -> str:
            try:
                citadel = _get_citadel()
                full_path = _resolve_path(file_path, code_dir)

                body = citadel.get_function_body(full_path, function_name)

                if body is None:
                    return f"Function '{function_name}' not found in {file_path}"

                return (
                    f"# {function_name} in {Path(file_path).name}\n\n```\n{body}\n```"
                )

            except Exception as e:
                logger.exception(f"Error in citadel_get_function_body: {e}")
                return f"Error extracting function body: {e}"

        loop = asyncio.get_event_loop()
        return await loop.run_in_executor(None, _get_function_body)

    async def citadel_get_function_bodies(
        file_path: str, function_names: list[str]
    ) -> str:
        """Batch extract multiple function bodies efficiently."""

        def _get_function_bodies() -> str:
            try:
                citadel = _get_citadel()
                full_path = _resolve_path(file_path, code_dir)

                bodies = citadel.get_function_bodies(full_path, function_names)

                lines = [f"# Function Bodies from {Path(file_path).name}", ""]

                found_count = 0
                for name, body in bodies.items():
                    if body is not None:
                        found_count += 1
                        lines.append(f"## {name}")
                        lines.append(f"```\n{body}\n```")
                        lines.append("")
                    else:
                        lines.append(f"## {name}")
                        lines.append("*Not found*")
                        lines.append("")

                lines.insert(
                    1, f"Found {found_count} of {len(function_names)} functions"
                )

                return "\n".join(lines)

            except Exception as e:
                logger.exception(f"Error in citadel_get_function_bodies: {e}")
                return f"Error extracting function bodies: {e}"

        loop = asyncio.get_event_loop()
        return await loop.run_in_executor(None, _get_function_bodies)

    async def citadel_get_file_stats(file_path: str) -> str:
        """Get structural statistics for planning documentation strategy."""

        def _get_file_stats() -> str:
            try:
                citadel = _get_citadel()
                full_path = _resolve_path(file_path, code_dir)

                stats = citadel.get_file_stats(full_path)

                if stats.get("error"):
                    return f"Error: {stats['error']}"

                lines = [
                    f"# File Statistics: {Path(file_path).name}",
                    "",
                    f"**Language:** {stats['language']}",
                    f"**Total Lines:** {stats['total_lines']}",
                    f"**Paragraph Count:** {stats['paragraph_count']}",
                    "",
                ]

                if stats.get("paragraphs"):
                    lines.append("## Paragraphs")
                    for para in stats["paragraphs"]:
                        line_range = (
                            f"{para.get('line_start', '?')}-{para.get('line_end', '?')}"
                        )
                        line_count = para.get("line_count", 0)
                        lines.append(
                            f"- **{para['name']}**: "
                            f"lines {line_range} ({line_count} lines)"
                        )

                return "\n".join(lines)

            except Exception as e:
                logger.exception(f"Error in citadel_get_file_stats: {e}")
                return f"Error getting file stats: {e}"

        loop = asyncio.get_event_loop()
        return await loop.run_in_executor(None, _get_file_stats)

    async def citadel_get_callers(file_path: str, function_name: str) -> str:
        """Find all callers of a specific function across the codebase."""

        def _get_callers() -> str:
            try:
                citadel = _get_citadel()
                full_path = _resolve_path(file_path, code_dir)

                callers = citadel.get_callers(full_path, function_name)

                if not callers:
                    return f"No callers found for '{function_name}'"

                lines = [f"# Callers of {function_name}", ""]
                for caller in callers:
                    func = caller.get("function") or "(file level)"
                    caller_file = Path(caller["file"]).name
                    line_num = caller.get("line", "?")
                    call_type = caller.get("type", "unknown")
                    lines.append(
                        f"- {caller_file}:{line_num} - {func} ({call_type})"
                    )

                lines.append("")
                lines.append(f"Total: {len(callers)} callers")

                return "\n".join(lines)

            except Exception as e:
                logger.exception(f"Error in citadel_get_callers: {e}")
                return f"Error finding callers: {e}"

        loop = asyncio.get_event_loop()
        return await loop.run_in_executor(None, _get_callers)

    async def citadel_get_sequence_diagrams(path: str, max_diagrams: int = 3) -> str:
        """Generate Mermaid sequence diagrams showing call chains."""

        def _get_sequence_diagrams() -> str:
            try:
                citadel = _get_citadel()
                full_path = _resolve_path(path, code_dir)

                diagrams = citadel.get_sequence_diagrams(
                    full_path, max_diagrams=max_diagrams
                )

                if not diagrams:
                    return "No significant call chains found"

                lines = [f"# Sequence Diagrams ({len(diagrams)} call chains)", ""]
                for i, diagram in enumerate(diagrams, 1):
                    lines.append(f"## Call Chain {i}")
                    lines.append(f"```mermaid\n{diagram}\n```")
                    lines.append("")

                return "\n".join(lines)

            except Exception as e:
                logger.exception(f"Error in citadel_get_sequence_diagrams: {e}")
                return f"Error generating sequence diagrams: {e}"

        loop = asyncio.get_event_loop()
        return await loop.run_in_executor(None, _get_sequence_diagrams)

    async def citadel_get_dead_code(path: str) -> str:
        """Find dead code (unreferenced artifacts) in the codebase."""

        def _get_dead_code() -> str:
            try:
                citadel = _get_citadel()
                full_path = _resolve_path(path, code_dir)

                dead = citadel.get_dead_code(full_path)

                if not dead:
                    return "No dead code detected"

                lines = [f"# Dead Code Analysis ({len(dead)} items)", ""]
                for item in dead[:20]:
                    item_file = (
                        Path(item.get("file", "?")).name if item.get("file") else "?"
                    )
                    lines.append(f"- **{item['name']}** ({item['type']})")
                    lines.append(f"  File: {item_file}, Line: {item.get('line', '?')}")
                    lines.append(f"  Reason: {item.get('reason', 'unreferenced')}")
                    lines.append("")

                if len(dead) > 20:
                    lines.append(f"... and {len(dead) - 20} more unreferenced items")

                return "\n".join(lines)

            except Exception as e:
                logger.exception(f"Error in citadel_get_dead_code: {e}")
                return f"Error finding dead code: {e}"

        loop = asyncio.get_event_loop()
        return await loop.run_in_executor(None, _get_dead_code)

    async def citadel_get_flow_diagram(
        file_path: str, paragraph: str | None = None
    ) -> str:
        """Generate a Mermaid flow diagram showing internal control flow."""

        def _get_flow_diagram() -> str:
            try:
                citadel = _get_citadel()
                full_path = _resolve_path(file_path, code_dir)

                diagram = citadel.get_flow_diagram(full_path, paragraph=paragraph)

                return (
                    f"# Flow Diagram for {Path(file_path).name}\n\n"
                    f"```mermaid\n{diagram}\n```"
                )

            except Exception as e:
                logger.exception(f"Error in citadel_get_flow_diagram: {e}")
                return f"Error generating flow diagram: {e}"

        loop = asyncio.get_event_loop()
        return await loop.run_in_executor(None, _get_flow_diagram)

    async def citadel_get_file_summary(file_path: str) -> str:
        """Get a compact summary of a source file."""

        def _get_file_summary() -> str:
            try:
                citadel = _get_citadel()
                full_path = _resolve_path(file_path, code_dir)

                summary = citadel.get_file_summary(full_path)

                if summary.get("error"):
                    return f"Error: {summary['error']}"

                lines = [
                    f"# Summary: {summary['file_name']}",
                    "",
                    f"**Language:** {summary['language']}",
                    f"**Total Lines:** {summary['total_lines']}",
                    f"**Paragraph Count:** {summary['paragraph_count']}",
                ]

                if summary.get("entry_points"):
                    lines.append(
                        f"**Entry Points:** {', '.join(summary['entry_points'])}"
                    )

                if summary.get("main_calls"):
                    lines.append(f"**Main Calls:** {', '.join(summary['main_calls'])}")

                return "\n".join(lines)

            except Exception as e:
                logger.exception(f"Error in citadel_get_file_summary: {e}")
                return f"Error getting file summary: {e}"

        loop = asyncio.get_event_loop()
        return await loop.run_in_executor(None, _get_file_summary)

    async def citadel_get_analysis_patterns(file_path: str) -> str:
        """Extract code patterns (data flow, control flow, error handling)."""

        def _get_analysis_patterns() -> str:
            try:
                citadel = _get_citadel()
                full_path = _resolve_path(file_path, code_dir)

                result = citadel.get_analysis_patterns(full_path)

                if result.error:
                    return f"Error: {result.error}"

                lines = [
                    f"# Analysis Patterns: {Path(file_path).name}",
                    "",
                    f"**Language:** {result.language}",
                    f"**Total Matches:** {result.total_matches}",
                    f"**Pattern Coverage:** {result.coverage_pct:.1f}%",
                    "",
                ]

                for category, cat_result in result.categories.items():
                    lines.append(
                        f"## {category.replace('_', ' ').title()} "
                        f"({cat_result.match_count} matches)"
                    )
                    if cat_result.patterns_matched:
                        for pattern_name, count in sorted(
                            cat_result.patterns_matched.items(), key=lambda x: -x[1]
                        ):
                            lines.append(f"  - {pattern_name}: {count}")
                    lines.append("")

                if result.required_missing:
                    lines.append("## Missing Required Patterns")
                    for pattern in result.required_missing:
                        lines.append(f"  - {pattern}")

                return "\n".join(lines)

            except Exception as e:
                logger.exception(f"Error in citadel_get_analysis_patterns: {e}")
                return f"Error extracting analysis patterns: {e}"

        loop = asyncio.get_event_loop()
        return await loop.run_in_executor(None, _get_analysis_patterns)

    # -------------------------------------------------------------------------
    # Tool Definitions
    # -------------------------------------------------------------------------

    return [
        ToolDefinition(
            name="citadel_analyze_file",
            description=(
                "Perform full structural analysis of a source file. "
                "Use this tool when you need to understand the complete structure "
                "of a source file, including all artifacts (programs, paragraphs, "
                "functions) and their relationships. "
                "Returns language detected, all artifacts with line numbers, "
                "callouts (calls, includes, reads, writes), and preprocessor includes."
            ),
            parameters={
                "type": "object",
                "properties": {
                    "file_path": {
                        "type": "string",
                        "description": (
                            "Path to source file "
                            "(relative to code directory or absolute)"
                        ),
                    },
                },
                "required": ["file_path"],
            },
            handler=citadel_analyze_file,
        ),
        ToolDefinition(
            name="citadel_get_functions",
            description=(
                "Get all functions/paragraphs in a file with their callouts. "
                "Use this tool for a quick overview of what functions exist in "
                "a file and what each function calls. More compact than full "
                "analyze_file when you only need function-level info."
            ),
            parameters={
                "type": "object",
                "properties": {
                    "file_path": {
                        "type": "string",
                        "description": (
                            "Path to source file "
                            "(relative to code directory or absolute)"
                        ),
                    },
                },
                "required": ["file_path"],
            },
            handler=citadel_get_functions,
        ),
        ToolDefinition(
            name="citadel_get_callouts",
            description=(
                "Get all callouts/references from a file or directory. "
                "Use this tool to find all external references (calls, includes, "
                "reads, writes) from a file or across a directory. Useful for "
                "understanding dependencies and data flow."
            ),
            parameters={
                "type": "object",
                "properties": {
                    "path": {
                        "type": "string",
                        "description": (
                            "Path to source file or directory "
                            "(relative to code directory or absolute)"
                        ),
                    },
                },
                "required": ["path"],
            },
            handler=citadel_get_callouts,
        ),
        ToolDefinition(
            name="citadel_get_includes",
            description=(
                "Get all preprocessor includes (COPY statements, etc.). "
                "Use this tool to find all files included via preprocessor "
                "directives like COBOL COPY statements, C #include, etc."
            ),
            parameters={
                "type": "object",
                "properties": {
                    "file_path": {
                        "type": "string",
                        "description": (
                            "Path to source file "
                            "(relative to code directory or absolute)"
                        ),
                    },
                },
                "required": ["file_path"],
            },
            handler=citadel_get_includes,
        ),
        ToolDefinition(
            name="citadel_get_function_body",
            description=(
                "Extract the source code body of a specific function. "
                "Use this tool when you need to see the actual implementation "
                "of a specific paragraph or function. Essential for understanding "
                "the business logic within a function."
            ),
            parameters={
                "type": "object",
                "properties": {
                    "file_path": {
                        "type": "string",
                        "description": (
                            "Path to source file "
                            "(relative to code directory or absolute)"
                        ),
                    },
                    "function_name": {
                        "type": "string",
                        "description": (
                            "Name of the function/paragraph to extract "
                            "(case-insensitive)"
                        ),
                    },
                },
                "required": ["file_path", "function_name"],
            },
            handler=citadel_get_function_body,
        ),
        ToolDefinition(
            name="citadel_get_function_bodies",
            description=(
                "Batch extract multiple function bodies efficiently. "
                "Use this tool when you need to examine several functions "
                "from the same file. More efficient than calling "
                "get_function_body multiple times because the file is "
                "parsed only once."
            ),
            parameters={
                "type": "object",
                "properties": {
                    "file_path": {
                        "type": "string",
                        "description": (
                            "Path to source file "
                            "(relative to code directory or absolute)"
                        ),
                    },
                    "function_names": {
                        "type": "array",
                        "items": {"type": "string"},
                        "description": "List of function/paragraph names to extract",
                    },
                },
                "required": ["file_path", "function_names"],
            },
            handler=citadel_get_function_bodies,
        ),
        ToolDefinition(
            name="citadel_get_file_stats",
            description=(
                "Get structural statistics for planning documentation strategy. "
                "Use this tool to understand the size and structure of a file "
                "before deciding how to document it. Returns line counts, "
                "paragraph counts, and paragraph details."
            ),
            parameters={
                "type": "object",
                "properties": {
                    "file_path": {
                        "type": "string",
                        "description": (
                            "Path to source file "
                            "(relative to code directory or absolute)"
                        ),
                    },
                },
                "required": ["file_path"],
            },
            handler=citadel_get_file_stats,
        ),
        ToolDefinition(
            name="citadel_get_callers",
            description=(
                "Find all callers of a specific function across the codebase. "
                "Use this tool for reverse dependency analysis - finding all "
                "places that call or reference a particular function. Essential "
                "for impact analysis and understanding code flow."
            ),
            parameters={
                "type": "object",
                "properties": {
                    "file_path": {
                        "type": "string",
                        "description": "Path to file containing the target function",
                    },
                    "function_name": {
                        "type": "string",
                        "description": "Name of the function to find callers for",
                    },
                },
                "required": ["file_path", "function_name"],
            },
            handler=citadel_get_callers,
        ),
        ToolDefinition(
            name="citadel_get_sequence_diagrams",
            description=(
                "Generate Mermaid sequence diagrams showing call chains. "
                "Use this tool to visualize the longest call chains across "
                "multiple files. Shows how programs call each other in sequence. "
                "Note: This analyzes the entire directory which may take time."
            ),
            parameters={
                "type": "object",
                "properties": {
                    "path": {
                        "type": "string",
                        "description": "Source directory or graph JSON file path",
                    },
                    "max_diagrams": {
                        "type": "integer",
                        "description": "Maximum number of diagrams to generate",
                        "default": 3,
                        "minimum": 1,
                        "maximum": 10,
                    },
                },
                "required": ["path"],
            },
            handler=citadel_get_sequence_diagrams,
        ),
        ToolDefinition(
            name="citadel_get_dead_code",
            description=(
                "Find dead code (unreferenced artifacts) in the codebase. "
                "Use this tool to identify paragraphs, copybooks, or programs "
                "that are never called or referenced. Useful for cleanup and "
                "understanding which code is actually used. "
                "Note: Entry points are excluded (they're expected to have no callers)."
            ),
            parameters={
                "type": "object",
                "properties": {
                    "path": {
                        "type": "string",
                        "description": "Source directory or graph JSON file path",
                    },
                },
                "required": ["path"],
            },
            handler=citadel_get_dead_code,
        ),
        ToolDefinition(
            name="citadel_get_flow_diagram",
            description=(
                "Generate a Mermaid flow diagram showing internal control flow. "
                "Use this tool to visualize how paragraphs call each other within "
                "a file. Produces a Mermaid flowchart that shows PERFORM relationships "
                "and external calls as leaf nodes."
            ),
            parameters={
                "type": "object",
                "properties": {
                    "file_path": {
                        "type": "string",
                        "description": "Path to source file (typically COBOL)",
                    },
                    "paragraph": {
                        "type": "string",
                        "description": (
                            "Optional starting paragraph "
                            "(shows entire file if not specified)"
                        ),
                    },
                },
                "required": ["file_path"],
            },
            handler=citadel_get_flow_diagram,
        ),
        ToolDefinition(
            name="citadel_get_file_summary",
            description=(
                "Get a compact summary of a source file. "
                "Use this tool for a quick overview of a file without detailed "
                "analysis. Returns essential metadata, paragraph count, entry "
                "points, and main calls."
            ),
            parameters={
                "type": "object",
                "properties": {
                    "file_path": {
                        "type": "string",
                        "description": (
                            "Path to source file "
                            "(relative to code directory or absolute)"
                        ),
                    },
                },
                "required": ["file_path"],
            },
            handler=citadel_get_file_summary,
        ),
        ToolDefinition(
            name="citadel_get_analysis_patterns",
            description=(
                "Extract code patterns (data flow, control flow, error handling). "
                "Use this tool to understand how data moves through the code, "
                "control flow structures used, and error handling approaches. "
                "Categories: data_flow (MOVE, COMPUTE, SET), "
                "control_flow (IF, EVALUATE, PERFORM loops, GO TO), "
                "error_handling (ON EXCEPTION, file status checks)."
            ),
            parameters={
                "type": "object",
                "properties": {
                    "file_path": {
                        "type": "string",
                        "description": (
                            "Path to source file "
                            "(relative to code directory or absolute)"
                        ),
                    },
                },
                "required": ["file_path"],
            },
            handler=citadel_get_analysis_patterns,
        ),
    ]
