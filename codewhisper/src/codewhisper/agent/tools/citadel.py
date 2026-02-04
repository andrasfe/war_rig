"""Citadel code analysis tools for CodeWhisper agent.

This module wraps Citadel SDK functions as tools for the agent, enabling
deep code analysis on mainframe codebases.

Tools:
    - citadel_analyze_file: Full structural analysis of a source file
    - citadel_get_functions: List all functions with their callouts
    - citadel_get_callouts: Get all references from a file or directory
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
    from codewhisper.agent.tools.citadel import register_citadel_tools
    from codewhisper.agent.tools.registry import ToolRegistry

    registry = ToolRegistry()
    register_citadel_tools(registry, config)
"""

from __future__ import annotations

import logging
from pathlib import Path
from typing import TYPE_CHECKING, Any

from codewhisper.agent.protocol import ToolDefinition

if TYPE_CHECKING:
    from codewhisper.agent.tools.registry import ToolRegistry
    from codewhisper.config import AgentConfig

logger = logging.getLogger(__name__)

# Module-level references set by register_citadel_tools
_config: AgentConfig | None = None
_citadel: Any = None


def register_citadel_tools(
    registry: "ToolRegistry",
    config: "AgentConfig",
) -> None:
    """Register Citadel analysis tools with the registry.

    Args:
        registry: The tool registry to register with.
        config: Agent configuration with code_dir path.
    """
    global _config
    _config = config

    # Register all Citadel tools
    _register_analyze_file(registry)
    _register_get_functions(registry)
    _register_get_callouts(registry)
    _register_get_includes(registry)
    _register_get_function_body(registry)
    _register_get_function_bodies(registry)
    _register_get_file_stats(registry)
    _register_get_callers(registry)
    _register_get_sequence_diagrams(registry)
    _register_get_dead_code(registry)
    _register_get_flow_diagram(registry)
    _register_get_file_summary(registry)
    _register_get_analysis_patterns(registry)


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


def _resolve_path(file_path: str) -> Path:
    """Resolve file path relative to code directory.

    Args:
        file_path: Path to file (relative to code_dir or absolute).

    Returns:
        Resolved absolute path.

    Raises:
        RuntimeError: If config is not set.
    """
    if _config is None:
        raise RuntimeError(
            "Citadel tools not configured. Call register_citadel_tools first."
        )

    path = Path(file_path)
    if not path.is_absolute():
        path = _config.code_dir / path
    return path.resolve()


# =============================================================================
# Tool Registration Functions
# =============================================================================


def _register_analyze_file(registry: "ToolRegistry") -> None:
    """Register citadel_analyze_file tool."""
    registry.register(
        ToolDefinition(
            name="citadel_analyze_file",
            description="""Perform full structural analysis of a source file.

Use this tool when you need to understand the complete structure
of a source file, including all artifacts (programs, paragraphs,
functions) and their relationships.

Returns: Language detected, all artifacts with line numbers,
callouts (calls, includes, reads, writes), preprocessor includes.
""",
            parameters={
                "type": "object",
                "properties": {
                    "file_path": {
                        "type": "string",
                        "description": "Path to source file (relative to code directory or absolute)",
                    },
                },
                "required": ["file_path"],
            },
            handler=_analyze_file_handler,
        )
    )


def _register_get_functions(registry: "ToolRegistry") -> None:
    """Register citadel_get_functions tool."""
    registry.register(
        ToolDefinition(
            name="citadel_get_functions",
            description="""Get all functions/paragraphs in a file with their callouts.

Use this tool for a quick overview of what functions exist in
a file and what each function calls. More compact than full
analyze_file when you only need function-level info.
""",
            parameters={
                "type": "object",
                "properties": {
                    "file_path": {
                        "type": "string",
                        "description": "Path to source file",
                    },
                },
                "required": ["file_path"],
            },
            handler=_get_functions_handler,
        )
    )


def _register_get_callouts(registry: "ToolRegistry") -> None:
    """Register citadel_get_callouts tool."""
    registry.register(
        ToolDefinition(
            name="citadel_get_callouts",
            description="""Get all callouts/references from a file or directory.

Use this tool to find all external references (calls, includes,
reads, writes) from a file or across a directory.
""",
            parameters={
                "type": "object",
                "properties": {
                    "path": {
                        "type": "string",
                        "description": "Path to source file or directory",
                    },
                },
                "required": ["path"],
            },
            handler=_get_callouts_handler,
        )
    )


def _register_get_includes(registry: "ToolRegistry") -> None:
    """Register citadel_get_includes tool."""
    registry.register(
        ToolDefinition(
            name="citadel_get_includes",
            description="""Get all preprocessor includes (COPY statements, etc.).

Use this tool to find all files included via preprocessor
directives like COBOL COPY statements.
""",
            parameters={
                "type": "object",
                "properties": {
                    "file_path": {
                        "type": "string",
                        "description": "Path to source file",
                    },
                },
                "required": ["file_path"],
            },
            handler=_get_includes_handler,
        )
    )


def _register_get_function_body(registry: "ToolRegistry") -> None:
    """Register citadel_get_function_body tool."""
    registry.register(
        ToolDefinition(
            name="citadel_get_function_body",
            description="""Extract the source code body of a specific function.

Use this tool when you need to see the actual implementation
of a specific paragraph or function.
""",
            parameters={
                "type": "object",
                "properties": {
                    "file_path": {
                        "type": "string",
                        "description": "Path to source file",
                    },
                    "function_name": {
                        "type": "string",
                        "description": "Name of the function/paragraph to extract",
                    },
                },
                "required": ["file_path", "function_name"],
            },
            handler=_get_function_body_handler,
        )
    )


def _register_get_function_bodies(registry: "ToolRegistry") -> None:
    """Register citadel_get_function_bodies tool."""
    registry.register(
        ToolDefinition(
            name="citadel_get_function_bodies",
            description="""Batch extract multiple function bodies efficiently.

Use this tool when you need to examine several functions
from the same file. More efficient than calling
get_function_body multiple times.
""",
            parameters={
                "type": "object",
                "properties": {
                    "file_path": {
                        "type": "string",
                        "description": "Path to source file",
                    },
                    "function_names": {
                        "type": "array",
                        "items": {"type": "string"},
                        "description": "List of function/paragraph names to extract",
                    },
                },
                "required": ["file_path", "function_names"],
            },
            handler=_get_function_bodies_handler,
        )
    )


def _register_get_file_stats(registry: "ToolRegistry") -> None:
    """Register citadel_get_file_stats tool."""
    registry.register(
        ToolDefinition(
            name="citadel_get_file_stats",
            description="""Get structural statistics for a file.

Use this tool to understand the size and structure of a file
before deciding how to document it.
""",
            parameters={
                "type": "object",
                "properties": {
                    "file_path": {
                        "type": "string",
                        "description": "Path to source file",
                    },
                },
                "required": ["file_path"],
            },
            handler=_get_file_stats_handler,
        )
    )


def _register_get_callers(registry: "ToolRegistry") -> None:
    """Register citadel_get_callers tool."""
    registry.register(
        ToolDefinition(
            name="citadel_get_callers",
            description="""Find all callers of a specific function across the codebase.

Use this tool for reverse dependency analysis - finding all
places that call or reference a particular function.
""",
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
            handler=_get_callers_handler,
        )
    )


def _register_get_sequence_diagrams(registry: "ToolRegistry") -> None:
    """Register citadel_get_sequence_diagrams tool."""
    registry.register(
        ToolDefinition(
            name="citadel_get_sequence_diagrams",
            description="""Generate Mermaid sequence diagrams showing call chains.

Use this tool to visualize the longest call chains across
multiple files.
""",
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
            handler=_get_sequence_diagrams_handler,
        )
    )


def _register_get_dead_code(registry: "ToolRegistry") -> None:
    """Register citadel_get_dead_code tool."""
    registry.register(
        ToolDefinition(
            name="citadel_get_dead_code",
            description="""Find dead code (unreferenced artifacts) in the codebase.

Use this tool to identify paragraphs, copybooks, or programs
that are never called or referenced.
""",
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
            handler=_get_dead_code_handler,
        )
    )


def _register_get_flow_diagram(registry: "ToolRegistry") -> None:
    """Register citadel_get_flow_diagram tool."""
    registry.register(
        ToolDefinition(
            name="citadel_get_flow_diagram",
            description="""Generate a Mermaid flow diagram showing internal control flow.

Use this tool to visualize how paragraphs call each other within
a file.
""",
            parameters={
                "type": "object",
                "properties": {
                    "file_path": {
                        "type": "string",
                        "description": "Path to source file",
                    },
                    "paragraph": {
                        "type": "string",
                        "description": "Optional starting paragraph (shows entire file if not specified)",
                    },
                },
                "required": ["file_path"],
            },
            handler=_get_flow_diagram_handler,
        )
    )


def _register_get_file_summary(registry: "ToolRegistry") -> None:
    """Register citadel_get_file_summary tool."""
    registry.register(
        ToolDefinition(
            name="citadel_get_file_summary",
            description="""Get a compact summary of a source file.

Use this tool for a quick overview of a file without detailed
analysis.
""",
            parameters={
                "type": "object",
                "properties": {
                    "file_path": {
                        "type": "string",
                        "description": "Path to source file",
                    },
                },
                "required": ["file_path"],
            },
            handler=_get_file_summary_handler,
        )
    )


def _register_get_analysis_patterns(registry: "ToolRegistry") -> None:
    """Register citadel_get_analysis_patterns tool."""
    registry.register(
        ToolDefinition(
            name="citadel_get_analysis_patterns",
            description="""Extract code patterns (data flow, control flow, error handling).

Use this tool to understand how data moves through the code,
control flow structures used, and error handling approaches.
""",
            parameters={
                "type": "object",
                "properties": {
                    "file_path": {
                        "type": "string",
                        "description": "Path to source file",
                    },
                },
                "required": ["file_path"],
            },
            handler=_get_analysis_patterns_handler,
        )
    )


# =============================================================================
# Tool Handler Functions
# =============================================================================


async def _analyze_file_handler(file_path: str) -> str:
    """Handler for citadel_analyze_file tool."""
    try:
        citadel = _get_citadel()
        full_path = _resolve_path(file_path)

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
                    lines.append(f"  ... and {len(artifact.callouts) - 10} more")
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


async def _get_functions_handler(file_path: str) -> str:
    """Handler for citadel_get_functions tool."""
    try:
        citadel = _get_citadel()
        full_path = _resolve_path(file_path)

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


async def _get_callouts_handler(path: str) -> str:
    """Handler for citadel_get_callouts tool."""
    try:
        citadel = _get_citadel()
        full_path = _resolve_path(path)

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


async def _get_includes_handler(file_path: str) -> str:
    """Handler for citadel_get_includes tool."""
    try:
        citadel = _get_citadel()
        full_path = _resolve_path(file_path)

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


async def _get_function_body_handler(file_path: str, function_name: str) -> str:
    """Handler for citadel_get_function_body tool."""
    try:
        citadel = _get_citadel()
        full_path = _resolve_path(file_path)

        body = citadel.get_function_body(full_path, function_name)

        if body is None:
            return f"Function '{function_name}' not found in {file_path}"

        return f"# {function_name} in {Path(file_path).name}\n\n```\n{body}\n```"

    except Exception as e:
        logger.exception(f"Error in citadel_get_function_body: {e}")
        return f"Error extracting function body: {e}"


async def _get_function_bodies_handler(
    file_path: str, function_names: list[str]
) -> str:
    """Handler for citadel_get_function_bodies tool."""
    try:
        citadel = _get_citadel()
        full_path = _resolve_path(file_path)

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

        lines.insert(1, f"Found {found_count} of {len(function_names)} functions")

        return "\n".join(lines)

    except Exception as e:
        logger.exception(f"Error in citadel_get_function_bodies: {e}")
        return f"Error extracting function bodies: {e}"


async def _get_file_stats_handler(file_path: str) -> str:
    """Handler for citadel_get_file_stats tool."""
    try:
        citadel = _get_citadel()
        full_path = _resolve_path(file_path)

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
                    f"- **{para['name']}**: lines {line_range} ({line_count} lines)"
                )

        return "\n".join(lines)

    except Exception as e:
        logger.exception(f"Error in citadel_get_file_stats: {e}")
        return f"Error getting file stats: {e}"


async def _get_callers_handler(file_path: str, function_name: str) -> str:
    """Handler for citadel_get_callers tool."""
    try:
        citadel = _get_citadel()
        full_path = _resolve_path(file_path)

        callers = citadel.get_callers(full_path, function_name)

        if not callers:
            return f"No callers found for '{function_name}'"

        lines = [f"# Callers of {function_name}", ""]
        for caller in callers:
            func = caller.get("function") or "(file level)"
            caller_file = Path(caller["file"]).name
            line_num = caller.get("line", "?")
            call_type = caller.get("type", "unknown")
            lines.append(f"- {caller_file}:{line_num} - {func} ({call_type})")

        lines.append("")
        lines.append(f"Total: {len(callers)} callers")

        return "\n".join(lines)

    except Exception as e:
        logger.exception(f"Error in citadel_get_callers: {e}")
        return f"Error finding callers: {e}"


async def _get_sequence_diagrams_handler(path: str, max_diagrams: int = 3) -> str:
    """Handler for citadel_get_sequence_diagrams tool."""
    try:
        citadel = _get_citadel()
        full_path = _resolve_path(path)

        diagrams = citadel.get_sequence_diagrams(full_path, max_diagrams=max_diagrams)

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


async def _get_dead_code_handler(path: str) -> str:
    """Handler for citadel_get_dead_code tool."""
    try:
        citadel = _get_citadel()
        full_path = _resolve_path(path)

        dead = citadel.get_dead_code(full_path)

        if not dead:
            return "No dead code detected"

        lines = [f"# Dead Code Analysis ({len(dead)} items)", ""]
        for item in dead[:20]:
            item_file = Path(item.get("file", "?")).name if item.get("file") else "?"
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


async def _get_flow_diagram_handler(
    file_path: str, paragraph: str | None = None
) -> str:
    """Handler for citadel_get_flow_diagram tool."""
    try:
        citadel = _get_citadel()
        full_path = _resolve_path(file_path)

        diagram = citadel.get_flow_diagram(full_path, paragraph=paragraph)

        return (
            f"# Flow Diagram for {Path(file_path).name}\n\n```mermaid\n{diagram}\n```"
        )

    except Exception as e:
        logger.exception(f"Error in citadel_get_flow_diagram: {e}")
        return f"Error generating flow diagram: {e}"


async def _get_file_summary_handler(file_path: str) -> str:
    """Handler for citadel_get_file_summary tool."""
    try:
        citadel = _get_citadel()
        full_path = _resolve_path(file_path)

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
            lines.append(f"**Entry Points:** {', '.join(summary['entry_points'])}")

        if summary.get("main_calls"):
            lines.append(f"**Main Calls:** {', '.join(summary['main_calls'])}")

        return "\n".join(lines)

    except Exception as e:
        logger.exception(f"Error in citadel_get_file_summary: {e}")
        return f"Error getting file summary: {e}"


async def _get_analysis_patterns_handler(file_path: str) -> str:
    """Handler for citadel_get_analysis_patterns tool."""
    try:
        citadel = _get_citadel()
        full_path = _resolve_path(file_path)

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
                f"## {category.replace('_', ' ').title()} ({cat_result.match_count} matches)"
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
