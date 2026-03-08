"""Pure-Python COBOL AST generator.

Reuses the citadel parsing stack (SourceReader, DataDivisionParser,
ProcedureDivisionParser, syntax_tree) and produces JSON-serialized
paragraph-level syntax trees.
"""

from __future__ import annotations

import json
import re
from pathlib import Path
from typing import Any

from citadel.cobol.source_reader import CobolSource, SourceLine, SourceReader
from citadel.cobol.syntax_tree import (
    ParagraphSyntaxTree,
    StatementType,
    SyntaxNode,
    build_file_ast,
    format_file_ast,
    validate_file_ast,
)

_PROGRAM_ID_RE = re.compile(
    r"PROGRAM-ID\.\s+([A-Za-z0-9_-]+)", re.IGNORECASE
)


def parse_cobol_ast(
    source_path: str | Path,
    copybook_dirs: list[str | Path] | None = None,
) -> tuple[dict[str, ParagraphSyntaxTree], str, str]:
    """Parse a COBOL source file and return paragraph-level ASTs.

    Parses a COBOL source file entirely in Python and returns AST
    structures plus a JSON string.

    Args:
        source_path: Path to the COBOL source file.
        copybook_dirs: Directories to search for copybooks.

    Returns:
        Tuple of (paragraph_name -> ParagraphSyntaxTree, full_ast_text,
        raw_json).
    """
    from citadel.cobol.data_division import DataDivisionParser
    from citadel.cobol.procedure_division import ProcedureDivisionParser

    cb_dirs = [str(d) for d in copybook_dirs] if copybook_dirs else None
    reader = SourceReader(
        copybook_dirs=cb_dirs,
        skip_missing_copybooks=True,
    )
    source = reader.read(str(source_path))

    data_items = DataDivisionParser().parse(source)
    proc_parser = ProcedureDivisionParser(data_items=data_items)
    paragraphs, _ = proc_parser.parse(source)

    para_source_lines: dict[str, list[SourceLine]] = {}
    for para in paragraphs:
        para_source_lines[para.name] = [
            ln for ln in source.lines
            if para.line_start <= ln.line_number <= para.line_end
        ]

    program_id = _extract_program_id(source)

    return build_ast_from_parsed(para_source_lines, data_items, program_id)


def build_ast_from_parsed(
    para_source_lines: dict[str, list[SourceLine]],
    data_items: list | None,
    program_id: str,
) -> tuple[dict[str, ParagraphSyntaxTree], str, str]:
    """Build AST from already-parsed data (avoids double-parsing in sdk.py).

    Args:
        para_source_lines: Paragraph name -> list of SourceLines.
        data_items: Parsed data items (or None).
        program_id: Program identifier string.

    Returns:
        Tuple of (paragraph_name -> ParagraphSyntaxTree, full_ast_text,
        raw_json).
    """
    trees = build_file_ast(
        para_source_lines, data_items, preserve_newlines=True,
    )

    # Validate AST against source
    issues = validate_file_ast(trees, para_source_lines)
    if issues:
        import logging
        _logger = logging.getLogger(__name__)
        _logger.warning(
            "AST validation: %d issues in %s", len(issues), program_id,
        )
        for issue in issues[:10]:
            _logger.warning(
                "  %s [%s] L%d: %s — %s",
                issue.paragraph, issue.node_type, issue.line,
                issue.issue, issue.source_text,
            )

    full_text = format_file_ast(trees)
    raw_json = _serialize_to_json(trees, program_id)
    return trees, full_text, raw_json


def _extract_program_id(source: CobolSource) -> str:
    """Extract PROGRAM-ID from IDENTIFICATION DIVISION, fallback to filename."""
    id_lines = source.lines_for_division("IDENTIFICATION")
    for ln in id_lines:
        m = _PROGRAM_ID_RE.search(ln.text)
        if m:
            return m.group(1).upper()
    # Fallback: filename stem
    return Path(source.source_file).stem.upper()


def _serialize_to_json(
    trees: dict[str, ParagraphSyntaxTree],
    program_id: str,
) -> str:
    """Serialize ParagraphSyntaxTree dict to JSON."""
    paragraphs: list[dict[str, Any]] = []
    for tree in trees.values():
        para_dict: dict[str, Any] = {
            "name": tree.paragraph_name,
            "line_start": tree.root.line_start,
            "line_end": tree.root.line_end,
            "statements": [
                _serialize_node(child)
                for child in tree.root.children
            ],
        }
        paragraphs.append(para_dict)

    data: dict[str, Any] = {
        "program_id": program_id,
        "paragraphs": paragraphs,
    }
    return json.dumps(data, indent=None, ensure_ascii=False)


def _serialize_node(node: SyntaxNode) -> dict[str, Any]:
    """Recursively serialize a SyntaxNode to a JSON-compatible dict."""
    type_str = node.node_type.value
    result: dict[str, Any] = {
        "type": type_str,
        "text": node.source_text,
        "line_start": node.line_start,
        "line_end": node.line_end,
        "attributes": node.attributes,
        "children": [_serialize_node(c) for c in node.children],
    }
    return result


# ---------------------------------------------------------------------------
# JSON → ParagraphSyntaxTree deserialization
# ---------------------------------------------------------------------------

_TYPE_MAP: dict[str, StatementType] = {
    "PARAGRAPH": StatementType.PARAGRAPH,
    "IF": StatementType.IF,
    "ELSE": StatementType.ELSE,
    "EVALUATE": StatementType.EVALUATE,
    "WHEN": StatementType.WHEN,
    "PERFORM_INLINE": StatementType.PERFORM_INLINE,
    "PERFORM": StatementType.PERFORM,
    "PERFORM_THRU": StatementType.PERFORM_THRU,
    "PERFORM_UNTIL": StatementType.PERFORM_UNTIL,
    "MOVE": StatementType.MOVE,
    "COMPUTE": StatementType.COMPUTE,
    "ADD": StatementType.ADD,
    "SUBTRACT": StatementType.SUBTRACT,
    "MULTIPLY": StatementType.MULTIPLY,
    "DIVIDE": StatementType.DIVIDE,
    "SET": StatementType.SET,
    "INITIALIZE": StatementType.INITIALIZE,
    "STRING": StatementType.STRING_STMT,
    "UNSTRING": StatementType.UNSTRING,
    "INSPECT": StatementType.INSPECT,
    "ACCEPT": StatementType.ACCEPT,
    "DISPLAY": StatementType.DISPLAY,
    "READ": StatementType.READ_STMT,
    "WRITE": StatementType.WRITE_STMT,
    "REWRITE": StatementType.REWRITE,
    "OPEN": StatementType.OPEN,
    "CLOSE": StatementType.CLOSE,
    "SORT": StatementType.SORT,
    "SEARCH": StatementType.SEARCH,
    "EXEC_SQL": StatementType.EXEC_SQL,
    "EXEC_CICS": StatementType.EXEC_CICS,
    "EXEC_DLI": StatementType.EXEC_DLI,
    "EXEC_OTHER": StatementType.EXEC_OTHER,
    "CALL": StatementType.CALL,
    "GOBACK": StatementType.GOBACK,
    "STOP_RUN": StatementType.STOP_RUN,
    "GO_TO": StatementType.GO_TO,
    "EXIT": StatementType.EXIT,
    "CONTINUE": StatementType.CONTINUE_STMT,
    "ALTER": StatementType.ALTER,
    "UNKNOWN": StatementType.UNKNOWN,
}


def load_ast_json(
    json_text: str,
) -> tuple[dict[str, ParagraphSyntaxTree], str]:
    """Load a ``.ast`` JSON file and return AST structures.

    Args:
        json_text: Raw JSON string (Cobalt AST format).

    Returns:
        Tuple of (paragraph_name -> ParagraphSyntaxTree, full_ast_text).
    """
    data = json.loads(json_text)
    trees = _deserialize_paragraphs(data)
    full_text = format_file_ast(trees)
    return trees, full_text


def _deserialize_paragraphs(
    data: dict[str, Any],
) -> dict[str, ParagraphSyntaxTree]:
    """Convert AST JSON to ParagraphSyntaxTree objects."""
    trees: dict[str, ParagraphSyntaxTree] = {}

    for para_json in data.get("paragraphs", []):
        name = para_json.get("name", "")
        if not name:
            continue

        statements_json = para_json.get("statements", [])
        children = [_json_to_node(s) for s in statements_json]

        line_start = para_json.get("line_start", 0)
        line_end = para_json.get("line_end", 0)

        root = SyntaxNode(
            node_type=StatementType.PARAGRAPH,
            source_text="",
            line_start=line_start,
            line_end=line_end,
            children=children,
            attributes={"name": name},
        )

        stmt_count = _count_statements(root)
        max_depth = _max_depth(root)

        trees[name] = ParagraphSyntaxTree(
            paragraph_name=name,
            root=root,
            statement_count=stmt_count,
            max_nesting_depth=max_depth,
        )

    return trees


def _json_to_node(data: dict[str, Any]) -> SyntaxNode:
    """Recursively convert a JSON statement dict to a SyntaxNode."""
    type_str = data.get("type", "UNKNOWN")
    node_type = _TYPE_MAP.get(type_str, StatementType.UNKNOWN)

    children_json = data.get("children", [])
    children = [_json_to_node(c) for c in children_json]

    attributes = data.get("attributes", {})
    attributes = {k: v for k, v in attributes.items() if v}

    return SyntaxNode(
        node_type=node_type,
        source_text=data.get("text", ""),
        line_start=data.get("line_start", 0),
        line_end=data.get("line_end", 0),
        children=children,
        attributes=attributes,
    )


def _count_statements(node: SyntaxNode) -> int:
    """Count non-root statement nodes."""
    count = 0
    for child in node.children:
        count += 1
        count += _count_statements(child)
    return count


def _max_depth(node: SyntaxNode, depth: int = 0) -> int:
    """Compute maximum nesting depth."""
    if not node.children:
        return depth
    return max(_max_depth(c, depth + 1) for c in node.children)
