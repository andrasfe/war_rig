"""Syntax tree builder for COBOL paragraphs.

Builds a statement-level AST with nested control flow
(IF/EVALUATE/PERFORM UNTIL) as branch nodes and data operations
(MOVE, COMPUTE, EXEC SQL, etc.) as leaf nodes.
"""

from __future__ import annotations

import re
from dataclasses import dataclass, field
from enum import Enum
from typing import Any

from citadel.cobol.source_reader import SourceLine

# ---------------------------------------------------------------------------
# Statement types
# ---------------------------------------------------------------------------

class StatementType(Enum):
    """Types of COBOL statements in the syntax tree."""

    # Root container
    PARAGRAPH = "PARAGRAPH"

    # Control flow containers (have children)
    IF = "IF"
    ELSE = "ELSE"
    EVALUATE = "EVALUATE"
    WHEN = "WHEN"
    PERFORM_INLINE = "PERFORM_INLINE"

    # PERFORM calls (leaf nodes)
    PERFORM = "PERFORM"
    PERFORM_THRU = "PERFORM_THRU"
    PERFORM_UNTIL = "PERFORM_UNTIL"

    # Data operations
    MOVE = "MOVE"
    COMPUTE = "COMPUTE"
    ADD = "ADD"
    SUBTRACT = "SUBTRACT"
    MULTIPLY = "MULTIPLY"
    DIVIDE = "DIVIDE"
    SET = "SET"
    INITIALIZE = "INITIALIZE"
    STRING_STMT = "STRING"
    UNSTRING = "UNSTRING"
    INSPECT = "INSPECT"

    # I/O
    ACCEPT = "ACCEPT"
    DISPLAY = "DISPLAY"
    READ_STMT = "READ"
    WRITE_STMT = "WRITE"
    REWRITE = "REWRITE"
    OPEN = "OPEN"
    CLOSE = "CLOSE"
    SORT = "SORT"
    SEARCH = "SEARCH"

    # EXEC blocks
    EXEC_SQL = "EXEC_SQL"
    EXEC_CICS = "EXEC_CICS"
    EXEC_DLI = "EXEC_DLI"
    EXEC_OTHER = "EXEC_OTHER"

    # Flow control
    CALL = "CALL"
    GOBACK = "GOBACK"
    STOP_RUN = "STOP_RUN"
    GO_TO = "GO_TO"
    EXIT = "EXIT"
    CONTINUE_STMT = "CONTINUE"
    ALTER = "ALTER"

    UNKNOWN = "UNKNOWN"


# ---------------------------------------------------------------------------
# Compiled regex patterns
# ---------------------------------------------------------------------------

_EXEC_START_RE = re.compile(
    r"\bEXEC\s+(\w+)\b", re.IGNORECASE
)
_EXEC_END_RE = re.compile(r"\bEND-EXEC\b", re.IGNORECASE)

_END_IF_RE = re.compile(r"^\s*END-IF\b", re.IGNORECASE)
_END_EVALUATE_RE = re.compile(
    r"^\s*END-EVALUATE\b", re.IGNORECASE
)
_END_PERFORM_RE = re.compile(
    r"^\s*END-PERFORM\b", re.IGNORECASE
)

_IF_START_RE = re.compile(r"^\s*IF\b", re.IGNORECASE)
_ELSE_RE = re.compile(r"^\s*ELSE\b", re.IGNORECASE)
_EVALUATE_START_RE = re.compile(
    r"^\s*EVALUATE\b", re.IGNORECASE
)
_WHEN_RE = re.compile(r"^\s*WHEN\b", re.IGNORECASE)

_PERFORM_UNTIL_INLINE_RE = re.compile(
    r"^\s*PERFORM\s+UNTIL\b", re.IGNORECASE
)
_PERFORM_VARYING_INLINE_RE = re.compile(
    r"^\s*PERFORM\s+VARYING\b", re.IGNORECASE
)
_PERFORM_THRU_RE = re.compile(
    r"^\s*PERFORM\s+(\S+)\s+THRU\s+(\S+)", re.IGNORECASE
)
_PERFORM_UNTIL_NAMED_RE = re.compile(
    r"^\s*PERFORM\s+(\S+)\s+UNTIL\b", re.IGNORECASE
)
_PERFORM_TIMES_RE = re.compile(
    r"^\s*PERFORM\s+(\S+)\s+(\d+)\s+TIMES", re.IGNORECASE
)
_PERFORM_SIMPLE_RE = re.compile(
    r"^\s*PERFORM\s+([A-Za-z0-9][A-Za-z0-9_-]*)",
    re.IGNORECASE,
)

_MOVE_RE = re.compile(
    r"\bMOVE\s+(.+?)\s+TO\s+(.+)", re.IGNORECASE
)
_COMPUTE_RE = re.compile(
    r"\bCOMPUTE\s+([A-Za-z][A-Za-z0-9_-]*)\s*=\s*(.+)",
    re.IGNORECASE,
)
_CALL_RE = re.compile(r"\bCALL\b", re.IGNORECASE)
_CALL_TARGET_RE = re.compile(
    r"\bCALL\s+['\"]?([A-Za-z0-9_-]+)", re.IGNORECASE
)
_GO_TO_RE = re.compile(r"\bGO\s+TO\b", re.IGNORECASE)
_GO_TO_TARGET_RE = re.compile(
    r"\bGO\s+TO\s+([A-Za-z0-9][A-Za-z0-9_-]*)",
    re.IGNORECASE,
)
_STOP_RUN_RE = re.compile(r"\bSTOP\s+RUN\b", re.IGNORECASE)
_GOBACK_RE = re.compile(r"\bGOBACK\b", re.IGNORECASE)
_EXIT_RE = re.compile(r"^\s*EXIT\b", re.IGNORECASE)
_CONTINUE_RE = re.compile(
    r"^\s*CONTINUE\b", re.IGNORECASE
)
_ALTER_RE = re.compile(r"^\s*ALTER\b", re.IGNORECASE)

_STMT_VERB_RE = re.compile(
    r"^\s*(?P<verb>MOVE|COMPUTE|ADD|SUBTRACT|MULTIPLY"
    r"|DIVIDE|SET|INITIALIZE|STRING|UNSTRING|INSPECT"
    r"|ACCEPT|DISPLAY|READ|WRITE|REWRITE|OPEN|CLOSE"
    r"|SORT|SEARCH)\b",
    re.IGNORECASE,
)

_PARA_HEADER_RE = re.compile(
    r"^[A-Za-z0-9][A-Za-z0-9_-]*\s*\.?\s*$"
)

_STMT_START_RE = re.compile(
    r"^\s*(?:MOVE|COMPUTE|ADD|SUBTRACT|MULTIPLY|DIVIDE"
    r"|SET|INITIALIZE|STRING|UNSTRING|INSPECT"
    r"|ACCEPT|DISPLAY|READ|WRITE|REWRITE|OPEN|CLOSE"
    r"|SORT|SEARCH|IF|ELSE|EVALUATE|WHEN|PERFORM|CALL"
    r"|GOBACK|STOP|GO|EXIT|ALTER|CONTINUE"
    r"|END-IF|END-EVALUATE|END-PERFORM|NOT)\b",
    re.IGNORECASE,
)

_IF_STRIP_RE = re.compile(r"^\s*IF\s+", re.IGNORECASE)
_THEN_STRIP_RE = re.compile(
    r"\s+THEN\s*$", re.IGNORECASE
)
_EVALUATE_STRIP_RE = re.compile(
    r"^\s*EVALUATE\s+", re.IGNORECASE
)
_WHEN_STRIP_RE = re.compile(
    r"^\s*WHEN\s+", re.IGNORECASE
)
_PERFORM_UNTIL_STRIP_RE = re.compile(
    r"^\s*PERFORM\s+UNTIL\s+", re.IGNORECASE
)
_PERFORM_STRIP_RE = re.compile(
    r"^\s*PERFORM\s+", re.IGNORECASE
)


# ---------------------------------------------------------------------------
# Data classes
# ---------------------------------------------------------------------------

@dataclass
class SyntaxNode:
    """A node in the COBOL paragraph syntax tree."""

    node_type: StatementType
    source_text: str
    line_start: int
    line_end: int
    children: list[SyntaxNode] = field(default_factory=list)
    attributes: dict[str, Any] = field(default_factory=dict)

    def walk(self) -> list[SyntaxNode]:
        """Return all nodes via pre-order traversal."""
        result = [self]
        for child in self.children:
            result.extend(child.walk())
        return result

    def format_tree(self, indent: int = 0) -> str:
        """Format this node and its children as an indented tree string."""
        lines: list[str] = []
        self._format_into(lines, "", True, indent == 0)
        return "\n".join(lines)

    def _format_into(
        self,
        lines: list[str],
        prefix: str,
        is_last: bool,
        is_root: bool,
    ) -> None:
        connector = "" if is_root else ("└── " if is_last else "├── ")
        label = self.node_type.value
        text = self.source_text
        if len(text) > 72:
            text = text[:69] + "..."
        if text:
            line = f"{prefix}{connector}{label}: {text}"
        else:
            line = f"{prefix}{connector}{label}"
        lines.append(line)
        child_prefix = (
            prefix if is_root
            else prefix + ("    " if is_last else "│   ")
        )
        for i, child in enumerate(self.children):
            child._format_into(
                lines, child_prefix,
                i == len(self.children) - 1, False,
            )


@dataclass
class ParagraphSyntaxTree:
    """Syntax tree for a single COBOL paragraph."""

    paragraph_name: str
    root: SyntaxNode
    statement_count: int
    max_nesting_depth: int

    def walk(self) -> list[SyntaxNode]:
        """Pre-order traversal of all nodes."""
        return self.root.walk()

    def find(
        self, node_type: StatementType
    ) -> list[SyntaxNode]:
        """Find all nodes of a given type."""
        return [
            n for n in self.walk()
            if n.node_type == node_type
        ]

    def __str__(self) -> str:
        header = (
            f"{self.paragraph_name}"
            f"  ({self.statement_count} statements,"
            f" depth={self.max_nesting_depth})"
        )
        body = self.root.format_tree()
        return f"{header}\n{body}"


# ---------------------------------------------------------------------------
# Statement assembly
# ---------------------------------------------------------------------------

@dataclass
class _RawStatement:
    """An assembled multi-line COBOL statement."""

    text: str
    line_start: int
    line_end: int


def _assemble_statements(
    lines: list[SourceLine],
) -> list[_RawStatement]:
    """Assemble SourceLines into complete COBOL statements."""
    code_lines = [
        ln for ln in lines
        if not ln.is_comment and not ln.is_blank
    ]
    if not code_lines:
        return []

    statements: list[_RawStatement] = []
    current_text = ""
    current_start = 0
    current_end = 0
    in_exec = False

    for line in code_lines:
        text = line.text.strip()
        if not text:
            continue

        # Skip paragraph header lines (Area A, name only)
        if (
            line.area_a.strip()
            and _PARA_HEADER_RE.match(text.rstrip("."))
        ):
            continue

        # EXEC blocks: accumulate until END-EXEC
        if _EXEC_START_RE.search(text) and not in_exec:
            if current_text:
                statements.append(_RawStatement(
                    current_text, current_start, current_end,
                ))
                current_text = ""
            in_exec = True
            current_text = text
            current_start = line.line_number
            current_end = line.line_number
            if _EXEC_END_RE.search(text):
                in_exec = False
                statements.append(_RawStatement(
                    current_text, current_start, current_end,
                ))
                current_text = ""
            continue

        if in_exec:
            current_text += " " + text
            current_end = line.line_number
            if _EXEC_END_RE.search(text):
                in_exec = False
                statements.append(_RawStatement(
                    current_text, current_start, current_end,
                ))
                current_text = ""
            continue

        # Check if line starts a new statement
        starts_new = bool(_STMT_START_RE.match(text))

        if starts_new:
            if current_text:
                statements.append(_RawStatement(
                    current_text, current_start, current_end,
                ))
            current_text = text
            current_start = line.line_number
            current_end = line.line_number
        else:
            if current_text:
                current_text += " " + text
                current_end = line.line_number
            else:
                current_text = text
                current_start = line.line_number
                current_end = line.line_number

        # Period terminates a statement
        if current_text.rstrip().endswith("."):
            statements.append(_RawStatement(
                current_text, current_start, current_end,
            ))
            current_text = ""

    if current_text:
        statements.append(_RawStatement(
            current_text, current_start, current_end,
        ))

    return statements


# ---------------------------------------------------------------------------
# Statement classification
# ---------------------------------------------------------------------------

def _classify(
    text: str,
) -> tuple[StatementType, dict[str, Any]]:
    """Classify a statement and extract key attributes."""
    stripped = text.strip()
    attrs: dict[str, Any] = {}

    # --- END markers (signals, not real nodes) ---
    if _END_IF_RE.match(stripped):
        return StatementType.UNKNOWN, {"_end": "IF"}
    if _END_EVALUATE_RE.match(stripped):
        return StatementType.UNKNOWN, {"_end": "EVALUATE"}
    if _END_PERFORM_RE.match(stripped):
        return StatementType.UNKNOWN, {"_end": "PERFORM"}

    # --- Control flow ---
    if _IF_START_RE.match(stripped):
        cond = _IF_STRIP_RE.sub("", stripped)
        cond = _THEN_STRIP_RE.sub("", cond).rstrip(".")
        attrs["condition"] = cond
        return StatementType.IF, attrs

    if _ELSE_RE.match(stripped):
        return StatementType.ELSE, attrs

    if _EVALUATE_START_RE.match(stripped):
        subject = _EVALUATE_STRIP_RE.sub(
            "", stripped
        ).rstrip(".")
        attrs["subject"] = subject
        return StatementType.EVALUATE, attrs

    if _WHEN_RE.match(stripped):
        value = _WHEN_STRIP_RE.sub(
            "", stripped
        ).rstrip(".")
        attrs["value"] = value
        return StatementType.WHEN, attrs

    # --- PERFORM variants (order matters) ---
    if _PERFORM_UNTIL_INLINE_RE.match(stripped):
        cond = _PERFORM_UNTIL_STRIP_RE.sub(
            "", stripped
        ).rstrip(".")
        attrs["condition"] = cond
        return StatementType.PERFORM_INLINE, attrs

    if _PERFORM_VARYING_INLINE_RE.match(stripped):
        body = _PERFORM_STRIP_RE.sub(
            "", stripped
        ).rstrip(".")
        attrs["varying"] = body
        return StatementType.PERFORM_INLINE, attrs

    m = _PERFORM_THRU_RE.match(stripped)
    if m:
        attrs["target"] = m.group(1).upper()
        attrs["thru"] = m.group(2).upper()
        return StatementType.PERFORM_THRU, attrs

    m = _PERFORM_UNTIL_NAMED_RE.match(stripped)
    if m:
        attrs["target"] = m.group(1).upper()
        cond = stripped[m.end():].strip().rstrip(".")
        attrs["condition"] = cond
        return StatementType.PERFORM_UNTIL, attrs

    m = _PERFORM_TIMES_RE.match(stripped)
    if m:
        attrs["target"] = m.group(1).upper()
        attrs["times"] = int(m.group(2))
        return StatementType.PERFORM, attrs

    m = _PERFORM_SIMPLE_RE.match(stripped)
    if m:
        attrs["target"] = m.group(1).upper()
        return StatementType.PERFORM, attrs

    # --- EXEC blocks ---
    exec_match = _EXEC_START_RE.search(stripped)
    if exec_match:
        exec_type = exec_match.group(1).upper()
        attrs["raw_text"] = stripped
        type_map = {
            "SQL": StatementType.EXEC_SQL,
            "CICS": StatementType.EXEC_CICS,
            "DLI": StatementType.EXEC_DLI,
        }
        return type_map.get(
            exec_type, StatementType.EXEC_OTHER
        ), attrs

    # --- Data operations (MOVE/COMPUTE with detail) ---
    m = _MOVE_RE.search(stripped)
    if m:
        attrs["source"] = m.group(1).strip().rstrip(".")
        attrs["targets"] = m.group(2).strip().rstrip(".")
        return StatementType.MOVE, attrs

    m = _COMPUTE_RE.search(stripped)
    if m:
        attrs["target"] = m.group(1).upper()
        attrs["expression"] = (
            m.group(2).strip().rstrip(".")
        )
        return StatementType.COMPUTE, attrs

    # --- Verb-based classification ---
    verb_match = _STMT_VERB_RE.match(stripped)
    if verb_match:
        verb = verb_match.group("verb").upper()
        verb_type_map = {
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
        }
        return verb_type_map.get(
            verb, StatementType.UNKNOWN
        ), attrs

    # --- Flow control ---
    if _CALL_RE.search(stripped):
        m = _CALL_TARGET_RE.search(stripped)
        if m:
            attrs["target"] = m.group(1).upper()
        return StatementType.CALL, attrs

    if _GOBACK_RE.search(stripped):
        return StatementType.GOBACK, attrs

    if _STOP_RUN_RE.search(stripped):
        return StatementType.STOP_RUN, attrs

    if _GO_TO_RE.search(stripped):
        m = _GO_TO_TARGET_RE.search(stripped)
        if m:
            attrs["target"] = m.group(1).upper()
        return StatementType.GO_TO, attrs

    if _EXIT_RE.match(stripped):
        return StatementType.EXIT, attrs

    if _CONTINUE_RE.match(stripped):
        return StatementType.CONTINUE_STMT, attrs

    if _ALTER_RE.match(stripped):
        return StatementType.ALTER, attrs

    return StatementType.UNKNOWN, attrs


# ---------------------------------------------------------------------------
# Tree builder
# ---------------------------------------------------------------------------

_CONTAINER_TYPES = frozenset({
    StatementType.IF,
    StatementType.ELSE,
    StatementType.EVALUATE,
    StatementType.WHEN,
    StatementType.PERFORM_INLINE,
})

_END_TYPE_MAP = {
    "IF": StatementType.IF,
    "EVALUATE": StatementType.EVALUATE,
    "PERFORM": StatementType.PERFORM_INLINE,
}


def _pop_to_end(
    stack: list[SyntaxNode], end_target: str
) -> None:
    """Pop the stack back past the matching container."""
    target = _END_TYPE_MAP.get(end_target)
    if target is None:
        return
    while len(stack) > 1:
        if stack[-1].node_type == target:
            stack.pop()
            return
        # Also pop through ELSE/WHEN which are children
        # of the target container
        stack.pop()


def _pop_to_type(
    stack: list[SyntaxNode],
    target: StatementType,
) -> None:
    """Pop back to (but not past) a node of the given type."""
    while len(stack) > 1:
        if stack[-1].node_type == target:
            return
        stack.pop()


def _build_tree(
    paragraph_name: str,
    raw_stmts: list[_RawStatement],
) -> ParagraphSyntaxTree:
    """Build a syntax tree from assembled statements."""
    line_start = raw_stmts[0].line_start if raw_stmts else 0
    line_end = raw_stmts[-1].line_end if raw_stmts else 0

    root = SyntaxNode(
        node_type=StatementType.PARAGRAPH,
        source_text="",
        line_start=line_start,
        line_end=line_end,
        attributes={"name": paragraph_name},
    )

    stack: list[SyntaxNode] = [root]
    statement_count = 0
    max_depth = 0

    for stmt in raw_stmts:
        stmt_type, attrs = _classify(stmt.text)

        # END-* markers: pop back past the matching opener
        if "_end" in attrs:
            _pop_to_end(stack, attrs["_end"])
            continue

        node = SyntaxNode(
            node_type=stmt_type,
            source_text=stmt.text.rstrip(".").strip(),
            line_start=stmt.line_start,
            line_end=stmt.line_end,
            attributes={
                k: v for k, v in attrs.items()
                if not k.startswith("_")
            },
        )
        statement_count += 1

        if stmt_type == StatementType.ELSE:
            # Pop back to the IF, add ELSE as its child
            _pop_to_type(stack, StatementType.IF)
            stack[-1].children.append(node)
            stack.append(node)

        elif stmt_type == StatementType.WHEN:
            # Pop any previous WHEN, stay inside EVALUATE
            if (
                len(stack) > 1
                and stack[-1].node_type
                == StatementType.WHEN
            ):
                stack.pop()
            stack[-1].children.append(node)
            stack.append(node)

        elif stmt_type in _CONTAINER_TYPES:
            stack[-1].children.append(node)
            stack.append(node)
            depth = len(stack) - 1
            if depth > max_depth:
                max_depth = depth

        else:
            # Leaf node
            stack[-1].children.append(node)

    return ParagraphSyntaxTree(
        paragraph_name=paragraph_name,
        root=root,
        statement_count=statement_count,
        max_nesting_depth=max_depth,
    )


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def build_paragraph_ast(
    paragraph_name: str,
    lines: list[SourceLine],
    data_items: list | None = None,
) -> ParagraphSyntaxTree:
    """Build a syntax tree for a single COBOL paragraph.

    Args:
        paragraph_name: Name of the paragraph.
        lines: SourceLines belonging to this paragraph.
        data_items: Optional DataItem list (reserved for future
            variable annotation on nodes).

    Returns:
        ParagraphSyntaxTree with nested statement nodes.
    """
    raw_stmts = _assemble_statements(lines)
    return _build_tree(paragraph_name, raw_stmts)
