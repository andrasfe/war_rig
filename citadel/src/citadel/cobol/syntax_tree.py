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
    START_STMT = "START"

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

    # Copybook references (placeholders left behind by source_reader
    # when a COPY statement has been resolved — the actual copybook
    # body is excluded via ``copybook_source``).
    COPY_STMT = "COPY"

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
_END_START_RE = re.compile(
    r"^\s*END-START\b", re.IGNORECASE
)
_END_SEARCH_RE = re.compile(
    r"^\s*END-SEARCH\b", re.IGNORECASE
)

_IF_START_RE = re.compile(r"^\s*IF\b", re.IGNORECASE)
_ELSE_RE = re.compile(r"^\s*ELSE\b", re.IGNORECASE)
_EVALUATE_START_RE = re.compile(
    r"^\s*EVALUATE\b", re.IGNORECASE
)
_WHEN_RE = re.compile(r"^\s*WHEN\b", re.IGNORECASE)
_START_RE = re.compile(r"^\s*START\b", re.IGNORECASE)

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
    r"\bCOMPUTE\s+(?P<target>[A-Za-z][A-Za-z0-9_-]*)"
    r"\s+(?:ROUNDED\s+)?(?:=|EQUAL(?:\s+TO)?)\s*(?P<expr>.+)",
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
    r"|SORT|SEARCH)(?=\s|\.|$)",
    re.IGNORECASE,
)

_PARA_HEADER_RE = re.compile(
    r"^[A-Za-z0-9][A-Za-z0-9_-]*\s*\.?\s*$"
)
_PARA_HEADER_TRAILING_RE = re.compile(
    r"^[A-Za-z0-9][A-Za-z0-9_-]*\s*\.\s+\S"
)

_STMT_START_RE = re.compile(
    r"^\s*(?:MOVE|COMPUTE|ADD|SUBTRACT|MULTIPLY|DIVIDE"
    r"|SET|INITIALIZE|STRING|UNSTRING|INSPECT"
    r"|ACCEPT|DISPLAY|READ|WRITE|REWRITE|OPEN|CLOSE"
    r"|SORT|SEARCH|START|IF|ELSE|EVALUATE|WHEN|PERFORM|CALL"
    r"|GOBACK|STOP|GO|EXIT|ALTER|CONTINUE|COPY"
    r"|END-IF|END-EVALUATE|END-PERFORM|END-START|END-SEARCH)\b"
    r"(?=\s|\.|$)",
    re.IGNORECASE,
)

_COPY_STMT_RE = re.compile(
    r"^\s*COPY\s+['\"]?(?P<name>[A-Za-z0-9_-]+)['\"]?",
    re.IGNORECASE,
)

_EMBEDDED_PERFORM_RE = re.compile(
    r"(?<!END-)\bPERFORM\b", re.IGNORECASE
)
_EMBEDDED_IF_RE = re.compile(
    r"\bIF\s*\(", re.IGNORECASE
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
_IF_INLINE_SPLIT_RE = re.compile(
    r"^\s*IF\s+(?P<cond>.+?)\s+"
    r"(?P<rest>(?:THEN\s+)?(?:MOVE|COMPUTE|ADD|SUBTRACT|MULTIPLY|DIVIDE"
    r"|SET|INITIALIZE|STRING|UNSTRING|INSPECT|ACCEPT|DISPLAY|READ|WRITE"
    r"|REWRITE|OPEN|CLOSE|SORT|SEARCH|START|PERFORM|CALL|GOBACK|STOP|GO"
    r"|EXIT|ALTER|CONTINUE)(?=\s|\.|$).*)$",
    re.IGNORECASE,
)
_IF_THEN_IF_SPLIT_RE = re.compile(
    r"^\s*IF\s+(?P<outer>.+?)\s+THEN\s+(?P<inner>IF\b.+)$",
    re.IGNORECASE,
)
_ELSE_IF_SPLIT_RE = re.compile(
    r"^\s*ELSE\s+(?P<inner>IF\b.+)$",
    re.IGNORECASE,
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
    period_terminated: bool


def _assemble_statements(
    lines: list[SourceLine],
    preserve_newlines: bool = False,
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

        # Skip paragraph header lines (Area A, name only). If the header
        # shares a line with a statement (e.g., "PARA. EXIT."), keep the
        # trailing statement text.
        if line.area_a.strip():
            if _PARA_HEADER_TRAILING_RE.match(text):
                parts = text.split(".", 1)
                text = parts[1].strip() if len(parts) > 1 else ""
                if not text:
                    continue
            elif _PARA_HEADER_RE.match(text.rstrip(".")):
                continue

        # EXEC blocks: accumulate until END-EXEC
        if _EXEC_START_RE.search(text) and not in_exec:
            if current_text:
                statements.append(_RawStatement(
                    current_text, current_start, current_end,
                    current_text.rstrip().endswith("."),
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
                    current_text.rstrip().endswith("."),
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
                    current_text.rstrip().endswith("."),
                ))
                current_text = ""
            continue

        # Check if line starts a new statement
        starts_new = bool(_STMT_START_RE.match(text))

        sep = "\n" if preserve_newlines else " "

        if starts_new:
            if current_text:
                statements.append(_RawStatement(
                    current_text, current_start, current_end,
                    current_text.rstrip().endswith("."),
                ))
            current_text = text
            current_start = line.line_number
            current_end = line.line_number
        else:
            if current_text:
                current_text += sep + text
                current_end = line.line_number
            else:
                current_text = text
                current_start = line.line_number
                current_end = line.line_number

        # Period terminates a statement
        if current_text.rstrip().endswith("."):
            statements.append(_RawStatement(
                current_text, current_start, current_end,
                current_text.rstrip().endswith("."),
            ))
            current_text = ""

    if current_text:
        statements.append(_RawStatement(
            current_text, current_start, current_end,
            current_text.rstrip().endswith("."),
        ))

    return _split_embedded_performs(statements)


def _split_embedded_performs(
    statements: list[_RawStatement],
) -> list[_RawStatement]:
    """Split statements that embed a PERFORM after another verb."""
    result: list[_RawStatement] = []

    for stmt in statements:
        text = stmt.text
        if (
            _IF_START_RE.match(text)
            or _EVALUATE_START_RE.match(text)
            or _WHEN_RE.match(text)
        ):
            result.append(stmt)
            continue

        parts: list[_RawStatement] = []
        working = text
        while True:
            perform_match = _EMBEDDED_PERFORM_RE.search(working)
            if_match = _EMBEDDED_IF_RE.search(working)

            candidates = [
                match
                for match in (perform_match, if_match)
                if match and match.start() > 0
            ]
            if not candidates:
                break

            match = min(candidates, key=lambda m: m.start())
            before = working[:match.start()].rstrip()
            after = working[match.start():].lstrip()
            if before:
                parts.append(_RawStatement(
                    before,
                    stmt.line_start,
                    stmt.line_end,
                    stmt.period_terminated,
                ))
            working = after

        if parts:
            if working:
                parts.append(_RawStatement(
                    working,
                    stmt.line_start,
                    stmt.line_end,
                    stmt.period_terminated,
                ))
            result.extend(parts)
        else:
            result.append(stmt)

    return _split_end_marker_tails(_split_inline_if_statements(result))


_END_MARKER_TAIL_RE = re.compile(
    r"^(?P<end>END-(?:IF|EVALUATE|PERFORM|START|SEARCH))\b\s+"
    r"(?P<tail>\S.*)$",
    re.IGNORECASE,
)


def _split_end_marker_tails(
    statements: list[_RawStatement],
) -> list[_RawStatement]:
    """Split ``END-IF EXIT`` style statements into two raw statements.

    COBOL allows multiple statements on a single line, so
    ``END-IF EXIT .`` or ``END-EVALUATE MOVE X TO Y .`` are legal.
    The raw statement collector groups the whole line into one chunk
    because it starts with a verb (END-IF is in ``_STMT_START_RE``).
    The classifier then matches the END marker and returns early,
    silently dropping whatever follows.

    This pass walks the statement list and splits any statement whose
    text starts with ``END-<KW>`` followed by a non-empty tail into
    two statements: the bare END marker, and the tail (which then
    goes through classification itself).
    """
    result: list[_RawStatement] = []
    for stmt in statements:
        text = stmt.text.strip()
        # Strip a trailing standalone period so the regex can see the
        # true tail, then reattach it to the tail statement.
        period_tail = ""
        if text.endswith("."):
            text = text[:-1].rstrip()
            period_tail = "."
        m = _END_MARKER_TAIL_RE.match(text)
        if not m:
            # Restore period if we stripped one; we didn't split.
            result.append(stmt)
            continue

        end_text = m.group("end").upper()
        tail_text = m.group("tail").strip() + period_tail
        result.append(_RawStatement(
            text=end_text,
            line_start=stmt.line_start,
            line_end=stmt.line_end,
            period_terminated=False,
        ))
        result.append(_RawStatement(
            text=tail_text,
            line_start=stmt.line_start,
            line_end=stmt.line_end,
            period_terminated=stmt.period_terminated,
        ))
    return result


def _split_inline_if_statements(
    statements: list[_RawStatement],
) -> list[_RawStatement]:
    """Split one-line IF statements that contain inline actions."""
    result: list[_RawStatement] = []

    for idx, stmt in enumerate(statements):
        text = stmt.text.strip()

        else_if_match = _ELSE_IF_SPLIT_RE.match(text)
        if else_if_match:
            result.append(_RawStatement(
                text="ELSE",
                line_start=stmt.line_start,
                line_end=stmt.line_end,
                period_terminated=False,
            ))
            result.append(_RawStatement(
                text=else_if_match.group("inner").strip(),
                line_start=stmt.line_start,
                line_end=stmt.line_end,
                period_terminated=stmt.period_terminated,
            ))
            continue

        then_if_match = _IF_THEN_IF_SPLIT_RE.match(text)
        if then_if_match:
            outer_if = f"IF {then_if_match.group('outer').strip()}"
            inner_if = then_if_match.group("inner").strip()
            result.append(_RawStatement(
                text=outer_if,
                line_start=stmt.line_start,
                line_end=stmt.line_end,
                period_terminated=False,
            ))
            result.append(_RawStatement(
                text=inner_if,
                line_start=stmt.line_start,
                line_end=stmt.line_end,
                period_terminated=stmt.period_terminated,
            ))
            continue

        match = _IF_INLINE_SPLIT_RE.match(text)
        if not match:
            result.append(stmt)
            continue

        cond = match.group("cond").strip()
        rest = match.group("rest").strip()
        rest = re.sub(r"^THEN\s+", "", rest, flags=re.IGNORECASE)
        rest_is_if = bool(_IF_START_RE.match(rest))
        has_then = bool(re.search(r"\bTHEN\b", text, re.IGNORECASE))
        next_is_else = (
            idx + 1 < len(statements)
            and bool(_ELSE_RE.match(statements[idx + 1].text.strip()))
        )

        if_stmt = f"IF {cond}"
        end_marker = _RawStatement(
            text="END-IF",
            line_start=stmt.line_start,
            line_end=stmt.line_end,
            period_terminated=False,
        )

        result.append(_RawStatement(
            text=if_stmt,
            line_start=stmt.line_start,
            line_end=stmt.line_end,
            period_terminated=False,
        ))
        result.append(_RawStatement(
            text=rest,
            line_start=stmt.line_start,
            line_end=stmt.line_end,
            period_terminated=stmt.period_terminated and (
                rest_is_if or next_is_else or has_then
            ),
        ))

        if not rest_is_if and not next_is_else and not has_then:
            if stmt.period_terminated:
                end_marker.period_terminated = True
            result.append(end_marker)

    return result


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
    if _END_START_RE.match(stripped):
        return StatementType.UNKNOWN, {"_end": "START"}
    if _END_SEARCH_RE.match(stripped):
        return StatementType.UNKNOWN, {"_end": "SEARCH"}

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

    if stripped.upper().startswith("SEARCH "):
        return StatementType.SEARCH, {"_container": "SEARCH"}

    if _START_RE.match(stripped):
        return StatementType.START_STMT, {"_container": "START"}

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
        attrs["target"] = m.group(1).upper().rstrip(".")
        attrs["thru"] = m.group(2).upper().rstrip(".")
        return StatementType.PERFORM_THRU, attrs

    m = _PERFORM_UNTIL_NAMED_RE.match(stripped)
    if m:
        attrs["target"] = m.group(1).upper().rstrip(".")
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

    # --- COPY placeholders (left behind by source_reader when the
    # COPY statement has been resolved; the inlined body itself is
    # filtered out via ``copybook_source`` in cobalt/generator.py).
    copy_match = _COPY_STMT_RE.match(stripped)
    if copy_match:
        attrs["target"] = copy_match.group("name").upper()
        return StatementType.COPY_STMT, attrs

    # --- Data operations (MOVE/COMPUTE with detail) ---
    # Use match() not search(): search() on "READ ... AT END MOVE 'Y' TO X
    # END-READ" falsely classifies the whole READ as a MOVE because of the
    # nested MOVE inside the AT END clause. match() anchors at the start of
    # the stripped statement, so only statements that actually begin with
    # MOVE/COMPUTE land here.
    m = _MOVE_RE.match(stripped)
    if m:
        attrs["source"] = m.group(1).strip().rstrip(".")
        attrs["targets"] = m.group(2).strip().rstrip(".")
        return StatementType.MOVE, attrs

    m = _COMPUTE_RE.match(stripped)
    if m:
        attrs["target"] = m.group("target").upper()
        attrs["expression"] = (
            m.group("expr").strip().rstrip(".")
        )
        return StatementType.COMPUTE, attrs

    if stripped.upper().startswith("COMPUTE ") and "=" in stripped:
        body = stripped[8:].rstrip(".")
        left, right = body.split("=", 1)
        left = left.strip()
        right = right.strip().rstrip(".")
        left_tokens = [tok for tok in left.split() if tok]
        target = ""
        for token in left_tokens:
            if token.upper() == "ROUNDED":
                continue
            target = token.rstrip(",")
            break
        if target:
            attrs["target"] = target.upper()
        attrs["expression"] = right
        if any(tok.upper() == "ROUNDED" for tok in left_tokens):
            attrs["rounded"] = True
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
    StatementType.SEARCH,
})

_END_TYPE_MAP = {
    "IF": StatementType.IF,
    "EVALUATE": StatementType.EVALUATE,
    "PERFORM": StatementType.PERFORM_INLINE,
    "SEARCH": StatementType.SEARCH,
}


def _pop_to_end(
    stack: list[SyntaxNode],
    container_tags: list[str | None],
    end_target: str,
) -> None:
    """Pop the stack back past the matching container."""
    target = _END_TYPE_MAP.get(end_target)
    if target is None:
        return
    while len(stack) > 1:
        if stack[-1].node_type == target:
            stack.pop()
            container_tags.pop()
            return
        # Also pop through ELSE/WHEN which are children
        # of the target container
        stack.pop()
        container_tags.pop()


def _pop_to_type(
    stack: list[SyntaxNode],
    container_tags: list[str | None],
    target: StatementType,
) -> None:
    """Pop back to (but not past) a node of the given type."""
    while len(stack) > 1:
        if stack[-1].node_type == target:
            return
        stack.pop()
        container_tags.pop()


def _pop_to_else_target(
    stack: list[SyntaxNode],
    container_tags: list[str | None],
) -> None:
    """Pop to ELSE owner: nearest unmatched IF, else nearest IF fallback."""
    target_index: int | None = None
    fallback_index: int | None = None

    for idx in range(len(stack) - 1, 0, -1):
        node = stack[idx]
        if node.node_type != StatementType.IF:
            continue
        if fallback_index is None:
            fallback_index = idx
        has_else_child = any(
            child.node_type == StatementType.ELSE
            for child in node.children
        )
        if not has_else_child:
            target_index = idx
            break

    if target_index is None:
        target_index = fallback_index
    if target_index is None:
        return

    while len(stack) - 1 > target_index:
        stack.pop()
        container_tags.pop()


def _pop_to_container_tag(
    stack: list[SyntaxNode],
    container_tags: list[str | None],
    tag: str,
) -> None:
    """Pop back past a custom container tag (e.g., START/END-START)."""
    while len(stack) > 1:
        if container_tags[-1] == tag:
            stack.pop()
            container_tags.pop()
            return
        stack.pop()
        container_tags.pop()


def _build_tree(
    paragraph_name: str,
    raw_stmts: list[_RawStatement],
    para_line_start: int,
    para_line_end: int,
) -> ParagraphSyntaxTree:
    """Build a syntax tree from assembled statements."""
    line_start = para_line_start or (raw_stmts[0].line_start if raw_stmts else 0)
    line_end = para_line_end or (raw_stmts[-1].line_end if raw_stmts else 0)

    root = SyntaxNode(
        node_type=StatementType.PARAGRAPH,
        source_text="",
        line_start=line_start,
        line_end=line_end,
        attributes={"name": paragraph_name},
    )

    stack: list[SyntaxNode] = [root]
    container_tags: list[str | None] = [None]
    statement_count = 0
    max_depth = 0

    for stmt in raw_stmts:
        stmt_type, attrs = _classify(stmt.text)

        # END-* markers: pop back past the matching opener
        if "_end" in attrs:
            if attrs["_end"] == "START":
                _pop_to_container_tag(
                    stack, container_tags, "START"
                )
            else:
                _pop_to_end(stack, container_tags, attrs["_end"])
            if stmt.period_terminated:
                stack = [root]
                container_tags = [None]
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
            _pop_to_else_target(stack, container_tags)
            stack[-1].children.append(node)
            stack.append(node)
            container_tags.append(None)

        elif stmt_type == StatementType.WHEN:
            # Pop any previous WHEN, stay inside EVALUATE
            if (
                len(stack) > 1
                and stack[-1].node_type
                == StatementType.WHEN
            ):
                stack.pop()
                container_tags.pop()
            stack[-1].children.append(node)
            stack.append(node)
            container_tags.append(None)

        elif (
            stmt_type in _CONTAINER_TYPES
            or attrs.get("_container") == "START"
        ):
            stack[-1].children.append(node)
            stack.append(node)
            container_tags.append(
                attrs.get("_container")
            )
            depth = len(stack) - 1
            if depth > max_depth:
                max_depth = depth

        else:
            # Leaf node
            stack[-1].children.append(node)

        if stmt.period_terminated:
            stack = [root]
            container_tags = [None]

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
    preserve_newlines: bool = False,
) -> ParagraphSyntaxTree:
    """Build a syntax tree for a single COBOL paragraph.

    Args:
        paragraph_name: Name of the paragraph.
        lines: SourceLines belonging to this paragraph.
        data_items: Optional DataItem list (reserved for future
            variable annotation on nodes).
        preserve_newlines: When True, use newlines instead of spaces
            to join continuation lines (EXEC blocks always use spaces).

    Returns:
        ParagraphSyntaxTree with nested statement nodes.
    """
    raw_stmts = _assemble_statements(lines, preserve_newlines)
    if lines:
        para_line_start = lines[0].line_number
        para_line_end = lines[-1].line_number
    else:
        para_line_start = 0
        para_line_end = 0
    return _build_tree(paragraph_name, raw_stmts, para_line_start, para_line_end)


def build_file_ast(
    paragraph_source_lines: dict[str, list[SourceLine]],
    data_items: list | None = None,
    preserve_newlines: bool = False,
) -> dict[str, ParagraphSyntaxTree]:
    """Build ASTs for all paragraphs in a file.

    Args:
        paragraph_source_lines: Mapping of uppercase paragraph name
            to the list of SourceLines belonging to that paragraph.
        data_items: Optional DataItem list (reserved for future
            variable annotation on nodes).
        preserve_newlines: When True, use newlines instead of spaces
            to join continuation lines (EXEC blocks always use spaces).

    Returns:
        Dict mapping uppercase paragraph name to its syntax tree.
    """
    trees: dict[str, ParagraphSyntaxTree] = {}
    for name, lines in paragraph_source_lines.items():
        trees[name] = build_paragraph_ast(
            name, lines, data_items, preserve_newlines,
        )
    return trees


@dataclass
class ASTValidationIssue:
    """A single AST validation finding."""

    paragraph: str
    node_type: str
    line: int
    issue: str
    source_text: str


def validate_file_ast(
    trees: dict[str, ParagraphSyntaxTree],
    source_lines: dict[str, list[SourceLine]] | None = None,
) -> list[ASTValidationIssue]:
    """Validate AST consistency after generation.

    Checks that each node's ``node_type`` is consistent with its
    ``source_text`` and that structural invariants hold.  Optionally
    cross-checks line numbers against the original source lines.

    Args:
        trees: Paragraph ASTs as returned by ``build_file_ast()``.
        source_lines: Optional mapping of paragraph name to its
            ``SourceLine`` list for line-level cross-checks.

    Returns:
        List of validation issues found (empty = clean).
    """
    issues: list[ASTValidationIssue] = []

    # Map node_type -> expected keyword(s) in source_text
    _keyword_map: dict[StatementType, tuple[str, ...]] = {
        StatementType.IF: ("IF",),
        StatementType.EVALUATE: ("EVALUATE",),
        StatementType.PERFORM: ("PERFORM",),
        StatementType.PERFORM_THRU: ("PERFORM",),
        StatementType.PERFORM_UNTIL: ("PERFORM",),
        StatementType.PERFORM_INLINE: ("PERFORM",),
        StatementType.MOVE: ("MOVE",),
        StatementType.COMPUTE: ("COMPUTE",),
        StatementType.ADD: ("ADD",),
        StatementType.SUBTRACT: ("SUBTRACT",),
        StatementType.MULTIPLY: ("MULTIPLY",),
        StatementType.DIVIDE: ("DIVIDE",),
        StatementType.CALL: ("CALL",),
        StatementType.GO_TO: ("GO",),
        StatementType.GOBACK: ("GOBACK",),
        StatementType.STOP_RUN: ("STOP",),
        StatementType.READ_STMT: ("READ",),
        StatementType.WRITE_STMT: ("WRITE",),
        StatementType.REWRITE: ("REWRITE",),
        StatementType.OPEN: ("OPEN",),
        StatementType.CLOSE: ("CLOSE",),
        StatementType.DISPLAY: ("DISPLAY",),
        StatementType.ACCEPT: ("ACCEPT",),
        StatementType.SET: ("SET",),
        StatementType.INITIALIZE: ("INITIALIZE",),
        StatementType.STRING_STMT: ("STRING",),
        StatementType.UNSTRING: ("UNSTRING",),
        StatementType.INSPECT: ("INSPECT",),
        StatementType.SORT: ("SORT",),
        StatementType.SEARCH: ("SEARCH",),
        StatementType.EXEC_SQL: ("EXEC",),
        StatementType.EXEC_CICS: ("EXEC",),
        StatementType.EXEC_DLI: ("EXEC",),
        StatementType.EXEC_OTHER: ("EXEC",),
        StatementType.EXIT: ("EXIT",),
        StatementType.ALTER: ("ALTER",),
    }

    # Build source line lookup for cross-checking
    line_lookup: dict[int, str] = {}
    if source_lines:
        for lines in source_lines.values():
            for sl in lines:
                line_lookup[sl.line_number] = sl.text

    for para_name, tree in trees.items():
        prev_end = tree.root.line_start

        for node in tree.walk():
            if node.node_type == StatementType.PARAGRAPH:
                continue

            text_upper = node.source_text.upper().strip()

            # Check 1: keyword present in source_text
            expected = _keyword_map.get(node.node_type)
            if (
                expected
                and node.node_type != StatementType.UNKNOWN
                and not any(kw in text_upper for kw in expected)
            ):
                issues.append(ASTValidationIssue(
                    paragraph=para_name,
                    node_type=node.node_type.value,
                    line=node.line_start,
                    issue=(
                        f"Expected keyword {expected} not found "
                        f"in source_text"
                    ),
                    source_text=node.source_text[:80],
                ))

            # Check 2: line_start <= line_end
            if node.line_start > node.line_end:
                issues.append(ASTValidationIssue(
                    paragraph=para_name,
                    node_type=node.node_type.value,
                    line=node.line_start,
                    issue=(
                        f"line_start ({node.line_start}) > "
                        f"line_end ({node.line_end})"
                    ),
                    source_text=node.source_text[:80],
                ))

            # Check 3: PERFORM/CALL target in attributes matches source
            target = node.attributes.get("target")
            if (
                target
                and node.node_type in (
                    StatementType.PERFORM,
                    StatementType.PERFORM_THRU,
                    StatementType.PERFORM_UNTIL,
                    StatementType.CALL,
                )
                and target.upper() not in text_upper
            ):
                    issues.append(ASTValidationIssue(
                        paragraph=para_name,
                        node_type=node.node_type.value,
                        line=node.line_start,
                        issue=(
                            f"Target '{target}' not found in "
                            f"source_text"
                        ),
                        source_text=node.source_text[:80],
                    ))

            # Check 4: cross-check against original source line
            if line_lookup and node.line_start in line_lookup:
                src_line = line_lookup[node.line_start].upper()
                if (
                    expected
                    and not any(kw in src_line for kw in expected)
                    and node.line_start == node.line_end
                ):
                        issues.append(ASTValidationIssue(
                            paragraph=para_name,
                            node_type=node.node_type.value,
                            line=node.line_start,
                            issue=(
                                f"Source line {node.line_start} "
                                f"doesn't contain expected keyword "
                                f"{expected}"
                            ),
                            source_text=line_lookup[node.line_start][:80],
                        ))

            # Check 5: line ordering within paragraph (non-container nodes)
            if (
                node.node_type not in _CONTAINER_TYPES
                and node.node_type != StatementType.ELSE
                and node.node_type != StatementType.WHEN
                and node.line_start < prev_end
                and node.children == []
            ):
                issues.append(ASTValidationIssue(
                    paragraph=para_name,
                    node_type=node.node_type.value,
                    line=node.line_start,
                    issue=(
                        f"Line {node.line_start} is before previous "
                        f"node's line_end {prev_end}"
                    ),
                    source_text=node.source_text[:80],
                ))

            if node.children == []:
                prev_end = node.line_end

    return issues


def format_file_ast(
    trees: dict[str, ParagraphSyntaxTree],
) -> str:
    """Format all paragraph trees into a single string.

    Each paragraph is separated by a blank line. The output is
    suitable for use as LLM context in place of raw source code.

    Args:
        trees: Dict of uppercase paragraph name to syntax tree,
            as returned by ``build_file_ast()``.

    Returns:
        Formatted multi-paragraph AST text.
    """
    parts: list[str] = []
    for tree in trees.values():
        parts.append(str(tree))
    return "\n\n".join(parts)
