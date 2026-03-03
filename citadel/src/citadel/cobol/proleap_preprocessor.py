"""Preprocessor for COBOL source files before ProLeap parsing.

ProLeap v4.0.0 has a fixed ANTLR grammar that does not support certain
DB2/SQL precompiler directives found in real-world mainframe COBOL.
This module rewrites problematic constructs so ProLeap can parse them.

Handled constructs:
- ``SQL TYPE IS <unsupported-type>`` — replaced with ``PIC X(4).``
- ``EXEC SQL INCLUDE <member> END-EXEC`` — commented out (also fixes
  continuation-split ``LUDE`` errors)

Line count is preserved by turning replaced lines into COBOL comment
lines (column 7 = ``*``), so AST line numbers stay valid.
"""

from __future__ import annotations

import logging
import re
import tempfile
from dataclasses import dataclass, field
from pathlib import Path

from citadel.cobol.source_reader import (
    COL_AREA_B_END,
    COL_INDICATOR,
    COMMENT_INDICATORS,
    CONTINUATION_INDICATOR,
)

logger = logging.getLogger(__name__)

# SQL TYPE IS variants that ProLeap's grammar already handles.
_KNOWN_SQL_TYPES = frozenset({
    "BLOB",
    "CLOB",
    "DBCLOB",
    "LONG-DATE",
    "LONG_DATE",
    "LONG-TIME",
    "LONG_TIME",
    "NUMERIC-DATE",
    "NUMERIC_DATE",
    "NUMERIC-TIME",
    "NUMERIC_TIME",
    "SHORT-DATE",
    "SHORT_DATE",
})


# ---------------------------------------------------------------------------
# Data classes
# ---------------------------------------------------------------------------


@dataclass
class LogicalLine:
    """A logical COBOL line that may span multiple physical lines."""

    physical_indices: list[int]
    text: str
    is_comment: bool = False


@dataclass
class PreprocessResult:
    """Result of preprocessing a COBOL source file."""

    output_path: Path
    original_path: Path
    transformations: list[str] = field(default_factory=list)
    line_count: int = 0


# ---------------------------------------------------------------------------
# Logical line builder
# ---------------------------------------------------------------------------

# Regex matching SQL TYPE IS on logical text:
#   <level> <name> SQL TYPE IS <type> [.<optional-period>]
_SQL_TYPE_IS_RE = re.compile(
    r"^\s*(?P<level>\d{2})\s+(?P<name>[A-Za-z0-9_-]+)\s+"
    r"SQL\s+TYPE\s+IS\s+(?P<type>[A-Za-z0-9_-]+)",
    re.IGNORECASE,
)

# Regex matching EXEC SQL INCLUDE on logical text
_EXEC_SQL_INCLUDE_RE = re.compile(
    r"^\s*EXEC\s+SQL\s+INCLUDE\s+(?P<member>[A-Za-z0-9_-]+)\s+END-EXEC",
    re.IGNORECASE,
)


def _build_logical_lines(lines: list[str]) -> list[LogicalLine]:
    """Group physical lines into logical lines using continuation markers.

    A continuation line (column 7 = ``-``) is appended to the preceding
    non-comment logical line.  Comment lines are kept as their own
    logical line.
    """
    logical: list[LogicalLine] = []
    current_indices: list[int] = []
    current_text = ""

    for i, raw in enumerate(lines):
        padded = raw.ljust(COL_AREA_B_END)
        indicator = padded[COL_INDICATOR] if len(padded) > COL_INDICATOR else " "

        if indicator in COMMENT_INDICATORS:
            # Flush any pending logical line
            if current_indices:
                logical.append(LogicalLine(list(current_indices), current_text))
                current_indices = []
                current_text = ""
            # Comment is its own logical line
            logical.append(LogicalLine(
                [i],
                padded[COL_INDICATOR + 1:COL_AREA_B_END].rstrip(),
                is_comment=True,
            ))
            continue

        if indicator == CONTINUATION_INDICATOR:
            # Append to current logical line
            if current_indices:
                current_indices.append(i)
                cont_text = padded[COL_INDICATOR + 1:COL_AREA_B_END].lstrip()
                current_text = current_text.rstrip() + cont_text.rstrip()
            else:
                # Orphan continuation — treat as new logical line
                current_indices = [i]
                current_text = padded[COL_INDICATOR + 1:COL_AREA_B_END].rstrip()
            continue

        # Normal line — flush previous and start new
        if current_indices:
            logical.append(LogicalLine(list(current_indices), current_text))
        current_indices = [i]
        current_text = padded[COL_INDICATOR + 1:COL_AREA_B_END].rstrip()

    # Flush remaining
    if current_indices:
        logical.append(LogicalLine(list(current_indices), current_text))

    return logical


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _make_comment_line(original_line: str) -> str:
    """Turn a line into a COBOL comment by setting column 7 to ``*``."""
    padded = original_line.ljust(COL_INDICATOR + 1)
    return padded[:COL_INDICATOR] + "*" + padded[COL_INDICATOR + 1:]


def _make_data_line(level: str, name: str, pic: str) -> str:
    """Build a fixed-format COBOL data definition line.

    Places level in Area A (cols 8-11), name and PIC in Area B (cols 12-72).
    """
    # Cols 1-6: spaces, Col 7: space, Cols 8-11: level + spaces, Col 12+: name PIC ...
    area_a = level.ljust(4)
    area_b = f"{name} {pic}"
    return f"      {' '}{area_a}{area_b}"


# ---------------------------------------------------------------------------
# Transforms
# ---------------------------------------------------------------------------


def _transform_sql_type_is(
    lines: list[str],
    logical_lines: list[LogicalLine],
) -> tuple[list[str], list[str]]:
    """Replace unsupported ``SQL TYPE IS`` declarations with ``PIC X(4).``"""
    result = list(lines)
    descriptions: list[str] = []

    for ll in logical_lines:
        if ll.is_comment:
            continue
        m = _SQL_TYPE_IS_RE.match(ll.text)
        if m is None:
            continue

        sql_type = m.group("type").upper().replace("-", "_")
        if sql_type in {t.replace("-", "_") for t in _KNOWN_SQL_TYPES}:
            continue

        level = m.group("level")
        name = m.group("name")

        # Replace first physical line with PIC X(4). definition
        first_idx = ll.physical_indices[0]
        result[first_idx] = _make_data_line(level, name, "PIC X(4).")

        # Comment out remaining continuation lines
        for idx in ll.physical_indices[1:]:
            result[idx] = _make_comment_line(lines[idx])

        descriptions.append(
            f"SQL TYPE IS {m.group('type')}: {level} {name} -> PIC X(4)"
        )

    return result, descriptions


def _transform_exec_sql_include(
    lines: list[str],
    logical_lines: list[LogicalLine],
) -> tuple[list[str], list[str]]:
    """Comment out ``EXEC SQL INCLUDE`` statements."""
    result = list(lines)
    descriptions: list[str] = []

    for ll in logical_lines:
        if ll.is_comment:
            continue
        m = _EXEC_SQL_INCLUDE_RE.match(ll.text)
        if m is None:
            continue

        member = m.group("member")

        for idx in ll.physical_indices:
            result[idx] = _make_comment_line(lines[idx])

        descriptions.append(f"EXEC SQL INCLUDE {member}: commented out")

    return result, descriptions


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


def preprocess_for_proleap(source_path: str | Path) -> PreprocessResult:
    """Read a COBOL source file, apply transforms, write temp file.

    Args:
        source_path: Path to the original COBOL source file.

    Returns:
        A ``PreprocessResult`` with the temp file path and applied
        transformations.
    """
    source_path = Path(source_path)
    with open(source_path, encoding="utf-8", errors="replace") as fh:
        lines = [line.rstrip("\n").rstrip("\r") for line in fh]

    logical_lines = _build_logical_lines(lines)
    all_transforms: list[str] = []

    # Apply transforms in order
    lines, descs = _transform_sql_type_is(lines, logical_lines)
    all_transforms.extend(descs)

    if descs:
        # Rebuild logical lines after SQL TYPE IS changes
        logical_lines = _build_logical_lines(lines)

    lines, descs = _transform_exec_sql_include(lines, logical_lines)
    all_transforms.extend(descs)

    if all_transforms:
        logger.info(
            "ProLeap preprocessor applied %d transform(s) to %s: %s",
            len(all_transforms),
            source_path.name,
            "; ".join(all_transforms),
        )

    # Write to temp file (same suffix so ProLeap recognises it)
    suffix = source_path.suffix or ".cbl"
    tmp = tempfile.NamedTemporaryFile(
        mode="w",
        suffix=suffix,
        prefix="proleap_pp_",
        delete=False,
        encoding="utf-8",
    )
    try:
        tmp.write("\n".join(lines))
        if lines:
            tmp.write("\n")
    finally:
        tmp.close()

    return PreprocessResult(
        output_path=Path(tmp.name),
        original_path=source_path,
        transformations=all_transforms,
        line_count=len(lines),
    )
