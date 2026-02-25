"""COBOL source reader for IBM Enterprise COBOL fixed-format files.

Handles the foundational step of the parser pipeline:
1. Fixed-format column parsing (sequence, indicator, Area A/B, identification)
2. Comment and blank line detection
3. Continuation line merging
4. COPY statement resolution (with REPLACING support and nesting)
5. Division/section identification

Reference: IBM Enterprise COBOL Language Reference, fixed-format layout.

Adapted from cbexplore's rosetta.parse.source_reader.
"""

from __future__ import annotations

import logging
import re
from dataclasses import dataclass
from pathlib import Path

logger = logging.getLogger(__name__)

# ---------------------------------------------------------------------------
# Column constants (0-based indices for Python slicing)
# ---------------------------------------------------------------------------
COL_SEQ_START = 0       # Columns 1-6: sequence number area
COL_SEQ_END = 6
COL_INDICATOR = 6       # Column 7: indicator area
COL_AREA_A_START = 7    # Columns 8-11: Area A
COL_AREA_A_END = 11
COL_AREA_B_START = 11   # Columns 12-72: Area B
COL_AREA_B_END = 72
COL_IDENT_START = 72    # Columns 73-80: identification area

# Indicator characters
COMMENT_INDICATORS = {"*", "/"}
CONTINUATION_INDICATOR = "-"
DEBUG_INDICATORS = {"D", "d"}

# COPY statement pattern:
#   COPY copybook-name [REPLACING ==old== BY ==new== ...].
_COPY_RE = re.compile(
    r"^\s*COPY\s+"
    r"(?P<name>[A-Za-z0-9_-]+)"
    r"(?:\s*\.\s*$"           # simple: COPY name.
    r"|"
    r"\s+REPLACING\b"         # or: COPY name REPLACING ...
    r"(?P<replacing>.*?)"
    r"\.\s*$)",
    re.IGNORECASE | re.DOTALL,
)

# Pattern for REPLACING ==old== BY ==new== pairs
_REPLACING_PAIR_RE = re.compile(
    r"==\s*(?P<old>.*?)\s*==\s+BY\s+==\s*(?P<new>.*?)\s*==",
    re.IGNORECASE,
)

# Division header pattern
_DIVISION_RE = re.compile(
    r"^\s*(?P<name>IDENTIFICATION|ENVIRONMENT|DATA|PROCEDURE)"
    r"\s+DIVISION",
    re.IGNORECASE,
)

# DATA DIVISION sub-section pattern
_DATA_SECTION_RE = re.compile(
    r"^\s*(?P<name>FILE|WORKING-STORAGE|LOCAL-STORAGE|LINKAGE)"
    r"\s+SECTION\s*\.",
    re.IGNORECASE,
)


# ---------------------------------------------------------------------------
# Data classes
# ---------------------------------------------------------------------------

@dataclass
class SourceLine:
    """A single line of COBOL source after normalization."""

    line_number: int
    indicator: str
    area_a: str
    area_b: str
    text: str
    is_comment: bool
    is_continuation: bool
    is_blank: bool
    raw: str
    copybook_source: str | None = None

    def __repr__(self) -> str:
        tag = ""
        if self.is_comment:
            tag = " [comment]"
        elif self.is_continuation:
            tag = " [continuation]"
        elif self.is_blank:
            tag = " [blank]"
        src = (
            f" ({self.copybook_source})"
            if self.copybook_source
            else ""
        )
        return (
            f"SourceLine({self.line_number}{tag}{src}: "
            f"{self.text!r})"
        )


@dataclass
class CobolSource:
    """Fully resolved COBOL source ready for parsing."""

    source_file: str
    source_format: str  # "FIXED"
    lines: list[SourceLine]
    total_lines: int
    copybooks_resolved: list[str]
    divisions: dict[str, tuple[int, int]]
    data_sections: dict[str, tuple[int, int]]

    @property
    def code_lines(self) -> list[SourceLine]:
        """Return only non-comment, non-blank lines."""
        return [
            ln
            for ln in self.lines
            if not ln.is_comment and not ln.is_blank
        ]

    def lines_for_division(self, name: str) -> list[SourceLine]:
        """Return all lines belonging to the named division."""
        key = name.upper()
        if key not in self.divisions:
            return []
        start, end = self.divisions[key]
        return [
            ln for ln in self.lines if start <= ln.line_number <= end
        ]

    def lines_for_section(self, name: str) -> list[SourceLine]:
        """Return all lines belonging to the named DATA DIVISION section."""
        key = name.upper()
        if key not in self.data_sections:
            return []
        start, end = self.data_sections[key]
        return [
            ln for ln in self.lines if start <= ln.line_number <= end
        ]


# ---------------------------------------------------------------------------
# Exceptions
# ---------------------------------------------------------------------------

class CopybookNotFoundError(FileNotFoundError):
    """Raised when a COPY statement references an unfound copybook."""

    def __init__(
        self, copybook_name: str, search_dirs: list[str]
    ) -> None:
        self.copybook_name = copybook_name
        self.search_dirs = search_dirs
        dirs_str = (
            ", ".join(search_dirs) if search_dirs else "(none)"
        )
        super().__init__(
            f"Copybook '{copybook_name}' not found in search "
            f"directories: {dirs_str}"
        )


class CopybookRecursionError(RuntimeError):
    """Raised when COPY resolution detects a circular include."""

    def __init__(
        self, copybook_name: str, chain: list[str]
    ) -> None:
        self.copybook_name = copybook_name
        self.chain = chain
        super().__init__(
            f"Circular COPY detected: "
            f"{' -> '.join(chain)} -> {copybook_name}"
        )


# ---------------------------------------------------------------------------
# SourceReader
# ---------------------------------------------------------------------------

class SourceReader:
    """Reads and normalizes IBM Enterprise COBOL fixed-format source files.

    Handles:
    - Fixed-format column parsing (cols 1-6 sequence, col 7 indicator,
      cols 8-11 Area A, cols 12-72 Area B, cols 73-80 identification)
    - Comment/blank line detection
    - Continuation line merging
    - COPY statement resolution with REPLACING support
    - Division/section identification
    """

    def __init__(
        self,
        copybook_dirs: list[str] | None = None,
        *,
        include_debug_lines: bool = False,
        max_copy_depth: int = 10,
        skip_missing_copybooks: bool = False,
    ) -> None:
        self.copybook_dirs: list[Path] = [
            Path(d) for d in (copybook_dirs or [])
        ]
        self.include_debug_lines = include_debug_lines
        self.max_copy_depth = max_copy_depth
        self.skip_missing_copybooks = skip_missing_copybooks
        self.missing_copybooks: list[str] = []

    # ----- public API -----

    def read(self, source_path: str) -> CobolSource:
        """Read a COBOL source file and produce normalized output.

        Args:
            source_path: Path to the main COBOL source file (.cbl).

        Returns:
            A CobolSource with all lines parsed, copybooks resolved,
            divisions and data sections identified.

        Raises:
            FileNotFoundError: If the source file does not exist.
            CopybookNotFoundError: If a copybook cannot be found.
            CopybookRecursionError: If circular COPY nesting detected.
        """
        path = Path(source_path)
        if not path.exists():
            raise FileNotFoundError(
                f"COBOL source file not found: {path}"
            )

        logger.info("Reading COBOL source: %s", path)

        # Phase 1: read raw lines and parse fixed-format columns
        raw_lines = self._read_file(path)
        original_line_count = len(raw_lines)
        parsed = self._parse_fixed_format(raw_lines)

        # Phase 2: resolve COPY statements
        resolved, copybooks = self._resolve_copies(
            parsed, copy_chain=[]
        )

        # Phase 3: merge continuation lines
        merged = self._merge_continuations(resolved)

        # Phase 4: identify divisions and data sections
        divisions = self._identify_divisions(merged)
        data_sections = self._identify_data_sections(
            merged, divisions
        )

        source = CobolSource(
            source_file=path.name,
            source_format="FIXED",
            lines=merged,
            total_lines=original_line_count,
            copybooks_resolved=sorted(set(copybooks)),
            divisions=divisions,
            data_sections=data_sections,
        )

        logger.info(
            "Source read complete: %d lines, %d copybooks resolved, "
            "divisions: %s",
            source.total_lines,
            len(source.copybooks_resolved),
            list(source.divisions.keys()),
        )
        return source

    # ----- file I/O -----

    def _read_file(self, path: Path) -> list[str]:
        """Read a file preserving trailing whitespace."""
        with open(path, encoding="utf-8", errors="replace") as fh:
            return [
                line.rstrip("\n").rstrip("\r") for line in fh
            ]

    # ----- fixed-format parsing -----

    def _parse_fixed_format(
        self,
        raw_lines: list[str],
        *,
        copybook_source: str | None = None,
        line_number_offset: int = 0,
    ) -> list[SourceLine]:
        """Parse raw text lines according to IBM fixed-format rules."""
        result: list[SourceLine] = []
        for idx, raw in enumerate(raw_lines, start=1):
            line_num = idx + line_number_offset

            # Pad short lines to at least 72 columns
            padded = raw.ljust(COL_AREA_B_END)

            indicator = (
                padded[COL_INDICATOR]
                if len(padded) > COL_INDICATOR
                else " "
            )
            area_a = padded[COL_AREA_A_START:COL_AREA_A_END]
            area_b = padded[COL_AREA_B_START:COL_AREA_B_END]

            # Effective text is cols 8-72 (Area A + Area B)
            text = (area_a + area_b).rstrip()

            is_comment = indicator in COMMENT_INDICATORS
            if (
                not self.include_debug_lines
                and indicator in DEBUG_INDICATORS
            ):
                is_comment = True

            is_continuation = indicator == CONTINUATION_INDICATOR
            is_blank = (
                not is_comment
                and not is_continuation
                and text.strip() == ""
            )

            result.append(
                SourceLine(
                    line_number=line_num,
                    indicator=indicator,
                    area_a=area_a,
                    area_b=area_b,
                    text=text,
                    is_comment=is_comment,
                    is_continuation=is_continuation,
                    is_blank=is_blank,
                    raw=raw,
                    copybook_source=copybook_source,
                )
            )

        return result

    # ----- COPY resolution -----

    def _resolve_copies(
        self,
        lines: list[SourceLine],
        copy_chain: list[str],
    ) -> tuple[list[SourceLine], list[str]]:
        """Resolve COPY statements by inlining copybook contents."""
        resolved: list[SourceLine] = []
        copybooks_found: list[str] = []

        for line in lines:
            if line.is_comment or line.is_blank:
                resolved.append(line)
                continue

            match = _COPY_RE.match(line.text)
            if match is None:
                resolved.append(line)
                continue

            copybook_name = match.group("name")
            replacing_text = match.group("replacing")
            replacements = self._parse_replacing(replacing_text)

            logger.debug(
                "Line %d: COPY %s%s",
                line.line_number,
                copybook_name,
                f" REPLACING {replacements}"
                if replacements
                else "",
            )

            # Detect circular inclusion
            if copybook_name.upper() in [
                c.upper() for c in copy_chain
            ]:
                raise CopybookRecursionError(
                    copybook_name, copy_chain
                )
            if len(copy_chain) >= self.max_copy_depth:
                raise CopybookRecursionError(
                    copybook_name,
                    copy_chain + [copybook_name],
                )

            try:
                copybook_path = self._find_copybook(copybook_name)
            except CopybookNotFoundError:
                if self.skip_missing_copybooks:
                    logger.warning(
                        "Copybook '%s' not found -- skipping "
                        "(line %d)",
                        copybook_name,
                        line.line_number,
                    )
                    self.missing_copybooks.append(copybook_name)
                    resolved.append(
                        SourceLine(
                            line_number=line.line_number,
                            indicator="*",
                            area_a="    ",
                            area_b=(
                                f"COPY {copybook_name} "
                                "-- NOT FOUND (skipped)"
                            ),
                            text=(
                                f"COPY {copybook_name} "
                                "-- NOT FOUND (skipped)"
                            ),
                            is_comment=True,
                            is_continuation=False,
                            is_blank=False,
                            raw=line.raw,
                            copybook_source=None,
                        )
                    )
                    continue
                raise
            copybooks_found.append(copybook_name)

            raw_cb_lines = self._read_file(copybook_path)

            if replacements:
                raw_cb_lines = self._apply_replacing(
                    raw_cb_lines, replacements
                )

            cb_parsed = self._parse_fixed_format(
                raw_cb_lines,
                copybook_source=copybook_name,
            )

            cb_resolved, nested_cbs = self._resolve_copies(
                cb_parsed,
                copy_chain=copy_chain + [copybook_name],
            )
            copybooks_found.extend(nested_cbs)

            resolved.extend(cb_resolved)

        return resolved, copybooks_found

    def _find_copybook(self, name: str) -> Path:
        """Search configured directories for a copybook file."""
        extensions = [".cpy", ".CPY", ".cbl", ".CBL", ""]
        for directory in self.copybook_dirs:
            for ext in extensions:
                candidate = directory / f"{name}{ext}"
                if candidate.is_file():
                    logger.debug("Found copybook: %s", candidate)
                    return candidate

        raise CopybookNotFoundError(
            name, [str(d) for d in self.copybook_dirs]
        )

    def _parse_replacing(
        self, replacing_text: str | None
    ) -> list[tuple[str, str]]:
        """Parse REPLACING ==old== BY ==new== pairs."""
        if not replacing_text:
            return []
        return [
            (m.group("old"), m.group("new"))
            for m in _REPLACING_PAIR_RE.finditer(replacing_text)
        ]

    def _apply_replacing(
        self,
        raw_lines: list[str],
        replacements: list[tuple[str, str]],
    ) -> list[str]:
        """Apply REPLACING substitutions to raw copybook lines."""
        result = []
        for line in raw_lines:
            for old, new in replacements:
                line = line.replace(old, new)
            result.append(line)
        return result

    # ----- continuation line merging -----

    def _merge_continuations(
        self, lines: list[SourceLine]
    ) -> list[SourceLine]:
        """Merge continuation lines into their preceding code line."""
        result: list[SourceLine] = []
        last_code_idx: int | None = None

        for line in lines:
            if not line.is_continuation:
                result.append(line)
                if not line.is_comment and not line.is_blank:
                    last_code_idx = len(result) - 1
                continue

            if last_code_idx is None:
                logger.warning(
                    "Line %d: continuation with no preceding "
                    "code line",
                    line.line_number,
                )
                result.append(line)
                continue

            target = result[last_code_idx]
            continued_text = target.text

            if self._has_open_literal(continued_text):
                cont_content = self._literal_continuation_text(
                    line.area_b
                )
            else:
                cont_content = line.area_b.lstrip()

            merged_text = (
                continued_text.rstrip() + cont_content.rstrip()
            )
            merged_area_b = (
                target.area_b.rstrip()
                + " "
                + cont_content.rstrip()
            )

            result[last_code_idx] = SourceLine(
                line_number=target.line_number,
                indicator=target.indicator,
                area_a=target.area_a,
                area_b=merged_area_b,
                text=merged_text,
                is_comment=False,
                is_continuation=False,
                is_blank=False,
                raw=target.raw,
                copybook_source=target.copybook_source,
            )

        return result

    def _has_open_literal(self, text: str) -> bool:
        """Check if text ends with an unclosed string literal."""
        for quote_char in ("'", '"'):
            count = 0
            i = 0
            while i < len(text):
                if text[i] == quote_char:
                    if (
                        i + 1 < len(text)
                        and text[i + 1] == quote_char
                    ):
                        i += 2
                        continue
                    count += 1
                i += 1
            if count % 2 != 0:
                return True
        return False

    def _literal_continuation_text(self, area_b: str) -> str:
        """Extract continuation text for a literal continuation."""
        for i, ch in enumerate(area_b):
            if ch in ("'", '"'):
                return area_b[i + 1:]
        return area_b.lstrip()

    # ----- division / section identification -----

    def _identify_divisions(
        self, lines: list[SourceLine]
    ) -> dict[str, tuple[int, int]]:
        """Identify COBOL division boundaries by line number."""
        division_starts: list[tuple[str, int]] = []

        for line in lines:
            if line.is_comment or line.is_blank:
                continue
            m = _DIVISION_RE.match(line.text)
            if m:
                div_name = m.group("name").upper()
                division_starts.append(
                    (div_name, line.line_number)
                )

        if not division_starts:
            return {}

        last_line = lines[-1].line_number if lines else 0
        divisions: dict[str, tuple[int, int]] = {}

        for i, (name, start) in enumerate(division_starts):
            if i + 1 < len(division_starts):
                end = division_starts[i + 1][1] - 1
            else:
                end = last_line
            divisions[name] = (start, end)

        return divisions

    def _identify_data_sections(
        self,
        lines: list[SourceLine],
        divisions: dict[str, tuple[int, int]],
    ) -> dict[str, tuple[int, int]]:
        """Identify DATA DIVISION sub-sections."""
        if "DATA" not in divisions:
            return {}

        data_start, data_end = divisions["DATA"]
        section_starts: list[tuple[str, int]] = []

        for line in lines:
            if (
                line.line_number < data_start
                or line.line_number > data_end
            ):
                continue
            if line.is_comment or line.is_blank:
                continue
            m = _DATA_SECTION_RE.match(line.text)
            if m:
                sec_name = m.group("name").upper()
                section_starts.append(
                    (sec_name, line.line_number)
                )

        if not section_starts:
            return {}

        sections: dict[str, tuple[int, int]] = {}
        for i, (name, start) in enumerate(section_starts):
            if i + 1 < len(section_starts):
                end = section_starts[i + 1][1] - 1
            else:
                end = data_end
            sections[name] = (start, end)

        return sections
