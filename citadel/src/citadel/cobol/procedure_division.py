"""PROCEDURE DIVISION parser for IBM Enterprise COBOL programs.

Extracts paragraph boundaries, PERFORM statements, EXEC blocks, variable
read/write analysis, and complexity scoring from the PROCEDURE DIVISION.

Adapted from cbexplore's rosetta.parse.procedure_division_parser.
"""

from __future__ import annotations

import logging
import re
from dataclasses import dataclass

from citadel.cobol.source_reader import CobolSource, SourceLine

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Data classes
# ---------------------------------------------------------------------------

@dataclass
class ExecBlock:
    """An EXEC statement block extracted from the PROCEDURE DIVISION."""

    id: str
    type: str
    subtype: str
    raw_text: str
    host_variables_in: list[str]
    host_variables_out: list[str]
    paragraph: str
    line_number: int


@dataclass
class ParagraphInfo:
    """A COBOL paragraph extracted from the PROCEDURE DIVISION."""

    name: str
    line_start: int
    line_end: int
    source_cobol: str
    performs: list[dict]
    exec_blocks: list[str]
    reads: list[str]
    writes: list[str]
    complexity_score: int
    notes: list[str]


# ---------------------------------------------------------------------------
# Regular expressions
# ---------------------------------------------------------------------------

_PARAGRAPH_NAME_RE = re.compile(
    r"^(?P<name>[A-Za-z0-9][A-Za-z0-9_-]*)\s*\.\s*$"
    r"|"
    r"^(?P<name2>[A-Za-z0-9][A-Za-z0-9_-]*)\s*$",
)

_PERFORM_THRU_RE = re.compile(
    r"\bPERFORM\s+(?P<target>[A-Za-z0-9][A-Za-z0-9_-]*)"
    r"\s+THRU\s+(?P<thru>[A-Za-z0-9][A-Za-z0-9_-]*)",
    re.IGNORECASE,
)

_PERFORM_UNTIL_NAME_RE = re.compile(
    r"\bPERFORM\s+(?P<target>[A-Za-z0-9][A-Za-z0-9_-]*)"
    r"\s+UNTIL\b",
    re.IGNORECASE,
)

_PERFORM_UNTIL_INLINE_RE = re.compile(
    r"^\s*PERFORM\s+UNTIL\b",
    re.IGNORECASE,
)

_PERFORM_SIMPLE_RE = re.compile(
    r"\bPERFORM\s+(?P<target>[A-Za-z0-9][A-Za-z0-9_-]*)\s*$"
    r"|\bPERFORM\s+"
    r"(?P<target2>[A-Za-z0-9][A-Za-z0-9_-]*)\s*(?=\n|$)",
    re.IGNORECASE,
)

_EXEC_START_RE = re.compile(
    r"\bEXEC\s+(?P<type>\w+)\b", re.IGNORECASE
)
_EXEC_END_RE = re.compile(r"\bEND-EXEC\b", re.IGNORECASE)

_DLI_FUNC_RE = re.compile(
    r"\bEXEC\s+DLI\s+"
    r"(?P<func>GN|GNP|GU|GHU|ISRT|REPL|DLET|CHKP)\b",
    re.IGNORECASE,
)

_PCB_RE = re.compile(
    r"\bPCB\s*\(\s*(?P<name>[A-Za-z0-9_-]+)\s*\)",
    re.IGNORECASE,
)

_SEGMENT_RE = re.compile(
    r"\bSEGMENT\s*\(\s*(?P<name>[A-Za-z0-9_-]+)\s*\)",
    re.IGNORECASE,
)

_INTO_RE = re.compile(
    r"\bINTO\s*\(\s*(?P<name>[A-Za-z0-9_-]+)\s*\)",
    re.IGNORECASE,
)

_FROM_EXEC_RE = re.compile(
    r"\bFROM\s*\(\s*(?P<name>[A-Za-z0-9_-]+)\s*\)",
    re.IGNORECASE,
)

_ID_RE = re.compile(
    r"\bID\s*\(\s*(?P<name>[A-Za-z0-9_-]+)\s*\)",
    re.IGNORECASE,
)

# ---------------------------------------------------------------------------
# Statement-level patterns for read/write analysis
# ---------------------------------------------------------------------------

_MOVE_RE = re.compile(
    r"\bMOVE\s+(?P<source>.+?)\s+TO\s+(?P<targets>.+)",
    re.IGNORECASE,
)

_COMPUTE_RE = re.compile(
    r"\bCOMPUTE\s+(?P<target>[A-Za-z][A-Za-z0-9_-]*)"
    r"\s*=\s*(?P<expr>.+)",
    re.IGNORECASE,
)

_ADD_TO_RE = re.compile(
    r"\bADD\s+(?P<value>.+?)\s+TO\s+"
    r"(?P<target>[A-Za-z][A-Za-z0-9_-]*)",
    re.IGNORECASE,
)

_SUBTRACT_FROM_RE = re.compile(
    r"\bSUBTRACT\s+(?P<value>.+?)\s+FROM\s+"
    r"(?P<target>[A-Za-z][A-Za-z0-9_-]*)",
    re.IGNORECASE,
)

_SET_TO_RE = re.compile(
    r"\bSET\s+(?P<target>[A-Za-z][A-Za-z0-9_-]*)\s+TO\b",
    re.IGNORECASE,
)

_ACCEPT_RE = re.compile(
    r"\bACCEPT\s+(?P<target>[A-Za-z][A-Za-z0-9_-]*)"
    r"\s+FROM\b",
    re.IGNORECASE,
)

_ACCEPT_SIMPLE_RE = re.compile(
    r"\bACCEPT\s+(?P<target>[A-Za-z][A-Za-z0-9_-]*)\b",
    re.IGNORECASE,
)

_DISPLAY_RE = re.compile(r"\bDISPLAY\b", re.IGNORECASE)

_IF_RE = re.compile(r"\bIF\b", re.IGNORECASE)
_EVALUATE_RE = re.compile(r"\bEVALUATE\b", re.IGNORECASE)
_WHEN_RE = re.compile(r"\bWHEN\b", re.IGNORECASE)

_GOBACK_RE = re.compile(r"\bGOBACK\b", re.IGNORECASE)
_STOP_RUN_RE = re.compile(r"\bSTOP\s+RUN\b", re.IGNORECASE)
_GO_TO_RE = re.compile(r"\bGO\s+TO\b", re.IGNORECASE)
_ALTER_RE = re.compile(r"\bALTER\b", re.IGNORECASE)

_END_PERFORM_RE = re.compile(
    r"\bEND-PERFORM\b", re.IGNORECASE
)

_PROCEDURE_DIV_RE = re.compile(
    r"^\s*PROCEDURE\s+DIVISION\b", re.IGNORECASE
)
_SECTION_HEADER_RE = re.compile(
    r"^\s*\w+\s+SECTION\b", re.IGNORECASE
)
_DIVISION_HEADER_RE = re.compile(
    r"^\s*\w+\s+DIVISION\b", re.IGNORECASE
)
_PARA_NAME_ONLY_RE = re.compile(
    r"^([A-Za-z0-9][A-Za-z0-9_-]*)$"
)
_IS_VARNAME_RE = re.compile(
    r"^[A-Za-z][A-Za-z0-9_-]*$"
)
_PERFORM_UNTIL_INLINE_MATCH_RE = re.compile(
    r"\s*PERFORM\s+UNTIL\b", re.IGNORECASE
)
_PERFORM_SIMPLE_SEARCH_RE = re.compile(
    r"\bPERFORM\s+([A-Za-z0-9][A-Za-z0-9_-]*)\s*$",
    re.IGNORECASE,
)
_STMT_START_RE = re.compile(
    r"^\s*(?:MOVE|COMPUTE|ADD|SUBTRACT|SET"
    r"|ACCEPT|DISPLAY|IF|EVALUATE|WHEN"
    r"|PERFORM|GOBACK|STOP|GO|EXIT|END-IF"
    r"|END-EVALUATE|END-PERFORM|ELSE|NOT"
    r"|CONTINUE)\b",
    re.IGNORECASE,
)
_EXIT_STMT_RE = re.compile(
    r"^\s*EXIT\s*\.?\s*$", re.IGNORECASE
)
_PERFORM_UNTIL_COND_RE = re.compile(
    r"\s*PERFORM\s+UNTIL\s+(?P<cond>.+)",
    re.IGNORECASE,
)
_IF_COUNT_RE = re.compile(r"\bIF\b", re.IGNORECASE)
_WHEN_COUNT_RE = re.compile(r"\bWHEN\b", re.IGNORECASE)
_PERFORM_SIMPLE_MULTILINE_RE = re.compile(
    r"\bPERFORM\s+([A-Za-z0-9][A-Za-z0-9_-]*)\s*$",
    re.IGNORECASE | re.MULTILINE,
)
_COMPUTE_WORD_RE = re.compile(
    r"\bCOMPUTE\b", re.IGNORECASE
)
_SUBTRACT_WORD_RE = re.compile(
    r"\bSUBTRACT\b", re.IGNORECASE
)
_END_IF_RE = re.compile(r"\bEND-IF\b")
_END_EVALUATE_RE = re.compile(r"\bEND-EVALUATE\b")
_PARA_HEADER_LINE_RE = re.compile(
    r"^[A-Za-z0-9][A-Za-z0-9_-]*\s*\.?\s*$"
)

_VARNAME_RE = re.compile(r"\b([A-Za-z][A-Za-z0-9_-]*)\b")

_COBOL_KEYWORDS = frozenset({
    "ACCEPT", "ADD", "ALTER", "CALL", "CANCEL", "CLOSE",
    "COMPUTE", "CONTINUE", "DELETE", "DISPLAY", "DIVIDE",
    "ELSE", "END-EVALUATE", "END-IF", "END-PERFORM",
    "END-EXEC", "EVALUATE", "EXEC", "EXIT", "FROM", "GO",
    "GOBACK", "IF", "INITIALIZE", "INSPECT", "INTO", "MOVE",
    "MULTIPLY", "NOT", "OPEN", "PERFORM", "READ", "RELEASE",
    "RETURN", "REWRITE", "SEARCH", "SET", "SORT", "START",
    "STOP", "STRING", "SUBTRACT", "THRU", "THROUGH", "TO",
    "UNSTRING", "UNTIL", "UPON", "WHEN", "WRITE", "AND",
    "OR", "THAN", "GREATER", "LESS", "EQUAL", "NUMERIC",
    "ALPHABETIC", "ALPHANUMERIC", "TRUE", "FALSE", "SPACES",
    "ZEROES", "ZEROS", "ZERO", "LOW-VALUES", "HIGH-VALUES",
    "ALL", "IS", "ARE", "ALSO", "OTHER", "USING", "BY",
    "VALUE", "VALUES", "VARYING", "GIVING", "REMAINDER",
    "ON", "OFF", "SIZE", "ERROR", "OVERFLOW", "EXCEPTION",
    "END-ADD", "END-SUBTRACT", "END-MULTIPLY",
    "END-DIVIDE", "END-COMPUTE", "END-STRING",
    "END-UNSTRING", "END-CALL", "END-READ", "END-WRITE",
    "END-SEARCH", "END-RETURN", "END-DELETE", "SEGMENT",
    "PCB", "DLI", "SQL", "CICS", "CHKP", "GN", "GNP", "GU",
    "GHU", "ISRT", "REPL", "DLET", "SEND", "MAP", "RECEIVE",
    "SECTION", "DIVISION", "PROCEDURE", "DATA",
    "WORKING-STORAGE", "LINKAGE", "FILE", "THEN", "RUN",
    "PROGRAM", "WITH", "REFERENCE", "CONTENT", "LENGTH",
    "OF", "IN",
})

_FIGURATIVE_CONSTANTS = frozenset({
    "SPACES", "SPACE", "ZEROES", "ZEROS", "ZERO",
    "LOW-VALUES", "LOW-VALUE", "HIGH-VALUES", "HIGH-VALUE",
    "QUOTES", "QUOTE", "NULL", "NULLS",
})

_SPECIAL_REGISTERS = frozenset({
    "RETURN-CODE", "SORT-RETURN", "TALLY", "SHIFT-OUT",
    "SHIFT-IN", "SORT-FILE-SIZE", "SORT-CORE-SIZE",
    "SORT-MESSAGE", "SORT-MODE-SIZE", "WHEN-COMPILED",
    "DEBUG-ITEM", "LINAGE-COUNTER", "DIBSTAT",
})


# ---------------------------------------------------------------------------
# Helper functions
# ---------------------------------------------------------------------------

def _is_variable_name(token: str) -> bool:
    """Check if a token looks like a COBOL variable name."""
    upper = token.upper()
    if upper in _COBOL_KEYWORDS:
        return False
    if upper in _FIGURATIVE_CONSTANTS:
        return False
    if not _IS_VARNAME_RE.match(token):
        return False
    if token.replace("-", "").replace("_", "").isdigit():
        return False
    return not (len(token) == 1 and token.upper() in "XAVSP")


def _extract_variable_names(text: str) -> list[str]:
    """Extract potential variable names from a COBOL expression."""
    seen: set[str] = set()
    result: list[str] = []
    for match in _VARNAME_RE.finditer(text):
        token = match.group(1)
        upper = token.upper()
        if upper not in seen and _is_variable_name(token):
            seen.add(upper)
            result.append(upper)
    return result


# ---------------------------------------------------------------------------
# Main parser class
# ---------------------------------------------------------------------------

class ProcedureDivisionParser:
    """Parses the PROCEDURE DIVISION of a COBOL program."""

    def __init__(
        self, data_items: list | None = None
    ) -> None:
        self._known_variables: set[str] = set()
        self._condition_to_parent: dict[str, str] = {}
        if data_items:
            for item in data_items:
                self._known_variables.add(
                    item.name.upper()
                )
                for cond in item.conditions_88:
                    if (
                        isinstance(cond, dict)
                        and "name" in cond
                    ):
                        cond_name = cond["name"].upper()
                        self._known_variables.add(cond_name)
                        self._condition_to_parent[
                            cond_name
                        ] = item.name.upper()
                if item.level == 88 and item.parent:
                    self._condition_to_parent[
                        item.name.upper()
                    ] = item.parent.upper()

    def parse(
        self, source: CobolSource
    ) -> tuple[list[ParagraphInfo], list[ExecBlock]]:
        """Parse the PROCEDURE DIVISION.

        Returns:
            Tuple of (paragraphs, exec_blocks).
        """
        proc_lines = self._get_procedure_lines(source)
        if not proc_lines:
            logger.warning(
                "No PROCEDURE DIVISION found in source"
            )
            return [], []

        paragraph_ranges = self._find_paragraph_boundaries(
            proc_lines
        )
        logger.info("Found %d paragraphs", len(paragraph_ranges))

        all_exec_blocks = self._extract_all_exec_blocks(
            proc_lines, paragraph_ranges
        )
        logger.info(
            "Found %d EXEC blocks", len(all_exec_blocks)
        )

        paragraphs: list[ParagraphInfo] = []
        for (
            name,
            start_line,
            end_line,
            lines_in_para,
        ) in paragraph_ranges:
            para = self._build_paragraph_info(
                name,
                start_line,
                end_line,
                lines_in_para,
                all_exec_blocks,
                source,
            )
            paragraphs.append(para)

        return paragraphs, all_exec_blocks

    # ----- get procedure lines -----

    def _get_procedure_lines(
        self, source: CobolSource
    ) -> list[SourceLine]:
        """Get all lines belonging to the PROCEDURE DIVISION."""
        if "PROCEDURE" not in source.divisions:
            return []
        return source.lines_for_division("PROCEDURE")

    # ----- paragraph boundary detection -----

    def _find_paragraph_boundaries(
        self, proc_lines: list[SourceLine]
    ) -> list[tuple[str, int, int, list[SourceLine]]]:
        """Identify paragraph boundaries in the PROCEDURE DIVISION."""
        para_starts: list[tuple[str, int, int]] = []

        for idx, line in enumerate(proc_lines):
            if line.is_comment or line.is_blank:
                continue

            if _PROCEDURE_DIV_RE.match(line.text):
                continue

            area_a = line.area_a
            if area_a.strip() == "":
                continue

            text = line.text.strip()
            if text.endswith("."):
                text = text[:-1].strip()

            if _SECTION_HEADER_RE.match(text):
                continue
            if _DIVISION_HEADER_RE.match(text):
                continue

            name_match = _PARA_NAME_ONLY_RE.match(text)
            if name_match:
                para_name = name_match.group(1).upper()
                para_starts.append(
                    (para_name, line.line_number, idx)
                )

        if not para_starts:
            return []

        result: list[
            tuple[str, int, int, list[SourceLine]]
        ] = []

        for i, (name, start_line, start_idx) in enumerate(
            para_starts
        ):
            if i + 1 < len(para_starts):
                next_start_idx = para_starts[i + 1][2]
                end_line = self._find_last_code_line(
                    proc_lines[start_idx:next_start_idx]
                )
                lines_in_para = proc_lines[
                    start_idx:next_start_idx
                ]
            else:
                end_line = self._find_last_code_line(
                    proc_lines[start_idx:]
                )
                lines_in_para = proc_lines[start_idx:]

            result.append(
                (name, start_line, end_line, lines_in_para)
            )

        return result

    def _find_last_code_line(
        self, lines: list[SourceLine]
    ) -> int:
        """Find the line number of the last non-comment line."""
        for line in reversed(lines):
            if not line.is_comment and not line.is_blank:
                return line.line_number
        return lines[0].line_number if lines else 0

    # ----- EXEC block extraction -----

    def _extract_all_exec_blocks(
        self,
        proc_lines: list[SourceLine],
        paragraph_ranges: list[
            tuple[str, int, int, list[SourceLine]]
        ],
    ) -> list[ExecBlock]:
        """Extract all EXEC blocks from the PROCEDURE DIVISION."""
        exec_blocks: list[ExecBlock] = []
        exec_counter = 0

        code_lines = [
            ln
            for ln in proc_lines
            if not ln.is_comment and not ln.is_blank
        ]

        i = 0
        while i < len(code_lines):
            line = code_lines[i]
            exec_match = _EXEC_START_RE.search(line.text)

            if exec_match is None:
                i += 1
                continue

            exec_type = exec_match.group("type").upper()
            block_lines = [line.text]
            block_start_line = line.line_number

            if _EXEC_END_RE.search(line.text):
                pass
            else:
                j = i + 1
                while j < len(code_lines):
                    block_lines.append(code_lines[j].text)
                    if _EXEC_END_RE.search(code_lines[j].text):
                        break
                    j += 1
                i = j

            exec_counter += 1
            full_text = " ".join(
                ln.strip() for ln in block_lines
            )
            full_text = re.sub(r"\s+", " ", full_text).strip()

            para_name = self._find_containing_paragraph(
                block_start_line, paragraph_ranges
            )

            exec_block = self._parse_exec_block(
                exec_counter,
                exec_type,
                full_text,
                para_name,
                block_start_line,
            )
            exec_blocks.append(exec_block)

            i += 1

        return exec_blocks

    def _find_containing_paragraph(
        self,
        line_number: int,
        paragraph_ranges: list[
            tuple[str, int, int, list[SourceLine]]
        ],
    ) -> str:
        """Find which paragraph contains a given line number."""
        for name, start, end, _ in paragraph_ranges:
            if start <= line_number <= end:
                return name
        return "UNKNOWN"

    def _parse_exec_block(
        self,
        counter: int,
        exec_type: str,
        raw_text: str,
        paragraph: str,
        line_number: int,
    ) -> ExecBlock:
        """Parse a single EXEC block into an ExecBlock object."""
        exec_id = f"exec-{counter:03d}"
        subtype = ""
        host_variables_in: list[str] = []
        host_variables_out: list[str] = []

        if exec_type == "DLI":
            func_match = _DLI_FUNC_RE.search(raw_text)
            if func_match:
                subtype = func_match.group("func").upper()

            pcb_match = _PCB_RE.search(raw_text)
            if pcb_match:
                host_variables_in.append(
                    pcb_match.group("name").upper()
                )

            into_match = _INTO_RE.search(raw_text)
            if into_match:
                host_variables_out.append(
                    into_match.group("name").upper()
                )

            from_match = _FROM_EXEC_RE.search(raw_text)
            if from_match:
                host_variables_in.append(
                    from_match.group("name").upper()
                )

            id_match = _ID_RE.search(raw_text)
            if id_match:
                host_variables_in.append(
                    id_match.group("name").upper()
                )

        elif exec_type == "SQL":
            for match in re.finditer(
                r":([A-Za-z][A-Za-z0-9_-]*)", raw_text
            ):
                var_name = match.group(1).upper()
                into_pos = raw_text.upper().find("INTO")
                if (
                    into_pos >= 0
                    and match.start() > into_pos
                ):
                    host_variables_out.append(var_name)
                else:
                    host_variables_in.append(var_name)
            subtype = self._extract_sql_subtype(raw_text)

        elif exec_type == "CICS":
            subtype = self._extract_cics_subtype(raw_text)

        return ExecBlock(
            id=exec_id,
            type=exec_type,
            subtype=subtype,
            raw_text=raw_text,
            host_variables_in=host_variables_in,
            host_variables_out=host_variables_out,
            paragraph=paragraph,
            line_number=line_number,
        )

    def _extract_sql_subtype(self, raw_text: str) -> str:
        """Extract SQL statement type from EXEC SQL block."""
        upper = raw_text.upper()
        for keyword in (
            "SELECT", "INSERT", "UPDATE", "DELETE",
            "DECLARE", "OPEN", "FETCH", "CLOSE", "COMMIT",
            "ROLLBACK",
        ):
            if keyword in upper:
                return keyword
        return "UNKNOWN"

    def _extract_cics_subtype(self, raw_text: str) -> str:
        """Extract CICS command type from EXEC CICS block."""
        upper = raw_text.upper()
        for keyword in (
            "SEND MAP", "RECEIVE MAP", "SEND", "RECEIVE",
            "READ", "WRITE", "REWRITE", "DELETE", "STARTBR",
            "READNEXT", "ENDBR", "LINK", "XCTL", "RETURN",
            "ABEND",
        ):
            if keyword in upper:
                return keyword
        return "UNKNOWN"

    # ----- paragraph info building -----

    def _build_paragraph_info(
        self,
        name: str,
        start_line: int,
        end_line: int,
        lines: list[SourceLine],
        all_exec_blocks: list[ExecBlock],
        source: CobolSource,
    ) -> ParagraphInfo:
        """Build a complete ParagraphInfo for a paragraph."""
        source_cobol = self._build_source_cobol(lines)
        performs = self._extract_performs(lines)

        exec_block_ids = [
            eb.id
            for eb in all_exec_blocks
            if eb.paragraph == name
        ]

        reads, writes = self._analyze_variables(
            lines, all_exec_blocks, name
        )

        complexity = self._compute_complexity(lines)
        notes = self._compute_notes(lines, name)

        return ParagraphInfo(
            name=name,
            line_start=start_line,
            line_end=end_line,
            source_cobol=source_cobol,
            performs=performs,
            exec_blocks=exec_block_ids,
            reads=sorted(set(reads)),
            writes=sorted(set(writes)),
            complexity_score=complexity,
            notes=notes,
        )

    def _build_source_cobol(
        self, lines: list[SourceLine]
    ) -> str:
        """Build the raw COBOL source text for a paragraph."""
        return "\n".join(line.raw for line in lines)

    # ----- PERFORM extraction -----

    def _extract_performs(
        self, lines: list[SourceLine]
    ) -> list[dict]:
        """Extract all PERFORM statements from paragraph lines."""
        performs: list[dict] = []
        seen_targets: set[str] = set()

        code_lines = [
            ln
            for ln in lines
            if not ln.is_comment and not ln.is_blank
        ]

        for line in code_lines:
            text = line.text

            area_a = line.area_a
            if area_a.strip() != "":
                stripped = text.strip().rstrip(".")
                if _PARA_NAME_ONLY_RE.match(stripped):
                    continue

            # PERFORM ... THRU ...
            for m in _PERFORM_THRU_RE.finditer(text):
                target = m.group("target").upper()
                thru = m.group("thru").upper()
                key = f"{target}|{thru}"
                if key not in seen_targets:
                    seen_targets.add(key)
                    performs.append({
                        "target": target,
                        "mechanism": "PERFORM THRU",
                        "thru": thru,
                    })
                continue

            # PERFORM name UNTIL condition
            m = _PERFORM_UNTIL_NAME_RE.search(text)
            if m:
                target = m.group("target").upper()
                key = f"{target}|UNTIL"
                if key not in seen_targets:
                    seen_targets.add(key)
                    performs.append({
                        "target": target,
                        "mechanism": "PERFORM UNTIL",
                    })
                continue

            # Inline PERFORM UNTIL
            if _PERFORM_UNTIL_INLINE_RE.match(text):
                continue

            # Simple PERFORM name
            m = _PERFORM_SIMPLE_SEARCH_RE.search(text)
            if m:
                target = m.group(1).upper()
                if target not in (
                    "UNTIL",
                    "VARYING",
                    "WITH",
                    "TIMES",
                ):
                    key = f"{target}|SIMPLE"
                    if key not in seen_targets:
                        seen_targets.add(key)
                        performs.append({
                            "target": target,
                            "mechanism": "PERFORM",
                        })

        return performs

    # ----- variable read/write analysis -----

    def _assemble_proc_statements(
        self, lines: list[SourceLine]
    ) -> list[str]:
        """Assemble multi-line COBOL procedure statements."""
        statements: list[str] = []
        current_text = ""
        in_exec_block = False

        for line in lines:
            text = line.text.strip()

            area_a = line.area_a
            if area_a.strip() != "":
                stripped = text.rstrip(".")
                if _PARA_NAME_ONLY_RE.match(
                    stripped.strip()
                ):
                    continue

            if _EXEC_START_RE.search(text):
                in_exec_block = True
                if _EXEC_END_RE.search(text):
                    in_exec_block = False
                continue
            if in_exec_block:
                if _EXEC_END_RE.search(text):
                    in_exec_block = False
                continue

            starts_new = bool(_STMT_START_RE.match(text))

            if starts_new:
                if current_text:
                    statements.append(current_text)
                current_text = text
            else:
                if current_text:
                    current_text += " " + text
                else:
                    current_text = text

            if current_text.rstrip().endswith("."):
                statements.append(current_text)
                current_text = ""

        if current_text:
            statements.append(current_text)

        return statements

    def _analyze_variables(
        self,
        lines: list[SourceLine],
        all_exec_blocks: list[ExecBlock],
        paragraph_name: str,
    ) -> tuple[list[str], list[str]]:
        """Analyze which variables are read and written."""
        reads: list[str] = []
        writes: list[str] = []
        code_lines = [
            ln
            for ln in lines
            if not ln.is_comment and not ln.is_blank
        ]

        assembled_stmts = self._assemble_proc_statements(
            code_lines
        )

        for text in assembled_stmts:
            text = text.strip()

            if _EXIT_STMT_RE.match(text):
                continue

            # MOVE source TO target1 target2 ...
            m = _MOVE_RE.search(text)
            if m:
                source_vars = self._filter_variables(
                    _extract_variable_names(m.group("source"))
                )
                target_vars = self._filter_variables(
                    _extract_variable_names(m.group("targets")),
                    permissive=True,
                )
                reads.extend(source_vars)
                writes.extend(target_vars)
                continue

            # COMPUTE target = expr
            m = _COMPUTE_RE.search(text)
            if m:
                target = m.group("target").upper()
                if self._is_known_variable(target):
                    writes.append(target)
                expr_vars = self._filter_variables(
                    _extract_variable_names(m.group("expr"))
                )
                reads.extend(expr_vars)
                continue

            # ADD value TO target
            m = _ADD_TO_RE.search(text)
            if m:
                value_vars = self._filter_variables(
                    _extract_variable_names(m.group("value"))
                )
                target = m.group("target").upper()
                reads.extend(value_vars)
                if self._is_known_variable(target):
                    writes.append(target)
                continue

            # SUBTRACT value FROM target
            m = _SUBTRACT_FROM_RE.search(text)
            if m:
                value_vars = self._filter_variables(
                    _extract_variable_names(m.group("value"))
                )
                target = m.group("target").upper()
                reads.extend(value_vars)
                if self._is_known_variable(target):
                    writes.append(target)
                    reads.append(target)
                continue

            # SET name TO TRUE
            m = _SET_TO_RE.search(text)
            if m:
                target = m.group("target").upper()
                if self._is_known_variable(target):
                    writes.append(target)
                continue

            # ACCEPT target FROM source
            m = _ACCEPT_RE.search(text)
            if m:
                target = m.group("target").upper()
                if self._is_known_variable(target):
                    writes.append(target)
                continue

            # DISPLAY items (reads)
            if _DISPLAY_RE.search(text):
                display_text = re.sub(
                    r"\bDISPLAY\b",
                    "",
                    text,
                    flags=re.IGNORECASE,
                )
                display_text = re.sub(
                    r"'[^']*'", "", display_text
                )
                display_text = re.sub(
                    r'"[^"]*"', "", display_text
                )
                display_vars = self._filter_variables(
                    _extract_variable_names(display_text)
                )
                reads.extend(display_vars)
                continue

            # IF/EVALUATE conditions (reads)
            if _IF_RE.search(text) or _EVALUATE_RE.search(
                text
            ):
                cond_text = text
                cond_text = re.sub(
                    r"'[^']*'", "", cond_text
                )
                cond_text = re.sub(
                    r'"[^"]*"', "", cond_text
                )
                cond_vars = self._filter_variables(
                    _extract_variable_names(cond_text)
                )
                reads.extend(cond_vars)
                continue

            # WHEN clauses
            if _WHEN_RE.search(text):
                when_text = re.sub(r"'[^']*'", "", text)
                when_text = re.sub(r'"[^"]*"', "", when_text)
                when_vars = self._filter_variables(
                    _extract_variable_names(when_text)
                )
                reads.extend(when_vars)
                continue

            # PERFORM UNTIL condition
            perform_until_match = _PERFORM_UNTIL_COND_RE.match(
                text
            )
            if perform_until_match:
                cond_text = perform_until_match.group("cond")
                cond_text = re.sub(
                    r"'[^']*'", "", cond_text
                )
                cond_text = re.sub(
                    r'"[^"]*"', "", cond_text
                )
                cond_vars = self._filter_variables(
                    _extract_variable_names(cond_text)
                )
                reads.extend(cond_vars)
                continue

        # Add exec block variables
        for eb in all_exec_blocks:
            if eb.paragraph == paragraph_name:
                reads.extend(
                    self._filter_variables(
                        eb.host_variables_in
                    )
                )
                writes.extend(
                    self._filter_variables(
                        eb.host_variables_out
                    )
                )

        reads = self._resolve_88_to_parents(reads)
        writes = self._resolve_88_to_parents(writes)

        return reads, writes

    def _resolve_88_to_parents(
        self, names: list[str]
    ) -> list[str]:
        """Resolve 88-level condition names to parent fields."""
        if not self._condition_to_parent:
            return names
        result: list[str] = []
        for name in names:
            upper = name.upper()
            if upper in self._condition_to_parent:
                result.append(
                    self._condition_to_parent[upper]
                )
            else:
                result.append(upper)
        return result

    def _filter_variables(
        self,
        names: list[str],
        *,
        permissive: bool = False,
    ) -> list[str]:
        """Filter a list of candidate variable names."""
        if not self._known_variables:
            return names
        result = []
        for n in names:
            upper = n.upper()
            if (
                upper in self._known_variables
                or upper in _SPECIAL_REGISTERS
                or (permissive and _is_variable_name(n))
            ):
                result.append(upper)
        return result

    def _is_known_variable(self, name: str) -> bool:
        """Check if a name is a known data item."""
        upper = name.upper()
        if upper in _SPECIAL_REGISTERS:
            return True
        if not self._known_variables:
            return _is_variable_name(name)
        return upper in self._known_variables

    # ----- complexity scoring -----

    def _compute_complexity(
        self, lines: list[SourceLine]
    ) -> int:
        """Compute complexity score 1-5 for a paragraph."""
        code_lines = [
            ln
            for ln in lines
            if not ln.is_comment and not ln.is_blank
        ]
        full_text = " ".join(ln.text for ln in code_lines)

        has_alter = bool(_ALTER_RE.search(full_text))
        has_goto = bool(_GO_TO_RE.search(full_text))
        has_if = bool(_IF_RE.search(full_text))
        has_evaluate = bool(_EVALUATE_RE.search(full_text))
        has_perform_thru = bool(
            _PERFORM_THRU_RE.search(full_text)
        )

        inline_perform_until_count = 0
        for ln in code_lines:
            if _PERFORM_UNTIL_INLINE_MATCH_RE.match(
                ln.text
            ):
                inline_perform_until_count += 1

        has_inline_perform_until = (
            inline_perform_until_count > 0
        )

        if_count = len(_IF_COUNT_RE.findall(full_text))
        when_count = len(_WHEN_COUNT_RE.findall(full_text))

        nesting = self._estimate_nesting_depth(code_lines)

        perform_call_count = len(
            _PERFORM_THRU_RE.findall(full_text)
        ) + len(
            _PERFORM_SIMPLE_MULTILINE_RE.findall(full_text)
        )

        if has_alter:
            return 5

        if has_goto:
            if nesting >= 3:
                return 5
            return 4

        if nesting >= 5:
            return 5

        if (
            inline_perform_until_count >= 2
            and has_perform_thru
        ):
            return 4

        if (
            has_inline_perform_until
            and has_perform_thru
            and perform_call_count > 3
        ):
            return 4

        if has_evaluate and when_count >= 3:
            return 3

        if has_if and has_evaluate:
            return 3

        if nesting >= 3 and if_count >= 2:
            return 3

        if if_count >= 2 and nesting >= 2:
            has_compute = bool(
                _COMPUTE_WORD_RE.search(full_text)
            )
            has_subtract = bool(
                _SUBTRACT_WORD_RE.search(full_text)
            )
            if has_compute or has_subtract:
                return 3

        if has_if or has_evaluate:
            return 2

        return 1

    def _estimate_nesting_depth(
        self, code_lines: list[SourceLine]
    ) -> int:
        """Estimate maximum nesting depth of control structures."""
        depth = 0
        max_depth = 0

        for line in code_lines:
            text = line.text.strip().upper()

            end_if_count = len(
                _END_IF_RE.findall(text)
            )
            end_eval_count = len(
                _END_EVALUATE_RE.findall(text)
            )
            end_perf_count = len(
                _END_PERFORM_RE.findall(text)
            )
            depth -= (
                end_if_count
                + end_eval_count
                + end_perf_count
            )
            depth = max(0, depth)

            stripped = _END_IF_RE.sub("", text)
            stripped = _END_EVALUATE_RE.sub("", stripped)
            stripped = _END_PERFORM_RE.sub("", stripped)

            if_count = len(_IF_COUNT_RE.findall(stripped))
            eval_count = len(
                _EVALUATE_RE.findall(stripped)
            )

            perf_until = (
                1
                if _PERFORM_UNTIL_INLINE_MATCH_RE.match(text)
                else 0
            )

            depth += if_count + eval_count + perf_until
            max_depth = max(max_depth, depth)

        return max_depth

    # ----- notes computation -----

    def _compute_notes(
        self, lines: list[SourceLine], name: str
    ) -> list[str]:
        """Generate descriptive notes for a paragraph."""
        notes: list[str] = []
        code_lines = [
            ln
            for ln in lines
            if not ln.is_comment and not ln.is_blank
        ]

        if not code_lines:
            return notes

        full_text = " ".join(ln.text for ln in code_lines)

        non_header_code = [
            ln
            for ln in code_lines
            if ln.area_a.strip() == ""
            or not _PARA_HEADER_LINE_RE.match(
                ln.text.strip()
            )
        ]
        if len(non_header_code) <= 1:
            remaining_text = " ".join(
                ln.text.strip() for ln in non_header_code
            )
            if _EXIT_STMT_RE.match(remaining_text):
                notes.append("EXIT paragraph")

        if _GOBACK_RE.search(full_text):
            notes.append("Contains GOBACK")

        if _STOP_RUN_RE.search(full_text):
            notes.append("Contains STOP RUN")

        if _GO_TO_RE.search(full_text):
            notes.append("Contains GO TO")

        if _ALTER_RE.search(full_text):
            notes.append("Contains ALTER")

        perform_until_count = 0
        for ln in code_lines:
            if _PERFORM_UNTIL_INLINE_MATCH_RE.match(
                ln.text
            ):
                perform_until_count += 1
        if perform_until_count >= 2:
            notes.append(
                "Contains nested PERFORM UNTIL loops"
            )
        elif perform_until_count == 1:
            notes.append("Contains PERFORM UNTIL loop")

        return notes


# ---------------------------------------------------------------------------
# Convenience function
# ---------------------------------------------------------------------------

def parse_procedure_division(
    source: CobolSource,
    data_items: list | None = None,
) -> tuple[list[ParagraphInfo], list[ExecBlock]]:
    """Convenience function to parse the PROCEDURE DIVISION."""
    parser = ProcedureDivisionParser(data_items=data_items)
    return parser.parse(source)
