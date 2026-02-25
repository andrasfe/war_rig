"""DATA DIVISION parser for IBM Enterprise COBOL programs.

Extracts all data item definitions from the DATA DIVISION (WORKING-STORAGE,
LINKAGE, and FILE sections) and builds a structured data dictionary with
full type information, hierarchy, and computed storage sizes.

Adapted from cbexplore's rosetta.parse.data_division_parser.
"""

from __future__ import annotations

import logging
import math
import re
from dataclasses import dataclass, field

from citadel.cobol.source_reader import CobolSource, SourceLine

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Data classes
# ---------------------------------------------------------------------------

@dataclass
class DataItem:
    """A single COBOL DATA DIVISION item with full type information."""

    name: str
    level: int
    picture: str | None
    usage: str | None
    value: str | None
    redefines: str | None
    occurs: int | None
    occurs_depending_on: str | None
    signed: bool
    decimal_positions: int
    byte_length: int
    section: str
    copybook_source: str | None
    line_number: int
    parent: str | None
    children: list[str] = field(default_factory=list)
    conditions_88: list[dict] = field(default_factory=list)
    python_type_hint: str = ""


# ---------------------------------------------------------------------------
# Regular expressions for clause extraction
# ---------------------------------------------------------------------------

_LEVEL_RE = re.compile(
    r"^\s*(?P<level>\d{1,2})\s+"
    r"(?P<name>FILLER|[A-Za-z][A-Za-z0-9_-]*)"
    r"(?P<rest>.*)",
    re.IGNORECASE,
)

_PIC_RE = re.compile(
    r"\bPIC(?:TURE)?\s+(?:IS\s+)?(?P<pic>[S9XAVP()0-9]+)",
    re.IGNORECASE,
)

_USAGE_RE = re.compile(
    r"\bUSAGE\s+(?:IS\s+)?(?P<usage>COMP(?:UTATIONAL)?(?:-[0-9])?"
    r"|BINARY|PACKED-DECIMAL|DISPLAY|INDEX|POINTER)\b",
    re.IGNORECASE,
)

_STANDALONE_USAGE_RE = re.compile(
    r"\b(?P<usage>COMP(?:UTATIONAL)?(?:-[0-9])?"
    r"|BINARY|PACKED-DECIMAL)\b",
    re.IGNORECASE,
)

_VALUE_RE = re.compile(
    r"\bVALUE\s+(?:IS\s+)?(?P<value>.+?)"
    r"(?=\s+(?:PIC|USAGE|COMP|BINARY|OCCURS|REDEFINES)\b"
    r"|\s*\.?\s*$)",
    re.IGNORECASE,
)

_REDEFINES_RE = re.compile(
    r"\bREDEFINES\s+(?P<target>[A-Za-z][A-Za-z0-9_-]*)",
    re.IGNORECASE,
)

_OCCURS_RE = re.compile(
    r"\bOCCURS\s+(?:(?P<min>\d+)\s+TO\s+)?(?P<count>\d+)\s*"
    r"(?:TIMES)?"
    r"(?:\s+DEPENDING\s+ON\s+"
    r"(?P<depending>[A-Za-z][A-Za-z0-9_-]*))?",
    re.IGNORECASE,
)

_VALUE_88_RE = re.compile(
    r"\bVALUE(?:S)?\s+(?:IS\s+|ARE\s+)?"
    r"(?P<values>.+?)(?:\s*\.?\s*$)",
    re.IGNORECASE,
)


# ---------------------------------------------------------------------------
# PIC string analysis helpers
# ---------------------------------------------------------------------------

def _expand_picture(pic: str) -> str:
    """Expand repetition factors in a PICTURE string."""
    result: list[str] = []
    i = 0
    while i < len(pic):
        ch = pic[i]
        if i + 1 < len(pic) and pic[i + 1] == "(":
            close = pic.index(")", i + 2)
            count = int(pic[i + 2:close])
            result.append(ch * count)
            i = close + 1
        else:
            result.append(ch)
            i += 1
    return "".join(result)


def _analyze_picture(
    pic_str: str,
) -> tuple[bool, int, int, str]:
    """Analyze a PIC string and return type information.

    Returns:
        Tuple of (signed, total_digits, decimal_positions, category).
    """
    pic = pic_str.strip().upper()

    signed = pic.startswith("S")
    if signed:
        pic = pic[1:]

    expanded = _expand_picture(pic)

    symbol_set = set(expanded.replace("V", ""))

    if symbol_set <= {"X"} or not symbol_set:
        return False, 0, 0, "alphanumeric"
    if symbol_set <= {"A"}:
        return False, 0, 0, "alphabetic"

    # Numeric
    if "V" in expanded:
        parts = expanded.split("V", 1)
        integer_digits = parts[0].count("9")
        decimal_digits = parts[1].count("9")
    else:
        integer_digits = expanded.count("9")
        decimal_digits = 0

    total_digits = integer_digits + decimal_digits
    return signed, total_digits, decimal_digits, "numeric"


def _compute_display_length(pic_str: str) -> int:
    """Compute the display length of a PIC string."""
    pic = pic_str.strip().upper()
    if pic.startswith("S"):
        pic = pic[1:]
    expanded = _expand_picture(pic)
    expanded = expanded.replace("V", "")
    return len(expanded)


# ---------------------------------------------------------------------------
# Byte length computation
# ---------------------------------------------------------------------------

def _compute_byte_length(
    pic_str: str | None,
    usage: str | None,
    total_digits: int,
    display_length: int,
    category: str,
) -> int:
    """Compute the storage size in bytes for a data item."""
    if pic_str is None:
        return 0  # Group item

    norm_usage = (usage or "DISPLAY").upper()

    if norm_usage in (
        "COMP",
        "COMP-4",
        "COMPUTATIONAL",
        "COMPUTATIONAL-4",
        "BINARY",
    ):
        if total_digits <= 4:
            return 2
        elif total_digits <= 9:
            return 4
        else:
            return 8

    elif norm_usage in (
        "COMP-3",
        "COMPUTATIONAL-3",
        "PACKED-DECIMAL",
    ):
        return math.ceil((total_digits + 1) / 2)

    else:
        return display_length


# ---------------------------------------------------------------------------
# Python type hint computation
# ---------------------------------------------------------------------------

def _compute_python_type(
    pic_str: str | None,
    usage: str | None,
    category: str,
    decimal_positions: int,
    occurs: int | None,
) -> str:
    """Compute the Python type hint for a data item."""
    if pic_str is None:
        return "dataclass"

    if category in ("alphanumeric", "alphabetic"):
        base_type = "str"
    elif decimal_positions > 0:
        base_type = "Decimal"
    else:
        base_type = "int"

    if occurs is not None:
        return f"list[{base_type}]"

    return base_type


# ---------------------------------------------------------------------------
# Value clause parsing
# ---------------------------------------------------------------------------

def _parse_value_clause(value_text: str) -> str:
    """Normalize a VALUE clause text."""
    val = value_text.strip().rstrip(".")

    if val.endswith("."):
        val = val[:-1].rstrip()

    if (
        (val.startswith("'") and val.endswith("'"))
        or (val.startswith('"') and val.endswith('"'))
    ) and len(val) >= 2:
        val = val[1:-1]

    return val


def _parse_88_values(value_text: str) -> list[dict]:
    """Parse a level-88 VALUE clause into a list of value entries."""
    val = value_text.strip().rstrip(".")

    thru_match = re.search(
        r"\bTHRU\b|\bTHROUGH\b", val, re.IGNORECASE
    )
    if thru_match:
        low = val[: thru_match.start()].strip().strip("'\"")
        high = val[thru_match.end() :].strip().strip("'\"")
        return [{"from": low, "to": high}]

    values: list[str] = []
    parts = re.findall(r"'[^']*'|\"[^\"]*\"|\S+", val)
    for part in parts:
        part = part.strip().strip(",")
        if not part:
            continue
        if (
            part.startswith("'") and part.endswith("'")
        ) or (part.startswith('"') and part.endswith('"')):
            values.append(part[1:-1])
        elif part.upper() not in (",", "OR"):
            values.append(part)

    return values


# ---------------------------------------------------------------------------
# Usage normalization
# ---------------------------------------------------------------------------

def _normalize_usage(usage_str: str) -> str:
    """Normalize USAGE clause values to canonical form."""
    u = usage_str.strip().upper()
    if u.startswith("COMPUTATIONAL"):
        u = u.replace("COMPUTATIONAL", "COMP")
    if u == "BINARY":
        u = "COMP"
    if u == "PACKED-DECIMAL":
        u = "COMP-3"
    return u


# ---------------------------------------------------------------------------
# Statement assembly (multi-line data items)
# ---------------------------------------------------------------------------

def _assemble_statements(
    lines: list[SourceLine],
) -> list[tuple[str, int, str | None]]:
    """Assemble multi-line COBOL data item statements."""
    statements: list[tuple[str, int, str | None]] = []
    current_text = ""
    current_line_num = 0
    current_copybook: str | None = None

    code_lines = [
        ln
        for ln in lines
        if not ln.is_comment and not ln.is_blank
    ]

    for line in code_lines:
        text = line.text.strip()
        if not text:
            continue

        if re.match(
            r"^\s*(?:FILE|WORKING-STORAGE|LOCAL-STORAGE"
            r"|LINKAGE)\s+SECTION\s*\.",
            text,
            re.IGNORECASE,
        ):
            continue

        level_match = re.match(r"^\s*(\d{1,2})\s+", text)

        if level_match:
            if current_text:
                statements.append((
                    current_text.strip(),
                    current_line_num,
                    current_copybook,
                ))
            current_text = text
            current_line_num = line.line_number
            current_copybook = line.copybook_source
        else:
            if current_text:
                current_text += " " + text
            else:
                continue

        if current_text.rstrip().endswith("."):
            statements.append((
                current_text.strip(),
                current_line_num,
                current_copybook,
            ))
            current_text = ""
            current_line_num = 0
            current_copybook = None

    if current_text:
        statements.append((
            current_text.strip(),
            current_line_num,
            current_copybook,
        ))

    return statements


# ---------------------------------------------------------------------------
# Main parser class
# ---------------------------------------------------------------------------

class DataDivisionParser:
    """Parses the DATA DIVISION of a COBOL program to extract data items."""

    def __init__(self) -> None:
        pass

    def parse(self, source: CobolSource) -> list[DataItem]:
        """Parse all data items from the DATA DIVISION."""
        all_items: list[DataItem] = []

        section_ranges = self._find_section_ranges(source)

        for section_name in (
            "FILE",
            "WORKING-STORAGE",
            "LINKAGE",
        ):
            if section_name not in section_ranges:
                continue

            start_idx, end_idx = section_ranges[section_name]
            section_lines = source.lines[start_idx:end_idx]

            if not section_lines:
                continue

            logger.info(
                "Parsing %s SECTION (%d lines)",
                section_name,
                len(section_lines),
            )
            items = self._parse_section(
                section_lines, section_name
            )
            all_items.extend(items)

        logger.info(
            "DATA DIVISION parsed: %d total data items "
            "(excluding level-88 conditions attached to parents)",
            len([
                item for item in all_items if item.level != 88
            ]),
        )

        return all_items

    def _find_section_ranges(
        self, source: CobolSource
    ) -> dict[str, tuple[int, int]]:
        """Identify DATA DIVISION section boundaries by list index."""
        _section_re = re.compile(
            r"^\s*(?P<name>FILE|WORKING-STORAGE|LOCAL-STORAGE"
            r"|LINKAGE)\s+SECTION\s*\.",
            re.IGNORECASE,
        )
        _division_re = re.compile(
            r"^\s*(?P<name>IDENTIFICATION|ENVIRONMENT|DATA"
            r"|PROCEDURE)\s+DIVISION",
            re.IGNORECASE,
        )

        section_starts: list[tuple[str, int]] = []
        procedure_idx = len(source.lines)

        for i, line in enumerate(source.lines):
            if line.is_comment or line.is_blank:
                continue

            div_match = _division_re.match(line.text)
            if div_match:
                div_name = div_match.group("name").upper()
                if div_name == "PROCEDURE":
                    procedure_idx = i
                    break

            m = _section_re.match(line.text)
            if m:
                sec_name = m.group("name").upper()
                section_starts.append((sec_name, i))

        if not section_starts:
            return {}

        sections: dict[str, tuple[int, int]] = {}
        for idx, (name, start) in enumerate(section_starts):
            if idx + 1 < len(section_starts):
                end = section_starts[idx + 1][1]
            else:
                end = procedure_idx
            sections[name] = (start, end)

        return sections

    def _parse_section(
        self, lines: list[SourceLine], section_name: str
    ) -> list[DataItem]:
        """Parse all data items in a single DATA DIVISION section."""
        statements = _assemble_statements(lines)
        if not statements:
            return []

        raw_items: list[DataItem] = []
        for stmt_text, line_num, copybook in statements:
            item = self._parse_statement(
                stmt_text, line_num, copybook, section_name
            )
            if item is not None:
                raw_items.append(item)

        if not raw_items:
            return []

        self._build_hierarchy(raw_items)
        self._attach_conditions(raw_items)
        self._compute_group_lengths(raw_items)

        return raw_items

    def _parse_statement(
        self,
        stmt: str,
        line_number: int,
        copybook_source: str | None,
        section: str,
    ) -> DataItem | None:
        """Parse a single data item statement into a DataItem."""
        text = stmt.rstrip(".")

        level_match = _LEVEL_RE.match(text)
        if not level_match:
            return None

        level = int(level_match.group("level"))
        name = level_match.group("name").upper()
        rest = level_match.group("rest")

        cb_source = None
        if copybook_source:
            cb_source = copybook_source
            if not cb_source.endswith(
                (".cpy", ".CPY", ".cbl", ".CBL")
            ):
                cb_source += ".cpy"

        # --- Parse PIC clause ---
        picture: str | None = None
        pic_match = _PIC_RE.search(rest)
        if pic_match:
            picture = pic_match.group("pic").upper()

        # --- Parse USAGE clause ---
        usage: str | None = None
        usage_match = _USAGE_RE.search(rest)
        if usage_match:
            usage = _normalize_usage(usage_match.group("usage"))
        else:
            standalone_match = _STANDALONE_USAGE_RE.search(rest)
            if standalone_match and (
                pic_match is None
                or standalone_match.start() > pic_match.end()
            ):
                usage = _normalize_usage(
                    standalone_match.group("usage")
                )

        if usage is None and level != 88:
            usage = "DISPLAY"

        # --- Parse REDEFINES clause ---
        redefines: str | None = None
        redef_match = _REDEFINES_RE.search(rest)
        if redef_match:
            redefines = redef_match.group("target").upper()

        # --- Parse OCCURS clause ---
        occurs: int | None = None
        occurs_depending_on: str | None = None
        occurs_match = _OCCURS_RE.search(rest)
        if occurs_match:
            occurs = int(occurs_match.group("count"))
            if occurs_match.group("depending"):
                occurs_depending_on = (
                    occurs_match.group("depending").upper()
                )

        # --- Parse VALUE clause ---
        value: str | None = None
        if level != 88:
            value_match = _VALUE_RE.search(rest)
            if value_match:
                value = _parse_value_clause(
                    value_match.group("value")
                )

        # --- Analyze PIC for type information ---
        signed = False
        decimal_positions = 0
        total_digits = 0
        display_length = 0
        category = "group"

        if picture:
            signed, total_digits, decimal_positions, category = (
                _analyze_picture(picture)
            )
            display_length = _compute_display_length(picture)

        # --- Compute byte length ---
        byte_length = _compute_byte_length(
            picture,
            usage,
            total_digits,
            display_length,
            category,
        )

        if occurs is not None and byte_length > 0:
            byte_length = byte_length * occurs

        # --- Compute Python type hint ---
        python_type = _compute_python_type(
            picture,
            usage,
            category,
            decimal_positions,
            occurs,
        )

        conditions_88: list[dict] = []

        value_88: list = []
        if level == 88:
            val_match = _VALUE_88_RE.search(rest)
            if val_match:
                value_88 = _parse_88_values(
                    val_match.group("values")
                )

        item = DataItem(
            name=name,
            level=level,
            picture=picture,
            usage=usage,
            value=value,
            redefines=redefines,
            occurs=occurs,
            occurs_depending_on=occurs_depending_on,
            signed=signed,
            decimal_positions=decimal_positions,
            byte_length=byte_length,
            section=section,
            copybook_source=cb_source,
            line_number=line_number,
            parent=None,
            children=[],
            conditions_88=conditions_88,
            python_type_hint=python_type,
        )

        if level == 88:
            item._values_88 = value_88  # type: ignore[attr-defined]

        return item

    def _build_hierarchy(self, items: list[DataItem]) -> None:
        """Build parent/children relationships based on level numbers."""
        parent_stack: list[tuple[int, int]] = []
        filler_counts: dict[str, int] = {}

        for i, item in enumerate(items):
            if item.level in (1, 77):
                item.parent = None
                parent_stack = [(item.level, i)]

            elif item.level == 88:
                for j in range(i - 1, -1, -1):
                    if items[j].level != 88:
                        item.parent = items[j].name
                        break

            elif item.level == 66:
                pass

            else:
                while (
                    parent_stack
                    and parent_stack[-1][0] >= item.level
                ):
                    parent_stack.pop()

                if parent_stack:
                    parent_idx = parent_stack[-1][1]
                    parent_item = items[parent_idx]
                    item.parent = parent_item.name

                    child_name = item.name
                    if child_name == "FILLER":
                        parent_key = parent_item.name
                        if parent_key not in filler_counts:
                            filler_counts[parent_key] = 0
                        filler_counts[parent_key] += 1
                        n = filler_counts[parent_key]
                        child_name = (
                            f"FILLER({parent_key}-{n})"
                        )
                        item.name = child_name

                    parent_item.children.append(child_name)

                parent_stack.append((item.level, i))

    def _attach_conditions(
        self, items: list[DataItem]
    ) -> None:
        """Attach level-88 conditions to their parent data items."""
        name_to_item: dict[str, DataItem] = {}
        for item in items:
            if item.level != 88:
                name_to_item[item.name] = item

        for item in items:
            if item.level != 88:
                continue

            values = getattr(item, "_values_88", [])
            if not values:
                continue

            parent_name = item.parent
            if parent_name is None:
                continue

            parent = name_to_item.get(parent_name)
            if parent is not None:
                parent.conditions_88.append({
                    "name": item.name,
                    "values": values,
                })

    def _compute_group_lengths(
        self, items: list[DataItem]
    ) -> None:
        """Compute byte lengths for group items by summing children."""
        name_to_items: dict[str, list[DataItem]] = {}
        for item in items:
            if item.name not in name_to_items:
                name_to_items[item.name] = []
            name_to_items[item.name].append(item)

        for item in reversed(items):
            if item.level == 88:
                continue

            if item.picture is not None:
                continue

            if not item.children:
                continue

            total = 0
            child_sources: set[str | None] = set()
            needs_source = item.copybook_source is None
            for child_name in item.children:
                if child_name in name_to_items:
                    for child_item in name_to_items[
                        child_name
                    ]:
                        if (
                            child_item.parent == item.name
                            and child_item.level != 88
                        ):
                            total += child_item.byte_length
                            if needs_source:
                                child_sources.add(
                                    child_item.copybook_source
                                )
                            break

            item.byte_length = total

            if needs_source and len(child_sources) == 1:
                sole_source = next(iter(child_sources))
                if sole_source is not None:
                    item.copybook_source = sole_source


# ---------------------------------------------------------------------------
# Convenience function
# ---------------------------------------------------------------------------

def parse_data_division(source: CobolSource) -> list[DataItem]:
    """Convenience function to parse the DATA DIVISION."""
    parser = DataDivisionParser()
    return parser.parse(source)
