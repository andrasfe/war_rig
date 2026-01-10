"""Copybook preprocessor for extracting record layouts.

This module provides deterministic extraction of structural elements from
COBOL copybooks, including:

- Record layout hierarchy
- Field definitions (level, name, PIC, usage)
- REDEFINES relationships
- OCCURS clauses

The extracted information serves as hints for the Scribe agent.
"""

import re
from typing import Any

from pydantic import BaseModel, Field

from war_rig.models.templates import FileType, UsageType
from war_rig.preprocessors.base import (
    BasePreprocessor,
    PreprocessorResult,
    SourceLocation,
)


class FieldDefinition(BaseModel):
    """Information about a field in a copybook."""

    level: int = Field(..., ge=1, le=88, description="COBOL level number")
    name: str = Field(..., description="Field name")
    picture: str | None = Field(default=None, description="PIC clause")
    usage: UsageType = Field(default=UsageType.DISPLAY, description="Usage type")
    value: str | None = Field(default=None, description="VALUE clause")
    redefines: str | None = Field(default=None, description="REDEFINES target")
    occurs: int | None = Field(default=None, description="OCCURS count")
    occurs_depending: str | None = Field(default=None, description="DEPENDING ON field")
    line: int = Field(..., ge=1, description="Line number")

    @property
    def is_group(self) -> bool:
        """Check if this is a group item (no PIC clause).

        Returns:
            True if this is a group item.
        """
        return self.picture is None and self.level not in {66, 88}

    @property
    def is_elementary(self) -> bool:
        """Check if this is an elementary item (has PIC clause).

        Returns:
            True if this is an elementary item.
        """
        return self.picture is not None

    @property
    def is_condition(self) -> bool:
        """Check if this is a condition name (level 88).

        Returns:
            True if this is a level 88 item.
        """
        return self.level == 88


class CopybookStructure(PreprocessorResult):
    """Structural information extracted from a copybook.

    Extends PreprocessorResult with copybook-specific structural elements.
    """

    file_type: FileType = Field(default=FileType.COPYBOOK)
    copybook_name: str | None = Field(
        default=None,
        description="Name of the copybook",
    )
    fields: list[FieldDefinition] = Field(
        default_factory=list,
        description="Field definitions",
    )
    root_items: list[str] = Field(
        default_factory=list,
        description="Level 01 item names",
    )
    total_fields: int = Field(
        default=0,
        ge=0,
        description="Total number of fields",
    )
    max_level: int = Field(
        default=0,
        ge=0,
        description="Maximum level number used",
    )
    has_redefines: bool = Field(
        default=False,
        description="Whether copybook contains REDEFINES",
    )
    has_occurs: bool = Field(
        default=False,
        description="Whether copybook contains OCCURS",
    )


class CopybookPreprocessor(BasePreprocessor[CopybookStructure]):
    """Preprocessor for COBOL copybooks.

    Extracts structural information using pattern matching and simple parsing.
    Does not use LLMs - all extraction is deterministic.

    Example:
        preprocessor = CopybookPreprocessor()
        structure = preprocessor.process(copybook_source, "RECORD.cpy")
        print(f"Found {len(structure.fields)} fields")
    """

    # Regex patterns for copybook elements
    FIELD_PATTERN = re.compile(
        r"^\s*(\d{1,2})\s+([\w-]+)\s*",
        re.MULTILINE,
    )
    PIC_PATTERN = re.compile(
        r"PIC(?:TURE)?\s+(?:IS\s+)?([^\s.]+)",
        re.IGNORECASE,
    )
    USAGE_PATTERN = re.compile(
        r"(?:USAGE\s+(?:IS\s+)?)?(?:COMP(?:-[0-9])?|BINARY|PACKED-DECIMAL|DISPLAY|POINTER|INDEX)",
        re.IGNORECASE,
    )
    VALUE_PATTERN = re.compile(
        r"VALUE\s+(?:IS\s+)?([^\s.]+|\"[^\"]*\"|'[^']*')",
        re.IGNORECASE,
    )
    REDEFINES_PATTERN = re.compile(
        r"REDEFINES\s+([\w-]+)",
        re.IGNORECASE,
    )
    OCCURS_PATTERN = re.compile(
        r"OCCURS\s+(\d+)(?:\s+TIMES)?",
        re.IGNORECASE,
    )
    OCCURS_DEPENDING_PATTERN = re.compile(
        r"DEPENDING\s+ON\s+([\w-]+)",
        re.IGNORECASE,
    )

    def get_supported_extensions(self) -> list[str]:
        """Get copybook file extensions.

        Returns:
            List of copybook file extensions.
        """
        return [".cpy", ".CPY", ".copy", ".COPY", ".cpb", ".CPB"]

    def detect_file_type(self, source: str, file_name: str) -> bool:
        """Detect if source is a copybook.

        Args:
            source: Source code content.
            file_name: Name of the source file.

        Returns:
            True if the source appears to be a copybook.
        """
        # Copybooks typically don't have PROCEDURE DIVISION
        # and do have data definitions
        source_upper = source.upper()

        has_procedure = "PROCEDURE DIVISION" in source_upper
        has_data_defs = bool(self.FIELD_PATTERN.search(source))

        return has_data_defs and not has_procedure

    def process(self, source: str, file_name: str) -> CopybookStructure:
        """Process copybook source and extract structure.

        Args:
            source: Copybook source code content.
            file_name: Name of the source file.

        Returns:
            CopybookStructure with extracted elements.
        """
        lines = source.split("\n")
        result = CopybookStructure(
            file_name=file_name,
            line_count=len(lines),
        )

        # Extract copybook name from file name
        import os
        base_name = os.path.splitext(os.path.basename(file_name))[0]
        result.copybook_name = base_name
        result.program_id = base_name

        # Extract field definitions
        result.fields = self._extract_fields(source, lines)
        result.total_fields = len(result.fields)

        # Find root items (level 01)
        result.root_items = [
            f.name for f in result.fields
            if f.level == 1
        ]

        # Calculate max level
        if result.fields:
            result.max_level = max(f.level for f in result.fields)

        # Check for REDEFINES and OCCURS
        result.has_redefines = any(f.redefines for f in result.fields)
        result.has_occurs = any(f.occurs for f in result.fields)

        return result

    def _extract_fields(self, source: str, lines: list[str]) -> list[FieldDefinition]:
        """Extract field definitions from copybook.

        Args:
            source: Copybook source code.
            lines: Source split into lines.

        Returns:
            List of FieldDefinition for each field found.
        """
        fields: list[FieldDefinition] = []

        # Build a map of complete statements (handling continuation)
        statements = self._build_statements(lines)

        for line_num, statement in statements:
            field = self._parse_field(statement, line_num)
            if field:
                fields.append(field)

        return fields

    def _build_statements(self, lines: list[str]) -> list[tuple[int, str]]:
        """Build complete statements from lines, handling continuation.

        COBOL statements can span multiple lines. This method combines
        continued lines into complete statements.

        Args:
            lines: Source split into lines.

        Returns:
            List of (line_number, statement) tuples.
        """
        statements: list[tuple[int, str]] = []
        current_statement = ""
        current_line = 0

        for i, line in enumerate(lines, start=1):
            # Skip comment lines (column 7 is *)
            if len(line) > 6 and line[6] == "*":
                continue

            # Skip empty lines
            stripped = line.strip()
            if not stripped:
                continue

            # Check if this is a continuation line (column 7 is -)
            is_continuation = len(line) > 6 and line[6] == "-"

            if is_continuation and current_statement:
                # Add to current statement
                current_statement += " " + stripped
            else:
                # Save previous statement if exists
                if current_statement:
                    statements.append((current_line, current_statement))

                # Start new statement
                current_statement = stripped
                current_line = i

            # Check if statement is complete (ends with .)
            if current_statement.rstrip().endswith("."):
                statements.append((current_line, current_statement))
                current_statement = ""

        # Don't forget last statement
        if current_statement:
            statements.append((current_line, current_statement))

        return statements

    def _parse_field(self, statement: str, line_num: int) -> FieldDefinition | None:
        """Parse a single field definition.

        Args:
            statement: The complete statement text.
            line_num: Line number of the statement.

        Returns:
            FieldDefinition if this is a valid field, None otherwise.
        """
        # Try to match field pattern
        match = self.FIELD_PATTERN.match(statement)
        if not match:
            return None

        level = int(match.group(1))
        name = match.group(2)

        # Skip FILLER items if desired (optional)
        # if name.upper() == "FILLER":
        #     return None

        # Extract PIC clause
        picture = None
        pic_match = self.PIC_PATTERN.search(statement)
        if pic_match:
            picture = pic_match.group(1)

        # Extract USAGE
        usage = UsageType.DISPLAY
        usage_match = self.USAGE_PATTERN.search(statement)
        if usage_match:
            usage_text = usage_match.group(0).upper()
            if "COMP-3" in usage_text or "PACKED" in usage_text:
                usage = UsageType.COMP_3
            elif "COMP-1" in usage_text:
                usage = UsageType.COMP_1
            elif "COMP-2" in usage_text:
                usage = UsageType.COMP_2
            elif "COMP-4" in usage_text or "BINARY" in usage_text:
                usage = UsageType.COMP_4
            elif "COMP-5" in usage_text:
                usage = UsageType.COMP_5
            elif "COMP" in usage_text:
                usage = UsageType.COMP
            elif "POINTER" in usage_text:
                usage = UsageType.POINTER
            elif "INDEX" in usage_text:
                usage = UsageType.INDEX

        # Extract VALUE
        value = None
        value_match = self.VALUE_PATTERN.search(statement)
        if value_match:
            value = value_match.group(1)

        # Extract REDEFINES
        redefines = None
        redefines_match = self.REDEFINES_PATTERN.search(statement)
        if redefines_match:
            redefines = redefines_match.group(1)

        # Extract OCCURS
        occurs = None
        occurs_match = self.OCCURS_PATTERN.search(statement)
        if occurs_match:
            occurs = int(occurs_match.group(1))

        # Extract DEPENDING ON
        occurs_depending = None
        depending_match = self.OCCURS_DEPENDING_PATTERN.search(statement)
        if depending_match:
            occurs_depending = depending_match.group(1)

        return FieldDefinition(
            level=level,
            name=name,
            picture=picture,
            usage=usage,
            value=value,
            redefines=redefines,
            occurs=occurs,
            occurs_depending=occurs_depending,
            line=line_num,
        )
