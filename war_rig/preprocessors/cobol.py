"""COBOL preprocessor for extracting structural information.

This module provides deterministic extraction of structural elements from
COBOL source code, including:

- Program identification
- Division structure
- Paragraph definitions
- PERFORM statements
- CALL statements
- COPY statements
- File definitions
- SQL statements (DB2)
- CICS commands

The extracted information serves as hints for the Scribe agent.
"""

import re

from pydantic import Field

from war_rig.models.templates import FileType
from war_rig.preprocessors.base import (
    BasePreprocessor,
    CallInfo,
    CICSCommandInfo,
    CopybookInfo,
    FileInfo,
    ParagraphInfo,
    PerformInfo,
    PreprocessorResult,
    SourceLocation,
    SQLStatementInfo,
)


class COBOLStructure(PreprocessorResult):
    """Structural information extracted from COBOL source code.

    Extends PreprocessorResult with COBOL-specific structural elements.
    """

    file_type: FileType = Field(default=FileType.COBOL)
    divisions: list[str] = Field(
        default_factory=list,
        description="COBOL divisions present (IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE)",
    )
    paragraphs: list[ParagraphInfo] = Field(
        default_factory=list,
        description="Paragraph/section definitions",
    )
    performs: list[PerformInfo] = Field(
        default_factory=list,
        description="PERFORM statements",
    )
    calls: list[CallInfo] = Field(
        default_factory=list,
        description="CALL statements",
    )
    copybooks: list[CopybookInfo] = Field(
        default_factory=list,
        description="COPY statements",
    )
    files: list[FileInfo] = Field(
        default_factory=list,
        description="File definitions",
    )
    sql_statements: list[SQLStatementInfo] = Field(
        default_factory=list,
        description="Embedded SQL statements",
    )
    cics_commands: list[CICSCommandInfo] = Field(
        default_factory=list,
        description="CICS commands",
    )
    working_storage_items: list[str] = Field(
        default_factory=list,
        description="Level 01 items in WORKING-STORAGE",
    )
    linkage_items: list[str] = Field(
        default_factory=list,
        description="Level 01 items in LINKAGE SECTION",
    )


class COBOLPreprocessor(BasePreprocessor[COBOLStructure]):
    """Preprocessor for COBOL source code.

    Extracts structural information using pattern matching and simple parsing.
    Does not use LLMs - all extraction is deterministic.

    Example:
        preprocessor = COBOLPreprocessor()
        structure = preprocessor.process(cobol_source, "PROGRAM.cbl")
        print(f"Found {len(structure.paragraphs)} paragraphs")
    """

    # Regex patterns for COBOL elements
    PROGRAM_ID_PATTERN = re.compile(
        r"PROGRAM-ID\.\s+(\w+)",
        re.IGNORECASE,
    )
    DIVISION_PATTERN = re.compile(
        r"^[\s\d]*(\w+)\s+DIVISION\s*\.",
        re.IGNORECASE | re.MULTILINE,
    )
    PARAGRAPH_PATTERN = re.compile(
        r"^[\s\d]*(\d{4}-[\w-]+|\w[\w-]*)\s*\.\s*$",
        re.MULTILINE,
    )
    PERFORM_PATTERN = re.compile(
        r"PERFORM\s+([\w-]+)(?:\s+THRU\s+([\w-]+))?",
        re.IGNORECASE,
    )
    CALL_PATTERN = re.compile(
        r"CALL\s+(?:\"([^\"]+)\"|'([^']+)'|([\w-]+))",
        re.IGNORECASE,
    )
    CALL_USING_PATTERN = re.compile(
        r"CALL\s+(?:\"[^\"]+\"|'[^']+'|[\w-]+)\s+USING\s+(.*?)(?:END-CALL|\.|$)",
        re.IGNORECASE | re.DOTALL,
    )
    COPY_PATTERN = re.compile(
        r"COPY\s+([\w-]+)",
        re.IGNORECASE,
    )
    SELECT_PATTERN = re.compile(
        r"SELECT\s+([\w-]+)\s+ASSIGN",
        re.IGNORECASE,
    )
    FD_PATTERN = re.compile(
        r"FD\s+([\w-]+)",
        re.IGNORECASE,
    )
    SQL_PATTERN = re.compile(
        r"EXEC\s+SQL\s+(SELECT|INSERT|UPDATE|DELETE|DECLARE\s+\w+\s+CURSOR)",
        re.IGNORECASE,
    )
    SQL_TABLE_PATTERN = re.compile(
        r"(?:FROM|INTO|UPDATE|TABLE)\s+([\w.]+)",
        re.IGNORECASE,
    )
    CICS_PATTERN = re.compile(
        r"EXEC\s+CICS\s+(\w+)",
        re.IGNORECASE,
    )
    CICS_RESOURCE_PATTERN = re.compile(
        r"(?:FILE|MAP|MAPSET|QUEUE|PROGRAM|TRANSID)\s*\(\s*['\"]?([\w-]+)['\"]?\s*\)",
        re.IGNORECASE,
    )
    LEVEL_01_PATTERN = re.compile(
        r"^\s*01\s+([\w-]+)\s*\.",
        re.MULTILINE,
    )

    def get_supported_extensions(self) -> list[str]:
        """Get COBOL file extensions.

        Returns:
            List of COBOL file extensions.
        """
        return [".cbl", ".cob", ".CBL", ".COB", ".cobol", ".COBOL"]

    def detect_file_type(self, source: str, file_name: str) -> bool:
        """Detect if source is COBOL code.

        Args:
            source: Source code content.
            file_name: Name of the source file.

        Returns:
            True if the source appears to be COBOL.
        """
        # Check for COBOL indicators
        indicators = [
            "IDENTIFICATION DIVISION",
            "PROCEDURE DIVISION",
            "WORKING-STORAGE SECTION",
            "PROGRAM-ID",
        ]
        source_upper = source.upper()
        return any(indicator in source_upper for indicator in indicators)

    def process(self, source: str, file_name: str) -> COBOLStructure:
        """Process COBOL source and extract structure.

        Args:
            source: COBOL source code content.
            file_name: Name of the source file.

        Returns:
            COBOLStructure with extracted elements.
        """
        lines = source.split("\n")
        result = COBOLStructure(
            file_name=file_name,
            line_count=len(lines),
        )

        # Extract program ID
        result.program_id = self._extract_program_id(source)

        # Extract divisions
        result.divisions = self._extract_divisions(source)

        # Extract paragraphs
        result.paragraphs = self._extract_paragraphs(source, lines)

        # Extract PERFORM statements
        result.performs = self._extract_performs(source, lines, result.paragraphs)

        # Extract CALL statements
        result.calls = self._extract_calls(source, lines)

        # Extract COPY statements
        result.copybooks = self._extract_copybooks(source, lines)

        # Extract file definitions
        result.files = self._extract_files(source, lines)

        # Extract SQL statements
        result.sql_statements = self._extract_sql(source, lines)

        # Extract CICS commands
        result.cics_commands = self._extract_cics(source, lines)

        # Extract level 01 items
        result.working_storage_items = self._extract_working_storage(source)
        result.linkage_items = self._extract_linkage(source)

        return result

    def _extract_program_id(self, source: str) -> str | None:
        """Extract PROGRAM-ID from source.

        Args:
            source: COBOL source code.

        Returns:
            Program ID if found, None otherwise.
        """
        match = self.PROGRAM_ID_PATTERN.search(source)
        return match.group(1) if match else None

    def _extract_divisions(self, source: str) -> list[str]:
        """Extract division names from source.

        Args:
            source: COBOL source code.

        Returns:
            List of division names found.
        """
        matches = self.DIVISION_PATTERN.findall(source)
        return [m.upper() for m in matches]

    def _extract_paragraphs(
        self,
        source: str,
        lines: list[str],
    ) -> list[ParagraphInfo]:
        """Extract paragraph definitions from source.

        Args:
            source: COBOL source code.
            lines: Source split into lines.

        Returns:
            List of ParagraphInfo for each paragraph found.
        """
        paragraphs: list[ParagraphInfo] = []

        # Find PROCEDURE DIVISION to start looking for paragraphs
        proc_div_line = 0
        for i, line in enumerate(lines):
            if "PROCEDURE DIVISION" in line.upper():
                proc_div_line = i
                break

        # Look for paragraph names after PROCEDURE DIVISION
        for i, line in enumerate(lines[proc_div_line:], start=proc_div_line + 1):
            # Skip comment lines (column 7 is *)
            stripped = line.lstrip()
            if stripped.startswith("*"):
                continue

            # Look for paragraph pattern
            match = self.PARAGRAPH_PATTERN.match(line)
            if match:
                para_name = match.group(1)
                # Skip common COBOL keywords that might match
                if para_name.upper() not in {
                    "END",
                    "IF",
                    "ELSE",
                    "PERFORM",
                    "MOVE",
                    "ADD",
                    "SUBTRACT",
                    "MULTIPLY",
                    "DIVIDE",
                    "COMPUTE",
                    "STOP",
                    "GOBACK",
                }:
                    paragraphs.append(ParagraphInfo(
                        name=para_name,
                        location=SourceLocation(start_line=i),
                    ))

        # Set end lines based on next paragraph start
        for i, para in enumerate(paragraphs[:-1]):
            para.location.end_line = paragraphs[i + 1].location.start_line - 1

        return paragraphs

    def _extract_performs(
        self,
        source: str,
        lines: list[str],
        paragraphs: list[ParagraphInfo],
    ) -> list[PerformInfo]:
        """Extract PERFORM statements from source.

        Args:
            source: COBOL source code.
            lines: Source split into lines.
            paragraphs: Previously extracted paragraphs.

        Returns:
            List of PerformInfo for each PERFORM found.
        """
        performs: list[PerformInfo] = []
        paragraph_map = {p.name: p for p in paragraphs}

        for i, line in enumerate(lines, start=1):
            # Skip comment lines
            stripped = line.lstrip()
            if stripped.startswith("*"):
                continue

            for match in self.PERFORM_PATTERN.finditer(line):
                to_para = match.group(1)
                thru_para = match.group(2)

                # Find which paragraph contains this PERFORM
                from_para = "UNKNOWN"
                for para in paragraphs:
                    if para.location.start_line <= i:
                        if para.location.end_line is None or i <= para.location.end_line:
                            from_para = para.name
                            break

                performs.append(PerformInfo(
                    from_paragraph=from_para,
                    to_paragraph=to_para,
                    thru_paragraph=thru_para,
                    line=i,
                ))

        return performs

    def _extract_calls(self, source: str, lines: list[str]) -> list[CallInfo]:
        """Extract CALL statements from source.

        Args:
            source: COBOL source code.
            lines: Source split into lines.

        Returns:
            List of CallInfo for each CALL found.
        """
        calls: list[CallInfo] = []

        for i, line in enumerate(lines, start=1):
            stripped = line.lstrip()
            if stripped.startswith("*"):
                continue

            match = self.CALL_PATTERN.search(line)
            if match:
                # Get program name from whichever group matched
                program = match.group(1) or match.group(2) or match.group(3)
                is_dynamic = match.group(3) is not None  # No quotes = dynamic

                # Look for USING clause
                using: list[str] = []
                using_match = self.CALL_USING_PATTERN.search(line)
                if using_match:
                    using_text = using_match.group(1)
                    # Parse the USING items
                    using = [
                        item.strip()
                        for item in re.split(r"\s+", using_text)
                        if item.strip() and item.upper() not in {"BY", "REFERENCE", "CONTENT", "VALUE"}
                    ]

                calls.append(CallInfo(
                    program=program,
                    using=using,
                    line=i,
                    is_dynamic=is_dynamic,
                ))

        return calls

    def _extract_copybooks(self, source: str, lines: list[str]) -> list[CopybookInfo]:
        """Extract COPY statements from source.

        Args:
            source: COBOL source code.
            lines: Source split into lines.

        Returns:
            List of CopybookInfo for each COPY found.
        """
        copybooks: list[CopybookInfo] = []

        for i, line in enumerate(lines, start=1):
            stripped = line.lstrip()
            if stripped.startswith("*"):
                continue

            match = self.COPY_PATTERN.search(line)
            if match:
                copybooks.append(CopybookInfo(
                    name=match.group(1),
                    line=i,
                ))

        return copybooks

    def _extract_files(self, source: str, lines: list[str]) -> list[FileInfo]:
        """Extract file definitions from source.

        Args:
            source: COBOL source code.
            lines: Source split into lines.

        Returns:
            List of FileInfo for each file found.
        """
        files: list[FileInfo] = []

        for i, line in enumerate(lines, start=1):
            stripped = line.lstrip()
            if stripped.startswith("*"):
                continue

            # Check for SELECT statement
            match = self.SELECT_PATTERN.search(line)
            if match:
                file_name = match.group(1)

                # Try to determine organization
                org = None
                if "INDEXED" in line.upper():
                    org = "INDEXED"
                elif "SEQUENTIAL" in line.upper():
                    org = "SEQUENTIAL"
                elif "RELATIVE" in line.upper():
                    org = "RELATIVE"

                files.append(FileInfo(
                    name=file_name,
                    organization=org,
                    line=i,
                ))

        return files

    def _extract_sql(self, source: str, lines: list[str]) -> list[SQLStatementInfo]:
        """Extract embedded SQL statements from source.

        Args:
            source: COBOL source code.
            lines: Source split into lines.

        Returns:
            List of SQLStatementInfo for each SQL statement found.
        """
        sql_statements: list[SQLStatementInfo] = []

        for i, line in enumerate(lines, start=1):
            stripped = line.lstrip()
            if stripped.startswith("*"):
                continue

            match = self.SQL_PATTERN.search(line)
            if match:
                operation = match.group(1).upper()
                if "CURSOR" in operation:
                    operation = "CURSOR"

                # Try to find table name
                table = None
                table_match = self.SQL_TABLE_PATTERN.search(line)
                if table_match:
                    table = table_match.group(1)

                sql_statements.append(SQLStatementInfo(
                    operation=operation,
                    table=table,
                    line=i,
                ))

        return sql_statements

    def _extract_cics(self, source: str, lines: list[str]) -> list[CICSCommandInfo]:
        """Extract CICS commands from source.

        Args:
            source: COBOL source code.
            lines: Source split into lines.

        Returns:
            List of CICSCommandInfo for each CICS command found.
        """
        cics_commands: list[CICSCommandInfo] = []

        for i, line in enumerate(lines, start=1):
            stripped = line.lstrip()
            if stripped.startswith("*"):
                continue

            match = self.CICS_PATTERN.search(line)
            if match:
                command = match.group(1).upper()

                # Try to find resource
                resource = None
                resource_match = self.CICS_RESOURCE_PATTERN.search(line)
                if resource_match:
                    resource = resource_match.group(1)

                cics_commands.append(CICSCommandInfo(
                    command=command,
                    resource=resource,
                    line=i,
                ))

        return cics_commands

    def _extract_working_storage(self, source: str) -> list[str]:
        """Extract level 01 items from WORKING-STORAGE SECTION.

        Args:
            source: COBOL source code.

        Returns:
            List of level 01 item names.
        """
        # Find WORKING-STORAGE SECTION
        ws_start = source.upper().find("WORKING-STORAGE SECTION")
        if ws_start == -1:
            return []

        # Find end of WORKING-STORAGE (next section or division)
        ws_end = len(source)
        for marker in ["LINKAGE SECTION", "PROCEDURE DIVISION", "LOCAL-STORAGE SECTION"]:
            pos = source.upper().find(marker, ws_start + 1)
            if pos != -1 and pos < ws_end:
                ws_end = pos

        ws_section = source[ws_start:ws_end]
        matches = self.LEVEL_01_PATTERN.findall(ws_section)
        return [m for m in matches if m.upper() != "FILLER"]

    def _extract_linkage(self, source: str) -> list[str]:
        """Extract level 01 items from LINKAGE SECTION.

        Args:
            source: COBOL source code.

        Returns:
            List of level 01 item names.
        """
        # Find LINKAGE SECTION
        ls_start = source.upper().find("LINKAGE SECTION")
        if ls_start == -1:
            return []

        # Find end of LINKAGE (next section or division)
        ls_end = len(source)
        for marker in ["PROCEDURE DIVISION", "LOCAL-STORAGE SECTION"]:
            pos = source.upper().find(marker, ls_start + 1)
            if pos != -1 and pos < ls_end:
                ls_end = pos

        ls_section = source[ls_start:ls_end]
        matches = self.LEVEL_01_PATTERN.findall(ls_section)
        return [m for m in matches if m.upper() != "FILLER"]
