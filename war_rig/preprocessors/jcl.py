"""JCL preprocessor for extracting job structure.

This module provides deterministic extraction of structural elements from
JCL (Job Control Language) source, including:

- Job name and parameters
- EXEC steps (programs and procedures)
- DD statements
- Dataset references

The extracted information serves as hints for the Scribe agent.
"""

import re
from typing import Any

from pydantic import BaseModel, Field

from war_rig.models.templates import FileType
from war_rig.preprocessors.base import (
    BasePreprocessor,
    PreprocessorResult,
    SourceLocation,
)


class JCLStepInfo(BaseModel):
    """Information about a JCL EXEC step."""

    name: str = Field(..., description="Step name")
    program: str | None = Field(default=None, description="PGM= value")
    proc: str | None = Field(default=None, description="PROC= value")
    condition: str | None = Field(default=None, description="COND= value")
    line: int = Field(..., ge=1, description="Line number")


class JCLDDInfo(BaseModel):
    """Information about a DD statement."""

    step_name: str = Field(..., description="Step this DD belongs to")
    dd_name: str = Field(..., description="DD name")
    dataset: str | None = Field(default=None, description="DSN= value")
    disposition: str | None = Field(default=None, description="DISP= value")
    sysout: str | None = Field(default=None, description="SYSOUT class")
    line: int = Field(..., ge=1, description="Line number")


class JCLStructure(PreprocessorResult):
    """Structural information extracted from JCL source.

    Extends PreprocessorResult with JCL-specific structural elements.
    """

    file_type: FileType = Field(default=FileType.JCL)
    job_name: str | None = Field(
        default=None,
        description="Job name from JOB card",
    )
    job_class: str | None = Field(
        default=None,
        description="Job execution class",
    )
    msgclass: str | None = Field(
        default=None,
        description="Message output class",
    )
    steps: list[JCLStepInfo] = Field(
        default_factory=list,
        description="EXEC steps",
    )
    dd_statements: list[JCLDDInfo] = Field(
        default_factory=list,
        description="DD statements",
    )
    procs_called: list[str] = Field(
        default_factory=list,
        description="Procedures called",
    )
    datasets_referenced: list[str] = Field(
        default_factory=list,
        description="All datasets referenced",
    )
    include_members: list[str] = Field(
        default_factory=list,
        description="INCLUDE members",
    )


class JCLPreprocessor(BasePreprocessor[JCLStructure]):
    """Preprocessor for JCL source code.

    Extracts structural information using pattern matching and simple parsing.
    Does not use LLMs - all extraction is deterministic.

    Example:
        preprocessor = JCLPreprocessor()
        structure = preprocessor.process(jcl_source, "JOB.jcl")
        print(f"Found {len(structure.steps)} steps")
    """

    # Regex patterns for JCL elements
    JOB_PATTERN = re.compile(
        r"^//(\w+)\s+JOB\s",
        re.MULTILINE,
    )
    JOB_CLASS_PATTERN = re.compile(
        r"CLASS=(\w)",
        re.IGNORECASE,
    )
    MSGCLASS_PATTERN = re.compile(
        r"MSGCLASS=(\w)",
        re.IGNORECASE,
    )
    EXEC_PATTERN = re.compile(
        r"^//(\w+)\s+EXEC\s+(?:PGM=(\w+)|PROC=(\w+)|(\w+))",
        re.MULTILINE | re.IGNORECASE,
    )
    COND_PATTERN = re.compile(
        r"COND=(\([^)]+\)|\w+)",
        re.IGNORECASE,
    )
    DD_PATTERN = re.compile(
        r"^//(\w+)\s+DD\s",
        re.MULTILINE,
    )
    DSN_PATTERN = re.compile(
        r"DSN=([^\s,]+)",
        re.IGNORECASE,
    )
    DISP_PATTERN = re.compile(
        r"DISP=(\([^)]+\)|\w+)",
        re.IGNORECASE,
    )
    SYSOUT_PATTERN = re.compile(
        r"SYSOUT=(\*|\w)",
        re.IGNORECASE,
    )
    INCLUDE_PATTERN = re.compile(
        r"^//\s*INCLUDE\s+MEMBER=(\w+)",
        re.MULTILINE | re.IGNORECASE,
    )

    def get_supported_extensions(self) -> list[str]:
        """Get JCL file extensions.

        Returns:
            List of JCL file extensions.
        """
        return [".jcl", ".JCL", ".job", ".JOB", ".proc", ".PROC"]

    def detect_file_type(self, source: str, file_name: str) -> bool:
        """Detect if source is JCL.

        Args:
            source: Source code content.
            file_name: Name of the source file.

        Returns:
            True if the source appears to be JCL.
        """
        # Check for JCL indicators
        indicators = [
            "// JOB ",
            "//JOB ",
            " EXEC PGM=",
            " EXEC PROC=",
            " DD ",
        ]
        source_upper = source.upper()
        return any(indicator in source_upper for indicator in indicators)

    def process(self, source: str, file_name: str) -> JCLStructure:
        """Process JCL source and extract structure.

        Args:
            source: JCL source code content.
            file_name: Name of the source file.

        Returns:
            JCLStructure with extracted elements.
        """
        lines = source.split("\n")
        result = JCLStructure(
            file_name=file_name,
            line_count=len(lines),
        )

        # Extract job information
        self._extract_job_info(source, result)

        # Extract steps
        result.steps = self._extract_steps(source, lines)

        # Extract DD statements
        result.dd_statements = self._extract_dd_statements(source, lines, result.steps)

        # Extract procedures called
        result.procs_called = list(set(
            step.proc for step in result.steps
            if step.proc is not None
        ))

        # Extract datasets
        result.datasets_referenced = self._extract_datasets(result.dd_statements)

        # Extract INCLUDEs
        result.include_members = self._extract_includes(source)

        # Set program_id to job_name for consistency
        result.program_id = result.job_name

        return result

    def _extract_job_info(self, source: str, result: JCLStructure) -> None:
        """Extract JOB card information.

        Args:
            source: JCL source code.
            result: JCLStructure to populate.
        """
        # Find job name
        match = self.JOB_PATTERN.search(source)
        if match:
            result.job_name = match.group(1)

        # Find job class
        match = self.JOB_CLASS_PATTERN.search(source)
        if match:
            result.job_class = match.group(1)

        # Find msgclass
        match = self.MSGCLASS_PATTERN.search(source)
        if match:
            result.msgclass = match.group(1)

    def _extract_steps(self, source: str, lines: list[str]) -> list[JCLStepInfo]:
        """Extract EXEC steps from source.

        Args:
            source: JCL source code.
            lines: Source split into lines.

        Returns:
            List of JCLStepInfo for each step found.
        """
        steps: list[JCLStepInfo] = []

        for i, line in enumerate(lines, start=1):
            match = self.EXEC_PATTERN.match(line)
            if match:
                step_name = match.group(1)
                program = match.group(2)
                proc = match.group(3) or match.group(4)

                # If no explicit PGM= or PROC=, it's a proc reference
                if not program and proc:
                    # Check if it looks like a program name
                    if proc.upper() in {"SORT", "IDCAMS", "IEBGENER", "IEBCOPY"}:
                        program = proc
                        proc = None

                # Look for COND parameter
                condition = None
                cond_match = self.COND_PATTERN.search(line)
                if cond_match:
                    condition = cond_match.group(1)

                steps.append(JCLStepInfo(
                    name=step_name,
                    program=program,
                    proc=proc if not program else None,
                    condition=condition,
                    line=i,
                ))

        return steps

    def _extract_dd_statements(
        self,
        source: str,
        lines: list[str],
        steps: list[JCLStepInfo],
    ) -> list[JCLDDInfo]:
        """Extract DD statements from source.

        Args:
            source: JCL source code.
            lines: Source split into lines.
            steps: Previously extracted steps.

        Returns:
            List of JCLDDInfo for each DD found.
        """
        dd_statements: list[JCLDDInfo] = []
        current_step = "UNKNOWN"

        for i, line in enumerate(lines, start=1):
            # Check if this line starts a new step
            for step in steps:
                if step.line == i:
                    current_step = step.name
                    break

            # Check for DD statement
            match = self.DD_PATTERN.match(line)
            if match:
                dd_name = match.group(1)

                # Extract DSN
                dataset = None
                dsn_match = self.DSN_PATTERN.search(line)
                if dsn_match:
                    dataset = dsn_match.group(1)

                # Extract DISP
                disposition = None
                disp_match = self.DISP_PATTERN.search(line)
                if disp_match:
                    disposition = disp_match.group(1)

                # Extract SYSOUT
                sysout = None
                sysout_match = self.SYSOUT_PATTERN.search(line)
                if sysout_match:
                    sysout = sysout_match.group(1)

                dd_statements.append(JCLDDInfo(
                    step_name=current_step,
                    dd_name=dd_name,
                    dataset=dataset,
                    disposition=disposition,
                    sysout=sysout,
                    line=i,
                ))

        return dd_statements

    def _extract_datasets(self, dd_statements: list[JCLDDInfo]) -> list[str]:
        """Extract unique dataset names from DD statements.

        Args:
            dd_statements: List of DD statements.

        Returns:
            List of unique dataset names.
        """
        datasets = set()
        for dd in dd_statements:
            if dd.dataset:
                datasets.add(dd.dataset)
        return sorted(datasets)

    def _extract_includes(self, source: str) -> list[str]:
        """Extract INCLUDE member names.

        Args:
            source: JCL source code.

        Returns:
            List of INCLUDE member names.
        """
        matches = self.INCLUDE_PATTERN.findall(source)
        return list(set(matches))
