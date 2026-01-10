"""Base preprocessor interface for War Rig.

This module defines the abstract base class and common models for all
source code preprocessors. Preprocessors extract structural information
without using LLMs - they perform deterministic parsing and pattern matching.

The extracted information helps agents understand code structure before
deep analysis begins.
"""

from abc import ABC, abstractmethod
from datetime import datetime
from typing import Any, Generic, TypeVar

from pydantic import BaseModel, Field

from war_rig.models.templates import FileType


class PreprocessorResult(BaseModel):
    """Base result model for all preprocessors.

    Contains common metadata and can be extended by specific preprocessors
    to include type-specific structural information.

    Attributes:
        file_name: Name of the processed file
        file_type: Detected file type
        program_id: Extracted program identifier (if found)
        line_count: Total lines in the file
        processed_at: When preprocessing occurred
        errors: Any errors encountered during preprocessing
        warnings: Non-fatal issues found during preprocessing
    """

    file_name: str = Field(
        ...,
        description="Name of the processed file",
    )
    file_type: FileType = Field(
        ...,
        description="Detected file type",
    )
    program_id: str | None = Field(
        default=None,
        description="Extracted program identifier (e.g., PROGRAM-ID)",
    )
    line_count: int = Field(
        default=0,
        ge=0,
        description="Total lines in the file",
    )
    processed_at: datetime = Field(
        default_factory=datetime.utcnow,
        description="When preprocessing occurred",
    )
    errors: list[str] = Field(
        default_factory=list,
        description="Errors encountered during preprocessing",
    )
    warnings: list[str] = Field(
        default_factory=list,
        description="Non-fatal issues found",
    )

    @property
    def has_errors(self) -> bool:
        """Check if preprocessing encountered errors.

        Returns:
            True if any errors occurred.
        """
        return len(self.errors) > 0

    @property
    def is_valid(self) -> bool:
        """Check if preprocessing produced valid results.

        Returns:
            True if no errors and program_id was found.
        """
        return not self.has_errors and self.program_id is not None


# Type variable for generic preprocessor result
T = TypeVar("T", bound=PreprocessorResult)


class BasePreprocessor(ABC, Generic[T]):
    """Abstract base class for source code preprocessors.

    Preprocessors perform deterministic extraction of structural information
    from mainframe source code. They do not use LLMs - all extraction is
    based on pattern matching and parsing rules.

    Subclasses must implement:
    - process(): Main processing method
    - detect_file_type(): Determine if file matches this preprocessor
    - get_supported_extensions(): Return supported file extensions

    Example:
        class COBOLPreprocessor(BasePreprocessor[COBOLStructure]):
            def process(self, source: str, file_name: str) -> COBOLStructure:
                # Extract COBOL structure
                ...
    """

    @abstractmethod
    def process(self, source: str, file_name: str) -> T:
        """Process source code and extract structural information.

        Args:
            source: The source code content as a string.
            file_name: Name of the source file (for error messages).

        Returns:
            A PreprocessorResult subclass with extracted structure.

        Raises:
            PreprocessingError: If preprocessing fails fatally.
        """
        pass

    @abstractmethod
    def detect_file_type(self, source: str, file_name: str) -> bool:
        """Determine if this preprocessor can handle the given file.

        Args:
            source: The source code content.
            file_name: Name of the source file.

        Returns:
            True if this preprocessor should handle this file.
        """
        pass

    @abstractmethod
    def get_supported_extensions(self) -> list[str]:
        """Get the file extensions this preprocessor handles.

        Returns:
            List of file extensions (including the dot, e.g., ['.cbl']).
        """
        pass

    def can_process(self, source: str, file_name: str) -> bool:
        """Check if this preprocessor can process the given file.

        First checks file extension, then uses detect_file_type for
        content-based detection.

        Args:
            source: The source code content.
            file_name: Name of the source file.

        Returns:
            True if this preprocessor can handle the file.
        """
        import os

        ext = os.path.splitext(file_name)[1].lower()
        supported_lower = [e.lower() for e in self.get_supported_extensions()]

        if ext in supported_lower:
            return True

        return self.detect_file_type(source, file_name)


class PreprocessingError(Exception):
    """Exception raised when preprocessing fails.

    Attributes:
        message: Description of the error.
        file_name: Name of the file being processed.
        line_number: Line number where error occurred (if known).
        context: Additional context about the error.
    """

    def __init__(
        self,
        message: str,
        file_name: str | None = None,
        line_number: int | None = None,
        context: str | None = None,
    ):
        """Initialize a PreprocessingError.

        Args:
            message: Description of the error.
            file_name: Name of the file being processed.
            line_number: Line number where error occurred.
            context: Additional context about the error.
        """
        self.message = message
        self.file_name = file_name
        self.line_number = line_number
        self.context = context

        full_message = message
        if file_name:
            full_message = f"{file_name}: {full_message}"
        if line_number:
            full_message = f"{full_message} (line {line_number})"
        if context:
            full_message = f"{full_message}\nContext: {context}"

        super().__init__(full_message)


# =============================================================================
# Common Structural Elements
# =============================================================================


class SourceLocation(BaseModel):
    """Location information for a source element."""

    start_line: int = Field(..., ge=1, description="Starting line number")
    end_line: int | None = Field(default=None, ge=1, description="Ending line number")
    column: int | None = Field(default=None, ge=1, description="Column number")

    @property
    def line_range(self) -> tuple[int, int]:
        """Get the line range as a tuple.

        Returns:
            Tuple of (start_line, end_line). If end_line is None,
            returns (start_line, start_line).
        """
        return (self.start_line, self.end_line or self.start_line)


class ParagraphInfo(BaseModel):
    """Information about a COBOL paragraph or section."""

    name: str = Field(..., description="Paragraph name")
    location: SourceLocation = Field(..., description="Source location")

    @property
    def start_line(self) -> int:
        """Get the starting line number."""
        return self.location.start_line

    @property
    def end_line(self) -> int | None:
        """Get the ending line number."""
        return self.location.end_line


class PerformInfo(BaseModel):
    """Information about a PERFORM statement."""

    from_paragraph: str = Field(..., description="Paragraph containing the PERFORM")
    to_paragraph: str = Field(..., description="Target paragraph")
    thru_paragraph: str | None = Field(default=None, description="THRU target if specified")
    line: int = Field(..., ge=1, description="Line number of PERFORM")


class CallInfo(BaseModel):
    """Information about a CALL statement."""

    program: str = Field(..., description="Called program name")
    using: list[str] = Field(default_factory=list, description="USING clause items")
    line: int = Field(..., ge=1, description="Line number of CALL")
    is_dynamic: bool = Field(default=False, description="Whether call is dynamic")


class CopybookInfo(BaseModel):
    """Information about a COPY statement."""

    name: str = Field(..., description="Copybook name")
    line: int = Field(..., ge=1, description="Line number of COPY")
    replacing: list[str] = Field(
        default_factory=list,
        description="REPLACING clause items",
    )


class FileInfo(BaseModel):
    """Information about a file definition."""

    name: str = Field(..., description="File name (FD/SELECT)")
    organization: str | None = Field(
        default=None,
        description="File organization (SEQUENTIAL, INDEXED, etc.)",
    )
    access_mode: str | None = Field(
        default=None,
        description="Access mode (SEQUENTIAL, RANDOM, DYNAMIC)",
    )
    line: int = Field(..., ge=1, description="Line number of definition")


class SQLStatementInfo(BaseModel):
    """Information about an embedded SQL statement."""

    operation: str = Field(..., description="SQL operation type")
    table: str | None = Field(default=None, description="Table name")
    line: int = Field(..., ge=1, description="Line number")
    cursor_name: str | None = Field(default=None, description="Cursor name if applicable")


class CICSCommandInfo(BaseModel):
    """Information about a CICS command."""

    command: str = Field(..., description="CICS command (READ, WRITE, etc.)")
    resource: str | None = Field(default=None, description="Resource name")
    line: int = Field(..., ge=1, description="Line number")
    options: dict[str, str] = Field(
        default_factory=dict,
        description="Command options",
    )
