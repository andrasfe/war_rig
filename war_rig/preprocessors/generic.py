"""Generic preprocessor for any source file type.

This preprocessor provides basic structural analysis for files that don't
have a specialized preprocessor (like listing files, BMS, etc.).
"""

import logging
import re
from pathlib import Path

from war_rig.models.templates import FileType
from war_rig.preprocessors.base import BasePreprocessor, PreprocessorResult

logger = logging.getLogger(__name__)


class GenericPreprocessor(BasePreprocessor):
    """Generic preprocessor for any source file.

    Provides basic analysis that works with any text-based source file.
    Extracts line counts, identifies potential sections, and detects
    basic patterns.
    """

    # Map extensions to file types
    EXTENSION_MAP = {
        ".lst": FileType.LISTING,
        ".list": FileType.LISTING,
        ".bms": FileType.BMS,
        ".pli": FileType.PLI,
        ".pl1": FileType.PLI,
    }

    def detect_file_type(self, source: str, file_name: str) -> bool:
        """Determine if this preprocessor can handle the given file.

        As a fallback preprocessor, this always returns True for files
        that aren't handled by more specific preprocessors.

        Args:
            source: The source code content.
            file_name: Name of the source file.

        Returns:
            True - this preprocessor accepts all files as a fallback.
        """
        return True

    def get_supported_extensions(self) -> list[str]:
        """Get the file extensions this preprocessor handles.

        Returns:
            List of extensions for listing, BMS, and PL/I files.
        """
        return list(self.EXTENSION_MAP.keys())

    def can_process(self, source_code: str, file_name: str) -> bool:
        """Check if this preprocessor can handle the file.

        This generic preprocessor accepts any file as a fallback.

        Args:
            source_code: The source code content.
            file_name: Name of the file.

        Returns:
            True - this preprocessor accepts all files as a fallback.
        """
        return True

    def _detect_file_type(self, file_name: str, source_code: str) -> FileType:
        """Detect file type from extension or content.

        Args:
            file_name: Name of the file.
            source_code: The source code content.

        Returns:
            Detected FileType.
        """
        ext = Path(file_name).suffix.lower()

        # Check extension map
        if ext in self.EXTENSION_MAP:
            return self.EXTENSION_MAP[ext]

        # Content-based detection
        upper_content = source_code[:2000].upper()

        # Check for COBOL indicators
        if "IDENTIFICATION DIVISION" in upper_content or "PROCEDURE DIVISION" in upper_content:
            return FileType.COBOL

        # Check for JCL indicators
        if upper_content.startswith("//") or "JOB " in upper_content[:500]:
            return FileType.JCL

        # Check for copybook indicators (data definitions without divisions)
        if re.search(r"^\s*01\s+\w+", source_code, re.MULTILINE) and "DIVISION" not in upper_content:
            return FileType.COPYBOOK

        return FileType.OTHER

    def process(self, source_code: str, file_name: str) -> PreprocessorResult:
        """Process the source file and extract basic structural information.

        Args:
            source_code: The source code content.
            file_name: Name of the file.

        Returns:
            PreprocessorResult with basic structural information.
        """
        lines = source_code.split("\n")
        file_type = self._detect_file_type(file_name, source_code)

        # Extract program ID from filename
        program_id = Path(file_name).stem.upper()

        # Count non-empty lines
        non_empty_lines = [ln for ln in lines if ln.strip()]

        # Find potential section markers
        sections: list[dict] = []
        section_pattern = re.compile(r"^[\s\*]*(?:SECTION|DIVISION|PROCEDURE|PARAGRAPH|STEP)\s*[:\.]?\s*(\w+)?", re.IGNORECASE)

        for i, line in enumerate(lines):
            match = section_pattern.search(line)
            if match:
                sections.append({
                    "line": i + 1,
                    "text": line.strip()[:80],
                })

        # Build hints
        hints = [
            f"File: {file_name}",
            f"Type: {file_type.value}",
            f"Total lines: {len(lines)}",
            f"Non-empty lines: {len(non_empty_lines)}",
        ]

        if sections:
            hints.append(f"Potential sections: {len(sections)}")

        return PreprocessorResult(
            program_id=program_id,
            file_type=file_type,
            line_count=len(lines),
            hints=hints,
            paragraphs=[],  # Generic preprocessor doesn't identify paragraphs
            performs=[],
            calls=[],
            copybooks=[],
            sql_statements=[],
            cics_commands=[],
            data_items=[],
            metadata={
                "non_empty_lines": len(non_empty_lines),
                "sections": sections[:20],  # Limit to first 20
                "detected_type": file_type.value,
            },
        )
