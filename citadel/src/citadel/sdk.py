"""
Citadel SDK - Programmatic interface for dependency graph extraction.

This module provides a simple API for agents and tools to extract
artifacts and their relationships from source files.

Example usage:
    from citadel.sdk import Citadel

    # Initialize once
    citadel = Citadel()

    # Analyze a single file
    result = citadel.analyze_file("/path/to/program.cbl")

    # Get functions/artifacts and their callouts
    for artifact in result.artifacts:
        print(f"{artifact.name} ({artifact.type})")
        for callout in artifact.callouts:
            print(f"  -> {callout.target} ({callout.relationship})")
"""

import logging
import re
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

from citadel.config import CitadelConfig, get_specs_cache_dir, load_config
from citadel.discovery import FileDiscovery
from citadel.graph.model import Artifact, SourceLocation
from citadel.parser import FileParseResult, ParserEngine, RawReference
from citadel.specs import ArtifactSpec, ArtifactType, RelationshipType, SpecManager

logger = logging.getLogger(__name__)


@dataclass
class Callout:
    """A reference/call from an artifact to another target."""

    target: str
    """The name of the target being called/referenced."""

    relationship: str
    """Type of relationship (calls, includes, reads, writes, etc.)."""

    target_type: str | None = None
    """Expected type of target (program, copybook, table, etc.)."""

    line: int | None = None
    """Line number where the callout occurs."""

    raw_text: str | None = None
    """The raw text of the reference in source."""

    def to_dict(self) -> dict[str, Any]:
        """Convert to dictionary for JSON serialization."""
        return {
            "target": self.target,
            "relationship": self.relationship,
            "target_type": self.target_type,
            "line": self.line,
            "raw_text": self.raw_text,
        }


@dataclass
class FileArtifact:
    """An artifact (function, program, class, etc.) defined in a file."""

    name: str
    """Canonical name of the artifact."""

    type: str
    """Type of artifact (program, function, class, paragraph, etc.)."""

    category: str
    """Category (code, data, interface)."""

    line_start: int | None = None
    """Starting line number in source."""

    line_end: int | None = None
    """Ending line number in source."""

    callouts: list[Callout] = field(default_factory=list)
    """References/calls made by this artifact."""

    def to_dict(self) -> dict[str, Any]:
        """Convert to dictionary for JSON serialization."""
        return {
            "name": self.name,
            "type": self.type,
            "category": self.category,
            "line_start": self.line_start,
            "line_end": self.line_end,
            "callouts": [c.to_dict() for c in self.callouts],
        }


@dataclass
class FileAnalysisResult:
    """Result of analyzing a single file."""

    file_path: str
    """Path to the analyzed file."""

    language: str
    """Detected language/spec used."""

    artifacts: list[FileArtifact]
    """Artifacts defined in the file."""

    file_level_callouts: list[Callout] = field(default_factory=list)
    """Callouts not associated with a specific artifact (e.g., imports at file level)."""

    preprocessor_includes: list[str] = field(default_factory=list)
    """Files included via preprocessor (COPY, #include, import)."""

    error: str | None = None
    """Error message if analysis failed."""

    def to_dict(self) -> dict[str, Any]:
        """Convert to dictionary for JSON serialization."""
        return {
            "file_path": self.file_path,
            "language": self.language,
            "artifacts": [a.to_dict() for a in self.artifacts],
            "file_level_callouts": [c.to_dict() for c in self.file_level_callouts],
            "preprocessor_includes": self.preprocessor_includes,
            "error": self.error,
        }

    def get_all_callouts(self) -> list[Callout]:
        """Get all callouts from all artifacts plus file-level callouts."""
        all_callouts = list(self.file_level_callouts)
        for artifact in self.artifacts:
            all_callouts.extend(artifact.callouts)
        return all_callouts

    def get_artifact_by_name(self, name: str) -> FileArtifact | None:
        """Find an artifact by name (case-insensitive)."""
        name_lower = name.lower()
        for artifact in self.artifacts:
            if artifact.name.lower() == name_lower:
                return artifact
        return None


class Citadel:
    """
    Citadel SDK for programmatic dependency extraction.

    This class provides a simple interface for agents to analyze source files
    and extract artifacts with their relationships.

    Example:
        citadel = Citadel()

        # Analyze a COBOL program
        result = citadel.analyze_file("COBPROG.cbl")

        # Print all functions and their calls
        for artifact in result.artifacts:
            print(f"{artifact.name}:")
            for call in artifact.callouts:
                print(f"  calls {call.target}")
    """

    def __init__(self, config: CitadelConfig | None = None):
        """
        Initialize the Citadel SDK.

        Args:
            config: Optional configuration. If not provided, loads from environment.
        """
        self.config = config or load_config()

        # Initialize spec manager
        builtin_dir = Path(__file__).parent.parent.parent / "specs" / "builtin"
        cache_dir = get_specs_cache_dir(self.config)
        self.spec_manager = SpecManager(builtin_dir, cache_dir)

        # Initialize parser
        self.parser = ParserEngine()

        # File discovery helper
        self._extension_to_spec: dict[str, str] = {}
        self._build_extension_map()

    def _build_extension_map(self) -> None:
        """Build mapping from file extensions to spec IDs."""
        for spec_id in self.spec_manager.list_available_specs():
            spec = self.spec_manager.get_spec(spec_id)
            if spec:
                for ext in spec.file_extensions:
                    self._extension_to_spec[ext.lower()] = spec_id

    def _get_spec_for_file(self, file_path: Path) -> ArtifactSpec | None:
        """Get the appropriate spec for a file."""
        ext = file_path.suffix.lower()
        spec_id = self._extension_to_spec.get(ext)
        if spec_id:
            return self.spec_manager.get_spec(spec_id)
        return None

    def analyze_file(self, file_path: str | Path) -> FileAnalysisResult:
        """
        Analyze a single file and extract its artifacts and callouts.

        Args:
            file_path: Path to the source file to analyze.

        Returns:
            FileAnalysisResult containing artifacts and their relationships.
        """
        file_path = Path(file_path)

        # Get spec for this file type
        spec = self._get_spec_for_file(file_path)
        if not spec:
            return FileAnalysisResult(
                file_path=str(file_path),
                language="unknown",
                artifacts=[],
                error=f"No spec found for file extension '{file_path.suffix}'"
            )

        # Read file content
        try:
            content = self._read_file(file_path)
        except Exception as e:
            return FileAnalysisResult(
                file_path=str(file_path),
                language=spec.language,
                artifacts=[],
                error=f"Failed to read file: {e}"
            )

        # Parse the file
        try:
            parse_result = self.parser.parse_file(file_path, content, spec)
        except Exception as e:
            return FileAnalysisResult(
                file_path=str(file_path),
                language=spec.language,
                artifacts=[],
                error=f"Failed to parse file: {e}"
            )

        # Convert to SDK result format
        return self._convert_parse_result(parse_result, spec)

    def _read_file(self, file_path: Path) -> str:
        """Read file content with encoding fallbacks."""
        encodings = ["utf-8", "latin-1", "cp1252"]
        for encoding in encodings:
            try:
                return file_path.read_text(encoding=encoding)
            except UnicodeDecodeError:
                continue
        raise ValueError(f"Could not decode file with any supported encoding")

    def _convert_parse_result(
        self,
        parse_result: FileParseResult,
        spec: ArtifactSpec
    ) -> FileAnalysisResult:
        """Convert internal parse result to SDK format."""

        # Build artifact map for associating callouts
        artifact_map: dict[str, FileArtifact] = {}
        artifacts: list[FileArtifact] = []

        for artifact in parse_result.artifacts_defined:
            file_artifact = FileArtifact(
                name=artifact.canonical_name,
                type=artifact.artifact_type.value,
                category=artifact.category.value,
                line_start=artifact.defined_in.line_start if artifact.defined_in else None,
                line_end=artifact.defined_in.line_end if artifact.defined_in else None,
                callouts=[],
            )
            artifacts.append(file_artifact)
            artifact_map[artifact.id] = file_artifact

        # Associate references with their containing artifacts
        file_level_callouts: list[Callout] = []

        for ref in parse_result.references_found:
            callout = Callout(
                target=ref.raw_text,
                relationship=ref.relationship_type.value if ref.relationship_type else "references",
                target_type=ref.expected_type.value if ref.expected_type else None,
                line=ref.location.line_start if ref.location else None,
                raw_text=ref.raw_text,
            )

            # Find containing artifact
            if ref.containing_artifact and ref.containing_artifact in artifact_map:
                artifact_map[ref.containing_artifact].callouts.append(callout)
            else:
                file_level_callouts.append(callout)

        # Extract preprocessor includes
        preprocessor_includes: list[str] = []
        for directive in parse_result.preprocessor_directives:
            if directive.get("target"):
                preprocessor_includes.append(directive["target"])

                # Also add as a callout
                callout = Callout(
                    target=directive["target"],
                    relationship=directive.get("relationship_type", "includes"),
                    target_type=directive.get("target_type"),
                    line=directive.get("location", {}).get("line_start"),
                )

                # Associate with first artifact if available
                if artifacts:
                    artifacts[0].callouts.append(callout)
                else:
                    file_level_callouts.append(callout)

        return FileAnalysisResult(
            file_path=str(parse_result.file_path),
            language=spec.language,
            artifacts=artifacts,
            file_level_callouts=file_level_callouts,
            preprocessor_includes=preprocessor_includes,
        )

    def get_functions(self, file_path: str | Path) -> list[dict[str, Any]]:
        """
        Get all functions/procedures/programs in a file with their callouts.

        This is a convenience method that returns a simplified dictionary format.

        Args:
            file_path: Path to the source file.

        Returns:
            List of dictionaries with function info and callouts.
        """
        result = self.analyze_file(file_path)

        if result.error:
            return [{"error": result.error}]

        functions = []
        for artifact in result.artifacts:
            functions.append({
                "name": artifact.name,
                "type": artifact.type,
                "line": artifact.line_start,
                "calls": [
                    {
                        "target": c.target,
                        "type": c.relationship,
                        "line": c.line,
                    }
                    for c in artifact.callouts
                ],
            })

        return functions

    def get_callouts(self, file_path: str | Path) -> list[dict[str, Any]]:
        """
        Get all callouts/references from a file.

        Args:
            file_path: Path to the source file.

        Returns:
            List of all callouts with their source artifacts.
        """
        result = self.analyze_file(file_path)

        if result.error:
            return [{"error": result.error}]

        callouts = []

        # File-level callouts
        for c in result.file_level_callouts:
            callouts.append({
                "from": None,
                "to": c.target,
                "type": c.relationship,
                "line": c.line,
            })

        # Artifact callouts
        for artifact in result.artifacts:
            for c in artifact.callouts:
                callouts.append({
                    "from": artifact.name,
                    "to": c.target,
                    "type": c.relationship,
                    "line": c.line,
                })

        return callouts

    def get_includes(self, file_path: str | Path) -> list[str]:
        """
        Get all included/imported files from a source file.

        Args:
            file_path: Path to the source file.

        Returns:
            List of included file names (COPY members, imports, etc.)
        """
        result = self.analyze_file(file_path)
        return result.preprocessor_includes

    def list_supported_extensions(self) -> list[str]:
        """
        List all file extensions supported by Citadel.

        Returns:
            List of supported file extensions.
        """
        return sorted(self._extension_to_spec.keys())

    def list_supported_languages(self) -> list[str]:
        """
        List all languages/specs supported by Citadel.

        Returns:
            List of supported language names.
        """
        languages = []
        for spec_id in self.spec_manager.list_available_specs():
            spec = self.spec_manager.get_spec(spec_id)
            if spec:
                languages.append(spec.language)
        return sorted(languages)

    def get_function_body(
        self, file_path: str | Path, function_name: str
    ) -> str | None:
        """
        Get the body text of a specific function/paragraph/method.

        Extracts only the function body content, not including the function
        definition line itself.

        Args:
            file_path: Path to the source file.
            function_name: Name of the function/paragraph to extract.

        Returns:
            The function body text, or None if the function is not found.
        """
        file_path = Path(file_path)

        # Get spec for this file type
        spec = self._get_spec_for_file(file_path)
        if not spec:
            logger.warning("No spec found for file extension '%s'", file_path.suffix)
            return None

        # Read file content
        try:
            content = self._read_file(file_path)
        except Exception as e:
            logger.warning("Failed to read file %s: %s", file_path, e)
            return None

        # Analyze the file to find the artifact
        result = self.analyze_file(file_path)
        artifact = result.get_artifact_by_name(function_name)

        if artifact is None:
            logger.debug("Function '%s' not found in %s", function_name, file_path)
            return None

        if artifact.line_start is None:
            logger.warning("Artifact '%s' has no line_start", function_name)
            return None

        # Get the lines of the file
        lines = content.splitlines()

        # Determine line_end if not already set
        if artifact.line_end is None:
            line_end = self._find_function_end(
                content, artifact.line_start, spec, result.artifacts, artifact
            )
        else:
            line_end = artifact.line_end

        # Extract the body (exclude the definition line, start from next line)
        # line_start is 1-indexed
        body_start = artifact.line_start  # Include definition line for now
        body_end = line_end

        if body_start > len(lines) or body_end > len(lines):
            logger.warning(
                "Line range %d-%d exceeds file length %d",
                body_start,
                body_end,
                len(lines),
            )
            return None

        # Extract lines (convert to 0-indexed)
        body_lines = lines[body_start - 1 : body_end]

        return "\n".join(body_lines)

    def _find_function_end(
        self,
        content: str,
        start_line: int,
        spec: ArtifactSpec,
        all_artifacts: list[FileArtifact],
        current_artifact: FileArtifact,
    ) -> int:
        """
        Find the ending line of a function based on language-specific rules.

        Args:
            content: The full file content.
            start_line: The starting line of the function (1-indexed).
            spec: The artifact specification for this language.
            all_artifacts: All artifacts in the file.
            current_artifact: The artifact we're finding the end for.

        Returns:
            The ending line number (1-indexed, inclusive).
        """
        lines = content.splitlines()
        total_lines = len(lines)

        # Language-specific end detection
        language = spec.language.lower()

        if language == "cobol":
            return self._find_cobol_paragraph_end(
                lines, start_line, all_artifacts, current_artifact
            )
        elif language == "python":
            return self._find_python_function_end(lines, start_line)
        else:
            # Default: use scope delimiters or next artifact
            return self._find_end_by_next_artifact_or_eof(
                lines, start_line, all_artifacts, current_artifact
            )

    def _find_cobol_paragraph_end(
        self,
        lines: list[str],
        start_line: int,
        all_artifacts: list[FileArtifact],
        current_artifact: FileArtifact,
    ) -> int:
        """
        Find the end of a COBOL paragraph.

        COBOL paragraphs end when:
        1. Another paragraph name is encountered (word followed by period at start in area A)
        2. A SECTION is encountered
        3. END-PROGRAM or end of PROCEDURE DIVISION is hit
        4. End of file is reached
        """
        total_lines = len(lines)

        # Pattern for paragraph/section names in COBOL (Area A starts at column 8, 0-indexed: 7)
        # A paragraph name is a word in columns 8-11 followed by a period
        paragraph_pattern = re.compile(r"^.{7}([A-Z0-9-]+)\s*\.$", re.IGNORECASE)
        section_pattern = re.compile(r"^.{7}([A-Z0-9-]+)\s+SECTION\s*\.", re.IGNORECASE)
        end_patterns = re.compile(
            r"^\s*(END-PROGRAM|END\s+PROGRAM|IDENTIFICATION\s+DIVISION|DATA\s+DIVISION)",
            re.IGNORECASE,
        )

        # Find the next artifact that starts after this one
        next_artifact_line = total_lines + 1
        for artifact in all_artifacts:
            if artifact.line_start and artifact.line_start > start_line:
                if artifact.line_start < next_artifact_line:
                    next_artifact_line = artifact.line_start

        # Scan from start_line + 1 to find the end
        for i in range(start_line, total_lines):  # start_line is 1-indexed
            line = lines[i]

            # Check for end markers
            if end_patterns.search(line):
                return i  # Return previous line (i is 0-indexed, so i is the line before)

            # Check for next paragraph or section (but not the current one)
            if i > start_line - 1:  # Skip the starting line itself
                if len(line) >= 8:
                    # Check for section first
                    if section_pattern.match(line):
                        return i  # Line before this section

                    # Check for paragraph
                    if paragraph_pattern.match(line):
                        return i  # Line before this paragraph

        # If no end marker found, return the last line of the file
        return total_lines

    def _find_python_function_end(self, lines: list[str], start_line: int) -> int:
        """
        Find the end of a Python function using indentation.

        A Python function ends when:
        1. A line with equal or less indentation than the def line is found
           (excluding blank lines and comments)
        2. End of file is reached
        """
        total_lines = len(lines)

        if start_line > total_lines:
            return start_line

        # Get the indentation of the definition line
        def_line = lines[start_line - 1]  # Convert to 0-indexed
        def_indent = len(def_line) - len(def_line.lstrip())

        # Track the last non-empty line as potential end
        last_content_line = start_line

        for i in range(start_line, total_lines):  # Start from line after def
            line = lines[i]
            stripped = line.strip()

            # Skip empty lines and comments - they don't end a function
            if not stripped or stripped.startswith("#"):
                continue

            # Get current line's indentation
            current_indent = len(line) - len(line.lstrip())

            # If we find a line with indentation <= def line's indentation,
            # the function ended on the previous content line
            if current_indent <= def_indent:
                return last_content_line

            # This line is part of the function body
            last_content_line = i + 1  # Convert back to 1-indexed

        # If we reach end of file, function extends to last content line
        return last_content_line

    def _find_end_by_next_artifact_or_eof(
        self,
        lines: list[str],
        start_line: int,
        all_artifacts: list[FileArtifact],
        current_artifact: FileArtifact,
    ) -> int:
        """
        Find function end by looking for the next artifact or end of file.

        This is the fallback for languages without specific rules.
        """
        total_lines = len(lines)

        # Find the next artifact that starts after this one
        next_start = total_lines + 1
        for artifact in all_artifacts:
            if artifact.line_start and artifact.line_start > start_line:
                if artifact.line_start < next_start:
                    next_start = artifact.line_start

        # Return the line before the next artifact, or EOF
        if next_start <= total_lines:
            return next_start - 1
        return total_lines


# Convenience function for one-off analysis
def analyze_file(file_path: str | Path) -> FileAnalysisResult:
    """
    Analyze a single file and extract artifacts and callouts.

    This is a convenience function that creates a Citadel instance
    and analyzes the file. For multiple files, create a Citadel
    instance and reuse it.

    Args:
        file_path: Path to the source file.

    Returns:
        FileAnalysisResult with artifacts and callouts.
    """
    citadel = Citadel()
    return citadel.analyze_file(file_path)


def get_functions(file_path: str | Path) -> list[dict[str, Any]]:
    """
    Get all functions in a file with their callouts.

    Convenience function for quick analysis.

    Args:
        file_path: Path to the source file.

    Returns:
        List of function dictionaries with callouts.
    """
    citadel = Citadel()
    return citadel.get_functions(file_path)


def get_function_body(file_path: str | Path, function_name: str) -> str | None:
    """
    Get the body text of a specific function/paragraph/method.

    Convenience function for quick extraction.

    Args:
        file_path: Path to the source file.
        function_name: Name of the function/paragraph to extract.

    Returns:
        The function body text, or None if the function is not found.
    """
    citadel = Citadel()
    return citadel.get_function_body(file_path, function_name)
