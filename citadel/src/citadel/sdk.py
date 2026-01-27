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

import asyncio
import json
import logging
import re
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

from citadel.analysis.dead_code import (
    DeadCodeItem,
    dead_code_summary,
    dead_code_to_dicts,
    find_dead_code,
)
from citadel.analysis.flow_diagram import generate_flow_diagram
from citadel.analysis.sequence_finder import find_longest_sequences, sequences_to_mermaid
from citadel.config import CitadelConfig, get_specs_cache_dir, load_config
from citadel.discovery import FileDiscovery
from citadel.graph.model import Artifact, DependencyGraph, SourceLocation
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

    resolved: bool | None = None
    """Whether the target was resolved to an existing artifact.

    None for single file analysis, True/False for directory analysis.
    """

    def to_dict(self) -> dict[str, Any]:
        """Convert to dictionary for JSON serialization."""
        result = {
            "target": self.target,
            "relationship": self.relationship,
            "target_type": self.target_type,
            "line": self.line,
            "raw_text": self.raw_text,
        }
        if self.resolved is not None:
            result["resolved"] = self.resolved
        return result


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

    def get_callouts(self, path: str | Path) -> list[dict[str, Any]]:
        """
        Get all callouts/references from a file or directory.

        When given a file path, returns callouts from that single file.
        When given a directory path, analyzes all supported files in the
        directory and returns callouts with a `resolved` field indicating
        whether the target exists in the codebase.

        Args:
            path: Path to a source file or directory to analyze.

        Returns:
            List of all callouts with their source artifacts.
            For directory analysis, includes a `resolved` field (True/False).
            For single file analysis, `resolved` is not included.
        """
        path = Path(path)

        if path.is_dir():
            return self._get_callouts_from_directory(path)
        else:
            return self._get_callouts_from_file(path)

    def _get_callouts_from_file(self, file_path: Path) -> list[dict[str, Any]]:
        """Get callouts from a single file."""
        result = self.analyze_file(file_path)

        if result.error:
            return [{"error": result.error}]

        callouts = []
        file_stem = file_path.stem  # Program name from filename

        # Note: preprocessor includes (COPY statements) are already added
        # as callouts to the first artifact by _convert_parse_result(),
        # so we do NOT add them separately here to avoid duplicates.

        # File-level callouts
        for c in result.file_level_callouts:
            callouts.append({
                "from": file_stem,
                "to": c.target,
                "type": c.relationship,
                "line": c.line,
            })

        # Artifact callouts (includes preprocessor directives with line numbers)
        for artifact in result.artifacts:
            for c in artifact.callouts:
                callouts.append({
                    "from": artifact.name,
                    "to": c.target,
                    "type": c.relationship,
                    "line": c.line,
                })

        return callouts

    def _get_callouts_from_directory(self, directory_path: Path) -> list[dict[str, Any]]:
        """
        Get callouts from all files in a directory with resolution status.

        Analyzes all supported source files in the directory and returns
        callouts with `resolved` indicating whether each target exists.
        """
        directory_path = directory_path.resolve()

        if not directory_path.exists():
            return [{"error": f"Directory not found: {directory_path}"}]

        # Discover all source files
        discovery = FileDiscovery(directory_path)
        discovered = discovery.discover()

        # Collect all files to analyze
        files_to_analyze: list[Path] = []
        for spec_files in discovered.files_by_spec.values():
            files_to_analyze.extend(spec_files)

        # First pass: analyze all files and collect artifacts
        all_artifacts: list[FileArtifact] = []
        all_callouts_data: list[tuple[str, str | None, Callout]] = []  # (file_stem, artifact_name, callout)

        for source_file in files_to_analyze:
            try:
                analysis = self.analyze_file(source_file)

                if analysis.error:
                    logger.debug("Skipping file with analysis error: %s - %s", source_file, analysis.error)
                    continue

                # Store artifacts
                all_artifacts.extend(analysis.artifacts)

                # Collect all callouts with their source info
                file_stem = source_file.stem  # Filename without extension

                # Note: preprocessor includes (COPY statements) are already
                # added as callouts to the first artifact by
                # _convert_parse_result(), so we do NOT add them separately
                # here to avoid duplicates.

                # File-level callouts
                for callout in analysis.file_level_callouts:
                    all_callouts_data.append((file_stem, file_stem, callout))

                # Artifact callouts (includes preprocessor directives with line numbers)
                for artifact in analysis.artifacts:
                    for callout in artifact.callouts:
                        all_callouts_data.append((file_stem, artifact.name, callout))

            except Exception as e:
                logger.debug("Error analyzing file %s: %s", source_file, e)

        # Build a set of known artifact names for resolution checking
        # Include file stems (program names) for copybook resolution
        known_artifacts: set[str] = set()
        known_files: set[str] = set()
        for artifact in all_artifacts:
            known_artifacts.add(artifact.name.lower())
        for source_file in files_to_analyze:
            known_files.add(source_file.stem.lower())

        # Convert callouts to result dictionaries with resolution status
        callouts = []

        for file_stem, artifact_name, callout in all_callouts_data:
            # Check if target is resolved
            target_lower = callout.target.lower() if callout.target else ""
            resolved = target_lower in known_artifacts or target_lower in known_files

            callouts.append({
                "from": artifact_name,
                "to": callout.target,
                "type": callout.relationship,
                "line": callout.line,
                "resolved": resolved,
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

    def clear_cache(self) -> None:
        """
        Clear all cached data (specs, parse results).

        This can help resolve issues when specs have been modified
        or when cached data becomes stale.
        """
        import shutil

        cache_dir = self.config.cache_dir
        if cache_dir.exists():
            shutil.rmtree(cache_dir)
            logger.info(f"Cleared cache at {cache_dir}")

        # Rebuild extension map after clearing
        self._build_extension_map()

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

        # For COBOL, trim trailing comment blocks that belong to next paragraph
        if spec.language.lower() == "cobol":
            body_lines = self._trim_trailing_cobol_comments(body_lines)

        return "\n".join(body_lines)

    def _trim_trailing_cobol_comments(self, lines: list[str]) -> list[str]:
        """
        Trim trailing comment-only lines from COBOL paragraph body.

        COBOL paragraphs often have comment header blocks like:
        *----------------------------------------------------------------*
        *                      PARAGRAPH-NAME
        *----------------------------------------------------------------*

        These belong to the NEXT paragraph, not the current one.
        """
        if not lines:
            return lines

        # Find the last non-comment, non-blank line
        last_content_idx = len(lines) - 1

        while last_content_idx >= 0:
            line = lines[last_content_idx]
            stripped = line.strip()

            # Empty line - continue checking
            if not stripped:
                last_content_idx -= 1
                continue

            # Check if it's a comment line (column 7 has * or /)
            if len(line) >= 7 and line[6] in ('*', '/'):
                last_content_idx -= 1
                continue

            # Found a non-comment, non-blank line - stop here
            break

        # Return lines up to and including the last content line
        return lines[: last_content_idx + 1]

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

    def get_callers(
        self,
        file_path: str | Path,
        function_name: str,
        search_paths: list[str | Path] | None = None,
    ) -> list[dict[str, Any]]:
        """
        Find all callers of a specific function across the codebase.

        This method searches through source files to find all locations that
        reference or call the target function. For COBOL, this means finding
        PERFORM and CALL statements. For copybooks, this finds COPY statements.

        Args:
            file_path: Path to the file containing the target function.
            function_name: Name of the function/paragraph to find callers for.
            search_paths: Directories to search for callers. If None, searches
                         the same directory as the target file and subdirectories.

        Returns:
            List of caller dictionaries, each containing:
            - file: Path to the file containing the call
            - function: Name of the function making the call
            - line: Line number where the call occurs
            - type: Type of call (performs, calls, includes, etc.)

        Example:
            >>> citadel = Citadel()
            >>> callers = citadel.get_callers("UTILS.cbl", "CALCULATE-TAX")
            >>> for caller in callers:
            ...     print(f"{caller['file']}:{caller['line']} - {caller['function']}")
        """
        file_path = Path(file_path).resolve()

        # Determine search paths
        if search_paths is None:
            # Search in the same directory and subdirectories
            search_dirs = [file_path.parent]
        else:
            search_dirs = [Path(p).resolve() for p in search_paths]

        # Verify target function exists (optional validation)
        target_result = self.analyze_file(file_path)
        target_artifact = target_result.get_artifact_by_name(function_name)

        # Get the canonical name if we found the artifact
        canonical_name = target_artifact.name if target_artifact else function_name

        # Collect all source files to analyze
        files_to_analyze: list[Path] = []
        for search_dir in search_dirs:
            if not search_dir.exists():
                logger.warning("Search path does not exist: %s", search_dir)
                continue

            if search_dir.is_file():
                # If a file is specified, just add it
                files_to_analyze.append(search_dir)
            else:
                # Discover all source files in the directory
                discovery = FileDiscovery(search_dir)
                result = discovery.discover()

                # Add all identified files
                for spec_files in result.files_by_spec.values():
                    files_to_analyze.extend(spec_files)

        # Analyze each file and find callers
        callers: list[dict[str, Any]] = []
        function_name_lower = canonical_name.lower()

        for source_file in files_to_analyze:
            try:
                analysis = self.analyze_file(source_file)

                if analysis.error:
                    logger.debug(
                        "Skipping file with analysis error: %s - %s",
                        source_file,
                        analysis.error,
                    )
                    continue

                # Check file-level callouts (e.g., COPY statements at file level)
                for callout in analysis.file_level_callouts:
                    if self._matches_target(callout.target, function_name_lower):
                        callers.append({
                            "file": str(source_file),
                            "function": None,  # File-level callout
                            "line": callout.line,
                            "type": callout.relationship,
                        })

                # Check each artifact's callouts
                for artifact in analysis.artifacts:
                    for callout in artifact.callouts:
                        if self._matches_target(callout.target, function_name_lower):
                            callers.append({
                                "file": str(source_file),
                                "function": artifact.name,
                                "line": callout.line,
                                "type": callout.relationship,
                            })

            except Exception as e:
                logger.warning("Error analyzing file %s: %s", source_file, e)
                continue

        # Sort callers by file, then by line number
        callers.sort(key=lambda x: (x["file"], x["line"] or 0))

        return callers

    def _matches_target(self, callout_target: str, target_name_lower: str) -> bool:
        """
        Check if a callout target matches the target function name.

        Performs case-insensitive matching and handles common variations
        like file extensions, prefixes, etc.

        Args:
            callout_target: The target name from a callout.
            target_name_lower: The lowercase target function name to match.

        Returns:
            True if the callout references the target function.
        """
        if not callout_target:
            return False

        callout_lower = callout_target.lower()

        # Direct match
        if callout_lower == target_name_lower:
            return True

        # Match without common file extensions for copybooks
        extensions_to_strip = [".cpy", ".copy", ".cbl", ".cob", ".cobol"]
        for ext in extensions_to_strip:
            if callout_lower.endswith(ext):
                if callout_lower[: -len(ext)] == target_name_lower:
                    return True
            if target_name_lower.endswith(ext):
                if callout_lower == target_name_lower[: -len(ext)]:
                    return True

        # Match base name (useful for paths like "COPYLIB/MEMBER")
        if "/" in callout_lower:
            base_name = callout_lower.split("/")[-1]
            if base_name == target_name_lower:
                return True

        return False

    def get_sequence_diagrams(
        self,
        path: str | Path,
        max_diagrams: int = 5,
        min_sequence_length: int = 2,
    ) -> list[str]:
        """
        Generate Mermaid sequence diagrams showing call chains.

        Analyzes the dependency graph from a source directory or pre-computed
        JSON file, identifies the longest call sequences, and generates
        Mermaid sequence diagram markup for visualization.

        Args:
            path: Source directory to analyze or path to a dependency graph
                JSON file (typically generated by `citadel analyze`).
            max_diagrams: Maximum number of sequence diagrams to generate.
                Diagrams are sorted by sequence length, longest first.
            min_sequence_length: Minimum number of calls (edges) in a sequence.
                Sequences shorter than this are excluded. Default is 2,
                meaning at least 3 participants.

        Returns:
            List of Mermaid sequence diagram strings, one per call chain.
            Each diagram can be rendered directly or embedded in Markdown.

        Raises:
            FileNotFoundError: If the path does not exist.
            ValueError: If the JSON file is not a valid dependency graph.

        Example:
            >>> citadel = Citadel()
            >>> diagrams = citadel.get_sequence_diagrams("./src")
            >>> for diagram in diagrams:
            ...     print(diagram)
            ...     print("---")

            >>> # Or from a pre-computed graph
            >>> diagrams = citadel.get_sequence_diagrams("./output/graph.json")
        """
        path = Path(path)

        if not path.exists():
            raise FileNotFoundError(f"Path not found: {path}")

        # Load or build the dependency graph
        if path.is_file() and path.suffix.lower() == ".json":
            graph = self._load_graph_from_json(path)
        elif path.is_dir():
            graph = self._analyze_directory_for_graph(path)
        else:
            raise ValueError(
                f"Path must be a directory or a JSON file, got: {path}"
            )

        # Extract artifacts and edges for sequence finding
        artifacts_dict = {
            artifact_id: {
                "name": artifact.canonical_name,
                "display_name": artifact.display_name,
                "type": artifact.artifact_type.value,
                "file": (
                    Path(artifact.defined_in.file_path).name
                    if artifact.defined_in
                    else None
                ),
            }
            for artifact_id, artifact in graph.artifacts.items()
        }

        # Include resolved relationships
        edges_list = [
            {
                "source": rel.from_artifact,
                "target": rel.to_artifact,
                "relationship_type": rel.relationship_type.value,
            }
            for rel in graph.relationships
        ]

        # Also include unresolved call relationships for sequence diagrams
        # These are calls/executes/performs to external or unresolved targets
        # Infer relationship type from expected_type
        call_target_types = {"program", "procedure", "paragraph"}
        for unres in graph.unresolved:
            expected = unres.expected_type
            if expected and expected.value in call_target_types:
                # Infer relationship type based on target type
                if expected.value == "paragraph":
                    rel_type = "performs"
                elif expected.value == "procedure":
                    rel_type = "executes"
                else:
                    rel_type = "calls"

                edges_list.append({
                    "source": unres.containing_artifact,
                    "target": unres.reference_text,
                    "relationship_type": rel_type,
                })
                # Add the target as a pseudo-artifact if not already known
                if unres.reference_text not in artifacts_dict:
                    artifacts_dict[unres.reference_text] = {
                        "name": unres.reference_text,
                        "display_name": unres.reference_text,
                        "type": expected.value,
                    }

        if not edges_list:
            logger.info("No relationships found in graph, returning empty diagrams")
            return []

        # Find longest sequences
        sequences = find_longest_sequences(
            artifacts=artifacts_dict,
            edges=edges_list,
            min_length=min_sequence_length,
            max_sequences=max_diagrams,
        )

        if not sequences:
            logger.info(
                "No sequences found with min_length=%d", min_sequence_length
            )
            return []

        # Convert each sequence to a separate Mermaid diagram
        diagrams: list[str] = []
        for i, sequence in enumerate(sequences):
            title = f"Call Chain {i + 1}" if len(sequences) > 1 else None
            diagram = sequences_to_mermaid(
                [sequence],
                artifacts=artifacts_dict,
                title=title,
            )
            diagrams.append(diagram)

        logger.info(
            "Generated %d sequence diagrams from %d relationships",
            len(diagrams),
            len(edges_list),
        )

        return diagrams

    def get_dead_code(
        self,
        path: str | Path,
        exclude_types: set[str] | None = None,
        include_only_types: set[str] | None = None,
    ) -> list[dict[str, Any]]:
        """
        Find dead code (unreferenced artifacts) in a codebase.

        Analyzes the dependency graph from a source directory or pre-computed
        JSON file and identifies artifacts that are never referenced by any
        other artifact. Entry points (JCL procedures, programs, first
        paragraphs) are excluded since they are expected to have no callers.

        Args:
            path: Source directory to analyze or path to a dependency graph
                JSON file (typically generated by ``citadel analyze``).
            exclude_types: Optional set of artifact types to skip entirely.
                For example, ``{"table", "dataset"}`` to ignore data artifacts.
            include_only_types: Optional set of artifact types to analyze.
                If provided, only these types are checked. Mutually exclusive
                with ``exclude_types``.

        Returns:
            List of dictionaries, each containing:
            - ``name``: Canonical name of the unreferenced artifact.
            - ``type``: Artifact type (paragraph, copybook, program, etc.).
            - ``file``: Source file where the artifact is defined.
            - ``line``: Line number where the artifact is defined.
            - ``reason``: Human-readable explanation.

        Raises:
            FileNotFoundError: If the path does not exist.
            ValueError: If the JSON file is not a valid dependency graph,
                or if both ``exclude_types`` and ``include_only_types`` are set.

        Example:
            >>> citadel = Citadel()
            >>> dead = citadel.get_dead_code("./src")
            >>> for item in dead:
            ...     print(f"{item['name']} ({item['type']}): {item['reason']}")

            >>> # From a pre-computed graph
            >>> dead = citadel.get_dead_code("./output/graph.json")
        """
        path = Path(path)

        if not path.exists():
            raise FileNotFoundError(f"Path not found: {path}")

        # Load or build the dependency graph.
        if path.is_file() and path.suffix.lower() == ".json":
            graph = self._load_graph_from_json(path)
        elif path.is_dir():
            graph = self._analyze_directory_for_graph(path)
        else:
            raise ValueError(
                f"Path must be a directory or a JSON file, got: {path}"
            )

        # Run dead code detection.
        dead_items = find_dead_code(
            graph,
            exclude_types=exclude_types,
            include_only_types=include_only_types,
        )

        logger.info(
            "Found %d dead code artifacts in %s",
            len(dead_items),
            path,
        )

        return dead_code_to_dicts(dead_items)

    def get_flow_diagram(
        self,
        file_path: str | Path,
        paragraph: str | None = None,
        include_external: bool = True,
    ) -> str:
        """Generate a Mermaid flow diagram for a COBOL file.

        Analyzes the file and produces a Mermaid flowchart showing the
        internal control flow (PERFORM relationships between paragraphs).
        External calls (CALL, reads, writes, EXEC SQL, EXEC CICS, includes)
        appear as distinctively shaped leaf nodes but are NOT recursively
        followed.

        Args:
            file_path: Path to COBOL source file.
            paragraph: Starting paragraph name. If ``None``, shows entire
                file flow.
            include_external: Whether to show external calls as leaf nodes.

        Returns:
            Mermaid flowchart string.

        Raises:
            ValueError: If the specified paragraph does not exist in the file.
        """
        result = self.analyze_file(file_path)

        if result.error:
            return f"flowchart TD\n    %% Error: {result.error}"

        title = Path(file_path).name
        return generate_flow_diagram(
            result,
            start_paragraph=paragraph,
            include_external=include_external,
            title=title,
        )

    def _load_graph_from_json(self, json_path: Path) -> DependencyGraph:
        """
        Load a dependency graph from a JSON file.

        Args:
            json_path: Path to the JSON file.

        Returns:
            Parsed DependencyGraph instance.

        Raises:
            ValueError: If the JSON file is not a valid dependency graph.
        """
        try:
            with open(json_path, "r", encoding="utf-8") as f:
                data = json.load(f)

            return DependencyGraph.model_validate(data)
        except json.JSONDecodeError as e:
            raise ValueError(f"Invalid JSON file: {e}") from e
        except Exception as e:
            raise ValueError(f"Failed to parse dependency graph: {e}") from e

    def _analyze_directory_for_graph(self, directory: Path) -> DependencyGraph:
        """
        Analyze a directory and build a dependency graph.

        Uses the Orchestrator to perform full analysis pipeline.

        Args:
            directory: Source directory to analyze.

        Returns:
            Complete DependencyGraph from analysis.
        """
        # Import here to avoid circular imports
        from citadel.orchestrator import Orchestrator

        orchestrator = Orchestrator(self.config)

        # Run the async analysis in a sync context
        try:
            loop = asyncio.get_running_loop()
        except RuntimeError:
            loop = None

        if loop is not None:
            # We're in an async context, create a new thread
            import concurrent.futures

            with concurrent.futures.ThreadPoolExecutor() as executor:
                future = executor.submit(
                    asyncio.run, orchestrator.analyze(directory)
                )
                return future.result()
        else:
            # We're in a sync context, just run it
            return asyncio.run(orchestrator.analyze(directory))


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


def get_callers(
    file_path: str | Path,
    function_name: str,
    search_paths: list[str | Path] | None = None,
) -> list[dict[str, Any]]:
    """
    Find all callers of a specific function across the codebase.

    Convenience function for quick analysis.

    Args:
        file_path: Path to the file containing the target function.
        function_name: Name of the function/paragraph to find callers for.
        search_paths: Directories to search for callers. If None, searches
                     the same directory as the target file and subdirectories.

    Returns:
        List of caller dictionaries with keys: file, function, line, type.

    Example:
        >>> from citadel import get_callers
        >>> callers = get_callers("UTILS.cbl", "CALCULATE-TAX")
        >>> for caller in callers:
        ...     print(f"{caller['file']}:{caller['line']} - {caller['function']}")
    """
    citadel = Citadel()
    return citadel.get_callers(file_path, function_name, search_paths)


def clear_cache() -> None:
    """
    Clear all cached data (specs, parse results).

    Convenience function for clearing the cache without creating
    a Citadel instance first.

    This can help resolve issues when specs have been modified
    or when cached data becomes stale.
    """
    citadel = Citadel()
    citadel.clear_cache()


def get_sequence_diagrams(
    path: str | Path,
    max_diagrams: int = 5,
    min_sequence_length: int = 2,
) -> list[str]:
    """
    Generate Mermaid sequence diagrams showing call chains.

    Convenience function for quick sequence diagram generation.

    Args:
        path: Source directory to analyze or path to a dependency graph
            JSON file (typically generated by `citadel analyze`).
        max_diagrams: Maximum number of sequence diagrams to generate.
        min_sequence_length: Minimum number of calls in a sequence (default 2).

    Returns:
        List of Mermaid sequence diagram strings.

    Example:
        >>> from citadel.sdk import get_sequence_diagrams
        >>> diagrams = get_sequence_diagrams("./src")
        >>> for diagram in diagrams:
        ...     print(diagram)
    """
    citadel = Citadel()
    return citadel.get_sequence_diagrams(path, max_diagrams, min_sequence_length)


def get_dead_code(
    path: str | Path,
    exclude_types: set[str] | None = None,
    include_only_types: set[str] | None = None,
) -> list[dict[str, Any]]:
    """
    Find dead code (unreferenced artifacts) in a codebase.

    Convenience function for quick dead code detection.

    Args:
        path: Source directory to analyze or path to a dependency graph
            JSON file (typically generated by ``citadel analyze``).
        exclude_types: Optional set of artifact types to skip entirely.
        include_only_types: Optional set of artifact types to analyze.

    Returns:
        List of dictionaries with keys: name, type, file, line, reason.

    Example:
        >>> from citadel.sdk import get_dead_code
        >>> dead = get_dead_code("./src")
        >>> for item in dead:
        ...     print(f"{item['name']} ({item['type']}): {item['reason']}")
    """
    citadel = Citadel()
    return citadel.get_dead_code(path, exclude_types, include_only_types)


def get_flow_diagram(
    file_path: str | Path,
    paragraph: str | None = None,
    include_external: bool = True,
) -> str:
    """
    Generate a Mermaid flow diagram for a COBOL file.

    Convenience function for quick flow diagram generation.

    Args:
        file_path: Path to COBOL source file.
        paragraph: Starting paragraph name. If None, shows entire file flow.
        include_external: Whether to show external calls as leaf nodes.

    Returns:
        Mermaid flowchart string.

    Example:
        >>> from citadel.sdk import get_flow_diagram
        >>> diagram = get_flow_diagram("PROGRAM.cbl")
        >>> print(diagram)
    """
    citadel = Citadel()
    return citadel.get_flow_diagram(file_path, paragraph, include_external)
