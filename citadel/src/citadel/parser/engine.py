"""
Parser engine for Citadel.

This module provides the ParserEngine class for deterministic pattern-based
extraction of artifacts and references from source files, as well as the
internal data structures used during parsing.
"""

import logging
import re
from pathlib import Path

from pydantic import BaseModel, Field

from citadel.graph.model import Artifact, SourceLocation
from citadel.parser.preprocessor import Preprocessor, PreprocessedSource
from citadel.specs.schema import (
    ArtifactCategory,
    ArtifactSpec,
    ArtifactType,
    ExtractionPattern,
    RelationshipType,
)

logger = logging.getLogger(__name__)


class RawReference(BaseModel):
    """A reference before resolution (internal use)."""

    raw_text: str
    pattern_name: str  # Which pattern matched
    expected_type: ArtifactType | None = None
    relationship_type: RelationshipType
    location: SourceLocation
    containing_artifact: str | None = None  # ID of artifact containing this
    columns_mentioned: list[str] = Field(default_factory=list)
    context_lines: list[str] = Field(default_factory=list)  # Surrounding code for LLM


class FileParseResult(BaseModel):
    """Result of parsing a single file (internal use)."""

    file_path: str
    language: str
    spec_id: str

    artifacts_defined: list[Artifact] = Field(default_factory=list)
    references_found: list[RawReference] = Field(default_factory=list)
    preprocessor_directives: list[dict] = Field(default_factory=list)  # COPY/INCLUDE info

    errors: list[str] = Field(default_factory=list)
    warnings: list[str] = Field(default_factory=list)


class ParserEngine:
    """
    Deterministic pattern-based extraction engine.

    Uses artifact specifications to extract definitions and references
    from source files through regex pattern matching.
    """

    def __init__(self) -> None:
        """Initialize parser with compiled pattern cache."""
        self._pattern_cache: dict[str, re.Pattern] = {}

    def parse_file(
        self,
        file_path: Path,
        content: str,
        spec: ArtifactSpec,
    ) -> FileParseResult:
        """
        Parse a single file using its spec.

        Process:
        1. Preprocess (strip comments, handle continuations)
        2. Extract definitions
        3. Extract references
        4. Extract preprocessor directives

        Args:
            file_path: Path to the source file
            content: Content of the source file
            spec: Artifact specification for this file type

        Returns:
            FileParseResult containing extracted artifacts and references
        """
        result = FileParseResult(
            file_path=str(file_path),
            language=spec.language,
            spec_id=spec.spec_id,
        )

        try:
            # Step 1: Preprocess the content
            preprocessor = Preprocessor(spec)
            preprocessed = preprocessor.process(content)

            # Step 2: Extract definitions
            definitions = self._extract_definitions(
                preprocessed.cleaned,
                spec,
                file_path,
                preprocessed.line_map,
            )

            # Step 2.5: Create file-level artifact if spec requires it
            if spec.create_file_artifact:
                file_artifact = self._create_file_artifact(file_path, spec)
                definitions.insert(0, file_artifact)

            result.artifacts_defined = definitions

            # Step 3: Extract references
            references = self._extract_references(
                preprocessed.cleaned,
                spec,
                file_path,
                definitions,
                preprocessed.line_map,
            )
            result.references_found = references

            # Step 4: Extract preprocessor directives
            directives = self._extract_preprocessor(
                preprocessed.cleaned,
                spec,
                file_path,
                preprocessed.line_map,
            )
            result.preprocessor_directives = directives

        except Exception as e:
            logger.exception("Error parsing file %s: %s", file_path, e)
            result.errors.append(f"Parse error: {e}")

        return result

    def _compile_pattern(self, pattern: ExtractionPattern) -> re.Pattern:
        """
        Compile and cache regex pattern.

        Uses pattern name as cache key to avoid recompiling the same pattern.

        Args:
            pattern: ExtractionPattern containing regex and flags

        Returns:
            Compiled regex pattern
        """
        cache_key = f"{pattern.name}:{pattern.pattern}:{pattern.ignore_case}:{pattern.multiline}:{pattern.dotall}"

        if cache_key not in self._pattern_cache:
            flags = 0
            if pattern.ignore_case:
                flags |= re.IGNORECASE
            if pattern.multiline:
                flags |= re.MULTILINE
            if pattern.dotall:
                flags |= re.DOTALL

            try:
                compiled = re.compile(pattern.pattern, flags)
                self._pattern_cache[cache_key] = compiled
            except re.error as e:
                logger.error("Invalid regex pattern '%s' in %s: %s", pattern.pattern, pattern.name, e)
                raise ValueError(f"Invalid regex pattern in {pattern.name}: {e}") from e

        return self._pattern_cache[cache_key]

    def _get_line_number(self, content: str, position: int) -> int:
        """
        Get line number for a position in content.

        Args:
            content: The source content
            position: Character position in content

        Returns:
            1-indexed line number
        """
        return content[:position].count("\n") + 1

    def _get_context_lines(
        self,
        content: str,
        line_num: int,
        context_size: int = 2,
    ) -> list[str]:
        """
        Get surrounding lines for context.

        Args:
            content: The source content
            line_num: 1-indexed line number
            context_size: Number of lines before and after to include

        Returns:
            List of context lines
        """
        lines = content.splitlines()
        start = max(0, line_num - context_size - 1)
        end = min(len(lines), line_num + context_size)
        return lines[start:end]

    def _build_source_location(
        self,
        file_path: Path,
        content: str,
        match: re.Match,
        line_map: dict[int, int],
    ) -> SourceLocation:
        """
        Build SourceLocation from a regex match.

        Args:
            file_path: Path to the source file
            content: The source content
            match: Regex match object
            line_map: Mapping from cleaned line numbers to original

        Returns:
            SourceLocation for the match
        """
        start_pos = match.start()
        end_pos = match.end()

        cleaned_line_start = self._get_line_number(content, start_pos)
        cleaned_line_end = self._get_line_number(content, end_pos)

        # Map back to original line numbers
        original_line_start = line_map.get(cleaned_line_start, cleaned_line_start)
        original_line_end = line_map.get(cleaned_line_end, cleaned_line_end)

        # Calculate column positions
        line_start_pos = content.rfind("\n", 0, start_pos) + 1
        column_start = start_pos - line_start_pos + 1

        return SourceLocation(
            file_path=str(file_path),
            line_start=original_line_start,
            line_end=original_line_end if original_line_end != original_line_start else None,
            column_start=column_start,
        )

    def _extract_identifier(
        self,
        match: re.Match,
        pattern: ExtractionPattern,
    ) -> str:
        """
        Extract identifier from match using capture groups.

        Args:
            match: Regex match object
            pattern: ExtractionPattern with capture_groups and join_with

        Returns:
            Extracted identifier string
        """
        parts = []
        for group_num in pattern.capture_groups:
            try:
                group_value = match.group(group_num)
                if group_value is not None:
                    parts.append(group_value)
            except IndexError:
                logger.warning(
                    "Capture group %d not found in pattern %s",
                    group_num,
                    pattern.name,
                )

        return pattern.join_with.join(parts)

    def _check_context_constraints(
        self,
        content: str,
        match: re.Match,
        pattern: ExtractionPattern,
    ) -> bool:
        """
        Check if match satisfies context constraints.

        Args:
            content: The source content
            match: Regex match object
            pattern: ExtractionPattern with context constraints

        Returns:
            True if all constraints are satisfied
        """
        # Check must_not_follow (negative lookbehind)
        if pattern.must_not_follow:
            try:
                lookbehind_pattern = re.compile(pattern.must_not_follow)
                # Check the text immediately before the match
                prefix = content[max(0, match.start() - 100) : match.start()]
                if lookbehind_pattern.search(prefix):
                    return False
            except re.error as e:
                logger.warning("Invalid must_not_follow pattern: %s", e)

        # Check must_not_precede (negative lookahead)
        if pattern.must_not_precede:
            try:
                lookahead_pattern = re.compile(pattern.must_not_precede)
                # Check the text immediately after the match
                suffix = content[match.end() : min(len(content), match.end() + 100)]
                if lookahead_pattern.search(suffix):
                    return False
            except re.error as e:
                logger.warning("Invalid must_not_precede pattern: %s", e)

        return True

    def _extract_definitions(
        self,
        content: str,
        spec: ArtifactSpec,
        file_path: Path,
        line_map: dict[int, int],
    ) -> list[Artifact]:
        """
        Extract all artifact definitions from content.

        Args:
            content: Preprocessed source content
            spec: Artifact specification
            file_path: Path to the source file
            line_map: Mapping from cleaned line numbers to original

        Returns:
            List of extracted Artifact objects
        """
        artifacts: list[Artifact] = []

        for pattern in spec.definition_patterns:
            try:
                compiled = self._compile_pattern(pattern)

                for match in compiled.finditer(content):
                    # Check context constraints
                    if not self._check_context_constraints(content, match, pattern):
                        continue

                    # Extract the identifier
                    identifier = self._extract_identifier(match, pattern)
                    if not identifier:
                        continue

                    # Determine artifact type
                    artifact_type = pattern.artifact_type or spec.primary_artifact_type

                    # Build source location
                    location = self._build_source_location(
                        file_path, content, match, line_map
                    )

                    # Determine category based on artifact type
                    category = self._get_category_for_type(artifact_type)

                    # Create artifact ID
                    artifact_id = f"{artifact_type.value}::{identifier}"

                    artifact = Artifact(
                        id=artifact_id,
                        artifact_type=artifact_type,
                        category=category,
                        canonical_name=identifier,
                        defined_in=location,
                        language=spec.language,
                    )

                    artifacts.append(artifact)
                    logger.debug(
                        "Extracted definition: %s at %s",
                        artifact_id,
                        location,
                    )

            except Exception as e:
                logger.warning(
                    "Error processing definition pattern '%s': %s",
                    pattern.name,
                    e,
                )

        return artifacts

    def _get_category_for_type(self, artifact_type: ArtifactType) -> ArtifactCategory:
        """
        Determine the category for an artifact type.

        Args:
            artifact_type: The artifact type

        Returns:
            Corresponding artifact category
        """
        code_types = {
            ArtifactType.PROGRAM,
            ArtifactType.PARAGRAPH,
            ArtifactType.PROCEDURE,
            ArtifactType.COPYBOOK,
            ArtifactType.MACRO,
            ArtifactType.FUNCTION,
            ArtifactType.CLASS,
            ArtifactType.METHOD,
            ArtifactType.MODULE,
        }
        data_types = {
            ArtifactType.TABLE,
            ArtifactType.VIEW,
            ArtifactType.FILE,
            ArtifactType.RECORD_LAYOUT,
            ArtifactType.COLUMN,
            ArtifactType.INDEX,
            ArtifactType.SEGMENT,
            ArtifactType.DATASET,
        }
        interface_types = {
            ArtifactType.TRANSACTION,
            ArtifactType.SCREEN,
            ArtifactType.QUEUE,
            ArtifactType.SERVICE,
            ArtifactType.MAP,
        }

        if artifact_type in code_types:
            return ArtifactCategory.CODE
        elif artifact_type in data_types:
            return ArtifactCategory.DATA
        elif artifact_type in interface_types:
            return ArtifactCategory.INTERFACE
        else:
            # Default to code if unknown
            return ArtifactCategory.CODE

    def _create_file_artifact(
        self,
        file_path: Path,
        spec: ArtifactSpec,
    ) -> Artifact:
        """
        Create an artifact from the file stem.

        This is used for specs where the filename itself represents an artifact,
        such as COBOL copybooks where COPY CVACT01Y references the file CVACT01Y.cpy.

        Args:
            file_path: Path to the source file
            spec: Artifact specification

        Returns:
            Artifact created from the file stem
        """
        # Extract the file stem (filename without extension)
        file_stem = file_path.stem

        # Apply naming conventions (e.g., uppercase for COBOL)
        if spec.naming.case == "upper":
            canonical_name = file_stem.upper()
        elif spec.naming.case == "lower":
            canonical_name = file_stem.lower()
        else:
            canonical_name = file_stem

        artifact_type = spec.primary_artifact_type
        category = self._get_category_for_type(artifact_type)
        artifact_id = f"{artifact_type.value}::{canonical_name}"

        # Create a source location pointing to the start of the file
        location = SourceLocation(
            file_path=str(file_path),
            line_start=1,
            line_end=None,
            column_start=1,
        )

        artifact = Artifact(
            id=artifact_id,
            artifact_type=artifact_type,
            category=category,
            canonical_name=canonical_name,
            defined_in=location,
            language=spec.language,
        )

        logger.debug(
            "Created file artifact: %s from file %s",
            artifact_id,
            file_path,
        )

        return artifact

    def _extract_references(
        self,
        content: str,
        spec: ArtifactSpec,
        file_path: Path,
        defined_artifacts: list[Artifact],
        line_map: dict[int, int],
    ) -> list[RawReference]:
        """
        Extract all references from content.

        Args:
            content: Preprocessed source content
            spec: Artifact specification
            file_path: Path to the source file
            defined_artifacts: Artifacts defined in this file (for containing_artifact)
            line_map: Mapping from cleaned line numbers to original

        Returns:
            List of extracted RawReference objects
        """
        references: list[RawReference] = []

        for pattern in spec.reference_patterns:
            if pattern.relationship_type is None:
                logger.warning(
                    "Reference pattern '%s' has no relationship_type, skipping",
                    pattern.name,
                )
                continue

            try:
                compiled = self._compile_pattern(pattern)

                for match in compiled.finditer(content):
                    # Check context constraints
                    if not self._check_context_constraints(content, match, pattern):
                        continue

                    # Extract the identifier
                    identifier = self._extract_identifier(match, pattern)
                    if not identifier:
                        continue

                    # Build source location
                    location = self._build_source_location(
                        file_path, content, match, line_map
                    )

                    # Get context lines
                    line_num = self._get_line_number(content, match.start())
                    context_lines = self._get_context_lines(content, line_num)

                    # Find containing artifact
                    containing_artifact = self._find_containing_artifact(
                        defined_artifacts, location.line_start
                    )

                    # Extract column names if pattern specifies
                    columns_mentioned: list[str] = []
                    if pattern.column_pattern:
                        columns_mentioned = self._extract_columns(
                            match.group(0), pattern.column_pattern
                        )

                    reference = RawReference(
                        raw_text=identifier,
                        pattern_name=pattern.name,
                        expected_type=pattern.target_type_hint,
                        relationship_type=pattern.relationship_type,
                        location=location,
                        containing_artifact=containing_artifact,
                        columns_mentioned=columns_mentioned,
                        context_lines=context_lines,
                    )

                    references.append(reference)
                    logger.debug(
                        "Extracted reference: %s (%s) at %s",
                        identifier,
                        pattern.relationship_type.value,
                        location,
                    )

            except Exception as e:
                logger.warning(
                    "Error processing reference pattern '%s': %s",
                    pattern.name,
                    e,
                )

        return references

    def _find_containing_artifact(
        self,
        artifacts: list[Artifact],
        line_num: int,
    ) -> str | None:
        """
        Find which artifact contains a given line.

        Simple heuristic: the artifact defined on or before the line
        that is closest to it.

        Args:
            artifacts: List of defined artifacts
            line_num: Line number to find container for

        Returns:
            Artifact ID or None if not found
        """
        containing: Artifact | None = None
        closest_distance = float("inf")

        for artifact in artifacts:
            if artifact.defined_in is None:
                continue

            artifact_line = artifact.defined_in.line_start
            if artifact_line <= line_num:
                distance = line_num - artifact_line
                if distance < closest_distance:
                    closest_distance = distance
                    containing = artifact

        return containing.id if containing else None

    def _extract_columns(self, text: str, column_pattern: str) -> list[str]:
        """
        Extract column names from text using a pattern.

        Args:
            text: Text to search for columns
            column_pattern: Regex pattern to extract column names

        Returns:
            List of extracted column names
        """
        try:
            pattern = re.compile(column_pattern, re.IGNORECASE)
            return pattern.findall(text)
        except re.error as e:
            logger.warning("Invalid column pattern '%s': %s", column_pattern, e)
            return []

    def _extract_preprocessor(
        self,
        content: str,
        spec: ArtifactSpec,
        file_path: Path,
        line_map: dict[int, int],
    ) -> list[dict]:
        """
        Extract preprocessor directives (COPY, INCLUDE).

        Args:
            content: Preprocessed source content
            spec: Artifact specification
            file_path: Path to the source file
            line_map: Mapping from cleaned line numbers to original

        Returns:
            List of directive dictionaries with type, target, and location
        """
        directives: list[dict] = []

        for pattern in spec.preprocessor_patterns:
            try:
                compiled = self._compile_pattern(pattern)

                for match in compiled.finditer(content):
                    # Check context constraints
                    if not self._check_context_constraints(content, match, pattern):
                        continue

                    # Extract the identifier
                    identifier = self._extract_identifier(match, pattern)
                    if not identifier:
                        continue

                    # Build source location
                    location = self._build_source_location(
                        file_path, content, match, line_map
                    )

                    directive = {
                        "type": pattern.name,
                        "target": identifier,
                        "target_type": (
                            pattern.target_type_hint.value
                            if pattern.target_type_hint
                            else None
                        ),
                        "relationship_type": (
                            pattern.relationship_type.value
                            if pattern.relationship_type
                            else None
                        ),
                        "location": {
                            "file_path": str(file_path),
                            "line_start": location.line_start,
                            "line_end": location.line_end,
                            "column_start": location.column_start,
                        },
                        "raw_match": match.group(0),
                    }

                    directives.append(directive)
                    logger.debug(
                        "Extracted preprocessor directive: %s -> %s at line %d",
                        pattern.name,
                        identifier,
                        location.line_start,
                    )

            except Exception as e:
                logger.warning(
                    "Error processing preprocessor pattern '%s': %s",
                    pattern.name,
                    e,
                )

        return directives
