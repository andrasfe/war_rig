"""COBOL-aware chunk splitter.

This module provides a splitter that understands COBOL source structure
and can split at semantic boundaries (DIVISION, SECTION, PARAGRAPH).

The splitter follows these requirements from the spec:
- Deterministic: Same source + profile = same chunk boundaries
- Semantic awareness: Prefer division/section/paragraph boundaries
- Context-bounded: Chunks must fit within token budget
"""

import re
from dataclasses import dataclass, field
from typing import NamedTuple

from war_rig.chunking.models import ChunkKind, ChunkSpec, SplitterProfile
from war_rig.chunking.base import Splitter, SplitResult


class SemanticBoundary(NamedTuple):
    """A semantic boundary in COBOL source code.

    Attributes:
        line_number: 1-indexed line number where boundary starts.
        name: Name of the division/section/paragraph.
        kind: Classification of the boundary.
        level: Hierarchy level (0=division, 1=section, 2=paragraph).
    """

    line_number: int
    name: str
    kind: ChunkKind
    level: int


@dataclass
class COBOLStructure:
    """Parsed COBOL program structure.

    Attributes:
        boundaries: All semantic boundaries found.
        divisions: Division boundaries indexed by name.
        sections: Section boundaries indexed by name.
        paragraphs: Paragraph boundaries indexed by name.
        total_lines: Total line count.
    """

    boundaries: list[SemanticBoundary] = field(default_factory=list)
    divisions: dict[str, SemanticBoundary] = field(default_factory=dict)
    sections: dict[str, SemanticBoundary] = field(default_factory=dict)
    paragraphs: dict[str, SemanticBoundary] = field(default_factory=dict)
    total_lines: int = 0


class COBOLSplitter(Splitter):
    """COBOL-aware source code splitter.

    Parses COBOL structure and creates chunks at semantic boundaries
    (divisions, sections, paragraphs) while respecting token budgets.

    Design Principles:
        - Deterministic chunking for same input
        - Prefers semantic boundaries over arbitrary line splits
        - Ensures chunks fit within context budget
        - Creates chunk kinds to support targeted follow-ups

    Artifact Types:
        This splitter handles the following artifact types:
        - "cobol": COBOL source programs (.cbl, .cob files)
        - "copybook": COBOL copybooks (.cpy files)

    Example:
        >>> splitter = COBOLSplitter()
        >>> profile = SplitterProfile(max_chunk_tokens=3500)
        >>> result = splitter.split(cobol_source, profile, "DRKBM100.cbl")
        >>> print(f"Created {len(result.chunks)} chunks")
    """

    @classmethod
    def get_artifact_types(cls) -> list[str]:
        """Return the artifact types this splitter handles.

        The COBOLSplitter handles COBOL source programs and copybooks.
        Both share similar structure and can be processed with the
        same semantic parsing logic.

        Returns:
            List containing "cobol" and "copybook".
        """
        return ["cobol", "copybook"]

    # COBOL division patterns (case-insensitive)
    # Format: DIVISION-NAME DIVISION.
    # Allow flexible leading whitespace to handle various COBOL formatting styles
    DIVISION_PATTERN = re.compile(
        r"^\s*(IDENTIFICATION|ENVIRONMENT|DATA|PROCEDURE)\s+DIVISION\s*\.?\s*$",
        re.IGNORECASE | re.MULTILINE,
    )

    # COBOL section patterns (within divisions)
    # Format: SECTION-NAME SECTION.
    SECTION_PATTERN = re.compile(
        r"^\s*([A-Z0-9][A-Z0-9\-]*)\s+SECTION\s*\.?\s*$",
        re.IGNORECASE | re.MULTILINE,
    )

    # COBOL paragraph patterns (in PROCEDURE DIVISION)
    # Format: PARAGRAPH-NAME. (at start of line, followed by statements)
    # Paragraph names start with letter, can contain letters, numbers, hyphens
    PARAGRAPH_PATTERN = re.compile(
        r"^\s*([A-Z][A-Z0-9\-]*)\s*\.\s*$",
        re.IGNORECASE | re.MULTILINE,
    )

    # Division name to ChunkKind mapping
    DIVISION_TO_KIND: dict[str, ChunkKind] = {
        "IDENTIFICATION": ChunkKind.IDENTIFICATION_DIVISION,
        "ENVIRONMENT": ChunkKind.ENVIRONMENT_DIVISION,
        "DATA": ChunkKind.DATA_DIVISION,
        "PROCEDURE": ChunkKind.PROCEDURE_DIVISION,
    }

    # Section name to ChunkKind mapping for DATA DIVISION
    SECTION_TO_KIND: dict[str, ChunkKind] = {
        "FILE": ChunkKind.FILE_SECTION,
        "WORKING-STORAGE": ChunkKind.WORKING_STORAGE,
        "LINKAGE": ChunkKind.LINKAGE_SECTION,
    }

    # Average characters per token for estimation
    CHARS_PER_TOKEN = 4

    def split(
        self,
        source: str,
        profile: SplitterProfile,
        artifact_id: str,
    ) -> SplitResult:
        """Split COBOL source code into analyzable chunks.

        The splitting algorithm:
        1. Parse COBOL structure to identify divisions/sections/paragraphs
        2. If semantic splitting enabled, create chunks at semantic boundaries
        3. If chunks exceed token budget, split further at sub-boundaries
        4. Apply overlap between chunks for context continuity

        Args:
            source: The COBOL source code to split.
            profile: Splitter configuration with token limits.
            artifact_id: Identifier for generating chunk IDs.

        Returns:
            SplitResult with chunk specifications.
        """
        lines = source.split("\n")
        structure = self._parse_structure(source, lines)
        warnings: list[str] = []

        if profile.prefer_semantic and structure.boundaries:
            chunks = self._split_semantic(
                source,
                lines,
                structure,
                profile,
                artifact_id,
            )
        else:
            # Fall back to line-based splitting
            chunks = self._split_by_lines(
                source,
                lines,
                profile,
                artifact_id,
            )

        # Validate and warn about oversized chunks
        for chunk in chunks:
            validation_errors = self.validate_chunk(chunk, profile)
            warnings.extend(validation_errors)

        return SplitResult(
            chunks=chunks,
            total_lines=structure.total_lines,
            total_estimated_tokens=self.estimate_tokens(source),
            semantic_boundaries_found=len(structure.boundaries),
            warnings=warnings,
        )

    def estimate_tokens(self, text: str) -> int:
        """Estimate token count for text.

        Uses a simple heuristic of ~4 characters per token.
        This is an approximation; actual tokenization varies by model.

        Args:
            text: Text to estimate tokens for.

        Returns:
            Estimated token count.
        """
        return max(1, len(text) // self.CHARS_PER_TOKEN)

    def detect_semantic_boundaries(
        self,
        source: str,
    ) -> list[tuple[int, str, ChunkKind]]:
        """Detect all semantic boundaries in COBOL source.

        Finds divisions, sections, and paragraphs.

        Args:
            source: The COBOL source code.

        Returns:
            List of (line_number, name, chunk_kind) tuples.
        """
        lines = source.split("\n")
        structure = self._parse_structure(source, lines)
        return [
            (b.line_number, b.name, b.kind)
            for b in structure.boundaries
        ]

    def _parse_structure(
        self,
        source: str,
        lines: list[str],
    ) -> COBOLStructure:
        """Parse COBOL source to extract structure.

        Identifies all divisions, sections, and paragraphs.

        Args:
            source: Full source text.
            lines: Source split into lines.

        Returns:
            COBOLStructure with all boundaries.
        """
        structure = COBOLStructure(total_lines=len(lines))
        current_division: str | None = None
        in_procedure_division = False

        for i, line in enumerate(lines):
            line_number = i + 1  # 1-indexed
            upper_line = line.upper().strip()

            # Skip empty lines and comments (column 7 = *)
            if not upper_line or (len(line) > 6 and line[6] == "*"):
                continue

            # Check for division
            division_match = self.DIVISION_PATTERN.match(line)
            if division_match:
                division_name = division_match.group(1).upper()
                kind = self.DIVISION_TO_KIND.get(division_name, ChunkKind.GENERIC)
                boundary = SemanticBoundary(
                    line_number=line_number,
                    name=f"{division_name} DIVISION",
                    kind=kind,
                    level=0,
                )
                structure.boundaries.append(boundary)
                structure.divisions[division_name] = boundary
                current_division = division_name
                in_procedure_division = division_name == "PROCEDURE"
                continue

            # Check for section
            section_match = self.SECTION_PATTERN.match(line)
            if section_match and not in_procedure_division:
                # Sections in DATA DIVISION
                section_name = section_match.group(1).upper()
                kind = self.SECTION_TO_KIND.get(section_name, ChunkKind.GENERIC)
                boundary = SemanticBoundary(
                    line_number=line_number,
                    name=f"{section_name} SECTION",
                    kind=kind,
                    level=1,
                )
                structure.boundaries.append(boundary)
                structure.sections[section_name] = boundary
                continue

            # Check for paragraph (only in PROCEDURE DIVISION)
            if in_procedure_division:
                # Paragraphs are names at start of line followed by period
                # Must not be a reserved word
                para_match = self.PARAGRAPH_PATTERN.match(line)
                if para_match:
                    para_name = para_match.group(1).upper()
                    # Skip if it looks like a section
                    if "SECTION" not in upper_line:
                        boundary = SemanticBoundary(
                            line_number=line_number,
                            name=para_name,
                            kind=ChunkKind.PROCEDURE_PART,
                            level=2,
                        )
                        structure.boundaries.append(boundary)
                        structure.paragraphs[para_name] = boundary

        return structure

    def _split_semantic(
        self,
        source: str,
        lines: list[str],
        structure: COBOLStructure,
        profile: SplitterProfile,
        artifact_id: str,
    ) -> list[ChunkSpec]:
        """Split at semantic boundaries.

        Creates chunks aligned with COBOL structure. If a semantic
        unit exceeds the token budget, it's further split into parts.

        Args:
            source: Full source text.
            lines: Source split into lines.
            structure: Parsed COBOL structure.
            profile: Splitter configuration.
            artifact_id: For generating chunk IDs.

        Returns:
            List of ChunkSpec objects.
        """
        chunks: list[ChunkSpec] = []

        # Sort boundaries by line number
        sorted_boundaries = sorted(structure.boundaries, key=lambda b: b.line_number)

        if not sorted_boundaries:
            # No boundaries found, use line-based splitting
            return self._split_by_lines(source, lines, profile, artifact_id)

        # Create chunks for each semantic boundary
        for i, boundary in enumerate(sorted_boundaries):
            # Determine end line
            if i + 1 < len(sorted_boundaries):
                end_line = sorted_boundaries[i + 1].line_number - 1
            else:
                end_line = structure.total_lines

            # Get chunk content
            chunk_lines = lines[boundary.line_number - 1:end_line]
            chunk_text = "\n".join(chunk_lines)
            estimated_tokens = self.estimate_tokens(chunk_text)

            # Check if chunk fits within budget
            if estimated_tokens <= profile.max_chunk_tokens:
                # Create single chunk
                chunk = self._create_chunk_spec(
                    artifact_id=artifact_id,
                    kind=boundary.kind,
                    start_line=boundary.line_number,
                    end_line=end_line,
                    division=self._get_current_division(boundary, sorted_boundaries),
                    section=boundary.name if boundary.level == 1 else None,
                    paragraphs=self._get_paragraphs_in_range(
                        structure, boundary.line_number, end_line
                    ),
                    estimated_tokens=estimated_tokens,
                    index=len(chunks),
                    name=boundary.name,
                )
                chunks.append(chunk)
            else:
                # Split oversized chunk into parts
                sub_chunks = self._split_oversized_chunk(
                    lines=lines,
                    start_line=boundary.line_number,
                    end_line=end_line,
                    kind=boundary.kind,
                    artifact_id=artifact_id,
                    profile=profile,
                    structure=structure,
                    base_index=len(chunks),
                    boundary=boundary,
                    sorted_boundaries=sorted_boundaries,
                )
                chunks.extend(sub_chunks)

        return chunks

    def _split_by_lines(
        self,
        source: str,
        lines: list[str],
        profile: SplitterProfile,
        artifact_id: str,
    ) -> list[ChunkSpec]:
        """Split by line count when semantic splitting not available.

        Args:
            source: Full source text.
            lines: Source split into lines.
            profile: Splitter configuration.
            artifact_id: For generating chunk IDs.

        Returns:
            List of ChunkSpec objects.
        """
        chunks: list[ChunkSpec] = []
        total_lines = len(lines)

        # Calculate lines per chunk based on token budget
        # Estimate ~10 tokens per line on average for COBOL
        lines_per_chunk = max(1, profile.max_chunk_tokens // 10)

        current_line = 1
        chunk_index = 0

        while current_line <= total_lines:
            end_line = min(current_line + lines_per_chunk - 1, total_lines)

            # Include overlap from previous chunk
            if chunk_index > 0 and profile.overlap_lines > 0:
                overlap_start = max(1, current_line - profile.overlap_lines)
            else:
                overlap_start = current_line

            chunk_lines = lines[overlap_start - 1:end_line]
            chunk_text = "\n".join(chunk_lines)
            estimated_tokens = self.estimate_tokens(chunk_text)

            chunk = ChunkSpec(
                chunk_id=self.generate_chunk_id(
                    artifact_id, ChunkKind.GENERIC, chunk_index
                ),
                chunk_kind=ChunkKind.GENERIC,
                start_line=overlap_start,
                end_line=end_line,
                estimated_tokens=estimated_tokens,
            )
            chunks.append(chunk)

            current_line = end_line + 1
            chunk_index += 1

        return chunks

    def _split_oversized_chunk(
        self,
        lines: list[str],
        start_line: int,
        end_line: int,
        kind: ChunkKind,
        artifact_id: str,
        profile: SplitterProfile,
        structure: COBOLStructure,
        base_index: int,
        boundary: SemanticBoundary,
        sorted_boundaries: list[SemanticBoundary],
    ) -> list[ChunkSpec]:
        """Split an oversized semantic chunk into smaller parts.

        For PROCEDURE DIVISION, tries to split at paragraph boundaries.
        Otherwise falls back to line-based splitting.

        Args:
            lines: All source lines.
            start_line: Start of the chunk (1-indexed).
            end_line: End of the chunk (1-indexed).
            kind: Chunk kind.
            artifact_id: For generating chunk IDs.
            profile: Splitter configuration.
            structure: Parsed structure.
            base_index: Starting index for chunk IDs.
            boundary: The boundary being split.
            sorted_boundaries: All boundaries.

        Returns:
            List of ChunkSpec objects.
        """
        chunks: list[ChunkSpec] = []

        # If in PROCEDURE DIVISION, try to split at paragraph boundaries
        if kind in (ChunkKind.PROCEDURE_DIVISION, ChunkKind.PROCEDURE_PART):
            paragraphs_in_range = [
                b for b in sorted_boundaries
                if b.level == 2 and start_line <= b.line_number <= end_line
            ]

            if len(paragraphs_in_range) > 1:
                # Group paragraphs into chunks that fit budget
                current_start = start_line
                current_paras: list[str] = []
                para_index = 0

                for j, para in enumerate(paragraphs_in_range):
                    # Determine paragraph end
                    if j + 1 < len(paragraphs_in_range):
                        para_end = paragraphs_in_range[j + 1].line_number - 1
                    else:
                        para_end = end_line

                    # Check if adding this paragraph exceeds budget
                    test_end = para_end
                    chunk_lines = lines[current_start - 1:test_end]
                    chunk_text = "\n".join(chunk_lines)
                    test_tokens = self.estimate_tokens(chunk_text)

                    if test_tokens > profile.max_chunk_tokens and current_paras:
                        # Create chunk with current paragraphs
                        prev_para_idx = j - 1
                        if prev_para_idx >= 0:
                            chunk_end = paragraphs_in_range[prev_para_idx].line_number - 1
                        else:
                            chunk_end = para.line_number - 1

                        chunk_lines = lines[current_start - 1:chunk_end]
                        chunk_text = "\n".join(chunk_lines)

                        chunk = self._create_chunk_spec(
                            artifact_id=artifact_id,
                            kind=ChunkKind.PROCEDURE_PART,
                            start_line=current_start,
                            end_line=chunk_end,
                            division="PROCEDURE",
                            paragraphs=current_paras.copy(),
                            estimated_tokens=self.estimate_tokens(chunk_text),
                            index=base_index + len(chunks),
                        )
                        chunks.append(chunk)

                        # Start new chunk
                        current_start = para.line_number
                        current_paras = [para.name]
                    else:
                        current_paras.append(para.name)

                # Create final chunk with remaining paragraphs
                if current_paras:
                    chunk_lines = lines[current_start - 1:end_line]
                    chunk_text = "\n".join(chunk_lines)

                    chunk = self._create_chunk_spec(
                        artifact_id=artifact_id,
                        kind=ChunkKind.PROCEDURE_PART,
                        start_line=current_start,
                        end_line=end_line,
                        division="PROCEDURE",
                        paragraphs=current_paras,
                        estimated_tokens=self.estimate_tokens(chunk_text),
                        index=base_index + len(chunks),
                    )
                    chunks.append(chunk)

                return chunks

        # Fall back to line-based splitting for oversized chunk
        lines_per_chunk = max(1, profile.max_chunk_tokens // 10)
        current_line = start_line
        part_index = 0

        while current_line <= end_line:
            chunk_end = min(current_line + lines_per_chunk - 1, end_line)
            chunk_lines = lines[current_line - 1:chunk_end]
            chunk_text = "\n".join(chunk_lines)

            chunk = ChunkSpec(
                chunk_id=self.generate_chunk_id(
                    artifact_id,
                    kind if kind != ChunkKind.PROCEDURE_DIVISION else ChunkKind.PROCEDURE_PART,
                    base_index + part_index,
                ),
                chunk_kind=kind if kind != ChunkKind.PROCEDURE_DIVISION else ChunkKind.PROCEDURE_PART,
                start_line=current_line,
                end_line=chunk_end,
                division=self._get_current_division(boundary, sorted_boundaries),
                paragraphs=self._get_paragraphs_in_range(
                    structure, current_line, chunk_end
                ),
                estimated_tokens=self.estimate_tokens(chunk_text),
            )
            chunks.append(chunk)

            current_line = chunk_end + 1
            part_index += 1

        return chunks

    def _create_chunk_spec(
        self,
        artifact_id: str,
        kind: ChunkKind,
        start_line: int,
        end_line: int,
        estimated_tokens: int,
        index: int,
        division: str | None = None,
        section: str | None = None,
        paragraphs: list[str] | None = None,
        name: str | None = None,
    ) -> ChunkSpec:
        """Create a ChunkSpec with generated ID.

        Args:
            artifact_id: Source artifact identifier.
            kind: Chunk classification.
            start_line: Start line (1-indexed).
            end_line: End line (1-indexed).
            estimated_tokens: Estimated token count.
            index: Chunk index for ID generation.
            division: Division name if applicable.
            section: Section name if applicable.
            paragraphs: List of paragraph names in chunk.
            name: Semantic name for chunk ID.

        Returns:
            ChunkSpec with all fields populated.
        """
        # Generate a semantic name for the chunk ID if not provided
        if name is None and paragraphs:
            # Use first paragraph name
            name = paragraphs[0]

        return ChunkSpec(
            chunk_id=self.generate_chunk_id(artifact_id, kind, index, name),
            chunk_kind=kind,
            start_line=start_line,
            end_line=end_line,
            division=division,
            section=section,
            paragraphs=paragraphs or [],
            estimated_tokens=estimated_tokens,
        )

    def _get_current_division(
        self,
        boundary: SemanticBoundary,
        sorted_boundaries: list[SemanticBoundary],
    ) -> str | None:
        """Get the division containing a boundary.

        Args:
            boundary: The boundary to check.
            sorted_boundaries: All boundaries sorted by line.

        Returns:
            Division name or None.
        """
        if boundary.level == 0:
            # This is a division
            return boundary.name.replace(" DIVISION", "")

        # Find most recent division
        for b in reversed(sorted_boundaries):
            if b.line_number < boundary.line_number and b.level == 0:
                return b.name.replace(" DIVISION", "")

        return None

    def _get_paragraphs_in_range(
        self,
        structure: COBOLStructure,
        start_line: int,
        end_line: int,
    ) -> list[str]:
        """Get paragraph names within a line range.

        Args:
            structure: Parsed structure.
            start_line: Start of range (1-indexed).
            end_line: End of range (1-indexed).

        Returns:
            List of paragraph names.
        """
        paragraphs = []
        for name, boundary in structure.paragraphs.items():
            if start_line <= boundary.line_number <= end_line:
                paragraphs.append(name)
        return sorted(paragraphs, key=lambda n: structure.paragraphs[n].line_number)
