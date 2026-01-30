"""COBOL-aware code chunking.

This module provides semantic-aware splitting of COBOL source files
at meaningful boundaries (divisions, sections, paragraphs) to maintain
documentation quality when processing large files.

The chunker preserves structural integrity by:
1. Never splitting in the middle of a statement
2. Keeping header context (IDENTIFICATION DIVISION) with each chunk
3. Including DATA DIVISION summary in each chunk for context
4. Splitting PROCEDURE DIVISION at section/paragraph boundaries
"""

from __future__ import annotations

import logging
import re
from dataclasses import dataclass

from war_rig.chunking.estimator import TokenEstimator
from war_rig.chunking.models import (
    ChunkContext,
    ChunkContextType,
    ChunkingResult,
    CodeChunk,
)

logger = logging.getLogger(__name__)


@dataclass
class COBOLStructure:
    """Parsed structure of a COBOL program.

    Contains the locations of key structural elements for chunking decisions.

    Attributes:
        identification_division: (start_line, end_line) or None.
        environment_division: (start_line, end_line) or None.
        data_division: (start_line, end_line) or None.
        procedure_division: (start_line, end_line) or None.
        sections: List of (name, start_line, end_line) tuples.
        paragraphs: List of (name, start_line, end_line) tuples.
    """

    identification_division: tuple[int, int] | None = None
    environment_division: tuple[int, int] | None = None
    data_division: tuple[int, int] | None = None
    procedure_division: tuple[int, int] | None = None
    sections: list[tuple[str, int, int]] = None  # type: ignore
    paragraphs: list[tuple[str, int, int]] = None  # type: ignore

    def __post_init__(self):
        if self.sections is None:
            self.sections = []
        if self.paragraphs is None:
            self.paragraphs = []


class COBOLChunker:
    """Splits COBOL code at semantic boundaries.

    The chunker analyzes COBOL structure and creates chunks that:
    1. Fit within the token budget
    2. Preserve semantic boundaries (don't split mid-statement)
    3. Include necessary context for documentation

    Chunking Strategy (Priority Order):
    1. If entire file fits: No chunking (single chunk)
    2. If PROCEDURE DIVISION fits separately: Split at division boundaries
    3. If PROCEDURE DIVISION too large: Split at SECTION boundaries
    4. If sections too large: Split at paragraph boundaries
    5. Last resort: Split at statement boundaries

    Attributes:
        estimator: Token estimator for size calculations.
        min_chunk_tokens: Minimum tokens per chunk (avoid tiny chunks).

    Example:
        >>> chunker = COBOLChunker()
        >>> if chunker.needs_chunking(source_code, max_tokens=15000):
        ...     result = chunker.chunk(source_code, max_tokens=8000, file_name="PROG.cbl")
        ...     for chunk in result.chunks:
        ...         print(f"Chunk {chunk.chunk_id}: lines {chunk.start_line}-{chunk.end_line}")
    """

    # Regex patterns for COBOL structure detection
    DIVISION_PATTERN = re.compile(
        r"^\s*(\d{6})?\s*(IDENTIFICATION|ENVIRONMENT|DATA|PROCEDURE)\s+DIVISION",
        re.IGNORECASE | re.MULTILINE,
    )
    SECTION_PATTERN = re.compile(
        r"^\s*(\d{6})?\s*([A-Z0-9-]+)\s+SECTION\s*\.",
        re.IGNORECASE | re.MULTILINE,
    )
    PARAGRAPH_PATTERN = re.compile(
        r"^\s*(\d{6})?\s*([A-Z0-9-]+)\s*\.\s*$",
        re.IGNORECASE | re.MULTILINE,
    )

    # Minimum tokens per chunk to avoid excessive fragmentation
    MIN_CHUNK_TOKENS: int = 500

    def __init__(
        self,
        estimator: TokenEstimator | None = None,
        min_chunk_tokens: int | None = None,
    ):
        """Initialize the COBOL chunker.

        Args:
            estimator: Token estimator. Created if not provided.
            min_chunk_tokens: Minimum tokens per chunk.
        """
        self.estimator = estimator or TokenEstimator()
        self.min_chunk_tokens = min_chunk_tokens or self.MIN_CHUNK_TOKENS

    def needs_chunking(self, source_code: str, max_tokens: int) -> bool:
        """Check if a file exceeds the token budget.

        Args:
            source_code: The source code to check.
            max_tokens: Maximum tokens allowed for source code.

        Returns:
            True if the file needs to be chunked.
        """
        estimated = self.estimator.estimate_source_tokens(source_code)
        needs_it = estimated > max_tokens

        if needs_it:
            logger.info(
                f"File needs chunking: {estimated} estimated tokens > {max_tokens} max"
            )

        return needs_it

    def chunk(
        self,
        source_code: str,
        max_tokens: int,
        file_name: str = "unknown",
    ) -> ChunkingResult:
        """Split COBOL code into semantically-bounded chunks.

        Args:
            source_code: The source code to chunk.
            max_tokens: Maximum tokens per chunk (for source code portion).
            file_name: Name of the source file (for chunk IDs).

        Returns:
            ChunkingResult with list of CodeChunks.

        TODO: Implement the full chunking logic.
            1. Parse COBOL structure using _parse_structure()
            2. Decide chunking strategy based on file size and structure
            3. Create chunks at appropriate boundaries
            4. Include header context in each chunk
            5. Calculate estimated tokens for each chunk
        """
        lines = source_code.split("\n")
        total_tokens = self.estimator.estimate_source_tokens(source_code)
        base_name = file_name.replace(".cbl", "").replace(".cob", "").upper()

        # If file fits in budget, return single chunk
        if total_tokens <= max_tokens:
            chunk = CodeChunk(
                chunk_id=f"{base_name}-full",
                content=source_code,
                start_line=1,
                end_line=len(lines),
                context_type=ChunkContextType.FULL_FILE,
                estimated_tokens=total_tokens,
                includes_header=True,
                includes_data_division=True,
            )
            return ChunkingResult(
                file_name=file_name,
                chunks=[chunk],
                total_tokens_estimated=total_tokens,
                chunking_strategy="No chunking needed - file fits in budget",
                needs_chunking=False,
                original_line_count=len(lines),
            )

        # Parse structure for intelligent splitting
        structure = self._parse_structure(source_code)

        # Try division-level chunking first
        chunks = self._chunk_by_divisions(
            source_code, lines, structure, max_tokens, base_name
        )

        if chunks:
            return ChunkingResult(
                file_name=file_name,
                chunks=chunks,
                total_tokens_estimated=sum(c.estimated_tokens for c in chunks),
                chunking_strategy="Division-level chunking",
                needs_chunking=True,
                original_line_count=len(lines),
            )

        # Fall back to section-level chunking
        chunks = self._chunk_by_sections(
            source_code, lines, structure, max_tokens, base_name
        )

        if chunks:
            return ChunkingResult(
                file_name=file_name,
                chunks=chunks,
                total_tokens_estimated=sum(c.estimated_tokens for c in chunks),
                chunking_strategy="Section-level chunking",
                needs_chunking=True,
                original_line_count=len(lines),
            )

        # Fall back to paragraph-level chunking
        chunks = self._chunk_by_paragraphs(
            source_code, lines, structure, max_tokens, base_name
        )

        if chunks:
            return ChunkingResult(
                file_name=file_name,
                chunks=chunks,
                total_tokens_estimated=sum(c.estimated_tokens for c in chunks),
                chunking_strategy="Paragraph-level chunking",
                needs_chunking=True,
                original_line_count=len(lines),
            )

        # Last resort: line-based chunking
        chunks = self._chunk_by_lines(lines, max_tokens, base_name)

        return ChunkingResult(
            file_name=file_name,
            chunks=chunks,
            total_tokens_estimated=sum(c.estimated_tokens for c in chunks),
            chunking_strategy="Line-based chunking (fallback)",
            needs_chunking=True,
            original_line_count=len(lines),
        )

    def _parse_structure(self, source_code: str) -> COBOLStructure:
        """Parse COBOL structure to identify division/section/paragraph boundaries.

        Args:
            source_code: The source code to parse.

        Returns:
            COBOLStructure with boundary locations.

        TODO: Implement structure parsing.
            - Find IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE DIVISION boundaries
            - Find SECTION boundaries within PROCEDURE DIVISION
            - Find paragraph boundaries within sections
            - Handle edge cases (missing divisions, etc.)
        """
        structure = COBOLStructure()
        lines = source_code.split("\n")

        # Find division boundaries
        division_positions: dict[str, int] = {}
        for match in self.DIVISION_PATTERN.finditer(source_code):
            division_name = match.group(2).upper()
            # Calculate line number from character position
            line_num = source_code[:match.start()].count("\n") + 1
            division_positions[division_name] = line_num

        # Set division ranges (start to next division or end)
        division_order = ["IDENTIFICATION", "ENVIRONMENT", "DATA", "PROCEDURE"]
        for i, div in enumerate(division_order):
            if div in division_positions:
                start = division_positions[div]
                # End is either next division or end of file
                end = len(lines)
                for next_div in division_order[i + 1:]:
                    if next_div in division_positions:
                        end = division_positions[next_div] - 1
                        break

                if div == "IDENTIFICATION":
                    structure.identification_division = (start, end)
                elif div == "ENVIRONMENT":
                    structure.environment_division = (start, end)
                elif div == "DATA":
                    structure.data_division = (start, end)
                elif div == "PROCEDURE":
                    structure.procedure_division = (start, end)

        # Find sections within PROCEDURE DIVISION
        if structure.procedure_division:
            proc_start, proc_end = structure.procedure_division
            proc_text = "\n".join(lines[proc_start - 1:proc_end])

            section_positions: list[tuple[str, int]] = []
            for match in self.SECTION_PATTERN.finditer(proc_text):
                section_name = match.group(2).upper()
                line_in_proc = proc_text[:match.start()].count("\n")
                absolute_line = proc_start + line_in_proc
                section_positions.append((section_name, absolute_line))

            # Calculate section ranges
            for i, (name, start) in enumerate(section_positions):
                if i + 1 < len(section_positions):
                    end = section_positions[i + 1][1] - 1
                else:
                    end = proc_end
                structure.sections.append((name, start, end))

        # Find paragraphs (simplified - within PROCEDURE DIVISION)
        if structure.procedure_division:
            proc_start, proc_end = structure.procedure_division
            for line_num in range(proc_start, proc_end + 1):
                if line_num - 1 < len(lines):
                    line = lines[line_num - 1]
                    # Simple paragraph detection: line starts with label, ends with period
                    match = self.PARAGRAPH_PATTERN.match(line)
                    if match:
                        para_name = match.group(2).upper()
                        # Don't count SECTION declarations as paragraphs
                        if "SECTION" not in line.upper():
                            # Paragraph ends at next paragraph or section
                            structure.paragraphs.append((para_name, line_num, line_num))

            # Fix paragraph end lines
            for i in range(len(structure.paragraphs)):
                name, start, _ = structure.paragraphs[i]
                if i + 1 < len(structure.paragraphs):
                    end = structure.paragraphs[i + 1][1] - 1
                else:
                    end = proc_end
                structure.paragraphs[i] = (name, start, end)

        return structure

    def _chunk_by_divisions(
        self,
        source_code: str,
        lines: list[str],
        structure: COBOLStructure,
        max_tokens: int,
        base_name: str,
    ) -> list[CodeChunk] | None:
        """Try to chunk by COBOL divisions.

        Creates chunks for: (header + DATA) and (header + PROCEDURE).

        Returns None if divisions are too large to chunk this way.

        TODO: Implement division-level chunking.
        """
        if not structure.procedure_division:
            return None

        # Calculate header (ID + ENV + DATA summaries)
        header_end = 0
        if structure.data_division:
            header_end = structure.data_division[1]
        elif structure.environment_division:
            header_end = structure.environment_division[1]
        elif structure.identification_division:
            header_end = structure.identification_division[1]

        if header_end == 0:
            return None

        header_content = "\n".join(lines[:header_end])
        header_tokens = self.estimator.estimate_source_tokens(header_content)

        proc_start, proc_end = structure.procedure_division
        proc_content = "\n".join(lines[proc_start - 1:proc_end])
        proc_tokens = self.estimator.estimate_source_tokens(proc_content)

        # Check if PROCEDURE DIVISION fits with header context
        if header_tokens + proc_tokens <= max_tokens:
            # Can fit in one chunk with header
            chunk = CodeChunk(
                chunk_id=f"{base_name}-chunk-1",
                content=source_code,
                start_line=1,
                end_line=len(lines),
                context_type=ChunkContextType.FULL_FILE,
                estimated_tokens=header_tokens + proc_tokens,
                includes_header=True,
                includes_data_division=True,
            )
            return [chunk]

        # PROCEDURE DIVISION alone is too big - need finer chunking
        if proc_tokens > max_tokens:
            return None  # Fall through to section-level

        # Create two chunks: Header+Data and Header-summary+Procedure
        chunks = []

        # Chunk 1: Full header with DATA DIVISION
        chunks.append(CodeChunk(
            chunk_id=f"{base_name}-chunk-1-header",
            content=header_content,
            start_line=1,
            end_line=header_end,
            context_type=ChunkContextType.DIVISION,
            context=ChunkContext(division="HEADER+DATA"),
            estimated_tokens=header_tokens,
            includes_header=True,
            includes_data_division=True,
        ))

        # Chunk 2: Summarized header + PROCEDURE DIVISION
        # Include a brief header summary for context
        header_summary = self._create_header_summary(lines, structure)
        proc_with_context = header_summary + "\n" + proc_content
        proc_context_tokens = self.estimator.estimate_source_tokens(proc_with_context)

        chunks.append(CodeChunk(
            chunk_id=f"{base_name}-chunk-2-procedure",
            content=proc_with_context,
            start_line=proc_start,
            end_line=proc_end,
            context_type=ChunkContextType.DIVISION,
            context=ChunkContext(division="PROCEDURE"),
            estimated_tokens=proc_context_tokens,
            includes_header=False,  # Only summary
            includes_data_division=False,  # Only summary
        ))

        return chunks

    def _chunk_by_sections(
        self,
        source_code: str,
        lines: list[str],
        structure: COBOLStructure,
        max_tokens: int,
        base_name: str,
    ) -> list[CodeChunk] | None:
        """Try to chunk by COBOL sections within PROCEDURE DIVISION.

        Returns None if sections are too large.

        TODO: Implement section-level chunking.
        """
        if not structure.sections:
            return None

        header_summary = self._create_header_summary(lines, structure)
        header_tokens = self.estimator.estimate_source_tokens(header_summary)

        # Check if any section is too large
        for name, start, end in structure.sections:
            section_content = "\n".join(lines[start - 1:end])
            section_tokens = self.estimator.estimate_source_tokens(section_content)
            if header_tokens + section_tokens > max_tokens:
                return None  # Section too large, need paragraph-level

        chunks = []
        chunk_num = 1

        # Create header chunk if substantial
        if structure.data_division:
            data_start, data_end = structure.data_division
            data_content = "\n".join(lines[:data_end])
            data_tokens = self.estimator.estimate_source_tokens(data_content)

            if data_tokens > self.min_chunk_tokens:
                chunks.append(CodeChunk(
                    chunk_id=f"{base_name}-chunk-{chunk_num}-header",
                    content=data_content,
                    start_line=1,
                    end_line=data_end,
                    context_type=ChunkContextType.DIVISION,
                    context=ChunkContext(division="HEADER+DATA"),
                    estimated_tokens=data_tokens,
                    includes_header=True,
                    includes_data_division=True,
                ))
                chunk_num += 1

        # Create chunks for each section
        for name, start, end in structure.sections:
            section_content = "\n".join(lines[start - 1:end])
            full_content = header_summary + "\n" + section_content
            tokens = self.estimator.estimate_source_tokens(full_content)

            chunks.append(CodeChunk(
                chunk_id=f"{base_name}-chunk-{chunk_num}-{name}",
                content=full_content,
                start_line=start,
                end_line=end,
                context_type=ChunkContextType.SECTION,
                context=ChunkContext(division="PROCEDURE", section=name),
                estimated_tokens=tokens,
                includes_header=False,
                includes_data_division=False,
            ))
            chunk_num += 1

        return chunks if chunks else None

    def _chunk_by_paragraphs(
        self,
        source_code: str,
        lines: list[str],
        structure: COBOLStructure,
        max_tokens: int,
        base_name: str,
    ) -> list[CodeChunk] | None:
        """Try to chunk by COBOL paragraphs.

        Groups paragraphs together until they approach max_tokens.

        TODO: Implement paragraph-level chunking.
        """
        if not structure.paragraphs:
            return None

        header_summary = self._create_header_summary(lines, structure)
        header_tokens = self.estimator.estimate_source_tokens(header_summary)

        chunks = []
        chunk_num = 1

        # Create header chunk
        if structure.data_division:
            data_start, data_end = structure.data_division
            data_content = "\n".join(lines[:data_end])
            data_tokens = self.estimator.estimate_source_tokens(data_content)

            if data_tokens > self.min_chunk_tokens:
                chunks.append(CodeChunk(
                    chunk_id=f"{base_name}-chunk-{chunk_num}-header",
                    content=data_content,
                    start_line=1,
                    end_line=data_end,
                    context_type=ChunkContextType.DIVISION,
                    estimated_tokens=data_tokens,
                    includes_header=True,
                    includes_data_division=True,
                ))
                chunk_num += 1

        # Group paragraphs into chunks
        current_paras: list[tuple[str, int, int]] = []
        current_tokens = header_tokens

        for para_name, para_start, para_end in structure.paragraphs:
            para_content = "\n".join(lines[para_start - 1:para_end])
            para_tokens = self.estimator.estimate_source_tokens(para_content)

            if current_tokens + para_tokens > max_tokens and current_paras:
                # Create chunk from accumulated paragraphs
                chunk_start = current_paras[0][1]
                chunk_end = current_paras[-1][2]
                chunk_content = header_summary + "\n" + "\n".join(
                    lines[chunk_start - 1:chunk_end]
                )
                para_names = [p[0] for p in current_paras]

                chunks.append(CodeChunk(
                    chunk_id=f"{base_name}-chunk-{chunk_num}",
                    content=chunk_content,
                    start_line=chunk_start,
                    end_line=chunk_end,
                    context_type=ChunkContextType.PARAGRAPH,
                    context=ChunkContext(
                        division="PROCEDURE",
                        parent_paragraphs=para_names,
                    ),
                    estimated_tokens=current_tokens,
                    includes_header=False,
                    includes_data_division=False,
                ))
                chunk_num += 1

                # Reset for next chunk
                current_paras = []
                current_tokens = header_tokens

            current_paras.append((para_name, para_start, para_end))
            current_tokens += para_tokens

        # Don't forget the last chunk
        if current_paras:
            chunk_start = current_paras[0][1]
            chunk_end = current_paras[-1][2]
            chunk_content = header_summary + "\n" + "\n".join(
                lines[chunk_start - 1:chunk_end]
            )
            para_names = [p[0] for p in current_paras]

            chunks.append(CodeChunk(
                chunk_id=f"{base_name}-chunk-{chunk_num}",
                content=chunk_content,
                start_line=chunk_start,
                end_line=chunk_end,
                context_type=ChunkContextType.PARAGRAPH,
                context=ChunkContext(
                    division="PROCEDURE",
                    parent_paragraphs=para_names,
                ),
                estimated_tokens=current_tokens,
                includes_header=False,
                includes_data_division=False,
            ))

        return chunks if chunks else None

    def _chunk_by_lines(
        self,
        lines: list[str],
        max_tokens: int,
        base_name: str,
    ) -> list[CodeChunk]:
        """Fallback: chunk by line count.

        Used when semantic boundaries are insufficient.

        TODO: Implement line-based chunking as fallback.
        """
        chunks = []
        chunk_num = 1

        # Estimate lines per chunk
        avg_chars_per_line = sum(len(line) for line in lines) / max(len(lines), 1)
        tokens_per_line = avg_chars_per_line / self.estimator.cobol_chars_per_token
        lines_per_chunk = int(max_tokens / max(tokens_per_line, 1))
        lines_per_chunk = max(lines_per_chunk, 50)  # Minimum 50 lines

        for i in range(0, len(lines), lines_per_chunk):
            chunk_lines = lines[i:i + lines_per_chunk]
            content = "\n".join(chunk_lines)
            tokens = self.estimator.estimate_source_tokens(content)

            chunks.append(CodeChunk(
                chunk_id=f"{base_name}-chunk-{chunk_num}",
                content=content,
                start_line=i + 1,
                end_line=min(i + lines_per_chunk, len(lines)),
                context_type=ChunkContextType.BLOCK,
                estimated_tokens=tokens,
                includes_header=(i == 0),
                includes_data_division=(i == 0),
            ))
            chunk_num += 1

        return chunks

    def _create_header_summary(
        self,
        lines: list[str],
        structure: COBOLStructure,
    ) -> str:
        """Create a brief header summary for inclusion in procedure chunks.

        Includes program ID and key data structure names.

        TODO: Implement header summary generation.
        """
        summary_parts = ["*" * 60]
        summary_parts.append("* CONTEXT: Header summary for chunked processing")
        summary_parts.append("*" * 60)

        # Extract PROGRAM-ID
        if structure.identification_division:
            id_start, id_end = structure.identification_division
            for line in lines[id_start - 1:id_end]:
                if "PROGRAM-ID" in line.upper():
                    summary_parts.append(f"* {line.strip()}")
                    break

        # Note that DATA DIVISION exists but is in another chunk
        if structure.data_division:
            summary_parts.append("* DATA DIVISION: See header chunk for full definitions")

        summary_parts.append("*" * 60)

        return "\n".join(summary_parts)
