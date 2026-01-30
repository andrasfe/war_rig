"""Generic line-based code chunking.

This module provides simple line-based splitting for non-COBOL source files.
It splits at line boundaries while trying to preserve logical groupings
(e.g., avoid splitting in the middle of a function if possible).
"""

from __future__ import annotations

import logging
import re

from war_rig.chunking.estimator import TokenEstimator
from war_rig.chunking.models import (
    ChunkContextType,
    ChunkingResult,
    CodeChunk,
)

logger = logging.getLogger(__name__)


class GenericChunker:
    """Splits source code at line boundaries.

    A simple chunker that works for any file type by splitting at line
    boundaries. It attempts to split at blank lines or function/class
    boundaries when possible.

    Attributes:
        estimator: Token estimator for size calculations.
        min_chunk_tokens: Minimum tokens per chunk (avoid tiny chunks).

    Example:
        >>> chunker = GenericChunker()
        >>> if chunker.needs_chunking(source_code, max_tokens=15000):
        ...     result = chunker.chunk(source_code, max_tokens=8000, file_name="app.py")
        ...     for chunk in result.chunks:
        ...         print(f"Chunk {chunk.chunk_id}: lines {chunk.start_line}-{chunk.end_line}")
    """

    # Patterns for detecting logical boundaries (function/class definitions)
    BOUNDARY_PATTERNS = [
        re.compile(r"^\s*(def|class|function|func)\s+", re.IGNORECASE),  # Python, JS, Go
        re.compile(r"^\s*(public|private|protected)?\s*(static)?\s*(void|int|string)", re.IGNORECASE),  # Java/C#
        re.compile(r"^\s*---", re.IGNORECASE),  # SQL batch separator
    ]

    def __init__(
        self,
        estimator: TokenEstimator | None = None,
        min_chunk_tokens: int = 500,
    ):
        """Initialize the generic chunker.

        Args:
            estimator: Token estimator. Creates default if not provided.
            min_chunk_tokens: Minimum tokens per chunk to avoid tiny chunks.
        """
        self.estimator = estimator or TokenEstimator()
        self.min_chunk_tokens = min_chunk_tokens

    def needs_chunking(self, source_code: str, max_tokens: int) -> bool:
        """Check if source code needs to be chunked.

        Args:
            source_code: The source code to check.
            max_tokens: Maximum tokens allowed per chunk.

        Returns:
            True if the code exceeds max_tokens and needs chunking.
        """
        estimated_tokens = self.estimator.estimate_tokens(source_code)
        return estimated_tokens > max_tokens

    def chunk(
        self,
        source_code: str,
        max_tokens: int,
        file_name: str,
    ) -> ChunkingResult:
        """Split source code into chunks at line boundaries.

        Args:
            source_code: The source code to chunk.
            max_tokens: Maximum tokens per chunk.
            file_name: Name of the source file (for chunk IDs).

        Returns:
            ChunkingResult containing all chunks and metadata.
        """
        lines = source_code.split("\n")
        total_lines = len(lines)

        # If it fits in one chunk, return as-is
        if not self.needs_chunking(source_code, max_tokens):
            chunk = CodeChunk(
                chunk_id=f"{file_name}:1-{total_lines}",
                content=source_code,
                start_line=1,
                end_line=total_lines,
                context_type=ChunkContextType.COMPLETE,
                estimated_tokens=self.estimator.estimate_tokens(source_code),
                includes_header=True,
                includes_data_division=True,
            )
            return ChunkingResult(
                file_name=file_name,
                chunks=[chunk],
                total_tokens_estimated=chunk.estimated_tokens,
                chunking_strategy="none",
                needs_chunking=False,
                original_line_count=total_lines,
            )

        # Need to split - find good boundary points
        chunks: list[CodeChunk] = []
        current_start = 0
        chunk_num = 1

        while current_start < total_lines:
            # Find how many lines fit in max_tokens
            chunk_end = self._find_chunk_end(lines, current_start, max_tokens)

            # Extract chunk content
            chunk_lines = lines[current_start:chunk_end]
            chunk_content = "\n".join(chunk_lines)

            chunk = CodeChunk(
                chunk_id=f"{file_name}:chunk-{chunk_num}",
                content=chunk_content,
                start_line=current_start + 1,  # 1-indexed
                end_line=chunk_end,
                context_type=ChunkContextType.SECTION,
                estimated_tokens=self.estimator.estimate_tokens(chunk_content),
                includes_header=(chunk_num == 1),
                includes_data_division=False,
            )
            chunks.append(chunk)

            current_start = chunk_end
            chunk_num += 1

        total_tokens = sum(c.estimated_tokens for c in chunks)

        logger.info(
            f"Split {file_name} into {len(chunks)} chunks "
            f"(~{total_tokens} total tokens)"
        )

        return ChunkingResult(
            file_name=file_name,
            chunks=chunks,
            total_tokens_estimated=total_tokens,
            chunking_strategy="line_boundary",
            needs_chunking=True,
            original_line_count=total_lines,
        )

    def _find_chunk_end(
        self,
        lines: list[str],
        start: int,
        max_tokens: int,
    ) -> int:
        """Find the best end point for a chunk starting at 'start'.

        Tries to end at a logical boundary (blank line, function definition)
        while staying within the token budget.

        Args:
            lines: All lines of the source code.
            start: Starting line index (0-based).
            max_tokens: Maximum tokens for this chunk.

        Returns:
            End line index (exclusive, 0-based).
        """
        total_lines = len(lines)

        # Binary search for approximate end point
        low, high = start + 1, total_lines
        last_valid = start + 1

        while low <= high:
            mid = (low + high) // 2
            chunk_content = "\n".join(lines[start:mid])
            tokens = self.estimator.estimate_tokens(chunk_content)

            if tokens <= max_tokens:
                last_valid = mid
                low = mid + 1
            else:
                high = mid - 1

        # Now last_valid is the furthest we can go within budget
        # Try to find a better boundary near last_valid
        best_boundary = last_valid

        # Look back up to 50 lines for a good boundary
        search_start = max(start + 1, last_valid - 50)
        for i in range(last_valid - 1, search_start - 1, -1):
            line = lines[i] if i < total_lines else ""

            # Prefer blank lines
            if not line.strip():
                best_boundary = i + 1
                break

            # Or function/class definitions
            for pattern in self.BOUNDARY_PATTERNS:
                if pattern.match(line):
                    best_boundary = i  # Split before the definition
                    break

        # Ensure we make progress
        if best_boundary <= start:
            best_boundary = last_valid

        return best_boundary
