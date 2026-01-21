"""Data models for code chunking.

This module defines the data structures used throughout the chunking
pipeline, from initial splitting through merge.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum
from typing import Any


class ChunkContextType(str, Enum):
    """Type of structural context for a chunk.

    Indicates what COBOL structural boundary the chunk represents.
    """

    FULL_FILE = "full_file"
    """Entire file (no chunking needed)."""

    DIVISION = "division"
    """A complete COBOL division (e.g., PROCEDURE DIVISION)."""

    SECTION = "section"
    """A COBOL section within the PROCEDURE DIVISION."""

    PARAGRAPH = "paragraph"
    """One or more COBOL paragraphs."""

    BLOCK = "block"
    """Arbitrary line-based block (fallback when semantic boundaries unavailable)."""


@dataclass
class ChunkContext:
    """Contextual information about where a chunk fits in the overall file.

    This metadata helps the Scribe understand the chunk's relationship
    to the rest of the file and produce coherent documentation.

    Attributes:
        division: Which COBOL division this chunk is from (if applicable).
        section: Which section this chunk is from (if applicable).
        parent_paragraphs: Paragraphs that PERFORM into this chunk's paragraphs.
        child_paragraphs: Paragraphs that this chunk's paragraphs PERFORM.
        copybooks_in_scope: Copybooks relevant to this chunk.
        preceding_context: Brief summary of what comes before this chunk.
        following_context: Brief summary of what comes after this chunk.
    """

    division: str | None = None
    section: str | None = None
    parent_paragraphs: list[str] = field(default_factory=list)
    child_paragraphs: list[str] = field(default_factory=list)
    copybooks_in_scope: list[str] = field(default_factory=list)
    preceding_context: str = ""
    following_context: str = ""


@dataclass
class CodeChunk:
    """A semantically-bounded code chunk with metadata.

    Represents a portion of a source file that has been split at
    meaningful boundaries (divisions, sections, paragraphs) to fit
    within the LLM's context window.

    Attributes:
        chunk_id: Unique identifier for this chunk (e.g., "CBACT01C-chunk-1").
        content: The actual source code for this chunk.
        start_line: First line number in the original file (1-indexed).
        end_line: Last line number in the original file (1-indexed).
        context_type: What kind of structural boundary this represents.
        context: Detailed context about this chunk's place in the file.
        estimated_tokens: Estimated token count for this chunk's content.
        includes_header: Whether this chunk includes file header info.
        includes_data_division: Whether DATA DIVISION is included/summarized.

    Example:
        >>> chunk = CodeChunk(
        ...     chunk_id="CBACT01C-chunk-1",
        ...     content="       IDENTIFICATION DIVISION...",
        ...     start_line=1,
        ...     end_line=500,
        ...     context_type=ChunkContextType.DIVISION,
        ...     estimated_tokens=5000,
        ... )
    """

    chunk_id: str
    content: str
    start_line: int
    end_line: int
    context_type: ChunkContextType
    context: ChunkContext = field(default_factory=ChunkContext)
    estimated_tokens: int = 0
    includes_header: bool = False
    includes_data_division: bool = False

    @property
    def line_count(self) -> int:
        """Number of lines in this chunk."""
        return self.end_line - self.start_line + 1

    def to_dict(self) -> dict[str, Any]:
        """Convert to dictionary for serialization."""
        return {
            "chunk_id": self.chunk_id,
            "start_line": self.start_line,
            "end_line": self.end_line,
            "context_type": self.context_type.value,
            "estimated_tokens": self.estimated_tokens,
            "includes_header": self.includes_header,
            "includes_data_division": self.includes_data_division,
            "line_count": self.line_count,
        }


@dataclass
class ChunkingResult:
    """Result of chunking a source file.

    Contains the chunks and metadata about the chunking decision.

    Attributes:
        file_name: Name of the source file.
        chunks: List of code chunks.
        total_tokens_estimated: Total estimated tokens across all chunks.
        chunking_strategy: Description of how the file was chunked.
        needs_chunking: Whether the file required chunking.
        original_line_count: Total lines in original file.

    Example:
        >>> result = chunker.chunk(source_code, max_tokens=15000)
        >>> if result.needs_chunking:
        ...     for chunk in result.chunks:
        ...         process_chunk(chunk)
    """

    file_name: str
    chunks: list[CodeChunk]
    total_tokens_estimated: int
    chunking_strategy: str
    needs_chunking: bool
    original_line_count: int

    @property
    def chunk_count(self) -> int:
        """Number of chunks."""
        return len(self.chunks)

    def to_dict(self) -> dict[str, Any]:
        """Convert to dictionary for serialization."""
        return {
            "file_name": self.file_name,
            "chunk_count": self.chunk_count,
            "total_tokens_estimated": self.total_tokens_estimated,
            "chunking_strategy": self.chunking_strategy,
            "needs_chunking": self.needs_chunking,
            "original_line_count": self.original_line_count,
            "chunks": [c.to_dict() for c in self.chunks],
        }
