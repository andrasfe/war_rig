"""Data models for the chunking module.

This module defines the core data structures used by splitters:
- ChunkKind: Classification of chunk types
- ChunkSpec: Specification for a single chunk
- SplitterProfile: Configuration for splitting behavior
"""

from dataclasses import dataclass, field
from enum import Enum
from typing import Any


class ChunkKind(str, Enum):
    """Classification of chunk content types.

    Used to categorize chunks for targeted processing and merging.
    """

    # COBOL-specific kinds
    IDENTIFICATION_DIVISION = "identification_division"
    ENVIRONMENT_DIVISION = "environment_division"
    DATA_DIVISION = "data_division"
    PROCEDURE_DIVISION = "procedure_division"
    FILE_SECTION = "file_section"
    WORKING_STORAGE = "working_storage"
    LINKAGE_SECTION = "linkage_section"
    PROCEDURE_PART = "procedure_part"

    # Generic kind for non-semantic chunks
    GENERIC = "generic"

    # Merged/aggregated chunks
    MERGED = "merged"


@dataclass
class ChunkSpec:
    """Specification for a single chunk of source code.

    Attributes:
        chunk_id: Unique identifier for this chunk.
        chunk_kind: Classification of chunk content.
        start_line: Starting line number (1-indexed).
        end_line: Ending line number (1-indexed, inclusive).
        estimated_tokens: Estimated token count for this chunk.
        division: COBOL division name if applicable.
        section: COBOL section name if applicable.
        paragraphs: List of paragraph names in this chunk.
        metadata: Additional metadata for this chunk.
    """

    chunk_id: str
    chunk_kind: ChunkKind
    start_line: int
    end_line: int
    estimated_tokens: int = 0
    division: str | None = None
    section: str | None = None
    paragraphs: list[str] = field(default_factory=list)
    metadata: dict[str, Any] = field(default_factory=dict)


@dataclass
class SplitterProfile:
    """Configuration for splitter behavior.

    Attributes:
        name: Name of this profile for identification.
        max_chunk_tokens: Maximum tokens per chunk.
        overlap_lines: Number of lines to overlap between chunks.
        prefer_semantic: If True, prefer semantic boundaries over line-based.
        custom_config: Additional configuration options.
    """

    name: str = "default"
    max_chunk_tokens: int = 3500
    overlap_lines: int = 10
    prefer_semantic: bool = True
    custom_config: dict[str, Any] = field(default_factory=dict)
