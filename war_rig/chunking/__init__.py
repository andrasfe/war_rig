"""Chunking module for splitting large source files.

This module provides a self-contained chunking framework for breaking source
artifacts into analyzable chunks that fit within LLM context budgets while
preserving semantic boundaries where possible.

Originally ported from the Atlas framework, this module is now embedded
directly in War Rig to eliminate external dependencies.

Components:
- Splitter: Abstract base class defining the splitter interface
- SplitResult: Result of a splitting operation
- SplitterRegistry: Registry for mapping artifact types to splitters
- COBOLSplitter: COBOL-aware splitter with semantic boundary detection
- LineBasedSplitter: Fallback splitter using simple line-count chunking

Design Principles:
- Deterministic chunking: Same source + profile = same chunks
- Plugin architecture: Register splitters by artifact type
- Fallback support: Unknown types use LineBasedSplitter
- Prefer semantic boundaries (divisions/sections/paragraphs)
- Ensure chunks fit within context budget
- Create chunk kinds to support targeted follow-ups

Usage:
    >>> from war_rig.chunking import get_splitter
    >>> splitter = get_splitter("cobol")
    >>> result = splitter.split(source, profile, "program.cbl")

Or via registry directly:
    >>> from war_rig.chunking import COBOLSplitter, SplitterProfile
    >>> splitter = COBOLSplitter()
    >>> profile = SplitterProfile(max_chunk_tokens=3500)
    >>> result = splitter.split(source, profile, "program.cbl")
"""

from war_rig.chunking.models import ChunkKind, ChunkSpec, SplitterProfile
from war_rig.chunking.base import Splitter, SplitResult
from war_rig.chunking.cobol import COBOLSplitter, COBOLStructure, SemanticBoundary
from war_rig.chunking.line_based import LineBasedSplitter
from war_rig.chunking.registry import (
    SplitterRegistry,
    SplitterNotFoundError,
    get_default_registry,
    reset_default_registry,
)


def get_splitter(artifact_type: str) -> Splitter:
    """Get a splitter for the given artifact type.

    Convenience function that wraps get_default_registry().get_splitter().

    Args:
        artifact_type: The artifact type (e.g., "cobol", "jcl").

    Returns:
        A Splitter instance for the artifact type.

    Raises:
        SplitterNotFoundError: If no splitter is registered for the type
            and no fallback is available.

    Example:
        >>> from war_rig.chunking import get_splitter
        >>> splitter = get_splitter("cobol")
        >>> result = splitter.split(source, profile, "program.cbl")
    """
    return get_default_registry().get_splitter(artifact_type)


__all__ = [
    # Models
    "ChunkKind",
    "ChunkSpec",
    "SplitterProfile",
    # Base classes
    "Splitter",
    "SplitResult",
    # Registry
    "SplitterRegistry",
    "SplitterNotFoundError",
    "get_default_registry",
    "get_splitter",
    "reset_default_registry",
    # Concrete implementations
    "COBOLSplitter",
    "COBOLStructure",
    "SemanticBoundary",
    "LineBasedSplitter",
]
