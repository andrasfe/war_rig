"""Chunking module for handling large source files.

This module provides semantic-aware code splitting for files that exceed
the LLM's context window. It preserves COBOL structure boundaries to
maintain documentation quality.

Key Components:
- TokenEstimator: Estimates token counts for prompt construction
- COBOLChunker: Splits COBOL code at semantic boundaries
- ChunkMerger: Combines partial documentation into unified output
- CodeChunk: Data model for a code chunk with metadata

Usage:
    from war_rig.chunking import COBOLChunker, ChunkMerger, TokenEstimator

    estimator = TokenEstimator()
    chunker = COBOLChunker(estimator)

    if chunker.needs_chunking(source_code, max_tokens=15000):
        chunks = chunker.chunk(source_code, max_tokens=15000)
        # Process each chunk through ScribeAgent
        # Then merge results
        merged = ChunkMerger().merge(chunks, chunk_outputs, file_name)

See Also:
    - docs/architecture/CHUNKING_DESIGN.md: Full design documentation
    - war_rig.workers.scribe_pool: Integration point
"""

from war_rig.chunking.cobol_chunker import COBOLChunker
from war_rig.chunking.estimator import TokenEstimator
from war_rig.chunking.generic_chunker import GenericChunker
from war_rig.chunking.merger import ChunkMerger
from war_rig.chunking.models import ChunkContext, ChunkingResult, CodeChunk

__all__ = [
    "CodeChunk",
    "ChunkContext",
    "ChunkingResult",
    "TokenEstimator",
    "COBOLChunker",
    "GenericChunker",
    "ChunkMerger",
]
