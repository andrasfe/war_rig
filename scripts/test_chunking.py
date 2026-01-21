#!/usr/bin/env python3
"""Test script for chunking integration.

Tests that large files are properly chunked and processed.
"""

import asyncio
import logging
import sys
from pathlib import Path

# Add project root to path
sys.path.insert(0, str(Path(__file__).parent.parent))

from war_rig.chunking import COBOLChunker, GenericChunker, TokenEstimator, ChunkMerger
from war_rig.models.templates import FileType

logging.basicConfig(level=logging.INFO, format='%(levelname)s: %(message)s')
logger = logging.getLogger(__name__)


def test_chunking(file_path: str, max_prompt_tokens: int = 21000):
    """Test chunking a file without actually calling the LLM.

    Args:
        file_path: Path to the source file.
        max_prompt_tokens: Simulated max prompt tokens setting.
    """
    # Read source file
    with open(file_path) as f:
        source_code = f.read()

    file_name = Path(file_path).name

    # Determine file type
    ext = Path(file_path).suffix.lower()
    if ext in ('.cbl', '.cob', '.cobol'):
        file_type = FileType.COBOL
    else:
        file_type = FileType.OTHER

    # Token estimation
    estimator = TokenEstimator()
    source_tokens = estimator.estimate_source_tokens(source_code)

    # Reserve 4000 tokens for overhead (system prompt, schema, etc.)
    max_source_tokens = max_prompt_tokens - 4000

    logger.info(f"File: {file_name}")
    logger.info(f"File type: {file_type}")
    logger.info(f"Source size: {len(source_code):,} chars, ~{source_tokens:,} tokens")
    logger.info(f"Max prompt tokens: {max_prompt_tokens:,}")
    logger.info(f"Max source tokens: {max_source_tokens:,}")
    logger.info("")

    # Check if chunking is needed
    if source_tokens <= max_source_tokens:
        logger.info("File fits within token limit - no chunking needed")
        return

    logger.info(f"File exceeds limit ({source_tokens:,} > {max_source_tokens:,}) - chunking required")
    logger.info("")

    # Select appropriate chunker
    if file_type == FileType.COBOL:
        chunker = COBOLChunker(estimator)
        logger.info("Using COBOLChunker (semantic boundaries)")
    else:
        chunker = GenericChunker(estimator)
        logger.info("Using GenericChunker (line boundaries)")

    # Perform chunking
    result = chunker.chunk(source_code, max_source_tokens, file_name)

    logger.info("")
    logger.info(f"Chunking result:")
    logger.info(f"  Strategy: {result.chunking_strategy}")
    logger.info(f"  Total chunks: {result.chunk_count}")
    logger.info(f"  Original lines: {result.original_line_count:,}")
    logger.info(f"  Total tokens (all chunks): {result.total_tokens_estimated:,}")
    logger.info("")

    for i, chunk in enumerate(result.chunks):
        logger.info(f"  Chunk {i+1}: {chunk.chunk_id}")
        logger.info(f"    Lines: {chunk.start_line}-{chunk.end_line} ({chunk.end_line - chunk.start_line + 1} lines)")
        logger.info(f"    Tokens: ~{chunk.estimated_tokens:,}")
        logger.info(f"    Type: {chunk.context_type}")
        logger.info(f"    Has header: {chunk.includes_header}")
        # Show first few characters of content
        preview = chunk.content[:100].replace('\n', ' ')
        logger.info(f"    Preview: {preview}...")
        logger.info("")

    return result


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="Test file chunking")
    parser.add_argument("file", nargs="?",
                       default="/home/andras/aws-mainframe-modernization-carddemo/app/cbl/COACTUPC.cbl",
                       help="Path to source file")
    parser.add_argument("--max-tokens", type=int, default=21000,
                       help="Max prompt tokens (default: 21000 for ~3 chunks)")

    args = parser.parse_args()

    test_chunking(args.file, args.max_tokens)
