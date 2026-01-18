"""Line-based splitter for generic artifact types.

This module provides a simple line-count-based splitter that serves as:
1. The fallback splitter for unknown artifact types
2. A baseline implementation for artifacts without semantic structure

The LineBasedSplitter divides source content into chunks based on line count,
respecting the configured context budget (max_chunk_tokens) and providing
stable chunk boundaries for deterministic splitting.

Key Features:
- Deterministic: Same source + profile = same chunk boundaries
- Context-bounded: Chunks respect max_chunk_tokens from profile
- Stable boundaries: Chunk IDs are based on line numbers, not content
- Overlap support: Adjacent chunks can share lines for context continuity

Usage:
    >>> from war_rig.chunking import LineBasedSplitter, SplitterProfile
    >>>
    >>> splitter = LineBasedSplitter()
    >>> profile = SplitterProfile(max_chunk_tokens=3500, overlap_lines=10)
    >>> result = splitter.split(source_text, profile, "artifact_id")
"""

from war_rig.chunking.base import Splitter, SplitResult
from war_rig.chunking.models import ChunkKind, ChunkSpec, SplitterProfile


class LineBasedSplitter(Splitter):
    """Simple line-count-based splitter for generic artifacts.

    This splitter divides source content into chunks based on line count,
    without any semantic awareness. It serves as the fallback for artifact
    types that don't have a specialized splitter registered.

    The splitting algorithm:
    1. Calculate lines per chunk based on token budget and average tokens/line
    2. Divide source into chunks of approximately equal size
    3. Apply overlap between chunks for context continuity
    4. Generate stable chunk IDs based on artifact_id and line numbers

    Design Principles:
    - Deterministic: Same input always produces same output
    - Stable boundaries: Chunk boundaries based on line numbers only
    - Budget-aware: Respects max_chunk_tokens from profile
    - Simple: No semantic parsing, just line counting

    Attributes:
        CHARS_PER_TOKEN: Average characters per token for estimation.
        CHARS_PER_LINE: Average characters per line for estimation.

    Example:
        >>> splitter = LineBasedSplitter()
        >>> profile = SplitterProfile(
        ...     name="line_based",
        ...     prefer_semantic=False,
        ...     max_chunk_tokens=3500,
        ...     overlap_lines=10,
        ... )
        >>> result = splitter.split(source, profile, "my_file.txt")
        >>> print(f"Created {len(result.chunks)} chunks")
    """

    # Average characters per token for estimation
    # This is a rough heuristic; actual tokenization varies by model
    CHARS_PER_TOKEN: int = 4

    # Average characters per line for initial chunk size calculation
    # Can be overridden via profile.custom_config["chars_per_line"]
    DEFAULT_CHARS_PER_LINE: int = 40

    @classmethod
    def get_artifact_types(cls) -> list[str]:
        """Return the artifact types this splitter handles.

        The LineBasedSplitter is designed as a fallback and doesn't
        declare specific artifact types. It can handle any text-based
        artifact but is typically used when no specialized splitter
        is available.

        Returns:
            Empty list - this is the fallback splitter.
        """
        # Empty list indicates this is the fallback splitter
        # It doesn't claim any specific artifact types
        return []

    def split(
        self,
        source: str,
        profile: SplitterProfile,
        artifact_id: str,
    ) -> SplitResult:
        """Split source into chunks based on line count.

        The algorithm:
        1. Split source into lines
        2. Calculate target lines per chunk based on token budget
        3. Create chunks of approximately equal size
        4. Apply overlap from profile for context continuity

        Args:
            source: The source text to split.
            profile: Splitter configuration with token limits.
            artifact_id: Identifier for generating chunk IDs.

        Returns:
            SplitResult with chunk specifications.
        """
        lines = source.split("\n")
        total_lines = len(lines)
        warnings: list[str] = []

        # Handle empty or whitespace-only source
        # Note: "".split("\n") returns [''], so we check if source is empty
        if not source or not source.strip():
            return SplitResult(
                chunks=[],
                total_lines=0,
                total_estimated_tokens=0,
                semantic_boundaries_found=0,
                warnings=["Source is empty"],
            )

        # Calculate lines per chunk based on token budget
        chars_per_line = profile.custom_config.get(
            "chars_per_line", self.DEFAULT_CHARS_PER_LINE
        )
        tokens_per_line = max(1, chars_per_line // self.CHARS_PER_TOKEN)
        lines_per_chunk = max(1, profile.max_chunk_tokens // tokens_per_line)

        chunks: list[ChunkSpec] = []
        current_line = 1  # 1-indexed
        chunk_index = 0

        while current_line <= total_lines:
            # Calculate chunk boundaries
            start_line = current_line
            end_line = min(current_line + lines_per_chunk - 1, total_lines)

            # Include overlap from previous chunk (except for first chunk)
            if chunk_index > 0 and profile.overlap_lines > 0:
                overlap_start = max(1, start_line - profile.overlap_lines)
                start_line = overlap_start

            # Get chunk content and estimate tokens
            chunk_lines = lines[start_line - 1 : end_line]
            chunk_text = "\n".join(chunk_lines)
            estimated_tokens = self.estimate_tokens(chunk_text)

            # Create chunk specification
            chunk = ChunkSpec(
                chunk_id=self.generate_chunk_id(
                    artifact_id, ChunkKind.GENERIC, chunk_index
                ),
                chunk_kind=ChunkKind.GENERIC,
                start_line=start_line,
                end_line=end_line,
                estimated_tokens=estimated_tokens,
                metadata={
                    "splitter": "line_based",
                    "lines_per_chunk": lines_per_chunk,
                },
            )
            chunks.append(chunk)

            # Validate chunk
            validation_errors = self.validate_chunk(chunk, profile)
            warnings.extend(validation_errors)

            # Move to next chunk
            # Advance by lines_per_chunk from the original (non-overlapped) start
            if chunk_index == 0:
                current_line = end_line + 1
            else:
                # For subsequent chunks, advance from original position
                current_line = (chunk_index * lines_per_chunk) + lines_per_chunk + 1

            chunk_index += 1

            # Safety check to prevent infinite loops
            if chunk_index > total_lines:
                warnings.append(
                    f"Safety limit reached: created {chunk_index} chunks for {total_lines} lines"
                )
                break

        return SplitResult(
            chunks=chunks,
            total_lines=total_lines,
            total_estimated_tokens=self.estimate_tokens(source),
            semantic_boundaries_found=0,  # No semantic boundaries for line-based
            warnings=warnings,
        )

    def estimate_tokens(self, text: str) -> int:
        """Estimate token count for text.

        Uses a simple heuristic of ~4 characters per token.
        This is an approximation; actual tokenization varies by model.

        Args:
            text: Text to estimate tokens for.

        Returns:
            Estimated token count (minimum 1).
        """
        return max(1, len(text) // self.CHARS_PER_TOKEN)

    def detect_semantic_boundaries(
        self,
        source: str,
    ) -> list[tuple[int, str, ChunkKind]]:
        """Detect semantic boundaries in source code.

        The LineBasedSplitter has no semantic awareness, so this
        always returns an empty list.

        Args:
            source: The source code.

        Returns:
            Empty list (no semantic boundaries detected).
        """
        return []
