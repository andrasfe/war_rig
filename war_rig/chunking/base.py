"""Abstract base class for source code splitters.

Splitters break source artifacts into chunks for analysis.
Different implementations handle different source types
(COBOL, JCL, copybooks, etc.).

Key Requirements:
- Deterministic: Same source + profile = same chunk boundaries
- Semantic awareness: Prefer division/section/paragraph boundaries
- Context-bounded: Chunks must fit within token budget

The splitter system supports a plugin architecture:
- Each splitter declares which artifact types it handles via get_artifact_types()
- The SplitterRegistry maps artifact types to appropriate splitters
- Unknown types fall back to LineBasedSplitter for simple line-count chunking
"""

from abc import ABC, abstractmethod
from dataclasses import dataclass, field

from war_rig.chunking.models import ChunkKind, ChunkSpec, SplitterProfile


@dataclass
class SplitResult:
    """Result of splitting a source artifact.

    Attributes:
        chunks: List of chunk specifications.
        total_lines: Total lines in the source.
        total_estimated_tokens: Total estimated tokens.
        semantic_boundaries_found: Number of semantic boundaries detected.
        warnings: Any warnings during splitting.
    """

    chunks: list[ChunkSpec]
    total_lines: int = 0
    total_estimated_tokens: int = 0
    semantic_boundaries_found: int = 0
    warnings: list[str] = field(default_factory=list)


class Splitter(ABC):
    """Abstract interface for source code splitting.

    Implementations should:
    - Produce deterministic chunk boundaries for the same input
    - Respect semantic boundaries where possible
    - Ensure chunks fit within the configured context budget
    - Generate stable chunk IDs
    - Declare which artifact types they handle via get_artifact_types()

    Design Principle:
        For the same source snapshot + splitter profile, chunk boundaries
        and chunk_ids MUST be stable (deterministic chunking).

    Plugin Architecture:
        Splitters register themselves with the SplitterRegistry by declaring
        which artifact types they handle. The registry uses this to route
        splitting requests to the appropriate implementation.

    Example Implementation:
        >>> class COBOLSplitter(Splitter):
        ...     @classmethod
        ...     def get_artifact_types(cls) -> list[str]:
        ...         return ["cobol", "copybook"]
        ...
        ...     def split(self, source: str, profile: SplitterProfile) -> SplitResult:
        ...         # Parse COBOL structure
        ...         divisions = self._parse_divisions(source)
        ...         # Split at semantic boundaries
        ...         chunks = self._create_chunks(divisions, profile)
        ...         return SplitResult(chunks=chunks)
    """

    @classmethod
    @abstractmethod
    def get_artifact_types(cls) -> list[str]:
        """Return the artifact types this splitter handles.

        Each splitter declares which artifact types it can process.
        The SplitterRegistry uses this to route splitting requests.

        Returns:
            List of artifact type strings (e.g., ["cobol", "copybook"]).

        Example:
            >>> COBOLSplitter.get_artifact_types()
            ['cobol', 'copybook']
        """
        pass

    @abstractmethod
    def split(
        self,
        source: str,
        profile: SplitterProfile,
        artifact_id: str,
    ) -> SplitResult:
        """Split source code into chunks.

        Args:
            source: The source code to split.
            profile: Splitter configuration.
            artifact_id: Identifier for generating chunk IDs.

        Returns:
            SplitResult with chunk specifications.
        """
        pass

    @abstractmethod
    def estimate_tokens(self, text: str) -> int:
        """Estimate token count for text.

        Used to ensure chunks fit within context budget.

        Args:
            text: Text to estimate tokens for.

        Returns:
            Estimated token count.

        Note:
            This is an estimate. Actual token count depends on
            the specific LLM tokenizer. A common heuristic is
            approximately 4 characters per token.
        """
        pass

    @abstractmethod
    def detect_semantic_boundaries(
        self,
        source: str,
    ) -> list[tuple[int, str, ChunkKind]]:
        """Detect semantic boundaries in source code.

        For COBOL, this finds divisions, sections, and paragraphs.

        Args:
            source: The source code.

        Returns:
            List of (line_number, name, chunk_kind) tuples.
            For splitters without semantic awareness, return an empty list.
        """
        pass

    def generate_chunk_id(
        self,
        artifact_id: str,
        chunk_kind: ChunkKind,
        index: int,
        name: str | None = None,
    ) -> str:
        """Generate a stable chunk ID.

        IDs should be deterministic based on inputs.

        Args:
            artifact_id: Source artifact identifier.
            chunk_kind: Classification of chunk content.
            index: Chunk index within its kind.
            name: Optional semantic name (paragraph, section).

        Returns:
            Stable chunk identifier.
        """
        base = artifact_id.replace(".", "_").lower()
        kind = chunk_kind.value

        if name:
            # Sanitize name for ID use
            safe_name = name.replace("-", "_").replace(" ", "_").lower()
            return f"{base}_{kind}_{safe_name}"
        else:
            return f"{base}_{kind}_{index:03d}"

    def validate_chunk(
        self,
        chunk: ChunkSpec,
        profile: SplitterProfile,
    ) -> list[str]:
        """Validate a chunk against profile constraints.

        Args:
            chunk: Chunk specification to validate.
            profile: Profile with constraints.

        Returns:
            List of validation error messages (empty if valid).
        """
        errors: list[str] = []

        if chunk.estimated_tokens > profile.max_chunk_tokens:
            errors.append(
                f"Chunk {chunk.chunk_id} exceeds token limit: "
                f"{chunk.estimated_tokens} > {profile.max_chunk_tokens}"
            )

        if chunk.start_line > chunk.end_line:
            errors.append(
                f"Chunk {chunk.chunk_id} has invalid line range: "
                f"{chunk.start_line} > {chunk.end_line}"
            )

        return errors

    def create_overlap(
        self,
        source_lines: list[str],
        chunk_end: int,
        overlap_lines: int,
    ) -> tuple[int, int]:
        """Calculate overlap range for the next chunk.

        Overlap provides context continuity between chunks.

        Args:
            source_lines: All source lines.
            chunk_end: End line of current chunk.
            overlap_lines: Number of lines to overlap.

        Returns:
            Tuple of (overlap_start, overlap_end) line numbers.
        """
        overlap_start = max(0, chunk_end - overlap_lines + 1)
        overlap_end = chunk_end
        return overlap_start, overlap_end
