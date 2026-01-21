"""Source code preparation for Scribe token limit handling.

This module provides a centralized abstraction for preparing source code
to fit within Scribe's token limits. It decides between strategies:

- **Passthrough**: Source fits within limit, return unchanged
- **Sampling**: Take a representative portion (for updates/clarifications)
- **Chunking**: Split semantically for separate processing (for initial docs)

Design Rationale:
    Chunking is used for Cycle 1 DOCUMENTATION because initial documentation
    needs to see the entire file to build a complete template. Each chunk
    produces partial documentation, then results are merged.

    Sampling is used for CLARIFICATION, CHROME, and Cycle > 1 DOCUMENTATION
    because we already have a template to update. A representative sample
    is sufficient for targeted updates.

Usage:
    from war_rig.workers.source_preparer import SourceCodePreparer, PreparationContext

    preparer = SourceCodePreparer(config.scribe)
    result = preparer.prepare(
        source_code=source,
        context=PreparationContext(
            ticket_type=TicketType.DOCUMENTATION,
            cycle_number=1,
            has_previous_template=False,
            file_type=FileType.COBOL,
        )
    )

    if result.strategy_used == "chunking":
        for chunk in result.chunks:
            output = await scribe_agent.ainvoke(...)
        merged = merger.merge(...)
    else:
        output = await scribe_agent.ainvoke(source_code=result.source_code)

See Also:
    - docs/architecture/SCRIBE_TOKEN_HANDLING_DESIGN.md: Full design document
    - war_rig.chunking: Chunking strategies
    - war_rig.workers.scribe_pool: Integration point
"""

from __future__ import annotations

import logging
import random
from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Any, Literal

from war_rig.beads import TicketType
from war_rig.chunking import (
    ChunkingResult,
    CodeChunk,
    COBOLChunker,
    GenericChunker,
    TokenEstimator,
)
from war_rig.models.templates import FileType

if TYPE_CHECKING:
    from war_rig.config import ScribeConfig

logger = logging.getLogger(__name__)


@dataclass
class PreparationContext:
    """Context for source code preparation decisions.

    Provides the information needed to select the appropriate
    preparation strategy (passthrough, sampling, or chunking).

    Attributes:
        ticket_type: Type of ticket being processed (DOCUMENTATION,
            CLARIFICATION, CHROME).
        cycle_number: Current iteration/cycle number. Cycle 1 is the
            initial documentation pass.
        has_previous_template: Whether a previous template exists for
            this file. True implies an update scenario.
        file_type: Type of source file, used to select the appropriate
            chunker (COBOL-specific vs generic).

    Example:
        >>> context = PreparationContext(
        ...     ticket_type=TicketType.DOCUMENTATION,
        ...     cycle_number=1,
        ...     has_previous_template=False,
        ...     file_type=FileType.COBOL,
        ... )
        >>> context.is_initial_documentation
        True
    """

    ticket_type: TicketType
    cycle_number: int
    has_previous_template: bool
    file_type: FileType = FileType.OTHER

    @property
    def is_initial_documentation(self) -> bool:
        """True if this is cycle 1 documentation without existing template."""
        return (
            self.ticket_type == TicketType.DOCUMENTATION
            and self.cycle_number == 1
            and not self.has_previous_template
        )

    @property
    def is_update_scenario(self) -> bool:
        """True if we're updating existing documentation."""
        return (
            self.has_previous_template
            or self.cycle_number > 1
            or self.ticket_type in (TicketType.CLARIFICATION, TicketType.CHROME)
        )


@dataclass
class PreparedSource:
    """Result of source code preparation for Scribe processing.

    Contains the prepared source code (possibly modified) along with
    metadata about what preparation was performed.

    Attributes:
        source_code: The prepared source code. May be sampled or original.
        strategy_used: The strategy that was applied:
            - "passthrough": Source unchanged (fit within limit)
            - "sampling": Random contiguous portion extracted
            - "chunking": Split into semantic chunks
        was_modified: True if source was altered from original.
        chunks: If chunked, the list of CodeChunks for separate processing.
            Each chunk should be processed independently, then results merged.
        chunking_result: Full chunking metadata if chunking was used.
        metadata: Additional info about the preparation, varies by strategy:
            - sampling: original_lines, sampled_lines, start_line, original_tokens
            - chunking: chunk_count, chunking_strategy

    Example:
        >>> prepared = preparer.prepare(source, context)
        >>> if prepared.strategy_used == "chunking":
        ...     for chunk in prepared.chunks:
        ...         process_chunk(chunk)
        ... else:
        ...     process_directly(prepared.source_code)
    """

    source_code: str
    strategy_used: Literal["passthrough", "sampling", "chunking"]
    was_modified: bool
    chunks: list[CodeChunk] | None = None
    chunking_result: ChunkingResult | None = None
    metadata: dict[str, Any] = field(default_factory=dict)

    @property
    def needs_chunked_processing(self) -> bool:
        """True if source was chunked and needs separate processing per chunk."""
        return self.strategy_used == "chunking" and self.chunks is not None


class SourceCodePreparer:
    """Prepares source code to fit within Scribe's token limits.

    This class centralizes all token limit enforcement for Scribe processing.
    It encapsulates the decision of whether to pass through, sample, or chunk
    source code based on the processing context and token budget.

    Decision Logic:
        1. If source fits within limit: PASSTHROUGH (unchanged)
        2. If cycle 1 DOCUMENTATION without template: CHUNKING
           - Splits at semantic boundaries (COBOL divisions/sections)
           - Each chunk processed separately, results merged
        3. Otherwise (updates, clarifications, chrome): SAMPLING
           - Takes representative contiguous portion
           - Sufficient for targeted template updates

    Attributes:
        config: Scribe configuration with token limits.
        estimator: Token estimator for size calculations.
        OVERHEAD_TOKENS: Reserved tokens for prompt overhead (system prompt,
            schema, instructions, response buffer).

    Example:
        >>> preparer = SourceCodePreparer(config.scribe)
        >>> result = preparer.prepare(source_code, context)
        >>> print(f"Strategy: {result.strategy_used}")
        >>> if result.needs_chunked_processing:
        ...     for chunk in result.chunks:
        ...         output = await process(chunk)

    See Also:
        - PreparationContext: Input context for preparation decisions
        - PreparedSource: Output containing prepared source and metadata
        - war_rig.chunking: Underlying chunking implementations
    """

    # Reserved tokens for prompt overhead:
    # - System prompt: ~2000 tokens
    # - Template schema: ~1500 tokens
    # - Instructions: ~500 tokens
    OVERHEAD_TOKENS: int = 4000

    def __init__(
        self,
        config: "ScribeConfig",
        estimator: TokenEstimator | None = None,
    ):
        """Initialize the source code preparer.

        Args:
            config: Scribe configuration containing max_prompt_tokens.
            estimator: Token estimator. If None, creates a default instance.
        """
        self.config = config
        self.estimator = estimator or TokenEstimator()

    @property
    def max_source_tokens(self) -> int:
        """Maximum tokens available for source code after overhead.

        Returns:
            Token budget for source code (total limit minus overhead).
        """
        return self.config.max_prompt_tokens - self.OVERHEAD_TOKENS

    def needs_preparation(self, source_code: str) -> bool:
        """Check if source code exceeds token limit.

        Args:
            source_code: The source code to check.

        Returns:
            True if source exceeds max_source_tokens and needs preparation.
        """
        source_tokens = self.estimator.estimate_source_tokens(source_code)
        return source_tokens > self.max_source_tokens

    def prepare(
        self,
        source_code: str,
        context: PreparationContext,
    ) -> PreparedSource:
        """Prepare source code for Scribe processing.

        Analyzes the source code size and processing context to determine
        the appropriate preparation strategy, then applies it.

        Args:
            source_code: The raw source code to prepare.
            context: Context about the processing scenario, used to
                select between chunking and sampling strategies.

        Returns:
            PreparedSource with the appropriate strategy applied.
            Check `needs_chunked_processing` to determine if chunked
            processing is required.

        Example:
            >>> prepared = preparer.prepare(large_source, context)
            >>> if prepared.needs_chunked_processing:
            ...     outputs = [await process(c) for c in prepared.chunks]
            ...     final = merger.merge(outputs)
            ... else:
            ...     final = await process(prepared.source_code)
        """
        source_tokens = self.estimator.estimate_source_tokens(source_code)

        # If within limit, pass through unchanged
        if source_tokens <= self.max_source_tokens:
            logger.debug(
                f"Source within limit ({source_tokens} <= {self.max_source_tokens}), "
                f"using passthrough"
            )
            return self._apply_passthrough(source_code)

        # Decide strategy based on context
        if self._should_use_chunking(context):
            logger.info(
                f"Source exceeds limit ({source_tokens} > {self.max_source_tokens}), "
                f"using chunking for {context.ticket_type.value} cycle {context.cycle_number}"
            )
            return self._apply_chunking(source_code, context)
        else:
            logger.info(
                f"Source exceeds limit ({source_tokens} > {self.max_source_tokens}), "
                f"using sampling for {context.ticket_type.value} cycle {context.cycle_number}"
            )
            return self._apply_sampling(source_code, source_tokens)

    def _should_use_chunking(self, context: PreparationContext) -> bool:
        """Determine if chunking should be used.

        Chunking is appropriate when:
        - This is initial documentation (cycle 1, no previous template)
        - We need to see the entire file to build a complete template

        Sampling is appropriate when:
        - We already have a template to update
        - A representative sample is sufficient for targeted changes

        Args:
            context: The preparation context.

        Returns:
            True if chunking should be used, False for sampling.
        """
        return context.is_initial_documentation

    def _apply_passthrough(self, source_code: str) -> PreparedSource:
        """Apply passthrough strategy (source unchanged).

        Args:
            source_code: The source code.

        Returns:
            PreparedSource with unchanged source.
        """
        return PreparedSource(
            source_code=source_code,
            strategy_used="passthrough",
            was_modified=False,
        )

    def _apply_sampling(
        self,
        source_code: str,
        source_tokens: int,
    ) -> PreparedSource:
        """Apply sampling strategy (extract representative portion).

        Takes a random contiguous portion of the source code. The random
        start position provides diversity across retries (different workers
        or retry attempts may see different portions).

        Args:
            source_code: The full source code.
            source_tokens: Estimated token count of full source.

        Returns:
            PreparedSource with sampled source and metadata.
        """
        lines = source_code.split("\n")
        total_lines = len(lines)

        # Calculate how many lines we can include
        ratio = self.max_source_tokens / source_tokens
        target_lines = max(10, int(total_lines * ratio))

        # Random start for diversity across retries
        max_start = max(0, total_lines - target_lines)
        start_line = random.randint(0, max_start) if max_start > 0 else 0

        # Extract sample
        sampled_lines = lines[start_line : start_line + target_lines]
        sampled_code = "\n".join(sampled_lines)

        # Hard truncate if still over character limit (safety net)
        chars_per_token = 4  # Conservative estimate
        max_chars = self.max_source_tokens * chars_per_token
        if len(sampled_code) > max_chars:
            sampled_code = sampled_code[:max_chars]

        # Add informative header
        header = (
            f"* NOTE: Source code sampled (lines {start_line + 1}-"
            f"{start_line + len(sampled_lines)} of {total_lines})\n"
            f"* Full source exceeds token limit ({source_tokens} tokens)\n\n"
        )

        return PreparedSource(
            source_code=header + sampled_code,
            strategy_used="sampling",
            was_modified=True,
            metadata={
                "original_lines": total_lines,
                "sampled_lines": len(sampled_lines),
                "start_line": start_line,
                "end_line": start_line + len(sampled_lines),
                "original_tokens": source_tokens,
                "target_tokens": self.max_source_tokens,
            },
        )

    def _apply_chunking(
        self,
        source_code: str,
        context: PreparationContext,
    ) -> PreparedSource:
        """Apply chunking strategy (semantic splitting).

        Splits source at semantic boundaries based on file type.
        COBOL files are split at division/section boundaries;
        other files use generic line-based chunking.

        Args:
            source_code: The full source code.
            context: Context with file type for chunker selection.

        Returns:
            PreparedSource with chunks list for separate processing.
        """
        # Select chunker based on file type
        if context.file_type == FileType.COBOL:
            chunker = COBOLChunker(self.estimator)
        else:
            chunker = GenericChunker(self.estimator)

        # Chunk the source
        chunking_result = chunker.chunk(
            source_code=source_code,
            max_tokens=self.max_source_tokens,
            file_name="source",
        )

        logger.info(
            f"Chunked source into {chunking_result.chunk_count} chunks "
            f"using {chunking_result.chunking_strategy}"
        )

        return PreparedSource(
            source_code=source_code,  # Original preserved for reference
            strategy_used="chunking",
            was_modified=False,  # Original not modified, just split
            chunks=chunking_result.chunks,
            chunking_result=chunking_result,
            metadata={
                "chunk_count": chunking_result.chunk_count,
                "chunking_strategy": chunking_result.chunking_strategy,
            },
        )
