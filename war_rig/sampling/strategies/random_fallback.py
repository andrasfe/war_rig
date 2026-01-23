"""Random fallback sampling strategy.

Priority 99 (lowest) strategy that provides random sampling when no
explicit hints are available. This maintains the current random
sampling behavior as a fallback.
"""

from __future__ import annotations

import logging
import random
from typing import TYPE_CHECKING

from war_rig.sampling.strategies.protocol import SourceWindow

if TYPE_CHECKING:
    from war_rig.sampling.analyzer import RelevanceHints
    from war_rig.sampling.context import SamplingContext

logger = logging.getLogger(__name__)


class RandomFallbackStrategy:
    """Fallback sampling strategy using random selection.

    This is the lowest priority strategy (priority=99) that only applies
    when no explicit hints are available. It provides random sampling
    behavior similar to the original implementation.

    The strategy samples a portion of the file based on the token budget
    ratio, with a random starting position to provide diversity across
    retry attempts.

    Attributes:
        RELEVANCE_SCORE: Relevance score for windows from this strategy (0.1).
        MIN_LINES: Minimum number of lines to sample.

    Example:
        >>> strategy = RandomFallbackStrategy()
        >>> # Only applies when hints.has_explicit_hints is False
        >>> if strategy.can_apply(hints, context):
        ...     windows = strategy.generate_windows(hints, context)
    """

    RELEVANCE_SCORE: float = 0.1
    MIN_LINES: int = 10

    @property
    def name(self) -> str:
        """Name of this strategy."""
        return "random_fallback"

    @property
    def priority(self) -> int:
        """Priority of this strategy (99 = lowest)."""
        return 99

    def can_apply(
        self,
        hints: "RelevanceHints",
        context: "SamplingContext",
    ) -> bool:
        """Check if random fallback should be used.

        Only applies when no explicit hints were found.

        Args:
            hints: Extracted relevance hints.
            context: Sampling context.

        Returns:
            True if no explicit hints are available.
        """
        return not hints.has_explicit_hints

    def generate_windows(
        self,
        hints: "RelevanceHints",
        context: "SamplingContext",
    ) -> list[SourceWindow]:
        """Generate a random sample window.

        Creates a single window that samples a portion of the file
        based on the token budget, with a random starting position.

        Args:
            hints: Relevance hints (not used, but required by protocol).
            context: Sampling context with source information.

        Returns:
            List containing a single random sample window.
        """
        # Calculate how many lines we can fit in the budget
        if context.estimated_source_tokens > 0:
            ratio = context.max_tokens / context.estimated_source_tokens
        else:
            ratio = 1.0

        target_lines = max(self.MIN_LINES, int(context.total_lines * ratio))

        # Ensure we don't exceed available lines
        target_lines = min(target_lines, context.total_lines)

        # Random starting position
        max_start = max(0, context.total_lines - target_lines)
        start_line = random.randint(1, max_start + 1) if max_start > 0 else 1

        end_line = min(context.total_lines, start_line + target_lines - 1)

        window = SourceWindow(
            start_line=start_line,
            end_line=end_line,
            reason=f"Random sample (lines {start_line}-{end_line} of {context.total_lines})",
            strategy_name=self.name,
            relevance_score=self.RELEVANCE_SCORE,
        )

        logger.debug(
            f"RandomFallback: Created window [{start_line}-{end_line}] "
            f"({target_lines} lines, {ratio:.1%} of source)"
        )

        return [window]
