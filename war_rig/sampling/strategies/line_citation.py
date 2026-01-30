"""Line citation sampling strategy.

Priority 1 strategy that creates windows around explicitly cited
line numbers from question evidence fields.
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING

from war_rig.sampling.strategies.protocol import SourceWindow

if TYPE_CHECKING:
    from war_rig.sampling.analyzer import RelevanceHints
    from war_rig.sampling.context import SamplingContext

logger = logging.getLogger(__name__)


class LineCitationStrategy:
    """Sampling strategy based on explicit line number citations.

    This is the highest priority strategy (priority=1) because explicit
    line citations in question evidence are the most reliable indicators
    of where to look in the source code.

    The strategy creates windows of +/-10 lines around each cited line
    number, providing enough context for understanding the code.

    Attributes:
        CONTEXT_LINES: Number of lines to include before and after each citation.
        RELEVANCE_SCORE: Relevance score for windows from this strategy (1.0 = highest).

    Example:
        >>> strategy = LineCitationStrategy()
        >>> if strategy.can_apply(hints, context):
        ...     windows = strategy.generate_windows(hints, context)
        ...     for w in windows:
        ...         print(f"Window: lines {w.start_line}-{w.end_line}")
    """

    CONTEXT_LINES: int = 10
    RELEVANCE_SCORE: float = 1.0

    @property
    def name(self) -> str:
        """Name of this strategy."""
        return "line_citation"

    @property
    def priority(self) -> int:
        """Priority of this strategy (1 = highest)."""
        return 1

    def can_apply(
        self,
        hints: RelevanceHints,
        context: SamplingContext,
    ) -> bool:
        """Check if there are line numbers to create windows around.

        Args:
            hints: Extracted relevance hints.
            context: Sampling context.

        Returns:
            True if there are line numbers or line ranges in hints.
        """
        return bool(hints.line_numbers or hints.line_ranges)

    def generate_windows(
        self,
        hints: RelevanceHints,
        context: SamplingContext,
    ) -> list[SourceWindow]:
        """Generate windows around cited line numbers.

        Creates a window of +/-CONTEXT_LINES around each cited line,
        clamped to valid source bounds.

        Args:
            hints: Relevance hints containing line numbers.
            context: Sampling context with source information.

        Returns:
            List of SourceWindow objects for each citation.
        """
        windows: list[SourceWindow] = []

        # Process individual line numbers
        for line_num in hints.line_numbers:
            window = SourceWindow(
                start_line=max(1, line_num - self.CONTEXT_LINES),
                end_line=min(context.total_lines, line_num + self.CONTEXT_LINES),
                reason=f"Lines around cited line {line_num}",
                strategy_name=self.name,
                relevance_score=self.RELEVANCE_SCORE,
            )
            windows.append(window)
            logger.debug(
                f"LineCitation: Created window [{window.start_line}-{window.end_line}] "
                f"for line {line_num}"
            )

        # Process line ranges
        for start, end in hints.line_ranges:
            # Add context around the range
            window = SourceWindow(
                start_line=max(1, start - self.CONTEXT_LINES),
                end_line=min(context.total_lines, end + self.CONTEXT_LINES),
                reason=f"Lines around cited range {start}-{end}",
                strategy_name=self.name,
                relevance_score=self.RELEVANCE_SCORE,
            )
            windows.append(window)
            logger.debug(
                f"LineCitation: Created window [{window.start_line}-{window.end_line}] "
                f"for range {start}-{end}"
            )

        return windows
