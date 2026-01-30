"""Section reference sampling strategy.

Priority 2 strategy that creates windows from template section citations
when questions reference specific documentation sections.
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING

from war_rig.sampling.strategies.protocol import SourceWindow

if TYPE_CHECKING:
    from war_rig.sampling.analyzer import RelevanceHints
    from war_rig.sampling.context import SamplingContext

logger = logging.getLogger(__name__)


class SectionReferenceStrategy:
    """Sampling strategy based on template section citations.

    This strategy (priority=2) uses the section field from questions
    to look up citations in the previous documentation template.
    It then creates windows around those cited lines.

    This is useful when a question like "The inputs section seems
    incomplete" references a section that has line citations in the
    existing template.

    Attributes:
        CONTEXT_LINES: Number of lines to include around each citation.
        RELEVANCE_SCORE: Relevance score for windows from this strategy (0.9).

    Example:
        >>> strategy = SectionReferenceStrategy()
        >>> # If question.section = "inputs" and template.inputs has citations
        >>> windows = strategy.generate_windows(hints, context)
    """

    CONTEXT_LINES: int = 10
    RELEVANCE_SCORE: float = 0.9

    @property
    def name(self) -> str:
        """Name of this strategy."""
        return "section_reference"

    @property
    def priority(self) -> int:
        """Priority of this strategy (2 = high)."""
        return 2

    def can_apply(
        self,
        hints: RelevanceHints,
        context: SamplingContext,
    ) -> bool:
        """Check if there are section citations to use.

        Args:
            hints: Extracted relevance hints.
            context: Sampling context.

        Returns:
            True if there are section citations in hints.
        """
        return bool(hints.section_citations)

    def generate_windows(
        self,
        hints: RelevanceHints,
        context: SamplingContext,
    ) -> list[SourceWindow]:
        """Generate windows around section citation lines.

        Creates windows around lines cited in template sections that
        are referenced by the questions.

        Args:
            hints: Relevance hints containing section citations.
            context: Sampling context with source information.

        Returns:
            List of SourceWindow objects for section citations.
        """
        windows: list[SourceWindow] = []

        for section_name, line_numbers in hints.section_citations.items():
            for line_num in line_numbers:
                window = SourceWindow(
                    start_line=max(1, line_num - self.CONTEXT_LINES),
                    end_line=min(context.total_lines, line_num + self.CONTEXT_LINES),
                    reason=f"Citation from '{section_name}' section (line {line_num})",
                    strategy_name=self.name,
                    relevance_score=self.RELEVANCE_SCORE,
                )
                windows.append(window)
                logger.debug(
                    f"SectionReference: Created window [{window.start_line}-{window.end_line}] "
                    f"for section '{section_name}' citation at line {line_num}"
                )

        return windows
