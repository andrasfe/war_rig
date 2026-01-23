"""Identifier mention sampling strategy.

Priority 3 strategy that searches source code for COBOL-style
identifiers mentioned in question text.
"""

from __future__ import annotations

import logging
import re
from typing import TYPE_CHECKING

from war_rig.sampling.strategies.protocol import SourceWindow

if TYPE_CHECKING:
    from war_rig.sampling.analyzer import RelevanceHints
    from war_rig.sampling.context import SamplingContext

logger = logging.getLogger(__name__)


class IdentifierMentionStrategy:
    """Sampling strategy based on identifier mentions in questions.

    This strategy (priority=3) searches the source code for COBOL-style
    identifiers (like CUSTOMER-RECORD, WS-ACCOUNT-ID) that appear in
    the question text. It creates windows around where those identifiers
    are used in the source.

    This helps when questions mention specific fields or paragraph names
    without explicit line citations.

    Attributes:
        CONTEXT_LINES: Number of lines to include around each match.
        RELEVANCE_SCORE: Relevance score for windows from this strategy (0.7).
        MAX_MATCHES_PER_IDENTIFIER: Maximum matches to include per identifier.

    Example:
        >>> strategy = IdentifierMentionStrategy()
        >>> # If question mentions "CUSTOMER-RECORD" and it appears in source
        >>> windows = strategy.generate_windows(hints, context)
    """

    CONTEXT_LINES: int = 10
    RELEVANCE_SCORE: float = 0.7
    MAX_MATCHES_PER_IDENTIFIER: int = 5

    @property
    def name(self) -> str:
        """Name of this strategy."""
        return "identifier_mention"

    @property
    def priority(self) -> int:
        """Priority of this strategy (3 = medium-high)."""
        return 3

    def can_apply(
        self,
        hints: "RelevanceHints",
        context: "SamplingContext",
    ) -> bool:
        """Check if there are identifiers to search for.

        Args:
            hints: Extracted relevance hints.
            context: Sampling context.

        Returns:
            True if there are identifiers in hints.
        """
        return bool(hints.identifiers)

    def generate_windows(
        self,
        hints: "RelevanceHints",
        context: "SamplingContext",
    ) -> list[SourceWindow]:
        """Generate windows around identifier occurrences in source.

        Searches the source code for each identifier and creates
        windows around the matches.

        Args:
            hints: Relevance hints containing identifiers.
            context: Sampling context with source code.

        Returns:
            List of SourceWindow objects for identifier matches.
        """
        windows: list[SourceWindow] = []

        for identifier in hints.identifiers:
            # Create a pattern to match the identifier as a whole word
            # Handle both exact match and with surrounding non-word chars
            pattern = re.compile(
                rf"\b{re.escape(identifier)}\b",
                re.IGNORECASE,
            )

            matches_found = 0
            for line_idx, line in enumerate(context.source_lines):
                if pattern.search(line):
                    line_num = line_idx + 1  # Convert to 1-indexed

                    window = SourceWindow(
                        start_line=max(1, line_num - self.CONTEXT_LINES),
                        end_line=min(context.total_lines, line_num + self.CONTEXT_LINES),
                        reason=f"Identifier '{identifier}' found at line {line_num}",
                        strategy_name=self.name,
                        relevance_score=self.RELEVANCE_SCORE,
                    )
                    windows.append(window)
                    logger.debug(
                        f"IdentifierMention: Found '{identifier}' at line {line_num}, "
                        f"created window [{window.start_line}-{window.end_line}]"
                    )

                    matches_found += 1
                    if matches_found >= self.MAX_MATCHES_PER_IDENTIFIER:
                        break

            if matches_found == 0:
                logger.debug(
                    f"IdentifierMention: Identifier '{identifier}' not found in source"
                )

        return windows
