"""Sampling orchestrator for intelligent source code sampling.

This module provides the SamplingOrchestrator class which coordinates
the entire intelligent sampling process for CLARIFICATION and CHROME tickets.
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from typing import TYPE_CHECKING

from war_rig.sampling.analyzer import RelevanceAnalyzer
from war_rig.sampling.composer import WindowComposer
from war_rig.sampling.context import SamplingContext
from war_rig.sampling.strategies import (
    IdentifierMentionStrategy,
    LineCitationStrategy,
    RandomFallbackStrategy,
    SamplingStrategy,
    SectionReferenceStrategy,
    SemanticRegionStrategy,
    SourceWindow,
)

if TYPE_CHECKING:
    from war_rig.beads import TicketType
    from war_rig.chunking import TokenEstimator
    from war_rig.models.templates import DocumentationTemplate, FileType
    from war_rig.models.tickets import ChallengerQuestion, ChromeTicket
    from war_rig.workers.source_preparer import PreparedSource

logger = logging.getLogger(__name__)


@dataclass
class SamplingResult:
    """Result of intelligent source code sampling.

    Contains the sampled source code and metadata about the sampling
    process for debugging and logging.

    Attributes:
        source_code: The final sampled source code with header.
        original_lines: Total lines in the original source.
        sampled_lines: Number of lines in the sample.
        strategies_used: Names of strategies that contributed windows.
        windows: Windows that were included in the sample.
        was_truncated: True if some windows were excluded due to budget.
        hints_found: Summary of hints extracted from questions/tickets.

    Example:
        >>> result = orchestrator.prepare_sample(...)
        >>> print(f"Sampled {result.sampled_lines} of {result.original_lines} lines")
    """

    source_code: str
    original_lines: int
    sampled_lines: int
    strategies_used: set[str]
    windows: list[SourceWindow]
    was_truncated: bool
    hints_found: dict[str, int] = field(default_factory=dict)


class SamplingOrchestrator:
    """Orchestrates intelligent source code sampling.

    The orchestrator coordinates the entire sampling process:
    1. Builds a SamplingContext from inputs
    2. Analyzes questions/tickets for relevance hints
    3. Applies eligible sampling strategies
    4. Composes windows into a final sample

    This provides a single entry point for intelligent sampling
    used by SourceCodePreparer for CLARIFICATION and CHROME tickets.

    Attributes:
        analyzer: RelevanceAnalyzer for extracting hints.
        composer: WindowComposer for merging and selecting windows.
        strategies: List of sampling strategies in priority order.
        estimator: TokenEstimator for token counting.

    Example:
        >>> orchestrator = SamplingOrchestrator(estimator)
        >>> result = orchestrator.prepare_sample(
        ...     source_code=source,
        ...     file_name="CBACT04C.cbl",
        ...     file_type=FileType.COBOL,
        ...     max_tokens=11000,
        ...     ticket_type=TicketType.CLARIFICATION,
        ...     challenger_questions=questions,
        ...     previous_template=template,
        ... )
    """

    def __init__(self, estimator: TokenEstimator):
        """Initialize the sampling orchestrator.

        Args:
            estimator: TokenEstimator for token counting.
        """
        self.estimator = estimator
        self.analyzer = RelevanceAnalyzer()
        self.composer = WindowComposer(estimator)

        # Initialize strategies in priority order
        self.strategies: list[SamplingStrategy] = [
            LineCitationStrategy(),
            SectionReferenceStrategy(),
            IdentifierMentionStrategy(),
            SemanticRegionStrategy(),
            RandomFallbackStrategy(),
        ]

    def prepare_sample(
        self,
        source_code: str,
        file_name: str,
        file_type: FileType,
        max_tokens: int,
        ticket_type: TicketType,
        challenger_questions: list[ChallengerQuestion] | None = None,
        chrome_tickets: list[ChromeTicket] | None = None,
        previous_template: DocumentationTemplate | None = None,
    ) -> SamplingResult:
        """Prepare an intelligent source code sample.

        Main entry point for intelligent sampling. Analyzes the provided
        questions/tickets, applies relevant strategies, and composes
        the final sample.

        Args:
            source_code: Complete source code to sample from.
            file_name: Name of the source file.
            file_type: Type of source file (COBOL, JCL, etc.).
            max_tokens: Maximum tokens for the sample.
            ticket_type: Type of ticket (CLARIFICATION, CHROME).
            challenger_questions: Questions from Challenger, if any.
            chrome_tickets: Chrome tickets from Imperator, if any.
            previous_template: Previous documentation template.

        Returns:
            SamplingResult with the sampled source and metadata.
        """
        # Estimate source tokens
        estimated_tokens = self.estimator.estimate_source_tokens(source_code)

        # Build sampling context
        context = SamplingContext.create(
            source_code=source_code,
            file_name=file_name,
            file_type=file_type,
            max_tokens=max_tokens,
            estimated_source_tokens=estimated_tokens,
            ticket_type=ticket_type,
            challenger_questions=challenger_questions,
            chrome_tickets=chrome_tickets,
            previous_template=previous_template,
        )

        logger.info(
            f"Intelligent sampling: {file_name}, {context.total_lines} lines, "
            f"{estimated_tokens} tokens -> max {max_tokens} tokens"
        )

        # Analyze for relevance hints
        hints = self.analyzer.analyze(context)

        # Log hint summary
        hints_found = {
            "line_numbers": len(hints.line_numbers),
            "line_ranges": len(hints.line_ranges),
            "section_names": len(hints.section_names),
            "section_citations": sum(len(v) for v in hints.section_citations.values()),
            "identifiers": len(hints.identifiers),
        }
        logger.debug(f"Hints extracted: {hints_found}")

        # Apply strategies and collect windows
        all_windows: list[SourceWindow] = []

        for strategy in self.strategies:
            if strategy.can_apply(hints, context):
                windows = strategy.generate_windows(hints, context)
                all_windows.extend(windows)
                logger.debug(
                    f"Strategy '{strategy.name}' (priority={strategy.priority}) "
                    f"generated {len(windows)} windows"
                )

        # Compose windows into final sample
        composed = self.composer.compose(
            windows=all_windows,
            source_lines=context.source_lines,
            max_tokens=max_tokens,
        )

        logger.info(
            f"Sampling complete: {composed.total_lines_sampled} lines, "
            f"strategies={sorted(composed.strategies_used)}, "
            f"truncated={composed.was_truncated}"
        )

        return SamplingResult(
            source_code=composed.source_code,
            original_lines=context.total_lines,
            sampled_lines=composed.total_lines_sampled,
            strategies_used=composed.strategies_used,
            windows=composed.windows_included,
            was_truncated=composed.was_truncated,
            hints_found=hints_found,
        )

    def to_prepared_source(self, result: SamplingResult) -> PreparedSource:
        """Convert SamplingResult to PreparedSource for compatibility.

        This method creates a PreparedSource object compatible with the
        existing SourceCodePreparer interface.

        Args:
            result: SamplingResult from prepare_sample().

        Returns:
            PreparedSource with sampling metadata.
        """
        from war_rig.workers.source_preparer import PreparedSource

        return PreparedSource(
            source_code=result.source_code,
            strategy_used="sampling",
            was_modified=True,
            metadata={
                "original_lines": result.original_lines,
                "sampled_lines": result.sampled_lines,
                "strategies_used": list(result.strategies_used),
                "window_count": len(result.windows),
                "was_truncated": result.was_truncated,
                "hints_found": result.hints_found,
                "intelligent_sampling": True,
            },
        )
