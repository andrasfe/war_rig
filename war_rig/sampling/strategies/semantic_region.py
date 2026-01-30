"""Semantic region sampling strategy.

Priority 4 strategy that maps question topics to COBOL divisions
and sections, sampling relevant semantic regions.
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


class SemanticRegionStrategy:
    """Sampling strategy based on COBOL semantic regions.

    This strategy (priority=4) maps question topics and section names
    to COBOL divisions and sections. For example, questions about
    "inputs" or "data flow" map to DATA DIVISION, while questions
    about "business rules" or "logic" map to PROCEDURE DIVISION.

    This provides relevant context even when no explicit line citations
    or identifiers are found.

    Attributes:
        RELEVANCE_SCORE: Relevance score for windows from this strategy (0.6).

    Topic Mappings:
        - inputs, outputs, data_flow, files -> DATA DIVISION
        - business_rules, logic, processing, error_handling -> PROCEDURE DIVISION
        - purpose, identification, header -> IDENTIFICATION DIVISION
        - environment, configuration -> ENVIRONMENT DIVISION

    Example:
        >>> strategy = SemanticRegionStrategy()
        >>> # If question.section = "inputs" -> DATA DIVISION window
        >>> windows = strategy.generate_windows(hints, context)
    """

    RELEVANCE_SCORE: float = 0.6

    # Map section/topic names to COBOL divisions
    TOPIC_TO_DIVISION: dict[str, list[str]] = {
        # Map to DATA DIVISION
        "inputs": ["DATA DIVISION"],
        "outputs": ["DATA DIVISION"],
        "data_flow": ["DATA DIVISION", "PROCEDURE DIVISION"],
        "files": ["DATA DIVISION", "ENVIRONMENT DIVISION"],
        "copybooks_used": ["DATA DIVISION"],
        "data": ["DATA DIVISION"],
        # Map to PROCEDURE DIVISION
        "business_rules": ["PROCEDURE DIVISION"],
        "logic": ["PROCEDURE DIVISION"],
        "processing": ["PROCEDURE DIVISION"],
        "error_handling": ["PROCEDURE DIVISION"],
        "called_programs": ["PROCEDURE DIVISION"],
        "sql_operations": ["PROCEDURE DIVISION"],
        "cics_operations": ["PROCEDURE DIVISION"],
        "paragraphs": ["PROCEDURE DIVISION"],
        # Map to IDENTIFICATION DIVISION
        "purpose": ["IDENTIFICATION DIVISION", "PROCEDURE DIVISION"],
        "header": ["IDENTIFICATION DIVISION"],
        "identification": ["IDENTIFICATION DIVISION"],
        # Map to ENVIRONMENT DIVISION
        "environment": ["ENVIRONMENT DIVISION"],
        "configuration": ["ENVIRONMENT DIVISION"],
        "calling_context": ["IDENTIFICATION DIVISION", "PROCEDURE DIVISION"],
    }

    # Patterns to find division boundaries in COBOL source
    DIVISION_PATTERNS: dict[str, re.Pattern[str]] = {
        "IDENTIFICATION DIVISION": re.compile(
            r"^\s*IDENTIFICATION\s+DIVISION\s*\.", re.IGNORECASE | re.MULTILINE
        ),
        "ENVIRONMENT DIVISION": re.compile(
            r"^\s*ENVIRONMENT\s+DIVISION\s*\.", re.IGNORECASE | re.MULTILINE
        ),
        "DATA DIVISION": re.compile(
            r"^\s*DATA\s+DIVISION\s*\.", re.IGNORECASE | re.MULTILINE
        ),
        "PROCEDURE DIVISION": re.compile(
            r"^\s*PROCEDURE\s+DIVISION", re.IGNORECASE | re.MULTILINE
        ),
    }

    # Standard division order
    DIVISION_ORDER: list[str] = [
        "IDENTIFICATION DIVISION",
        "ENVIRONMENT DIVISION",
        "DATA DIVISION",
        "PROCEDURE DIVISION",
    ]

    @property
    def name(self) -> str:
        """Name of this strategy."""
        return "semantic_region"

    @property
    def priority(self) -> int:
        """Priority of this strategy (4 = medium)."""
        return 4

    def can_apply(
        self,
        hints: RelevanceHints,
        context: SamplingContext,
    ) -> bool:
        """Check if this strategy can be applied.

        Applies when there are section names and the file is COBOL.

        Args:
            hints: Extracted relevance hints.
            context: Sampling context.

        Returns:
            True if there are section names and file is COBOL-like.
        """
        from war_rig.models.templates import FileType

        # Only apply to COBOL files
        if context.file_type not in (FileType.COBOL, FileType.COPYBOOK):
            # Check if source looks like COBOL
            source_upper = context.source_code.upper()
            if not any(
                div in source_upper
                for div in ["IDENTIFICATION DIVISION", "PROCEDURE DIVISION", "DATA DIVISION"]
            ):
                return False

        return bool(hints.section_names)

    def generate_windows(
        self,
        hints: RelevanceHints,
        context: SamplingContext,
    ) -> list[SourceWindow]:
        """Generate windows for relevant COBOL divisions.

        Maps section names to divisions and finds those divisions
        in the source code.

        Args:
            hints: Relevance hints containing section names.
            context: Sampling context with source code.

        Returns:
            List of SourceWindow objects for relevant divisions.
        """
        windows: list[SourceWindow] = []

        # Determine which divisions to sample based on section names
        divisions_needed: set[str] = set()
        for section in hints.section_names:
            section_lower = section.lower()
            if section_lower in self.TOPIC_TO_DIVISION:
                divisions_needed.update(self.TOPIC_TO_DIVISION[section_lower])

        if not divisions_needed:
            logger.debug(
                f"SemanticRegion: No division mapping for sections {hints.section_names}"
            )
            return windows

        # Find division boundaries in source
        division_bounds = self._find_division_boundaries(context)

        if not division_bounds:
            logger.debug("SemanticRegion: No COBOL divisions found in source")
            return windows

        # Create windows for needed divisions
        for division_name in divisions_needed:
            if division_name in division_bounds:
                start, end = division_bounds[division_name]
                window = SourceWindow(
                    start_line=start,
                    end_line=end,
                    reason=f"Semantic region: {division_name}",
                    strategy_name=self.name,
                    relevance_score=self.RELEVANCE_SCORE,
                )
                windows.append(window)
                logger.debug(
                    f"SemanticRegion: Created window [{start}-{end}] "
                    f"for {division_name}"
                )

        return windows

    def _find_division_boundaries(
        self,
        context: SamplingContext,
    ) -> dict[str, tuple[int, int]]:
        """Find the line boundaries of each COBOL division.

        Args:
            context: Sampling context with source code.

        Returns:
            Dict mapping division name to (start_line, end_line) tuple.
        """
        bounds: dict[str, tuple[int, int]] = {}
        division_starts: dict[str, int] = {}

        # Find where each division starts
        for div_name, pattern in self.DIVISION_PATTERNS.items():
            match = pattern.search(context.source_code)
            if match:
                # Convert character offset to line number
                line_num = context.source_code[:match.start()].count("\n") + 1
                division_starts[div_name] = line_num

        if not division_starts:
            return bounds

        # Sort divisions by their start position
        sorted_divisions = sorted(
            division_starts.items(),
            key=lambda x: x[1],
        )

        # Calculate end of each division (start of next - 1, or end of file)
        for i, (div_name, start_line) in enumerate(sorted_divisions):
            if i + 1 < len(sorted_divisions):
                # End at the line before the next division
                end_line = sorted_divisions[i + 1][1] - 1
            else:
                # Last division ends at end of file
                end_line = context.total_lines

            bounds[div_name] = (start_line, end_line)

        return bounds
