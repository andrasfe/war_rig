"""Window composer for merging and selecting source windows.

This module provides the WindowComposer class which takes multiple
SourceWindow objects from various strategies and composes them into
a final sample that fits within the token budget.
"""

from __future__ import annotations

import logging
from dataclasses import dataclass
from typing import TYPE_CHECKING

from war_rig.sampling.strategies.protocol import SourceWindow

if TYPE_CHECKING:
    from war_rig.chunking import TokenEstimator

logger = logging.getLogger(__name__)


@dataclass
class ComposedSample:
    """Result of composing windows into a final sample.

    Contains the sampled source code along with metadata about
    what was included and why.

    Attributes:
        source_code: The composed source code sample.
        windows_included: List of windows that were included in the sample.
        total_lines_sampled: Total number of unique lines in the sample.
        strategies_used: Set of strategy names that contributed windows.
        header: Explanatory header prepended to the sample.
        was_truncated: True if some windows were excluded due to budget.

    Example:
        >>> sample = composer.compose(windows, source_lines, max_tokens)
        >>> print(f"Sampled {sample.total_lines_sampled} lines")
        >>> print(f"Strategies used: {sample.strategies_used}")
    """

    source_code: str
    windows_included: list[SourceWindow]
    total_lines_sampled: int
    strategies_used: set[str]
    header: str
    was_truncated: bool = False


class WindowComposer:
    """Composes multiple source windows into a final sample.

    The composer takes windows from various sampling strategies and:
    1. Sorts by relevance_score descending
    2. Merges overlapping windows
    3. Adds context margins (+/- 5 lines)
    4. Greedily selects windows until token budget is exhausted
    5. Generates an explanatory header

    This ensures the most relevant portions are included while staying
    within the token budget.

    Attributes:
        estimator: TokenEstimator for calculating token counts.
        CONTEXT_MARGIN: Additional lines to add around each window.

    Example:
        >>> composer = WindowComposer(estimator)
        >>> sample = composer.compose(windows, source_lines, max_tokens=10000)
        >>> print(sample.source_code)
    """

    CONTEXT_MARGIN: int = 5

    def __init__(self, estimator: TokenEstimator):
        """Initialize the window composer.

        Args:
            estimator: TokenEstimator for token counting.
        """
        self.estimator = estimator

    def compose(
        self,
        windows: list[SourceWindow],
        source_lines: list[str],
        max_tokens: int,
    ) -> ComposedSample:
        """Compose windows into a final sample.

        Args:
            windows: List of SourceWindow objects from strategies.
            source_lines: Source code split into lines.
            max_tokens: Maximum tokens for the sample.

        Returns:
            ComposedSample with the composed source code and metadata.
        """
        if not windows:
            return ComposedSample(
                source_code="",
                windows_included=[],
                total_lines_sampled=0,
                strategies_used=set(),
                header="* No relevant source code windows identified\n",
            )

        total_lines = len(source_lines)

        # Step 1: Sort by relevance score (descending)
        sorted_windows = sorted(
            windows,
            key=lambda w: (-w.relevance_score, w.start_line),
        )

        # Step 2: Add context margins and clamp to bounds
        expanded_windows = [
            self._expand_window(w, total_lines)
            for w in sorted_windows
        ]

        # Step 3: Merge overlapping windows
        merged_windows = self._merge_overlapping(expanded_windows)

        # Step 4: Greedily select windows within budget
        selected_windows, was_truncated = self._select_within_budget(
            merged_windows,
            source_lines,
            max_tokens,
        )

        if not selected_windows:
            return ComposedSample(
                source_code="",
                windows_included=[],
                total_lines_sampled=0,
                strategies_used=set(),
                header="* Token budget too small for any source windows\n",
                was_truncated=True,
            )

        # Step 5: Generate the composed source code
        source_code, lines_sampled = self._generate_source(
            selected_windows,
            source_lines,
            total_lines,
        )

        # Step 6: Generate header
        strategies_used = {w.strategy_name for w in selected_windows}
        header = self._generate_header(
            selected_windows,
            lines_sampled,
            total_lines,
            strategies_used,
            was_truncated,
        )

        return ComposedSample(
            source_code=header + source_code,
            windows_included=selected_windows,
            total_lines_sampled=lines_sampled,
            strategies_used=strategies_used,
            header=header,
            was_truncated=was_truncated,
        )

    def _expand_window(
        self,
        window: SourceWindow,
        total_lines: int,
    ) -> SourceWindow:
        """Expand a window with context margin.

        Args:
            window: Window to expand.
            total_lines: Maximum valid line number.

        Returns:
            New window with expanded bounds.
        """
        return SourceWindow(
            start_line=max(1, window.start_line - self.CONTEXT_MARGIN),
            end_line=min(total_lines, window.end_line + self.CONTEXT_MARGIN),
            reason=window.reason,
            strategy_name=window.strategy_name,
            relevance_score=window.relevance_score,
        )

    def _merge_overlapping(
        self,
        windows: list[SourceWindow],
    ) -> list[SourceWindow]:
        """Merge overlapping or adjacent windows.

        Processes windows in order and merges any that overlap.
        Maintains the highest relevance score when merging.

        Args:
            windows: List of windows to merge (should be sorted by relevance).

        Returns:
            List of non-overlapping windows.
        """
        if not windows:
            return []

        # Sort by start_line for merging
        sorted_by_line = sorted(windows, key=lambda w: w.start_line)
        merged: list[SourceWindow] = [sorted_by_line[0]]

        for window in sorted_by_line[1:]:
            if merged[-1].overlaps(window):
                # Merge with the last window
                merged[-1] = merged[-1].merge(window)
            else:
                merged.append(window)

        # Re-sort by relevance for selection
        return sorted(merged, key=lambda w: (-w.relevance_score, w.start_line))

    def _select_within_budget(
        self,
        windows: list[SourceWindow],
        source_lines: list[str],
        max_tokens: int,
    ) -> tuple[list[SourceWindow], bool]:
        """Select windows that fit within token budget.

        Greedily adds windows in relevance order until budget is exceeded.

        Args:
            windows: Windows sorted by relevance (highest first).
            source_lines: Source code lines.
            max_tokens: Maximum tokens allowed.

        Returns:
            Tuple of (selected windows, was_truncated flag).
        """
        selected: list[SourceWindow] = []
        current_tokens = 0
        was_truncated = False

        # Reserve some tokens for header (estimate ~200 tokens)
        header_reserve = 200
        effective_max = max_tokens - header_reserve

        for window in windows:
            # Calculate tokens for this window
            window_lines = source_lines[window.start_line - 1 : window.end_line]
            window_text = "\n".join(window_lines)
            window_tokens = self.estimator.estimate_source_tokens(window_text)

            if current_tokens + window_tokens <= effective_max:
                selected.append(window)
                current_tokens += window_tokens
            else:
                # Can't fit this window
                was_truncated = True
                # Try to find smaller windows that might fit
                # (continue loop to find any small windows that fit)

        return selected, was_truncated

    def _generate_source(
        self,
        windows: list[SourceWindow],
        source_lines: list[str],
        total_lines: int,
    ) -> tuple[str, int]:
        """Generate the final source code from selected windows.

        Includes line numbers and ellipsis markers between non-contiguous
        windows.

        Args:
            windows: Selected windows to include.
            source_lines: Source code lines.
            total_lines: Total lines in source.

        Returns:
            Tuple of (source code string, total lines sampled).
        """
        # Sort windows by start line for output
        sorted_windows = sorted(windows, key=lambda w: w.start_line)

        parts: list[str] = []
        lines_sampled = 0
        last_end = 0

        for window in sorted_windows:
            # Add ellipsis if there's a gap
            if last_end > 0 and window.start_line > last_end + 1:
                gap_size = window.start_line - last_end - 1
                parts.append(f"\n... [{gap_size} lines omitted] ...\n\n")

            # Add the window content with line numbers
            for i in range(window.start_line, window.end_line + 1):
                if i <= len(source_lines):
                    line_content = source_lines[i - 1]
                    # Format: line number right-aligned in 6 chars, then tab, then content
                    parts.append(f"{i:6d}\t{line_content}")
                    lines_sampled += 1

            last_end = window.end_line

        # Add ellipsis at end if we didn't include the last lines
        if sorted_windows and sorted_windows[-1].end_line < total_lines:
            remaining = total_lines - sorted_windows[-1].end_line
            parts.append(f"\n... [{remaining} lines to end of file] ...")

        return "\n".join(parts), lines_sampled

    def _generate_header(
        self,
        windows: list[SourceWindow],
        lines_sampled: int,
        total_lines: int,
        strategies_used: set[str],
        was_truncated: bool,
    ) -> str:
        """Generate an explanatory header for the sample.

        Args:
            windows: Windows included in the sample.
            lines_sampled: Total lines sampled.
            total_lines: Total lines in source.
            strategies_used: Strategies that contributed windows.
            was_truncated: Whether some windows were excluded.

        Returns:
            Header string to prepend to the sample.
        """
        lines = [
            "* INTELLIGENT SOURCE SAMPLING",
            f"* Sampled {lines_sampled} of {total_lines} lines "
            f"({100 * lines_sampled / total_lines:.1f}%)",
            f"* Strategies used: {', '.join(sorted(strategies_used))}",
        ]

        if was_truncated:
            lines.append("* NOTE: Some relevant regions excluded due to token budget")

        # Add window descriptions
        lines.append("* Windows included:")
        for window in sorted(windows, key=lambda w: w.start_line):
            lines.append(
                f"*   - Lines {window.start_line}-{window.end_line}: {window.reason}"
            )

        lines.append("*")
        lines.append("")  # Empty line before source code

        return "\n".join(lines)
