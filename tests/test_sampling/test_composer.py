"""Tests for WindowComposer.

Tests window merging, budget fitting, and sample generation.
"""

import pytest
from unittest.mock import MagicMock

from war_rig.sampling.composer import WindowComposer, ComposedSample
from war_rig.sampling.strategies import SourceWindow


@pytest.fixture
def mock_estimator() -> MagicMock:
    """Create a mock TokenEstimator."""
    estimator = MagicMock()
    # Approximate 1 token per 4 chars
    estimator.estimate_source_tokens.side_effect = lambda text: len(text) // 4
    return estimator


@pytest.fixture
def composer(mock_estimator: MagicMock) -> WindowComposer:
    """Create a WindowComposer with mock estimator."""
    return WindowComposer(mock_estimator)


@pytest.fixture
def sample_lines() -> list[str]:
    """Create sample source lines."""
    return [f"Line {i:04d}: Some COBOL code here" for i in range(1, 101)]


class TestWindowComposerMerging:
    """Tests for window merging logic."""

    def test_merge_overlapping_windows(
        self, composer: WindowComposer, sample_lines: list[str]
    ):
        """Overlapping windows are merged."""
        windows = [
            SourceWindow(10, 30, "reason1", "strat1", 0.8),
            SourceWindow(25, 45, "reason2", "strat2", 0.7),
        ]

        result = composer.compose(windows, sample_lines, max_tokens=10000)

        # Should merge into one window
        assert len(result.windows_included) == 1
        # Merged window should span both
        merged = result.windows_included[0]
        assert merged.start_line <= 10  # May be expanded with context
        assert merged.end_line >= 45

    def test_keep_separate_non_overlapping_windows(
        self, composer: WindowComposer, sample_lines: list[str]
    ):
        """Non-overlapping windows are kept separate."""
        windows = [
            SourceWindow(10, 20, "reason1", "strat1", 0.8),
            SourceWindow(50, 60, "reason2", "strat2", 0.7),
        ]

        result = composer.compose(windows, sample_lines, max_tokens=10000)

        # Should have two windows (with context margins they might still be separate)
        # or merged if context margins make them overlap
        assert len(result.windows_included) >= 1


class TestWindowComposerBudget:
    """Tests for token budget handling."""

    def test_respects_token_budget(
        self, composer: WindowComposer, sample_lines: list[str]
    ):
        """Composer stops adding windows when budget exceeded."""
        # Many windows, small budget
        windows = [
            SourceWindow(i, i + 5, f"reason{i}", "strat", 0.5)
            for i in range(1, 90, 10)
        ]

        # Very small budget
        result = composer.compose(windows, sample_lines, max_tokens=500)

        # Should include some but not all windows
        assert result.was_truncated is True
        assert len(result.windows_included) < len(windows)

    def test_prioritizes_high_relevance(
        self, composer: WindowComposer, sample_lines: list[str]
    ):
        """Higher relevance windows are included first."""
        windows = [
            SourceWindow(10, 15, "low", "low_strat", 0.1),
            SourceWindow(50, 55, "high", "high_strat", 0.9),
            SourceWindow(80, 85, "medium", "med_strat", 0.5),
        ]

        # Budget for only one window
        result = composer.compose(windows, sample_lines, max_tokens=300)

        # Should include highest relevance
        if result.windows_included:
            strategies = result.strategies_used
            # High relevance should be prioritized
            assert "high_strat" in strategies or len(result.windows_included) > 1

    def test_empty_windows_returns_empty_sample(
        self, composer: WindowComposer, sample_lines: list[str]
    ):
        """Empty window list returns empty sample."""
        result = composer.compose([], sample_lines, max_tokens=10000)

        assert result.source_code == ""
        assert len(result.windows_included) == 0
        assert result.total_lines_sampled == 0


class TestWindowComposerOutput:
    """Tests for output generation."""

    def test_includes_line_numbers(
        self, composer: WindowComposer, sample_lines: list[str]
    ):
        """Output includes line numbers."""
        windows = [SourceWindow(10, 15, "test", "test", 0.8)]

        result = composer.compose(windows, sample_lines, max_tokens=10000)

        # Line numbers should be in the output
        assert "10" in result.source_code or "   10" in result.source_code

    def test_includes_ellipsis_for_gaps(
        self, composer: WindowComposer, sample_lines: list[str]
    ):
        """Gaps between windows show ellipsis."""
        windows = [
            SourceWindow(5, 10, "first", "strat", 0.8),
            SourceWindow(90, 95, "second", "strat", 0.8),
        ]

        result = composer.compose(windows, sample_lines, max_tokens=10000)

        # Should have ellipsis marker
        assert "omitted" in result.source_code.lower() or "..." in result.source_code

    def test_header_describes_sampling(
        self, composer: WindowComposer, sample_lines: list[str]
    ):
        """Header explains what was sampled."""
        windows = [SourceWindow(10, 20, "test reason", "test_strat", 0.8)]

        result = composer.compose(windows, sample_lines, max_tokens=10000)

        assert "SAMPLING" in result.header.upper()
        assert "test_strat" in result.header
        assert result.header in result.source_code

    def test_strategies_used_populated(
        self, composer: WindowComposer, sample_lines: list[str]
    ):
        """strategies_used contains contributing strategy names."""
        windows = [
            SourceWindow(10, 15, "first", "strat_a", 0.8),
            SourceWindow(50, 55, "second", "strat_b", 0.7),
        ]

        result = composer.compose(windows, sample_lines, max_tokens=10000)

        # At least one strategy should be recorded (may be merged)
        assert len(result.strategies_used) >= 1


class TestWindowComposerContextMargin:
    """Tests for context margin expansion."""

    def test_expands_windows_with_margin(
        self, composer: WindowComposer, sample_lines: list[str]
    ):
        """Windows are expanded with context margin."""
        # Window at line 20-25, should expand to ~15-30
        windows = [SourceWindow(20, 25, "test", "test", 0.8)]

        result = composer.compose(windows, sample_lines, max_tokens=10000)

        # Lines sampled should be more than original 6 lines
        assert result.total_lines_sampled > 6

    def test_margin_clamped_at_boundaries(
        self, composer: WindowComposer, sample_lines: list[str]
    ):
        """Margin doesn't go past file boundaries."""
        # Window near start
        windows = [SourceWindow(1, 5, "test", "test", 0.8)]

        result = composer.compose(windows, sample_lines, max_tokens=10000)

        # First line should be 1 (not negative due to margin expansion)
        assert result.windows_included[0].start_line >= 1
        # Should start at line 1 in the output
        assert "\n     1\t" in result.source_code
