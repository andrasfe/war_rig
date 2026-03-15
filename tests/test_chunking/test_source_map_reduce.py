"""Tests for SourceMapReducer — map-reduce for oversized paragraphs."""

from __future__ import annotations

from unittest.mock import AsyncMock, MagicMock

import pytest

from war_rig.chunking.source_map_reduce import (
    MapReduceResult,
    SourceMapReducer,
    WindowSummary,
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


def _make_cobol_source(num_lines: int) -> str:
    """Generate synthetic COBOL source with ~72 chars/line."""
    lines = []
    for i in range(1, num_lines + 1):
        # Pad to ~72 chars to match real COBOL fixed-width lines
        base = f"      {'*' if i % 50 == 0 else ' '} LINE-{i:05d} MOVE WS-FIELD-A TO WS-OUTPUT-RECORD-B"
        lines.append(base.ljust(72))
    return "\n".join(lines)


def _mock_provider(summary_text: str = "Mock summary.") -> MagicMock:
    """Create a mock LLM provider that returns a fixed summary."""
    provider = MagicMock()
    response = MagicMock()
    response.content = summary_text
    response.tokens_used = 50
    provider.complete = AsyncMock(return_value=response)
    return provider


@pytest.fixture
def reducer() -> SourceMapReducer:
    """SourceMapReducer with max_prompt_tokens=15000 and mock provider."""
    return SourceMapReducer(
        provider=_mock_provider(),
        model="test-model",
        max_prompt_tokens=15000,
    )


@pytest.fixture
def large_reducer() -> SourceMapReducer:
    """SourceMapReducer with max_prompt_tokens=65000 (large context)."""
    return SourceMapReducer(
        provider=_mock_provider(),
        model="test-model",
        max_prompt_tokens=65000,
    )


# ---------------------------------------------------------------------------
# needs_map_reduce
# ---------------------------------------------------------------------------


class TestNeedsMapReduce:
    def test_small_source_does_not_trigger(self, reducer: SourceMapReducer):
        source = _make_cobol_source(50)
        assert not reducer.needs_map_reduce(source)

    def test_large_source_triggers(self, reducer: SourceMapReducer):
        # Threshold = 15000 - 6000 = 9000 tokens
        # At 3.5 chars/token, that's ~31.5K chars => ~437 lines of 72 chars
        source = _make_cobol_source(600)
        assert reducer.needs_map_reduce(source)

    def test_threshold_scales_with_max_prompt_tokens(
        self, large_reducer: SourceMapReducer,
    ):
        # Threshold = 65000 - 6000 = 59000 tokens => ~206.5K chars => ~2868 lines
        source_small = _make_cobol_source(2000)
        source_large = _make_cobol_source(5000)
        assert not large_reducer.needs_map_reduce(source_small)
        assert large_reducer.needs_map_reduce(source_large)


# ---------------------------------------------------------------------------
# _compute_window_params
# ---------------------------------------------------------------------------


class TestComputeWindowParams:
    def test_returns_reasonable_values(self, reducer: SourceMapReducer):
        window_lines, overlap = reducer._compute_window_params()
        assert window_lines >= 100
        assert overlap >= 10
        assert overlap < window_lines

    def test_scales_with_budget(self):
        small = SourceMapReducer(
            provider=_mock_provider(), model="m", max_prompt_tokens=10000,
        )
        large = SourceMapReducer(
            provider=_mock_provider(), model="m", max_prompt_tokens=100000,
        )
        small_w, _ = small._compute_window_params()
        large_w, _ = large._compute_window_params()
        assert large_w > small_w


# ---------------------------------------------------------------------------
# _split_windows
# ---------------------------------------------------------------------------


class TestSplitWindows:
    def test_covers_all_lines(self, reducer: SourceMapReducer):
        lines = [f"line {i}" for i in range(2000)]
        windows = reducer._split_windows(lines)
        # First window starts at 0
        assert windows[0][0] == 0
        # Last window reaches the end
        assert windows[-1][1] == len(lines)

    def test_single_window_for_small_source(self, large_reducer: SourceMapReducer):
        lines = [f"line {i}" for i in range(50)]
        windows = large_reducer._split_windows(lines)
        assert len(windows) == 1
        assert windows[0] == (0, 50)

    def test_windows_overlap(self, reducer: SourceMapReducer):
        lines = [f"line {i}" for i in range(2000)]
        windows = reducer._split_windows(lines)
        if len(windows) > 1:
            # Second window should start before first window ends
            assert windows[1][0] < windows[0][1]


# ---------------------------------------------------------------------------
# _select_representative_excerpt
# ---------------------------------------------------------------------------


class TestSelectRepresentativeExcerpt:
    def test_respects_token_budget(self, reducer: SourceMapReducer):
        lines = [f"      MOVE FIELD-{i:04d} TO OUTPUT." for i in range(500)]
        excerpt = reducer._select_representative_excerpt(lines, max_tokens=500)
        # Should be much shorter than the full 500 lines
        assert excerpt.count("\n") < 400

    def test_includes_omitted_notice(self, reducer: SourceMapReducer):
        lines = [f"      MOVE A TO B." for _ in range(500)]
        excerpt = reducer._select_representative_excerpt(lines, max_tokens=100)
        assert "more lines omitted" in excerpt

    def test_no_omitted_notice_when_all_fit(self, reducer: SourceMapReducer):
        lines = ["      MOVE A TO B."] * 3
        excerpt = reducer._select_representative_excerpt(lines, max_tokens=10000)
        assert "omitted" not in excerpt


# ---------------------------------------------------------------------------
# _assemble_reduced_source
# ---------------------------------------------------------------------------


class TestAssembleReducedSource:
    def test_structure(self):
        summaries = [
            WindowSummary(
                window_id=1, line_start=1, line_end=500,
                total_lines=1000, summary="Window 1 summary.",
            ),
            WindowSummary(
                window_id=2, line_start=450, line_end=1000,
                total_lines=1000, summary="Window 2 summary.",
            ),
        ]
        result = SourceMapReducer._assemble_reduced_source(
            summaries, "MOVE A TO B.", "9000-MAIN", 1000,
        )
        assert "9000-MAIN" in result
        assert "1000 lines" in result
        assert "Window 1 of 2" in result
        assert "Window 2 of 2" in result
        assert "Window 1 summary." in result
        assert "```cobol" in result
        assert "MOVE A TO B." in result


# ---------------------------------------------------------------------------
# reduce (integration)
# ---------------------------------------------------------------------------


class TestReduce:
    async def test_end_to_end(self):
        provider = _mock_provider("Structured summary of this window.")
        reducer = SourceMapReducer(
            provider=provider,
            model="test-model",
            max_prompt_tokens=15000,
        )
        source = _make_cobol_source(2000)
        result = await reducer.reduce(source, paragraph_name="9000-BIG-PARA")

        assert isinstance(result, MapReduceResult)
        assert result.original_lines == 2000
        assert result.num_windows > 1
        assert len(result.window_summaries) == result.num_windows
        assert result.tokens_saved > 0
        assert "9000-BIG-PARA" in result.reduced_source
        assert "Structured summary" in result.reduced_source

    async def test_provider_failure_uses_fallback(self):
        provider = MagicMock()
        provider.complete = AsyncMock(side_effect=RuntimeError("API error"))
        reducer = SourceMapReducer(
            provider=provider,
            model="test-model",
            max_prompt_tokens=15000,
        )
        source = _make_cobol_source(2000)
        result = await reducer.reduce(source, paragraph_name="FAIL-PARA")

        # Should still produce a result with fallback summaries
        assert result.num_windows > 0
        for ws in result.window_summaries:
            assert "fallback" in ws.summary.lower()

    async def test_window_count_scales_with_source_size(self):
        provider = _mock_provider("Summary.")
        small_reducer = SourceMapReducer(
            provider=provider, model="m", max_prompt_tokens=15000,
        )
        source_2k = _make_cobol_source(2000)
        source_8k = _make_cobol_source(8500)

        result_2k = await small_reducer.reduce(source_2k)
        result_8k = await small_reducer.reduce(source_8k)

        assert result_8k.num_windows > result_2k.num_windows
