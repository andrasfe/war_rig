"""Tests for SourceCodePreparer token limit handling.

These tests verify the token limit enforcement logic for Scribe processing,
ensuring the correct strategy (passthrough, sampling, chunking) is selected
based on source size and processing context.
"""

from unittest.mock import MagicMock

import pytest

from war_rig.beads import TicketType
from war_rig.models.templates import FileType
from war_rig.workers.source_preparer import (
    PreparationContext,
    PreparedSource,
    SourceCodePreparer,
)


@pytest.fixture
def mock_scribe_config():
    """Create a mock ScribeConfig with known token limits."""
    config = MagicMock()
    config.max_prompt_tokens = 15000  # Standard limit
    return config


@pytest.fixture
def preparer(mock_scribe_config):
    """Create a SourceCodePreparer with mock config."""
    return SourceCodePreparer(config=mock_scribe_config)


@pytest.fixture
def small_source():
    """Source code that fits within token limit (~1000 tokens)."""
    return "\n".join([f"       MOVE FIELD-{i} TO OUTPUT-{i}." for i in range(100)])


@pytest.fixture
def large_source():
    """Source code that exceeds token limit (~50000 tokens)."""
    # Create a large COBOL-like source
    lines = ["       IDENTIFICATION DIVISION."]
    lines.extend([f"       MOVE FIELD-{i} TO OUTPUT-{i}." for i in range(5000)])
    return "\n".join(lines)


class TestPreparationContext:
    """Tests for PreparationContext dataclass."""

    def test_is_initial_documentation_cycle_1_no_template(self):
        """Cycle 1 DOCUMENTATION without template is initial documentation."""
        context = PreparationContext(
            ticket_type=TicketType.DOCUMENTATION,
            cycle_number=1,
            has_previous_template=False,
        )
        assert context.is_initial_documentation is True
        assert context.is_update_scenario is False

    def test_is_not_initial_documentation_cycle_2(self):
        """Cycle 2 DOCUMENTATION is not initial documentation."""
        context = PreparationContext(
            ticket_type=TicketType.DOCUMENTATION,
            cycle_number=2,
            has_previous_template=False,
        )
        assert context.is_initial_documentation is False
        assert context.is_update_scenario is True

    def test_is_not_initial_documentation_with_template(self):
        """Cycle 1 with existing template is not initial documentation."""
        context = PreparationContext(
            ticket_type=TicketType.DOCUMENTATION,
            cycle_number=1,
            has_previous_template=True,
        )
        assert context.is_initial_documentation is False
        assert context.is_update_scenario is True

    def test_clarification_is_update_scenario(self):
        """CLARIFICATION tickets are always update scenarios."""
        context = PreparationContext(
            ticket_type=TicketType.CLARIFICATION,
            cycle_number=1,
            has_previous_template=True,
        )
        assert context.is_initial_documentation is False
        assert context.is_update_scenario is True

    def test_chrome_is_update_scenario(self):
        """CHROME tickets are always update scenarios."""
        context = PreparationContext(
            ticket_type=TicketType.CHROME,
            cycle_number=1,
            has_previous_template=True,
        )
        assert context.is_initial_documentation is False
        assert context.is_update_scenario is True


class TestPreparedSource:
    """Tests for PreparedSource dataclass."""

    def test_needs_chunked_processing_when_chunked(self):
        """needs_chunked_processing is True when chunked with chunks."""
        prepared = PreparedSource(
            source_code="original",
            strategy_used="chunking",
            was_modified=False,
            chunks=[MagicMock()],  # Non-empty chunks list
        )
        assert prepared.needs_chunked_processing is True

    def test_needs_chunked_processing_false_for_sampling(self):
        """needs_chunked_processing is False for sampling."""
        prepared = PreparedSource(
            source_code="sampled",
            strategy_used="sampling",
            was_modified=True,
        )
        assert prepared.needs_chunked_processing is False

    def test_needs_chunked_processing_false_for_passthrough(self):
        """needs_chunked_processing is False for passthrough."""
        prepared = PreparedSource(
            source_code="original",
            strategy_used="passthrough",
            was_modified=False,
        )
        assert prepared.needs_chunked_processing is False


class TestSourceCodePreparer:
    """Tests for SourceCodePreparer class."""

    def test_max_source_tokens(self, preparer):
        """max_source_tokens subtracts overhead from config limit."""
        # 15000 - 4000 (overhead) = 11000
        assert preparer.max_source_tokens == 11000

    def test_needs_preparation_small_source(self, preparer, small_source):
        """Small source does not need preparation."""
        assert preparer.needs_preparation(small_source) is False

    def test_needs_preparation_large_source(self, preparer, large_source):
        """Large source needs preparation."""
        assert preparer.needs_preparation(large_source) is True


class TestPassthroughStrategy:
    """Tests for passthrough strategy (source fits within limit)."""

    def test_passthrough_when_under_limit(self, preparer, small_source):
        """Source under token limit returns unchanged."""
        context = PreparationContext(
            ticket_type=TicketType.DOCUMENTATION,
            cycle_number=1,
            has_previous_template=False,
        )

        result = preparer.prepare(small_source, context)

        assert result.strategy_used == "passthrough"
        assert result.was_modified is False
        assert result.source_code == small_source
        assert result.chunks is None
        assert result.needs_chunked_processing is False

    def test_passthrough_preserves_all_content(self, preparer):
        """Passthrough preserves source exactly."""
        source = "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. TEST."
        context = PreparationContext(
            ticket_type=TicketType.CLARIFICATION,
            cycle_number=3,
            has_previous_template=True,
        )

        result = preparer.prepare(source, context)

        assert result.source_code == source


class TestSamplingStrategy:
    """Tests for sampling strategy (updates with large source)."""

    def test_sampling_for_clarification_ticket(self, preparer, large_source):
        """CLARIFICATION tickets use sampling strategy."""
        context = PreparationContext(
            ticket_type=TicketType.CLARIFICATION,
            cycle_number=1,
            has_previous_template=True,
        )

        result = preparer.prepare(large_source, context)

        assert result.strategy_used == "sampling"
        assert result.was_modified is True
        assert result.chunks is None
        assert result.needs_chunked_processing is False

    def test_sampling_for_chrome_ticket(self, preparer, large_source):
        """CHROME tickets use sampling strategy."""
        context = PreparationContext(
            ticket_type=TicketType.CHROME,
            cycle_number=1,
            has_previous_template=True,
        )

        result = preparer.prepare(large_source, context)

        assert result.strategy_used == "sampling"

    def test_sampling_for_cycle_gt_1_documentation(self, preparer, large_source):
        """Cycle > 1 DOCUMENTATION uses sampling."""
        context = PreparationContext(
            ticket_type=TicketType.DOCUMENTATION,
            cycle_number=2,
            has_previous_template=True,
        )

        result = preparer.prepare(large_source, context)

        assert result.strategy_used == "sampling"

    def test_sampling_adds_header(self, preparer, large_source):
        """Sampled source includes informative header."""
        context = PreparationContext(
            ticket_type=TicketType.CLARIFICATION,
            cycle_number=1,
            has_previous_template=True,
        )

        result = preparer.prepare(large_source, context)

        assert "NOTE: Source code sampled" in result.source_code
        assert "exceeds token limit" in result.source_code

    def test_sampling_includes_metadata(self, preparer, large_source):
        """Sampling metadata includes line range info."""
        context = PreparationContext(
            ticket_type=TicketType.CLARIFICATION,
            cycle_number=1,
            has_previous_template=True,
        )

        result = preparer.prepare(large_source, context)

        assert "original_lines" in result.metadata
        assert "sampled_lines" in result.metadata
        assert "start_line" in result.metadata
        assert "original_tokens" in result.metadata

    def test_sampling_reduces_size(self, preparer, large_source):
        """Sampled source is smaller than original."""
        context = PreparationContext(
            ticket_type=TicketType.CLARIFICATION,
            cycle_number=1,
            has_previous_template=True,
        )

        result = preparer.prepare(large_source, context)

        # Sampled should be significantly smaller
        assert len(result.source_code) < len(large_source)


class TestChunkingStrategy:
    """Tests for chunking strategy (initial documentation of large files)."""

    def test_chunking_for_cycle_1_documentation(self, preparer, large_source):
        """Cycle 1 DOCUMENTATION uses chunking for large files."""
        context = PreparationContext(
            ticket_type=TicketType.DOCUMENTATION,
            cycle_number=1,
            has_previous_template=False,
            file_type=FileType.COBOL,
        )

        result = preparer.prepare(large_source, context)

        assert result.strategy_used == "chunking"
        assert result.was_modified is False
        assert result.chunks is not None
        assert len(result.chunks) > 0
        assert result.needs_chunked_processing is True

    def test_chunking_preserves_original(self, preparer, large_source):
        """Chunking returns original source with chunks list."""
        context = PreparationContext(
            ticket_type=TicketType.DOCUMENTATION,
            cycle_number=1,
            has_previous_template=False,
            file_type=FileType.COBOL,
        )

        result = preparer.prepare(large_source, context)

        # Original source preserved (for reference if needed)
        assert result.source_code == large_source

    def test_chunking_metadata_includes_count(self, preparer, large_source):
        """Chunking metadata includes chunk count and strategy."""
        context = PreparationContext(
            ticket_type=TicketType.DOCUMENTATION,
            cycle_number=1,
            has_previous_template=False,
            file_type=FileType.COBOL,
        )

        result = preparer.prepare(large_source, context)

        assert "chunk_count" in result.metadata
        assert "chunking_strategy" in result.metadata

    def test_chunking_result_preserved(self, preparer, large_source):
        """Full ChunkingResult is available in result."""
        context = PreparationContext(
            ticket_type=TicketType.DOCUMENTATION,
            cycle_number=1,
            has_previous_template=False,
            file_type=FileType.COBOL,
        )

        result = preparer.prepare(large_source, context)

        assert result.chunking_result is not None
        assert result.chunking_result.chunk_count == len(result.chunks)

    def test_generic_chunker_for_non_cobol(self, preparer, large_source):
        """Non-COBOL files use GenericChunker."""
        context = PreparationContext(
            ticket_type=TicketType.DOCUMENTATION,
            cycle_number=1,
            has_previous_template=False,
            file_type=FileType.OTHER,
        )

        result = preparer.prepare(large_source, context)

        assert result.strategy_used == "chunking"
        # GenericChunker will still produce chunks
        assert result.chunks is not None


class TestEdgeCases:
    """Tests for edge cases and boundary conditions."""

    def test_empty_source(self, preparer):
        """Empty source returns passthrough."""
        context = PreparationContext(
            ticket_type=TicketType.DOCUMENTATION,
            cycle_number=1,
            has_previous_template=False,
        )

        result = preparer.prepare("", context)

        assert result.strategy_used == "passthrough"
        assert result.source_code == ""

    def test_exactly_at_limit(self, mock_scribe_config):
        """Source exactly at limit uses passthrough."""
        # Create source that's exactly at the limit
        # This is tricky to test precisely, so we'll mock the estimator
        mock_estimator = MagicMock()
        mock_estimator.estimate_source_tokens.return_value = 11000  # Exactly at limit

        preparer = SourceCodePreparer(
            config=mock_scribe_config,
            estimator=mock_estimator,
        )
        context = PreparationContext(
            ticket_type=TicketType.DOCUMENTATION,
            cycle_number=1,
            has_previous_template=False,
        )

        result = preparer.prepare("source", context)

        assert result.strategy_used == "passthrough"

    def test_one_token_over_limit(self, mock_scribe_config):
        """Source one token over limit triggers preparation."""
        mock_estimator = MagicMock()
        mock_estimator.estimate_source_tokens.return_value = 11001  # One over

        preparer = SourceCodePreparer(
            config=mock_scribe_config,
            estimator=mock_estimator,
        )

        assert preparer.needs_preparation("source") is True

    def test_sampling_with_few_lines(self, mock_scribe_config):
        """Sampling with very few lines still produces valid output."""
        mock_estimator = MagicMock()
        mock_estimator.estimate_source_tokens.return_value = 20000  # Over limit

        preparer = SourceCodePreparer(
            config=mock_scribe_config,
            estimator=mock_estimator,
        )
        context = PreparationContext(
            ticket_type=TicketType.CLARIFICATION,
            cycle_number=1,
            has_previous_template=True,
        )

        # Source with only 5 lines
        source = "line1\nline2\nline3\nline4\nline5"
        result = preparer.prepare(source, context)

        assert result.strategy_used == "sampling"
        # Should sample at least 10 lines (minimum) or all available
        assert result.metadata["sampled_lines"] >= min(10, 5)


class TestStrategySelection:
    """Tests verifying correct strategy selection logic."""

    @pytest.mark.parametrize(
        "ticket_type,cycle_number,has_template,expected_strategy",
        [
            # Cycle 1 DOCUMENTATION without template -> chunking
            (TicketType.DOCUMENTATION, 1, False, "chunking"),
            # Cycle 1 DOCUMENTATION with template -> sampling
            (TicketType.DOCUMENTATION, 1, True, "sampling"),
            # Cycle 2+ DOCUMENTATION -> sampling
            (TicketType.DOCUMENTATION, 2, False, "sampling"),
            (TicketType.DOCUMENTATION, 3, True, "sampling"),
            # CLARIFICATION -> sampling
            (TicketType.CLARIFICATION, 1, True, "sampling"),
            (TicketType.CLARIFICATION, 2, True, "sampling"),
            # CHROME -> sampling
            (TicketType.CHROME, 1, True, "sampling"),
            (TicketType.CHROME, 2, True, "sampling"),
        ],
    )
    def test_strategy_selection(
        self,
        preparer,
        large_source,
        ticket_type,
        cycle_number,
        has_template,
        expected_strategy,
    ):
        """Verify correct strategy is selected for each scenario."""
        context = PreparationContext(
            ticket_type=ticket_type,
            cycle_number=cycle_number,
            has_previous_template=has_template,
            file_type=FileType.COBOL,
        )

        result = preparer.prepare(large_source, context)

        assert result.strategy_used == expected_strategy, (
            f"Expected {expected_strategy} for {ticket_type.value} "
            f"cycle {cycle_number} template={has_template}, "
            f"got {result.strategy_used}"
        )
