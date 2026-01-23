"""Tests for sampling strategies.

Tests each strategy's can_apply() and generate_windows() methods.
"""

import pytest
from unittest.mock import MagicMock

from war_rig.beads import TicketType
from war_rig.models.templates import FileType
from war_rig.sampling.analyzer import RelevanceHints
from war_rig.sampling.context import SamplingContext
from war_rig.sampling.strategies import (
    LineCitationStrategy,
    SectionReferenceStrategy,
    IdentifierMentionStrategy,
    SemanticRegionStrategy,
    RandomFallbackStrategy,
    SourceWindow,
)


@pytest.fixture
def sample_cobol_source() -> str:
    """Sample COBOL source code with divisions."""
    return """       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CUSTOMER-RECORD.
          05 WS-CUSTOMER-ID    PIC X(10).
          05 WS-CUSTOMER-NAME  PIC X(50).
       01 WS-ACCOUNT-DATA.
          05 WS-ACCOUNT-ID     PIC X(10).
          05 WS-BALANCE        PIC 9(10)V99.

       PROCEDURE DIVISION.
       MAIN-PROCESS.
           PERFORM INITIALIZE-DATA.
           PERFORM PROCESS-ACCOUNT.
           STOP RUN.

       INITIALIZE-DATA.
           MOVE SPACES TO WS-CUSTOMER-RECORD.
           MOVE ZEROS TO WS-BALANCE.

       PROCESS-ACCOUNT.
           IF WS-BALANCE > 0
              DISPLAY "HAS BALANCE"
           END-IF.
"""


@pytest.fixture
def cobol_context(sample_cobol_source: str) -> SamplingContext:
    """Create a SamplingContext with COBOL source."""
    return SamplingContext.create(
        source_code=sample_cobol_source,
        file_name="TESTPROG.cbl",
        file_type=FileType.COBOL,
        max_tokens=5000,
        estimated_source_tokens=10000,
        ticket_type=TicketType.CLARIFICATION,
    )


class TestSourceWindow:
    """Tests for SourceWindow dataclass."""

    def test_line_count(self):
        """line_count returns inclusive count."""
        window = SourceWindow(
            start_line=10,
            end_line=20,
            reason="test",
            strategy_name="test",
            relevance_score=0.5,
        )
        assert window.line_count == 11

    def test_overlaps_adjacent(self):
        """Adjacent windows are considered overlapping for merging."""
        w1 = SourceWindow(10, 20, "test", "test", 0.5)
        w2 = SourceWindow(21, 30, "test", "test", 0.5)
        assert w1.overlaps(w2) is True

    def test_overlaps_true(self):
        """Overlapping windows return True."""
        w1 = SourceWindow(10, 25, "test", "test", 0.5)
        w2 = SourceWindow(20, 30, "test", "test", 0.5)
        assert w1.overlaps(w2) is True

    def test_overlaps_false(self):
        """Non-overlapping windows return False."""
        w1 = SourceWindow(10, 20, "test", "test", 0.5)
        w2 = SourceWindow(30, 40, "test", "test", 0.5)
        assert w1.overlaps(w2) is False

    def test_merge(self):
        """Merged window spans both inputs."""
        w1 = SourceWindow(10, 25, "reason1", "strat1", 0.8)
        w2 = SourceWindow(20, 35, "reason2", "strat2", 0.6)
        merged = w1.merge(w2)

        assert merged.start_line == 10
        assert merged.end_line == 35
        assert merged.relevance_score == 0.8  # Takes higher score
        assert merged.strategy_name == "strat1"  # Takes higher scorer's strategy

    def test_clamp(self):
        """Clamping restricts to valid bounds."""
        # Create a valid window first
        window = SourceWindow(1, 150, "test", "test", 0.5)
        clamped = window.clamp(max_line=100)

        assert clamped.start_line == 1
        assert clamped.end_line == 100

    def test_invalid_relevance_score(self):
        """Relevance score must be 0-1."""
        with pytest.raises(ValueError):
            SourceWindow(1, 10, "test", "test", 1.5)

    def test_invalid_line_bounds(self):
        """end_line must be >= start_line."""
        with pytest.raises(ValueError):
            SourceWindow(20, 10, "test", "test", 0.5)


class TestLineCitationStrategy:
    """Tests for LineCitationStrategy."""

    def test_properties(self):
        """Strategy has correct name and priority."""
        strategy = LineCitationStrategy()
        assert strategy.name == "line_citation"
        assert strategy.priority == 1

    def test_can_apply_with_line_numbers(self, cobol_context: SamplingContext):
        """can_apply returns True when line numbers present."""
        strategy = LineCitationStrategy()
        hints = RelevanceHints(line_numbers=[10, 20])
        assert strategy.can_apply(hints, cobol_context) is True

    def test_can_apply_with_line_ranges(self, cobol_context: SamplingContext):
        """can_apply returns True when line ranges present."""
        strategy = LineCitationStrategy()
        hints = RelevanceHints(line_ranges=[(10, 20)])
        assert strategy.can_apply(hints, cobol_context) is True

    def test_can_apply_false_when_empty(self, cobol_context: SamplingContext):
        """can_apply returns False when no lines."""
        strategy = LineCitationStrategy()
        hints = RelevanceHints()
        assert strategy.can_apply(hints, cobol_context) is False

    def test_generate_windows_for_line_numbers(self, cobol_context: SamplingContext):
        """Windows are created around each cited line."""
        strategy = LineCitationStrategy()
        hints = RelevanceHints(line_numbers=[15])

        windows = strategy.generate_windows(hints, cobol_context)

        assert len(windows) == 1
        assert windows[0].start_line == 5  # 15 - 10
        assert windows[0].end_line == 25  # 15 + 10
        assert windows[0].relevance_score == 1.0

    def test_generate_windows_clamped(self, cobol_context: SamplingContext):
        """Windows near boundaries are clamped."""
        strategy = LineCitationStrategy()
        hints = RelevanceHints(line_numbers=[5])  # Near start

        windows = strategy.generate_windows(hints, cobol_context)

        assert windows[0].start_line == 1  # Clamped to 1


class TestSectionReferenceStrategy:
    """Tests for SectionReferenceStrategy."""

    def test_properties(self):
        """Strategy has correct name and priority."""
        strategy = SectionReferenceStrategy()
        assert strategy.name == "section_reference"
        assert strategy.priority == 2

    def test_can_apply_with_section_citations(self, cobol_context: SamplingContext):
        """can_apply returns True when section citations present."""
        strategy = SectionReferenceStrategy()
        hints = RelevanceHints(section_citations={"inputs": [10, 20]})
        assert strategy.can_apply(hints, cobol_context) is True

    def test_can_apply_false_when_empty(self, cobol_context: SamplingContext):
        """can_apply returns False when no section citations."""
        strategy = SectionReferenceStrategy()
        hints = RelevanceHints(section_names=["inputs"])  # Names but no citations
        assert strategy.can_apply(hints, cobol_context) is False

    def test_generate_windows_for_citations(self, cobol_context: SamplingContext):
        """Windows are created for each section citation line."""
        strategy = SectionReferenceStrategy()
        hints = RelevanceHints(section_citations={"inputs": [10, 20]})

        windows = strategy.generate_windows(hints, cobol_context)

        assert len(windows) == 2
        assert windows[0].relevance_score == 0.9


class TestIdentifierMentionStrategy:
    """Tests for IdentifierMentionStrategy."""

    def test_properties(self):
        """Strategy has correct name and priority."""
        strategy = IdentifierMentionStrategy()
        assert strategy.name == "identifier_mention"
        assert strategy.priority == 3

    def test_can_apply_with_identifiers(self, cobol_context: SamplingContext):
        """can_apply returns True when identifiers present."""
        strategy = IdentifierMentionStrategy()
        hints = RelevanceHints(identifiers=["WS-CUSTOMER-ID"])
        assert strategy.can_apply(hints, cobol_context) is True

    def test_can_apply_false_when_empty(self, cobol_context: SamplingContext):
        """can_apply returns False when no identifiers."""
        strategy = IdentifierMentionStrategy()
        hints = RelevanceHints()
        assert strategy.can_apply(hints, cobol_context) is False

    def test_generate_windows_finds_identifier(self, cobol_context: SamplingContext):
        """Windows are created where identifier is found."""
        strategy = IdentifierMentionStrategy()
        hints = RelevanceHints(identifiers=["WS-CUSTOMER-ID"])

        windows = strategy.generate_windows(hints, cobol_context)

        # Should find WS-CUSTOMER-ID in the source
        assert len(windows) >= 1
        assert windows[0].relevance_score == 0.7

    def test_generate_windows_not_found(self, cobol_context: SamplingContext):
        """No windows when identifier not in source."""
        strategy = IdentifierMentionStrategy()
        hints = RelevanceHints(identifiers=["NONEXISTENT-FIELD"])

        windows = strategy.generate_windows(hints, cobol_context)

        assert len(windows) == 0


class TestSemanticRegionStrategy:
    """Tests for SemanticRegionStrategy."""

    def test_properties(self):
        """Strategy has correct name and priority."""
        strategy = SemanticRegionStrategy()
        assert strategy.name == "semantic_region"
        assert strategy.priority == 4

    def test_can_apply_with_sections(self, cobol_context: SamplingContext):
        """can_apply returns True for COBOL with section names."""
        strategy = SemanticRegionStrategy()
        hints = RelevanceHints(section_names=["inputs"])
        assert strategy.can_apply(hints, cobol_context) is True

    def test_can_apply_false_non_cobol(self):
        """can_apply returns False for non-COBOL files without divisions."""
        strategy = SemanticRegionStrategy()
        context = SamplingContext.create(
            source_code="regular code without divisions",
            file_name="test.py",
            file_type=FileType.OTHER,
            max_tokens=5000,
            estimated_source_tokens=10000,
            ticket_type=TicketType.CLARIFICATION,
        )
        hints = RelevanceHints(section_names=["inputs"])
        assert strategy.can_apply(hints, context) is False

    def test_generate_windows_for_data_division(self, cobol_context: SamplingContext):
        """inputs section maps to DATA DIVISION."""
        strategy = SemanticRegionStrategy()
        hints = RelevanceHints(section_names=["inputs"])

        windows = strategy.generate_windows(hints, cobol_context)

        # Should create window for DATA DIVISION
        assert len(windows) >= 1
        assert any("DATA DIVISION" in w.reason for w in windows)
        assert windows[0].relevance_score == 0.6

    def test_generate_windows_for_procedure_division(self, cobol_context: SamplingContext):
        """business_rules section maps to PROCEDURE DIVISION."""
        strategy = SemanticRegionStrategy()
        hints = RelevanceHints(section_names=["business_rules"])

        windows = strategy.generate_windows(hints, cobol_context)

        # Should create window for PROCEDURE DIVISION
        assert len(windows) >= 1
        assert any("PROCEDURE DIVISION" in w.reason for w in windows)


class TestRandomFallbackStrategy:
    """Tests for RandomFallbackStrategy."""

    def test_properties(self):
        """Strategy has correct name and priority."""
        strategy = RandomFallbackStrategy()
        assert strategy.name == "random_fallback"
        assert strategy.priority == 99

    def test_can_apply_when_no_hints(self, cobol_context: SamplingContext):
        """can_apply returns True when no explicit hints."""
        strategy = RandomFallbackStrategy()
        hints = RelevanceHints()
        assert strategy.can_apply(hints, cobol_context) is True

    def test_can_apply_false_when_hints_exist(self, cobol_context: SamplingContext):
        """can_apply returns False when hints exist."""
        strategy = RandomFallbackStrategy()
        hints = RelevanceHints(line_numbers=[10])
        assert strategy.can_apply(hints, cobol_context) is False

    def test_generate_windows_creates_single_window(self, cobol_context: SamplingContext):
        """Generates a single window for random sample."""
        strategy = RandomFallbackStrategy()
        hints = RelevanceHints()

        windows = strategy.generate_windows(hints, cobol_context)

        assert len(windows) == 1
        assert windows[0].relevance_score == 0.1
        assert windows[0].start_line >= 1
        assert windows[0].end_line <= cobol_context.total_lines
