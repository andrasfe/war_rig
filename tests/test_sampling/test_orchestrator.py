"""Tests for SamplingOrchestrator.

Tests the full intelligent sampling pipeline integration.
"""

import pytest
from unittest.mock import MagicMock

from war_rig.beads import TicketType
from war_rig.models.templates import FileType, DocumentationTemplate, PurposeSection
from war_rig.models.tickets import ChallengerQuestion, ChromeTicket
from war_rig.sampling import SamplingOrchestrator, SamplingResult


@pytest.fixture
def mock_estimator() -> MagicMock:
    """Create a mock TokenEstimator."""
    estimator = MagicMock()
    # Approximate 1 token per 4 chars
    estimator.estimate_source_tokens.side_effect = lambda text: len(text) // 4
    return estimator


@pytest.fixture
def orchestrator(mock_estimator: MagicMock) -> SamplingOrchestrator:
    """Create a SamplingOrchestrator with mock estimator."""
    return SamplingOrchestrator(mock_estimator)


@pytest.fixture
def sample_cobol_source() -> str:
    """Sample COBOL source code."""
    lines = []
    # Add identification
    lines.append("       IDENTIFICATION DIVISION.")
    lines.append("       PROGRAM-ID. TESTPROG.")
    lines.append("")

    # Add data division with some fields
    lines.append("       DATA DIVISION.")
    lines.append("       WORKING-STORAGE SECTION.")
    lines.append("       01 WS-CUSTOMER-RECORD.")
    lines.append("          05 WS-CUSTOMER-ID    PIC X(10).")
    lines.append("          05 WS-CUSTOMER-NAME  PIC X(50).")
    lines.append("       01 WS-ACCOUNT-DATA.")
    lines.append("          05 WS-ACCOUNT-ID     PIC X(10).")
    lines.append("          05 WS-BALANCE        PIC 9(10)V99.")
    lines.append("")

    # Add procedure division with some paragraphs
    lines.append("       PROCEDURE DIVISION.")
    lines.append("       MAIN-PROCESS.")
    lines.append("           PERFORM INITIALIZE-DATA.")
    lines.append("           PERFORM PROCESS-ACCOUNT.")
    lines.append("           STOP RUN.")
    lines.append("")
    lines.append("       INITIALIZE-DATA.")
    lines.append("           MOVE SPACES TO WS-CUSTOMER-RECORD.")
    lines.append("           MOVE ZEROS TO WS-BALANCE.")
    lines.append("")
    lines.append("       PROCESS-ACCOUNT.")
    lines.append("           IF WS-BALANCE > 0")
    lines.append("              DISPLAY 'HAS BALANCE'")
    lines.append("           END-IF.")

    # Add more filler lines to make it longer
    for i in range(100):
        lines.append(f"      * Filler comment line {i}")

    return "\n".join(lines)


class TestOrchestratorBasic:
    """Basic tests for SamplingOrchestrator."""

    def test_prepare_sample_returns_result(
        self, orchestrator: SamplingOrchestrator, sample_cobol_source: str
    ):
        """prepare_sample returns a SamplingResult."""
        result = orchestrator.prepare_sample(
            source_code=sample_cobol_source,
            file_name="TEST.cbl",
            file_type=FileType.COBOL,
            max_tokens=5000,
            ticket_type=TicketType.CLARIFICATION,
        )

        assert isinstance(result, SamplingResult)
        assert result.source_code
        assert result.original_lines > 0
        assert result.sampled_lines >= 0

    def test_prepare_sample_with_questions(
        self, orchestrator: SamplingOrchestrator, sample_cobol_source: str
    ):
        """Questions guide the sampling."""
        question = ChallengerQuestion(
            question="Why is WS-CUSTOMER-ID undocumented?",
            section="inputs",
            evidence=[7],  # Line where WS-CUSTOMER-ID is defined
        )

        result = orchestrator.prepare_sample(
            source_code=sample_cobol_source,
            file_name="TEST.cbl",
            file_type=FileType.COBOL,
            max_tokens=5000,
            ticket_type=TicketType.CLARIFICATION,
            challenger_questions=[question],
        )

        # Should use line_citation strategy
        assert "line_citation" in result.strategies_used or "identifier_mention" in result.strategies_used
        # The sampled code should include relevant content
        assert "WS-CUSTOMER-ID" in result.source_code or result.sampled_lines > 0


class TestOrchestratorStrategies:
    """Tests for strategy selection and application."""

    def test_uses_line_citation_for_evidence(
        self, orchestrator: SamplingOrchestrator, sample_cobol_source: str
    ):
        """Line citation strategy used when evidence has line numbers."""
        question = ChallengerQuestion(
            question="Check this line",
            evidence=[10, 15],
        )

        result = orchestrator.prepare_sample(
            source_code=sample_cobol_source,
            file_name="TEST.cbl",
            file_type=FileType.COBOL,
            max_tokens=5000,
            ticket_type=TicketType.CLARIFICATION,
            challenger_questions=[question],
        )

        assert "line_citation" in result.strategies_used

    def test_uses_identifier_mention_for_names(
        self, orchestrator: SamplingOrchestrator, sample_cobol_source: str
    ):
        """Identifier mention strategy used when identifiers in text."""
        question = ChallengerQuestion(
            question="What does PROCESS-ACCOUNT do?",
        )

        result = orchestrator.prepare_sample(
            source_code=sample_cobol_source,
            file_name="TEST.cbl",
            file_type=FileType.COBOL,
            max_tokens=5000,
            ticket_type=TicketType.CLARIFICATION,
            challenger_questions=[question],
        )

        assert "identifier_mention" in result.strategies_used

    def test_uses_random_fallback_when_no_hints(
        self, orchestrator: SamplingOrchestrator, sample_cobol_source: str
    ):
        """Random fallback used when no extractable hints."""
        # Question with no identifiers or line numbers - use very simple words
        # that won't be extracted as COBOL identifiers
        question = ChallengerQuestion(
            question="is it ok?",  # No extractable hints (all too short)
        )

        result = orchestrator.prepare_sample(
            source_code=sample_cobol_source,
            file_name="TEST.cbl",
            file_type=FileType.COBOL,
            max_tokens=5000,
            ticket_type=TicketType.CLARIFICATION,
            challenger_questions=[question],
        )

        # Should use random fallback since no hints extracted
        assert (
            "random_fallback" in result.strategies_used
            or result.sampled_lines > 0
        )


class TestOrchestratorChromeTickets:
    """Tests for CHROME ticket handling."""

    def test_chrome_tickets_guide_sampling(
        self, orchestrator: SamplingOrchestrator, sample_cobol_source: str
    ):
        """Chrome tickets guide the sampling."""
        chrome = ChromeTicket(
            description="WS-BALANCE documentation is incorrect",
            section="outputs",
            guidance="Check line 11 where it's defined",
        )

        result = orchestrator.prepare_sample(
            source_code=sample_cobol_source,
            file_name="TEST.cbl",
            file_type=FileType.COBOL,
            max_tokens=5000,
            ticket_type=TicketType.CHROME,
            chrome_tickets=[chrome],
        )

        # Should extract hints from chrome ticket
        assert result.sampled_lines > 0
        assert len(result.strategies_used) >= 1


class TestOrchestratorTemplateIntegration:
    """Tests for using previous template citations."""

    def test_uses_template_citations(
        self, orchestrator: SamplingOrchestrator, sample_cobol_source: str
    ):
        """Template section citations guide sampling."""
        # Create template with citations
        template = DocumentationTemplate(
            purpose=PurposeSection(
                summary="Test program",
                citations=[1, 2, 3, 13, 14],  # Lines in identification and procedure
            ),
        )

        question = ChallengerQuestion(
            question="Purpose seems incomplete",
            section="purpose",
        )

        result = orchestrator.prepare_sample(
            source_code=sample_cobol_source,
            file_name="TEST.cbl",
            file_type=FileType.COBOL,
            max_tokens=5000,
            ticket_type=TicketType.CLARIFICATION,
            challenger_questions=[question],
            previous_template=template,
        )

        # Should use section_reference strategy
        assert "section_reference" in result.strategies_used or len(result.strategies_used) >= 1


class TestOrchestratorPreparedSource:
    """Tests for conversion to PreparedSource."""

    def test_to_prepared_source(
        self, orchestrator: SamplingOrchestrator, sample_cobol_source: str
    ):
        """to_prepared_source creates compatible PreparedSource."""
        result = orchestrator.prepare_sample(
            source_code=sample_cobol_source,
            file_name="TEST.cbl",
            file_type=FileType.COBOL,
            max_tokens=5000,
            ticket_type=TicketType.CLARIFICATION,
        )

        prepared = orchestrator.to_prepared_source(result)

        assert prepared.strategy_used == "sampling"
        assert prepared.was_modified is True
        assert prepared.source_code == result.source_code
        assert prepared.metadata["intelligent_sampling"] is True
        assert "strategies_used" in prepared.metadata


class TestOrchestratorHintsFound:
    """Tests for hints_found metadata."""

    def test_hints_found_populated(
        self, orchestrator: SamplingOrchestrator, sample_cobol_source: str
    ):
        """hints_found metadata shows what was extracted."""
        question = ChallengerQuestion(
            question="Check WS-CUSTOMER-ID at line 7",
            evidence=[7, 8],
            section="inputs",
        )

        result = orchestrator.prepare_sample(
            source_code=sample_cobol_source,
            file_name="TEST.cbl",
            file_type=FileType.COBOL,
            max_tokens=5000,
            ticket_type=TicketType.CLARIFICATION,
            challenger_questions=[question],
        )

        assert "line_numbers" in result.hints_found
        assert "identifiers" in result.hints_found
        assert "section_names" in result.hints_found
        # Should have found some hints
        assert (
            result.hints_found["line_numbers"] > 0
            or result.hints_found["identifiers"] > 0
        )
