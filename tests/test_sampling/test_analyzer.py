"""Tests for RelevanceAnalyzer hint extraction.

Tests the ability to extract line numbers, identifiers, and section
references from ChallengerQuestion and ChromeTicket objects.
"""

import pytest
from unittest.mock import MagicMock

from war_rig.beads import TicketType
from war_rig.models.templates import FileType, DocumentationTemplate, PurposeSection
from war_rig.models.tickets import ChallengerQuestion, ChromeTicket
from war_rig.sampling.analyzer import RelevanceAnalyzer, RelevanceHints
from war_rig.sampling.context import SamplingContext


@pytest.fixture
def analyzer() -> RelevanceAnalyzer:
    """Create a RelevanceAnalyzer instance."""
    return RelevanceAnalyzer()


@pytest.fixture
def sample_source() -> str:
    """Sample COBOL source code for testing."""
    lines = [f"       Line {i:04d} of source code" for i in range(1, 101)]
    return "\n".join(lines)


@pytest.fixture
def basic_context(sample_source: str) -> SamplingContext:
    """Create a basic SamplingContext for testing."""
    return SamplingContext.create(
        source_code=sample_source,
        file_name="TEST.cbl",
        file_type=FileType.COBOL,
        max_tokens=5000,
        estimated_source_tokens=10000,
        ticket_type=TicketType.CLARIFICATION,
    )


class TestRelevanceHints:
    """Tests for RelevanceHints dataclass."""

    def test_has_explicit_hints_with_line_numbers(self):
        """Line numbers count as explicit hints."""
        hints = RelevanceHints(line_numbers=[10, 20])
        assert hints.has_explicit_hints is True

    def test_has_explicit_hints_with_line_ranges(self):
        """Line ranges count as explicit hints."""
        hints = RelevanceHints(line_ranges=[(10, 20)])
        assert hints.has_explicit_hints is True

    def test_has_explicit_hints_with_section_citations(self):
        """Section citations count as explicit hints."""
        hints = RelevanceHints(section_citations={"inputs": [50, 60]})
        assert hints.has_explicit_hints is True

    def test_has_explicit_hints_with_identifiers(self):
        """Identifiers count as explicit hints."""
        hints = RelevanceHints(identifiers=["CUSTOMER-RECORD"])
        assert hints.has_explicit_hints is True

    def test_has_explicit_hints_false_when_empty(self):
        """Empty hints return False."""
        hints = RelevanceHints()
        assert hints.has_explicit_hints is False

    def test_has_explicit_hints_false_with_only_section_names(self):
        """Section names alone (without citations) don't count."""
        hints = RelevanceHints(section_names=["inputs"])
        assert hints.has_explicit_hints is False

    def test_all_cited_lines(self):
        """all_cited_lines combines and deduplicates line sources."""
        hints = RelevanceHints(
            line_numbers=[10, 20, 30],
            section_citations={"inputs": [20, 40], "outputs": [50]},
        )
        assert hints.all_cited_lines == [10, 20, 30, 40, 50]


class TestRelevanceAnalyzerLineExtraction:
    """Tests for extracting line numbers from questions."""

    def test_extract_line_from_evidence(self, analyzer: RelevanceAnalyzer, basic_context: SamplingContext):
        """Lines in evidence field are extracted."""
        question = ChallengerQuestion(
            question="Why is this field unused?",
            evidence=[45, 67],
            section="inputs",
        )
        basic_context.challenger_questions = [question]

        hints = analyzer.analyze(basic_context)

        assert 45 in hints.line_numbers
        assert 67 in hints.line_numbers

    def test_extract_line_from_question_text(self, analyzer: RelevanceAnalyzer, basic_context: SamplingContext):
        """Line references in question text are extracted."""
        question = ChallengerQuestion(
            question="Line 25 seems to have an error. Also check line 50.",
            section="business_rules",
        )
        basic_context.challenger_questions = [question]

        hints = analyzer.analyze(basic_context)

        assert 25 in hints.line_numbers
        assert 50 in hints.line_numbers

    def test_extract_line_range_from_text(self, analyzer: RelevanceAnalyzer, basic_context: SamplingContext):
        """Line ranges like 'lines 10-20' are extracted."""
        question = ChallengerQuestion(
            question="Lines 30-45 need review",
            section="data_flow",
        )
        basic_context.challenger_questions = [question]

        hints = analyzer.analyze(basic_context)

        assert (30, 45) in hints.line_ranges

    def test_invalid_line_numbers_filtered(self, analyzer: RelevanceAnalyzer, basic_context: SamplingContext):
        """Line numbers outside source bounds are filtered."""
        question = ChallengerQuestion(
            question="Check line 500",  # Source only has 100 lines
            evidence=[200],  # Also out of bounds
        )
        basic_context.challenger_questions = [question]

        hints = analyzer.analyze(basic_context)

        assert 500 not in hints.line_numbers
        assert 200 not in hints.line_numbers


class TestRelevanceAnalyzerIdentifierExtraction:
    """Tests for extracting COBOL identifiers from questions."""

    def test_extract_identifiers(self, analyzer: RelevanceAnalyzer, basic_context: SamplingContext):
        """COBOL-style identifiers are extracted from text."""
        question = ChallengerQuestion(
            question="The CUSTOMER-RECORD field is not documented. "
                     "What about WS-ACCOUNT-ID?",
        )
        basic_context.challenger_questions = [question]

        hints = analyzer.analyze(basic_context)

        assert "CUSTOMER-RECORD" in hints.identifiers
        assert "WS-ACCOUNT-ID" in hints.identifiers

    def test_filter_common_words(self, analyzer: RelevanceAnalyzer, basic_context: SamplingContext):
        """Common English words are not extracted as identifiers."""
        question = ChallengerQuestion(
            question="THE MISSING section SHOULD have more details",
        )
        basic_context.challenger_questions = [question]

        hints = analyzer.analyze(basic_context)

        # Common words should be filtered
        assert "THE" not in hints.identifiers
        assert "MISSING" not in hints.identifiers
        assert "SHOULD" not in hints.identifiers

    def test_short_identifiers_filtered(self, analyzer: RelevanceAnalyzer, basic_context: SamplingContext):
        """Identifiers shorter than 3 chars are filtered."""
        question = ChallengerQuestion(
            question="Field A and ID need review",
        )
        basic_context.challenger_questions = [question]

        hints = analyzer.analyze(basic_context)

        # Too short
        assert "A" not in hints.identifiers
        assert "ID" not in hints.identifiers


class TestRelevanceAnalyzerSectionExtraction:
    """Tests for extracting section references from questions."""

    def test_extract_section_name(self, analyzer: RelevanceAnalyzer, basic_context: SamplingContext):
        """Section field is extracted from questions."""
        question = ChallengerQuestion(
            question="This seems incomplete",
            section="inputs",
        )
        basic_context.challenger_questions = [question]

        hints = analyzer.analyze(basic_context)

        assert "inputs" in hints.section_names

    def test_section_citations_from_template(self, analyzer: RelevanceAnalyzer, basic_context: SamplingContext):
        """Section citations are looked up from previous template."""
        # Create template with citations
        template = DocumentationTemplate(
            purpose=PurposeSection(
                summary="Test program",
                citations=[10, 20, 30],
            ),
        )
        basic_context.previous_template = template

        question = ChallengerQuestion(
            question="Purpose seems wrong",
            section="purpose",
        )
        basic_context.challenger_questions = [question]

        hints = analyzer.analyze(basic_context)

        assert "purpose" in hints.section_citations
        assert 10 in hints.section_citations["purpose"]
        assert 20 in hints.section_citations["purpose"]
        assert 30 in hints.section_citations["purpose"]


class TestRelevanceAnalyzerChromeTickets:
    """Tests for extracting hints from ChromeTicket objects."""

    def test_extract_from_chrome_description(self, analyzer: RelevanceAnalyzer, basic_context: SamplingContext):
        """Line numbers and identifiers are extracted from chrome description."""
        chrome = ChromeTicket(
            description="Line 42 incorrectly documents PROCESS-ACCOUNT",
            section="business_rules",
        )
        basic_context.chrome_tickets = [chrome]
        basic_context.ticket_type = TicketType.CHROME

        hints = analyzer.analyze(basic_context)

        assert 42 in hints.line_numbers
        assert "PROCESS-ACCOUNT" in hints.identifiers
        assert "business_rules" in hints.section_names

    def test_extract_from_chrome_guidance(self, analyzer: RelevanceAnalyzer, basic_context: SamplingContext):
        """Hints are extracted from chrome guidance field."""
        chrome = ChromeTicket(
            description="Missing information",
            guidance="Check line 75 where VALIDATE-INPUT is called",
        )
        basic_context.chrome_tickets = [chrome]
        basic_context.ticket_type = TicketType.CHROME

        hints = analyzer.analyze(basic_context)

        assert 75 in hints.line_numbers
        assert "VALIDATE-INPUT" in hints.identifiers


class TestRelevanceAnalyzerMultipleSources:
    """Tests for combining hints from multiple questions/tickets."""

    def test_combine_multiple_questions(self, analyzer: RelevanceAnalyzer, basic_context: SamplingContext):
        """Hints from multiple questions are combined."""
        q1 = ChallengerQuestion(
            question="Check FIELD-A at line 10",
            section="inputs",
        )
        q2 = ChallengerQuestion(
            question="Also FIELD-B at line 20",
            section="outputs",
        )
        basic_context.challenger_questions = [q1, q2]

        hints = analyzer.analyze(basic_context)

        assert 10 in hints.line_numbers
        assert 20 in hints.line_numbers
        assert "FIELD-A" in hints.identifiers
        assert "FIELD-B" in hints.identifiers
        assert "inputs" in hints.section_names
        assert "outputs" in hints.section_names

    def test_deduplicate_hints(self, analyzer: RelevanceAnalyzer, basic_context: SamplingContext):
        """Duplicate hints are deduplicated."""
        q1 = ChallengerQuestion(
            question="Line 10 has FIELD-A",
            evidence=[10],
        )
        q2 = ChallengerQuestion(
            question="Also line 10 and FIELD-A",
            evidence=[10],
        )
        basic_context.challenger_questions = [q1, q2]

        hints = analyzer.analyze(basic_context)

        # Should only appear once
        assert hints.line_numbers.count(10) == 1
        assert hints.identifiers.count("FIELD-A") == 1
