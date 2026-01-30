"""Tests for PatternAggregator.

Tests cover:
- Pattern association with paragraphs by line range
- Variable frequency extraction from data flow matches
- Paragraph complexity classification
- Scribe insights aggregation
- Challenger facts aggregation
- Token budget verification
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any

import pytest

from war_rig.analysis.pattern_aggregator import (
    PatternAggregator,
    estimate_tokens,
)


# Mock classes to simulate Citadel SDK types
@dataclass
class MockAnalysisPatternMatch:
    """Mock for AnalysisPatternMatchSDK."""

    pattern_name: str
    category: str
    captured: list[str]
    line: int | None = None
    context: list[str] = field(default_factory=list)


@dataclass
class MockAnalysisCategoryResult:
    """Mock for AnalysisCategoryResult."""

    category: str
    matches: list[MockAnalysisPatternMatch]
    match_count: int = 0
    patterns_matched: dict[str, int] = field(default_factory=dict)

    def __post_init__(self) -> None:
        self.match_count = len(self.matches)
        for match in self.matches:
            self.patterns_matched[match.pattern_name] = (
                self.patterns_matched.get(match.pattern_name, 0) + 1
            )


@dataclass
class MockFileAnalysisPatternResult:
    """Mock for FileAnalysisPatternResult."""

    file_path: str
    language: str
    categories: dict[str, MockAnalysisCategoryResult]
    total_matches: int = 0
    coverage_pct: float = 0.0
    required_missing: list[str] = field(default_factory=list)
    error: str | None = None

    def __post_init__(self) -> None:
        self.total_matches = sum(
            cat.match_count for cat in self.categories.values()
        )


class TestEstimateTokens:
    """Tests for token estimation."""

    def test_empty_string(self) -> None:
        """Empty string should have 0 tokens."""
        assert estimate_tokens("") == 0

    def test_short_string(self) -> None:
        """Short strings should estimate based on character count."""
        # 12 chars / 4 = 3 tokens
        assert estimate_tokens("hello world!") == 3

    def test_longer_string(self) -> None:
        """Longer strings should maintain ratio."""
        text = "This is a longer piece of text for testing."
        expected = len(text) // 4
        assert estimate_tokens(text) == expected


class TestAssociatePatternswithParagraphs:
    """Tests for _associate_patterns_with_paragraphs."""

    def test_basic_association(self) -> None:
        """Matches are associated with correct paragraphs by line range."""
        aggregator = PatternAggregator()

        matches = [
            {"pattern_name": "move_simple", "category": "data_flow", "line": 105},
            {"pattern_name": "if_perform", "category": "control_flow", "line": 150},
            {"pattern_name": "move_simple", "category": "data_flow", "line": 160},
            {"pattern_name": "file_status_check", "category": "error_handling", "line": 220},
        ]

        paragraphs = [
            {"name": "1000-INIT", "line_start": 100, "line_end": 120},
            {"name": "2000-PROCESS", "line_start": 140, "line_end": 180},
            {"name": "3000-CLEANUP", "line_start": 200, "line_end": 250},
        ]

        result = aggregator._associate_patterns_with_paragraphs(matches, paragraphs)

        assert len(result["1000-INIT"]) == 1
        assert result["1000-INIT"][0]["line"] == 105

        assert len(result["2000-PROCESS"]) == 2
        assert result["2000-PROCESS"][0]["line"] == 150
        assert result["2000-PROCESS"][1]["line"] == 160

        assert len(result["3000-CLEANUP"]) == 1
        assert result["3000-CLEANUP"][0]["line"] == 220

    def test_match_outside_all_paragraphs(self) -> None:
        """Matches outside all paragraphs are not associated."""
        aggregator = PatternAggregator()

        matches = [
            {"pattern_name": "move_simple", "category": "data_flow", "line": 50},
            {"pattern_name": "move_simple", "category": "data_flow", "line": 500},
        ]

        paragraphs = [
            {"name": "1000-INIT", "line_start": 100, "line_end": 120},
        ]

        result = aggregator._associate_patterns_with_paragraphs(matches, paragraphs)

        assert len(result["1000-INIT"]) == 0

    def test_match_without_line(self) -> None:
        """Matches without line numbers are skipped."""
        aggregator = PatternAggregator()

        matches = [
            {"pattern_name": "move_simple", "category": "data_flow", "line": None},
        ]

        paragraphs = [
            {"name": "1000-INIT", "line_start": 100, "line_end": 200},
        ]

        result = aggregator._associate_patterns_with_paragraphs(matches, paragraphs)

        assert len(result["1000-INIT"]) == 0

    def test_empty_paragraphs(self) -> None:
        """Empty paragraph list returns empty result."""
        aggregator = PatternAggregator()

        matches = [
            {"pattern_name": "move_simple", "category": "data_flow", "line": 100},
        ]

        result = aggregator._associate_patterns_with_paragraphs(matches, [])

        assert result == {}


class TestExtractVariableFrequency:
    """Tests for _extract_variable_frequency."""

    def test_move_simple(self) -> None:
        """MOVE_SIMPLE pattern extracts source and target."""
        aggregator = PatternAggregator()

        matches = [
            {"pattern_name": "move_simple", "category": "data_flow",
             "captured": ["SOURCE-VAR", "TARGET-VAR"]},
            {"pattern_name": "move_simple", "category": "data_flow",
             "captured": ["SOURCE-VAR", "OTHER-VAR"]},
        ]

        result = aggregator._extract_variable_frequency(matches)

        assert result["SOURCE-VAR"] == 2
        assert result["TARGET-VAR"] == 1
        assert result["OTHER-VAR"] == 1

    def test_compute_pattern(self) -> None:
        """COMPUTE pattern extracts target variable."""
        aggregator = PatternAggregator()

        matches = [
            {"pattern_name": "compute_expression", "category": "data_flow",
             "captured": ["RESULT-VAR", "A + B"]},
            {"pattern_name": "compute", "category": "data_flow",
             "captured": ["RESULT-VAR"]},
        ]

        result = aggregator._extract_variable_frequency(matches)

        assert result["RESULT-VAR"] == 2

    def test_arithmetic_patterns(self) -> None:
        """ADD/SUBTRACT patterns extract all operands."""
        aggregator = PatternAggregator()

        matches = [
            {"pattern_name": "add_statement", "category": "data_flow",
             "captured": ["VAR-A", "VAR-B", "VAR-RESULT"]},
            {"pattern_name": "subtract", "category": "data_flow",
             "captured": ["VAR-X", "VAR-Y"]},
        ]

        result = aggregator._extract_variable_frequency(matches)

        assert result["VAR-A"] == 1
        assert result["VAR-B"] == 1
        assert result["VAR-RESULT"] == 1
        assert result["VAR-X"] == 1
        assert result["VAR-Y"] == 1

    def test_empty_matches(self) -> None:
        """Empty matches returns empty dict."""
        aggregator = PatternAggregator()

        result = aggregator._extract_variable_frequency([])

        assert result == {}


class TestComputeParagraphComplexity:
    """Tests for _compute_paragraph_complexity."""

    def test_simple_paragraph(self) -> None:
        """Few matches yields simple complexity."""
        aggregator = PatternAggregator()

        matches = [
            {"pattern_name": "move_simple", "category": "data_flow"},
            {"pattern_name": "move_simple", "category": "data_flow"},
        ]

        result = aggregator._compute_paragraph_complexity(matches)

        assert result == "simple"

    def test_moderate_paragraph(self) -> None:
        """Several decision points yields moderate complexity."""
        aggregator = PatternAggregator()

        matches = [
            {"pattern_name": "if_perform", "category": "control_flow"},
            {"pattern_name": "if_then", "category": "control_flow"},
            {"pattern_name": "move_simple", "category": "data_flow"},
            {"pattern_name": "move_simple", "category": "data_flow"},
            {"pattern_name": "move_simple", "category": "data_flow"},
        ]

        result = aggregator._compute_paragraph_complexity(matches)

        assert result == "moderate"

    def test_complex_paragraph_with_loops(self) -> None:
        """Loops and error handling yields complex."""
        aggregator = PatternAggregator()

        matches = [
            {"pattern_name": "perform_until", "category": "control_flow"},
            {"pattern_name": "if_perform", "category": "control_flow"},
            {"pattern_name": "if_then", "category": "control_flow"},
            {"pattern_name": "file_status_check", "category": "error_handling"},
            {"pattern_name": "move_simple", "category": "data_flow"},
            {"pattern_name": "move_simple", "category": "data_flow"},
            {"pattern_name": "move_simple", "category": "data_flow"},
            {"pattern_name": "move_simple", "category": "data_flow"},
            {"pattern_name": "move_simple", "category": "data_flow"},
        ]

        result = aggregator._compute_paragraph_complexity(matches)

        assert result == "complex"

    def test_empty_matches(self) -> None:
        """Empty matches yields simple."""
        aggregator = PatternAggregator()

        result = aggregator._compute_paragraph_complexity([])

        assert result == "simple"


class TestAggregateForScribe:
    """Tests for aggregate_for_scribe."""

    def test_basic_aggregation(self) -> None:
        """Basic aggregation produces expected structure."""
        aggregator = PatternAggregator()

        # Create mock pattern result
        data_flow_matches = [
            MockAnalysisPatternMatch(
                pattern_name="move_simple",
                category="data_flow",
                captured=["SOURCE", "TARGET"],
                line=105,
            ),
            MockAnalysisPatternMatch(
                pattern_name="compute_expression",
                category="data_flow",
                captured=["RESULT", "A + B"],
                line=110,
            ),
        ]
        control_flow_matches = [
            MockAnalysisPatternMatch(
                pattern_name="if_perform",
                category="control_flow",
                captured=["OTHER-PARA"],
                line=115,
            ),
        ]

        pattern_result = MockFileAnalysisPatternResult(
            file_path="/test/file.cbl",
            language="cobol",
            categories={
                "data_flow": MockAnalysisCategoryResult(
                    category="data_flow",
                    matches=data_flow_matches,
                ),
                "control_flow": MockAnalysisCategoryResult(
                    category="control_flow",
                    matches=control_flow_matches,
                ),
            },
        )

        outline = [
            {"name": "1000-INIT", "line_start": 100, "line_end": 120},
        ]

        result = aggregator.aggregate_for_scribe(pattern_result, outline)

        assert "file_summary" in result
        assert "paragraph_hints" in result
        assert "critical_patterns" in result

        # File summary should have key variables
        assert "key_variables" in result["file_summary"]
        assert "complexity_indicators" in result["file_summary"]

    def test_none_pattern_result(self) -> None:
        """None pattern result returns empty dict."""
        aggregator = PatternAggregator()

        result = aggregator.aggregate_for_scribe(None, [])

        assert result == {}

    def test_error_pattern_result(self) -> None:
        """Pattern result with error returns empty dict."""
        aggregator = PatternAggregator()

        pattern_result = MockFileAnalysisPatternResult(
            file_path="/test/file.cbl",
            language="cobol",
            categories={},
            error="Analysis failed",
        )

        result = aggregator.aggregate_for_scribe(pattern_result, [])

        assert result == {}

    def test_respects_max_paragraphs(self) -> None:
        """Output is limited to max_paragraphs."""
        aggregator = PatternAggregator(max_paragraphs=2)

        # Create matches in 3 paragraphs
        matches = []
        for i in range(3):
            start = 100 + i * 50
            matches.append(MockAnalysisPatternMatch(
                pattern_name="move_simple",
                category="data_flow",
                captured=["VAR1", "VAR2"],
                line=start + 10,
            ))

        pattern_result = MockFileAnalysisPatternResult(
            file_path="/test/file.cbl",
            language="cobol",
            categories={
                "data_flow": MockAnalysisCategoryResult(
                    category="data_flow",
                    matches=matches,
                ),
            },
        )

        outline = [
            {"name": f"PARA-{i}", "line_start": 100 + i * 50, "line_end": 140 + i * 50}
            for i in range(3)
        ]

        result = aggregator.aggregate_for_scribe(pattern_result, outline)

        assert len(result["paragraph_hints"]) <= 2


class TestAggregateForChallenger:
    """Tests for aggregate_for_challenger."""

    def test_basic_aggregation(self) -> None:
        """Basic aggregation produces expected structure."""
        aggregator = PatternAggregator()

        # Create mock pattern result
        data_flow_matches = [
            MockAnalysisPatternMatch(
                pattern_name="move_simple",
                category="data_flow",
                captured=["SOURCE", "TARGET"],
                line=105,
            ),
        ]

        pattern_result = MockFileAnalysisPatternResult(
            file_path="/test/file.cbl",
            language="cobol",
            categories={
                "data_flow": MockAnalysisCategoryResult(
                    category="data_flow",
                    matches=data_flow_matches,
                ),
            },
        )

        outline = [
            {"name": "1000-INIT", "line_start": 100, "line_end": 120},
        ]

        result = aggregator.aggregate_for_challenger(pattern_result, outline)

        assert "paragraph_facts" in result
        assert "validation_cues" in result
        assert "expected_coverage" in result

    def test_paragraph_facts_structure(self) -> None:
        """Paragraph facts have correct structure."""
        aggregator = PatternAggregator()

        # Create matches in a paragraph
        matches = [
            MockAnalysisPatternMatch(
                pattern_name="move_simple",
                category="data_flow",
                captured=["VAR1", "VAR2"],
                line=105,
            ),
            MockAnalysisPatternMatch(
                pattern_name="if_perform",
                category="control_flow",
                captured=["OTHER"],
                line=110,
            ),
            MockAnalysisPatternMatch(
                pattern_name="file_status_check",
                category="error_handling",
                captured=["FS-VAR"],
                line=115,
            ),
        ]

        pattern_result = MockFileAnalysisPatternResult(
            file_path="/test/file.cbl",
            language="cobol",
            categories={
                "data_flow": MockAnalysisCategoryResult(
                    category="data_flow",
                    matches=[m for m in matches if m.category == "data_flow"],
                ),
                "control_flow": MockAnalysisCategoryResult(
                    category="control_flow",
                    matches=[m for m in matches if m.category == "control_flow"],
                ),
                "error_handling": MockAnalysisCategoryResult(
                    category="error_handling",
                    matches=[m for m in matches if m.category == "error_handling"],
                ),
            },
        )

        outline = [
            {"name": "1000-INIT", "line_start": 100, "line_end": 120},
        ]

        result = aggregator.aggregate_for_challenger(pattern_result, outline)

        assert len(result["paragraph_facts"]) == 1
        fact = result["paragraph_facts"][0]
        assert fact["paragraph_name"] == "1000-INIT"
        assert fact["data_flow_count"] == 1
        assert fact["control_flow_count"] == 1
        assert fact["error_handling_count"] == 1

    def test_none_pattern_result(self) -> None:
        """None pattern result returns empty dict."""
        aggregator = PatternAggregator()

        result = aggregator.aggregate_for_challenger(None, [])

        assert result == {}


class TestTokenBudget:
    """Tests for token budget limits."""

    def test_output_within_token_budget(self) -> None:
        """Output stays within approximately 4000 tokens."""
        aggregator = PatternAggregator()

        # Create a large number of matches
        matches = []
        for i in range(100):
            matches.append(MockAnalysisPatternMatch(
                pattern_name="move_simple",
                category="data_flow",
                captured=[f"VAR-{i}", f"TARGET-{i}"],
                line=100 + i * 2,
            ))
            matches.append(MockAnalysisPatternMatch(
                pattern_name="if_perform",
                category="control_flow",
                captured=[f"PARA-{i}"],
                line=100 + i * 2 + 1,
            ))

        pattern_result = MockFileAnalysisPatternResult(
            file_path="/test/file.cbl",
            language="cobol",
            categories={
                "data_flow": MockAnalysisCategoryResult(
                    category="data_flow",
                    matches=[m for m in matches if m.category == "data_flow"],
                ),
                "control_flow": MockAnalysisCategoryResult(
                    category="control_flow",
                    matches=[m for m in matches if m.category == "control_flow"],
                ),
            },
        )

        # Create many paragraphs
        outline = [
            {"name": f"PARA-{i}", "line_start": 100 + i * 10, "line_end": 109 + i * 10}
            for i in range(50)
        ]

        result = aggregator.aggregate_for_scribe(pattern_result, outline)

        # Serialize to string and estimate tokens
        import json
        result_str = json.dumps(result, indent=2)
        token_estimate = estimate_tokens(result_str)

        # Should be well under 4000 tokens
        assert token_estimate < 4000, f"Token estimate {token_estimate} exceeds budget"


class TestHintGeneration:
    """Tests for _generate_documentation_hints."""

    def test_data_flow_heavy_hint(self) -> None:
        """Data flow heavy paragraph gets appropriate hint."""
        aggregator = PatternAggregator()

        matches = [
            {"pattern_name": "move_simple", "category": "data_flow",
             "captured": ["VAR-A", "VAR-B"]},
            {"pattern_name": "move_simple", "category": "data_flow",
             "captured": ["VAR-C", "VAR-D"]},
            {"pattern_name": "move_simple", "category": "data_flow",
             "captured": ["VAR-E", "VAR-F"]},
            {"pattern_name": "compute_expression", "category": "data_flow",
             "captured": ["RESULT", "EXPR"]},
        ]

        hints = aggregator._generate_documentation_hints("TEST-PARA", matches)

        assert len(hints) >= 1
        # Should mention modifying variables
        assert any("Modifies" in h for h in hints)

    def test_error_handling_hint(self) -> None:
        """Error handling paragraph gets appropriate hint."""
        aggregator = PatternAggregator()

        matches = [
            {"pattern_name": "file_status_check", "category": "error_handling",
             "captured": ["FILE-STATUS"]},
            {"pattern_name": "sqlcode_check", "category": "error_handling",
             "captured": ["SQLCODE"]},
        ]

        hints = aggregator._generate_documentation_hints("TEST-PARA", matches)

        assert len(hints) >= 1
        # Should mention error handling
        assert any("error" in h.lower() for h in hints)

    def test_loop_hint(self) -> None:
        """Loop paragraph gets appropriate hint."""
        aggregator = PatternAggregator()

        matches = [
            {"pattern_name": "perform_until", "category": "control_flow",
             "captured": ["LOOP-PARA", "EOF-FLAG"]},
        ]

        hints = aggregator._generate_documentation_hints("TEST-PARA", matches)

        assert len(hints) >= 1
        # Should mention loop
        assert any("Loop" in h for h in hints)

    def test_max_hints_per_paragraph(self) -> None:
        """Hints are limited to max_hints_per_paragraph."""
        aggregator = PatternAggregator(max_hints_per_paragraph=2)

        # Create matches that would generate many hints
        matches = [
            {"pattern_name": "move_simple", "category": "data_flow",
             "captured": ["A", "B"]},
            {"pattern_name": "move_simple", "category": "data_flow",
             "captured": ["C", "D"]},
            {"pattern_name": "move_simple", "category": "data_flow",
             "captured": ["E", "F"]},
            {"pattern_name": "perform_until", "category": "control_flow",
             "captured": ["LOOP", "COND"]},
            {"pattern_name": "file_status_check", "category": "error_handling",
             "captured": ["FS"]},
            {"pattern_name": "if_perform", "category": "control_flow",
             "captured": ["PARA1"]},
            {"pattern_name": "if_then", "category": "control_flow",
             "captured": []},
        ]

        hints = aggregator._generate_documentation_hints("TEST-PARA", matches)

        assert len(hints) <= 2
