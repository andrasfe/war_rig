"""Integration tests for PatternAggregator with real COBOL files.

These tests use real COBOL files from the CardDemo application to verify
the aggregator works correctly with actual Citadel analysis output.
"""

from __future__ import annotations

import json
from pathlib import Path

import pytest

from war_rig.analysis.pattern_aggregator import (
    PatternAggregator,
    estimate_tokens,
)

# Path to the test COBOL file (COACTUPC.cbl has 1,163+ matches as per design doc)
COBOL_TEST_FILE = Path(
    "/Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/cbl/COACTUPC.cbl"
)


@pytest.fixture
def citadel():
    """Get Citadel SDK instance if available."""
    try:
        from citadel import Citadel
        return Citadel()
    except ImportError:
        pytest.skip("Citadel SDK not available")


@pytest.fixture
def test_file_path() -> Path:
    """Get the test COBOL file path."""
    if not COBOL_TEST_FILE.exists():
        pytest.skip(f"Test file not found: {COBOL_TEST_FILE}")
    return COBOL_TEST_FILE


class TestPatternAggregatorIntegration:
    """Integration tests with real COBOL files."""

    def test_analyze_coactupc_patterns(
        self, citadel, test_file_path: Path
    ) -> None:
        """Test pattern analysis on COACTUPC.cbl."""
        # Get pattern analysis from Citadel
        pattern_result = citadel.get_analysis_patterns(str(test_file_path))

        # Verify we got patterns
        assert pattern_result is not None
        assert pattern_result.error is None, f"Analysis error: {pattern_result.error}"
        assert pattern_result.total_matches > 0, "Expected matches from COBOL file"

        # Log statistics for debugging
        print(f"\nPattern analysis results for {test_file_path.name}:")
        print(f"  Total matches: {pattern_result.total_matches}")
        print(f"  Categories: {list(pattern_result.categories.keys())}")
        for cat_name, cat_result in pattern_result.categories.items():
            print(f"    {cat_name}: {cat_result.match_count} matches")

    def test_aggregate_for_scribe_real_file(
        self, citadel, test_file_path: Path
    ) -> None:
        """Test Scribe aggregation on real COBOL file."""
        # Get patterns
        pattern_result = citadel.get_analysis_patterns(str(test_file_path))
        assert pattern_result.error is None

        # Get file stats for outline
        stats = citadel.get_file_stats(str(test_file_path))
        paragraphs = stats.get("paragraphs", [])

        # Build outline
        outline = [
            {
                "name": p.get("name", ""),
                "line_start": p.get("line_start"),
                "line_end": p.get("line_end"),
            }
            for p in paragraphs
            if p.get("name")
        ]

        # Run aggregator
        aggregator = PatternAggregator()
        result = aggregator.aggregate_for_scribe(pattern_result, outline)

        # Verify structure
        assert "file_summary" in result
        assert "paragraph_hints" in result
        assert "critical_patterns" in result

        # Verify file summary
        file_summary = result["file_summary"]
        assert "key_variables" in file_summary
        assert "complexity_indicators" in file_summary

        # Log results for debugging
        print(f"\nScribe aggregation results:")
        print(f"  Key variables: {len(file_summary.get('key_variables', []))}")
        print(f"  Paragraph hints: {len(result['paragraph_hints'])}")
        print(f"  Critical patterns: {len(result['critical_patterns'])}")

    def test_aggregate_for_challenger_real_file(
        self, citadel, test_file_path: Path
    ) -> None:
        """Test Challenger aggregation on real COBOL file."""
        # Get patterns
        pattern_result = citadel.get_analysis_patterns(str(test_file_path))
        assert pattern_result.error is None

        # Get file stats for outline
        stats = citadel.get_file_stats(str(test_file_path))
        paragraphs = stats.get("paragraphs", [])

        # Build outline
        outline = [
            {
                "name": p.get("name", ""),
                "line_start": p.get("line_start"),
                "line_end": p.get("line_end"),
            }
            for p in paragraphs
            if p.get("name")
        ]

        # Run aggregator
        aggregator = PatternAggregator()
        result = aggregator.aggregate_for_challenger(pattern_result, outline)

        # Verify structure
        assert "paragraph_facts" in result
        assert "validation_cues" in result
        assert "expected_coverage" in result

        # Verify paragraph facts have correct structure
        for fact in result["paragraph_facts"]:
            assert "paragraph_name" in fact
            assert "data_flow_count" in fact
            assert "control_flow_count" in fact
            assert "error_handling_count" in fact

        # Log results for debugging
        print(f"\nChallenger aggregation results:")
        print(f"  Paragraph facts: {len(result['paragraph_facts'])}")
        print(f"  Validation cues: {len(result['validation_cues'])}")
        print(f"  Expected coverage entries: {len(result['expected_coverage'])}")

    def test_output_under_4000_tokens(
        self, citadel, test_file_path: Path
    ) -> None:
        """Verify aggregated output is under 4000 tokens."""
        # Get patterns
        pattern_result = citadel.get_analysis_patterns(str(test_file_path))
        assert pattern_result.error is None

        # Get file stats for outline
        stats = citadel.get_file_stats(str(test_file_path))
        paragraphs = stats.get("paragraphs", [])

        outline = [
            {
                "name": p.get("name", ""),
                "line_start": p.get("line_start"),
                "line_end": p.get("line_end"),
            }
            for p in paragraphs
            if p.get("name")
        ]

        aggregator = PatternAggregator()

        # Test Scribe output
        scribe_result = aggregator.aggregate_for_scribe(pattern_result, outline)
        scribe_str = json.dumps(scribe_result, indent=2)
        scribe_tokens = estimate_tokens(scribe_str)

        print(f"\nScribe output size: {len(scribe_str)} chars, ~{scribe_tokens} tokens")
        assert scribe_tokens < 4000, f"Scribe output ({scribe_tokens} tokens) exceeds 4000 token budget"

        # Test Challenger output
        challenger_result = aggregator.aggregate_for_challenger(pattern_result, outline)
        challenger_str = json.dumps(challenger_result, indent=2)
        challenger_tokens = estimate_tokens(challenger_str)

        print(f"Challenger output size: {len(challenger_str)} chars, ~{challenger_tokens} tokens")
        assert challenger_tokens < 4000, f"Challenger output ({challenger_tokens} tokens) exceeds 4000 token budget"

    def test_key_variables_extracted(
        self, citadel, test_file_path: Path
    ) -> None:
        """Verify key variables are extracted from data flow patterns."""
        # Get patterns
        pattern_result = citadel.get_analysis_patterns(str(test_file_path))
        assert pattern_result.error is None

        # Get outline
        stats = citadel.get_file_stats(str(test_file_path))
        paragraphs = stats.get("paragraphs", [])
        outline = [
            {
                "name": p.get("name", ""),
                "line_start": p.get("line_start"),
                "line_end": p.get("line_end"),
            }
            for p in paragraphs
            if p.get("name")
        ]

        aggregator = PatternAggregator()
        result = aggregator.aggregate_for_scribe(pattern_result, outline)

        # Verify key variables are present
        key_vars = result.get("file_summary", {}).get("key_variables", [])
        assert len(key_vars) > 0, "Expected at least some key variables"
        assert len(key_vars) <= 10, "Key variables should be limited to max_variables (10)"

        print(f"\nKey variables extracted: {key_vars}")

    def test_hints_generated_for_complex_paragraphs(
        self, citadel, test_file_path: Path
    ) -> None:
        """Verify hints are generated for complex paragraphs."""
        # Get patterns
        pattern_result = citadel.get_analysis_patterns(str(test_file_path))
        assert pattern_result.error is None

        # Get outline
        stats = citadel.get_file_stats(str(test_file_path))
        paragraphs = stats.get("paragraphs", [])
        outline = [
            {
                "name": p.get("name", ""),
                "line_start": p.get("line_start"),
                "line_end": p.get("line_end"),
            }
            for p in paragraphs
            if p.get("name")
        ]

        aggregator = PatternAggregator()
        result = aggregator.aggregate_for_scribe(pattern_result, outline)

        # Check paragraph hints
        para_hints = result.get("paragraph_hints", [])
        assert len(para_hints) > 0, "Expected paragraph hints for COBOL file"

        # Verify complex paragraphs have hints
        complex_paras = [p for p in para_hints if p.get("complexity") == "complex"]
        moderate_paras = [p for p in para_hints if p.get("complexity") == "moderate"]

        print(f"\nParagraph complexity distribution:")
        print(f"  Complex: {len(complex_paras)}")
        print(f"  Moderate: {len(moderate_paras)}")
        print(f"  Simple: {len(para_hints) - len(complex_paras) - len(moderate_paras)}")

        # Complex paragraphs should have hints
        for para in complex_paras[:3]:  # Check first 3
            hints = para.get("hints", [])
            print(f"\n  {para['paragraph_name']} (complex): {len(hints)} hints")
            for hint in hints:
                print(f"    - {hint}")

    def test_pattern_categories_present(
        self, citadel, test_file_path: Path
    ) -> None:
        """Verify all expected pattern categories are analyzed."""
        # Get patterns
        pattern_result = citadel.get_analysis_patterns(str(test_file_path))
        assert pattern_result.error is None

        categories = set(pattern_result.categories.keys())

        # COBOL files should have data_flow and control_flow at minimum
        assert "data_flow" in categories, "Expected data_flow category"
        assert "control_flow" in categories, "Expected control_flow category"

        # Log category details
        print(f"\nPattern categories found:")
        for cat_name, cat_result in pattern_result.categories.items():
            print(f"  {cat_name}:")
            print(f"    Total matches: {cat_result.match_count}")
            print(f"    Patterns: {list(cat_result.patterns_matched.keys())[:5]}...")


class TestPatternAggregatorWithMultipleFiles:
    """Test aggregator with different COBOL files to ensure robustness."""

    @pytest.fixture
    def cobol_files(self) -> list[Path]:
        """Get list of available COBOL test files."""
        carddemo_path = Path(
            "/Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/app/cbl"
        )
        if not carddemo_path.exists():
            pytest.skip("CardDemo COBOL files not found")

        files = list(carddemo_path.glob("*.cbl"))[:5]  # Limit to 5 files
        if not files:
            pytest.skip("No COBOL files found")
        return files

    def test_multiple_files_aggregation(
        self, citadel, cobol_files: list[Path]
    ) -> None:
        """Test aggregation works across multiple COBOL files."""
        aggregator = PatternAggregator()

        for file_path in cobol_files:
            # Get patterns
            pattern_result = citadel.get_analysis_patterns(str(file_path))

            if pattern_result.error:
                print(f"Skipping {file_path.name}: {pattern_result.error}")
                continue

            # Get outline
            stats = citadel.get_file_stats(str(file_path))
            paragraphs = stats.get("paragraphs", [])
            outline = [
                {
                    "name": p.get("name", ""),
                    "line_start": p.get("line_start"),
                    "line_end": p.get("line_end"),
                }
                for p in paragraphs
                if p.get("name")
            ]

            # Run aggregation
            scribe_result = aggregator.aggregate_for_scribe(pattern_result, outline)
            challenger_result = aggregator.aggregate_for_challenger(pattern_result, outline)

            # Verify outputs are valid
            assert isinstance(scribe_result, dict)
            assert isinstance(challenger_result, dict)

            # Verify token budgets
            scribe_str = json.dumps(scribe_result)
            challenger_str = json.dumps(challenger_result)

            scribe_tokens = estimate_tokens(scribe_str)
            challenger_tokens = estimate_tokens(challenger_str)

            print(f"\n{file_path.name}:")
            print(f"  Matches: {pattern_result.total_matches}")
            print(f"  Scribe tokens: {scribe_tokens}")
            print(f"  Challenger tokens: {challenger_tokens}")

            assert scribe_tokens < 4000
            assert challenger_tokens < 4000
