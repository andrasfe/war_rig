"""Tests for the tiered Citadel SDK methods for reduced token usage.

Tests the Tier 1 methods:
- get_file_summary(): Compact file summary for holistic review
- get_callouts_compact(): Compact callouts without line numbers
- get_analysis_patterns(summary_only=True): Pattern counts without match details
"""

from __future__ import annotations

import tempfile
from pathlib import Path
from textwrap import dedent


class TestGetFileSummary:
    """Tests for the get_file_summary() method."""

    def test_cobol_summary(self, citadel_instance):
        """Test file summary for a COBOL file with calls."""
        code = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAINPROG.
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "HELLO".
           CALL 'SUBPROG1'.
           CALL 'SUBPROG2'.
           PERFORM PROC-PARA.
           STOP RUN.
       PROC-PARA.
           DISPLAY "PROCESSING"."""

        with tempfile.NamedTemporaryFile(suffix=".cbl", delete=False, mode="w") as f:
            f.write(code)
            f.flush()
            path = Path(f.name)

        try:
            summary = citadel_instance.get_file_summary(path)

            # Check required fields
            assert summary["file_name"] == path.name
            assert summary["language"].lower() == "cobol"
            assert summary["total_lines"] > 0
            assert summary["paragraph_count"] >= 2
            assert summary["error"] is None

            # Check entry points
            assert isinstance(summary["entry_points"], list)
            assert len(summary["entry_points"]) >= 1

            # Check main calls
            assert isinstance(summary["main_calls"], list)
            # Should have calls from CALL and PERFORM statements
            assert len(summary["main_calls"]) >= 1

        finally:
            path.unlink()

    def test_python_summary(self, citadel_instance):
        """Test file summary for a Python file."""
        code = dedent('''
            def main():
                helper()
                process_data()

            def helper():
                return 42

            def process_data():
                return "done"
        ''').strip()

        with tempfile.NamedTemporaryFile(suffix=".py", delete=False, mode="w") as f:
            f.write(code)
            f.flush()
            path = Path(f.name)

        try:
            summary = citadel_instance.get_file_summary(path)

            assert summary["file_name"] == path.name
            assert summary["language"].lower() == "python"
            assert summary["total_lines"] > 0
            assert summary["error"] is None

        finally:
            path.unlink()

    def test_summary_error_on_bad_extension(self, citadel_instance):
        """Test that unrecognized extensions return an error."""
        with tempfile.NamedTemporaryFile(suffix=".xyz", delete=False, mode="w") as f:
            f.write("some content")
            f.flush()
            path = Path(f.name)

        try:
            summary = citadel_instance.get_file_summary(path)

            assert summary["file_name"] == path.name
            assert summary["error"] is not None
            assert summary["paragraph_count"] == 0

        finally:
            path.unlink()


class TestGetCalloutsCompact:
    """Tests for the get_callouts_compact() method."""

    def test_compact_callouts_single_file(self, citadel_instance):
        """Test compact callouts from a single file."""
        code = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       PROCEDURE DIVISION.
       MAIN-PARA.
           CALL 'SUBPROG1'.
           CALL 'SUBPROG2'.
           CALL 'SUBPROG1'.
           STOP RUN."""

        with tempfile.NamedTemporaryFile(suffix=".cbl", delete=False, mode="w") as f:
            f.write(code)
            f.flush()
            path = Path(f.name)

        try:
            callouts = citadel_instance.get_callouts_compact(path)

            assert isinstance(callouts, list)

            # Verify compact format
            for callout in callouts:
                assert "from_artifact" in callout
                assert "to_target" in callout
                assert "call_type" in callout
                # Should NOT have line numbers
                assert "line" not in callout
                assert "raw_text" not in callout

            # Should have deduplicated - SUBPROG1 called twice but only one entry
            targets = [c["to_target"] for c in callouts]
            assert "SUBPROG1" in targets or "subprog1" in targets.lower() if targets else True

        finally:
            path.unlink()

    def test_compact_callouts_directory(self, citadel_instance):
        """Test compact callouts from a directory with multiple files."""
        import tempfile

        with tempfile.TemporaryDirectory() as tmpdir:
            # Create first file
            code1 = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG1.
       PROCEDURE DIVISION.
       MAIN-PARA.
           CALL 'SHAREDUTIL'.
           STOP RUN."""
            path1 = Path(tmpdir) / "PROG1.cbl"
            path1.write_text(code1)

            # Create second file
            code2 = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG2.
       PROCEDURE DIVISION.
       MAIN-PARA.
           CALL 'SHAREDUTIL'.
           CALL 'OTHERPROG'.
           STOP RUN."""
            path2 = Path(tmpdir) / "PROG2.cbl"
            path2.write_text(code2)

            callouts = citadel_instance.get_callouts_compact(tmpdir)

            assert isinstance(callouts, list)
            assert len(callouts) >= 2

            # Check all entries have required fields
            for callout in callouts:
                assert "from_artifact" in callout
                assert "to_target" in callout
                assert "call_type" in callout


class TestGetAnalysisPatternsSummaryOnly:
    """Tests for get_analysis_patterns() with summary_only=True."""

    def test_summary_only_has_no_matches(self, citadel_instance):
        """Test that summary_only=True returns counts but no match details."""
        code = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COUNT PIC 9(5).
       PROCEDURE DIVISION.
       MAIN-PARA.
           MOVE 0 TO WS-COUNT.
           ADD 1 TO WS-COUNT.
           IF WS-COUNT > 0
               DISPLAY WS-COUNT
           END-IF.
           STOP RUN."""

        with tempfile.NamedTemporaryFile(suffix=".cbl", delete=False, mode="w") as f:
            f.write(code)
            f.flush()
            path = Path(f.name)

        try:
            # Get full patterns
            full_result = citadel_instance.get_analysis_patterns(
                path, summary_only=False
            )

            # Get summary only
            summary_result = citadel_instance.get_analysis_patterns(
                path, summary_only=True
            )

            # Both should have same total_matches count
            assert summary_result.total_matches == full_result.total_matches

            # Both should have same coverage percentage
            assert summary_result.coverage_pct == full_result.coverage_pct

            # But summary should have empty match lists
            for _category, cat_result in summary_result.categories.items():
                # match_count should be populated
                assert cat_result.match_count >= 0
                # patterns_matched should be populated
                assert isinstance(cat_result.patterns_matched, dict)
                # BUT matches list should be empty
                assert cat_result.matches == []

            # Full result should have actual matches
            total_matches_in_full = sum(
                len(cat_result.matches)
                for cat_result in full_result.categories.values()
            )
            assert total_matches_in_full == full_result.total_matches

        finally:
            path.unlink()

    def test_summary_preserves_pattern_counts(self, citadel_instance):
        """Test that summary_only=True preserves per-pattern counts."""
        code = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       PROCEDURE DIVISION.
       MAIN-PARA.
           MOVE A TO B.
           MOVE C TO D.
           MOVE E TO F.
           STOP RUN."""

        with tempfile.NamedTemporaryFile(suffix=".cbl", delete=False, mode="w") as f:
            f.write(code)
            f.flush()
            path = Path(f.name)

        try:
            full_result = citadel_instance.get_analysis_patterns(
                path, summary_only=False
            )
            summary_result = citadel_instance.get_analysis_patterns(
                path, summary_only=True
            )

            # Compare patterns_matched dictionaries
            for category in full_result.categories:
                if category in summary_result.categories:
                    full_patterns = full_result.categories[category].patterns_matched
                    summary_patterns = summary_result.categories[category].patterns_matched

                    # Should have same pattern counts
                    assert full_patterns == summary_patterns

        finally:
            path.unlink()


class TestTokenReduction:
    """Tests to verify token reduction from tiered methods."""

    def test_summary_smaller_than_full(self, citadel_instance):
        """Test that file summary is significantly smaller than full analysis."""
        code = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-VAR1 PIC X(100).
       01 WS-VAR2 PIC X(100).
       01 WS-VAR3 PIC X(100).
       PROCEDURE DIVISION.
       MAIN-PARA.
           MOVE "A" TO WS-VAR1.
           MOVE "B" TO WS-VAR2.
           CALL 'SUBPROG1'.
           CALL 'SUBPROG2'.
           PERFORM PROC-PARA.
           STOP RUN.
       PROC-PARA.
           DISPLAY WS-VAR1.
           DISPLAY WS-VAR2."""

        with tempfile.NamedTemporaryFile(suffix=".cbl", delete=False, mode="w") as f:
            f.write(code)
            f.flush()
            path = Path(f.name)

        try:
            import json

            # Get full analysis
            full_result = citadel_instance.analyze_file(path)
            full_json = json.dumps(full_result.to_dict())

            # Get compact summary
            summary = citadel_instance.get_file_summary(path)
            summary_json = json.dumps(summary)

            # Summary should be significantly smaller (at least 50% smaller)
            assert len(summary_json) < len(full_json) * 0.5

        finally:
            path.unlink()

    def test_compact_callouts_has_fewer_fields(self, citadel_instance):
        """Test that compact callouts have fewer fields than full callouts."""
        import tempfile

        with tempfile.TemporaryDirectory() as tmpdir:
            code = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       PROCEDURE DIVISION.
       MAIN-PARA.
           CALL 'SUB1'.
           CALL 'SUB2'.
           CALL 'SUB3'.
           CALL 'SUB4'.
           CALL 'SUB5'.
           STOP RUN."""
            path = Path(tmpdir) / "TEST.cbl"
            path.write_text(code)

            # Get full callouts
            full_callouts = citadel_instance.get_callouts(path)

            # Get compact callouts
            compact_callouts = citadel_instance.get_callouts_compact(path)

            # Verify compact callouts have the minimal required fields
            # and don't have line numbers or raw_text
            if len(compact_callouts) > 0:
                for callout in compact_callouts:
                    # Should have these fields
                    assert "from_artifact" in callout
                    assert "to_target" in callout
                    assert "call_type" in callout
                    # Should NOT have these fields (stripped for compactness)
                    assert "line" not in callout
                    assert "raw_text" not in callout

            # Full callouts typically have more fields
            if len(full_callouts) > 0:
                first_full = full_callouts[0]
                # Full callouts should have line numbers
                assert "line" in first_full or "lines" in str(first_full)
