"""Tests for the ProLeap COBOL preprocessor."""

from __future__ import annotations

from pathlib import Path

import pytest

from citadel.cobol.proleap_preprocessor import (
    LogicalLine,
    PreprocessResult,
    _build_logical_lines,
    _make_comment_line,
    _make_data_line,
    _transform_exec_sql_include,
    _transform_sql_type_is,
    preprocess_for_proleap,
)
from citadel.cobol.source_reader import COL_INDICATOR


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _cobol_line(text: str, indicator: str = " ") -> str:
    """Build a fixed-format COBOL line with text starting at Area A (col 8)."""
    seq = "      "  # cols 1-6
    # Pad text to fill up to col 72
    content = text.ljust(65)  # 72 - 7 = 65
    return f"{seq}{indicator}{content}"


def _cobol_continuation(text: str) -> str:
    """Build a continuation line (column 7 = '-')."""
    return _cobol_line(text, indicator="-")


def _cobol_comment(text: str) -> str:
    """Build a comment line (column 7 = '*')."""
    return _cobol_line(text, indicator="*")


# ---------------------------------------------------------------------------
# TestLogicalLines
# ---------------------------------------------------------------------------


class TestLogicalLines:
    """Tests for _build_logical_lines."""

    def test_simple_lines(self) -> None:
        lines = [
            _cobol_line("MOVE A TO B."),
            _cobol_line("DISPLAY 'HELLO'."),
        ]
        logical = _build_logical_lines(lines)
        assert len(logical) == 2
        assert "MOVE A TO B." in logical[0].text
        assert "DISPLAY 'HELLO'." in logical[1].text

    def test_continuation_grouping(self) -> None:
        lines = [
            _cobol_line("MOVE 'VERY-LONG-VALUE-TH"),
            _cobol_continuation("AT-CONTINUES' TO WS-VAR."),
        ]
        logical = _build_logical_lines(lines)
        assert len(logical) == 1
        assert logical[0].physical_indices == [0, 1]
        assert "VERY-LONG-VALUE-TH" in logical[0].text
        assert "AT-CONTINUES" in logical[0].text

    def test_comment_line_separate(self) -> None:
        lines = [
            _cobol_comment("THIS IS A COMMENT"),
            _cobol_line("MOVE X TO Y."),
        ]
        logical = _build_logical_lines(lines)
        assert len(logical) == 2
        # Comment line should be its own logical line
        assert logical[0].physical_indices == [0]
        assert logical[1].physical_indices == [1]

    def test_short_lines(self) -> None:
        """Lines shorter than 7 chars should not crash."""
        lines = ["", "SHORT"]
        logical = _build_logical_lines(lines)
        assert len(logical) == 2

    def test_multiple_continuations(self) -> None:
        lines = [
            _cobol_line("01 WS-LONG-VAR SQL"),
            _cobol_continuation("TYPE IS RESULT-SET-LO"),
            _cobol_continuation("CATOR VARYING."),
        ]
        logical = _build_logical_lines(lines)
        assert len(logical) == 1
        assert logical[0].physical_indices == [0, 1, 2]


# ---------------------------------------------------------------------------
# TestSqlTypeIsTransform
# ---------------------------------------------------------------------------


class TestSqlTypeIsTransform:
    """Tests for _transform_sql_type_is."""

    def test_single_line_unsupported(self) -> None:
        lines = [_cobol_line("01 WS-LOC SQL TYPE IS RESULT-SET-LOCATOR VARYING.")]
        logical = _build_logical_lines(lines)
        result, descs = _transform_sql_type_is(lines, logical)
        assert len(result) == 1
        assert "PIC X(4)" in result[0]
        assert "01" in result[0]
        assert "WS-LOC" in result[0]
        assert len(descs) == 1
        assert "RESULT-SET-LOCATOR" in descs[0]

    def test_multi_line_continuation(self) -> None:
        lines = [
            _cobol_line("01 WS-LOC SQL TYPE IS RESULT-SET-"),
            _cobol_continuation("LOCATOR VARYING."),
        ]
        logical = _build_logical_lines(lines)
        result, descs = _transform_sql_type_is(lines, logical)
        assert len(result) == 2  # line count preserved
        assert "PIC X(4)" in result[0]
        assert result[1][COL_INDICATOR] == "*"  # continuation became comment
        assert len(descs) == 1

    def test_known_type_left_alone(self) -> None:
        lines = [_cobol_line("01 WS-DATA SQL TYPE IS BLOB.")]
        logical = _build_logical_lines(lines)
        result, descs = _transform_sql_type_is(lines, logical)
        assert result == lines
        assert descs == []

    def test_known_type_clob(self) -> None:
        lines = [_cobol_line("01 WS-DATA SQL TYPE IS CLOB.")]
        logical = _build_logical_lines(lines)
        result, descs = _transform_sql_type_is(lines, logical)
        assert result == lines
        assert descs == []

    def test_level_and_name_preserved(self) -> None:
        lines = [_cobol_line("05 MY-SPECIAL-VAR SQL TYPE IS ROWID.")]
        logical = _build_logical_lines(lines)
        result, descs = _transform_sql_type_is(lines, logical)
        assert "05" in result[0]
        assert "MY-SPECIAL-VAR" in result[0]

    def test_line_count_preserved(self) -> None:
        lines = [
            _cobol_line("01 WS-LOC SQL TYPE IS RESULT-SET-"),
            _cobol_continuation("LOCATOR VARYING."),
            _cobol_line("01 WS-OTHER PIC X(10)."),
        ]
        logical = _build_logical_lines(lines)
        result, descs = _transform_sql_type_is(lines, logical)
        assert len(result) == len(lines)

    def test_comment_line_not_touched(self) -> None:
        lines = [_cobol_comment("01 WS-LOC SQL TYPE IS RESULT-SET-LOCATOR VARYING.")]
        logical = _build_logical_lines(lines)
        result, descs = _transform_sql_type_is(lines, logical)
        assert result == lines
        assert descs == []


# ---------------------------------------------------------------------------
# TestExecSqlIncludeTransform
# ---------------------------------------------------------------------------


class TestExecSqlIncludeTransform:
    """Tests for _transform_exec_sql_include."""

    def test_single_line(self) -> None:
        lines = [_cobol_line("EXEC SQL INCLUDE SQLCA END-EXEC.")]
        logical = _build_logical_lines(lines)
        result, descs = _transform_exec_sql_include(lines, logical)
        assert len(result) == 1
        assert result[0][COL_INDICATOR] == "*"
        assert len(descs) == 1
        assert "SQLCA" in descs[0]

    def test_multi_line(self) -> None:
        lines = [
            _cobol_line("EXEC SQL INCLUDE SQLCA END-"),
            _cobol_continuation("EXEC."),
        ]
        logical = _build_logical_lines(lines)
        result, descs = _transform_exec_sql_include(lines, logical)
        assert len(result) == 2
        assert result[0][COL_INDICATOR] == "*"
        assert result[1][COL_INDICATOR] == "*"
        assert len(descs) == 1

    def test_continuation_split_lude(self) -> None:
        """The classic INC + LUDE continuation error scenario."""
        lines = [
            _cobol_line("EXEC SQL INC"),
            _cobol_continuation("LUDE SQLCA END-EXEC."),
        ]
        logical = _build_logical_lines(lines)
        result, descs = _transform_exec_sql_include(lines, logical)
        assert len(result) == 2
        assert result[0][COL_INDICATOR] == "*"
        assert result[1][COL_INDICATOR] == "*"
        assert "SQLCA" in descs[0]

    def test_regular_exec_sql_not_touched(self) -> None:
        lines = [_cobol_line("EXEC SQL SELECT * FROM TABLE END-EXEC.")]
        logical = _build_logical_lines(lines)
        result, descs = _transform_exec_sql_include(lines, logical)
        assert result == lines
        assert descs == []

    def test_line_count_preserved(self) -> None:
        lines = [
            _cobol_line("EXEC SQL"),
            _cobol_continuation("INCLUDE DCLACCT END-EXEC."),
            _cobol_line("MOVE X TO Y."),
        ]
        logical = _build_logical_lines(lines)
        result, descs = _transform_exec_sql_include(lines, logical)
        assert len(result) == len(lines)


# ---------------------------------------------------------------------------
# TestPreprocessForProleap
# ---------------------------------------------------------------------------


class TestPreprocessForProleap:
    """Tests for the full preprocess_for_proleap pipeline."""

    def test_full_pipeline(self, tmp_path: Path) -> None:
        source = tmp_path / "TEST.cbl"
        source.write_text(
            "\n".join([
                _cobol_line("IDENTIFICATION DIVISION."),
                _cobol_line("01 WS-LOC SQL TYPE IS RESULT-SET-LOCATOR VARYING."),
                _cobol_line("EXEC SQL INCLUDE SQLCA END-EXEC."),
                _cobol_line("MOVE A TO B."),
            ])
            + "\n"
        )
        pp = preprocess_for_proleap(source)
        try:
            assert pp.output_path.exists()
            assert pp.original_path == source
            assert pp.line_count == 4
            assert len(pp.transformations) == 2

            output_lines = pp.output_path.read_text().splitlines()
            assert len(output_lines) == 4
            # SQL TYPE IS replaced
            assert "PIC X(4)" in output_lines[1]
            # EXEC SQL INCLUDE commented out
            assert output_lines[2][COL_INDICATOR] == "*"
            # MOVE untouched
            assert "MOVE A TO B" in output_lines[3]
        finally:
            pp.output_path.unlink(missing_ok=True)

    def test_temp_file_creation(self, tmp_path: Path) -> None:
        source = tmp_path / "SIMPLE.cbl"
        source.write_text(_cobol_line("MOVE A TO B.") + "\n")
        pp = preprocess_for_proleap(source)
        try:
            assert pp.output_path.exists()
            assert pp.output_path.suffix == ".cbl"
            assert "proleap_pp_" in pp.output_path.name
        finally:
            pp.output_path.unlink(missing_ok=True)

    def test_no_transforms_passthrough(self, tmp_path: Path) -> None:
        source = tmp_path / "CLEAN.cbl"
        original_content = "\n".join([
            _cobol_line("IDENTIFICATION DIVISION."),
            _cobol_line("MOVE A TO B."),
        ]) + "\n"
        source.write_text(original_content)
        pp = preprocess_for_proleap(source)
        try:
            assert pp.transformations == []
            assert pp.line_count == 2
            output = pp.output_path.read_text()
            # Should be functionally identical
            assert "IDENTIFICATION DIVISION" in output
            assert "MOVE A TO B" in output
        finally:
            pp.output_path.unlink(missing_ok=True)

    def test_multiple_transforms_combined(self, tmp_path: Path) -> None:
        source = tmp_path / "MULTI.cbl"
        source.write_text(
            "\n".join([
                _cobol_line("01 WS-A SQL TYPE IS ROWID."),
                _cobol_line("01 WS-B SQL TYPE IS XML-CLOB."),
                _cobol_line("EXEC SQL INCLUDE DCLACCT END-EXEC."),
                _cobol_line("EXEC SQL INCLUDE SQLCA END-EXEC."),
            ])
            + "\n"
        )
        pp = preprocess_for_proleap(source)
        try:
            assert len(pp.transformations) == 4
            assert pp.line_count == 4
        finally:
            pp.output_path.unlink(missing_ok=True)

    def test_transformations_list_populated(self, tmp_path: Path) -> None:
        source = tmp_path / "DESC.cbl"
        source.write_text(
            _cobol_line("01 WS-LOC SQL TYPE IS RESULT-SET-LOCATOR VARYING.")
            + "\n"
        )
        pp = preprocess_for_proleap(source)
        try:
            assert len(pp.transformations) == 1
            assert "RESULT-SET-LOCATOR" in pp.transformations[0]
            assert "WS-LOC" in pp.transformations[0]
        finally:
            pp.output_path.unlink(missing_ok=True)


# ---------------------------------------------------------------------------
# TestEdgeCases
# ---------------------------------------------------------------------------


class TestEdgeCases:
    """Edge case tests."""

    def test_empty_file(self, tmp_path: Path) -> None:
        source = tmp_path / "EMPTY.cbl"
        source.write_text("")
        pp = preprocess_for_proleap(source)
        try:
            assert pp.line_count == 0
            assert pp.transformations == []
        finally:
            pp.output_path.unlink(missing_ok=True)

    def test_short_lines(self) -> None:
        """Lines shorter than column 7 should not crash."""
        lines = ["", "SHORT", "12345"]
        logical = _build_logical_lines(lines)
        # Should not raise
        assert len(logical) >= 1

    def test_column_72_boundary(self) -> None:
        """Content at column 72 boundary should be handled correctly."""
        # Build a line that is exactly 72 chars
        line = "      " + " " + "A" * 65  # 6 + 1 + 65 = 72
        assert len(line) == 72
        lines = [line]
        logical = _build_logical_lines(lines)
        assert len(logical) == 1

    def test_make_comment_line(self) -> None:
        original = _cobol_line("MOVE A TO B.")
        commented = _make_comment_line(original)
        assert commented[COL_INDICATOR] == "*"
        # Rest of line preserved
        assert commented[COL_INDICATOR + 1:] == original[COL_INDICATOR + 1:]

    def test_make_data_line(self) -> None:
        line = _make_data_line("01", "WS-VAR", "PIC X(4).")
        assert "01" in line
        assert "WS-VAR" in line
        assert "PIC X(4)." in line
        # Column 7 should be space (not indicator)
        assert line[COL_INDICATOR] == " "
