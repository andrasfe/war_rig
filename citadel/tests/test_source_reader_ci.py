"""Tests for case-insensitive copybook lookup in SourceReader."""

from pathlib import Path

import pytest

from citadel.cobol.source_reader import CopybookNotFoundError, SourceReader


@pytest.fixture
def copybook_dir(tmp_path: Path) -> Path:
    d = tmp_path / "cpys"
    d.mkdir()
    return d


def _make_cobol(tmp_path: Path, name: str, lines: list[str]) -> Path:
    """Write a minimal fixed-format COBOL file."""
    path = tmp_path / name
    formatted = []
    for line in lines:
        # Pad to fixed-format: 6-char seq, space indicator, then content
        formatted.append(f"000000 {line}")
    path.write_text("\n".join(formatted), encoding="utf-8")
    return path


class TestCaseInsensitiveLookup:
    def test_exact_match_still_works(self, tmp_path: Path, copybook_dir: Path) -> None:
        (copybook_dir / "WSCOPY.cpy").write_text("      *COPY", encoding="utf-8")
        reader = SourceReader(
            copybook_dirs=[str(copybook_dir)],
            skip_missing_copybooks=True,
        )
        result = reader._find_copybook("WSCOPY")
        assert result.name == "WSCOPY.cpy"

    def test_case_insensitive_fallback(self, tmp_path: Path, copybook_dir: Path) -> None:
        # File on disk is lowercase, COPY statement uses uppercase
        (copybook_dir / "wscpybbk.cpy").write_text("      *COPY", encoding="utf-8")
        reader = SourceReader(
            copybook_dirs=[str(copybook_dir)],
            skip_missing_copybooks=True,
        )
        result = reader._find_copybook("WSCPYBBK")
        assert result.name == "wscpybbk.cpy"

    def test_case_insensitive_reverse(self, tmp_path: Path, copybook_dir: Path) -> None:
        # File on disk is uppercase, COPY uses lowercase
        (copybook_dir / "BIGCOPY.CPY").write_text("      *COPY", encoding="utf-8")
        reader = SourceReader(
            copybook_dirs=[str(copybook_dir)],
            skip_missing_copybooks=True,
        )
        result = reader._find_copybook("bigcopy")
        assert result.name == "BIGCOPY.CPY"

    def test_not_found_raises(self, tmp_path: Path, copybook_dir: Path) -> None:
        reader = SourceReader(
            copybook_dirs=[str(copybook_dir)],
            skip_missing_copybooks=False,
        )
        with pytest.raises(CopybookNotFoundError):
            reader._find_copybook("NONEXIST")

    def test_dir_cache_populated_on_ci_lookup(self, tmp_path: Path, copybook_dir: Path) -> None:
        # File is lowercase, lookup is uppercase — forces case-insensitive path
        (copybook_dir / "mycopy.cpy").write_text("      *COPY", encoding="utf-8")
        reader = SourceReader(copybook_dirs=[str(copybook_dir)])
        reader._find_copybook("MYCOPY")
        assert str(copybook_dir) in reader._dir_cache

    def test_missing_copybooks_tracked(self, tmp_path: Path, copybook_dir: Path) -> None:
        src = _make_cobol(tmp_path, "TEST.cbl", [
            "IDENTIFICATION DIVISION.",
            "PROGRAM-ID. TEST.",
            "PROCEDURE DIVISION.",
            "MAIN-PARA.",
            "    COPY MISSING-CPY.",
            "    STOP RUN.",
        ])
        reader = SourceReader(
            copybook_dirs=[str(copybook_dir)],
            skip_missing_copybooks=True,
        )
        reader.read(str(src))
        assert "MISSING-CPY" in reader.missing_copybooks

    def test_resolved_copybook_not_in_missing(self, tmp_path: Path, copybook_dir: Path) -> None:
        (copybook_dir / "found.cpy").write_text(
            "000000     01 WS-FIELD PIC X.", encoding="utf-8"
        )
        src = _make_cobol(tmp_path, "TEST.cbl", [
            "IDENTIFICATION DIVISION.",
            "PROGRAM-ID. TEST.",
            "DATA DIVISION.",
            "WORKING-STORAGE SECTION.",
            "    COPY FOUND.",
            "PROCEDURE DIVISION.",
            "MAIN-PARA.",
            "    STOP RUN.",
        ])
        reader = SourceReader(
            copybook_dirs=[str(copybook_dir)],
            skip_missing_copybooks=True,
        )
        result = reader.read(str(src))
        assert reader.missing_copybooks == []
        assert "FOUND" in result.copybooks_resolved
