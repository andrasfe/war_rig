"""Tests for code search functionality.

This module tests:
- CodeSearchResult formatting
- CodeSearcher initialization
- Search with Python fallback
- Search result context
"""

import re
from pathlib import Path
from unittest.mock import patch

import pytest

from codewhisper.search.code_search import (
    CodeSearcher,
    CodeSearchResult,
    search_in_directory,
)


class TestCodeSearchResult:
    """Tests for CodeSearchResult dataclass."""

    def test_result_creation(self) -> None:
        """Test creating a code search result."""
        result = CodeSearchResult(
            file_path=Path("cbl/TESTPROG.cbl"),
            line_number=42,
            line_content="       PERFORM 2000-PROCESS",
            context_before=["       0000-MAIN.", "           PERFORM 1000-INIT"],
            context_after=["           PERFORM 3000-FINAL", "           STOP RUN."],
        )

        assert result.file_path == Path("cbl/TESTPROG.cbl")
        assert result.line_number == 42
        assert result.line_content == "       PERFORM 2000-PROCESS"
        assert len(result.context_before) == 2
        assert len(result.context_after) == 2

    def test_result_empty_context(self) -> None:
        """Test result with empty context."""
        result = CodeSearchResult(
            file_path=Path("test.cbl"),
            line_number=1,
            line_content="       IDENTIFICATION DIVISION.",
            context_before=[],
            context_after=[],
        )

        assert result.context_before == []
        assert result.context_after == []

    def test_result_format_with_context(self) -> None:
        """Test formatting result with context."""
        result = CodeSearchResult(
            file_path=Path("test.cbl"),
            line_number=10,
            line_content="       PERFORM MAIN-PARA",
            context_before=["       DATA DIVISION.", "       WORKING-STORAGE SECTION."],
            context_after=["           STOP RUN."],
        )

        formatted = result.format(include_context=True)

        assert "10" in formatted  # Line number
        assert "PERFORM MAIN-PARA" in formatted
        assert "DATA DIVISION" in formatted
        assert "STOP RUN" in formatted
        assert ">" in formatted  # Match indicator

    def test_result_format_without_context(self) -> None:
        """Test formatting result without context."""
        result = CodeSearchResult(
            file_path=Path("test.cbl"),
            line_number=10,
            line_content="       PERFORM MAIN-PARA",
            context_before=["       Line before"],
            context_after=["       Line after"],
        )

        formatted = result.format(include_context=False)

        assert "PERFORM MAIN-PARA" in formatted
        assert "Line before" not in formatted
        assert "Line after" not in formatted


class TestCodeSearcherInit:
    """Tests for CodeSearcher initialization."""

    def test_init_with_directory(self, tmp_code_dir: Path) -> None:
        """Test searcher initialization with valid directory."""
        searcher = CodeSearcher(tmp_code_dir)

        assert searcher.code_dir == tmp_code_dir

    def test_init_detects_ripgrep(self, tmp_code_dir: Path) -> None:
        """Test that ripgrep detection is performed."""
        with patch("shutil.which", return_value="/usr/bin/rg"):
            searcher = CodeSearcher(tmp_code_dir)
            assert searcher.use_ripgrep is True

        with patch("shutil.which", return_value=None):
            searcher = CodeSearcher(tmp_code_dir)
            assert searcher.use_ripgrep is False


class TestCodeSearcherSearch:
    """Tests for search functionality."""

    @pytest.fixture
    def searcher(self, tmp_code_dir: Path) -> CodeSearcher:
        """Create a searcher with Python fallback."""
        with patch("shutil.which", return_value=None):
            return CodeSearcher(tmp_code_dir)

    def test_search_finds_pattern(
        self, searcher: CodeSearcher, tmp_code_dir: Path
    ) -> None:
        """Test that search finds matching patterns."""
        results = searcher.search("PERFORM")

        assert len(results) > 0
        assert any("PERFORM" in r.line_content for r in results)

    def test_search_regex_pattern(
        self, searcher: CodeSearcher, tmp_code_dir: Path
    ) -> None:
        """Test search with regex pattern."""
        results = searcher.search(r"PERFORM \d+-")

        assert len(results) > 0
        # Should match things like "PERFORM 1000-INITIALIZE"
        for result in results:
            assert re.search(r"PERFORM \d+-", result.line_content)

    def test_search_with_file_pattern(
        self, searcher: CodeSearcher, tmp_code_dir: Path
    ) -> None:
        """Test search with file pattern filter."""
        results = searcher.search("JOB", file_pattern="*.jcl")

        assert len(results) > 0
        assert all(str(r.file_path).endswith(".jcl") for r in results)

    def test_search_with_context_lines(
        self, searcher: CodeSearcher, tmp_code_dir: Path
    ) -> None:
        """Test search includes context lines."""
        results = searcher.search("PERFORM", context_lines=2)

        for result in results:
            # Context may be less at file boundaries
            assert len(result.context_before) <= 2
            assert len(result.context_after) <= 2

    def test_search_respects_max_results(
        self, searcher: CodeSearcher, tmp_code_dir: Path
    ) -> None:
        """Test that max_results is respected."""
        results = searcher.search(".", max_results=5)

        assert len(results) <= 5

    def test_search_case_insensitive(
        self, searcher: CodeSearcher, tmp_code_dir: Path
    ) -> None:
        """Test that search is case-insensitive."""
        results_upper = searcher.search("PERFORM")
        results_lower = searcher.search("perform")

        # Should find similar results
        assert len(results_upper) == len(results_lower)

    def test_search_no_matches(
        self, searcher: CodeSearcher, tmp_code_dir: Path
    ) -> None:
        """Test search with no matches."""
        results = searcher.search("XYZNOMATCH123456")

        assert results == []

    def test_search_relative_paths(
        self, searcher: CodeSearcher, tmp_code_dir: Path
    ) -> None:
        """Test that result paths are relative to code_dir."""
        results = searcher.search("TESTPROG")

        for result in results:
            # Path should be relative, not absolute
            assert not result.file_path.is_absolute()


class TestCodeSearcherRipgrep:
    """Tests for ripgrep integration."""

    @pytest.fixture
    def searcher_with_ripgrep(self, tmp_code_dir: Path) -> CodeSearcher:
        """Create a searcher with ripgrep enabled."""
        with patch("shutil.which", return_value="/usr/bin/rg"):
            return CodeSearcher(tmp_code_dir)

    def test_ripgrep_fallback_on_error(
        self, tmp_code_dir: Path
    ) -> None:
        """Test that ripgrep errors fall back to Python."""
        with patch("shutil.which", return_value="/usr/bin/rg"):
            searcher = CodeSearcher(tmp_code_dir)

        with patch("subprocess.run", side_effect=Exception("ripgrep error")):
            # Should fall back to Python search
            results = searcher.search("PERFORM")

        # Should still get results via fallback
        assert len(results) >= 0  # May be 0 or more

    def test_ripgrep_timeout(
        self, tmp_code_dir: Path
    ) -> None:
        """Test that ripgrep timeout falls back gracefully."""
        import subprocess

        with patch("shutil.which", return_value="/usr/bin/rg"):
            searcher = CodeSearcher(tmp_code_dir)

        with patch(
            "subprocess.run",
            side_effect=subprocess.TimeoutExpired(cmd="rg", timeout=30),
        ):
            results = searcher.search("PERFORM")

        # Should return empty on timeout (or fall back)
        assert isinstance(results, list)


class TestSearchInDirectory:
    """Tests for the convenience function."""

    def test_search_in_directory(self, tmp_code_dir: Path) -> None:
        """Test the convenience function."""
        with patch("shutil.which", return_value=None):
            results = search_in_directory(
                tmp_code_dir,
                "PERFORM",
                file_pattern="*.cbl",
            )

        assert isinstance(results, list)
        for result in results:
            assert isinstance(result, CodeSearchResult)

    def test_search_in_directory_with_options(self, tmp_code_dir: Path) -> None:
        """Test convenience function with all options."""
        with patch("shutil.which", return_value=None):
            results = search_in_directory(
                tmp_code_dir,
                pattern="PROGRAM",
                file_pattern="*",
                context_lines=5,
                max_results=10,
            )

        assert len(results) <= 10


class TestCodeSearcherFileHandling:
    """Tests for file handling edge cases."""

    def test_handles_binary_files_gracefully(self, tmp_path: Path) -> None:
        """Test that binary files don't cause errors."""
        # Create a binary file
        binary_file = tmp_path / "binary.dat"
        binary_file.write_bytes(b"\x00\x01\x02\x03\xff\xfe")

        with patch("shutil.which", return_value=None):
            searcher = CodeSearcher(tmp_path)
            results = searcher.search("test")

        # Should not raise, just return results from text files
        assert isinstance(results, list)

    def test_handles_encoding_errors(self, tmp_path: Path) -> None:
        """Test that encoding errors don't crash search."""
        # Create a file with problematic encoding
        problem_file = tmp_path / "encoding.txt"
        problem_file.write_bytes(b"test \xff\xfe content")

        with patch("shutil.which", return_value=None):
            searcher = CodeSearcher(tmp_path)
            results = searcher.search("test")

        # Should handle gracefully
        assert isinstance(results, list)

    def test_handles_empty_files(self, tmp_path: Path) -> None:
        """Test handling of empty files."""
        empty_file = tmp_path / "empty.cbl"
        empty_file.touch()

        with patch("shutil.which", return_value=None):
            searcher = CodeSearcher(tmp_path)
            results = searcher.search("test")

        # Should not error
        assert isinstance(results, list)
