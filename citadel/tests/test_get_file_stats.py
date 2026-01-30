"""Tests for the get_file_stats() method in the Citadel SDK."""

from __future__ import annotations

import tempfile
from pathlib import Path
from textwrap import dedent


class TestGetFileStatsPython:
    """Tests for Python file stats extraction."""

    def test_basic_stats(self, citadel_instance):
        """Test basic file stats for a Python file."""
        code = dedent('''
            def hello():
                print("Hello!")
                return 42

            def goodbye():
                print("Goodbye!")
        ''').strip()

        with tempfile.NamedTemporaryFile(suffix=".py", delete=False, mode="w") as f:
            f.write(code)
            f.flush()
            path = Path(f.name)

        try:
            stats = citadel_instance.get_file_stats(path)
            assert stats["total_lines"] > 0
            assert stats["paragraph_count"] >= 1  # Python parser may merge functions
            assert stats["language"] is not None
            assert isinstance(stats["paragraphs"], list)
            assert len(stats["paragraphs"]) >= 1

            # Check paragraph structure
            for para in stats["paragraphs"]:
                assert "name" in para
                assert "line_start" in para
                assert "line_end" in para
                assert "line_count" in para
        finally:
            path.unlink()


class TestGetFileStatsCOBOL:
    """Tests for COBOL file stats extraction."""

    def test_cobol_stats(self, citadel_instance):
        """Test file stats for a COBOL file."""
        code = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "HELLO".
           PERFORM SUB-PARA.
           STOP RUN.
       SUB-PARA.
           DISPLAY "IN SUB"."""

        with tempfile.NamedTemporaryFile(suffix=".cbl", delete=False, mode="w") as f:
            f.write(code)
            f.flush()
            path = Path(f.name)

        try:
            stats = citadel_instance.get_file_stats(path)
            assert stats["total_lines"] > 0
            assert stats["paragraph_count"] >= 2

            # Verify paragraphs have line ranges
            para_names = [p["name"] for p in stats["paragraphs"]]
            assert any("MAIN" in name.upper() for name in para_names)
            assert any("SUB" in name.upper() for name in para_names)

            for para in stats["paragraphs"]:
                if para["line_start"] is not None and para["line_end"] is not None:
                    assert para["line_count"] == para["line_end"] - para["line_start"] + 1
        finally:
            path.unlink()


class TestGetFileStatsEdgeCases:
    """Tests for edge cases in file stats."""

    def test_empty_file(self, citadel_instance):
        """Test stats for an empty file."""
        with tempfile.NamedTemporaryFile(suffix=".py", delete=False, mode="w") as f:
            f.write("")
            f.flush()
            path = Path(f.name)

        try:
            stats = citadel_instance.get_file_stats(path)
            assert stats["paragraph_count"] == 0
            assert stats["paragraphs"] == []
        finally:
            path.unlink()

    def test_nonexistent_file(self, citadel_instance):
        """Test stats for a nonexistent file."""
        stats = citadel_instance.get_file_stats("/nonexistent/path.py")
        # Should return an error or empty stats without crashing
        assert "error" in stats or stats["paragraph_count"] == 0


class TestGetFileStatsConvenience:
    """Tests for the module-level convenience function."""

    def test_convenience_function(self):
        """Test that the module-level convenience function works."""
        from citadel.sdk import get_file_stats

        code = dedent('''
            def hello():
                return 42
        ''').strip()

        with tempfile.NamedTemporaryFile(suffix=".py", delete=False, mode="w") as f:
            f.write(code)
            f.flush()
            path = Path(f.name)

        try:
            stats = get_file_stats(path)
            assert stats["total_lines"] > 0
            assert stats["paragraph_count"] >= 1
        finally:
            path.unlink()
