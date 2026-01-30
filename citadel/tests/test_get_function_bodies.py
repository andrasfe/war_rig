"""Tests for the get_function_bodies() batch method in the Citadel SDK."""

from __future__ import annotations

import tempfile
from pathlib import Path
from textwrap import dedent
from unittest.mock import patch


class TestGetFunctionBodiesPython:
    """Tests for batch Python function body extraction."""

    def test_found_body_returned(self, citadel_instance):
        """Test that a found function body is returned correctly."""
        code = dedent('''
            def hello():
                print("Hello!")
                return 42
        ''').strip()

        with tempfile.NamedTemporaryFile(suffix=".py", delete=False, mode="w") as f:
            f.write(code)
            f.flush()
            path = Path(f.name)

        try:
            bodies = citadel_instance.get_function_bodies(path, ["hello"])
            assert isinstance(bodies, dict)
            assert "hello" in bodies
            assert bodies["hello"] is not None
            assert 'print("Hello!")' in bodies["hello"]
        finally:
            path.unlink()

    def test_none_for_missing_functions(self, citadel_instance):
        """Test that None is returned for functions that don't exist."""
        code = dedent('''
            def hello():
                print("Hello!")
        ''').strip()

        with tempfile.NamedTemporaryFile(suffix=".py", delete=False, mode="w") as f:
            f.write(code)
            f.flush()
            path = Path(f.name)

        try:
            bodies = citadel_instance.get_function_bodies(
                path, ["hello", "nonexistent", "also_missing"]
            )
            assert bodies["hello"] is not None
            assert bodies["nonexistent"] is None
            assert bodies["also_missing"] is None
        finally:
            path.unlink()

    def test_single_parse(self, citadel_instance):
        """Test that the file is only analyzed once for batch extraction."""
        code = dedent('''
            def hello():
                print("Hello!")
        ''').strip()

        with tempfile.NamedTemporaryFile(suffix=".py", delete=False, mode="w") as f:
            f.write(code)
            f.flush()
            path = Path(f.name)

        try:
            with patch.object(
                citadel_instance, "analyze_file", wraps=citadel_instance.analyze_file
            ) as mock_analyze:
                bodies = citadel_instance.get_function_bodies(
                    path, ["hello", "missing"]
                )
                # analyze_file should be called exactly once
                assert mock_analyze.call_count == 1
                assert bodies["hello"] is not None
        finally:
            path.unlink()


class TestGetFunctionBodiesCOBOL:
    """Tests for batch COBOL paragraph body extraction."""

    def test_cobol_paragraphs(self, citadel_instance):
        """Test extracting multiple COBOL paragraph bodies."""
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
            bodies = citadel_instance.get_function_bodies(
                path, ["MAIN-PARA", "SUB-PARA"]
            )
            assert bodies["MAIN-PARA"] is not None
            assert 'DISPLAY "HELLO"' in bodies["MAIN-PARA"]
            assert bodies["SUB-PARA"] is not None
            assert 'DISPLAY "IN SUB"' in bodies["SUB-PARA"]
        finally:
            path.unlink()

    def test_cobol_single_parse(self, citadel_instance):
        """Test COBOL batch uses single parse."""
        code = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "HELLO".
           STOP RUN.
       SUB-PARA.
           DISPLAY "IN SUB"."""

        with tempfile.NamedTemporaryFile(suffix=".cbl", delete=False, mode="w") as f:
            f.write(code)
            f.flush()
            path = Path(f.name)

        try:
            with patch.object(
                citadel_instance, "analyze_file", wraps=citadel_instance.analyze_file
            ) as mock_analyze:
                bodies = citadel_instance.get_function_bodies(
                    path, ["MAIN-PARA", "SUB-PARA"]
                )
                assert mock_analyze.call_count == 1
        finally:
            path.unlink()


class TestGetFunctionBodiesEdgeCases:
    """Tests for edge cases in batch function body extraction."""

    def test_empty_names_list(self, citadel_instance):
        """Test with empty function names list."""
        code = dedent('''
            def hello():
                print("Hello!")
        ''').strip()

        with tempfile.NamedTemporaryFile(suffix=".py", delete=False, mode="w") as f:
            f.write(code)
            f.flush()
            path = Path(f.name)

        try:
            bodies = citadel_instance.get_function_bodies(path, [])
            assert bodies == {}
        finally:
            path.unlink()

    def test_unsupported_file_type(self, citadel_instance):
        """Test batch extraction with unsupported file type."""
        with tempfile.NamedTemporaryFile(suffix=".xyz", delete=False, mode="w") as f:
            f.write("some content")
            f.flush()
            path = Path(f.name)

        try:
            bodies = citadel_instance.get_function_bodies(
                path, ["something", "another"]
            )
            assert bodies == {"something": None, "another": None}
        finally:
            path.unlink()

    def test_file_not_found(self, citadel_instance):
        """Test batch extraction when file doesn't exist."""
        bodies = citadel_instance.get_function_bodies(
            "/nonexistent/path.py", ["func1", "func2"]
        )
        assert bodies == {"func1": None, "func2": None}


class TestGetFunctionBodiesConvenience:
    """Tests for the module-level convenience function."""

    def test_convenience_function(self):
        """Test that the module-level convenience function works."""
        from citadel.sdk import get_function_bodies

        code = dedent('''
            def hello():
                return 42
        ''').strip()

        with tempfile.NamedTemporaryFile(suffix=".py", delete=False, mode="w") as f:
            f.write(code)
            f.flush()
            path = Path(f.name)

        try:
            bodies = get_function_bodies(path, ["hello", "missing"])
            assert bodies["hello"] is not None
            assert "return 42" in bodies["hello"]
            assert bodies["missing"] is None
        finally:
            path.unlink()
