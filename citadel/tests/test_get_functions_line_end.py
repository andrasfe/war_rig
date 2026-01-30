"""Tests for line_end field in get_functions() output."""

from __future__ import annotations

import tempfile
from pathlib import Path
from textwrap import dedent


class TestGetFunctionsLineEnd:
    """Tests that get_functions() includes line_end in output."""

    def test_python_functions_have_line_end(self, citadel_instance):
        """Test that Python function dicts include line_end."""
        code = dedent('''
            def hello():
                """Say hello."""
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
            functions = citadel_instance.get_functions(path)
            assert len(functions) >= 1

            for func in functions:
                assert "line_end" in func, f"Function {func['name']} missing line_end"
                assert func["line_end"] is not None, f"Function {func['name']} has None line_end"
                assert func["line_end"] >= func["line"], (
                    f"Function {func['name']}: line_end ({func['line_end']}) < line ({func['line']})"
                )
        finally:
            path.unlink()

    def test_cobol_paragraphs_have_line_end(self, citadel_instance):
        """Test that COBOL paragraph dicts include line_end."""
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
            functions = citadel_instance.get_functions(path)
            assert len(functions) >= 2

            for func in functions:
                assert "line_end" in func, f"Paragraph {func['name']} missing line_end"
                if func["line_end"] is not None:
                    assert func["line_end"] >= func["line"], (
                        f"Paragraph {func['name']}: line_end ({func['line_end']}) < line ({func['line']})"
                    )
        finally:
            path.unlink()
