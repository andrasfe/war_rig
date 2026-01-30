"""
Tests for the get_function_body() method in the Citadel SDK.
"""

from __future__ import annotations

import tempfile
from pathlib import Path
from textwrap import dedent


class TestGetFunctionBodyPython:
    """Tests for Python function body extraction."""

    def test_simple_function(self, citadel_instance):
        """Test extracting a simple Python function body."""
        code = dedent('''
            def hello():
                """Say hello."""
                print("Hello, World!")
                return 42

            def goodbye():
                print("Goodbye!")
        ''').strip()

        with tempfile.NamedTemporaryFile(suffix=".py", delete=False, mode="w") as f:
            f.write(code)
            f.flush()
            path = Path(f.name)

        try:
            body = citadel_instance.get_function_body(path, "hello")
            assert body is not None
            assert 'def hello():' in body
            assert '"""Say hello."""' in body
            assert 'print("Hello, World!")' in body
            assert 'return 42' in body
            # Should not include the goodbye function
            assert 'goodbye' not in body
        finally:
            path.unlink()

    def test_function_not_found(self, citadel_instance):
        """Test that None is returned when function doesn't exist."""
        code = dedent('''
            def hello():
                print("Hello!")
        ''').strip()

        with tempfile.NamedTemporaryFile(suffix=".py", delete=False, mode="w") as f:
            f.write(code)
            f.flush()
            path = Path(f.name)

        try:
            body = citadel_instance.get_function_body(path, "nonexistent")
            assert body is None
        finally:
            path.unlink()

    def test_class_method(self, citadel_instance):
        """Test extracting a class definition."""
        code = dedent('''
            class MyClass:
                def __init__(self):
                    self.value = 0

                def get_value(self):
                    return self.value

            def standalone():
                pass
        ''').strip()

        with tempfile.NamedTemporaryFile(suffix=".py", delete=False, mode="w") as f:
            f.write(code)
            f.flush()
            path = Path(f.name)

        try:
            body = citadel_instance.get_function_body(path, "MyClass")
            assert body is not None
            assert 'class MyClass:' in body
            assert 'def __init__(self):' in body
            assert 'def get_value(self):' in body
            # Should not include standalone function
            assert 'def standalone' not in body
        finally:
            path.unlink()

    def test_nested_function(self, citadel_instance):
        """Test function with nested indentation."""
        code = dedent('''
            def outer():
                def inner():
                    pass
                for i in range(10):
                    if i > 5:
                        print(i)
                return True

            def other():
                pass
        ''').strip()

        with tempfile.NamedTemporaryFile(suffix=".py", delete=False, mode="w") as f:
            f.write(code)
            f.flush()
            path = Path(f.name)

        try:
            body = citadel_instance.get_function_body(path, "outer")
            assert body is not None
            assert 'def outer():' in body
            assert 'def inner():' in body
            assert 'for i in range(10):' in body
            assert 'return True' in body
            assert 'def other' not in body
        finally:
            path.unlink()


class TestGetFunctionBodyCOBOL:
    """Tests for COBOL paragraph body extraction."""

    def test_simple_paragraph(self, citadel_instance):
        """Test extracting a simple COBOL paragraph."""
        # COBOL requires proper fixed-format column positions:
        # - Area A (columns 8-11) for paragraph names
        # - Area B (columns 12-72) for statements
        # Using raw string with 7 spaces prefix for Area A content
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
            body = citadel_instance.get_function_body(path, "MAIN-PARA")
            assert body is not None
            assert 'MAIN-PARA' in body
            assert 'DISPLAY "HELLO"' in body
            assert 'PERFORM SUB-PARA' in body
            # Should not include SUB-PARA content
            assert 'DISPLAY "IN SUB"' not in body
        finally:
            path.unlink()

    def test_paragraph_not_found(self, citadel_instance):
        """Test that None is returned when paragraph doesn't exist."""
        # COBOL requires proper fixed-format column positions
        code = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       PROCEDURE DIVISION.
       MAIN-PARA.
           STOP RUN."""

        with tempfile.NamedTemporaryFile(suffix=".cbl", delete=False, mode="w") as f:
            f.write(code)
            f.flush()
            path = Path(f.name)

        try:
            body = citadel_instance.get_function_body(path, "NONEXISTENT")
            assert body is None
        finally:
            path.unlink()


class TestGetFunctionBodyEdgeCases:
    """Tests for edge cases in function body extraction."""

    def test_unsupported_file_type(self, citadel_instance):
        """Test that None is returned for unsupported file types."""
        with tempfile.NamedTemporaryFile(suffix=".xyz", delete=False, mode="w") as f:
            f.write("some content")
            f.flush()
            path = Path(f.name)

        try:
            body = citadel_instance.get_function_body(path, "something")
            assert body is None
        finally:
            path.unlink()

    def test_file_not_found(self, citadel_instance):
        """Test that None is returned when file doesn't exist."""
        body = citadel_instance.get_function_body("/nonexistent/path.py", "func")
        assert body is None

    def test_case_insensitive_lookup(self, citadel_instance):
        """Test that function lookup is case-insensitive."""
        code = dedent('''
            def HelloWorld():
                return True
        ''').strip()

        with tempfile.NamedTemporaryFile(suffix=".py", delete=False, mode="w") as f:
            f.write(code)
            f.flush()
            path = Path(f.name)

        try:
            # Lookup with different case
            body = citadel_instance.get_function_body(path, "helloworld")
            assert body is not None
            assert 'def HelloWorld():' in body
        finally:
            path.unlink()
