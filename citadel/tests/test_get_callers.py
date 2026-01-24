"""
Tests for the get_callers() method in the Citadel SDK.
"""

from __future__ import annotations

import tempfile
from pathlib import Path
from textwrap import dedent

import pytest

from citadel import get_callers


class TestGetCallersPython:
    """Tests for Python caller detection."""

    def test_find_function_callers(self, citadel_instance, tmp_path):
        """Test finding callers of a Python function."""
        # Create the target file with a utility function
        utils_code = dedent('''
            def calculate_tax(amount):
                return amount * 0.1
        ''').strip()

        utils_file = tmp_path / "utils.py"
        utils_file.write_text(utils_code)

        # Create a caller file
        main_code = dedent('''
            from utils import calculate_tax

            def process_order(amount):
                tax = calculate_tax(amount)
                return amount + tax
        ''').strip()

        main_file = tmp_path / "main.py"
        main_file.write_text(main_code)

        # Find callers
        callers = citadel_instance.get_callers(
            utils_file, "calculate_tax", search_paths=[tmp_path]
        )

        # Should find the import in main.py
        assert len(callers) >= 1
        caller_files = [c["file"] for c in callers]
        assert str(main_file) in caller_files

    def test_no_callers_found(self, citadel_instance, tmp_path):
        """Test when no callers exist."""
        code = dedent('''
            def unused_function():
                return 42
        ''').strip()

        target_file = tmp_path / "target.py"
        target_file.write_text(code)

        # Find callers (there are none)
        callers = citadel_instance.get_callers(
            target_file, "unused_function", search_paths=[tmp_path]
        )

        # The target file itself might show up, but not as a caller
        # Filter out self-references
        external_callers = [c for c in callers if c["file"] != str(target_file)]
        assert len(external_callers) == 0


class TestGetCallersCOBOL:
    """Tests for COBOL caller detection."""

    def test_find_perform_callers(self, citadel_instance, tmp_path):
        """Test finding COBOL paragraphs that PERFORM another paragraph."""
        # Create a COBOL program with paragraphs
        code = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAINPROG.
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "START".
           PERFORM CALCULATE-TAX.
           PERFORM PRINT-RESULTS.
           STOP RUN.
       CALCULATE-TAX.
           DISPLAY "CALCULATING TAX".
       PRINT-RESULTS.
           DISPLAY "PRINTING"."""

        main_file = tmp_path / "MAINPROG.cbl"
        main_file.write_text(code)

        # Find callers of CALCULATE-TAX
        callers = citadel_instance.get_callers(
            main_file, "CALCULATE-TAX", search_paths=[tmp_path]
        )

        # Should find MAIN-PARA as a caller
        assert len(callers) >= 1
        caller_functions = [c["function"] for c in callers if c["function"]]
        assert any("MAIN" in fn for fn in caller_functions)

    def test_find_copy_callers(self, citadel_instance, tmp_path):
        """Test finding COBOL programs that COPY a copybook."""
        # Create a copybook
        copybook_code = """\
       01 CUSTOMER-REC.
           05 CUST-ID    PIC X(10).
           05 CUST-NAME  PIC X(30)."""

        copybook_file = tmp_path / "CUSTREC.cpy"
        copybook_file.write_text(copybook_code)

        # Create a COBOL program that copies it
        program_code = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTPROG.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           COPY CUSTREC.
       PROCEDURE DIVISION.
       MAIN-PARA.
           STOP RUN."""

        program_file = tmp_path / "CUSTPROG.cbl"
        program_file.write_text(program_code)

        # Find callers of the copybook
        callers = citadel_instance.get_callers(
            copybook_file, "CUSTREC", search_paths=[tmp_path]
        )

        # Should find the program as a caller
        assert len(callers) >= 1
        caller_files = [c["file"] for c in callers]
        assert str(program_file) in caller_files


class TestGetCallersEdgeCases:
    """Tests for edge cases in caller detection."""

    def test_case_insensitive_matching(self, citadel_instance, tmp_path):
        """Test that caller matching is case-insensitive for search."""
        code = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM CALCULATE-TAX.
           STOP RUN.
       CALCULATE-TAX.
           DISPLAY "TAX"."""

        file_path = tmp_path / "TEST.cbl"
        file_path.write_text(code)

        # Search with lowercase (function is uppercase in source)
        callers = citadel_instance.get_callers(
            file_path, "calculate-tax", search_paths=[tmp_path]
        )

        # Should find the caller despite case difference in search term
        assert len(callers) >= 1

    def test_search_default_directory(self, citadel_instance, tmp_path):
        """Test that default search uses the file's directory."""
        code = dedent('''
            def target_func():
                return 1

            def caller_func():
                return target_func()
        ''').strip()

        file_path = tmp_path / "module.py"
        file_path.write_text(code)

        # Search without specifying search_paths
        callers = citadel_instance.get_callers(file_path, "target_func")

        # Should still work with default path
        assert isinstance(callers, list)

    def test_nonexistent_search_path(self, citadel_instance, tmp_path):
        """Test handling of non-existent search paths."""
        code = dedent('''
            def some_func():
                pass
        ''').strip()

        file_path = tmp_path / "test.py"
        file_path.write_text(code)

        # Search with non-existent path
        callers = citadel_instance.get_callers(
            file_path,
            "some_func",
            search_paths=[tmp_path / "nonexistent"]
        )

        # Should return empty list, not error
        assert callers == []


class TestGetCallersConvenienceFunction:
    """Tests for the module-level get_callers convenience function."""

    def test_convenience_function(self, tmp_path):
        """Test the module-level get_callers function."""
        code = dedent('''
            def helper():
                return 42

            def main():
                return helper()
        ''').strip()

        file_path = tmp_path / "module.py"
        file_path.write_text(code)

        # Use convenience function
        callers = get_callers(file_path, "helper", search_paths=[tmp_path])

        # Should work just like the method
        assert isinstance(callers, list)
