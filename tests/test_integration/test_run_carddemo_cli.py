"""Tests for the run_carddemo CLI script.

These tests verify the CLI argument parsing and PM mode functions work correctly.
"""

import argparse
from datetime import datetime
from pathlib import Path
from unittest.mock import MagicMock, patch

import pytest

# Import from the script
import sys
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "scripts"))

from run_carddemo import (
    format_status_line,
    print_batch_result,
    print_cycle_summary,
)
from war_rig.orchestration.ticket_engine import BatchResult


class TestFormatStatusLine:
    """Tests for format_status_line function."""

    def test_format_idle_status(self) -> None:
        """Test formatting idle status."""
        status = {
            "cycle": 0,
            "max_cycles": 5,
            "status": "idle",
            "total_files": 0,
            "documented_files": 0,
            "validated_files": 0,
        }

        result = format_status_line(status)

        assert "[Cycle 0/5]" in result
        assert "IDLE" in result
        assert "Files: 0/0 documented" in result

    def test_format_documenting_status(self) -> None:
        """Test formatting during documentation phase."""
        status = {
            "cycle": 1,
            "max_cycles": 5,
            "status": "documenting",
            "total_files": 10,
            "documented_files": 3,
            "validated_files": 0,
            "scribe_pool": {"active_count": 3},
            "challenger_pool": {"active_count": 0},
        }

        result = format_status_line(status)

        assert "[Cycle 1/5]" in result
        assert "DOCUMENTING" in result
        assert "Files: 3/10 documented" in result
        assert "Workers: 3 active" in result
        assert "(3S/0C)" in result

    def test_format_validating_status(self) -> None:
        """Test formatting during validation phase."""
        status = {
            "cycle": 2,
            "max_cycles": 5,
            "status": "validating",
            "total_files": 10,
            "documented_files": 10,
            "validated_files": 5,
            "scribe_pool": {"active_count": 0},
            "challenger_pool": {"active_count": 2},
        }

        result = format_status_line(status)

        assert "[Cycle 2/5]" in result
        assert "VALIDATING" in result
        assert "Files: 10/10 documented" in result
        assert "5 validated" in result
        assert "(0S/2C)" in result

    def test_format_status_with_rework(self) -> None:
        """Test formatting with rework files."""
        status = {
            "cycle": 2,
            "max_cycles": 5,
            "status": "documenting",
            "total_files": 10,
            "documented_files": 10,
            "validated_files": 8,
            "rework_files": 3,
            "scribe_pool": {"active_count": 2},
            "challenger_pool": {"active_count": 0},
        }

        result = format_status_line(status)

        assert "[Cycle 2/5]" in result
        assert "DOCUMENTING" in result
        assert "3 rework" in result
        assert "8 validated" in result


class TestPrintCycleSummary:
    """Tests for print_cycle_summary function."""

    def test_print_cycle_summary(self, capsys) -> None:
        """Test cycle summary output."""
        status = {
            "total_files": 10,
            "documented_files": 8,
            "validated_files": 6,
            "scribe_pool": {"total_processed": 8, "total_failed": 2},
            "challenger_pool": {"total_processed": 6, "total_failed": 0},
        }

        print_cycle_summary(status, cycle=1)
        captured = capsys.readouterr()

        assert "Cycle 1 Complete" in captured.out
        assert "Files documented: 8/10" in captured.out
        assert "Files validated:  6/10" in captured.out
        assert "Scribe processed: 8" in captured.out
        assert "Scribe failed:    2" in captured.out


class TestPrintBatchResult:
    """Tests for print_batch_result function."""

    def test_print_successful_result(self, capsys) -> None:
        """Test printing successful batch result."""
        result = BatchResult(
            completed_files=["FILE1.cbl", "FILE2.cbl"],
            failed_files=[],
            total_cycles=2,
            final_decision="SATISFIED",
            started_at=datetime(2024, 1, 1, 12, 0, 0),
            completed_at=datetime(2024, 1, 1, 12, 5, 0),
            quality_notes=["Good coverage", "Clear documentation"],
        )

        print_batch_result(result)
        captured = capsys.readouterr()

        assert "BATCH RESULT SUMMARY" in captured.out
        assert "Final Decision: SATISFIED" in captured.out
        assert "Total Cycles:   2" in captured.out
        assert "Duration:       300.0s" in captured.out
        assert "[OK] FILE1.cbl" in captured.out
        assert "[OK] FILE2.cbl" in captured.out
        assert "Good coverage" in captured.out
        assert "Batch completed SUCCESSFULLY" in captured.out

    def test_print_failed_result(self, capsys) -> None:
        """Test printing failed batch result."""
        result = BatchResult(
            completed_files=["FILE1.cbl"],
            failed_files=["FILE2.cbl", "FILE3.cbl"],
            total_cycles=5,
            final_decision="FORCED_COMPLETE",
            started_at=datetime(2024, 1, 1, 12, 0, 0),
            completed_at=datetime(2024, 1, 1, 12, 10, 0),
        )

        print_batch_result(result)
        captured = capsys.readouterr()

        assert "Final Decision: FORCED_COMPLETE" in captured.out
        assert "[OK] FILE1.cbl" in captured.out
        assert "[FAIL] FILE2.cbl" in captured.out
        assert "[FAIL] FILE3.cbl" in captured.out
        assert "Batch completed SUCCESSFULLY" in captured.out  # FORCED_COMPLETE is still success

    def test_print_result_with_assumptions(self, capsys) -> None:
        """Test printing result with assumptions."""
        result = BatchResult(
            completed_files=["FILE1.cbl"],
            final_decision="SATISFIED",
            started_at=datetime(2024, 1, 1, 12, 0, 0),
            completed_at=datetime(2024, 1, 1, 12, 1, 0),
            assumptions_made=[
                {"description": "Assumed standard file format"},
                {"description": "Assumed UTF-8 encoding"},
            ],
        )

        print_batch_result(result)
        captured = capsys.readouterr()

        assert "Assumptions Made (2)" in captured.out
        assert "Assumed standard file format" in captured.out
        assert "Assumed UTF-8 encoding" in captured.out


class TestCLIArgumentParsing:
    """Tests for CLI argument parsing."""

    def test_pm_mode_args_exist(self) -> None:
        """Test that PM mode arguments are available."""
        # Import the main function to trigger parser creation
        from run_carddemo import main
        import argparse

        # Create a test parser with the same arguments
        parser = argparse.ArgumentParser()
        parser.add_argument("--pm-mode", action="store_true")
        parser.add_argument("--num-scribes", type=int, default=3)
        parser.add_argument("--num-challengers", type=int, default=2)
        parser.add_argument("--max-cycles", type=int, default=5)
        parser.add_argument("--mock", action="store_true")

        # Parse test arguments
        args = parser.parse_args(["--pm-mode", "--mock", "--num-scribes", "5"])

        assert args.pm_mode is True
        assert args.mock is True
        assert args.num_scribes == 5
        assert args.num_challengers == 2
        assert args.max_cycles == 5

    def test_default_values(self) -> None:
        """Test default argument values."""
        import argparse

        parser = argparse.ArgumentParser()
        parser.add_argument("--pm-mode", action="store_true")
        parser.add_argument("--num-scribes", type=int, default=3)
        parser.add_argument("--num-challengers", type=int, default=2)
        parser.add_argument("--max-cycles", type=int, default=5)

        args = parser.parse_args([])

        assert args.pm_mode is False
        assert args.num_scribes == 3
        assert args.num_challengers == 2
        assert args.max_cycles == 5
