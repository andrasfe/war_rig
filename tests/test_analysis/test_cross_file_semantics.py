"""Tests for CrossFileCallSemanticsAggregator.

This module tests the aggregation of call semantics from multiple files
for cross-file sequence diagram data flow.
"""

import pytest

from war_rig.analysis.cross_file_semantics import (
    AggregatedSemantics,
    CrossFileCallSemanticsAggregator,
)
from war_rig.models.templates import CallSemantics


class TestCrossFileCallSemanticsAggregator:
    """Tests for CrossFileCallSemanticsAggregator."""

    def test_init_empty(self) -> None:
        """Test initialization creates empty aggregator."""
        aggregator = CrossFileCallSemanticsAggregator()

        assert len(aggregator) == 0
        assert aggregator.get_all_semantics() == []
        assert aggregator.get_files_with_semantics() == []

    def test_add_single_file_semantics(self) -> None:
        """Test adding semantics from a single file."""
        aggregator = CrossFileCallSemanticsAggregator()

        semantics = [
            CallSemantics(
                caller="MAIN-PARA",
                callee="2000-PROCESS",
                inputs=["WS-INPUT-1", "WS-INPUT-2"],
                outputs=["WS-RESULT"],
                purpose="Process the input data",
            ),
            CallSemantics(
                caller="2000-PROCESS",
                callee="3000-VALIDATE",
                inputs=["WS-DATA"],
                outputs=["WS-VALID-FLAG"],
                purpose="Validate the data",
            ),
        ]

        aggregator.add_file_semantics("PROG1.cbl", semantics)

        assert len(aggregator) == 2
        assert "PROG1.cbl" in aggregator.get_files_with_semantics()

    def test_add_multiple_file_semantics(self) -> None:
        """Test adding semantics from multiple files."""
        aggregator = CrossFileCallSemanticsAggregator()

        semantics1 = [
            CallSemantics(
                caller="MAIN",
                callee="PROCESS",
                inputs=["X"],
                outputs=["Y"],
            )
        ]
        semantics2 = [
            CallSemantics(
                caller="PROCESS",
                callee="HELPER",
                inputs=["A", "B"],
                outputs=["C"],
            )
        ]

        aggregator.add_file_semantics("FILE1.cbl", semantics1)
        aggregator.add_file_semantics("FILE2.cbl", semantics2)

        assert len(aggregator) == 2
        files = aggregator.get_files_with_semantics()
        assert "FILE1.cbl" in files
        assert "FILE2.cbl" in files

    def test_add_none_semantics(self) -> None:
        """Test adding None semantics is a no-op."""
        aggregator = CrossFileCallSemanticsAggregator()

        aggregator.add_file_semantics("PROG.cbl", None)

        assert len(aggregator) == 0

    def test_add_empty_semantics(self) -> None:
        """Test adding empty list is a no-op."""
        aggregator = CrossFileCallSemanticsAggregator()

        aggregator.add_file_semantics("PROG.cbl", [])

        assert len(aggregator) == 0

    def test_get_semantics_by_pair(self) -> None:
        """Test retrieving semantics by caller/callee pair."""
        aggregator = CrossFileCallSemanticsAggregator()

        semantics = [
            CallSemantics(
                caller="MAIN",
                callee="PROCESS",
                inputs=["X", "Y"],
                outputs=["RESULT"],
                purpose="Do processing",
            )
        ]
        aggregator.add_file_semantics("PROG.cbl", semantics)

        result = aggregator.get_semantics("MAIN", "PROCESS")

        assert result is not None
        assert result.caller == "MAIN"
        assert result.callee == "PROCESS"
        assert result.inputs == ["X", "Y"]
        assert result.outputs == ["RESULT"]
        assert result.purpose == "Do processing"
        assert result.source_file == "PROG.cbl"

    def test_get_semantics_case_insensitive(self) -> None:
        """Test that lookup is case-insensitive."""
        aggregator = CrossFileCallSemanticsAggregator()

        semantics = [
            CallSemantics(
                caller="Main-Para",
                callee="Process-Data",
                inputs=["X"],
                outputs=["Y"],
            )
        ]
        aggregator.add_file_semantics("PROG.cbl", semantics)

        # Different case combinations should all work
        assert aggregator.get_semantics("main-para", "process-data") is not None
        assert aggregator.get_semantics("MAIN-PARA", "PROCESS-DATA") is not None
        assert aggregator.get_semantics("Main-Para", "Process-Data") is not None

    def test_get_semantics_not_found(self) -> None:
        """Test that missing pair returns None."""
        aggregator = CrossFileCallSemanticsAggregator()

        semantics = [
            CallSemantics(
                caller="MAIN",
                callee="PROCESS",
                inputs=["X"],
                outputs=["Y"],
            )
        ]
        aggregator.add_file_semantics("PROG.cbl", semantics)

        result = aggregator.get_semantics("MAIN", "NONEXISTENT")

        assert result is None

    def test_later_file_overrides_earlier(self) -> None:
        """Test that later files override earlier semantics for same pair."""
        aggregator = CrossFileCallSemanticsAggregator()

        semantics1 = [
            CallSemantics(
                caller="MAIN",
                callee="PROCESS",
                inputs=["OLD-INPUT"],
                outputs=["OLD-OUTPUT"],
            )
        ]
        semantics2 = [
            CallSemantics(
                caller="MAIN",
                callee="PROCESS",
                inputs=["NEW-INPUT"],
                outputs=["NEW-OUTPUT"],
            )
        ]

        aggregator.add_file_semantics("FILE1.cbl", semantics1)
        aggregator.add_file_semantics("FILE2.cbl", semantics2)

        result = aggregator.get_semantics("MAIN", "PROCESS")

        assert result is not None
        assert result.inputs == ["NEW-INPUT"]
        assert result.outputs == ["NEW-OUTPUT"]
        assert result.source_file == "FILE2.cbl"

    def test_to_flow_diagram_format(self) -> None:
        """Test conversion to flow diagram format."""
        aggregator = CrossFileCallSemanticsAggregator()

        semantics = [
            CallSemantics(
                caller="MAIN",
                callee="PROCESS",
                inputs=["X", "Y"],
                outputs=["RESULT"],
                purpose="Process data",
            ),
            CallSemantics(
                caller="PROCESS",
                callee="HELPER",
                inputs=["A"],
                outputs=["B", "C"],
                purpose=None,
            ),
        ]
        aggregator.add_file_semantics("PROG.cbl", semantics)

        result = aggregator.to_flow_diagram_format()

        assert "MAIN->PROCESS" in result
        assert "PROCESS->HELPER" in result

        main_process = result["MAIN->PROCESS"]
        assert main_process["inputs"] == ["X", "Y"]
        assert main_process["outputs"] == ["RESULT"]
        assert main_process["purpose"] == "Process data"

        process_helper = result["PROCESS->HELPER"]
        assert process_helper["inputs"] == ["A"]
        assert process_helper["outputs"] == ["B", "C"]
        assert process_helper["purpose"] is None

    def test_to_flow_diagram_format_truncates_variables(self) -> None:
        """Test that variable lists are truncated to max_variables."""
        aggregator = CrossFileCallSemanticsAggregator()

        many_inputs = [f"VAR-{i}" for i in range(10)]
        many_outputs = [f"OUT-{i}" for i in range(10)]

        semantics = [
            CallSemantics(
                caller="MAIN",
                callee="PROCESS",
                inputs=many_inputs,
                outputs=many_outputs,
            )
        ]
        aggregator.add_file_semantics("PROG.cbl", semantics)

        # Default max_variables is 5
        result = aggregator.to_flow_diagram_format()

        main_process = result["MAIN->PROCESS"]
        assert len(main_process["inputs"]) == 5
        assert len(main_process["outputs"]) == 5

        # Custom max_variables
        result_custom = aggregator.to_flow_diagram_format(max_variables=3)
        main_process_custom = result_custom["MAIN->PROCESS"]
        assert len(main_process_custom["inputs"]) == 3
        assert len(main_process_custom["outputs"]) == 3

    def test_skip_semantics_without_caller_or_callee(self) -> None:
        """Test that semantics with missing caller/callee are skipped."""
        aggregator = CrossFileCallSemanticsAggregator()

        semantics = [
            CallSemantics(
                caller="",
                callee="PROCESS",
                inputs=["X"],
                outputs=["Y"],
            ),
            CallSemantics(
                caller="MAIN",
                callee="",
                inputs=["X"],
                outputs=["Y"],
            ),
            CallSemantics(
                caller="VALID",
                callee="TARGET",
                inputs=["X"],
                outputs=["Y"],
            ),
        ]
        aggregator.add_file_semantics("PROG.cbl", semantics)

        # Only the valid one should be added
        assert len(aggregator) == 1
        assert aggregator.get_semantics("VALID", "TARGET") is not None

    def test_get_cross_file_semantics(self) -> None:
        """Test cross-file semantics lookup."""
        aggregator = CrossFileCallSemanticsAggregator()

        semantics = [
            CallSemantics(
                caller="MAIN",
                callee="PROCESS",
                inputs=["X"],
                outputs=["Y"],
            )
        ]
        aggregator.add_file_semantics("PROG.cbl", semantics)

        # Currently delegates to get_semantics (file context not used yet)
        result = aggregator.get_cross_file_semantics(
            caller_file="FILE1.cbl",
            caller_para="MAIN",
            callee_file="FILE2.cbl",
            callee_para="PROCESS",
        )

        assert result is not None
        assert result.caller == "MAIN"
        assert result.callee == "PROCESS"

    def test_get_all_semantics(self) -> None:
        """Test getting all aggregated semantics."""
        aggregator = CrossFileCallSemanticsAggregator()

        semantics = [
            CallSemantics(
                caller="A",
                callee="B",
                inputs=["X"],
                outputs=["Y"],
            ),
            CallSemantics(
                caller="B",
                callee="C",
                inputs=["P"],
                outputs=["Q"],
            ),
        ]
        aggregator.add_file_semantics("PROG.cbl", semantics)

        all_sem = aggregator.get_all_semantics()

        assert len(all_sem) == 2
        callers = {s.caller for s in all_sem}
        assert callers == {"A", "B"}


class TestAggregatedSemantics:
    """Tests for AggregatedSemantics dataclass."""

    def test_create_with_defaults(self) -> None:
        """Test creating with default values."""
        sem = AggregatedSemantics(caller="MAIN", callee="PROCESS")

        assert sem.caller == "MAIN"
        assert sem.callee == "PROCESS"
        assert sem.inputs == []
        assert sem.outputs == []
        assert sem.purpose is None
        assert sem.source_file == ""

    def test_create_with_all_fields(self) -> None:
        """Test creating with all fields."""
        sem = AggregatedSemantics(
            caller="MAIN",
            callee="PROCESS",
            inputs=["X", "Y"],
            outputs=["Z"],
            purpose="Do something",
            source_file="PROG.cbl",
        )

        assert sem.caller == "MAIN"
        assert sem.callee == "PROCESS"
        assert sem.inputs == ["X", "Y"]
        assert sem.outputs == ["Z"]
        assert sem.purpose == "Do something"
        assert sem.source_file == "PROG.cbl"
