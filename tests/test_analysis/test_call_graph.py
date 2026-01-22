"""Tests for call graph analysis module."""

import json
import tempfile
from pathlib import Path

import pytest

from war_rig.analysis.call_graph import (
    CallGraphAnalyzer,
    CallGraphAnalysis,
    CallRelationship,
    ProgramInfo,
    SYSTEM_UTILITIES,
)


class TestSystemUtilities:
    """Tests for system utility classification."""

    def test_common_system_utils_classified(self):
        """Test that common system utilities are in the set."""
        assert "IEFBR14" in SYSTEM_UTILITIES
        assert "IDCAMS" in SYSTEM_UTILITIES
        assert "IEBGENER" in SYSTEM_UTILITIES
        assert "SORT" in SYSTEM_UTILITIES
        assert "IKJEFT01" in SYSTEM_UTILITIES
        assert "DFHECP1$" in SYSTEM_UTILITIES

    def test_custom_programs_not_classified(self):
        """Test that custom program names are not in system utilities."""
        assert "CUSTPGM" not in SYSTEM_UTILITIES
        assert "MAINBAT" not in SYSTEM_UTILITIES
        assert "CALCBAL" not in SYSTEM_UTILITIES


class TestProgramInfo:
    """Tests for ProgramInfo dataclass."""

    def test_create_program_info(self):
        """Test creating a ProgramInfo instance."""
        info = ProgramInfo(
            program_id="TESTPGM",
            file_name="TESTPGM.cbl",
            summary="Test program",
        )
        assert info.program_id == "TESTPGM"
        assert info.file_name == "TESTPGM.cbl"
        assert info.summary == "Test program"
        assert info.calls == []  # Default empty list


class TestCallRelationship:
    """Tests for CallRelationship dataclass."""

    def test_create_call_relationship(self):
        """Test creating a CallRelationship instance."""
        rel = CallRelationship(
            caller="MAIN",
            callee="SUBPGM",
            call_type="STATIC_CALL",
        )
        assert rel.caller == "MAIN"
        assert rel.callee == "SUBPGM"
        assert rel.call_type == "STATIC_CALL"
        assert rel.is_dynamic is False


class TestCallGraphAnalysis:
    """Tests for CallGraphAnalysis dataclass."""

    def test_has_gaps_with_missing(self):
        """Test has_gaps returns True when custom programs are missing."""
        analysis = CallGraphAnalysis(
            documented_programs={},
            external_dependencies={"CUSTPGM", "IEFBR14"},
            system_utilities={"IEFBR14"},
            custom_missing={"CUSTPGM"},
            entry_points=set(),
            leaf_nodes=set(),
            call_chains=[],
            dynamic_calls=[],
            total_calls=0,
        )
        assert analysis.has_gaps() is True

    def test_has_gaps_without_missing(self):
        """Test has_gaps returns False when no custom programs are missing."""
        analysis = CallGraphAnalysis(
            documented_programs={},
            external_dependencies={"IEFBR14"},
            system_utilities={"IEFBR14"},
            custom_missing=set(),
            entry_points=set(),
            leaf_nodes=set(),
            call_chains=[],
            dynamic_calls=[],
            total_calls=0,
        )
        assert analysis.has_gaps() is False

    def test_get_missing_for_documentation(self):
        """Test get_missing_for_documentation returns sorted list."""
        analysis = CallGraphAnalysis(
            documented_programs={},
            external_dependencies=set(),
            system_utilities=set(),
            custom_missing={"ZEBRA", "ALPHA", "BETA"},
            entry_points=set(),
            leaf_nodes=set(),
            call_chains=[],
            dynamic_calls=[],
            total_calls=0,
        )
        result = analysis.get_missing_for_documentation()
        assert result == ["ALPHA", "BETA", "ZEBRA"]


class TestCallGraphAnalyzer:
    """Tests for CallGraphAnalyzer."""

    @pytest.fixture
    def temp_output_dir(self):
        """Create a temporary output directory with test documentation."""
        with tempfile.TemporaryDirectory() as tmpdir:
            output_dir = Path(tmpdir)

            # Create a sample documentation file
            doc = {
                "header": {
                    "program_id": "MAINPGM",
                    "file_name": "MAINPGM.cbl",
                    "file_type": "COBOL",
                },
                "purpose": {
                    "summary": "Main batch program",
                    "program_type": "BATCH",
                },
                "called_programs": [
                    {"program_name": "SUBPGM1", "call_type": "STATIC_CALL"},
                    {"program_name": "SUBPGM2", "call_type": "DYNAMIC_CALL"},
                    {"program_name": "IEFBR14", "call_type": "EXEC"},  # System util
                ],
            }
            doc_path = output_dir / "MAINPGM.doc.json"
            doc_path.write_text(json.dumps(doc), encoding="utf-8")

            # Create another documented program
            doc2 = {
                "header": {
                    "program_id": "SUBPGM1",
                    "file_name": "SUBPGM1.cbl",
                    "file_type": "COBOL",
                },
                "purpose": {
                    "summary": "Sub program 1",
                    "program_type": "SUBROUTINE",
                },
                "called_programs": [],
            }
            doc2_path = output_dir / "SUBPGM1.doc.json"
            doc2_path.write_text(json.dumps(doc2), encoding="utf-8")

            yield output_dir

    def test_analyze_finds_programs(self, temp_output_dir):
        """Test that analyze finds documented programs."""
        analyzer = CallGraphAnalyzer(doc_directory=temp_output_dir)
        analysis = analyzer.analyze()

        assert len(analysis.documented_programs) == 2
        assert "MAINPGM" in analysis.documented_programs
        assert "SUBPGM1" in analysis.documented_programs

    def test_analyze_finds_calls(self, temp_output_dir):
        """Test that analyze extracts call relationships."""
        analyzer = CallGraphAnalyzer(doc_directory=temp_output_dir)
        analysis = analyzer.analyze()

        # Should have calls from MAINPGM
        mainpgm = analysis.documented_programs.get("MAINPGM")
        assert mainpgm is not None
        assert len(mainpgm.calls) > 0

    def test_analyze_classifies_system_utilities(self, temp_output_dir):
        """Test that analyze correctly classifies system utilities."""
        analyzer = CallGraphAnalyzer(doc_directory=temp_output_dir)
        analysis = analyzer.analyze()

        # IEFBR14 should be classified as system utility
        assert "IEFBR14" in analysis.system_utilities
        assert "IEFBR14" not in analysis.custom_missing

    def test_analyze_identifies_missing_custom(self, temp_output_dir):
        """Test that analyze identifies missing custom programs."""
        analyzer = CallGraphAnalyzer(doc_directory=temp_output_dir)
        analysis = analyzer.analyze()

        # SUBPGM2 is called but not documented and not a system utility
        assert "SUBPGM2" in analysis.custom_missing
        assert analysis.has_gaps()

    def test_analyze_finds_dynamic_calls(self, temp_output_dir):
        """Test that analyze tracks dynamic calls (variable program names).

        Note: 'dynamic' in this context means calls where the program name
        is a variable like (WS-PROGRAM-NAME), not just calls with DYNAMIC_CALL type.
        """
        # Add a doc with a truly dynamic call (variable name)
        doc3 = {
            "header": {
                "program_id": "DISPATCH",
                "file_name": "DISPATCH.cbl",
                "file_type": "COBOL",
            },
            "purpose": {
                "summary": "Dispatch program",
                "program_type": "BATCH",
            },
            "called_programs": [
                {"program_name": "(WS-PROGRAM-NAME)", "call_type": "DYNAMIC_CALL"},
            ],
        }
        import json
        doc3_path = temp_output_dir / "DISPATCH.doc.json"
        doc3_path.write_text(json.dumps(doc3), encoding="utf-8")

        analyzer = CallGraphAnalyzer(doc_directory=temp_output_dir)
        analysis = analyzer.analyze()

        # Should track the dynamic call with variable name
        assert len(analysis.dynamic_calls) > 0
        dynamic_callees = [c.callee for c in analysis.dynamic_calls]
        assert "(WS-PROGRAM-NAME)" in dynamic_callees or "WS-PROGRAM-NAME" in dynamic_callees

    def test_entry_and_leaf_classification(self, temp_output_dir):
        """Test entry point and leaf node classification."""
        analyzer = CallGraphAnalyzer(doc_directory=temp_output_dir)
        analysis = analyzer.analyze()

        # MAINPGM has no callers -> entry point
        assert "MAINPGM" in analysis.entry_points

        # SUBPGM1 has no callees -> leaf node
        assert "SUBPGM1" in analysis.leaf_nodes

    def test_generate_markdown_report(self, temp_output_dir):
        """Test markdown report generation."""
        analyzer = CallGraphAnalyzer(doc_directory=temp_output_dir)
        analysis = analyzer.analyze()

        report = analyzer.generate_markdown_report(analysis)

        assert "# " in report  # Has heading
        assert "MAINPGM" in report
        assert "SUBPGM1" in report
        assert "mermaid" in report  # Has mermaid diagram

    def test_generate_system_design_md(self, temp_output_dir):
        """Test SYSTEM_DESIGN.md generation."""
        analyzer = CallGraphAnalyzer(doc_directory=temp_output_dir)
        analysis = analyzer.analyze()

        system_design = analyzer.generate_system_design_md(analysis)

        assert "External" in system_design or "System" in system_design
        assert "IEFBR14" in system_design  # Should document system utilities


class TestCallGraphAnalyzerEmpty:
    """Tests for CallGraphAnalyzer with no documentation."""

    def test_analyze_empty_directory(self):
        """Test analysis of empty directory."""
        with tempfile.TemporaryDirectory() as tmpdir:
            analyzer = CallGraphAnalyzer(doc_directory=Path(tmpdir))
            analysis = analyzer.analyze()

            assert len(analysis.documented_programs) == 0
            assert analysis.total_calls == 0
            assert not analysis.has_gaps()
