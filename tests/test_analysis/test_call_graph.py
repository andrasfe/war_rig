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
        """Create a temporary output directory with test documentation and Citadel graph."""
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

            # Create Citadel dependency graph matching the doc.json data
            graph = {
                "version": "1.0",
                "generated_at": "2026-01-25T00:00:00",
                "source_root": str(output_dir),
                "artifacts": {
                    "procedure::MAINPGM": {
                        "id": "procedure::MAINPGM",
                        "artifact_type": "procedure",
                        "category": "code",
                        "canonical_name": "MAINPGM",
                        "defined_in": {
                            "file_path": str(output_dir / "MAINPGM.cbl"),
                            "line_start": 1,
                            "line_end": 100,
                        },
                        "language": "COBOL",
                    },
                    "procedure::SUBPGM1": {
                        "id": "procedure::SUBPGM1",
                        "artifact_type": "procedure",
                        "category": "code",
                        "canonical_name": "SUBPGM1",
                        "defined_in": {
                            "file_path": str(output_dir / "SUBPGM1.cbl"),
                            "line_start": 1,
                            "line_end": 50,
                        },
                        "language": "COBOL",
                    },
                },
                "relationships": [
                    {
                        "id": "rel-1",
                        "from_artifact": "procedure::MAINPGM",
                        "to_artifact": "procedure::SUBPGM1",
                        "relationship_type": "calls",
                        "location": {
                            "file_path": str(output_dir / "MAINPGM.cbl"),
                            "line_start": 50,
                        },
                        "confidence": 1.0,
                    },
                    {
                        "id": "rel-2",
                        "from_artifact": "procedure::MAINPGM",
                        "to_artifact": "procedure::SUBPGM2",
                        "relationship_type": "calls",
                        "location": {
                            "file_path": str(output_dir / "MAINPGM.cbl"),
                            "line_start": 60,
                        },
                        "confidence": 1.0,
                    },
                    {
                        "id": "rel-3",
                        "from_artifact": "procedure::MAINPGM",
                        "to_artifact": "procedure::IEFBR14",
                        "relationship_type": "calls",
                        "location": {
                            "file_path": str(output_dir / "MAINPGM.cbl"),
                            "line_start": 70,
                        },
                        "confidence": 1.0,
                    },
                ],
                "unresolved": [],
                "statistics": {
                    "files_analyzed": 2,
                    "artifacts_total": 2,
                    "relationships_total": 3,
                },
            }
            graph_path = output_dir / "dependency_graph.json"
            graph_path.write_text(json.dumps(graph), encoding="utf-8")

            yield output_dir, graph_path

    def test_analyze_finds_programs(self, temp_output_dir):
        """Test that analyze finds documented programs."""
        output_dir, graph_path = temp_output_dir
        analyzer = CallGraphAnalyzer(doc_directory=output_dir)
        analysis = analyzer.analyze(dependency_graph_path=graph_path)

        assert len(analysis.documented_programs) == 2
        assert "MAINPGM" in analysis.documented_programs
        assert "SUBPGM1" in analysis.documented_programs

    def test_analyze_finds_calls(self, temp_output_dir):
        """Test that analyze extracts call relationships."""
        output_dir, graph_path = temp_output_dir
        analyzer = CallGraphAnalyzer(doc_directory=output_dir)
        analysis = analyzer.analyze(dependency_graph_path=graph_path)

        # Should have calls from MAINPGM
        mainpgm = analysis.documented_programs.get("MAINPGM")
        assert mainpgm is not None
        assert len(mainpgm.calls) > 0

    def test_analyze_classifies_system_utilities(self, temp_output_dir):
        """Test that analyze correctly classifies system utilities."""
        output_dir, graph_path = temp_output_dir
        analyzer = CallGraphAnalyzer(doc_directory=output_dir)
        analysis = analyzer.analyze(dependency_graph_path=graph_path)

        # IEFBR14 should be classified as system utility
        assert "IEFBR14" in analysis.system_utilities
        assert "IEFBR14" not in analysis.custom_missing

    def test_analyze_identifies_missing_custom(self, temp_output_dir):
        """Test that analyze identifies missing custom programs."""
        output_dir, graph_path = temp_output_dir
        analyzer = CallGraphAnalyzer(doc_directory=output_dir)
        analysis = analyzer.analyze(dependency_graph_path=graph_path)

        # SUBPGM2 is called but not documented and not a system utility
        assert "SUBPGM2" in analysis.custom_missing
        assert analysis.has_gaps()

    def test_analyze_finds_dynamic_calls(self, temp_output_dir):
        """Test that analyze tracks dynamic calls (variable program names).

        Dynamic calls are detected from the Citadel graph's unresolved references
        when the callee name matches a variable pattern like (WS-PROGRAM-NAME).
        """
        output_dir, graph_path = temp_output_dir

        # Add a DISPATCH program and an unresolved dynamic call to the graph
        graph = json.loads(graph_path.read_text(encoding="utf-8"))
        graph["artifacts"]["procedure::DISPATCH"] = {
            "id": "procedure::DISPATCH",
            "artifact_type": "procedure",
            "category": "code",
            "canonical_name": "DISPATCH",
            "defined_in": {
                "file_path": str(output_dir / "DISPATCH.cbl"),
                "line_start": 1,
                "line_end": 50,
            },
            "language": "COBOL",
        }
        graph["unresolved"].append({
            "reference_text": "(WS-PROGRAM-NAME)",
            "expected_type": "procedure",
            "containing_artifact": "procedure::DISPATCH",
            "location": {"line_start": 25},
        })
        graph_path.write_text(json.dumps(graph), encoding="utf-8")

        analyzer = CallGraphAnalyzer(doc_directory=output_dir)
        analysis = analyzer.analyze(dependency_graph_path=graph_path)

        # Should track the dynamic call with variable name
        assert len(analysis.dynamic_calls) > 0
        dynamic_callees = [c.callee for c in analysis.dynamic_calls]
        assert any("WS-PROGRAM-NAME" in c for c in dynamic_callees)

    def test_entry_and_leaf_classification(self, temp_output_dir):
        """Test entry point and leaf node classification."""
        output_dir, graph_path = temp_output_dir
        analyzer = CallGraphAnalyzer(doc_directory=output_dir)
        analysis = analyzer.analyze(dependency_graph_path=graph_path)

        # MAINPGM has no callers -> entry point
        assert "MAINPGM" in analysis.entry_points

        # SUBPGM1 has no callees -> leaf node
        assert "SUBPGM1" in analysis.leaf_nodes

    def test_generate_markdown_report(self, temp_output_dir):
        """Test markdown report generation."""
        output_dir, graph_path = temp_output_dir
        analyzer = CallGraphAnalyzer(doc_directory=output_dir)
        analysis = analyzer.analyze(dependency_graph_path=graph_path)

        report = analyzer.generate_markdown_report(analysis)

        assert "# " in report  # Has heading
        assert "MAINPGM" in report
        assert "SUBPGM1" in report
        assert "mermaid" in report  # Has mermaid diagram

    def test_generate_system_design_md(self, temp_output_dir):
        """Test README.md generation."""
        output_dir, graph_path = temp_output_dir
        analyzer = CallGraphAnalyzer(doc_directory=output_dir)
        analysis = analyzer.analyze(dependency_graph_path=graph_path)

        system_design = analyzer.generate_system_design_md(analysis)

        assert "External" in system_design or "System" in system_design
        assert "IEFBR14" in system_design  # Should document system utilities

    def test_generate_system_design_md_with_sequence_diagrams(self, temp_output_dir):
        """Test README.md generation with sequence diagrams."""
        output_dir, graph_path = temp_output_dir
        analyzer = CallGraphAnalyzer(doc_directory=output_dir)
        analysis = analyzer.analyze(dependency_graph_path=graph_path)

        # Sample sequence diagrams (Mermaid format)
        sequence_diagrams = [
            "```mermaid\nsequenceDiagram\n    MAINPGM->>SUBPGM1: CALL\n    SUBPGM1->>MAINPGM: return\n```",
            "```mermaid\nsequenceDiagram\n    JOBSTEP->>BUILDBAT: EXEC\n```",
        ]

        system_design = analyzer.generate_system_design_md(
            analysis,
            sequence_diagrams=sequence_diagrams,
        )

        # Verify Flows section is present
        assert "## Flows" in system_design
        assert "key call sequences" in system_design

        # Verify individual flows are labeled
        assert "### Flow 1" in system_design
        assert "### Flow 2" in system_design

        # Verify the diagrams are included
        assert "sequenceDiagram" in system_design
        assert "MAINPGM->>SUBPGM1: CALL" in system_design
        assert "JOBSTEP->>BUILDBAT: EXEC" in system_design

    def test_generate_system_design_md_without_sequence_diagrams(self, temp_output_dir):
        """Test README.md generation without sequence diagrams skips Flows section."""
        output_dir, graph_path = temp_output_dir
        analyzer = CallGraphAnalyzer(doc_directory=output_dir)
        analysis = analyzer.analyze(dependency_graph_path=graph_path)

        # Call without sequence_diagrams parameter
        system_design = analyzer.generate_system_design_md(analysis)

        # Verify Flows section is NOT present
        assert "## Flows" not in system_design
        assert "### Flow 1" not in system_design

    def test_generate_system_design_md_with_empty_sequence_diagrams(self, temp_output_dir):
        """Test README.md generation with empty sequence diagrams list."""
        output_dir, graph_path = temp_output_dir
        analyzer = CallGraphAnalyzer(doc_directory=output_dir)
        analysis = analyzer.analyze(dependency_graph_path=graph_path)

        # Pass empty list
        system_design = analyzer.generate_system_design_md(
            analysis,
            sequence_diagrams=[],
        )

        # Verify Flows section is NOT present (empty list should be skipped)
        assert "## Flows" not in system_design


class TestCallGraphAnalyzerEmpty:
    """Tests for CallGraphAnalyzer with no documentation."""

    def test_analyze_raises_without_graph(self):
        """Test that analyze raises ValueError when no graph is provided."""
        with tempfile.TemporaryDirectory() as tmpdir:
            analyzer = CallGraphAnalyzer(doc_directory=Path(tmpdir))

            with pytest.raises(ValueError, match="Citadel dependency graph required"):
                analyzer.analyze()


class TestCitadelGraphLoading:
    """Tests for loading call graph from Citadel dependency_graph.json."""

    @pytest.fixture
    def citadel_graph_fixture(self):
        """Create a temporary directory with a Citadel dependency graph."""
        with tempfile.TemporaryDirectory() as tmpdir:
            output_dir = Path(tmpdir)

            # Create a sample Citadel dependency graph
            graph = {
                "version": "1.0",
                "generated_at": "2026-01-25T00:00:00",
                "source_root": str(output_dir),
                "artifacts": {
                    "procedure::MAINPGM": {
                        "id": "procedure::MAINPGM",
                        "artifact_type": "procedure",
                        "category": "code",
                        "canonical_name": "MAINPGM",
                        "defined_in": {
                            "file_path": str(output_dir / "MAINPGM.cbl"),
                            "line_start": 1,
                            "line_end": 100,
                        },
                        "language": "COBOL",
                    },
                    "procedure::SUBPGM1": {
                        "id": "procedure::SUBPGM1",
                        "artifact_type": "procedure",
                        "category": "code",
                        "canonical_name": "SUBPGM1",
                        "defined_in": {
                            "file_path": str(output_dir / "SUBPGM1.cbl"),
                            "line_start": 1,
                            "line_end": 50,
                        },
                        "language": "COBOL",
                    },
                    "procedure::BUILDBAT": {
                        "id": "procedure::BUILDBAT",
                        "artifact_type": "procedure",
                        "category": "code",
                        "canonical_name": "BUILDBAT",
                        "defined_in": {
                            "file_path": str(output_dir / "BUILDBAT.prc"),
                            "line_start": 1,
                            "line_end": 23,
                        },
                        "language": "JCL",
                    },
                },
                "relationships": [
                    {
                        "id": "rel-1",
                        "from_artifact": "procedure::MAINPGM",
                        "to_artifact": "procedure::SUBPGM1",
                        "relationship_type": "calls",
                        "location": {
                            "file_path": str(output_dir / "MAINPGM.cbl"),
                            "line_start": 50,
                        },
                        "confidence": 1.0,
                    },
                    {
                        "id": "rel-2",
                        "from_artifact": "procedure::MAINPGM",
                        "to_artifact": "procedure::BUILDBAT",
                        "relationship_type": "calls",
                        "location": {
                            "file_path": str(output_dir / "MAINPGM.cbl"),
                            "line_start": 75,
                        },
                        "confidence": 1.0,
                    },
                ],
                "unresolved": [],
                "statistics": {
                    "files_analyzed": 3,
                    "artifacts_total": 3,
                    "relationships_total": 2,
                },
            }

            graph_path = output_dir / "dependency_graph.json"
            graph_path.write_text(json.dumps(graph), encoding="utf-8")

            yield output_dir, graph_path

    def test_load_citadel_graph_extracts_artifacts(self, citadel_graph_fixture):
        """Test that load_from_citadel_graph extracts artifacts correctly."""
        output_dir, graph_path = citadel_graph_fixture
        analyzer = CallGraphAnalyzer(doc_directory=output_dir)

        analyzer.load_from_citadel_graph(graph_path)

        assert analyzer._citadel_loaded is True
        # Should have 3 programs (MAINPGM, SUBPGM1, BUILDBAT)
        # The programs dict will have both canonical names and artifact IDs
        canonical_programs = {
            k for k in analyzer._citadel_programs.keys() if "::" not in k
        }
        assert "MAINPGM" in canonical_programs
        assert "SUBPGM1" in canonical_programs
        assert "BUILDBAT" in canonical_programs

    def test_load_citadel_graph_extracts_relationships(self, citadel_graph_fixture):
        """Test that load_from_citadel_graph extracts call relationships."""
        output_dir, graph_path = citadel_graph_fixture
        analyzer = CallGraphAnalyzer(doc_directory=output_dir)

        analyzer.load_from_citadel_graph(graph_path)

        assert len(analyzer._citadel_calls) == 2
        callers = {c.caller for c in analyzer._citadel_calls}
        callees = {c.callee for c in analyzer._citadel_calls}
        assert "MAINPGM" in callers
        assert "SUBPGM1" in callees
        assert "BUILDBAT" in callees

    def test_analyze_with_citadel_graph(self, citadel_graph_fixture):
        """Test that analyze uses Citadel graph when provided."""
        output_dir, graph_path = citadel_graph_fixture
        analyzer = CallGraphAnalyzer(doc_directory=output_dir)

        analysis = analyzer.analyze(dependency_graph_path=graph_path)

        assert len(analysis.documented_programs) >= 3
        assert analysis.total_calls == 2
        assert "MAINPGM" in analysis.entry_points  # No callers

    def test_analyze_raises_when_no_graph_path(self, citadel_graph_fixture):
        """Test that analyze raises ValueError when no graph path provided."""
        output_dir, _ = citadel_graph_fixture
        analyzer = CallGraphAnalyzer(doc_directory=output_dir)

        with pytest.raises(ValueError, match="Citadel dependency graph required"):
            analyzer.analyze()

    def test_analyze_raises_when_graph_missing(self, citadel_graph_fixture):
        """Test that analyze raises ValueError when graph file does not exist."""
        output_dir, _ = citadel_graph_fixture
        analyzer = CallGraphAnalyzer(doc_directory=output_dir)

        fake_path = output_dir / "nonexistent_graph.json"
        with pytest.raises(ValueError, match="Citadel dependency graph required"):
            analyzer.analyze(dependency_graph_path=fake_path)

    def test_analyze_merges_doc_metadata_with_citadel(self, citadel_graph_fixture):
        """Test that analyze merges doc.json metadata with Citadel graph."""
        output_dir, graph_path = citadel_graph_fixture

        # Create a doc.json file with summary
        doc = {
            "header": {
                "program_id": "MAINPGM",
                "file_name": "MAINPGM.cbl",
                "file_type": "COBOL",
            },
            "purpose": {
                "summary": "Main program for processing",
                "program_type": "BATCH",
            },
            "called_programs": [],
        }
        doc_path = output_dir / "MAINPGM.doc.json"
        doc_path.write_text(json.dumps(doc), encoding="utf-8")

        analyzer = CallGraphAnalyzer(doc_directory=output_dir)
        analysis = analyzer.analyze(dependency_graph_path=graph_path)

        # Should have the summary from doc.json merged in
        mainpgm = analysis.documented_programs.get("MAINPGM")
        assert mainpgm is not None
        assert mainpgm.summary == "Main program for processing"

    def test_citadel_graph_relationship_types(self):
        """Test that different relationship types are mapped correctly."""
        with tempfile.TemporaryDirectory() as tmpdir:
            output_dir = Path(tmpdir)

            graph = {
                "version": "1.0",
                "artifacts": {
                    "program::CALLER": {
                        "id": "program::CALLER",
                        "artifact_type": "program",
                        "canonical_name": "CALLER",
                        "defined_in": {"file_path": "/test/CALLER.cbl"},
                        "language": "COBOL",
                    },
                    "program::TARGET": {
                        "id": "program::TARGET",
                        "artifact_type": "program",
                        "canonical_name": "TARGET",
                        "defined_in": {"file_path": "/test/TARGET.cbl"},
                        "language": "COBOL",
                    },
                },
                "relationships": [
                    {
                        "from_artifact": "program::CALLER",
                        "to_artifact": "program::TARGET",
                        "relationship_type": "performs",
                        "location": {"line_start": 10},
                    },
                ],
            }

            graph_path = output_dir / "dependency_graph.json"
            graph_path.write_text(json.dumps(graph), encoding="utf-8")

            analyzer = CallGraphAnalyzer(doc_directory=output_dir)
            analyzer.load_from_citadel_graph(graph_path)

            assert len(analyzer._citadel_calls) == 1
            call = analyzer._citadel_calls[0]
            assert call.call_type == "PERFORM"
            assert call.caller == "CALLER"
            assert call.callee == "TARGET"

