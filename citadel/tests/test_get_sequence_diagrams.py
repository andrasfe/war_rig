"""
Tests for the get_sequence_diagrams SDK method.

These tests verify the sequence diagram generation functionality
including loading from JSON files and analyzing directories.
"""

import json
from pathlib import Path

import pytest

from citadel.sdk import get_sequence_diagrams


class TestGetSequenceDiagramsFromJson:
    """Tests for loading dependency graphs from JSON files."""

    def test_simple_chain_from_json(self, citadel_instance, tmp_path: Path):
        """Generate sequence diagrams from a simple JSON graph."""
        # Create a minimal dependency graph JSON
        graph_data = {
            "version": "1.0",
            "generated_at": "2024-01-01T00:00:00",
            "source_root": "/test",
            "artifacts": {
                "program::A": {
                    "id": "program::A",
                    "artifact_type": "program",
                    "category": "code",
                    "canonical_name": "A",
                    "language": "COBOL",
                },
                "program::B": {
                    "id": "program::B",
                    "artifact_type": "program",
                    "category": "code",
                    "canonical_name": "B",
                    "language": "COBOL",
                },
                "program::C": {
                    "id": "program::C",
                    "artifact_type": "program",
                    "category": "code",
                    "canonical_name": "C",
                    "language": "COBOL",
                },
            },
            "relationships": [
                {
                    "id": "rel-1",
                    "from_artifact": "program::A",
                    "to_artifact": "program::B",
                    "relationship_type": "calls",
                    "location": {"file_path": "/test/A.cbl", "line_start": 100},
                    "evidence_text": "CALL 'B'",
                },
                {
                    "id": "rel-2",
                    "from_artifact": "program::B",
                    "to_artifact": "program::C",
                    "relationship_type": "calls",
                    "location": {"file_path": "/test/B.cbl", "line_start": 50},
                    "evidence_text": "CALL 'C'",
                },
            ],
            "unresolved": [],
            "statistics": {
                "files_analyzed": 3,
                "files_skipped": 0,
                "files_failed": 0,
                "artifacts_by_type": {"program": 3},
                "artifacts_total": 3,
                "relationships_by_type": {"calls": 2},
                "relationships_total": 2,
                "unresolved_count": 0,
                "resolution_rate": 1.0,
            },
            "config_hash": "test",
        }

        json_path = tmp_path / "graph.json"
        with open(json_path, "w") as f:
            json.dump(graph_data, f)

        diagrams = citadel_instance.get_sequence_diagrams(json_path, min_sequence_length=2)

        assert len(diagrams) == 1
        diagram = diagrams[0]
        assert "sequenceDiagram" in diagram
        assert "A" in diagram
        assert "B" in diagram
        assert "C" in diagram
        assert "calls" in diagram

    def test_min_length_filtering(self, citadel_instance, tmp_path: Path):
        """Sequences shorter than min_length should be excluded."""
        graph_data = {
            "version": "1.0",
            "generated_at": "2024-01-01T00:00:00",
            "source_root": "/test",
            "artifacts": {
                "program::A": {
                    "id": "program::A",
                    "artifact_type": "program",
                    "category": "code",
                    "canonical_name": "A",
                    "language": "COBOL",
                },
                "program::B": {
                    "id": "program::B",
                    "artifact_type": "program",
                    "category": "code",
                    "canonical_name": "B",
                    "language": "COBOL",
                },
            },
            "relationships": [
                {
                    "id": "rel-1",
                    "from_artifact": "program::A",
                    "to_artifact": "program::B",
                    "relationship_type": "calls",
                    "location": {"file_path": "/test/A.cbl", "line_start": 100},
                    "evidence_text": "CALL 'B'",
                },
            ],
            "unresolved": [],
            "statistics": {
                "files_analyzed": 2,
                "files_skipped": 0,
                "files_failed": 0,
                "artifacts_by_type": {"program": 2},
                "artifacts_total": 2,
                "relationships_by_type": {"calls": 1},
                "relationships_total": 1,
                "unresolved_count": 0,
                "resolution_rate": 1.0,
            },
            "config_hash": "test",
        }

        json_path = tmp_path / "graph.json"
        with open(json_path, "w") as f:
            json.dump(graph_data, f)

        # With min_length=2, single-edge sequence should be excluded
        diagrams = citadel_instance.get_sequence_diagrams(json_path, min_sequence_length=2)
        assert len(diagrams) == 0

        # With min_length=1, single-edge sequence should be included
        diagrams = citadel_instance.get_sequence_diagrams(json_path, min_sequence_length=1)
        assert len(diagrams) == 1

    def test_max_diagrams_limiting(self, citadel_instance, tmp_path: Path):
        """max_diagrams should limit the number of returned diagrams."""
        graph_data = {
            "version": "1.0",
            "generated_at": "2024-01-01T00:00:00",
            "source_root": "/test",
            "artifacts": {
                "program::A": {
                    "id": "program::A",
                    "artifact_type": "program",
                    "category": "code",
                    "canonical_name": "A",
                    "language": "COBOL",
                },
                "program::B": {
                    "id": "program::B",
                    "artifact_type": "program",
                    "category": "code",
                    "canonical_name": "B",
                    "language": "COBOL",
                },
                "program::X": {
                    "id": "program::X",
                    "artifact_type": "program",
                    "category": "code",
                    "canonical_name": "X",
                    "language": "COBOL",
                },
                "program::Y": {
                    "id": "program::Y",
                    "artifact_type": "program",
                    "category": "code",
                    "canonical_name": "Y",
                    "language": "COBOL",
                },
            },
            "relationships": [
                {
                    "id": "rel-1",
                    "from_artifact": "program::A",
                    "to_artifact": "program::B",
                    "relationship_type": "calls",
                    "location": {"file_path": "/test/A.cbl", "line_start": 100},
                    "evidence_text": "CALL 'B'",
                },
                {
                    "id": "rel-2",
                    "from_artifact": "program::X",
                    "to_artifact": "program::Y",
                    "relationship_type": "calls",
                    "location": {"file_path": "/test/X.cbl", "line_start": 100},
                    "evidence_text": "CALL 'Y'",
                },
            ],
            "unresolved": [],
            "statistics": {
                "files_analyzed": 4,
                "files_skipped": 0,
                "files_failed": 0,
                "artifacts_by_type": {"program": 4},
                "artifacts_total": 4,
                "relationships_by_type": {"calls": 2},
                "relationships_total": 2,
                "unresolved_count": 0,
                "resolution_rate": 1.0,
            },
            "config_hash": "test",
        }

        json_path = tmp_path / "graph.json"
        with open(json_path, "w") as f:
            json.dump(graph_data, f)

        # Without limit, should get 2 diagrams
        diagrams = citadel_instance.get_sequence_diagrams(json_path, min_sequence_length=1)
        assert len(diagrams) == 2

        # With limit, should get only 1
        diagrams = citadel_instance.get_sequence_diagrams(
            json_path, max_diagrams=1, min_sequence_length=1
        )
        assert len(diagrams) == 1

    def test_empty_graph_returns_empty_list(self, citadel_instance, tmp_path: Path):
        """Empty graph should return empty list."""
        graph_data = {
            "version": "1.0",
            "generated_at": "2024-01-01T00:00:00",
            "source_root": "/test",
            "artifacts": {},
            "relationships": [],
            "unresolved": [],
            "statistics": {
                "files_analyzed": 0,
                "files_skipped": 0,
                "files_failed": 0,
                "artifacts_by_type": {},
                "artifacts_total": 0,
                "relationships_by_type": {},
                "relationships_total": 0,
                "unresolved_count": 0,
                "resolution_rate": 1.0,
            },
            "config_hash": "test",
        }

        json_path = tmp_path / "graph.json"
        with open(json_path, "w") as f:
            json.dump(graph_data, f)

        diagrams = citadel_instance.get_sequence_diagrams(json_path)

        assert diagrams == []


class TestGetSequenceDiagramsErrorHandling:
    """Tests for error handling in get_sequence_diagrams."""

    def test_file_not_found(self, citadel_instance):
        """Should raise FileNotFoundError for non-existent path."""
        with pytest.raises(FileNotFoundError):
            citadel_instance.get_sequence_diagrams("/nonexistent/path")

    def test_invalid_json_file(self, citadel_instance, tmp_path: Path):
        """Should raise ValueError for invalid JSON."""
        invalid_json = tmp_path / "invalid.json"
        invalid_json.write_text("{ this is not valid json")

        with pytest.raises(ValueError, match="Invalid JSON"):
            citadel_instance.get_sequence_diagrams(invalid_json)

    def test_invalid_graph_structure(self, citadel_instance, tmp_path: Path):
        """Should raise ValueError for JSON that isn't a valid graph."""
        invalid_graph = tmp_path / "not_a_graph.json"
        invalid_graph.write_text('{"foo": "bar"}')

        with pytest.raises(ValueError, match="Failed to parse dependency graph"):
            citadel_instance.get_sequence_diagrams(invalid_graph)

    def test_non_json_file_rejected(self, citadel_instance, tmp_path: Path):
        """Should raise ValueError for non-JSON files."""
        txt_file = tmp_path / "data.txt"
        txt_file.write_text("some text")

        with pytest.raises(ValueError, match="must be a directory or a JSON file"):
            citadel_instance.get_sequence_diagrams(txt_file)


class TestConvenienceFunction:
    """Tests for the module-level convenience function."""

    def test_convenience_function_works(self, tmp_path: Path):
        """get_sequence_diagrams convenience function should work."""
        graph_data = {
            "version": "1.0",
            "generated_at": "2024-01-01T00:00:00",
            "source_root": "/test",
            "artifacts": {
                "program::A": {
                    "id": "program::A",
                    "artifact_type": "program",
                    "category": "code",
                    "canonical_name": "A",
                    "language": "COBOL",
                },
                "program::B": {
                    "id": "program::B",
                    "artifact_type": "program",
                    "category": "code",
                    "canonical_name": "B",
                    "language": "COBOL",
                },
            },
            "relationships": [
                {
                    "id": "rel-1",
                    "from_artifact": "program::A",
                    "to_artifact": "program::B",
                    "relationship_type": "calls",
                    "location": {"file_path": "/test/A.cbl", "line_start": 100},
                    "evidence_text": "CALL 'B'",
                },
            ],
            "unresolved": [],
            "statistics": {
                "files_analyzed": 2,
                "files_skipped": 0,
                "files_failed": 0,
                "artifacts_by_type": {"program": 2},
                "artifacts_total": 2,
                "relationships_by_type": {"calls": 1},
                "relationships_total": 1,
                "unresolved_count": 0,
                "resolution_rate": 1.0,
            },
            "config_hash": "test",
        }

        json_path = tmp_path / "graph.json"
        with open(json_path, "w") as f:
            json.dump(graph_data, f)

        # Use the module-level convenience function
        diagrams = get_sequence_diagrams(json_path, min_sequence_length=1)

        assert len(diagrams) == 1
        assert "sequenceDiagram" in diagrams[0]


class TestMermaidOutput:
    """Tests for the Mermaid diagram output format."""

    def test_mermaid_structure(self, citadel_instance, tmp_path: Path):
        """Verify the Mermaid diagram has correct structure."""
        graph_data = {
            "version": "1.0",
            "generated_at": "2024-01-01T00:00:00",
            "source_root": "/test",
            "artifacts": {
                "program::MAIN": {
                    "id": "program::MAIN",
                    "artifact_type": "program",
                    "category": "code",
                    "canonical_name": "MAIN",
                    "display_name": "Main Program",
                    "language": "COBOL",
                },
                "program::CALC": {
                    "id": "program::CALC",
                    "artifact_type": "program",
                    "category": "code",
                    "canonical_name": "CALC",
                    "language": "COBOL",
                },
                "program::REPORT": {
                    "id": "program::REPORT",
                    "artifact_type": "program",
                    "category": "code",
                    "canonical_name": "REPORT",
                    "language": "COBOL",
                },
            },
            "relationships": [
                {
                    "id": "rel-1",
                    "from_artifact": "program::MAIN",
                    "to_artifact": "program::CALC",
                    "relationship_type": "calls",
                    "location": {"file_path": "/test/MAIN.cbl", "line_start": 100},
                    "evidence_text": "CALL 'CALC'",
                },
                {
                    "id": "rel-2",
                    "from_artifact": "program::CALC",
                    "to_artifact": "program::REPORT",
                    "relationship_type": "calls",
                    "location": {"file_path": "/test/CALC.cbl", "line_start": 50},
                    "evidence_text": "CALL 'REPORT'",
                },
            ],
            "unresolved": [],
            "statistics": {
                "files_analyzed": 3,
                "files_skipped": 0,
                "files_failed": 0,
                "artifacts_by_type": {"program": 3},
                "artifacts_total": 3,
                "relationships_by_type": {"calls": 2},
                "relationships_total": 2,
                "unresolved_count": 0,
                "resolution_rate": 1.0,
            },
            "config_hash": "test",
        }

        json_path = tmp_path / "graph.json"
        with open(json_path, "w") as f:
            json.dump(graph_data, f)

        diagrams = citadel_instance.get_sequence_diagrams(json_path, min_sequence_length=2)

        assert len(diagrams) == 1
        diagram = diagrams[0]

        # Check Mermaid structure
        assert diagram.startswith("sequenceDiagram")
        assert "participant" in diagram
        assert "->>" in diagram  # Arrow syntax

        # Check that display_name is used when available
        assert "Main Program" in diagram or "MAIN" in diagram
