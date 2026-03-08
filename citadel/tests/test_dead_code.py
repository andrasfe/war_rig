"""
Tests for the dead code detection algorithm.

These tests verify correctness of dead code detection using synthetic
dependency graphs, without any file I/O. They cover entry point
exclusion, different artifact types, type filtering, and edge cases.
"""

from __future__ import annotations

import pytest

from citadel.analysis.dead_code import (
    DeadCodeItem,
    dead_code_summary,
    dead_code_to_dicts,
    find_dead_code,
)
from citadel.graph.model import (
    Artifact,
    DependencyGraph,
    GraphStatistics,
    Relationship,
    SourceLocation,
)
from citadel.specs.schema import (
    ArtifactCategory,
    ArtifactType,
    RelationshipType,
)

# ---------------------------------------------------------------------------
# Helper factories
# ---------------------------------------------------------------------------

def _make_stats(**overrides) -> GraphStatistics:
    """Create a minimal GraphStatistics for testing."""
    defaults = {
        "files_analyzed": 1,
        "files_skipped": 0,
        "files_failed": 0,
        "artifacts_total": 0,
        "relationships_total": 0,
        "unresolved_count": 0,
        "resolution_rate": 100.0,
    }
    defaults.update(overrides)
    return GraphStatistics(**defaults)


def _make_artifact(
    artifact_id: str,
    name: str,
    art_type: ArtifactType,
    category: ArtifactCategory = ArtifactCategory.CODE,
    file_path: str = "SRC.cbl",
    line_start: int = 1,
    language: str = "cobol",
) -> tuple[str, Artifact]:
    """Create an (id, Artifact) tuple for inserting into a graph."""
    return artifact_id, Artifact(
        id=artifact_id,
        artifact_type=art_type,
        category=category,
        canonical_name=name,
        defined_in=SourceLocation(file_path=file_path, line_start=line_start),
        language=language,
    )


def _make_relationship(
    from_id: str,
    to_id: str,
    rel_type: RelationshipType,
    rel_id: str | None = None,
) -> Relationship:
    """Create a Relationship edge for testing."""
    return Relationship(
        id=rel_id or f"{from_id}->{to_id}",
        from_artifact=from_id,
        to_artifact=to_id,
        relationship_type=rel_type,
        location=SourceLocation(file_path="SRC.cbl", line_start=1),
        evidence_text=f"{from_id} -> {to_id}",
    )


def _make_graph(
    artifacts: dict[str, Artifact],
    relationships: list[Relationship] | None = None,
) -> DependencyGraph:
    """Build a DependencyGraph from artifacts and relationships."""
    return DependencyGraph(
        source_root="/test",
        artifacts=artifacts,
        relationships=relationships or [],
        statistics=_make_stats(
            artifacts_total=len(artifacts),
            relationships_total=len(relationships or []),
        ),
        config_hash="test",
    )


# ---------------------------------------------------------------------------
# Test classes
# ---------------------------------------------------------------------------


class TestEmptyGraph:
    """Edge case: empty dependency graph."""

    def test_empty_graph_returns_empty(self):
        """An empty graph should produce zero dead code items."""
        graph = _make_graph(artifacts={})
        dead = find_dead_code(graph)
        assert dead == []

    def test_no_relationships_but_all_entry_points(self):
        """Programs with no edges are entry points, not dead code."""
        artifacts = dict([
            _make_artifact("program::A", "A", ArtifactType.PROGRAM),
            _make_artifact("program::B", "B", ArtifactType.PROGRAM),
        ])
        graph = _make_graph(artifacts)
        dead = find_dead_code(graph)
        assert dead == []


class TestAllDead:
    """Edge case: every artifact is dead code."""

    def test_paragraphs_excluded_from_graph_detection(self):
        """Paragraphs are excluded from graph-based detection (use AST instead)."""
        artifacts = dict([
            _make_artifact("program::MAIN", "MAIN", ArtifactType.PROGRAM,
                           file_path="MAIN.cbl", line_start=1),
            _make_artifact("paragraph::MAIN-PARA", "MAIN-PARA",
                           ArtifactType.PARAGRAPH,
                           file_path="MAIN.cbl", line_start=50),
            _make_artifact("paragraph::DEAD-A", "DEAD-A",
                           ArtifactType.PARAGRAPH,
                           file_path="MAIN.cbl", line_start=100),
        ])
        graph = _make_graph(artifacts)

        dead = find_dead_code(graph)

        # No paragraphs should appear — graph can't detect intra-file refs
        dead_names = {item.name for item in dead}
        assert "DEAD-A" not in dead_names
        assert "MAIN-PARA" not in dead_names
        assert "MAIN" not in dead_names

    def test_single_dead_copybook(self):
        """A copybook that nobody includes is dead code."""
        artifacts = dict([
            _make_artifact("copybook::UNUSED", "UNUSED", ArtifactType.COPYBOOK,
                           file_path="UNUSED.cpy", line_start=1),
        ])
        graph = _make_graph(artifacts)

        dead = find_dead_code(graph)

        assert len(dead) == 1
        assert dead[0].name == "UNUSED"
        assert dead[0].artifact_type == "copybook"
        assert "never included" in dead[0].reason


class TestNoneDead:
    """Edge case: everything is referenced."""

    def test_fully_connected_graph(self):
        """When every artifact has at least one incoming edge, no dead code."""
        artifacts = dict([
            _make_artifact("program::MAIN", "MAIN", ArtifactType.PROGRAM,
                           file_path="MAIN.cbl"),
            _make_artifact("paragraph::INIT", "INIT", ArtifactType.PARAGRAPH,
                           file_path="MAIN.cbl", line_start=50),
            _make_artifact("paragraph::PROCESS", "PROCESS",
                           ArtifactType.PARAGRAPH,
                           file_path="MAIN.cbl", line_start=100),
            _make_artifact("copybook::UTILS", "UTILS", ArtifactType.COPYBOOK,
                           file_path="UTILS.cpy"),
        ])
        relationships = [
            _make_relationship("paragraph::INIT", "paragraph::PROCESS",
                               RelationshipType.PERFORMS),
            _make_relationship("program::MAIN", "copybook::UTILS",
                               RelationshipType.INCLUDES),
            _make_relationship("program::MAIN", "paragraph::INIT",
                               RelationshipType.PERFORMS),
        ]
        graph = _make_graph(artifacts, relationships)

        dead = find_dead_code(graph)

        # MAIN is a program (entry point), INIT has incoming from MAIN,
        # PROCESS has incoming from INIT, UTILS has incoming from MAIN.
        assert dead == []


class TestEntryPointExclusion:
    """Tests for entry point detection and exclusion logic."""

    def test_programs_are_entry_points(self):
        """Programs are always considered entry points."""
        artifacts = dict([
            _make_artifact("program::COBPROG", "COBPROG",
                           ArtifactType.PROGRAM),
        ])
        graph = _make_graph(artifacts)

        dead = find_dead_code(graph)

        assert len(dead) == 0

    def test_jcl_procedures_are_entry_points(self):
        """JCL procedures are always entry points."""
        artifacts = dict([
            _make_artifact("procedure::MYJOB", "MYJOB",
                           ArtifactType.PROCEDURE,
                           file_path="JOB.jcl", language="jcl"),
        ])
        graph = _make_graph(artifacts)

        dead = find_dead_code(graph)

        assert len(dead) == 0

    def test_jcl_executed_program_is_entry_point(self):
        """A program executed via EXEC PGM= in JCL is an entry point."""
        artifacts = dict([
            _make_artifact("procedure::STEP1", "STEP1",
                           ArtifactType.PROCEDURE,
                           file_path="JOB.jcl", language="jcl"),
            _make_artifact("program::COBPROG", "COBPROG",
                           ArtifactType.PROGRAM,
                           file_path="COBPROG.cbl"),
        ])
        relationships = [
            _make_relationship("procedure::STEP1", "program::COBPROG",
                               RelationshipType.EXECUTES),
        ]
        graph = _make_graph(artifacts, relationships)

        dead = find_dead_code(graph)

        assert len(dead) == 0

    def test_paragraphs_always_excluded(self):
        """All paragraphs are excluded from graph-based detection."""
        artifacts = dict([
            _make_artifact("program::MAIN", "MAIN", ArtifactType.PROGRAM,
                           file_path="MAIN.cbl", line_start=1),
            _make_artifact("paragraph::MAIN-PARA", "MAIN-PARA",
                           ArtifactType.PARAGRAPH,
                           file_path="MAIN.cbl", line_start=50),
            _make_artifact("paragraph::HELPER", "HELPER",
                           ArtifactType.PARAGRAPH,
                           file_path="MAIN.cbl", line_start=100),
        ])
        graph = _make_graph(artifacts)

        dead = find_dead_code(graph)

        dead_names = {item.name for item in dead}
        assert "MAIN-PARA" not in dead_names
        assert "HELPER" not in dead_names


class TestDifferentArtifactTypes:
    """Tests for dead code detection across different artifact types."""

    def test_unreferenced_copybook(self):
        """Copybooks with no INCLUDES edge are dead."""
        artifacts = dict([
            _make_artifact("program::MAIN", "MAIN", ArtifactType.PROGRAM),
            _make_artifact("copybook::USED", "USED", ArtifactType.COPYBOOK,
                           file_path="USED.cpy"),
            _make_artifact("copybook::UNUSED", "UNUSED",
                           ArtifactType.COPYBOOK, file_path="UNUSED.cpy"),
        ])
        relationships = [
            _make_relationship("program::MAIN", "copybook::USED",
                               RelationshipType.INCLUDES),
        ]
        graph = _make_graph(artifacts, relationships)

        dead = find_dead_code(graph)

        assert len(dead) == 1
        assert dead[0].name == "UNUSED"
        assert dead[0].artifact_type == "copybook"

    def test_unreferenced_table(self):
        """Tables with no READ/WRITE edges are dead."""
        artifacts = dict([
            _make_artifact("program::MAIN", "MAIN", ArtifactType.PROGRAM),
            _make_artifact("table::CUSTMAST", "CUSTMAST", ArtifactType.TABLE,
                           category=ArtifactCategory.DATA,
                           file_path="DDL.sql", language="sql"),
            _make_artifact("table::ORPHAN", "ORPHAN", ArtifactType.TABLE,
                           category=ArtifactCategory.DATA,
                           file_path="DDL.sql", language="sql", line_start=50),
        ])
        relationships = [
            _make_relationship("program::MAIN", "table::CUSTMAST",
                               RelationshipType.READS),
        ]
        graph = _make_graph(artifacts, relationships)

        dead = find_dead_code(graph)

        assert len(dead) == 1
        assert dead[0].name == "ORPHAN"
        assert dead[0].artifact_type == "table"
        assert "never read" in dead[0].reason

    def test_unreferenced_screen(self):
        """Screens/maps with no SEND/RECEIVE are dead."""
        artifacts = dict([
            _make_artifact("program::MAIN", "MAIN", ArtifactType.PROGRAM),
            _make_artifact("screen::MENU-MAP", "MENU-MAP",
                           ArtifactType.SCREEN,
                           category=ArtifactCategory.INTERFACE,
                           file_path="MAPS.bms"),
            _make_artifact("screen::OLD-MAP", "OLD-MAP",
                           ArtifactType.SCREEN,
                           category=ArtifactCategory.INTERFACE,
                           file_path="MAPS.bms", line_start=50),
        ])
        relationships = [
            _make_relationship("program::MAIN", "screen::MENU-MAP",
                               RelationshipType.SENDS_TO),
        ]
        graph = _make_graph(artifacts, relationships)

        dead = find_dead_code(graph)

        assert len(dead) == 1
        assert dead[0].name == "OLD-MAP"

    def test_mixed_types(self):
        """Dead code detection across multiple artifact types at once."""
        artifacts = dict([
            _make_artifact("program::MAIN", "MAIN", ArtifactType.PROGRAM,
                           file_path="MAIN.cbl", line_start=1),
            _make_artifact("paragraph::MAIN-PARA", "MAIN-PARA",
                           ArtifactType.PARAGRAPH,
                           file_path="MAIN.cbl", line_start=50),
            _make_artifact("copybook::USED-CPY", "USED-CPY",
                           ArtifactType.COPYBOOK, file_path="USED.cpy"),
            _make_artifact("copybook::DEAD-CPY", "DEAD-CPY",
                           ArtifactType.COPYBOOK, file_path="DEAD.cpy"),
        ])
        relationships = [
            _make_relationship("program::MAIN", "copybook::USED-CPY",
                               RelationshipType.INCLUDES),
        ]
        graph = _make_graph(artifacts, relationships)

        dead = find_dead_code(graph)

        dead_names = {item.name for item in dead}
        # Paragraphs excluded from graph-based detection
        assert "MAIN-PARA" not in dead_names
        assert "DEAD-CPY" in dead_names
        assert "USED-CPY" not in dead_names
        assert "MAIN" not in dead_names


class TestTypeFiltering:
    """Tests for the exclude_types and include_only_types parameters."""

    def _build_mixed_graph(self) -> DependencyGraph:
        """Build a graph with dead code of multiple types."""
        artifacts = dict([
            _make_artifact("program::MAIN", "MAIN", ArtifactType.PROGRAM),
            _make_artifact("copybook::DEAD", "DEAD-CPY",
                           ArtifactType.COPYBOOK, file_path="DEAD.cpy"),
            _make_artifact("table::DEAD", "DEAD-TBL", ArtifactType.TABLE,
                           category=ArtifactCategory.DATA,
                           file_path="DDL.sql", language="sql"),
        ])
        return _make_graph(artifacts)

    def test_exclude_types(self):
        """Excluding table type should omit tables from results."""
        graph = self._build_mixed_graph()
        dead = find_dead_code(graph, exclude_types={"table"})

        dead_types = {item.artifact_type for item in dead}
        assert "table" not in dead_types
        assert "copybook" in dead_types

    def test_include_only_types(self):
        """Including only copybook type should limit results."""
        graph = self._build_mixed_graph()
        dead = find_dead_code(graph, include_only_types={"copybook"})

        assert all(item.artifact_type == "copybook" for item in dead)
        assert len(dead) == 1

    def test_mutually_exclusive_filters_raises(self):
        """Setting both exclude and include should raise ValueError."""
        graph = self._build_mixed_graph()
        with pytest.raises(ValueError, match="Cannot specify both"):
            find_dead_code(
                graph,
                exclude_types={"table"},
                include_only_types={"copybook"},
            )


class TestReasonMessages:
    """Tests for human-readable reason messages."""

    def test_copybook_reason(self):
        """Copybook dead code should mention COPY."""
        artifacts = dict([
            _make_artifact("copybook::X", "X", ArtifactType.COPYBOOK),
        ])
        graph = _make_graph(artifacts)
        dead = find_dead_code(graph)

        assert len(dead) == 1
        assert "COPY" in dead[0].reason

    def test_generic_reason(self):
        """Unknown types should get a generic reason."""
        artifacts = dict([
            _make_artifact("macro::X", "X", ArtifactType.MACRO),
        ])
        graph = _make_graph(artifacts)
        dead = find_dead_code(graph)

        assert len(dead) == 1
        assert "never referenced" in dead[0].reason


class TestSorting:
    """Tests for deterministic output ordering."""

    def test_sorted_by_type_then_file_then_name(self):
        """Results should be sorted by type, then file, then name."""
        artifacts = dict([
            # Two copybooks in different files
            _make_artifact("copybook::B", "B", ArtifactType.COPYBOOK,
                           file_path="B.cpy"),
            _make_artifact("copybook::A", "A", ArtifactType.COPYBOOK,
                           file_path="A.cpy"),
            # A table (different type, should come after copybook)
            _make_artifact("table::Z", "Z", ArtifactType.TABLE,
                           category=ArtifactCategory.DATA,
                           file_path="DDL.sql", language="sql"),
        ])
        graph = _make_graph(artifacts)
        dead = find_dead_code(graph)

        types = [item.artifact_type for item in dead]
        # Copybooks come before tables alphabetically
        assert types == sorted(types)


class TestDeadCodeItem:
    """Tests for the DeadCodeItem data class."""

    def test_to_dict(self):
        """to_dict should return all expected keys."""
        item = DeadCodeItem(
            name="TEST",
            artifact_type="paragraph",
            file="TEST.cbl",
            line=42,
            reason="Never performed",
        )
        d = item.to_dict()

        assert d["name"] == "TEST"
        assert d["type"] == "paragraph"
        assert d["file"] == "TEST.cbl"
        assert d["line"] == 42
        assert d["reason"] == "Never performed"

    def test_to_dict_with_none_file(self):
        """Artifacts without source location should have None file/line."""
        item = DeadCodeItem(
            name="X",
            artifact_type="copybook",
            file=None,
            line=None,
            reason="Test",
        )
        d = item.to_dict()

        assert d["file"] is None
        assert d["line"] is None


class TestConvenienceFunctions:
    """Tests for dead_code_to_dicts and dead_code_summary."""

    def test_to_dicts(self):
        """dead_code_to_dicts should convert items to list of dicts."""
        items = [
            DeadCodeItem("A", "paragraph", "X.cbl", 10, "reason A"),
            DeadCodeItem("B", "copybook", "Y.cpy", 1, "reason B"),
        ]
        dicts = dead_code_to_dicts(items)

        assert len(dicts) == 2
        assert dicts[0]["name"] == "A"
        assert dicts[1]["name"] == "B"

    def test_summary_counts(self):
        """dead_code_summary should count items by type."""
        items = [
            DeadCodeItem("A", "paragraph", "X.cbl", 10, "reason A"),
            DeadCodeItem("B", "paragraph", "X.cbl", 20, "reason B"),
            DeadCodeItem("C", "copybook", "Y.cpy", 1, "reason C"),
        ]
        summary = dead_code_summary(items)

        assert summary["total"] == 3
        assert summary["by_type"]["paragraph"] == 2
        assert summary["by_type"]["copybook"] == 1
        assert summary["files_affected"] == 2

    def test_summary_empty(self):
        """Summary of empty list should return zero counts."""
        summary = dead_code_summary([])

        assert summary["total"] == 0
        assert summary["by_type"] == {}
        assert summary["files_affected"] == 0


class TestComplexGraph:
    """Tests with realistic graph structures."""

    def test_multi_program_codebase(self):
        """
        Simulates a small mainframe codebase:
        - JCL procedure STEP1 executes PROG-A
        - PROG-A includes COMMON-CPY
        - DEAD-CPY is never included
        - Paragraphs are excluded from graph-based detection
        """
        artifacts = dict([
            _make_artifact("procedure::STEP1", "STEP1",
                           ArtifactType.PROCEDURE,
                           file_path="JOB.jcl", language="jcl"),
            _make_artifact("program::PROG-A", "PROG-A",
                           ArtifactType.PROGRAM,
                           file_path="PROGA.cbl", line_start=1),
            _make_artifact("paragraph::INIT-PARA", "INIT-PARA",
                           ArtifactType.PARAGRAPH,
                           file_path="PROGA.cbl", line_start=50),
            _make_artifact("copybook::COMMON-CPY", "COMMON-CPY",
                           ArtifactType.COPYBOOK, file_path="COMMON.cpy"),
            _make_artifact("copybook::DEAD-CPY", "DEAD-CPY",
                           ArtifactType.COPYBOOK, file_path="DEAD.cpy"),
        ])
        relationships = [
            _make_relationship("procedure::STEP1", "program::PROG-A",
                               RelationshipType.EXECUTES),
            _make_relationship("program::PROG-A", "copybook::COMMON-CPY",
                               RelationshipType.INCLUDES),
        ]
        graph = _make_graph(artifacts, relationships)

        dead = find_dead_code(graph)

        dead_names = {item.name for item in dead}
        assert "DEAD-CPY" in dead_names
        assert "STEP1" not in dead_names
        assert "PROG-A" not in dead_names
        assert "INIT-PARA" not in dead_names  # paragraphs excluded
        assert "COMMON-CPY" not in dead_names

    def test_artifact_without_source_location(self):
        """Artifacts without defined_in should still be analyzed."""
        aid = "copybook::GHOST"
        artifact = Artifact(
            id=aid,
            artifact_type=ArtifactType.COPYBOOK,
            category=ArtifactCategory.CODE,
            canonical_name="GHOST",
            defined_in=None,
            language="cobol",
        )
        graph = _make_graph(artifacts={aid: artifact})

        dead = find_dead_code(graph)

        assert len(dead) == 1
        assert dead[0].name == "GHOST"
        assert dead[0].file is None
        assert dead[0].line is None
