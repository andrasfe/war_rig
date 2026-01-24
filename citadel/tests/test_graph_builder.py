"""
Tests for the GraphBuilder.
"""

from pathlib import Path

import pytest

from citadel.config import CitadelConfig
from citadel.graph.builder import GraphBuilder
from citadel.graph.model import (
    Artifact,
    Relationship,
    SourceLocation,
    UnresolvedReference,
)
from citadel.specs.schema import (
    ArtifactCategory,
    ArtifactType,
    RelationshipType,
)


@pytest.fixture
def config() -> CitadelConfig:
    """Create a test configuration."""
    return CitadelConfig(
        cache_dir=Path("/tmp/citadel_test"),
        llm_model="test-model",
        llm_disambiguation=False,
        parallel_files=2,
    )


@pytest.fixture
def builder(tmp_path: Path, config: CitadelConfig) -> GraphBuilder:
    """Create a GraphBuilder for testing."""
    source_root = tmp_path / "source"
    source_root.mkdir()
    return GraphBuilder(source_root, config)


def make_artifact(
    artifact_id: str,
    artifact_type: ArtifactType = ArtifactType.PROGRAM,
    language: str = "cobol",
) -> Artifact:
    """Helper to create Artifact objects for testing."""
    return Artifact(
        id=artifact_id,
        artifact_type=artifact_type,
        category=ArtifactCategory.CODE,
        canonical_name=artifact_id.split("::")[-1],
        language=language,
    )


def make_relationship(
    from_id: str,
    to_id: str,
    rel_type: RelationshipType = RelationshipType.CALLS,
) -> Relationship:
    """Helper to create Relationship objects for testing."""
    import uuid

    return Relationship(
        id=str(uuid.uuid4()),
        from_artifact=from_id,
        to_artifact=to_id,
        relationship_type=rel_type,
        location=SourceLocation(file_path="test.cbl", line_start=10),
        evidence_text="CALL TEST",
    )


def make_unresolved(
    reference_text: str,
    expected_type: ArtifactType | None = None,
) -> UnresolvedReference:
    """Helper to create UnresolvedReference objects for testing."""
    return UnresolvedReference(
        reference_text=reference_text,
        expected_type=expected_type,
        location=SourceLocation(file_path="test.cbl", line_start=10),
        reason="no match found",
    )


class TestGraphBuilder:
    """Tests for GraphBuilder."""

    def test_add_artifacts(self, builder: GraphBuilder) -> None:
        """Test adding artifacts to the builder."""
        artifacts = [
            make_artifact("program::MAIN"),
            make_artifact("program::UTIL"),
        ]

        builder.add_artifacts(artifacts)

        assert builder.get_artifact_count() == 2
        assert builder.get_artifact("program::MAIN") is not None
        assert builder.get_artifact("program::UTIL") is not None

    def test_add_artifacts_deduplication(self, builder: GraphBuilder) -> None:
        """Test that duplicate artifacts are handled."""
        artifact1 = make_artifact("program::MAIN")
        artifact2 = make_artifact("program::MAIN")
        artifact2.defined_in = SourceLocation(file_path="main.cbl", line_start=1)

        builder.add_artifacts([artifact1])
        builder.add_artifacts([artifact2])

        # Should still have only one artifact
        assert builder.get_artifact_count() == 1
        # Should have the version with defined_in
        assert builder.get_artifact("program::MAIN").defined_in is not None

    def test_add_relationships(self, builder: GraphBuilder) -> None:
        """Test adding relationships to the builder."""
        artifacts = [
            make_artifact("program::MAIN"),
            make_artifact("program::UTIL"),
        ]
        builder.add_artifacts(artifacts)

        relationships = [
            make_relationship("program::MAIN", "program::UTIL"),
        ]
        builder.add_relationships(relationships)

        assert builder.get_relationship_count() == 1

    def test_relationships_with_unknown_artifacts(
        self, builder: GraphBuilder
    ) -> None:
        """Test that relationships with unknown artifacts are tracked."""
        artifacts = [make_artifact("program::MAIN")]
        builder.add_artifacts(artifacts)

        # Relationship references unknown artifact
        relationships = [
            make_relationship("program::MAIN", "program::UNKNOWN"),
        ]
        builder.add_relationships(relationships)

        # Should not be added to main relationships
        assert builder.get_relationship_count() == 0
        # Should be tracked as orphan
        assert len(builder.get_orphan_relationships()) == 1

    def test_add_unresolved(self, builder: GraphBuilder) -> None:
        """Test adding unresolved references."""
        unresolved = [
            make_unresolved("UNKNOWN_TABLE", ArtifactType.TABLE),
            make_unresolved("MYSTERY_PROG"),
        ]
        builder.add_unresolved(unresolved)

        assert builder.get_unresolved_count() == 2

    def test_set_specs_used(self, builder: GraphBuilder) -> None:
        """Test setting specs used."""
        specs = {"cobol": "cobol-v1", "sql": "sql-v1"}
        builder.set_specs_used(specs)

        # Build graph and check specs are included
        builder.add_artifacts([make_artifact("program::MAIN")])
        graph = builder.build()

        assert graph.statistics.specs_used == specs

    def test_file_counts(self, builder: GraphBuilder) -> None:
        """Test recording file processing counts."""
        builder.record_file_analyzed()
        builder.record_file_analyzed()
        builder.record_file_skipped()
        builder.record_file_failed()

        builder.add_artifacts([make_artifact("program::MAIN")])
        graph = builder.build()

        assert graph.statistics.files_analyzed == 2
        assert graph.statistics.files_skipped == 1
        assert graph.statistics.files_failed == 1

    def test_set_file_counts(self, builder: GraphBuilder) -> None:
        """Test setting file counts directly."""
        builder.set_file_counts(analyzed=10, skipped=2, failed=1)

        builder.add_artifacts([make_artifact("program::MAIN")])
        graph = builder.build()

        assert graph.statistics.files_analyzed == 10
        assert graph.statistics.files_skipped == 2
        assert graph.statistics.files_failed == 1

    def test_build_computes_statistics(self, builder: GraphBuilder) -> None:
        """Test that build() computes correct statistics."""
        artifacts = [
            make_artifact("program::MAIN", ArtifactType.PROGRAM),
            make_artifact("program::UTIL", ArtifactType.PROGRAM),
            make_artifact("table::CUSTOMER", ArtifactType.TABLE, "sql"),
        ]
        builder.add_artifacts(artifacts)

        relationships = [
            make_relationship(
                "program::MAIN", "program::UTIL", RelationshipType.CALLS
            ),
            make_relationship(
                "program::MAIN", "table::CUSTOMER", RelationshipType.READS
            ),
        ]
        builder.add_relationships(relationships)

        builder.add_unresolved([make_unresolved("UNKNOWN")])

        graph = builder.build()

        assert graph.statistics.artifacts_total == 3
        assert graph.statistics.artifacts_by_type["program"] == 2
        assert graph.statistics.artifacts_by_type["table"] == 1
        assert graph.statistics.relationships_total == 2
        assert graph.statistics.relationships_by_type["calls"] == 1
        assert graph.statistics.relationships_by_type["reads"] == 1
        assert graph.statistics.unresolved_count == 1

    def test_build_includes_all_components(self, builder: GraphBuilder) -> None:
        """Test that build() includes all added components."""
        artifacts = [make_artifact("program::MAIN")]
        relationships = [
            make_relationship("program::MAIN", "program::MAIN", RelationshipType.CALLS)
        ]  # Self-reference
        unresolved = [make_unresolved("UNKNOWN")]

        builder.add_artifacts(artifacts)
        builder.add_relationships(relationships)
        builder.add_unresolved(unresolved)

        graph = builder.build()

        assert len(graph.artifacts) == 1
        assert len(graph.relationships) == 1
        assert len(graph.unresolved) == 1

    def test_resolution_rate_calculation(self, builder: GraphBuilder) -> None:
        """Test that resolution rate is calculated correctly."""
        builder.add_artifacts([make_artifact("program::MAIN")])
        builder.add_relationships(
            [make_relationship("program::MAIN", "program::MAIN")]
        )
        builder.add_unresolved([make_unresolved("UNKNOWN")])

        graph = builder.build()

        # 1 resolved out of 2 total = 50%
        assert graph.statistics.resolution_rate == 50.0

    def test_resolution_rate_all_resolved(self, builder: GraphBuilder) -> None:
        """Test resolution rate when all references are resolved."""
        builder.add_artifacts([make_artifact("program::MAIN")])
        builder.add_relationships(
            [make_relationship("program::MAIN", "program::MAIN")]
        )
        # No unresolved

        graph = builder.build()

        assert graph.statistics.resolution_rate == 100.0

    def test_languages_detected(self, builder: GraphBuilder) -> None:
        """Test that languages are detected and tracked."""
        artifacts = [
            make_artifact("program::MAIN", ArtifactType.PROGRAM, "cobol"),
            make_artifact("table::CUSTOMER", ArtifactType.TABLE, "sql"),
        ]
        builder.add_artifacts(artifacts)

        graph = builder.build()

        assert set(graph.statistics.languages_detected) == {"cobol", "sql"}

    def test_config_hash(self, builder: GraphBuilder) -> None:
        """Test that config hash is computed."""
        builder.add_artifacts([make_artifact("program::MAIN")])
        graph = builder.build()

        assert graph.config_hash is not None
        assert len(graph.config_hash) == 16  # First 16 chars of SHA256

    def test_get_artifacts_by_type(self, builder: GraphBuilder) -> None:
        """Test getting artifacts filtered by type."""
        artifacts = [
            make_artifact("program::MAIN", ArtifactType.PROGRAM),
            make_artifact("program::UTIL", ArtifactType.PROGRAM),
            make_artifact("table::CUSTOMER", ArtifactType.TABLE),
        ]
        builder.add_artifacts(artifacts)

        programs = builder.get_artifacts_by_type("program")
        tables = builder.get_artifacts_by_type("table")

        assert len(programs) == 2
        assert len(tables) == 1

    def test_get_relationships_for_artifact(self, builder: GraphBuilder) -> None:
        """Test getting relationships for a specific artifact."""
        artifacts = [
            make_artifact("program::MAIN"),
            make_artifact("program::A"),
            make_artifact("program::B"),
        ]
        builder.add_artifacts(artifacts)

        relationships = [
            make_relationship("program::MAIN", "program::A"),  # outgoing
            make_relationship("program::B", "program::MAIN"),  # incoming
        ]
        builder.add_relationships(relationships)

        outgoing = builder.get_relationships_for("program::MAIN", "outgoing")
        incoming = builder.get_relationships_for("program::MAIN", "incoming")
        both = builder.get_relationships_for("program::MAIN", "both")

        assert len(outgoing) == 1
        assert len(incoming) == 1
        assert len(both) == 2

    def test_validate_warns_on_orphans(self, builder: GraphBuilder) -> None:
        """Test that validation warns about orphan relationships."""
        builder.add_artifacts([make_artifact("program::MAIN")])
        builder.add_relationships(
            [make_relationship("program::MAIN", "program::UNKNOWN")]
        )

        warnings = builder.validate()

        assert any("unknown artifacts" in w for w in warnings)

    def test_validate_warns_on_isolated_artifacts(
        self, builder: GraphBuilder
    ) -> None:
        """Test that validation warns about isolated artifacts."""
        builder.add_artifacts([make_artifact("program::ISOLATED")])
        # No relationships

        warnings = builder.validate()

        assert any("no relationships" in w for w in warnings)

    def test_validate_warns_on_high_unresolved_rate(
        self, builder: GraphBuilder
    ) -> None:
        """Test that validation warns about high unresolved rate."""
        builder.add_artifacts([make_artifact("program::MAIN")])
        # 1 relationship, but many unresolved
        builder.add_relationships(
            [make_relationship("program::MAIN", "program::MAIN")]
        )
        for i in range(10):
            builder.add_unresolved([make_unresolved(f"UNKNOWN_{i}")])

        warnings = builder.validate()

        assert any("unresolved reference rate" in w for w in warnings)

    def test_merge_builders(
        self, builder: GraphBuilder, tmp_path: Path, config: CitadelConfig
    ) -> None:
        """Test merging two graph builders."""
        other_root = tmp_path / "other"
        other_root.mkdir()
        other = GraphBuilder(other_root, config)

        # Add to first builder
        builder.add_artifacts([make_artifact("program::MAIN")])
        builder.record_file_analyzed()

        # Add to second builder
        other.add_artifacts([make_artifact("program::UTIL")])
        other.record_file_analyzed()

        # Merge
        builder.merge(other)

        assert builder.get_artifact_count() == 2
        graph = builder.build()
        assert graph.statistics.files_analyzed == 2

    def test_clear(self, builder: GraphBuilder) -> None:
        """Test clearing the builder."""
        builder.add_artifacts([make_artifact("program::MAIN")])
        builder.add_unresolved([make_unresolved("UNKNOWN")])
        builder.record_file_analyzed()

        builder.clear()

        assert builder.get_artifact_count() == 0
        assert builder.get_relationship_count() == 0
        assert builder.get_unresolved_count() == 0

    def test_source_root_in_graph(
        self, tmp_path: Path, config: CitadelConfig
    ) -> None:
        """Test that source root is captured in the graph."""
        source_root = tmp_path / "my_source"
        source_root.mkdir()
        builder = GraphBuilder(source_root, config)

        builder.add_artifacts([make_artifact("program::MAIN")])
        graph = builder.build()

        assert str(source_root.absolute()) in graph.source_root
