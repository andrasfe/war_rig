"""
Tests for the CrossReferenceResolver.
"""

import pytest

from citadel.graph.model import Artifact, SourceLocation
from citadel.parser.engine import RawReference
from citadel.resolver.alias_resolver import AliasResolver
from citadel.resolver.cross_reference import CrossReferenceResolver
from citadel.resolver.registry import ArtifactRegistry
from citadel.specs.schema import (
    AliasRule,
    ArtifactCategory,
    ArtifactType,
    RelationshipType,
)


@pytest.fixture
def registry() -> ArtifactRegistry:
    """Create a populated artifact registry for testing."""
    reg = ArtifactRegistry()

    # Add some test artifacts
    reg.register(
        Artifact(
            id="table::CUSTOMER",
            artifact_type=ArtifactType.TABLE,
            category=ArtifactCategory.DATA,
            canonical_name="CUSTOMER",
            language="sql",
        )
    )
    reg.register(
        Artifact(
            id="table::ORDER_HEADER",
            artifact_type=ArtifactType.TABLE,
            category=ArtifactCategory.DATA,
            canonical_name="ORDER_HEADER",
            language="sql",
        )
    )
    reg.register(
        Artifact(
            id="program::CUSTMAINT",
            artifact_type=ArtifactType.PROGRAM,
            category=ArtifactCategory.CODE,
            canonical_name="CUSTMAINT",
            language="cobol",
        )
    )
    reg.register(
        Artifact(
            id="copybook::CUSTOMER_REC",
            artifact_type=ArtifactType.COPYBOOK,
            category=ArtifactCategory.CODE,
            canonical_name="CUSTOMER_REC",
            aliases=["CUST-REC", "CUSTREC"],
            language="cobol",
        )
    )

    return reg


@pytest.fixture
def alias_resolver(registry: ArtifactRegistry) -> AliasResolver:
    """Create an alias resolver with test rules."""
    rules = [
        AliasRule(
            source_type=ArtifactType.PROGRAM,
            target_type=ArtifactType.TABLE,
            transformations=["uppercase", "remove_hyphens"],
            confidence_base=0.9,
        ),
    ]
    return AliasResolver(registry, rules)


@pytest.fixture
def resolver(
    registry: ArtifactRegistry, alias_resolver: AliasResolver
) -> CrossReferenceResolver:
    """Create a cross-reference resolver for testing."""
    return CrossReferenceResolver(registry, alias_resolver)


def make_raw_reference(
    raw_text: str,
    relationship_type: RelationshipType = RelationshipType.READS,
    expected_type: ArtifactType | None = None,
    containing_artifact: str | None = None,
) -> RawReference:
    """Helper to create RawReference objects for testing."""
    return RawReference(
        raw_text=raw_text,
        pattern_name="test_pattern",
        expected_type=expected_type,
        relationship_type=relationship_type,
        location=SourceLocation(file_path="test.cbl", line_start=10),
        containing_artifact=containing_artifact,
    )


class TestCrossReferenceResolver:
    """Tests for CrossReferenceResolver."""

    def test_resolve_exact_match(self, resolver: CrossReferenceResolver) -> None:
        """Test resolving a reference with exact name match."""
        ref = make_raw_reference(
            "CUSTOMER",
            RelationshipType.READS,
            ArtifactType.TABLE,
            "program::CUSTMAINT",
        )

        resolved, unresolved = resolver.resolve_all([ref])

        assert len(resolved) == 1
        assert len(unresolved) == 0
        assert resolved[0].to_artifact == "table::CUSTOMER"
        assert resolved[0].from_artifact == "program::CUSTMAINT"
        assert resolved[0].confidence == 1.0
        assert resolved[0].resolution_method == "exact"

    def test_resolve_alias_match(self, resolver: CrossReferenceResolver) -> None:
        """Test resolving a reference via alias."""
        ref = make_raw_reference(
            "CUST-REC",
            RelationshipType.INCLUDES,
            ArtifactType.COPYBOOK,
            "program::CUSTMAINT",
        )

        resolved, unresolved = resolver.resolve_all([ref])

        assert len(resolved) == 1
        assert len(unresolved) == 0
        assert resolved[0].to_artifact == "copybook::CUSTOMER_REC"

    def test_unresolved_reference(self, resolver: CrossReferenceResolver) -> None:
        """Test that unknown references become unresolved."""
        ref = make_raw_reference(
            "UNKNOWN_TABLE",
            RelationshipType.READS,
            ArtifactType.TABLE,
            "program::CUSTMAINT",
        )

        resolved, unresolved = resolver.resolve_all([ref])

        assert len(resolved) == 0
        assert len(unresolved) == 1
        assert unresolved[0].reference_text == "UNKNOWN_TABLE"
        assert unresolved[0].expected_type == ArtifactType.TABLE

    def test_resolve_multiple_references(
        self, resolver: CrossReferenceResolver
    ) -> None:
        """Test resolving multiple references at once."""
        refs = [
            make_raw_reference("CUSTOMER", RelationshipType.READS, ArtifactType.TABLE),
            make_raw_reference(
                "ORDER_HEADER", RelationshipType.READS, ArtifactType.TABLE
            ),
            make_raw_reference(
                "UNKNOWN", RelationshipType.READS, ArtifactType.TABLE
            ),
        ]

        resolved, unresolved = resolver.resolve_all(refs)

        assert len(resolved) == 2
        assert len(unresolved) == 1

    def test_deduplication(self, resolver: CrossReferenceResolver) -> None:
        """Test that duplicate relationships are deduplicated."""
        refs = [
            make_raw_reference(
                "CUSTOMER",
                RelationshipType.READS,
                ArtifactType.TABLE,
                "program::CUSTMAINT",
            ),
            make_raw_reference(
                "CUSTOMER",
                RelationshipType.READS,
                ArtifactType.TABLE,
                "program::CUSTMAINT",
            ),
        ]

        resolved, unresolved = resolver.resolve_all(refs)

        # Should only have one relationship after dedup
        assert len(resolved) == 1
        assert len(unresolved) == 0

    def test_resolution_stats(self, resolver: CrossReferenceResolver) -> None:
        """Test that resolution statistics are tracked correctly."""
        refs = [
            make_raw_reference("CUSTOMER", RelationshipType.READS, ArtifactType.TABLE),
            make_raw_reference("UNKNOWN", RelationshipType.READS, ArtifactType.TABLE),
        ]

        resolver.resolve_all(refs)
        stats = resolver.get_resolution_stats()

        assert "exact" in stats or "alias" in stats or "transformation" in stats
        assert stats.get("unresolved", 0) == 1

    def test_resolution_rate(self, resolver: CrossReferenceResolver) -> None:
        """Test resolution rate calculation."""
        refs = [
            make_raw_reference("CUSTOMER", RelationshipType.READS, ArtifactType.TABLE),
            make_raw_reference("UNKNOWN", RelationshipType.READS, ArtifactType.TABLE),
        ]

        resolver.resolve_all(refs)
        rate = resolver.get_resolution_rate()

        assert rate == 50.0  # 1 resolved out of 2

    def test_resolve_single(self, resolver: CrossReferenceResolver) -> None:
        """Test resolving a single reference."""
        ref = make_raw_reference(
            "CUSTOMER",
            RelationshipType.READS,
            ArtifactType.TABLE,
            "program::CUSTMAINT",
        )

        result = resolver.resolve_single(ref)

        # Should return a Relationship for successful resolution
        from citadel.graph.model import Relationship

        assert isinstance(result, Relationship)
        assert result.to_artifact == "table::CUSTOMER"

    def test_resolve_single_unresolved(
        self, resolver: CrossReferenceResolver
    ) -> None:
        """Test that resolve_single returns UnresolvedReference for failures."""
        ref = make_raw_reference(
            "NONEXISTENT",
            RelationshipType.READS,
            ArtifactType.TABLE,
        )

        result = resolver.resolve_single(ref)

        from citadel.graph.model import UnresolvedReference

        assert isinstance(result, UnresolvedReference)
        assert result.reference_text == "NONEXISTENT"

    def test_synthetic_source_for_missing_containing_artifact(
        self, resolver: CrossReferenceResolver
    ) -> None:
        """Test that file path is used as source when containing_artifact is None."""
        ref = make_raw_reference(
            "CUSTOMER",
            RelationshipType.READS,
            ArtifactType.TABLE,
            containing_artifact=None,  # No containing artifact
        )

        resolved, _ = resolver.resolve_all([ref])

        assert len(resolved) == 1
        assert resolved[0].from_artifact == "file::test.cbl"

    def test_reset_stats(self, resolver: CrossReferenceResolver) -> None:
        """Test that stats can be reset."""
        refs = [
            make_raw_reference("CUSTOMER", RelationshipType.READS, ArtifactType.TABLE),
        ]

        resolver.resolve_all(refs)
        assert resolver.get_resolution_rate() > 0

        resolver.reset_stats()
        assert resolver.get_resolution_rate() == 0.0
