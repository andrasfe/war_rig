"""Unit tests for knowledge graph data models.

Tests:
- EntityType enum has all expected values
- RelationType enum has all expected values
- Entity creation and qualified_name property
- Triple creation with defaults
- RawTriple creation and qualified name properties
- Conflict creation with defaults
- Neighborhood creation with defaults
- TripleDelta computed properties (change_count, change_rate, has_converged)
"""

import pytest

from war_rig.knowledge_graph.models import (
    Conflict,
    Entity,
    EntityType,
    Neighborhood,
    RawTriple,
    RelationType,
    Triple,
    TripleDelta,
)


class TestEntityType:
    """Tests for EntityType enum."""

    def test_has_all_expected_values(self):
        expected = {
            "PROGRAM",
            "JCL_JOB",
            "JCL_STEP",
            "DATASET",
            "COPYBOOK",
            "FIELD",
            "PARAGRAPH",
            "DB_TABLE",
            "CUSTOM",
        }
        actual = {e.value for e in EntityType}
        assert actual == expected

    def test_count(self):
        assert len(EntityType) == 9

    def test_str_enum(self):
        assert EntityType.PROGRAM == "PROGRAM"
        assert isinstance(EntityType.PROGRAM, str)


class TestRelationType:
    """Tests for RelationType enum."""

    def test_has_all_expected_values(self):
        expected = {
            "CALLS",
            "READS",
            "WRITES",
            "INCLUDES",
            "EXECUTES",
            "DEFINES_INPUT",
            "DEFINES_OUTPUT",
            "CONTAINS_STEP",
            "DEFINES_FIELD",
            "QUERIES",
            "MODIFIES",
            "PERFORMS",
            "RELATED_TO",
        }
        actual = {r.value for r in RelationType}
        assert actual == expected

    def test_count(self):
        assert len(RelationType) == 13

    def test_str_enum(self):
        assert RelationType.CALLS == "CALLS"
        assert isinstance(RelationType.CALLS, str)


class TestEntity:
    """Tests for Entity model."""

    def test_create_minimal(self):
        entity = Entity(entity_type=EntityType.PROGRAM, name="ACCT0100")
        assert entity.entity_type == EntityType.PROGRAM
        assert entity.name == "ACCT0100"
        assert entity.id is None
        assert entity.properties == {}

    def test_create_with_id_and_properties(self):
        entity = Entity(
            id=42,
            entity_type=EntityType.DATASET,
            name="ACCT.MASTER.FILE",
            properties={"dsn": "ACCT.MASTER.FILE", "record_length": 80},
        )
        assert entity.id == 42
        assert entity.entity_type == EntityType.DATASET
        assert entity.name == "ACCT.MASTER.FILE"
        assert entity.properties["dsn"] == "ACCT.MASTER.FILE"
        assert entity.properties["record_length"] == 80

    def test_qualified_name(self):
        entity = Entity(entity_type=EntityType.PROGRAM, name="ACCT0100")
        assert entity.qualified_name == "PROGRAM:ACCT0100"

    def test_qualified_name_jcl_job(self):
        entity = Entity(entity_type=EntityType.JCL_JOB, name="ACCTJOB1")
        assert entity.qualified_name == "JCL_JOB:ACCTJOB1"

    def test_qualified_name_copybook(self):
        entity = Entity(entity_type=EntityType.COPYBOOK, name="ACCTCPY1")
        assert entity.qualified_name == "COPYBOOK:ACCTCPY1"

    def test_qualified_name_db_table(self):
        entity = Entity(entity_type=EntityType.DB_TABLE, name="ACCOUNT_TBL")
        assert entity.qualified_name == "DB_TABLE:ACCOUNT_TBL"

    def test_properties_default_empty_dict(self):
        entity = Entity(entity_type=EntityType.FIELD, name="WS-AMOUNT")
        assert entity.properties == {}
        assert isinstance(entity.properties, dict)


class TestTriple:
    """Tests for Triple model."""

    def test_create_minimal(self):
        triple = Triple(
            subject_id=1,
            predicate=RelationType.CALLS,
            object_id=2,
        )
        assert triple.subject_id == 1
        assert triple.predicate == RelationType.CALLS
        assert triple.object_id == 2

    def test_defaults(self):
        triple = Triple(
            subject_id=1,
            predicate=RelationType.READS,
            object_id=2,
        )
        assert triple.id is None
        assert triple.confirmed is False
        assert triple.corroboration_count == 1
        assert triple.source_pass is None
        assert triple.source_artifact is None
        assert triple.timestamp is not None

    def test_create_with_all_fields(self):
        triple = Triple(
            id=10,
            subject_id=1,
            predicate=RelationType.WRITES,
            object_id=3,
            source_pass="pass_2",
            source_artifact="ACCT0100.cbl",
            confirmed=True,
            corroboration_count=3,
        )
        assert triple.id == 10
        assert triple.source_pass == "pass_2"
        assert triple.source_artifact == "ACCT0100.cbl"
        assert triple.confirmed is True
        assert triple.corroboration_count == 3

    def test_corroboration_count_minimum(self):
        with pytest.raises(Exception):
            Triple(
                subject_id=1,
                predicate=RelationType.CALLS,
                object_id=2,
                corroboration_count=0,
            )


class TestRawTriple:
    """Tests for RawTriple model."""

    def test_create(self):
        raw = RawTriple(
            subject_type=EntityType.PROGRAM,
            subject_name="ACCT0100",
            predicate=RelationType.CALLS,
            object_type=EntityType.PROGRAM,
            object_name="ACCT0200",
        )
        assert raw.subject_type == EntityType.PROGRAM
        assert raw.subject_name == "ACCT0100"
        assert raw.predicate == RelationType.CALLS
        assert raw.object_type == EntityType.PROGRAM
        assert raw.object_name == "ACCT0200"
        assert raw.source_pass is None
        assert raw.source_artifact is None

    def test_subject_qualified(self):
        raw = RawTriple(
            subject_type=EntityType.PROGRAM,
            subject_name="ACCT0100",
            predicate=RelationType.CALLS,
            object_type=EntityType.PROGRAM,
            object_name="ACCT0200",
        )
        assert raw.subject_qualified == "PROGRAM:ACCT0100"

    def test_object_qualified(self):
        raw = RawTriple(
            subject_type=EntityType.JCL_STEP,
            subject_name="STEP01",
            predicate=RelationType.EXECUTES,
            object_type=EntityType.PROGRAM,
            object_name="ACCT0100",
        )
        assert raw.object_qualified == "PROGRAM:ACCT0100"

    def test_with_provenance(self):
        raw = RawTriple(
            subject_type=EntityType.PROGRAM,
            subject_name="ACCT0100",
            predicate=RelationType.READS,
            object_type=EntityType.DATASET,
            object_name="INPUT.FILE",
            source_pass="pass_1",
            source_artifact="ACCT0100.cbl",
        )
        assert raw.source_pass == "pass_1"
        assert raw.source_artifact == "ACCT0100.cbl"


class TestConflict:
    """Tests for Conflict model."""

    def test_create_minimal(self):
        conflict = Conflict(triple_a_id=1, triple_b_id=2)
        assert conflict.triple_a_id == 1
        assert conflict.triple_b_id == 2

    def test_defaults(self):
        conflict = Conflict(triple_a_id=1, triple_b_id=2)
        assert conflict.id is None
        assert conflict.resolved is False
        assert conflict.resolution is None

    def test_create_with_all_fields(self):
        conflict = Conflict(
            id=5,
            triple_a_id=10,
            triple_b_id=20,
            resolved=True,
            resolution="Kept triple 10 (READS) based on OPEN INPUT analysis",
        )
        assert conflict.id == 5
        assert conflict.resolved is True
        assert conflict.resolution is not None


class TestNeighborhood:
    """Tests for Neighborhood model."""

    def test_create_minimal(self):
        target = Entity(entity_type=EntityType.PROGRAM, name="ACCT0100")
        neighborhood = Neighborhood(target=target)
        assert neighborhood.target == target
        assert neighborhood.entities == []
        assert neighborhood.triples == []
        assert neighborhood.hop_depth == 2

    def test_defaults(self):
        target = Entity(id=1, entity_type=EntityType.PROGRAM, name="ACCT0100")
        neighborhood = Neighborhood(target=target)
        assert neighborhood.hop_depth == 2
        assert neighborhood.entities == []
        assert neighborhood.triples == []

    def test_create_with_entities_and_triples(self):
        target = Entity(id=1, entity_type=EntityType.PROGRAM, name="ACCT0100")
        called = Entity(id=2, entity_type=EntityType.PROGRAM, name="ACCT0200")
        triple = Triple(
            id=10,
            subject_id=1,
            predicate=RelationType.CALLS,
            object_id=2,
        )
        neighborhood = Neighborhood(
            target=target,
            entities=[target, called],
            triples=[triple],
            hop_depth=1,
        )
        assert len(neighborhood.entities) == 2
        assert len(neighborhood.triples) == 1
        assert neighborhood.hop_depth == 1

    def test_hop_depth_validation_min(self):
        target = Entity(entity_type=EntityType.PROGRAM, name="TEST")
        with pytest.raises(Exception):
            Neighborhood(target=target, hop_depth=0)

    def test_hop_depth_validation_max(self):
        target = Entity(entity_type=EntityType.PROGRAM, name="TEST")
        with pytest.raises(Exception):
            Neighborhood(target=target, hop_depth=6)


class TestTripleDelta:
    """Tests for TripleDelta computed properties."""

    def _make_triple(self, triple_id: int) -> Triple:
        return Triple(
            id=triple_id,
            subject_id=1,
            predicate=RelationType.CALLS,
            object_id=2,
        )

    def test_empty_delta(self):
        delta = TripleDelta(
            pass_from="pass_1",
            pass_to="pass_2",
            added=[],
            removed=[],
            modified=[],
            total_triples=0,
        )
        assert delta.change_count == 0
        assert delta.change_rate == 0.0
        assert delta.has_converged is True

    def test_small_delta_converged(self):
        added = [self._make_triple(i) for i in range(1)]
        delta = TripleDelta(
            pass_from="pass_1",
            pass_to="pass_2",
            added=added,
            removed=[],
            modified=[],
            total_triples=100,
        )
        assert delta.change_count == 1
        assert delta.change_rate == pytest.approx(0.01)
        assert delta.has_converged is True

    def test_large_delta_not_converged(self):
        added = [self._make_triple(i) for i in range(10)]
        delta = TripleDelta(
            pass_from="pass_1",
            pass_to="pass_2",
            added=added,
            removed=[],
            modified=[],
            total_triples=100,
        )
        assert delta.change_count == 10
        assert delta.change_rate == pytest.approx(0.10)
        assert delta.has_converged is False

    def test_boundary_not_converged(self):
        added = [self._make_triple(i) for i in range(5)]
        delta = TripleDelta(
            pass_from="pass_1",
            pass_to="pass_2",
            added=added,
            removed=[],
            modified=[],
            total_triples=100,
        )
        assert delta.change_count == 5
        assert delta.change_rate == pytest.approx(0.05)
        assert delta.has_converged is False  # < 0.05 required, 0.05 is not < 0.05

    def test_change_count_includes_all_categories(self):
        delta = TripleDelta(
            pass_from="pass_1",
            pass_to="pass_2",
            added=[self._make_triple(1), self._make_triple(2)],
            removed=[self._make_triple(3)],
            modified=[self._make_triple(4), self._make_triple(5), self._make_triple(6)],
            total_triples=100,
        )
        assert delta.change_count == 6
        assert delta.change_rate == pytest.approx(0.06)
        assert delta.has_converged is False

    def test_just_below_threshold_converged(self):
        added = [self._make_triple(i) for i in range(4)]
        delta = TripleDelta(
            pass_from="pass_1",
            pass_to="pass_2",
            added=added,
            removed=[],
            modified=[],
            total_triples=100,
        )
        assert delta.change_rate == pytest.approx(0.04)
        assert delta.has_converged is True

    def test_defaults(self):
        delta = TripleDelta(pass_from="pass_1", pass_to="pass_2")
        assert delta.added == []
        assert delta.removed == []
        assert delta.modified == []
        assert delta.total_triples == 0
