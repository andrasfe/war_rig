"""Unit tests for SQLiteGraphStore.

Tests:
- Initialization: schema creation, WAL mode
- Entity operations: upsert, idempotency, property merge, get, get_by_id
- Triple operations: insert, corroboration, get_triples_for_entity, ingest_raw_triples
- Neighborhood queries: 1-hop, 2-hop, unknown entity
- Conflict management: record, get_unresolved, resolve
- Delta computation: added/removed triples, empty passes
- Bulk operations: get_all_entities, get_all_triples, get_triple_count
- Error handling: operations before initialize()
"""

import pytest

from war_rig.knowledge_graph.models import (
    EntityType,
    RawTriple,
    RelationType,
)
from war_rig.knowledge_graph.sqlite_store import SQLiteGraphStore


@pytest.fixture()
async def store(tmp_path):
    """Create and initialize a SQLiteGraphStore for testing."""
    db_path = tmp_path / "test_kg.db"
    s = SQLiteGraphStore(db_path)
    await s.initialize()
    yield s
    await s.close()


class TestInitialization:
    """Tests for store initialization."""

    async def test_schema_created(self, store):
        """Tables and indexes are created after initialize()."""
        cursor = await store._conn.execute(
            "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name"
        )
        rows = await cursor.fetchall()
        table_names = {row["name"] for row in rows}
        assert "entities" in table_names
        assert "triples" in table_names
        assert "conflicts" in table_names

    async def test_wal_mode_enabled(self, store):
        """WAL journal mode is set."""
        cursor = await store._conn.execute("PRAGMA journal_mode")
        row = await cursor.fetchone()
        assert row[0].lower() == "wal"

    async def test_foreign_keys_enabled(self, store):
        """Foreign keys pragma is on."""
        cursor = await store._conn.execute("PRAGMA foreign_keys")
        row = await cursor.fetchone()
        assert row[0] == 1

    async def test_indexes_created(self, store):
        """Expected indexes are present."""
        cursor = await store._conn.execute(
            "SELECT name FROM sqlite_master WHERE type='index'"
        )
        rows = await cursor.fetchall()
        index_names = {row["name"] for row in rows}
        assert "idx_triples_subject" in index_names
        assert "idx_triples_object" in index_names
        assert "idx_triples_predicate" in index_names
        assert "idx_triples_pass" in index_names
        assert "idx_entities_type_name" in index_names


class TestEntityOperations:
    """Tests for entity upsert and retrieval."""

    async def test_upsert_creates_entity(self, store):
        entity = await store.upsert_entity(EntityType.PROGRAM, "ACCT0100")
        assert entity.id is not None
        assert entity.entity_type == EntityType.PROGRAM
        assert entity.name == "ACCT0100"

    async def test_upsert_is_idempotent(self, store):
        entity1 = await store.upsert_entity(EntityType.PROGRAM, "ACCT0100")
        entity2 = await store.upsert_entity(EntityType.PROGRAM, "ACCT0100")
        assert entity1.id == entity2.id

    async def test_upsert_merges_properties(self, store):
        await store.upsert_entity(
            EntityType.PROGRAM, "ACCT0100", properties={"language": "COBOL"}
        )
        entity = await store.upsert_entity(
            EntityType.PROGRAM, "ACCT0100", properties={"loc": 500}
        )
        assert entity.properties["language"] == "COBOL"
        assert entity.properties["loc"] == 500

    async def test_upsert_overwrites_existing_property(self, store):
        await store.upsert_entity(
            EntityType.PROGRAM, "ACCT0100", properties={"loc": 100}
        )
        entity = await store.upsert_entity(
            EntityType.PROGRAM, "ACCT0100", properties={"loc": 500}
        )
        assert entity.properties["loc"] == 500

    async def test_get_entity_found(self, store):
        await store.upsert_entity(EntityType.DATASET, "ACCT.MASTER.FILE")
        entity = await store.get_entity(EntityType.DATASET, "ACCT.MASTER.FILE")
        assert entity is not None
        assert entity.name == "ACCT.MASTER.FILE"
        assert entity.entity_type == EntityType.DATASET

    async def test_get_entity_not_found(self, store):
        entity = await store.get_entity(EntityType.PROGRAM, "NONEXISTENT")
        assert entity is None

    async def test_get_entity_by_id(self, store):
        created = await store.upsert_entity(EntityType.COPYBOOK, "ACCTCPY1")
        assert created.id is not None
        fetched = await store.get_entity_by_id(created.id)
        assert fetched is not None
        assert fetched.name == "ACCTCPY1"
        assert fetched.entity_type == EntityType.COPYBOOK

    async def test_get_entity_by_id_not_found(self, store):
        entity = await store.get_entity_by_id(99999)
        assert entity is None

    async def test_different_types_same_name(self, store):
        prog = await store.upsert_entity(EntityType.PROGRAM, "SHARED_NAME")
        ds = await store.upsert_entity(EntityType.DATASET, "SHARED_NAME")
        assert prog.id != ds.id


class TestTripleOperations:
    """Tests for triple insert, corroboration, and retrieval."""

    async def test_insert_triple_creates(self, store):
        subj = await store.upsert_entity(EntityType.PROGRAM, "ACCT0100")
        obj = await store.upsert_entity(EntityType.PROGRAM, "ACCT0200")
        triple = await store.insert_triple(
            subj.id, RelationType.CALLS, obj.id,
            source_pass="pass_1", source_artifact="ACCT0100.cbl",
        )
        assert triple.id is not None
        assert triple.subject_id == subj.id
        assert triple.predicate == RelationType.CALLS
        assert triple.object_id == obj.id
        assert triple.confirmed is False
        assert triple.corroboration_count == 1

    async def test_insert_triple_corroborates_duplicate(self, store):
        subj = await store.upsert_entity(EntityType.PROGRAM, "ACCT0100")
        obj = await store.upsert_entity(EntityType.PROGRAM, "ACCT0200")
        t1 = await store.insert_triple(subj.id, RelationType.CALLS, obj.id)
        t2 = await store.insert_triple(subj.id, RelationType.CALLS, obj.id)
        assert t2.id == t1.id
        assert t2.corroboration_count == 2
        assert t2.confirmed is True

    async def test_get_triples_for_entity_as_subject(self, store):
        subj = await store.upsert_entity(EntityType.PROGRAM, "ACCT0100")
        obj = await store.upsert_entity(EntityType.PROGRAM, "ACCT0200")
        await store.insert_triple(subj.id, RelationType.CALLS, obj.id)

        triples = await store.get_triples_for_entity(
            subj.id, as_subject=True, as_object=False,
        )
        assert len(triples) == 1
        assert triples[0].subject_id == subj.id

    async def test_get_triples_for_entity_as_object(self, store):
        subj = await store.upsert_entity(EntityType.PROGRAM, "ACCT0100")
        obj = await store.upsert_entity(EntityType.PROGRAM, "ACCT0200")
        await store.insert_triple(subj.id, RelationType.CALLS, obj.id)

        triples = await store.get_triples_for_entity(
            obj.id, as_subject=False, as_object=True,
        )
        assert len(triples) == 1
        assert triples[0].object_id == obj.id

    async def test_get_triples_for_entity_both(self, store):
        a = await store.upsert_entity(EntityType.PROGRAM, "A")
        b = await store.upsert_entity(EntityType.PROGRAM, "B")
        c = await store.upsert_entity(EntityType.PROGRAM, "C")
        await store.insert_triple(a.id, RelationType.CALLS, b.id)
        await store.insert_triple(c.id, RelationType.CALLS, a.id)

        triples = await store.get_triples_for_entity(
            a.id, as_subject=True, as_object=True,
        )
        assert len(triples) == 2

    async def test_get_triples_for_entity_neither(self, store):
        subj = await store.upsert_entity(EntityType.PROGRAM, "A")
        obj = await store.upsert_entity(EntityType.PROGRAM, "B")
        await store.insert_triple(subj.id, RelationType.CALLS, obj.id)

        triples = await store.get_triples_for_entity(
            subj.id, as_subject=False, as_object=False,
        )
        assert triples == []

    async def test_ingest_raw_triples(self, store):
        raw = [
            RawTriple(
                subject_type=EntityType.PROGRAM,
                subject_name="ACCT0100",
                predicate=RelationType.CALLS,
                object_type=EntityType.PROGRAM,
                object_name="ACCT0200",
                source_pass="pass_1",
                source_artifact="ACCT0100.cbl",
            ),
            RawTriple(
                subject_type=EntityType.PROGRAM,
                subject_name="ACCT0100",
                predicate=RelationType.READS,
                object_type=EntityType.DATASET,
                object_name="INPUT.FILE",
                source_pass="pass_1",
            ),
        ]
        triples = await store.ingest_raw_triples(raw)
        assert len(triples) == 2

        # Entities should have been created
        prog = await store.get_entity(EntityType.PROGRAM, "ACCT0100")
        assert prog is not None
        ds = await store.get_entity(EntityType.DATASET, "INPUT.FILE")
        assert ds is not None

    async def test_ingest_raw_triples_empty(self, store):
        triples = await store.ingest_raw_triples([])
        assert triples == []


class TestNeighborhoodQueries:
    """Tests for N-hop neighborhood queries."""

    async def test_unknown_entity_returns_none(self, store):
        result = await store.get_neighborhood("NONEXISTENT", EntityType.PROGRAM)
        assert result is None

    async def test_1_hop_neighborhood(self, store):
        a = await store.upsert_entity(EntityType.PROGRAM, "A")
        b = await store.upsert_entity(EntityType.PROGRAM, "B")
        c = await store.upsert_entity(EntityType.PROGRAM, "C")
        await store.insert_triple(a.id, RelationType.CALLS, b.id)
        # C is connected to B, not directly to A
        await store.insert_triple(b.id, RelationType.CALLS, c.id)

        neighborhood = await store.get_neighborhood("A", EntityType.PROGRAM, hops=1)
        assert neighborhood is not None
        assert neighborhood.target.name == "A"
        entity_names = {e.name for e in neighborhood.entities}
        # 1-hop from A should include A and B (direct connection)
        assert "A" in entity_names
        assert "B" in entity_names

    async def test_2_hop_includes_indirect(self, store):
        a = await store.upsert_entity(EntityType.PROGRAM, "A")
        b = await store.upsert_entity(EntityType.PROGRAM, "B")
        c = await store.upsert_entity(EntityType.PROGRAM, "C")
        await store.insert_triple(a.id, RelationType.CALLS, b.id)
        await store.insert_triple(b.id, RelationType.CALLS, c.id)

        neighborhood = await store.get_neighborhood("A", EntityType.PROGRAM, hops=2)
        assert neighborhood is not None
        entity_names = {e.name for e in neighborhood.entities}
        assert "A" in entity_names
        assert "B" in entity_names
        assert "C" in entity_names
        assert len(neighborhood.triples) == 2
        assert neighborhood.hop_depth == 2

    async def test_isolated_entity_neighborhood(self, store):
        await store.upsert_entity(EntityType.PROGRAM, "ISOLATED")
        neighborhood = await store.get_neighborhood(
            "ISOLATED", EntityType.PROGRAM, hops=2
        )
        assert neighborhood is not None
        assert neighborhood.target.name == "ISOLATED"
        assert len(neighborhood.entities) == 1
        assert len(neighborhood.triples) == 0


class TestConflictManagement:
    """Tests for conflict recording, retrieval, and resolution."""

    async def test_record_conflict(self, store):
        subj = await store.upsert_entity(EntityType.PROGRAM, "ACCT0100")
        obj = await store.upsert_entity(EntityType.DATASET, "FILE1")
        t1 = await store.insert_triple(subj.id, RelationType.READS, obj.id)
        t2 = await store.insert_triple(subj.id, RelationType.WRITES, obj.id)

        conflict = await store.record_conflict(t1.id, t2.id)
        assert conflict.id is not None
        assert conflict.triple_a_id == t1.id
        assert conflict.triple_b_id == t2.id
        assert conflict.resolved is False
        assert conflict.resolution is None

    async def test_get_unresolved_conflicts(self, store):
        subj = await store.upsert_entity(EntityType.PROGRAM, "ACCT0100")
        obj = await store.upsert_entity(EntityType.DATASET, "FILE1")
        t1 = await store.insert_triple(subj.id, RelationType.READS, obj.id)
        t2 = await store.insert_triple(subj.id, RelationType.WRITES, obj.id)

        await store.record_conflict(t1.id, t2.id)
        conflicts = await store.get_unresolved_conflicts()
        assert len(conflicts) == 1
        assert conflicts[0].resolved is False

    async def test_resolve_conflict(self, store):
        subj = await store.upsert_entity(EntityType.PROGRAM, "ACCT0100")
        obj = await store.upsert_entity(EntityType.DATASET, "FILE1")
        t1 = await store.insert_triple(subj.id, RelationType.READS, obj.id)
        t2 = await store.insert_triple(subj.id, RelationType.WRITES, obj.id)

        conflict = await store.record_conflict(t1.id, t2.id)
        await store.resolve_conflict(conflict.id, "Kept READS based on OPEN INPUT")

        unresolved = await store.get_unresolved_conflicts()
        assert len(unresolved) == 0

    async def test_get_unresolved_excludes_resolved(self, store):
        subj = await store.upsert_entity(EntityType.PROGRAM, "ACCT0100")
        obj1 = await store.upsert_entity(EntityType.DATASET, "FILE1")
        obj2 = await store.upsert_entity(EntityType.DATASET, "FILE2")
        t1 = await store.insert_triple(subj.id, RelationType.READS, obj1.id)
        t2 = await store.insert_triple(subj.id, RelationType.WRITES, obj1.id)
        t3 = await store.insert_triple(subj.id, RelationType.READS, obj2.id)
        t4 = await store.insert_triple(subj.id, RelationType.WRITES, obj2.id)

        c1 = await store.record_conflict(t1.id, t2.id)
        await store.record_conflict(t3.id, t4.id)

        # Resolve only the first conflict
        await store.resolve_conflict(c1.id, "Resolved")

        unresolved = await store.get_unresolved_conflicts()
        assert len(unresolved) == 1


class TestDeltaComputation:
    """Tests for compute_delta between passes."""

    async def test_empty_passes_return_empty_delta(self, store):
        delta = await store.compute_delta("pass_1", "pass_2")
        assert delta.pass_from == "pass_1"
        assert delta.pass_to == "pass_2"
        assert delta.added == []
        assert delta.removed == []
        assert delta.modified == []

    async def test_delta_with_added_triples(self, store):
        subj = await store.upsert_entity(EntityType.PROGRAM, "ACCT0100")
        obj = await store.upsert_entity(EntityType.PROGRAM, "ACCT0200")

        # pass_1 has one triple
        await store.insert_triple(
            subj.id, RelationType.CALLS, obj.id,
            source_pass="pass_1",
        )

        # pass_2 has a different triple
        ds = await store.upsert_entity(EntityType.DATASET, "FILE1")
        await store.insert_triple(
            subj.id, RelationType.READS, ds.id,
            source_pass="pass_2",
        )

        delta = await store.compute_delta("pass_1", "pass_2")
        assert len(delta.added) == 1
        assert delta.added[0].predicate == RelationType.READS

    async def test_delta_with_removed_triples(self, store):
        subj = await store.upsert_entity(EntityType.PROGRAM, "ACCT0100")
        obj = await store.upsert_entity(EntityType.PROGRAM, "ACCT0200")

        # pass_1 has a triple that pass_2 does not
        await store.insert_triple(
            subj.id, RelationType.CALLS, obj.id,
            source_pass="pass_1",
        )

        delta = await store.compute_delta("pass_1", "pass_2")
        assert len(delta.removed) == 1
        assert delta.removed[0].predicate == RelationType.CALLS

    async def test_delta_total_triples_is_graph_total(self, store):
        subj = await store.upsert_entity(EntityType.PROGRAM, "ACCT0100")
        obj = await store.upsert_entity(EntityType.PROGRAM, "ACCT0200")
        ds = await store.upsert_entity(EntityType.DATASET, "FILE1")

        await store.insert_triple(
            subj.id, RelationType.CALLS, obj.id, source_pass="pass_1",
        )
        await store.insert_triple(
            subj.id, RelationType.READS, ds.id, source_pass="pass_2",
        )

        delta = await store.compute_delta("pass_1", "pass_2")
        # total_triples reflects the entire graph, not just the pass
        assert delta.total_triples == 2


class TestBulkOperations:
    """Tests for get_all_entities, get_all_triples, get_triple_count."""

    async def test_get_all_entities_empty(self, store):
        entities = await store.get_all_entities()
        assert entities == []

    async def test_get_all_entities(self, store):
        await store.upsert_entity(EntityType.PROGRAM, "A")
        await store.upsert_entity(EntityType.PROGRAM, "B")
        await store.upsert_entity(EntityType.DATASET, "DS1")
        entities = await store.get_all_entities()
        assert len(entities) == 3

    async def test_get_all_triples_empty(self, store):
        triples = await store.get_all_triples()
        assert triples == []

    async def test_get_all_triples(self, store):
        a = await store.upsert_entity(EntityType.PROGRAM, "A")
        b = await store.upsert_entity(EntityType.PROGRAM, "B")
        c = await store.upsert_entity(EntityType.DATASET, "DS1")
        await store.insert_triple(a.id, RelationType.CALLS, b.id)
        await store.insert_triple(a.id, RelationType.READS, c.id)
        triples = await store.get_all_triples()
        assert len(triples) == 2

    async def test_get_triple_count_empty(self, store):
        count = await store.get_triple_count()
        assert count == 0

    async def test_get_triple_count(self, store):
        a = await store.upsert_entity(EntityType.PROGRAM, "A")
        b = await store.upsert_entity(EntityType.PROGRAM, "B")
        await store.insert_triple(a.id, RelationType.CALLS, b.id)
        count = await store.get_triple_count()
        assert count == 1


class TestErrorHandling:
    """Tests for error conditions."""

    async def test_operations_before_initialize_raises(self, tmp_path):
        db_path = tmp_path / "uninitialized.db"
        store = SQLiteGraphStore(db_path)
        with pytest.raises(RuntimeError, match="not initialized"):
            await store.upsert_entity(EntityType.PROGRAM, "ACCT0100")

    async def test_get_entity_before_initialize_raises(self, tmp_path):
        db_path = tmp_path / "uninitialized.db"
        store = SQLiteGraphStore(db_path)
        with pytest.raises(RuntimeError, match="not initialized"):
            await store.get_entity(EntityType.PROGRAM, "ACCT0100")

    async def test_close_without_initialize_is_safe(self, tmp_path):
        db_path = tmp_path / "uninitialized.db"
        store = SQLiteGraphStore(db_path)
        # Should not raise
        await store.close()
