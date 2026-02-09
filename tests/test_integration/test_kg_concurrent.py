"""Integration tests for concurrent knowledge graph access.

Tests that the SQLite store handles concurrent writers correctly,
simulating parallel Scribe workers.
"""

import asyncio

import pytest

from war_rig.knowledge_graph.models import EntityType, RawTriple, RelationType
from war_rig.knowledge_graph.sqlite_store import SQLiteGraphStore


@pytest.fixture()
async def store(tmp_path):
    """Create and initialize a SQLiteGraphStore for testing."""
    db_path = tmp_path / "test_concurrent.db"
    s = SQLiteGraphStore(db_path)
    await s.initialize()
    yield s
    await s.close()


class TestConcurrentEntityUpserts:
    """Concurrent entity upsert operations."""

    async def test_concurrent_entity_upserts(self, store):
        """Launch 10 asyncio tasks each upserting 10 different entities,
        verify all 100 entities are stored."""

        async def upsert_batch(task_id: int) -> None:
            for i in range(10):
                name = f"ENTITY_T{task_id:02d}_E{i:02d}"
                await store.upsert_entity(EntityType.PROGRAM, name)

        tasks = [upsert_batch(tid) for tid in range(10)]
        await asyncio.gather(*tasks)

        entities = await store.get_all_entities()
        assert len(entities) == 100

        # Verify all expected names are present
        entity_names = {e.name for e in entities}
        for tid in range(10):
            for i in range(10):
                expected_name = f"ENTITY_T{tid:02d}_E{i:02d}"
                assert expected_name in entity_names


class TestConcurrentTripleWrites:
    """Concurrent triple write operations."""

    async def test_concurrent_triple_writes(self, store):
        """Launch 5 asyncio tasks each writing 5 triples,
        verify all 25 triples are stored."""

        async def write_batch(task_id: int) -> None:
            for i in range(5):
                raw = RawTriple(
                    subject_type=EntityType.PROGRAM,
                    subject_name=f"CALLER_T{task_id}",
                    predicate=RelationType.CALLS,
                    object_type=EntityType.PROGRAM,
                    object_name=f"CALLEE_T{task_id}_I{i}",
                    source_pass="concurrent_test",
                    source_artifact=f"test_{task_id}.cbl",
                )
                await store.ingest_raw_triples([raw])

        tasks = [write_batch(tid) for tid in range(5)]
        await asyncio.gather(*tasks)

        triple_count = await store.get_triple_count()
        assert triple_count == 25

        # Verify all triples are present
        all_triples = await store.get_all_triples()
        assert len(all_triples) == 25


class TestConcurrentReadWrite:
    """Concurrent read and write operations."""

    async def test_concurrent_read_write(self, store):
        """One task writes triples while another reads neighborhoods,
        no errors should occur."""
        # Pre-seed with a base entity so reads have something to query
        await store.upsert_entity(EntityType.PROGRAM, "BASE_PROGRAM")

        write_errors: list[Exception] = []
        read_errors: list[Exception] = []

        async def writer() -> None:
            try:
                for i in range(20):
                    raw = RawTriple(
                        subject_type=EntityType.PROGRAM,
                        subject_name="BASE_PROGRAM",
                        predicate=RelationType.CALLS,
                        object_type=EntityType.PROGRAM,
                        object_name=f"TARGET_{i:03d}",
                        source_pass="concurrent_rw",
                        source_artifact="base.cbl",
                    )
                    await store.ingest_raw_triples([raw])
                    # Small yield to allow reader to interleave
                    await asyncio.sleep(0)
            except Exception as exc:
                write_errors.append(exc)

        async def reader() -> None:
            try:
                for _ in range(20):
                    # Query neighborhood -- may get partial results, but should not error
                    neighborhood = await store.get_neighborhood(
                        "BASE_PROGRAM", EntityType.PROGRAM, hops=1
                    )
                    # Neighborhood can be None if entity not fully committed yet
                    if neighborhood is not None:
                        assert neighborhood.target.name == "BASE_PROGRAM"
                    await asyncio.sleep(0)
            except Exception as exc:
                read_errors.append(exc)

        await asyncio.gather(writer(), reader())

        assert write_errors == [], f"Writer errors: {write_errors}"
        assert read_errors == [], f"Reader errors: {read_errors}"

        # After all writes complete, verify data integrity
        triple_count = await store.get_triple_count()
        assert triple_count == 20

    async def test_concurrent_multi_reader_single_writer(self, store):
        """Multiple readers and a single writer operating concurrently."""
        await store.upsert_entity(EntityType.PROGRAM, "SHARED_PROG")

        errors: list[Exception] = []

        async def writer() -> None:
            try:
                for i in range(15):
                    raw = RawTriple(
                        subject_type=EntityType.PROGRAM,
                        subject_name="SHARED_PROG",
                        predicate=RelationType.INCLUDES,
                        object_type=EntityType.COPYBOOK,
                        object_name=f"CPY_{i:03d}",
                        source_pass="multi_rw",
                        source_artifact="shared.cbl",
                    )
                    await store.ingest_raw_triples([raw])
                    await asyncio.sleep(0)
            except Exception as exc:
                errors.append(exc)

        async def reader(reader_id: int) -> None:
            try:
                for _ in range(10):
                    entities = await store.get_all_entities()
                    assert isinstance(entities, list)
                    count = await store.get_triple_count()
                    assert count >= 0
                    await asyncio.sleep(0)
            except Exception as exc:
                errors.append(exc)

        # 1 writer + 3 readers
        tasks = [writer()] + [reader(rid) for rid in range(3)]
        await asyncio.gather(*tasks)

        assert errors == [], f"Concurrent errors: {errors}"

        # Verify final state
        triple_count = await store.get_triple_count()
        assert triple_count == 15
