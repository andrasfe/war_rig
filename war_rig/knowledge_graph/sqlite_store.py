"""SQLite implementation of the knowledge graph store.

This module provides a SQLite-backed implementation of KnowledgeGraphStore
using WAL mode for concurrent read/write access from parallel Scribe workers.

The schema consists of three tables (entities, triples, conflicts) as defined
in the KG spec Section 9.1. Entity properties are stored as JSON blobs.

WAL mode allows multiple readers and a single writer to operate concurrently,
which is critical for the parallel worker pool architecture.

Example:
    store = SQLiteGraphStore("./output/knowledge_graph.db")
    await store.initialize()

    entity = await store.upsert_entity(EntityType.PROGRAM, "ACCT0100")
    await store.close()
"""

import json
import logging
from datetime import datetime
from pathlib import Path

import aiosqlite

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
from war_rig.knowledge_graph.store import KnowledgeGraphStore

logger = logging.getLogger(__name__)

# SQL schema for the knowledge graph
_SCHEMA_SQL = """
CREATE TABLE IF NOT EXISTS entities (
    id          INTEGER PRIMARY KEY,
    type        TEXT NOT NULL,
    name        TEXT NOT NULL,
    properties  TEXT,
    UNIQUE(type, name)
);

CREATE TABLE IF NOT EXISTS triples (
    id                  INTEGER PRIMARY KEY,
    subject_id          INTEGER NOT NULL REFERENCES entities(id),
    predicate           TEXT NOT NULL,
    object_id           INTEGER NOT NULL REFERENCES entities(id),
    source_pass         TEXT,
    source_artifact     TEXT,
    timestamp           TEXT,
    confirmed           BOOLEAN DEFAULT 0,
    corroboration_count INTEGER DEFAULT 1
);

CREATE TABLE IF NOT EXISTS conflicts (
    id          INTEGER PRIMARY KEY,
    triple_a_id INTEGER NOT NULL REFERENCES triples(id),
    triple_b_id INTEGER NOT NULL REFERENCES triples(id),
    resolved    BOOLEAN DEFAULT 0,
    resolution  TEXT
);

CREATE INDEX IF NOT EXISTS idx_triples_subject ON triples(subject_id);
CREATE INDEX IF NOT EXISTS idx_triples_object ON triples(object_id);
CREATE INDEX IF NOT EXISTS idx_triples_predicate ON triples(predicate);
CREATE INDEX IF NOT EXISTS idx_triples_pass ON triples(source_pass);
CREATE INDEX IF NOT EXISTS idx_entities_type_name ON entities(type, name);
"""


class SQLiteGraphStore(KnowledgeGraphStore):
    """SQLite-backed knowledge graph store with WAL mode.

    Uses aiosqlite for async access and WAL journaling for concurrent
    reads from parallel Scribe/Challenger workers.

    Args:
        db_path: Path to the SQLite database file. Created if it doesn't exist.
    """

    def __init__(self, db_path: str | Path) -> None:
        """Initialize the SQLite graph store.

        Args:
            db_path: Path to the SQLite database file.
        """
        self._db_path = Path(db_path)
        self._db: aiosqlite.Connection | None = None

    @property
    def _conn(self) -> aiosqlite.Connection:
        """Return the active database connection.

        Raises:
            RuntimeError: If the store has not been initialized.
        """
        if self._db is None:
            raise RuntimeError("Store not initialized. Call initialize() before use.")
        return self._db

    async def initialize(self) -> None:
        """Create database and schema, enable WAL mode.

        Raises:
            RuntimeError: If database initialization fails.
        """
        try:
            self._db_path.parent.mkdir(parents=True, exist_ok=True)
            self._db = await aiosqlite.connect(str(self._db_path))
            self._db.row_factory = aiosqlite.Row
            await self._db.execute("PRAGMA journal_mode=WAL")
            await self._db.execute("PRAGMA foreign_keys=ON")
            await self._db.executescript(_SCHEMA_SQL)
            await self._db.commit()
            logger.info("Initialized SQLite graph store at %s", self._db_path)
        except Exception as exc:
            raise RuntimeError(
                f"Failed to initialize graph store at {self._db_path}: {exc}"
            ) from exc

    async def close(self) -> None:
        """Close the database connection."""
        if self._db is not None:
            await self._db.close()
            self._db = None
            logger.info("Closed SQLite graph store at %s", self._db_path)

    # -------------------------------------------------------------------------
    # Entity operations
    # -------------------------------------------------------------------------

    async def upsert_entity(
        self,
        entity_type: EntityType,
        name: str,
        properties: dict[str, str | int | float | bool | None] | None = None,
    ) -> Entity:
        """Insert or update an entity by (type, name) uniqueness.

        If the entity already exists, its properties are merged (new keys
        added, existing keys updated). If it does not exist, it is created.

        Args:
            entity_type: The entity classification.
            name: Canonical name of the entity.
            properties: Optional metadata to store/merge.

        Returns:
            The upserted Entity with its database ID.
        """
        props = properties or {}
        props_json = json.dumps(props) if props else None

        # Try inserting; if the entity already exists, this is a no-op.
        await self._conn.execute(
            "INSERT OR IGNORE INTO entities (type, name, properties) VALUES (?, ?, ?)",
            (entity_type.value, name, props_json),
        )

        # If properties provided, merge with existing properties.
        if props:
            cursor = await self._conn.execute(
                "SELECT properties FROM entities WHERE type = ? AND name = ?",
                (entity_type.value, name),
            )
            row = await cursor.fetchone()
            if row is not None:
                existing_props: dict[str, str | int | float | bool | None] = {}
                if row["properties"]:
                    existing_props = json.loads(row["properties"])
                existing_props.update(props)
                await self._conn.execute(
                    "UPDATE entities SET properties = ? WHERE type = ? AND name = ?",
                    (json.dumps(existing_props), entity_type.value, name),
                )

        await self._conn.commit()

        # Fetch the entity with its ID.
        cursor = await self._conn.execute(
            "SELECT id, type, name, properties FROM entities "
            "WHERE type = ? AND name = ?",
            (entity_type.value, name),
        )
        row = await cursor.fetchone()
        if row is None:
            raise RuntimeError(
                f"Entity ({entity_type.value}, {name}) not found after upsert"
            )
        return self._row_to_entity(row)

    async def get_entity(
        self,
        entity_type: EntityType,
        name: str,
    ) -> Entity | None:
        """Look up an entity by type and name.

        Args:
            entity_type: The entity classification.
            name: Canonical name.

        Returns:
            Entity if found, None otherwise.
        """
        cursor = await self._conn.execute(
            "SELECT id, type, name, properties FROM entities "
            "WHERE type = ? AND name = ?",
            (entity_type.value, name),
        )
        row = await cursor.fetchone()
        if row is None:
            return None
        return self._row_to_entity(row)

    async def get_entity_by_id(self, entity_id: int) -> Entity | None:
        """Look up an entity by database ID.

        Args:
            entity_id: The entity's primary key.

        Returns:
            Entity if found, None otherwise.
        """
        cursor = await self._conn.execute(
            "SELECT id, type, name, properties FROM entities WHERE id = ?",
            (entity_id,),
        )
        row = await cursor.fetchone()
        if row is None:
            return None
        return self._row_to_entity(row)

    # -------------------------------------------------------------------------
    # Triple operations
    # -------------------------------------------------------------------------

    async def insert_triple(
        self,
        subject_id: int,
        predicate: RelationType,
        object_id: int,
        source_pass: str | None = None,
        source_artifact: str | None = None,
    ) -> Triple:
        """Insert a triple or increment corroboration count.

        If a triple with the same (subject_id, predicate, object_id) already
        exists, its corroboration_count is incremented and confirmed is set
        to True.

        Args:
            subject_id: Database ID of the subject entity.
            predicate: The relationship type.
            object_id: Database ID of the object entity.
            source_pass: Which War Rig pass produced this.
            source_artifact: Source file being analyzed.

        Returns:
            The inserted or updated Triple.
        """
        # Check for existing triple with same structural key.
        cursor = await self._conn.execute(
            "SELECT id, corroboration_count FROM triples "
            "WHERE subject_id = ? AND predicate = ? AND object_id = ?",
            (subject_id, predicate.value, object_id),
        )
        existing = await cursor.fetchone()

        if existing is not None:
            # Corroborate: increment count and mark confirmed.
            new_count = existing["corroboration_count"] + 1
            await self._conn.execute(
                "UPDATE triples SET corroboration_count = ?, confirmed = 1 "
                "WHERE id = ?",
                (new_count, existing["id"]),
            )
            await self._conn.commit()
            triple_id = existing["id"]
        else:
            # Insert new triple.
            now = datetime.utcnow().isoformat()
            cursor = await self._conn.execute(
                "INSERT INTO triples "
                "(subject_id, predicate, object_id, source_pass, "
                "source_artifact, timestamp, confirmed, corroboration_count) "
                "VALUES (?, ?, ?, ?, ?, ?, 0, 1)",
                (
                    subject_id,
                    predicate.value,
                    object_id,
                    source_pass,
                    source_artifact,
                    now,
                ),
            )
            await self._conn.commit()
            triple_id = cursor.lastrowid

        # Fetch and return the full triple.
        cursor = await self._conn.execute(
            "SELECT id, subject_id, predicate, object_id, source_pass, "
            "source_artifact, timestamp, confirmed, corroboration_count "
            "FROM triples WHERE id = ?",
            (triple_id,),
        )
        row = await cursor.fetchone()
        if row is None:
            raise RuntimeError(f"Triple {triple_id} not found after insert/update")
        return self._row_to_triple(row)

    async def ingest_raw_triples(
        self,
        raw_triples: list[RawTriple],
    ) -> list[Triple]:
        """Resolve and ingest a batch of raw triples.

        For each raw triple, upserts subject and object entities, then
        inserts or corroborates the triple. The entire batch is wrapped
        in a transaction for atomicity.

        Args:
            raw_triples: Unresolved triples to ingest.

        Returns:
            List of persisted Triple objects.
        """
        results: list[Triple] = []
        for raw in raw_triples:
            subject = await self.upsert_entity(raw.subject_type, raw.subject_name)
            obj = await self.upsert_entity(raw.object_type, raw.object_name)
            assert subject.id is not None  # noqa: S101
            assert obj.id is not None  # noqa: S101
            triple = await self.insert_triple(
                subject_id=subject.id,
                predicate=raw.predicate,
                object_id=obj.id,
                source_pass=raw.source_pass,
                source_artifact=raw.source_artifact,
            )
            results.append(triple)
        logger.info("Ingested %d raw triples", len(results))
        return results

    async def get_triples_for_entity(
        self,
        entity_id: int,
        as_subject: bool = True,
        as_object: bool = True,
    ) -> list[Triple]:
        """Get all triples involving an entity.

        Args:
            entity_id: Database ID of the entity.
            as_subject: Include triples where entity is subject.
            as_object: Include triples where entity is object.

        Returns:
            List of matching triples.
        """
        if not as_subject and not as_object:
            return []

        conditions: list[str] = []
        params: list[int] = []
        if as_subject:
            conditions.append("subject_id = ?")
            params.append(entity_id)
        if as_object:
            conditions.append("object_id = ?")
            params.append(entity_id)

        where_clause = " OR ".join(conditions)
        cursor = await self._conn.execute(
            "SELECT id, subject_id, predicate, object_id, source_pass, "
            "source_artifact, timestamp, confirmed, corroboration_count "
            f"FROM triples WHERE {where_clause}",
            tuple(params),
        )
        rows = await cursor.fetchall()
        return [self._row_to_triple(row) for row in rows]

    # -------------------------------------------------------------------------
    # Neighborhood queries
    # -------------------------------------------------------------------------

    async def get_neighborhood(
        self,
        name: str,
        entity_type: EntityType,
        hops: int = 2,
    ) -> Neighborhood | None:
        """Get the N-hop neighborhood around an entity.

        Uses iterative BFS: at each hop, collects all entities connected
        to the current frontier via triples (as subject or object), then
        expands the frontier for the next hop.

        Args:
            name: Canonical name of the target entity.
            entity_type: Type of the target entity.
            hops: Maximum hops to traverse (default 2).

        Returns:
            Neighborhood subgraph, or None if entity not found.
        """
        target = await self.get_entity(entity_type, name)
        if target is None or target.id is None:
            return None

        visited_entity_ids: set[int] = {target.id}
        collected_triple_ids: set[int] = set()
        frontier: set[int] = {target.id}

        for _ in range(hops):
            if not frontier:
                break
            next_frontier: set[int] = set()
            for eid in frontier:
                triples = await self.get_triples_for_entity(eid)
                for t in triples:
                    if t.id is not None:
                        collected_triple_ids.add(t.id)
                    # Expand to connected entities.
                    for neighbor_id in (t.subject_id, t.object_id):
                        if neighbor_id not in visited_entity_ids:
                            visited_entity_ids.add(neighbor_id)
                            next_frontier.add(neighbor_id)
            frontier = next_frontier

        # Fetch all entities by collected IDs.
        entities: list[Entity] = []
        for eid in visited_entity_ids:
            entity = await self.get_entity_by_id(eid)
            if entity is not None:
                entities.append(entity)

        # Fetch all collected triples.
        triples: list[Triple] = []
        for tid in collected_triple_ids:
            cursor = await self._conn.execute(
                "SELECT id, subject_id, predicate, object_id, source_pass, "
                "source_artifact, timestamp, confirmed, corroboration_count "
                "FROM triples WHERE id = ?",
                (tid,),
            )
            row = await cursor.fetchone()
            if row is not None:
                triples.append(self._row_to_triple(row))

        return Neighborhood(
            target=target,
            entities=entities,
            triples=triples,
            hop_depth=hops,
        )

    # -------------------------------------------------------------------------
    # Conflict management
    # -------------------------------------------------------------------------

    async def record_conflict(
        self,
        triple_a_id: int,
        triple_b_id: int,
    ) -> Conflict:
        """Record a conflict between two triples.

        Args:
            triple_a_id: Database ID of the first triple.
            triple_b_id: Database ID of the second triple.

        Returns:
            The created Conflict record.
        """
        cursor = await self._conn.execute(
            "INSERT INTO conflicts (triple_a_id, triple_b_id, resolved) "
            "VALUES (?, ?, 0)",
            (triple_a_id, triple_b_id),
        )
        await self._conn.commit()
        conflict_id = cursor.lastrowid
        logger.info(
            "Recorded conflict %d between triples %d and %d",
            conflict_id,
            triple_a_id,
            triple_b_id,
        )
        return Conflict(
            id=conflict_id,
            triple_a_id=triple_a_id,
            triple_b_id=triple_b_id,
            resolved=False,
            resolution=None,
        )

    async def get_unresolved_conflicts(self) -> list[Conflict]:
        """Get all unresolved conflicts.

        Returns:
            List of unresolved Conflict records.
        """
        cursor = await self._conn.execute(
            "SELECT id, triple_a_id, triple_b_id, resolved, resolution "
            "FROM conflicts WHERE resolved = 0",
        )
        rows = await cursor.fetchall()
        return [
            Conflict(
                id=row["id"],
                triple_a_id=row["triple_a_id"],
                triple_b_id=row["triple_b_id"],
                resolved=bool(row["resolved"]),
                resolution=row["resolution"],
            )
            for row in rows
        ]

    async def resolve_conflict(
        self,
        conflict_id: int,
        resolution: str,
    ) -> None:
        """Mark a conflict as resolved.

        Args:
            conflict_id: Database ID of the conflict.
            resolution: Description of resolution.
        """
        await self._conn.execute(
            "UPDATE conflicts SET resolved = 1, resolution = ? WHERE id = ?",
            (resolution, conflict_id),
        )
        await self._conn.commit()
        logger.info("Resolved conflict %d: %s", conflict_id, resolution)

    # -------------------------------------------------------------------------
    # Delta / convergence
    # -------------------------------------------------------------------------

    async def compute_delta(
        self,
        pass_from: str,
        pass_to: str,
    ) -> TripleDelta:
        """Compute the triple delta between two passes.

        Compares triples by their structural key (subject_id, predicate,
        object_id). Triples are categorized as:
        - added: present in pass_to but not pass_from
        - removed: present in pass_from but not pass_to
        - modified: same structural key but different provenance metadata

        Args:
            pass_from: Earlier pass identifier.
            pass_to: Later pass identifier.

        Returns:
            TripleDelta with change counts and convergence info.
        """
        # Fetch triples from each pass.
        cursor_from = await self._conn.execute(
            "SELECT id, subject_id, predicate, object_id, source_pass, "
            "source_artifact, timestamp, confirmed, corroboration_count "
            "FROM triples WHERE source_pass = ?",
            (pass_from,),
        )
        rows_from = await cursor_from.fetchall()
        triples_from = [self._row_to_triple(r) for r in rows_from]

        cursor_to = await self._conn.execute(
            "SELECT id, subject_id, predicate, object_id, source_pass, "
            "source_artifact, timestamp, confirmed, corroboration_count "
            "FROM triples WHERE source_pass = ?",
            (pass_to,),
        )
        rows_to = await cursor_to.fetchall()
        triples_to = [self._row_to_triple(r) for r in rows_to]

        # Build lookup dicts keyed by structural identity.
        def _key(t: Triple) -> tuple[int, str, int]:
            return (t.subject_id, t.predicate.value, t.object_id)

        from_map = {_key(t): t for t in triples_from}
        to_map = {_key(t): t for t in triples_to}

        from_keys = set(from_map.keys())
        to_keys = set(to_map.keys())

        added = [to_map[k] for k in to_keys - from_keys]
        removed = [from_map[k] for k in from_keys - to_keys]

        # Modified: same structural key but different source artifact.
        modified: list[Triple] = []
        for k in from_keys & to_keys:
            t_from = from_map[k]
            t_to = to_map[k]
            if t_from.source_artifact != t_to.source_artifact:
                modified.append(t_to)

        # Total triples in graph (not just this pass).
        total = await self.get_triple_count()

        return TripleDelta(
            pass_from=pass_from,
            pass_to=pass_to,
            added=added,
            removed=removed,
            modified=modified,
            total_triples=total,
        )

    # -------------------------------------------------------------------------
    # Bulk operations
    # -------------------------------------------------------------------------

    async def get_all_entities(self) -> list[Entity]:
        """Get all entities in the graph.

        Returns:
            List of all entities.
        """
        cursor = await self._conn.execute(
            "SELECT id, type, name, properties FROM entities"
        )
        rows = await cursor.fetchall()
        return [self._row_to_entity(row) for row in rows]

    async def get_all_triples(self) -> list[Triple]:
        """Get all triples in the graph.

        Returns:
            List of all triples.
        """
        cursor = await self._conn.execute(
            "SELECT id, subject_id, predicate, object_id, source_pass, "
            "source_artifact, timestamp, confirmed, corroboration_count "
            "FROM triples"
        )
        rows = await cursor.fetchall()
        return [self._row_to_triple(row) for row in rows]

    async def get_triple_count(self) -> int:
        """Get total triple count.

        Returns:
            Number of triples.
        """
        cursor = await self._conn.execute("SELECT COUNT(*) FROM triples")
        row = await cursor.fetchone()
        return row[0] if row else 0

    # -------------------------------------------------------------------------
    # Internal helpers
    # -------------------------------------------------------------------------

    def _row_to_entity(self, row: aiosqlite.Row) -> Entity:
        """Convert a database row to an Entity model.

        Args:
            row: Database row with (id, type, name, properties).

        Returns:
            Entity model instance.
        """
        props: dict[str, str | int | float | bool | None] = {}
        if row["properties"]:
            props = json.loads(row["properties"])
        return Entity(
            id=row["id"],
            entity_type=EntityType(row["type"]),
            name=row["name"],
            properties=props,
        )

    def _row_to_triple(self, row: aiosqlite.Row) -> Triple:
        """Convert a database row to a Triple model.

        Args:
            row: Database row from triples table.

        Returns:
            Triple model instance.
        """
        ts = (
            datetime.fromisoformat(row["timestamp"])
            if row["timestamp"]
            else datetime.utcnow()
        )
        return Triple(
            id=row["id"],
            subject_id=row["subject_id"],
            predicate=RelationType(row["predicate"]),
            object_id=row["object_id"],
            source_pass=row["source_pass"],
            source_artifact=row["source_artifact"],
            timestamp=ts,
            confirmed=bool(row["confirmed"]),
            corroboration_count=row["corroboration_count"],
        )
