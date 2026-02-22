"""Abstract knowledge graph store interface.

This module defines the repository pattern interface for the knowledge graph
storage backend. The abstract class enables swapping between SQLite (default)
and Neo4j (optional upgrade) without changing consuming code.

All methods are async to support concurrent access from parallel Scribe workers.

Example:
    store: KnowledgeGraphStore = SQLiteGraphStore("./output/kg.db")
    await store.initialize()

    entity = await store.upsert_entity(EntityType.PROGRAM, "ACCT0100")
    neighborhood = await store.get_neighborhood("ACCT0100", EntityType.PROGRAM)
"""

from abc import ABC, abstractmethod

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


class KnowledgeGraphStore(ABC):
    """Abstract interface for knowledge graph persistence.

    Implementations must support concurrent access from parallel Scribe
    workers. The SQLite implementation uses WAL mode for this purpose.

    All methods are async to allow non-blocking I/O in the async pipeline.
    """

    @abstractmethod
    async def initialize(self) -> None:
        """Initialize the store, creating schema if needed.

        Must be called before any other operations. Idempotent —
        safe to call multiple times.

        Raises:
            RuntimeError: If initialization fails.
        """
        ...

    @abstractmethod
    async def close(self) -> None:
        """Close the store and release resources.

        Should be called during shutdown. Safe to call multiple times.
        """
        ...

    # -------------------------------------------------------------------------
    # Entity operations
    # -------------------------------------------------------------------------

    @abstractmethod
    async def upsert_entity(
        self,
        entity_type: EntityType,
        name: str,
        properties: dict[str, str | int | float | bool | None] | None = None,
    ) -> Entity:
        """Insert or update an entity by (type, name) uniqueness constraint.

        If the entity already exists, its properties are merged (new keys
        added, existing keys updated). If it does not exist, it is created.

        Args:
            entity_type: The entity classification.
            name: Canonical name of the entity.
            properties: Optional metadata to store/merge.

        Returns:
            The upserted Entity with its database ID populated.
        """
        ...

    @abstractmethod
    async def get_entity(
        self,
        entity_type: EntityType,
        name: str,
    ) -> Entity | None:
        """Look up an entity by its type and name.

        Args:
            entity_type: The entity classification.
            name: Canonical name of the entity.

        Returns:
            The Entity if found, None otherwise.
        """
        ...

    @abstractmethod
    async def get_entity_by_id(self, entity_id: int) -> Entity | None:
        """Look up an entity by its database ID.

        Args:
            entity_id: The entity's primary key.

        Returns:
            The Entity if found, None otherwise.
        """
        ...

    # -------------------------------------------------------------------------
    # Triple operations
    # -------------------------------------------------------------------------

    @abstractmethod
    async def insert_triple(
        self,
        subject_id: int,
        predicate: RelationType,
        object_id: int,
        source_pass: str | None = None,
        source_artifact: str | None = None,
        confirmed: bool = False,
    ) -> Triple:
        """Insert a new triple or increment corroboration if it already exists.

        If a triple with the same (subject_id, predicate, object_id) exists,
        its corroboration_count is incremented and confirmed is set to True.

        Args:
            subject_id: Database ID of the subject entity.
            predicate: The relationship type.
            object_id: Database ID of the object entity.
            source_pass: Which War Rig pass produced this triple.
            source_artifact: Source file being analyzed.
            confirmed: If True, mark as confirmed on first insertion.

        Returns:
            The inserted or updated Triple.
        """
        ...

    @abstractmethod
    async def ingest_raw_triples(
        self,
        raw_triples: list[RawTriple],
        confirmed: bool = False,
    ) -> list[Triple]:
        """Resolve and ingest a batch of raw triples.

        For each raw triple:
        1. Upsert subject and object entities
        2. Insert or corroborate the triple

        This is the primary batch ingestion method used after parsing
        Scribe output or preprocessor extraction.

        Args:
            raw_triples: List of unresolved triples to ingest.
            confirmed: If True, mark new triples as confirmed on first
                insertion (for ground-truth sources).

        Returns:
            List of persisted Triple objects with database IDs.
        """
        ...

    @abstractmethod
    async def get_triples_for_entity(
        self,
        entity_id: int,
        as_subject: bool = True,
        as_object: bool = True,
    ) -> list[Triple]:
        """Get all triples involving an entity.

        Args:
            entity_id: Database ID of the entity.
            as_subject: Include triples where entity is the subject.
            as_object: Include triples where entity is the object.

        Returns:
            List of matching triples.
        """
        ...

    # -------------------------------------------------------------------------
    # Neighborhood queries
    # -------------------------------------------------------------------------

    @abstractmethod
    async def get_neighborhood(
        self,
        name: str,
        entity_type: EntityType,
        hops: int = 2,
    ) -> Neighborhood | None:
        """Get the N-hop neighborhood around an entity.

        Returns all entities and triples reachable within `hops` hops
        of the target entity. Used by the Context Injector to build
        the structured context block for Scribe/Challenger prompts.

        Args:
            name: Canonical name of the target entity.
            entity_type: Type of the target entity.
            hops: Maximum number of hops to traverse (default 2).

        Returns:
            Neighborhood containing the subgraph, or None if entity not found.
        """
        ...

    # -------------------------------------------------------------------------
    # Conflict management
    # -------------------------------------------------------------------------

    @abstractmethod
    async def record_conflict(
        self,
        triple_a_id: int,
        triple_b_id: int,
    ) -> Conflict:
        """Record a conflict between two contradictory triples.

        Args:
            triple_a_id: Database ID of the first triple.
            triple_b_id: Database ID of the second triple.

        Returns:
            The created Conflict record.
        """
        ...

    @abstractmethod
    async def get_unresolved_conflicts(self) -> list[Conflict]:
        """Get all unresolved conflicts.

        Returns:
            List of conflicts where resolved is False.
        """
        ...

    @abstractmethod
    async def resolve_conflict(
        self,
        conflict_id: int,
        resolution: str,
    ) -> None:
        """Mark a conflict as resolved.

        Args:
            conflict_id: Database ID of the conflict.
            resolution: Description of which triple was kept and why.
        """
        ...

    # -------------------------------------------------------------------------
    # Delta / convergence
    # -------------------------------------------------------------------------

    @abstractmethod
    async def compute_delta(
        self,
        pass_from: str,
        pass_to: str,
    ) -> TripleDelta:
        """Compute the triple delta between two passes.

        Used to track convergence — when the delta drops below 5%,
        the graph has stabilized and additional passes are unlikely
        to improve documentation accuracy.

        Args:
            pass_from: Earlier pass identifier (e.g., "pass_1").
            pass_to: Later pass identifier (e.g., "pass_2").

        Returns:
            TripleDelta with added, removed, and modified triples.
        """
        ...

    # -------------------------------------------------------------------------
    # Bulk operations
    # -------------------------------------------------------------------------

    @abstractmethod
    async def get_all_entities(self) -> list[Entity]:
        """Get all entities in the graph.

        Returns:
            List of all entities.
        """
        ...

    @abstractmethod
    async def get_all_triples(self) -> list[Triple]:
        """Get all triples in the graph.

        Returns:
            List of all triples.
        """
        ...

    @abstractmethod
    async def get_triple_count(self) -> int:
        """Get the total number of triples in the graph.

        Returns:
            Total triple count.
        """
        ...
