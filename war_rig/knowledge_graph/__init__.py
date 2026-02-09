"""Knowledge graph subsystem for War Rig.

This package implements a lightweight knowledge graph that captures structural
relationships between mainframe artifacts (programs, jobs, datasets, copybooks,
fields, paragraphs, DB tables). The graph is built incrementally during Scribe
documentation passes and injected as focused context into subsequent passes.

Key components:
- models: Pydantic models for entities, triples, conflicts, neighborhoods
- store: Abstract KnowledgeGraphStore interface (repository pattern)
- sqlite_store: SQLite implementation with WAL mode for concurrent writes
- extractors: Deterministic triple extraction from preprocessor output
- parser: LLM triple output parser for Scribe-emitted triples
- context: Context formatting for Scribe/Challenger prompt injection
- ingestion: Triple ingestion coordination
- conflicts: Conflict detection and resolution tracking
- queries: Graph query helpers (neighborhood, delta)

Example:
    from war_rig.knowledge_graph import KnowledgeGraphStore, SQLiteGraphStore
    from war_rig.knowledge_graph.models import EntityType, RelationType

    store = SQLiteGraphStore("./output/knowledge_graph.db")
    await store.initialize()

    # Query neighborhood for a program
    neighborhood = await store.get_neighborhood("ACCT0100", EntityType.PROGRAM, hops=2)
"""

from war_rig.knowledge_graph.conflicts import ConflictDetector
from war_rig.knowledge_graph.context import ContextFormatter
from war_rig.knowledge_graph.extractors import TripleExtractor
from war_rig.knowledge_graph.ingestion import (
    TripleIngestionCoordinator,
    ingest_preprocessor_triples,
)
from war_rig.knowledge_graph.manager import KnowledgeGraphManager
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
from war_rig.knowledge_graph.parser import TripleOutputParser
from war_rig.knowledge_graph.queries import GraphQueryHelper
from war_rig.knowledge_graph.sqlite_store import SQLiteGraphStore
from war_rig.knowledge_graph.store import KnowledgeGraphStore

__all__ = [
    # Models
    "Entity",
    "EntityType",
    "RelationType",
    "Triple",
    "RawTriple",
    "Conflict",
    "Neighborhood",
    "TripleDelta",
    # Store
    "KnowledgeGraphStore",
    "SQLiteGraphStore",
    # Components
    "TripleExtractor",
    "TripleOutputParser",
    "ContextFormatter",
    "TripleIngestionCoordinator",
    "ingest_preprocessor_triples",
    "ConflictDetector",
    "GraphQueryHelper",
    # Manager (pipeline facade)
    "KnowledgeGraphManager",
]
