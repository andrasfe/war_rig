"""Knowledge graph data models.

This module defines the Pydantic models for the knowledge graph domain:

- EntityType / RelationType: Enums for the fixed schema
- Entity: A node in the graph (program, dataset, copybook, etc.)
- Triple: A subject-predicate-object relationship with provenance
- RawTriple: An unresolved triple as parsed from LLM output
- Conflict: Two contradictory triples pending resolution
- Neighborhood: A 2-hop subgraph around a target entity
- TripleDelta: Change set between passes for convergence tracking

The schema is intentionally fixed (not RDF/OWL) because mainframe codebases
have a well-bounded set of entity and relationship types.
"""

from datetime import datetime
from enum import Enum

from pydantic import BaseModel, Field


class EntityType(str, Enum):
    """Classification of knowledge graph entity types.

    Maps to the fixed schema defined in the KG spec Section 4.1.
    CUSTOM is an escape hatch for constructs not covered by the schema.
    """

    PROGRAM = "PROGRAM"
    JCL_JOB = "JCL_JOB"
    JCL_STEP = "JCL_STEP"
    DATASET = "DATASET"
    COPYBOOK = "COPYBOOK"
    FIELD = "FIELD"
    PARAGRAPH = "PARAGRAPH"
    DB_TABLE = "DB_TABLE"
    CUSTOM = "CUSTOM"


class RelationType(str, Enum):
    """Classification of knowledge graph relationship types.

    Maps to the fixed schema defined in the KG spec Section 4.2.
    RELATED_TO is an escape hatch for relationships not covered by the schema.
    """

    CALLS = "CALLS"
    READS = "READS"
    WRITES = "WRITES"
    INCLUDES = "INCLUDES"
    EXECUTES = "EXECUTES"
    DEFINES_INPUT = "DEFINES_INPUT"
    DEFINES_OUTPUT = "DEFINES_OUTPUT"
    CONTAINS_STEP = "CONTAINS_STEP"
    DEFINES_FIELD = "DEFINES_FIELD"
    QUERIES = "QUERIES"
    MODIFIES = "MODIFIES"
    PERFORMS = "PERFORMS"
    RELATED_TO = "RELATED_TO"


class Entity(BaseModel):
    """A node in the knowledge graph.

    Represents a mainframe artifact with its type, canonical name,
    and optional properties for richer context injection.

    Attributes:
        id: Database-assigned unique identifier (None before persistence).
        entity_type: Classification of this entity.
        name: Canonical name (e.g., program ID, DSN, copybook name).
        properties: Type-specific metadata as a JSON-serializable dict.
            PROGRAM: source_file, language, loc, last_documented_pass
            DATASET: dsn, record_format, record_length, organization
            COPYBOOK: source_file, included_by_count
            FIELD: picture_clause, level_number, parent_structure
    """

    id: int | None = Field(
        default=None,
        description="Database-assigned primary key (None before insert)",
    )
    entity_type: EntityType = Field(
        ...,
        description="Classification of this entity",
    )
    name: str = Field(
        ...,
        description="Canonical name of the entity",
    )
    properties: dict[str, str | int | float | bool | None] = Field(
        default_factory=dict,
        description="Type-specific metadata (JSON-serializable)",
    )

    @property
    def qualified_name(self) -> str:
        """Return TYPE:name format used in triple notation.

        Returns:
            String in the form 'ENTITY_TYPE:entity_name'.
        """
        return f"{self.entity_type.value}:{self.name}"


class Triple(BaseModel):
    """A subject-predicate-object relationship in the knowledge graph.

    Each triple captures a structural relationship between two entities,
    along with provenance metadata for confidence tracking across passes.

    Attributes:
        id: Database-assigned unique identifier (None before persistence).
        subject_id: Foreign key to the subject entity.
        predicate: The relationship type.
        object_id: Foreign key to the object entity.
        source_pass: Which War Rig pass produced this triple.
        source_artifact: The source file being analyzed when emitted.
        timestamp: When the triple was emitted.
        confirmed: True when corroborated by Challenger or subsequent pass.
        corroboration_count: Number of independent confirmations.
    """

    id: int | None = Field(
        default=None,
        description="Database-assigned primary key (None before insert)",
    )
    subject_id: int = Field(
        ...,
        description="Foreign key to the subject entity",
    )
    predicate: RelationType = Field(
        ...,
        description="Relationship type between subject and object",
    )
    object_id: int = Field(
        ...,
        description="Foreign key to the object entity",
    )
    source_pass: str | None = Field(
        default=None,
        description="Which War Rig pass produced this triple (e.g., pass_1)",
    )
    source_artifact: str | None = Field(
        default=None,
        description="Source file the Scribe was analyzing when it emitted this",
    )
    timestamp: datetime = Field(
        default_factory=datetime.utcnow,
        description="When the triple was emitted",
    )
    confirmed: bool = Field(
        default=False,
        description="True when corroborated by Challenger or subsequent pass",
    )
    corroboration_count: int = Field(
        default=1,
        ge=1,
        description="Number of independent confirmations",
    )


class RawTriple(BaseModel):
    """An unresolved triple as parsed from LLM or preprocessor output.

    This is the intermediate representation before entity resolution
    and database insertion. Subject and object are identified by
    type:name strings rather than database IDs.

    Attributes:
        subject_type: Entity type of the subject.
        subject_name: Name of the subject entity.
        predicate: The relationship type.
        object_type: Entity type of the object.
        object_name: Name of the object entity.
        source_pass: Which pass produced this triple.
        source_artifact: The source file being analyzed.
    """

    subject_type: EntityType = Field(
        ...,
        description="Entity type of the subject",
    )
    subject_name: str = Field(
        ...,
        description="Name of the subject entity",
    )
    predicate: RelationType = Field(
        ...,
        description="Relationship type",
    )
    object_type: EntityType = Field(
        ...,
        description="Entity type of the object",
    )
    object_name: str = Field(
        ...,
        description="Name of the object entity",
    )
    source_pass: str | None = Field(
        default=None,
        description="Which War Rig pass produced this triple",
    )
    source_artifact: str | None = Field(
        default=None,
        description="Source file the Scribe was analyzing",
    )

    @property
    def subject_qualified(self) -> str:
        """Return subject as TYPE:name string."""
        return f"{self.subject_type.value}:{self.subject_name}"

    @property
    def object_qualified(self) -> str:
        """Return object as TYPE:name string."""
        return f"{self.object_type.value}:{self.object_name}"


class Conflict(BaseModel):
    """A pair of contradictory triples pending resolution.

    Created when two Scribes emit conflicting claims about the same
    relationship (e.g., one says READS, another says WRITES for the
    same program-dataset pair).

    Attributes:
        id: Database-assigned unique identifier (None before persistence).
        triple_a_id: Foreign key to the first conflicting triple.
        triple_b_id: Foreign key to the second conflicting triple.
        resolved: Whether this conflict has been resolved.
        resolution: Description of which triple was kept and why.
    """

    id: int | None = Field(
        default=None,
        description="Database-assigned primary key (None before insert)",
    )
    triple_a_id: int = Field(
        ...,
        description="Foreign key to the first conflicting triple",
    )
    triple_b_id: int = Field(
        ...,
        description="Foreign key to the second conflicting triple",
    )
    resolved: bool = Field(
        default=False,
        description="Whether this conflict has been resolved",
    )
    resolution: str | None = Field(
        default=None,
        description="Which triple was kept and why",
    )


class Neighborhood(BaseModel):
    """A subgraph around a target entity, used for context injection.

    Contains entities and triples reachable within N hops of the target.
    The context formatter uses this to build the structured context block
    injected into Scribe/Challenger prompts.

    Attributes:
        target: The central entity of this neighborhood.
        entities: All entities in the neighborhood (including target).
        triples: All triples connecting entities in the neighborhood.
        hop_depth: How many hops were traversed to build this neighborhood.
    """

    target: Entity = Field(
        ...,
        description="The central entity of this neighborhood",
    )
    entities: list[Entity] = Field(
        default_factory=list,
        description="All entities in the neighborhood (including target)",
    )
    triples: list[Triple] = Field(
        default_factory=list,
        description="All triples connecting entities in the neighborhood",
    )
    hop_depth: int = Field(
        default=2,
        ge=1,
        le=5,
        description="How many hops were traversed",
    )


class TripleDelta(BaseModel):
    """Change set between two passes for convergence tracking.

    Used to determine when the graph has stabilized (triple delta < 5%).
    Tracks new, modified, and removed triples between consecutive passes.

    Attributes:
        pass_from: The earlier pass identifier.
        pass_to: The later pass identifier.
        added: Triples present in pass_to but not pass_from.
        removed: Triples present in pass_from but not pass_to.
        modified: Triples whose provenance changed between passes.
        total_triples: Total triples in the graph after pass_to.
    """

    pass_from: str = Field(
        ...,
        description="Earlier pass identifier (e.g., pass_1)",
    )
    pass_to: str = Field(
        ...,
        description="Later pass identifier (e.g., pass_2)",
    )
    added: list[Triple] = Field(
        default_factory=list,
        description="Triples added in the later pass",
    )
    removed: list[Triple] = Field(
        default_factory=list,
        description="Triples removed in the later pass",
    )
    modified: list[Triple] = Field(
        default_factory=list,
        description="Triples with changed provenance",
    )
    total_triples: int = Field(
        default=0,
        ge=0,
        description="Total triples in the graph after pass_to",
    )

    @property
    def change_count(self) -> int:
        """Total number of changed triples."""
        return len(self.added) + len(self.removed) + len(self.modified)

    @property
    def change_rate(self) -> float:
        """Fraction of triples that changed (0.0 to 1.0).

        Returns 0.0 if total_triples is 0 (empty graph).
        """
        if self.total_triples == 0:
            return 0.0
        return self.change_count / self.total_triples

    @property
    def has_converged(self) -> bool:
        """Check if change rate is below 5% convergence threshold."""
        return self.change_rate < 0.05
