"""
Dependency graph data models.

This module defines the core data structures for the dependency graph,
including artifacts (nodes), relationships (edges), and the complete
graph container with statistics.
"""

from datetime import datetime
from typing import Any

from pydantic import BaseModel, Field

from citadel.specs.schema import ArtifactCategory, ArtifactType, RelationshipType


class SourceLocation(BaseModel):
    """Location in source code."""

    file_path: str
    line_start: int
    line_end: int | None = None
    column_start: int | None = None
    column_end: int | None = None

    def __str__(self) -> str:
        if self.line_end and self.line_end != self.line_start:
            return f"{self.file_path}:{self.line_start}-{self.line_end}"
        return f"{self.file_path}:{self.line_start}"


class Artifact(BaseModel):
    """
    A node in the dependency graph.

    Represents any identifiable entity: program, table, copybook, etc.
    """

    # Identity
    id: str  # "{type}::{canonical_name}"
    artifact_type: ArtifactType
    category: ArtifactCategory
    canonical_name: str

    # Aliases and naming
    aliases: list[str] = Field(default_factory=list)
    display_name: str | None = None  # Human-friendly name

    # Source info
    defined_in: SourceLocation | None = None
    language: str

    # Type-specific attributes
    attributes: dict[str, Any] = Field(default_factory=dict)
    # Examples:
    #   TABLE: {"columns": ["COL1", "COL2"], "primary_key": ["COL1"]}
    #   PROGRAM: {"entry_point": "MAIN", "compiler_options": [...]}
    #   RECORD_LAYOUT: {"fields": [...], "total_length": 500}

    # For record layouts: mapping to data artifact
    maps_to: str | None = None  # Artifact ID of table/file
    field_mappings: dict[str, str] = Field(default_factory=dict)  # local_field -> canonical_column


class Relationship(BaseModel):
    """
    An edge in the dependency graph.

    Represents a relationship between two artifacts.
    """

    # Identity
    id: str  # Auto-generated UUID

    # Endpoints
    from_artifact: str  # Artifact ID
    to_artifact: str  # Artifact ID
    relationship_type: RelationshipType

    # Evidence
    location: SourceLocation  # Where this relationship is expressed
    evidence_text: str  # The actual source text

    # For data relationships
    columns_accessed: list[str] = Field(default_factory=list)
    access_mode: str | None = None  # For files: INPUT, OUTPUT, I-O

    # Resolution metadata
    confidence: float = 1.0
    resolution_method: str = "exact"  # exact, alias, fuzzy, llm
    is_resolved: bool = True


class UnresolvedReference(BaseModel):
    """
    A reference that could not be resolved to a known artifact.
    """

    reference_text: str  # What was referenced
    expected_type: ArtifactType | None = None
    location: SourceLocation
    containing_artifact: str | None = None  # Which artifact contains this ref

    # Resolution attempt info
    candidates: list[str] = Field(default_factory=list)  # Artifact IDs that might match
    best_score: float = 0.0
    reason: str  # Why resolution failed


class GraphStatistics(BaseModel):
    """Statistics about the dependency graph."""

    files_analyzed: int
    files_skipped: int
    files_failed: int

    artifacts_by_type: dict[str, int] = Field(default_factory=dict)
    artifacts_total: int

    relationships_by_type: dict[str, int] = Field(default_factory=dict)
    relationships_total: int

    unresolved_count: int
    resolution_rate: float  # Percentage resolved

    languages_detected: list[str] = Field(default_factory=list)
    specs_used: dict[str, str] = Field(default_factory=dict)  # language -> spec_id


class DependencyGraph(BaseModel):
    """
    The complete dependency graph output.

    This is the final deliverable of the analysis.
    """

    # Metadata
    version: str = "1.0"
    generated_at: datetime = Field(default_factory=datetime.utcnow)
    source_root: str

    # Graph data
    artifacts: dict[str, Artifact] = Field(default_factory=dict)  # ID -> Artifact
    relationships: list[Relationship] = Field(default_factory=list)
    unresolved: list[UnresolvedReference] = Field(default_factory=list)

    # Statistics
    statistics: GraphStatistics

    # Reproducibility
    config_hash: str  # Hash of config used
    specs_hashes: dict[str, str] = Field(default_factory=dict)  # language -> spec content hash
