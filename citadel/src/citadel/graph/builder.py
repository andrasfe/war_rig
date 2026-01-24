"""
Graph builder for Citadel.

Assembles the final dependency graph from artifacts, relationships, and
unresolved references. Computes statistics and produces the complete
DependencyGraph output structure.
"""

import hashlib
import logging
from collections import defaultdict
from datetime import datetime
from pathlib import Path

from citadel.config import CitadelConfig
from citadel.graph.model import (
    Artifact,
    DependencyGraph,
    GraphStatistics,
    Relationship,
    UnresolvedReference,
)

logger = logging.getLogger(__name__)


class GraphBuilder:
    """
    Assembles the final dependency graph from parsed and resolved components.

    The builder accumulates artifacts, relationships, and unresolved references
    during the analysis process, then produces a complete DependencyGraph with
    computed statistics.

    Usage:
        builder = GraphBuilder(source_root, config)
        builder.add_artifacts(artifacts)
        builder.add_relationships(relationships)
        builder.add_unresolved(unresolved)
        builder.set_specs_used(specs)
        graph = builder.build()

    The builder tracks:
    - Artifacts indexed by ID with duplicate detection
    - Relationships with validation against known artifacts
    - Unresolved references for later LLM disambiguation
    - File processing statistics
    - Spec version information for reproducibility
    """

    def __init__(self, source_root: Path, config: CitadelConfig) -> None:
        """
        Initialize the graph builder.

        Args:
            source_root: Root directory of the analyzed source code.
            config: Citadel configuration instance.
        """
        self._source_root = source_root
        self._config = config

        # Graph components
        self._artifacts: dict[str, Artifact] = {}
        self._relationships: list[Relationship] = []
        self._unresolved: list[UnresolvedReference] = []

        # Metadata tracking
        self._specs_used: dict[str, str] = {}  # language -> spec_id
        self._specs_hashes: dict[str, str] = {}  # language -> content hash

        # File processing stats
        self._files_analyzed: int = 0
        self._files_skipped: int = 0
        self._files_failed: int = 0
        self._languages_detected: set[str] = set()

        # Validation tracking
        self._orphan_relationships: list[Relationship] = []

    def add_artifacts(self, artifacts: list[Artifact]) -> None:
        """
        Add artifacts to the graph.

        Handles duplicates by updating existing artifacts if the new one
        has more information (e.g., a defined location when previous didn't).

        Args:
            artifacts: List of Artifact objects to add.
        """
        for artifact in artifacts:
            self._add_single_artifact(artifact)

    def _add_single_artifact(self, artifact: Artifact) -> None:
        """
        Add a single artifact with duplicate handling.

        Args:
            artifact: The artifact to add.
        """
        if artifact.id in self._artifacts:
            existing = self._artifacts[artifact.id]
            # Merge: prefer artifact with defined_in location
            if existing.defined_in is None and artifact.defined_in is not None:
                self._artifacts[artifact.id] = artifact
                logger.debug(
                    "Updated artifact %s with location info from %s",
                    artifact.id,
                    artifact.defined_in,
                )
            elif artifact.aliases and not existing.aliases:
                # Merge aliases
                existing_aliases = set(existing.aliases)
                existing_aliases.update(artifact.aliases)
                existing.aliases = list(existing_aliases)
                logger.debug(
                    "Merged aliases for artifact %s: %s",
                    artifact.id,
                    existing.aliases,
                )
        else:
            self._artifacts[artifact.id] = artifact
            self._languages_detected.add(artifact.language)
            logger.debug("Added artifact: %s (%s)", artifact.id, artifact.artifact_type.value)

    def add_relationships(self, relationships: list[Relationship]) -> None:
        """
        Add relationships to the graph.

        Validates that referenced artifacts exist. Relationships referencing
        unknown artifacts are tracked separately for reporting.

        Args:
            relationships: List of Relationship objects to add.
        """
        for relationship in relationships:
            self._add_single_relationship(relationship)

    def _add_single_relationship(self, relationship: Relationship) -> None:
        """
        Add a single relationship with validation.

        Args:
            relationship: The relationship to add.
        """
        # Check if both endpoints exist
        from_exists = (
            relationship.from_artifact in self._artifacts
            or relationship.from_artifact.startswith("file::")
        )
        to_exists = relationship.to_artifact in self._artifacts

        if not from_exists or not to_exists:
            if not from_exists:
                logger.warning(
                    "Relationship %s references unknown source artifact: %s",
                    relationship.id,
                    relationship.from_artifact,
                )
            if not to_exists:
                logger.warning(
                    "Relationship %s references unknown target artifact: %s",
                    relationship.id,
                    relationship.to_artifact,
                )
            self._orphan_relationships.append(relationship)
        else:
            self._relationships.append(relationship)
            logger.debug(
                "Added relationship: %s -[%s]-> %s",
                relationship.from_artifact,
                relationship.relationship_type.value,
                relationship.to_artifact,
            )

    def add_unresolved(self, unresolved: list[UnresolvedReference]) -> None:
        """
        Add unresolved references to the graph.

        These are references that could not be resolved to known artifacts
        and may need LLM-assisted disambiguation.

        Args:
            unresolved: List of UnresolvedReference objects to add.
        """
        self._unresolved.extend(unresolved)
        logger.debug("Added %d unresolved references", len(unresolved))

    def set_specs_used(self, specs: dict[str, str]) -> None:
        """
        Set the specifications used during analysis.

        This information is used for reproducibility and versioning.

        Args:
            specs: Dictionary mapping language names to spec IDs.
        """
        self._specs_used = specs.copy()
        logger.debug("Set specs used: %s", list(specs.keys()))

    def set_specs_hashes(self, hashes: dict[str, str]) -> None:
        """
        Set content hashes for the specifications used.

        Args:
            hashes: Dictionary mapping language names to spec content hashes.
        """
        self._specs_hashes = hashes.copy()

    def record_file_analyzed(self) -> None:
        """Record that a file was successfully analyzed."""
        self._files_analyzed += 1

    def record_file_skipped(self) -> None:
        """Record that a file was skipped (e.g., binary, too large)."""
        self._files_skipped += 1

    def record_file_failed(self) -> None:
        """Record that a file analysis failed with an error."""
        self._files_failed += 1

    def set_file_counts(
        self,
        analyzed: int,
        skipped: int,
        failed: int,
    ) -> None:
        """
        Set file processing counts directly.

        Args:
            analyzed: Number of files successfully analyzed.
            skipped: Number of files skipped.
            failed: Number of files that failed processing.
        """
        self._files_analyzed = analyzed
        self._files_skipped = skipped
        self._files_failed = failed

    def build(self) -> DependencyGraph:
        """
        Finalize and return the complete dependency graph.

        Computes statistics, generates config hash, and assembles
        all components into the final DependencyGraph structure.

        Returns:
            The complete DependencyGraph with all artifacts, relationships,
            unresolved references, and computed statistics.
        """
        # Compute statistics
        statistics = self._compute_statistics()

        # Generate config hash for reproducibility
        config_hash = self._compute_config_hash()

        # Build the final graph
        graph = DependencyGraph(
            version="1.0",
            generated_at=datetime.utcnow(),
            source_root=str(self._source_root.absolute()),
            artifacts=self._artifacts.copy(),
            relationships=self._relationships.copy(),
            unresolved=self._unresolved.copy(),
            statistics=statistics,
            config_hash=config_hash,
            specs_hashes=self._specs_hashes.copy(),
        )

        logger.info(
            "Built dependency graph: %d artifacts, %d relationships, %d unresolved",
            len(self._artifacts),
            len(self._relationships),
            len(self._unresolved),
        )

        return graph

    def _compute_statistics(self) -> GraphStatistics:
        """
        Compute statistics about the dependency graph.

        Returns:
            GraphStatistics with counts and metrics for the graph.
        """
        # Count artifacts by type
        artifacts_by_type: dict[str, int] = defaultdict(int)
        for artifact in self._artifacts.values():
            artifacts_by_type[artifact.artifact_type.value] += 1

        # Count relationships by type
        relationships_by_type: dict[str, int] = defaultdict(int)
        for relationship in self._relationships:
            relationships_by_type[relationship.relationship_type.value] += 1

        # Calculate resolution rate
        total_refs = len(self._relationships) + len(self._unresolved)
        resolution_rate = (
            (len(self._relationships) / total_refs * 100) if total_refs > 0 else 100.0
        )

        return GraphStatistics(
            files_analyzed=self._files_analyzed,
            files_skipped=self._files_skipped,
            files_failed=self._files_failed,
            artifacts_by_type=dict(artifacts_by_type),
            artifacts_total=len(self._artifacts),
            relationships_by_type=dict(relationships_by_type),
            relationships_total=len(self._relationships),
            unresolved_count=len(self._unresolved),
            resolution_rate=resolution_rate,
            languages_detected=sorted(self._languages_detected),
            specs_used=self._specs_used.copy(),
        )

    def _compute_config_hash(self) -> str:
        """
        Compute a hash of the configuration for reproducibility.

        Returns:
            SHA256 hash of configuration settings that affect analysis.
        """
        # Include config settings that affect analysis
        config_str = (
            f"llm_model={self._config.llm_model};"
            f"llm_disambiguation={self._config.llm_disambiguation};"
            f"parallel_files={self._config.parallel_files}"
        )
        return hashlib.sha256(config_str.encode()).hexdigest()[:16]

    def get_artifact_count(self) -> int:
        """Get the current number of artifacts."""
        return len(self._artifacts)

    def get_relationship_count(self) -> int:
        """Get the current number of relationships."""
        return len(self._relationships)

    def get_unresolved_count(self) -> int:
        """Get the current number of unresolved references."""
        return len(self._unresolved)

    def get_orphan_relationships(self) -> list[Relationship]:
        """
        Get relationships that reference unknown artifacts.

        Returns:
            List of relationships with invalid artifact references.
        """
        return self._orphan_relationships.copy()

    def get_artifact(self, artifact_id: str) -> Artifact | None:
        """
        Get an artifact by ID.

        Args:
            artifact_id: The artifact ID to look up.

        Returns:
            The Artifact if found, None otherwise.
        """
        return self._artifacts.get(artifact_id)

    def get_artifacts_by_type(self, artifact_type: str) -> list[Artifact]:
        """
        Get all artifacts of a specific type.

        Args:
            artifact_type: The artifact type value (e.g., "program", "table").

        Returns:
            List of matching artifacts.
        """
        return [
            artifact
            for artifact in self._artifacts.values()
            if artifact.artifact_type.value == artifact_type
        ]

    def get_relationships_for(
        self,
        artifact_id: str,
        direction: str = "both",
    ) -> list[Relationship]:
        """
        Get relationships involving an artifact.

        Args:
            artifact_id: The artifact ID to find relationships for.
            direction: "outgoing" (from this artifact), "incoming" (to this artifact),
                or "both" (either direction).

        Returns:
            List of matching relationships.
        """
        results: list[Relationship] = []

        for rel in self._relationships:
            if direction in ("outgoing", "both") and rel.from_artifact == artifact_id:
                results.append(rel)
            elif direction in ("incoming", "both") and rel.to_artifact == artifact_id:
                results.append(rel)

        return results

    def validate(self) -> list[str]:
        """
        Validate the graph for common issues.

        Returns:
            List of validation warning messages.
        """
        warnings: list[str] = []

        # Check for orphan relationships
        if self._orphan_relationships:
            warnings.append(
                f"{len(self._orphan_relationships)} relationships reference unknown artifacts"
            )

        # Check for isolated artifacts (no relationships)
        artifact_ids_in_rels = set()
        for rel in self._relationships:
            artifact_ids_in_rels.add(rel.from_artifact)
            artifact_ids_in_rels.add(rel.to_artifact)

        isolated = [
            aid for aid in self._artifacts if aid not in artifact_ids_in_rels
        ]
        if isolated:
            warnings.append(f"{len(isolated)} artifacts have no relationships")

        # Check for duplicate relationships
        seen = set()
        duplicates = 0
        for rel in self._relationships:
            key = (rel.from_artifact, rel.to_artifact, rel.relationship_type.value)
            if key in seen:
                duplicates += 1
            seen.add(key)
        if duplicates:
            warnings.append(f"{duplicates} duplicate relationships detected")

        # Check unresolved rate
        total_refs = len(self._relationships) + len(self._unresolved)
        if total_refs > 0:
            unresolved_rate = len(self._unresolved) / total_refs * 100
            if unresolved_rate > 20:
                warnings.append(
                    f"High unresolved reference rate: {unresolved_rate:.1f}%"
                )

        return warnings

    def merge(self, other: "GraphBuilder") -> None:
        """
        Merge another graph builder's data into this one.

        Useful for parallel processing where multiple builders are used.

        Args:
            other: Another GraphBuilder to merge from.
        """
        # Merge artifacts
        for artifact in other._artifacts.values():
            self._add_single_artifact(artifact)

        # Merge relationships
        for relationship in other._relationships:
            self._add_single_relationship(relationship)

        # Merge unresolved
        self._unresolved.extend(other._unresolved)

        # Merge specs
        self._specs_used.update(other._specs_used)
        self._specs_hashes.update(other._specs_hashes)

        # Merge file counts
        self._files_analyzed += other._files_analyzed
        self._files_skipped += other._files_skipped
        self._files_failed += other._files_failed

        # Merge languages
        self._languages_detected.update(other._languages_detected)

        logger.debug(
            "Merged graph builder: +%d artifacts, +%d relationships",
            len(other._artifacts),
            len(other._relationships),
        )

    def clear(self) -> None:
        """Reset the builder to empty state."""
        self._artifacts.clear()
        self._relationships.clear()
        self._unresolved.clear()
        self._specs_used.clear()
        self._specs_hashes.clear()
        self._files_analyzed = 0
        self._files_skipped = 0
        self._files_failed = 0
        self._languages_detected.clear()
        self._orphan_relationships.clear()
        logger.debug("Graph builder cleared")
