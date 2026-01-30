"""
Cross-reference resolver for Citadel.

Binds raw references extracted from source files to known artifacts in the registry.
This is the central resolution component that transforms RawReference objects into
resolved Relationship objects or UnresolvedReference entries.
"""

import logging
import uuid
from collections import defaultdict

from citadel.graph.model import Relationship, UnresolvedReference
from citadel.parser.engine import RawReference
from citadel.resolver.alias_resolver import AliasResolver
from citadel.resolver.registry import ArtifactRegistry

logger = logging.getLogger(__name__)


class CrossReferenceResolver:
    """
    Resolves raw references to artifacts and builds relationship edges.

    The resolver takes raw references from parsed files and attempts to bind
    them to known artifacts in the registry using a multi-stage resolution
    process:

    1. Exact match - Direct lookup by ID or canonical name
    2. Alias match - Lookup via registered aliases
    3. Transformation match - Apply naming convention transformations
    4. Fuzzy match - Use fuzzy string matching as fallback

    References that cannot be resolved are collected as UnresolvedReference
    objects for later LLM-assisted disambiguation or manual review.

    Attributes:
        resolution_stats: Dictionary tracking resolution method counts.
    """

    def __init__(
        self,
        registry: ArtifactRegistry,
        alias_resolver: AliasResolver,
    ) -> None:
        """
        Initialize the cross-reference resolver.

        Args:
            registry: The artifact registry containing all known artifacts.
            alias_resolver: The alias resolver for applying transformation rules.
        """
        self._registry = registry
        self._alias_resolver = alias_resolver
        self._resolution_stats: dict[str, int] = defaultdict(int)

    def resolve_all(
        self,
        raw_references: list[RawReference],
    ) -> tuple[list[Relationship], list[UnresolvedReference]]:
        """
        Resolve all references, returning resolved relationships and unresolved references.

        Iterates through all raw references and attempts resolution for each.
        Successfully resolved references become Relationship objects.
        Failed resolutions become UnresolvedReference objects with candidate
        information for later disambiguation.

        Args:
            raw_references: List of raw references extracted from parsed files.

        Returns:
            A tuple of (resolved_relationships, unresolved_references):
            - resolved_relationships: List of Relationship objects for successful resolutions.
            - unresolved_references: List of UnresolvedReference objects for failed resolutions.
        """
        resolved: list[Relationship] = []
        unresolved: list[UnresolvedReference] = []

        # Track relationships to avoid duplicates
        seen_relationships: set[tuple[str, str, str]] = set()

        for raw_ref in raw_references:
            artifact_id, confidence, method = self._alias_resolver.resolve(raw_ref)

            if artifact_id is not None:
                # Successfully resolved
                relationship = self._create_relationship(
                    raw_ref, artifact_id, confidence, method
                )

                # Deduplicate relationships
                rel_key = (
                    relationship.from_artifact,
                    relationship.to_artifact,
                    relationship.relationship_type.value,
                )
                if rel_key not in seen_relationships:
                    resolved.append(relationship)
                    seen_relationships.add(rel_key)
                    self._resolution_stats[method] += 1
                    logger.debug(
                        "Resolved reference '%s' -> %s (method=%s, confidence=%.2f)",
                        raw_ref.raw_text,
                        artifact_id,
                        method,
                        confidence,
                    )
            else:
                # Failed to resolve
                unresolved_ref = self._create_unresolved(raw_ref, method)
                unresolved.append(unresolved_ref)
                self._resolution_stats["unresolved"] += 1
                logger.debug(
                    "Unresolved reference: '%s' at %s (reason=%s)",
                    raw_ref.raw_text,
                    raw_ref.location,
                    method,
                )

        # Log summary
        total = len(raw_references)
        resolved_count = len(resolved)
        unresolved_count = len(unresolved)
        logger.info(
            "Resolution complete: %d/%d resolved (%.1f%%), %d unresolved",
            resolved_count,
            total,
            (resolved_count / total * 100) if total > 0 else 0,
            unresolved_count,
        )

        return resolved, unresolved

    def resolve_single(
        self,
        raw_reference: RawReference,
    ) -> Relationship | UnresolvedReference:
        """
        Resolve a single reference.

        Convenience method for resolving one reference at a time.

        Args:
            raw_reference: The raw reference to resolve.

        Returns:
            Either a Relationship (if resolved) or UnresolvedReference (if not).
        """
        artifact_id, confidence, method = self._alias_resolver.resolve(raw_reference)

        if artifact_id is not None:
            self._resolution_stats[method] += 1
            return self._create_relationship(
                raw_reference, artifact_id, confidence, method
            )
        else:
            self._resolution_stats["unresolved"] += 1
            return self._create_unresolved(raw_reference, method)

    def _create_relationship(
        self,
        raw_ref: RawReference,
        target_artifact_id: str,
        confidence: float,
        resolution_method: str,
    ) -> Relationship:
        """
        Create a Relationship from a resolved reference.

        Args:
            raw_ref: The original raw reference.
            target_artifact_id: The ID of the resolved target artifact.
            confidence: The confidence score of the resolution.
            resolution_method: The method used to resolve (exact, alias, etc.).

        Returns:
            A Relationship object representing the resolved reference.
        """
        # Generate a unique relationship ID
        relationship_id = str(uuid.uuid4())

        # Determine the source artifact (from containing_artifact or create synthetic)
        from_artifact = raw_ref.containing_artifact
        if from_artifact is None:
            # If no containing artifact, use the file path as a synthetic source
            from_artifact = f"file::{raw_ref.location.file_path}"

        return Relationship(
            id=relationship_id,
            from_artifact=from_artifact,
            to_artifact=target_artifact_id,
            relationship_type=raw_ref.relationship_type,
            location=raw_ref.location,
            evidence_text=raw_ref.raw_text,
            columns_accessed=raw_ref.columns_mentioned,
            confidence=confidence,
            resolution_method=resolution_method,
            is_resolved=True,
        )

    def _create_unresolved(
        self,
        raw_ref: RawReference,
        reason: str,
    ) -> UnresolvedReference:
        """
        Create an UnresolvedReference from a failed resolution.

        Includes candidate information for LLM disambiguation.

        Args:
            raw_ref: The original raw reference.
            reason: The reason resolution failed.

        Returns:
            An UnresolvedReference object with candidate information.
        """
        # Get potential candidates from fuzzy matching for LLM context
        candidates: list[str] = []
        best_score: float = 0.0

        # Try to find similar artifacts for context
        fuzzy_results = self._registry.lookup_fuzzy(
            raw_ref.raw_text,
            raw_ref.expected_type,
            threshold=0.5,  # Lower threshold to get more candidates
        )

        for artifact, score in fuzzy_results[:5]:  # Top 5 candidates
            candidates.append(artifact.id)
            if score > best_score:
                best_score = score

        return UnresolvedReference(
            reference_text=raw_ref.raw_text,
            expected_type=raw_ref.expected_type,
            location=raw_ref.location,
            containing_artifact=raw_ref.containing_artifact,
            candidates=candidates,
            best_score=best_score,
            reason=reason,
        )

    def get_resolution_stats(self) -> dict[str, int]:
        """
        Get resolution statistics.

        Returns:
            Dictionary mapping resolution methods to counts.
            Keys include: "exact", "alias", "transformation", "fuzzy", "unresolved".
        """
        return dict(self._resolution_stats)

    def get_resolution_rate(self) -> float:
        """
        Calculate the overall resolution rate.

        Returns:
            Percentage of references that were successfully resolved (0.0 to 100.0).
        """
        total = sum(self._resolution_stats.values())
        if total == 0:
            return 0.0

        unresolved = self._resolution_stats.get("unresolved", 0)
        resolved = total - unresolved
        return (resolved / total) * 100

    def reset_stats(self) -> None:
        """Reset resolution statistics."""
        self._resolution_stats.clear()

    def bulk_register_aliases(
        self,
        alias_mappings: list[tuple[str, str, float]],
    ) -> int:
        """
        Register multiple aliases in the registry.

        Convenience method for batch alias registration.

        Args:
            alias_mappings: List of (alias, canonical_id, confidence) tuples.

        Returns:
            Number of aliases successfully registered.
        """
        registered = 0
        for alias, canonical_id, confidence in alias_mappings:
            try:
                self._registry.register_alias(alias, canonical_id, confidence)
                registered += 1
            except ValueError as e:
                logger.warning(
                    "Failed to register alias '%s' -> '%s': %s",
                    alias,
                    canonical_id,
                    e,
                )
        return registered

    def resolve_with_context(
        self,
        raw_ref: RawReference,
        additional_context: dict[str, str] | None = None,
    ) -> tuple[str | None, float, str, list[str]]:
        """
        Resolve a reference with additional context information.

        Extended resolution that returns candidate information even for
        successful resolutions, useful for validation and debugging.

        Args:
            raw_ref: The raw reference to resolve.
            additional_context: Optional dictionary of additional context
                (e.g., surrounding code patterns, schema hints).

        Returns:
            A tuple of (artifact_id, confidence, method, candidates) where:
            - artifact_id: The resolved artifact ID, or None if unresolved.
            - confidence: Confidence score (0.0 to 1.0).
            - method: The resolution method used or failure reason.
            - candidates: List of candidate artifact IDs considered.
        """
        # Get candidates regardless of resolution outcome
        candidates: list[str] = []

        # Get fuzzy candidates for context
        fuzzy_results = self._registry.lookup_fuzzy(
            raw_ref.raw_text,
            raw_ref.expected_type,
            threshold=0.6,
        )
        candidates = [artifact.id for artifact, _ in fuzzy_results[:10]]

        # Perform actual resolution
        artifact_id, confidence, method = self._alias_resolver.resolve(raw_ref)

        return (artifact_id, confidence, method, candidates)
