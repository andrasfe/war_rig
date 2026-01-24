"""
Artifact registry for Citadel.

Central registry of all discovered artifacts with fast lookup by name, alias, and type.
Provides exact matching, alias resolution, and fuzzy matching capabilities.
"""

import logging
from collections import defaultdict
from typing import Iterator

from rapidfuzz import fuzz, process

from citadel.graph.model import Artifact
from citadel.specs.schema import ArtifactType

logger = logging.getLogger(__name__)


class ArtifactRegistry:
    """
    Central registry of all discovered artifacts.

    Provides fast lookup by name, alias, and type. Supports exact matching,
    alias resolution, and fuzzy matching using rapidfuzz.

    The registry maintains multiple indices for efficient lookups:
    - Primary index: artifact ID -> Artifact
    - Name index: canonical_name (lowercased) -> list of artifact IDs
    - Type index: ArtifactType -> list of artifact IDs
    - Alias index: alias (lowercased) -> (artifact ID, confidence)
    """

    def __init__(self) -> None:
        """Initialize empty registry with indices."""
        self._artifacts: dict[str, Artifact] = {}  # ID -> Artifact
        self._by_name: dict[str, list[str]] = defaultdict(list)  # canonical_name -> [IDs]
        self._by_type: dict[ArtifactType, list[str]] = defaultdict(list)  # type -> [IDs]
        self._aliases: dict[str, tuple[str, float]] = {}  # alias -> (ID, confidence)

    def register(self, artifact: Artifact) -> None:
        """
        Add artifact to registry, updating all indices.

        If an artifact with the same ID already exists, it will be replaced
        and indices will be updated accordingly.

        Args:
            artifact: The artifact to register.

        Raises:
            ValueError: If artifact has an empty or invalid ID.
        """
        if not artifact.id:
            raise ValueError("Artifact must have a non-empty ID")

        # Remove from indices if already registered (for re-registration)
        if artifact.id in self._artifacts:
            self._remove_from_indices(artifact.id)

        # Store artifact
        self._artifacts[artifact.id] = artifact

        # Index by canonical name (case-insensitive lookup)
        name_key = artifact.canonical_name.lower()
        self._by_name[name_key].append(artifact.id)

        # Index by type
        self._by_type[artifact.artifact_type].append(artifact.id)

        # Register artifact's aliases
        for alias in artifact.aliases:
            alias_key = alias.lower()
            # Only register if not already registered with higher confidence
            if alias_key not in self._aliases or self._aliases[alias_key][1] < 1.0:
                self._aliases[alias_key] = (artifact.id, 1.0)

        logger.debug(
            "Registered artifact: %s (type=%s, name=%s)",
            artifact.id,
            artifact.artifact_type.value,
            artifact.canonical_name,
        )

    def _remove_from_indices(self, artifact_id: str) -> None:
        """
        Remove artifact from all indices.

        Args:
            artifact_id: ID of the artifact to remove from indices.
        """
        artifact = self._artifacts.get(artifact_id)
        if artifact is None:
            return

        # Remove from name index
        name_key = artifact.canonical_name.lower()
        if name_key in self._by_name:
            self._by_name[name_key] = [
                aid for aid in self._by_name[name_key] if aid != artifact_id
            ]
            if not self._by_name[name_key]:
                del self._by_name[name_key]

        # Remove from type index
        if artifact.artifact_type in self._by_type:
            self._by_type[artifact.artifact_type] = [
                aid for aid in self._by_type[artifact.artifact_type] if aid != artifact_id
            ]
            if not self._by_type[artifact.artifact_type]:
                del self._by_type[artifact.artifact_type]

        # Remove aliases pointing to this artifact
        aliases_to_remove = [
            alias for alias, (aid, _) in self._aliases.items() if aid == artifact_id
        ]
        for alias in aliases_to_remove:
            del self._aliases[alias]

    def register_alias(
        self, alias: str, canonical_id: str, confidence: float = 1.0
    ) -> None:
        """
        Register an alias mapping.

        Maps an alias string to a canonical artifact ID with a confidence score.
        If the alias already exists with lower confidence, it will be replaced.

        Args:
            alias: The alias string to register.
            canonical_id: The artifact ID that this alias points to.
            confidence: Confidence score (0.0 to 1.0) for this mapping.

        Raises:
            ValueError: If canonical_id does not exist in registry.
            ValueError: If confidence is not between 0.0 and 1.0.
        """
        if canonical_id not in self._artifacts:
            raise ValueError(f"Cannot register alias: artifact '{canonical_id}' not found")

        if not 0.0 <= confidence <= 1.0:
            raise ValueError(f"Confidence must be between 0.0 and 1.0, got {confidence}")

        alias_key = alias.lower()

        # Only update if new confidence is higher or alias doesn't exist
        if alias_key not in self._aliases or self._aliases[alias_key][1] < confidence:
            self._aliases[alias_key] = (canonical_id, confidence)
            logger.debug(
                "Registered alias: %s -> %s (confidence=%.2f)",
                alias,
                canonical_id,
                confidence,
            )

    def lookup(
        self, name: str, expected_type: ArtifactType | None = None
    ) -> list[tuple[Artifact, float]]:
        """
        Find artifacts matching a name.

        Searches in order:
        1. Exact match by artifact ID
        2. Match by canonical name
        3. Match by registered alias

        Args:
            name: The name to look up.
            expected_type: Optional type filter. If provided, only artifacts
                of this type will be returned.

        Returns:
            List of (artifact, confidence) tuples ordered by confidence descending.
            Confidence is 1.0 for exact/name matches, alias confidence for alias matches.
        """
        results: list[tuple[Artifact, float]] = []
        seen_ids: set[str] = set()

        name_lower = name.lower()

        # Try exact ID match first
        if name in self._artifacts:
            artifact = self._artifacts[name]
            if expected_type is None or artifact.artifact_type == expected_type:
                results.append((artifact, 1.0))
                seen_ids.add(name)

        # Try canonical name match
        if name_lower in self._by_name:
            for artifact_id in self._by_name[name_lower]:
                if artifact_id in seen_ids:
                    continue
                artifact = self._artifacts[artifact_id]
                if expected_type is None or artifact.artifact_type == expected_type:
                    results.append((artifact, 1.0))
                    seen_ids.add(artifact_id)

        # Try alias match
        if name_lower in self._aliases:
            artifact_id, confidence = self._aliases[name_lower]
            if artifact_id not in seen_ids:
                artifact = self._artifacts.get(artifact_id)
                if artifact and (expected_type is None or artifact.artifact_type == expected_type):
                    results.append((artifact, confidence))
                    seen_ids.add(artifact_id)

        # Sort by confidence descending
        results.sort(key=lambda x: x[1], reverse=True)

        return results

    def lookup_exact(self, artifact_id: str) -> Artifact | None:
        """
        Get artifact by exact ID.

        Args:
            artifact_id: The exact artifact ID to look up.

        Returns:
            The Artifact if found, None otherwise.
        """
        return self._artifacts.get(artifact_id)

    def lookup_fuzzy(
        self,
        name: str,
        expected_type: ArtifactType | None = None,
        threshold: float = 0.7,
    ) -> list[tuple[Artifact, float]]:
        """
        Fuzzy match against artifact names using rapidfuzz.

        Uses token_set_ratio for better matching of names with different
        word orders or additional/missing words.

        Args:
            name: The name to match against.
            expected_type: Optional type filter. If provided, only artifacts
                of this type will be considered.
            threshold: Minimum similarity score (0.0 to 1.0). Default is 0.7.

        Returns:
            List of (artifact, confidence) tuples ordered by confidence descending.
            Confidence is the normalized similarity score (0.0 to 1.0).
        """
        if not name:
            return []

        # Build candidate list based on type filter
        if expected_type is not None:
            candidate_ids = self._by_type.get(expected_type, [])
        else:
            candidate_ids = list(self._artifacts.keys())

        if not candidate_ids:
            return []

        # Build mapping of canonical names to artifact IDs for fuzzy matching
        # We match against canonical names, not IDs
        name_to_ids: dict[str, list[str]] = defaultdict(list)
        for artifact_id in candidate_ids:
            artifact = self._artifacts[artifact_id]
            name_to_ids[artifact.canonical_name].append(artifact_id)

        # Use rapidfuzz to find matches
        # token_set_ratio handles word reordering and partial matches well
        candidates = list(name_to_ids.keys())
        if not candidates:
            return []

        # Get fuzzy matches using process.extract
        # score_cutoff is a percentage (0-100), threshold is 0.0-1.0
        matches = process.extract(
            name,
            candidates,
            scorer=fuzz.token_set_ratio,
            score_cutoff=threshold * 100,
            limit=None,  # Return all matches above threshold
        )

        results: list[tuple[Artifact, float]] = []
        seen_ids: set[str] = set()

        for matched_name, score, _ in matches:
            # Convert score from 0-100 to 0.0-1.0
            confidence = score / 100.0

            # Add all artifacts with this canonical name
            for artifact_id in name_to_ids[matched_name]:
                if artifact_id not in seen_ids:
                    artifact = self._artifacts[artifact_id]
                    results.append((artifact, confidence))
                    seen_ids.add(artifact_id)

        # Sort by confidence descending
        results.sort(key=lambda x: x[1], reverse=True)

        return results

    def get_by_type(self, artifact_type: ArtifactType) -> list[Artifact]:
        """
        Get all artifacts of a given type.

        Args:
            artifact_type: The type of artifacts to retrieve.

        Returns:
            List of all artifacts of the specified type.
        """
        artifact_ids = self._by_type.get(artifact_type, [])
        return [self._artifacts[aid] for aid in artifact_ids]

    def get_all(self) -> list[Artifact]:
        """
        Get all registered artifacts.

        Returns:
            List of all artifacts in the registry.
        """
        return list(self._artifacts.values())

    def size(self) -> int:
        """
        Get the number of registered artifacts.

        Returns:
            Number of artifacts in the registry.
        """
        return len(self._artifacts)

    def __len__(self) -> int:
        """Return the number of artifacts (supports len())."""
        return self.size()

    def __contains__(self, artifact_id: str) -> bool:
        """Check if an artifact ID is in the registry (supports 'in' operator)."""
        return artifact_id in self._artifacts

    def __iter__(self) -> Iterator[Artifact]:
        """Iterate over all artifacts."""
        return iter(self._artifacts.values())

    def clear(self) -> None:
        """Remove all artifacts and reset indices."""
        self._artifacts.clear()
        self._by_name.clear()
        self._by_type.clear()
        self._aliases.clear()
        logger.debug("Registry cleared")

    def get_aliases_for(self, artifact_id: str) -> list[tuple[str, float]]:
        """
        Get all aliases pointing to a specific artifact.

        Args:
            artifact_id: The artifact ID to find aliases for.

        Returns:
            List of (alias, confidence) tuples for aliases pointing to this artifact.
        """
        return [
            (alias, confidence)
            for alias, (aid, confidence) in self._aliases.items()
            if aid == artifact_id
        ]

    def get_statistics(self) -> dict[str, int]:
        """
        Get registry statistics.

        Returns:
            Dictionary with counts for artifacts, types, names, and aliases.
        """
        return {
            "total_artifacts": len(self._artifacts),
            "unique_names": len(self._by_name),
            "artifact_types": len(self._by_type),
            "registered_aliases": len(self._aliases),
        }
