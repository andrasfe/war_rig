"""Conflict detection for the knowledge graph.

This module detects contradictory triples in the graph. A conflict occurs
when two Scribes emit incompatible claims about the same relationship
(per spec Section 8.2), for example:

- Scribe-A says PROGRAM:X READS DATASET:Y
- Scribe-B says PROGRAM:X WRITES DATASET:Y

Conflicting triples are recorded in the conflicts table with both triples
marked as unresolved. The next pass's Scribe receives both claims as context
and is instructed to determine which is correct.

Example:
    detector = ConflictDetector(store)
    new_conflicts = await detector.detect_conflicts(newly_ingested_triples)
"""

import logging

from war_rig.knowledge_graph.models import Conflict, RelationType, Triple
from war_rig.knowledge_graph.store import KnowledgeGraphStore

logger = logging.getLogger(__name__)

# Relationship pairs that are mutually exclusive for the same entity pair
_CONFLICTING_PREDICATES: list[tuple[RelationType, RelationType]] = [
    (RelationType.READS, RelationType.WRITES),
    (RelationType.DEFINES_INPUT, RelationType.DEFINES_OUTPUT),
    (RelationType.QUERIES, RelationType.MODIFIES),
]


class ConflictDetector:
    """Detects contradictory triples in the knowledge graph.

    Checks newly ingested triples against existing graph data for
    incompatible relationship claims between the same entity pairs.

    Args:
        store: The knowledge graph store to query.
    """

    def __init__(self, store: KnowledgeGraphStore) -> None:
        """Initialize the conflict detector.

        Args:
            store: Knowledge graph store for querying existing triples.
        """
        self._store = store

    async def detect_conflicts(
        self,
        new_triples: list[Triple],
    ) -> list[Conflict]:
        """Check new triples for conflicts with existing graph data.

        For each new triple, checks if a contradictory triple exists
        (same subject-object pair with a conflicting predicate).

        Args:
            new_triples: Recently ingested triples to check.

        Returns:
            List of newly detected Conflict records (already persisted).
        """
        conflicts: list[Conflict] = []
        for triple in new_triples:
            existing_triples = await self._store.get_triples_for_entity(
                triple.subject_id, as_subject=True, as_object=False
            )
            for existing in existing_triples:
                if (
                    existing.object_id == triple.object_id
                    and existing.predicate != triple.predicate
                    and self._are_conflicting(existing.predicate, triple.predicate)
                ):
                    conflict = await self._store.record_conflict(
                        existing.id, triple.id  # type: ignore[arg-type]
                    )
                    conflicts.append(conflict)
                    logger.warning(
                        "Conflict detected: triple %s (%s) vs triple %s (%s) "
                        "for subject_id=%d, object_id=%d",
                        existing.id,
                        existing.predicate.value,
                        triple.id,
                        triple.predicate.value,
                        triple.subject_id,
                        triple.object_id,
                    )
        return conflicts

    def _are_conflicting(
        self,
        predicate_a: RelationType,
        predicate_b: RelationType,
    ) -> bool:
        """Check if two predicates are mutually exclusive.

        Args:
            predicate_a: First relationship type.
            predicate_b: Second relationship type.

        Returns:
            True if the predicates conflict.
        """
        if predicate_a == predicate_b:
            return False
        for pair_a, pair_b in _CONFLICTING_PREDICATES:
            if (predicate_a == pair_a and predicate_b == pair_b) or (
                predicate_a == pair_b and predicate_b == pair_a
            ):
                return True
        return False
