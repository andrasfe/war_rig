"""Unit tests for ConflictDetector.

Tests:
- _are_conflicting predicate pair checking
- detect_conflicts with no conflicts
- detect_conflicts with READS/WRITES conflict
- detect_conflicts with DEFINES_INPUT/DEFINES_OUTPUT conflict
- detect_conflicts with QUERIES/MODIFIES conflict
- detect_conflicts skips same-predicate corroboration
- detect_conflicts skips non-conflicting different predicates
- detect_conflicts handles multiple new triples
"""

from unittest.mock import AsyncMock, MagicMock

import pytest

from war_rig.knowledge_graph.conflicts import ConflictDetector
from war_rig.knowledge_graph.models import Conflict, RelationType, Triple


def _make_triple(
    triple_id: int,
    subject_id: int,
    predicate: RelationType,
    object_id: int,
) -> Triple:
    """Create a Triple with minimal required fields."""
    return Triple(
        id=triple_id,
        subject_id=subject_id,
        predicate=predicate,
        object_id=object_id,
        source_pass="pass_1",
    )


def _make_conflict(
    conflict_id: int,
    triple_a_id: int,
    triple_b_id: int,
) -> Conflict:
    """Create a Conflict with minimal required fields."""
    return Conflict(
        id=conflict_id,
        triple_a_id=triple_a_id,
        triple_b_id=triple_b_id,
        resolved=False,
    )


@pytest.fixture()
def mock_store() -> MagicMock:
    """Create a mock KnowledgeGraphStore with async methods."""
    store = MagicMock()
    store.get_triples_for_entity = AsyncMock(return_value=[])
    store.record_conflict = AsyncMock()
    return store


@pytest.fixture()
def detector(mock_store: MagicMock) -> ConflictDetector:
    """Create a ConflictDetector with a mock store."""
    return ConflictDetector(mock_store)


class TestAreConflicting:
    """Tests for _are_conflicting predicate pair checking."""

    def test_same_predicate_not_conflicting(self, detector: ConflictDetector):
        """Same predicate is corroboration, not conflict."""
        assert detector._are_conflicting(RelationType.READS, RelationType.READS) is False
        assert detector._are_conflicting(RelationType.WRITES, RelationType.WRITES) is False

    def test_reads_writes_conflict(self, detector: ConflictDetector):
        """READS and WRITES are conflicting."""
        assert detector._are_conflicting(RelationType.READS, RelationType.WRITES) is True
        assert detector._are_conflicting(RelationType.WRITES, RelationType.READS) is True

    def test_defines_input_output_conflict(self, detector: ConflictDetector):
        """DEFINES_INPUT and DEFINES_OUTPUT are conflicting."""
        assert (
            detector._are_conflicting(
                RelationType.DEFINES_INPUT, RelationType.DEFINES_OUTPUT
            )
            is True
        )
        assert (
            detector._are_conflicting(
                RelationType.DEFINES_OUTPUT, RelationType.DEFINES_INPUT
            )
            is True
        )

    def test_queries_modifies_conflict(self, detector: ConflictDetector):
        """QUERIES and MODIFIES are conflicting."""
        assert (
            detector._are_conflicting(RelationType.QUERIES, RelationType.MODIFIES)
            is True
        )
        assert (
            detector._are_conflicting(RelationType.MODIFIES, RelationType.QUERIES)
            is True
        )

    def test_non_conflicting_different_predicates(self, detector: ConflictDetector):
        """Different predicates that are not in the conflict list."""
        assert (
            detector._are_conflicting(RelationType.CALLS, RelationType.READS) is False
        )
        assert (
            detector._are_conflicting(RelationType.INCLUDES, RelationType.EXECUTES)
            is False
        )
        assert (
            detector._are_conflicting(RelationType.READS, RelationType.CALLS) is False
        )


class TestDetectConflicts:
    """Tests for detect_conflicts method."""

    async def test_no_new_triples(
        self, detector: ConflictDetector, mock_store: MagicMock
    ):
        """Empty input returns empty list."""
        result = await detector.detect_conflicts([])
        assert result == []
        mock_store.get_triples_for_entity.assert_not_called()

    async def test_no_existing_triples(
        self, detector: ConflictDetector, mock_store: MagicMock
    ):
        """No existing triples means no conflicts."""
        new_triple = _make_triple(10, subject_id=1, predicate=RelationType.READS, object_id=2)
        mock_store.get_triples_for_entity.return_value = []

        result = await detector.detect_conflicts([new_triple])

        assert result == []
        mock_store.get_triples_for_entity.assert_called_once_with(
            1, as_subject=True, as_object=False
        )
        mock_store.record_conflict.assert_not_called()

    async def test_same_predicate_no_conflict(
        self, detector: ConflictDetector, mock_store: MagicMock
    ):
        """Same predicate on same entity pair is corroboration, not conflict."""
        existing = _make_triple(1, subject_id=1, predicate=RelationType.READS, object_id=2)
        new_triple = _make_triple(10, subject_id=1, predicate=RelationType.READS, object_id=2)
        mock_store.get_triples_for_entity.return_value = [existing]

        result = await detector.detect_conflicts([new_triple])

        assert result == []
        mock_store.record_conflict.assert_not_called()

    async def test_different_object_no_conflict(
        self, detector: ConflictDetector, mock_store: MagicMock
    ):
        """Different object_id means no conflict even with conflicting predicates."""
        existing = _make_triple(1, subject_id=1, predicate=RelationType.READS, object_id=2)
        new_triple = _make_triple(10, subject_id=1, predicate=RelationType.WRITES, object_id=3)
        mock_store.get_triples_for_entity.return_value = [existing]

        result = await detector.detect_conflicts([new_triple])

        assert result == []
        mock_store.record_conflict.assert_not_called()

    async def test_reads_writes_conflict_detected(
        self, detector: ConflictDetector, mock_store: MagicMock
    ):
        """READS vs WRITES on same subject-object pair creates a conflict."""
        existing = _make_triple(1, subject_id=1, predicate=RelationType.READS, object_id=2)
        new_triple = _make_triple(10, subject_id=1, predicate=RelationType.WRITES, object_id=2)
        conflict = _make_conflict(100, triple_a_id=1, triple_b_id=10)

        mock_store.get_triples_for_entity.return_value = [existing]
        mock_store.record_conflict.return_value = conflict

        result = await detector.detect_conflicts([new_triple])

        assert len(result) == 1
        assert result[0].triple_a_id == 1
        assert result[0].triple_b_id == 10
        mock_store.record_conflict.assert_called_once_with(1, 10)

    async def test_queries_modifies_conflict_detected(
        self, detector: ConflictDetector, mock_store: MagicMock
    ):
        """QUERIES vs MODIFIES on same subject-object pair creates a conflict."""
        existing = _make_triple(5, subject_id=3, predicate=RelationType.QUERIES, object_id=4)
        new_triple = _make_triple(15, subject_id=3, predicate=RelationType.MODIFIES, object_id=4)
        conflict = _make_conflict(200, triple_a_id=5, triple_b_id=15)

        mock_store.get_triples_for_entity.return_value = [existing]
        mock_store.record_conflict.return_value = conflict

        result = await detector.detect_conflicts([new_triple])

        assert len(result) == 1
        assert result[0].triple_a_id == 5
        assert result[0].triple_b_id == 15

    async def test_non_conflicting_predicates_no_conflict(
        self, detector: ConflictDetector, mock_store: MagicMock
    ):
        """Different but non-conflicting predicates on same entity pair."""
        existing = _make_triple(1, subject_id=1, predicate=RelationType.CALLS, object_id=2)
        new_triple = _make_triple(10, subject_id=1, predicate=RelationType.READS, object_id=2)
        mock_store.get_triples_for_entity.return_value = [existing]

        result = await detector.detect_conflicts([new_triple])

        assert result == []
        mock_store.record_conflict.assert_not_called()

    async def test_multiple_new_triples_multiple_conflicts(
        self, detector: ConflictDetector, mock_store: MagicMock
    ):
        """Multiple new triples can each produce conflicts."""
        existing_1 = _make_triple(1, subject_id=1, predicate=RelationType.READS, object_id=2)
        existing_2 = _make_triple(2, subject_id=3, predicate=RelationType.QUERIES, object_id=4)

        new_triple_1 = _make_triple(10, subject_id=1, predicate=RelationType.WRITES, object_id=2)
        new_triple_2 = _make_triple(11, subject_id=3, predicate=RelationType.MODIFIES, object_id=4)

        conflict_1 = _make_conflict(100, triple_a_id=1, triple_b_id=10)
        conflict_2 = _make_conflict(101, triple_a_id=2, triple_b_id=11)

        mock_store.get_triples_for_entity.side_effect = [
            [existing_1],  # For new_triple_1's subject
            [existing_2],  # For new_triple_2's subject
        ]
        mock_store.record_conflict.side_effect = [conflict_1, conflict_2]

        result = await detector.detect_conflicts([new_triple_1, new_triple_2])

        assert len(result) == 2
        assert mock_store.record_conflict.call_count == 2
