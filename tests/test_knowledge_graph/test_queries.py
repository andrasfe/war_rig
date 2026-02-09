"""Unit tests for GraphQueryHelper.

Tests:
- get_called_programs: forward CALLS traversal
- get_calling_programs: reverse CALLS traversal
- get_datasets_for_program: READS and WRITES filtering
- get_copybooks_for_program: INCLUDES traversal
- get_programs_sharing_copybook: reverse INCLUDES traversal
- get_jcl_context_for_program: 2-hop JCL traversal
- check_convergence: delta threshold comparison
- get_unconfirmed_triples: filtering by confirmed flag
"""

from unittest.mock import AsyncMock, MagicMock

import pytest

from war_rig.knowledge_graph.models import (
    Entity,
    EntityType,
    RelationType,
    Triple,
    TripleDelta,
)
from war_rig.knowledge_graph.queries import GraphQueryHelper


def _make_entity(
    entity_id: int,
    entity_type: EntityType,
    name: str,
) -> Entity:
    """Create an Entity with minimal required fields."""
    return Entity(id=entity_id, entity_type=entity_type, name=name)


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


@pytest.fixture()
def mock_store() -> MagicMock:
    """Create a mock KnowledgeGraphStore with async methods."""
    store = MagicMock()
    store.get_entity = AsyncMock(return_value=None)
    store.get_entity_by_id = AsyncMock(return_value=None)
    store.get_triples_for_entity = AsyncMock(return_value=[])
    store.get_all_triples = AsyncMock(return_value=[])
    store.compute_delta = AsyncMock()
    return store


@pytest.fixture()
def helper(mock_store: MagicMock) -> GraphQueryHelper:
    """Create a GraphQueryHelper with a mock store."""
    return GraphQueryHelper(mock_store)


class TestGetCalledPrograms:
    """Tests for get_called_programs."""

    async def test_program_not_found(
        self, helper: GraphQueryHelper, mock_store: MagicMock
    ):
        """Returns empty list when program does not exist."""
        mock_store.get_entity.return_value = None
        result = await helper.get_called_programs("NONEXISTENT")
        assert result == []

    async def test_no_calls(self, helper: GraphQueryHelper, mock_store: MagicMock):
        """Returns empty list when program has no CALLS triples."""
        program = _make_entity(1, EntityType.PROGRAM, "ACCT0100")
        mock_store.get_entity.return_value = program
        mock_store.get_triples_for_entity.return_value = []

        result = await helper.get_called_programs("ACCT0100")
        assert result == []

    async def test_returns_called_programs(
        self, helper: GraphQueryHelper, mock_store: MagicMock
    ):
        """Returns entities for CALLS triples where program is subject."""
        program = _make_entity(1, EntityType.PROGRAM, "ACCT0100")
        called = _make_entity(2, EntityType.PROGRAM, "ACCT0200")
        triple = _make_triple(10, subject_id=1, predicate=RelationType.CALLS, object_id=2)

        mock_store.get_entity.return_value = program
        mock_store.get_triples_for_entity.return_value = [triple]
        mock_store.get_entity_by_id.return_value = called

        result = await helper.get_called_programs("ACCT0100")

        assert len(result) == 1
        assert result[0].name == "ACCT0200"
        mock_store.get_triples_for_entity.assert_called_once_with(
            1, as_subject=True, as_object=False
        )

    async def test_filters_non_calls_triples(
        self, helper: GraphQueryHelper, mock_store: MagicMock
    ):
        """Only returns entities from CALLS triples, not READS etc."""
        program = _make_entity(1, EntityType.PROGRAM, "ACCT0100")
        calls_triple = _make_triple(
            10, subject_id=1, predicate=RelationType.CALLS, object_id=2
        )
        reads_triple = _make_triple(
            11, subject_id=1, predicate=RelationType.READS, object_id=3
        )
        called = _make_entity(2, EntityType.PROGRAM, "ACCT0200")

        mock_store.get_entity.return_value = program
        mock_store.get_triples_for_entity.return_value = [calls_triple, reads_triple]
        mock_store.get_entity_by_id.return_value = called

        result = await helper.get_called_programs("ACCT0100")

        assert len(result) == 1
        # get_entity_by_id called only for the CALLS triple
        mock_store.get_entity_by_id.assert_called_once_with(2)


class TestGetCallingPrograms:
    """Tests for get_calling_programs."""

    async def test_program_not_found(
        self, helper: GraphQueryHelper, mock_store: MagicMock
    ):
        """Returns empty list when program does not exist."""
        mock_store.get_entity.return_value = None
        result = await helper.get_calling_programs("NONEXISTENT")
        assert result == []

    async def test_returns_calling_programs(
        self, helper: GraphQueryHelper, mock_store: MagicMock
    ):
        """Returns entities that call the given program."""
        called_program = _make_entity(2, EntityType.PROGRAM, "ACCT0200")
        caller = _make_entity(1, EntityType.PROGRAM, "ACCT0100")
        triple = _make_triple(
            10, subject_id=1, predicate=RelationType.CALLS, object_id=2
        )

        mock_store.get_entity.return_value = called_program
        mock_store.get_triples_for_entity.return_value = [triple]
        mock_store.get_entity_by_id.return_value = caller

        result = await helper.get_calling_programs("ACCT0200")

        assert len(result) == 1
        assert result[0].name == "ACCT0100"
        mock_store.get_triples_for_entity.assert_called_once_with(
            2, as_subject=False, as_object=True
        )


class TestGetDatasetsForProgram:
    """Tests for get_datasets_for_program."""

    async def test_program_not_found(
        self, helper: GraphQueryHelper, mock_store: MagicMock
    ):
        """Returns empty dict structure when program does not exist."""
        mock_store.get_entity.return_value = None
        result = await helper.get_datasets_for_program("NONEXISTENT")
        assert result == {"reads": [], "writes": []}

    async def test_separates_reads_and_writes(
        self, helper: GraphQueryHelper, mock_store: MagicMock
    ):
        """Correctly separates datasets into reads and writes lists."""
        program = _make_entity(1, EntityType.PROGRAM, "ACCT0100")
        ds_read = _make_entity(10, EntityType.DATASET, "INPUT.FILE")
        ds_write = _make_entity(11, EntityType.DATASET, "OUTPUT.FILE")

        reads_triple = _make_triple(
            100, subject_id=1, predicate=RelationType.READS, object_id=10
        )
        writes_triple = _make_triple(
            101, subject_id=1, predicate=RelationType.WRITES, object_id=11
        )

        mock_store.get_entity.return_value = program
        mock_store.get_triples_for_entity.return_value = [reads_triple, writes_triple]
        mock_store.get_entity_by_id.side_effect = lambda eid: {
            10: ds_read,
            11: ds_write,
        }.get(eid)

        result = await helper.get_datasets_for_program("ACCT0100")

        assert len(result["reads"]) == 1
        assert result["reads"][0].name == "INPUT.FILE"
        assert len(result["writes"]) == 1
        assert result["writes"][0].name == "OUTPUT.FILE"

    async def test_ignores_non_dataset_triples(
        self, helper: GraphQueryHelper, mock_store: MagicMock
    ):
        """Only picks up READS and WRITES predicates."""
        program = _make_entity(1, EntityType.PROGRAM, "ACCT0100")
        calls_triple = _make_triple(
            100, subject_id=1, predicate=RelationType.CALLS, object_id=2
        )

        mock_store.get_entity.return_value = program
        mock_store.get_triples_for_entity.return_value = [calls_triple]

        result = await helper.get_datasets_for_program("ACCT0100")

        assert result == {"reads": [], "writes": []}


class TestGetCopybooksForProgram:
    """Tests for get_copybooks_for_program."""

    async def test_program_not_found(
        self, helper: GraphQueryHelper, mock_store: MagicMock
    ):
        """Returns empty list when program does not exist."""
        mock_store.get_entity.return_value = None
        result = await helper.get_copybooks_for_program("NONEXISTENT")
        assert result == []

    async def test_returns_included_copybooks(
        self, helper: GraphQueryHelper, mock_store: MagicMock
    ):
        """Returns copybook entities linked via INCLUDES."""
        program = _make_entity(1, EntityType.PROGRAM, "ACCT0100")
        copybook = _make_entity(5, EntityType.COPYBOOK, "ACCTCPY")
        triple = _make_triple(
            100, subject_id=1, predicate=RelationType.INCLUDES, object_id=5
        )

        mock_store.get_entity.return_value = program
        mock_store.get_triples_for_entity.return_value = [triple]
        mock_store.get_entity_by_id.return_value = copybook

        result = await helper.get_copybooks_for_program("ACCT0100")

        assert len(result) == 1
        assert result[0].name == "ACCTCPY"


class TestGetProgramsSharingCopybook:
    """Tests for get_programs_sharing_copybook."""

    async def test_copybook_not_found(
        self, helper: GraphQueryHelper, mock_store: MagicMock
    ):
        """Returns empty list when copybook does not exist."""
        mock_store.get_entity.return_value = None
        result = await helper.get_programs_sharing_copybook("NONEXISTENT")
        assert result == []

    async def test_returns_programs_including_copybook(
        self, helper: GraphQueryHelper, mock_store: MagicMock
    ):
        """Returns programs that include the copybook."""
        copybook = _make_entity(5, EntityType.COPYBOOK, "ACCTCPY")
        program = _make_entity(1, EntityType.PROGRAM, "ACCT0100")
        triple = _make_triple(
            100, subject_id=1, predicate=RelationType.INCLUDES, object_id=5
        )

        mock_store.get_entity.return_value = copybook
        mock_store.get_triples_for_entity.return_value = [triple]
        mock_store.get_entity_by_id.return_value = program

        result = await helper.get_programs_sharing_copybook("ACCTCPY")

        assert len(result) == 1
        assert result[0].name == "ACCT0100"
        mock_store.get_triples_for_entity.assert_called_once_with(
            5, as_subject=False, as_object=True
        )


class TestGetJclContextForProgram:
    """Tests for get_jcl_context_for_program."""

    async def test_program_not_found(
        self, helper: GraphQueryHelper, mock_store: MagicMock
    ):
        """Returns empty dict structure when program does not exist."""
        mock_store.get_entity.return_value = None
        result = await helper.get_jcl_context_for_program("NONEXISTENT")
        assert result == {"steps": [], "jobs": [], "co_programs": []}

    async def test_full_2hop_traversal(
        self, helper: GraphQueryHelper, mock_store: MagicMock
    ):
        """Tests complete 2-hop JCL traversal."""
        program = _make_entity(1, EntityType.PROGRAM, "ACCT0100")
        co_program = _make_entity(2, EntityType.PROGRAM, "ACCT0200")
        step1 = _make_entity(10, EntityType.JCL_STEP, "STEP01")
        step2 = _make_entity(11, EntityType.JCL_STEP, "STEP02")
        job = _make_entity(20, EntityType.JCL_JOB, "ACCTJOB")

        # Triples: step1 EXECUTES program, step2 EXECUTES co_program
        exec_triple_1 = _make_triple(
            100, subject_id=10, predicate=RelationType.EXECUTES, object_id=1
        )
        exec_triple_2 = _make_triple(
            101, subject_id=11, predicate=RelationType.EXECUTES, object_id=2
        )
        # Triples: job CONTAINS_STEP step1, job CONTAINS_STEP step2
        contains_1 = _make_triple(
            200, subject_id=20, predicate=RelationType.CONTAINS_STEP, object_id=10
        )
        contains_2 = _make_triple(
            201, subject_id=20, predicate=RelationType.CONTAINS_STEP, object_id=11
        )

        mock_store.get_entity.return_value = program

        # Call sequence:
        # 1. get_triples_for_entity(1, as_subject=False, as_object=True) -> exec_triple_1
        # 2. get_triples_for_entity(10, as_subject=False, as_object=True) -> contains_1
        # 3. get_triples_for_entity(20, as_subject=True, as_object=False) -> contains_1, contains_2
        # 4. get_triples_for_entity(10, as_subject=True, as_object=False) -> exec_triple_1
        # 5. get_triples_for_entity(11, as_subject=True, as_object=False) -> exec_triple_2
        mock_store.get_triples_for_entity.side_effect = [
            [exec_triple_1],                  # step1 EXECUTES program
            [contains_1],                      # job CONTAINS_STEP step1
            [contains_1, contains_2],          # job's steps
            [exec_triple_1],                   # step1 executes program (self, excluded)
            [exec_triple_2],                   # step2 executes co_program
        ]

        mock_store.get_entity_by_id.side_effect = lambda eid: {
            1: program,
            2: co_program,
            10: step1,
            11: step2,
            20: job,
        }.get(eid)

        result = await helper.get_jcl_context_for_program("ACCT0100")

        assert len(result["steps"]) == 1
        assert result["steps"][0].name == "STEP01"
        assert len(result["jobs"]) == 1
        assert result["jobs"][0].name == "ACCTJOB"
        assert len(result["co_programs"]) == 1
        assert result["co_programs"][0].name == "ACCT0200"

    async def test_no_steps_execute_program(
        self, helper: GraphQueryHelper, mock_store: MagicMock
    ):
        """Returns empty results when no steps execute the program."""
        program = _make_entity(1, EntityType.PROGRAM, "ACCT0100")
        mock_store.get_entity.return_value = program
        mock_store.get_triples_for_entity.return_value = []

        result = await helper.get_jcl_context_for_program("ACCT0100")

        assert result == {"steps": [], "jobs": [], "co_programs": []}

    async def test_deduplicates_jobs(
        self, helper: GraphQueryHelper, mock_store: MagicMock
    ):
        """Does not add the same job twice if multiple steps point to it."""
        program = _make_entity(1, EntityType.PROGRAM, "ACCT0100")
        step1 = _make_entity(10, EntityType.JCL_STEP, "STEP01")
        step2 = _make_entity(11, EntityType.JCL_STEP, "STEP02")
        job = _make_entity(20, EntityType.JCL_JOB, "ACCTJOB")

        exec_1 = _make_triple(
            100, subject_id=10, predicate=RelationType.EXECUTES, object_id=1
        )
        exec_2 = _make_triple(
            101, subject_id=11, predicate=RelationType.EXECUTES, object_id=1
        )
        contains_1 = _make_triple(
            200, subject_id=20, predicate=RelationType.CONTAINS_STEP, object_id=10
        )
        contains_2 = _make_triple(
            201, subject_id=20, predicate=RelationType.CONTAINS_STEP, object_id=11
        )

        mock_store.get_entity.return_value = program

        # Both steps execute the program
        # Both steps are contained by the same job
        mock_store.get_triples_for_entity.side_effect = [
            [exec_1, exec_2],                  # program as object -> both steps execute it
            [contains_1],                       # step1 as object -> job contains step1
            [contains_2],                       # step2 as object -> job contains step2 (same job)
            [contains_1, contains_2],           # job's steps (for the one unique job)
            [exec_1],                           # step1 executes program (self, excluded)
            [exec_2],                           # step2 executes program (self, excluded)
        ]

        mock_store.get_entity_by_id.side_effect = lambda eid: {
            1: program,
            10: step1,
            11: step2,
            20: job,
        }.get(eid)

        result = await helper.get_jcl_context_for_program("ACCT0100")

        # Job should appear only once
        assert len(result["jobs"]) == 1
        assert result["jobs"][0].name == "ACCTJOB"


class TestCheckConvergence:
    """Tests for check_convergence."""

    async def test_converged(self, helper: GraphQueryHelper, mock_store: MagicMock):
        """Returns True when change rate is below threshold."""
        delta = TripleDelta(
            pass_from="pass_1",
            pass_to="pass_2",
            added=[],
            removed=[],
            modified=[],
            total_triples=100,
        )
        mock_store.compute_delta.return_value = delta

        result = await helper.check_convergence("pass_1", "pass_2", threshold=0.05)

        assert result is True
        mock_store.compute_delta.assert_called_once_with("pass_1", "pass_2")

    async def test_not_converged(
        self, helper: GraphQueryHelper, mock_store: MagicMock
    ):
        """Returns False when change rate is at or above threshold."""
        # 10 added out of 100 total = 10% > 5%
        added_triples = [
            _make_triple(i, subject_id=1, predicate=RelationType.CALLS, object_id=2)
            for i in range(10)
        ]
        delta = TripleDelta(
            pass_from="pass_1",
            pass_to="pass_2",
            added=added_triples,
            removed=[],
            modified=[],
            total_triples=100,
        )
        mock_store.compute_delta.return_value = delta

        result = await helper.check_convergence("pass_1", "pass_2", threshold=0.05)

        assert result is False

    async def test_custom_threshold(
        self, helper: GraphQueryHelper, mock_store: MagicMock
    ):
        """Respects custom threshold parameter."""
        added_triples = [
            _make_triple(i, subject_id=1, predicate=RelationType.CALLS, object_id=2)
            for i in range(10)
        ]
        delta = TripleDelta(
            pass_from="pass_1",
            pass_to="pass_2",
            added=added_triples,
            removed=[],
            modified=[],
            total_triples=100,
        )
        mock_store.compute_delta.return_value = delta

        # 10% change rate, 15% threshold -> converged
        result = await helper.check_convergence("pass_1", "pass_2", threshold=0.15)
        assert result is True

    async def test_empty_graph(self, helper: GraphQueryHelper, mock_store: MagicMock):
        """Empty graph (0 total triples) has 0.0 change rate -> converged."""
        delta = TripleDelta(
            pass_from="pass_1",
            pass_to="pass_2",
            added=[],
            removed=[],
            modified=[],
            total_triples=0,
        )
        mock_store.compute_delta.return_value = delta

        result = await helper.check_convergence("pass_1", "pass_2")
        assert result is True


class TestGetUnconfirmedTriples:
    """Tests for get_unconfirmed_triples."""

    async def test_no_triples(self, helper: GraphQueryHelper, mock_store: MagicMock):
        """Returns empty list when there are no triples."""
        mock_store.get_all_triples.return_value = []
        result = await helper.get_unconfirmed_triples()
        assert result == []

    async def test_filters_confirmed(
        self, helper: GraphQueryHelper, mock_store: MagicMock
    ):
        """Only returns triples where confirmed is False."""
        confirmed_triple = Triple(
            id=1,
            subject_id=1,
            predicate=RelationType.CALLS,
            object_id=2,
            confirmed=True,
        )
        unconfirmed_triple = Triple(
            id=2,
            subject_id=3,
            predicate=RelationType.READS,
            object_id=4,
            confirmed=False,
        )
        mock_store.get_all_triples.return_value = [
            confirmed_triple,
            unconfirmed_triple,
        ]

        result = await helper.get_unconfirmed_triples()

        assert len(result) == 1
        assert result[0].id == 2
        assert result[0].confirmed is False

    async def test_all_unconfirmed(
        self, helper: GraphQueryHelper, mock_store: MagicMock
    ):
        """Returns all triples when none are confirmed."""
        triples = [
            Triple(
                id=i,
                subject_id=1,
                predicate=RelationType.CALLS,
                object_id=2,
                confirmed=False,
            )
            for i in range(3)
        ]
        mock_store.get_all_triples.return_value = triples

        result = await helper.get_unconfirmed_triples()
        assert len(result) == 3
