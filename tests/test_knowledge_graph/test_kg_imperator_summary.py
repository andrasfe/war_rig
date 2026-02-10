"""Tests for Knowledge Graph Imperator summary (Layer 1).

Tests cover:
- GraphQueryHelper.get_entity_breakdown()
- GraphQueryHelper.get_confirmation_stats()
- KnowledgeGraphManager.get_imperator_summary()
- KnowledgeGraphManager._extract_pass_number()
- Imperator prompt injection for both full and compact holistic review
"""

from unittest.mock import AsyncMock, MagicMock

import pytest

from war_rig.knowledge_graph.models import (
    Entity,
    EntityType,
    RelationType,
    Triple,
)
from war_rig.knowledge_graph.queries import GraphQueryHelper

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


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
    confirmed: bool = False,
    corroboration_count: int = 1,
) -> Triple:
    """Create a Triple with configurable confirmation state."""
    return Triple(
        id=triple_id,
        subject_id=subject_id,
        predicate=predicate,
        object_id=object_id,
        source_pass="pass_1",
        confirmed=confirmed,
        corroboration_count=corroboration_count,
    )


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture()
def mock_store() -> MagicMock:
    """Create a mock KnowledgeGraphStore with async methods."""
    store = MagicMock()
    store.get_all_entities = AsyncMock(return_value=[])
    store.get_all_triples = AsyncMock(return_value=[])
    store.get_unresolved_conflicts = AsyncMock(return_value=[])
    store.compute_delta = AsyncMock()
    store.get_triple_count = AsyncMock(return_value=0)
    return store


@pytest.fixture()
def helper(mock_store: MagicMock) -> GraphQueryHelper:
    """Create a GraphQueryHelper with a mock store."""
    return GraphQueryHelper(mock_store)


# ===========================================================================
# Tests for GraphQueryHelper.get_entity_breakdown
# ===========================================================================


class TestGetEntityBreakdown:
    """Tests for get_entity_breakdown."""

    async def test_empty_graph(
        self, helper: GraphQueryHelper, mock_store: MagicMock
    ):
        """Returns empty dict when graph has no entities."""
        mock_store.get_all_entities.return_value = []
        result = await helper.get_entity_breakdown()
        assert result == {}

    async def test_single_type(
        self, helper: GraphQueryHelper, mock_store: MagicMock
    ):
        """Returns correct count for single entity type."""
        mock_store.get_all_entities.return_value = [
            _make_entity(1, EntityType.PROGRAM, "ACCT0100"),
            _make_entity(2, EntityType.PROGRAM, "ACCT0200"),
        ]
        result = await helper.get_entity_breakdown()
        assert result == {"PROGRAM": 2}

    async def test_multiple_types(
        self, helper: GraphQueryHelper, mock_store: MagicMock
    ):
        """Returns correct counts across multiple entity types."""
        mock_store.get_all_entities.return_value = [
            _make_entity(1, EntityType.PROGRAM, "ACCT0100"),
            _make_entity(2, EntityType.PROGRAM, "ACCT0200"),
            _make_entity(3, EntityType.DATASET, "ACCT.MASTER"),
            _make_entity(4, EntityType.COPYBOOK, "ACCTCPY"),
            _make_entity(5, EntityType.COPYBOOK, "ERRORCPY"),
            _make_entity(6, EntityType.COPYBOOK, "COMMCPY"),
        ]
        result = await helper.get_entity_breakdown()
        assert result == {"PROGRAM": 2, "DATASET": 1, "COPYBOOK": 3}


# ===========================================================================
# Tests for GraphQueryHelper.get_confirmation_stats
# ===========================================================================


class TestGetConfirmationStats:
    """Tests for get_confirmation_stats."""

    async def test_empty_graph(
        self, helper: GraphQueryHelper, mock_store: MagicMock
    ):
        """Returns zero stats for empty graph."""
        mock_store.get_all_triples.return_value = []
        result = await helper.get_confirmation_stats()
        assert result["confirmed_count"] == 0
        assert result["total"] == 0
        assert result["confirmation_rate"] == 0.0
        assert result["avg_corroboration"] == 0.0

    async def test_all_confirmed(
        self, helper: GraphQueryHelper, mock_store: MagicMock
    ):
        """100% confirmation rate when all triples confirmed."""
        mock_store.get_all_triples.return_value = [
            _make_triple(1, 1, RelationType.CALLS, 2, confirmed=True, corroboration_count=2),
            _make_triple(2, 1, RelationType.READS, 3, confirmed=True, corroboration_count=3),
        ]
        result = await helper.get_confirmation_stats()
        assert result["confirmed_count"] == 2
        assert result["total"] == 2
        assert result["confirmation_rate"] == 1.0
        assert result["avg_corroboration"] == 2.5

    async def test_partial_confirmation(
        self, helper: GraphQueryHelper, mock_store: MagicMock
    ):
        """Correct rate when some triples unconfirmed."""
        mock_store.get_all_triples.return_value = [
            _make_triple(1, 1, RelationType.CALLS, 2, confirmed=True, corroboration_count=2),
            _make_triple(2, 1, RelationType.READS, 3, confirmed=False, corroboration_count=1),
            _make_triple(3, 2, RelationType.WRITES, 3, confirmed=True, corroboration_count=1),
            _make_triple(4, 3, RelationType.INCLUDES, 4, confirmed=False, corroboration_count=1),
        ]
        result = await helper.get_confirmation_stats()
        assert result["confirmed_count"] == 2
        assert result["total"] == 4
        assert result["confirmation_rate"] == 0.5
        # avg corroboration: (2+1+1+1)/4 = 1.25, rounded to 1.2
        assert result["avg_corroboration"] == 1.2

    async def test_no_confirmed(
        self, helper: GraphQueryHelper, mock_store: MagicMock
    ):
        """0% confirmation rate when no triples confirmed."""
        mock_store.get_all_triples.return_value = [
            _make_triple(1, 1, RelationType.CALLS, 2, confirmed=False),
            _make_triple(2, 1, RelationType.READS, 3, confirmed=False),
        ]
        result = await helper.get_confirmation_stats()
        assert result["confirmed_count"] == 0
        assert result["total"] == 2
        assert result["confirmation_rate"] == 0.0
        assert result["avg_corroboration"] == 1.0


# ===========================================================================
# Tests for KnowledgeGraphManager.get_imperator_summary
# ===========================================================================


class TestGetImperatorSummary:
    """Tests for KnowledgeGraphManager.get_imperator_summary."""

    def _make_manager(
        self,
        enabled: bool = True,
    ) -> tuple:
        """Create a manager with mocked internals.

        Returns:
            Tuple of (manager, mock_store, mock_query_helper).
        """
        from war_rig.knowledge_graph.manager import KnowledgeGraphManager

        config = MagicMock()
        config.knowledge_graph_enabled = enabled
        manager = KnowledgeGraphManager(config)

        mock_store = MagicMock()
        mock_store.get_all_entities = AsyncMock(return_value=[])
        mock_store.get_all_triples = AsyncMock(return_value=[])
        mock_store.get_unresolved_conflicts = AsyncMock(return_value=[])
        mock_store.compute_delta = AsyncMock()
        mock_store.get_triple_count = AsyncMock(return_value=0)

        mock_query = MagicMock()
        mock_query.get_entity_breakdown = AsyncMock(return_value={})
        mock_query.get_confirmation_stats = AsyncMock(
            return_value={
                "confirmed_count": 0,
                "total": 0,
                "confirmation_rate": 0.0,
                "avg_corroboration": 0.0,
            }
        )

        manager._store = mock_store
        manager._query_helper = mock_query

        return manager, mock_store, mock_query

    async def test_disabled_returns_empty(self):
        """Returns empty string when KG is disabled."""
        manager, _, _ = self._make_manager(enabled=False)
        result = await manager.get_imperator_summary()
        assert result == ""

    async def test_no_store_returns_empty(self):
        """Returns empty string when store is None."""
        manager, _, _ = self._make_manager(enabled=True)
        manager._store = None
        result = await manager.get_imperator_summary()
        assert result == ""

    async def test_no_query_helper_returns_empty(self):
        """Returns empty string when query helper is None."""
        manager, _, _ = self._make_manager(enabled=True)
        manager._query_helper = None
        result = await manager.get_imperator_summary()
        assert result == ""

    async def test_basic_summary(self):
        """Generates correct summary with entities, triples, and conflicts."""
        manager, mock_store, mock_query = self._make_manager()

        mock_query.get_entity_breakdown.return_value = {
            "PROGRAM": 15,
            "DATASET": 12,
            "COPYBOOK": 10,
            "JCL_JOB": 5,
        }
        mock_query.get_confirmation_stats.return_value = {
            "confirmed_count": 89,
            "total": 127,
            "confirmation_rate": 0.70,
            "avg_corroboration": 1.4,
        }
        mock_store.get_unresolved_conflicts.return_value = [
            MagicMock(),
            MagicMock(),
            MagicMock(),
        ]

        result = await manager.get_imperator_summary()

        assert "## Knowledge Graph Health" in result
        assert "42 entities" in result
        assert "15 programs" in result
        assert "12 datasets" in result
        assert "10 copybooks" in result
        assert "5 jcl_jobs" in result
        assert "127 triples" in result
        assert "70% confirmed" in result
        assert "1.4x" in result
        assert "3 unresolved conflicts" in result

    async def test_summary_without_pass(self):
        """No convergence line when current_pass is None."""
        manager, mock_store, mock_query = self._make_manager()

        mock_query.get_entity_breakdown.return_value = {"PROGRAM": 2}
        mock_query.get_confirmation_stats.return_value = {
            "confirmed_count": 5,
            "total": 10,
            "confirmation_rate": 0.5,
            "avg_corroboration": 1.0,
        }
        mock_store.get_unresolved_conflicts.return_value = []

        result = await manager.get_imperator_summary(current_pass=None)

        assert "Convergence" not in result

    async def test_summary_with_pass_1_no_convergence(self):
        """No convergence line for pass_1 (no previous pass)."""
        manager, mock_store, mock_query = self._make_manager()

        mock_query.get_entity_breakdown.return_value = {"PROGRAM": 2}
        mock_query.get_confirmation_stats.return_value = {
            "confirmed_count": 5,
            "total": 10,
            "confirmation_rate": 0.5,
            "avg_corroboration": 1.0,
        }
        mock_store.get_unresolved_conflicts.return_value = []

        result = await manager.get_imperator_summary(current_pass="pass_1")

        assert "Convergence" not in result

    async def test_summary_with_convergence(self):
        """Includes convergence line for pass_2+."""
        manager, mock_store, mock_query = self._make_manager()

        mock_query.get_entity_breakdown.return_value = {"PROGRAM": 5}
        mock_query.get_confirmation_stats.return_value = {
            "confirmed_count": 20,
            "total": 25,
            "confirmation_rate": 0.8,
            "avg_corroboration": 1.5,
        }
        mock_store.get_unresolved_conflicts.return_value = []

        delta = MagicMock()
        delta.change_rate = 0.032
        delta.has_converged = True
        mock_store.compute_delta.return_value = delta

        result = await manager.get_imperator_summary(current_pass="pass_2")

        assert "Convergence: 3.2% delta (converged)" in result
        mock_store.compute_delta.assert_called_once_with("pass_1", "pass_2")

    async def test_summary_not_converged(self):
        """Shows 'not converged' when delta is above threshold."""
        manager, mock_store, mock_query = self._make_manager()

        mock_query.get_entity_breakdown.return_value = {"PROGRAM": 5}
        mock_query.get_confirmation_stats.return_value = {
            "confirmed_count": 10,
            "total": 25,
            "confirmation_rate": 0.4,
            "avg_corroboration": 1.1,
        }
        mock_store.get_unresolved_conflicts.return_value = []

        delta = MagicMock()
        delta.change_rate = 0.15
        delta.has_converged = False
        mock_store.compute_delta.return_value = delta

        result = await manager.get_imperator_summary(current_pass="pass_3")

        assert "Convergence: 15.0% delta (not converged)" in result
        mock_store.compute_delta.assert_called_once_with("pass_2", "pass_3")

    async def test_exception_returns_empty(self):
        """Returns empty string on exception (safe default)."""
        manager, mock_store, mock_query = self._make_manager()

        mock_query.get_entity_breakdown.side_effect = RuntimeError("db error")

        result = await manager.get_imperator_summary()

        assert result == ""

    async def test_convergence_exception_still_returns_summary(self):
        """Delta failure is silently ignored, summary still returned."""
        manager, mock_store, mock_query = self._make_manager()

        mock_query.get_entity_breakdown.return_value = {"PROGRAM": 3}
        mock_query.get_confirmation_stats.return_value = {
            "confirmed_count": 6,
            "total": 10,
            "confirmation_rate": 0.6,
            "avg_corroboration": 1.2,
        }
        mock_store.get_unresolved_conflicts.return_value = []
        mock_store.compute_delta.side_effect = RuntimeError("delta error")

        result = await manager.get_imperator_summary(current_pass="pass_2")

        # Summary should still be returned without convergence line
        assert "## Knowledge Graph Health" in result
        assert "3 entities" in result
        assert "Convergence" not in result

    async def test_empty_graph_summary(self):
        """Generates correct summary for empty graph."""
        manager, mock_store, mock_query = self._make_manager()

        mock_query.get_entity_breakdown.return_value = {}
        mock_query.get_confirmation_stats.return_value = {
            "confirmed_count": 0,
            "total": 0,
            "confirmation_rate": 0.0,
            "avg_corroboration": 0.0,
        }
        mock_store.get_unresolved_conflicts.return_value = []

        result = await manager.get_imperator_summary()

        assert "0 entities (none)" in result
        assert "0 triples" in result
        assert "0 unresolved conflicts" in result


# ===========================================================================
# Tests for KnowledgeGraphManager._extract_pass_number
# ===========================================================================


class TestExtractPassNumber:
    """Tests for the static _extract_pass_number helper."""

    def test_standard_pass(self):
        """Extracts pass number from standard format."""
        from war_rig.knowledge_graph.manager import KnowledgeGraphManager

        assert KnowledgeGraphManager._extract_pass_number("pass_1") == 1
        assert KnowledgeGraphManager._extract_pass_number("pass_2") == 2
        assert KnowledgeGraphManager._extract_pass_number("pass_10") == 10

    def test_non_numeric(self):
        """Returns None for non-numeric suffix."""
        from war_rig.knowledge_graph.manager import KnowledgeGraphManager

        assert KnowledgeGraphManager._extract_pass_number("pass_abc") is None

    def test_no_underscore(self):
        """Returns None when no underscore separator."""
        from war_rig.knowledge_graph.manager import KnowledgeGraphManager

        assert KnowledgeGraphManager._extract_pass_number("preprocess") is None

    def test_custom_prefix(self):
        """Extracts number with custom prefix."""
        from war_rig.knowledge_graph.manager import KnowledgeGraphManager

        assert KnowledgeGraphManager._extract_pass_number("cycle_3") == 3


# ===========================================================================
# Tests for Imperator prompt injection
# ===========================================================================


class TestImperatorPromptInjection:
    """Tests that KG summary is injected into Imperator prompts."""

    def _make_imperator(self):  # noqa: ANN202
        """Create an ImperatorAgent with minimal config."""
        from war_rig.agents.imperator import ImperatorAgent
        from war_rig.config import APIConfig, ImperatorConfig

        return ImperatorAgent(
            config=ImperatorConfig(model="test-model"),
            api_config=APIConfig(),
        )

    def test_holistic_prompt_includes_kg_summary(self):
        """Full holistic prompt includes KG summary when provided."""
        from war_rig.agents.imperator import HolisticReviewInput

        imperator = self._make_imperator()
        input_data = HolisticReviewInput(
            batch_id="test-batch",
            cycle=1,
            knowledge_graph_summary="## Knowledge Graph Health\n- 10 entities\n- 20 triples",
        )
        prompt = imperator._build_holistic_user_prompt(input_data)
        assert "## Knowledge Graph Health" in prompt
        assert "10 entities" in prompt
        assert "20 triples" in prompt

    def test_holistic_prompt_omits_kg_when_none(self):
        """Full holistic prompt does not include KG section when None."""
        from war_rig.agents.imperator import HolisticReviewInput

        imperator = self._make_imperator()
        input_data = HolisticReviewInput(
            batch_id="test-batch",
            cycle=1,
            knowledge_graph_summary=None,
        )
        prompt = imperator._build_holistic_user_prompt(input_data)
        assert "Knowledge Graph Health" not in prompt

    def test_compact_prompt_includes_kg_summary(self):
        """Compact holistic prompt includes KG summary when provided."""
        from war_rig.agents.imperator import HolisticReviewInputCompact

        imperator = self._make_imperator()
        input_data = HolisticReviewInputCompact(
            batch_id="test-batch",
            cycle=2,
            knowledge_graph_summary="## Knowledge Graph Health\n- 5 entities\n- 8 triples",
        )
        prompt = imperator._build_compact_holistic_user_prompt(input_data)
        assert "## Knowledge Graph Health" in prompt
        assert "5 entities" in prompt

    def test_compact_prompt_omits_kg_when_none(self):
        """Compact holistic prompt does not include KG section when None."""
        from war_rig.agents.imperator import HolisticReviewInputCompact

        imperator = self._make_imperator()
        input_data = HolisticReviewInputCompact(
            batch_id="test-batch",
            cycle=1,
            knowledge_graph_summary=None,
        )
        prompt = imperator._build_compact_holistic_user_prompt(input_data)
        assert "Knowledge Graph Health" not in prompt

    def test_holistic_prompt_kg_after_data_flow(self):
        """KG summary appears after Data Flow section in full prompt."""
        from war_rig.agents.imperator import HolisticReviewInput

        imperator = self._make_imperator()
        input_data = HolisticReviewInput(
            batch_id="test-batch",
            cycle=1,
            data_flow={"PROG1": ["FILE1", "FILE2"]},
            knowledge_graph_summary="## Knowledge Graph Health\n- test",
            per_file_confidence={},
        )
        prompt = imperator._build_holistic_user_prompt(input_data)

        data_flow_pos = prompt.find("## Data Flow")
        kg_pos = prompt.find("## Knowledge Graph Health")
        task_pos = prompt.find("## Task")

        assert data_flow_pos < kg_pos < task_pos

    def test_compact_prompt_kg_before_task(self):
        """KG summary appears before Task section in compact prompt."""
        from war_rig.agents.imperator import HolisticReviewInputCompact

        imperator = self._make_imperator()
        input_data = HolisticReviewInputCompact(
            batch_id="test-batch",
            cycle=1,
            knowledge_graph_summary="## Knowledge Graph Health\n- test",
        )
        prompt = imperator._build_compact_holistic_user_prompt(input_data)

        kg_pos = prompt.find("## Knowledge Graph Health")
        task_pos = prompt.find("## Task")

        assert kg_pos < task_pos


# ===========================================================================
# Tests for summary token budget
# ===========================================================================


class TestSummaryTokenBudget:
    """Verify the summary stays under ~200 tokens (~800 chars)."""

    async def test_large_graph_under_budget(self):
        """Summary stays compact even with many entity types."""
        from war_rig.knowledge_graph.manager import KnowledgeGraphManager

        config = MagicMock()
        config.knowledge_graph_enabled = True
        manager = KnowledgeGraphManager(config)

        mock_store = MagicMock()
        mock_query = MagicMock()

        # Simulate a large graph with all entity types
        mock_query.get_entity_breakdown = AsyncMock(
            return_value={
                "PROGRAM": 50,
                "DATASET": 40,
                "COPYBOOK": 30,
                "JCL_JOB": 20,
                "JCL_STEP": 60,
                "FIELD": 100,
                "PARAGRAPH": 80,
                "DB_TABLE": 15,
                "CUSTOM": 5,
            }
        )
        mock_query.get_confirmation_stats = AsyncMock(
            return_value={
                "confirmed_count": 350,
                "total": 500,
                "confirmation_rate": 0.70,
                "avg_corroboration": 1.8,
            }
        )
        mock_store.get_unresolved_conflicts = AsyncMock(
            return_value=[MagicMock() for _ in range(12)]
        )

        delta = MagicMock()
        delta.change_rate = 0.032
        delta.has_converged = True
        mock_store.compute_delta = AsyncMock(return_value=delta)

        manager._store = mock_store
        manager._query_helper = mock_query

        result = await manager.get_imperator_summary(current_pass="pass_3")

        # Should be well under 800 chars
        assert len(result) < 800, f"Summary too long: {len(result)} chars"
        # Verify all sections are present
        assert "## Knowledge Graph Health" in result
        assert "400 entities" in result
        assert "500 triples" in result
        assert "12 unresolved conflicts" in result
        assert "Convergence" in result
