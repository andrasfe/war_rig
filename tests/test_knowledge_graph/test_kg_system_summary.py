"""Tests for Knowledge Graph system summary (Layer 3 README enrichment).

Tests cover:
- KnowledgeGraphManager.get_system_summary()
- ImperatorAgent._format_call_semantics_table()
- ImperatorAgent._build_system_design_prompt() enrichment injection
- generate_system_design() new parameter forwarding
"""

from unittest.mock import AsyncMock, MagicMock

from war_rig.knowledge_graph.models import (
    Entity,
    EntityType,
    RelationType,
    Triple,
)

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


def _make_manager(enabled: bool = True) -> tuple:
    """Create a KnowledgeGraphManager with mocked internals.

    Returns:
        Tuple of (manager, mock_store).
    """
    from war_rig.knowledge_graph.manager import KnowledgeGraphManager

    config = MagicMock()
    config.knowledge_graph_enabled = enabled
    manager = KnowledgeGraphManager(config)

    mock_store = MagicMock()
    mock_store.get_all_entities = AsyncMock(return_value=[])
    mock_store.get_all_triples = AsyncMock(return_value=[])
    mock_store.get_triple_count = AsyncMock(return_value=0)

    manager._store = mock_store
    return manager, mock_store


# ===========================================================================
# Tests for KnowledgeGraphManager.get_system_summary
# ===========================================================================


class TestGetSystemSummary:
    """Tests for get_system_summary."""

    async def test_disabled_returns_empty(self):
        """Returns empty string when KG is disabled."""
        manager, _ = _make_manager(enabled=False)
        result = await manager.get_system_summary()
        assert result == ""

    async def test_no_store_returns_empty(self):
        """Returns empty string when store is None."""
        manager, _ = _make_manager(enabled=True)
        manager._store = None
        result = await manager.get_system_summary()
        assert result == ""

    async def test_empty_graph_returns_empty(self):
        """Returns empty string for empty graph."""
        manager, mock_store = _make_manager()
        mock_store.get_all_entities.return_value = []
        mock_store.get_all_triples.return_value = []
        result = await manager.get_system_summary()
        assert result == ""

    async def test_no_triples_returns_empty(self):
        """Returns empty string when there are entities but no triples."""
        manager, mock_store = _make_manager()
        mock_store.get_all_entities.return_value = [
            _make_entity(1, EntityType.PROGRAM, "PROG1"),
        ]
        mock_store.get_all_triples.return_value = []
        result = await manager.get_system_summary()
        assert result == ""

    async def test_hub_programs_section(self):
        """Hub programs section lists programs by connection count."""
        manager, mock_store = _make_manager()
        mock_store.get_all_entities.return_value = [
            _make_entity(1, EntityType.PROGRAM, "ACCT0100"),
            _make_entity(2, EntityType.PROGRAM, "ACCT0200"),
            _make_entity(3, EntityType.PROGRAM, "ACCT0300"),
        ]
        mock_store.get_all_triples.return_value = [
            _make_triple(1, 1, RelationType.CALLS, 2),
            _make_triple(2, 1, RelationType.CALLS, 3),
            _make_triple(3, 2, RelationType.CALLS, 3),
        ]

        result = await manager.get_system_summary()

        assert "## System Architecture (from Knowledge Graph)" in result
        assert "### Hub Programs (most connections)" in result
        assert "ACCT0100" in result
        # ACCT0100 has 2 calls (subject) = most connected
        assert "ACCT0100" in result.split("ACCT0200")[0]  # ACCT0100 listed first

    async def test_data_flow_hotspots_section(self):
        """Data flow hotspots show datasets with multiple readers/writers."""
        manager, mock_store = _make_manager()
        mock_store.get_all_entities.return_value = [
            _make_entity(1, EntityType.PROGRAM, "PROG1"),
            _make_entity(2, EntityType.PROGRAM, "PROG2"),
            _make_entity(3, EntityType.PROGRAM, "PROG3"),
            _make_entity(10, EntityType.DATASET, "ACCT.MASTER.FILE"),
        ]
        mock_store.get_all_triples.return_value = [
            _make_triple(1, 1, RelationType.READS, 10),
            _make_triple(2, 2, RelationType.READS, 10),
            _make_triple(3, 3, RelationType.WRITES, 10),
        ]

        result = await manager.get_system_summary()

        assert "### Data Flow Hotspots" in result
        assert "ACCT.MASTER.FILE" in result
        assert "read by 2 programs" in result
        assert "written by 1" in result

    async def test_shared_resources_section(self):
        """Shared resources show copybooks included by multiple programs."""
        manager, mock_store = _make_manager()
        mock_store.get_all_entities.return_value = [
            _make_entity(1, EntityType.PROGRAM, "PROG1"),
            _make_entity(2, EntityType.PROGRAM, "PROG2"),
            _make_entity(3, EntityType.PROGRAM, "PROG3"),
            _make_entity(20, EntityType.COPYBOOK, "COMMAREA"),
        ]
        mock_store.get_all_triples.return_value = [
            _make_triple(1, 1, RelationType.INCLUDES, 20),
            _make_triple(2, 2, RelationType.INCLUDES, 20),
            _make_triple(3, 3, RelationType.INCLUDES, 20),
        ]

        result = await manager.get_system_summary()

        assert "### Shared Resources" in result
        assert "COMMAREA" in result
        assert "included by 3 programs" in result

    async def test_no_shared_copybooks_section_omitted(self):
        """Shared resources section omitted when no copybook has >1 includer."""
        manager, mock_store = _make_manager()
        mock_store.get_all_entities.return_value = [
            _make_entity(1, EntityType.PROGRAM, "PROG1"),
            _make_entity(20, EntityType.COPYBOOK, "COMMAREA"),
        ]
        mock_store.get_all_triples.return_value = [
            _make_triple(1, 1, RelationType.INCLUDES, 20),
        ]

        result = await manager.get_system_summary()

        assert "### Shared Resources" not in result

    async def test_hub_programs_capped_at_5(self):
        """Hub programs section lists at most 5 entries."""
        manager, mock_store = _make_manager()
        entities = [
            _make_entity(i, EntityType.PROGRAM, f"PROG{i:04d}")
            for i in range(1, 9)
        ]
        triples = []
        tid = 1
        for i in range(1, 9):
            for j in range(1, 9):
                if i != j:
                    triples.append(
                        _make_triple(tid, i, RelationType.CALLS, j)
                    )
                    tid += 1

        mock_store.get_all_entities.return_value = entities
        mock_store.get_all_triples.return_value = triples

        result = await manager.get_system_summary()

        # Count program entries in hub section
        hub_section = result.split("### Hub Programs")[1]
        # Each entry starts with "- PROG"
        entry_count = hub_section.count("- PROG")
        assert entry_count <= 5

    async def test_datasets_capped_at_5(self):
        """Data flow hotspots section lists at most 5 datasets."""
        manager, mock_store = _make_manager()
        entities = [_make_entity(1, EntityType.PROGRAM, "PROG1")]
        entities.append(_make_entity(2, EntityType.PROGRAM, "PROG2"))
        for i in range(10, 20):
            entities.append(_make_entity(i, EntityType.DATASET, f"DS{i}"))

        triples = []
        tid = 1
        for i in range(10, 20):
            triples.append(_make_triple(tid, 1, RelationType.READS, i))
            tid += 1
            triples.append(_make_triple(tid, 2, RelationType.READS, i))
            tid += 1

        mock_store.get_all_entities.return_value = entities
        mock_store.get_all_triples.return_value = triples

        result = await manager.get_system_summary()

        hotspot_section = result.split("### Data Flow Hotspots")[1]
        if "### Shared Resources" in hotspot_section:
            hotspot_section = hotspot_section.split("### Shared Resources")[0]
        entry_count = hotspot_section.count("- DS")
        assert entry_count <= 5

    async def test_token_budget_cap(self):
        """Summary is capped by max_tokens budget."""
        manager, mock_store = _make_manager()

        # Create a large graph
        entities = []
        for i in range(1, 50):
            entities.append(_make_entity(i, EntityType.PROGRAM, f"PROG{i:04d}"))
        for i in range(100, 150):
            entities.append(_make_entity(i, EntityType.DATASET, f"DATASET.{i}"))

        triples = []
        tid = 1
        for i in range(1, 50):
            for j in range(100, 150):
                triples.append(_make_triple(tid, i, RelationType.READS, j))
                tid += 1

        mock_store.get_all_entities.return_value = entities
        mock_store.get_all_triples.return_value = triples

        # Very small budget
        result = await manager.get_system_summary(max_tokens=50)

        assert len(result) <= 200  # 50 tokens * 4 chars

    async def test_exception_returns_empty(self):
        """Returns empty string on exception (safe default)."""
        manager, mock_store = _make_manager()
        mock_store.get_all_entities.side_effect = RuntimeError("db error")

        result = await manager.get_system_summary()
        assert result == ""

    async def test_full_summary_structure(self):
        """Full summary has all three sections when data is available."""
        manager, mock_store = _make_manager()
        mock_store.get_all_entities.return_value = [
            _make_entity(1, EntityType.PROGRAM, "ACCT0100"),
            _make_entity(2, EntityType.PROGRAM, "ACCT0200"),
            _make_entity(10, EntityType.DATASET, "ACCT.MASTER"),
            _make_entity(20, EntityType.COPYBOOK, "COMMAREA"),
        ]
        mock_store.get_all_triples.return_value = [
            _make_triple(1, 1, RelationType.CALLS, 2),
            _make_triple(2, 1, RelationType.READS, 10),
            _make_triple(3, 2, RelationType.READS, 10),
            _make_triple(4, 1, RelationType.INCLUDES, 20),
            _make_triple(5, 2, RelationType.INCLUDES, 20),
        ]

        result = await manager.get_system_summary()

        assert "## System Architecture (from Knowledge Graph)" in result
        assert "### Hub Programs" in result
        assert "### Data Flow Hotspots" in result
        assert "### Shared Resources" in result

    async def test_summary_under_400_token_budget(self):
        """Summary stays under approximate 400 token budget (~1600 chars)."""
        manager, mock_store = _make_manager()
        mock_store.get_all_entities.return_value = [
            _make_entity(1, EntityType.PROGRAM, "ACCT0100"),
            _make_entity(2, EntityType.PROGRAM, "ACCT0200"),
            _make_entity(3, EntityType.PROGRAM, "ACCT0300"),
            _make_entity(4, EntityType.PROGRAM, "REPT0100"),
            _make_entity(5, EntityType.PROGRAM, "REPT0200"),
            _make_entity(10, EntityType.DATASET, "ACCT.MASTER"),
            _make_entity(11, EntityType.DATASET, "TRANS.LOG"),
            _make_entity(12, EntityType.DATASET, "REPORT.OUT"),
            _make_entity(20, EntityType.COPYBOOK, "COMMAREA"),
            _make_entity(21, EntityType.COPYBOOK, "ACCT-REC"),
            _make_entity(22, EntityType.COPYBOOK, "ERR-HAND"),
        ]
        mock_store.get_all_triples.return_value = [
            _make_triple(1, 1, RelationType.CALLS, 2),
            _make_triple(2, 1, RelationType.CALLS, 3),
            _make_triple(3, 2, RelationType.CALLS, 3),
            _make_triple(4, 4, RelationType.CALLS, 5),
            _make_triple(5, 1, RelationType.READS, 10),
            _make_triple(6, 2, RelationType.READS, 10),
            _make_triple(7, 3, RelationType.WRITES, 10),
            _make_triple(8, 1, RelationType.READS, 11),
            _make_triple(9, 4, RelationType.WRITES, 11),
            _make_triple(10, 5, RelationType.WRITES, 12),
            _make_triple(11, 1, RelationType.INCLUDES, 20),
            _make_triple(12, 2, RelationType.INCLUDES, 20),
            _make_triple(13, 3, RelationType.INCLUDES, 20),
            _make_triple(14, 4, RelationType.INCLUDES, 20),
            _make_triple(15, 1, RelationType.INCLUDES, 21),
            _make_triple(16, 2, RelationType.INCLUDES, 21),
            _make_triple(17, 1, RelationType.INCLUDES, 22),
            _make_triple(18, 3, RelationType.INCLUDES, 22),
        ]

        result = await manager.get_system_summary()

        # 400 tokens ~1600 chars, allow some margin
        assert len(result) < 1600, f"Summary too long: {len(result)} chars"


# ===========================================================================
# Tests for ImperatorAgent._format_call_semantics_table
# ===========================================================================


class TestFormatCallSemanticsTable:
    """Tests for _format_call_semantics_table."""

    def _make_imperator(self):  # noqa: ANN202
        """Create an ImperatorAgent with minimal config."""
        from war_rig.agents.imperator import ImperatorAgent
        from war_rig.config import APIConfig, ImperatorConfig

        return ImperatorAgent(
            config=ImperatorConfig(model="test-model"),
            api_config=APIConfig(),
        )

    def test_empty_semantics_returns_empty(self):
        """Returns empty list for empty or None semantics."""
        from war_rig.agents.imperator import ImperatorAgent

        assert ImperatorAgent._format_call_semantics_table({}) == []
        assert ImperatorAgent._format_call_semantics_table(None) == []

    def test_single_entry(self):
        """Formats a single call semantics entry correctly."""
        from war_rig.agents.imperator import ImperatorAgent

        semantics = {
            "ACCT0100->ACCT0200": {
                "inputs": ["ACCT-NUM", "ACCT-TYPE"],
                "outputs": ["ACCT-BALANCE", "STATUS"],
                "purpose": "Validate account",
            }
        }
        result = ImperatorAgent._format_call_semantics_table(semantics)

        assert "## Cross-File Data Flow (call semantics)" in result[0]
        assert "| Caller -> Callee |" in result[2]
        assert "ACCT0100->ACCT0200" in result[4]
        assert "ACCT-NUM, ACCT-TYPE" in result[4]
        assert "ACCT-BALANCE, STATUS" in result[4]
        assert "Validate account" in result[4]

    def test_missing_fields_use_dash(self):
        """Missing inputs/outputs/purpose default to dash."""
        from war_rig.agents.imperator import ImperatorAgent

        semantics = {
            "PROG1->PROG2": {
                "inputs": [],
                "outputs": [],
            }
        }
        result = ImperatorAgent._format_call_semantics_table(semantics)
        row = result[4]
        assert "| - | - | - |" in row

    def test_truncation_at_max_rows(self):
        """Table is truncated at max_rows with a summary row."""
        from war_rig.agents.imperator import ImperatorAgent

        semantics = {
            f"PROG{i}->PROG{i+1}": {
                "inputs": ["X"],
                "outputs": ["Y"],
                "purpose": f"Call {i}",
            }
            for i in range(30)
        }
        result = ImperatorAgent._format_call_semantics_table(
            semantics, max_rows=20
        )

        # 4 header lines + 20 data rows + 1 truncation row = 25
        assert len(result) == 25
        assert "... and 10 more call pairs" in result[-1]

    def test_long_purpose_truncated(self):
        """Purpose text longer than 60 chars is truncated."""
        from war_rig.agents.imperator import ImperatorAgent

        semantics = {
            "A->B": {
                "inputs": ["X"],
                "outputs": ["Y"],
                "purpose": "A" * 80,
            }
        }
        result = ImperatorAgent._format_call_semantics_table(semantics)
        row = result[4]
        assert "..." in row
        # Purpose should be at most 60 chars
        purpose_part = row.split("|")[4].strip()
        assert len(purpose_part) <= 60


# ===========================================================================
# Tests for _build_system_design_prompt enrichment
# ===========================================================================


class TestSystemDesignPromptEnrichment:
    """Tests that new data sources are injected into system design prompt."""

    def _make_imperator(self):  # noqa: ANN202
        """Create an ImperatorAgent with minimal config."""
        from war_rig.agents.imperator import ImperatorAgent
        from war_rig.config import APIConfig, ImperatorConfig

        return ImperatorAgent(
            config=ImperatorConfig(model="test-model"),
            api_config=APIConfig(),
        )

    def test_prompt_includes_call_semantics(self):
        """Cross-file call semantics table is injected into prompt."""
        from war_rig.agents.imperator import HolisticReviewInput

        imperator = self._make_imperator()
        input_data = HolisticReviewInput(batch_id="test", cycle=1)
        semantics = {
            "ACCT0100->ACCT0200": {
                "inputs": ["ACCT-NUM"],
                "outputs": ["STATUS"],
                "purpose": "Validate",
            }
        }
        prompt = imperator._build_system_design_prompt(
            input_data, cross_file_call_semantics=semantics
        )
        assert "## Cross-File Data Flow (call semantics)" in prompt
        assert "ACCT0100->ACCT0200" in prompt
        assert "ACCT-NUM" in prompt

    def test_prompt_includes_entry_points(self):
        """Entry points section is injected into prompt."""
        from war_rig.agents.imperator import HolisticReviewInput

        imperator = self._make_imperator()
        input_data = HolisticReviewInput(batch_id="test", cycle=1)
        prompt = imperator._build_system_design_prompt(
            input_data, entry_points=["ACCT0100", "REPT0100"]
        )
        assert "## System Entry Points" in prompt
        assert "ACCT0100 (no callers - likely batch or CICS entry)" in prompt
        assert "REPT0100 (no callers - likely batch or CICS entry)" in prompt

    def test_prompt_includes_call_chains(self):
        """Call chains section is injected into prompt."""
        from war_rig.agents.imperator import HolisticReviewInput

        imperator = self._make_imperator()
        input_data = HolisticReviewInput(batch_id="test", cycle=1)
        prompt = imperator._build_system_design_prompt(
            input_data,
            call_chains=[
                ["ACCT0100", "ACCT0200", "ACCT0300"],
                ["REPT0100", "REPT0200"],
            ],
        )
        assert "## Typical Execution Flows" in prompt
        assert "1. ACCT0100 -> ACCT0200 -> ACCT0300" in prompt
        assert "2. REPT0100 -> REPT0200" in prompt

    def test_prompt_includes_kg_system_summary(self):
        """KG system summary is injected as-is into prompt."""
        from war_rig.agents.imperator import HolisticReviewInput

        imperator = self._make_imperator()
        input_data = HolisticReviewInput(batch_id="test", cycle=1)
        kg_summary = (
            "## System Architecture (from Knowledge Graph)\n\n"
            "### Hub Programs\n"
            "- ACCT0100: 8 relationships"
        )
        prompt = imperator._build_system_design_prompt(
            input_data, kg_system_summary=kg_summary
        )
        assert "## System Architecture (from Knowledge Graph)" in prompt
        assert "ACCT0100: 8 relationships" in prompt

    def test_prompt_omits_sections_when_none(self):
        """No enrichment sections when all params are None."""
        from war_rig.agents.imperator import HolisticReviewInput

        imperator = self._make_imperator()
        input_data = HolisticReviewInput(batch_id="test", cycle=1)
        prompt = imperator._build_system_design_prompt(input_data)

        assert "## Cross-File Data Flow" not in prompt
        assert "## System Entry Points" not in prompt
        assert "## Typical Execution Flows" not in prompt
        assert "## System Architecture (from Knowledge Graph)" not in prompt

    def test_enrichment_before_existing_content(self):
        """Enrichment sections appear before existing content section."""
        from war_rig.agents.imperator import HolisticReviewInput

        imperator = self._make_imperator()
        input_data = HolisticReviewInput(batch_id="test", cycle=1)
        prompt = imperator._build_system_design_prompt(
            input_data,
            existing_content="# Old README content",
            entry_points=["PROG1"],
            kg_system_summary="## System Architecture (from Knowledge Graph)\ntest",
        )

        entry_pos = prompt.find("## System Entry Points")
        kg_pos = prompt.find("## System Architecture (from Knowledge Graph)")
        existing_pos = prompt.find("## Existing README.md Content")

        assert entry_pos < existing_pos
        assert kg_pos < existing_pos

    def test_enrichment_before_output_format(self):
        """Enrichment sections appear before output format section."""
        from war_rig.agents.imperator import HolisticReviewInput

        imperator = self._make_imperator()
        input_data = HolisticReviewInput(batch_id="test", cycle=1)
        prompt = imperator._build_system_design_prompt(
            input_data,
            call_chains=[["A", "B", "C"]],
        )

        chains_pos = prompt.find("## Typical Execution Flows")
        output_pos = prompt.find("## Output Format")

        assert chains_pos < output_pos

    def test_entry_points_capped_at_5(self):
        """At most 5 entry points are included."""
        from war_rig.agents.imperator import HolisticReviewInput

        imperator = self._make_imperator()
        input_data = HolisticReviewInput(batch_id="test", cycle=1)
        entry_points = [f"PROG{i}" for i in range(10)]
        prompt = imperator._build_system_design_prompt(
            input_data, entry_points=entry_points
        )
        # Count occurrences of "no callers" marker
        assert prompt.count("no callers") == 5

    def test_call_chains_capped_at_3(self):
        """At most 3 call chains are included."""
        from war_rig.agents.imperator import HolisticReviewInput

        imperator = self._make_imperator()
        input_data = HolisticReviewInput(batch_id="test", cycle=1)
        chains = [["A", "B"] for _ in range(6)]
        prompt = imperator._build_system_design_prompt(
            input_data, call_chains=chains
        )
        # Only numbered entries 1-3 should appear
        assert "1. A -> B" in prompt
        assert "3. A -> B" in prompt
        assert "4. A -> B" not in prompt


# ===========================================================================
# Tests for generate_system_design parameter forwarding
# ===========================================================================


class TestGenerateSystemDesignParams:
    """Tests that generate_system_design passes new params to prompt builder."""

    def _make_imperator(self):  # noqa: ANN202
        """Create an ImperatorAgent with minimal config."""
        from war_rig.agents.imperator import ImperatorAgent
        from war_rig.config import APIConfig, ImperatorConfig

        return ImperatorAgent(
            config=ImperatorConfig(model="test-model"),
            api_config=APIConfig(),
        )

    async def test_mock_mode_ignores_new_params(self):
        """Mock mode returns mock output without using new params."""
        from war_rig.agents.imperator import HolisticReviewInput

        imperator = self._make_imperator()
        input_data = HolisticReviewInput(batch_id="test", cycle=1)
        result = await imperator.generate_system_design(
            input_data,
            use_mock=True,
            cross_file_call_semantics={"A->B": {"inputs": ["X"]}},
            kg_system_summary="test summary",
            entry_points=["PROG1"],
            call_chains=[["A", "B"]],
        )
        assert result.success is True
        assert result.markdown is not None
