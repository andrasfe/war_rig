"""Tests for KG tools for CodeWhisper SDK."""

from unittest.mock import AsyncMock, MagicMock

import pytest

from war_rig.agents.kg_tools import create_kg_tools


@pytest.fixture
def mock_entity():
    """Create a mock Entity."""
    entity = MagicMock()
    entity.name = "PROG01"
    entity.id = 1
    return entity


@pytest.fixture
def mock_kg_manager():
    """Create a mock KnowledgeGraphManager with query helper."""
    manager = MagicMock()
    manager.enabled = True
    manager._query_helper = MagicMock()
    manager._store = MagicMock()
    manager.get_system_summary = AsyncMock(
        return_value="## System Architecture\n- 5 programs"
    )
    return manager


@pytest.fixture
def disabled_kg_manager():
    """Create a disabled KG manager."""
    manager = MagicMock()
    manager.enabled = False
    manager._query_helper = None
    manager._store = None
    return manager


class TestCreateKgTools:
    """Test the create_kg_tools factory function."""

    def test_creates_five_tools(self, mock_kg_manager):
        tools = create_kg_tools(mock_kg_manager)
        assert len(tools) == 5

    def test_tool_names(self, mock_kg_manager):
        tools = create_kg_tools(mock_kg_manager)
        names = {t.name for t in tools}
        assert names == {
            "kg_get_hub_entities",
            "kg_get_program_relationships",
            "kg_get_programs_for_dataset",
            "kg_get_jcl_context",
            "kg_get_copybook_users",
        }

    def test_tools_have_handlers(self, mock_kg_manager):
        tools = create_kg_tools(mock_kg_manager)
        for tool in tools:
            assert tool.handler is not None

    def test_tools_have_valid_schemas(self, mock_kg_manager):
        tools = create_kg_tools(mock_kg_manager)
        for tool in tools:
            assert tool.parameters["type"] == "object"
            assert "properties" in tool.parameters

    def test_tools_have_descriptions(self, mock_kg_manager):
        tools = create_kg_tools(mock_kg_manager)
        for tool in tools:
            assert len(tool.description) > 20


class TestKgGetHubEntities:
    """Test kg_get_hub_entities tool."""

    async def test_returns_system_summary(self, mock_kg_manager):
        tools = create_kg_tools(mock_kg_manager)
        tool = next(t for t in tools if t.name == "kg_get_hub_entities")
        result = await tool.handler()
        assert "System Architecture" in result
        mock_kg_manager.get_system_summary.assert_called_once()

    async def test_disabled_kg_returns_message(self, disabled_kg_manager):
        tools = create_kg_tools(disabled_kg_manager)
        tool = next(t for t in tools if t.name == "kg_get_hub_entities")
        result = await tool.handler()
        assert "not available" in result

    async def test_empty_graph_returns_message(self, mock_kg_manager):
        mock_kg_manager.get_system_summary = AsyncMock(return_value="")
        tools = create_kg_tools(mock_kg_manager)
        tool = next(t for t in tools if t.name == "kg_get_hub_entities")
        result = await tool.handler()
        assert "empty" in result.lower()

    async def test_handles_exception(self, mock_kg_manager):
        mock_kg_manager.get_system_summary = AsyncMock(
            side_effect=RuntimeError("DB error")
        )
        tools = create_kg_tools(mock_kg_manager)
        tool = next(t for t in tools if t.name == "kg_get_hub_entities")
        result = await tool.handler()
        assert "Error" in result


class TestKgGetProgramRelationships:
    """Test kg_get_program_relationships tool."""

    async def test_returns_relationships(self, mock_kg_manager, mock_entity):
        qh = mock_kg_manager._query_helper
        qh.get_called_programs = AsyncMock(return_value=[mock_entity])
        qh.get_calling_programs = AsyncMock(return_value=[])
        qh.get_datasets_for_program = AsyncMock(
            return_value={"reads": [], "writes": []}
        )
        qh.get_copybooks_for_program = AsyncMock(return_value=[])

        tools = create_kg_tools(mock_kg_manager)
        tool = next(
            t for t in tools if t.name == "kg_get_program_relationships"
        )
        result = await tool.handler(program_name="MAINPGM")
        assert "PROG01" in result
        assert "Calls" in result

    async def test_no_relationships(self, mock_kg_manager):
        qh = mock_kg_manager._query_helper
        qh.get_called_programs = AsyncMock(return_value=[])
        qh.get_calling_programs = AsyncMock(return_value=[])
        qh.get_datasets_for_program = AsyncMock(
            return_value={"reads": [], "writes": []}
        )
        qh.get_copybooks_for_program = AsyncMock(return_value=[])

        tools = create_kg_tools(mock_kg_manager)
        tool = next(
            t for t in tools if t.name == "kg_get_program_relationships"
        )
        result = await tool.handler(program_name="UNKNOWN")
        assert "No relationships" in result

    async def test_disabled_kg(self, disabled_kg_manager):
        tools = create_kg_tools(disabled_kg_manager)
        tool = next(
            t for t in tools if t.name == "kg_get_program_relationships"
        )
        result = await tool.handler(program_name="PROG")
        assert "not available" in result

    async def test_datasets_included(self, mock_kg_manager, mock_entity):
        ds_entity = MagicMock()
        ds_entity.name = "ACCOUNTS"
        qh = mock_kg_manager._query_helper
        qh.get_called_programs = AsyncMock(return_value=[])
        qh.get_calling_programs = AsyncMock(return_value=[])
        qh.get_datasets_for_program = AsyncMock(
            return_value={"reads": [ds_entity], "writes": []}
        )
        qh.get_copybooks_for_program = AsyncMock(return_value=[])

        tools = create_kg_tools(mock_kg_manager)
        tool = next(
            t for t in tools if t.name == "kg_get_program_relationships"
        )
        result = await tool.handler(program_name="PROG")
        assert "ACCOUNTS" in result
        assert "Reads" in result


class TestKgGetProgramsForDataset:
    """Test kg_get_programs_for_dataset tool."""

    async def test_returns_readers_writers(self, mock_kg_manager):
        from war_rig.knowledge_graph.models import RelationType

        store = mock_kg_manager._store
        ds_entity = MagicMock()
        ds_entity.id = 10
        store.get_entity = AsyncMock(return_value=ds_entity)

        # Create mock triples
        triple_read = MagicMock()
        triple_read.predicate = RelationType.READS
        triple_read.subject_id = 1
        triple_write = MagicMock()
        triple_write.predicate = RelationType.WRITES
        triple_write.subject_id = 2
        store.get_triples_for_entity = AsyncMock(
            return_value=[triple_read, triple_write]
        )

        reader = MagicMock()
        reader.name = "READER01"
        writer = MagicMock()
        writer.name = "WRITER01"
        store.get_entity_by_id = AsyncMock(
            side_effect=lambda eid: reader if eid == 1 else writer
        )

        tools = create_kg_tools(mock_kg_manager)
        tool = next(
            t for t in tools if t.name == "kg_get_programs_for_dataset"
        )
        result = await tool.handler(dataset_name="ACCTFILE")
        assert "READER01" in result
        assert "WRITER01" in result

    async def test_dataset_not_found(self, mock_kg_manager):
        mock_kg_manager._store.get_entity = AsyncMock(return_value=None)
        tools = create_kg_tools(mock_kg_manager)
        tool = next(
            t for t in tools if t.name == "kg_get_programs_for_dataset"
        )
        result = await tool.handler(dataset_name="MISSING")
        assert "not found" in result


class TestKgGetJclContext:
    """Test kg_get_jcl_context tool."""

    async def test_returns_context(self, mock_kg_manager):
        job = MagicMock()
        job.name = "DAILYJOB"
        step = MagicMock()
        step.name = "STEP01"
        co_prog = MagicMock()
        co_prog.name = "COPROG01"

        qh = mock_kg_manager._query_helper
        qh.get_jcl_context_for_program = AsyncMock(
            return_value={
                "steps": [step],
                "jobs": [job],
                "co_programs": [co_prog],
            }
        )

        tools = create_kg_tools(mock_kg_manager)
        tool = next(t for t in tools if t.name == "kg_get_jcl_context")
        result = await tool.handler(program_name="PROG01")
        assert "DAILYJOB" in result
        assert "STEP01" in result
        assert "COPROG01" in result

    async def test_no_jcl_context(self, mock_kg_manager):
        qh = mock_kg_manager._query_helper
        qh.get_jcl_context_for_program = AsyncMock(
            return_value={"steps": [], "jobs": [], "co_programs": []}
        )

        tools = create_kg_tools(mock_kg_manager)
        tool = next(t for t in tools if t.name == "kg_get_jcl_context")
        result = await tool.handler(program_name="UNKNOWN")
        assert "No JCL context" in result


class TestKgGetCopybookUsers:
    """Test kg_get_copybook_users tool."""

    async def test_returns_users(self, mock_kg_manager, mock_entity):
        prog2 = MagicMock()
        prog2.name = "PROG02"
        qh = mock_kg_manager._query_helper
        qh.get_programs_sharing_copybook = AsyncMock(
            return_value=[mock_entity, prog2]
        )

        tools = create_kg_tools(mock_kg_manager)
        tool = next(t for t in tools if t.name == "kg_get_copybook_users")
        result = await tool.handler(copybook_name="CBACTM01")
        assert "PROG01" in result
        assert "PROG02" in result

    async def test_no_users(self, mock_kg_manager):
        qh = mock_kg_manager._query_helper
        qh.get_programs_sharing_copybook = AsyncMock(return_value=[])

        tools = create_kg_tools(mock_kg_manager)
        tool = next(t for t in tools if t.name == "kg_get_copybook_users")
        result = await tool.handler(copybook_name="UNKNOWN")
        assert "No programs" in result
