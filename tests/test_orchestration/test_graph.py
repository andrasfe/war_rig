"""Tests for LangGraph orchestration."""

import pytest

from war_rig.config import WarRigConfig
from war_rig.orchestration.graph import create_war_rig_graph, WarRigGraph, analyze_file
from war_rig.orchestration.state import create_initial_state, WarRigState
from war_rig.models.templates import FileType


class TestWarRigState:
    """Tests for WarRigState management."""

    def test_create_initial_state(self, sample_cobol_source):
        """Test creating initial state."""
        state = create_initial_state(
            source_code=sample_cobol_source,
            file_name="TEST.cbl",
            rig_id="TEST_RIG",
            max_iterations=3,
        )

        assert state["source_code"] == sample_cobol_source
        assert state["file_name"] == "TEST.cbl"
        assert state["rig_id"] == "TEST_RIG"
        assert state["iteration"] == 0
        assert state["max_iterations"] == 3
        assert state["should_continue"] is True
        assert state["decision"] is None

    def test_initial_state_defaults(self, sample_cobol_source):
        """Test initial state default values."""
        state = create_initial_state(
            source_code=sample_cobol_source,
            file_name="TEST.cbl",
        )

        assert state["rig_id"] == "ALPHA"
        assert state["max_iterations"] == 3
        assert state["use_mock"] is False
        assert state["challenger_questions"] == []
        assert state["scribe_responses"] == []
        assert state["chrome_tickets"] == []


class TestWarRigGraph:
    """Tests for WarRigGraph."""

    @pytest.fixture
    def graph(self, test_config) -> WarRigGraph:
        """Create graph instance."""
        return WarRigGraph(test_config)

    def test_create_graph(self, test_config):
        """Test graph creation."""
        graph = create_war_rig_graph(test_config)
        assert isinstance(graph, WarRigGraph)
        assert graph.config == test_config

    def test_graph_has_nodes(self, graph):
        """Test that graph has expected nodes."""
        # The compiled graph should have the nodes we defined
        assert graph.nodes is not None
        assert graph.graph is not None

    @pytest.mark.asyncio
    async def test_mock_run(self, graph, sample_cobol_source):
        """Test running graph with mock agents."""
        result = await graph.ainvoke(
            source_code=sample_cobol_source,
            file_name="TESTPROG.cbl",
            use_mock=True,
        )

        assert isinstance(result, dict)
        assert "decision" in result
        assert result["decision"] in ["WITNESSED", "CHROME", "VALHALLA", "FORCED"]
        assert "iteration" in result
        assert result["iteration"] >= 1

    @pytest.mark.asyncio
    async def test_mock_produces_template(self, graph, sample_cobol_source):
        """Test that mock run produces a documentation template."""
        result = await graph.ainvoke(
            source_code=sample_cobol_source,
            file_name="TESTPROG.cbl",
            use_mock=True,
        )

        # Should have a current or final template
        template = result.get("final_template") or result.get("current_template")
        assert template is not None
        assert template.header.program_id is not None

    @pytest.mark.asyncio
    async def test_mock_respects_max_iterations(self, sample_cobol_source, test_config):
        """Test that graph respects max iterations."""
        test_config.max_iterations = 2
        graph = WarRigGraph(test_config)

        result = await graph.ainvoke(
            source_code=sample_cobol_source,
            file_name="TESTPROG.cbl",
            use_mock=True,
        )

        assert result["iteration"] <= test_config.max_iterations


class TestAnalyzeFile:
    """Tests for analyze_file convenience function."""

    @pytest.mark.asyncio
    async def test_analyze_file_mock(self, sample_cobol_source, test_config):
        """Test analyze_file with mock agents."""
        result = await analyze_file(
            source_code=sample_cobol_source,
            file_name="TESTPROG.cbl",
            config=test_config,
            use_mock=True,
        )

        assert result is not None
        assert "decision" in result


class TestPreprocessNode:
    """Tests for preprocess node."""

    @pytest.fixture
    def graph(self, test_config) -> WarRigGraph:
        """Create graph instance."""
        return WarRigGraph(test_config)

    @pytest.mark.asyncio
    async def test_preprocess_detects_cobol(self, graph, sample_cobol_source):
        """Test preprocessing detects COBOL files."""
        state = create_initial_state(
            source_code=sample_cobol_source,
            file_name="TEST.cbl",
        )

        result = await graph.nodes.preprocess(state)

        assert result["file_type"] == FileType.COBOL
        assert result["preprocessor_result"] is not None
        assert result["preprocessor_result"].program_id == "TESTPROG"

    @pytest.mark.asyncio
    async def test_preprocess_detects_jcl(self, graph, sample_jcl_source):
        """Test preprocessing detects JCL files."""
        state = create_initial_state(
            source_code=sample_jcl_source,
            file_name="TEST.jcl",
        )

        result = await graph.nodes.preprocess(state)

        assert result["file_type"] == FileType.JCL
        assert result["preprocessor_result"] is not None
