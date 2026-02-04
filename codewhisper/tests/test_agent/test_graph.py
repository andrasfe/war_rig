"""Tests for agent graph definition.

This module tests:
- CodeWhisperAgent initialization
- Graph building and compilation
- Agent chat interface
- State transitions
"""

from pathlib import Path
from unittest.mock import MagicMock, patch

import pytest

from codewhisper.agent.graph import (
    SYSTEM_PROMPT,
    CodeWhisperAgent,
    create_agent,
)
from codewhisper.agent.langchain_factory import get_langchain_model
from codewhisper.config import AgentConfig
from codewhisper.skills.index import SkillsIndex
from codewhisper.skills.loader import Skill


class TestSystemPrompt:
    """Tests for the system prompt."""

    def test_system_prompt_exists(self) -> None:
        """Test that system prompt is defined."""
        assert SYSTEM_PROMPT is not None
        assert len(SYSTEM_PROMPT) > 100  # Should be substantial

    def test_system_prompt_contains_key_capabilities(self) -> None:
        """Test that system prompt describes key capabilities."""
        prompt_lower = SYSTEM_PROMPT.lower()
        assert "skill" in prompt_lower
        assert "search" in prompt_lower
        assert "code" in prompt_lower

    def test_system_prompt_mentions_mainframe(self) -> None:
        """Test that system prompt mentions mainframe context."""
        prompt_lower = SYSTEM_PROMPT.lower()
        assert "mainframe" in prompt_lower or "cobol" in prompt_lower


class TestCodeWhisperAgentInit:
    """Tests for CodeWhisperAgent initialization."""

    @pytest.fixture
    def mock_skills_index(self) -> SkillsIndex:
        """Create a mock skills index."""
        skills = [
            Skill(
                name="test-skill",
                description="Test skill",
                content="# Test",
                file_path=Path("/tmp/skill.md"),
            )
        ]
        return SkillsIndex(skills)

    @pytest.fixture
    def mock_config(self, tmp_path: Path) -> AgentConfig:
        """Create a mock agent config."""
        skills_dir = tmp_path / "skills"
        skills_dir.mkdir()
        code_dir = tmp_path / "code"
        code_dir.mkdir()

        return AgentConfig(
            skills_dir=skills_dir,
            code_dir=code_dir,
            model="test-model",
            provider="openrouter",
        )

    def test_agent_init(
        self,
        mock_config: AgentConfig,
        mock_skills_index: SkillsIndex,
    ) -> None:
        """Test agent initialization."""
        with patch(
            "codewhisper.agent.graph.get_langchain_model"
        ) as mock_get_model:
            mock_get_model.return_value = MagicMock()
            agent = CodeWhisperAgent(mock_config, mock_skills_index)

            assert agent.config is mock_config
            assert agent.skills_index is mock_skills_index
            # LLM is eagerly initialized if not provided
            assert agent._llm is not None
            # Graph is lazily built
            assert agent._graph is None

    def test_agent_init_with_llm(
        self,
        mock_config: AgentConfig,
        mock_skills_index: SkillsIndex,
    ) -> None:
        """Test agent initialization with custom LLM."""
        mock_llm = MagicMock()

        agent = CodeWhisperAgent(mock_config, mock_skills_index, llm=mock_llm)

        assert agent._llm is mock_llm

    def test_agent_configures_tools(
        self,
        mock_config: AgentConfig,
        mock_skills_index: SkillsIndex,
    ) -> None:
        """Test that agent initialization configures tools."""
        with patch("codewhisper.agent.graph.configure_tools") as mock_configure:
            with patch(
                "codewhisper.agent.graph.get_langchain_model"
            ) as mock_get_model:
                mock_get_model.return_value = MagicMock()
                CodeWhisperAgent(mock_config, mock_skills_index)

        mock_configure.assert_called_once_with(mock_skills_index, mock_config)


class TestCodeWhisperAgentGraph:
    """Tests for agent graph building."""

    @pytest.fixture
    def agent(self, tmp_path: Path) -> CodeWhisperAgent:
        """Create an agent for testing."""
        skills_dir = tmp_path / "skills"
        skills_dir.mkdir()
        code_dir = tmp_path / "code"
        code_dir.mkdir()

        config = AgentConfig(
            skills_dir=skills_dir,
            code_dir=code_dir,
        )
        skills = [
            Skill(
                name="test",
                description="Test",
                content="Test",
                file_path=Path("/tmp/skill.md"),
            )
        ]
        index = SkillsIndex(skills)

        # Create a mock LLM with bind_tools support
        mock_llm = MagicMock()
        mock_llm.bind_tools = MagicMock(return_value=mock_llm)

        return CodeWhisperAgent(config, index, llm=mock_llm)

    def test_graph_property_builds_on_first_access(
        self,
        agent: CodeWhisperAgent,
    ) -> None:
        """Test that graph is built on first access."""
        assert agent._graph is None

        graph = agent.graph

        assert graph is not None
        assert agent._graph is graph

    def test_graph_property_caches_result(
        self,
        agent: CodeWhisperAgent,
    ) -> None:
        """Test that graph is cached after first build."""
        graph1 = agent.graph
        graph2 = agent.graph

        assert graph1 is graph2

    def test_graph_is_compiled(
        self,
        agent: CodeWhisperAgent,
    ) -> None:
        """Test that graph is compiled and can be invoked."""
        graph = agent.graph

        # CompiledStateGraph should have invoke method
        assert hasattr(graph, "invoke") or hasattr(graph, "ainvoke")


class TestCodeWhisperAgentChat:
    """Tests for agent chat interface."""

    @pytest.fixture
    def agent(self, tmp_path: Path) -> CodeWhisperAgent:
        """Create an agent for testing with mock LLM."""
        from langchain_core.messages import AIMessage

        skills_dir = tmp_path / "skills"
        skills_dir.mkdir()
        code_dir = tmp_path / "code"
        code_dir.mkdir()

        config = AgentConfig(
            skills_dir=skills_dir,
            code_dir=code_dir,
        )
        skills = [
            Skill(
                name="test",
                description="Test",
                content="Test",
                file_path=Path("/tmp/skill.md"),
            )
        ]
        index = SkillsIndex(skills)

        # Create mock LLM that returns a simple AIMessage
        mock_llm = MagicMock()
        mock_llm.bind_tools = MagicMock(return_value=mock_llm)
        mock_llm.ainvoke = MagicMock(return_value=AIMessage(content="Test response"))

        # Make ainvoke actually async
        async def mock_ainvoke(*args, **kwargs):
            return AIMessage(content="Test response")
        mock_llm.ainvoke = mock_ainvoke

        return CodeWhisperAgent(config, index, llm=mock_llm)

    async def test_chat_creates_initial_state(
        self,
        agent: CodeWhisperAgent,
    ) -> None:
        """Test that chat creates initial state with user message."""
        response = await agent.chat("What does TESTPROG do?")

        assert response is not None
        assert isinstance(response, str)
        assert "Test response" in response

    async def test_chat_returns_assistant_response(
        self,
        agent: CodeWhisperAgent,
    ) -> None:
        """Test that chat returns the assistant's response."""
        response = await agent.chat("Hello")

        # Should get a response string
        assert response is not None
        assert len(response) > 0


class TestCodeWhisperAgentReset:
    """Tests for agent reset functionality."""

    @pytest.fixture
    def agent(self, tmp_path: Path) -> CodeWhisperAgent:
        """Create an agent for testing with mock LLM."""
        skills_dir = tmp_path / "skills"
        skills_dir.mkdir()
        code_dir = tmp_path / "code"
        code_dir.mkdir()

        config = AgentConfig(
            skills_dir=skills_dir,
            code_dir=code_dir,
        )
        skills = []
        index = SkillsIndex(skills)

        from langchain_core.messages import AIMessage

        # Create mock LLM with proper async support
        mock_llm = MagicMock()
        mock_llm.bind_tools = MagicMock(return_value=mock_llm)

        async def mock_ainvoke(*args, **kwargs):
            return AIMessage(content="Test response")
        mock_llm.ainvoke = mock_ainvoke

        return CodeWhisperAgent(config, index, llm=mock_llm)

    async def test_reset_clears_state(
        self,
        agent: CodeWhisperAgent,
    ) -> None:
        """Test that reset clears conversation state."""
        # Have a conversation first
        await agent.chat("Hello")

        # Verify we have conversation state
        assert agent._conversation_state is not None

        # Reset
        await agent.reset()

        # Verify state is cleared
        assert agent._conversation_state is None


class TestCreateAgent:
    """Tests for create_agent factory function."""

    def test_create_agent_loads_skills(self, tmp_skills_dir: Path, tmp_code_dir: Path) -> None:
        """Test that create_agent loads skills from directory."""
        config = AgentConfig(
            skills_dir=tmp_skills_dir,
            code_dir=tmp_code_dir,
        )

        with patch(
            "codewhisper.agent.graph.get_langchain_model"
        ) as mock_get_model:
            mock_get_model.return_value = MagicMock()
            agent = create_agent(config)

            assert agent is not None
            assert isinstance(agent, CodeWhisperAgent)
            # Skills should be loaded
            assert len(agent.skills_index) > 0

    def test_create_agent_returns_agent_instance(
        self, tmp_skills_dir: Path, tmp_code_dir: Path
    ) -> None:
        """Test that create_agent returns a CodeWhisperAgent."""
        config = AgentConfig(
            skills_dir=tmp_skills_dir,
            code_dir=tmp_code_dir,
        )

        with patch(
            "codewhisper.agent.graph.get_langchain_model"
        ) as mock_get_model:
            mock_get_model.return_value = MagicMock()
            agent = create_agent(config)

            assert isinstance(agent, CodeWhisperAgent)
            assert agent.config is config
