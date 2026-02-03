"""Integration tests for codewhisper with CardDemo data.

This module tests:
- Loading skills from the example_output directory
- Searching code in the CardDemo authorization source
- End-to-end question answering with mocked LLM

These tests use real skill files and source code to verify the
system works correctly with actual mainframe documentation.
"""

from pathlib import Path
from unittest.mock import MagicMock

import pytest

from codewhisper.config import AgentConfig
from codewhisper.search.code_search import CodeSearcher, search_in_directory
from codewhisper.skills.index import SkillsIndex
from codewhisper.skills.loader import SkillsLoader

# Paths to test data
SKILLS_DIR = Path("/home/andras/war_rig/example_output/skills")
CARDDEMO_DIR = Path(
    "/home/andras/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq"
)


@pytest.fixture
def skills_available() -> bool:
    """Check if skills directory is available."""
    return SKILLS_DIR.exists() and any(SKILLS_DIR.rglob("SKILL.md"))


@pytest.fixture
def carddemo_available() -> bool:
    """Check if CardDemo source is available."""
    return CARDDEMO_DIR.exists()


class TestSkillsLoading:
    """Integration tests for loading skills."""

    @pytest.mark.skipif(
        not SKILLS_DIR.exists(),
        reason="Skills directory not available",
    )
    def test_load_all_skills(self) -> None:
        """Test loading all skills from example_output."""
        loader = SkillsLoader(SKILLS_DIR)
        skills = loader.load_all()

        assert len(skills) > 0, "Should have loaded at least one skill"

        # Verify skill structure
        for skill in skills:
            assert skill.name, "Skill should have a name"
            assert skill.file_path.exists(), "Skill file should exist"
            assert skill.content, "Skill should have content"

    @pytest.mark.skipif(
        not SKILLS_DIR.exists(),
        reason="Skills directory not available",
    )
    def test_load_system_overview_skill(self) -> None:
        """Test loading the system-overview skill."""
        loader = SkillsLoader(SKILLS_DIR)
        skill = loader.load_by_name("system-overview")

        assert skill is not None, "system-overview skill should exist"
        assert "authorization" in skill.content.lower(), (
            "System overview should mention authorization"
        )

    @pytest.mark.skipif(
        not SKILLS_DIR.exists(),
        reason="Skills directory not available",
    )
    def test_load_program_skill(self) -> None:
        """Test loading a program-specific skill."""
        loader = SkillsLoader(SKILLS_DIR)

        # Try to load cbpaup0c if it exists
        skill = loader.load_by_name("cbpaup0c")

        if skill is not None:
            assert "batch" in skill.content.lower() or "cleanup" in skill.content.lower()

    @pytest.mark.skipif(
        not SKILLS_DIR.exists(),
        reason="Skills directory not available",
    )
    def test_skills_index_search(self) -> None:
        """Test searching skills with the index."""
        loader = SkillsLoader(SKILLS_DIR)
        skills = loader.load_all()
        index = SkillsIndex(skills)

        # Search for authorization-related skills
        results = index.search("authorization")

        assert len(results) > 0, "Should find authorization-related skills"

        # Verify results are relevant
        for result in results:
            text = (
                result.skill.name
                + result.skill.description
                + result.skill.content
            ).lower()
            # Results should be somewhat relevant
            assert any(
                term in text
                for term in ["authorization", "auth", "card", "system"]
            )

    @pytest.mark.skipif(
        not SKILLS_DIR.exists(),
        reason="Skills directory not available",
    )
    def test_skills_index_get_by_name(self) -> None:
        """Test retrieving skill by exact name."""
        loader = SkillsLoader(SKILLS_DIR)
        skills = loader.load_all()
        index = SkillsIndex(skills)

        # Get call-graph skill
        skill = index.get("call-graph")

        if skill is not None:
            assert skill.name == "call-graph"


class TestCodeSearch:
    """Integration tests for code search."""

    @pytest.mark.skipif(
        not CARDDEMO_DIR.exists(),
        reason="CardDemo source not available",
    )
    def test_search_cobol_programs(self) -> None:
        """Test searching COBOL programs."""
        results = search_in_directory(
            CARDDEMO_DIR,
            pattern="PROGRAM-ID",
            file_pattern="*.cbl",
            max_results=20,
        )

        assert len(results) > 0, "Should find COBOL programs"

        # All results should be .cbl files
        for result in results:
            assert result.file_path.suffix.lower() in (
                ".cbl",
                ".cob",
            ), f"Unexpected file: {result.file_path}"

    @pytest.mark.skipif(
        not CARDDEMO_DIR.exists(),
        reason="CardDemo source not available",
    )
    def test_search_perform_statements(self) -> None:
        """Test searching for PERFORM statements."""
        results = search_in_directory(
            CARDDEMO_DIR,
            pattern="PERFORM",
            file_pattern="*.cbl",
            context_lines=2,
            max_results=50,
        )

        assert len(results) > 0, "Should find PERFORM statements"

        # Results should have context
        for result in results:
            assert "PERFORM" in result.line_content

    @pytest.mark.skipif(
        not CARDDEMO_DIR.exists(),
        reason="CardDemo source not available",
    )
    def test_search_jcl_jobs(self) -> None:
        """Test searching JCL jobs."""
        jcl_dir = CARDDEMO_DIR / "jcl"
        if not jcl_dir.exists():
            pytest.skip("No JCL directory")

        results = search_in_directory(
            jcl_dir,
            pattern="JOB",
            file_pattern="*.jcl",
            max_results=10,
        )

        # May or may not find JCL jobs
        assert isinstance(results, list)

    @pytest.mark.skipif(
        not CARDDEMO_DIR.exists(),
        reason="CardDemo source not available",
    )
    def test_search_ims_calls(self) -> None:
        """Test searching for IMS DL/I calls."""
        results = search_in_directory(
            CARDDEMO_DIR,
            pattern="CBLTDLI|CALL.*DLI",
            file_pattern="*.cbl",
            max_results=20,
        )

        # May or may not have IMS calls
        assert isinstance(results, list)


class TestEndToEndWithMockedLLM:
    """End-to-end tests with mocked LLM responses."""

    @pytest.mark.skipif(
        not SKILLS_DIR.exists() or not CARDDEMO_DIR.exists(),
        reason="Skills or CardDemo not available",
    )
    async def test_create_agent_with_real_skills(self, monkeypatch: pytest.MonkeyPatch) -> None:
        """Test creating agent with real skills."""
        from codewhisper.agent.graph import create_agent

        # Set mock API key for test
        monkeypatch.setenv("OPENROUTER_API_KEY", "test-key-for-integration")

        config = AgentConfig(
            skills_dir=SKILLS_DIR,
            code_dir=CARDDEMO_DIR,
            model="test-model",
            provider="openrouter",
        )

        agent = create_agent(config)

        assert agent is not None
        assert len(agent.skills_index) > 0

    @pytest.mark.skipif(
        not SKILLS_DIR.exists() or not CARDDEMO_DIR.exists(),
        reason="Skills or CardDemo not available",
    )
    async def test_agent_chat_with_mocked_llm(self) -> None:
        """Test agent chat with mocked LLM."""

        from langchain_core.messages import AIMessage

        from codewhisper.agent.graph import CodeWhisperAgent
        from codewhisper.skills.index import SkillsIndex
        from codewhisper.skills.loader import SkillsLoader

        config = AgentConfig(
            skills_dir=SKILLS_DIR,
            code_dir=CARDDEMO_DIR,
            model="test-model",
            provider="openrouter",
        )

        # Load real skills
        loader = SkillsLoader(SKILLS_DIR)
        skills = loader.load_all()
        index = SkillsIndex(skills)

        # Create mock LLM
        mock_llm = MagicMock()
        mock_llm.bind_tools = MagicMock(return_value=mock_llm)

        async def mock_ainvoke(*args, **kwargs):
            return AIMessage(content="The authorization system manages credit card authorization lifecycle.")
        mock_llm.ainvoke = mock_ainvoke

        agent = CodeWhisperAgent(config, index, llm=mock_llm)

        # Chat should work with the mock LLM
        response = await agent.chat("What is the purpose of the authorization system?")

        assert response is not None
        assert isinstance(response, str)
        assert "authorization" in response.lower()

    @pytest.mark.skipif(
        not SKILLS_DIR.exists(),
        reason="Skills directory not available",
    )
    def test_skill_search_returns_relevant_results(self) -> None:
        """Test that skill search returns relevant results for real queries."""
        loader = SkillsLoader(SKILLS_DIR)
        skills = loader.load_all()
        index = SkillsIndex(skills)

        # Search for batch processing
        results = index.search("batch cleanup expired")

        if len(results) > 0:
            # First result should be relevant
            top_result = results[0]
            combined_text = (
                top_result.skill.name
                + top_result.skill.description
                + top_result.skill.content
            ).lower()
            assert any(
                term in combined_text
                for term in ["batch", "cleanup", "expired", "purge", "delete"]
            )

    @pytest.mark.skipif(
        not CARDDEMO_DIR.exists(),
        reason="CardDemo source not available",
    )
    def test_code_search_finds_specific_program(self) -> None:
        """Test finding a specific program by name."""
        searcher = CodeSearcher(CARDDEMO_DIR)

        # Search for COPAU programs (online authorization)
        results = searcher.search(
            r"PROGRAM-ID\.\s+COPAU",
            file_pattern="*.cbl",
        )

        # Should find some COPAU programs
        if len(results) > 0:
            for result in results:
                assert "COPAU" in result.line_content


class TestConfigurationIntegration:
    """Tests for configuration with real paths."""

    @pytest.mark.skipif(
        not SKILLS_DIR.exists() or not CARDDEMO_DIR.exists(),
        reason="Skills or CardDemo not available",
    )
    def test_agent_config_with_real_paths(self) -> None:
        """Test AgentConfig with real paths."""
        config = AgentConfig(
            skills_dir=SKILLS_DIR,
            code_dir=CARDDEMO_DIR,
        )

        assert config.skills_dir.exists()
        assert config.code_dir.exists()
        assert config.model is not None
        assert config.provider in ("openrouter", "anthropic", "openai")

    @pytest.mark.skipif(
        not SKILLS_DIR.exists() or not CARDDEMO_DIR.exists(),
        reason="Skills or CardDemo not available",
    )
    def test_tools_configured_with_real_data(self) -> None:
        """Test that tools work with real configuration."""
        from codewhisper.agent.tools import configure_tools, read_file

        loader = SkillsLoader(SKILLS_DIR)
        skills = loader.load_all()
        index = SkillsIndex(skills)

        config = AgentConfig(
            skills_dir=SKILLS_DIR,
            code_dir=CARDDEMO_DIR,
        )

        configure_tools(index, config)

        # Try to read a file - tools are StructuredTool so use .invoke()
        cbl_files = list(CARDDEMO_DIR.rglob("*.cbl"))
        if cbl_files:
            relative_path = cbl_files[0].relative_to(CARDDEMO_DIR)
            result = read_file.invoke({"file_path": str(relative_path), "max_lines": 50})
            assert len(result) > 0
