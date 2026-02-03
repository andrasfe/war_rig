"""Tests for codewhisper configuration models.

This module tests:
- AgentConfig validation and defaults
- SkillMetadata model
- SearchResult model
"""

from pathlib import Path

import pytest
from pydantic import ValidationError

from codewhisper.config import AgentConfig, SearchResult, SkillMetadata


class TestAgentConfig:
    """Tests for the AgentConfig model."""

    def test_create_agent_config_defaults(self, tmp_path: Path) -> None:
        """Test creating agent config with defaults."""
        skills_dir = tmp_path / "skills"
        skills_dir.mkdir()
        code_dir = tmp_path / "code"
        code_dir.mkdir()

        config = AgentConfig(
            skills_dir=skills_dir,
            code_dir=code_dir,
        )

        assert config.skills_dir == skills_dir
        assert config.code_dir == code_dir
        assert config.model == "anthropic/claude-sonnet-4-20250514"
        assert config.provider == "openrouter"
        assert config.temperature == 0.3
        assert config.max_history == 20
        assert config.verbose is False

    def test_create_agent_config_custom(self, tmp_path: Path) -> None:
        """Test creating agent config with custom values."""
        skills_dir = tmp_path / "skills"
        skills_dir.mkdir()
        code_dir = tmp_path / "code"
        code_dir.mkdir()

        config = AgentConfig(
            skills_dir=skills_dir,
            code_dir=code_dir,
            model="openai/gpt-4o",
            provider="anthropic",
            temperature=0.5,
            max_history=50,
            verbose=True,
        )

        assert config.model == "openai/gpt-4o"
        assert config.provider == "anthropic"
        assert config.temperature == 0.5
        assert config.max_history == 50
        assert config.verbose is True

    def test_temperature_validation_min(self, tmp_path: Path) -> None:
        """Test that temperature has a minimum of 0.0."""
        skills_dir = tmp_path / "skills"
        skills_dir.mkdir()
        code_dir = tmp_path / "code"
        code_dir.mkdir()

        with pytest.raises(ValidationError) as exc_info:
            AgentConfig(
                skills_dir=skills_dir,
                code_dir=code_dir,
                temperature=-0.1,
            )
        assert "temperature" in str(exc_info.value).lower()

    def test_temperature_validation_max(self, tmp_path: Path) -> None:
        """Test that temperature has a maximum of 2.0."""
        skills_dir = tmp_path / "skills"
        skills_dir.mkdir()
        code_dir = tmp_path / "code"
        code_dir.mkdir()

        with pytest.raises(ValidationError) as exc_info:
            AgentConfig(
                skills_dir=skills_dir,
                code_dir=code_dir,
                temperature=2.5,
            )
        assert "temperature" in str(exc_info.value).lower()

    def test_max_history_must_be_positive(self, tmp_path: Path) -> None:
        """Test that max_history must be positive."""
        skills_dir = tmp_path / "skills"
        skills_dir.mkdir()
        code_dir = tmp_path / "code"
        code_dir.mkdir()

        with pytest.raises(ValidationError) as exc_info:
            AgentConfig(
                skills_dir=skills_dir,
                code_dir=code_dir,
                max_history=0,
            )
        assert "max_history" in str(exc_info.value).lower()

    def test_config_env_prefix(self) -> None:
        """Test that environment prefix is CODEWHISPER_."""
        assert AgentConfig.model_config.get("env_prefix") == "CODEWHISPER_"


class TestSkillMetadata:
    """Tests for the SkillMetadata model."""

    def test_create_skill_metadata(self) -> None:
        """Test creating skill metadata."""
        metadata = SkillMetadata(
            name="test-skill",
            description="A test skill",
            file_path=Path("/path/to/skill/SKILL.md"),
            content="# Test Skill\n\nContent here.",
        )

        assert metadata.name == "test-skill"
        assert metadata.description == "A test skill"
        assert metadata.file_path == Path("/path/to/skill/SKILL.md")
        assert metadata.content == "# Test Skill\n\nContent here."

    def test_skill_metadata_name_required(self) -> None:
        """Test that name is required."""
        with pytest.raises(ValidationError):
            SkillMetadata(  # type: ignore
                description="A test skill",
                file_path=Path("/path/to/skill/SKILL.md"),
            )

    def test_skill_metadata_file_path_required(self) -> None:
        """Test that file_path is required."""
        with pytest.raises(ValidationError):
            SkillMetadata(  # type: ignore
                name="test-skill",
                description="A test skill",
            )

    def test_skill_metadata_defaults(self) -> None:
        """Test skill metadata default values."""
        metadata = SkillMetadata(
            name="test-skill",
            file_path=Path("/path/to/skill/SKILL.md"),
        )

        assert metadata.description == ""
        assert metadata.content == ""


class TestSearchResult:
    """Tests for the SearchResult model."""

    def test_create_search_result(self) -> None:
        """Test creating search result."""
        result = SearchResult(
            file_path=Path("cbl/TEST.cbl"),
            line_number=42,
            line_content="       PERFORM 1000-PROCESS",
        )

        assert result.file_path == Path("cbl/TEST.cbl")
        assert result.line_number == 42
        assert result.line_content == "       PERFORM 1000-PROCESS"
        assert result.context_before == []
        assert result.context_after == []

    def test_search_result_with_context(self) -> None:
        """Test search result with context lines."""
        result = SearchResult(
            file_path=Path("cbl/TEST.cbl"),
            line_number=10,
            line_content="       MAIN-PARA.",
            context_before=["      *", "       PROCEDURE DIVISION."],
            context_after=["           PERFORM INIT", "           STOP RUN."],
        )

        assert len(result.context_before) == 2
        assert len(result.context_after) == 2
        assert "PROCEDURE DIVISION" in result.context_before[1]

    def test_search_result_line_number_must_be_positive(self) -> None:
        """Test that line_number must be positive."""
        with pytest.raises(ValidationError) as exc_info:
            SearchResult(
                file_path=Path("test.cbl"),
                line_number=0,
                line_content="test",
            )
        assert "line_number" in str(exc_info.value).lower()

    def test_search_result_required_fields(self) -> None:
        """Test that required fields are enforced."""
        with pytest.raises(ValidationError):
            SearchResult(  # type: ignore
                line_number=1,
                line_content="test",
            )

        with pytest.raises(ValidationError):
            SearchResult(  # type: ignore
                file_path=Path("test.cbl"),
                line_content="test",
            )

        with pytest.raises(ValidationError):
            SearchResult(  # type: ignore
                file_path=Path("test.cbl"),
                line_number=1,
            )
