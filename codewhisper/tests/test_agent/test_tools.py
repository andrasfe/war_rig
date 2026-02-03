"""Tests for agent tools.

This module tests:
- Tool input schemas
- search_skills tool
- load_skill tool
- search_code tool
- read_file tool
- Tool configuration
"""

from pathlib import Path
from unittest.mock import MagicMock

import pytest
from pydantic import ValidationError

from codewhisper.agent.tools import (
    CodeSearchInput,
    FileReadInput,
    SkillLoadInput,
    SkillSearchInput,
    configure_tools,
    get_all_tools,
    load_skill,
    read_file,
    search_code,
    search_skills,
)


class TestSkillSearchInput:
    """Tests for SkillSearchInput schema."""

    def test_valid_input(self) -> None:
        """Test valid skill search input."""
        input_data = SkillSearchInput(query="authorization")
        assert input_data.query == "authorization"
        assert input_data.limit == 5  # default

    def test_custom_limit(self) -> None:
        """Test skill search with custom limit."""
        input_data = SkillSearchInput(query="test", limit=10)
        assert input_data.limit == 10

    def test_limit_validation_min(self) -> None:
        """Test that limit has minimum of 1."""
        with pytest.raises(ValidationError):
            SkillSearchInput(query="test", limit=0)

    def test_limit_validation_max(self) -> None:
        """Test that limit has maximum of 20."""
        with pytest.raises(ValidationError):
            SkillSearchInput(query="test", limit=25)

    def test_query_required(self) -> None:
        """Test that query is required."""
        with pytest.raises(ValidationError):
            SkillSearchInput()  # type: ignore


class TestSkillLoadInput:
    """Tests for SkillLoadInput schema."""

    def test_valid_input(self) -> None:
        """Test valid skill load input."""
        input_data = SkillLoadInput(skill_name="cbpaup0c")
        assert input_data.skill_name == "cbpaup0c"

    def test_skill_name_required(self) -> None:
        """Test that skill_name is required."""
        with pytest.raises(ValidationError):
            SkillLoadInput()  # type: ignore


class TestCodeSearchInput:
    """Tests for CodeSearchInput schema."""

    def test_valid_input(self) -> None:
        """Test valid code search input."""
        input_data = CodeSearchInput(pattern="PERFORM.*PROCESS")
        assert input_data.pattern == "PERFORM.*PROCESS"
        assert input_data.file_pattern == "*"
        assert input_data.context_lines == 3

    def test_custom_file_pattern(self) -> None:
        """Test code search with custom file pattern."""
        input_data = CodeSearchInput(
            pattern="CALL",
            file_pattern="*.cbl",
        )
        assert input_data.file_pattern == "*.cbl"

    def test_custom_context_lines(self) -> None:
        """Test code search with custom context lines."""
        input_data = CodeSearchInput(
            pattern="test",
            context_lines=5,
        )
        assert input_data.context_lines == 5

    def test_context_lines_validation_min(self) -> None:
        """Test that context_lines has minimum of 0."""
        input_data = CodeSearchInput(pattern="test", context_lines=0)
        assert input_data.context_lines == 0

    def test_context_lines_validation_max(self) -> None:
        """Test that context_lines has maximum of 10."""
        with pytest.raises(ValidationError):
            CodeSearchInput(pattern="test", context_lines=15)


class TestFileReadInput:
    """Tests for FileReadInput schema."""

    def test_valid_input(self) -> None:
        """Test valid file read input."""
        input_data = FileReadInput(file_path="cbl/TEST.cbl")
        assert input_data.file_path == "cbl/TEST.cbl"
        assert input_data.max_lines == 500

    def test_custom_max_lines(self) -> None:
        """Test file read with custom max lines."""
        input_data = FileReadInput(file_path="test.cbl", max_lines=1000)
        assert input_data.max_lines == 1000

    def test_max_lines_validation_min(self) -> None:
        """Test that max_lines has minimum of 1."""
        with pytest.raises(ValidationError):
            FileReadInput(file_path="test.cbl", max_lines=0)

    def test_max_lines_validation_max(self) -> None:
        """Test that max_lines has maximum of 2000."""
        with pytest.raises(ValidationError):
            FileReadInput(file_path="test.cbl", max_lines=3000)


class TestConfigureTools:
    """Tests for configure_tools function."""

    def test_configure_tools(self) -> None:
        """Test that configure_tools sets module-level references."""
        mock_index = MagicMock()
        mock_config = MagicMock()

        configure_tools(mock_index, mock_config)

        # Import the module variables to check they're set
        from codewhisper.agent import tools

        assert tools._skills_index is mock_index
        assert tools._config is mock_config


class TestGetAllTools:
    """Tests for get_all_tools function."""

    def test_get_all_tools_returns_list(self) -> None:
        """Test that get_all_tools returns a list of tools."""
        tools = get_all_tools()

        assert isinstance(tools, list)
        assert len(tools) == 4  # search_skills, load_skill, search_code, read_file

    def test_get_all_tools_contains_expected_tools(self) -> None:
        """Test that all expected tools are returned."""
        tools = get_all_tools()
        tool_names = [t.name for t in tools]

        assert "search_skills" in tool_names
        assert "load_skill" in tool_names
        assert "search_code" in tool_names
        assert "read_file" in tool_names


class TestSearchSkillsTool:
    """Tests for search_skills tool."""

    def test_search_skills_no_index(self) -> None:
        """Test search_skills returns error when index not configured."""
        from codewhisper.agent import tools

        tools._skills_index = None

        result = search_skills.invoke({"query": "test query"})

        assert "error" in result.lower()
        assert "not configured" in result.lower()

    def test_search_skills_with_index(self) -> None:
        """Test search_skills with configured index."""
        from codewhisper.agent import tools

        mock_index = MagicMock()
        mock_index.search.return_value = []
        tools._skills_index = mock_index

        result = search_skills.invoke({"query": "authorization", "limit": 3})

        # With empty results, should say no skills found
        assert "no skills found" in result.lower() or "authorization" in result.lower()


class TestLoadSkillTool:
    """Tests for load_skill tool."""

    def test_load_skill_no_index(self) -> None:
        """Test load_skill returns error when index not configured."""
        from codewhisper.agent import tools

        tools._skills_index = None

        result = load_skill.invoke({"skill_name": "cbpaup0c"})

        assert "error" in result.lower()
        assert "not configured" in result.lower()

    def test_load_skill_with_index(self) -> None:
        """Test load_skill with configured index."""
        from codewhisper.agent import tools

        mock_index = MagicMock()
        mock_index.get.return_value = None
        mock_index.list_names.return_value = []
        tools._skills_index = mock_index

        result = load_skill.invoke({"skill_name": "system-overview"})

        # With no skill found, should say not found
        assert "not found" in result.lower() or "system-overview" in result.lower()


class TestSearchCodeTool:
    """Tests for search_code tool."""

    def test_search_code_no_config(self) -> None:
        """Test search_code returns error when config not set."""
        from codewhisper.agent import tools

        tools._config = None

        result = search_code.invoke({"pattern": "PERFORM"})

        assert "error" in result.lower()
        assert "not set" in result.lower()

    def test_search_code_with_config(self, tmp_path: Path) -> None:
        """Test search_code with configured config."""
        from codewhisper.agent import tools

        mock_config = MagicMock()
        mock_config.code_dir = tmp_path  # Use real path
        tools._config = mock_config

        result = search_code.invoke({"pattern": "PERFORM", "file_pattern": "*.cbl"})

        # With empty directory, no matches expected
        assert "no matches" in result.lower() or "PERFORM" in result


class TestReadFileTool:
    """Tests for read_file tool."""

    def test_read_file_no_config(self) -> None:
        """Test read_file returns error when config not set."""
        from codewhisper.agent import tools

        tools._config = None

        result = read_file.invoke({"file_path": "test.cbl"})

        assert "error" in result.lower()
        assert "not set" in result.lower()

    def test_read_file_not_found(self, tmp_path: Path) -> None:
        """Test read_file returns error when file doesn't exist."""
        from codewhisper.agent import tools

        mock_config = MagicMock()
        mock_config.code_dir = tmp_path
        tools._config = mock_config

        result = read_file.invoke({"file_path": "nonexistent.cbl"})

        assert "not found" in result.lower()

    def test_read_file_success(self, tmp_path: Path) -> None:
        """Test read_file successfully reads a file."""
        from codewhisper.agent import tools

        # Create a test file
        test_file = tmp_path / "test.cbl"
        test_file.write_text("       IDENTIFICATION DIVISION.\n       PROGRAM-ID. TEST.")

        mock_config = MagicMock()
        mock_config.code_dir = tmp_path
        tools._config = mock_config

        result = read_file.invoke({"file_path": "test.cbl"})

        assert "IDENTIFICATION DIVISION" in result
        assert "PROGRAM-ID" in result
        # Should have line numbers
        assert "1" in result or "   1" in result

    def test_read_file_truncation(self, tmp_path: Path) -> None:
        """Test read_file truncates large files."""
        from codewhisper.agent import tools

        # Create a file with many lines
        lines = [f"       LINE {i}" for i in range(100)]
        test_file = tmp_path / "large.cbl"
        test_file.write_text("\n".join(lines))

        mock_config = MagicMock()
        mock_config.code_dir = tmp_path
        tools._config = mock_config

        result = read_file.invoke({"file_path": "large.cbl", "max_lines": 10})

        # Should be truncated
        assert "LINE 0" in result
        assert "LINE 9" in result
        # Should indicate truncation
        assert "truncated" in result.lower()
