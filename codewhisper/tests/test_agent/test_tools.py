"""Tests for agent tools.

This module tests:
- Tool input schemas
- search_skills tool
- load_skill tool
- search_code tool
- read_file tool
- Tool configuration
"""

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
        # 4 knowledge tools + 13 citadel tools = 17 total
        assert len(tools) == 17

    def test_get_all_tools_contains_expected_knowledge_tools(self) -> None:
        """Test that all expected knowledge tools are returned."""
        tools = get_all_tools()
        tool_names = [t.name for t in tools]

        assert "search_skills" in tool_names
        assert "load_skill" in tool_names
        assert "search_code" in tool_names
        assert "read_file" in tool_names

    def test_get_all_tools_contains_expected_citadel_tools(self) -> None:
        """Test that all expected citadel analysis tools are returned."""
        tools = get_all_tools()
        tool_names = [t.name for t in tools]

        # All 13 citadel tools should be present
        expected_citadel_tools = [
            "citadel_analyze_file",
            "citadel_get_functions",
            "citadel_get_callouts",
            "citadel_get_includes",
            "citadel_get_function_body",
            "citadel_get_function_bodies",
            "citadel_get_file_stats",
            "citadel_get_callers",
            "citadel_get_sequence_diagrams",
            "citadel_get_dead_code",
            "citadel_get_flow_diagram",
            "citadel_get_file_summary",
            "citadel_get_analysis_patterns",
        ]
        for tool_name in expected_citadel_tools:
            assert tool_name in tool_names, f"Missing citadel tool: {tool_name}"
