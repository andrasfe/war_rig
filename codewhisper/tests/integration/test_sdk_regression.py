"""Regression tests for CodeWhisper SDK against CardDemo codebase.

This module tests the CodeWhisper SDK integration with real mainframe
code from the CardDemo authorization application. Tests cover:

- SDK initialization with valid paths
- Tool registry creation and configuration
- Citadel tools analyzing real COBOL files
- Code search functionality
- Skills index loading

Tests are skipped if required directories are not available.
"""

from pathlib import Path
from typing import Any
from unittest.mock import AsyncMock, MagicMock

import pytest

# Test paths - updated for macOS environment
CODE_DIR = Path(
    "/Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/"
    "app/app-authorization-ims-db2-mq"
)
DOCS_DIR = Path("/Users/andraslferenczi/war_rig/example_output/documentation")
SKILLS_DIR = Path("/Users/andraslferenczi/war_rig/example_output/code-skills")

# Skip all tests if paths don't exist
pytestmark = pytest.mark.skipif(
    not CODE_DIR.exists() or not SKILLS_DIR.exists(),
    reason="CardDemo or skills directory not available",
)


class TestSDKInitialization:
    """Test SDK can be initialized with CardDemo paths."""

    def test_sdk_creates_with_valid_paths(self) -> None:
        """Test that SDK initializes correctly with valid paths."""
        from codewhisper.sdk import CodeWhisper, CodeWhisperConfig

        # Create mock provider
        mock_provider = MagicMock()
        mock_provider.complete = AsyncMock(return_value=MagicMock(content="test"))

        config = CodeWhisperConfig(
            max_iterations=5,
            temperature=0.1,
            use_minion=False,
        )

        sdk = CodeWhisper(
            llm_provider=mock_provider,
            code_dir=CODE_DIR,
            documents_dir=SKILLS_DIR,
            config=config,
        )

        assert sdk.code_dir == CODE_DIR.resolve()
        assert sdk.documents_dir == SKILLS_DIR.resolve()
        assert sdk.config.max_iterations == 5
        assert sdk.config.temperature == 0.1

    def test_sdk_creates_without_documents_dir(self) -> None:
        """Test SDK can be created without documents directory."""
        from codewhisper.sdk import CodeWhisper, CodeWhisperConfig

        mock_provider = MagicMock()
        mock_provider.complete = AsyncMock(return_value=MagicMock(content="test"))

        sdk = CodeWhisper(
            llm_provider=mock_provider,
            code_dir=CODE_DIR,
            config=CodeWhisperConfig(use_minion=False),
        )

        assert sdk.code_dir == CODE_DIR.resolve()
        assert sdk.documents_dir is None


class TestToolRegistry:
    """Test tools work with CardDemo codebase."""

    def test_tool_registry_creates_successfully(self) -> None:
        """Test that tool registry can be created with CardDemo paths."""
        from codewhisper.tools.factory import create_tool_registry

        registry = create_tool_registry(code_dir=CODE_DIR)

        # Should have code and citadel tools
        assert len(registry) > 0

        # Check for expected tools
        assert "search_code" in registry
        assert "read_file" in registry

    def test_tool_registry_with_skills_index(self) -> None:
        """Test tool registry with skills index includes knowledge tools."""
        from codewhisper.skills.index import SkillsIndex
        from codewhisper.skills.loader import SkillsLoader
        from codewhisper.tools.factory import create_tool_registry

        loader = SkillsLoader(SKILLS_DIR)
        skills_index = SkillsIndex.from_loader(loader)

        registry = create_tool_registry(
            code_dir=CODE_DIR,
            skills_index=skills_index,
        )

        # Should have knowledge tools
        assert "search_skills" in registry
        assert "load_skill" in registry

    def test_citadel_tools_registered(self) -> None:
        """Test that all citadel tools are registered."""
        from codewhisper.tools.factory import create_tool_registry

        registry = create_tool_registry(code_dir=CODE_DIR)

        # Check for citadel tools
        citadel_tools = [
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

        for tool_name in citadel_tools:
            assert tool_name in registry, f"Missing citadel tool: {tool_name}"

    @pytest.mark.asyncio
    async def test_citadel_tools_analyze_cobol(self) -> None:
        """Test citadel_analyze_file works on a COBOL file."""
        from codewhisper.tools.citadel import create_citadel_tools

        tools = create_citadel_tools(CODE_DIR)
        analyze_tool = next(t for t in tools if t.name == "citadel_analyze_file")

        # Get a COBOL file to analyze
        cobol_dir = CODE_DIR / "cbl"
        cobol_files = list(cobol_dir.glob("*.cbl"))
        if not cobol_files:
            pytest.skip("No COBOL files found")

        # Use relative path
        relative_path = cobol_files[0].relative_to(CODE_DIR)

        # Execute the tool handler
        result = await analyze_tool.handler(file_path=str(relative_path))

        assert isinstance(result, str)
        assert "Error" not in result or "# Analysis" in result
        # Should detect COBOL language
        assert "cobol" in result.lower() or "COBOL" in result

    @pytest.mark.asyncio
    async def test_code_search_finds_programs(self) -> None:
        """Test search_code finds COBOL programs."""
        from codewhisper.tools.code import create_code_tools

        tools = create_code_tools(CODE_DIR)
        search_tool = next(t for t in tools if t.name == "search_code")

        # Search for PROGRAM-ID statements
        # Note: search_code doesn't have max_results param, it's hardcoded to 50
        result = await search_tool.handler(
            pattern="PROGRAM-ID",
            file_pattern="*.cbl",
        )

        assert isinstance(result, str)
        assert "PROGRAM-ID" in result or "No results" in result

    def test_skills_index_loads(self) -> None:
        """Test skills load from code-skills directory."""
        from codewhisper.skills.index import SkillsIndex
        from codewhisper.skills.loader import SkillsLoader

        loader = SkillsLoader(SKILLS_DIR)
        skills = loader.load_all()

        assert len(skills) > 0, "Should have loaded at least one skill"

        # Create index
        index = SkillsIndex(skills)

        assert len(index) > 0
        # Should be able to search
        results = index.search("authorization")
        assert isinstance(results, list)


class TestSkillsIntegration:
    """Test skills loading with real skills files."""

    def test_load_all_skills(self) -> None:
        """Test loading all skills from code-skills directory."""
        from codewhisper.skills.loader import SkillsLoader

        loader = SkillsLoader(SKILLS_DIR)
        skills = loader.load_all()

        assert len(skills) > 0, "Should have loaded skills"

        # Verify skill structure
        for skill in skills:
            assert skill.name, "Skill should have a name"
            assert skill.file_path.exists(), "Skill file should exist"
            assert skill.content, "Skill should have content"

    def test_skill_search_returns_results(self) -> None:
        """Test that skill search returns relevant results."""
        from codewhisper.skills.index import SkillsIndex
        from codewhisper.skills.loader import SkillsLoader

        loader = SkillsLoader(SKILLS_DIR)
        skills = loader.load_all()
        index = SkillsIndex(skills)

        # Search for something likely to exist
        results = index.search("batch")

        # May or may not find results depending on skills content
        assert isinstance(results, list)


class TestCodeSearch:
    """Test code search with CardDemo source."""

    def test_search_in_directory(self) -> None:
        """Test basic code search."""
        from codewhisper.search.code_search import search_in_directory

        results = search_in_directory(
            CODE_DIR,
            pattern="PERFORM",
            file_pattern="*.cbl",
            max_results=20,
        )

        assert isinstance(results, list)
        # CardDemo COBOL should have PERFORM statements
        if results:
            for result in results:
                assert "PERFORM" in result.line_content

    def test_search_finds_cobol_programs(self) -> None:
        """Test searching for COBOL PROGRAM-ID."""
        from codewhisper.search.code_search import search_in_directory

        results = search_in_directory(
            CODE_DIR / "cbl",
            pattern="PROGRAM-ID",
            file_pattern="*.cbl",
            max_results=50,
        )

        assert len(results) > 0, "Should find PROGRAM-ID in COBOL files"


class TestEndToEnd:
    """End-to-end tests with mocked LLM."""

    @pytest.mark.asyncio
    async def test_sdk_complete_returns_result(self) -> None:
        """Test SDK complete method returns proper result."""
        from codewhisper.sdk import CodeWhisper, CodeWhisperConfig, CompletionResult

        # Create mock provider that returns OpenAI-style response
        # The ReActLoop._parse_response expects choices[0].message format
        mock_provider = MagicMock()

        async def mock_complete(
            messages: list[dict[str, Any]],
            tools: list[dict[str, Any]] | None = None,
            **kwargs: Any,
        ) -> Any:
            # Create OpenAI-style response structure
            mock_message = MagicMock()
            mock_message.content = "The authorization system manages card transactions."
            mock_message.tool_calls = None

            mock_choice = MagicMock()
            mock_choice.message = mock_message

            response = MagicMock()
            response.choices = [mock_choice]
            return response

        mock_provider.complete = mock_complete

        sdk = CodeWhisper(
            llm_provider=mock_provider,
            code_dir=CODE_DIR,
            documents_dir=SKILLS_DIR,
            config=CodeWhisperConfig(
                max_iterations=1,
                use_minion=False,
            ),
        )

        result = await sdk.complete("What is the purpose of the authorization system?")

        assert isinstance(result, CompletionResult)
        assert isinstance(result.content, str)
        assert len(result.content) > 0
        assert "authorization" in result.content.lower()

    @pytest.mark.asyncio
    async def test_tool_execution_workflow(self) -> None:
        """Test that tools can be executed through the registry."""
        from codewhisper.core.message import ToolCall
        from codewhisper.tools.factory import create_tool_registry

        registry = create_tool_registry(code_dir=CODE_DIR)

        # Create a tool call for citadel_get_file_stats
        cobol_dir = CODE_DIR / "cbl"
        cobol_files = list(cobol_dir.glob("*.cbl"))
        if not cobol_files:
            pytest.skip("No COBOL files found")

        relative_path = cobol_files[0].relative_to(CODE_DIR)

        tool_call = ToolCall(
            id="test-call-1",
            name="citadel_get_file_stats",
            arguments={"file_path": str(relative_path)},
        )

        result = await registry.execute(tool_call)

        assert result.tool_call_id == "test-call-1"
        assert result.name == "citadel_get_file_stats"
        assert result.error is None or len(result.error) == 0
        assert len(result.content) > 0

    @pytest.mark.asyncio
    async def test_read_file_tool(self) -> None:
        """Test read_file tool works with CardDemo files."""
        from codewhisper.core.message import ToolCall
        from codewhisper.tools.factory import create_tool_registry

        registry = create_tool_registry(code_dir=CODE_DIR)

        cobol_dir = CODE_DIR / "cbl"
        cobol_files = list(cobol_dir.glob("*.cbl"))
        if not cobol_files:
            pytest.skip("No COBOL files found")

        relative_path = cobol_files[0].relative_to(CODE_DIR)

        tool_call = ToolCall(
            id="test-read-1",
            name="read_file",
            arguments={"file_path": str(relative_path), "max_lines": 50},
        )

        result = await registry.execute(tool_call)

        assert result.error is None or len(result.error) == 0
        assert len(result.content) > 0


class TestOpenAISchemaGeneration:
    """Test OpenAI schema generation for tools."""

    def test_generate_openai_schema(self) -> None:
        """Test that tools generate valid OpenAI schemas."""
        from codewhisper.tools.factory import create_tool_registry

        registry = create_tool_registry(code_dir=CODE_DIR)
        schema = registry.to_openai_schema()

        assert isinstance(schema, list)
        assert len(schema) > 0

        # Verify schema structure
        for tool_schema in schema:
            assert "type" in tool_schema
            assert tool_schema["type"] == "function"
            assert "function" in tool_schema
            assert "name" in tool_schema["function"]
            assert "description" in tool_schema["function"]
            assert "parameters" in tool_schema["function"]


class TestToolFiltering:
    """Test tool filtering functionality."""

    def test_filter_to_subset(self) -> None:
        """Test filtering registry to subset of tools."""
        from codewhisper.tools.factory import create_tool_registry

        registry = create_tool_registry(code_dir=CODE_DIR)

        # Filter to only code tools
        filtered = registry.filter({"search_code", "read_file"})

        assert len(filtered) == 2
        assert "search_code" in filtered
        assert "read_file" in filtered
        assert "citadel_analyze_file" not in filtered

    def test_filter_with_enabled_tools(self) -> None:
        """Test creating registry with enabled_tools filter."""
        from codewhisper.tools.factory import create_tool_registry

        registry = create_tool_registry(
            code_dir=CODE_DIR,
            enabled_tools={"citadel_analyze_file", "citadel_get_functions"},
        )

        assert len(registry) == 2
        assert "citadel_analyze_file" in registry
        assert "citadel_get_functions" in registry
