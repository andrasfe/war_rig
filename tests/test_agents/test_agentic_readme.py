"""Tests for AgenticReadmeGenerator."""

from pathlib import Path
from unittest.mock import AsyncMock, MagicMock, patch

import pytest

from war_rig.agents.agentic_readme import (
    SECTIONS,
    AgenticReadmeConfig,
    AgenticReadmeGenerator,
    ReadmeSection,
    StructuralContext,
    _parse_inline_questions,
)
from war_rig.agents.imperator import SystemDesignOutput


class TestReadmeSection:
    """Test ReadmeSection dataclass."""

    def test_section_creation(self):
        section = ReadmeSection(
            number=1,
            name="Test Section",
            prompt_template="Write about tests.",
        )
        assert section.number == 1
        assert section.name == "Test Section"
        assert section.min_sentences == 5
        assert section.requires_kg is False

    def test_section_with_kg(self):
        section = ReadmeSection(
            number=1,
            name="Data",
            prompt_template="Query data.",
            requires_kg=True,
        )
        assert section.requires_kg is True

    def test_nine_sections_defined(self):
        assert len(SECTIONS) == 9

    def test_sections_are_numbered_sequentially(self):
        for i, section in enumerate(SECTIONS, start=1):
            assert section.number == i

    def test_section_names(self):
        names = [s.name for s in SECTIONS]
        assert "Executive Summary" in names
        assert "Architecture Overview" in names
        assert "Open Questions and Uncertainties" in names


class TestStructuralContext:
    """Test StructuralContext dataclass."""

    def test_empty_context(self):
        ctx = StructuralContext()
        result = ctx.to_context_string()
        assert "Structural Context" in result

    def test_with_entry_points(self):
        ctx = StructuralContext(entry_points=["MAINPGM", "BATCH01"])
        result = ctx.to_context_string()
        assert "MAINPGM" in result
        assert "BATCH01" in result
        assert "Entry Points" in result

    def test_with_call_chains(self):
        ctx = StructuralContext(
            call_chains=[["A", "B", "C"], ["X", "Y"]]
        )
        result = ctx.to_context_string()
        assert "A → B → C" in result
        assert "X → Y" in result

    def test_with_mermaid_diagram(self):
        ctx = StructuralContext(
            call_graph_mermaid="```mermaid\ngraph TD\nA-->B\n```"
        )
        result = ctx.to_context_string()
        assert "```mermaid" in result
        assert "A-->B" in result

    def test_with_shared_copybooks(self):
        ctx = StructuralContext(
            shared_copybooks={"CBACTM01": ["PROG1", "PROG2"]}
        )
        result = ctx.to_context_string()
        assert "CBACTM01" in result
        assert "PROG1, PROG2" in result

    def test_with_doc_path_table(self):
        ctx = StructuralContext(
            doc_path_table="| Component | Doc Path |\n|---|---|\n| PROG1 | `PROG1.cbl.md` |"
        )
        result = ctx.to_context_string()
        assert "PROG1.cbl.md" in result

    def test_with_kg_summary(self):
        ctx = StructuralContext(
            kg_summary="## Knowledge Graph\n- 10 entities"
        )
        result = ctx.to_context_string()
        assert "Knowledge Graph" in result

    def test_single_user_copybook_not_shown(self):
        ctx = StructuralContext(
            shared_copybooks={"SINGLE": ["PROG1"]}
        )
        result = ctx.to_context_string()
        # Single-user copybooks shouldn't appear
        assert "SINGLE" not in result


class TestAgenticReadmeConfig:
    """Test AgenticReadmeConfig dataclass."""

    def test_defaults(self):
        config = AgenticReadmeConfig()
        assert config.max_iterations_per_section == 10
        assert config.temperature == 0.3
        assert config.max_tokens == 4096
        assert config.use_minion is True
        assert config.merge_pass_enabled is True

    def test_custom_config(self):
        config = AgenticReadmeConfig(
            max_iterations_per_section=5,
            temperature=0.5,
            merge_pass_enabled=False,
        )
        assert config.max_iterations_per_section == 5
        assert config.temperature == 0.5
        assert config.merge_pass_enabled is False


class TestParseInlineQuestions:
    """Test the _parse_inline_questions function."""

    def test_no_questions(self):
        markdown = "# Hello\n\nSome content."
        questions = _parse_inline_questions(markdown)
        assert len(questions) == 0

    def test_single_question(self):
        markdown = "## Summary\n\n❓ QUESTION: What is the purpose?\n"
        questions = _parse_inline_questions(markdown)
        assert len(questions) == 1
        assert questions[0].question_id == "Q001"
        assert "purpose" in questions[0].question_text
        assert questions[0].context == "Summary"

    def test_multiple_questions(self):
        markdown = (
            "## Arch\n\n❓ QUESTION: How does A work?\n"
            "## Data\n\n❓ QUESTION: Where is B stored?\n"
        )
        questions = _parse_inline_questions(markdown)
        assert len(questions) == 2
        assert questions[0].context == "Arch"
        assert questions[1].context == "Data"

    def test_cycle_number(self):
        markdown = "❓ QUESTION: Test?\n"
        questions = _parse_inline_questions(markdown, cycle=3)
        assert questions[0].cycle_asked == 3

    def test_question_without_header(self):
        markdown = "❓ QUESTION: No header context?\n"
        questions = _parse_inline_questions(markdown)
        assert questions[0].context == "General"


class TestAgenticReadmeGenerator:
    """Test AgenticReadmeGenerator with mocked SDK."""

    @pytest.fixture
    def generator(self):
        return AgenticReadmeGenerator(
            code_dir=Path("/tmp/code"),
            skills_dir=Path("/tmp/skills"),
            config=AgenticReadmeConfig(merge_pass_enabled=False),
        )

    @pytest.fixture
    def structural_context(self):
        return StructuralContext(
            entry_points=["MAINPGM"],
            doc_path_table="| Component | Doc Path |\n|---|---|\n| MAIN | `MAIN.cbl.md` |",
        )

    async def test_generate_returns_system_design_output(
        self, generator, structural_context
    ):
        """Test that generate() returns a proper SystemDesignOutput."""
        mock_sdk = MagicMock()
        mock_result = MagicMock()
        mock_result.content = "## 1. Executive Summary\n\nTest content.\n"
        mock_result.tool_calls_made = 2
        mock_result.iterations = 3
        mock_sdk.complete = AsyncMock(return_value=mock_result)
        mock_sdk.add_system_message = MagicMock()

        with patch.object(generator, "_create_sdk", return_value=mock_sdk):
            result = await generator.generate(structural_context)

        assert isinstance(result, SystemDesignOutput)
        assert result.success is True
        assert len(result.markdown) > 0
        assert "System Design Document" in result.markdown

    async def test_generate_calls_sdk_for_each_section(
        self, generator, structural_context
    ):
        """Test that SDK.complete is called once per section."""
        mock_sdk = MagicMock()
        mock_result = MagicMock()
        mock_result.content = "## Section\n\nContent.\n"
        mock_result.tool_calls_made = 1
        mock_result.iterations = 1
        mock_sdk.complete = AsyncMock(return_value=mock_result)
        mock_sdk.add_system_message = MagicMock()

        with patch.object(generator, "_create_sdk", return_value=mock_sdk):
            await generator.generate(structural_context)

        # Should be called once per section (9 sections)
        assert mock_sdk.complete.call_count == 9

    async def test_generate_handles_section_failure(
        self, generator, structural_context
    ):
        """Test graceful handling when a section fails."""
        mock_sdk = MagicMock()
        call_count = 0

        async def mock_complete(prompt):
            nonlocal call_count
            call_count += 1
            if call_count == 3:
                raise RuntimeError("LLM error")
            result = MagicMock()
            result.content = f"## Section {call_count}\n\nContent.\n"
            result.tool_calls_made = 0
            result.iterations = 1
            return result

        mock_sdk.complete = mock_complete
        mock_sdk.add_system_message = MagicMock()

        with patch.object(generator, "_create_sdk", return_value=mock_sdk):
            result = await generator.generate(structural_context)

        assert result.success is True
        # Failed section should have error message
        assert "Section generation failed" in result.markdown

    async def test_generate_appends_sequence_diagrams(
        self, generator, structural_context
    ):
        """Test that sequence diagrams are appended."""
        mock_sdk = MagicMock()
        mock_result = MagicMock()
        mock_result.content = "## Section\n\nContent.\n"
        mock_result.tool_calls_made = 0
        mock_result.iterations = 1
        mock_sdk.complete = AsyncMock(return_value=mock_result)
        mock_sdk.add_system_message = MagicMock()

        diagrams = ["sequenceDiagram\n  A->>B: Call"]

        with patch.object(generator, "_create_sdk", return_value=mock_sdk):
            result = await generator.generate(structural_context, diagrams)

        assert "## Flows" in result.markdown
        assert "A->>B" in result.markdown

    async def test_merge_pass_when_enabled(self, structural_context):
        """Test that merge pass runs when enabled."""
        gen = AgenticReadmeGenerator(
            code_dir=Path("/tmp/code"),
            config=AgenticReadmeConfig(merge_pass_enabled=True),
        )

        mock_sdk = MagicMock()
        call_count = 0

        async def mock_complete(prompt):
            nonlocal call_count
            call_count += 1
            result = MagicMock()
            if "Review and clean up" in prompt:
                # Merge pass - return something substantial
                result.content = "# System Design Document\n\n" + "Merged content.\n" * 50
            else:
                result.content = f"## Section {call_count}\n\nContent.\n"
            result.tool_calls_made = 0
            result.iterations = 1
            return result

        mock_sdk.complete = mock_complete
        mock_sdk.add_system_message = MagicMock()

        with patch.object(gen, "_create_sdk", return_value=mock_sdk):
            await gen.generate(structural_context)

        # 9 sections + 1 merge pass = 10 calls
        assert call_count == 10

    def test_build_section_prompt(self, generator, structural_context):
        """Test prompt building for a section."""
        section = SECTIONS[0]  # Executive Summary
        prompt = generator._build_section_prompt(
            section, structural_context, {}
        )
        assert "Executive Summary" in prompt
        assert "search_skills" in prompt

    def test_build_section_prompt_kg_unavailable(self, structural_context):
        """Test prompt notes KG unavailability."""
        gen = AgenticReadmeGenerator(
            code_dir=Path("/tmp/code"),
            kg_manager=None,
        )
        section = SECTIONS[0]  # Has requires_kg=True
        prompt = gen._build_section_prompt(section, structural_context, {})
        assert "not available" in prompt

    def test_assemble_document(self, generator):
        """Test document assembly."""
        sections = {
            "Executive Summary": "## 1. Executive Summary\n\nSummary.",
            "Architecture Overview": "## 2. Architecture Overview\n\nArch.",
        }
        result = generator._assemble_document(sections)
        assert "# System Design Document" in result
        assert "Summary." in result
        assert "Arch." in result

    def test_format_flows_section(self):
        """Test flows section formatting."""
        diagrams = [
            "sequenceDiagram\n  A->>B: Call",
            "```mermaid\nsequenceDiagram\n  C->>D: Call\n```",
        ]
        result = AgenticReadmeGenerator._format_flows_section(diagrams)
        assert "## Flows" in result
        assert "A->>B" in result
        assert "C->>D" in result
        # Verify mermaid fences aren't doubled
        assert result.count("```mermaid") == 2

    def test_format_flows_empty(self):
        """Test empty flows section."""
        result = AgenticReadmeGenerator._format_flows_section([])
        assert result == ""

    async def test_create_sdk_with_kg(self):
        """Test SDK creation registers KG tools."""
        mock_manager = MagicMock()
        mock_manager.enabled = True

        gen = AgenticReadmeGenerator(
            code_dir=Path("/tmp/code"),
            skills_dir=Path("/tmp/skills"),
            kg_manager=mock_manager,
        )

        with patch("war_rig.providers.get_provider_from_env") as mock_prov, \
             patch("codewhisper.sdk.CodeWhisper") as mock_cw_cls, \
             patch("war_rig.agents.kg_tools.create_kg_tools") as mock_kg_tools:
            mock_prov.return_value = MagicMock()
            mock_sdk_instance = MagicMock()
            mock_cw_cls.return_value = mock_sdk_instance
            mock_kg_tools.return_value = [MagicMock(), MagicMock()]

            gen._create_sdk()

            mock_kg_tools.assert_called_once_with(mock_manager)
            mock_sdk_instance.tool_registry.register_all.assert_called_once()

    async def test_create_sdk_without_kg(self):
        """Test SDK creation skips KG tools when manager is None."""
        gen = AgenticReadmeGenerator(
            code_dir=Path("/tmp/code"),
            kg_manager=None,
        )

        with patch("war_rig.providers.get_provider_from_env") as mock_prov, \
             patch("codewhisper.sdk.CodeWhisper") as mock_cw_cls:
            mock_prov.return_value = MagicMock()
            mock_sdk_instance = MagicMock()
            mock_cw_cls.return_value = mock_sdk_instance

            gen._create_sdk()

            mock_sdk_instance.tool_registry.register_all.assert_not_called()
