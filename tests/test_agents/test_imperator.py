"""Tests for the Imperator agent.

This module contains unit tests for the Imperator agent, focusing on
the system design document generation functionality.
"""

import pytest

from war_rig.agents.imperator import (
    FileDocumentation,
    HolisticReviewInput,
    ImperatorAgent,
)
from war_rig.config import ImperatorConfig
from war_rig.models.templates import (
    CalledProgram,
    CallingContext,
    CallType,
    CopybookLocation,
    CopybookReference,
    DocumentationTemplate,
    HeaderSection,
    InputOutput,
    IOType,
    ProgramType,
    PurposeSection,
)


@pytest.fixture
def imperator_config() -> ImperatorConfig:
    """Create a minimal Imperator configuration for testing."""
    return ImperatorConfig(model="gpt-4o-mini")


@pytest.fixture
def imperator_agent(imperator_config: ImperatorConfig) -> ImperatorAgent:
    """Create an Imperator agent instance for testing."""
    return ImperatorAgent(config=imperator_config)


@pytest.fixture
def sample_file_documentation() -> list[FileDocumentation]:
    """Create sample file documentation for testing."""
    # Create first program documentation
    header1 = HeaderSection(
        file_name="MAINPGM.CBL",
        program_id="MAINPGM",
        file_type="COBOL",
    )
    purpose1 = PurposeSection(
        summary="Main batch program that orchestrates daily processing.",
        business_context="Handles end-of-day batch processing for accounts.",
        program_type=ProgramType.BATCH,
    )
    template1 = DocumentationTemplate(
        header=header1,
        purpose=purpose1,
        called_programs=[
            CalledProgram(
                program_name="SUBPGM1",
                call_type=CallType.STATIC_CALL,
                purpose="Process transactions",
            ),
            CalledProgram(
                program_name="SUBPGM2",
                call_type=CallType.STATIC_CALL,
                purpose="Generate reports",
            ),
        ],
        inputs=[
            InputOutput(
                name="TRANS-FILE",
                io_type=IOType.FILE_SEQUENTIAL,
                description="Transaction input file",
            ),
        ],
        outputs=[
            InputOutput(
                name="REPORT-FILE",
                io_type=IOType.REPORT,
                description="Daily report output",
            ),
        ],
        copybooks_used=[
            CopybookReference(
                copybook_name="COMMON.CPY",
                purpose="Common data structures",
                location=CopybookLocation.WORKING_STORAGE,
            ),
        ],
    )
    doc1 = FileDocumentation(
        file_name="MAINPGM.CBL",
        program_id="MAINPGM",
        template=template1,
    )

    # Create second program documentation (online)
    header2 = HeaderSection(
        file_name="INQPGM.CBL",
        program_id="INQPGM",
        file_type="COBOL",
    )
    purpose2 = PurposeSection(
        summary="Online inquiry program for account lookup.",
        business_context="Provides real-time account inquiry for customer service.",
        program_type=ProgramType.ONLINE_CICS,
    )
    calling_context2 = CallingContext(called_by=["MAINMENU"])
    template2 = DocumentationTemplate(
        header=header2,
        purpose=purpose2,
        calling_context=calling_context2,
        copybooks_used=[
            CopybookReference(
                copybook_name="COMMON.CPY",
                purpose="Common data structures",
                location=CopybookLocation.WORKING_STORAGE,
            ),
            CopybookReference(
                copybook_name="SCREEN.CPY",
                purpose="Screen layouts",
                location=CopybookLocation.WORKING_STORAGE,
            ),
        ],
    )
    doc2 = FileDocumentation(
        file_name="INQPGM.CBL",
        program_id="INQPGM",
        template=template2,
    )

    return [doc1, doc2]


@pytest.fixture
def sample_holistic_input(
    sample_file_documentation: list[FileDocumentation],
) -> HolisticReviewInput:
    """Create sample holistic review input for testing."""
    return HolisticReviewInput(
        batch_id="TEST-BATCH-001",
        cycle=3,
        file_documentation=sample_file_documentation,
        call_graph={
            "MAINPGM": ["SUBPGM1", "SUBPGM2"],
            "SUBPGM1": ["UTILITY1"],
        },
        shared_copybooks={
            "COMMON.CPY": ["MAINPGM", "INQPGM", "SUBPGM1"],
            "SCREEN.CPY": ["INQPGM"],
        },
        data_flow={
            "MAINPGM": ["TRANS-FILE", "REPORT-FILE"],
        },
    )


class TestBuildSystemDesignPrompt:
    """Tests for the _build_system_design_prompt method."""

    def test_basic_prompt_structure(
        self,
        imperator_agent: ImperatorAgent,
        sample_holistic_input: HolisticReviewInput,
    ):
        """Test that the prompt contains all required sections."""
        prompt = imperator_agent._build_system_design_prompt(sample_holistic_input)

        # Check header section
        assert "# System Design Document Generation" in prompt
        assert "**Batch ID**: TEST-BATCH-001" in prompt
        assert "**Review Cycle**: 3" in prompt
        assert "**Total Files Documented**: 2" in prompt

        # Check required document structure sections
        assert "## Your Task" in prompt
        assert "## Required Document Structure" in prompt
        assert "### 1. Executive Summary" in prompt
        assert "### 2. Architecture Overview" in prompt
        assert "### 3. Key Components and Relationships" in prompt
        assert "### 4. Data Flows" in prompt
        assert "### 5. Subsystem Breakdown" in prompt

        # Check guidelines
        assert "## Important Guidelines" in prompt
        assert "QUESTION:" in prompt  # Should mention the question marker

        # Check output format
        assert "## Output Format" in prompt
        assert "markdown" in prompt.lower()

    def test_prompt_includes_file_documentation(
        self,
        imperator_agent: ImperatorAgent,
        sample_holistic_input: HolisticReviewInput,
    ):
        """Test that file documentation summaries are included."""
        prompt = imperator_agent._build_system_design_prompt(sample_holistic_input)

        # Check program documentation section
        assert "## Program Documentation Summaries" in prompt

        # Check BATCH programs
        assert "### BATCH Programs" in prompt
        assert "**MAINPGM**" in prompt
        assert "MAINPGM.CBL" in prompt
        assert "Main batch program that orchestrates daily processing" in prompt

        # Check ONLINE_CICS programs
        assert "### ONLINE_CICS Programs" in prompt
        assert "**INQPGM**" in prompt
        assert "Online inquiry program for account lookup" in prompt

        # Check business context is included
        assert "end-of-day batch processing" in prompt
        assert "customer service" in prompt

    def test_prompt_includes_call_graph(
        self,
        imperator_agent: ImperatorAgent,
        sample_holistic_input: HolisticReviewInput,
    ):
        """Test that call graph information is included."""
        prompt = imperator_agent._build_system_design_prompt(sample_holistic_input)

        assert "## Call Graph" in prompt
        assert "**MAINPGM** calls: SUBPGM1, SUBPGM2" in prompt
        assert "**SUBPGM1** calls: UTILITY1" in prompt

    def test_prompt_includes_shared_copybooks(
        self,
        imperator_agent: ImperatorAgent,
        sample_holistic_input: HolisticReviewInput,
    ):
        """Test that shared copybooks are included."""
        prompt = imperator_agent._build_system_design_prompt(sample_holistic_input)

        assert "## Shared Copybooks" in prompt
        assert "**COMMON.CPY**" in prompt
        # Should list users
        assert "MAINPGM" in prompt
        assert "INQPGM" in prompt

    def test_prompt_includes_data_flow(
        self,
        imperator_agent: ImperatorAgent,
        sample_holistic_input: HolisticReviewInput,
    ):
        """Test that data flow information is included."""
        prompt = imperator_agent._build_system_design_prompt(sample_holistic_input)

        assert "## Data Flow Information" in prompt
        assert "**MAINPGM**" in prompt
        assert "TRANS-FILE" in prompt
        assert "REPORT-FILE" in prompt

    def test_prompt_without_existing_content(
        self,
        imperator_agent: ImperatorAgent,
        sample_holistic_input: HolisticReviewInput,
    ):
        """Test prompt generation without existing content."""
        prompt = imperator_agent._build_system_design_prompt(sample_holistic_input)

        # Should have creation instruction
        assert "Create a comprehensive SYSTEM_DESIGN.md document" in prompt
        # Should not have update instruction
        assert "enhance and update" not in prompt.lower()
        # Should not have existing content section
        assert "## Existing SYSTEM_DESIGN.md Content" not in prompt

    def test_prompt_with_existing_content(
        self,
        imperator_agent: ImperatorAgent,
        sample_holistic_input: HolisticReviewInput,
    ):
        """Test prompt generation with existing content for updates."""
        existing = """# System Design

## Executive Summary

This is the existing system design document.

## Components

- MAINPGM: Main program
"""
        prompt = imperator_agent._build_system_design_prompt(
            sample_holistic_input,
            existing_content=existing,
        )

        # Should have update instruction
        assert "enhance and update" in prompt.lower()
        # Should include existing content section
        assert "## Existing SYSTEM_DESIGN.md Content" in prompt
        assert "```markdown" in prompt
        assert "This is the existing system design document" in prompt
        # Should mention preserving content
        assert "preserving valuable content" in prompt.lower()

    def test_prompt_with_empty_file_documentation(
        self,
        imperator_agent: ImperatorAgent,
    ):
        """Test prompt generation with no file documentation."""
        empty_input = HolisticReviewInput(
            batch_id="EMPTY-BATCH",
            cycle=1,
            file_documentation=[],
        )

        prompt = imperator_agent._build_system_design_prompt(empty_input)

        assert "**Total Files Documented**: 0" in prompt
        assert "*No file documentation available.*" in prompt

    def test_prompt_includes_called_programs(
        self,
        imperator_agent: ImperatorAgent,
        sample_holistic_input: HolisticReviewInput,
    ):
        """Test that called programs from documentation are shown."""
        prompt = imperator_agent._build_system_design_prompt(sample_holistic_input)

        # MAINPGM calls SUBPGM1 and SUBPGM2 in its documentation
        assert "**Calls**:" in prompt
        assert "SUBPGM1" in prompt
        assert "SUBPGM2" in prompt

    def test_prompt_includes_calling_context(
        self,
        imperator_agent: ImperatorAgent,
        sample_holistic_input: HolisticReviewInput,
    ):
        """Test that calling context (called_by) is shown."""
        prompt = imperator_agent._build_system_design_prompt(sample_holistic_input)

        # INQPGM is called by MAINMENU
        assert "**Called By**:" in prompt
        assert "MAINMENU" in prompt

    def test_prompt_includes_inputs_outputs(
        self,
        imperator_agent: ImperatorAgent,
        sample_holistic_input: HolisticReviewInput,
    ):
        """Test that inputs and outputs are shown."""
        prompt = imperator_agent._build_system_design_prompt(sample_holistic_input)

        assert "**Inputs**:" in prompt
        assert "TRANS-FILE" in prompt
        assert "**Outputs**:" in prompt
        assert "REPORT-FILE" in prompt

    def test_prompt_includes_copybooks_from_docs(
        self,
        imperator_agent: ImperatorAgent,
        sample_holistic_input: HolisticReviewInput,
    ):
        """Test that copybooks from documentation are shown."""
        prompt = imperator_agent._build_system_design_prompt(sample_holistic_input)

        assert "**Copybooks**:" in prompt
        assert "COMMON.CPY" in prompt
        assert "SCREEN.CPY" in prompt

    def test_question_marker_format(
        self,
        imperator_agent: ImperatorAgent,
        sample_holistic_input: HolisticReviewInput,
    ):
        """Test that the question marker format is correct."""
        prompt = imperator_agent._build_system_design_prompt(sample_holistic_input)

        # Should use the emoji question mark
        assert "\u2753 QUESTION:" in prompt or "QUESTION:" in prompt

    def test_output_format_instructions(
        self,
        imperator_agent: ImperatorAgent,
        sample_holistic_input: HolisticReviewInput,
    ):
        """Test that output format instructions are clear."""
        prompt = imperator_agent._build_system_design_prompt(sample_holistic_input)

        assert "Output ONLY the markdown content" in prompt
        assert "Do not wrap in JSON" in prompt


class TestParseInlineQuestions:
    """Tests for the _parse_inline_questions helper method."""

    def test_parse_single_question(
        self,
        imperator_agent: ImperatorAgent,
    ):
        """Test parsing a single inline question."""
        markdown = """## Executive Summary

This is a test.

❓ QUESTION: What is the main purpose?
"""
        questions = imperator_agent._parse_inline_questions(markdown, cycle=1)

        assert len(questions) == 1
        assert questions[0].question_id == "Q001"
        assert questions[0].question_text == "What is the main purpose?"
        assert questions[0].context == "Executive Summary"
        assert questions[0].cycle_asked == 1

    def test_parse_multiple_questions(
        self,
        imperator_agent: ImperatorAgent,
    ):
        """Test parsing multiple inline questions."""
        markdown = """# System Design

## Executive Summary

❓ QUESTION: What database is used?

## Architecture

The system uses layers.

❓ QUESTION: Is there a caching layer?

### Components

❓ QUESTION: What about shared utilities?
"""
        questions = imperator_agent._parse_inline_questions(markdown, cycle=2)

        assert len(questions) == 3
        assert questions[0].question_id == "Q001"
        assert questions[0].question_text == "What database is used?"
        assert questions[0].context == "Executive Summary"

        assert questions[1].question_id == "Q002"
        assert questions[1].question_text == "Is there a caching layer?"
        assert questions[1].context == "Architecture"

        assert questions[2].question_id == "Q003"
        assert questions[2].question_text == "What about shared utilities?"
        assert questions[2].context == "Components"

        # All should have cycle 2
        for q in questions:
            assert q.cycle_asked == 2

    def test_parse_no_questions(
        self,
        imperator_agent: ImperatorAgent,
    ):
        """Test parsing markdown with no questions."""
        markdown = """## Summary

This document has no questions.

## Details

Just regular content.
"""
        questions = imperator_agent._parse_inline_questions(markdown, cycle=1)

        assert len(questions) == 0

    def test_parse_question_at_end_of_file(
        self,
        imperator_agent: ImperatorAgent,
    ):
        """Test parsing a question at the end of file (no trailing newline)."""
        markdown = """## Section

❓ QUESTION: Final question here"""
        questions = imperator_agent._parse_inline_questions(markdown, cycle=1)

        assert len(questions) == 1
        assert questions[0].question_text == "Final question here"

    def test_parse_question_with_extra_whitespace(
        self,
        imperator_agent: ImperatorAgent,
    ):
        """Test parsing questions with varying whitespace."""
        markdown = """## Test

❓  QUESTION:  Spaced out question text
"""
        questions = imperator_agent._parse_inline_questions(markdown, cycle=1)

        assert len(questions) == 1
        assert questions[0].question_text == "Spaced out question text"


class TestFindSectionContext:
    """Tests for the _find_section_context helper method."""

    def test_find_context_with_h2_header(
        self,
        imperator_agent: ImperatorAgent,
    ):
        """Test finding context from a level 2 header."""
        markdown = """## Executive Summary

Some content here.
"""
        # Position after the header
        context = imperator_agent._find_section_context(markdown, position=40)
        assert context == "Executive Summary"

    def test_find_context_with_nested_headers(
        self,
        imperator_agent: ImperatorAgent,
    ):
        """Test finding context picks the nearest header."""
        markdown = """# Main Title

## Section One

Content

### Subsection

More content here.
"""
        # Position in the subsection
        context = imperator_agent._find_section_context(markdown, position=70)
        assert context == "Subsection"

    def test_find_context_no_headers(
        self,
        imperator_agent: ImperatorAgent,
    ):
        """Test finding context when no headers exist."""
        markdown = "Just some plain text without headers."
        context = imperator_agent._find_section_context(markdown, position=20)
        assert context == "General"


class TestExtractSections:
    """Tests for the _extract_sections helper method."""

    def test_extract_h2_sections(
        self,
        imperator_agent: ImperatorAgent,
    ):
        """Test extracting level 2 headers."""
        markdown = """# Main Title

## Executive Summary

Content

## Architecture

More content

## Data Flows

Final content
"""
        sections = imperator_agent._extract_sections(markdown)

        assert len(sections) == 3
        assert sections == ["Executive Summary", "Architecture", "Data Flows"]

    def test_extract_ignores_h1_and_h3(
        self,
        imperator_agent: ImperatorAgent,
    ):
        """Test that only h2 headers are extracted."""
        markdown = """# Title (h1)

## Section One (h2)

### Subsection (h3)

## Section Two (h2)

#### Deep section (h4)
"""
        sections = imperator_agent._extract_sections(markdown)

        assert len(sections) == 2
        assert "Section One (h2)" in sections
        assert "Section Two (h2)" in sections

    def test_extract_empty_markdown(
        self,
        imperator_agent: ImperatorAgent,
    ):
        """Test extracting from empty markdown."""
        sections = imperator_agent._extract_sections("")
        assert sections == []


class TestGenerateSystemDesign:
    """Tests for the generate_system_design method."""

    @pytest.mark.asyncio
    async def test_mock_mode_returns_valid_output(
        self,
        imperator_agent: ImperatorAgent,
        sample_holistic_input: HolisticReviewInput,
    ):
        """Test that mock mode returns a valid SystemDesignOutput."""
        result = await imperator_agent.generate_system_design(
            sample_holistic_input,
            use_mock=True,
        )

        assert result.success is True
        assert result.error is None
        assert len(result.markdown) > 0
        assert "# System Design Document" in result.markdown
        assert len(result.questions) > 0
        assert len(result.sections_updated) > 0

    @pytest.mark.asyncio
    async def test_mock_mode_includes_batch_info(
        self,
        imperator_agent: ImperatorAgent,
        sample_holistic_input: HolisticReviewInput,
    ):
        """Test that mock mode includes batch and cycle info."""
        result = await imperator_agent.generate_system_design(
            sample_holistic_input,
            use_mock=True,
        )

        assert "TEST-BATCH-001" in result.markdown
        assert "Cycle: 3" in result.markdown

    @pytest.mark.asyncio
    async def test_mock_mode_includes_questions(
        self,
        imperator_agent: ImperatorAgent,
        sample_holistic_input: HolisticReviewInput,
    ):
        """Test that mock mode includes sample questions."""
        result = await imperator_agent.generate_system_design(
            sample_holistic_input,
            use_mock=True,
        )

        assert len(result.questions) >= 2
        # Check question structure
        for q in result.questions:
            assert q.question_id.startswith("Q")
            assert len(q.question_text) > 0
            assert len(q.context) > 0
            assert q.cycle_asked == 3  # Should match input cycle

    @pytest.mark.asyncio
    async def test_mock_mode_with_existing_content(
        self,
        imperator_agent: ImperatorAgent,
        sample_holistic_input: HolisticReviewInput,
    ):
        """Test that mock mode notes when existing content was provided."""
        existing = "# Old Document\n\nSome old content."

        result = await imperator_agent.generate_system_design(
            sample_holistic_input,
            existing_content=existing,
            use_mock=True,
        )

        assert result.success is True
        assert "existing content provided" in result.markdown.lower()

    @pytest.mark.asyncio
    async def test_mock_mode_with_empty_documentation(
        self,
        imperator_agent: ImperatorAgent,
    ):
        """Test mock mode with no file documentation."""
        empty_input = HolisticReviewInput(
            batch_id="EMPTY-BATCH",
            cycle=1,
            file_documentation=[],
        )

        result = await imperator_agent.generate_system_design(
            empty_input,
            use_mock=True,
        )

        assert result.success is True
        assert "0 documented programs" in result.markdown

    @pytest.mark.asyncio
    async def test_mock_mode_sections_updated(
        self,
        imperator_agent: ImperatorAgent,
        sample_holistic_input: HolisticReviewInput,
    ):
        """Test that mock mode returns expected sections."""
        result = await imperator_agent.generate_system_design(
            sample_holistic_input,
            use_mock=True,
        )

        expected_sections = [
            "Executive Summary",
            "Architecture Overview",
            "Key Components and Relationships",
            "Data Flows",
            "Subsystem Breakdown",
        ]

        for section in expected_sections:
            assert section in result.sections_updated


class TestHolisticReviewParallelExecution:
    """Tests for the parallel execution of holistic review and system design generation."""

    @staticmethod
    def _create_valid_holistic_json_response() -> str:
        """Create a valid JSON response that can be parsed by _parse_holistic_response."""
        import json

        response_data = {
            "decision": "SATISFIED",
            "reasoning": "All documentation meets quality standards.",
            "overall_quality": "GOOD",
            "file_feedback": {},
            "consistency_issues": [],
            "clarification_requests": [],
            "assumptions": [],
            "quality_notes": [],
            "priority_files": [],
            "missing_documentation": [],
        }
        return json.dumps(response_data)

    @pytest.mark.asyncio
    async def test_parallel_execution_both_succeed(
        self,
        imperator_agent: ImperatorAgent,
        sample_holistic_input: HolisticReviewInput,
        tmp_path,
    ):
        """Test that both operations run and succeed in parallel."""
        from unittest.mock import AsyncMock, patch

        # Create mock responses - must be valid JSON for parsing
        mock_holistic_response = self._create_valid_holistic_json_response()

        mock_design_output = imperator_agent._create_mock_system_design(
            sample_holistic_input, None
        )

        # Patch both async methods
        with patch.object(
            imperator_agent, "_call_llm", new_callable=AsyncMock
        ) as mock_llm, patch.object(
            imperator_agent, "generate_system_design", new_callable=AsyncMock
        ) as mock_design:
            mock_llm.return_value = mock_holistic_response
            mock_design.return_value = mock_design_output

            result = await imperator_agent.holistic_review(
                sample_holistic_input,
                use_mock=False,
                output_directory=tmp_path,
            )

            # Verify both methods were called
            mock_llm.assert_called_once()
            mock_design.assert_called_once()

            # Verify result is successful
            assert result.success is True
            assert result.system_design_generated is True

    @pytest.mark.asyncio
    async def test_holistic_review_fails_entire_operation_fails(
        self,
        imperator_agent: ImperatorAgent,
        sample_holistic_input: HolisticReviewInput,
        tmp_path,
    ):
        """Test that if holistic review LLM call fails, the entire operation fails."""
        from unittest.mock import AsyncMock, patch

        mock_design_output = imperator_agent._create_mock_system_design(
            sample_holistic_input, None
        )

        with patch.object(
            imperator_agent, "_call_llm", new_callable=AsyncMock
        ) as mock_llm, patch.object(
            imperator_agent, "generate_system_design", new_callable=AsyncMock
        ) as mock_design:
            # Holistic review fails
            mock_llm.side_effect = RuntimeError("LLM API error")
            # System design succeeds
            mock_design.return_value = mock_design_output

            result = await imperator_agent.holistic_review(
                sample_holistic_input,
                use_mock=False,
                output_directory=tmp_path,
            )

            # Verify operation failed
            assert result.success is False
            assert "LLM API error" in result.error

    @pytest.mark.asyncio
    async def test_system_design_fails_holistic_review_succeeds(
        self,
        imperator_agent: ImperatorAgent,
        sample_holistic_input: HolisticReviewInput,
        tmp_path,
    ):
        """Test that if system design fails, holistic review still succeeds."""
        from unittest.mock import AsyncMock, patch

        # Create mock response - must be valid JSON for parsing
        mock_holistic_response = self._create_valid_holistic_json_response()

        with patch.object(
            imperator_agent, "_call_llm", new_callable=AsyncMock
        ) as mock_llm, patch.object(
            imperator_agent, "generate_system_design", new_callable=AsyncMock
        ) as mock_design:
            # Holistic review succeeds
            mock_llm.return_value = mock_holistic_response
            # System design fails
            mock_design.side_effect = RuntimeError("System design API error")

            result = await imperator_agent.holistic_review(
                sample_holistic_input,
                use_mock=False,
                output_directory=tmp_path,
            )

            # Verify holistic review succeeded
            assert result.success is True
            # But system design failed gracefully
            assert result.system_design_generated is False

    @pytest.mark.asyncio
    async def test_no_output_directory_skips_system_design(
        self,
        imperator_agent: ImperatorAgent,
        sample_holistic_input: HolisticReviewInput,
    ):
        """Test that without output_directory, only holistic review runs."""
        from unittest.mock import AsyncMock, patch

        # Create mock response - must be valid JSON for parsing
        mock_holistic_response = self._create_valid_holistic_json_response()

        with patch.object(
            imperator_agent, "_call_llm", new_callable=AsyncMock
        ) as mock_llm, patch.object(
            imperator_agent, "generate_system_design", new_callable=AsyncMock
        ) as mock_design:
            mock_llm.return_value = mock_holistic_response

            result = await imperator_agent.holistic_review(
                sample_holistic_input,
                use_mock=False,
                output_directory=None,  # No output directory
            )

            # Verify holistic review was called
            mock_llm.assert_called_once()
            # Verify system design was NOT called
            mock_design.assert_not_called()

            # Verify result is successful
            assert result.success is True

    @pytest.mark.asyncio
    async def test_system_design_returns_failure_result(
        self,
        imperator_agent: ImperatorAgent,
        sample_holistic_input: HolisticReviewInput,
        tmp_path,
    ):
        """Test handling when system design returns a failed result object."""
        from unittest.mock import AsyncMock, patch

        from war_rig.agents.imperator import SystemDesignOutput

        # Create mock response - must be valid JSON for parsing
        mock_holistic_response = self._create_valid_holistic_json_response()

        # System design returns a failure result (not an exception)
        mock_design_output = SystemDesignOutput(
            success=False,
            error="Failed to parse LLM response",
            markdown="",
        )

        with patch.object(
            imperator_agent, "_call_llm", new_callable=AsyncMock
        ) as mock_llm, patch.object(
            imperator_agent, "generate_system_design", new_callable=AsyncMock
        ) as mock_design:
            mock_llm.return_value = mock_holistic_response
            mock_design.return_value = mock_design_output

            result = await imperator_agent.holistic_review(
                sample_holistic_input,
                use_mock=False,
                output_directory=tmp_path,
            )

            # Verify holistic review succeeded
            assert result.success is True
            # But system design generation is marked as failed
            assert result.system_design_generated is False

    @pytest.mark.asyncio
    async def test_parallel_execution_writes_system_design_file(
        self,
        imperator_agent: ImperatorAgent,
        sample_holistic_input: HolisticReviewInput,
        tmp_path,
    ):
        """Test that system design is written to file on success."""
        from unittest.mock import AsyncMock, patch

        # Create mock response - must be valid JSON for parsing
        mock_holistic_response = self._create_valid_holistic_json_response()

        mock_design_output = imperator_agent._create_mock_system_design(
            sample_holistic_input, None
        )

        with patch.object(
            imperator_agent, "_call_llm", new_callable=AsyncMock
        ) as mock_llm, patch.object(
            imperator_agent, "generate_system_design", new_callable=AsyncMock
        ) as mock_design:
            mock_llm.return_value = mock_holistic_response
            mock_design.return_value = mock_design_output

            result = await imperator_agent.holistic_review(
                sample_holistic_input,
                use_mock=False,
                output_directory=tmp_path,
            )

            # Verify file was written
            system_design_path = tmp_path / "SYSTEM_DESIGN.md"
            assert system_design_path.exists()
            assert system_design_path.read_text() == mock_design_output.markdown

            # Verify result fields are populated
            assert result.success is True
            assert result.system_design_generated is True
            assert result.system_design_path == str(system_design_path)
            assert len(result.system_design_questions) > 0
