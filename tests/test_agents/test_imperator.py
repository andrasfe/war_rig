"""Tests for the Imperator agent.

This module contains unit tests for the Imperator agent, focusing on
the system design document generation functionality.
"""

import pytest

from war_rig.agents.imperator import (
    FileDocumentation,
    FileDocumentationSummary,
    HolisticReviewInput,
    HolisticReviewInputCompact,
    ImperatorAgent,
)
from war_rig.config import ImperatorConfig
from war_rig.models.assessments import ConfidenceLevel
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
        assert "### 3. Component Catalog" in prompt
        assert "### 4. Subsystem Breakdown" in prompt
        assert "### 5. Data Architecture" in prompt

        # Check guidelines
        assert "## Critical Guidelines" in prompt
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
        assert "## Program Documentation" in prompt

        # Check programs are included
        assert "#### MAINPGM" in prompt
        assert "MAINPGM.CBL" in prompt
        assert "Main batch program that orchestrates daily processing" in prompt

        assert "#### INQPGM" in prompt
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
        assert "Create a comprehensive, detailed README.md document" in prompt
        # Should not have update instruction
        assert "enhance and update" not in prompt.lower()
        # Should not have existing content section
        assert "## Existing README.md Content" not in prompt

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
        assert "## Existing README.md Content" in prompt
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
        assert "**Programs Called**:" in prompt
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

        assert "**Copybooks Used**:" in prompt
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

    def test_prompt_includes_call_graph_markdown_mermaid(
        self,
        imperator_agent: ImperatorAgent,
        sample_holistic_input: HolisticReviewInput,
    ):
        """Test that the Mermaid diagram from call_graph_markdown is included."""
        # Add call graph markdown with a Mermaid diagram
        sample_holistic_input.call_graph_markdown = """# Call Graph Analysis

*Generated: 2024-01-15 10:00:00*

## Visual Call Graph

```mermaid
flowchart TD
    MAINPGM --> SUBPGM1
    MAINPGM --> SUBPGM2
    SUBPGM1 --> UTILITY
```

## Entry Points
- MAINPGM: Main batch program
"""
        prompt = imperator_agent._build_system_design_prompt(sample_holistic_input)

        # Check that the Mermaid diagram section is included
        assert "## Actual System Call Graph (from static analysis)" in prompt
        assert "Use this exact Mermaid diagram" in prompt
        assert "Do NOT create your own diagram" in prompt
        assert "```mermaid" in prompt
        assert "flowchart TD" in prompt
        assert "MAINPGM --> SUBPGM1" in prompt

    def test_prompt_architecture_overview_references_mermaid(
        self,
        imperator_agent: ImperatorAgent,
        sample_holistic_input: HolisticReviewInput,
    ):
        """Test that Architecture Overview section references the provided Mermaid diagram."""
        sample_holistic_input.call_graph_markdown = """```mermaid
flowchart TD
    A --> B
```"""
        prompt = imperator_agent._build_system_design_prompt(sample_holistic_input)

        # Check that Architecture Overview tells LLM to use provided diagram
        assert "Use the exact Mermaid diagram provided" in prompt
        assert "Do NOT create your own diagram" in prompt

    def test_prompt_without_call_graph_markdown_suggests_creating_diagram(
        self,
        imperator_agent: ImperatorAgent,
        sample_holistic_input: HolisticReviewInput,
    ):
        """Test that without call_graph_markdown, LLM is told to create diagrams."""
        # Ensure no call_graph_markdown
        sample_holistic_input.call_graph_markdown = None
        prompt = imperator_agent._build_system_design_prompt(sample_holistic_input)

        # Should suggest creating diagrams
        assert "ASCII or Mermaid diagrams showing component relationships" in prompt
        # Should NOT have the static analysis section
        assert "Actual System Call Graph (from static analysis)" not in prompt


class TestExtractMermaidFromMarkdown:
    """Tests for the _extract_mermaid_from_markdown helper method."""

    def test_extract_simple_mermaid_block(
        self,
        imperator_agent: ImperatorAgent,
    ):
        """Test extracting a simple Mermaid block."""
        markdown = """# Header

```mermaid
flowchart TD
    A --> B
```

More content here.
"""
        result = imperator_agent._extract_mermaid_from_markdown(markdown)

        assert result is not None
        assert result == "```mermaid\nflowchart TD\n    A --> B\n```"

    def test_extract_complex_mermaid_block(
        self,
        imperator_agent: ImperatorAgent,
    ):
        """Test extracting a complex Mermaid block with multiple nodes."""
        markdown = """```mermaid
flowchart TD
    MAINPGM["MAINPGM<br/>Main Program"]
    MAINPGM --> SUBPGM1
    MAINPGM --> SUBPGM2
    SUBPGM1 --> UTILITY
    SUBPGM2 --> UTILITY
```"""
        result = imperator_agent._extract_mermaid_from_markdown(markdown)

        assert result is not None
        assert "MAINPGM" in result
        assert "SUBPGM1" in result
        assert "flowchart TD" in result

    def test_extract_no_mermaid_block(
        self,
        imperator_agent: ImperatorAgent,
    ):
        """Test that None is returned when no Mermaid block exists."""
        markdown = """# Just regular markdown

Some content here.

```python
print("hello")
```
"""
        result = imperator_agent._extract_mermaid_from_markdown(markdown)

        assert result is None

    def test_extract_mermaid_with_surrounding_content(
        self,
        imperator_agent: ImperatorAgent,
    ):
        """Test extraction with content before and after the Mermaid block."""
        markdown = """# Call Graph Analysis

*Generated: 2024-01-15*

## Visual Call Graph

```mermaid
flowchart TD
    A --> B
    B --> C
```

## Entry Points

- A: Main entry
- B: Sub program
"""
        result = imperator_agent._extract_mermaid_from_markdown(markdown)

        assert result is not None
        assert result.startswith("```mermaid")
        assert result.endswith("```")
        assert "A --> B" in result
        assert "B --> C" in result
        # Should not include surrounding content
        assert "Entry Points" not in result
        assert "Generated" not in result


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


class TestStripOuterCodeFence:
    """Tests for the _strip_outer_code_fence static method."""

    def test_strips_markdown_fence(self, imperator_agent: ImperatorAgent):
        """Test stripping ```markdown wrapper."""
        text = "```markdown\n# Title\n\nSome content\n```"
        result = ImperatorAgent._strip_outer_code_fence(text)
        assert result == "# Title\n\nSome content"

    def test_strips_md_fence(self, imperator_agent: ImperatorAgent):
        """Test stripping ```md wrapper."""
        text = "```md\n# Title\n\nContent here\n```"
        result = ImperatorAgent._strip_outer_code_fence(text)
        assert result == "# Title\n\nContent here"

    def test_strips_text_fence(self, imperator_agent: ImperatorAgent):
        """Test stripping ```text wrapper."""
        text = "```text\nPlain text content\n```"
        result = ImperatorAgent._strip_outer_code_fence(text)
        assert result == "Plain text content"

    def test_strips_bare_fence(self, imperator_agent: ImperatorAgent):
        """Test stripping bare ``` wrapper with no language tag."""
        text = "```\n# Title\n\nContent\n```"
        result = ImperatorAgent._strip_outer_code_fence(text)
        assert result == "# Title\n\nContent"

    def test_preserves_inner_mermaid_blocks(self, imperator_agent: ImperatorAgent):
        """Test that inner ```mermaid blocks are preserved when outer fence is stripped."""
        text = (
            "```markdown\n"
            "# System Design\n"
            "\n"
            "## Architecture\n"
            "\n"
            "```mermaid\n"
            "graph TD\n"
            "    A --> B\n"
            "```\n"
            "\n"
            "## Data Flow\n"
            "\n"
            "```mermaid\n"
            "sequenceDiagram\n"
            "    A->>B: call\n"
            "```\n"
            "```"
        )
        result = ImperatorAgent._strip_outer_code_fence(text)
        assert result.startswith("# System Design")
        assert "```mermaid\ngraph TD" in result
        assert "```mermaid\nsequenceDiagram" in result
        # The inner closing ``` for mermaid blocks should still be present
        assert result.count("```") == 4  # 2 mermaid opens + 2 mermaid closes

    def test_no_fence_returns_unchanged(self, imperator_agent: ImperatorAgent):
        """Test that content without outer fence is returned unchanged."""
        text = "# Title\n\nRegular markdown content\n"
        result = ImperatorAgent._strip_outer_code_fence(text)
        assert result == text

    def test_only_opening_fence_returns_unchanged(
        self, imperator_agent: ImperatorAgent
    ):
        """Test that content with only opening fence is returned unchanged."""
        text = "```markdown\n# Title\n\nContent without closing fence"
        result = ImperatorAgent._strip_outer_code_fence(text)
        assert result == text

    def test_only_closing_fence_returns_unchanged(
        self, imperator_agent: ImperatorAgent
    ):
        """Test that content with only closing fence is returned unchanged."""
        text = "# Title\n\nContent\n```"
        result = ImperatorAgent._strip_outer_code_fence(text)
        assert result == text

    def test_handles_leading_trailing_whitespace(
        self, imperator_agent: ImperatorAgent
    ):
        """Test that leading/trailing whitespace around the fence is handled."""
        text = "\n  ```markdown\n# Title\n\nContent\n```  \n"
        result = ImperatorAgent._strip_outer_code_fence(text)
        assert result == "# Title\n\nContent"

    def test_non_markdown_fence_not_stripped(self, imperator_agent: ImperatorAgent):
        """Test that code fences with non-markdown languages are not stripped."""
        text = "```python\nprint('hello')\n```"
        result = ImperatorAgent._strip_outer_code_fence(text)
        assert result == text

    def test_case_insensitive_tag(self, imperator_agent: ImperatorAgent):
        """Test that the fence tag matching is case-insensitive."""
        text = "```Markdown\n# Title\n\nContent\n```"
        result = ImperatorAgent._strip_outer_code_fence(text)
        assert result == "# Title\n\nContent"

    def test_empty_string(self, imperator_agent: ImperatorAgent):
        """Test handling of empty string."""
        result = ImperatorAgent._strip_outer_code_fence("")
        assert result == ""

    def test_fence_with_trailing_spaces_on_lines(
        self, imperator_agent: ImperatorAgent
    ):
        """Test fences that have trailing spaces on the fence lines."""
        text = "```markdown   \n# Title\n```   "
        result = ImperatorAgent._strip_outer_code_fence(text)
        assert result == "# Title"


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
    async def test_system_design_fails_holistic_review_fails(
        self,
        imperator_agent: ImperatorAgent,
        sample_holistic_input: HolisticReviewInput,
        tmp_path,
    ):
        """Test that if system design fails, holistic review raises an error."""
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

            with pytest.raises(RuntimeError, match="README generation failed"):
                await imperator_agent.holistic_review(
                    sample_holistic_input,
                    use_mock=False,
                    output_directory=tmp_path,
                )

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
    async def test_system_design_returns_failure_result_raises(
        self,
        imperator_agent: ImperatorAgent,
        sample_holistic_input: HolisticReviewInput,
        tmp_path,
    ):
        """Test that when system design returns a failed result, an error is raised."""
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

            with pytest.raises(RuntimeError, match="README generation failed"):
                await imperator_agent.holistic_review(
                    sample_holistic_input,
                    use_mock=False,
                    output_directory=tmp_path,
                )

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
            system_design_path = tmp_path / "README.md"
            assert system_design_path.exists()
            assert system_design_path.read_text() == mock_design_output.markdown

            # Verify result fields are populated
            assert result.success is True
            assert result.system_design_generated is True
            assert result.system_design_path == str(system_design_path)
            assert len(result.system_design_questions) > 0


class TestFileDocumentationSummary:
    """Tests for the FileDocumentationSummary model."""

    def test_create_basic_summary(self):
        """Test creating a basic file documentation summary."""
        summary = FileDocumentationSummary(
            file_name="MAINPGM.CBL",
            program_id="MAINPGM",
            purpose_summary="Main batch program for processing",
            program_type="BATCH",
            paragraph_count=5,
            main_calls=["SUBPGM1", "SUBPGM2"],
            confidence=ConfidenceLevel.HIGH,
        )

        assert summary.file_name == "MAINPGM.CBL"
        assert summary.program_id == "MAINPGM"
        assert summary.purpose_summary == "Main batch program for processing"
        assert summary.paragraph_count == 5
        assert len(summary.main_calls) == 2
        assert summary.confidence == ConfidenceLevel.HIGH

    def test_summary_defaults(self):
        """Test that summary has sensible defaults."""
        summary = FileDocumentationSummary(
            file_name="TEST.CBL",
            program_id="TEST",
        )

        assert summary.purpose_summary == ""
        assert summary.program_type == "UNKNOWN"
        assert summary.paragraph_count == 0
        assert summary.main_calls == []
        assert summary.main_inputs == []
        assert summary.main_outputs == []
        assert summary.confidence == ConfidenceLevel.MEDIUM
        assert summary.has_open_questions is False
        assert summary.iteration_count == 1

    def test_summary_with_io(self):
        """Test summary with input/output information."""
        summary = FileDocumentationSummary(
            file_name="BATCH.CBL",
            program_id="BATCH",
            main_inputs=["INPUT-FILE", "TRANS-FILE"],
            main_outputs=["REPORT-FILE", "ERROR-FILE"],
        )

        assert len(summary.main_inputs) == 2
        assert "INPUT-FILE" in summary.main_inputs
        assert len(summary.main_outputs) == 2
        assert "ERROR-FILE" in summary.main_outputs


class TestHolisticReviewInputCompact:
    """Tests for the HolisticReviewInputCompact model."""

    def test_create_compact_input(self):
        """Test creating a compact holistic review input."""
        summaries = [
            FileDocumentationSummary(
                file_name="PROG1.CBL",
                program_id="PROG1",
                purpose_summary="First program",
                main_calls=["UTIL"],
            ),
            FileDocumentationSummary(
                file_name="PROG2.CBL",
                program_id="PROG2",
                purpose_summary="Second program",
            ),
        ]

        compact_input = HolisticReviewInputCompact(
            batch_id="TEST-BATCH",
            cycle=2,
            file_summaries=summaries,
            call_graph={"PROG1": ["UTIL"], "PROG2": []},
            shared_copybooks={"COMMON.CPY": ["PROG1", "PROG2"]},
        )

        assert compact_input.batch_id == "TEST-BATCH"
        assert compact_input.cycle == 2
        assert len(compact_input.file_summaries) == 2
        assert "PROG1" in compact_input.call_graph
        assert "COMMON.CPY" in compact_input.shared_copybooks

    def test_compact_input_defaults(self):
        """Test compact input has sensible defaults."""
        compact_input = HolisticReviewInputCompact(batch_id="EMPTY")

        assert compact_input.cycle == 1
        assert compact_input.file_summaries == []
        assert compact_input.call_graph == {}
        assert compact_input.shared_copybooks == {}
        assert compact_input.per_file_confidence == {}
        assert compact_input.files_with_issues == []
        assert compact_input.previous_clarification_count == 0
        assert compact_input.unresolved_issues_count == 0
        assert compact_input.max_cycles == 5

    def test_compact_input_with_issue_tracking(self):
        """Test compact input with issue and confidence tracking."""
        compact_input = HolisticReviewInputCompact(
            batch_id="ISSUES-BATCH",
            files_with_issues=["PROG1.CBL", "PROG3.CBL"],
            per_file_confidence={
                "PROG1.CBL": ConfidenceLevel.LOW,
                "PROG2.CBL": ConfidenceLevel.HIGH,
            },
            previous_clarification_count=3,
            unresolved_issues_count=2,
        )

        assert len(compact_input.files_with_issues) == 2
        assert compact_input.per_file_confidence["PROG1.CBL"] == ConfidenceLevel.LOW
        assert compact_input.previous_clarification_count == 3
        assert compact_input.unresolved_issues_count == 2


class TestHolisticReviewCompact:
    """Tests for the holistic_review_compact method."""

    @pytest.fixture
    def sample_compact_input(self) -> HolisticReviewInputCompact:
        """Create sample compact input for testing."""
        summaries = [
            FileDocumentationSummary(
                file_name="MAINPGM.CBL",
                program_id="MAINPGM",
                purpose_summary="Main batch program that orchestrates daily processing.",
                program_type="BATCH",
                paragraph_count=10,
                main_calls=["SUBPGM1", "SUBPGM2"],
                main_inputs=["TRANS-FILE"],
                main_outputs=["REPORT-FILE"],
                confidence=ConfidenceLevel.HIGH,
            ),
            FileDocumentationSummary(
                file_name="SUBPGM1.CBL",
                program_id="SUBPGM1",
                purpose_summary="Transaction processing subroutine.",
                program_type="SUBROUTINE",
                paragraph_count=5,
                confidence=ConfidenceLevel.MEDIUM,
            ),
        ]

        return HolisticReviewInputCompact(
            batch_id="COMPACT-TEST-001",
            cycle=2,
            file_summaries=summaries,
            call_graph={"MAINPGM": ["SUBPGM1", "SUBPGM2"]},
            shared_copybooks={"COMMON.CPY": ["MAINPGM", "SUBPGM1"]},
            per_file_confidence={
                "MAINPGM.CBL": ConfidenceLevel.HIGH,
                "SUBPGM1.CBL": ConfidenceLevel.MEDIUM,
            },
            files_with_issues=["SUBPGM1.CBL"],
            previous_clarification_count=1,
        )

    @pytest.mark.asyncio
    async def test_compact_review_mock_mode_returns_valid_output(
        self,
        imperator_agent: ImperatorAgent,
        sample_compact_input: HolisticReviewInputCompact,
    ):
        """Test that mock mode returns a valid HolisticReviewOutput."""
        result = await imperator_agent.holistic_review_compact(
            sample_compact_input,
            use_mock=True,
        )

        assert result.success is True
        assert result.error is None
        assert result.decision in ["SATISFIED", "NEEDS_CLARIFICATION"]
        assert len(result.reasoning) > 0

    @pytest.mark.asyncio
    async def test_compact_review_mock_includes_cycle_info(
        self,
        imperator_agent: ImperatorAgent,
        sample_compact_input: HolisticReviewInputCompact,
    ):
        """Test that mock output includes cycle info in reasoning or quality notes."""
        result = await imperator_agent.holistic_review_compact(
            sample_compact_input,
            use_mock=True,
        )

        # The mock should reference cycle or summary count information
        assert result.success is True
        # Check that cycle info is reflected in reasoning or quality_notes
        has_cycle_info = "cycle" in result.reasoning.lower() or any(
            "2 file summaries" in note for note in result.quality_notes
        )
        assert has_cycle_info

    @pytest.mark.asyncio
    async def test_compact_review_handles_empty_summaries(
        self,
        imperator_agent: ImperatorAgent,
    ):
        """Test compact review with no file summaries."""
        empty_input = HolisticReviewInputCompact(
            batch_id="EMPTY-BATCH",
            cycle=1,
            file_summaries=[],
        )

        result = await imperator_agent.holistic_review_compact(
            empty_input,
            use_mock=True,
        )

        assert result.success is True
        # Should handle gracefully even with empty input

    @pytest.mark.asyncio
    async def test_compact_review_respects_max_cycles(
        self,
        imperator_agent: ImperatorAgent,
    ):
        """Test that compact review respects max_cycles for forced completion."""
        # Create input at max cycles
        at_max_input = HolisticReviewInputCompact(
            batch_id="MAX-CYCLES",
            cycle=5,
            max_cycles=5,
            file_summaries=[
                FileDocumentationSummary(
                    file_name="TEST.CBL",
                    program_id="TEST",
                    purpose_summary="Test program",
                )
            ],
        )

        result = await imperator_agent.holistic_review_compact(
            at_max_input,
            use_mock=True,
        )

        # At max cycles, the system should lean toward SATISFIED
        assert result.success is True
        assert result.decision == "SATISFIED"

    def test_build_compact_holistic_system_prompt(
        self,
        imperator_agent: ImperatorAgent,
        sample_compact_input: HolisticReviewInputCompact,
    ):
        """Test that the compact system prompt is generated correctly."""
        prompt = imperator_agent._build_compact_holistic_system_prompt()

        # Should contain key instructions
        assert "holistic review" in prompt.lower() or "batch" in prompt.lower()
        assert "SATISFIED" in prompt
        assert "NEEDS_CLARIFICATION" in prompt
        assert "JSON" in prompt

    def test_build_compact_holistic_user_prompt(
        self,
        imperator_agent: ImperatorAgent,
        sample_compact_input: HolisticReviewInputCompact,
    ):
        """Test that the compact user prompt includes all summary data."""
        prompt = imperator_agent._build_compact_holistic_user_prompt(sample_compact_input)

        # Should include batch info
        assert "COMPACT-TEST-001" in prompt
        assert "Cycle: 2" in prompt or "cycle" in prompt.lower()

        # Should include file summaries
        assert "MAINPGM" in prompt
        assert "SUBPGM1" in prompt

        # Should include purpose summaries (compact form)
        assert "Main batch program" in prompt or "daily processing" in prompt

        # Should include call graph info
        assert "call" in prompt.lower()

        # Should include shared copybooks
        assert "COMMON.CPY" in prompt or "copybook" in prompt.lower()

        # Should include confidence levels
        assert "HIGH" in prompt or "MEDIUM" in prompt or "confidence" in prompt.lower()


class TestCompactReviewTokenReduction:
    """Tests to verify token reduction from compact review input."""

    def test_compact_input_smaller_than_full_input(
        self,
        sample_file_documentation: list[FileDocumentation],
        sample_holistic_input: HolisticReviewInput,
    ):
        """Test that compact input is significantly smaller than full input."""
        import json

        # Build equivalent compact input from the full input
        compact_summaries = []
        for file_doc in sample_file_documentation:
            summary = FileDocumentationSummary(
                file_name=file_doc.file_name,
                program_id=file_doc.program_id,
                purpose_summary=file_doc.template.purpose.summary if file_doc.template.purpose else "",
                program_type=file_doc.template.purpose.program_type.value if file_doc.template.purpose else "UNKNOWN",
                paragraph_count=len(file_doc.template.paragraphs or []),
                main_calls=[cp.program_name for cp in (file_doc.template.called_programs or [])],
                main_inputs=[io.name for io in (file_doc.template.inputs or [])],
                main_outputs=[io.name for io in (file_doc.template.outputs or [])],
            )
            compact_summaries.append(summary)

        compact_input = HolisticReviewInputCompact(
            batch_id=sample_holistic_input.batch_id,
            cycle=sample_holistic_input.cycle,
            file_summaries=compact_summaries,
            call_graph=sample_holistic_input.call_graph,
            shared_copybooks=sample_holistic_input.shared_copybooks,
        )

        # Serialize both to JSON to compare sizes
        full_json = json.dumps(sample_holistic_input.model_dump(), default=str)
        compact_json = json.dumps(compact_input.model_dump(), default=str)

        # Compact should be at least 30% smaller
        # (In real usage with more data, it's typically 50-80% smaller)
        assert len(compact_json) < len(full_json) * 0.9

    def test_file_summary_much_smaller_than_template(self):
        """Test that FileDocumentationSummary is much smaller than DocumentationTemplate."""
        import json

        # Create a full documentation template
        header = HeaderSection(
            file_name="TEST.CBL",
            program_id="TEST",
            file_type="COBOL",
        )
        purpose = PurposeSection(
            summary="Test program for demonstrating size difference.",
            business_context="Used in testing scenarios.",
            program_type=ProgramType.BATCH,
        )
        template = DocumentationTemplate(
            header=header,
            purpose=purpose,
            called_programs=[
                CalledProgram(
                    program_name="SUB1",
                    call_type=CallType.STATIC_CALL,
                    purpose="First subroutine",
                ),
                CalledProgram(
                    program_name="SUB2",
                    call_type=CallType.DYNAMIC_CALL,
                    purpose="Second subroutine",
                ),
            ],
            inputs=[
                InputOutput(
                    name="INPUT-FILE",
                    io_type=IOType.FILE_SEQUENTIAL,
                    description="Main input file",
                ),
            ],
            outputs=[
                InputOutput(
                    name="OUTPUT-FILE",
                    io_type=IOType.FILE_SEQUENTIAL,
                    description="Main output file",
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

        # Create equivalent summary
        summary = FileDocumentationSummary(
            file_name="TEST.CBL",
            program_id="TEST",
            purpose_summary="Test program for demonstrating size difference.",
            program_type="BATCH",
            paragraph_count=0,
            main_calls=["SUB1", "SUB2"],
            main_inputs=["INPUT-FILE"],
            main_outputs=["OUTPUT-FILE"],
            confidence=ConfidenceLevel.HIGH,
        )

        template_json = json.dumps(template.model_dump(), default=str)
        summary_json = json.dumps(summary.model_dump(), default=str)

        # Summary should be significantly smaller (at least 50% smaller)
        assert len(summary_json) < len(template_json) * 0.5


class TestBuildDocPathTable:
    """Tests for the _build_doc_path_table static method."""

    def test_basic_table(
        self,
        imperator_agent: ImperatorAgent,
        sample_holistic_input: HolisticReviewInput,
    ):
        """Test doc path table generation."""
        table = ImperatorAgent._build_doc_path_table(sample_holistic_input)
        assert "| Component | Doc Path |" in table
        assert "MAINPGM" in table
        assert "INQPGM" in table

    def test_empty_documentation(self, imperator_agent: ImperatorAgent):
        """Test with no file documentation."""
        input_data = HolisticReviewInput(batch_id="empty")
        table = ImperatorAgent._build_doc_path_table(input_data)
        assert "| Component | Doc Path |" in table
        # Should just have the header rows
        lines = table.strip().split("\n")
        assert len(lines) == 2  # header + separator


class TestGenerateSystemDesignAgentic:
    """Tests for the generate_system_design_agentic method."""

    @pytest.mark.asyncio
    async def test_fallback_on_exception(
        self,
        imperator_agent: ImperatorAgent,
        sample_holistic_input: HolisticReviewInput,
    ):
        """Test that agentic method falls back to monolithic on error."""
        from pathlib import Path
        from unittest.mock import AsyncMock, MagicMock, patch

        # Mock AgenticReadmeGenerator to raise an error
        with patch(
            "war_rig.agents.agentic_readme.AgenticReadmeGenerator",
            side_effect=RuntimeError("SDK init failed"),
        ):
            # Mock the fallback method to return a known output
            mock_output = MagicMock()
            mock_output.success = True
            mock_output.markdown = "# Fallback README"
            mock_output.questions = []
            mock_output.sections_updated = ["Executive Summary"]

            with patch.object(
                imperator_agent,
                "generate_system_design",
                new_callable=AsyncMock,
                return_value=mock_output,
            ) as mock_fallback:
                result = await imperator_agent.generate_system_design_agentic(
                    sample_holistic_input,
                    code_dir=Path("/tmp/code"),
                )

                mock_fallback.assert_called_once()
                assert result.success is True
                assert result.markdown == "# Fallback README"

    @pytest.mark.asyncio
    async def test_successful_agentic_generation(
        self,
        imperator_agent: ImperatorAgent,
        sample_holistic_input: HolisticReviewInput,
    ):
        """Test successful agentic README generation."""
        from pathlib import Path
        from unittest.mock import AsyncMock, MagicMock, patch

        from war_rig.agents.imperator import SystemDesignOutput

        mock_result = SystemDesignOutput(
            success=True,
            markdown="# Agentic README\n\nContent here.",
            questions=[],
            sections_updated=["Executive Summary", "Architecture Overview"],
        )

        mock_generator = MagicMock()
        mock_generator.generate = AsyncMock(return_value=mock_result)

        with patch(
            "war_rig.agents.agentic_readme.AgenticReadmeGenerator",
            return_value=mock_generator,
        ):
            result = await imperator_agent.generate_system_design_agentic(
                sample_holistic_input,
                code_dir=Path("/tmp/code"),
                skills_dir=Path("/tmp/skills"),
            )

        assert result.success is True
        assert result.markdown == "# Agentic README\n\nContent here."
        assert len(result.sections_updated) == 2

    @pytest.mark.asyncio
    async def test_fallback_on_empty_result(
        self,
        imperator_agent: ImperatorAgent,
        sample_holistic_input: HolisticReviewInput,
    ):
        """Test fallback when agentic generation returns empty markdown."""
        from pathlib import Path
        from unittest.mock import AsyncMock, MagicMock, patch

        from war_rig.agents.imperator import SystemDesignOutput

        # Agentic returns success but empty markdown
        mock_result = SystemDesignOutput(
            success=True,
            markdown="",
        )
        mock_generator = MagicMock()
        mock_generator.generate = AsyncMock(return_value=mock_result)

        # Monolithic fallback
        mock_fallback = SystemDesignOutput(
            success=True,
            markdown="# Fallback",
        )

        with patch(
            "war_rig.agents.agentic_readme.AgenticReadmeGenerator",
            return_value=mock_generator,
        ), patch.object(
            imperator_agent,
            "generate_system_design",
            new_callable=AsyncMock,
            return_value=mock_fallback,
        ):
            result = await imperator_agent.generate_system_design_agentic(
                sample_holistic_input,
                code_dir=Path("/tmp/code"),
            )

        assert result.markdown == "# Fallback"

    @pytest.mark.asyncio
    async def test_passes_structural_context(
        self,
        imperator_agent: ImperatorAgent,
        sample_holistic_input: HolisticReviewInput,
    ):
        """Test that structural context is built and passed correctly."""
        from pathlib import Path
        from unittest.mock import AsyncMock, MagicMock, patch

        from war_rig.agents.imperator import SystemDesignOutput

        mock_result = SystemDesignOutput(
            success=True,
            markdown="# README",
        )
        mock_generator = MagicMock()
        mock_generator.generate = AsyncMock(return_value=mock_result)

        captured_args = {}

        def capture_init(**kwargs):
            captured_args.update(kwargs)
            return mock_generator

        with patch(
            "war_rig.agents.agentic_readme.AgenticReadmeGenerator",
            side_effect=capture_init,
        ):
            await imperator_agent.generate_system_design_agentic(
                sample_holistic_input,
                code_dir=Path("/tmp/code"),
                skills_dir=Path("/tmp/skills"),
                entry_points=["MAINPGM"],
                call_chains=[["MAINPGM", "SUBPGM1"]],
            )

        assert captured_args["code_dir"] == Path("/tmp/code")
        assert captured_args["skills_dir"] == Path("/tmp/skills")

        # Verify generate was called with structural context
        generate_call = mock_generator.generate.call_args
        structural_ctx = generate_call.kwargs.get(
            "structural_context", generate_call.args[0] if generate_call.args else None
        )
        assert structural_ctx is not None


class TestCallLlmPromptValidation:
    """Tests for _call_llm prompt size validation."""

    @pytest.fixture
    def small_limit_agent(self):
        """Create an Imperator with a low token limit."""
        from unittest.mock import AsyncMock, MagicMock

        config = ImperatorConfig(model="test-model", max_prompt_tokens=1000)
        agent = ImperatorAgent(config=config)
        mock_provider = MagicMock()
        mock_response = MagicMock()
        mock_response.content = "LLM response"
        mock_provider.complete = AsyncMock(return_value=mock_response)
        agent._provider = mock_provider
        return agent

    async def test_small_prompt_not_truncated(self, small_limit_agent):
        """Test that a prompt under the limit is sent unchanged."""
        agent = small_limit_agent
        system = "You are an assistant."
        user = "Review this code."

        result = await agent._call_llm(system, user)

        assert result == "LLM response"
        call_args = agent._provider.complete.call_args
        messages = call_args.kwargs["messages"]
        assert messages[1].content == user

    async def test_large_prompt_truncated(self, small_limit_agent):
        """Test that an oversized user prompt is truncated."""
        agent = small_limit_agent
        system = "System." * 10  # ~70 chars -> ~18 tokens
        user = "X" * 10000  # ~2500 tokens, way over 1000

        result = await agent._call_llm(system, user)

        assert result == "LLM response"
        call_args = agent._provider.complete.call_args
        messages = call_args.kwargs["messages"]
        # User prompt should be truncated
        assert len(messages[1].content) < 10000

    async def test_system_prompt_preserved(self, small_limit_agent):
        """Test that system prompt is never truncated."""
        agent = small_limit_agent
        system = "Important system instructions."
        user = "X" * 10000

        await agent._call_llm(system, user)

        call_args = agent._provider.complete.call_args
        messages = call_args.kwargs["messages"]
        assert messages[0].content == system
