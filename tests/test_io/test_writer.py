"""Tests for documentation writer functionality."""

import tempfile
from pathlib import Path

import pytest

from war_rig.config import SystemConfig
from war_rig.io.writer import DocumentationWriter
from war_rig.models.templates import (
    CallSemantics,
    DocumentationTemplate,
    FunctionCall,
    HeaderSection,
    Paragraph,
    ProgramType,
    PurposeSection,
)


@pytest.fixture
def temp_output_dir():
    """Create a temporary output directory."""
    with tempfile.TemporaryDirectory() as tmpdir:
        yield Path(tmpdir)


@pytest.fixture
def writer(temp_output_dir):
    """Create a DocumentationWriter with temp directory."""
    config = SystemConfig(
        input_directory=temp_output_dir,
        output_directory=temp_output_dir / "output",
    )
    return DocumentationWriter(config)


class TestRenderSequenceDiagram:
    """Tests for _render_sequence_diagram method."""

    def test_empty_paragraphs_returns_empty_string(self, writer):
        """Test that empty paragraphs list returns empty string."""
        template = DocumentationTemplate(
            header=HeaderSection(program_id="TEST"),
            purpose=PurposeSection(summary="Test", program_type=ProgramType.BATCH),
            paragraphs=[],
        )
        result = writer._render_sequence_diagram(template)
        assert result == ""

    def test_no_outgoing_calls_returns_empty_string(self, writer):
        """Test that paragraphs without outgoing calls return empty string."""
        template = DocumentationTemplate(
            header=HeaderSection(program_id="TEST"),
            purpose=PurposeSection(summary="Test", program_type=ProgramType.BATCH),
            paragraphs=[
                Paragraph(paragraph_name="MAIN-PARA", purpose="Main"),
                Paragraph(paragraph_name="SUB-PARA", purpose="Sub"),
            ],
        )
        result = writer._render_sequence_diagram(template)
        assert result == ""

    def test_basic_diagram_without_semantics(self, writer):
        """Test basic diagram generation without call_semantics."""
        template = DocumentationTemplate(
            header=HeaderSection(program_id="TEST"),
            purpose=PurposeSection(summary="Test", program_type=ProgramType.BATCH),
            paragraphs=[
                Paragraph(
                    paragraph_name="MAIN-PARA",
                    purpose="Main",
                    outgoing_calls=[
                        FunctionCall(target="SUB-PARA", call_type="performs"),
                    ],
                ),
                Paragraph(paragraph_name="SUB-PARA", purpose="Sub"),
            ],
        )
        result = writer._render_sequence_diagram(template)

        assert "```mermaid" in result
        assert "sequenceDiagram" in result
        # Hyphens sanitized to underscores in IDs, originals in participant labels
        assert "participant MAIN_PARA as MAIN-PARA" in result
        assert "participant SUB_PARA as SUB-PARA" in result
        assert "MAIN_PARA->>SUB_PARA: performs" in result
        assert "```" in result

    def test_enhanced_diagram_with_inputs(self, writer):
        """Test diagram with call_semantics inputs."""
        template = DocumentationTemplate(
            header=HeaderSection(program_id="TEST"),
            purpose=PurposeSection(summary="Test", program_type=ProgramType.BATCH),
            paragraphs=[
                Paragraph(
                    paragraph_name="MAIN-PARA",
                    purpose="Main",
                    outgoing_calls=[
                        FunctionCall(target="SUB-PARA", call_type="performs"),
                    ],
                ),
            ],
            call_semantics=[
                CallSemantics(
                    caller="MAIN-PARA",
                    callee="SUB-PARA",
                    inputs=["WS-INPUT-FIELD", "WS-FLAG"],
                    outputs=[],
                ),
            ],
        )
        result = writer._render_sequence_diagram(template)

        assert "MAIN_PARA->>SUB_PARA: WS-INPUT-FIELD; WS-FLAG" in result
        # No return arrow since outputs is empty
        assert "SUB_PARA-->>MAIN_PARA" not in result

    def test_enhanced_diagram_with_outputs(self, writer):
        """Test diagram with call_semantics outputs."""
        template = DocumentationTemplate(
            header=HeaderSection(program_id="TEST"),
            purpose=PurposeSection(summary="Test", program_type=ProgramType.BATCH),
            paragraphs=[
                Paragraph(
                    paragraph_name="MAIN-PARA",
                    purpose="Main",
                    outgoing_calls=[
                        FunctionCall(target="SUB-PARA", call_type="performs"),
                    ],
                ),
            ],
            call_semantics=[
                CallSemantics(
                    caller="MAIN-PARA",
                    callee="SUB-PARA",
                    inputs=[],
                    outputs=["WS-RESULT"],
                ),
            ],
        )
        result = writer._render_sequence_diagram(template)

        # Forward arrow should show "performs" when inputs is empty
        assert "MAIN_PARA->>SUB_PARA: performs" in result
        # Return arrow should show output
        assert "SUB_PARA-->>MAIN_PARA: WS-RESULT" in result

    def test_enhanced_diagram_with_inputs_and_outputs(self, writer):
        """Test diagram with both inputs and outputs."""
        template = DocumentationTemplate(
            header=HeaderSection(program_id="TEST"),
            purpose=PurposeSection(summary="Test", program_type=ProgramType.BATCH),
            paragraphs=[
                Paragraph(
                    paragraph_name="MAIN-PARA",
                    purpose="Main",
                    outgoing_calls=[
                        FunctionCall(target="SUB-PARA", call_type="performs"),
                    ],
                ),
            ],
            call_semantics=[
                CallSemantics(
                    caller="MAIN-PARA",
                    callee="SUB-PARA",
                    inputs=["WS-INPUT"],
                    outputs=["WS-OUTPUT"],
                ),
            ],
        )
        result = writer._render_sequence_diagram(template)

        assert "MAIN_PARA->>SUB_PARA: WS-INPUT" in result
        assert "SUB_PARA-->>MAIN_PARA: WS-OUTPUT" in result

    def test_truncation_of_long_input_list(self, writer):
        """Test that long input lists are truncated."""
        template = DocumentationTemplate(
            header=HeaderSection(program_id="TEST"),
            purpose=PurposeSection(summary="Test", program_type=ProgramType.BATCH),
            paragraphs=[
                Paragraph(
                    paragraph_name="MAIN-PARA",
                    purpose="Main",
                    outgoing_calls=[
                        FunctionCall(target="SUB-PARA", call_type="performs"),
                    ],
                ),
            ],
            call_semantics=[
                CallSemantics(
                    caller="MAIN-PARA",
                    callee="SUB-PARA",
                    inputs=["VAR1", "VAR2", "VAR3", "VAR4", "VAR5"],
                    outputs=[],
                ),
            ],
        )
        result = writer._render_sequence_diagram(template)

        # Should show first 3 items + ellipsis (commas become semicolons)
        assert "VAR1; VAR2; VAR3..." in result
        assert "VAR4" not in result
        assert "VAR5" not in result

    def test_truncation_of_long_variable_names(self, writer):
        """Test that long variable names are truncated."""
        template = DocumentationTemplate(
            header=HeaderSection(program_id="TEST"),
            purpose=PurposeSection(summary="Test", program_type=ProgramType.BATCH),
            paragraphs=[
                Paragraph(
                    paragraph_name="MAIN-PARA",
                    purpose="Main",
                    outgoing_calls=[
                        FunctionCall(target="SUB-PARA", call_type="performs"),
                    ],
                ),
            ],
            call_semantics=[
                CallSemantics(
                    caller="MAIN-PARA",
                    callee="SUB-PARA",
                    inputs=["WS-VERY-LONG-VARIABLE-NAME-THAT-EXCEEDS-LIMIT"],
                    outputs=[],
                ),
            ],
        )
        result = writer._render_sequence_diagram(template)

        # Variable name should be truncated (17 chars + "...")
        assert "WS-VERY-LONG-VARI..." in result
        assert "WS-VERY-LONG-VARIABLE-NAME-THAT-EXCEEDS-LIMIT" not in result

    def test_multiple_calls_from_same_paragraph(self, writer):
        """Test diagram with multiple calls from one paragraph."""
        template = DocumentationTemplate(
            header=HeaderSection(program_id="TEST"),
            purpose=PurposeSection(summary="Test", program_type=ProgramType.BATCH),
            paragraphs=[
                Paragraph(
                    paragraph_name="MAIN-PARA",
                    purpose="Main",
                    outgoing_calls=[
                        FunctionCall(target="SUB-PARA-1", call_type="performs"),
                        FunctionCall(target="SUB-PARA-2", call_type="performs"),
                    ],
                ),
            ],
        )
        result = writer._render_sequence_diagram(template)

        assert "MAIN_PARA->>SUB_PARA_1: performs" in result
        assert "MAIN_PARA->>SUB_PARA_2: performs" in result

    def test_paragraph_without_name_skipped(self, writer):
        """Test that paragraphs without names are skipped."""
        template = DocumentationTemplate(
            header=HeaderSection(program_id="TEST"),
            purpose=PurposeSection(summary="Test", program_type=ProgramType.BATCH),
            paragraphs=[
                Paragraph(
                    paragraph_name=None,  # No name
                    purpose="Unnamed",
                    outgoing_calls=[
                        FunctionCall(target="SUB-PARA", call_type="performs"),
                    ],
                ),
                Paragraph(
                    paragraph_name="MAIN-PARA",
                    purpose="Main",
                    outgoing_calls=[
                        FunctionCall(target="SUB-PARA", call_type="performs"),
                    ],
                ),
            ],
        )
        result = writer._render_sequence_diagram(template)

        # Should only have the call from MAIN-PARA (sanitized to MAIN_PARA)
        lines = result.split("\n")
        call_lines = [line for line in lines if "->>" in line and "participant" not in line]
        assert len(call_lines) == 1
        assert "MAIN_PARA" in call_lines[0]

    def test_fallback_without_matching_semantics(self, writer):
        """Test fallback to 'performs' when semantics don't match."""
        template = DocumentationTemplate(
            header=HeaderSection(program_id="TEST"),
            purpose=PurposeSection(summary="Test", program_type=ProgramType.BATCH),
            paragraphs=[
                Paragraph(
                    paragraph_name="MAIN-PARA",
                    purpose="Main",
                    outgoing_calls=[
                        FunctionCall(target="SUB-PARA", call_type="performs"),
                    ],
                ),
            ],
            call_semantics=[
                # Semantics for a different caller-callee pair
                CallSemantics(
                    caller="OTHER-PARA",
                    callee="SUB-PARA",
                    inputs=["WS-INPUT"],
                    outputs=["WS-OUTPUT"],
                ),
            ],
        )
        result = writer._render_sequence_diagram(template)

        # Should use fallback label
        assert "MAIN_PARA->>SUB_PARA: performs" in result
        # Should not have return arrow
        assert "-->>MAIN_PARA" not in result


class TestTruncateLabel:
    """Tests for _truncate_label helper method."""

    def test_empty_list(self, writer):
        """Test that empty list returns empty string."""
        result = writer._truncate_label([])
        assert result == ""

    def test_single_item(self, writer):
        """Test single item list."""
        result = writer._truncate_label(["VAR1"])
        assert result == "VAR1"

    def test_multiple_items_under_limit(self, writer):
        """Test multiple items under the limit."""
        result = writer._truncate_label(["VAR1", "VAR2", "VAR3"])
        assert result == "VAR1, VAR2, VAR3"

    def test_items_over_limit(self, writer):
        """Test items over the default limit of 3."""
        result = writer._truncate_label(["VAR1", "VAR2", "VAR3", "VAR4"])
        assert result == "VAR1, VAR2, VAR3..."

    def test_custom_max_items(self, writer):
        """Test with custom max_items parameter."""
        result = writer._truncate_label(["VAR1", "VAR2", "VAR3", "VAR4"], max_items=2)
        assert result == "VAR1, VAR2..."

    def test_long_item_truncation(self, writer):
        """Test that individual items longer than 20 chars are truncated."""
        result = writer._truncate_label(["THIS-IS-A-VERY-LONG-VARIABLE-NAME"])
        assert result == "THIS-IS-A-VERY-LO..."
        assert len(result) == 20


class TestSequenceDiagramIntegration:
    """Integration tests for sequence diagram in markdown output."""

    def test_sequence_diagram_in_markdown(self, writer):
        """Test that sequence diagram appears in markdown output."""
        template = DocumentationTemplate(
            header=HeaderSection(program_id="TEST", analyzed_by="WAR_RIG"),
            purpose=PurposeSection(summary="Test program", program_type=ProgramType.BATCH),
            paragraphs=[
                Paragraph(
                    paragraph_name="MAIN-PARA",
                    purpose="Main control",
                    outgoing_calls=[
                        FunctionCall(target="SUB-PARA", call_type="performs"),
                    ],
                ),
            ],
        )
        result = writer._template_to_markdown(template)

        assert "## Sequence Diagram" in result
        assert "```mermaid" in result
        assert "sequenceDiagram" in result
        assert "MAIN_PARA->>SUB_PARA: performs" in result

    def test_no_sequence_diagram_without_calls(self, writer):
        """Test that no sequence diagram section when no calls exist."""
        template = DocumentationTemplate(
            header=HeaderSection(program_id="TEST", analyzed_by="WAR_RIG"),
            purpose=PurposeSection(summary="Test program", program_type=ProgramType.BATCH),
            paragraphs=[
                Paragraph(paragraph_name="MAIN-PARA", purpose="Main control"),
            ],
        )
        result = writer._template_to_markdown(template)

        assert "## Sequence Diagram" not in result
