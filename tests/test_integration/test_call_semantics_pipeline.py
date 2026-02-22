"""Integration tests for the call semantics pipeline.

This module tests the end-to-end integration of call semantics analysis:
1. ScribeWorker generates documentation for a COBOL file
2. Call semantics enrichment runs and populates the template
3. DocumentationWriter renders the sequence diagram with data flow annotations

The tests verify:
- End-to-end pipeline with mocked LLM
- COBOL-only enrichment (not applied to other file types)
- Graceful degradation when enrichment fails
- Backward compatibility with empty call_semantics
"""

import json
from unittest.mock import AsyncMock, MagicMock, patch

import pytest

from war_rig.analysis.call_semantics import CallSemanticsAnalyzer
from war_rig.beads import (
    ProgramManagerTicket,
    TicketState,
    TicketType,
)
from war_rig.config import APIConfig, SystemConfig, WarRigConfig
from war_rig.io.writer import DocumentationWriter
from war_rig.models.templates import (
    CallSemantics,
    DocumentationTemplate,
    FileType,
    FunctionCall,
    HeaderSection,
    Paragraph,
    ProgramType,
    PurposeSection,
)
from war_rig.providers import CompletionResponse

# =============================================================================
# Sample COBOL Source Code
# =============================================================================

SAMPLE_COBOL_SOURCE = """       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTCALL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-INPUT-RECORD.
           05  WS-CUSTOMER-ID      PIC X(10).
           05  WS-AMOUNT           PIC 9(7)V99.
       01  WS-OUTPUT-RECORD.
           05  WS-RESULT-CODE      PIC 9(2).
           05  WS-RESULT-MSG       PIC X(50).
       01  WS-FLAGS.
           05  WS-VALID-FLAG       PIC X VALUE 'N'.
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALIZE.
           PERFORM 2000-VALIDATE-INPUT.
           IF WS-VALID-FLAG = 'Y'
               PERFORM 3000-PROCESS-DATA
           END-IF.
           PERFORM 9000-CLEANUP.
           STOP RUN.
       1000-INITIALIZE.
           MOVE SPACES TO WS-OUTPUT-RECORD.
           MOVE 'N' TO WS-VALID-FLAG.
       2000-VALIDATE-INPUT.
           IF WS-CUSTOMER-ID NOT = SPACES
               MOVE 'Y' TO WS-VALID-FLAG
           END-IF.
       3000-PROCESS-DATA.
           MOVE 0 TO WS-RESULT-CODE.
           MOVE 'SUCCESS' TO WS-RESULT-MSG.
       9000-CLEANUP.
           DISPLAY 'CLEANUP COMPLETE'.
"""

SAMPLE_PYTHON_SOURCE = """def main():
    print("Hello, World!")

if __name__ == "__main__":
    main()
"""

SAMPLE_JCL_SOURCE = """//TESTJOB  JOB (ACCT),'TEST',CLASS=A
//STEP1    EXEC PGM=IEFBR14
//DD1      DD DSN=TEST.FILE,DISP=SHR
"""


# =============================================================================
# Mock LLM Response for Call Semantics
# =============================================================================

def make_call_semantics_response(calls: list[dict]) -> str:
    """Create a mock LLM response for call semantics."""
    return json.dumps({"calls": calls})


MOCK_CALL_SEMANTICS_RESPONSE = make_call_semantics_response([
    {
        "caller": "0000-MAIN",
        "callee": "1000-INITIALIZE",
        "inputs": [],
        "outputs": ["WS-OUTPUT-RECORD", "WS-VALID-FLAG"],
        "purpose": "Initializes working storage variables",
    },
    {
        "caller": "0000-MAIN",
        "callee": "2000-VALIDATE-INPUT",
        "inputs": ["WS-CUSTOMER-ID"],
        "outputs": ["WS-VALID-FLAG"],
        "purpose": "Validates the customer ID input",
    },
    {
        "caller": "0000-MAIN",
        "callee": "3000-PROCESS-DATA",
        "inputs": ["WS-CUSTOMER-ID", "WS-AMOUNT"],
        "outputs": ["WS-RESULT-CODE", "WS-RESULT-MSG"],
        "purpose": "Processes validated customer data",
    },
    {
        "caller": "0000-MAIN",
        "callee": "9000-CLEANUP",
        "inputs": [],
        "outputs": [],
        "purpose": "Performs cleanup operations",
    },
])


# =============================================================================
# Fixtures
# =============================================================================


@pytest.fixture
def temp_dir(tmp_path):
    """Create a temporary directory for test files."""
    return tmp_path


@pytest.fixture
def cobol_source_file(temp_dir):
    """Create a temporary COBOL source file."""
    cobol_path = temp_dir / "TESTCALL.cbl"
    cobol_path.write_text(SAMPLE_COBOL_SOURCE)
    return cobol_path


@pytest.fixture
def python_source_file(temp_dir):
    """Create a temporary Python source file."""
    py_path = temp_dir / "test.py"
    py_path.write_text(SAMPLE_PYTHON_SOURCE)
    return py_path


@pytest.fixture
def jcl_source_file(temp_dir):
    """Create a temporary JCL source file."""
    jcl_path = temp_dir / "TEST.jcl"
    jcl_path.write_text(SAMPLE_JCL_SOURCE)
    return jcl_path


@pytest.fixture
def mock_api_config():
    """Create a mock API configuration."""
    return APIConfig(
        provider="openrouter",
        api_key="test-api-key",
        base_url="https://openrouter.ai/api/v1",
    )


@pytest.fixture
def mock_llm_provider():
    """Create a mock LLM provider that returns realistic call semantics."""
    provider = MagicMock()
    provider.default_model = "test-model"
    provider.complete = AsyncMock(
        return_value=CompletionResponse(
            content=MOCK_CALL_SEMANTICS_RESPONSE,
            model="test-model",
            tokens_used=200,
        )
    )
    return provider


@pytest.fixture
def mock_llm_provider_failing():
    """Create a mock LLM provider that fails."""
    provider = MagicMock()
    provider.default_model = "test-model"
    provider.complete = AsyncMock(
        side_effect=Exception("API Error: Service unavailable")
    )
    return provider


@pytest.fixture
def system_config(temp_dir):
    """Create a system config for tests."""
    return SystemConfig(
        input_directory=temp_dir,
        output_directory=temp_dir / "output",
    )


@pytest.fixture
def writer(system_config):
    """Create a DocumentationWriter."""
    return DocumentationWriter(system_config)


@pytest.fixture
def sample_citadel_context():
    """Create sample Citadel context for TESTCALL program."""
    return [
        {
            "name": "0000-MAIN",
            "type": "paragraph",
            "line": 14,
            "line_end": 22,
            "calls": [
                {"target": "1000-INITIALIZE", "type": "performs", "line": 15},
                {"target": "2000-VALIDATE-INPUT", "type": "performs", "line": 16},
                {"target": "3000-PROCESS-DATA", "type": "performs", "line": 18},
                {"target": "9000-CLEANUP", "type": "performs", "line": 20},
            ],
        },
        {
            "name": "1000-INITIALIZE",
            "type": "paragraph",
            "line": 23,
            "line_end": 25,
            "calls": [],
        },
        {
            "name": "2000-VALIDATE-INPUT",
            "type": "paragraph",
            "line": 26,
            "line_end": 29,
            "calls": [],
        },
        {
            "name": "3000-PROCESS-DATA",
            "type": "paragraph",
            "line": 30,
            "line_end": 32,
            "calls": [],
        },
        {
            "name": "9000-CLEANUP",
            "type": "paragraph",
            "line": 33,
            "line_end": 34,
            "calls": [],
        },
    ]


@pytest.fixture
def sample_pm_ticket_cobol(cobol_source_file):
    """Create a sample PM ticket for COBOL file."""
    return ProgramManagerTicket(
        ticket_id="war_rig-test-cobol",
        ticket_type=TicketType.DOCUMENTATION,
        state=TicketState.CREATED,
        file_name=cobol_source_file.name,
        program_id="TESTCALL",
        cycle_number=1,
        metadata={
            "source_code": SAMPLE_COBOL_SOURCE,
            "file_path": str(cobol_source_file),
        },
    )


@pytest.fixture
def sample_pm_ticket_python(python_source_file):
    """Create a sample PM ticket for Python file."""
    return ProgramManagerTicket(
        ticket_id="war_rig-test-python",
        ticket_type=TicketType.DOCUMENTATION,
        state=TicketState.CREATED,
        file_name=python_source_file.name,
        program_id="test",
        cycle_number=1,
        metadata={
            "source_code": SAMPLE_PYTHON_SOURCE,
            "file_path": str(python_source_file),
        },
    )


@pytest.fixture
def sample_pm_ticket_jcl(jcl_source_file):
    """Create a sample PM ticket for JCL file."""
    return ProgramManagerTicket(
        ticket_id="war_rig-test-jcl",
        ticket_type=TicketType.DOCUMENTATION,
        state=TicketState.CREATED,
        file_name=jcl_source_file.name,
        program_id="TESTJOB",
        cycle_number=1,
        metadata={
            "source_code": SAMPLE_JCL_SOURCE,
            "file_path": str(jcl_source_file),
        },
    )


@pytest.fixture
def template_with_paragraphs_and_calls():
    """Create a template with paragraphs that have outgoing calls."""
    return DocumentationTemplate(
        header=HeaderSection(
            program_id="TESTCALL",
            file_name="TESTCALL.cbl",
            file_type=FileType.COBOL,
            analyzed_by="WAR_RIG",
        ),
        purpose=PurposeSection(
            summary="Test program for call semantics",
            program_type=ProgramType.BATCH,
        ),
        paragraphs=[
            Paragraph(
                paragraph_name="0000-MAIN",
                purpose="Main control paragraph",
                outgoing_calls=[
                    FunctionCall(target="1000-INITIALIZE", call_type="performs"),
                    FunctionCall(target="2000-VALIDATE-INPUT", call_type="performs"),
                    FunctionCall(target="3000-PROCESS-DATA", call_type="performs"),
                    FunctionCall(target="9000-CLEANUP", call_type="performs"),
                ],
            ),
            Paragraph(
                paragraph_name="1000-INITIALIZE",
                purpose="Initialize working storage",
            ),
            Paragraph(
                paragraph_name="2000-VALIDATE-INPUT",
                purpose="Validate input data",
            ),
            Paragraph(
                paragraph_name="3000-PROCESS-DATA",
                purpose="Process customer data",
            ),
            Paragraph(
                paragraph_name="9000-CLEANUP",
                purpose="Cleanup operations",
            ),
        ],
    )


@pytest.fixture
def template_without_call_semantics(template_with_paragraphs_and_calls):
    """Template with calls but no call semantics populated."""
    return template_with_paragraphs_and_calls


@pytest.fixture
def template_with_call_semantics(template_with_paragraphs_and_calls):
    """Template with call semantics populated."""
    template = template_with_paragraphs_and_calls
    template.call_semantics = [
        CallSemantics(
            caller="0000-MAIN",
            callee="1000-INITIALIZE",
            inputs=[],
            outputs=["WS-OUTPUT-RECORD", "WS-VALID-FLAG"],
            purpose="Initializes working storage variables",
        ),
        CallSemantics(
            caller="0000-MAIN",
            callee="2000-VALIDATE-INPUT",
            inputs=["WS-CUSTOMER-ID"],
            outputs=["WS-VALID-FLAG"],
            purpose="Validates the customer ID input",
        ),
        CallSemantics(
            caller="0000-MAIN",
            callee="3000-PROCESS-DATA",
            inputs=["WS-CUSTOMER-ID", "WS-AMOUNT"],
            outputs=["WS-RESULT-CODE", "WS-RESULT-MSG"],
            purpose="Processes validated customer data",
        ),
        CallSemantics(
            caller="0000-MAIN",
            callee="9000-CLEANUP",
            inputs=[],
            outputs=[],
            purpose="Performs cleanup operations",
        ),
    ]
    return template


# =============================================================================
# Test Classes
# =============================================================================


class TestCallSemanticsAnalyzerIntegration:
    """Integration tests for CallSemanticsAnalyzer with mocked LLM."""

    async def test_analyzer_processes_cobol_and_returns_semantics(
        self, mock_api_config, mock_llm_provider, sample_citadel_context, cobol_source_file
    ):
        """Test that analyzer processes COBOL file and returns call semantics."""
        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config,
            provider=mock_llm_provider,
        )

        # Mock function bodies
        with patch.object(analyzer, "_get_function_bodies") as mock_bodies:
            mock_bodies.return_value = {
                "0000-MAIN": "PERFORM 1000-INITIALIZE.\nPERFORM 2000-VALIDATE-INPUT.",
                "1000-INITIALIZE": "MOVE SPACES TO WS-OUTPUT-RECORD.",
                "2000-VALIDATE-INPUT": "IF WS-CUSTOMER-ID NOT = SPACES...",
                "3000-PROCESS-DATA": "MOVE 0 TO WS-RESULT-CODE.",
                "9000-CLEANUP": "DISPLAY 'CLEANUP COMPLETE'.",
            }

            result = await analyzer.analyze_file(
                source_path=cobol_source_file,
                citadel_context=sample_citadel_context,
            )

        # Should return call semantics
        assert len(result) >= 4
        for cs in result:
            assert isinstance(cs, CallSemantics)
            assert cs.caller
            assert cs.callee

        # Check specific caller-callee pairs exist
        pairs = {(cs.caller, cs.callee) for cs in result}
        assert ("0000-MAIN", "1000-INITIALIZE") in pairs
        assert ("0000-MAIN", "2000-VALIDATE-INPUT") in pairs

    async def test_analyzer_handles_empty_context(
        self, mock_api_config, mock_llm_provider, cobol_source_file
    ):
        """Test that analyzer handles empty Citadel context gracefully."""
        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config,
            provider=mock_llm_provider,
        )

        result = await analyzer.analyze_file(
            source_path=cobol_source_file,
            citadel_context=[],
        )

        assert result == []
        mock_llm_provider.complete.assert_not_called()

    async def test_analyzer_handles_llm_failure(
        self, mock_api_config, mock_llm_provider_failing, sample_citadel_context, cobol_source_file
    ):
        """Test that analyzer returns empty semantics when LLM fails."""
        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config,
            provider=mock_llm_provider_failing,
        )

        with patch.object(analyzer, "_get_function_bodies") as mock_bodies:
            mock_bodies.return_value = {
                "0000-MAIN": "PERFORM 1000-INITIALIZE.",
                "1000-INITIALIZE": "MOVE SPACES TO WS-OUTPUT-RECORD.",
                "2000-VALIDATE-INPUT": "IF WS-CUSTOMER-ID NOT = SPACES...",
                "3000-PROCESS-DATA": "MOVE 0 TO WS-RESULT-CODE.",
                "9000-CLEANUP": "DISPLAY 'CLEANUP COMPLETE'.",
            }

            # Should not raise, returns empty semantics for failed batch
            result = await analyzer.analyze_file(
                source_path=cobol_source_file,
                citadel_context=sample_citadel_context,
            )

        # Should return semantics with empty inputs/outputs
        assert len(result) >= 4
        for cs in result:
            assert cs.inputs == []
            assert cs.outputs == []
            assert cs.purpose is None


class TestWriterSequenceDiagramIntegration:
    """Integration tests for DocumentationWriter sequence diagram rendering."""

    def test_writer_renders_diagram_without_call_semantics(
        self, writer, template_without_call_semantics
    ):
        """Test that writer renders basic diagram when call_semantics is empty."""
        result = writer._render_sequence_diagram(template_without_call_semantics)

        assert "```mermaid" in result
        assert "sequenceDiagram" in result
        # Should use fallback "performs" labels (hyphens sanitized to underscores in IDs)
        assert "0000_MAIN->>1000_INITIALIZE: performs" in result
        assert "0000_MAIN->>2000_VALIDATE_INPUT: performs" in result
        assert "0000_MAIN->>3000_PROCESS_DATA: performs" in result
        assert "0000_MAIN->>9000_CLEANUP: performs" in result

    def test_writer_renders_enhanced_diagram_with_call_semantics(
        self, writer, template_with_call_semantics
    ):
        """Test that writer renders enhanced diagram with data flow annotations."""
        result = writer._render_sequence_diagram(template_with_call_semantics)

        assert "```mermaid" in result
        assert "sequenceDiagram" in result

        # Check for enhanced labels with inputs (hyphens sanitized, commas become slashes)
        assert "0000_MAIN->>2000_VALIDATE_INPUT: WS-CUSTOMER-ID" in result
        assert "0000_MAIN->>3000_PROCESS_DATA: WS-CUSTOMER-ID / WS-AMOUNT" in result

        # Check for return arrows with outputs
        assert "1000_INITIALIZE-->>0000_MAIN: WS-OUTPUT-RECORD / WS-VALID-FLAG" in result
        assert "2000_VALIDATE_INPUT-->>0000_MAIN: WS-VALID-FLAG" in result
        assert "3000_PROCESS_DATA-->>0000_MAIN: WS-RESULT-CODE / WS-RESULT-MSG" in result

        # Cleanup has no inputs/outputs, should show "performs"
        assert "0000_MAIN->>9000_CLEANUP: performs" in result
        # No return arrow for cleanup
        assert "9000_CLEANUP-->>0000_MAIN" not in result

    def test_writer_markdown_includes_sequence_diagram_section(
        self, writer, template_with_call_semantics
    ):
        """Test that markdown output includes Sequence Diagram section."""
        result = writer._template_to_markdown(template_with_call_semantics)

        assert "## Sequence Diagram" in result
        assert "```mermaid" in result
        assert "sequenceDiagram" in result
        # Enhanced data flow should be present
        assert "WS-CUSTOMER-ID" in result

    def test_writer_markdown_no_diagram_without_calls(self, writer):
        """Test that markdown has no diagram section when no calls exist."""
        template = DocumentationTemplate(
            header=HeaderSection(program_id="SIMPLE", analyzed_by="WAR_RIG"),
            purpose=PurposeSection(summary="Simple program", program_type=ProgramType.BATCH),
            paragraphs=[
                Paragraph(paragraph_name="MAIN-PARA", purpose="Main"),
            ],
        )
        result = writer._template_to_markdown(template)

        assert "## Sequence Diagram" not in result


class TestGracefulDegradation:
    """Tests for graceful degradation when call semantics enrichment fails."""

    def test_empty_call_semantics_results_in_fallback_labels(
        self, writer, template_with_paragraphs_and_calls
    ):
        """Test that empty call_semantics results in 'performs' fallback labels."""
        # Template has outgoing_calls but no call_semantics
        template = template_with_paragraphs_and_calls
        template.call_semantics = []

        result = writer._render_sequence_diagram(template)

        # All calls should show "performs" fallback (hyphens sanitized to underscores in IDs)
        assert "0000_MAIN->>1000_INITIALIZE: performs" in result
        assert "0000_MAIN->>2000_VALIDATE_INPUT: performs" in result
        assert "0000_MAIN->>3000_PROCESS_DATA: performs" in result
        assert "0000_MAIN->>9000_CLEANUP: performs" in result
        # No return arrows (check lines that aren't participant declarations)
        lines = [l for l in result.split("\n") if "-->>" in l]
        assert len(lines) == 0

    def test_partial_call_semantics_uses_fallback_for_missing(self, writer):
        """Test that missing semantics for some calls use fallback."""
        template = DocumentationTemplate(
            header=HeaderSection(program_id="TEST", analyzed_by="WAR_RIG"),
            purpose=PurposeSection(summary="Test", program_type=ProgramType.BATCH),
            paragraphs=[
                Paragraph(
                    paragraph_name="MAIN",
                    purpose="Main",
                    outgoing_calls=[
                        FunctionCall(target="SUB-1", call_type="performs"),
                        FunctionCall(target="SUB-2", call_type="performs"),
                    ],
                ),
            ],
            call_semantics=[
                # Only semantics for SUB-1, not SUB-2
                CallSemantics(
                    caller="MAIN",
                    callee="SUB-1",
                    inputs=["WS-INPUT"],
                    outputs=["WS-OUTPUT"],
                ),
            ],
        )

        result = writer._render_sequence_diagram(template)

        # SUB-1 should have enhanced labels (hyphens sanitized in IDs)
        assert "MAIN->>SUB_1: WS-INPUT" in result
        assert "SUB_1-->>MAIN: WS-OUTPUT" in result

        # SUB-2 should use fallback
        assert "MAIN->>SUB_2: performs" in result

    def test_call_semantics_with_empty_inputs_outputs(self, writer):
        """Test that call semantics with empty inputs/outputs uses fallback."""
        template = DocumentationTemplate(
            header=HeaderSection(program_id="TEST", analyzed_by="WAR_RIG"),
            purpose=PurposeSection(summary="Test", program_type=ProgramType.BATCH),
            paragraphs=[
                Paragraph(
                    paragraph_name="MAIN",
                    purpose="Main",
                    outgoing_calls=[
                        FunctionCall(target="SUB", call_type="performs"),
                    ],
                ),
            ],
            call_semantics=[
                CallSemantics(
                    caller="MAIN",
                    callee="SUB",
                    inputs=[],
                    outputs=[],
                    purpose="Empty semantics",
                ),
            ],
        )

        result = writer._render_sequence_diagram(template)

        # Should use fallback label
        assert "MAIN->>SUB: performs" in result
        # No return arrow
        assert "SUB-->>MAIN" not in result


class TestFileTypeFiltering:
    """Tests for file type filtering in call semantics enrichment."""

    async def test_cobol_file_gets_enrichment(
        self, mock_api_config, mock_llm_provider, sample_citadel_context, cobol_source_file
    ):
        """Test that COBOL files receive call semantics enrichment."""
        from war_rig.beads import BeadsClient
        from war_rig.workers.scribe_pool import ScribeWorker

        # Create mock config
        mock_config = MagicMock(spec=WarRigConfig)
        mock_config.num_scribes = 1
        mock_config.input_directory = cobol_source_file.parent
        mock_config.output_directory = cobol_source_file.parent / "output"
        mock_config.exit_on_error = True
        mock_config.enable_call_semantics = True
        mock_config.minion_scribe_model = "anthropic/claude-3-haiku-20240307"
        mock_config.num_minion_scribes = 4
        mock_config.minion_scribe_batch_size = 5
        mock_config.scribe = MagicMock()
        mock_config.scribe.model = "claude-sonnet-4-20250514"
        mock_config.scribe.temperature = 0.3
        mock_config.scribe.max_prompt_tokens = 15000
        mock_config.api = mock_api_config

        mock_beads = MagicMock(spec=BeadsClient)

        worker = ScribeWorker(
            worker_id="test-scribe",
            config=mock_config,
            beads_client=mock_beads,
        )

        # Verify COBOL file type is detected
        file_type = worker._determine_file_type("TEST.cbl")
        assert file_type == FileType.COBOL

        # Verify method signature exists
        assert hasattr(worker, "_enrich_call_semantics")

    def test_python_file_type_detection(self, temp_dir):
        """Test that Python files are detected as OTHER type."""
        from war_rig.beads import BeadsClient
        from war_rig.workers.scribe_pool import ScribeWorker

        mock_config = MagicMock(spec=WarRigConfig)
        mock_config.input_directory = temp_dir
        mock_config.output_directory = temp_dir / "output"
        mock_config.enable_call_semantics = True
        mock_config.minion_scribe_model = "anthropic/claude-3-haiku-20240307"
        mock_config.num_minion_scribes = 4
        mock_config.minion_scribe_batch_size = 5
        mock_config.scribe = MagicMock()
        mock_config.scribe.max_prompt_tokens = 15000
        mock_config.api = MagicMock()

        mock_beads = MagicMock(spec=BeadsClient)

        worker = ScribeWorker(
            worker_id="test-scribe",
            config=mock_config,
            beads_client=mock_beads,
        )

        # Python files should be OTHER
        assert worker._determine_file_type("test.py") == FileType.OTHER

    def test_jcl_file_type_detection(self, temp_dir):
        """Test that JCL files are detected as JCL type."""
        from war_rig.beads import BeadsClient
        from war_rig.workers.scribe_pool import ScribeWorker

        mock_config = MagicMock(spec=WarRigConfig)
        mock_config.input_directory = temp_dir
        mock_config.output_directory = temp_dir / "output"
        mock_config.enable_call_semantics = True
        mock_config.minion_scribe_model = "anthropic/claude-3-haiku-20240307"
        mock_config.num_minion_scribes = 4
        mock_config.minion_scribe_batch_size = 5
        mock_config.scribe = MagicMock()
        mock_config.scribe.max_prompt_tokens = 15000
        mock_config.api = MagicMock()

        mock_beads = MagicMock(spec=BeadsClient)

        worker = ScribeWorker(
            worker_id="test-scribe",
            config=mock_config,
            beads_client=mock_beads,
        )

        # JCL files should be JCL
        assert worker._determine_file_type("JOB.jcl") == FileType.JCL


class TestEnrichCallSemanticsMethod:
    """Tests for _enrich_call_semantics method behavior."""

    async def test_enrich_returns_unchanged_template_for_non_cobol(self, temp_dir):
        """Test that enrichment returns template unchanged for non-COBOL files."""
        from war_rig.beads import BeadsClient
        from war_rig.workers.scribe_pool import ScribeWorker

        mock_config = MagicMock(spec=WarRigConfig)
        mock_config.input_directory = temp_dir
        mock_config.output_directory = temp_dir / "output"
        mock_config.scribe = MagicMock()
        mock_config.scribe.max_prompt_tokens = 15000
        mock_config.api = MagicMock()
        mock_config.api.provider = "openrouter"
        mock_config.api.api_key = "test-key"
        mock_config.enable_call_semantics = True
        mock_config.minion_scribe_model = "anthropic/claude-3-haiku-20240307"
        mock_config.num_minion_scribes = 4
        mock_config.minion_scribe_batch_size = 5

        mock_beads = MagicMock(spec=BeadsClient)

        worker = ScribeWorker(
            worker_id="test-scribe",
            config=mock_config,
            beads_client=mock_beads,
        )

        # Create a template
        template = DocumentationTemplate(
            header=HeaderSection(program_id="TEST"),
            purpose=PurposeSection(summary="Test", program_type=ProgramType.BATCH),
        )

        # Create a ticket
        ticket = ProgramManagerTicket(
            ticket_id="test-ticket",
            ticket_type=TicketType.DOCUMENTATION,
            state=TicketState.CREATED,
            file_name="test.py",
            program_id="test",
            cycle_number=1,
            metadata={},
        )

        # Call enrich for Python file (should skip)
        result = await worker._enrich_call_semantics(
            template=template,
            ticket=ticket,
            file_type=FileType.OTHER,
        )

        # Template should be unchanged
        assert result is template
        assert result.call_semantics == []

    async def test_enrich_returns_unchanged_template_without_citadel(self, temp_dir):
        """Test that enrichment returns template unchanged when Citadel is not available."""
        from war_rig.beads import BeadsClient
        from war_rig.workers.scribe_pool import ScribeWorker

        mock_config = MagicMock(spec=WarRigConfig)
        mock_config.input_directory = temp_dir
        mock_config.output_directory = temp_dir / "output"
        mock_config.scribe = MagicMock()
        mock_config.scribe.max_prompt_tokens = 15000
        mock_config.api = MagicMock()
        mock_config.api.provider = "openrouter"
        mock_config.api.api_key = "test-key"
        mock_config.enable_call_semantics = True
        mock_config.minion_scribe_model = "anthropic/claude-3-haiku-20240307"
        mock_config.num_minion_scribes = 4
        mock_config.minion_scribe_batch_size = 5

        mock_beads = MagicMock(spec=BeadsClient)

        worker = ScribeWorker(
            worker_id="test-scribe",
            config=mock_config,
            beads_client=mock_beads,
        )

        # Ensure Citadel is None
        worker._citadel = None

        # Create a template
        template = DocumentationTemplate(
            header=HeaderSection(program_id="TESTCALL"),
            purpose=PurposeSection(summary="Test", program_type=ProgramType.BATCH),
        )

        # Create a ticket for COBOL file
        ticket = ProgramManagerTicket(
            ticket_id="test-ticket",
            ticket_type=TicketType.DOCUMENTATION,
            state=TicketState.CREATED,
            file_name="TESTCALL.cbl",
            program_id="TESTCALL",
            cycle_number=1,
            metadata={},
        )

        # Call enrich for COBOL file without Citadel
        result = await worker._enrich_call_semantics(
            template=template,
            ticket=ticket,
            file_type=FileType.COBOL,
        )

        # Template should be unchanged
        assert result is template
        assert result.call_semantics == []


class TestEndToEndPipeline:
    """End-to-end pipeline tests with mocked components."""

    def test_complete_pipeline_output(self, writer, template_with_call_semantics, temp_dir):
        """Test complete pipeline from template with semantics to markdown output."""
        # Generate markdown
        markdown = writer._template_to_markdown(template_with_call_semantics)

        # Verify all expected sections
        assert "# TESTCALL" in markdown
        assert "## Purpose" in markdown
        assert "## Key Paragraphs" in markdown
        assert "## Sequence Diagram" in markdown

        # Verify sequence diagram content
        assert "```mermaid" in markdown
        assert "sequenceDiagram" in markdown
        assert "WS-CUSTOMER-ID" in markdown  # Input variable
        assert "WS-RESULT-CODE" in markdown  # Output variable

        # Verify data flow arrows
        assert "->>" in markdown  # Forward arrows
        assert "-->>" in markdown  # Return arrows

    def test_pipeline_output_file_creation(self, writer, template_with_call_semantics, temp_dir):
        """Test that pipeline creates output files correctly."""
        # Create a mock state
        state = {
            "final_template": template_with_call_semantics,
            "file_type": FileType.COBOL,
            "decision": "WITNESSED",
        }

        # Write result
        outputs = writer.write_result(state)

        # Verify files created
        assert "final_json" in outputs
        assert outputs["final_json"].exists()
        assert "final_md" in outputs
        assert outputs["final_md"].exists()

        # Verify markdown content
        md_content = outputs["final_md"].read_text()
        assert "## Sequence Diagram" in md_content
        assert "WS-CUSTOMER-ID" in md_content
