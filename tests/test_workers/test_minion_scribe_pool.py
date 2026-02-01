"""Tests for MinionScribePool module.

This module provides comprehensive unit tests for the MinionScribePool class,
which manages parallel workers for call semantics analysis using fast, cheap LLMs.
"""

import asyncio
import json
from pathlib import Path
from unittest.mock import AsyncMock, MagicMock, patch

import pytest

from war_rig.config import APIConfig, WarRigConfig
from war_rig.models.templates import CallSemantics
from war_rig.providers import CompletionResponse
from war_rig.workers.minion_scribe_pool import MinionScribePool

# =============================================================================
# Fixtures
# =============================================================================


@pytest.fixture
def mock_config() -> MagicMock:
    """Create a mock WarRigConfig with minion scribe settings."""
    config = MagicMock(spec=WarRigConfig)
    config.num_minion_scribes = 4
    config.minion_scribe_batch_size = 5
    config.minion_scribe_model = "anthropic/claude-3-haiku-20240307"
    config.enable_call_semantics = True
    config.api = APIConfig(
        provider="openrouter",
        api_key="test-api-key",
        base_url="https://openrouter.ai/api/v1",
    )
    return config


@pytest.fixture
def mock_provider():
    """Create a mock LLM provider."""
    provider = MagicMock()
    provider.default_model = "test-model"
    provider.complete = AsyncMock(
        return_value=CompletionResponse(
            content='{"calls": []}',
            model="test-model",
            tokens_used=100,
        )
    )
    return provider


@pytest.fixture
def sample_citadel_context():
    """Create sample Citadel context with paragraphs and calls."""
    return [
        {
            "name": "MAIN-PARAGRAPH",
            "type": "paragraph",
            "line": 100,
            "line_end": 150,
            "calls": [
                {"target": "VALIDATE-INPUT", "type": "performs", "line": 110},
                {"target": "PROCESS-DATA", "type": "performs", "line": 120},
            ],
        },
        {
            "name": "VALIDATE-INPUT",
            "type": "paragraph",
            "line": 200,
            "line_end": 250,
            "calls": [],
        },
        {
            "name": "PROCESS-DATA",
            "type": "paragraph",
            "line": 300,
            "line_end": 400,
            "calls": [
                {"target": "WRITE-OUTPUT", "type": "performs", "line": 350},
            ],
        },
        {
            "name": "WRITE-OUTPUT",
            "type": "paragraph",
            "line": 450,
            "line_end": 500,
            "calls": [],
        },
    ]


@pytest.fixture
def sample_working_storage():
    """Create sample WORKING-STORAGE section."""
    return """01  WS-INPUT-RECORD.
    05  WS-CUSTOMER-ID     PIC X(10).
    05  WS-CUSTOMER-NAME   PIC X(50).
    05  WS-AMOUNT          PIC 9(7)V99.

01  WS-OUTPUT-RECORD.
    05  WS-RESULT-CODE     PIC 9(2).
    05  WS-RESULT-MSG      PIC X(100).
"""


@pytest.fixture
def valid_llm_response():
    """Create a valid LLM response with call semantics."""
    return json.dumps(
        {
            "calls": [
                {
                    "caller": "MAIN-PARAGRAPH",
                    "callee": "VALIDATE-INPUT",
                    "inputs": ["WS-CUSTOMER-ID", "WS-AMOUNT"],
                    "outputs": ["WS-VALID-FLAG", "WS-RESULT-CODE"],
                    "purpose": "Validates customer ID and amount before processing",
                },
                {
                    "caller": "MAIN-PARAGRAPH",
                    "callee": "PROCESS-DATA",
                    "inputs": ["WS-INPUT-RECORD"],
                    "outputs": ["WS-OUTPUT-RECORD"],
                    "purpose": "Processes validated input data and generates output",
                },
            ]
        }
    )


# =============================================================================
# Initialization Tests
# =============================================================================


class TestMinionScribePoolInit:
    """Tests for MinionScribePool initialization."""

    def test_init_with_default_config(self, mock_config, mock_provider):
        """Test initialization with default configuration."""
        pool = MinionScribePool(config=mock_config, provider=mock_provider)

        assert pool.num_workers == 4
        assert pool.batch_size == 5
        assert pool.model == "anthropic/claude-3-haiku-20240307"
        assert pool.provider == mock_provider

    def test_init_with_custom_config(self, mock_provider):
        """Test initialization with custom configuration."""
        config = MagicMock(spec=WarRigConfig)
        config.num_minion_scribes = 8
        config.minion_scribe_batch_size = 10
        config.minion_scribe_model = "anthropic/claude-3-sonnet-20240229"
        config.api = APIConfig(
            provider="openrouter",
            api_key="test-key",
            base_url="https://test.api.com",
        )

        pool = MinionScribePool(config=config, provider=mock_provider)

        assert pool.num_workers == 8
        assert pool.batch_size == 10
        assert pool.model == "anthropic/claude-3-sonnet-20240229"


# =============================================================================
# Call Edge Extraction Tests
# =============================================================================


class TestExtractCallEdges:
    """Tests for _extract_call_edges method."""

    def test_extract_call_edges_basic(self, mock_config, mock_provider):
        """Test extracting call edges from Citadel context."""
        pool = MinionScribePool(config=mock_config, provider=mock_provider)

        citadel_context = [
            {
                "name": "PARA-A",
                "calls": [
                    {"target": "PARA-B", "type": "performs"},
                    {"target": "PARA-C", "type": "perform"},
                ],
            },
            {
                "name": "PARA-B",
                "calls": [{"target": "PARA-D", "type": ""}],  # Empty type
            },
        ]

        edges = pool._extract_call_edges(citadel_context)

        assert len(edges) == 3
        assert ("PARA-A", "PARA-B") in edges
        assert ("PARA-A", "PARA-C") in edges
        assert ("PARA-B", "PARA-D") in edges

    def test_extract_call_edges_excludes_external_calls(
        self, mock_config, mock_provider
    ):
        """Test that CALL statements (external programs) are excluded."""
        pool = MinionScribePool(config=mock_config, provider=mock_provider)

        citadel_context = [
            {
                "name": "MAIN",
                "calls": [
                    {"target": "SUB-PARA", "type": "performs"},
                    {"target": "EXTPROG", "type": "calls"},  # External CALL
                    {"target": "LINKPGM", "type": "links"},  # CICS LINK
                ],
            },
        ]

        edges = pool._extract_call_edges(citadel_context)

        # Only PERFORM should be included
        assert len(edges) == 1
        assert ("MAIN", "SUB-PARA") in edges

    def test_extract_call_edges_skips_empty_names(self, mock_config, mock_provider):
        """Test that empty caller/callee names are skipped."""
        pool = MinionScribePool(config=mock_config, provider=mock_provider)

        citadel_context = [
            {
                "name": "",  # Empty caller name
                "calls": [{"target": "PARA-B", "type": "performs"}],
            },
            {
                "name": "PARA-A",
                "calls": [{"target": "", "type": "performs"}],  # Empty callee
            },
        ]

        edges = pool._extract_call_edges(citadel_context)

        assert len(edges) == 0


# =============================================================================
# Batch Creation Tests
# =============================================================================


class TestCreateBatches:
    """Tests for _create_batches method."""

    def test_create_batches_even_split(self, mock_config, mock_provider):
        """Test batch creation with even split."""
        mock_config.minion_scribe_batch_size = 3
        pool = MinionScribePool(config=mock_config, provider=mock_provider)

        edges = [("A", "B"), ("C", "D"), ("E", "F"), ("G", "H"), ("I", "J"), ("K", "L")]
        batches = pool._create_batches(edges)

        assert len(batches) == 2
        assert len(batches[0]) == 3
        assert len(batches[1]) == 3

    def test_create_batches_uneven_split(self, mock_config, mock_provider):
        """Test batch creation with uneven split."""
        mock_config.minion_scribe_batch_size = 3
        pool = MinionScribePool(config=mock_config, provider=mock_provider)

        edges = [("A", "B"), ("C", "D"), ("E", "F"), ("G", "H")]
        batches = pool._create_batches(edges)

        assert len(batches) == 2
        assert len(batches[0]) == 3
        assert len(batches[1]) == 1

    def test_create_batches_single_batch(self, mock_config, mock_provider):
        """Test batch creation when all edges fit in one batch."""
        mock_config.minion_scribe_batch_size = 10
        pool = MinionScribePool(config=mock_config, provider=mock_provider)

        edges = [("A", "B"), ("C", "D"), ("E", "F")]
        batches = pool._create_batches(edges)

        assert len(batches) == 1
        assert len(batches[0]) == 3

    def test_create_batches_empty(self, mock_config, mock_provider):
        """Test batch creation with empty edge list."""
        pool = MinionScribePool(config=mock_config, provider=mock_provider)

        batches = pool._create_batches([])

        assert len(batches) == 0


# =============================================================================
# Empty Semantics Tests
# =============================================================================


class TestEmptySemanticsForBatch:
    """Tests for _empty_semantics_for_batch method."""

    def test_empty_semantics_for_batch(self, mock_config, mock_provider):
        """Test creating empty semantics for a batch."""
        pool = MinionScribePool(config=mock_config, provider=mock_provider)

        batch = [("A", "B"), ("C", "D"), ("E", "F")]
        result = pool._empty_semantics_for_batch(batch)

        assert len(result) == 3
        for i, (caller, callee) in enumerate(batch):
            assert result[i].caller == caller
            assert result[i].callee == callee
            assert result[i].inputs == []
            assert result[i].outputs == []
            assert result[i].purpose is None


# =============================================================================
# Analyze File Tests
# =============================================================================


class TestAnalyzeFile:
    """Tests for analyze_file method."""

    async def test_analyze_file_empty_context(self, mock_config, mock_provider):
        """Test analyze_file with empty citadel context."""
        pool = MinionScribePool(config=mock_config, provider=mock_provider)

        result = await pool.analyze_file(
            source_path=Path("/test/PROGRAM.cbl"),
            citadel_context=[],
        )

        assert result == []
        mock_provider.complete.assert_not_called()

    async def test_analyze_file_no_call_edges(self, mock_config, mock_provider):
        """Test analyze_file when context has no call edges."""
        pool = MinionScribePool(config=mock_config, provider=mock_provider)

        citadel_context = [
            {
                "name": "STANDALONE-PARA",
                "type": "paragraph",
                "calls": [],
            },
        ]

        result = await pool.analyze_file(
            source_path=Path("/test/PROGRAM.cbl"),
            citadel_context=citadel_context,
        )

        assert result == []
        mock_provider.complete.assert_not_called()

    async def test_analyze_file_with_calls(
        self, mock_config, mock_provider, sample_citadel_context, valid_llm_response
    ):
        """Test analyze_file with call edges."""
        mock_provider.complete = AsyncMock(
            return_value=CompletionResponse(
                content=valid_llm_response,
                model="test-model",
                tokens_used=100,
            )
        )

        pool = MinionScribePool(config=mock_config, provider=mock_provider)

        with patch(
            "war_rig.workers.minion_scribe_pool.CallSemanticsAnalyzer"
        ) as MockAnalyzer:
            mock_analyzer = MagicMock()
            mock_analyzer._get_function_bodies = MagicMock(
                return_value={
                    "MAIN-PARAGRAPH": "PERFORM VALIDATE-INPUT.",
                    "VALIDATE-INPUT": "IF WS-VALID-FLAG = 'Y'...",
                    "PROCESS-DATA": "MOVE WS-INPUT TO WS-OUTPUT.",
                    "WRITE-OUTPUT": "WRITE OUTPUT-RECORD.",
                }
            )
            mock_analyzer._truncate_working_storage = MagicMock(return_value=None)
            mock_analyzer._analyze_batch = AsyncMock(
                return_value=[
                    CallSemantics(
                        caller="MAIN-PARAGRAPH",
                        callee="VALIDATE-INPUT",
                        inputs=["WS-CUSTOMER-ID"],
                        outputs=["WS-VALID-FLAG"],
                        purpose="Validate input",
                    ),
                ]
            )
            MockAnalyzer.return_value = mock_analyzer

            result = await pool.analyze_file(
                source_path=Path("/test/PROGRAM.cbl"),
                citadel_context=sample_citadel_context,
            )

        # Should have results (exact count depends on batching)
        assert len(result) >= 1
        assert all(isinstance(cs, CallSemantics) for cs in result)


# =============================================================================
# Parallel Processing Tests
# =============================================================================


class TestParallelProcessing:
    """Tests for parallel processing functionality."""

    async def test_multiple_batches_processed_in_parallel(
        self, mock_config, mock_provider
    ):
        """Test that multiple batches are processed in parallel."""
        mock_config.num_minion_scribes = 3
        mock_config.minion_scribe_batch_size = 2

        # Track which batches were processed and their timing
        processed_batches: list[int] = []
        processing_lock = asyncio.Lock()

        async def mock_analyze_batch(batch, bodies, working_storage, file_name):
            # Simulate some processing time
            await asyncio.sleep(0.01)
            async with processing_lock:
                processed_batches.append(len(batch))
            return [
                CallSemantics(
                    caller=caller,
                    callee=callee,
                    inputs=[],
                    outputs=[],
                    purpose=None,
                )
                for caller, callee in batch
            ]

        pool = MinionScribePool(config=mock_config, provider=mock_provider)

        with patch(
            "war_rig.workers.minion_scribe_pool.CallSemanticsAnalyzer"
        ) as MockAnalyzer:
            mock_analyzer = MagicMock()
            mock_analyzer._get_function_bodies = MagicMock(
                return_value={"A": "code", "B": "code", "C": "code", "D": "code"}
            )
            mock_analyzer._truncate_working_storage = MagicMock(return_value=None)
            mock_analyzer._analyze_batch = AsyncMock(side_effect=mock_analyze_batch)
            MockAnalyzer.return_value = mock_analyzer

            # Create context with 6 call edges -> 3 batches of 2
            citadel_context = [
                {
                    "name": "MAIN",
                    "calls": [
                        {"target": f"PARA-{i}", "type": "performs"}
                        for i in range(6)
                    ],
                }
            ]

            result = await pool.analyze_file(
                source_path=Path("/test/PROGRAM.cbl"),
                citadel_context=citadel_context,
            )

        # All 6 edges should be processed
        assert len(result) == 6
        # All batches should be processed
        assert sum(processed_batches) == 6

    async def test_worker_error_handled_gracefully(self, mock_config, mock_provider):
        """Test that errors in individual batches are handled gracefully."""
        mock_config.num_minion_scribes = 2
        mock_config.minion_scribe_batch_size = 2

        call_count = 0

        async def mock_analyze_batch_with_error(
            batch, bodies, working_storage, file_name
        ):
            nonlocal call_count
            call_count += 1
            if call_count == 1:
                # First batch fails
                raise Exception("Simulated LLM error")
            # Other batches succeed
            return [
                CallSemantics(
                    caller=caller,
                    callee=callee,
                    inputs=["VAR1"],
                    outputs=["VAR2"],
                    purpose="Success",
                )
                for caller, callee in batch
            ]

        pool = MinionScribePool(config=mock_config, provider=mock_provider)

        with patch(
            "war_rig.workers.minion_scribe_pool.CallSemanticsAnalyzer"
        ) as MockAnalyzer:
            mock_analyzer = MagicMock()
            mock_analyzer._get_function_bodies = MagicMock(return_value={})
            mock_analyzer._truncate_working_storage = MagicMock(return_value=None)
            mock_analyzer._analyze_batch = AsyncMock(
                side_effect=mock_analyze_batch_with_error
            )
            MockAnalyzer.return_value = mock_analyzer

            # Create context with 4 call edges -> 2 batches of 2
            citadel_context = [
                {
                    "name": "MAIN",
                    "calls": [
                        {"target": f"PARA-{i}", "type": "performs"}
                        for i in range(4)
                    ],
                }
            ]

            result = await pool.analyze_file(
                source_path=Path("/test/PROGRAM.cbl"),
                citadel_context=citadel_context,
            )

        # Should still return 4 results (some with empty semantics from failed batch)
        assert len(result) == 4

        # At least some should have real data from successful batch
        successful = [cs for cs in result if cs.inputs]
        assert len(successful) >= 2

        # Failed batch should have empty semantics
        failed = [cs for cs in result if not cs.inputs]
        assert len(failed) >= 2


# =============================================================================
# Integration Tests
# =============================================================================


class TestIntegration:
    """Integration tests for MinionScribePool with real-ish config."""

    async def test_full_pipeline(self, mock_provider, sample_citadel_context):
        """Test the full analysis pipeline."""
        config = MagicMock(spec=WarRigConfig)
        config.num_minion_scribes = 2
        config.minion_scribe_batch_size = 2
        config.minion_scribe_model = "anthropic/claude-3-haiku-20240307"
        config.api = APIConfig(
            provider="openrouter",
            api_key="test-key",
            base_url="https://test.api.com",
        )

        # Create response that matches the batch structure
        response_content = json.dumps(
            {
                "calls": [
                    {
                        "caller": "MAIN-PARAGRAPH",
                        "callee": "VALIDATE-INPUT",
                        "inputs": ["WS-INPUT"],
                        "outputs": ["WS-FLAG"],
                        "purpose": "Validate input data",
                    }
                ]
            }
        )

        mock_provider.complete = AsyncMock(
            return_value=CompletionResponse(
                content=response_content,
                model="test-model",
                tokens_used=100,
            )
        )

        pool = MinionScribePool(config=config, provider=mock_provider)

        with patch(
            "war_rig.workers.minion_scribe_pool.CallSemanticsAnalyzer"
        ) as MockAnalyzer:
            mock_analyzer = MagicMock()
            mock_analyzer._get_function_bodies = MagicMock(
                return_value={
                    "MAIN-PARAGRAPH": "PERFORM VALIDATE-INPUT.",
                    "VALIDATE-INPUT": "IF WS-FLAG...",
                    "PROCESS-DATA": "MOVE A TO B.",
                    "WRITE-OUTPUT": "WRITE REC.",
                }
            )
            mock_analyzer._truncate_working_storage = MagicMock(return_value="WS-VAR")
            mock_analyzer._analyze_batch = AsyncMock(
                return_value=[
                    CallSemantics(
                        caller="MAIN-PARAGRAPH",
                        callee="VALIDATE-INPUT",
                        inputs=["WS-INPUT"],
                        outputs=["WS-FLAG"],
                        purpose="Validate input data",
                    )
                ]
            )
            MockAnalyzer.return_value = mock_analyzer

            result = await pool.analyze_file(
                source_path=Path("/test/PROGRAM.cbl"),
                citadel_context=sample_citadel_context,
                working_storage="01 WS-VAR PIC X.",
            )

        # Should have results
        assert len(result) >= 1
        # All should be valid CallSemantics
        for cs in result:
            assert isinstance(cs, CallSemantics)
            assert cs.caller
            assert cs.callee

    async def test_working_storage_passed_to_analyzer(
        self, mock_config, mock_provider, sample_citadel_context, sample_working_storage
    ):
        """Test that working storage is passed to the analyzer."""
        pool = MinionScribePool(config=mock_config, provider=mock_provider)

        truncate_called_with = []

        with patch(
            "war_rig.workers.minion_scribe_pool.CallSemanticsAnalyzer"
        ) as MockAnalyzer:
            mock_analyzer = MagicMock()
            mock_analyzer._get_function_bodies = MagicMock(return_value={})

            def capture_truncate(ws):
                truncate_called_with.append(ws)
                return ws

            mock_analyzer._truncate_working_storage = MagicMock(
                side_effect=capture_truncate
            )
            mock_analyzer._analyze_batch = AsyncMock(return_value=[])
            MockAnalyzer.return_value = mock_analyzer

            await pool.analyze_file(
                source_path=Path("/test/PROGRAM.cbl"),
                citadel_context=sample_citadel_context,
                working_storage=sample_working_storage,
            )

        # Truncate should have been called with the working storage
        assert len(truncate_called_with) >= 1
        assert any(ws == sample_working_storage for ws in truncate_called_with)


# =============================================================================
# BeadsClient Sub-Ticket Tests
# =============================================================================


class TestSubTicketTracking:
    """Tests for sub-ticket creation and tracking with BeadsClient."""

    @pytest.fixture
    def mock_beads_client(self):
        """Create a mock BeadsClient."""
        from unittest.mock import MagicMock

        from war_rig.beads import BeadsClient, ProgramManagerTicket, TicketState

        client = MagicMock(spec=BeadsClient)

        # Mock create_pm_ticket to return a ticket
        def create_ticket(**kwargs):
            ticket = MagicMock(spec=ProgramManagerTicket)
            ticket.ticket_id = f"call-sem-{kwargs.get('batch_index', 0)}"
            return ticket

        client.create_pm_ticket.side_effect = create_ticket
        client.update_ticket_state.return_value = True
        return client

    def test_init_with_beads_client(self, mock_config, mock_provider, mock_beads_client):
        """Test initialization with BeadsClient."""
        pool = MinionScribePool(
            config=mock_config,
            provider=mock_provider,
            beads_client=mock_beads_client,
        )

        assert pool.beads_client is mock_beads_client

    def test_init_without_beads_client(self, mock_config, mock_provider):
        """Test initialization without BeadsClient."""
        pool = MinionScribePool(
            config=mock_config,
            provider=mock_provider,
        )

        assert pool.beads_client is None

    async def test_analyze_file_creates_sub_tickets(
        self, mock_config, mock_provider, mock_beads_client, sample_citadel_context
    ):
        """Test that analyze_file creates sub-tickets when parent_ticket_id provided."""
        mock_config.minion_scribe_batch_size = 2  # Create multiple batches

        pool = MinionScribePool(
            config=mock_config,
            provider=mock_provider,
            beads_client=mock_beads_client,
        )

        with patch(
            "war_rig.workers.minion_scribe_pool.CallSemanticsAnalyzer"
        ) as MockAnalyzer:
            mock_analyzer = MagicMock()
            mock_analyzer._get_function_bodies = MagicMock(return_value={})
            mock_analyzer._truncate_working_storage = MagicMock(return_value=None)
            mock_analyzer._analyze_batch = AsyncMock(
                return_value=[
                    CallSemantics(
                        caller="MAIN-PARAGRAPH",
                        callee="VALIDATE-INPUT",
                        inputs=[],
                        outputs=[],
                        purpose=None,
                    )
                ]
            )
            MockAnalyzer.return_value = mock_analyzer

            await pool.analyze_file(
                source_path=Path("/test/PROGRAM.cbl"),
                citadel_context=sample_citadel_context,
                parent_ticket_id="mem-000001",
            )

        # Sub-tickets should have been created (one per batch)
        assert mock_beads_client.create_pm_ticket.called
        # Check that parent_ticket_id was passed
        for call in mock_beads_client.create_pm_ticket.call_args_list:
            assert call.kwargs.get("parent_ticket_id") == "mem-000001"

    async def test_analyze_file_no_sub_tickets_without_parent_id(
        self, mock_config, mock_provider, mock_beads_client, sample_citadel_context
    ):
        """Test that no sub-tickets are created without parent_ticket_id."""
        pool = MinionScribePool(
            config=mock_config,
            provider=mock_provider,
            beads_client=mock_beads_client,
        )

        with patch(
            "war_rig.workers.minion_scribe_pool.CallSemanticsAnalyzer"
        ) as MockAnalyzer:
            mock_analyzer = MagicMock()
            mock_analyzer._get_function_bodies = MagicMock(return_value={})
            mock_analyzer._truncate_working_storage = MagicMock(return_value=None)
            mock_analyzer._analyze_batch = AsyncMock(return_value=[])
            MockAnalyzer.return_value = mock_analyzer

            await pool.analyze_file(
                source_path=Path("/test/PROGRAM.cbl"),
                citadel_context=sample_citadel_context,
                # No parent_ticket_id
            )

        # No sub-tickets should be created
        mock_beads_client.create_pm_ticket.assert_not_called()

    async def test_sub_ticket_updated_on_success(
        self, mock_config, mock_provider, mock_beads_client, sample_citadel_context
    ):
        """Test that sub-tickets are updated and closed on success."""
        mock_config.minion_scribe_batch_size = 10  # Single batch

        pool = MinionScribePool(
            config=mock_config,
            provider=mock_provider,
            beads_client=mock_beads_client,
        )

        with patch(
            "war_rig.workers.minion_scribe_pool.CallSemanticsAnalyzer"
        ) as MockAnalyzer:
            mock_analyzer = MagicMock()
            mock_analyzer._get_function_bodies = MagicMock(return_value={})
            mock_analyzer._truncate_working_storage = MagicMock(return_value=None)
            mock_analyzer._analyze_batch = AsyncMock(
                return_value=[
                    CallSemantics(
                        caller="MAIN-PARAGRAPH",
                        callee="VALIDATE-INPUT",
                        inputs=["WS-VAR"],
                        outputs=["WS-RESULT"],
                        purpose="Test",
                    )
                ]
            )
            MockAnalyzer.return_value = mock_analyzer

            await pool.analyze_file(
                source_path=Path("/test/PROGRAM.cbl"),
                citadel_context=sample_citadel_context,
                parent_ticket_id="mem-000001",
            )

        # update_ticket_state should have been called with COMPLETED
        assert mock_beads_client.update_ticket_state.called
        # Check that at least one call was for COMPLETED state
        from war_rig.beads import TicketState

        completed_calls = [
            call
            for call in mock_beads_client.update_ticket_state.call_args_list
            if call.args[1] == TicketState.COMPLETED or call.kwargs.get("state") == TicketState.COMPLETED
        ]
        assert len(completed_calls) >= 1

    async def test_sub_ticket_updated_on_failure(
        self, mock_config, mock_provider, mock_beads_client, sample_citadel_context
    ):
        """Test that sub-tickets are marked as BLOCKED on failure."""
        mock_config.minion_scribe_batch_size = 10  # Single batch

        pool = MinionScribePool(
            config=mock_config,
            provider=mock_provider,
            beads_client=mock_beads_client,
        )

        with patch(
            "war_rig.workers.minion_scribe_pool.CallSemanticsAnalyzer"
        ) as MockAnalyzer:
            mock_analyzer = MagicMock()
            mock_analyzer._get_function_bodies = MagicMock(return_value={})
            mock_analyzer._truncate_working_storage = MagicMock(return_value=None)
            mock_analyzer._analyze_batch = AsyncMock(
                side_effect=Exception("LLM error")
            )
            MockAnalyzer.return_value = mock_analyzer

            await pool.analyze_file(
                source_path=Path("/test/PROGRAM.cbl"),
                citadel_context=sample_citadel_context,
                parent_ticket_id="mem-000001",
            )

        # update_ticket_state should have been called with BLOCKED
        from war_rig.beads import TicketState

        blocked_calls = [
            call
            for call in mock_beads_client.update_ticket_state.call_args_list
            if call.args[1] == TicketState.BLOCKED or call.kwargs.get("state") == TicketState.BLOCKED
        ]
        assert len(blocked_calls) >= 1
