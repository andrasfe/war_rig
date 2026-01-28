"""Tests for Challenger worker pool.

Tests for ChallengerWorker and ChallengerWorkerPool classes that implement
parallel validation of documentation tickets.
"""

import asyncio
import json
from datetime import datetime
from unittest.mock import AsyncMock, MagicMock, patch

import pytest

from war_rig.agents.challenger import ChallengerOutput
from war_rig.beads import (
    BeadsClient,
    BeadsPriority,
    ProgramManagerTicket,
    TicketState,
    TicketType,
)
from war_rig.config import WarRigConfig, ChallengerConfig
from war_rig.models.templates import (
    DocumentationTemplate,
    FileType,
    HeaderSection,
    PurposeSection,
    ProgramType,
)
from war_rig.models.tickets import (
    ChallengerQuestion,
    QuestionSeverity,
    QuestionType,
)
from war_rig.workers.challenger_pool import (
    ChallengerWorker,
    ChallengerWorkerPool,
    ValidationResult,
    WorkerState,
    WorkerStatus,
)


# =============================================================================
# Fixtures
# =============================================================================


@pytest.fixture
def mock_beads_client() -> MagicMock:
    """Create a mock BeadsClient."""
    client = MagicMock(spec=BeadsClient)
    client.get_available_tickets.return_value = []
    client.claim_ticket.return_value = True
    client.update_ticket_state.return_value = True
    client.create_pm_ticket.return_value = ProgramManagerTicket(
        ticket_id="war_rig-rework001",
        ticket_type=TicketType.CLARIFICATION,
        state=TicketState.CREATED,
        file_name="TEST.cbl",
        program_id="TEST",
        cycle_number=2,
    )
    return client


@pytest.fixture
def mock_config(tmp_path) -> MagicMock:
    """Create a mock WarRigConfig."""
    from pathlib import Path

    config = MagicMock(spec=WarRigConfig)
    config.num_challengers = 3
    config.max_questions_per_round = 5
    config.max_iterations = 2
    config.output_directory = tmp_path / "output"
    config.output_directory.mkdir(parents=True, exist_ok=True)
    config.input_directory = tmp_path / "input"
    config.input_directory.mkdir(parents=True, exist_ok=True)
    config.exit_on_error = True
    config.challenger = MagicMock(spec=ChallengerConfig)
    config.challenger.model = "claude-sonnet-4-20250514"
    config.challenger.temperature = 0.3
    config.challenger.max_prompt_tokens = 15000
    config.api = MagicMock()
    config.api.provider = "openrouter"
    config.api.api_key = "test-key"
    config.api.base_url = "https://test.api.com"
    return config


@pytest.fixture
def sample_template() -> DocumentationTemplate:
    """Create a sample documentation template."""
    return DocumentationTemplate(
        header=HeaderSection(
            program_id="TESTPROG",
            file_name="TESTPROG.cbl",
            file_type=FileType.COBOL,
            analyzed_by="TEST_RIG",
            analyzed_at=datetime(2024, 1, 1, 12, 0, 0),
            iteration_count=1,
        ),
        purpose=PurposeSection(
            summary="Test program",
            business_context="Test context",
            program_type=ProgramType.BATCH,
            citations=[1, 2, 3],
        ),
    )


@pytest.fixture
def sample_validation_ticket(sample_template) -> ProgramManagerTicket:
    """Create a sample VALIDATION ticket with proper metadata."""
    return ProgramManagerTicket(
        ticket_id="war_rig-val001",
        ticket_type=TicketType.VALIDATION,
        state=TicketState.CREATED,
        file_name="TESTPROG.cbl",
        program_id="TESTPROG",
        cycle_number=1,
        metadata={
            "template": sample_template.model_dump(),
            "source_code": """       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       PROCEDURE DIVISION.
       0000-MAIN.
           DISPLAY 'HELLO WORLD'.
           STOP RUN.
""",
            "file_type": "cobol",
        },
    )


# =============================================================================
# WorkerStatus Tests
# =============================================================================


class TestWorkerStatus:
    """Tests for WorkerStatus dataclass."""

    def test_create_default_status(self):
        """Test creating status with defaults."""
        status = WorkerStatus(worker_id="challenger-1")

        assert status.worker_id == "challenger-1"
        assert status.state == WorkerState.IDLE
        assert status.current_ticket_id is None
        assert status.tickets_processed == 0
        assert status.tickets_validated == 0
        assert status.tickets_rejected == 0
        assert status.error_count == 0

    def test_create_full_status(self):
        """Test creating status with all fields."""
        now = datetime.utcnow()
        status = WorkerStatus(
            worker_id="challenger-2",
            state=WorkerState.PROCESSING,
            current_ticket_id="war_rig-abc123",
            tickets_processed=10,
            tickets_validated=7,
            tickets_rejected=3,
            last_activity=now,
            error_count=1,
        )

        assert status.current_ticket_id == "war_rig-abc123"
        assert status.tickets_processed == 10
        assert status.tickets_validated == 7
        assert status.tickets_rejected == 3
        assert status.error_count == 1


# =============================================================================
# WorkerState Tests
# =============================================================================


class TestWorkerState:
    """Tests for WorkerState enum."""

    def test_all_states_exist(self):
        """Test that all expected states exist."""
        assert WorkerState.IDLE.value == "idle"
        assert WorkerState.POLLING.value == "polling"
        assert WorkerState.PROCESSING.value == "processing"
        assert WorkerState.STOPPING.value == "stopping"
        assert WorkerState.STOPPED.value == "stopped"


# =============================================================================
# ValidationResult Tests
# =============================================================================


class TestValidationResult:
    """Tests for ValidationResult dataclass."""

    def test_create_successful_valid_result(self):
        """Test creating a successful validation result with valid docs."""
        result = ValidationResult(
            success=True,
            is_valid=True,
            issues_found=[],
            blocking_questions=[],
        )

        assert result.success is True
        assert result.is_valid is True
        assert len(result.issues_found) == 0
        assert len(result.blocking_questions) == 0

    def test_create_successful_invalid_result(self):
        """Test creating a successful validation result with invalid docs."""
        result = ValidationResult(
            success=True,
            is_valid=False,
            issues_found=["Purpose section is vague"],
            blocking_questions=[
                {
                    "question_id": "Q-001",
                    "section": "purpose",
                    "question_type": "CLARIFICATION",
                    "question": "What is the business context?",
                    "severity": "BLOCKING",
                    "evidence": [1, 2],
                }
            ],
        )

        assert result.success is True
        assert result.is_valid is False
        assert len(result.issues_found) == 1
        assert len(result.blocking_questions) == 1

    def test_create_failed_result(self):
        """Test creating a failed validation result."""
        result = ValidationResult(
            success=False,
            is_valid=False,
            issues_found=["Validation error occurred"],
        )

        assert result.success is False
        assert result.is_valid is False


# =============================================================================
# ChallengerWorker Tests
# =============================================================================


class TestChallengerWorker:
    """Tests for ChallengerWorker class."""

    def test_create_worker(self, mock_config, mock_beads_client):
        """Test worker creation."""
        worker = ChallengerWorker(
            worker_id="challenger-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        assert worker.worker_id == "challenger-1"
        assert worker.config == mock_config
        assert worker.beads_client == mock_beads_client
        assert worker.poll_interval == 2.0
        assert worker.status.state == WorkerState.IDLE

    def test_create_worker_custom_poll_interval(self, mock_config, mock_beads_client):
        """Test worker creation with custom poll interval."""
        worker = ChallengerWorker(
            worker_id="challenger-1",
            config=mock_config,
            beads_client=mock_beads_client,
            poll_interval=5.0,
        )

        assert worker.poll_interval == 5.0

    def test_get_status(self, mock_config, mock_beads_client):
        """Test get_status returns current WorkerStatus."""
        worker = ChallengerWorker(
            worker_id="challenger-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        status = worker.get_status()

        assert isinstance(status, WorkerStatus)
        assert status.worker_id == "challenger-1"
        assert status.state == WorkerState.IDLE

    @pytest.mark.asyncio
    async def test_start_creates_task(self, mock_config, mock_beads_client):
        """Test that start() creates a background task."""
        mock_beads_client.get_available_tickets.return_value = []

        worker = ChallengerWorker(
            worker_id="challenger-1",
            config=mock_config,
            beads_client=mock_beads_client,
            poll_interval=0.1,
        )

        await worker.start()

        # Task should be created
        assert worker._task is not None
        assert not worker._task.done()

        # Clean up
        await worker.stop()

    @pytest.mark.asyncio
    async def test_start_warns_if_already_running(self, mock_config, mock_beads_client):
        """Test that start() warns if worker is already running."""
        mock_beads_client.get_available_tickets.return_value = []

        worker = ChallengerWorker(
            worker_id="challenger-1",
            config=mock_config,
            beads_client=mock_beads_client,
            poll_interval=0.1,
        )

        await worker.start()

        # Second start should just return (with warning logged)
        await worker.start()

        # Should still be running
        assert worker._task is not None

        await worker.stop()

    @pytest.mark.asyncio
    async def test_stop_sets_state_to_stopped(self, mock_config, mock_beads_client):
        """Test that stop() sets state to STOPPED."""
        mock_beads_client.get_available_tickets.return_value = []

        worker = ChallengerWorker(
            worker_id="challenger-1",
            config=mock_config,
            beads_client=mock_beads_client,
            poll_interval=0.1,
        )

        await worker.start()
        await asyncio.sleep(0.05)  # Let worker start
        await worker.stop()

        assert worker.status.state == WorkerState.STOPPED

    @pytest.mark.asyncio
    async def test_run_loop_stops_after_empty_polls(self, mock_config, mock_beads_client):
        """Test that worker stops after consecutive empty polls."""
        mock_beads_client.get_available_tickets.return_value = []

        worker = ChallengerWorker(
            worker_id="challenger-1",
            config=mock_config,
            beads_client=mock_beads_client,
            poll_interval=0.01,  # Fast polling for test
        )

        await worker.start()

        # Wait for worker to stop (should stop after max_empty_polls)
        await asyncio.wait_for(worker._task, timeout=5.0)

        assert worker.status.state == WorkerState.IDLE

    @pytest.mark.asyncio
    async def test_poll_for_ticket_claims_ticket(
        self, mock_config, mock_beads_client, sample_validation_ticket
    ):
        """Test that _poll_for_ticket claims available ticket."""
        mock_beads_client.get_available_tickets.return_value = [sample_validation_ticket]
        mock_beads_client.claim_ticket.return_value = True

        worker = ChallengerWorker(
            worker_id="challenger-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        ticket = await worker._poll_for_ticket()

        assert ticket is not None
        assert ticket.ticket_id == sample_validation_ticket.ticket_id
        mock_beads_client.claim_ticket.assert_called_once_with(
            sample_validation_ticket.ticket_id,
            "challenger-1",
        )
        mock_beads_client.update_ticket_state.assert_called_once_with(
            sample_validation_ticket.ticket_id,
            TicketState.IN_PROGRESS,
        )

    @pytest.mark.asyncio
    async def test_poll_for_ticket_returns_none_when_empty(
        self, mock_config, mock_beads_client
    ):
        """Test that _poll_for_ticket returns None when no tickets available."""
        mock_beads_client.get_available_tickets.return_value = []

        worker = ChallengerWorker(
            worker_id="challenger-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        ticket = await worker._poll_for_ticket()

        assert ticket is None

    @pytest.mark.asyncio
    async def test_poll_for_ticket_tries_next_on_claim_failure(
        self, mock_config, mock_beads_client
    ):
        """Test that _poll_for_ticket tries next ticket if claim fails."""
        ticket1 = ProgramManagerTicket(
            ticket_id="war_rig-001",
            ticket_type=TicketType.VALIDATION,
            state=TicketState.CREATED,
            file_name="PROG1.cbl",
            program_id="PROG1",
            cycle_number=1,
        )
        ticket2 = ProgramManagerTicket(
            ticket_id="war_rig-002",
            ticket_type=TicketType.VALIDATION,
            state=TicketState.CREATED,
            file_name="PROG2.cbl",
            program_id="PROG2",
            cycle_number=1,
        )

        mock_beads_client.get_available_tickets.return_value = [ticket1, ticket2]
        # First claim fails, second succeeds
        mock_beads_client.claim_ticket.side_effect = [False, True]

        worker = ChallengerWorker(
            worker_id="challenger-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        ticket = await worker._poll_for_ticket()

        assert ticket is not None
        assert ticket.ticket_id == "war_rig-002"
        assert mock_beads_client.claim_ticket.call_count == 2

    def test_load_documentation_state_valid(
        self, mock_config, mock_beads_client, sample_validation_ticket
    ):
        """Test _load_documentation_state loads valid state from metadata."""
        worker = ChallengerWorker(
            worker_id="challenger-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        state = worker._load_documentation_state(sample_validation_ticket)

        assert state is not None
        assert "template" in state
        assert "source_code" in state
        assert state["file_name"] == "TESTPROG.cbl"

    def test_load_documentation_state_missing_template(
        self, mock_config, mock_beads_client
    ):
        """Test _load_documentation_state returns None when template missing."""
        ticket = ProgramManagerTicket(
            ticket_id="war_rig-001",
            ticket_type=TicketType.VALIDATION,
            state=TicketState.CREATED,
            file_name="TEST.cbl",
            metadata={
                "source_code": "some code",
                # No template
            },
        )

        worker = ChallengerWorker(
            worker_id="challenger-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        state = worker._load_documentation_state(ticket)

        assert state is None

    def test_load_documentation_state_missing_source_code(
        self, mock_config, mock_beads_client, sample_template
    ):
        """Test _load_documentation_state returns None when source code missing."""
        ticket = ProgramManagerTicket(
            ticket_id="war_rig-001",
            ticket_type=TicketType.VALIDATION,
            state=TicketState.CREATED,
            file_name="TEST.cbl",
            metadata={
                "template": sample_template.model_dump(),
                # No source_code
            },
        )

        worker = ChallengerWorker(
            worker_id="challenger-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        state = worker._load_documentation_state(ticket)

        assert state is None

    def test_load_documentation_state_no_metadata(
        self, mock_config, mock_beads_client
    ):
        """Test _load_documentation_state returns None when no metadata."""
        ticket = ProgramManagerTicket(
            ticket_id="war_rig-001",
            ticket_type=TicketType.VALIDATION,
            state=TicketState.CREATED,
            file_name="TEST.cbl",
            metadata=None,
        )

        worker = ChallengerWorker(
            worker_id="challenger-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        state = worker._load_documentation_state(ticket)

        assert state is None

    def test_create_rework_ticket(
        self, mock_config, mock_beads_client, sample_validation_ticket
    ):
        """Test _create_rework_ticket creates CLARIFICATION ticket."""
        result = ValidationResult(
            success=True,
            is_valid=False,
            issues_found=["Missing error handling"],
            blocking_questions=[
                {
                    "question_id": "Q-001",
                    "section": "error_handling",
                    "question_type": "COMPLETENESS",
                    "question": "What error handling exists?",
                    "severity": "BLOCKING",
                    "evidence": [10, 11],
                }
            ],
        )

        worker = ChallengerWorker(
            worker_id="challenger-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        rework_ticket = worker._create_rework_ticket(sample_validation_ticket, result)

        assert rework_ticket is not None
        mock_beads_client.create_pm_ticket.assert_called_once()
        call_kwargs = mock_beads_client.create_pm_ticket.call_args[1]
        assert call_kwargs["ticket_type"] == TicketType.CLARIFICATION
        # Cycle number stays same - only orchestrator increments cycles
        assert call_kwargs["cycle_number"] == 1
        assert call_kwargs["parent_ticket_id"] == sample_validation_ticket.ticket_id

    def test_create_rework_ticket_priority_based_on_issues(
        self, mock_config, mock_beads_client, sample_validation_ticket
    ):
        """Test _create_rework_ticket sets priority based on issue count."""
        # 3+ issues = HIGH priority
        result_many = ValidationResult(
            success=True,
            is_valid=False,
            blocking_questions=[{"severity": "BLOCKING"}] * 3,
        )

        worker = ChallengerWorker(
            worker_id="challenger-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        worker._create_rework_ticket(sample_validation_ticket, result_many)

        call_kwargs = mock_beads_client.create_pm_ticket.call_args[1]
        assert call_kwargs["priority"] == BeadsPriority.HIGH

        mock_beads_client.reset_mock()

        # 1-2 issues = MEDIUM priority
        result_few = ValidationResult(
            success=True,
            is_valid=False,
            blocking_questions=[{"severity": "BLOCKING"}],
        )

        worker._create_rework_ticket(sample_validation_ticket, result_few)

        call_kwargs = mock_beads_client.create_pm_ticket.call_args[1]
        assert call_kwargs["priority"] == BeadsPriority.MEDIUM

    @pytest.mark.asyncio
    async def test_process_ticket_valid_documentation(
        self, mock_config, mock_beads_client, sample_validation_ticket
    ):
        """Test _process_ticket marks ticket completed for valid docs."""
        worker = ChallengerWorker(
            worker_id="challenger-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        # Mock the challenger agent to return valid output
        mock_challenger_output = ChallengerOutput(
            success=True,
            questions=[],  # No blocking questions
            issues_found=[],
        )
        worker.challenger = MagicMock()
        worker.challenger.ainvoke = AsyncMock(return_value=mock_challenger_output)

        await worker._process_ticket(sample_validation_ticket)

        # Should update to COMPLETED
        mock_beads_client.update_ticket_state.assert_called_with(
            sample_validation_ticket.ticket_id,
            TicketState.COMPLETED,
            reason="Documentation validated successfully",
        )
        assert worker.status.tickets_validated == 1

    @pytest.mark.asyncio
    async def test_process_ticket_invalid_documentation(
        self, mock_config, mock_beads_client, sample_validation_ticket
    ):
        """Test _process_ticket creates rework ticket for invalid docs."""
        worker = ChallengerWorker(
            worker_id="challenger-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        # Create a real ChallengerQuestion with BLOCKING severity
        blocking_question = ChallengerQuestion(
            question_id="Q-001",
            section="purpose",
            question_type=QuestionType.CLARIFICATION,
            question="What is the business context?",
            evidence=[1, 2],
            severity=QuestionSeverity.BLOCKING,
            iteration=1,
        )

        mock_challenger_output = ChallengerOutput(
            success=True,
            questions=[blocking_question],
            issues_found=["Purpose is vague"],
        )
        worker.challenger = MagicMock()
        worker.challenger.ainvoke = AsyncMock(return_value=mock_challenger_output)

        await worker._process_ticket(sample_validation_ticket)

        # Should have created rework ticket
        mock_beads_client.create_pm_ticket.assert_called_once()
        assert worker.status.tickets_rejected == 1

    @pytest.mark.asyncio
    async def test_process_ticket_blocks_on_state_load_failure(
        self, mock_config, mock_beads_client
    ):
        """Test _process_ticket blocks ticket when state cannot be loaded."""
        ticket = ProgramManagerTicket(
            ticket_id="war_rig-001",
            ticket_type=TicketType.VALIDATION,
            state=TicketState.CREATED,
            file_name="TEST.cbl",
            metadata={},  # Invalid metadata
        )

        worker = ChallengerWorker(
            worker_id="challenger-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        await worker._process_ticket(ticket)

        # Should update to BLOCKED
        mock_beads_client.update_ticket_state.assert_called_with(
            ticket.ticket_id,
            TicketState.BLOCKED,
            reason="Could not load documentation state from parent ticket",
        )


# =============================================================================
# ChallengerWorkerPool Tests
# =============================================================================


class TestChallengerWorkerPool:
    """Tests for ChallengerWorkerPool class."""

    def test_create_pool(self, mock_config, mock_beads_client):
        """Test pool creation with correct number of workers."""
        pool = ChallengerWorkerPool(
            num_workers=3,
            config=mock_config,
            beads_client=mock_beads_client,
        )

        assert pool.num_workers == 3
        assert len(pool.workers) == 3
        assert pool.is_running is False

    def test_create_pool_generates_worker_ids(self, mock_config, mock_beads_client):
        """Test that pool generates sequential worker IDs."""
        pool = ChallengerWorkerPool(
            num_workers=3,
            config=mock_config,
            beads_client=mock_beads_client,
        )

        worker_ids = [w.worker_id for w in pool.workers]
        assert worker_ids == ["challenger-1", "challenger-2", "challenger-3"]

    def test_create_pool_with_custom_poll_interval(
        self, mock_config, mock_beads_client
    ):
        """Test pool creation with custom poll interval."""
        pool = ChallengerWorkerPool(
            num_workers=2,
            config=mock_config,
            beads_client=mock_beads_client,
            poll_interval=5.0,
        )

        for worker in pool.workers:
            assert worker.poll_interval == 5.0

    @pytest.mark.asyncio
    async def test_start_starts_all_workers(self, mock_config, mock_beads_client):
        """Test that start() starts all workers."""
        mock_beads_client.get_available_tickets.return_value = []

        pool = ChallengerWorkerPool(
            num_workers=2,
            config=mock_config,
            beads_client=mock_beads_client,
            poll_interval=0.1,
        )

        await pool.start()

        assert pool.is_running is True

        # Give workers time to start
        await asyncio.sleep(0.05)

        # All workers should have tasks
        for worker in pool.workers:
            assert worker._task is not None

        await pool.stop()

    @pytest.mark.asyncio
    async def test_start_warns_if_already_running(self, mock_config, mock_beads_client):
        """Test that start() warns if pool is already running."""
        mock_beads_client.get_available_tickets.return_value = []

        pool = ChallengerWorkerPool(
            num_workers=1,
            config=mock_config,
            beads_client=mock_beads_client,
            poll_interval=0.1,
        )

        await pool.start()

        # Second start should just return (with warning)
        await pool.start()

        assert pool.is_running is True

        await pool.stop()

    @pytest.mark.asyncio
    async def test_stop_stops_all_workers(self, mock_config, mock_beads_client):
        """Test that stop() stops all workers."""
        mock_beads_client.get_available_tickets.return_value = []

        pool = ChallengerWorkerPool(
            num_workers=2,
            config=mock_config,
            beads_client=mock_beads_client,
            poll_interval=0.1,
        )

        await pool.start()
        await asyncio.sleep(0.05)
        await pool.stop()

        assert pool.is_running is False
        for worker in pool.workers:
            assert worker.status.state == WorkerState.STOPPED

    @pytest.mark.asyncio
    async def test_stop_does_nothing_if_not_running(
        self, mock_config, mock_beads_client
    ):
        """Test that stop() does nothing if pool not running."""
        pool = ChallengerWorkerPool(
            num_workers=2,
            config=mock_config,
            beads_client=mock_beads_client,
        )

        # Should not raise
        await pool.stop()

        assert pool.is_running is False

    @pytest.mark.asyncio
    async def test_wait_for_completion(self, mock_config, mock_beads_client):
        """Test wait_for_completion waits for all workers."""
        mock_beads_client.get_available_tickets.return_value = []

        pool = ChallengerWorkerPool(
            num_workers=2,
            config=mock_config,
            beads_client=mock_beads_client,
            poll_interval=0.01,  # Fast for test
        )

        await pool.start()
        await pool.wait_for_completion()

        # Should have finished
        assert pool.is_running is False

    def test_get_status_not_running(self, mock_config, mock_beads_client):
        """Test get_status when pool is not running."""
        pool = ChallengerWorkerPool(
            num_workers=2,
            config=mock_config,
            beads_client=mock_beads_client,
        )

        status = pool.get_status()

        assert status["running"] is False
        assert status["num_workers"] == 2
        assert status["active_workers"] == 0
        assert status["total_tickets_processed"] == 0
        assert len(status["workers"]) == 2

    @pytest.mark.asyncio
    async def test_get_status_running(self, mock_config, mock_beads_client):
        """Test get_status when pool is running."""
        mock_beads_client.get_available_tickets.return_value = []

        pool = ChallengerWorkerPool(
            num_workers=2,
            config=mock_config,
            beads_client=mock_beads_client,
            poll_interval=0.1,
        )

        await pool.start()
        await asyncio.sleep(0.05)

        status = pool.get_status()

        assert status["running"] is True
        assert status["num_workers"] == 2

        await pool.stop()

    def test_get_status_aggregates_statistics(self, mock_config, mock_beads_client):
        """Test get_status aggregates worker statistics."""
        pool = ChallengerWorkerPool(
            num_workers=2,
            config=mock_config,
            beads_client=mock_beads_client,
        )

        # Manually set worker statistics
        pool.workers[0].status.tickets_processed = 5
        pool.workers[0].status.tickets_validated = 3
        pool.workers[0].status.tickets_rejected = 2
        pool.workers[0].status.error_count = 1

        pool.workers[1].status.tickets_processed = 3
        pool.workers[1].status.tickets_validated = 2
        pool.workers[1].status.tickets_rejected = 1
        pool.workers[1].status.error_count = 0

        status = pool.get_status()

        assert status["total_tickets_processed"] == 8
        assert status["total_tickets_validated"] == 5
        assert status["total_tickets_rejected"] == 3
        assert status["total_errors"] == 1

    def test_is_running_property(self, mock_config, mock_beads_client):
        """Test is_running property."""
        pool = ChallengerWorkerPool(
            num_workers=2,
            config=mock_config,
            beads_client=mock_beads_client,
        )

        assert pool.is_running is False

        pool._running = True
        assert pool.is_running is True


# =============================================================================
# Integration-style Tests
# =============================================================================


class TestChallengerPoolIntegration:
    """Integration-style tests for ChallengerWorkerPool."""

    @pytest.mark.asyncio
    async def test_pool_processes_tickets(
        self, mock_config, mock_beads_client, sample_validation_ticket
    ):
        """Test that pool processes available tickets."""
        # Return ticket on first call, then empty
        call_count = [0]

        def get_tickets(*args, **kwargs):
            call_count[0] += 1
            if call_count[0] == 1:
                return [sample_validation_ticket]
            return []

        mock_beads_client.get_available_tickets.side_effect = get_tickets

        pool = ChallengerWorkerPool(
            num_workers=1,
            config=mock_config,
            beads_client=mock_beads_client,
            poll_interval=0.01,
        )

        # Mock the challenger agent for all workers
        mock_output = ChallengerOutput(success=True, questions=[], issues_found=[])
        for worker in pool.workers:
            worker.challenger = MagicMock()
            worker.challenger.ainvoke = AsyncMock(return_value=mock_output)

        await pool.start()
        await pool.wait_for_completion()

        # Should have processed the ticket
        status = pool.get_status()
        assert status["total_tickets_processed"] >= 1


# =============================================================================
# Lock-Skipped Ticket Tests (Infinite Loop Prevention)
# =============================================================================


class TestLockSkippedTickets:
    """Tests for lock-skipped ticket tracking to prevent infinite loops.

    When a worker claims a ticket but can't acquire the file lock (because
    another worker has it), it releases the ticket back to CREATED state.
    Without tracking, the worker would immediately re-claim the same ticket,
    creating an infinite loop. These tests verify the fix that tracks
    recently-released tickets and skips them for a configurable duration.
    """

    def test_lock_skipped_tracking_initialized(self, mock_config, mock_beads_client):
        """Test that lock-skipped tracking structures are initialized."""
        worker = ChallengerWorker(
            worker_id="challenger-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        assert hasattr(worker, "_lock_skipped_tickets")
        assert hasattr(worker, "_lock_skip_duration")
        assert isinstance(worker._lock_skipped_tickets, dict)
        assert worker._lock_skip_duration > 0

    @pytest.mark.asyncio
    async def test_lock_skip_expires_after_duration(
        self, mock_config, mock_beads_client, sample_validation_ticket
    ):
        """Test that lock-skipped tickets become claimable again after skip duration expires."""
        worker = ChallengerWorker(
            worker_id="challenger-1",
            config=mock_config,
            beads_client=mock_beads_client,
            poll_interval=0.05,
        )

        # Set a very short skip duration for testing
        worker._lock_skip_duration = 0.1

        # Manually add ticket to lock-skipped set
        worker._lock_skipped_tickets[sample_validation_ticket.ticket_id] = datetime.utcnow()

        # Ticket should be skipped initially
        mock_beads_client.get_available_tickets.return_value = [sample_validation_ticket]
        mock_beads_client.claim_ticket.return_value = True

        ticket = await worker._poll_for_ticket()
        assert ticket is None, "Ticket should be skipped immediately after lock failure"

        # Wait for skip duration to expire
        await asyncio.sleep(0.15)

        # Now ticket should be claimable again
        ticket = await worker._poll_for_ticket()
        assert ticket is not None, "Ticket should be claimable after skip duration expires"
        assert ticket.ticket_id == sample_validation_ticket.ticket_id

    @pytest.mark.asyncio
    async def test_multiple_tickets_only_locked_one_skipped(
        self, mock_config, mock_beads_client
    ):
        """Test that only the lock-failed ticket is skipped, not other available tickets."""
        # Create two tickets
        ticket1 = ProgramManagerTicket(
            ticket_id="ticket-1",
            ticket_type=TicketType.VALIDATION,
            state=TicketState.CREATED,
            file_name="FILE1.cbl",
            program_id="FILE1",
            cycle_number=1,
        )
        ticket2 = ProgramManagerTicket(
            ticket_id="ticket-2",
            ticket_type=TicketType.VALIDATION,
            state=TicketState.CREATED,
            file_name="FILE2.cbl",
            program_id="FILE2",
            cycle_number=1,
        )

        worker = ChallengerWorker(
            worker_id="challenger-1",
            config=mock_config,
            beads_client=mock_beads_client,
            poll_interval=0.05,
        )

        # Manually mark ticket1 as lock-skipped
        worker._lock_skipped_tickets["ticket-1"] = datetime.utcnow()

        # Both tickets available
        mock_beads_client.get_available_tickets.return_value = [ticket1, ticket2]
        mock_beads_client.claim_ticket.return_value = True

        # Should skip ticket1 but claim ticket2
        claimed = await worker._poll_for_ticket()

        assert claimed is not None
        assert claimed.ticket_id == "ticket-2", (
            "Should claim ticket-2 since ticket-1 is lock-skipped"
        )

    @pytest.mark.asyncio
    async def test_lock_failure_adds_to_skip_set(
        self, mock_config, mock_beads_client, sample_validation_ticket
    ):
        """Test that failing to acquire a lock adds the ticket to the skip set."""
        from war_rig.utils.file_lock import FileLockManager

        lock_manager = FileLockManager()

        # Pre-lock the file (new naming convention includes source extension)
        output_file = str(mock_config.output_directory / "TESTPROG.cbl.doc.json")
        await lock_manager.acquire(output_file, "other-worker")

        worker = ChallengerWorker(
            worker_id="challenger-1",
            config=mock_config,
            beads_client=mock_beads_client,
            file_lock_manager=lock_manager,
        )

        # Verify ticket not in skip set initially
        assert sample_validation_ticket.ticket_id not in worker._lock_skipped_tickets

        # Process the ticket (will fail to acquire lock)
        await worker._process_ticket(sample_validation_ticket)

        # Verify ticket was added to skip set
        assert sample_validation_ticket.ticket_id in worker._lock_skipped_tickets

        # Cleanup
        await lock_manager.release(output_file, "other-worker")


# =============================================================================
# Structural Pre-check Tests: _get_citadel_context
# =============================================================================


class TestGetCitadelContext:
    """Tests for ChallengerWorker._get_citadel_context method."""

    def test_successful_context_loading(self, mock_config, mock_beads_client):
        """Test successful Citadel analysis returns dict with paragraphs, includes, dead_code."""
        worker = ChallengerWorker(
            worker_id="challenger-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        # Set up mock Citadel
        mock_citadel = MagicMock()
        worker._citadel = mock_citadel

        # Create mock analysis result
        mock_callout = MagicMock()
        mock_callout.target = "SUB-PARA"
        mock_callout.relationship = "PERFORMS"
        mock_callout.line = 100

        mock_artifact = MagicMock()
        mock_artifact.name = "MAIN-PARA"
        mock_artifact.line_start = 50
        mock_artifact.line_end = 120
        mock_artifact.callouts = [mock_callout]

        mock_result = MagicMock()
        mock_result.error = None
        mock_result.artifacts = [mock_artifact]
        mock_result.preprocessor_includes = ["COPYLIB.cpy"]

        mock_citadel.analyze_file.return_value = mock_result

        context = worker._get_citadel_context("/path/to/TEST.cbl")

        assert context is not None
        assert "paragraphs" in context
        assert "includes" in context
        assert "dead_code" in context

        assert len(context["paragraphs"]) == 1
        para = context["paragraphs"][0]
        assert para["name"] == "MAIN-PARA"
        assert para["line_start"] == 50
        assert para["line_end"] == 120
        assert len(para["calls"]) == 1
        assert para["calls"][0]["target"] == "SUB-PARA"
        assert para["calls"][0]["type"] == "PERFORMS"
        assert para["calls"][0]["line"] == 100

        assert context["includes"] == ["COPYLIB.cpy"]
        mock_citadel.analyze_file.assert_called_once_with("/path/to/TEST.cbl")

    def test_returns_none_when_citadel_is_none(self, mock_config, mock_beads_client):
        """Test returns None when self._citadel is None."""
        worker = ChallengerWorker(
            worker_id="challenger-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )
        # _citadel defaults to None without dependency_graph_path
        assert worker._citadel is None

        result = worker._get_citadel_context("/path/to/TEST.cbl")
        assert result is None

    def test_returns_none_on_citadel_exception(self, mock_config, mock_beads_client):
        """Test returns None when Citadel analyze_file raises an exception."""
        worker = ChallengerWorker(
            worker_id="challenger-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        mock_citadel = MagicMock()
        mock_citadel.analyze_file.side_effect = RuntimeError("Citadel analysis crashed")
        worker._citadel = mock_citadel

        result = worker._get_citadel_context("/path/to/TEST.cbl")
        assert result is None


# =============================================================================
# Structural Pre-check Tests: _find_perform_thru_targets
# =============================================================================


class TestFindPerformThruTargets:
    """Tests for ChallengerWorker._find_perform_thru_targets method."""

    def test_finds_thru_targets(self, mock_config, mock_beads_client, tmp_path):
        """Test finding PERFORM THRU target paragraphs from source file."""
        source_file = tmp_path / "PROG.cbl"
        source_file.write_text(
            "       IDENTIFICATION DIVISION.\n"
            "       PROGRAM-ID. PROG.\n"
            "       PROCEDURE DIVISION.\n"
            "           PERFORM 1000-START THRU 1000-EXIT.\n"
            "           PERFORM 2000-PROCESS THRU 2000-END.\n"
            "           STOP RUN.\n",
            encoding="utf-8",
        )

        worker = ChallengerWorker(
            worker_id="challenger-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        targets = worker._find_perform_thru_targets(str(source_file))

        assert "1000-EXIT" in targets
        assert "2000-END" in targets
        assert len(targets) == 2

    def test_finds_through_targets(self, mock_config, mock_beads_client, tmp_path):
        """Test finding PERFORM THROUGH (synonym for THRU) target paragraphs."""
        source_file = tmp_path / "PROG.cbl"
        source_file.write_text(
            "       PROCEDURE DIVISION.\n"
            "           PERFORM INIT-PARA THROUGH INIT-EXIT.\n"
            "           STOP RUN.\n",
            encoding="utf-8",
        )

        worker = ChallengerWorker(
            worker_id="challenger-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        targets = worker._find_perform_thru_targets(str(source_file))

        assert "INIT-EXIT" in targets
        assert len(targets) == 1

    def test_returns_empty_set_for_no_perform_thru(
        self, mock_config, mock_beads_client, tmp_path
    ):
        """Test returns empty set when file has no PERFORM THRU."""
        source_file = tmp_path / "SIMPLE.cbl"
        source_file.write_text(
            "       PROCEDURE DIVISION.\n"
            "           PERFORM SOME-PARA.\n"
            "           STOP RUN.\n",
            encoding="utf-8",
        )

        worker = ChallengerWorker(
            worker_id="challenger-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        targets = worker._find_perform_thru_targets(str(source_file))

        assert targets == set()

    def test_returns_empty_set_for_nonexistent_file(
        self, mock_config, mock_beads_client
    ):
        """Test returns empty set for a file that does not exist."""
        worker = ChallengerWorker(
            worker_id="challenger-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        targets = worker._find_perform_thru_targets("/nonexistent/path/GHOST.cbl")

        assert targets == set()


# =============================================================================
# Structural Pre-check Tests: _run_structural_precheck
# =============================================================================


class TestRunStructuralPrecheck:
    """Tests for ChallengerWorker._run_structural_precheck method."""

    def _make_worker(self, mock_config, mock_beads_client):
        """Helper to create a ChallengerWorker for structural pre-check tests."""
        return ChallengerWorker(
            worker_id="challenger-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

    def _make_template(self, para_names=None, copybook_names=None, **kwargs):
        """Helper to create a DocumentationTemplate with configurable paragraphs.

        Args:
            para_names: List of paragraph names to include. Each is created as a
                Paragraph with the given name and default calls/outgoing_calls.
            copybook_names: List of copybook names to include.
            **kwargs: Extra fields passed to DocumentationTemplate.
        """
        from war_rig.models.templates import (
            CopybookReference,
            HeaderSection,
            InputOutput,
            BusinessRule,
            Paragraph,
            PurposeSection,
            ProgramType,
            FileType,
        )

        paragraphs = []
        if para_names:
            for name in para_names:
                paragraphs.append(Paragraph(paragraph_name=name))

        copybooks = []
        if copybook_names:
            for cn in copybook_names:
                copybooks.append(CopybookReference(copybook_name=cn))

        defaults = dict(
            header=HeaderSection(
                program_id="TEST",
                file_name="TEST.cbl",
                file_type=FileType.COBOL,
            ),
            purpose=PurposeSection(
                summary="Test program for structural pre-check.",
                business_context="Testing",
                program_type=ProgramType.BATCH,
            ),
            inputs=[InputOutput(name="INPUT-FILE")],
            outputs=[InputOutput(name="OUTPUT-FILE")],
            business_rules=[BusinessRule(rule_id="BR1", description="A rule")],
            paragraphs=paragraphs,
            copybooks_used=copybooks,
        )
        defaults.update(kwargs)
        return DocumentationTemplate(**defaults)

    def _make_citadel_context(
        self, para_names=None, includes=None, dead_code=None, calls_map=None,
    ):
        """Helper to create a Citadel context dict.

        Args:
            para_names: List of paragraph name strings.
            includes: List of included file name strings.
            dead_code: List of dead code dicts.
            calls_map: Dict mapping paragraph name to list of call target strings.
        """
        paragraphs = []
        if para_names:
            for name in para_names:
                calls = []
                if calls_map and name in calls_map:
                    for target in calls_map[name]:
                        calls.append({
                            "target": target,
                            "type": "PERFORMS",
                            "line": 100,
                        })
                paragraphs.append({
                    "name": name,
                    "line_start": 10,
                    "line_end": 50,
                    "calls": calls,
                })

        return {
            "paragraphs": paragraphs,
            "includes": includes or [],
            "dead_code": dead_code or [],
        }

    def test_missing_paragraphs_blocking(self, mock_config, mock_beads_client):
        """Test: Citadel has 5 paragraphs, template has 3 -> 2 BLOCKING issues."""
        worker = self._make_worker(mock_config, mock_beads_client)

        citadel_context = self._make_citadel_context(
            para_names=["PARA-A", "PARA-B", "PARA-C", "PARA-D", "PARA-E"]
        )
        template = self._make_template(
            para_names=["PARA-A", "PARA-B", "PARA-C"]
        )

        issues = worker._run_structural_precheck(
            citadel_context, template, "TEST.cbl"
        )

        # Should find 2 missing paragraphs (PARA-D and PARA-E)
        missing_para_issues = [
            q for q in issues
            if q.section == "paragraphs"
            and q.severity == QuestionSeverity.BLOCKING
            and "not documented" in q.question.lower()
        ]
        assert len(missing_para_issues) == 2

        # All missing paragraph issues should be BLOCKING
        for issue in missing_para_issues:
            assert issue.severity == QuestionSeverity.BLOCKING
            assert issue.question_type == QuestionType.COMPLETENESS

    def test_no_missing_paragraphs(self, mock_config, mock_beads_client):
        """Test: Same paragraphs in Citadel and template -> no missing paragraph issues."""
        worker = self._make_worker(mock_config, mock_beads_client)

        citadel_context = self._make_citadel_context(
            para_names=["PARA-A", "PARA-B", "PARA-C"]
        )
        template = self._make_template(
            para_names=["PARA-A", "PARA-B", "PARA-C"]
        )

        issues = worker._run_structural_precheck(
            citadel_context, template, "TEST.cbl"
        )

        # Should have no missing paragraph issues
        missing_para_issues = [
            q for q in issues
            if q.section == "paragraphs"
            and "not documented" in q.question.lower()
        ]
        assert len(missing_para_issues) == 0

    def test_dead_code_exclusion(self, mock_config, mock_beads_client):
        """Test: Missing paragraph in dead_code -> not flagged."""
        worker = self._make_worker(mock_config, mock_beads_client)

        citadel_context = self._make_citadel_context(
            para_names=["PARA-A", "PARA-B", "DEAD-PARA"],
            dead_code=[{"name": "DEAD-PARA", "type": "paragraph"}],
        )
        template = self._make_template(para_names=["PARA-A", "PARA-B"])

        issues = worker._run_structural_precheck(
            citadel_context, template, "TEST.cbl"
        )

        # DEAD-PARA should not be flagged
        missing_para_issues = [
            q for q in issues
            if q.section == "paragraphs"
            and "not documented" in q.question.lower()
        ]
        assert len(missing_para_issues) == 0

    def test_thru_target_exclusion(self, mock_config, mock_beads_client):
        """Test: Missing paragraph is a THRU target -> not flagged."""
        worker = self._make_worker(mock_config, mock_beads_client)

        citadel_context = self._make_citadel_context(
            para_names=["PARA-A", "PARA-B", "PARA-EXIT"]
        )
        template = self._make_template(para_names=["PARA-A", "PARA-B"])

        thru_targets = {"PARA-EXIT"}

        issues = worker._run_structural_precheck(
            citadel_context, template, "TEST.cbl", thru_targets=thru_targets
        )

        # PARA-EXIT should not be flagged because it is a THRU target
        missing_para_issues = [
            q for q in issues
            if q.section == "paragraphs"
            and "not documented" in q.question.lower()
        ]
        assert len(missing_para_issues) == 0

    def test_missing_calls_important(self, mock_config, mock_beads_client):
        """Test: Citadel says para X calls Y and Z, template para X only has Y -> 1 IMPORTANT."""
        from war_rig.models.templates import Paragraph

        worker = self._make_worker(mock_config, mock_beads_client)

        citadel_context = self._make_citadel_context(
            para_names=["MAIN-PARA"],
            calls_map={"MAIN-PARA": ["SUB-A", "SUB-B"]},
        )

        # Template has MAIN-PARA calling only SUB-A (missing SUB-B)
        template = self._make_template()
        template.paragraphs = [
            Paragraph(paragraph_name="MAIN-PARA", calls=["SUB-A"]),
        ]

        issues = worker._run_structural_precheck(
            citadel_context, template, "TEST.cbl"
        )

        # Should find 1 missing call (SUB-B)
        missing_call_issues = [
            q for q in issues
            if q.section == "paragraphs"
            and "calls" in q.question.lower()
            and "SUB-B" in q.question.upper()
        ]
        assert len(missing_call_issues) == 1
        assert missing_call_issues[0].severity == QuestionSeverity.IMPORTANT

    def test_missing_copybooks_important(self, mock_config, mock_beads_client):
        """Test: Citadel has 2 includes, template has 1 -> 1 IMPORTANT issue."""
        worker = self._make_worker(mock_config, mock_beads_client)

        citadel_context = self._make_citadel_context(
            para_names=["PARA-A"],
            includes=["COPY1.cpy", "COPY2.cpy"],
        )
        template = self._make_template(
            para_names=["PARA-A"],
            copybook_names=["COPY1.cpy"],
        )

        issues = worker._run_structural_precheck(
            citadel_context, template, "TEST.cbl"
        )

        missing_cb_issues = [
            q for q in issues
            if q.section == "copybooks_used"
            and q.severity == QuestionSeverity.IMPORTANT
        ]
        assert len(missing_cb_issues) == 1
        assert "COPY2.CPY" in missing_cb_issues[0].question.upper()

    def test_empty_critical_sections(self, mock_config, mock_beads_client):
        """Test: Empty purpose -> BLOCKING; empty inputs -> IMPORTANT."""
        worker = self._make_worker(mock_config, mock_beads_client)

        citadel_context = self._make_citadel_context(para_names=["PARA-A"])

        # Template with None purpose and empty inputs
        template = self._make_template(para_names=["PARA-A"])
        template.purpose = None  # type: ignore[assignment]
        template.inputs = []  # Empty inputs

        issues = worker._run_structural_precheck(
            citadel_context, template, "TEST.cbl"
        )

        # Purpose missing -> BLOCKING
        purpose_issues = [
            q for q in issues if q.section == "purpose"
        ]
        assert len(purpose_issues) == 1
        assert purpose_issues[0].severity == QuestionSeverity.BLOCKING

        # Empty inputs -> IMPORTANT
        input_issues = [
            q for q in issues if q.section == "inputs"
        ]
        assert len(input_issues) == 1
        assert input_issues[0].severity == QuestionSeverity.IMPORTANT

    def test_empty_purpose_summary(self, mock_config, mock_beads_client):
        """Test: purpose exists but summary is empty -> BLOCKING."""
        from war_rig.models.templates import PurposeSection

        worker = self._make_worker(mock_config, mock_beads_client)

        citadel_context = self._make_citadel_context(para_names=["PARA-A"])
        template = self._make_template(para_names=["PARA-A"])
        template.purpose = PurposeSection(summary="", business_context="test")

        issues = worker._run_structural_precheck(
            citadel_context, template, "TEST.cbl"
        )

        purpose_issues = [q for q in issues if q.section == "purpose"]
        assert len(purpose_issues) == 1
        assert purpose_issues[0].severity == QuestionSeverity.BLOCKING

    def test_case_insensitive_matching(self, mock_config, mock_beads_client):
        """Test: Citadel 'MAIN-PROCESS', template 'main-process' -> no false positive."""
        worker = self._make_worker(mock_config, mock_beads_client)

        citadel_context = self._make_citadel_context(
            para_names=["MAIN-PROCESS", "SUB-ROUTINE"]
        )
        template = self._make_template(
            para_names=["main-process", "sub-routine"]
        )

        issues = worker._run_structural_precheck(
            citadel_context, template, "TEST.cbl"
        )

        # Should have no missing paragraph issues (case-insensitive match)
        missing_para_issues = [
            q for q in issues
            if q.section == "paragraphs"
            and "not documented" in q.question.lower()
        ]
        assert len(missing_para_issues) == 0

    def test_issues_sorted_blocking_first(self, mock_config, mock_beads_client):
        """Test: Results are sorted with BLOCKING before IMPORTANT."""
        worker = self._make_worker(mock_config, mock_beads_client)

        # Create a scenario with both BLOCKING and IMPORTANT issues
        citadel_context = self._make_citadel_context(
            para_names=["PARA-A", "PARA-MISSING"],
            includes=["MISSING-COPY.cpy"],
        )
        template = self._make_template(para_names=["PARA-A"])

        issues = worker._run_structural_precheck(
            citadel_context, template, "TEST.cbl"
        )

        # Should have at least one BLOCKING (missing para) and one IMPORTANT (missing copy)
        blocking_indices = [
            i for i, q in enumerate(issues)
            if q.severity == QuestionSeverity.BLOCKING
        ]
        important_indices = [
            i for i, q in enumerate(issues)
            if q.severity == QuestionSeverity.IMPORTANT
        ]

        assert len(blocking_indices) > 0
        assert len(important_indices) > 0

        # All BLOCKING should come before all IMPORTANT
        if blocking_indices and important_indices:
            assert max(blocking_indices) < min(important_indices)


# =============================================================================
# Structural Pre-check Integration Tests
# =============================================================================


class TestStructuralPrecheckIntegration:
    """Integration tests for structural pre-check in the validation pipeline."""

    @pytest.mark.asyncio
    async def test_structural_issues_prepended_to_blocking_questions(
        self, mock_config, mock_beads_client, sample_validation_ticket
    ):
        """Test that structural issues come first in blocking_questions list."""
        worker = ChallengerWorker(
            worker_id="challenger-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        # Create structural issues
        structural_issue = ChallengerQuestion(
            question_id="STRUCT-001",
            section="paragraphs",
            question_type=QuestionType.COMPLETENESS,
            question="Paragraph 'MISSING-PARA' exists in source but not in template.",
            severity=QuestionSeverity.BLOCKING,
        )

        # Create LLM blocking question
        llm_question = ChallengerQuestion(
            question_id="LLM-001",
            section="purpose",
            question_type=QuestionType.CLARIFICATION,
            question="What is the business context?",
            severity=QuestionSeverity.BLOCKING,
        )

        mock_challenger_output = ChallengerOutput(
            success=True,
            questions=[llm_question],
            issues_found=[],
        )
        worker.challenger = MagicMock()
        worker.challenger.ainvoke = AsyncMock(return_value=mock_challenger_output)

        state = worker._load_documentation_state(sample_validation_ticket)
        assert state is not None

        result = await worker._validate_documentation(
            state, sample_validation_ticket,
            structural_issues=[structural_issue],
        )

        assert result.success is True
        assert result.is_valid is False
        assert len(result.blocking_questions) >= 2

        # Structural issue should be first
        first_q = result.blocking_questions[0]
        assert first_q["question_id"] == "STRUCT-001"
        assert result.structural_issues_count == 1

    @pytest.mark.asyncio
    async def test_llm_skipped_on_3_or_more_structural_blockers(
        self, mock_config, mock_beads_client, sample_validation_ticket
    ):
        """Test: Pre-check returns 3+ BLOCKING -> ChallengerAgent.ainvoke NOT called."""
        worker = ChallengerWorker(
            worker_id="challenger-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        # Create 3 BLOCKING structural issues
        structural_issues = [
            ChallengerQuestion(
                question_id=f"STRUCT-{i:03d}",
                section="paragraphs",
                question_type=QuestionType.COMPLETENESS,
                question=f"Paragraph 'MISSING-{i}' not documented.",
                severity=QuestionSeverity.BLOCKING,
            )
            for i in range(3)
        ]

        # Mock the challenger agent - should NOT be called
        worker.challenger = MagicMock()
        worker.challenger.ainvoke = AsyncMock()

        state = worker._load_documentation_state(sample_validation_ticket)
        assert state is not None

        result = await worker._validate_documentation(
            state, sample_validation_ticket,
            structural_issues=structural_issues,
        )

        # LLM should NOT have been invoked
        worker.challenger.ainvoke.assert_not_called()

        # Result should be valid success=True (pre-check ran) but is_valid=False
        assert result.success is True
        assert result.is_valid is False

        # Only BLOCKING structural issues should appear in result
        assert len(result.blocking_questions) == 3
        for q in result.blocking_questions:
            assert q["severity"] == QuestionSeverity.BLOCKING.value

        assert result.structural_issues_count == 3

    @pytest.mark.asyncio
    async def test_no_citadel_runs_llm_only(
        self, mock_config, mock_beads_client, sample_validation_ticket
    ):
        """Test: self._citadel is None -> validation runs LLM-only (backwards compatible)."""
        worker = ChallengerWorker(
            worker_id="challenger-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        # Ensure no Citadel
        assert worker._citadel is None

        mock_challenger_output = ChallengerOutput(
            success=True,
            questions=[],
            issues_found=[],
        )
        worker.challenger = MagicMock()
        worker.challenger.ainvoke = AsyncMock(return_value=mock_challenger_output)

        await worker._process_ticket(sample_validation_ticket)

        # LLM should have been called
        worker.challenger.ainvoke.assert_called_once()

        # Ticket should be validated
        assert worker.status.tickets_validated == 1

    @pytest.mark.asyncio
    async def test_structural_issues_count_in_result(
        self, mock_config, mock_beads_client, sample_validation_ticket
    ):
        """Test that structural_issues_count is tracked in ValidationResult."""
        worker = ChallengerWorker(
            worker_id="challenger-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        # One IMPORTANT (non-blocking) structural issue
        structural_issue = ChallengerQuestion(
            question_id="STRUCT-001",
            section="copybooks_used",
            question_type=QuestionType.COMPLETENESS,
            question="Missing copybook",
            severity=QuestionSeverity.IMPORTANT,
        )

        mock_challenger_output = ChallengerOutput(
            success=True,
            questions=[],
            issues_found=[],
        )
        worker.challenger = MagicMock()
        worker.challenger.ainvoke = AsyncMock(return_value=mock_challenger_output)

        state = worker._load_documentation_state(sample_validation_ticket)
        assert state is not None

        result = await worker._validate_documentation(
            state, sample_validation_ticket,
            structural_issues=[structural_issue],
        )

        # Structural count should be tracked
        assert result.structural_issues_count == 1
        # IMPORTANT issues are not prepended to blocking_questions
        # (only BLOCKING structural issues are)
        assert result.is_valid is True


# =============================================================================
# Challenger Citadel Wiring Tests (D5)
# =============================================================================


class TestChallengerCitadelWiring:
    """Test that ChallengerWorkerPool receives dependency_graph_path."""

    def test_pool_passes_dependency_graph_path(self, mock_config, mock_beads_client, tmp_path):
        """Verify pool passes dependency_graph_path to workers."""
        graph_path = tmp_path / "dep_graph.json"
        graph_path.write_text("{}")

        pool = ChallengerWorkerPool(
            num_workers=1,
            config=mock_config,
            beads_client=mock_beads_client,
            exit_on_error=False,
            dependency_graph_path=graph_path,
        )

        # Pool passes dependency_graph_path to workers
        assert len(pool.workers) == 1
        assert pool.workers[0]._dependency_graph_path == graph_path

    def test_worker_initializes_citadel_with_graph_path(self, mock_config, mock_beads_client, tmp_path):
        """Verify worker initializes Citadel when dependency graph provided."""
        graph_path = tmp_path / "dep_graph.json"
        graph_path.write_text("{}")

        worker = ChallengerWorker(
            worker_id="challenger-test",
            config=mock_config,
            beads_client=mock_beads_client,
            exit_on_error=False,
            dependency_graph_path=graph_path,
        )

        # Citadel should be initialized (if installed)
        # The _dependency_graph_path should be set regardless
        assert worker._dependency_graph_path == graph_path

    def test_worker_no_citadel_without_graph_path(self, mock_config, mock_beads_client):
        """Verify worker does not initialize Citadel without graph path."""
        worker = ChallengerWorker(
            worker_id="challenger-test",
            config=mock_config,
            beads_client=mock_beads_client,
            exit_on_error=False,
        )

        assert worker._dependency_graph_path is None
        assert worker._citadel is None

    def test_resolve_source_path_from_metadata(self, mock_config, mock_beads_client):
        """Verify _resolve_source_path uses metadata file_path when available."""
        worker = ChallengerWorker(
            worker_id="challenger-test",
            config=mock_config,
            beads_client=mock_beads_client,
            exit_on_error=False,
        )

        ticket = ProgramManagerTicket(
            ticket_id="war_rig-test001",
            ticket_type=TicketType.VALIDATION,
            state=TicketState.CREATED,
            file_name="TESTPROG.cbl",
            program_id="TESTPROG",
            cycle_number=1,
            metadata={"file_path": "/custom/path/TESTPROG.cbl"},
        )

        from pathlib import Path
        resolved = worker._resolve_source_path(ticket)
        assert resolved == Path("/custom/path/TESTPROG.cbl")

    def test_resolve_source_path_fallback(self, mock_config, mock_beads_client):
        """Verify _resolve_source_path falls back to input_directory."""
        worker = ChallengerWorker(
            worker_id="challenger-test",
            config=mock_config,
            beads_client=mock_beads_client,
            exit_on_error=False,
        )

        ticket = ProgramManagerTicket(
            ticket_id="war_rig-test001",
            ticket_type=TicketType.VALIDATION,
            state=TicketState.CREATED,
            file_name="TESTPROG.cbl",
            program_id="TESTPROG",
            cycle_number=1,
            metadata={},
        )

        resolved = worker._resolve_source_path(ticket)
        assert resolved == worker._input_directory / "TESTPROG.cbl"


# =============================================================================
# Challenger LLM Validation with Bodies Tests (D6)
# =============================================================================


class TestChallengerLLMValidationWithBodies:
    """Test that Challenger validation passes Citadel bodies to ChallengerInput."""

    @pytest.fixture
    def worker_with_citadel(self, mock_config, mock_beads_client, tmp_path):
        """Create a ChallengerWorker with a mocked Citadel instance."""
        graph_path = tmp_path / "dep_graph.json"
        graph_path.write_text("{}")

        worker = ChallengerWorker(
            worker_id="challenger-test",
            config=mock_config,
            beads_client=mock_beads_client,
            exit_on_error=False,
            dependency_graph_path=graph_path,
        )

        # Mock the Citadel SDK
        mock_citadel = MagicMock()
        mock_citadel.get_function_bodies.return_value = {
            "0000-MAIN": "       0000-MAIN.\n           DISPLAY 'HELLO'.\n           STOP RUN.",
        }
        worker._citadel = mock_citadel
        return worker

    async def test_bodies_passed_to_challenger_input(
        self, worker_with_citadel, sample_template, sample_validation_ticket
    ):
        """Test that function bodies are passed to ChallengerInput citadel_context."""
        worker = worker_with_citadel

        # Add a paragraph to the template so bodies can be fetched
        from war_rig.models.templates import Paragraph
        sample_template.paragraphs = [
            Paragraph(paragraph_name="0000-MAIN", purpose="Main entry point"),
        ]

        # Build validation state directly
        state = {
            "template": sample_template,
            "source_code": "       0000-MAIN.\n           STOP RUN.",
            "file_name": "TESTPROG.cbl",
            "file_type": FileType.COBOL,
            "iteration": 1,
        }

        # Mock challenger agent to capture the input
        captured_inputs = []

        async def capture_input(input_data):
            captured_inputs.append(input_data)
            return ChallengerOutput(success=True)

        worker.challenger.ainvoke = capture_input

        await worker._validate_documentation(state, sample_validation_ticket)

        # Verify citadel_context was passed
        assert len(captured_inputs) == 1
        challenger_input = captured_inputs[0]
        assert challenger_input.citadel_context is not None
        assert "paragraph_bodies" in challenger_input.citadel_context
        bodies = challenger_input.citadel_context["paragraph_bodies"]
        assert "0000-MAIN" in bodies

    async def test_validation_works_without_citadel(
        self, mock_config, mock_beads_client, sample_template, sample_validation_ticket
    ):
        """Test backward compatibility: validation works without Citadel."""
        worker = ChallengerWorker(
            worker_id="challenger-test",
            config=mock_config,
            beads_client=mock_beads_client,
            exit_on_error=False,
        )

        assert worker._citadel is None

        # Build validation state directly
        state = {
            "template": sample_template,
            "source_code": "       0000-MAIN.\n           STOP RUN.",
            "file_name": "TESTPROG.cbl",
            "file_type": FileType.COBOL,
            "iteration": 1,
        }

        # Mock challenger agent
        async def mock_invoke(input_data):
            # citadel_context should be None when Citadel is not available
            assert input_data.citadel_context is None
            return ChallengerOutput(success=True)

        worker.challenger.ainvoke = mock_invoke

        result = await worker._validate_documentation(state, sample_validation_ticket)
        assert result.success is True

    def test_citadel_context_in_prompt(self):
        """Test that ChallengerAgent renders citadel_context in prompt."""
        from war_rig.agents.challenger import ChallengerAgent, ChallengerInput
        from war_rig.config import ChallengerConfig, APIConfig

        agent = ChallengerAgent(
            config=ChallengerConfig(model="test-model"),
            api_config=APIConfig(
                provider="mock",
                api_key="test",
            ),
        )

        input_data = ChallengerInput(
            template=sample_template_for_prompt(),
            source_code="       0000-MAIN.\n           STOP RUN.",
            file_name="TEST.cbl",
            file_type=FileType.COBOL,
            citadel_context={
                "paragraph_bodies": {
                    "0000-MAIN": "       0000-MAIN.\n           DISPLAY 'HELLO'.\n           STOP RUN.",
                }
            },
        )

        prompt = agent._build_user_prompt(input_data)

        assert "Paragraph Source Code (from static analysis)" in prompt
        assert "0000-MAIN" in prompt
        assert "DISPLAY 'HELLO'" in prompt
        assert "Cross-reference" in prompt

    def test_no_citadel_section_without_context(self):
        """Test that prompt has no Citadel section without citadel_context."""
        from war_rig.agents.challenger import ChallengerAgent, ChallengerInput
        from war_rig.config import ChallengerConfig, APIConfig

        agent = ChallengerAgent(
            config=ChallengerConfig(model="test-model"),
            api_config=APIConfig(
                provider="mock",
                api_key="test",
            ),
        )

        input_data = ChallengerInput(
            template=sample_template_for_prompt(),
            source_code="       0000-MAIN.\n           STOP RUN.",
            file_name="TEST.cbl",
            file_type=FileType.COBOL,
        )

        prompt = agent._build_user_prompt(input_data)
        assert "Paragraph Source Code (from static analysis)" not in prompt


def sample_template_for_prompt() -> DocumentationTemplate:
    """Helper to create a template for prompt tests."""
    return DocumentationTemplate(
        header=HeaderSection(
            program_id="TEST",
            file_name="TEST.cbl",
            file_type=FileType.COBOL,
            analyzed_by="TEST",
            analyzed_at=datetime(2024, 1, 1),
            iteration_count=1,
        ),
        purpose=PurposeSection(
            summary="Test program",
            business_context="Test",
            program_type=ProgramType.BATCH,
            citations=[1],
        ),
    )
