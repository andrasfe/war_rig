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
