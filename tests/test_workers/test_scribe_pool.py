"""Tests for Scribe worker pool.

Tests for ScribeWorker and ScribeWorkerPool classes that implement
parallel processing of documentation tickets.
"""

import asyncio
from datetime import datetime
from unittest.mock import AsyncMock, MagicMock, patch

import pytest

from war_rig.agents.scribe import ScribeOutput
from war_rig.beads import (
    BeadsClient,
    BeadsPriority,
    ProgramManagerTicket,
    TicketState,
    TicketType,
)
from war_rig.config import WarRigConfig
from war_rig.models.templates import FileType
from war_rig.workers.scribe_pool import (
    ScribeWorker,
    ScribeWorkerPool,
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
    return client


@pytest.fixture
def mock_config(tmp_path) -> MagicMock:
    """Create a mock WarRigConfig."""
    config = MagicMock(spec=WarRigConfig)
    config.num_scribes = 3
    config.input_directory = tmp_path
    config.scribe = MagicMock()
    config.scribe.model = "claude-sonnet-4-20250514"
    config.scribe.temperature = 0.3
    config.scribe.max_tokens = 4000
    config.api = MagicMock()
    config.api.provider = "openrouter"
    config.api.api_key = "test-key"
    config.api.base_url = "https://test.api.com"
    return config


@pytest.fixture
def sample_pm_ticket() -> ProgramManagerTicket:
    """Create a sample Program Manager ticket."""
    return ProgramManagerTicket(
        ticket_id="war_rig-test123",
        ticket_type=TicketType.DOCUMENTATION,
        state=TicketState.CREATED,
        file_name="TESTPROG.cbl",
        program_id="TESTPROG",
        cycle_number=1,
        metadata={
            "source_code": """       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       PROCEDURE DIVISION.
       0000-MAIN.
           DISPLAY 'HELLO WORLD'.
           STOP RUN.
""",
        },
    )


# =============================================================================
# WorkerStatus Tests
# =============================================================================


class TestWorkerStatus:
    """Tests for WorkerStatus dataclass."""

    def test_create_default_status(self):
        """Test creating status with defaults."""
        status = WorkerStatus(
            worker_id="scribe-1",
            state=WorkerState.IDLE,
        )

        assert status.worker_id == "scribe-1"
        assert status.state == WorkerState.IDLE
        assert status.current_ticket_id is None
        assert status.tickets_processed == 0
        assert status.tickets_failed == 0

    def test_create_full_status(self):
        """Test creating status with all fields."""
        now = datetime.utcnow()
        status = WorkerStatus(
            worker_id="scribe-2",
            state=WorkerState.PROCESSING,
            current_ticket_id="war_rig-abc123",
            tickets_processed=5,
            tickets_failed=1,
            last_activity=now,
            error_message=None,
        )

        assert status.current_ticket_id == "war_rig-abc123"
        assert status.tickets_processed == 5
        assert status.tickets_failed == 1


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
        assert WorkerState.STOPPED.value == "stopped"
        assert WorkerState.ERROR.value == "error"


# =============================================================================
# ScribeWorker Tests
# =============================================================================


class TestScribeWorker:
    """Tests for ScribeWorker class."""

    def test_create_worker(self, mock_config, mock_beads_client):
        """Test worker creation."""
        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        assert worker.worker_id == "scribe-1"
        assert worker.status.state == WorkerState.IDLE

    def test_compatible_ticket_types(self):
        """Test that worker lists compatible ticket types."""
        assert TicketType.DOCUMENTATION in ScribeWorker.COMPATIBLE_TICKET_TYPES
        assert TicketType.CLARIFICATION in ScribeWorker.COMPATIBLE_TICKET_TYPES
        assert TicketType.CHROME in ScribeWorker.COMPATIBLE_TICKET_TYPES
        # Should not include validation or holistic review
        assert TicketType.VALIDATION not in ScribeWorker.COMPATIBLE_TICKET_TYPES
        assert TicketType.HOLISTIC_REVIEW not in ScribeWorker.COMPATIBLE_TICKET_TYPES

    def test_determine_file_type_cobol(self, mock_config, mock_beads_client):
        """Test file type detection for COBOL files."""
        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        assert worker._determine_file_type("TEST.cbl") == FileType.COBOL
        assert worker._determine_file_type("TEST.CBL") == FileType.COBOL
        assert worker._determine_file_type("TEST.cob") == FileType.COBOL

    def test_determine_file_type_copybook(self, mock_config, mock_beads_client):
        """Test file type detection for copybooks."""
        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        assert worker._determine_file_type("TEST.cpy") == FileType.COPYBOOK
        assert worker._determine_file_type("TEST.copy") == FileType.COPYBOOK

    def test_determine_file_type_jcl(self, mock_config, mock_beads_client):
        """Test file type detection for JCL files."""
        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        assert worker._determine_file_type("JOB.jcl") == FileType.JCL

    def test_determine_file_type_other(self, mock_config, mock_beads_client):
        """Test file type detection for unknown files."""
        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        assert worker._determine_file_type("TEST.txt") == FileType.OTHER
        assert worker._determine_file_type("TEST.py") == FileType.OTHER

    def test_status_property(self, mock_config, mock_beads_client):
        """Test status property returns current state."""
        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        status = worker.status

        assert isinstance(status, WorkerStatus)
        assert status.worker_id == "scribe-1"

    @pytest.mark.asyncio
    async def test_stop_sets_flag(self, mock_config, mock_beads_client):
        """Test that stop() sets the should_stop flag."""
        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        await worker.stop()

        assert worker._should_stop is True

    @pytest.mark.asyncio
    async def test_run_stops_on_no_tickets(self, mock_config, mock_beads_client):
        """Test that worker stops when no tickets available after timeout."""
        mock_beads_client.get_available_tickets.return_value = []

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
            poll_interval=0.1,
            idle_timeout=0.3,
        )

        await worker.run()

        assert worker.status.state == WorkerState.STOPPED
        assert worker.status.tickets_processed == 0

    @pytest.mark.asyncio
    async def test_run_processes_ticket(
        self, mock_config, mock_beads_client, sample_pm_ticket
    ):
        """Test that worker processes available tickets."""
        # Return ticket on first call, then empty
        call_count = [0]

        def get_tickets(*args, **kwargs):
            call_count[0] += 1
            if call_count[0] == 1:
                return [sample_pm_ticket]
            return []

        mock_beads_client.get_available_tickets.side_effect = get_tickets

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
            poll_interval=0.1,
            idle_timeout=0.3,
        )

        # Mock the scribe agent by patching the internal attribute
        mock_output = ScribeOutput(success=True)
        mock_agent = MagicMock()
        mock_agent.ainvoke = AsyncMock(return_value=mock_output)
        worker._scribe_agent = mock_agent

        await worker.run()

        assert worker.status.tickets_processed == 1
        assert mock_beads_client.update_ticket_state.called


# =============================================================================
# ScribeWorkerPool Tests
# =============================================================================


class TestScribeWorkerPool:
    """Tests for ScribeWorkerPool class."""

    def test_create_pool(self, mock_config, mock_beads_client):
        """Test pool creation."""
        pool = ScribeWorkerPool(
            config=mock_config,
            beads_client=mock_beads_client,
        )

        assert pool.num_workers == 3  # From mock_config.num_scribes
        assert pool._started is False

    def test_create_pool_with_custom_workers(self, mock_config, mock_beads_client):
        """Test pool creation with custom worker count."""
        pool = ScribeWorkerPool(
            config=mock_config,
            beads_client=mock_beads_client,
            num_workers=5,
        )

        assert pool.num_workers == 5

    @pytest.mark.asyncio
    async def test_start_creates_workers(self, mock_config, mock_beads_client):
        """Test that start() creates worker instances."""
        mock_beads_client.get_available_tickets.return_value = []

        pool = ScribeWorkerPool(
            config=mock_config,
            beads_client=mock_beads_client,
            num_workers=2,
            idle_timeout=0.1,
        )

        await pool.start()

        assert pool._started is True
        assert len(pool._workers) == 2
        assert len(pool._tasks) == 2

        # Wait for workers to stop (no tickets)
        await pool.wait()
        await pool.stop()

    @pytest.mark.asyncio
    async def test_start_raises_if_already_started(self, mock_config, mock_beads_client):
        """Test that starting an already-started pool raises error."""
        mock_beads_client.get_available_tickets.return_value = []

        pool = ScribeWorkerPool(
            config=mock_config,
            beads_client=mock_beads_client,
            num_workers=1,
            idle_timeout=0.1,
        )

        await pool.start()

        with pytest.raises(RuntimeError, match="already started"):
            await pool.start()

        await pool.wait()
        await pool.stop()

    @pytest.mark.asyncio
    async def test_stop_clears_workers(self, mock_config, mock_beads_client):
        """Test that stop() clears worker lists."""
        mock_beads_client.get_available_tickets.return_value = []

        pool = ScribeWorkerPool(
            config=mock_config,
            beads_client=mock_beads_client,
            num_workers=2,
            idle_timeout=0.1,
        )

        await pool.start()
        await pool.wait()
        await pool.stop()

        assert pool._started is False
        assert len(pool._workers) == 0
        assert len(pool._tasks) == 0

    def test_get_status_not_started(self, mock_config, mock_beads_client):
        """Test get_status() when pool not started."""
        pool = ScribeWorkerPool(
            config=mock_config,
            beads_client=mock_beads_client,
        )

        status = pool.get_status()

        assert status["started"] is False
        assert status["num_workers"] == 3
        assert status["active_count"] == 0
        assert status["workers"] == []

    @pytest.mark.asyncio
    async def test_get_status_started(self, mock_config, mock_beads_client):
        """Test get_status() when pool is running."""
        mock_beads_client.get_available_tickets.return_value = []

        pool = ScribeWorkerPool(
            config=mock_config,
            beads_client=mock_beads_client,
            num_workers=2,
            idle_timeout=0.2,
        )

        await pool.start()

        # Give workers a moment to start
        await asyncio.sleep(0.05)

        status = pool.get_status()

        assert status["started"] is True
        assert status["num_workers"] == 2
        assert len(status["workers"]) == 2

        await pool.wait()
        await pool.stop()

    def test_is_idle_not_started(self, mock_config, mock_beads_client):
        """Test is_idle() when pool not started."""
        pool = ScribeWorkerPool(
            config=mock_config,
            beads_client=mock_beads_client,
        )

        assert pool.is_idle() is True

    def test_is_done_not_started(self, mock_config, mock_beads_client):
        """Test is_done() when pool not started."""
        pool = ScribeWorkerPool(
            config=mock_config,
            beads_client=mock_beads_client,
        )

        assert pool.is_done() is True

    @pytest.mark.asyncio
    async def test_is_done_after_completion(self, mock_config, mock_beads_client):
        """Test is_done() after workers complete."""
        mock_beads_client.get_available_tickets.return_value = []

        pool = ScribeWorkerPool(
            config=mock_config,
            beads_client=mock_beads_client,
            num_workers=1,
            idle_timeout=0.1,
        )

        await pool.start()
        await pool.wait()

        assert pool.is_done() is True

        await pool.stop()
