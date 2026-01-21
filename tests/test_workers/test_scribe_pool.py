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
    config.output_directory = tmp_path / "output"
    config.scribe = MagicMock()
    config.scribe.model = "claude-sonnet-4-20250514"
    config.scribe.temperature = 0.3
    config.scribe.max_prompt_tokens = 15000  # Required for SourceCodePreparer
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


# =============================================================================
# Idle Timeout Tests
# =============================================================================


class TestIdleTimeout:
    """Tests for idle timeout behavior."""

    @pytest.mark.asyncio
    async def test_idle_timeout_measured_from_processing_end(
        self, mock_config, mock_beads_client, sample_pm_ticket
    ):
        """Test that idle timeout is measured from when processing ENDS, not starts.

        This prevents workers from stopping immediately after completing a long-running
        ticket (> idle_timeout duration).
        """
        # Track processing time
        processing_duration = 0.4  # Longer than idle_timeout
        idle_timeout = 0.2

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
            poll_interval=0.05,
            idle_timeout=idle_timeout,
        )

        # Mock the agent to simulate slow processing
        async def slow_processing(*args, **kwargs):
            await asyncio.sleep(processing_duration)
            return ScribeOutput(success=True)

        mock_agent = MagicMock()
        mock_agent.ainvoke = slow_processing
        worker._scribe_agent = mock_agent

        start_time = datetime.utcnow()
        await worker.run()
        total_time = (datetime.utcnow() - start_time).total_seconds()

        # Worker should have:
        # 1. Processed the ticket (0.4s)
        # 2. Then waited idle_timeout (0.2s) before stopping
        # Total should be > processing_duration + idle_timeout
        # If idle was measured from start, worker would stop immediately after processing
        assert total_time >= processing_duration + idle_timeout - 0.1  # Small tolerance
        assert worker.status.tickets_processed == 1

    @pytest.mark.asyncio
    async def test_worker_doesnt_stop_immediately_after_long_processing(
        self, mock_config, mock_beads_client, sample_pm_ticket
    ):
        """Test that worker doesn't stop immediately after finishing a long ticket."""
        processing_time = 0.3
        idle_timeout = 0.1

        tickets_returned = [0]

        def get_tickets(*args, **kwargs):
            tickets_returned[0] += 1
            if tickets_returned[0] == 1:
                return [sample_pm_ticket]
            return []

        mock_beads_client.get_available_tickets.side_effect = get_tickets

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
            poll_interval=0.02,
            idle_timeout=idle_timeout,
        )

        # Track how many times poll is called after processing
        poll_after_processing = [0]
        original_poll = worker._poll_for_ticket

        async def tracking_poll():
            if tickets_returned[0] > 1:
                poll_after_processing[0] += 1
            return await original_poll()

        worker._poll_for_ticket = tracking_poll

        async def slow_agent(*args, **kwargs):
            await asyncio.sleep(processing_time)
            return ScribeOutput(success=True)

        mock_agent = MagicMock()
        mock_agent.ainvoke = slow_agent
        worker._scribe_agent = mock_agent

        await worker.run()

        # After processing (0.3s), worker should poll multiple times before idle timeout (0.1s)
        # If idle was measured from start, worker would stop immediately (poll_after_processing = 0)
        assert poll_after_processing[0] >= 2, (
            f"Expected at least 2 polls after processing, got {poll_after_processing[0]}. "
            "Worker may be stopping immediately after long-running tasks."
        )


# =============================================================================
# CancelledError Handling Tests
# =============================================================================


class TestCancelledErrorHandling:
    """Tests for CancelledError handling in workers."""

    @pytest.mark.asyncio
    async def test_cancelled_during_processing_resets_ticket(
        self, mock_config, mock_beads_client, sample_pm_ticket
    ):
        """Test that cancellation during processing resets ticket to CREATED."""
        mock_beads_client.get_available_tickets.return_value = [sample_pm_ticket]

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
            poll_interval=0.1,
            idle_timeout=30.0,
        )

        # Mock agent that takes forever (will be cancelled)
        async def slow_forever(*args, **kwargs):
            await asyncio.sleep(100)
            return ScribeOutput(success=True)

        mock_agent = MagicMock()
        mock_agent.ainvoke = slow_forever
        worker._scribe_agent = mock_agent

        # Start worker and cancel it mid-processing
        task = asyncio.create_task(worker.run())
        await asyncio.sleep(0.1)  # Let it start processing
        task.cancel()

        with pytest.raises(asyncio.CancelledError):
            await task

        # Verify ticket was reset to CREATED (not left as IN_PROGRESS)
        reset_calls = [
            call for call in mock_beads_client.update_ticket_state.call_args_list
            if call[0][1] == TicketState.CREATED
        ]
        assert len(reset_calls) >= 1, "Ticket should be reset to CREATED on cancellation"

    @pytest.mark.asyncio
    async def test_cancelled_ticket_has_retry_reason(
        self, mock_config, mock_beads_client, sample_pm_ticket
    ):
        """Test that cancelled ticket reset includes appropriate reason."""
        mock_beads_client.get_available_tickets.return_value = [sample_pm_ticket]

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
            poll_interval=0.1,
            idle_timeout=30.0,
        )

        async def slow_forever(*args, **kwargs):
            await asyncio.sleep(100)
            return ScribeOutput(success=True)

        mock_agent = MagicMock()
        mock_agent.ainvoke = slow_forever
        worker._scribe_agent = mock_agent

        task = asyncio.create_task(worker.run())
        await asyncio.sleep(0.1)
        task.cancel()

        with pytest.raises(asyncio.CancelledError):
            await task

        # Check the reason includes "cancel" or "retry"
        reset_calls = [
            call for call in mock_beads_client.update_ticket_state.call_args_list
            if call[0][1] == TicketState.CREATED and "reason" in call[1]
        ]
        if reset_calls:
            reason = reset_calls[-1][1].get("reason", "")
            assert "cancel" in reason.lower() or "retry" in reason.lower()


# =============================================================================
# Stop Behavior Tests
# =============================================================================


class TestStopBehavior:
    """Tests for worker stop behavior."""

    @pytest.mark.asyncio
    async def test_stop_already_stopped_worker_no_duplicate_log(
        self, mock_config, mock_beads_client, caplog
    ):
        """Test that stopping an already-stopped worker doesn't log 'Stop requested'."""
        mock_beads_client.get_available_tickets.return_value = []

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
            poll_interval=0.05,
            idle_timeout=0.1,
        )

        # Run until natural stop (idle timeout)
        await worker.run()
        assert worker.status.state == WorkerState.STOPPED

        # Clear logs and call stop() on already-stopped worker
        caplog.clear()
        await worker.stop()

        # Should not log "Stop requested" for already-stopped worker
        stop_requested_logs = [
            r for r in caplog.records if "Stop requested" in r.message
        ]
        assert len(stop_requested_logs) == 0, (
            "Should not log 'Stop requested' for already-stopped worker"
        )
