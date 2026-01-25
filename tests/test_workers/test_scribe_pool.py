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
    config.exit_on_error = True
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


# =============================================================================
# Critical Section Validation Tests (IMPFB-005)
# =============================================================================


class TestCriticalSectionValidation:
    """Tests for critical section validation in Scribe worker."""

    def test_validate_critical_sections_no_context(
        self, mock_config, mock_beads_client
    ):
        """Test validation passes when no feedback context."""
        from war_rig.models.templates import DocumentationTemplate

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        template = DocumentationTemplate(file_name="TEST.cbl")
        is_valid, empty = worker._validate_critical_sections(template, None)

        assert is_valid is True
        assert empty == []

    def test_validate_critical_sections_no_critical_sections(
        self, mock_config, mock_beads_client
    ):
        """Test validation passes when feedback context has no critical sections."""
        from war_rig.models.templates import DocumentationTemplate

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        template = DocumentationTemplate(file_name="TEST.cbl")
        feedback_context = {"critical_sections": []}

        is_valid, empty = worker._validate_critical_sections(template, feedback_context)

        assert is_valid is True
        assert empty == []

    def test_validate_critical_sections_purpose_empty(
        self, mock_config, mock_beads_client
    ):
        """Test validation fails when purpose section is empty."""
        from war_rig.models.templates import DocumentationTemplate, PurposeSection

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        # Template with empty purpose
        template = DocumentationTemplate(
            file_name="TEST.cbl",
            purpose=PurposeSection(summary=""),  # Empty!
        )
        feedback_context = {"critical_sections": ["purpose"]}

        is_valid, empty = worker._validate_critical_sections(template, feedback_context)

        assert is_valid is False
        assert "purpose" in empty

    def test_validate_critical_sections_purpose_populated(
        self, mock_config, mock_beads_client
    ):
        """Test validation passes when purpose section has content."""
        from war_rig.models.templates import DocumentationTemplate, PurposeSection

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        template = DocumentationTemplate(
            file_name="TEST.cbl",
            purpose=PurposeSection(summary="This program processes customer data."),
        )
        feedback_context = {"critical_sections": ["purpose"]}

        is_valid, empty = worker._validate_critical_sections(template, feedback_context)

        assert is_valid is True
        assert empty == []

    def test_validate_critical_sections_inputs_empty(
        self, mock_config, mock_beads_client
    ):
        """Test validation fails when inputs section is empty."""
        from war_rig.models.templates import DocumentationTemplate

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        template = DocumentationTemplate(
            file_name="TEST.cbl",
            inputs=[],  # Empty inputs
        )
        feedback_context = {"critical_sections": ["inputs"]}

        is_valid, empty = worker._validate_critical_sections(template, feedback_context)

        assert is_valid is False
        assert "inputs" in empty

    def test_validate_critical_sections_outputs_empty(
        self, mock_config, mock_beads_client
    ):
        """Test validation fails when outputs section is empty."""
        from war_rig.models.templates import DocumentationTemplate

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        template = DocumentationTemplate(
            file_name="TEST.cbl",
            outputs=[],  # Empty outputs
        )
        feedback_context = {"critical_sections": ["outputs"]}

        is_valid, empty = worker._validate_critical_sections(template, feedback_context)

        assert is_valid is False
        assert "outputs" in empty

    def test_validate_critical_sections_multiple_missing(
        self, mock_config, mock_beads_client
    ):
        """Test validation reports all missing sections."""
        from war_rig.models.templates import DocumentationTemplate, PurposeSection

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        template = DocumentationTemplate(
            file_name="TEST.cbl",
            purpose=PurposeSection(summary=""),
            inputs=[],
            outputs=[],
        )
        feedback_context = {"critical_sections": ["purpose", "inputs", "outputs"]}

        is_valid, empty = worker._validate_critical_sections(template, feedback_context)

        assert is_valid is False
        assert len(empty) == 3
        assert "purpose" in empty
        assert "inputs" in empty
        assert "outputs" in empty

    def test_validate_critical_sections_copybook_skips_called_programs(
        self, mock_config, mock_beads_client
    ):
        """Test validation skips called_programs for copybook files."""
        from war_rig.models.templates import (
            DocumentationTemplate,
            HeaderSection,
            FileType,
        )

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        # Copybook with empty called_programs (legitimate)
        template = DocumentationTemplate(
            file_name="TEST.cpy",
            header=HeaderSection(
                program_id="TEST",
                file_name="TEST.cpy",
                file_type=FileType.COPYBOOK,
            ),
            called_programs=[],  # Empty but valid for copybook
        )
        feedback_context = {"critical_sections": ["called_programs", "data_flow"]}

        is_valid, empty = worker._validate_critical_sections(template, feedback_context)

        # Should pass - copybooks don't call programs
        assert is_valid is True
        assert empty == []

    def test_validate_critical_sections_copybook_still_checks_purpose(
        self, mock_config, mock_beads_client
    ):
        """Test validation still checks purpose for copybook files."""
        from war_rig.models.templates import (
            DocumentationTemplate,
            HeaderSection,
            FileType,
            PurposeSection,
        )

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        template = DocumentationTemplate(
            file_name="TEST.cpy",
            header=HeaderSection(
                program_id="TEST",
                file_name="TEST.cpy",
                file_type=FileType.COPYBOOK,
            ),
            purpose=PurposeSection(summary=""),  # Empty - should fail
        )
        feedback_context = {"critical_sections": ["purpose", "called_programs"]}

        is_valid, empty = worker._validate_critical_sections(template, feedback_context)

        # Purpose should fail, called_programs should be skipped
        assert is_valid is False
        assert "purpose" in empty
        assert "called_programs" not in empty

    def test_validate_critical_sections_jcl_skips_called_programs(
        self, mock_config, mock_beads_client
    ):
        """Test validation skips called_programs for JCL files."""
        from war_rig.models.templates import (
            DocumentationTemplate,
            HeaderSection,
            FileType,
        )

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        template = DocumentationTemplate(
            file_name="TEST.jcl",
            header=HeaderSection(
                program_id="TEST",
                file_name="TEST.jcl",
                file_type=FileType.JCL,
            ),
            called_programs=[],
        )
        feedback_context = {"critical_sections": ["called_programs"]}

        is_valid, empty = worker._validate_critical_sections(template, feedback_context)

        # Should pass - JCL doesn't use called_programs in the same way
        assert is_valid is True
        assert empty == []

    def test_validate_critical_sections_cobol_checks_all(
        self, mock_config, mock_beads_client
    ):
        """Test validation checks all sections for COBOL programs."""
        from war_rig.models.templates import (
            DocumentationTemplate,
            HeaderSection,
            FileType,
        )

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        template = DocumentationTemplate(
            file_name="TEST.cbl",
            header=HeaderSection(
                program_id="TEST",
                file_name="TEST.cbl",
                file_type=FileType.COBOL,
            ),
            called_programs=[],  # Empty - should fail for COBOL
        )
        feedback_context = {"critical_sections": ["called_programs"]}

        is_valid, empty = worker._validate_critical_sections(template, feedback_context)

        # Should fail - COBOL programs should have called_programs checked
        assert is_valid is False
        assert "called_programs" in empty

    def test_validate_critical_sections_detected_file_type_overrides_template(
        self, mock_config, mock_beads_client
    ):
        """Test that detected_file_type takes precedence over template.header.file_type.

        This tests the fix for when LLM doesn't populate header.file_type but the
        extension-detected file_type is passed explicitly.
        """
        from war_rig.models.templates import (
            DocumentationTemplate,
            HeaderSection,
            FileType,
        )

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        # Template with NO file_type in header (simulates LLM not setting it)
        template = DocumentationTemplate(
            file_name="TEST.cpy",
            header=HeaderSection(
                program_id="TEST",
                file_name="TEST.cpy",
                file_type=None,  # LLM didn't populate this
            ),
            called_programs=[],  # Empty
        )
        feedback_context = {"critical_sections": ["called_programs", "data_flow"]}

        # Without detected_file_type, validation would fail
        is_valid, empty = worker._validate_critical_sections(
            template, feedback_context, detected_file_type=None
        )
        assert is_valid is False
        assert "called_programs" in empty

        # With detected_file_type=COPYBOOK, validation should pass
        is_valid, empty = worker._validate_critical_sections(
            template, feedback_context, detected_file_type=FileType.COPYBOOK
        )
        assert is_valid is True
        assert empty == []

    def test_validate_critical_sections_detected_file_type_with_no_header(
        self, mock_config, mock_beads_client
    ):
        """Test that detected_file_type works even when template.header is None."""
        from war_rig.models.templates import DocumentationTemplate, FileType

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        # Template with completely missing header
        template = DocumentationTemplate(
            file_name="TEST.cpy",
            header=None,  # No header at all
            called_programs=[],
        )
        feedback_context = {"critical_sections": ["called_programs", "data_flow"]}

        # With detected_file_type=COPYBOOK, validation should skip these sections
        is_valid, empty = worker._validate_critical_sections(
            template, feedback_context, detected_file_type=FileType.COPYBOOK
        )
        assert is_valid is True
        assert empty == []

    def test_validate_critical_sections_listing_skips_most(
        self, mock_config, mock_beads_client
    ):
        """Test validation skips most sections for LISTING files (compiler output).

        LISTING should skip: called_programs, data_flow, copybooks, sql_operations,
        cics_operations, business_rules, error_handling, inputs, outputs
        Only keeps: purpose
        """
        from war_rig.models.templates import (
            DocumentationTemplate,
            HeaderSection,
            FileType,
            PurposeSection,
        )

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        # LISTING with all empty sections that should be skipped
        template = DocumentationTemplate(
            file_name="TEST.lst",
            header=HeaderSection(
                program_id="TEST",
                file_name="TEST.lst",
                file_type=FileType.LISTING,
            ),
            purpose=PurposeSection(summary="Compiler output listing"),  # Has content
            inputs=[],
            outputs=[],
            called_programs=[],
            copybooks_used=[],
            sql_operations=[],
            cics_operations=[],
            business_rules=[],
            error_handling=[],
        )
        # Request all the sections that should be skipped
        feedback_context = {
            "critical_sections": [
                "called_programs",
                "data_flow",
                "copybooks",
                "sql_operations",
                "cics_operations",
                "business_rules",
                "error_handling",
                "inputs",
                "outputs",
            ]
        }

        is_valid, empty = worker._validate_critical_sections(template, feedback_context)

        # Should pass - LISTING skips all of these
        assert is_valid is True
        assert empty == []

    def test_validate_critical_sections_other_is_lenient(
        self, mock_config, mock_beads_client
    ):
        """Test validation is lenient for OTHER (unknown) file types.

        OTHER should skip: called_programs, data_flow, copybooks, sql_operations,
        cics_operations, business_rules, error_handling
        """
        from war_rig.models.templates import (
            DocumentationTemplate,
            HeaderSection,
            FileType,
            PurposeSection,
        )

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        # OTHER with empty sections that should be skipped
        template = DocumentationTemplate(
            file_name="TEST.xyz",
            header=HeaderSection(
                program_id="TEST",
                file_name="TEST.xyz",
                file_type=FileType.OTHER,
            ),
            purpose=PurposeSection(summary="Unknown file type"),  # Has content
            called_programs=[],
            copybooks_used=[],
            sql_operations=[],
            cics_operations=[],
            business_rules=[],
            error_handling=[],
        )
        # Request all sections that should be skipped for OTHER
        feedback_context = {
            "critical_sections": [
                "called_programs",
                "data_flow",
                "copybooks",
                "sql_operations",
                "cics_operations",
                "business_rules",
                "error_handling",
            ]
        }

        is_valid, empty = worker._validate_critical_sections(template, feedback_context)

        # Should pass - OTHER skips all of these
        assert is_valid is True
        assert empty == []

    def test_validate_critical_sections_asm_skips_copybooks(
        self, mock_config, mock_beads_client
    ):
        """Test validation skips copybooks, cics, sql for ASM (assembler) files.

        ASM should skip: copybooks (uses macros), cics_operations, sql_operations
        """
        from war_rig.models.templates import (
            DocumentationTemplate,
            HeaderSection,
            FileType,
            PurposeSection,
        )

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        # ASM with empty sections that should be skipped
        template = DocumentationTemplate(
            file_name="TEST.asm",
            header=HeaderSection(
                program_id="TEST",
                file_name="TEST.asm",
                file_type=FileType.ASM,
            ),
            purpose=PurposeSection(summary="Assembler program"),  # Has content
            copybooks_used=[],  # Empty but valid for ASM (uses macros)
            cics_operations=[],  # Empty but valid for ASM
            sql_operations=[],  # Empty but valid for ASM
        )
        # Request sections that should be skipped for ASM
        feedback_context = {
            "critical_sections": [
                "copybooks",
                "cics_operations",
                "sql_operations",
            ]
        }

        is_valid, empty = worker._validate_critical_sections(template, feedback_context)

        # Should pass - ASM skips copybooks, cics, sql
        assert is_valid is True
        assert empty == []

    def test_validate_critical_sections_sort_skips_most(
        self, mock_config, mock_beads_client
    ):
        """Test validation skips most sections for SORT (DFSORT control cards).

        SORT should skip: called_programs, copybooks, cics_operations, sql_operations,
        business_rules, error_handling, data_flow
        Keeps: purpose, inputs, outputs
        """
        from war_rig.models.templates import (
            DocumentationTemplate,
            HeaderSection,
            FileType,
            PurposeSection,
            InputOutput,
            IOType,
        )

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        # SORT with populated purpose, inputs, outputs (which are kept)
        # and empty sections that should be skipped
        template = DocumentationTemplate(
            file_name="TEST.srt",
            header=HeaderSection(
                program_id="TEST",
                file_name="TEST.srt",
                file_type=FileType.SORT,
            ),
            purpose=PurposeSection(summary="DFSORT control cards for sorting"),
            inputs=[InputOutput(name="SORTIN", io_type=IOType.FILE_SEQUENTIAL)],
            outputs=[InputOutput(name="SORTOUT", io_type=IOType.FILE_SEQUENTIAL)],
            called_programs=[],  # Empty but valid for SORT
            copybooks_used=[],  # Empty but valid for SORT
            cics_operations=[],  # Empty but valid for SORT
            sql_operations=[],  # Empty but valid for SORT
            business_rules=[],  # Empty but valid for SORT
            error_handling=[],  # Empty but valid for SORT
        )
        # Request sections that should be skipped for SORT
        feedback_context = {
            "critical_sections": [
                "called_programs",
                "copybooks",
                "cics_operations",
                "sql_operations",
                "business_rules",
                "error_handling",
                "data_flow",
            ]
        }

        is_valid, empty = worker._validate_critical_sections(template, feedback_context)

        # Should pass - SORT skips all of these
        assert is_valid is True
        assert empty == []


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
        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        assert hasattr(worker, "_lock_skipped_tickets")
        assert hasattr(worker, "_lock_skip_duration")
        assert isinstance(worker._lock_skipped_tickets, dict)
        assert worker._lock_skip_duration > 0

    @pytest.mark.asyncio
    async def test_lock_skipped_ticket_not_reclaimed_immediately(
        self, mock_config, mock_beads_client, sample_pm_ticket, tmp_path
    ):
        """Test that a ticket released due to lock failure is not immediately reclaimed.

        This is the core test for the infinite loop bug fix.
        """
        from war_rig.utils.file_lock import FileLockManager

        # Create a real file lock manager
        lock_manager = FileLockManager()

        # Pre-acquire the lock to simulate another worker holding it
        output_file = str(tmp_path / "output" / "TESTPROG.doc.json")
        (tmp_path / "output").mkdir(parents=True, exist_ok=True)

        # Simulate another worker holding the lock
        await lock_manager.acquire(output_file, "other-worker")

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
            output_directory=tmp_path / "output",
            file_lock_manager=lock_manager,
            poll_interval=0.05,
            idle_timeout=0.3,
        )

        # Return the same ticket multiple times to simulate re-claiming scenario
        claim_attempts = [0]

        def get_tickets(*args, **kwargs):
            return [sample_pm_ticket]

        def claim_ticket(ticket_id, worker_id):
            claim_attempts[0] += 1
            return True  # Always succeed in claiming

        mock_beads_client.get_available_tickets.side_effect = get_tickets
        mock_beads_client.claim_ticket.side_effect = claim_ticket

        # Run worker for a short duration
        await worker.run()

        # Release the lock we held
        await lock_manager.release(output_file, "other-worker")

        # Without the fix, claim_attempts would be very high (infinite loop)
        # With the fix, it should be limited (ticket skipped after first lock failure)
        # Allow some claims due to timing, but should be << what infinite loop would cause
        # In 0.3 seconds with 0.05 poll interval, unthrottled would be ~6 claims
        # With 5 second skip, should only be 1 claim before skip kicks in
        assert claim_attempts[0] <= 3, (
            f"Worker claimed ticket {claim_attempts[0]} times. "
            "Expected <= 3 with lock-skip fix preventing infinite loop."
        )

    @pytest.mark.asyncio
    async def test_lock_skip_expires_after_duration(
        self, mock_config, mock_beads_client, sample_pm_ticket
    ):
        """Test that lock-skipped tickets become claimable again after skip duration expires."""
        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
            poll_interval=0.05,
            idle_timeout=30.0,
        )

        # Set a very short skip duration for testing
        worker._lock_skip_duration = 0.1

        # Manually add ticket to lock-skipped set
        worker._lock_skipped_tickets[sample_pm_ticket.ticket_id] = datetime.utcnow()

        # Ticket should be skipped initially
        mock_beads_client.get_available_tickets.return_value = [sample_pm_ticket]
        mock_beads_client.claim_ticket.return_value = True

        ticket = await worker._poll_for_ticket()
        assert ticket is None, "Ticket should be skipped immediately after lock failure"

        # Wait for skip duration to expire
        await asyncio.sleep(0.15)

        # Now ticket should be claimable again
        ticket = await worker._poll_for_ticket()
        assert ticket is not None, "Ticket should be claimable after skip duration expires"
        assert ticket.ticket_id == sample_pm_ticket.ticket_id

    @pytest.mark.asyncio
    async def test_multiple_tickets_only_locked_one_skipped(
        self, mock_config, mock_beads_client, tmp_path
    ):
        """Test that only the lock-failed ticket is skipped, not other available tickets."""
        from war_rig.utils.file_lock import FileLockManager

        # Create two tickets
        ticket1 = ProgramManagerTicket(
            ticket_id="ticket-1",
            ticket_type=TicketType.DOCUMENTATION,
            state=TicketState.CREATED,
            file_name="FILE1.cbl",
            program_id="FILE1",
            cycle_number=1,
            metadata={"source_code": "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. FILE1."},
        )
        ticket2 = ProgramManagerTicket(
            ticket_id="ticket-2",
            ticket_type=TicketType.DOCUMENTATION,
            state=TicketState.CREATED,
            file_name="FILE2.cbl",
            program_id="FILE2",
            cycle_number=1,
            metadata={"source_code": "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. FILE2."},
        )

        lock_manager = FileLockManager()
        (tmp_path / "output").mkdir(parents=True, exist_ok=True)

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
            output_directory=tmp_path / "output",
            file_lock_manager=lock_manager,
            poll_interval=0.05,
            idle_timeout=30.0,
        )

        # Lock file for ticket1
        file1_path = str(tmp_path / "output" / "FILE1.doc.json")
        await lock_manager.acquire(file1_path, "other-worker")

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

        # Cleanup
        await lock_manager.release(file1_path, "other-worker")

    @pytest.mark.asyncio
    async def test_lock_failure_adds_to_skip_set(
        self, mock_config, mock_beads_client, sample_pm_ticket, tmp_path
    ):
        """Test that failing to acquire a lock adds the ticket to the skip set."""
        from war_rig.utils.file_lock import FileLockManager

        lock_manager = FileLockManager()
        (tmp_path / "output").mkdir(parents=True, exist_ok=True)

        # Pre-lock the file (new naming convention includes source extension)
        output_file = str(tmp_path / "output" / "TESTPROG.cbl.doc.json")
        await lock_manager.acquire(output_file, "other-worker")

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
            output_directory=tmp_path / "output",
            file_lock_manager=lock_manager,
        )

        # Verify ticket not in skip set initially
        assert sample_pm_ticket.ticket_id not in worker._lock_skipped_tickets

        # Process the ticket (will fail to acquire lock)
        await worker._process_ticket(sample_pm_ticket)

        # Verify ticket was added to skip set
        assert sample_pm_ticket.ticket_id in worker._lock_skipped_tickets

        # Cleanup
        await lock_manager.release(output_file, "other-worker")
