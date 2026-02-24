"""Tests for Scribe worker pool.

Tests for ScribeWorker and ScribeWorkerPool classes that implement
parallel processing of documentation tickets.
"""

import asyncio
import json
from datetime import datetime
from unittest.mock import AsyncMock, MagicMock

import pytest

from war_rig.agents.scribe import ScribeOutput
from war_rig.beads import (
    BeadsClient,
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
    config.scribe.citadel_max_paragraphs_per_batch = 20  # Max paragraphs per batch
    config.api = MagicMock()
    config.api.provider = "openrouter"
    config.api.api_key = "test-key"
    config.api.base_url = "https://test.api.com"
    config.enable_call_semantics = True
    # Minion scribe pool config
    config.num_minion_scribes = 4
    config.minion_scribe_batch_size = 5
    config.minion_scribe_model = "anthropic/claude-3-haiku-20240307"
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
    async def test_start_raises_if_already_started(
        self, mock_config, mock_beads_client
    ):
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
            call
            for call in mock_beads_client.update_ticket_state.call_args_list
            if call[0][1] == TicketState.CREATED
        ]
        assert len(reset_calls) >= 1, (
            "Ticket should be reset to CREATED on cancellation"
        )

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
            call
            for call in mock_beads_client.update_ticket_state.call_args_list
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
            FileType,
            HeaderSection,
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
            FileType,
            HeaderSection,
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
            FileType,
            HeaderSection,
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
            FileType,
            HeaderSection,
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
            FileType,
            HeaderSection,
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
            FileType,
            HeaderSection,
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
            FileType,
            HeaderSection,
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
            FileType,
            HeaderSection,
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
            FileType,
            HeaderSection,
            InputOutput,
            IOType,
            PurposeSection,
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
        assert ticket is not None, (
            "Ticket should be claimable after skip duration expires"
        )
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
            metadata={
                "source_code": "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. FILE1."
            },
        )
        ticket2 = ProgramManagerTicket(
            ticket_id="ticket-2",
            ticket_type=TicketType.DOCUMENTATION,
            state=TicketState.CREATED,
            file_name="FILE2.cbl",
            program_id="FILE2",
            cycle_number=1,
            metadata={
                "source_code": "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. FILE2."
            },
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


# =============================================================================
# Citadel Enrichment Tests
# =============================================================================


class TestApplyCitadelEnrichment:
    """Tests for _apply_citadel_enrichment helper method."""

    async def test_returns_template_unchanged_when_citadel_not_available(
        self, mock_config, mock_beads_client, sample_pm_ticket, tmp_path
    ):
        """When Citadel SDK is not initialized, template is returned as-is."""
        from war_rig.models.templates import DocumentationTemplate

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
            output_directory=tmp_path / "output",
        )
        # Ensure citadel is not available
        worker._citadel = None

        template = DocumentationTemplate()
        result = await worker._apply_citadel_enrichment(
            template, sample_pm_ticket, FileType.COBOL
        )
        assert result is template
        assert result.flow_diagram is None

    async def test_generates_flow_diagram_for_cobol(
        self, mock_config, mock_beads_client, sample_pm_ticket, tmp_path
    ):
        """When Citadel is available and file is COBOL, flow diagram is set."""
        from war_rig.models.templates import DocumentationTemplate

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
            output_directory=tmp_path / "output",
        )

        mock_citadel = MagicMock()
        mock_citadel.get_flow_diagram.return_value = "flowchart TD\n    A --> B"
        worker._citadel = mock_citadel

        # Mock _get_citadel_context to return None (no enrichment, just flow diagram)
        worker._get_citadel_context = MagicMock(return_value=None)

        template = DocumentationTemplate()
        result = await worker._apply_citadel_enrichment(
            template, sample_pm_ticket, FileType.COBOL
        )
        assert result.flow_diagram == "flowchart TD\n    A --> B"
        mock_citadel.get_flow_diagram.assert_called_once()

    async def test_skips_flow_diagram_for_non_cobol(
        self, mock_config, mock_beads_client, tmp_path
    ):
        """Flow diagram is not generated for non-COBOL file types."""
        from war_rig.models.templates import DocumentationTemplate

        ticket = ProgramManagerTicket(
            ticket_id="war_rig-jcl1",
            ticket_type=TicketType.DOCUMENTATION,
            state=TicketState.CREATED,
            file_name="TEST.jcl",
            program_id="TEST",
            cycle_number=1,
            metadata={},
        )

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
            output_directory=tmp_path / "output",
        )

        mock_citadel = MagicMock()
        worker._citadel = mock_citadel
        worker._get_citadel_context = MagicMock(return_value=None)

        template = DocumentationTemplate()
        result = await worker._apply_citadel_enrichment(template, ticket, FileType.JCL)
        assert result.flow_diagram is None
        mock_citadel.get_flow_diagram.assert_not_called()

    async def test_enriches_paragraphs_with_citadel_context(
        self, mock_config, mock_beads_client, sample_pm_ticket, tmp_path
    ):
        """When citadel context is available, paragraphs are enriched."""
        from war_rig.models.templates import DocumentationTemplate, HeaderSection

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
            output_directory=tmp_path / "output",
        )

        mock_citadel = MagicMock()
        mock_citadel.get_flow_diagram.return_value = "flowchart TD\n    A --> B"
        worker._citadel = mock_citadel

        citadel_context = {"functions": [], "includes": []}
        worker._get_citadel_context = MagicMock(return_value=citadel_context)

        enriched_template = DocumentationTemplate(
            header=HeaderSection(file_name="ENRICHED.cbl")
        )
        worker._enrich_paragraphs_with_citadel = AsyncMock(
            return_value=enriched_template
        )

        template = DocumentationTemplate()
        result = await worker._apply_citadel_enrichment(
            template, sample_pm_ticket, FileType.COBOL
        )

        worker._enrich_paragraphs_with_citadel.assert_called_once()
        # The enriched template should have been returned (with flow diagram added)
        assert result.header is not None
        assert result.header.file_name == "ENRICHED.cbl"
        assert result.flow_diagram == "flowchart TD\n    A --> B"

    async def test_handles_flow_diagram_exception_gracefully(
        self, mock_config, mock_beads_client, sample_pm_ticket, tmp_path
    ):
        """Flow diagram generation errors are caught, template still returned."""
        from war_rig.models.templates import DocumentationTemplate

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
            output_directory=tmp_path / "output",
        )

        mock_citadel = MagicMock()
        mock_citadel.get_flow_diagram.side_effect = RuntimeError("parse error")
        worker._citadel = mock_citadel
        worker._get_citadel_context = MagicMock(return_value=None)

        template = DocumentationTemplate()
        result = await worker._apply_citadel_enrichment(
            template, sample_pm_ticket, FileType.COBOL
        )
        # Should still return template despite error
        assert result.flow_diagram is None
        assert result is template

    async def test_uses_metadata_file_path_when_available(
        self, mock_config, mock_beads_client, tmp_path
    ):
        """When ticket metadata has file_path, it is used for Citadel."""
        from war_rig.models.templates import DocumentationTemplate

        custom_path = "/custom/path/PROG.cbl"
        ticket = ProgramManagerTicket(
            ticket_id="war_rig-meta1",
            ticket_type=TicketType.DOCUMENTATION,
            state=TicketState.CREATED,
            file_name="PROG.cbl",
            program_id="PROG",
            cycle_number=1,
            metadata={"file_path": custom_path},
        )

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
            output_directory=tmp_path / "output",
        )

        mock_citadel = MagicMock()
        mock_citadel.get_flow_diagram.return_value = "flowchart TD\n    X --> Y"
        worker._citadel = mock_citadel
        worker._get_citadel_context = MagicMock(return_value=None)

        template = DocumentationTemplate()
        await worker._apply_citadel_enrichment(template, ticket, FileType.COBOL)

        # Verify Citadel was called with the metadata path, not input_directory
        worker._get_citadel_context.assert_called_once_with(custom_path)
        mock_citadel.get_flow_diagram.assert_called_once_with(custom_path)


# =============================================================================
# Performance Fix Tests: Callers Lookup from Dependency Graph (war_rig-7831)
# =============================================================================


class TestBuildCallersLookup:
    """Tests for _build_callers_lookup - caching callers from dependency graph.

    This tests the fix for war_rig-7831: instead of calling get_callers() per
    paragraph (which triggers expensive directory scans), we build a lookup
    table from the dependency graph at worker initialization.
    """

    def test_builds_lookup_from_valid_graph(
        self, mock_config, mock_beads_client, tmp_path
    ):
        """Lookup is built correctly from a valid dependency graph."""
        # Create a minimal dependency graph
        graph = {
            "version": "1.0",
            "artifacts": {
                "paragraph::MAIN-PARA": {
                    "defined_in": {
                        "file_path": "/app/src/PROGRAM.cbl",
                    }
                },
                "paragraph::HELPER-PARA": {
                    "defined_in": {
                        "file_path": "/app/src/UTILS.cbl",
                    }
                },
                "paragraph::CALLER-PARA": {
                    "defined_in": {
                        "file_path": "/app/src/CALLER.cbl",
                    }
                },
            },
            "relationships": [
                {
                    "from_artifact": "paragraph::MAIN-PARA",
                    "to_artifact": "paragraph::HELPER-PARA",
                    "relationship_type": "performs",
                    "location": {
                        "file_path": "/app/src/PROGRAM.cbl",
                        "line_start": 100,
                    },
                },
                {
                    "from_artifact": "paragraph::CALLER-PARA",
                    "to_artifact": "paragraph::HELPER-PARA",
                    "relationship_type": "calls",
                    "location": {
                        "file_path": "/app/src/CALLER.cbl",
                        "line_start": 50,
                    },
                },
            ],
        }

        graph_path = tmp_path / "dependency_graph.json"
        graph_path.write_text(json.dumps(graph))

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
            output_directory=tmp_path / "output",
            dependency_graph_path=graph_path,
        )

        # Check lookup was built
        assert len(worker._callers_lookup) == 1

        # HELPER-PARA in UTILS.cbl should have 2 callers
        key = "/app/src/UTILS.cbl:HELPER-PARA"
        assert key in worker._callers_lookup
        callers = worker._callers_lookup[key]
        assert len(callers) == 2

        # Verify caller entries match expected format
        caller_files = {c["file"] for c in callers}
        assert "/app/src/PROGRAM.cbl" in caller_files
        assert "/app/src/CALLER.cbl" in caller_files

        caller_from_program = next(
            c for c in callers if c["file"] == "/app/src/PROGRAM.cbl"
        )
        assert caller_from_program["function"] == "MAIN-PARA"
        assert caller_from_program["line"] == 100
        assert caller_from_program["type"] == "performs"

    def test_returns_empty_lookup_when_graph_missing(
        self, mock_config, mock_beads_client, tmp_path
    ):
        """Lookup is empty when dependency graph does not exist."""
        graph_path = tmp_path / "nonexistent.json"

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
            output_directory=tmp_path / "output",
            dependency_graph_path=graph_path,
        )

        assert worker._callers_lookup == {}

    def test_returns_empty_lookup_when_graph_invalid_json(
        self, mock_config, mock_beads_client, tmp_path
    ):
        """Lookup is empty when dependency graph contains invalid JSON."""
        graph_path = tmp_path / "invalid.json"
        graph_path.write_text("not valid json {{{")

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
            output_directory=tmp_path / "output",
            dependency_graph_path=graph_path,
        )

        assert worker._callers_lookup == {}

    def test_filters_non_call_relationships(
        self, mock_config, mock_beads_client, tmp_path
    ):
        """Non-call relationship types are excluded from lookup."""
        graph = {
            "version": "1.0",
            "artifacts": {
                "paragraph::PARA1": {"defined_in": {"file_path": "/app/src/FILE1.cbl"}},
                "data::FIELD1": {"defined_in": {"file_path": "/app/src/FILE1.cbl"}},
            },
            "relationships": [
                {
                    "from_artifact": "paragraph::PARA1",
                    "to_artifact": "data::FIELD1",
                    "relationship_type": "reads",  # data dependency, not call
                    "location": {
                        "file_path": "/app/src/FILE1.cbl",
                        "line_start": 10,
                    },
                },
            ],
        }

        graph_path = tmp_path / "dependency_graph.json"
        graph_path.write_text(json.dumps(graph))

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
            output_directory=tmp_path / "output",
            dependency_graph_path=graph_path,
        )

        # No call relationships, so lookup should be empty
        assert worker._callers_lookup == {}

    @pytest.mark.asyncio
    async def test_enrich_uses_cached_lookup(
        self, mock_config, mock_beads_client, tmp_path
    ):
        """_enrich_paragraphs_with_citadel uses cached lookup, not get_callers."""
        from war_rig.models.templates import DocumentationTemplate, Paragraph

        graph = {
            "version": "1.0",
            "artifacts": {
                "paragraph::DO-SOMETHING": {
                    "defined_in": {"file_path": "/app/src/MAIN.cbl"}
                },
                "paragraph::CALLER-1": {
                    "defined_in": {"file_path": "/app/src/OTHER.cbl"}
                },
            },
            "relationships": [
                {
                    "from_artifact": "paragraph::CALLER-1",
                    "to_artifact": "paragraph::DO-SOMETHING",
                    "relationship_type": "performs",
                    "location": {
                        "file_path": "/app/src/OTHER.cbl",
                        "line_start": 200,
                    },
                },
            ],
        }

        graph_path = tmp_path / "dependency_graph.json"
        graph_path.write_text(json.dumps(graph))

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
            output_directory=tmp_path / "output",
            dependency_graph_path=graph_path,
        )

        # Mock Citadel but DO NOT mock get_callers - it should not be called
        mock_citadel = MagicMock()
        mock_citadel.get_function_body.return_value = None
        worker._citadel = mock_citadel

        template = DocumentationTemplate(
            paragraphs=[Paragraph(paragraph_name="DO-SOMETHING")]
        )

        citadel_context = {"functions": [], "includes": [], "dead_code": []}

        result = await worker._enrich_paragraphs_with_citadel(
            template, "/app/src/MAIN.cbl", citadel_context
        )

        # Verify incoming_calls was populated from the cached lookup
        assert len(result.paragraphs) == 1
        para = result.paragraphs[0]
        assert len(para.incoming_calls) == 1
        assert para.incoming_calls[0].file == "/app/src/OTHER.cbl"
        assert para.incoming_calls[0].function == "CALLER-1"
        assert para.incoming_calls[0].line == 200
        assert para.incoming_calls[0].call_type == "performs"

        # Verify get_callers was NOT called (the whole point of this fix)
        mock_citadel.get_callers.assert_not_called()


# =============================================================================
# Bug Fix Tests: Paragraph Stub Population from Citadel
# =============================================================================


class TestCitadelParagraphStubPopulation:
    """Tests for Bug 1 fix: populate paragraph stubs from Citadel when LLM
    produces empty paragraphs (e.g., during chunked processing)."""

    @pytest.mark.asyncio
    async def test_empty_paragraphs_populated_from_citadel(
        self, mock_config, mock_beads_client, tmp_path
    ):
        """When template has no paragraphs but Citadel found functions,
        paragraph stubs should be created from Citadel data."""
        from war_rig.models.templates import DocumentationTemplate

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
            output_directory=tmp_path / "output",
        )
        mock_citadel = MagicMock()
        mock_citadel.get_callers.return_value = []
        mock_citadel.get_function_body.return_value = None
        worker._citadel = mock_citadel

        template = DocumentationTemplate()
        assert template.paragraphs == []

        citadel_context = {
            "functions": [
                {"name": "0000-MAIN", "type": "paragraph", "line": 100, "calls": []},
                {"name": "1000-INIT", "type": "paragraph", "line": 200, "calls": []},
                {
                    "name": "2000-PROCESS",
                    "type": "paragraph",
                    "line": 300,
                    "calls": [
                        {"target": "3000-OUTPUT", "type": "performs", "line": 320},
                    ],
                },
            ],
        }

        result = await worker._enrich_paragraphs_with_citadel(
            template, "/fake/PROG.cbl", citadel_context
        )

        assert len(result.paragraphs) == 3
        names = [p.paragraph_name for p in result.paragraphs]
        assert "0000-MAIN" in names
        assert "1000-INIT" in names
        assert "2000-PROCESS" in names

        # Verify stubs have citation from Citadel line info
        main_para = next(
            p for p in result.paragraphs if p.paragraph_name == "0000-MAIN"
        )
        assert main_para.citation == (100, 100)
        assert "[Citadel]" in main_para.purpose

    @pytest.mark.asyncio
    async def test_existing_paragraphs_not_overwritten(
        self, mock_config, mock_beads_client, tmp_path
    ):
        """When template already has paragraphs, Citadel should NOT replace them."""
        from war_rig.models.templates import DocumentationTemplate, Paragraph

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
            output_directory=tmp_path / "output",
        )
        mock_citadel = MagicMock()
        mock_citadel.get_callers.return_value = []
        mock_citadel.get_function_body.return_value = None
        worker._citadel = mock_citadel

        template = DocumentationTemplate(
            paragraphs=[
                Paragraph(paragraph_name="0000-MAIN", purpose="Main control flow"),
            ]
        )

        citadel_context = {
            "functions": [
                {"name": "0000-MAIN", "type": "paragraph", "line": 100, "calls": []},
                {"name": "1000-INIT", "type": "paragraph", "line": 200, "calls": []},
            ],
        }

        result = await worker._enrich_paragraphs_with_citadel(
            template, "/fake/PROG.cbl", citadel_context
        )

        # Should keep existing paragraph, not add stubs
        assert len(result.paragraphs) == 1
        assert result.paragraphs[0].paragraph_name == "0000-MAIN"
        assert result.paragraphs[0].purpose == "Main control flow"

    @pytest.mark.asyncio
    async def test_empty_paragraphs_no_citadel_functions(
        self, mock_config, mock_beads_client, tmp_path
    ):
        """When template has no paragraphs and Citadel has no functions,
        paragraphs should remain empty."""
        from war_rig.models.templates import DocumentationTemplate

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
            output_directory=tmp_path / "output",
        )
        mock_citadel = MagicMock()
        worker._citadel = mock_citadel

        template = DocumentationTemplate()
        citadel_context = {"functions": []}

        result = await worker._enrich_paragraphs_with_citadel(
            template, "/fake/PROG.cbl", citadel_context
        )

        assert len(result.paragraphs) == 0

    @pytest.mark.asyncio
    async def test_no_citadel_returns_template_unchanged(
        self, mock_config, mock_beads_client, tmp_path
    ):
        """When Citadel is not available, template is returned unchanged."""
        from war_rig.models.templates import DocumentationTemplate

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
            output_directory=tmp_path / "output",
        )
        worker._citadel = None  # No Citadel

        template = DocumentationTemplate()
        citadel_context = {
            "functions": [
                {"name": "0000-MAIN", "type": "paragraph", "line": 100, "calls": []},
            ],
        }

        result = await worker._enrich_paragraphs_with_citadel(
            template, "/fake/PROG.cbl", citadel_context
        )

        assert len(result.paragraphs) == 0

    @pytest.mark.asyncio
    async def test_stubs_get_outgoing_calls_enrichment(
        self, mock_config, mock_beads_client, tmp_path
    ):
        """Citadel-populated stubs should also receive outgoing call enrichment."""
        from war_rig.models.templates import DocumentationTemplate

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
            output_directory=tmp_path / "output",
        )
        mock_citadel = MagicMock()
        mock_citadel.get_callers.return_value = []
        mock_citadel.get_function_body.return_value = None
        worker._citadel = mock_citadel

        template = DocumentationTemplate()
        citadel_context = {
            "functions": [
                {
                    "name": "2000-PROCESS",
                    "type": "paragraph",
                    "line": 300,
                    "calls": [
                        {"target": "3000-OUTPUT", "type": "performs", "line": 320},
                        {"target": "UTIL-PROG", "type": "calls", "line": 330},
                    ],
                },
            ],
        }

        result = await worker._enrich_paragraphs_with_citadel(
            template, "/fake/PROG.cbl", citadel_context
        )

        assert len(result.paragraphs) == 1
        para = result.paragraphs[0]
        assert para.paragraph_name == "2000-PROCESS"
        assert len(para.outgoing_calls) == 2
        assert para.outgoing_calls[0].target == "3000-OUTPUT"
        assert para.outgoing_calls[1].target == "UTIL-PROG"


# =============================================================================
# Bug Fix Tests: PERFORM THRU Dead Code False Positives
# =============================================================================


class TestPerformThruDeadCodeFilter:
    """Tests for Bug 2 fix: EXIT paragraphs used as PERFORM THRU range
    endpoints should not be flagged as dead code."""

    def test_find_perform_thru_targets_basic(
        self, mock_config, mock_beads_client, tmp_path
    ):
        """Test extraction of PERFORM THRU target paragraph names."""
        source = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALIZE THRU 1000-EXIT.
           PERFORM 2000-PROCESS THRU 2000-EXIT.
           STOP RUN.
       1000-INITIALIZE.
           DISPLAY 'INIT'.
       1000-EXIT.
           EXIT.
       2000-PROCESS.
           DISPLAY 'PROCESS'.
       2000-EXIT.
           EXIT.
"""
        source_file = tmp_path / "TESTPROG.cbl"
        source_file.write_text(source)

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
            output_directory=tmp_path / "output",
        )

        targets = worker._find_perform_thru_targets(str(source_file))

        assert "1000-EXIT" in targets
        assert "2000-EXIT" in targets
        assert "1000-INITIALIZE" not in targets
        assert "2000-PROCESS" not in targets

    def test_find_perform_thru_targets_through_keyword(
        self, mock_config, mock_beads_client, tmp_path
    ):
        """Test extraction with THROUGH keyword (synonym for THRU)."""
        source = """\
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INIT THROUGH 1000-EXIT.
"""
        source_file = tmp_path / "TESTPROG.cbl"
        source_file.write_text(source)

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
            output_directory=tmp_path / "output",
        )

        targets = worker._find_perform_thru_targets(str(source_file))

        assert "1000-EXIT" in targets

    def test_find_perform_thru_targets_case_insensitive(
        self, mock_config, mock_beads_client, tmp_path
    ):
        """Test that THRU pattern matching is case-insensitive."""
        source = """\
       PROCEDURE DIVISION.
           perform 1000-init thru 1000-exit.
"""
        source_file = tmp_path / "TESTPROG.cbl"
        source_file.write_text(source)

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
            output_directory=tmp_path / "output",
        )

        targets = worker._find_perform_thru_targets(str(source_file))

        # Should be uppercased in the result set
        assert "1000-EXIT" in targets

    def test_find_perform_thru_targets_nonexistent_file(
        self, mock_config, mock_beads_client, tmp_path
    ):
        """Test graceful handling of missing file."""
        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
            output_directory=tmp_path / "output",
        )

        targets = worker._find_perform_thru_targets("/nonexistent/file.cbl")

        assert targets == set()

    def test_find_perform_thru_targets_no_thru(
        self, mock_config, mock_beads_client, tmp_path
    ):
        """Test file with PERFORM but no THRU."""
        source = """\
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INIT.
           PERFORM 2000-PROCESS.
"""
        source_file = tmp_path / "TESTPROG.cbl"
        source_file.write_text(source)

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
            output_directory=tmp_path / "output",
        )

        targets = worker._find_perform_thru_targets(str(source_file))

        assert targets == set()

    @pytest.mark.asyncio
    async def test_dead_code_exit_paragraphs_filtered(
        self, mock_config, mock_beads_client, tmp_path
    ):
        """EXIT paragraphs in PERFORM THRU should not be marked as dead code."""
        from war_rig.models.templates import DocumentationTemplate, Paragraph

        # Create source with PERFORM THRU
        source = """\
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INIT THRU 1000-EXIT.
           PERFORM 2000-PROCESS THRU 2000-EXIT.
       1000-INIT.
           DISPLAY 'INIT'.
       1000-EXIT.
           EXIT.
       2000-PROCESS.
           DISPLAY 'PROCESS'.
       2000-EXIT.
           EXIT.
       9999-UNUSED.
           DISPLAY 'NEVER CALLED'.
"""
        source_file = tmp_path / "TESTPROG.cbl"
        source_file.write_text(source)

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
            output_directory=tmp_path / "output",
        )
        mock_citadel = MagicMock()
        mock_citadel.get_callers.return_value = []
        mock_citadel.get_function_body.return_value = None
        worker._citadel = mock_citadel

        template = DocumentationTemplate(
            paragraphs=[
                Paragraph(paragraph_name="0000-MAIN", purpose="Main entry"),
                Paragraph(paragraph_name="1000-INIT", purpose="Initialize"),
                Paragraph(paragraph_name="1000-EXIT", purpose="Exit init"),
                Paragraph(paragraph_name="2000-PROCESS", purpose="Process"),
                Paragraph(paragraph_name="2000-EXIT", purpose="Exit process"),
                Paragraph(paragraph_name="9999-UNUSED", purpose="Unused"),
            ]
        )

        citadel_context = {
            "functions": [
                {"name": "0000-MAIN", "type": "paragraph", "line": 3, "calls": []},
                {"name": "1000-INIT", "type": "paragraph", "line": 6, "calls": []},
                {"name": "1000-EXIT", "type": "paragraph", "line": 8, "calls": []},
                {"name": "2000-PROCESS", "type": "paragraph", "line": 10, "calls": []},
                {"name": "2000-EXIT", "type": "paragraph", "line": 12, "calls": []},
                {"name": "9999-UNUSED", "type": "paragraph", "line": 14, "calls": []},
            ],
            "dead_code": [
                {
                    "name": "1000-EXIT",
                    "type": "paragraph",
                    "line": 8,
                    "reason": "Paragraph '1000-EXIT' is never PERFORMed",
                },
                {
                    "name": "2000-EXIT",
                    "type": "paragraph",
                    "line": 12,
                    "reason": "Paragraph '2000-EXIT' is never PERFORMed",
                },
                {
                    "name": "9999-UNUSED",
                    "type": "paragraph",
                    "line": 14,
                    "reason": "Paragraph '9999-UNUSED' is never PERFORMed",
                },
            ],
        }

        result = await worker._enrich_paragraphs_with_citadel(
            template, str(source_file), citadel_context
        )

        # EXIT paragraphs should NOT be flagged as dead code
        exit_1000 = next(
            p for p in result.paragraphs if p.paragraph_name == "1000-EXIT"
        )
        assert exit_1000.is_dead_code is False
        assert exit_1000.dead_code_reason is None

        exit_2000 = next(
            p for p in result.paragraphs if p.paragraph_name == "2000-EXIT"
        )
        assert exit_2000.is_dead_code is False
        assert exit_2000.dead_code_reason is None

        # Truly unused paragraph SHOULD still be flagged
        unused = next(p for p in result.paragraphs if p.paragraph_name == "9999-UNUSED")
        assert unused.is_dead_code is True
        assert unused.dead_code_reason is not None

        # Dead code section should only contain 9999-UNUSED
        assert len(result.dead_code) == 1
        assert result.dead_code[0].name == "9999-UNUSED"

    @pytest.mark.asyncio
    async def test_dead_code_no_thru_keeps_all_dead_items(
        self, mock_config, mock_beads_client, tmp_path
    ):
        """When no PERFORM THRU in source, all dead code items should remain."""
        from war_rig.models.templates import DocumentationTemplate, Paragraph

        source = """\
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INIT.
       1000-INIT.
           DISPLAY 'INIT'.
       9999-UNUSED.
           EXIT.
"""
        source_file = tmp_path / "TESTPROG.cbl"
        source_file.write_text(source)

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
            output_directory=tmp_path / "output",
        )
        mock_citadel = MagicMock()
        mock_citadel.get_callers.return_value = []
        mock_citadel.get_function_body.return_value = None
        worker._citadel = mock_citadel

        template = DocumentationTemplate(
            paragraphs=[
                Paragraph(paragraph_name="0000-MAIN", purpose="Main"),
                Paragraph(paragraph_name="1000-INIT", purpose="Init"),
                Paragraph(paragraph_name="9999-UNUSED", purpose="Unused"),
            ]
        )

        citadel_context = {
            "functions": [
                {"name": "0000-MAIN", "type": "paragraph", "line": 3, "calls": []},
                {"name": "1000-INIT", "type": "paragraph", "line": 5, "calls": []},
                {"name": "9999-UNUSED", "type": "paragraph", "line": 7, "calls": []},
            ],
            "dead_code": [
                {
                    "name": "9999-UNUSED",
                    "type": "paragraph",
                    "line": 7,
                    "reason": "Paragraph '9999-UNUSED' is never PERFORMed",
                },
            ],
        }

        result = await worker._enrich_paragraphs_with_citadel(
            template, str(source_file), citadel_context
        )

        # 9999-UNUSED should still be dead code
        unused = next(p for p in result.paragraphs if p.paragraph_name == "9999-UNUSED")
        assert unused.is_dead_code is True

        assert len(result.dead_code) == 1
        assert result.dead_code[0].name == "9999-UNUSED"

    @pytest.mark.asyncio
    async def test_dead_code_missing_source_file_still_works(
        self, mock_config, mock_beads_client, tmp_path
    ):
        """When source file is missing, dead code items are kept (no crash)."""
        from war_rig.models.templates import DocumentationTemplate, Paragraph

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
            output_directory=tmp_path / "output",
        )
        mock_citadel = MagicMock()
        mock_citadel.get_callers.return_value = []
        mock_citadel.get_function_body.return_value = None
        worker._citadel = mock_citadel

        template = DocumentationTemplate(
            paragraphs=[
                Paragraph(paragraph_name="1000-EXIT", purpose="Exit marker"),
            ]
        )

        citadel_context = {
            "functions": [
                {"name": "1000-EXIT", "type": "paragraph", "line": 8, "calls": []},
            ],
            "dead_code": [
                {
                    "name": "1000-EXIT",
                    "type": "paragraph",
                    "line": 8,
                    "reason": "Paragraph '1000-EXIT' is never PERFORMed",
                },
            ],
        }

        # Source file doesn't exist - _find_perform_thru_targets returns empty set
        result = await worker._enrich_paragraphs_with_citadel(
            template, "/nonexistent/PROG.cbl", citadel_context
        )

        # Should still mark as dead code since we can't verify THRU usage
        exit_para = result.paragraphs[0]
        assert exit_para.is_dead_code is True
        assert len(result.dead_code) == 1


# =============================================================================
# Citadel-Guided Hybrid Processing Tests (D4)
# =============================================================================


class TestCitadelGuidedEligibility:
    """Tests for _is_citadel_guided_eligible."""

    def test_eligible_cobol_with_citadel(
        self, mock_config, mock_beads_client, tmp_path
    ):
        """COBOL files with Citadel available should be eligible."""
        worker = ScribeWorker(
            worker_id="scribe-test",
            config=mock_config,
            beads_client=mock_beads_client,
            input_directory=tmp_path,
            output_directory=tmp_path / "output",
            exit_on_error=False,
        )
        # Mock Citadel
        worker._citadel = MagicMock()

        assert worker._is_citadel_guided_eligible(FileType.COBOL, "source") is True

    def test_not_eligible_without_citadel(
        self, mock_config, mock_beads_client, tmp_path
    ):
        """Without Citadel, no file should be eligible."""
        worker = ScribeWorker(
            worker_id="scribe-test",
            config=mock_config,
            beads_client=mock_beads_client,
            input_directory=tmp_path,
            output_directory=tmp_path / "output",
            exit_on_error=False,
        )

        assert worker._citadel is None
        assert worker._is_citadel_guided_eligible(FileType.COBOL, "source") is False

    def test_not_eligible_non_cobol(self, mock_config, mock_beads_client, tmp_path):
        """Non-COBOL files should not be eligible even with Citadel."""
        worker = ScribeWorker(
            worker_id="scribe-test",
            config=mock_config,
            beads_client=mock_beads_client,
            input_directory=tmp_path,
            output_directory=tmp_path / "output",
            exit_on_error=False,
        )
        worker._citadel = MagicMock()

        assert worker._is_citadel_guided_eligible(FileType.JCL, "source") is False
        assert worker._is_citadel_guided_eligible(FileType.PLI, "source") is False
        assert worker._is_citadel_guided_eligible(FileType.ASM, "source") is False


class TestCitadelGuidedDocumentation:
    """Tests for _process_citadel_guided_documentation."""

    @pytest.fixture
    def citadel_worker(self, mock_config, mock_beads_client, tmp_path):
        """Create a ScribeWorker with mocked Citadel."""
        # Add config fields needed for Citadel-guided processing
        mock_config.citadel_guided_threshold_lines = 2000
        mock_config.citadel_guided_threshold_paragraphs = 15
        # Use small batch size for testing batched processing
        mock_config.scribe.citadel_max_paragraphs_per_batch = 5

        worker = ScribeWorker(
            worker_id="scribe-test",
            config=mock_config,
            beads_client=mock_beads_client,
            input_directory=tmp_path,
            output_directory=tmp_path / "output",
            exit_on_error=False,
        )

        # Mock Citadel SDK
        mock_citadel = MagicMock()
        mock_citadel.get_file_stats.return_value = {
            "total_lines": 100,
            "paragraph_count": 3,
            "language": "cobol",
            "paragraphs": [
                {
                    "name": "0000-MAIN",
                    "line_start": 10,
                    "line_end": 20,
                    "line_count": 11,
                },
                {
                    "name": "1000-PROCESS",
                    "line_start": 22,
                    "line_end": 40,
                    "line_count": 19,
                },
                {
                    "name": "9999-EXIT",
                    "line_start": 42,
                    "line_end": 45,
                    "line_count": 4,
                },
            ],
        }
        mock_citadel.get_functions.return_value = [
            {
                "name": "0000-MAIN",
                "type": "paragraph",
                "line": 10,
                "line_end": 20,
                "calls": [
                    {"target": "1000-PROCESS", "type": "performs", "line": 15},
                ],
            },
            {
                "name": "1000-PROCESS",
                "type": "paragraph",
                "line": 22,
                "line_end": 40,
                "calls": [],
            },
            {
                "name": "9999-EXIT",
                "type": "paragraph",
                "line": 42,
                "line_end": 45,
                "calls": [],
            },
        ]
        mock_citadel.get_includes.return_value = []
        mock_citadel.get_function_bodies.return_value = {
            "0000-MAIN": "       0000-MAIN.\n           PERFORM 1000-PROCESS.\n           STOP RUN.",
            "1000-PROCESS": "       1000-PROCESS.\n           DISPLAY 'PROCESSING'.",
            "9999-EXIT": "       9999-EXIT.\n           EXIT.",
        }
        mock_citadel.get_callers.return_value = []
        mock_citadel.get_flow_diagram.return_value = None
        mock_citadel.get_dead_code.return_value = []
        worker._citadel = mock_citadel

        return worker

    async def test_small_file_single_pass(self, citadel_worker, sample_pm_ticket):
        """Test small file gets single-pass processing with outline."""
        worker = citadel_worker

        # Mock scribe agent to capture input
        from war_rig.models.templates import DocumentationTemplate, Paragraph

        captured_inputs = []

        async def mock_ainvoke(input_data):
            captured_inputs.append(input_data)
            template = DocumentationTemplate()
            template.paragraphs = [
                Paragraph(paragraph_name="0000-MAIN", purpose="Main entry"),
                Paragraph(paragraph_name="1000-PROCESS", purpose="Processing"),
                Paragraph(paragraph_name="9999-EXIT", purpose="Exit point"),
            ]
            return ScribeOutput(success=True, template=template)

        worker._scribe_agent = MagicMock()
        worker._scribe_agent.ainvoke = mock_ainvoke

        result = await worker._process_citadel_guided_documentation(
            sample_pm_ticket,
            "source code",
            FileType.COBOL,
        )

        assert result.success is True
        assert result.template is not None
        # Should have been called (single pass)
        assert len(captured_inputs) >= 1
        # First call should include citadel_outline
        assert captured_inputs[0].citadel_outline is not None
        assert len(captured_inputs[0].citadel_outline) == 3

    async def test_large_file_batched(self, citadel_worker, sample_pm_ticket):
        """Test large file uses ticket-dispatch: processes batch 0 and creates continuation."""
        worker = citadel_worker

        # Make file appear large
        worker._citadel.get_file_stats.return_value = {
            "total_lines": 3000,
            "paragraph_count": 20,
            "language": "cobol",
            "paragraphs": [
                {
                    "name": f"PARA-{i}",
                    "line_start": i * 100,
                    "line_end": (i + 1) * 100 - 1,
                    "line_count": 100,
                }
                for i in range(20)
            ],
        }
        worker._citadel.get_functions.return_value = [
            {
                "name": f"PARA-{i}",
                "type": "paragraph",
                "line": i * 100,
                "line_end": (i + 1) * 100 - 1,
                "calls": [],
            }
            for i in range(20)
        ]
        worker._citadel.get_function_bodies.return_value = {
            f"PARA-{i}": f"       PARA-{i}.\n           DISPLAY 'PARA {i}'."
            for i in range(20)
        }

        from war_rig.models.templates import DocumentationTemplate, Paragraph

        call_count = 0

        async def mock_ainvoke(input_data):
            nonlocal call_count
            call_count += 1
            template = DocumentationTemplate()
            if input_data.citadel_outline:
                template.paragraphs = [
                    Paragraph(paragraph_name=p["name"], purpose=f"Purpose {p['name']}")
                    for p in input_data.citadel_outline
                ]
            return ScribeOutput(success=True, template=template)

        worker._scribe_agent = MagicMock()
        worker._scribe_agent.ainvoke = mock_ainvoke

        result = await worker._process_citadel_guided_documentation(
            sample_pm_ticket,
            "source code",
            FileType.COBOL,
        )

        # Ticket-dispatch mode: batch 0 processed, rest via continuation tickets
        assert result.success is True
        # Template is None — the synthesis ticket will produce the final template
        assert result.template is None
        # Only batch 0 was processed inline
        assert call_count == 1
        # A CHUNK_CONTINUATION ticket was created for batch 1
        worker.beads_client.create_pm_ticket.assert_called_once()
        create_call = worker.beads_client.create_pm_ticket.call_args
        assert create_call.kwargs["ticket_type"] == TicketType.CHUNK_CONTINUATION
        assert create_call.kwargs["metadata"]["chunk_batch_idx"] == 1

    async def test_large_file_batched_loop(self, citadel_worker, sample_pm_ticket):
        """Test large file with ticket_dispatch=False uses synchronous loop."""
        worker = citadel_worker

        # Make file appear large
        worker._citadel.get_file_stats.return_value = {
            "total_lines": 3000,
            "paragraph_count": 20,
            "language": "cobol",
            "paragraphs": [
                {
                    "name": f"PARA-{i}",
                    "line_start": i * 100,
                    "line_end": (i + 1) * 100 - 1,
                    "line_count": 100,
                }
                for i in range(20)
            ],
        }
        worker._citadel.get_functions.return_value = [
            {
                "name": f"PARA-{i}",
                "type": "paragraph",
                "line": i * 100,
                "line_end": (i + 1) * 100 - 1,
                "calls": [],
            }
            for i in range(20)
        ]
        worker._citadel.get_function_bodies.return_value = {
            f"PARA-{i}": f"       PARA-{i}.\n           DISPLAY 'PARA {i}'."
            for i in range(20)
        }

        from war_rig.models.templates import DocumentationTemplate, Paragraph

        call_count = 0

        async def mock_ainvoke(input_data):
            nonlocal call_count
            call_count += 1
            template = DocumentationTemplate()
            if input_data.citadel_outline:
                template.paragraphs = [
                    Paragraph(paragraph_name=p["name"], purpose=f"Purpose {p['name']}")
                    for p in input_data.citadel_outline
                ]
            return ScribeOutput(success=True, template=template)

        worker._scribe_agent = MagicMock()
        worker._scribe_agent.ainvoke = mock_ainvoke

        # Call _process_citadel_batched directly with ticket_dispatch=False
        outline = worker._citadel.get_file_stats.return_value["paragraphs"]
        result = await worker._process_citadel_batched(
            sample_pm_ticket,
            "source code",
            FileType.COBOL,
            formatting_strict=False,
            outline=outline,
            source_path=worker.input_directory / "TESTPROG.cbl",
            stats=worker._citadel.get_file_stats.return_value,
            ticket_dispatch=False,
        )

        assert result.success is True
        assert result.template is not None
        # Should have been called multiple times (batches of 5 for 20 paragraphs)
        assert call_count >= 4  # 20 / 5 = 4 batches

    async def test_gap_fill(self, citadel_worker, sample_pm_ticket):
        """Test gap-fill catches missing paragraphs."""
        worker = citadel_worker

        from war_rig.models.templates import DocumentationTemplate, Paragraph

        call_count = 0

        async def mock_ainvoke(input_data):
            nonlocal call_count
            call_count += 1
            template = DocumentationTemplate()
            if call_count == 1:
                # First call: only return 2 of 3 paragraphs
                template.paragraphs = [
                    Paragraph(paragraph_name="0000-MAIN", purpose="Main entry"),
                    Paragraph(paragraph_name="1000-PROCESS", purpose="Processing"),
                ]
            else:
                # Gap-fill call: return the missing paragraph
                template.paragraphs = [
                    Paragraph(paragraph_name="9999-EXIT", purpose="Exit point"),
                ]
            return ScribeOutput(success=True, template=template)

        worker._scribe_agent = MagicMock()
        worker._scribe_agent.ainvoke = mock_ainvoke

        result = await worker._process_citadel_guided_documentation(
            sample_pm_ticket,
            "source code",
            FileType.COBOL,
        )

        assert result.success is True
        assert result.template is not None
        # All 3 paragraphs should be present after gap-fill
        para_names = {p.paragraph_name for p in result.template.paragraphs}
        assert "0000-MAIN" in para_names
        assert "1000-PROCESS" in para_names
        assert "9999-EXIT" in para_names

    async def test_fallback_on_citadel_failure(
        self, citadel_worker, sample_pm_ticket, tmp_path
    ):
        """Test fallback to standard processing when Citadel fails."""
        worker = citadel_worker

        # Make Citadel stats fail
        worker._citadel.get_file_stats.side_effect = Exception("Citadel error")

        # Write a source file for the standard path
        source_file = tmp_path / "TESTPROG.cbl"
        source_file.write_text(sample_pm_ticket.metadata["source_code"])

        from war_rig.models.templates import DocumentationTemplate

        async def mock_ainvoke(input_data):
            # No citadel_outline should be set in fallback path
            return ScribeOutput(
                success=True,
                template=DocumentationTemplate(),
            )

        worker._scribe_agent = MagicMock()
        worker._scribe_agent.ainvoke = mock_ainvoke

        result = await worker._process_citadel_guided_documentation(
            sample_pm_ticket,
            "source code",
            FileType.COBOL,
        )

        # Should have fallen back to standard processing
        assert result.success is True

    async def test_threshold_config(self, citadel_worker, sample_pm_ticket):
        """Test that threshold config controls small vs large path selection."""
        worker = citadel_worker

        # Set very low thresholds to force batched processing
        worker.config.citadel_guided_threshold_lines = 50
        worker.config.citadel_guided_threshold_paragraphs = 2

        from war_rig.models.templates import DocumentationTemplate, Paragraph

        call_count = 0

        async def mock_ainvoke(input_data):
            nonlocal call_count
            call_count += 1
            template = DocumentationTemplate()
            if input_data.citadel_outline:
                template.paragraphs = [
                    Paragraph(paragraph_name=p["name"], purpose=f"Purpose {p['name']}")
                    for p in input_data.citadel_outline
                ]
            return ScribeOutput(success=True, template=template)

        worker._scribe_agent = MagicMock()
        worker._scribe_agent.ainvoke = mock_ainvoke

        result = await worker._process_citadel_guided_documentation(
            sample_pm_ticket,
            "source code",
            FileType.COBOL,
        )

        assert result.success is True
        # With low thresholds (100 lines >= 50, 3 paragraphs >= 2),
        # should have used batched path (multiple calls)
        assert call_count >= 1


class TestTicketBasedChunking:
    """Tests for ticket-based chunking: batch plan, continuation, synthesis."""

    @pytest.fixture
    def chunk_worker(self, mock_config, mock_beads_client, tmp_path):
        """Create a ScribeWorker with mocked Citadel for chunking tests."""
        mock_config.citadel_guided_threshold_lines = 2000
        mock_config.citadel_guided_threshold_paragraphs = 15
        mock_config.scribe.citadel_max_paragraphs_per_batch = 5

        worker = ScribeWorker(
            worker_id="chunk-test",
            config=mock_config,
            beads_client=mock_beads_client,
            input_directory=tmp_path,
            output_directory=tmp_path / "output",
            exit_on_error=False,
        )

        mock_citadel = MagicMock()
        mock_citadel.get_function_bodies.return_value = {
            f"PARA-{i}": f"       PARA-{i}.\n           DISPLAY 'PARA {i}'."
            for i in range(10)
        }
        mock_citadel.get_callers.return_value = []
        mock_citadel.get_flow_diagram.return_value = None
        mock_citadel.get_dead_code.return_value = []
        worker._citadel = mock_citadel

        return worker

    @pytest.fixture
    def batch_plan(self, tmp_path):
        """Create a sample batch plan dict."""
        return {
            "file_name": "TESTPROG.cbl",
            "source_path": str(tmp_path / "TESTPROG.cbl"),
            "total_batches": 3,
            "batches": [
                [
                    {"name": "PARA-0", "line_start": 1, "line_end": 50, "line_count": 50},
                    {"name": "PARA-1", "line_start": 51, "line_end": 100, "line_count": 50},
                ],
                [
                    {"name": "PARA-2", "line_start": 101, "line_end": 150, "line_count": 50},
                    {"name": "PARA-3", "line_start": 151, "line_end": 200, "line_count": 50},
                ],
                [
                    {"name": "PARA-4", "line_start": 201, "line_end": 250, "line_count": 50},
                ],
            ],
            "outline": [
                {"name": f"PARA-{i}", "line_start": i * 50 + 1, "line_end": (i + 1) * 50}
                for i in range(5)
            ],
            "stats": {"total_lines": 250, "paragraph_count": 5},
            "parent_ticket_id": "war_rig-parent",
            "cycle_number": 1,
            "program_id": "TESTPROG",
            "copybook_contents": {},
            "file_path": str(tmp_path / "TESTPROG.cbl"),
            "feedback_context": None,
        }

    def test_save_and_load_batch_plan(self, chunk_worker, sample_pm_ticket):
        """Test saving and loading a batch plan."""
        from pathlib import Path

        batches = [
            [{"name": "PARA-0"}],
            [{"name": "PARA-1"}],
        ]
        outline = [{"name": "PARA-0"}, {"name": "PARA-1"}]
        stats = {"total_lines": 100}
        source_path = Path("/tmp/TESTPROG.cbl")

        chunk_worker._save_batch_plan(
            "TESTPROG.cbl", batches, outline, stats, source_path, sample_pm_ticket,
        )

        plan = chunk_worker._load_batch_plan("TESTPROG.cbl")
        assert plan is not None
        assert plan["total_batches"] == 2
        assert len(plan["batches"]) == 2
        assert plan["file_name"] == "TESTPROG.cbl"
        assert plan["source_path"] == "/tmp/TESTPROG.cbl"
        assert plan["parent_ticket_id"] == sample_pm_ticket.ticket_id

    def test_load_batch_plan_missing(self, chunk_worker):
        """Test loading a non-existent batch plan returns None."""
        plan = chunk_worker._load_batch_plan("NONEXISTENT.cbl")
        assert plan is None

    def test_create_chunk_ticket_continuation(self, chunk_worker, sample_pm_ticket):
        """Test creating a CHUNK_CONTINUATION ticket."""
        mock_ticket = MagicMock()
        mock_ticket.ticket_id = "chunk-cont-1"
        chunk_worker.beads_client.create_pm_ticket.return_value = mock_ticket

        result = chunk_worker._create_chunk_ticket(
            sample_pm_ticket, TicketType.CHUNK_CONTINUATION, 2,
        )

        assert result is not None
        chunk_worker.beads_client.create_pm_ticket.assert_called_once()
        call_kwargs = chunk_worker.beads_client.create_pm_ticket.call_args.kwargs
        assert call_kwargs["ticket_type"] == TicketType.CHUNK_CONTINUATION
        assert call_kwargs["metadata"]["chunk_batch_idx"] == 2

    def test_create_chunk_ticket_synthesis(self, chunk_worker, sample_pm_ticket):
        """Test creating a CHUNK_SYNTHESIS ticket."""
        mock_ticket = MagicMock()
        mock_ticket.ticket_id = "chunk-synth-1"
        chunk_worker.beads_client.create_pm_ticket.return_value = mock_ticket

        result = chunk_worker._create_chunk_ticket(
            sample_pm_ticket, TicketType.CHUNK_SYNTHESIS, -1,
        )

        assert result is not None
        call_kwargs = chunk_worker.beads_client.create_pm_ticket.call_args.kwargs
        assert call_kwargs["ticket_type"] == TicketType.CHUNK_SYNTHESIS

    async def test_process_chunk_continuation_creates_next(
        self, chunk_worker, batch_plan, tmp_path,
    ):
        """Test continuation processes a batch and creates next ticket."""
        from war_rig.models.templates import DocumentationTemplate, Paragraph

        # Save the batch plan
        chunk_worker._save_batch_plan(
            "TESTPROG.cbl",
            batch_plan["batches"],
            batch_plan["outline"],
            batch_plan["stats"],
            tmp_path / "TESTPROG.cbl",
            ProgramManagerTicket(
                ticket_id="parent",
                ticket_type=TicketType.DOCUMENTATION,
                state=TicketState.COMPLETED,
                file_name="TESTPROG.cbl",
                program_id="TESTPROG",
            ),
        )

        # Write source file for line-range fallback
        (tmp_path / "TESTPROG.cbl").write_text(
            "\n".join(f"       LINE {i}" for i in range(300))
        )

        # Mock ScribeAgent
        async def mock_ainvoke(input_data):
            template = DocumentationTemplate()
            template.paragraphs = [
                Paragraph(paragraph_name=p["name"], purpose=f"Purpose {p['name']}")
                for p in input_data.citadel_outline
            ]
            return ScribeOutput(success=True, template=template)

        chunk_worker._scribe_agent = MagicMock()
        chunk_worker._scribe_agent.ainvoke = mock_ainvoke

        # Process batch 1 (middle batch, should create continuation for batch 2)
        ticket = ProgramManagerTicket(
            ticket_id="cont-1",
            ticket_type=TicketType.CHUNK_CONTINUATION,
            state=TicketState.IN_PROGRESS,
            file_name="TESTPROG.cbl",
            program_id="TESTPROG",
            metadata={"chunk_batch_idx": 1},
        )

        result = await chunk_worker._process_chunk_continuation(ticket)

        assert result.success is True
        assert result.template is None  # Synthesis produces the final template
        # Should create continuation for batch 2
        chunk_worker.beads_client.create_pm_ticket.assert_called_once()
        call_kwargs = chunk_worker.beads_client.create_pm_ticket.call_args.kwargs
        assert call_kwargs["ticket_type"] == TicketType.CHUNK_CONTINUATION
        assert call_kwargs["metadata"]["chunk_batch_idx"] == 2

    async def test_process_chunk_continuation_last_creates_synthesis(
        self, chunk_worker, batch_plan, tmp_path,
    ):
        """Test last continuation creates a synthesis ticket."""
        from war_rig.models.templates import DocumentationTemplate, Paragraph

        # Save the batch plan
        chunk_worker._save_batch_plan(
            "TESTPROG.cbl",
            batch_plan["batches"],
            batch_plan["outline"],
            batch_plan["stats"],
            tmp_path / "TESTPROG.cbl",
            ProgramManagerTicket(
                ticket_id="parent",
                ticket_type=TicketType.DOCUMENTATION,
                state=TicketState.COMPLETED,
                file_name="TESTPROG.cbl",
                program_id="TESTPROG",
            ),
        )

        (tmp_path / "TESTPROG.cbl").write_text(
            "\n".join(f"       LINE {i}" for i in range(300))
        )

        async def mock_ainvoke(input_data):
            template = DocumentationTemplate()
            template.paragraphs = [
                Paragraph(paragraph_name=p["name"], purpose=f"Purpose {p['name']}")
                for p in input_data.citadel_outline
            ]
            return ScribeOutput(success=True, template=template)

        chunk_worker._scribe_agent = MagicMock()
        chunk_worker._scribe_agent.ainvoke = mock_ainvoke

        # Process batch 2 (last batch, should create synthesis)
        ticket = ProgramManagerTicket(
            ticket_id="cont-2",
            ticket_type=TicketType.CHUNK_CONTINUATION,
            state=TicketState.IN_PROGRESS,
            file_name="TESTPROG.cbl",
            program_id="TESTPROG",
            metadata={"chunk_batch_idx": 2},
        )

        result = await chunk_worker._process_chunk_continuation(ticket)

        assert result.success is True
        chunk_worker.beads_client.create_pm_ticket.assert_called_once()
        call_kwargs = chunk_worker.beads_client.create_pm_ticket.call_args.kwargs
        assert call_kwargs["ticket_type"] == TicketType.CHUNK_SYNTHESIS

    async def test_process_chunk_continuation_no_plan(self, chunk_worker):
        """Test continuation fails gracefully when batch plan is missing."""
        ticket = ProgramManagerTicket(
            ticket_id="cont-orphan",
            ticket_type=TicketType.CHUNK_CONTINUATION,
            state=TicketState.IN_PROGRESS,
            file_name="NONEXISTENT.cbl",
            program_id="TESTPROG",
            metadata={"chunk_batch_idx": 0},
        )

        result = await chunk_worker._process_chunk_continuation(ticket)

        assert result.success is False
        assert "Batch plan not found" in result.error

    async def test_process_chunk_synthesis(
        self, chunk_worker, batch_plan, tmp_path,
    ):
        """Test synthesis merges all chunks into final template."""
        from war_rig.models.templates import (
            DocumentationTemplate,
            Paragraph,
            PurposeSection,
        )

        # Save the batch plan
        chunk_worker._save_batch_plan(
            "TESTPROG.cbl",
            batch_plan["batches"],
            batch_plan["outline"],
            batch_plan["stats"],
            tmp_path / "TESTPROG.cbl",
            ProgramManagerTicket(
                ticket_id="parent",
                ticket_type=TicketType.DOCUMENTATION,
                state=TicketState.COMPLETED,
                file_name="TESTPROG.cbl",
                program_id="TESTPROG",
            ),
        )

        (tmp_path / "TESTPROG.cbl").write_text(
            "\n".join(f"       LINE {i}" for i in range(300))
        )

        # Save chunks for all 3 batches
        t0 = DocumentationTemplate(
            purpose=PurposeSection(summary="Test program"),
            paragraphs=[
                Paragraph(paragraph_name="PARA-0", purpose="Purpose 0"),
                Paragraph(paragraph_name="PARA-1", purpose="Purpose 1"),
            ],
        )
        t1 = DocumentationTemplate(
            paragraphs=[
                Paragraph(paragraph_name="PARA-2", purpose="Purpose 2"),
                Paragraph(paragraph_name="PARA-3", purpose="Purpose 3"),
            ],
        )
        t2 = DocumentationTemplate(
            paragraphs=[
                Paragraph(paragraph_name="PARA-4", purpose="Purpose 4"),
            ],
        )
        chunk_worker._save_chunk("TESTPROG.cbl", 0, t0, 3)
        chunk_worker._save_chunk("TESTPROG.cbl", 1, t1, 3)
        chunk_worker._save_chunk("TESTPROG.cbl", 2, t2, 3)

        # Mock gap-fill to return unchanged
        async def mock_ainvoke(input_data):
            template = DocumentationTemplate()
            return ScribeOutput(success=True, template=template)

        chunk_worker._scribe_agent = MagicMock()
        chunk_worker._scribe_agent.ainvoke = mock_ainvoke

        ticket = ProgramManagerTicket(
            ticket_id="synth-1",
            ticket_type=TicketType.CHUNK_SYNTHESIS,
            state=TicketState.IN_PROGRESS,
            file_name="TESTPROG.cbl",
            program_id="TESTPROG",
            metadata={"chunk_batch_idx": -1},
        )

        result = await chunk_worker._process_chunk_synthesis(ticket)

        assert result.success is True
        assert result.template is not None
        para_names = {p.paragraph_name for p in result.template.paragraphs}
        assert len(para_names) == 5
        for i in range(5):
            assert f"PARA-{i}" in para_names

    async def test_process_chunk_synthesis_no_chunks(
        self, chunk_worker, batch_plan, tmp_path,
    ):
        """Test synthesis fails when no chunks exist."""
        chunk_worker._save_batch_plan(
            "TESTPROG.cbl",
            batch_plan["batches"],
            batch_plan["outline"],
            batch_plan["stats"],
            tmp_path / "TESTPROG.cbl",
            ProgramManagerTicket(
                ticket_id="parent",
                ticket_type=TicketType.DOCUMENTATION,
                state=TicketState.COMPLETED,
                file_name="TESTPROG.cbl",
                program_id="TESTPROG",
            ),
        )

        ticket = ProgramManagerTicket(
            ticket_id="synth-orphan",
            ticket_type=TicketType.CHUNK_SYNTHESIS,
            state=TicketState.IN_PROGRESS,
            file_name="TESTPROG.cbl",
            program_id="TESTPROG",
            metadata={"chunk_batch_idx": -1},
        )

        result = await chunk_worker._process_chunk_synthesis(ticket)

        assert result.success is False
        assert "No chunks found" in result.error


class TestPromptValidation:
    """Tests for _validated_invoke prompt validation loop."""

    @pytest.fixture
    def validation_worker(self, mock_config, mock_beads_client, tmp_path):
        """Create a ScribeWorker for validation tests."""
        mock_config.scribe.max_prompt_tokens = 5000

        worker = ScribeWorker(
            worker_id="validate-test",
            config=mock_config,
            beads_client=mock_beads_client,
            input_directory=tmp_path,
            output_directory=tmp_path / "output",
            exit_on_error=False,
        )
        return worker

    async def test_small_prompt_passes_through(self, validation_worker):
        """Test that a prompt under the limit passes without modification."""
        from war_rig.agents.scribe import ScribeInput

        scribe_input = ScribeInput(
            source_code="DISPLAY 'HELLO'.",
            file_name="SMALL.cbl",
            file_type=FileType.COBOL,
        )

        async def mock_ainvoke(input_data):
            return ScribeOutput(success=True)

        validation_worker._scribe_agent = MagicMock()
        validation_worker._scribe_agent.ainvoke = mock_ainvoke

        result = await validation_worker._validated_invoke(scribe_input)
        assert result.success is True

    async def test_large_outline_gets_reduced(self, validation_worker):
        """Test that an oversized outline is trimmed to fit."""
        from war_rig.agents.scribe import ScribeInput

        # Set very low limit to force reduction
        validation_worker.config.scribe.max_prompt_tokens = 4500

        outline = [
            {"name": f"PARA-{i}", "line_start": i * 100, "line_end": (i + 1) * 100}
            for i in range(20)
        ]

        # Large source code to push over limit
        scribe_input = ScribeInput(
            source_code="X" * 20000,
            file_name="BIG.cbl",
            file_type=FileType.COBOL,
            citadel_outline=outline,
        )

        invoked_input = None

        async def mock_ainvoke(input_data):
            nonlocal invoked_input
            invoked_input = input_data
            return ScribeOutput(success=True)

        validation_worker._scribe_agent = MagicMock()
        validation_worker._scribe_agent.ainvoke = mock_ainvoke

        result = await validation_worker._validated_invoke(scribe_input)

        assert result.success is True
        # Outline should have been reduced (some paragraphs removed)
        assert invoked_input is not None
        assert len(invoked_input.citadel_outline) < 20

    async def test_source_truncation_when_single_paragraph(self, validation_worker):
        """Test that source is truncated when single paragraph exceeds limit."""
        from war_rig.agents.scribe import ScribeInput

        validation_worker.config.scribe.max_prompt_tokens = 4500

        scribe_input = ScribeInput(
            source_code="X" * 50000,
            file_name="HUGE.cbl",
            file_type=FileType.COBOL,
            citadel_outline=[{"name": "ONLY-PARA"}],
        )

        invoked_input = None

        async def mock_ainvoke(input_data):
            nonlocal invoked_input
            invoked_input = input_data
            return ScribeOutput(success=True)

        validation_worker._scribe_agent = MagicMock()
        validation_worker._scribe_agent.ainvoke = mock_ainvoke

        result = await validation_worker._validated_invoke(scribe_input)

        assert result.success is True
        assert invoked_input is not None
        # Source should have been truncated
        assert len(invoked_input.source_code) < 50000

    async def test_no_outline_source_truncation(self, validation_worker):
        """Test source truncation when no citadel outline present."""
        from war_rig.agents.scribe import ScribeInput

        validation_worker.config.scribe.max_prompt_tokens = 4500

        scribe_input = ScribeInput(
            source_code="X" * 50000,
            file_name="HUGE.cbl",
            file_type=FileType.COBOL,
        )

        invoked_input = None

        async def mock_ainvoke(input_data):
            nonlocal invoked_input
            invoked_input = input_data
            return ScribeOutput(success=True)

        validation_worker._scribe_agent = MagicMock()
        validation_worker._scribe_agent.ainvoke = mock_ainvoke

        result = await validation_worker._validated_invoke(scribe_input)

        assert result.success is True
        assert invoked_input is not None
        assert len(invoked_input.source_code) < 50000


class TestScribeOutlineInPrompt:
    """Test that ScribeAgent renders citadel_outline in prompt."""

    def test_outline_rendered_in_prompt(self):
        """Test that the citadel_outline is rendered in _build_user_prompt."""
        from war_rig.agents.scribe import ScribeAgent, ScribeInput
        from war_rig.config import APIConfig, ScribeConfig

        agent = ScribeAgent(
            config=ScribeConfig(model="test-model"),
            api_config=APIConfig(
                provider="mock",
                api_key="test",
            ),
        )

        input_data = ScribeInput(
            source_code="       0000-MAIN.\n           STOP RUN.",
            file_name="TEST.cbl",
            file_type=FileType.COBOL,
            citadel_outline=[
                {
                    "name": "0000-MAIN",
                    "line_start": 1,
                    "line_end": 5,
                    "calls": [
                        {"target": "1000-PROCESS", "type": "performs"},
                    ],
                },
                {"name": "1000-PROCESS", "line_start": 7, "line_end": 15, "calls": []},
            ],
        )

        prompt = agent._build_user_prompt(input_data)

        assert "Paragraph Outline (from static analysis)" in prompt
        assert "0000-MAIN" in prompt
        assert "1000-PROCESS" in prompt
        assert "lines 1-5" in prompt
        assert "You MUST document ALL listed paragraphs" in prompt

    def test_no_outline_without_field(self):
        """Test that no outline section is rendered without citadel_outline."""
        from war_rig.agents.scribe import ScribeAgent, ScribeInput
        from war_rig.config import APIConfig, ScribeConfig

        agent = ScribeAgent(
            config=ScribeConfig(model="test-model"),
            api_config=APIConfig(
                provider="mock",
                api_key="test",
            ),
        )

        input_data = ScribeInput(
            source_code="       0000-MAIN.\n           STOP RUN.",
            file_name="TEST.cbl",
            file_type=FileType.COBOL,
        )

        prompt = agent._build_user_prompt(input_data)

        assert "Paragraph Outline" not in prompt


# =============================================================================
# Shared MinionScribePool Tests
# =============================================================================


class TestSharedMinionScribePool:
    """Tests for shared MinionScribePool across ScribeWorkers."""

    def test_pool_creates_shared_minion_pool(self, mock_config, mock_beads_client):
        """Test that ScribeWorkerPool creates a shared MinionScribePool."""
        pool = ScribeWorkerPool(
            config=mock_config,
            beads_client=mock_beads_client,
        )

        # With enable_call_semantics=True, a shared minion pool should be created
        assert pool._minion_pool is not None

    def test_pool_no_minion_pool_when_disabled(self, mock_config, mock_beads_client):
        """Test that no minion pool is created when call_semantics is disabled."""
        mock_config.enable_call_semantics = False

        pool = ScribeWorkerPool(
            config=mock_config,
            beads_client=mock_beads_client,
        )

        assert pool._minion_pool is None

    @pytest.mark.asyncio
    async def test_workers_receive_shared_minion_pool(
        self, mock_config, mock_beads_client
    ):
        """Test that all workers receive the same shared MinionScribePool."""
        mock_beads_client.get_available_tickets.return_value = []

        pool = ScribeWorkerPool(
            config=mock_config,
            beads_client=mock_beads_client,
            num_workers=3,
            idle_timeout=0.1,
        )

        await pool.start()

        # All workers should have the same minion pool reference
        assert len(pool._workers) == 3
        for worker in pool._workers:
            assert worker._minion_scribe_pool is pool._minion_pool

        await pool.wait()
        await pool.stop()

    @pytest.mark.asyncio
    async def test_workers_no_minion_pool_when_disabled(
        self, mock_config, mock_beads_client
    ):
        """Test that workers have no minion pool when disabled."""
        mock_config.enable_call_semantics = False
        mock_beads_client.get_available_tickets.return_value = []

        pool = ScribeWorkerPool(
            config=mock_config,
            beads_client=mock_beads_client,
            num_workers=2,
            idle_timeout=0.1,
        )

        await pool.start()

        # Workers should have no minion pool
        for worker in pool._workers:
            assert worker._minion_scribe_pool is None

        await pool.wait()
        await pool.stop()

    def test_worker_accepts_minion_pool_parameter(self, mock_config, mock_beads_client):
        """Test that ScribeWorker accepts minion_pool parameter."""
        from war_rig.workers.minion_scribe_pool import MinionScribePool

        # Create a mock minion pool
        minion_pool = MagicMock(spec=MinionScribePool)

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
            minion_pool=minion_pool,
        )

        assert worker._minion_scribe_pool is minion_pool

    def test_worker_creates_own_pool_when_not_provided(
        self, mock_config, mock_beads_client
    ):
        """Test that ScribeWorker creates its own pool when not provided."""
        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
            # No minion_pool parameter
        )

        # With enable_call_semantics=True, worker creates its own pool
        assert worker._minion_scribe_pool is not None

    def test_worker_no_pool_when_disabled(self, mock_config, mock_beads_client):
        """Test that ScribeWorker has no pool when call_semantics is disabled."""
        mock_config.enable_call_semantics = False

        worker = ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

        assert worker._minion_scribe_pool is None

    def test_shared_minion_pool_has_beads_client(self, mock_config, mock_beads_client):
        """Test that the shared MinionScribePool has the BeadsClient."""
        pool = ScribeWorkerPool(
            config=mock_config,
            beads_client=mock_beads_client,
        )

        assert pool._minion_pool is not None
        assert pool._minion_pool.beads_client is mock_beads_client


# =============================================================================
# Template Merge Tests
# =============================================================================


class TestMergeClarificationTemplate:
    """Tests for _merge_clarification_template method.

    This method preserves paragraphs from the previous template that are not
    present in the new template (returned by the LLM). This prevents paragraph
    loss when source code is sampled for CLARIFICATION tickets.
    """

    @pytest.fixture
    def worker(self, mock_config, mock_beads_client):
        """Create a ScribeWorker for testing."""
        return ScribeWorker(
            worker_id="scribe-1",
            config=mock_config,
            beads_client=mock_beads_client,
        )

    def test_preserves_paragraphs_from_previous(self, worker):
        """Test that paragraphs from previous_template are preserved."""
        from war_rig.models.templates import DocumentationTemplate, Paragraph

        # Previous template has 5 paragraphs
        previous_template = DocumentationTemplate()
        previous_template.paragraphs = [
            Paragraph(paragraph_name="0000-MAIN", purpose="Entry point"),
            Paragraph(paragraph_name="1000-PROCESS", purpose="Process data"),
            Paragraph(paragraph_name="2000-VALIDATE", purpose="Validate input"),
            Paragraph(paragraph_name="3000-OUTPUT", purpose="Generate output"),
            Paragraph(paragraph_name="9999-EXIT", purpose="Exit paragraph"),
        ]

        # New template only has 2 paragraphs (LLM only saw sampled source)
        new_template = DocumentationTemplate()
        new_template.paragraphs = [
            Paragraph(paragraph_name="0000-MAIN", purpose="Updated entry point"),
            Paragraph(paragraph_name="1000-PROCESS", purpose="Updated process"),
        ]

        # Merge should add the 3 missing paragraphs
        merged = worker._merge_clarification_template(new_template, previous_template)

        # Should have all 5 paragraphs
        assert len(merged.paragraphs) == 5
        para_names = {p.paragraph_name for p in merged.paragraphs}
        assert para_names == {
            "0000-MAIN",
            "1000-PROCESS",
            "2000-VALIDATE",
            "3000-OUTPUT",
            "9999-EXIT",
        }

    def test_updated_paragraphs_take_precedence(self, worker):
        """Test that updated paragraphs from new_template take precedence."""
        from war_rig.models.templates import DocumentationTemplate, Paragraph

        previous_template = DocumentationTemplate()
        previous_template.paragraphs = [
            Paragraph(paragraph_name="0000-MAIN", purpose="Old purpose"),
        ]

        new_template = DocumentationTemplate()
        new_template.paragraphs = [
            Paragraph(paragraph_name="0000-MAIN", purpose="New purpose"),
        ]

        merged = worker._merge_clarification_template(new_template, previous_template)

        # Should have 1 paragraph with the new purpose
        assert len(merged.paragraphs) == 1
        assert merged.paragraphs[0].purpose == "New purpose"

    def test_case_insensitive_matching(self, worker):
        """Test that paragraph name matching is case-insensitive."""
        from war_rig.models.templates import DocumentationTemplate, Paragraph

        previous_template = DocumentationTemplate()
        previous_template.paragraphs = [
            Paragraph(paragraph_name="main-para", purpose="Lower case"),
        ]

        new_template = DocumentationTemplate()
        new_template.paragraphs = [
            Paragraph(paragraph_name="MAIN-PARA", purpose="Upper case"),
        ]

        merged = worker._merge_clarification_template(new_template, previous_template)

        # Should have 1 paragraph (not 2) due to case-insensitive matching
        assert len(merged.paragraphs) == 1
        assert merged.paragraphs[0].purpose == "Upper case"

    def test_empty_new_template_preserves_all(self, worker):
        """Test that empty new_template preserves all from previous."""
        from war_rig.models.templates import DocumentationTemplate, Paragraph

        previous_template = DocumentationTemplate()
        previous_template.paragraphs = [
            Paragraph(paragraph_name="PARA-1", purpose="First"),
            Paragraph(paragraph_name="PARA-2", purpose="Second"),
        ]

        new_template = DocumentationTemplate()
        new_template.paragraphs = []

        merged = worker._merge_clarification_template(new_template, previous_template)

        # Should have all 2 paragraphs from previous
        assert len(merged.paragraphs) == 2

    def test_identical_templates_no_change(self, worker):
        """Test that identical templates result in no additions."""
        from war_rig.models.templates import DocumentationTemplate, Paragraph

        previous_template = DocumentationTemplate()
        previous_template.paragraphs = [
            Paragraph(paragraph_name="PARA-1", purpose="First"),
        ]

        new_template = DocumentationTemplate()
        new_template.paragraphs = [
            Paragraph(paragraph_name="PARA-1", purpose="Updated first"),
        ]

        merged = worker._merge_clarification_template(new_template, previous_template)

        # Should have 1 paragraph
        assert len(merged.paragraphs) == 1
