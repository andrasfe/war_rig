"""Tests for the TicketOrchestrator.

These tests verify the ticket-based orchestration engine coordinates
all components correctly through the batch documentation workflow.
"""

from __future__ import annotations

import asyncio
from datetime import datetime
from pathlib import Path
from unittest.mock import AsyncMock, MagicMock, patch

import pytest

from war_rig.agents.imperator import (
    HolisticReviewOutput,
    ImperatorHolisticDecision,
)
from war_rig.beads import (
    BeadsClient,
    BeadsPriority,
    ProgramManagerTicket,
    TicketState,
    TicketType,
)
from war_rig.orchestration.ticket_engine import (
    BatchResult,
    OrchestrationStatus,
    TicketOrchestrator,
)


# =============================================================================
# Fixtures
# =============================================================================


@pytest.fixture
def mock_beads_client() -> MagicMock:
    """Create a mock BeadsClient for testing."""
    client = MagicMock(spec=BeadsClient)
    client.enabled = False
    client.dry_run = True

    # Default behavior: return empty lists
    client.get_available_tickets.return_value = []
    client.get_tickets_by_state.return_value = []
    client.create_pm_ticket.return_value = None
    client.claim_ticket.return_value = True
    client.update_ticket_state.return_value = True

    return client


@pytest.fixture
def mock_config(tmp_path: Path) -> MagicMock:
    """Create a mock WarRigConfig for testing."""
    config = MagicMock()
    config.beads_enabled = False
    config.beads_dry_run = True
    config.num_scribes = 2
    config.num_challengers = 1
    config.pm_max_cycles = 3
    config.max_iterations = 2
    config.max_questions_per_round = 3
    config.rig_id = "TEST_RIG"
    config.output_directory = tmp_path / "output"
    config.output_directory.mkdir(exist_ok=True)

    # Mock agent configs
    config.imperator = MagicMock()
    config.imperator.model = "test-model"
    config.imperator.temperature = 0.1

    config.api = MagicMock()
    config.api.provider = "mock"

    return config


@pytest.fixture
def sample_pm_tickets() -> list[ProgramManagerTicket]:
    """Create sample ProgramManagerTickets for testing."""
    return [
        ProgramManagerTicket(
            ticket_id="war_rig-test-001",
            ticket_type=TicketType.DOCUMENTATION,
            state=TicketState.COMPLETED,
            file_name="PROGRAM1.cbl",
            program_id="PROGRAM1",
            cycle_number=1,
        ),
        ProgramManagerTicket(
            ticket_id="war_rig-test-002",
            ticket_type=TicketType.DOCUMENTATION,
            state=TicketState.COMPLETED,
            file_name="PROGRAM2.cbl",
            program_id="PROGRAM2",
            cycle_number=1,
        ),
    ]


@pytest.fixture
def input_dir_with_files(tmp_path: Path) -> Path:
    """Create a temporary input directory with sample COBOL files."""
    input_dir = tmp_path / "input"
    input_dir.mkdir()

    # Create sample COBOL file
    (input_dir / "TESTPROG.cbl").write_text("""       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       PROCEDURE DIVISION.
           DISPLAY 'HELLO WORLD'.
           STOP RUN.
""")

    return input_dir


# =============================================================================
# BatchResult Tests
# =============================================================================


class TestBatchResult:
    """Tests for BatchResult dataclass."""

    def test_batch_result_defaults(self) -> None:
        """Test BatchResult initializes with empty defaults."""
        result = BatchResult()

        assert result.completed_files == []
        assert result.failed_files == []
        assert result.total_cycles == 0
        assert result.final_decision == ""
        assert result.documentation_outputs == {}
        assert result.started_at is None
        assert result.completed_at is None

    def test_batch_result_success_satisfied(self) -> None:
        """Test success property returns True for SATISFIED."""
        result = BatchResult(final_decision="SATISFIED")
        assert result.success is True

    def test_batch_result_success_forced(self) -> None:
        """Test success property returns True for FORCED_COMPLETE."""
        result = BatchResult(final_decision="FORCED_COMPLETE")
        assert result.success is True

    def test_batch_result_success_needs_clarification(self) -> None:
        """Test success property returns False for NEEDS_CLARIFICATION."""
        result = BatchResult(final_decision="NEEDS_CLARIFICATION")
        assert result.success is False

    def test_batch_result_duration(self) -> None:
        """Test duration_seconds calculation."""
        result = BatchResult(
            started_at=datetime(2024, 1, 1, 12, 0, 0),
            completed_at=datetime(2024, 1, 1, 12, 5, 30),
        )
        assert result.duration_seconds == 330.0  # 5 min 30 sec

    def test_batch_result_duration_not_started(self) -> None:
        """Test duration_seconds returns 0 when not started."""
        result = BatchResult()
        assert result.duration_seconds == 0.0

    def test_batch_result_to_dict(self) -> None:
        """Test to_dict produces serializable output."""
        result = BatchResult(
            completed_files=["FILE1.cbl", "FILE2.cbl"],
            failed_files=["FILE3.cbl"],
            total_cycles=2,
            final_decision="SATISFIED",
            started_at=datetime(2024, 1, 1, 12, 0, 0),
            completed_at=datetime(2024, 1, 1, 12, 5, 0),
        )

        d = result.to_dict()

        assert d["completed_files"] == ["FILE1.cbl", "FILE2.cbl"]
        assert d["failed_files"] == ["FILE3.cbl"]
        assert d["total_cycles"] == 2
        assert d["final_decision"] == "SATISFIED"
        assert d["success"] is True
        assert d["duration_seconds"] == 300.0


# =============================================================================
# TicketOrchestrator Initialization Tests
# =============================================================================


class TestTicketOrchestratorInit:
    """Tests for TicketOrchestrator initialization."""

    def test_init_with_defaults(self, mock_config: MagicMock) -> None:
        """Test initialization with default parameters."""
        with patch(
            "war_rig.orchestration.ticket_engine.load_config",
            return_value=mock_config,
        ):
            with patch(
                "war_rig.orchestration.ticket_engine.get_beads_client"
            ) as mock_get_client:
                mock_get_client.return_value = MagicMock(spec=BeadsClient)

                orchestrator = TicketOrchestrator()

                assert orchestrator.config == mock_config
                assert orchestrator.use_mock is False

    def test_init_with_custom_config(
        self,
        mock_config: MagicMock,
        mock_beads_client: MagicMock,
    ) -> None:
        """Test initialization with custom config and beads client."""
        orchestrator = TicketOrchestrator(
            config=mock_config,
            beads_client=mock_beads_client,
            use_mock=True,
        )

        assert orchestrator.config == mock_config
        assert orchestrator.beads_client == mock_beads_client
        assert orchestrator.use_mock is True

    def test_initial_state(
        self,
        mock_config: MagicMock,
        mock_beads_client: MagicMock,
    ) -> None:
        """Test initial orchestration state."""
        orchestrator = TicketOrchestrator(
            config=mock_config,
            beads_client=mock_beads_client,
        )

        assert orchestrator.state.status == OrchestrationStatus.IDLE
        assert orchestrator.state.cycle == 0
        assert orchestrator.state.batch_id == ""


# =============================================================================
# TicketOrchestrator Status Tests
# =============================================================================


class TestTicketOrchestratorStatus:
    """Tests for get_status method."""

    def test_get_status_idle(
        self,
        mock_config: MagicMock,
        mock_beads_client: MagicMock,
    ) -> None:
        """Test get_status when idle."""
        orchestrator = TicketOrchestrator(
            config=mock_config,
            beads_client=mock_beads_client,
        )

        status = orchestrator.get_status()

        assert status["status"] == "idle"
        assert status["cycle"] == 0
        assert status["max_cycles"] == 3
        assert status["total_files"] == 0

    def test_get_status_includes_pool_status(
        self,
        mock_config: MagicMock,
        mock_beads_client: MagicMock,
    ) -> None:
        """Test get_status includes pool status when pools exist."""
        orchestrator = TicketOrchestrator(
            config=mock_config,
            beads_client=mock_beads_client,
        )

        # Manually set a mock pool to test status inclusion
        mock_pool = MagicMock()
        mock_pool.get_status.return_value = {"active_count": 2}
        orchestrator._scribe_pool = mock_pool

        status = orchestrator.get_status()

        assert "scribe_pool" in status
        assert status["scribe_pool"]["active_count"] == 2


# =============================================================================
# TicketOrchestrator Stop Tests
# =============================================================================


class TestTicketOrchestratorStop:
    """Tests for stop method."""

    @pytest.mark.asyncio
    async def test_stop_sets_flag(
        self,
        mock_config: MagicMock,
        mock_beads_client: MagicMock,
    ) -> None:
        """Test stop sets the stop flag and status."""
        orchestrator = TicketOrchestrator(
            config=mock_config,
            beads_client=mock_beads_client,
        )

        await orchestrator.stop()

        assert orchestrator._stop_requested is True
        assert orchestrator.state.status == OrchestrationStatus.STOPPED

    @pytest.mark.asyncio
    async def test_stop_stops_pools(
        self,
        mock_config: MagicMock,
        mock_beads_client: MagicMock,
    ) -> None:
        """Test stop calls stop on worker pools."""
        orchestrator = TicketOrchestrator(
            config=mock_config,
            beads_client=mock_beads_client,
        )

        # Add mock pools
        mock_scribe_pool = AsyncMock()
        mock_challenger_pool = AsyncMock()
        orchestrator._scribe_pool = mock_scribe_pool
        orchestrator._challenger_pool = mock_challenger_pool

        await orchestrator.stop()

        mock_scribe_pool.stop.assert_called_once()
        mock_challenger_pool.stop.assert_called_once()


# =============================================================================
# TicketOrchestrator run_batch Tests
# =============================================================================


class TestTicketOrchestratorRunBatch:
    """Tests for run_batch method."""

    @pytest.mark.asyncio
    async def test_run_batch_invalid_directory(
        self,
        mock_config: MagicMock,
        mock_beads_client: MagicMock,
    ) -> None:
        """Test run_batch raises for non-existent directory."""
        orchestrator = TicketOrchestrator(
            config=mock_config,
            beads_client=mock_beads_client,
        )

        with pytest.raises(ValueError, match="does not exist"):
            await orchestrator.run_batch(Path("/non/existent/path"))

    @pytest.mark.asyncio
    async def test_run_batch_no_files(
        self,
        mock_config: MagicMock,
        mock_beads_client: MagicMock,
        tmp_path: Path,
    ) -> None:
        """Test run_batch handles empty directory."""
        orchestrator = TicketOrchestrator(
            config=mock_config,
            beads_client=mock_beads_client,
            use_mock=True,
        )

        # Patch program_manager.initialize_batch to return empty list
        orchestrator.program_manager.initialize_batch = MagicMock(return_value=[])
        orchestrator.program_manager.batch_id = "test-batch"

        result = await orchestrator.run_batch(tmp_path)

        assert result.final_decision == "NO_FILES"
        assert result.completed_files == []
        assert orchestrator.state.status == OrchestrationStatus.COMPLETED

    @pytest.mark.asyncio
    async def test_run_batch_satisfied_first_cycle(
        self,
        mock_config: MagicMock,
        mock_beads_client: MagicMock,
        sample_pm_tickets: list[ProgramManagerTicket],
        tmp_path: Path,
    ) -> None:
        """Test run_batch completes on first cycle when satisfied."""
        orchestrator = TicketOrchestrator(
            config=mock_config,
            beads_client=mock_beads_client,
            use_mock=True,
        )

        # Mock program manager
        orchestrator.program_manager.initialize_batch = MagicMock(
            return_value=sample_pm_tickets
        )
        orchestrator.program_manager.batch_id = "test-batch"

        # Mock beads client to return completed tickets
        mock_beads_client.get_tickets_by_state.return_value = sample_pm_tickets

        # Mock worker pools
        with patch.object(
            orchestrator, "_run_worker_cycle", new_callable=AsyncMock
        ):
            # Mock holistic review to be satisfied
            satisfied_output = HolisticReviewOutput(
                success=True,
                decision=ImperatorHolisticDecision.SATISFIED,
                quality_notes=["Good quality"],
            )

            with patch.object(
                orchestrator,
                "_run_holistic_review",
                new_callable=AsyncMock,
                return_value=satisfied_output,
            ):
                result = await orchestrator.run_batch(tmp_path)

        assert result.final_decision == "SATISFIED"
        assert result.total_cycles == 1
        assert "PROGRAM1.cbl" in result.completed_files
        assert "PROGRAM2.cbl" in result.completed_files

    @pytest.mark.asyncio
    async def test_run_batch_max_cycles_forced(
        self,
        mock_config: MagicMock,
        mock_beads_client: MagicMock,
        sample_pm_tickets: list[ProgramManagerTicket],
        tmp_path: Path,
    ) -> None:
        """Test run_batch forces completion at max cycles."""
        mock_config.pm_max_cycles = 2

        orchestrator = TicketOrchestrator(
            config=mock_config,
            beads_client=mock_beads_client,
            use_mock=True,
        )

        # Mock program manager
        orchestrator.program_manager.initialize_batch = MagicMock(
            return_value=sample_pm_tickets
        )
        orchestrator.program_manager.batch_id = "test-batch"
        orchestrator.program_manager.handle_clarifications = MagicMock()

        # Mock beads client
        mock_beads_client.get_tickets_by_state.return_value = sample_pm_tickets

        # Mock worker cycles
        with patch.object(
            orchestrator, "_run_worker_cycle", new_callable=AsyncMock
        ):
            # First cycle: needs clarification
            needs_clarification = HolisticReviewOutput(
                success=True,
                decision=ImperatorHolisticDecision.NEEDS_CLARIFICATION,
                clarification_requests=[],
            )
            # Second cycle: still needs clarification but max reached
            with patch.object(
                orchestrator,
                "_run_holistic_review",
                new_callable=AsyncMock,
                return_value=needs_clarification,
            ):
                result = await orchestrator.run_batch(tmp_path)

        assert result.final_decision == "FORCED_COMPLETE"
        assert result.total_cycles == 2


# =============================================================================
# TicketOrchestrator Internal Methods Tests
# =============================================================================


class TestTicketOrchestratorInternals:
    """Tests for internal methods."""

    def test_map_issue_priority(
        self,
        mock_config: MagicMock,
        mock_beads_client: MagicMock,
    ) -> None:
        """Test _map_issue_priority mapping."""
        from war_rig.models.tickets import IssuePriority

        orchestrator = TicketOrchestrator(
            config=mock_config,
            beads_client=mock_beads_client,
        )

        assert orchestrator._map_issue_priority(IssuePriority.CRITICAL) == BeadsPriority.CRITICAL
        assert orchestrator._map_issue_priority(IssuePriority.HIGH) == BeadsPriority.HIGH
        assert orchestrator._map_issue_priority(IssuePriority.MEDIUM) == BeadsPriority.MEDIUM

    def test_update_progress(
        self,
        mock_config: MagicMock,
        mock_beads_client: MagicMock,
        sample_pm_tickets: list[ProgramManagerTicket],
    ) -> None:
        """Test _update_progress updates state counters."""
        orchestrator = TicketOrchestrator(
            config=mock_config,
            beads_client=mock_beads_client,
        )

        # Mock beads client to return completed tickets
        mock_beads_client.get_tickets_by_state.return_value = sample_pm_tickets

        orchestrator._update_progress()

        # Should count completed docs (first call) and validated (second call)
        # Since both return same list, both counters get same value
        assert orchestrator.state.documented_files == 2

    def test_collect_results(
        self,
        mock_config: MagicMock,
        mock_beads_client: MagicMock,
        sample_pm_tickets: list[ProgramManagerTicket],
    ) -> None:
        """Test _collect_results gathers final results."""
        orchestrator = TicketOrchestrator(
            config=mock_config,
            beads_client=mock_beads_client,
        )
        orchestrator._state.cycle = 2

        # Mock completed tickets
        mock_beads_client.get_tickets_by_state.side_effect = [
            sample_pm_tickets,  # completed docs
            [],  # blocked docs
        ]

        result = BatchResult()
        result = orchestrator._collect_results(result)

        assert result.total_cycles == 2
        assert len(result.completed_files) == 2
        assert len(result.failed_files) == 0
        assert "PROGRAM1.cbl" in result.completed_files
