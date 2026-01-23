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
    ClarificationRequest,
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

    # Required for system overview generation
    client._pm_ticket_cache = {}

    # Required for orphan check
    client.reset_orphaned_tickets.return_value = 0

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
    config.exit_on_error = True

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
            # First cycle: needs clarification (must have actual issues to avoid SATISFIED shortcut)
            clarification = ClarificationRequest(
                question="What does PROGRAM1 do?",
                context="Need more detail",
                files=["PROGRAM1.cbl"],
            )
            needs_clarification = HolisticReviewOutput(
                success=True,
                decision=ImperatorHolisticDecision.NEEDS_CLARIFICATION,
                clarification_requests=[clarification],
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


# =============================================================================
# Feedback Context Tests (IMPFB-002, IMPFB-003)
# =============================================================================


class TestFeedbackContextBuilding:
    """Tests for feedback context building from Imperator review."""

    def test_parse_quality_note_empty_section(
        self,
        mock_config: MagicMock,
        mock_beads_client: MagicMock,
    ) -> None:
        """Test _parse_quality_note correctly parses empty section notes."""
        orchestrator = TicketOrchestrator(
            config=mock_config,
            beads_client=mock_beads_client,
        )

        note_str = "The inputs section is empty for most files"
        note = orchestrator._parse_quality_note(note_str, 0)

        assert note.category == "empty_section"
        assert "inputs" in note.description.lower()

    def test_parse_quality_note_missing_citation(
        self,
        mock_config: MagicMock,
        mock_beads_client: MagicMock,
    ) -> None:
        """Test _parse_quality_note correctly parses citation notes."""
        orchestrator = TicketOrchestrator(
            config=mock_config,
            beads_client=mock_beads_client,
        )

        note_str = "Missing line number citations for claims"
        note = orchestrator._parse_quality_note(note_str, 0)

        assert note.category == "missing_citation"

    def test_parse_quality_note_cross_reference(
        self,
        mock_config: MagicMock,
        mock_beads_client: MagicMock,
    ) -> None:
        """Test _parse_quality_note correctly parses cross-reference notes."""
        orchestrator = TicketOrchestrator(
            config=mock_config,
            beads_client=mock_beads_client,
        )

        note_str = "No cross-reference information between programs"
        note = orchestrator._parse_quality_note(note_str, 0)

        assert note.category == "no_cross_reference"

    def test_identify_critical_sections_default(
        self,
        mock_config: MagicMock,
        mock_beads_client: MagicMock,
    ) -> None:
        """Test _identify_critical_sections includes default sections."""
        orchestrator = TicketOrchestrator(
            config=mock_config,
            beads_client=mock_beads_client,
        )

        # No notes - should return default critical sections
        critical = orchestrator._identify_critical_sections([])

        assert "purpose" in critical
        assert "inputs" in critical
        assert "outputs" in critical

    def test_identify_critical_sections_from_notes(
        self,
        mock_config: MagicMock,
        mock_beads_client: MagicMock,
    ) -> None:
        """Test _identify_critical_sections adds sections from empty_section notes."""
        from war_rig.models.tickets import QualityNote

        orchestrator = TicketOrchestrator(
            config=mock_config,
            beads_client=mock_beads_client,
        )

        notes = [
            QualityNote(
                category="empty_section",
                description="Empty business_rules",
                affected_sections=["business_rules", "error_handling"],
            )
        ]

        critical = orchestrator._identify_critical_sections(notes)

        assert "business_rules" in critical
        assert "error_handling" in critical
        assert "purpose" in critical  # Default still present

    def test_build_feedback_context_from_holistic_review(
        self,
        mock_config: MagicMock,
        mock_beads_client: MagicMock,
    ) -> None:
        """Test _build_feedback_context creates proper context from review."""
        from typing import Any
        from war_rig.models.tickets import ChromeTicket, IssueType, IssuePriority

        # Rebuild the model to resolve forward references
        HolisticReviewOutput.model_rebuild()

        orchestrator = TicketOrchestrator(
            config=mock_config,
            beads_client=mock_beads_client,
        )
        orchestrator._state.cycle = 1

        # Create a mock holistic review output
        review_output = HolisticReviewOutput(
            decision=ImperatorHolisticDecision.NEEDS_CLARIFICATION,
            quality_notes=[
                "Empty inputs section in documentation",
                "Missing line number citations",
            ],
            quality_notes_structured=[
                {
                    "category": "empty_section",
                    "severity": "critical",
                    "description": "Inputs section is empty",
                    "affected_sections": ["inputs"],
                },
            ],
            file_feedback={
                "PROG.cbl": [
                    ChromeTicket(
                        ticket_id="CHR-001",
                        section="inputs",
                        issue_type=IssueType.INCOMPLETE,
                        description="Input section incomplete",
                        priority=IssuePriority.HIGH,
                    )
                ]
            },
            clarification_requests=[],
        )

        ctx = orchestrator._build_feedback_context(review_output)

        # Should use structured notes when available
        assert len(ctx.quality_notes) == 1
        assert ctx.quality_notes[0].category == "empty_section"
        assert ctx.quality_notes[0].severity == "critical"

        # Should have critical sections
        assert "inputs" in ctx.critical_sections

        # Should have previous cycle issues
        assert "PROG.cbl" in ctx.previous_cycle_issues
        assert len(ctx.previous_cycle_issues["PROG.cbl"]) == 1

        # Default settings
        assert ctx.augment_existing is True
        assert ctx.required_citations is True


# =============================================================================
# Max Ticket Retries Tests
# =============================================================================


class TestMaxTicketRetries:
    """Tests for max_ticket_retries feature that prevents endless loops."""

    @pytest.fixture
    def blocked_ticket(self) -> ProgramManagerTicket:
        """Create a blocked ticket for testing."""
        return ProgramManagerTicket(
            ticket_id="war_rig-test-blocked",
            ticket_type=TicketType.DOCUMENTATION,
            state=TicketState.BLOCKED,
            file_name="BLOCKED.cbl",
            program_id="BLOCKED",
            cycle_number=1,
            metadata={"retry_count": 0},
        )

    @pytest.fixture
    def ticket_at_max_retries(self) -> ProgramManagerTicket:
        """Create a ticket that has reached max retries."""
        return ProgramManagerTicket(
            ticket_id="war_rig-test-maxed",
            ticket_type=TicketType.DOCUMENTATION,
            state=TicketState.BLOCKED,
            file_name="MAXED.cbl",
            program_id="MAXED",
            cycle_number=1,
            metadata={"retry_count": 5},  # Already at max (default is 5)
        )

    @pytest.mark.asyncio
    async def test_super_scribe_rescue_increments_retry_count(
        self,
        mock_config: MagicMock,
        mock_beads_client: MagicMock,
        blocked_ticket: ProgramManagerTicket,
    ) -> None:
        """Test _run_super_scribe_rescue increments retry_count in metadata."""
        mock_config.max_ticket_retries = 5
        mock_config.exit_on_error = True
        mock_config.super_scribe_model = "anthropic/claude-opus-4-20250514"
        mock_config.num_super_scribes = 1
        mock_config.model_dump.return_value = {
            "scribe_model": "test-model",
            "num_scribes": 1,
            "exit_on_error": True,
            "max_ticket_retries": 5,
        }

        orchestrator = TicketOrchestrator(
            config=mock_config,
            beads_client=mock_beads_client,
        )
        orchestrator._input_directory = Path("/tmp")
        orchestrator._file_lock_manager = None

        # Mock beads client to return blocked ticket
        mock_beads_client.get_tickets_by_state.return_value = [blocked_ticket]

        # Track the metadata_updates passed to update_ticket_state
        captured_metadata_updates = {}

        def capture_update(ticket_id, state, reason=None, metadata_updates=None, decision=None):
            if metadata_updates:
                captured_metadata_updates[ticket_id] = metadata_updates
            return True

        mock_beads_client.update_ticket_state.side_effect = capture_update

        # Mock the ScribeWorkerPool
        with patch(
            "war_rig.orchestration.ticket_engine.ScribeWorkerPool"
        ) as MockPool:
            mock_pool = MagicMock()
            mock_pool.start = AsyncMock()
            mock_pool.wait = AsyncMock()
            mock_pool.stop = AsyncMock()
            mock_pool.get_status.return_value = {
                "total_processed": 1,
                "total_failed": 0,
            }
            MockPool.return_value = mock_pool

            # After rescue, no more blocked tickets
            mock_beads_client.get_tickets_by_state.side_effect = [
                [blocked_ticket],  # First call - find blocked
                [],                 # After rescue - no more blocked
            ]
            mock_beads_client.get_available_tickets.return_value = []

            await orchestrator._run_super_scribe_rescue()

            # Verify retry_count was incremented
            assert blocked_ticket.ticket_id in captured_metadata_updates
            assert captured_metadata_updates[blocked_ticket.ticket_id]["retry_count"] == 1

    @pytest.mark.asyncio
    async def test_super_scribe_rescue_raises_on_exceeded_retries(
        self,
        mock_config: MagicMock,
        mock_beads_client: MagicMock,
        ticket_at_max_retries: ProgramManagerTicket,
    ) -> None:
        """Test _run_super_scribe_rescue raises MaxTicketRetriesExceeded when limit exceeded."""
        from war_rig.utils.exceptions import MaxTicketRetriesExceeded

        mock_config.max_ticket_retries = 5
        mock_config.exit_on_error = True

        orchestrator = TicketOrchestrator(
            config=mock_config,
            beads_client=mock_beads_client,
        )

        # Mock beads client to return ticket that has exceeded max retries
        mock_beads_client.get_tickets_by_state.return_value = [ticket_at_max_retries]

        # Should raise MaxTicketRetriesExceeded
        with pytest.raises(MaxTicketRetriesExceeded) as exc_info:
            await orchestrator._run_super_scribe_rescue()

        # Verify exception details
        assert exc_info.value.ticket_id == ticket_at_max_retries.ticket_id
        assert exc_info.value.file_name == ticket_at_max_retries.file_name
        assert exc_info.value.retry_count == 6  # 5 + 1 for this attempt
        assert exc_info.value.max_retries == 5

    @pytest.mark.asyncio
    async def test_super_scribe_rescue_skips_exceeded_tickets_when_not_exit_on_error(
        self,
        mock_config: MagicMock,
        mock_beads_client: MagicMock,
        ticket_at_max_retries: ProgramManagerTicket,
    ) -> None:
        """Test tickets exceeding max retries are skipped when exit_on_error=False."""
        mock_config.max_ticket_retries = 5
        mock_config.exit_on_error = False  # Don't exit on error
        mock_config.super_scribe_model = "anthropic/claude-opus-4-20250514"
        mock_config.num_super_scribes = 1
        mock_config.model_dump.return_value = {
            "scribe_model": "test-model",
            "num_scribes": 1,
            "exit_on_error": False,
            "max_ticket_retries": 5,
        }

        orchestrator = TicketOrchestrator(
            config=mock_config,
            beads_client=mock_beads_client,
        )
        orchestrator._input_directory = Path("/tmp")
        orchestrator._file_lock_manager = None

        # Track which tickets were reset
        reset_tickets = []

        def track_update(ticket_id, state, reason=None, metadata_updates=None, decision=None):
            if state == TicketState.CREATED:
                reset_tickets.append(ticket_id)
            return True

        mock_beads_client.update_ticket_state.side_effect = track_update
        mock_beads_client.get_tickets_by_state.return_value = [ticket_at_max_retries]

        # Mock the ScribeWorkerPool - won't be called since no tickets to reset
        with patch(
            "war_rig.orchestration.ticket_engine.ScribeWorkerPool"
        ) as MockPool:
            mock_pool = AsyncMock()
            MockPool.return_value = mock_pool

            # Should not raise, but ticket should not be reset
            await orchestrator._run_super_scribe_rescue()

            # Exceeded ticket should NOT be reset to CREATED
            assert ticket_at_max_retries.ticket_id not in reset_tickets

    @pytest.mark.asyncio
    async def test_super_scribe_rescue_raises_after_rescue_still_blocked(
        self,
        mock_config: MagicMock,
        mock_beads_client: MagicMock,
        blocked_ticket: ProgramManagerTicket,
    ) -> None:
        """Test raises MaxTicketRetriesExceeded when ticket still blocked after reaching max retries."""
        from war_rig.utils.exceptions import MaxTicketRetriesExceeded

        mock_config.max_ticket_retries = 1  # Low limit for testing
        mock_config.exit_on_error = True
        mock_config.super_scribe_model = "anthropic/claude-opus-4-20250514"
        mock_config.num_super_scribes = 1
        mock_config.model_dump.return_value = {
            "scribe_model": "test-model",
            "num_scribes": 1,
            "exit_on_error": True,
            "max_ticket_retries": 1,
        }

        orchestrator = TicketOrchestrator(
            config=mock_config,
            beads_client=mock_beads_client,
        )
        orchestrator._input_directory = Path("/tmp")
        orchestrator._file_lock_manager = None

        # After reset and rescue, ticket is still blocked with retry_count = 1
        blocked_after_rescue = ProgramManagerTicket(
            ticket_id=blocked_ticket.ticket_id,
            ticket_type=TicketType.DOCUMENTATION,
            state=TicketState.BLOCKED,
            file_name=blocked_ticket.file_name,
            program_id=blocked_ticket.program_id,
            cycle_number=1,
            metadata={"retry_count": 1},  # Now at max
        )

        mock_beads_client.update_ticket_state.return_value = True

        # First call returns original blocked ticket, second call returns still-blocked ticket
        mock_beads_client.get_tickets_by_state.side_effect = [
            [blocked_ticket],       # Find blocked tickets
            [blocked_after_rescue], # Still blocked after rescue
        ]
        mock_beads_client.get_available_tickets.return_value = []

        with patch(
            "war_rig.orchestration.ticket_engine.ScribeWorkerPool"
        ) as MockPool:
            mock_pool = MagicMock()
            mock_pool.start = AsyncMock()
            mock_pool.wait = AsyncMock()
            mock_pool.stop = AsyncMock()
            mock_pool.get_status.return_value = {
                "total_processed": 0,
                "total_failed": 1,
            }
            MockPool.return_value = mock_pool

            # Should raise because ticket is still blocked at max retries
            with pytest.raises(MaxTicketRetriesExceeded) as exc_info:
                await orchestrator._run_super_scribe_rescue()

            assert exc_info.value.ticket_id == blocked_ticket.ticket_id
            assert exc_info.value.retry_count == 1
            assert exc_info.value.max_retries == 1

    @pytest.mark.asyncio
    async def test_super_scribe_rescue_injects_feedback_context(
        self,
        mock_config: MagicMock,
        mock_beads_client: MagicMock,
        blocked_ticket: ProgramManagerTicket,
    ) -> None:
        """Test _run_super_scribe_rescue injects feedback_context when resetting tickets (IMPFB-005)."""
        from war_rig.models.tickets import FeedbackContext, QualityNote

        mock_config.max_ticket_retries = 5
        mock_config.exit_on_error = True
        mock_config.super_scribe_model = "anthropic/claude-opus-4-20250514"
        mock_config.num_super_scribes = 1
        mock_config.model_dump.return_value = {
            "scribe_model": "test-model",
            "num_scribes": 1,
            "exit_on_error": True,
            "max_ticket_retries": 5,
        }

        orchestrator = TicketOrchestrator(
            config=mock_config,
            beads_client=mock_beads_client,
        )
        orchestrator._input_directory = Path("/tmp")
        orchestrator._file_lock_manager = None

        # Set up feedback context (simulating Imperator review completed)
        orchestrator._current_feedback_context = FeedbackContext(
            quality_notes=[
                QualityNote(
                    category="empty_section",
                    severity="critical",
                    description="Inputs section is empty",
                    affected_sections=["inputs"],
                )
            ],
            critical_sections=["inputs", "outputs"],
            required_citations=True,
            augment_existing=True,
        )

        # Mock beads client to return blocked ticket
        mock_beads_client.get_tickets_by_state.return_value = [blocked_ticket]

        # Track the metadata_updates passed to update_ticket_state
        captured_metadata_updates = {}

        def capture_update(ticket_id, state, reason=None, metadata_updates=None, decision=None):
            if metadata_updates:
                captured_metadata_updates[ticket_id] = metadata_updates
            return True

        mock_beads_client.update_ticket_state.side_effect = capture_update

        # Mock the ScribeWorkerPool
        with patch(
            "war_rig.orchestration.ticket_engine.ScribeWorkerPool"
        ) as MockPool:
            mock_pool = MagicMock()
            mock_pool.start = AsyncMock()
            mock_pool.wait = AsyncMock()
            mock_pool.stop = AsyncMock()
            mock_pool.get_status.return_value = {
                "total_processed": 1,
                "total_failed": 0,
            }
            MockPool.return_value = mock_pool

            # After rescue, no more blocked tickets
            mock_beads_client.get_tickets_by_state.side_effect = [
                [blocked_ticket],  # First call - find blocked
                [],                 # After rescue - no more blocked
            ]
            mock_beads_client.get_available_tickets.return_value = []

            await orchestrator._run_super_scribe_rescue()

            # Verify feedback_context was included in metadata updates
            assert blocked_ticket.ticket_id in captured_metadata_updates
            metadata = captured_metadata_updates[blocked_ticket.ticket_id]
            assert "retry_count" in metadata
            assert "feedback_context" in metadata

            # Verify feedback_context contents
            fc = metadata["feedback_context"]
            assert len(fc["quality_notes"]) == 1
            assert fc["quality_notes"][0]["category"] == "empty_section"
            assert "inputs" in fc["critical_sections"]
            assert fc["augment_existing"] is True


class TestMaxTicketRetriesExceededException:
    """Tests for MaxTicketRetriesExceeded exception."""

    def test_exception_message_format(self) -> None:
        """Test exception message contains all relevant information."""
        from war_rig.utils.exceptions import MaxTicketRetriesExceeded

        exc = MaxTicketRetriesExceeded(
            ticket_id="DOC-12345678",
            file_name="TESTPROG.cbl",
            retry_count=6,
            max_retries=5,
        )

        message = str(exc)
        assert "DOC-12345678" in message
        assert "TESTPROG.cbl" in message
        assert "6" in message
        assert "5" in message

    def test_exception_attributes(self) -> None:
        """Test exception stores attributes correctly."""
        from war_rig.utils.exceptions import MaxTicketRetriesExceeded

        exc = MaxTicketRetriesExceeded(
            ticket_id="DOC-12345678",
            file_name="TESTPROG.cbl",
            retry_count=6,
            max_retries=5,
        )

        assert exc.ticket_id == "DOC-12345678"
        assert exc.file_name == "TESTPROG.cbl"
        assert exc.retry_count == 6
        assert exc.max_retries == 5
