"""Tests for Program Manager agent.

Tests for ProgramManagerAgent class that orchestrates batch documentation
workflows through the beads ticket system.
"""

from datetime import datetime
from pathlib import Path
from unittest.mock import MagicMock, patch

import pytest

from war_rig.agents.program_manager import (
    BatchTicketSummary,
    ClarificationRequest,
    CycleSummary,
    ProgramManagerAgent,
    ProgramManagerInput,
    ProgramManagerOutput,
)
from war_rig.beads import (
    BeadsClient,
    BeadsPriority,
    ProgramManagerTicket,
    TicketState,
    TicketType,
)
from war_rig.config import (
    ChallengerConfig,
    ImperatorConfig,
    ScribeConfig,
    WarRigConfig,
)
from war_rig.io.reader import SourceFile
from war_rig.models.templates import FileType

# =============================================================================
# Fixtures
# =============================================================================


@pytest.fixture
def mock_beads_client() -> MagicMock:
    """Create a mock BeadsClient for isolated testing."""
    client = MagicMock(spec=BeadsClient)
    client.enabled = True
    client.dry_run = False
    client.create_pm_ticket.return_value = ProgramManagerTicket(
        ticket_id="war_rig-test001",
        ticket_type=TicketType.DOCUMENTATION,
        state=TicketState.CREATED,
        file_name="TEST.cbl",
        program_id="TEST",
        cycle_number=1,
    )
    client.get_available_tickets.return_value = []
    client.get_tickets_by_state.return_value = []
    client.claim_ticket.return_value = True
    client.update_ticket_state.return_value = True
    # Required for deduplication in initialize_batch
    client._pm_ticket_cache = {}
    return client


@pytest.fixture
def mock_config() -> WarRigConfig:
    """Create a mock WarRigConfig for testing."""
    return WarRigConfig(
        rig_id="TEST_RIG",
        max_iterations=2,
        max_questions_per_round=3,
        max_chrome_tickets=3,
        scribe=ScribeConfig(
            model="claude-sonnet-4-20250514",
            temperature=0.1,
        ),
        challenger=ChallengerConfig(
            model="claude-sonnet-4-20250514",
            temperature=0.1,
        ),
        imperator=ImperatorConfig(
            model="claude-sonnet-4-20250514",
            temperature=0.1,
        ),
    )


@pytest.fixture
def sample_source_files() -> list[SourceFile]:
    """Create sample source files for testing."""
    return [
        SourceFile(
            path=Path("/tmp/test/PROG1.cbl"),
            name="PROG1.cbl",
            stem="PROG1",
            file_type=FileType.COBOL,
            size_bytes=1000,
        ),
        SourceFile(
            path=Path("/tmp/test/PROG2.cbl"),
            name="PROG2.cbl",
            stem="PROG2",
            file_type=FileType.COBOL,
            size_bytes=2000,
        ),
        SourceFile(
            path=Path("/tmp/test/JOB1.jcl"),
            name="JOB1.jcl",
            stem="JOB1",
            file_type=FileType.JCL,
            size_bytes=500,
        ),
    ]


@pytest.fixture
def sample_pm_tickets() -> list[ProgramManagerTicket]:
    """Create sample PM tickets for testing."""
    return [
        ProgramManagerTicket(
            ticket_id="war_rig-001",
            ticket_type=TicketType.DOCUMENTATION,
            state=TicketState.CREATED,
            file_name="PROG1.cbl",
            program_id="PROG1",
            cycle_number=1,
            metadata={"batch_id": "batch-test"},
        ),
        ProgramManagerTicket(
            ticket_id="war_rig-002",
            ticket_type=TicketType.DOCUMENTATION,
            state=TicketState.COMPLETED,
            file_name="PROG2.cbl",
            program_id="PROG2",
            cycle_number=1,
            metadata={"batch_id": "batch-test"},
        ),
    ]


# =============================================================================
# BatchTicketSummary Tests
# =============================================================================


class TestBatchTicketSummary:
    """Tests for BatchTicketSummary dataclass."""

    def test_create_empty_summary(self):
        """Test creating an empty summary with defaults."""
        summary = BatchTicketSummary()

        assert summary.total == 0
        assert summary.created_count == 0
        assert summary.in_progress_count == 0
        assert summary.completed_count == 0
        assert summary.blocked_count == 0
        assert summary.documentation_count == 0
        assert summary.validation_count == 0

    def test_created_count_property(self):
        """Test created_count property."""
        summary = BatchTicketSummary(
            by_state={TicketState.CREATED: 5},
        )

        assert summary.created_count == 5

    def test_in_progress_count_property(self):
        """Test in_progress_count combines CLAIMED and IN_PROGRESS."""
        summary = BatchTicketSummary(
            by_state={
                TicketState.CLAIMED: 2,
                TicketState.IN_PROGRESS: 3,
            },
        )

        assert summary.in_progress_count == 5

    def test_completed_count_property(self):
        """Test completed_count property."""
        summary = BatchTicketSummary(
            by_state={TicketState.COMPLETED: 10},
        )

        assert summary.completed_count == 10

    def test_blocked_count_property(self):
        """Test blocked_count property."""
        summary = BatchTicketSummary(
            by_state={TicketState.BLOCKED: 2},
        )

        assert summary.blocked_count == 2

    def test_documentation_count_property(self):
        """Test documentation_count property."""
        summary = BatchTicketSummary(
            by_type={TicketType.DOCUMENTATION: 15},
        )

        assert summary.documentation_count == 15

    def test_validation_count_property(self):
        """Test validation_count property."""
        summary = BatchTicketSummary(
            by_type={TicketType.VALIDATION: 8},
        )

        assert summary.validation_count == 8


# =============================================================================
# ClarificationRequest Tests
# =============================================================================


class TestClarificationRequest:
    """Tests for ClarificationRequest dataclass."""

    def test_create_minimal_request(self):
        """Test creating request with required fields only."""
        request = ClarificationRequest(
            file_name="TEST.cbl",
            issue_description="Missing error handling documentation",
        )

        assert request.file_name == "TEST.cbl"
        assert request.issue_description == "Missing error handling documentation"
        assert request.section is None
        assert request.priority == BeadsPriority.MEDIUM
        assert request.guidance is None
        assert request.parent_ticket_id is None

    def test_create_full_request(self):
        """Test creating request with all fields."""
        request = ClarificationRequest(
            file_name="TEST.cbl",
            issue_description="Purpose section is vague",
            section="purpose",
            priority=BeadsPriority.HIGH,
            guidance="Add specific business context",
            parent_ticket_id="war_rig-parent123",
        )

        assert request.file_name == "TEST.cbl"
        assert request.section == "purpose"
        assert request.priority == BeadsPriority.HIGH
        assert request.guidance == "Add specific business context"
        assert request.parent_ticket_id == "war_rig-parent123"


# =============================================================================
# CycleSummary Tests
# =============================================================================


class TestCycleSummary:
    """Tests for CycleSummary dataclass."""

    def test_create_cycle_summary(self):
        """Test creating a cycle summary."""
        ticket_summary = BatchTicketSummary(
            by_state={TicketState.COMPLETED: 5},
            total=5,
        )
        now = datetime.utcnow()

        summary = CycleSummary(
            batch_id="batch-test",
            cycle_number=1,
            started_at=now,
            ticket_summary=ticket_summary,
            files_total=5,
            files_documented=5,
            files_validated=3,
            is_complete=True,
            can_trigger_holistic_review=True,
        )

        assert summary.batch_id == "batch-test"
        assert summary.cycle_number == 1
        assert summary.files_total == 5
        assert summary.files_documented == 5
        assert summary.files_validated == 3
        assert summary.is_complete is True
        assert summary.can_trigger_holistic_review is True

    def test_to_dict(self):
        """Test CycleSummary.to_dict() method."""
        ticket_summary = BatchTicketSummary(
            by_state={
                TicketState.COMPLETED: 5,
                TicketState.IN_PROGRESS: 2,
            },
            total=7,
        )
        now = datetime.utcnow()

        summary = CycleSummary(
            batch_id="batch-test",
            cycle_number=2,
            started_at=now,
            ticket_summary=ticket_summary,
            files_total=10,
            files_documented=7,
            files_validated=5,
            is_complete=False,
            can_trigger_holistic_review=False,
        )

        result = summary.to_dict()

        assert result["batch_id"] == "batch-test"
        assert result["cycle_number"] == 2
        assert result["started_at"] == now.isoformat()
        assert result["files_total"] == 10
        assert result["files_documented"] == 7
        assert result["files_validated"] == 5
        assert result["is_complete"] is False
        assert result["can_trigger_holistic_review"] is False
        assert result["tickets"]["total"] == 7
        assert result["tickets"]["completed"] == 5
        assert result["tickets"]["in_progress"] == 2


# =============================================================================
# ProgramManagerAgent Tests
# =============================================================================


class TestProgramManagerAgentInit:
    """Tests for ProgramManagerAgent initialization."""

    def test_init_with_config(self, mock_config, mock_beads_client):
        """Test agent initialization with config."""
        pm = ProgramManagerAgent(
            config=mock_config,
            beads_client=mock_beads_client,
        )

        assert pm.name == "ProgramManager"
        assert pm.beads == mock_beads_client
        assert pm.batch_id is None
        assert pm.cycle_number == 1
        assert pm.discovered_files == []
        assert pm.created_tickets == []

    def test_init_without_beads_client(self, mock_config):
        """Test agent initialization creates default beads client."""
        with patch("war_rig.agents.program_manager.get_beads_client") as mock_get:
            mock_get.return_value = MagicMock(spec=BeadsClient)

            pm = ProgramManagerAgent(config=mock_config)

            mock_get.assert_called_once()
            assert pm.beads is not None

    def test_war_rig_config_property(self, mock_config, mock_beads_client):
        """Test war_rig_config property returns the config."""
        pm = ProgramManagerAgent(
            config=mock_config,
            beads_client=mock_beads_client,
        )

        assert pm.war_rig_config == mock_config


class TestProgramManagerAgentInitializeBatch:
    """Tests for ProgramManagerAgent.initialize_batch() method."""

    def test_initialize_batch_creates_tickets(
        self, mock_config, mock_beads_client, sample_source_files, tmp_path
    ):
        """Test initialize_batch creates tickets for discovered files."""
        # Setup mock source reader
        with patch.object(
            ProgramManagerAgent,
            "__init__",
            lambda self, **kwargs: None,
        ):
            pm = ProgramManagerAgent.__new__(ProgramManagerAgent)
            pm._war_rig_config = mock_config
            pm.beads = mock_beads_client
            pm.source_reader = MagicMock()
            pm.source_reader.discover_files.return_value = sample_source_files
            pm.batch_id = None
            pm.cycle_number = 1
            pm.discovered_files = []
            pm.created_tickets = []
            pm._batch_start_time = None

        tickets = pm.initialize_batch(
            input_dir=tmp_path,
            batch_id="batch-test123",
        )

        assert pm.batch_id == "batch-test123"
        assert pm.cycle_number == 1
        assert len(pm.discovered_files) == 3
        assert mock_beads_client.create_pm_ticket.call_count == 3

    def test_initialize_batch_generates_batch_id(
        self, mock_config, mock_beads_client, tmp_path
    ):
        """Test initialize_batch generates batch ID if not provided."""
        with patch.object(
            ProgramManagerAgent,
            "__init__",
            lambda self, **kwargs: None,
        ):
            pm = ProgramManagerAgent.__new__(ProgramManagerAgent)
            pm._war_rig_config = mock_config
            pm.beads = mock_beads_client
            pm.source_reader = MagicMock()
            pm.source_reader.discover_files.return_value = []
            pm.batch_id = None
            pm.cycle_number = 1
            pm.discovered_files = []
            pm.created_tickets = []
            pm._batch_start_time = None

        pm.initialize_batch(input_dir=tmp_path)

        assert pm.batch_id is not None
        assert pm.batch_id.startswith("batch-")


class TestProgramManagerAgentGetBatchStatus:
    """Tests for ProgramManagerAgent.get_batch_status() method."""

    def test_get_batch_status_returns_counts(self, mock_config, mock_beads_client):
        """Test get_batch_status returns correct ticket counts."""
        # Setup mock to return different tickets for different states
        def mock_get_by_state(state):
            if state == TicketState.COMPLETED:
                return [
                    ProgramManagerTicket(
                        ticket_id="war_rig-001",
                        ticket_type=TicketType.DOCUMENTATION,
                        state=TicketState.COMPLETED,
                        file_name="PROG1.cbl",
                        metadata={"batch_id": "batch-test"},
                    ),
                ]
            return []

        mock_beads_client.get_tickets_by_state.side_effect = mock_get_by_state

        with patch.object(
            ProgramManagerAgent,
            "__init__",
            lambda self, **kwargs: None,
        ):
            pm = ProgramManagerAgent.__new__(ProgramManagerAgent)
            pm._war_rig_config = mock_config
            pm.beads = mock_beads_client
            pm.batch_id = "batch-test"
            pm.cycle_number = 1

        status = pm.get_batch_status()

        # Should return counts for all states
        assert "created" in status
        assert "completed" in status
        assert "in_progress" in status
        assert "blocked" in status


class TestProgramManagerAgentIsBatchComplete:
    """Tests for ProgramManagerAgent.is_batch_complete() method."""

    def test_batch_complete_when_all_completed(self, mock_config, mock_beads_client):
        """Test is_batch_complete returns True when all tickets completed."""
        with patch.object(
            ProgramManagerAgent,
            "__init__",
            lambda self, **kwargs: None,
        ):
            pm = ProgramManagerAgent.__new__(ProgramManagerAgent)
            pm._war_rig_config = mock_config
            pm.beads = mock_beads_client
            pm.batch_id = "batch-test"

        # Mock get_batch_status to return all completed
        with patch.object(pm, "get_batch_status") as mock_status:
            mock_status.return_value = {
                "created": 0,
                "claimed": 0,
                "in_progress": 0,
                "blocked": 0,
                "completed": 5,
                "rework": 0,
                "cancelled": 0,
                "merged": 0,
            }

            assert pm.is_batch_complete() is True

    def test_batch_not_complete_with_created_tickets(
        self, mock_config, mock_beads_client
    ):
        """Test is_batch_complete returns False when created tickets exist."""
        with patch.object(
            ProgramManagerAgent,
            "__init__",
            lambda self, **kwargs: None,
        ):
            pm = ProgramManagerAgent.__new__(ProgramManagerAgent)
            pm._war_rig_config = mock_config
            pm.beads = mock_beads_client
            pm.batch_id = "batch-test"

        with patch.object(pm, "get_batch_status") as mock_status:
            mock_status.return_value = {
                "created": 2,
                "claimed": 0,
                "in_progress": 0,
                "blocked": 0,
                "completed": 3,
                "rework": 0,
                "cancelled": 0,
                "merged": 0,
            }

            assert pm.is_batch_complete() is False

    def test_batch_not_complete_with_in_progress_tickets(
        self, mock_config, mock_beads_client
    ):
        """Test is_batch_complete returns False when in_progress tickets exist."""
        with patch.object(
            ProgramManagerAgent,
            "__init__",
            lambda self, **kwargs: None,
        ):
            pm = ProgramManagerAgent.__new__(ProgramManagerAgent)
            pm._war_rig_config = mock_config
            pm.beads = mock_beads_client
            pm.batch_id = "batch-test"

        with patch.object(pm, "get_batch_status") as mock_status:
            mock_status.return_value = {
                "created": 0,
                "claimed": 0,
                "in_progress": 1,
                "blocked": 0,
                "completed": 4,
                "rework": 0,
                "cancelled": 0,
                "merged": 0,
            }

            assert pm.is_batch_complete() is False

    def test_batch_not_complete_with_blocked_tickets(
        self, mock_config, mock_beads_client
    ):
        """Test is_batch_complete returns False when blocked tickets exist."""
        with patch.object(
            ProgramManagerAgent,
            "__init__",
            lambda self, **kwargs: None,
        ):
            pm = ProgramManagerAgent.__new__(ProgramManagerAgent)
            pm._war_rig_config = mock_config
            pm.beads = mock_beads_client
            pm.batch_id = "batch-test"

        with patch.object(pm, "get_batch_status") as mock_status:
            mock_status.return_value = {
                "created": 0,
                "claimed": 0,
                "in_progress": 0,
                "blocked": 1,
                "completed": 4,
                "rework": 0,
                "cancelled": 0,
                "merged": 0,
            }

            assert pm.is_batch_complete() is False

    def test_batch_not_complete_with_no_completed(self, mock_config, mock_beads_client):
        """Test is_batch_complete returns False when no tickets completed."""
        with patch.object(
            ProgramManagerAgent,
            "__init__",
            lambda self, **kwargs: None,
        ):
            pm = ProgramManagerAgent.__new__(ProgramManagerAgent)
            pm._war_rig_config = mock_config
            pm.beads = mock_beads_client
            pm.batch_id = "batch-test"

        with patch.object(pm, "get_batch_status") as mock_status:
            mock_status.return_value = {
                "created": 0,
                "claimed": 0,
                "in_progress": 0,
                "blocked": 0,
                "completed": 0,
                "rework": 0,
                "cancelled": 0,
                "merged": 0,
            }

            assert pm.is_batch_complete() is False


class TestProgramManagerAgentTriggerHolisticReview:
    """Tests for ProgramManagerAgent.trigger_holistic_review() method."""

    def test_trigger_holistic_review_creates_ticket(
        self, mock_config, mock_beads_client
    ):
        """Test trigger_holistic_review creates HOLISTIC_REVIEW ticket."""
        holistic_ticket = ProgramManagerTicket(
            ticket_id="war_rig-holistic001",
            ticket_type=TicketType.HOLISTIC_REVIEW,
            state=TicketState.CREATED,
            file_name="PROG1.cbl,PROG2.cbl",
            program_id="BATCH-batch-test",
            cycle_number=1,
        )
        mock_beads_client.create_pm_ticket.return_value = holistic_ticket

        with patch.object(
            ProgramManagerAgent,
            "__init__",
            lambda self, **kwargs: None,
        ):
            pm = ProgramManagerAgent.__new__(ProgramManagerAgent)
            pm._war_rig_config = mock_config
            pm.beads = mock_beads_client
            pm.batch_id = "batch-test"
            pm.cycle_number = 1
            pm.discovered_files = [
                SourceFile(
                    path=Path("/tmp/PROG1.cbl"),
                    name="PROG1.cbl",
                    stem="PROG1",
                    file_type=FileType.COBOL,
                    size_bytes=100,
                ),
                SourceFile(
                    path=Path("/tmp/PROG2.cbl"),
                    name="PROG2.cbl",
                    stem="PROG2",
                    file_type=FileType.COBOL,
                    size_bytes=200,
                ),
            ]

        # Mock is_batch_complete to return True
        with patch.object(pm, "is_batch_complete", return_value=True):
            ticket = pm.trigger_holistic_review()

        assert ticket is not None
        assert ticket.ticket_type == TicketType.HOLISTIC_REVIEW
        mock_beads_client.create_pm_ticket.assert_called_once()
        call_kwargs = mock_beads_client.create_pm_ticket.call_args[1]
        assert call_kwargs["ticket_type"] == TicketType.HOLISTIC_REVIEW
        assert call_kwargs["priority"] == BeadsPriority.HIGH

    def test_trigger_holistic_review_raises_when_not_complete(
        self, mock_config, mock_beads_client
    ):
        """Test trigger_holistic_review raises error when batch not complete."""
        with patch.object(
            ProgramManagerAgent,
            "__init__",
            lambda self, **kwargs: None,
        ):
            pm = ProgramManagerAgent.__new__(ProgramManagerAgent)
            pm._war_rig_config = mock_config
            pm.beads = mock_beads_client
            pm.batch_id = "batch-test"
            pm.cycle_number = 1

        with patch.object(pm, "is_batch_complete", return_value=False):
            with pytest.raises(ValueError, match="batch is not complete"):
                pm.trigger_holistic_review()

    def test_trigger_holistic_review_raises_without_batch(
        self, mock_config, mock_beads_client
    ):
        """Test trigger_holistic_review raises error without active batch."""
        with patch.object(
            ProgramManagerAgent,
            "__init__",
            lambda self, **kwargs: None,
        ):
            pm = ProgramManagerAgent.__new__(ProgramManagerAgent)
            pm._war_rig_config = mock_config
            pm.beads = mock_beads_client
            pm.batch_id = None  # No active batch
            pm.cycle_number = 1

        with patch.object(pm, "is_batch_complete", return_value=True):
            with pytest.raises(ValueError, match="No active batch"):
                pm.trigger_holistic_review()


class TestProgramManagerAgentHandleClarifications:
    """Tests for ProgramManagerAgent.handle_clarifications() method."""

    def test_handle_clarifications_creates_tickets_when_files_exist(
        self, mock_config, mock_beads_client, tmp_path
    ):
        """Test handle_clarifications creates tickets when source and docs exist."""
        clarification_ticket = ProgramManagerTicket(
            ticket_id="war_rig-clar001",
            ticket_type=TicketType.CLARIFICATION,
            state=TicketState.CREATED,
            file_name="PROG1.cbl",
            program_id="PROG1",
            cycle_number=2,
        )
        mock_beads_client.create_pm_ticket.return_value = clarification_ticket

        # Create mock source files and documentation
        input_dir = tmp_path / "input"
        output_dir = tmp_path / "output"
        input_dir.mkdir()
        output_dir.mkdir()
        (input_dir / "PROG1.cbl").write_text("COBOL SOURCE")
        (input_dir / "PROG2.cbl").write_text("COBOL SOURCE")
        (output_dir / "PROG1.cbl.doc.json").write_text("{}")
        (output_dir / "PROG2.cbl.doc.json").write_text("{}")

        # Update mock config to use tmp_path
        mock_config.output_directory = output_dir

        with patch.object(
            ProgramManagerAgent,
            "__init__",
            lambda self, **kwargs: None,
        ):
            pm = ProgramManagerAgent.__new__(ProgramManagerAgent)
            pm._war_rig_config = mock_config
            pm.beads = mock_beads_client
            pm.batch_id = "batch-test"
            pm.cycle_number = 1
            pm.discovered_files = []
            pm._input_directory = input_dir

        requests = [
            ClarificationRequest(
                file_name="PROG1.cbl",
                issue_description="Missing error handling",
                section="error_handling",
                priority=BeadsPriority.HIGH,
                parent_ticket_id="war_rig-parent001",
            ),
            ClarificationRequest(
                file_name="PROG2.cbl",
                issue_description="Vague purpose section",
                section="purpose",
            ),
        ]

        tickets = pm.handle_clarifications(requests)

        # With files and docs existing, both tickets should be created
        assert len(tickets) == 2
        assert pm.cycle_number == 2  # Cycle incremented
        assert mock_beads_client.create_pm_ticket.call_count == 2

    def test_handle_clarifications_skips_nonexistent_files(
        self, mock_config, mock_beads_client, tmp_path
    ):
        """Test handle_clarifications skips tickets for files that don't exist."""
        # Create empty directories - no source files or docs
        input_dir = tmp_path / "input"
        output_dir = tmp_path / "output"
        input_dir.mkdir()
        output_dir.mkdir()

        mock_config.output_directory = output_dir

        with patch.object(
            ProgramManagerAgent,
            "__init__",
            lambda self, **kwargs: None,
        ):
            pm = ProgramManagerAgent.__new__(ProgramManagerAgent)
            pm._war_rig_config = mock_config
            pm.beads = mock_beads_client
            pm.batch_id = "batch-test"
            pm.cycle_number = 1
            pm.discovered_files = []
            pm._input_directory = input_dir

        requests = [
            ClarificationRequest(
                file_name="NONEXISTENT.cbl",
                issue_description="Missing section",
                section="purpose",
            ),
        ]

        tickets = pm.handle_clarifications(requests)

        # File doesn't exist, ticket should be skipped
        assert len(tickets) == 0
        assert mock_beads_client.create_pm_ticket.call_count == 0

    def test_handle_clarifications_raises_without_batch(
        self, mock_config, mock_beads_client
    ):
        """Test handle_clarifications raises error without active batch."""
        with patch.object(
            ProgramManagerAgent,
            "__init__",
            lambda self, **kwargs: None,
        ):
            pm = ProgramManagerAgent.__new__(ProgramManagerAgent)
            pm._war_rig_config = mock_config
            pm.beads = mock_beads_client
            pm.batch_id = None  # No active batch
            pm.cycle_number = 1
            pm.discovered_files = []
            pm._input_directory = None

        with pytest.raises(ValueError, match="No active batch"):
            pm.handle_clarifications([])

    def test_handle_clarifications_creates_chrome_for_no_parent(
        self, mock_config, mock_beads_client, tmp_path
    ):
        """Test handle_clarifications creates CHROME ticket when no parent."""
        chrome_ticket = ProgramManagerTicket(
            ticket_id="war_rig-chrome001",
            ticket_type=TicketType.CHROME,
            state=TicketState.CREATED,
            file_name="PROG1.cbl",
            program_id="PROG1",
            cycle_number=2,
        )
        mock_beads_client.create_pm_ticket.return_value = chrome_ticket

        # Create mock source file and documentation
        input_dir = tmp_path / "input"
        output_dir = tmp_path / "output"
        input_dir.mkdir()
        output_dir.mkdir()
        (input_dir / "PROG1.cbl").write_text("COBOL SOURCE")
        (output_dir / "PROG1.cbl.doc.json").write_text("{}")

        mock_config.output_directory = output_dir

        with patch.object(
            ProgramManagerAgent,
            "__init__",
            lambda self, **kwargs: None,
        ):
            pm = ProgramManagerAgent.__new__(ProgramManagerAgent)
            pm._war_rig_config = mock_config
            pm.beads = mock_beads_client
            pm.batch_id = "batch-test"
            pm.cycle_number = 1
            pm.discovered_files = []
            pm._input_directory = input_dir

        requests = [
            ClarificationRequest(
                file_name="PROG1.cbl",
                issue_description="Missing section",
                # No parent_ticket_id - should create CHROME
            ),
        ]

        pm.handle_clarifications(requests)

        call_kwargs = mock_beads_client.create_pm_ticket.call_args[1]
        assert call_kwargs["ticket_type"] == TicketType.CHROME


class TestProgramManagerAgentGetCycleSummary:
    """Tests for ProgramManagerAgent.get_cycle_summary() method."""

    def test_get_cycle_summary_returns_summary(self, mock_config, mock_beads_client):
        """Test get_cycle_summary returns CycleSummary object."""
        mock_beads_client.get_available_tickets.return_value = []
        mock_beads_client.get_tickets_by_state.return_value = []

        with patch.object(
            ProgramManagerAgent,
            "__init__",
            lambda self, **kwargs: None,
        ):
            pm = ProgramManagerAgent.__new__(ProgramManagerAgent)
            pm._war_rig_config = mock_config
            pm.beads = mock_beads_client
            pm.batch_id = "batch-test"
            pm.cycle_number = 1
            pm.discovered_files = [
                SourceFile(
                    path=Path("/tmp/PROG1.cbl"),
                    name="PROG1.cbl",
                    stem="PROG1",
                    file_type=FileType.COBOL,
                    size_bytes=100,
                ),
            ]
            pm._batch_start_time = datetime.utcnow()

        with patch.object(pm, "get_batch_status") as mock_status:
            mock_status.return_value = {
                "created": 0,
                "claimed": 0,
                "in_progress": 0,
                "blocked": 0,
                "completed": 1,
                "rework": 0,
                "cancelled": 0,
                "merged": 0,
            }
            with patch.object(pm, "is_batch_complete", return_value=True):
                summary = pm.get_cycle_summary()

        assert isinstance(summary, CycleSummary)
        assert summary.batch_id == "batch-test"
        assert summary.cycle_number == 1
        assert summary.files_total == 1

    def test_get_cycle_summary_raises_without_batch(
        self, mock_config, mock_beads_client
    ):
        """Test get_cycle_summary raises error without active batch."""
        with patch.object(
            ProgramManagerAgent,
            "__init__",
            lambda self, **kwargs: None,
        ):
            pm = ProgramManagerAgent.__new__(ProgramManagerAgent)
            pm._war_rig_config = mock_config
            pm.beads = mock_beads_client
            pm.batch_id = None  # No active batch

        with pytest.raises(ValueError, match="No active batch"):
            pm.get_cycle_summary()


# =============================================================================
# ProgramManagerInput Tests
# =============================================================================


class TestProgramManagerInput:
    """Tests for ProgramManagerInput model."""

    def test_create_minimal_input(self, tmp_path):
        """Test creating input with required fields only."""
        inp = ProgramManagerInput(
            input_directory=tmp_path,
        )

        assert inp.input_directory == tmp_path
        assert inp.file_types is None
        assert inp.recursive is True
        assert inp.batch_id is None
        assert inp.cycle_number == 1

    def test_create_full_input(self, tmp_path):
        """Test creating input with all fields."""
        inp = ProgramManagerInput(
            input_directory=tmp_path,
            file_types=[FileType.COBOL, FileType.JCL],
            recursive=False,
            batch_id="custom-batch-id",
            cycle_number=2,
        )

        assert inp.input_directory == tmp_path
        assert inp.file_types == [FileType.COBOL, FileType.JCL]
        assert inp.recursive is False
        assert inp.batch_id == "custom-batch-id"
        assert inp.cycle_number == 2


# =============================================================================
# ProgramManagerOutput Tests
# =============================================================================


class TestProgramManagerOutput:
    """Tests for ProgramManagerOutput model."""

    def test_create_success_output(self):
        """Test creating successful output."""
        output = ProgramManagerOutput(
            success=True,
            batch_id="batch-test",
            cycle_number=1,
            files_discovered=5,
            tickets_created=5,
            tickets_by_state={"created": 5},
            tickets_by_type={"documentation": 5},
            is_batch_complete=False,
            source_files=["PROG1.cbl", "PROG2.cbl"],
        )

        assert output.success is True
        assert output.batch_id == "batch-test"
        assert output.files_discovered == 5
        assert output.tickets_created == 5

    def test_create_error_output(self):
        """Test creating error output."""
        output = ProgramManagerOutput(
            success=False,
            error="Something went wrong",
        )

        assert output.success is False
        assert output.error == "Something went wrong"
        assert output.batch_id == ""


# =============================================================================
# Async Tests
# =============================================================================


class TestProgramManagerAgentAsync:
    """Async tests for ProgramManagerAgent."""

    @pytest.mark.asyncio
    async def test_ainvoke_success(self, mock_config, mock_beads_client, tmp_path):
        """Test ainvoke returns successful output."""
        with patch.object(
            ProgramManagerAgent,
            "__init__",
            lambda self, **kwargs: None,
        ):
            pm = ProgramManagerAgent.__new__(ProgramManagerAgent)
            pm._war_rig_config = mock_config
            pm.beads = mock_beads_client
            pm.source_reader = MagicMock()
            pm.source_reader.discover_files.return_value = []
            pm.batch_id = None
            pm.cycle_number = 1
            pm.discovered_files = []
            pm.created_tickets = []
            pm._batch_start_time = None
            pm.name = "ProgramManager"

        input_data = ProgramManagerInput(
            input_directory=tmp_path,
            batch_id="test-batch",
        )

        with patch.object(pm, "get_batch_status", return_value={"created": 0}):
            with patch.object(pm, "is_batch_complete", return_value=False):
                output = await pm.ainvoke(input_data)

        assert output.success is True
        assert output.batch_id == "test-batch"

    @pytest.mark.asyncio
    async def test_ainvoke_handles_error(self, mock_config, mock_beads_client, tmp_path):
        """Test ainvoke handles errors gracefully."""
        with patch.object(
            ProgramManagerAgent,
            "__init__",
            lambda self, **kwargs: None,
        ):
            pm = ProgramManagerAgent.__new__(ProgramManagerAgent)
            pm._war_rig_config = mock_config
            pm.beads = mock_beads_client
            pm.source_reader = MagicMock()
            pm.source_reader.discover_files.side_effect = Exception("Test error")
            pm.batch_id = None
            pm.cycle_number = 1
            pm.discovered_files = []
            pm.created_tickets = []
            pm._batch_start_time = None
            pm.name = "ProgramManager"

        input_data = ProgramManagerInput(
            input_directory=tmp_path,
        )

        output = await pm.ainvoke(input_data)

        assert output.success is False
        assert "Test error" in output.error
