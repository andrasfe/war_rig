"""Integration tests for Program Manager workflow with CardDemo codebase.

These tests verify the full Program Manager orchestration workflow using
real COBOL files from the AWS CardDemo sample application. All tests use
mock agents to avoid LLM calls while exercising the full ticket-based
coordination infrastructure.

Test Categories:
- PM initialization: Batch creation and ticket generation
- Scribe pool: Parallel documentation processing
- Challenger pool: Parallel validation processing
- Full workflow: End-to-end orchestration with mock agents

Prerequisites:
- CardDemo repository must be cloned to ./aws-mainframe-modernization-carddemo/app
- Run: git clone https://github.com/aws-samples/aws-mainframe-modernization-carddemo.git
"""

import asyncio
from datetime import datetime
from pathlib import Path
from unittest.mock import AsyncMock, MagicMock, patch

import pytest

from war_rig.agents.program_manager import (
    ProgramManagerAgent,
    ProgramManagerInput,
)
from war_rig.agents.scribe import ScribeOutput
from war_rig.beads import (
    BeadsClient,
    BeadsPriority,
    ProgramManagerTicket,
    TicketState,
    TicketType,
)
from war_rig.config import WarRigConfig
from war_rig.models.assessments import ConfidenceAssessment, ConfidenceLevel
from war_rig.models.templates import (
    DocumentationTemplate,
    FileType,
    HeaderSection,
    ProgramType,
    PurposeSection,
)
from war_rig.orchestration.ticket_engine import (
    BatchResult,
    OrchestrationStatus,
    TicketOrchestrator,
)
from war_rig.workers.challenger_pool import (
    ChallengerWorker,
    ChallengerWorkerPool,
)
from war_rig.workers.challenger_pool import (
    WorkerState as ChallengerWorkerState,
)
from war_rig.workers.scribe_pool import (
    ScribeWorkerPool,
)

# =============================================================================
# Constants and Configuration
# =============================================================================

CARDDEMO_PATH = Path("aws-mainframe-modernization-carddemo/app")
CARDDEMO_CBL_PATH = CARDDEMO_PATH / "cbl"

# Subset of CardDemo files for testing (to keep tests fast)
TEST_FILES = [
    "CBACT04C.cbl",  # Interest calculator - complex batch
    "CBSTM03B.CBL",  # Statement subroutine - smaller file
    "CBCUS01C.cbl",  # Customer lookup - moderate size
]


# =============================================================================
# Skip Marker for Missing CardDemo
# =============================================================================

carddemo_required = pytest.mark.skipif(
    not CARDDEMO_PATH.exists(),
    reason="CardDemo not cloned - run: git clone https://github.com/aws-samples/aws-mainframe-modernization-carddemo.git"
)


# =============================================================================
# Fixtures - CardDemo Access
# =============================================================================


@pytest.fixture
def carddemo_path() -> Path:
    """Return the CardDemo path if available."""
    if not CARDDEMO_PATH.exists():
        pytest.skip("CardDemo not cloned")
    return CARDDEMO_PATH


@pytest.fixture
def carddemo_cbl_path(carddemo_path) -> Path:
    """Return the CardDemo COBOL source path."""
    return carddemo_path / "cbl"


@pytest.fixture
def sample_carddemo_files(carddemo_cbl_path) -> list[Path]:
    """Return paths to a subset of CardDemo COBOL files for testing."""
    files = []
    for name in TEST_FILES:
        path = carddemo_cbl_path / name
        if path.exists():
            files.append(path)
    return files


# =============================================================================
# Fixtures - Mock BeadsClient
# =============================================================================


@pytest.fixture
def mock_beads_client() -> MagicMock:
    """Create a mock BeadsClient that simulates ticket operations.

    This mock maintains internal state to simulate realistic ticket operations
    including creation, claiming, and state transitions.
    """
    client = MagicMock(spec=BeadsClient)

    # Internal ticket storage
    tickets: dict[str, ProgramManagerTicket] = {}
    ticket_counter = [0]
    claimed_tickets: set[str] = set()

    def create_pm_ticket(
        ticket_type: TicketType,
        file_name: str,
        program_id: str | None = None,
        cycle_number: int = 1,
        parent_ticket_id: str | None = None,
        priority: BeadsPriority = BeadsPriority.MEDIUM,
        metadata: dict | None = None,
    ) -> ProgramManagerTicket:
        """Simulate creating a PM ticket."""
        ticket_counter[0] += 1
        ticket_id = f"war_rig-test{ticket_counter[0]:04d}"

        ticket = ProgramManagerTicket(
            ticket_id=ticket_id,
            ticket_type=ticket_type,
            state=TicketState.CREATED,
            file_name=file_name,
            program_id=program_id,
            cycle_number=cycle_number,
            parent_ticket_id=parent_ticket_id,
            metadata=metadata or {},
        )
        tickets[ticket_id] = ticket
        return ticket

    def get_available_tickets(
        ticket_type: TicketType | None = None,
        cycle_number: int | None = None,
    ) -> list[ProgramManagerTicket]:
        """Return unclaimed tickets in CREATED state."""
        available = []
        for ticket in tickets.values():
            if ticket.state != TicketState.CREATED:
                continue
            if ticket.ticket_id in claimed_tickets:
                continue
            if ticket_type and ticket.ticket_type != ticket_type:
                continue
            if cycle_number and ticket.cycle_number != cycle_number:
                continue
            available.append(ticket)
        return available

    def claim_ticket(ticket_id: str, worker_id: str) -> bool:
        """Simulate atomic ticket claiming."""
        if ticket_id not in tickets:
            return False
        if ticket_id in claimed_tickets:
            return False

        claimed_tickets.add(ticket_id)
        tickets[ticket_id].state = TicketState.CLAIMED
        tickets[ticket_id].worker_id = worker_id
        tickets[ticket_id].claimed_at = datetime.utcnow()
        return True

    def update_ticket_state(
        ticket_id: str,
        new_state: TicketState,
        reason: str | None = None,
    ) -> bool:
        """Update ticket state."""
        if ticket_id not in tickets:
            return False
        tickets[ticket_id].state = new_state
        tickets[ticket_id].updated_at = datetime.utcnow()
        return True

    def get_tickets_by_state(
        state: TicketState,
        ticket_type: TicketType | None = None,
        cycle_number: int | None = None,
    ) -> list[ProgramManagerTicket]:
        """Return tickets in a specific state."""
        result = []
        for ticket in tickets.values():
            if ticket.state != state:
                continue
            if ticket_type and ticket.ticket_type != ticket_type:
                continue
            if cycle_number and ticket.cycle_number != cycle_number:
                continue
            result.append(ticket)
        return result

    # Attach mock implementations
    client.create_pm_ticket.side_effect = create_pm_ticket
    client.get_available_tickets.side_effect = get_available_tickets
    client.claim_ticket.side_effect = claim_ticket
    client.update_ticket_state.side_effect = update_ticket_state
    client.get_tickets_by_state.side_effect = get_tickets_by_state
    client.reset_orphaned_tickets.return_value = 0

    # Expose internals for test assertions
    client._tickets = tickets
    client._claimed_tickets = claimed_tickets
    # Code accesses _pm_ticket_cache directly from BeadsClient
    client._pm_ticket_cache = tickets

    return client


# =============================================================================
# Fixtures - Mock Configuration
# =============================================================================


@pytest.fixture
def mock_war_rig_config(tmp_path, carddemo_path) -> MagicMock:
    """Create a mock WarRigConfig for testing."""
    config = MagicMock(spec=WarRigConfig)

    # Basic settings
    config.rig_id = "TEST_RIG"
    config.num_scribes = 2
    config.num_challengers = 1
    config.max_iterations = 2
    config.max_questions_per_round = 3
    config.max_chrome_tickets = 3
    config.pm_max_cycles = 3
    config.exit_on_error = True
    config.max_ticket_retries = 5
    config.use_mock = True
    config.enable_call_semantics = True

    # Beads settings
    config.beads_enabled = False  # Use mock client
    config.beads_dry_run = True

    # Paths
    config.input_directory = carddemo_path / "cbl"
    config.output_directory = tmp_path / "output"
    config.output_directory.mkdir(exist_ok=True)

    # Agent configs
    config.scribe = MagicMock()
    config.scribe.model = "mock-model"
    config.scribe.temperature = 0.3
    config.scribe.max_prompt_tokens = 15000

    config.challenger = MagicMock()
    config.challenger.model = "mock-model"
    config.challenger.temperature = 0.5

    config.imperator = MagicMock()
    config.imperator.model = "mock-model"
    config.imperator.temperature = 0.2

    # API config
    config.api = MagicMock()
    config.api.provider = "mock"
    config.api.api_key = "mock-key"
    config.api.base_url = "https://mock.api.com"

    # System config
    config.system = MagicMock()
    config.system.input_directory = config.input_directory
    config.system.output_directory = config.output_directory
    config.system.file_extensions = MagicMock()
    config.system.file_extensions.cobol = [".cbl", ".cob", ".CBL", ".COB"]
    config.system.file_extensions.copybook = [".cpy", ".CPY", ".copy", ".COPY"]
    config.system.file_extensions.jcl = [".jcl", ".JCL"]
    config.system.file_extensions.bms = [".bms", ".BMS"]
    config.system.file_extensions.pli = [".pli", ".PLI", ".pl1", ".PL1"]

    return config


# =============================================================================
# Fixtures - Mock Agents
# =============================================================================


@pytest.fixture
def mock_scribe_output() -> ScribeOutput:
    """Create a mock ScribeOutput for testing."""
    template = DocumentationTemplate(
        header=HeaderSection(
            program_id="TESTPROG",
            file_name="TESTPROG.cbl",
            file_type=FileType.COBOL,
            analyzed_by="TEST_RIG",
            analyzed_at=datetime.utcnow(),
            iteration_count=1,
        ),
        purpose=PurposeSection(
            summary="[MOCK] Test program documentation",
            business_context="Generated by mock agent for testing",
            program_type=ProgramType.BATCH,
            citations=[1, 10, 50],
        ),
    )

    confidence = ConfidenceAssessment(
        program_id="TESTPROG",
        iteration=1,
        overall_confidence=ConfidenceLevel.MEDIUM,
        reasoning="Mock output - no actual analysis performed",
    )

    return ScribeOutput(
        success=True,
        template=template,
        confidence=confidence,
        responses=[],
        open_questions=["This is mock output"],
    )


# =============================================================================
# Tests - Program Manager Initialization
# =============================================================================


@pytest.mark.integration
@carddemo_required
class TestPMInitializeBatch:
    """Test Program Manager batch initialization with CardDemo files."""

    def test_pm_initialize_batch_discovers_files(
        self,
        mock_beads_client,
        mock_war_rig_config,
        carddemo_cbl_path,
    ):
        """Test that PM discovers CardDemo COBOL files and creates tickets."""
        # Create PM with mock beads client
        pm = ProgramManagerAgent(
            config=mock_war_rig_config,
            beads_client=mock_beads_client,
        )

        # Initialize batch with CardDemo CBL directory
        tickets = pm.initialize_batch(carddemo_cbl_path)

        # Verify tickets were created
        assert len(tickets) > 0
        assert len(tickets) >= 10  # CardDemo has many COBOL files

        # Verify ticket properties
        for ticket in tickets:
            assert ticket.ticket_type == TicketType.DOCUMENTATION
            assert ticket.state == TicketState.CREATED
            assert ticket.file_name.lower().endswith((".cbl", ".cob"))
            assert ticket.cycle_number == 1

        # Verify batch ID was set
        assert pm.batch_id is not None
        assert pm.batch_id.startswith("batch-")

    def test_pm_initialize_batch_creates_metadata(
        self,
        mock_beads_client,
        mock_war_rig_config,
        carddemo_cbl_path,
    ):
        """Test that PM creates tickets with proper metadata."""
        pm = ProgramManagerAgent(
            config=mock_war_rig_config,
            beads_client=mock_beads_client,
        )

        tickets = pm.initialize_batch(carddemo_cbl_path)

        # Check metadata on first ticket
        ticket = tickets[0]
        assert "batch_id" in ticket.metadata
        assert "file_path" in ticket.metadata
        assert "file_type" in ticket.metadata
        assert ticket.metadata["file_type"] == "COBOL"

    def test_pm_batch_status_after_init(
        self,
        mock_beads_client,
        mock_war_rig_config,
        carddemo_cbl_path,
    ):
        """Test batch status reporting after initialization."""
        pm = ProgramManagerAgent(
            config=mock_war_rig_config,
            beads_client=mock_beads_client,
        )

        tickets = pm.initialize_batch(carddemo_cbl_path)
        status = pm.get_batch_status()

        # All tickets should be in CREATED state
        assert status.get(TicketState.CREATED.value, 0) >= len(tickets)
        assert status.get(TicketState.COMPLETED.value, 0) == 0
        assert status.get(TicketState.IN_PROGRESS.value, 0) == 0

    @pytest.mark.asyncio
    async def test_pm_ainvoke_creates_output(
        self,
        mock_beads_client,
        mock_war_rig_config,
        carddemo_cbl_path,
    ):
        """Test PM async invocation creates proper output."""
        pm = ProgramManagerAgent(
            config=mock_war_rig_config,
            beads_client=mock_beads_client,
        )

        input_data = ProgramManagerInput(
            input_directory=carddemo_cbl_path,
            recursive=False,
        )

        output = await pm.ainvoke(input_data)

        assert output.success is True
        assert output.files_discovered > 0
        assert output.tickets_created > 0
        assert output.batch_id != ""
        assert len(output.source_files) > 0


# =============================================================================
# Tests - Scribe Worker Pool
# =============================================================================


@pytest.mark.integration
@carddemo_required
class TestScribePoolProcessesFiles:
    """Test Scribe worker pool processes CardDemo files."""

    @pytest.mark.asyncio
    async def test_scribe_pool_processes_tickets(
        self,
        mock_beads_client,
        mock_war_rig_config,
        sample_carddemo_files,
        mock_scribe_output,
    ):
        """Test that Scribe pool processes documentation tickets.

        This test verifies that ScribeWorkerPool can claim and process tickets
        when they are available. We use mocking to avoid actual LLM calls.
        """
        # Create tickets for sample files BEFORE starting the pool
        for file_path in sample_carddemo_files[:2]:
            source_code = file_path.read_text()
            mock_beads_client.create_pm_ticket(
                ticket_type=TicketType.DOCUMENTATION,
                file_name=file_path.name,
                program_id=file_path.stem.upper(),
                metadata={"source_code": source_code},
            )

        # Patch the ScribeAgent BEFORE creating the pool
        with patch(
            "war_rig.workers.scribe_pool.ScribeAgent"
        ) as MockScribeAgent:
            mock_agent_instance = MagicMock()
            mock_agent_instance.ainvoke = AsyncMock(return_value=mock_scribe_output)
            MockScribeAgent.return_value = mock_agent_instance

            # Create pool within the patch context
            pool = ScribeWorkerPool(
                config=mock_war_rig_config,
                beads_client=mock_beads_client,
                num_workers=2,
                poll_interval=0.1,
                idle_timeout=2.0,  # Longer timeout to ensure processing
            )

            await pool.start()
            await pool.wait()

            # Get status before stopping
            status = pool.get_status()

            await pool.stop()

        # Verify tickets were processed
        assert status["total_processed"] >= 1

    @pytest.mark.asyncio
    async def test_scribe_pool_creates_workers(
        self,
        mock_beads_client,
        mock_war_rig_config,
    ):
        """Test that Scribe pool creates correct number of workers."""
        pool = ScribeWorkerPool(
            config=mock_war_rig_config,
            beads_client=mock_beads_client,
            num_workers=3,
            poll_interval=0.1,
            idle_timeout=0.5,
        )

        await pool.start()

        status = pool.get_status()
        assert status["started"] is True
        assert status["num_workers"] == 3
        assert len(status["workers"]) == 3

        await pool.wait()
        await pool.stop()

    @pytest.mark.asyncio
    async def test_scribe_pool_stops_when_no_tickets(
        self,
        mock_beads_client,
        mock_war_rig_config,
    ):
        """Test that Scribe pool stops when no tickets available."""
        # Don't create any tickets
        pool = ScribeWorkerPool(
            config=mock_war_rig_config,
            beads_client=mock_beads_client,
            num_workers=1,
            poll_interval=0.1,
            idle_timeout=0.3,
        )

        await pool.start()
        await pool.wait()

        assert pool.is_done() is True

        status = pool.get_status()
        assert status["total_processed"] == 0

        await pool.stop()


# =============================================================================
# Tests - Challenger Worker Pool
# =============================================================================


@pytest.mark.integration
@carddemo_required
class TestChallengerPoolValidates:
    """Test Challenger worker pool validates documentation."""

    @pytest.mark.asyncio
    async def test_challenger_pool_creates_workers(
        self,
        mock_beads_client,
        mock_war_rig_config,
    ):
        """Test that Challenger pool creates correct number of workers."""
        pool = ChallengerWorkerPool(
            num_workers=2,
            config=mock_war_rig_config,
            beads_client=mock_beads_client,
            poll_interval=0.1,
        )

        await pool.start()

        status = pool.get_status()
        assert status["running"] is True
        assert status["num_workers"] == 2
        assert len(status["workers"]) == 2

        await pool.stop()

    @pytest.mark.asyncio
    async def test_challenger_pool_stops_when_no_tickets(
        self,
        mock_beads_client,
        mock_war_rig_config,
    ):
        """Test that Challenger pool stops when no VALIDATION tickets available."""
        pool = ChallengerWorkerPool(
            num_workers=1,
            config=mock_war_rig_config,
            beads_client=mock_beads_client,
            poll_interval=0.1,
        )

        await pool.start()

        # Wait briefly for workers to poll and find no tickets
        await asyncio.sleep(0.5)

        status = pool.get_status()
        assert status["total_tickets_processed"] == 0

        await pool.stop()

    @pytest.mark.asyncio
    async def test_challenger_worker_status_tracking(
        self,
        mock_beads_client,
        mock_war_rig_config,
    ):
        """Test that Challenger workers track status correctly."""
        worker = ChallengerWorker(
            worker_id="challenger-test",
            config=mock_war_rig_config,
            beads_client=mock_beads_client,
            poll_interval=0.1,
        )

        initial_status = worker.get_status()
        assert initial_status.worker_id == "challenger-test"
        assert initial_status.state == ChallengerWorkerState.IDLE
        assert initial_status.tickets_processed == 0


# =============================================================================
# Tests - Full Workflow Mock
# =============================================================================


@pytest.mark.integration
@carddemo_required
class TestFullWorkflowMock:
    """Test complete workflow with mock agents."""

    @pytest.mark.asyncio
    async def test_full_workflow_mock_single_file(
        self,
        mock_beads_client,
        mock_war_rig_config,
        sample_carddemo_files,
        mock_scribe_output,
    ):
        """Test full workflow with a single CardDemo file using mocks."""
        # Use first sample file
        file_path = sample_carddemo_files[0]
        source_code = file_path.read_text()

        # Create a documentation ticket
        ticket = mock_beads_client.create_pm_ticket(
            ticket_type=TicketType.DOCUMENTATION,
            file_name=file_path.name,
            program_id=file_path.stem.upper(),
            metadata={"source_code": source_code},
        )

        # Verify ticket was created
        assert ticket is not None
        assert ticket.state == TicketState.CREATED

        # Simulate claiming the ticket
        claim_result = mock_beads_client.claim_ticket(ticket.ticket_id, "scribe-1")
        assert claim_result is True

        # Verify claim
        assert ticket.ticket_id in mock_beads_client._claimed_tickets

        # Simulate processing and completion
        mock_beads_client.update_ticket_state(
            ticket.ticket_id,
            TicketState.IN_PROGRESS,
        )

        mock_beads_client.update_ticket_state(
            ticket.ticket_id,
            TicketState.COMPLETED,
            reason="Documentation completed",
        )

        # Verify final state
        completed_tickets = mock_beads_client.get_tickets_by_state(
            TicketState.COMPLETED,
            ticket_type=TicketType.DOCUMENTATION,
        )
        assert len(completed_tickets) == 1
        assert completed_tickets[0].file_name == file_path.name

    @pytest.mark.asyncio
    async def test_full_workflow_with_validation(
        self,
        mock_beads_client,
        mock_war_rig_config,
        sample_carddemo_files,
    ):
        """Test workflow with documentation and validation tickets."""
        file_path = sample_carddemo_files[0]
        source_code = file_path.read_text()

        # Step 1: Create DOCUMENTATION ticket
        doc_ticket = mock_beads_client.create_pm_ticket(
            ticket_type=TicketType.DOCUMENTATION,
            file_name=file_path.name,
            program_id=file_path.stem.upper(),
            metadata={"source_code": source_code},
        )

        # Step 2: Scribe claims and processes
        mock_beads_client.claim_ticket(doc_ticket.ticket_id, "scribe-1")
        mock_beads_client.update_ticket_state(doc_ticket.ticket_id, TicketState.IN_PROGRESS)
        mock_beads_client.update_ticket_state(doc_ticket.ticket_id, TicketState.COMPLETED)

        # Step 3: Create VALIDATION ticket
        val_ticket = mock_beads_client.create_pm_ticket(
            ticket_type=TicketType.VALIDATION,
            file_name=file_path.name,
            program_id=file_path.stem.upper(),
            parent_ticket_id=doc_ticket.ticket_id,
            metadata={
                "source_code": source_code,
                "template": "{}",  # Simplified for test
            },
        )

        # Step 4: Challenger claims and processes
        mock_beads_client.claim_ticket(val_ticket.ticket_id, "challenger-1")
        mock_beads_client.update_ticket_state(val_ticket.ticket_id, TicketState.IN_PROGRESS)
        mock_beads_client.update_ticket_state(val_ticket.ticket_id, TicketState.COMPLETED)

        # Verify both tickets completed
        completed_docs = mock_beads_client.get_tickets_by_state(
            TicketState.COMPLETED,
            ticket_type=TicketType.DOCUMENTATION,
        )
        completed_vals = mock_beads_client.get_tickets_by_state(
            TicketState.COMPLETED,
            ticket_type=TicketType.VALIDATION,
        )

        assert len(completed_docs) == 1
        assert len(completed_vals) == 1

    @pytest.mark.asyncio
    async def test_workflow_with_clarification_cycle(
        self,
        mock_beads_client,
        mock_war_rig_config,
        sample_carddemo_files,
    ):
        """Test workflow that requires clarification (multi-cycle)."""
        file_path = sample_carddemo_files[0]
        source_code = file_path.read_text()

        # Cycle 1: Documentation
        doc_ticket = mock_beads_client.create_pm_ticket(
            ticket_type=TicketType.DOCUMENTATION,
            file_name=file_path.name,
            program_id=file_path.stem.upper(),
            cycle_number=1,
            metadata={"source_code": source_code},
        )

        mock_beads_client.claim_ticket(doc_ticket.ticket_id, "scribe-1")
        mock_beads_client.update_ticket_state(doc_ticket.ticket_id, TicketState.COMPLETED)

        # Validation finds issues - create CLARIFICATION ticket for cycle 2
        clar_ticket = mock_beads_client.create_pm_ticket(
            ticket_type=TicketType.CLARIFICATION,
            file_name=file_path.name,
            program_id=file_path.stem.upper(),
            cycle_number=2,
            parent_ticket_id=doc_ticket.ticket_id,
            metadata={
                "issue_description": "Missing error handling documentation",
                "source_code": source_code,
            },
        )

        # Scribe addresses clarification
        mock_beads_client.claim_ticket(clar_ticket.ticket_id, "scribe-1")
        mock_beads_client.update_ticket_state(clar_ticket.ticket_id, TicketState.COMPLETED)

        # Verify multi-cycle operation
        cycle_1_tickets = mock_beads_client.get_tickets_by_state(
            TicketState.COMPLETED,
            cycle_number=1,
        )
        cycle_2_tickets = mock_beads_client.get_tickets_by_state(
            TicketState.COMPLETED,
            cycle_number=2,
        )

        assert len(cycle_1_tickets) == 1
        assert len(cycle_2_tickets) == 1


# =============================================================================
# Tests - Ticket Orchestrator
# =============================================================================


@pytest.mark.integration
@carddemo_required
class TestTicketOrchestratorCarddemo:
    """Test TicketOrchestrator end-to-end with CardDemo."""

    def test_orchestrator_initialization(
        self,
        mock_beads_client,
        mock_war_rig_config,
    ):
        """Test TicketOrchestrator initializes correctly."""
        orchestrator = TicketOrchestrator(
            config=mock_war_rig_config,
            beads_client=mock_beads_client,
            use_mock=True,
        )

        assert orchestrator.config == mock_war_rig_config
        assert orchestrator.beads_client == mock_beads_client
        assert orchestrator.use_mock is True
        assert orchestrator.state.status == OrchestrationStatus.IDLE

    def test_orchestrator_get_status(
        self,
        mock_beads_client,
        mock_war_rig_config,
    ):
        """Test TicketOrchestrator status reporting."""
        orchestrator = TicketOrchestrator(
            config=mock_war_rig_config,
            beads_client=mock_beads_client,
            use_mock=True,
        )

        status = orchestrator.get_status()

        assert "batch_id" in status
        assert "cycle" in status
        assert "status" in status
        assert "total_files" in status
        assert status["status"] == "idle"

    @pytest.mark.asyncio
    async def test_orchestrator_stop(
        self,
        mock_beads_client,
        mock_war_rig_config,
    ):
        """Test TicketOrchestrator graceful stop."""
        orchestrator = TicketOrchestrator(
            config=mock_war_rig_config,
            beads_client=mock_beads_client,
            use_mock=True,
        )

        await orchestrator.stop()

        assert orchestrator.state.status == OrchestrationStatus.STOPPED

    @pytest.mark.asyncio
    async def test_orchestrator_handles_empty_directory(
        self,
        mock_beads_client,
        mock_war_rig_config,
        tmp_path,
    ):
        """Test TicketOrchestrator handles empty input directory."""
        empty_dir = tmp_path / "empty"
        empty_dir.mkdir()

        orchestrator = TicketOrchestrator(
            config=mock_war_rig_config,
            beads_client=mock_beads_client,
            use_mock=True,
        )

        result = await orchestrator.run_batch(empty_dir)

        assert result.final_decision == "NO_FILES"
        assert len(result.completed_files) == 0

    @pytest.mark.asyncio
    async def test_orchestrator_raises_for_missing_directory(
        self,
        mock_beads_client,
        mock_war_rig_config,
        tmp_path,
    ):
        """Test TicketOrchestrator raises error for non-existent directory."""
        missing_dir = tmp_path / "does_not_exist"

        orchestrator = TicketOrchestrator(
            config=mock_war_rig_config,
            beads_client=mock_beads_client,
            use_mock=True,
        )

        with pytest.raises(ValueError, match="does not exist"):
            await orchestrator.run_batch(missing_dir)

    @pytest.mark.asyncio
    async def test_orchestrator_batch_result_structure(
        self,
        mock_beads_client,
        mock_war_rig_config,
        tmp_path,
    ):
        """Test that BatchResult has correct structure."""
        # Create empty directory for quick test
        empty_dir = tmp_path / "test_dir"
        empty_dir.mkdir()

        orchestrator = TicketOrchestrator(
            config=mock_war_rig_config,
            beads_client=mock_beads_client,
            use_mock=True,
        )

        result = await orchestrator.run_batch(empty_dir)

        # Verify BatchResult structure
        assert isinstance(result, BatchResult)
        assert isinstance(result.completed_files, list)
        assert isinstance(result.failed_files, list)
        assert isinstance(result.total_cycles, int)
        assert isinstance(result.documentation_outputs, dict)

        # Test to_dict method
        result_dict = result.to_dict()
        assert "completed_files" in result_dict
        assert "failed_files" in result_dict
        assert "total_cycles" in result_dict
        assert "duration_seconds" in result_dict


# =============================================================================
# Tests - Integration with Real File Content
# =============================================================================


@pytest.mark.integration
@carddemo_required
class TestRealFileContent:
    """Tests that use real CardDemo file content."""

    def test_read_cbact04c_content(self, carddemo_cbl_path):
        """Test reading CBACT04C.cbl content."""
        file_path = carddemo_cbl_path / "CBACT04C.cbl"

        if not file_path.exists():
            pytest.skip("CBACT04C.cbl not found")

        content = file_path.read_text()

        # Verify it's valid COBOL
        assert "IDENTIFICATION DIVISION" in content
        assert "PROGRAM-ID" in content
        assert "PROCEDURE DIVISION" in content

    def test_read_multiple_carddemo_files(self, sample_carddemo_files):
        """Test reading multiple CardDemo files."""
        for file_path in sample_carddemo_files:
            content = file_path.read_text()

            # Each should be valid COBOL
            assert "IDENTIFICATION DIVISION" in content
            assert len(content) > 100  # Non-trivial content

    def test_carddemo_file_sizes(self, carddemo_cbl_path):
        """Test that CardDemo files have expected sizes."""
        files = list(carddemo_cbl_path.glob("*.cbl")) + list(carddemo_cbl_path.glob("*.CBL"))

        assert len(files) > 10  # Expect many COBOL files

        # Verify files have content
        for file_path in files[:5]:
            assert file_path.stat().st_size > 0
