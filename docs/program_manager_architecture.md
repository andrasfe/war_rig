# Program Manager Architecture

## Overview

This document describes the architecture for the Program Manager enhancement to War Rig, enabling parallel processing of multiple source files through ticket-based coordination.

### Current System

The existing War Rig system processes files sequentially using a single-file LangGraph workflow:

```
preprocess -> scribe -> challenger -> imperator -> [loop or end]
```

Each file goes through the Scribe-Challenger-Imperator loop independently, with the Imperator making per-file approval decisions.

### New Requirements

The Program Manager introduces:

1. **Batch initialization**: Open beads tickets for all source files at start
2. **Parallel Scribes**: Multiple Scribe workers (default 3) pick available tickets
3. **Parallel Challengers**: Multiple Challenger workers (default 2) process Scribe-completed work
4. **Holistic review**: Imperator reviews the entire batch when all tickets close
5. **Cycle management**: Imperator can open new tickets, restarting the cycle
6. **Termination**: Max cycles or Imperator satisfaction

---

## 1. Ticket Types and State Machine

### 1.1 Ticket Types

The Program Manager uses distinct ticket types to track different work items:

| Ticket Type | Description | Created By | Assigned To |
|-------------|-------------|------------|-------------|
| `DOCUMENTATION` | Initial documentation task for a source file | Program Manager | Scribe |
| `VALIDATION` | Validation task for completed documentation | System (auto) | Challenger |
| `CLARIFICATION` | Question from Challenger requiring Scribe response | Challenger | Scribe |
| `CHROME` | Issue ticket from Imperator requiring rework | Imperator | Scribe |
| `HOLISTIC_REVIEW` | Batch review task for Imperator | System (auto) | Imperator |

### 1.2 Ticket Schema Extension

Extend `BeadsTicket` in `/Users/andraslferenczi/war_rig/war_rig/beads.py`:

```python
class ProgramManagerTicketType(str, Enum):
    """Extended ticket types for Program Manager workflow."""

    DOCUMENTATION = "documentation"      # Initial file documentation
    VALIDATION = "validation"            # Challenger validation
    CLARIFICATION = "clarification"      # Challenger question
    CHROME = "chrome"                    # Imperator issue
    HOLISTIC_REVIEW = "holistic_review"  # Batch review


@dataclass
class ProgramManagerTicket:
    """Extended ticket for Program Manager workflow."""

    id: str
    ticket_type: ProgramManagerTicketType
    status: TicketStatus
    priority: BeadsPriority

    # File association
    file_name: str
    program_id: str | None = None

    # Workflow tracking
    cycle: int = 1
    parent_ticket_id: str | None = None  # For CLARIFICATION/CHROME tickets

    # Assignment
    assigned_worker: str | None = None
    assigned_at: datetime | None = None

    # Results
    documentation_state: str | None = None  # JSON serialized WarRigState

    # Metadata
    created_at: datetime = field(default_factory=datetime.utcnow)
    updated_at: datetime = field(default_factory=datetime.utcnow)
```

### 1.3 Ticket State Machine

```
                                    +-----------+
                                    |  CREATED  |
                                    +-----+-----+
                                          |
                                    (worker picks)
                                          |
                                          v
                                    +-----+-----+
                                    |  CLAIMED  |
                                    +-----+-----+
                                          |
                                   (work starts)
                                          |
                                          v
                                    +-----+-----+
                                    |IN_PROGRESS|
                                    +-----+-----+
                                          |
                         +----------------+----------------+
                         |                |                |
                   (completed)     (needs rework)    (blocked)
                         |                |                |
                         v                v                v
                   +-----+-----+    +-----+-----+    +-----+-----+
                   | COMPLETED |    |  REWORK   |    |  BLOCKED  |
                   +-----------+    +-----+-----+    +-----+-----+
                                          |                |
                                    (new cycle)     (dependency
                                          |          resolved)
                                          |                |
                                          +-------+--------+
                                                  |
                                                  v
                                            +-----+-----+
                                            |  CREATED  |
                                            +-----------+

Terminal States:
  - COMPLETED: Work done successfully
  - CANCELLED: Ticket no longer needed
  - MERGED: Combined into another ticket
```

**State Definitions:**

| State | Description |
|-------|-------------|
| `CREATED` | Ticket created, waiting for worker |
| `CLAIMED` | Worker has claimed ticket, preparing to start |
| `IN_PROGRESS` | Worker actively processing |
| `COMPLETED` | Work finished successfully |
| `REWORK` | Needs additional work (new cycle) |
| `BLOCKED` | Waiting on dependency |
| `CANCELLED` | Ticket no longer relevant |
| `MERGED` | Combined with another ticket |

**State Transitions:**

| From | To | Trigger | Actor |
|------|-----|---------|-------|
| CREATED | CLAIMED | Worker picks ticket | Worker |
| CLAIMED | IN_PROGRESS | Work begins | Worker |
| IN_PROGRESS | COMPLETED | Work finished | Worker |
| IN_PROGRESS | BLOCKED | Dependency identified | Worker |
| COMPLETED | REWORK | Imperator issues CHROME | Imperator |
| REWORK | CREATED | New cycle starts | Program Manager |
| BLOCKED | CREATED | Dependency resolved | System |

---

## 2. Worker Pool Architecture

### 2.1 Worker Types

```python
class WorkerType(str, Enum):
    """Types of workers in the pool."""

    SCRIBE = "scribe"
    CHALLENGER = "challenger"
    IMPERATOR = "imperator"


@dataclass
class WorkerConfig:
    """Configuration for worker pools."""

    scribe_count: int = 3        # Number of parallel Scribes
    challenger_count: int = 2    # Number of parallel Challengers
    imperator_count: int = 1     # Single Imperator (holistic review)

    # Ticket selection
    max_concurrent_per_worker: int = 1
    claim_timeout_seconds: int = 300

    # Retry behavior
    max_retries: int = 2
    retry_delay_seconds: int = 30
```

### 2.2 Worker Pool Manager

The Worker Pool Manager coordinates workers and ticket assignment:

```python
class WorkerPoolManager:
    """Manages worker pools for parallel processing.

    Responsibilities:
    - Spawn and manage worker tasks
    - Coordinate ticket claiming (prevent double-claims)
    - Track worker health and progress
    - Handle worker failures and retries
    """

    def __init__(
        self,
        config: WorkerConfig,
        beads_client: BeadsClient,
        war_rig_config: WarRigConfig,
    ):
        self.config = config
        self.beads = beads_client
        self.war_rig_config = war_rig_config

        # Worker tracking
        self._workers: dict[str, WorkerTask] = {}
        self._claim_lock = asyncio.Lock()

    async def start_workers(self) -> None:
        """Start all worker pools."""

    async def stop_workers(self) -> None:
        """Gracefully stop all workers."""

    async def claim_ticket(
        self,
        worker_id: str,
        worker_type: WorkerType,
    ) -> ProgramManagerTicket | None:
        """Atomically claim an available ticket for a worker."""

    async def release_ticket(
        self,
        worker_id: str,
        ticket_id: str,
        new_status: TicketStatus,
    ) -> None:
        """Release a ticket after processing."""
```

### 2.3 Ticket Selection Strategy

Workers select tickets based on priority and type compatibility:

```python
class TicketSelector:
    """Determines which tickets a worker can pick.

    Selection priority:
    1. Higher priority tickets first (CRITICAL > HIGH > MEDIUM > LOW)
    2. Older tickets first (FIFO within priority)
    3. Type compatibility (Scribes get DOCUMENTATION/CLARIFICATION/CHROME,
                          Challengers get VALIDATION)
    """

    # Ticket types each worker can process
    WORKER_TICKET_TYPES = {
        WorkerType.SCRIBE: [
            ProgramManagerTicketType.DOCUMENTATION,
            ProgramManagerTicketType.CLARIFICATION,
            ProgramManagerTicketType.CHROME,
        ],
        WorkerType.CHALLENGER: [
            ProgramManagerTicketType.VALIDATION,
        ],
        WorkerType.IMPERATOR: [
            ProgramManagerTicketType.HOLISTIC_REVIEW,
        ],
    }

    def get_next_ticket(
        self,
        worker_type: WorkerType,
        available_tickets: list[ProgramManagerTicket],
    ) -> ProgramManagerTicket | None:
        """Select the next ticket for a worker to process."""

        # Filter by compatible types
        compatible = [
            t for t in available_tickets
            if t.ticket_type in self.WORKER_TICKET_TYPES[worker_type]
            and t.status == TicketStatus.CREATED
        ]

        if not compatible:
            return None

        # Sort by priority (lower value = higher priority), then by created_at
        compatible.sort(key=lambda t: (t.priority.value, t.created_at))

        return compatible[0]
```

### 2.4 Worker Implementation

Each worker type follows a common pattern:

```python
class BaseWorker(ABC):
    """Base class for all workers."""

    def __init__(
        self,
        worker_id: str,
        worker_type: WorkerType,
        pool_manager: WorkerPoolManager,
        war_rig_config: WarRigConfig,
    ):
        self.worker_id = worker_id
        self.worker_type = worker_type
        self.pool_manager = pool_manager
        self.config = war_rig_config

    async def run(self) -> None:
        """Main worker loop."""
        while not self._should_stop:
            # Try to claim a ticket
            ticket = await self.pool_manager.claim_ticket(
                self.worker_id,
                self.worker_type,
            )

            if ticket is None:
                # No work available, wait and retry
                await asyncio.sleep(self.config.poll_interval)
                continue

            try:
                # Process the ticket
                result = await self.process_ticket(ticket)

                # Update ticket status
                await self.pool_manager.release_ticket(
                    self.worker_id,
                    ticket.id,
                    TicketStatus.COMPLETED if result.success else TicketStatus.BLOCKED,
                )

            except Exception as e:
                logger.error(f"Worker {self.worker_id} failed: {e}")
                await self.pool_manager.release_ticket(
                    self.worker_id,
                    ticket.id,
                    TicketStatus.BLOCKED,
                )

    @abstractmethod
    async def process_ticket(self, ticket: ProgramManagerTicket) -> WorkerResult:
        """Process a single ticket. Implemented by subclasses."""
        pass


class ScribeWorker(BaseWorker):
    """Scribe worker that processes documentation tickets."""

    async def process_ticket(self, ticket: ProgramManagerTicket) -> WorkerResult:
        """Process a DOCUMENTATION, CLARIFICATION, or CHROME ticket."""

        # Load the existing state if available
        state = self._load_state(ticket)

        # Run the appropriate part of the workflow
        if ticket.ticket_type == ProgramManagerTicketType.DOCUMENTATION:
            return await self._initial_documentation(ticket, state)
        elif ticket.ticket_type == ProgramManagerTicketType.CLARIFICATION:
            return await self._respond_to_clarification(ticket, state)
        elif ticket.ticket_type == ProgramManagerTicketType.CHROME:
            return await self._address_chrome(ticket, state)


class ChallengerWorker(BaseWorker):
    """Challenger worker that validates completed documentation."""

    async def process_ticket(self, ticket: ProgramManagerTicket) -> WorkerResult:
        """Process a VALIDATION ticket."""

        # Load the documentation state from parent ticket
        state = self._load_state(ticket)

        # Run challenger validation
        output = await self.challenger.ainvoke(
            ChallengerInput(
                template=state["current_template"],
                source_code=state["source_code"],
                file_name=state["file_name"],
                file_type=state["file_type"],
                preprocessor_result=state.get("preprocessor_result"),
                iteration=state.get("iteration", 1),
            )
        )

        # Create CLARIFICATION tickets for blocking questions
        clarification_tickets = []
        for question in output.questions:
            if question.severity == QuestionSeverity.BLOCKING:
                clarification_tickets.append(
                    self._create_clarification_ticket(ticket, question)
                )

        return WorkerResult(
            success=output.success,
            output=output,
            child_tickets=clarification_tickets,
        )
```

---

## 3. Orchestration Flow

### 3.1 Program Manager Component

The Program Manager orchestrates the entire batch workflow:

```python
class ProgramManager:
    """Orchestrates parallel documentation of multiple source files.

    Lifecycle:
    1. Initialize batch with source files
    2. Create DOCUMENTATION tickets for all files
    3. Start worker pools
    4. Monitor progress
    5. Trigger holistic review when all files processed
    6. Handle Imperator feedback (new cycle or completion)
    7. Terminate on max cycles or satisfaction
    """

    def __init__(
        self,
        config: ProgramManagerConfig,
        war_rig_config: WarRigConfig,
        beads_client: BeadsClient,
    ):
        self.config = config
        self.war_rig_config = war_rig_config
        self.beads = beads_client

        self.batch_id: str | None = None
        self.cycle: int = 0
        self.worker_pool: WorkerPoolManager | None = None

    async def run_batch(
        self,
        source_files: list[SourceFile],
    ) -> BatchResult:
        """Run the complete batch documentation workflow."""

        self.batch_id = self._generate_batch_id()

        # Phase 1: Initialize
        await self._initialize_batch(source_files)

        while self.cycle < self.config.max_cycles:
            self.cycle += 1

            # Phase 2: Create initial tickets (or rework tickets)
            await self._create_cycle_tickets(source_files)

            # Phase 3: Run workers until all tickets complete
            await self._run_worker_cycle()

            # Phase 4: Holistic review
            imperator_decision = await self._holistic_review()

            if imperator_decision.satisfied:
                # Success!
                return BatchResult(
                    success=True,
                    cycle=self.cycle,
                    decision=imperator_decision,
                    documentation=self._collect_documentation(),
                )

            # Imperator not satisfied - prepare for next cycle
            await self._prepare_rework(imperator_decision)

        # Max cycles reached
        return BatchResult(
            success=False,
            cycle=self.cycle,
            decision=ImperatorDecision.FORCED,
            documentation=self._collect_documentation(),
        )
```

### 3.2 Batch Workflow Diagram

```
+----------------+
| Program Manager|
+-------+--------+
        |
        | 1. Initialize batch
        v
+-------+--------+     +------------------+
| Create DOCUMENTATION |<---+ Source Files |
| tickets for all files|     +------------------+
+-------+--------+
        |
        | 2. Start workers
        v
+-------+--------+     +------------------+     +------------------+
| Scribe Pool    |     | Challenger Pool  |     | Ticket Queue     |
| (3 workers)    |<--->| (2 workers)      |<--->| (managed by bd)  |
+-------+--------+     +------------------+     +------------------+
        |                      |
        | 3. Workers process tickets until queue empty
        v                      v
+-------+--------+     +------------------+
| Documentation  |---->| VALIDATION       |
| COMPLETED      |     | tickets created  |
+----------------+     +------------------+
        |                      |
        | 4. All tickets completed
        v
+-------+--------+
| Create HOLISTIC|
| REVIEW ticket  |
+-------+--------+
        |
        v
+-------+--------+
| Imperator      |
| (single worker)|
+-------+--------+
        |
        +----------+----------+
        |                     |
        v                     v
  [Satisfied]           [Not Satisfied]
        |                     |
        v                     v
+-------+--------+     +-------+--------+
| BATCH COMPLETE |     | Create CHROME  |
+----------------+     | tickets, new   |
                       | cycle          |
                       +----------------+
```

### 3.3 Workflow State

```python
@dataclass
class BatchState:
    """State tracking for a batch run."""

    batch_id: str
    cycle: int
    status: BatchStatus

    # File tracking
    source_files: list[SourceFile]
    file_states: dict[str, WarRigState]  # file_name -> state

    # Ticket tracking
    active_tickets: list[str]  # ticket IDs
    completed_tickets: list[str]

    # Progress
    files_documented: int = 0
    files_validated: int = 0
    files_approved: int = 0

    # Timing
    started_at: datetime
    completed_at: datetime | None = None

    # Results
    final_decision: ImperatorDecision | None = None
    imperator_feedback: list[ChromeTicket] = field(default_factory=list)


class BatchStatus(str, Enum):
    """Batch processing status."""

    INITIALIZING = "initializing"
    DOCUMENTING = "documenting"    # Scribes working
    VALIDATING = "validating"      # Challengers working
    REVIEWING = "reviewing"        # Imperator holistic review
    REWORK = "rework"              # Processing Imperator feedback
    COMPLETED = "completed"
    FAILED = "failed"
```

---

## 4. Imperator Holistic Review Mode

### 4.1 Review Trigger

The holistic review is triggered when all DOCUMENTATION and VALIDATION tickets are completed:

```python
class HolisticReviewTrigger:
    """Determines when to trigger Imperator holistic review."""

    async def should_trigger(
        self,
        batch_state: BatchState,
        beads_client: BeadsClient,
    ) -> bool:
        """Check if all file-level work is complete."""

        # Get all tickets for this batch
        tickets = await beads_client.get_batch_tickets(batch_state.batch_id)

        # Check that no work tickets are pending
        work_tickets = [
            t for t in tickets
            if t.ticket_type in (
                ProgramManagerTicketType.DOCUMENTATION,
                ProgramManagerTicketType.VALIDATION,
                ProgramManagerTicketType.CLARIFICATION,
                ProgramManagerTicketType.CHROME,
            )
        ]

        pending = [t for t in work_tickets if t.status != TicketStatus.COMPLETED]

        return len(pending) == 0
```

**Force review override:** If tickets are stuck in non-terminal states (e.g., due to
provider errors), the pending-ticket check can be bypassed by creating a signal file:

```bash
touch <output_dir>/.force_review
```

The signal file is consumed (deleted) when detected. See the "Force Holistic Review"
section in the main README for details.

### 4.2 Holistic Review Input

The Imperator receives aggregated information for batch review:

```python
@dataclass
class HolisticReviewInput:
    """Input for Imperator holistic review."""

    batch_id: str
    cycle: int

    # All documentation
    file_documentation: list[FileDocumentation]

    # Aggregated assessments
    challenger_assessments: list[ChallengerAssessment]

    # Cross-file analysis
    shared_copybooks: dict[str, list[str]]  # copybook -> [files using it]
    call_graph: dict[str, list[str]]        # program -> [called programs]
    data_flow: dict[str, list[str]]         # file -> [input/output files]

    # Quality metrics
    per_file_confidence: dict[str, ConfidenceLevel]
    per_file_issues: dict[str, list[str]]

    # Previous feedback (if cycle > 1)
    previous_chrome_tickets: list[ChromeTicket]
    resolution_status: dict[str, bool]  # ticket_id -> resolved


@dataclass
class FileDocumentation:
    """Documentation for a single file."""

    file_name: str
    program_id: str
    template: DocumentationTemplate
    scribe_confidence: ConfidenceAssessment
    challenger_assessment: ChallengerAssessment
    iteration_count: int
```

### 4.3 Holistic Review Output

```python
@dataclass
class HolisticReviewOutput:
    """Output from Imperator holistic review."""

    decision: HolisticDecision

    # Per-file feedback
    file_feedback: dict[str, list[ChromeTicket]]  # file_name -> tickets

    # Cross-file issues
    consistency_issues: list[ConsistencyIssue]
    missing_documentation: list[str]  # file_names that need more work

    # Quality assessment
    overall_quality: QualityLevel
    quality_notes: list[str]

    # Recommendations
    priority_files: list[str]  # files to focus on in next cycle


class HolisticDecision(str, Enum):
    """Possible holistic review decisions."""

    BATCH_WITNESSED = "batch_witnessed"      # All approved
    BATCH_VALHALLA = "batch_valhalla"        # Exceptional quality
    NEEDS_REWORK = "needs_rework"            # Issues to address
    BATCH_FORCED = "batch_forced"            # Max cycles, accept as-is


@dataclass
class ConsistencyIssue:
    """Cross-file consistency issue identified by Imperator."""

    issue_type: str  # NAMING, DATA_FLOW, INTERFACE, BUSINESS_RULE
    description: str
    affected_files: list[str]
    guidance: str
```

### 4.4 Extended Imperator Agent

```python
class ImperatorHolisticAgent(ImperatorAgent):
    """Extended Imperator for holistic batch review.

    Additional responsibilities:
    - Cross-file consistency checking
    - Data flow validation across programs
    - Interface documentation verification
    - Batch-level quality assessment
    """

    def _build_holistic_system_prompt(self) -> str:
        """System prompt for holistic review mode."""
        return """You are the Imperator conducting a holistic review of documentation for multiple programs.

## Holistic Review Criteria

Beyond individual file quality, evaluate:

1. **Consistency**: Are similar patterns documented consistently?
2. **Data Flow**: Are input/output relationships between programs documented?
3. **Call Chains**: Are CALL relationships properly documented on both ends?
4. **Shared Resources**: Are copybooks and common data areas explained?
5. **Business Process**: Does the documentation tell a coherent story?

## Cross-File Issues

Flag issues like:
- Program A documents calling B, but B doesn't mention being called
- Inconsistent naming for the same data elements
- Missing documentation for programs in call chains
- Conflicting business rule documentation

## Decision Criteria

BATCH_WITNESSED: All files meet quality, cross-file issues resolved
BATCH_VALHALLA: Exceptional quality with excellent cross-references
NEEDS_REWORK: Issues requiring another cycle
BATCH_FORCED: Max cycles reached, accept current state
"""

    async def holistic_review(
        self,
        input_data: HolisticReviewInput,
    ) -> HolisticReviewOutput:
        """Perform holistic review of batch documentation."""
        # Implementation
        pass
```

---

## 5. Cycle Management and Termination

### 5.1 Cycle Configuration

```python
@dataclass
class CycleConfig:
    """Configuration for cycle management."""

    max_cycles: int = 5                    # Maximum Scribe-Challenger-Imperator cycles
    max_per_file_iterations: int = 3       # Max iterations within a file

    # Termination thresholds
    min_quality_threshold: float = 0.7     # Minimum acceptable quality score
    convergence_threshold: int = 2         # Cycles without improvement -> stop

    # Timeouts
    cycle_timeout_minutes: int = 60        # Max time per cycle
    batch_timeout_minutes: int = 300       # Max time for entire batch
```

### 5.2 Termination Conditions

```python
class TerminationEvaluator:
    """Evaluates whether batch processing should terminate."""

    def should_terminate(
        self,
        batch_state: BatchState,
        config: CycleConfig,
        imperator_decision: HolisticReviewOutput,
    ) -> tuple[bool, TerminationReason]:
        """Determine if processing should stop."""

        # Success termination
        if imperator_decision.decision in (
            HolisticDecision.BATCH_WITNESSED,
            HolisticDecision.BATCH_VALHALLA,
        ):
            return True, TerminationReason.IMPERATOR_SATISFIED

        # Forced termination - max cycles
        if batch_state.cycle >= config.max_cycles:
            return True, TerminationReason.MAX_CYCLES

        # Forced termination - timeout
        elapsed = datetime.utcnow() - batch_state.started_at
        if elapsed.total_seconds() / 60 > config.batch_timeout_minutes:
            return True, TerminationReason.TIMEOUT

        # Forced termination - no improvement
        if self._no_improvement(batch_state, config.convergence_threshold):
            return True, TerminationReason.NO_IMPROVEMENT

        # Continue processing
        return False, None

    def _no_improvement(
        self,
        batch_state: BatchState,
        threshold: int,
    ) -> bool:
        """Check if quality has stagnated."""
        # Compare quality metrics across recent cycles
        # Return True if no improvement for `threshold` cycles
        pass


class TerminationReason(str, Enum):
    """Reasons for batch termination."""

    IMPERATOR_SATISFIED = "imperator_satisfied"
    MAX_CYCLES = "max_cycles"
    TIMEOUT = "timeout"
    NO_IMPROVEMENT = "no_improvement"
    ERROR = "error"
```

### 5.3 Cycle Transition Logic

```python
class CycleManager:
    """Manages transitions between cycles."""

    async def start_new_cycle(
        self,
        batch_state: BatchState,
        imperator_feedback: HolisticReviewOutput,
    ) -> None:
        """Initialize a new cycle based on Imperator feedback."""

        batch_state.cycle += 1

        # Create CHROME tickets for each piece of feedback
        for file_name, chrome_tickets in imperator_feedback.file_feedback.items():
            for ticket in chrome_tickets:
                await self.beads.create_ticket(
                    title=f"[Cycle {batch_state.cycle}] {file_name}: {ticket.description}",
                    ticket_type=ProgramManagerTicketType.CHROME,
                    priority=self._map_priority(ticket.priority),
                    assignee="scribe",
                    labels=[
                        f"batch-{batch_state.batch_id}",
                        f"cycle-{batch_state.cycle}",
                        file_name,
                    ],
                    parent_id=self._get_file_ticket(file_name),
                )

        # Create tickets for consistency issues
        for issue in imperator_feedback.consistency_issues:
            for file_name in issue.affected_files:
                await self.beads.create_ticket(
                    title=f"[Cycle {batch_state.cycle}] {file_name}: {issue.issue_type}",
                    ticket_type=ProgramManagerTicketType.CHROME,
                    priority=BeadsPriority.HIGH,
                    assignee="scribe",
                    labels=[
                        f"batch-{batch_state.batch_id}",
                        f"cycle-{batch_state.cycle}",
                        "consistency",
                        file_name,
                    ],
                )
```

---

## 6. Integration with Existing LangGraph Per-File Processing

### 6.1 Preserved Components

The existing per-file LangGraph workflow remains intact and is used by Scribe workers:

```python
# Existing workflow preserved in war_rig/orchestration/graph.py
class WarRigGraph:
    """Existing per-file workflow - no changes needed."""

    async def ainvoke(
        self,
        source_code: str,
        file_name: str,
        copybook_contents: dict[str, str] | None = None,
        use_mock: bool = False,
    ) -> WarRigState:
        """Run single-file documentation workflow."""
        # ... existing implementation ...
```

### 6.2 Scribe Worker Integration

Scribe workers use the existing graph but with modified termination:

```python
class ScribeWorker(BaseWorker):
    """Scribe worker integrating with existing LangGraph workflow."""

    def __init__(self, ...):
        super().__init__(...)

        # Use existing graph with modified config
        self.graph = WarRigGraph(
            config=self._create_scribe_only_config(),
        )

    def _create_scribe_only_config(self) -> WarRigConfig:
        """Config that runs Scribe node only (no Challenger/Imperator)."""
        config = self.war_rig_config.model_copy()
        config.max_iterations = 1  # Single pass per ticket
        return config

    async def process_ticket(self, ticket: ProgramManagerTicket) -> WorkerResult:
        """Process using existing WarRigGraph infrastructure."""

        # Load source file
        source = await self._load_source(ticket.file_name)

        # Run preprocessing (existing node)
        preprocessed = await self.graph.nodes.preprocess({
            "source_code": source.code,
            "file_name": source.file_name,
        })

        # Run Scribe documentation (existing node)
        result = await self.graph.nodes.scribe_document({
            **preprocessed,
            "source_code": source.code,
            "file_name": source.file_name,
            "copybook_contents": source.copybooks,
            "iteration": ticket.cycle,
            # Include previous state if rework
            "previous_template": self._get_previous_template(ticket),
            "chrome_tickets": self._get_chrome_tickets(ticket),
        })

        return WorkerResult(
            success=result.get("error") is None,
            state=result,
        )
```

### 6.3 State Serialization

Documentation state is serialized to beads tickets for handoff between workers:

```python
class StateSerializer:
    """Serializes WarRigState for storage in beads tickets."""

    def serialize(self, state: WarRigState) -> str:
        """Convert state to JSON for storage."""
        # Extract serializable portions
        serializable = {
            "source_code": state["source_code"],
            "file_name": state["file_name"],
            "file_type": state.get("file_type", FileType.OTHER).value,
            "iteration": state.get("iteration", 1),
            "current_template": (
                state["current_template"].model_dump()
                if state.get("current_template")
                else None
            ),
            "current_confidence": (
                state["current_confidence"].model_dump()
                if state.get("current_confidence")
                else None
            ),
            "challenger_assessment": (
                state["challenger_assessment"].model_dump()
                if state.get("challenger_assessment")
                else None
            ),
            "preprocessor_result": (
                state["preprocessor_result"].model_dump()
                if state.get("preprocessor_result")
                else None
            ),
        }
        return json.dumps(serializable)

    def deserialize(self, data: str) -> WarRigState:
        """Reconstruct state from JSON."""
        raw = json.loads(data)

        return create_initial_state(
            source_code=raw["source_code"],
            file_name=raw["file_name"],
            file_type=FileType(raw["file_type"]),
            # ... restore other fields ...
        )
```

### 6.4 New Module Structure

```
war_rig/
    orchestration/
        __init__.py
        graph.py              # Existing - per-file LangGraph
        nodes.py              # Existing - node implementations
        state.py              # Existing - WarRigState

        program_manager/      # NEW - batch orchestration
            __init__.py
            manager.py        # ProgramManager class
            config.py         # ProgramManagerConfig
            batch_state.py    # BatchState, BatchStatus

        workers/              # NEW - worker pool
            __init__.py
            base.py           # BaseWorker
            pool.py           # WorkerPoolManager
            scribe.py         # ScribeWorker
            challenger.py     # ChallengerWorker
            selector.py       # TicketSelector

        holistic/             # NEW - holistic review
            __init__.py
            trigger.py        # HolisticReviewTrigger
            input_builder.py  # Build HolisticReviewInput
            imperator.py      # ImperatorHolisticAgent

    beads.py                  # Extended with ProgramManagerTicket

    agents/
        __init__.py
        base.py               # Existing
        scribe.py             # Existing
        challenger.py         # Existing
        imperator.py          # Existing - extended for holistic
```

---

## 7. Configuration

### 7.1 Program Manager Configuration

```python
@dataclass
class ProgramManagerConfig:
    """Configuration for Program Manager."""

    # Worker pools
    scribe_count: int = 3
    challenger_count: int = 2

    # Cycle management
    max_cycles: int = 5
    max_per_file_iterations: int = 3

    # Timeouts
    worker_poll_interval_seconds: int = 5
    claim_timeout_seconds: int = 300
    cycle_timeout_minutes: int = 60
    batch_timeout_minutes: int = 300

    # Quality thresholds
    min_quality_threshold: float = 0.7
    convergence_threshold: int = 2

    # Beads integration
    beads_enabled: bool = True
    beads_dry_run: bool = False

    # LangGraph
    war_rig_config: WarRigConfig | None = None
```

### 7.2 Environment Variables

```bash
# Worker pool sizes
WAR_RIG_SCRIBE_COUNT=3
WAR_RIG_CHALLENGER_COUNT=2

# Cycle limits
WAR_RIG_MAX_CYCLES=5
WAR_RIG_MAX_PER_FILE_ITERATIONS=3

# Timeouts
WAR_RIG_CYCLE_TIMEOUT_MINUTES=60
WAR_RIG_BATCH_TIMEOUT_MINUTES=300

# Quality
WAR_RIG_MIN_QUALITY_THRESHOLD=0.7
WAR_RIG_CONVERGENCE_THRESHOLD=2
```

---

## 8. Example Usage

```python
import asyncio
from war_rig.orchestration.program_manager import ProgramManager, ProgramManagerConfig
from war_rig.beads import get_beads_client

async def document_codebase():
    """Document multiple COBOL programs in parallel."""

    # Load source files
    source_files = [
        SourceFile("MAINPGM.cbl", load_cobol("MAINPGM.cbl")),
        SourceFile("SUBPGM1.cbl", load_cobol("SUBPGM1.cbl")),
        SourceFile("SUBPGM2.cbl", load_cobol("SUBPGM2.cbl")),
        SourceFile("COPYBK1.cpy", load_copybook("COPYBK1.cpy")),
    ]

    # Configure
    config = ProgramManagerConfig(
        scribe_count=3,
        challenger_count=2,
        max_cycles=5,
    )

    # Create Program Manager
    pm = ProgramManager(
        config=config,
        beads_client=get_beads_client(),
    )

    # Run batch documentation
    result = await pm.run_batch(source_files)

    if result.success:
        print(f"Documentation complete after {result.cycle} cycles")
        print(f"Decision: {result.decision}")

        for doc in result.documentation:
            print(f"  {doc.file_name}: {doc.template.header.final_status}")
    else:
        print(f"Documentation incomplete: {result.termination_reason}")


if __name__ == "__main__":
    asyncio.run(document_codebase())
```

---

## 9. Future Considerations

### 9.1 Scalability

- **Distributed workers**: Workers could run on separate machines, coordinating via beads
- **Queue backends**: Replace beads with Redis/RabbitMQ for high-volume processing
- **Caching**: Cache preprocessor results and copybook contents

### 9.2 Observability

- **Metrics**: Track tickets/second, cycle times, quality trends
- **Tracing**: Distributed tracing for worker operations
- **Dashboards**: Real-time batch progress visualization

### 9.3 Recovery

- **Checkpointing**: Save batch state for recovery after failures
- **Idempotent operations**: Ensure ticket operations can be safely retried
- **Dead letter queue**: Handle permanently failed tickets

---

## Appendix A: Ticket Type Details

### DOCUMENTATION Ticket

Created by Program Manager for each source file at batch start.

**Fields:**
- `file_name`: Source file to document
- `source_code`: Stored or referenced
- `copybook_contents`: Resolved copybooks
- `cycle`: Current batch cycle (1 initially)

**Completion:**
- Scribe produces `DocumentationTemplate`
- State serialized and stored
- Triggers VALIDATION ticket creation

### VALIDATION Ticket

Auto-created when DOCUMENTATION ticket completes.

**Fields:**
- `parent_ticket_id`: Link to DOCUMENTATION ticket
- `documentation_state`: Serialized state from Scribe

**Completion:**
- Challenger produces assessment
- May create CLARIFICATION tickets
- Updates state with assessment

### CLARIFICATION Ticket

Created by Challenger for blocking questions.

**Fields:**
- `parent_ticket_id`: Link to VALIDATION ticket
- `question`: The question text
- `question_type`: CLARIFICATION, VERIFICATION, etc.
- `severity`: BLOCKING, IMPORTANT, MINOR
- `evidence`: Line numbers

**Completion:**
- Scribe provides response
- Updates documentation if needed

### CHROME Ticket

Created by Imperator during holistic review.

**Fields:**
- `file_name`: Target file
- `section`: Template section needing work
- `issue_type`: VAGUE, MISSING, etc.
- `description`: Issue description
- `guidance`: Improvement hints
- `priority`: CRITICAL, HIGH, MEDIUM
- `cycle`: Cycle when created

**Completion:**
- Scribe addresses issue
- Updates documentation

### HOLISTIC_REVIEW Ticket

Auto-created when all file tickets complete.

**Fields:**
- `batch_id`: Batch identifier
- `cycle`: Current cycle
- `file_documentation`: All completed docs

**Completion:**
- Imperator reviews batch
- Either approves or creates CHROME tickets
