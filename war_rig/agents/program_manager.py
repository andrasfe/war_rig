"""Program Manager agent for batch documentation orchestration.

The Program Manager orchestrates parallel processing of multiple source files
through ticket-based coordination. It creates and tracks beads tickets for
each source file, monitors batch progress, and triggers holistic reviews.

Responsibilities:
- Initialize batch with source file discovery
- Create DOCUMENTATION tickets for all source files
- Track ticket state and batch progress
- Trigger holistic review when all tickets complete
- Handle clarification requests from Imperator feedback
- Manage cycle transitions and termination

This agent follows the architecture defined in:
    docs/program_manager_architecture.md
"""

import logging
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Any

from pydantic import Field

from war_rig.agents.base import AgentInput, AgentOutput, BaseAgent
from war_rig.beads import (
    BeadsClient,
    BeadsPriority,
    ProgramManagerTicket,
    TicketState,
    TicketType,
    get_beads_client,
)
from war_rig.config import APIConfig, ModelConfig, WarRigConfig, load_config
from war_rig.io.reader import SourceFile, SourceReader
from war_rig.models.templates import FileType

logger = logging.getLogger(__name__)


class ProgramManagerInput(AgentInput):
    """Input data for the Program Manager agent.

    Contains information about the batch of files to process.
    """

    input_directory: Path = Field(
        ...,
        description="Directory containing source files to process",
    )
    file_types: list[FileType] | None = Field(
        default=None,
        description="File types to include (all if None)",
    )
    recursive: bool = Field(
        default=True,
        description="Whether to search subdirectories",
    )
    batch_id: str | None = Field(
        default=None,
        description="Optional batch identifier (auto-generated if None)",
    )
    cycle_number: int = Field(
        default=1,
        ge=1,
        description="Current cycle number",
    )


class ProgramManagerOutput(AgentOutput):
    """Output from the Program Manager agent.

    Contains batch status and ticket information.
    """

    batch_id: str = Field(
        default="",
        description="The batch identifier",
    )
    cycle_number: int = Field(
        default=1,
        description="Current cycle number",
    )
    files_discovered: int = Field(
        default=0,
        ge=0,
        description="Number of source files discovered",
    )
    tickets_created: int = Field(
        default=0,
        ge=0,
        description="Number of tickets created",
    )
    tickets_by_state: dict[str, int] = Field(
        default_factory=dict,
        description="Count of tickets by state",
    )
    tickets_by_type: dict[str, int] = Field(
        default_factory=dict,
        description="Count of tickets by type",
    )
    is_batch_complete: bool = Field(
        default=False,
        description="Whether all documentation and validation tickets are done",
    )
    source_files: list[str] = Field(
        default_factory=list,
        description="List of discovered source file names",
    )


@dataclass
class BatchTicketSummary:
    """Summary of tickets for a batch.

    Tracks counts by state and type for quick status checks.
    """

    by_state: dict[TicketState, int] = field(default_factory=dict)
    by_type: dict[TicketType, int] = field(default_factory=dict)
    total: int = 0

    @property
    def created_count(self) -> int:
        """Count of tickets in CREATED state."""
        return self.by_state.get(TicketState.CREATED, 0)

    @property
    def in_progress_count(self) -> int:
        """Count of tickets in progress (CLAIMED + IN_PROGRESS)."""
        return (
            self.by_state.get(TicketState.CLAIMED, 0) +
            self.by_state.get(TicketState.IN_PROGRESS, 0)
        )

    @property
    def completed_count(self) -> int:
        """Count of completed tickets."""
        return self.by_state.get(TicketState.COMPLETED, 0)

    @property
    def blocked_count(self) -> int:
        """Count of blocked tickets."""
        return self.by_state.get(TicketState.BLOCKED, 0)

    @property
    def documentation_count(self) -> int:
        """Count of documentation tickets."""
        return self.by_type.get(TicketType.DOCUMENTATION, 0)

    @property
    def validation_count(self) -> int:
        """Count of validation tickets."""
        return self.by_type.get(TicketType.VALIDATION, 0)


@dataclass
class ClarificationRequest:
    """A clarification request from Imperator feedback.

    Used to track issues that need to be addressed in a new cycle.
    """

    file_name: str
    issue_description: str
    section: str | None = None
    priority: BeadsPriority = BeadsPriority.MEDIUM
    guidance: str | None = None
    parent_ticket_id: str | None = None
    # Optional template/source data to pass to Scribe (avoids disk lookup)
    template_data: dict | None = None
    source_code: str | None = None
    file_path: str | None = None


@dataclass
class CycleSummary:
    """Summary of the current cycle's progress and status.

    Provides a snapshot of where the batch stands in the workflow.
    """

    batch_id: str
    cycle_number: int
    started_at: datetime
    ticket_summary: BatchTicketSummary
    files_total: int
    files_documented: int
    files_validated: int
    is_complete: bool
    can_trigger_holistic_review: bool

    def to_dict(self) -> dict[str, Any]:
        """Convert to dictionary for serialization."""
        return {
            "batch_id": self.batch_id,
            "cycle_number": self.cycle_number,
            "started_at": self.started_at.isoformat(),
            "files_total": self.files_total,
            "files_documented": self.files_documented,
            "files_validated": self.files_validated,
            "is_complete": self.is_complete,
            "can_trigger_holistic_review": self.can_trigger_holistic_review,
            "tickets": {
                "total": self.ticket_summary.total,
                "created": self.ticket_summary.created_count,
                "in_progress": self.ticket_summary.in_progress_count,
                "completed": self.ticket_summary.completed_count,
                "blocked": self.ticket_summary.blocked_count,
            },
        }


class ProgramManagerAgent(BaseAgent[ProgramManagerInput, ProgramManagerOutput]):
    """Program Manager agent for orchestrating batch documentation workflows.

    The Program Manager is responsible for:
    1. Scanning input directories for source files
    2. Creating DOCUMENTATION tickets for each file
    3. Tracking ticket state and batch progress
    4. Triggering holistic review when all file-level work completes
    5. Handling Imperator feedback and creating new cycle tickets

    Unlike other agents (Scribe, Challenger, Imperator), the Program Manager
    does not directly use LLM capabilities. It is an orchestration agent that
    coordinates the work of other agents through the beads ticket system.

    Example:
        config = load_config()
        pm = ProgramManagerAgent(config)

        output = await pm.ainvoke(ProgramManagerInput(
            input_directory=Path("./input"),
        ))
        print(f"Created {output.tickets_created} tickets for {output.files_discovered} files")

    Attributes:
        beads: BeadsClient for ticket operations
        source_reader: SourceReader for file discovery
        batch_id: Current batch identifier
        cycle_number: Current cycle number
        discovered_files: List of discovered source files
    """

    def __init__(
        self,
        config: WarRigConfig | None = None,
        beads_client: BeadsClient | None = None,
        api_config: APIConfig | None = None,
    ):
        """Initialize the Program Manager agent.

        Args:
            config: War Rig configuration. If None, loads from environment.
            beads_client: BeadsClient for ticket operations. If None, uses default.
            api_config: API configuration (not used by this agent, but kept for
                consistency with BaseAgent pattern).
        """
        # Load config if not provided
        if config is None:
            config = load_config()
        self._war_rig_config = config

        # Create a minimal model config for base class
        # Program Manager doesn't use LLM, but BaseAgent requires config
        model_config = ModelConfig(model="none")

        super().__init__(model_config, api_config, name="ProgramManager")

        # Initialize beads client
        if beads_client is None:
            self.beads = get_beads_client(
                enabled=config.beads_enabled,
                dry_run=config.beads_dry_run,
            )
        else:
            self.beads = beads_client

        # Initialize source reader
        self.source_reader = SourceReader(config.system)

        # Batch state
        self.batch_id: str | None = None
        self.cycle_number: int = 1
        self.discovered_files: list[SourceFile] = []
        self.created_tickets: list[ProgramManagerTicket] = []
        self._batch_start_time: datetime | None = None

    @property
    def war_rig_config(self) -> WarRigConfig:
        """Get the War Rig configuration."""
        return self._war_rig_config

    def _generate_batch_id(self) -> str:
        """Generate a unique batch identifier.

        Returns:
            Batch ID in format 'batch-YYYYMMDD-HHMMSS'.
        """
        timestamp = datetime.utcnow().strftime("%Y%m%d-%H%M%S")
        return f"batch-{timestamp}"

    def initialize_batch(
        self,
        input_dir: Path,
        file_types: list[FileType] | None = None,
        recursive: bool = True,
        batch_id: str | None = None,
    ) -> list[ProgramManagerTicket]:
        """Scan input directory and create DOCUMENTATION tickets for each file.

        This is the primary initialization method that:
        1. Scans the input directory for source files
        2. Creates a DOCUMENTATION ticket for each discovered file
        3. Returns the list of created tickets

        Args:
            input_dir: Directory containing source files to process.
            file_types: File types to include (all recognized types if None).
            recursive: Whether to search subdirectories.
            batch_id: Optional batch identifier (auto-generated if None).

        Returns:
            List of created ProgramManagerTicket objects.

        Example:
            pm = ProgramManagerAgent(config)
            tickets = pm.initialize_batch(Path("./input"))
            print(f"Created {len(tickets)} documentation tickets")
        """
        # Set batch ID
        self.batch_id = batch_id or self._generate_batch_id()
        self.cycle_number = 1
        self._batch_start_time = datetime.utcnow()
        self.created_tickets = []

        logger.info(f"Initializing batch {self.batch_id} from {input_dir}")

        # Discover source files
        self.discovered_files = list(
            self.source_reader.discover_files(
                directory=input_dir,
                file_types=file_types,
                recursive=recursive,
            )
        )

        logger.info(f"Discovered {len(self.discovered_files)} source files")

        # Get existing DOCUMENTATION tickets to avoid duplicates on resume
        existing_doc_tickets = {
            t.file_name: t
            for t in self.beads._pm_ticket_cache.values()
            if t.ticket_type == TicketType.DOCUMENTATION
        }
        skipped_count = 0

        # Create DOCUMENTATION ticket for each file (skip if exists)
        for source_file in self.discovered_files:
            # Check if ticket already exists for this file
            if source_file.name in existing_doc_tickets:
                existing = existing_doc_tickets[source_file.name]
                self.created_tickets.append(existing)
                skipped_count += 1
                logger.debug(
                    f"Skipping {source_file.name} - ticket {existing.ticket_id} "
                    f"already exists ({existing.state.value})"
                )
                continue

            program_id = source_file.stem.upper()

            ticket = self.beads.create_pm_ticket(
                ticket_type=TicketType.DOCUMENTATION,
                file_name=source_file.name,
                program_id=program_id,
                cycle_number=self.cycle_number,
                priority=BeadsPriority.MEDIUM,
                metadata={
                    "batch_id": self.batch_id,
                    "file_path": str(source_file.path),
                    "file_type": source_file.file_type.value,
                    "size_bytes": source_file.size_bytes,
                },
            )

            if ticket:
                self.created_tickets.append(ticket)
                logger.debug(f"Created ticket {ticket.ticket_id} for {source_file.name}")

        if skipped_count > 0:
            logger.info(
                f"Batch {self.batch_id}: Resuming with {skipped_count} existing tickets, "
                f"created {len(self.created_tickets) - skipped_count} new tickets"
            )
        else:
            logger.info(
                f"Batch {self.batch_id} initialized with {len(self.created_tickets)} "
                f"documentation tickets"
            )

        return self.created_tickets

    def get_batch_status(self) -> dict[str, int]:
        """Get counts of tickets by state.

        Queries the beads system for current ticket states and returns
        a summary of counts by state.

        Returns:
            Dictionary mapping state names to ticket counts.

        Example:
            status = pm.get_batch_status()
            print(f"Completed: {status.get('completed', 0)}")
            print(f"In Progress: {status.get('in_progress', 0)}")
        """
        status: dict[str, int] = {}

        # Initialize all states to 0
        for state in TicketState:
            status[state.value] = 0

        # Query tickets for each state
        for state in TicketState:
            tickets = self.beads.get_tickets_by_state(state)
            # Filter to only this batch's tickets based on metadata/labels
            batch_tickets = [
                t for t in tickets
                if self._is_batch_ticket(t)
            ]
            status[state.value] = len(batch_tickets)

        return status

    def _is_batch_ticket(self, ticket: ProgramManagerTicket) -> bool:
        """Check if a ticket belongs to the current batch.

        Args:
            ticket: The ticket to check.

        Returns:
            True if the ticket is part of the current batch.
        """
        if not self.batch_id:
            return False

        # Check if batch_id is in metadata
        metadata = ticket.metadata or {}
        return metadata.get("batch_id") == self.batch_id

    def is_batch_complete(self) -> bool:
        """Check if all documentation and validation tickets are done.

        A batch is complete when all DOCUMENTATION and VALIDATION tickets
        are in the COMPLETED state, with no tickets in CREATED, CLAIMED,
        IN_PROGRESS, or BLOCKED states.

        Returns:
            True if all doc/validation work is complete.

        Example:
            while not pm.is_batch_complete():
                time.sleep(10)  # Wait for workers to process
            pm.trigger_holistic_review()
        """
        status = self.get_batch_status()

        # Check for any incomplete states
        incomplete_states = [
            TicketState.CREATED.value,
            TicketState.CLAIMED.value,
            TicketState.IN_PROGRESS.value,
            TicketState.BLOCKED.value,
        ]

        for state in incomplete_states:
            if status.get(state, 0) > 0:
                return False

        # Must have at least some completed tickets
        return status.get(TicketState.COMPLETED.value, 0) > 0

    def trigger_holistic_review(self) -> ProgramManagerTicket | None:
        """Create a HOLISTIC_REVIEW ticket for the Imperator.

        This should be called when is_batch_complete() returns True.
        Creates a single holistic review ticket that triggers the Imperator
        to review all completed documentation.

        Returns:
            The created HOLISTIC_REVIEW ticket, or None if creation failed.

        Raises:
            ValueError: If the batch is not complete.

        Example:
            if pm.is_batch_complete():
                review_ticket = pm.trigger_holistic_review()
                print(f"Holistic review triggered: {review_ticket.ticket_id}")
        """
        if not self.is_batch_complete():
            raise ValueError(
                "Cannot trigger holistic review: batch is not complete. "
                "Call is_batch_complete() first."
            )

        if not self.batch_id:
            raise ValueError("No active batch. Call initialize_batch() first.")

        logger.info(f"Triggering holistic review for batch {self.batch_id}")

        # Collect all file names for the review
        file_names = [f.name for f in self.discovered_files]

        ticket = self.beads.create_pm_ticket(
            ticket_type=TicketType.HOLISTIC_REVIEW,
            file_name=",".join(file_names[:5]) + ("..." if len(file_names) > 5 else ""),
            program_id=f"BATCH-{self.batch_id}",
            cycle_number=self.cycle_number,
            priority=BeadsPriority.HIGH,
            metadata={
                "batch_id": self.batch_id,
                "file_count": len(file_names),
                "file_names": file_names,
                "cycle": self.cycle_number,
            },
        )

        if ticket:
            logger.info(f"Created holistic review ticket: {ticket.ticket_id}")

        return ticket

    def handle_clarifications(
        self,
        clarification_requests: list[ClarificationRequest],
    ) -> list[ProgramManagerTicket]:
        """Create CLARIFICATION tickets from Imperator feedback.

        When the Imperator reviews the batch and identifies issues,
        this method creates CLARIFICATION or CHROME tickets for each
        issue that needs to be addressed.

        Args:
            clarification_requests: List of clarification requests from
                Imperator feedback.

        Returns:
            List of created tickets.

        Example:
            requests = [
                ClarificationRequest(
                    file_name="PROGRAM.cbl",
                    issue_description="Missing error handling documentation",
                    section="error_handling",
                    priority=BeadsPriority.HIGH,
                ),
            ]
            tickets = pm.handle_clarifications(requests)
        """
        if not self.batch_id:
            raise ValueError("No active batch. Call initialize_batch() first.")

        # Increment cycle for new work
        self.cycle_number += 1

        logger.info(
            f"Creating {len(clarification_requests)} clarification tickets "
            f"for cycle {self.cycle_number}"
        )

        created_tickets: list[ProgramManagerTicket] = []

        for request in clarification_requests:
            # Determine ticket type based on whether it has a parent
            ticket_type = (
                TicketType.CLARIFICATION
                if request.parent_ticket_id
                else TicketType.CHROME
            )

            # Extract program ID from file name
            program_id = Path(request.file_name).stem.upper()

            # Build metadata with optional template/source data
            metadata: dict[str, Any] = {
                "batch_id": self.batch_id,
                "section": request.section,
                "issue_description": request.issue_description,
                "guidance": request.guidance,
            }
            # Include template/source data if provided (avoids disk lookup in Scribe)
            if request.template_data:
                metadata["template"] = request.template_data
            if request.source_code:
                metadata["source_code"] = request.source_code
            if request.file_path:
                metadata["file_path"] = request.file_path

            ticket = self.beads.create_pm_ticket(
                ticket_type=ticket_type,
                file_name=request.file_name,
                program_id=program_id,
                cycle_number=self.cycle_number,
                parent_ticket_id=request.parent_ticket_id,
                priority=request.priority,
                metadata=metadata,
            )

            if ticket:
                created_tickets.append(ticket)
                logger.debug(
                    f"Created {ticket_type.value} ticket {ticket.ticket_id} "
                    f"for {request.file_name}"
                )

        logger.info(f"Created {len(created_tickets)} clarification/chrome tickets")

        return created_tickets

    def get_cycle_summary(self) -> CycleSummary:
        """Get a summary of the current cycle's progress.

        Provides a comprehensive snapshot of the batch state including
        file counts, ticket counts by state and type, and completion status.

        Returns:
            CycleSummary with current cycle state.

        Example:
            summary = pm.get_cycle_summary()
            print(f"Cycle {summary.cycle_number}: {summary.files_documented}/{summary.files_total} documented")
        """
        if not self.batch_id:
            raise ValueError("No active batch. Call initialize_batch() first.")

        # Build ticket summary
        ticket_summary = BatchTicketSummary()
        status = self.get_batch_status()

        for state_name, count in status.items():
            try:
                state = TicketState(state_name)
                ticket_summary.by_state[state] = count
                ticket_summary.total += count
            except ValueError:
                pass

        # Count by type
        for ticket_type in TicketType:
            # Query tickets of this type
            created = self.beads.get_available_tickets(ticket_type=ticket_type)
            completed = self.beads.get_tickets_by_state(
                TicketState.COMPLETED,
                ticket_type=ticket_type,
            )
            in_progress = self.beads.get_tickets_by_state(
                TicketState.IN_PROGRESS,
                ticket_type=ticket_type,
            )
            ticket_summary.by_type[ticket_type] = len(created) + len(completed) + len(in_progress)

        # Calculate file-level progress
        files_total = len(self.discovered_files)

        # Count documented files (DOCUMENTATION tickets completed)
        doc_completed = self.beads.get_tickets_by_state(
            TicketState.COMPLETED,
            ticket_type=TicketType.DOCUMENTATION,
        )
        files_documented = len([t for t in doc_completed if self._is_batch_ticket(t)])

        # Count validated files (VALIDATION tickets completed)
        val_completed = self.beads.get_tickets_by_state(
            TicketState.COMPLETED,
            ticket_type=TicketType.VALIDATION,
        )
        files_validated = len([t for t in val_completed if self._is_batch_ticket(t)])

        # Check completion status
        is_complete = self.is_batch_complete()

        # Can trigger holistic review if complete and no review ticket exists
        can_trigger = is_complete
        if can_trigger:
            # Check if holistic review ticket already exists
            review_tickets = self.beads.get_tickets_by_state(
                TicketState.CREATED,
                ticket_type=TicketType.HOLISTIC_REVIEW,
            )
            in_progress_reviews = self.beads.get_tickets_by_state(
                TicketState.IN_PROGRESS,
                ticket_type=TicketType.HOLISTIC_REVIEW,
            )
            if review_tickets or in_progress_reviews:
                can_trigger = False

        return CycleSummary(
            batch_id=self.batch_id,
            cycle_number=self.cycle_number,
            started_at=self._batch_start_time or datetime.utcnow(),
            ticket_summary=ticket_summary,
            files_total=files_total,
            files_documented=files_documented,
            files_validated=files_validated,
            is_complete=is_complete,
            can_trigger_holistic_review=can_trigger,
        )

    # =========================================================================
    # BaseAgent abstract method implementations
    # Note: Program Manager doesn't use LLM, so these are minimal implementations
    # =========================================================================

    def _build_system_prompt(self) -> str:
        """Build system prompt (not used by Program Manager).

        The Program Manager doesn't use LLM capabilities directly.
        This implementation is provided for BaseAgent compatibility.

        Returns:
            Empty string.
        """
        return ""

    def _build_user_prompt(self, input_data: ProgramManagerInput) -> str:
        """Build user prompt (not used by Program Manager).

        The Program Manager doesn't use LLM capabilities directly.
        This implementation is provided for BaseAgent compatibility.

        Args:
            input_data: The input data.

        Returns:
            Empty string.
        """
        return ""

    def _parse_response(
        self,
        response: str,
        input_data: ProgramManagerInput,
    ) -> ProgramManagerOutput:
        """Parse response (not used by Program Manager).

        The Program Manager doesn't use LLM capabilities directly.
        This implementation is provided for BaseAgent compatibility.

        Args:
            response: The response string.
            input_data: The input data.

        Returns:
            Empty ProgramManagerOutput.
        """
        return ProgramManagerOutput()

    def _create_error_output(
        self,
        error: str,
        input_data: ProgramManagerInput,
    ) -> ProgramManagerOutput:
        """Create an error output.

        Args:
            error: Error message.
            input_data: Original input.

        Returns:
            ProgramManagerOutput indicating failure.
        """
        return ProgramManagerOutput(
            success=False,
            error=error,
        )

    async def ainvoke(self, input_data: ProgramManagerInput) -> ProgramManagerOutput:
        """Asynchronously invoke the Program Manager.

        This method initializes a batch and returns status information.
        Unlike other agents, it doesn't call an LLM - it orchestrates
        ticket creation and tracking.

        Args:
            input_data: The Program Manager's input.

        Returns:
            ProgramManagerOutput with batch status.
        """
        logger.info(
            f"ProgramManager: Initializing batch from {input_data.input_directory}"
        )

        try:
            # Initialize the batch
            tickets = self.initialize_batch(
                input_dir=input_data.input_directory,
                file_types=input_data.file_types,
                recursive=input_data.recursive,
                batch_id=input_data.batch_id,
            )

            # Get status
            status = self.get_batch_status()

            # Build output
            output = ProgramManagerOutput(
                success=True,
                batch_id=self.batch_id or "",
                cycle_number=self.cycle_number,
                files_discovered=len(self.discovered_files),
                tickets_created=len(tickets),
                tickets_by_state=status,
                tickets_by_type={
                    TicketType.DOCUMENTATION.value: len(tickets),
                },
                is_batch_complete=self.is_batch_complete(),
                source_files=[f.name for f in self.discovered_files],
            )

            logger.info(
                f"ProgramManager: Batch {self.batch_id} initialized with "
                f"{len(tickets)} tickets"
            )

            return output

        except Exception as e:
            logger.error(f"ProgramManager: Error during batch initialization: {e}")
            return ProgramManagerOutput(
                success=False,
                error=str(e),
            )

    def invoke(self, input_data: ProgramManagerInput) -> ProgramManagerOutput:
        """Synchronously invoke the Program Manager.

        Args:
            input_data: The Program Manager's input.

        Returns:
            ProgramManagerOutput with batch status.
        """
        import asyncio

        try:
            loop = asyncio.get_running_loop()
            import concurrent.futures
            future = asyncio.run_coroutine_threadsafe(
                self.ainvoke(input_data),
                loop,
            )
            return future.result()
        except RuntimeError:
            return asyncio.run(self.ainvoke(input_data))
