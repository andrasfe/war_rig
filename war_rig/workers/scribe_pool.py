"""Scribe worker pool for parallel documentation processing.

This module implements a parallel worker pool for Scribe agents, enabling
concurrent processing of documentation tickets in the Program Manager workflow.

Key Components:
- ScribeWorker: Individual worker that polls for and processes tickets
- ScribeWorkerPool: Manages N ScribeWorker instances with lifecycle control

Architecture:
    Workers poll the BeadsClient for available DOCUMENTATION tickets, claim them
    atomically, process using ScribeAgent, and update ticket state on completion.
    The pool stops when no more tickets are available after a configurable timeout.

Example:
    from war_rig.workers import ScribeWorkerPool
    from war_rig.beads import BeadsClient
    from war_rig.config import load_config

    config = load_config()
    client = BeadsClient()
    pool = ScribeWorkerPool(config=config, beads_client=client)

    await pool.start()
    status = pool.get_status()
    await pool.stop()

See Also:
    - war_rig.beads: TicketType, TicketState, BeadsClient
    - war_rig.agents.scribe: ScribeAgent, ScribeInput, ScribeOutput
    - docs/program_manager_architecture.md: Section 2 Worker Pool Architecture
"""

from __future__ import annotations

import asyncio
import json
import logging
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import Any

from war_rig.agents.scribe import ScribeAgent, ScribeInput, ScribeOutput
from war_rig.beads import (
    BeadsClient,
    BeadsPriority,
    ProgramManagerTicket,
    TicketState,
    TicketType,
)
from war_rig.chunking import (
    ChunkingResult,
    ChunkMerger,
    COBOLChunker,
    GenericChunker,
    TokenEstimator,
)
from war_rig.config import WarRigConfig
from war_rig.models.templates import DocumentationTemplate, FileType
from war_rig.workers.source_preparer import (
    PreparationContext,
    PreparedSource,
    SourceCodePreparer,
)
from war_rig.models.tickets import ChallengerQuestion, ChromeTicket
from war_rig.utils import log_error

logger = logging.getLogger(__name__)


class WorkerState(str, Enum):
    """State of an individual worker.

    Workers transition through states as they process tickets:
    - IDLE: Worker started but not processing a ticket
    - POLLING: Worker is polling for available tickets
    - PROCESSING: Worker is actively processing a ticket
    - STOPPED: Worker has been stopped
    - ERROR: Worker encountered a fatal error
    """

    IDLE = "idle"
    POLLING = "polling"
    PROCESSING = "processing"
    STOPPED = "stopped"
    ERROR = "error"


@dataclass
class WorkerStatus:
    """Status information for a single worker.

    Provides real-time status of a worker including its current state,
    the ticket it's processing (if any), and statistics.

    Attributes:
        worker_id: Unique identifier for the worker.
        state: Current worker state.
        current_ticket_id: ID of ticket being processed, if any.
        tickets_processed: Total tickets successfully processed.
        tickets_failed: Total tickets that failed processing.
        last_activity: Timestamp of last state change.
        error_message: Most recent error message, if in ERROR state.
    """

    worker_id: str
    state: WorkerState
    current_ticket_id: str | None = None
    tickets_processed: int = 0
    tickets_failed: int = 0
    last_activity: datetime = field(default_factory=datetime.utcnow)
    error_message: str | None = None


class ScribeWorker:
    """Individual Scribe worker that processes documentation tickets.

    The ScribeWorker runs in its own asyncio task, continuously polling
    for available tickets, claiming them atomically, and processing them
    using the ScribeAgent. On completion or failure, the ticket state is
    updated appropriately.

    Responsibilities:
    - Poll for available DOCUMENTATION tickets using BeadsClient
    - Claim tickets atomically to prevent race conditions
    - Process files using ScribeAgent
    - Update ticket state on completion (COMPLETED or BLOCKED)
    - Handle errors gracefully and release tickets on failure

    Ticket Types Processed:
    - DOCUMENTATION: Initial documentation for a source file
    - CLARIFICATION: Response to Challenger questions (future)
    - CHROME: Imperator-requested rework (future)

    Attributes:
        worker_id: Unique identifier for this worker (e.g., "scribe-1").
        config: War Rig configuration.
        beads_client: Client for ticket operations.
        scribe_agent: Agent for documentation generation.

    Example:
        worker = ScribeWorker(
            worker_id="scribe-1",
            config=config,
            beads_client=client,
        )
        await worker.run()  # Runs until stopped
    """

    # Ticket types this worker can process
    COMPATIBLE_TICKET_TYPES = [
        TicketType.DOCUMENTATION,
        TicketType.CLARIFICATION,
        TicketType.CHROME,
    ]

    def __init__(
        self,
        worker_id: str,
        config: WarRigConfig,
        beads_client: BeadsClient,
        input_directory: Path | None = None,
        output_directory: Path | None = None,
        poll_interval: float = 2.0,
        idle_timeout: float = 30.0,
    ):
        """Initialize the Scribe worker.

        Args:
            worker_id: Unique identifier for this worker.
            config: War Rig configuration for agent setup.
            beads_client: Client for ticket operations.
            input_directory: Directory containing source files to process.
            output_directory: Directory to store documentation outputs.
            poll_interval: Seconds between polling attempts when no work available.
            idle_timeout: Seconds of no available tickets before stopping.
        """
        self.worker_id = worker_id
        self.config = config
        self.beads_client = beads_client
        # Resolve to absolute paths to ensure consistent file access
        self.input_directory = (input_directory or config.input_directory).resolve()
        self.output_directory = (output_directory or config.output_directory).resolve()
        self.poll_interval = poll_interval
        self.idle_timeout = idle_timeout

        # Ensure output directory exists
        self.output_directory.mkdir(parents=True, exist_ok=True)

        # Initialize the Scribe agent
        self._scribe_agent: ScribeAgent | None = None

        # Worker state
        self._status = WorkerStatus(
            worker_id=worker_id,
            state=WorkerState.IDLE,
        )
        self._should_stop = False
        self._task: asyncio.Task[None] | None = None

        # Track idle time for auto-stop
        self._last_work_time: datetime = datetime.utcnow()

        # Track tickets this worker has failed on (to avoid re-picking immediately)
        # Other workers with different LLMs can still pick them up
        self._failed_tickets: set[str] = set()

        # Initialize source code preparer for centralized token limit handling
        self._source_preparer = SourceCodePreparer(config.scribe)

    @property
    def scribe_agent(self) -> ScribeAgent:
        """Get the Scribe agent, creating it lazily.

        Returns:
            Configured ScribeAgent instance.
        """
        if self._scribe_agent is None:
            self._scribe_agent = ScribeAgent(
                config=self.config.scribe,
                api_config=self.config.api,
            )
        return self._scribe_agent

    @property
    def status(self) -> WorkerStatus:
        """Get the current worker status.

        Returns:
            Current WorkerStatus with state and statistics.
        """
        return self._status

    def _update_state(self, state: WorkerState, ticket_id: str | None = None) -> None:
        """Update worker state and record activity time.

        Args:
            state: New worker state.
            ticket_id: Current ticket ID if processing.
        """
        self._status.state = state
        self._status.current_ticket_id = ticket_id
        self._status.last_activity = datetime.utcnow()

        logger.debug(f"Worker {self.worker_id}: state -> {state.value}")

    async def run(self) -> None:
        """Main worker loop.

        Continuously polls for tickets, processes them, and updates state.
        Exits when:
        - stop() is called
        - No tickets available for idle_timeout seconds
        - Fatal error occurs

        This method is designed to be run as an asyncio task.
        """
        logger.info(f"Worker {self.worker_id}: Starting")
        self._should_stop = False
        self._last_work_time = datetime.utcnow()

        try:
            while not self._should_stop:
                # Poll for available tickets
                self._update_state(WorkerState.POLLING)
                ticket = await self._poll_for_ticket()

                if ticket is None:
                    # No work available
                    idle_duration = (
                        datetime.utcnow() - self._last_work_time
                    ).total_seconds()

                    if idle_duration >= self.idle_timeout:
                        logger.info(
                            f"Worker {self.worker_id}: No tickets for "
                            f"{idle_duration:.1f}s, stopping"
                        )
                        break

                    self._update_state(WorkerState.IDLE)
                    await asyncio.sleep(self.poll_interval)
                    continue

                # We have a ticket - process it
                await self._process_ticket(ticket)
                # Update last work time AFTER processing so idle timeout
                # is measured from when we finished, not when we started
                self._last_work_time = datetime.utcnow()

        except asyncio.CancelledError:
            logger.info(f"Worker {self.worker_id}: Cancelled")
            # Reset any in-progress ticket so it can be retried
            if self._status.current_ticket_id:
                logger.warning(
                    f"Worker {self.worker_id}: Resetting ticket {self._status.current_ticket_id} "
                    f"to CREATED due to cancellation"
                )
                try:
                    self.beads_client.update_ticket_state(
                        self._status.current_ticket_id,
                        TicketState.CREATED,
                        reason="Worker cancelled mid-processing, reset for retry",
                    )
                except Exception as e:
                    logger.error(f"Failed to reset ticket on cancellation: {e}")
            raise

        except Exception as e:
            logger.error(f"Worker {self.worker_id}: Fatal error: {e}")
            log_error(
                e,
                context={
                    "worker_id": self.worker_id,
                    "last_ticket": self._status.current_ticket_id,
                    "state": self._status.state.value if self._status.state else None,
                },
            )
            self._status.state = WorkerState.ERROR
            self._status.error_message = str(e)
            raise

        finally:
            self._update_state(WorkerState.STOPPED)
            logger.info(
                f"Worker {self.worker_id}: Stopped "
                f"(processed={self._status.tickets_processed}, "
                f"failed={self._status.tickets_failed})"
            )

    async def stop(self) -> None:
        """Signal the worker to stop gracefully.

        The worker will complete its current ticket before stopping.
        Use cancel() for immediate termination.
        """
        # Only log if worker hasn't already stopped naturally
        if self._status.state != WorkerState.STOPPED:
            logger.info(f"Worker {self.worker_id}: Stop requested")
        self._should_stop = True

    async def _poll_for_ticket(self) -> ProgramManagerTicket | None:
        """Poll for and claim an available ticket.

        Queries the BeadsClient for tickets in CREATED state that match
        our supported types (DOCUMENTATION, CLARIFICATION, CHROME).
        Attempts to claim one atomically. Skips tickets this worker
        has already failed on (other workers can still pick them up).

        Returns:
            Claimed ProgramManagerTicket if successful, None otherwise.
        """
        # Get available tickets for all supported types
        all_tickets: list[ProgramManagerTicket] = []
        for ticket_type in self.COMPATIBLE_TICKET_TYPES:
            tickets = self.beads_client.get_available_tickets(
                ticket_type=ticket_type,
            )
            all_tickets.extend(tickets)

        if not all_tickets:
            return None

        tickets = all_tickets

        # Filter out tickets this worker has already failed on
        # (other workers with different LLMs can still try them)
        tickets = [t for t in tickets if t.ticket_id not in self._failed_tickets]

        if not tickets:
            return None

        # Sort by priority (lower value = higher priority), then by created_at
        tickets.sort(key=lambda t: (t.cycle_number, t.created_at))

        # Try to claim the first available ticket
        for ticket in tickets:
            if self.beads_client.claim_ticket(ticket.ticket_id, self.worker_id):
                logger.info(
                    f"Worker {self.worker_id}: Claimed ticket {ticket.ticket_id} "
                    f"({ticket.file_name})"
                )
                return ticket

        # All tickets were claimed by other workers
        return None

    async def _process_ticket(self, ticket: ProgramManagerTicket) -> None:
        """Process a claimed ticket.

        Updates ticket to IN_PROGRESS, runs the ScribeAgent, and updates
        the final state (COMPLETED or BLOCKED).

        Args:
            ticket: The claimed ticket to process.
        """
        self._update_state(WorkerState.PROCESSING, ticket.ticket_id)

        try:
            # Update ticket to IN_PROGRESS
            self.beads_client.update_ticket_state(
                ticket.ticket_id,
                TicketState.IN_PROGRESS,
            )

            # Process based on ticket type
            if ticket.ticket_type == TicketType.DOCUMENTATION:
                result = await self._process_documentation_ticket(ticket)
            elif ticket.ticket_type == TicketType.CLARIFICATION:
                result = await self._process_clarification_ticket(ticket)
            elif ticket.ticket_type == TicketType.CHROME:
                result = await self._process_chrome_ticket(ticket)
            else:
                logger.warning(
                    f"Worker {self.worker_id}: Unsupported ticket type "
                    f"{ticket.ticket_type}"
                )
                result = ScribeOutput(success=False, error="Unsupported ticket type")

            # Update ticket state based on result
            if result.success and result.needs_revalidation:
                # Special case: CLARIFICATION/CHROME ticket had no questions/issues
                # Mark as completed and create new VALIDATION ticket for fresh review
                self.beads_client.update_ticket_state(
                    ticket.ticket_id,
                    TicketState.COMPLETED,
                    reason="No questions/issues found, triggering revalidation",
                )
                logger.info(
                    f"Worker {self.worker_id}: Ticket {ticket.ticket_id} had no questions/issues, "
                    f"marking complete and creating new VALIDATION ticket for fresh review"
                )
                # Create VALIDATION ticket so Challenger can re-review from scratch
                if result.template:
                    # Save template to disk so CLARIFICATION tickets can load it
                    self._save_template(ticket.file_name, result.template)
                    self._create_validation_ticket(ticket, result)
            elif result.success and not result.responses_incomplete:
                self.beads_client.update_ticket_state(
                    ticket.ticket_id,
                    TicketState.COMPLETED,
                    reason="Documentation completed successfully",
                )
                self._status.tickets_processed += 1
                logger.info(
                    f"Worker {self.worker_id}: Completed ticket {ticket.ticket_id}"
                )

                # Create VALIDATION ticket for Challenger to re-validate
                # This applies to DOCUMENTATION, CLARIFICATION, and CHROME tickets
                if result.template and ticket.ticket_type in self.COMPATIBLE_TICKET_TYPES:
                    # Save template to disk so CLARIFICATION tickets can load it
                    self._save_template(ticket.file_name, result.template)
                    self._create_validation_ticket(ticket, result)
            elif result.success and result.responses_incomplete:
                # Success but responses were incomplete - leave ticket for retry
                logger.warning(
                    f"Worker {self.worker_id}: Ticket {ticket.ticket_id} succeeded but "
                    f"responses were incomplete, leaving for retry"
                )
                # Save any partial template progress
                if result.template:
                    self._save_template(ticket.file_name, result.template)
                # Reset to CREATED so another worker can retry
                self.beads_client.update_ticket_state(
                    ticket.ticket_id,
                    TicketState.CREATED,
                    reason="Responses incomplete, available for retry",
                )
            else:
                # First failure - retry once with enhanced formatting prompt
                logger.warning(
                    f"Worker {self.worker_id}: Ticket {ticket.ticket_id} failed, "
                    f"retrying with strict formatting: {result.error}"
                )

                # Retry with formatting_strict=True
                if ticket.ticket_type == TicketType.DOCUMENTATION:
                    result = await self._process_documentation_ticket(
                        ticket, formatting_strict=True
                    )
                elif ticket.ticket_type == TicketType.CLARIFICATION:
                    result = await self._process_clarification_ticket(
                        ticket, formatting_strict=True
                    )
                elif ticket.ticket_type == TicketType.CHROME:
                    result = await self._process_chrome_ticket(
                        ticket, formatting_strict=True
                    )

                if result.success and not result.responses_incomplete:
                    # Retry succeeded
                    self.beads_client.update_ticket_state(
                        ticket.ticket_id,
                        TicketState.COMPLETED,
                        reason="Documentation completed on retry",
                    )
                    self._status.tickets_processed += 1
                    logger.info(
                        f"Worker {self.worker_id}: Completed ticket {ticket.ticket_id} on retry"
                    )
                    # Create VALIDATION ticket for re-validation
                    if result.template and ticket.ticket_type in self.COMPATIBLE_TICKET_TYPES:
                        # Save template to disk so CLARIFICATION tickets can load it
                        self._save_template(ticket.file_name, result.template)
                        self._create_validation_ticket(ticket, result)
                elif result.success and result.responses_incomplete:
                    # Retry succeeded but responses incomplete - leave for another worker
                    logger.warning(
                        f"Worker {self.worker_id}: Ticket {ticket.ticket_id} retry succeeded but "
                        f"responses were incomplete, leaving for another worker"
                    )
                    # Save any partial template progress
                    if result.template:
                        self._save_template(ticket.file_name, result.template)
                    # Add to failed set so THIS worker won't re-pick it
                    self._failed_tickets.add(ticket.ticket_id)
                    self.beads_client.update_ticket_state(
                        ticket.ticket_id,
                        TicketState.CREATED,
                        reason="Responses incomplete on retry, available for other workers",
                    )
                else:
                    # Second failure - reset ticket for other workers, move on
                    # Add to failed set so THIS worker won't re-pick it
                    self._failed_tickets.add(ticket.ticket_id)
                    self.beads_client.update_ticket_state(
                        ticket.ticket_id,
                        TicketState.CREATED,  # Reset for other workers
                        reason=f"Failed twice, available for other workers: {result.error}",
                    )
                    logger.warning(
                        f"Worker {self.worker_id}: Ticket {ticket.ticket_id} failed twice, "
                        f"resetting for other workers (different LLM may succeed)"
                    )

        except asyncio.CancelledError:
            # Worker cancelled mid-processing - reset ticket for retry
            logger.warning(
                f"Worker {self.worker_id}: Cancelled while processing {ticket.ticket_id}, "
                f"resetting to CREATED for retry"
            )
            try:
                self.beads_client.update_ticket_state(
                    ticket.ticket_id,
                    TicketState.CREATED,
                    reason="Worker cancelled mid-processing, reset for retry",
                )
            except Exception as reset_err:
                logger.error(f"Failed to reset ticket on cancellation: {reset_err}")
            raise  # Re-raise to exit the worker

        except Exception as e:
            # Release ticket on error
            logger.error(
                f"Worker {self.worker_id}: Error processing {ticket.ticket_id}: {e}"
            )
            self.beads_client.update_ticket_state(
                ticket.ticket_id,
                TicketState.BLOCKED,
                reason=f"Worker error: {e}",
            )
            self._status.tickets_failed += 1

        finally:
            self._update_state(WorkerState.IDLE)

    def _get_doc_output_path(self, file_name: str) -> Path:
        """Get the output path for a documentation file.

        Args:
            file_name: Source file name.

        Returns:
            Path to the documentation JSON file.
        """
        base_name = Path(file_name).stem
        return self.output_directory / f"{base_name}.doc.json"

    def _load_previous_template(self, file_name: str) -> DocumentationTemplate | None:
        """Load the previous iteration's documentation template.

        Args:
            file_name: Source file name.

        Returns:
            DocumentationTemplate if exists, None otherwise.
        """
        doc_path = self._get_doc_output_path(file_name)
        if not doc_path.exists():
            return None

        try:
            with open(doc_path) as f:
                data = json.load(f)
            return DocumentationTemplate.model_validate(data)
        except Exception as e:
            logger.warning(
                f"Worker {self.worker_id}: Failed to load previous template "
                f"from {doc_path}: {e}"
            )
            return None

    def _load_template_from_parent(
        self, ticket: ProgramManagerTicket
    ) -> DocumentationTemplate | None:
        """Load template from parent ticket's metadata.

        This is a fallback for tickets created before templates were saved to disk.
        Looks up the parent validation ticket and extracts the template from its metadata.

        Args:
            ticket: The CLARIFICATION or CHROME ticket.

        Returns:
            DocumentationTemplate if found in parent, None otherwise.
        """
        # Try parent_ticket_id first, then metadata field
        parent_id = ticket.parent_ticket_id or ticket.metadata.get(
            "parent_validation_ticket"
        )
        if not parent_id:
            return None

        try:
            # Access the ticket cache directly
            parent_ticket = self.beads_client._pm_ticket_cache.get(parent_id)
            if not parent_ticket:
                logger.warning(
                    f"Worker {self.worker_id}: Parent ticket {parent_id} not found in cache"
                )
                return None

            template_data = parent_ticket.metadata.get("template")
            if not template_data:
                logger.warning(
                    f"Worker {self.worker_id}: No template in parent ticket {parent_id} metadata"
                )
                return None

            template = DocumentationTemplate.model_validate(template_data)
            logger.info(
                f"Worker {self.worker_id}: Loaded template from parent ticket {parent_id}"
            )

            # Save to disk for future use
            self._save_template(ticket.file_name, template)
            logger.info(
                f"Worker {self.worker_id}: Saved recovered template to disk for {ticket.file_name}"
            )

            return template

        except Exception as e:
            logger.warning(
                f"Worker {self.worker_id}: Failed to load template from parent "
                f"{parent_id}: {e}"
            )
            return None

    def _save_template(self, file_name: str, template: DocumentationTemplate) -> None:
        """Save the documentation template to the output directory.

        Creates a backup of the existing template before overwriting to protect
        against corruption from bad LLM responses.

        Args:
            file_name: Source file name.
            template: The documentation template to save.
        """
        doc_path = self._get_doc_output_path(file_name)
        backup_path = Path(str(doc_path) + ".bak")

        try:
            # Backup existing template before overwriting
            if doc_path.exists():
                import shutil
                shutil.copy2(doc_path, backup_path)
                logger.debug(
                    f"Worker {self.worker_id}: Backed up {doc_path} to {backup_path}"
                )

            with open(doc_path, "w") as f:
                json.dump(template.model_dump(mode="json"), f, indent=2, default=str)
            logger.debug(
                f"Worker {self.worker_id}: Saved documentation to {doc_path}"
            )
        except Exception as e:
            logger.error(
                f"Worker {self.worker_id}: Failed to save template to {doc_path}: {e}"
            )
            # Try to restore backup if save failed
            if backup_path.exists():
                try:
                    import shutil
                    shutil.copy2(backup_path, doc_path)
                    logger.info(
                        f"Worker {self.worker_id}: Restored backup after save failure"
                    )
                except Exception:
                    pass

    def _prepare_source_for_processing(
        self,
        source_code: str,
        ticket: ProgramManagerTicket,
        previous_template: DocumentationTemplate | None,
    ) -> PreparedSource:
        """Prepare source code with centralized token limit handling.

        This method provides a single entry point for all source code preparation,
        delegating to SourceCodePreparer which decides whether to:
        - Pass through (if within token limit)
        - Sample (for updates/clarifications/chrome)
        - Chunk (for initial documentation of large files)

        Args:
            source_code: The raw source code to prepare.
            ticket: The ticket being processed (provides context).
            previous_template: Previous documentation template, if any.

        Returns:
            PreparedSource with the appropriate strategy applied.
        """
        file_type = self._determine_file_type(ticket.file_name)
        context = PreparationContext(
            ticket_type=ticket.ticket_type,
            cycle_number=ticket.cycle_number,
            has_previous_template=previous_template is not None,
            file_type=file_type,
        )
        return self._source_preparer.prepare(source_code, context)

    async def _process_documentation_ticket(
        self,
        ticket: ProgramManagerTicket,
        formatting_strict: bool = False,
    ) -> ScribeOutput:
        """Process a DOCUMENTATION ticket.

        Loads the source file and runs documentation through ScribeAgent.
        If this is iteration > 1, loads the previous template for updates.

        Args:
            ticket: The DOCUMENTATION ticket to process.
            formatting_strict: If True, add extra JSON formatting instructions.

        Returns:
            ScribeOutput with documentation results.
        """
        # Load source code from file or metadata
        source_code = ticket.metadata.get("source_code", "")
        if not source_code:
            # Read from file system - check metadata for full path first
            # (file_path in metadata may include subdirectory paths)
            metadata_path = ticket.metadata.get("file_path")
            if metadata_path:
                source_path = Path(metadata_path)
            else:
                source_path = self.input_directory / ticket.file_name

            if not source_path.exists():
                logger.error(
                    f"Worker {self.worker_id}: Source file not found: {source_path}"
                )
                return ScribeOutput(
                    success=False,
                    error=f"Source file not found: {source_path}",
                )
            try:
                source_code = source_path.read_text(encoding="utf-8", errors="replace")
                logger.debug(
                    f"Worker {self.worker_id}: Loaded {len(source_code)} bytes from {source_path}"
                )
            except Exception as e:
                logger.error(
                    f"Worker {self.worker_id}: Failed to read {source_path}: {e}"
                )
                return ScribeOutput(
                    success=False,
                    error=f"Failed to read source file: {e}",
                )

        # Load previous template if this is an update iteration
        previous_template = None
        if ticket.cycle_number > 1:
            previous_template = self._load_previous_template(ticket.file_name)
            if previous_template:
                logger.debug(
                    f"Worker {self.worker_id}: Loaded previous template for "
                    f"{ticket.file_name} (iteration {ticket.cycle_number})"
                )

        # Prepare source code using centralized token limit handling
        prepared = self._prepare_source_for_processing(
            source_code, ticket, previous_template
        )

        # Determine file type from extension
        file_type = self._determine_file_type(ticket.file_name)

        # If chunking is needed, process each chunk separately
        if prepared.needs_chunked_processing:
            logger.info(
                f"Worker {self.worker_id}: File {ticket.file_name} needs chunking "
                f"({prepared.metadata.get('chunk_count', 0)} chunks)"
            )
            output = await self._process_chunked_documentation(
                ticket=ticket,
                source_code=source_code,
                file_type=file_type,
                formatting_strict=formatting_strict,
                chunking_result=prepared.chunking_result,
            )
            # Save the template if successful
            if output.success and output.template:
                self._save_template(ticket.file_name, output.template)
            return output

        # Log if source was sampled
        if prepared.was_modified:
            logger.info(
                f"Worker {self.worker_id}: Source {prepared.strategy_used} for "
                f"{ticket.file_name} (cycle {ticket.cycle_number})"
            )

        # Build Scribe input with prepared source
        scribe_input = ScribeInput(
            source_code=prepared.source_code,
            file_name=ticket.file_name,
            file_type=file_type,
            iteration=ticket.cycle_number,
            copybook_contents=ticket.metadata.get("copybook_contents", {}),
            previous_template=previous_template,
            formatting_strict=formatting_strict,
        )

        # Run the Scribe agent
        logger.debug(
            f"Worker {self.worker_id}: Running Scribe for {ticket.file_name}"
        )
        output = await self.scribe_agent.ainvoke(scribe_input)

        # Save the template if successful
        if output.success and output.template:
            self._save_template(ticket.file_name, output.template)

        return output

    async def _process_chunked_documentation(
        self,
        ticket: ProgramManagerTicket,
        source_code: str,
        file_type: FileType,
        formatting_strict: bool = False,
        chunking_result: ChunkingResult | None = None,
    ) -> ScribeOutput:
        """Process a large file by chunking it semantically.

        Splits the file at structural boundaries (COBOL divisions/sections for
        COBOL files, line boundaries for other files), processes each chunk
        through ScribeAgent, and merges the results into a unified template.

        Args:
            ticket: The DOCUMENTATION ticket to process.
            source_code: The source code (already determined to need chunking).
            file_type: Type of the source file.
            formatting_strict: If True, add extra JSON formatting instructions.
            chunking_result: Pre-computed chunking result from SourceCodePreparer.
                If None, chunking will be performed here.

        Returns:
            ScribeOutput with merged documentation from all chunks.
        """
        merger = ChunkMerger()

        # Use provided chunking result or compute it
        if chunking_result is None:
            # Initialize chunking components
            estimator = TokenEstimator()
            # Use COBOL-specific chunker for COBOL files, generic for others
            if file_type == FileType.COBOL:
                chunker = COBOLChunker(estimator)
            else:
                chunker = GenericChunker(estimator)

            # Calculate available tokens for source code
            max_source_tokens = self._source_preparer.max_source_tokens

            logger.info(
                f"Worker {self.worker_id}: Chunking {ticket.file_name} "
                f"(max {max_source_tokens} tokens per chunk)"
            )

            # Chunk the source code
            chunking_result = chunker.chunk(
                source_code=source_code,
                max_tokens=max_source_tokens,
                file_name=ticket.file_name,
            )

        logger.info(
            f"Worker {self.worker_id}: Split {ticket.file_name} into "
            f"{chunking_result.chunk_count} chunks using {chunking_result.chunking_strategy}"
        )

        # Process each chunk through ScribeAgent
        chunk_outputs: list[ScribeOutput] = []
        for i, chunk in enumerate(chunking_result.chunks):
            logger.info(
                f"Worker {self.worker_id}: Processing chunk {i + 1}/{chunking_result.chunk_count} "
                f"({chunk.chunk_id}, lines {chunk.start_line}-{chunk.end_line}, "
                f"~{chunk.estimated_tokens} tokens)"
            )

            # Build ScribeInput for this chunk
            chunk_input = ScribeInput(
                source_code=chunk.content,
                file_name=f"{ticket.file_name} (chunk {i + 1}/{chunking_result.chunk_count})",
                file_type=file_type,
                iteration=ticket.cycle_number,
                copybook_contents=ticket.metadata.get("copybook_contents", {}),
                formatting_strict=formatting_strict,
            )

            # Run Scribe on this chunk
            try:
                output = await self.scribe_agent.ainvoke(chunk_input)
                chunk_outputs.append(output)

                if not output.success:
                    logger.warning(
                        f"Worker {self.worker_id}: Chunk {chunk.chunk_id} failed: {output.error}"
                    )
            except Exception as e:
                logger.error(
                    f"Worker {self.worker_id}: Exception processing chunk {chunk.chunk_id}: {e}"
                )
                chunk_outputs.append(ScribeOutput(
                    success=False,
                    error=f"Chunk processing error: {e}",
                ))

        # Check if we have any successful outputs
        successful_outputs = [o for o in chunk_outputs if o.success and o.template]
        if not successful_outputs:
            logger.error(
                f"Worker {self.worker_id}: All {len(chunk_outputs)} chunks failed for {ticket.file_name}"
            )
            return ScribeOutput(
                success=False,
                error=f"All {len(chunk_outputs)} chunks failed during processing",
            )

        logger.info(
            f"Worker {self.worker_id}: {len(successful_outputs)}/{len(chunk_outputs)} "
            f"chunks succeeded for {ticket.file_name}"
        )

        # Merge chunk outputs into unified template
        try:
            merged_template = merger.merge(
                chunks=chunking_result.chunks,
                chunk_outputs=chunk_outputs,
                file_name=ticket.file_name,
            )

            # Calculate total tokens used across all chunks
            total_tokens = sum(o.tokens_used for o in chunk_outputs)

            logger.info(
                f"Worker {self.worker_id}: Successfully merged {len(successful_outputs)} chunks "
                f"for {ticket.file_name} ({total_tokens} total tokens)"
            )

            return ScribeOutput(
                success=True,
                template=merged_template,
                tokens_used=total_tokens,
                open_questions=[
                    f"[CHUNKED] File was processed in {chunking_result.chunk_count} chunks "
                    f"using {chunking_result.chunking_strategy}"
                ],
            )

        except Exception as e:
            logger.error(
                f"Worker {self.worker_id}: Failed to merge chunks for {ticket.file_name}: {e}"
            )
            return ScribeOutput(
                success=False,
                error=f"Chunk merge failed: {e}",
            )

    async def _process_clarification_ticket(
        self,
        ticket: ProgramManagerTicket,
        formatting_strict: bool = False,
    ) -> ScribeOutput:
        """Process a CLARIFICATION ticket.

        Responds to Challenger questions about existing documentation by loading
        the previous template, source code, and questions, then running the
        Scribe agent to address the feedback.

        Args:
            ticket: The CLARIFICATION ticket to process.
            formatting_strict: If True, add extra JSON formatting instructions.

        Returns:
            ScribeOutput with updated documentation addressing the questions.
        """
        # Load the previous template - prefer metadata (passed from Challenger),
        # fall back to disk if not present
        previous_template = None
        template_data = ticket.metadata.get("template")
        if template_data:
            try:
                previous_template = DocumentationTemplate.model_validate(template_data)
                logger.debug(
                    f"Worker {self.worker_id}: Loaded template from ticket metadata"
                )
            except Exception as e:
                logger.warning(
                    f"Worker {self.worker_id}: Failed to parse template from metadata: {e}"
                )
                # Try lenient parsing
                try:
                    previous_template = DocumentationTemplate.model_construct(**template_data)
                    logger.debug(
                        f"Worker {self.worker_id}: Loaded template via lenient parsing"
                    )
                except Exception:
                    pass

        # Fall back to disk if not in metadata
        if not previous_template:
            previous_template = self._load_previous_template(ticket.file_name)

        # Fall back to parent validation ticket's metadata
        if not previous_template:
            previous_template = self._load_template_from_parent(ticket)

        if not previous_template:
            logger.error(
                f"Worker {self.worker_id}: No previous template found for "
                f"clarification ticket {ticket.ticket_id}"
            )
            return ScribeOutput(
                success=False,
                error=f"No previous documentation found for {ticket.file_name}",
            )

        # Load source code - prefer metadata (passed from Challenger), fall back to disk
        source_code = ticket.metadata.get("source_code")
        if not source_code:
            metadata_path = ticket.metadata.get("file_path")
            if metadata_path:
                source_path = Path(metadata_path)
            else:
                source_path = self.input_directory / ticket.file_name

            if not source_path.exists():
                return ScribeOutput(
                    success=False,
                    error=f"Source file not found: {source_path}",
                )
            try:
                source_code = source_path.read_text(encoding="utf-8", errors="replace")
            except Exception as e:
                return ScribeOutput(
                    success=False,
                    error=f"Failed to read source file: {e}",
                )

        # Prepare source code using centralized token limit handling
        prepared = self._prepare_source_for_processing(
            source_code, ticket, previous_template
        )
        if prepared.was_modified:
            logger.info(
                f"Worker {self.worker_id}: Source {prepared.strategy_used} for "
                f"clarification ticket ({ticket.file_name})"
            )

        # Extract challenger questions from ticket metadata
        challenger_questions: list[ChallengerQuestion] = []
        questions_data = ticket.metadata.get("challenger_questions", [])
        for q in questions_data:
            if isinstance(q, dict):
                try:
                    challenger_questions.append(ChallengerQuestion.model_validate(q))
                except Exception as e:
                    # Log validation error and try lenient parsing
                    logger.warning(
                        f"Worker {self.worker_id}: Failed to validate question in ticket "
                        f"{ticket.ticket_id}: {e}. Question data: {q}"
                    )
                    # Create question with just the essential fields
                    try:
                        challenger_questions.append(
                            ChallengerQuestion(
                                question_id=q.get("question_id", f"Q-{len(challenger_questions)}"),
                                question=q.get("question", str(q)),
                                section=q.get("section"),
                                context=q.get("context", q.get("evidence", "")),
                            )
                        )
                    except Exception as e2:
                        logger.error(
                            f"Worker {self.worker_id}: Lenient question parsing also failed: {e2}"
                        )
            elif isinstance(q, ChallengerQuestion):
                challenger_questions.append(q)

        # Also check for single question in issue_description
        if ticket.metadata.get("issue_description"):
            challenger_questions.append(
                ChallengerQuestion(
                    question_id=f"CLR-{ticket.ticket_id[-8:]}",
                    question=ticket.metadata["issue_description"],
                    section=ticket.metadata.get("section"),
                    context=ticket.metadata.get("guidance", ""),
                )
            )

        if not challenger_questions:
            logger.warning(
                f"Worker {self.worker_id}: No questions found in clarification ticket "
                f"{ticket.ticket_id}. Ticket metadata: challenger_questions={bool(questions_data)}, "
                f"issue_description={bool(ticket.metadata.get('issue_description'))}. "
                f"Marking for revalidation so Challenger can review from scratch."
            )
            # Return success with needs_revalidation flag so a new VALIDATION ticket
            # gets created and a Challenger can re-review the documentation
            return ScribeOutput(
                success=True,
                template=previous_template,  # Preserve existing work
                needs_revalidation=True,
            )

        # Get file_type from metadata first (set during initial documentation),
        # fall back to inference from filename
        file_type = self._get_file_type_from_metadata(ticket)

        # Build Scribe input with prepared source and previous template
        scribe_input = ScribeInput(
            source_code=prepared.source_code,
            file_name=ticket.file_name,
            file_type=file_type,
            iteration=ticket.cycle_number,
            copybook_contents=ticket.metadata.get("copybook_contents", {}),
            previous_template=previous_template,
            challenger_questions=challenger_questions,
            formatting_strict=formatting_strict,
        )

        logger.info(
            f"Worker {self.worker_id}: Processing clarification for {ticket.file_name} "
            f"with {len(challenger_questions)} questions"
        )
        output = await self.scribe_agent.ainvoke(scribe_input)

        # Save updated template if successful
        if output.success and output.template:
            self._save_template(ticket.file_name, output.template)

        return output

    async def _process_chrome_ticket(
        self,
        ticket: ProgramManagerTicket,
        formatting_strict: bool = False,
    ) -> ScribeOutput:
        """Process a CHROME ticket.

        Addresses an Imperator-issued rework request by loading the previous
        template, source code, and chrome tickets, then running the Scribe
        agent to address the feedback.

        Args:
            ticket: The CHROME ticket to process.
            formatting_strict: If True, add extra JSON formatting instructions.

        Returns:
            ScribeOutput with updated documentation addressing the chrome issues.
        """
        # Skip CROSS_FILE tickets - these are system-level and can't be handled
        # by file-specific Scribes. They need holistic treatment.
        if ticket.file_name == "CROSS_FILE":
            logger.info(
                f"Worker {self.worker_id}: Skipping CROSS_FILE chrome ticket "
                f"{ticket.ticket_id} - requires holistic handling"
            )
            # Mark as completed since these are informational for the Imperator
            return ScribeOutput(
                success=True,
                template=None,  # No template update for cross-file issues
                open_questions=[
                    f"Cross-file issue noted: {ticket.metadata.get('issue_description', 'See ticket details')}"
                ],
            )

        # Load the previous template - prefer metadata, fall back to disk
        previous_template = None
        template_data = ticket.metadata.get("template")
        if template_data:
            try:
                previous_template = DocumentationTemplate.model_validate(template_data)
                logger.debug(
                    f"Worker {self.worker_id}: Loaded template from ticket metadata"
                )
            except Exception as e:
                logger.warning(
                    f"Worker {self.worker_id}: Failed to parse template from metadata: {e}"
                )
                # Try lenient parsing
                try:
                    previous_template = DocumentationTemplate.model_construct(**template_data)
                    logger.debug(
                        f"Worker {self.worker_id}: Loaded template via lenient parsing"
                    )
                except Exception:
                    pass

        # Fall back to disk if not in metadata
        if not previous_template:
            previous_template = self._load_previous_template(ticket.file_name)

        # Fall back to parent ticket's metadata (for tickets created before disk-save fix)
        if not previous_template:
            previous_template = self._load_template_from_parent(ticket)

        if not previous_template:
            logger.error(
                f"Worker {self.worker_id}: No previous template found for "
                f"chrome ticket {ticket.ticket_id}"
            )
            return ScribeOutput(
                success=False,
                error=f"No previous documentation found for {ticket.file_name}",
            )

        # Load source code - prefer metadata, fall back to disk
        source_code = ticket.metadata.get("source_code")
        if not source_code:
            metadata_path = ticket.metadata.get("file_path")
            if metadata_path:
                source_path = Path(metadata_path)
            else:
                source_path = self.input_directory / ticket.file_name

            if not source_path.exists():
                return ScribeOutput(
                    success=False,
                    error=f"Source file not found: {source_path}",
                )
            try:
                source_code = source_path.read_text(encoding="utf-8", errors="replace")
            except Exception as e:
                return ScribeOutput(
                    success=False,
                    error=f"Failed to read source file: {e}",
                )

        # Prepare source code using centralized token limit handling
        prepared = self._prepare_source_for_processing(
            source_code, ticket, previous_template
        )
        if prepared.was_modified:
            logger.info(
                f"Worker {self.worker_id}: Source {prepared.strategy_used} for "
                f"chrome ticket ({ticket.file_name})"
            )

        # Extract chrome tickets from ticket metadata
        chrome_tickets: list[ChromeTicket] = []
        chrome_data = ticket.metadata.get("chrome_tickets", [])
        for ct in chrome_data:
            if isinstance(ct, dict):
                chrome_tickets.append(ChromeTicket.model_validate(ct))
            elif isinstance(ct, ChromeTicket):
                chrome_tickets.append(ct)

        # Also check for single issue in issue_description
        if ticket.metadata.get("issue_description"):
            from war_rig.models.tickets import IssuePriority
            chrome_tickets.append(
                ChromeTicket(
                    ticket_id=f"CHR-{ticket.ticket_id[-8:]}",
                    description=ticket.metadata["issue_description"],
                    section=ticket.metadata.get("section"),
                    priority=IssuePriority.MEDIUM,
                    guidance=ticket.metadata.get("guidance"),
                )
            )

        if not chrome_tickets:
            logger.warning(
                f"Worker {self.worker_id}: No chrome issues found in ticket "
                f"{ticket.ticket_id}. Ticket metadata: chrome_tickets={bool(chrome_data)}, "
                f"issue_description={bool(ticket.metadata.get('issue_description'))}. "
                f"Marking for revalidation so Challenger can review from scratch."
            )
            # Return success with needs_revalidation flag so a new VALIDATION ticket
            # gets created and a Challenger can re-review the documentation
            return ScribeOutput(
                success=True,
                template=previous_template,  # Preserve existing work
                needs_revalidation=True,
            )

        # Get file_type from metadata first (set during initial documentation),
        # fall back to inference from filename
        file_type = self._get_file_type_from_metadata(ticket)

        # Build Scribe input with prepared source and previous template
        scribe_input = ScribeInput(
            source_code=prepared.source_code,
            file_name=ticket.file_name,
            file_type=file_type,
            iteration=ticket.cycle_number,
            copybook_contents=ticket.metadata.get("copybook_contents", {}),
            previous_template=previous_template,
            chrome_tickets=chrome_tickets,
            formatting_strict=formatting_strict,
        )

        logger.info(
            f"Worker {self.worker_id}: Processing chrome for {ticket.file_name} "
            f"with {len(chrome_tickets)} issues"
        )
        output = await self.scribe_agent.ainvoke(scribe_input)

        # Save updated template if successful
        if output.success and output.template:
            self._save_template(ticket.file_name, output.template)

        return output

    def _create_validation_ticket(
        self,
        doc_ticket: ProgramManagerTicket,
        result: ScribeOutput,
    ) -> ProgramManagerTicket | None:
        """Create a VALIDATION ticket for Challenger after completing documentation.

        The validation ticket contains the documentation state (template, source code)
        in its metadata so the Challenger can validate without re-reading files.

        Args:
            doc_ticket: The completed DOCUMENTATION ticket.
            result: The ScribeOutput containing the generated template.

        Returns:
            The created VALIDATION ticket, or None if creation failed.
        """
        # Build metadata with documentation state for Challenger
        validation_metadata: dict[str, Any] = {
            "parent_documentation_ticket": doc_ticket.ticket_id,
            "scribe_worker": self.worker_id,
        }

        # Include template as JSON for Challenger to validate
        if result.template:
            validation_metadata["template"] = result.template.model_dump(mode="json")

        # Include source code - use file_path from metadata if available
        metadata_path = doc_ticket.metadata.get("file_path")
        if metadata_path:
            source_path = Path(metadata_path)
        else:
            source_path = self.input_directory / doc_ticket.file_name

        # Store file_path only - workers load content from disk to avoid metadata bloat
        if source_path.exists():
            validation_metadata["file_path"] = str(source_path)
        else:
            logger.warning(
                f"Worker {self.worker_id}: Source file not found for validation: {source_path}"
            )

        # Include file type
        validation_metadata["file_type"] = self._determine_file_type(
            doc_ticket.file_name
        ).value

        # Create the validation ticket
        validation_ticket = self.beads_client.create_pm_ticket(
            ticket_type=TicketType.VALIDATION,
            file_name=doc_ticket.file_name,
            program_id=doc_ticket.program_id,
            cycle_number=doc_ticket.cycle_number,
            parent_ticket_id=doc_ticket.ticket_id,
            priority=BeadsPriority.MEDIUM,
            metadata=validation_metadata,
        )

        if validation_ticket:
            logger.info(
                f"Worker {self.worker_id}: Created validation ticket "
                f"{validation_ticket.ticket_id} for {doc_ticket.file_name}"
            )
        else:
            logger.warning(
                f"Worker {self.worker_id}: Failed to create validation ticket "
                f"for {doc_ticket.file_name}"
            )

        return validation_ticket

    def _determine_file_type(self, file_name: str) -> FileType:
        """Determine the FileType from a file name.

        Args:
            file_name: Name of the source file.

        Returns:
            Appropriate FileType based on extension.
        """
        lower_name = file_name.lower()

        if lower_name.endswith((".cbl", ".cob")):
            return FileType.COBOL
        elif lower_name.endswith((".cpy", ".copy")):
            return FileType.COPYBOOK
        elif lower_name.endswith(".jcl"):
            return FileType.JCL
        elif lower_name.endswith(".bms"):
            return FileType.BMS
        elif lower_name.endswith((".pli", ".pl1")):
            return FileType.PLI
        else:
            return FileType.OTHER

    def _get_file_type_from_metadata(self, ticket: ProgramManagerTicket) -> FileType:
        """Get FileType from ticket metadata, falling back to filename inference.

        Args:
            ticket: The ticket with potential file_type in metadata.

        Returns:
            FileType from metadata or inferred from filename.
        """
        file_type_str = ticket.metadata.get("file_type")
        if file_type_str:
            try:
                return FileType(file_type_str)
            except ValueError:
                # Try uppercase version
                try:
                    return FileType(file_type_str.upper())
                except ValueError:
                    logger.warning(
                        f"Worker {self.worker_id}: Invalid file_type '{file_type_str}' "
                        f"in ticket {ticket.ticket_id}, inferring from filename"
                    )
        return self._determine_file_type(ticket.file_name)


class ScribeWorkerPool:
    """Pool manager for multiple ScribeWorker instances.

    Manages the lifecycle of N ScribeWorker instances (configured via
    config.num_scribes), providing coordinated start/stop and status reporting.

    The pool uses asyncio for concurrent execution, with each worker running
    in its own task. Workers automatically stop when no more tickets are
    available (after idle_timeout).

    Responsibilities:
    - Create and manage N ScribeWorker instances
    - Start all workers concurrently
    - Stop workers gracefully on request or when work is complete
    - Provide aggregate status across all workers

    Attributes:
        config: War Rig configuration.
        beads_client: Shared client for ticket operations.
        num_workers: Number of worker instances.

    Example:
        config = load_config()  # num_scribes = 3
        client = BeadsClient()

        pool = ScribeWorkerPool(config=config, beads_client=client)
        await pool.start()

        # Poll status while processing
        while not pool.is_idle():
            status = pool.get_status()
            print(f"Active workers: {status['active_count']}")
            await asyncio.sleep(5)

        await pool.stop()

    See Also:
        - ScribeWorker: Individual worker implementation
        - docs/program_manager_architecture.md: Section 2.2 Worker Pool Manager
    """

    def __init__(
        self,
        config: WarRigConfig,
        beads_client: BeadsClient,
        input_directory: Path | None = None,
        output_directory: Path | None = None,
        num_workers: int | None = None,
        poll_interval: float = 2.0,
        idle_timeout: float = 30.0,
    ):
        """Initialize the Scribe worker pool.

        Args:
            config: War Rig configuration.
            beads_client: Client for ticket operations.
            input_directory: Directory containing source files to process.
            output_directory: Directory to store documentation outputs.
            num_workers: Number of workers. Defaults to config.num_scribes.
            poll_interval: Seconds between polls for each worker.
            idle_timeout: Seconds of no work before workers auto-stop.
        """
        self.config = config
        self.beads_client = beads_client
        self.input_directory = input_directory or config.input_directory
        self.output_directory = output_directory or config.output_directory
        self.num_workers = num_workers or config.num_scribes
        self.poll_interval = poll_interval
        self.idle_timeout = idle_timeout

        # Worker instances (created on start)
        self._workers: list[ScribeWorker] = []
        self._tasks: list[asyncio.Task[None]] = []
        self._started = False
        self._stopped = False

    async def start(self) -> None:
        """Start all workers in the pool.

        Creates worker instances and starts them as asyncio tasks.
        Workers begin polling for tickets immediately.

        Raises:
            RuntimeError: If the pool is already started.
        """
        if self._started:
            raise RuntimeError("Pool is already started")

        logger.info(f"ScribeWorkerPool: Starting {self.num_workers} workers")
        self._started = True

        # Create worker instances
        self._workers = [
            ScribeWorker(
                worker_id=f"scribe-{i + 1}",
                config=self.config,
                beads_client=self.beads_client,
                input_directory=self.input_directory,
                output_directory=self.output_directory,
                poll_interval=self.poll_interval,
                idle_timeout=self.idle_timeout,
            )
            for i in range(self.num_workers)
        ]

        # Start worker tasks
        self._tasks = [
            asyncio.create_task(worker.run(), name=worker.worker_id)
            for worker in self._workers
        ]

        logger.info(f"ScribeWorkerPool: All {self.num_workers} workers started")

    async def stop(self) -> None:
        """Gracefully stop all workers in the pool.

        Signals each worker to stop and waits for them to complete
        their current ticket before terminating.
        """
        if not self._started:
            return

        logger.info("ScribeWorkerPool: Stopping all workers")

        # Signal all workers to stop
        for worker in self._workers:
            await worker.stop()

        # Wait for all tasks to complete (with timeout)
        if self._tasks:
            try:
                await asyncio.wait_for(
                    asyncio.gather(*self._tasks, return_exceptions=True),
                    timeout=60.0,
                )
            except asyncio.TimeoutError:
                logger.warning("ScribeWorkerPool: Timeout waiting for workers, cancelling")
                for task in self._tasks:
                    if not task.done():
                        task.cancel()

        self._started = False
        self._stopped = True
        self._workers = []
        self._tasks = []

        logger.info("ScribeWorkerPool: All workers stopped")

    async def wait(self) -> None:
        """Wait for all workers to complete naturally.

        Workers will stop when there are no more tickets available
        for idle_timeout seconds.
        """
        if self._tasks:
            await asyncio.gather(*self._tasks, return_exceptions=True)

    def get_status(self) -> dict[str, Any]:
        """Get aggregate status of the worker pool.

        Returns:
            Dictionary containing:
            - started: Whether the pool is running
            - num_workers: Total workers in pool
            - active_count: Workers currently processing
            - idle_count: Workers waiting for work
            - stopped_count: Workers that have stopped
            - error_count: Workers in error state
            - total_processed: Total tickets processed across all workers
            - total_failed: Total tickets failed across all workers
            - workers: List of individual WorkerStatus objects
        """
        if not self._started:
            return {
                "started": False,
                "num_workers": self.num_workers,
                "active_count": 0,
                "idle_count": 0,
                "stopped_count": 0,
                "error_count": 0,
                "total_processed": 0,
                "total_failed": 0,
                "workers": [],
            }

        active = 0
        idle = 0
        stopped = 0
        error = 0
        total_processed = 0
        total_failed = 0
        worker_statuses = []

        for worker in self._workers:
            status = worker.status
            worker_statuses.append(status)
            total_processed += status.tickets_processed
            total_failed += status.tickets_failed

            if status.state == WorkerState.PROCESSING:
                active += 1
            elif status.state == WorkerState.IDLE:
                idle += 1
            elif status.state == WorkerState.POLLING:
                idle += 1
            elif status.state == WorkerState.STOPPED:
                stopped += 1
            elif status.state == WorkerState.ERROR:
                error += 1

        return {
            "started": self._started,
            "num_workers": self.num_workers,
            "active_count": active,
            "idle_count": idle,
            "stopped_count": stopped,
            "error_count": error,
            "total_processed": total_processed,
            "total_failed": total_failed,
            "workers": worker_statuses,
        }

    def is_idle(self) -> bool:
        """Check if all workers are idle or stopped.

        Returns:
            True if no workers are actively processing.
        """
        if not self._started:
            return True

        for worker in self._workers:
            if worker.status.state == WorkerState.PROCESSING:
                return False

        return True

    def is_done(self) -> bool:
        """Check if all workers have stopped.

        Returns:
            True if all workers are in STOPPED or ERROR state.
        """
        if not self._started:
            return True

        for worker in self._workers:
            if worker.status.state not in (WorkerState.STOPPED, WorkerState.ERROR):
                return False

        return True
