"""Challenger worker pool for parallel documentation validation.

This module provides the ChallengerWorker and ChallengerWorkerPool classes for
parallel validation of documentation in the Program Manager workflow.

Challengers poll for VALIDATION tickets (created when Scribe completes DOCUMENTATION),
validate the documentation using the ChallengerAgent, and either close the ticket
if valid or create REWORK tickets for the Scribe if issues are found.

Example:
    config = WarRigConfig(...)
    beads_client = BeadsClient()
    pool = ChallengerWorkerPool(
        num_workers=config.num_challengers,
        config=config,
        beads_client=beads_client,
    )
    await pool.start()
    # Workers will process until no more tickets available
    await pool.stop()
"""

from __future__ import annotations

import asyncio
import json
import logging
from collections.abc import Callable
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any

from war_rig.agents.challenger import ChallengerAgent, ChallengerInput, ChallengerOutput
from war_rig.beads import (
    BeadsClient,
    BeadsPriority,
    ProgramManagerTicket,
    TicketState,
    TicketType,
)
from war_rig.config import WarRigConfig
from war_rig.models.templates import DocumentationTemplate, FileType
from war_rig.models.tickets import QuestionSeverity
from war_rig.preprocessors.base import PreprocessorResult
from war_rig.utils import log_error

logger = logging.getLogger(__name__)


class WorkerState(str, Enum):
    """State of a worker in the pool."""

    IDLE = "idle"
    POLLING = "polling"
    PROCESSING = "processing"
    STOPPING = "stopping"
    STOPPED = "stopped"


@dataclass
class WorkerStatus:
    """Status information for a worker.

    Attributes:
        worker_id: Unique identifier for the worker.
        state: Current state of the worker.
        current_ticket_id: ID of the ticket being processed, if any.
        tickets_processed: Total tickets processed by this worker.
        tickets_validated: Tickets that passed validation.
        tickets_rejected: Tickets that required rework.
        last_activity: Timestamp of last activity.
        error_count: Number of errors encountered.
    """

    worker_id: str
    state: WorkerState = WorkerState.IDLE
    current_ticket_id: str | None = None
    tickets_processed: int = 0
    tickets_validated: int = 0
    tickets_rejected: int = 0
    last_activity: datetime = field(default_factory=datetime.utcnow)
    error_count: int = 0


@dataclass
class ValidationResult:
    """Result of validating a documentation ticket.

    Attributes:
        success: Whether validation completed without errors.
        is_valid: Whether the documentation passed validation.
        issues_found: List of issues found during validation.
        blocking_questions: Questions that require Scribe attention.
        challenger_output: Full output from the ChallengerAgent.
    """

    success: bool
    is_valid: bool
    issues_found: list[str] = field(default_factory=list)
    blocking_questions: list[dict[str, Any]] = field(default_factory=list)
    challenger_output: ChallengerOutput | None = None


class ChallengerWorker:
    """Worker that validates documentation tickets.

    The ChallengerWorker polls for VALIDATION tickets, validates the documentation
    using the ChallengerAgent, and updates ticket state appropriately.

    Attributes:
        worker_id: Unique identifier for this worker.
        config: War Rig configuration.
        beads_client: Client for ticket operations.
        challenger: The ChallengerAgent instance.
        status: Current status of the worker.

    Example:
        worker = ChallengerWorker(
            worker_id="challenger-1",
            config=config,
            beads_client=beads_client,
        )
        await worker.run()
    """

    def __init__(
        self,
        worker_id: str,
        config: WarRigConfig,
        beads_client: BeadsClient,
        poll_interval: float = 2.0,
        upstream_active_check: Callable[[], bool] | None = None,
    ):
        """Initialize the ChallengerWorker.

        Args:
            worker_id: Unique identifier for this worker.
            config: War Rig configuration.
            beads_client: Client for ticket operations.
            poll_interval: Seconds between polling attempts.
            upstream_active_check: Optional callback that returns True if upstream
                (documentation) is still in progress. When set, workers won't
                idle-timeout while upstream might produce more work.
        """
        self.worker_id = worker_id
        self.config = config
        self.beads_client = beads_client
        self.poll_interval = poll_interval
        self.upstream_active_check = upstream_active_check

        # Create the ChallengerAgent
        self.challenger = ChallengerAgent(
            config=config.challenger,
            api_config=config.api,
        )

        # Worker state
        self.status = WorkerStatus(worker_id=worker_id)
        self._stop_event = asyncio.Event()
        self._task: asyncio.Task[None] | None = None

        # Track tickets this worker has failed on (to avoid re-picking immediately)
        # Other workers with different LLMs can still pick them up
        self._failed_tickets: set[str] = set()

    async def start(self) -> None:
        """Start the worker's processing loop.

        Creates a background task that polls for and processes tickets.
        Returns immediately; use stop() to terminate.
        """
        if self._task is not None and not self._task.done():
            logger.warning(f"Worker {self.worker_id} is already running")
            return

        self._stop_event.clear()
        self._task = asyncio.create_task(self._run_loop())
        logger.info(f"Worker {self.worker_id} started")

    async def stop(self) -> None:
        """Stop the worker gracefully.

        Signals the worker to stop and waits for the current task to complete.
        """
        logger.info(f"Worker {self.worker_id} stopping")
        self.status.state = WorkerState.STOPPING
        self._stop_event.set()

        if self._task is not None:
            try:
                await asyncio.wait_for(self._task, timeout=30.0)
            except asyncio.TimeoutError:
                logger.warning(f"Worker {self.worker_id} did not stop in time, cancelling")
                self._task.cancel()
                try:
                    await self._task
                except asyncio.CancelledError:
                    pass

        self.status.state = WorkerState.STOPPED
        logger.info(f"Worker {self.worker_id} stopped")

    async def _run_loop(self) -> None:
        """Main worker loop that polls for and processes tickets."""
        consecutive_empty_polls = 0
        max_empty_polls = 10  # Stop after this many consecutive empty polls

        while not self._stop_event.is_set():
            try:
                self.status.state = WorkerState.POLLING
                self.status.last_activity = datetime.utcnow()

                # Poll for available VALIDATION tickets
                ticket = await self._poll_for_ticket()

                if ticket is None:
                    consecutive_empty_polls += 1

                    # If upstream (documentation) is still active, don't count
                    # toward idle timeout - more VALIDATION tickets may come
                    if self.upstream_active_check and self.upstream_active_check():
                        consecutive_empty_polls = 0
                        logger.debug(
                            f"Worker {self.worker_id}: No tickets but upstream active, waiting"
                        )
                    else:
                        logger.debug(
                            f"Worker {self.worker_id}: No tickets available "
                            f"({consecutive_empty_polls}/{max_empty_polls})"
                        )

                    # Stop if no tickets have been available for a while
                    if consecutive_empty_polls >= max_empty_polls:
                        logger.info(
                            f"Worker {self.worker_id}: No tickets available after "
                            f"{max_empty_polls} polls, stopping"
                        )
                        break

                    await asyncio.sleep(self.poll_interval)
                    continue

                # Reset empty poll counter when we get a ticket
                consecutive_empty_polls = 0

                # Process the ticket
                self.status.state = WorkerState.PROCESSING
                self.status.current_ticket_id = ticket.ticket_id
                self.status.last_activity = datetime.utcnow()

                try:
                    await self._process_ticket(ticket)
                except Exception as e:
                    logger.error(
                        f"Worker {self.worker_id}: Error processing ticket "
                        f"{ticket.ticket_id}: {e}"
                    )
                    log_error(
                        e,
                        ticket_id=ticket.ticket_id,
                        context={
                            "worker_id": self.worker_id,
                            "file_name": ticket.file_name,
                            "ticket_type": ticket.ticket_type.value,
                        },
                    )
                    self.status.error_count += 1

                    # Mark ticket as blocked on error
                    self.beads_client.update_ticket_state(
                        ticket.ticket_id,
                        TicketState.BLOCKED,
                        reason=f"Error during validation: {e}",
                    )
                finally:
                    self.status.current_ticket_id = None
                    self.status.tickets_processed += 1

            except asyncio.CancelledError:
                logger.debug(f"Worker {self.worker_id}: Cancelled")
                raise
            except Exception as e:
                logger.error(f"Worker {self.worker_id}: Unexpected error: {e}")
                self.status.error_count += 1
                await asyncio.sleep(self.poll_interval)

        self.status.state = WorkerState.IDLE

    async def _poll_for_ticket(self) -> ProgramManagerTicket | None:
        """Poll for an available VALIDATION ticket and claim it.

        Skips tickets this worker has already failed on (other workers
        with different LLMs can still pick them up).

        Returns:
            Claimed ticket, or None if no tickets available.
        """
        # Get available VALIDATION tickets
        available = self.beads_client.get_available_tickets(
            ticket_type=TicketType.VALIDATION,
        )

        if not available:
            return None

        # Filter out tickets this worker has already failed on
        available = [t for t in available if t.ticket_id not in self._failed_tickets]

        if not available:
            return None

        # Sort by priority (lower value = higher priority), then by creation time
        available.sort(key=lambda t: (t.cycle_number, t.created_at))

        # Try to claim a ticket atomically
        for ticket in available:
            if self.beads_client.claim_ticket(ticket.ticket_id, self.worker_id):
                logger.info(
                    f"Worker {self.worker_id}: Claimed ticket {ticket.ticket_id}"
                )

                # Update state to IN_PROGRESS
                self.beads_client.update_ticket_state(
                    ticket.ticket_id,
                    TicketState.IN_PROGRESS,
                )

                return ticket

        return None

    async def _process_ticket(self, ticket: ProgramManagerTicket) -> None:
        """Process a claimed VALIDATION ticket.

        Args:
            ticket: The ticket to process.
        """
        logger.info(
            f"Worker {self.worker_id}: Processing validation ticket "
            f"{ticket.ticket_id} for {ticket.file_name}"
        )

        # Load the documentation state from the ticket metadata
        state = self._load_documentation_state(ticket)

        if state is None:
            logger.error(
                f"Worker {self.worker_id}: Could not load documentation state "
                f"for ticket {ticket.ticket_id}"
            )
            self.beads_client.update_ticket_state(
                ticket.ticket_id,
                TicketState.BLOCKED,
                reason="Could not load documentation state from parent ticket",
            )
            return

        # Run validation
        result = await self._validate_documentation(state, ticket)

        if not result.success:
            # First failure - retry with enhanced formatting
            logger.warning(
                f"Worker {self.worker_id}: Validation failed for {ticket.ticket_id}, "
                f"retrying with strict formatting"
            )
            result = await self._validate_documentation(state, ticket, formatting_strict=True)

            if not result.success:
                # Second failure - reset ticket for other workers
                self._failed_tickets.add(ticket.ticket_id)
                self.beads_client.update_ticket_state(
                    ticket.ticket_id,
                    TicketState.CREATED,  # Reset for other workers
                    reason="Validation failed twice, available for other workers",
                )
                logger.warning(
                    f"Worker {self.worker_id}: Validation failed twice for {ticket.ticket_id}, "
                    f"resetting for other workers"
                )
                return

        if result.is_valid:
            # Documentation is valid - close the ticket
            logger.info(
                f"Worker {self.worker_id}: Documentation validated for "
                f"{ticket.file_name}"
            )
            self.beads_client.update_ticket_state(
                ticket.ticket_id,
                TicketState.COMPLETED,
                reason="Documentation validated successfully",
            )
            self.status.tickets_validated += 1
        else:
            # Issues found - create REWORK ticket for Scribe
            logger.info(
                f"Worker {self.worker_id}: Issues found in documentation for "
                f"{ticket.file_name}, creating rework ticket"
            )
            self._create_rework_ticket(ticket, result)
            self.beads_client.update_ticket_state(
                ticket.ticket_id,
                TicketState.COMPLETED,
                reason=f"Validation complete, {len(result.blocking_questions)} issues found",
            )
            self.status.tickets_rejected += 1

    def _load_documentation_state(
        self,
        ticket: ProgramManagerTicket,
    ) -> dict[str, Any] | None:
        """Load the documentation state from ticket metadata.

        The documentation state is stored in the ticket's metadata field
        as a JSON-serialized dictionary containing the template, source code,
        and other validation inputs.

        Args:
            ticket: The ticket containing state in metadata.

        Returns:
            Dictionary with documentation state, or None if loading failed.
        """
        metadata = ticket.metadata

        if not metadata:
            logger.warning(
                f"Worker {self.worker_id}: Ticket {ticket.ticket_id} has no metadata"
            )
            return None

        # Try to extract documentation state from metadata
        state: dict[str, Any] = {}

        # Get template from metadata (could be JSON string or dict)
        template_data = metadata.get("template")
        if template_data:
            if isinstance(template_data, str):
                try:
                    template_data = json.loads(template_data)
                except json.JSONDecodeError:
                    logger.error("Failed to parse template JSON")
                    return None
            try:
                state["template"] = DocumentationTemplate.model_validate(template_data)
            except Exception as e:
                # Try lenient parsing - schema mismatches from LLM outputs are common
                logger.warning(
                    f"Strict template validation failed: {e}. Trying lenient parsing."
                )
                try:
                    # Use model_construct to bypass validation entirely
                    # This creates the object without running validators
                    header = template_data.get("header", {})
                    purpose = template_data.get("purpose", {})
                    state["template"] = DocumentationTemplate.model_construct(
                        header=header if isinstance(header, dict) else {},
                        purpose=purpose if isinstance(purpose, dict) else {},
                        inputs=template_data.get("inputs", []),
                        outputs=template_data.get("outputs", []),
                        called_programs=template_data.get("called_programs", []),
                        calling_context=template_data.get("calling_context", {}),
                        business_rules=template_data.get("business_rules", []),
                        data_flow=template_data.get("data_flow", {}),
                        copybooks_used=template_data.get("copybooks_used", []),
                        paragraphs=template_data.get("paragraphs", []),
                        error_handling=template_data.get("error_handling", []),
                        open_questions=template_data.get("open_questions", []),
                    )
                    logger.info("Successfully used lenient parsing for template")
                except Exception as e2:
                    logger.error(f"Lenient parsing also failed: {e2}")
                    return None
        else:
            logger.warning("No template found in ticket metadata")
            return None

        # Get source code
        state["source_code"] = metadata.get("source_code", "")
        if not state["source_code"]:
            logger.warning("No source code found in ticket metadata")
            return None

        # Get file info
        state["file_name"] = ticket.file_name
        file_type_str = metadata.get("file_type", "other")
        try:
            state["file_type"] = FileType(file_type_str)
        except ValueError:
            state["file_type"] = FileType.OTHER

        # Get preprocessor result if available
        preprocessor_data = metadata.get("preprocessor_result")
        if preprocessor_data:
            if isinstance(preprocessor_data, str):
                try:
                    preprocessor_data = json.loads(preprocessor_data)
                except json.JSONDecodeError:
                    preprocessor_data = None
            if preprocessor_data:
                try:
                    state["preprocessor_result"] = PreprocessorResult.model_validate(
                        preprocessor_data
                    )
                except Exception:
                    state["preprocessor_result"] = None
        else:
            state["preprocessor_result"] = None

        # Get iteration
        state["iteration"] = ticket.cycle_number

        return state

    async def _validate_documentation(
        self,
        state: dict[str, Any],
        ticket: ProgramManagerTicket,
        formatting_strict: bool = False,
    ) -> ValidationResult:
        """Validate documentation using the ChallengerAgent.

        Args:
            state: The documentation state to validate.
            ticket: The ticket being processed.
            formatting_strict: If True, add extra JSON formatting instructions.

        Returns:
            ValidationResult with validation outcome.
        """
        try:
            # Build ChallengerInput
            challenger_input = ChallengerInput(
                template=state["template"],
                source_code=state["source_code"],
                file_name=state["file_name"],
                file_type=state["file_type"],
                preprocessor_result=state.get("preprocessor_result"),
                iteration=state.get("iteration", 1),
                max_questions=self.config.max_questions_per_round,
                formatting_strict=formatting_strict,
            )

            # Invoke the ChallengerAgent
            output = await self.challenger.ainvoke(challenger_input)

            if not output.success:
                return ValidationResult(
                    success=False,
                    is_valid=False,
                    challenger_output=output,
                )

            # Analyze the output to determine if documentation is valid
            issues_found = output.issues_found or []
            blocking_questions = [
                {
                    "question_id": q.question_id,
                    "section": q.section,
                    "question_type": q.question_type.value,
                    "question": q.question,
                    "severity": q.severity.value,
                    "evidence": q.evidence,
                }
                for q in output.questions
                if q.severity == QuestionSeverity.BLOCKING
            ]

            # Documentation is valid if no blocking questions and no critical issues
            is_valid = len(blocking_questions) == 0 and all(
                "WRONG" not in issue.upper() for issue in issues_found
            )

            return ValidationResult(
                success=True,
                is_valid=is_valid,
                issues_found=issues_found,
                blocking_questions=blocking_questions,
                challenger_output=output,
            )

        except Exception as e:
            logger.error(f"Validation error: {e}")
            log_error(
                e,
                ticket_id=ticket.ticket_id,
                context={
                    "worker_id": self.worker_id,
                    "file_name": ticket.file_name,
                    "operation": "_validate_documentation",
                },
            )
            return ValidationResult(
                success=False,
                is_valid=False,
                issues_found=[str(e)],
            )

    def _create_rework_ticket(
        self,
        validation_ticket: ProgramManagerTicket,
        result: ValidationResult,
    ) -> ProgramManagerTicket | None:
        """Create a REWORK ticket for the Scribe to address issues.

        Args:
            validation_ticket: The validation ticket that found issues.
            result: The validation result with issues.

        Returns:
            The created rework ticket, or None if creation failed.
        """
        # Build metadata for the rework ticket
        # Use "challenger_questions" key - this is what Scribe looks for
        rework_metadata: dict[str, Any] = {
            "parent_validation_ticket": validation_ticket.ticket_id,
            "issues_found": result.issues_found,
            "challenger_questions": result.blocking_questions,  # Scribe expects this key
            "challenger_worker": self.worker_id,
        }

        # Include the original documentation state so Scribe can update it
        if validation_ticket.metadata:
            rework_metadata["template"] = validation_ticket.metadata.get("template")
            rework_metadata["source_code"] = validation_ticket.metadata.get("source_code")
            rework_metadata["file_type"] = validation_ticket.metadata.get("file_type")
            rework_metadata["preprocessor_result"] = validation_ticket.metadata.get(
                "preprocessor_result"
            )

        # Determine priority based on number of issues
        num_issues = len(result.blocking_questions)
        if num_issues >= 3:
            priority = BeadsPriority.HIGH
        elif num_issues >= 1:
            priority = BeadsPriority.MEDIUM
        else:
            priority = BeadsPriority.LOW

        # Create the rework ticket
        # Use same cycle number - cycle only increments when orchestrator starts new cycle
        rework_ticket = self.beads_client.create_pm_ticket(
            ticket_type=TicketType.CLARIFICATION,  # CLARIFICATION for Scribe to address
            file_name=validation_ticket.file_name,
            program_id=validation_ticket.program_id,
            cycle_number=validation_ticket.cycle_number,
            parent_ticket_id=validation_ticket.ticket_id,
            priority=priority,
            metadata=rework_metadata,
        )

        if rework_ticket:
            logger.info(
                f"Worker {self.worker_id}: Created rework ticket "
                f"{rework_ticket.ticket_id} for {validation_ticket.file_name}"
            )

        return rework_ticket

    def get_status(self) -> WorkerStatus:
        """Get the current worker status.

        Returns:
            Current WorkerStatus.
        """
        return self.status


class ChallengerWorkerPool:
    """Pool of ChallengerWorker instances for parallel validation.

    The ChallengerWorkerPool manages multiple ChallengerWorker instances,
    coordinating their startup, shutdown, and status reporting.

    Attributes:
        num_workers: Number of workers in the pool.
        config: War Rig configuration.
        beads_client: Client for ticket operations.
        workers: List of ChallengerWorker instances.

    Example:
        pool = ChallengerWorkerPool(
            num_workers=2,
            config=config,
            beads_client=beads_client,
        )
        await pool.start()
        status = pool.get_status()
        await pool.stop()
    """

    def __init__(
        self,
        num_workers: int,
        config: WarRigConfig,
        beads_client: BeadsClient,
        poll_interval: float = 2.0,
        upstream_active_check: Callable[[], bool] | None = None,
    ):
        """Initialize the ChallengerWorkerPool.

        Args:
            num_workers: Number of workers to create.
            config: War Rig configuration.
            beads_client: Client for ticket operations.
            poll_interval: Seconds between polling attempts for workers.
            upstream_active_check: Optional callback that returns True if upstream
                (documentation) is still in progress. When set, workers won't
                idle-timeout while upstream might produce more work. This enables
                running Scribes and Challengers in parallel as a pipeline.
        """
        self.num_workers = num_workers
        self.config = config
        self.beads_client = beads_client
        self.poll_interval = poll_interval
        self.upstream_active_check = upstream_active_check

        # Create workers
        self.workers: list[ChallengerWorker] = []
        for i in range(num_workers):
            worker = ChallengerWorker(
                worker_id=f"challenger-{i + 1}",
                config=config,
                beads_client=beads_client,
                poll_interval=poll_interval,
                upstream_active_check=upstream_active_check,
            )
            self.workers.append(worker)

        self._running = False
        logger.info(f"ChallengerWorkerPool initialized with {num_workers} workers")

    async def start(self) -> None:
        """Start all workers in the pool.

        Starts each worker's processing loop concurrently.
        """
        if self._running:
            logger.warning("ChallengerWorkerPool is already running")
            return

        self._running = True
        logger.info(f"Starting {self.num_workers} challenger workers")

        # Start all workers
        await asyncio.gather(*[worker.start() for worker in self.workers])

        logger.info("All challenger workers started")

    async def stop(self) -> None:
        """Gracefully stop all workers in the pool.

        Signals all workers to stop and waits for them to complete.
        """
        if not self._running:
            logger.warning("ChallengerWorkerPool is not running")
            return

        logger.info(f"Stopping {self.num_workers} challenger workers")

        # Stop all workers concurrently
        await asyncio.gather(
            *[worker.stop() for worker in self.workers],
            return_exceptions=True,
        )

        self._running = False
        logger.info("All challenger workers stopped")

    async def wait_for_completion(self) -> None:
        """Wait for all workers to complete their current work and stop.

        This method waits for all worker tasks to finish. Workers will stop
        automatically when no more tickets are available.
        """
        if not self._running:
            return

        tasks = [
            worker._task for worker in self.workers
            if worker._task is not None and not worker._task.done()
        ]

        if tasks:
            await asyncio.gather(*tasks, return_exceptions=True)

        self._running = False

    def get_status(self) -> dict[str, Any]:
        """Get status of all workers in the pool.

        Returns:
            Dictionary containing pool status and per-worker status.
        """
        worker_statuses = [worker.get_status() for worker in self.workers]

        # Aggregate statistics
        total_processed = sum(s.tickets_processed for s in worker_statuses)
        total_validated = sum(s.tickets_validated for s in worker_statuses)
        total_rejected = sum(s.tickets_rejected for s in worker_statuses)
        total_errors = sum(s.error_count for s in worker_statuses)

        active_workers = sum(
            1 for s in worker_statuses
            if s.state in (WorkerState.POLLING, WorkerState.PROCESSING)
        )

        return {
            "running": self._running,
            "num_workers": self.num_workers,
            "active_workers": active_workers,
            "total_tickets_processed": total_processed,
            "total_tickets_validated": total_validated,
            "total_tickets_rejected": total_rejected,
            "total_errors": total_errors,
            "workers": [
                {
                    "worker_id": s.worker_id,
                    "state": s.state.value,
                    "current_ticket_id": s.current_ticket_id,
                    "tickets_processed": s.tickets_processed,
                    "tickets_validated": s.tickets_validated,
                    "tickets_rejected": s.tickets_rejected,
                    "error_count": s.error_count,
                    "last_activity": s.last_activity.isoformat(),
                }
                for s in worker_statuses
            ],
        }

    @property
    def is_running(self) -> bool:
        """Check if the pool is currently running."""
        return self._running
