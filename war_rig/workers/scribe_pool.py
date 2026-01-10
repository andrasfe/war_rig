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
import logging
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any

from war_rig.agents.scribe import ScribeAgent, ScribeInput, ScribeOutput
from war_rig.beads import (
    BeadsClient,
    BeadsPriority,
    ProgramManagerTicket,
    TicketState,
    TicketType,
)
from war_rig.config import WarRigConfig
from war_rig.models.templates import FileType

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
        poll_interval: float = 2.0,
        idle_timeout: float = 30.0,
    ):
        """Initialize the Scribe worker.

        Args:
            worker_id: Unique identifier for this worker.
            config: War Rig configuration for agent setup.
            beads_client: Client for ticket operations.
            poll_interval: Seconds between polling attempts when no work available.
            idle_timeout: Seconds of no available tickets before stopping.
        """
        self.worker_id = worker_id
        self.config = config
        self.beads_client = beads_client
        self.poll_interval = poll_interval
        self.idle_timeout = idle_timeout

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
                self._last_work_time = datetime.utcnow()
                await self._process_ticket(ticket)

        except asyncio.CancelledError:
            logger.info(f"Worker {self.worker_id}: Cancelled")
            raise

        except Exception as e:
            logger.error(f"Worker {self.worker_id}: Fatal error: {e}")
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
        logger.info(f"Worker {self.worker_id}: Stop requested")
        self._should_stop = True

    async def _poll_for_ticket(self) -> ProgramManagerTicket | None:
        """Poll for and claim an available ticket.

        Queries the BeadsClient for DOCUMENTATION tickets in CREATED state,
        then attempts to claim one atomically.

        Returns:
            Claimed ProgramManagerTicket if successful, None otherwise.
        """
        # Get available tickets for our compatible types
        # For now, focus on DOCUMENTATION tickets
        tickets = self.beads_client.get_available_tickets(
            ticket_type=TicketType.DOCUMENTATION,
        )

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
            if result.success:
                self.beads_client.update_ticket_state(
                    ticket.ticket_id,
                    TicketState.COMPLETED,
                    reason="Documentation completed successfully",
                )
                self._status.tickets_processed += 1
                logger.info(
                    f"Worker {self.worker_id}: Completed ticket {ticket.ticket_id}"
                )
            else:
                self.beads_client.update_ticket_state(
                    ticket.ticket_id,
                    TicketState.BLOCKED,
                    reason=f"Processing failed: {result.error}",
                )
                self._status.tickets_failed += 1
                logger.warning(
                    f"Worker {self.worker_id}: Ticket {ticket.ticket_id} "
                    f"failed: {result.error}"
                )

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

    async def _process_documentation_ticket(
        self,
        ticket: ProgramManagerTicket,
    ) -> ScribeOutput:
        """Process a DOCUMENTATION ticket.

        Loads the source file and runs initial documentation through ScribeAgent.

        Args:
            ticket: The DOCUMENTATION ticket to process.

        Returns:
            ScribeOutput with documentation results.
        """
        # Load source code from metadata if available
        source_code = ticket.metadata.get("source_code", "")
        if not source_code:
            logger.warning(
                f"Worker {self.worker_id}: No source code in ticket {ticket.ticket_id}"
            )
            return ScribeOutput(
                success=False,
                error="No source code provided in ticket metadata",
            )

        # Determine file type from extension
        file_type = self._determine_file_type(ticket.file_name)

        # Build Scribe input
        scribe_input = ScribeInput(
            source_code=source_code,
            file_name=ticket.file_name,
            file_type=file_type,
            iteration=ticket.cycle_number,
            copybook_contents=ticket.metadata.get("copybook_contents", {}),
        )

        # Run the Scribe agent
        logger.debug(
            f"Worker {self.worker_id}: Running Scribe for {ticket.file_name}"
        )
        output = await self.scribe_agent.ainvoke(scribe_input)

        return output

    async def _process_clarification_ticket(
        self,
        ticket: ProgramManagerTicket,
    ) -> ScribeOutput:
        """Process a CLARIFICATION ticket.

        Responds to a Challenger question about existing documentation.

        Args:
            ticket: The CLARIFICATION ticket to process.

        Returns:
            ScribeOutput with response to the question.
        """
        # TODO: Implement clarification handling
        # This requires loading the original documentation and question
        logger.info(
            f"Worker {self.worker_id}: CLARIFICATION tickets not yet implemented"
        )
        return ScribeOutput(
            success=False,
            error="CLARIFICATION tickets not yet implemented",
        )

    async def _process_chrome_ticket(
        self,
        ticket: ProgramManagerTicket,
    ) -> ScribeOutput:
        """Process a CHROME ticket.

        Addresses an Imperator-issued rework request.

        Args:
            ticket: The CHROME ticket to process.

        Returns:
            ScribeOutput with reworked documentation.
        """
        # TODO: Implement chrome ticket handling
        # This requires loading the original documentation and chrome issue
        logger.info(
            f"Worker {self.worker_id}: CHROME tickets not yet implemented"
        )
        return ScribeOutput(
            success=False,
            error="CHROME tickets not yet implemented",
        )

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
        num_workers: int | None = None,
        poll_interval: float = 2.0,
        idle_timeout: float = 30.0,
    ):
        """Initialize the Scribe worker pool.

        Args:
            config: War Rig configuration.
            beads_client: Client for ticket operations.
            num_workers: Number of workers. Defaults to config.num_scribes.
            poll_interval: Seconds between polls for each worker.
            idle_timeout: Seconds of no work before workers auto-stop.
        """
        self.config = config
        self.beads_client = beads_client
        self.num_workers = num_workers or config.num_scribes
        self.poll_interval = poll_interval
        self.idle_timeout = idle_timeout

        # Worker instances (created on start)
        self._workers: list[ScribeWorker] = []
        self._tasks: list[asyncio.Task[None]] = []
        self._started = False

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
