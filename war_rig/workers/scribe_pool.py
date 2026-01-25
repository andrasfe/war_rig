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
import re
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
from war_rig.models.templates import (
    CallerReference,
    DocumentationTemplate,
    FileType,
    FunctionCall,
)
from war_rig.workers.source_preparer import (
    PreparationContext,
    PreparedSource,
    SourceCodePreparer,
)
from war_rig.models.tickets import ChallengerQuestion, ChromeTicket
from war_rig.utils import log_error
from war_rig.utils.exceptions import FatalWorkerError
from war_rig.utils.file_lock import FileLockManager

logger = logging.getLogger(__name__)

# Maximum function body size (chars) before chunking
MAX_FUNCTION_BODY_CHARS = 8000


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

    # File extensions to search for discovery tickets
    DISCOVERY_EXTENSIONS = [
        ".cbl", ".cob", ".cobol",  # COBOL
        ".prc", ".proc",           # JCL procedures
        ".jcl",                    # JCL
        ".asm", ".s",              # Assembler
        ".pli", ".pl1",            # PL/I
        ".cpy", ".copy",           # Copybooks
    ]

    # Patterns to search for symbol DEFINITIONS in source code (not calls)
    # These patterns find where a symbol is defined, not where it's referenced
    SYMBOL_DEFINITION_PATTERNS = [
        # COBOL definitions
        r"^\s*{symbol}\s+SECTION\s*\.",           # Section definition
        r"^\s*{symbol}\s*\.\s*$",                 # Paragraph definition (name alone on line ending with period)
        r"PROGRAM-ID\s*\.\s*{symbol}",            # Program ID with period
        r"PROGRAM-ID\s+{symbol}",                 # Program ID without period
        # JCL definitions
        r"^//\s*{symbol}\s+PROC",                 # JCL PROC definition
        # Assembler definitions
        r"^{symbol}\s+CSECT",                     # Control section
        r"^{symbol}\s+START",                     # Program start
        r"^{symbol}\s+ENTRY",                     # Entry point
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
        file_lock_manager: FileLockManager | None = None,
        exit_on_error: bool = True,
        dependency_graph_path: Path | None = None,
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
            file_lock_manager: Optional centralized lock manager for file locking.
                When provided, workers will acquire locks on output files before
                processing and skip files that are locked by other workers.
            exit_on_error: If True, raise FatalWorkerError on processing errors
                instead of just logging. Default True.
            dependency_graph_path: Optional path to Citadel dependency graph JSON.
                When provided, workers can use Citadel to enrich documentation
                with function call references and cross-file relationships.
        """
        self.worker_id = worker_id
        self.config = config
        self.beads_client = beads_client
        self.exit_on_error = exit_on_error
        # Resolve to absolute paths to ensure consistent file access
        self.input_directory = (input_directory or config.input_directory).resolve()
        self.output_directory = (output_directory or config.output_directory).resolve()
        self.poll_interval = poll_interval
        self.idle_timeout = idle_timeout
        self.file_lock_manager = file_lock_manager

        # Ensure output directory exists
        self.output_directory.mkdir(parents=True, exist_ok=True)

        # Initialize the Scribe agent
        self._scribe_agent: ScribeAgent | None = None

        # Initialize Citadel SDK if dependency graph path available
        self._citadel = None
        self._dependency_graph_path = dependency_graph_path
        if dependency_graph_path and dependency_graph_path.exists():
            try:
                from citadel import Citadel
                self._citadel = Citadel()
                logger.debug(f"Worker {worker_id}: Citadel SDK initialized")
            except ImportError:
                logger.debug(f"Worker {worker_id}: Citadel SDK not available")

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

        # Track tickets released due to file locks with their release timestamps
        # This prevents infinite loops where a worker claims the same ticket repeatedly
        # when the file is locked by another worker. After the skip duration expires,
        # the ticket becomes eligible for claiming again.
        self._lock_skipped_tickets: dict[str, datetime] = {}
        self._lock_skip_duration: float = 5.0  # seconds to skip a lock-released ticket

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

    def _get_citadel_context(self, file_path: str) -> dict | None:
        """Get Citadel analysis context for a file.

        Uses the Citadel SDK to analyze a source file and extract function
        definitions with their outgoing calls and includes.

        Args:
            file_path: Path to the source file to analyze.

        Returns:
            Dictionary with:
            - functions: list of {name, type, calls: [{target, type, line}]}
            - includes: list of included file names

            Returns None if Citadel is not available or analysis fails.
        """
        if not self._citadel:
            return None

        try:
            functions = self._citadel.get_functions(file_path)
            includes = self._citadel.get_includes(file_path)

            logger.debug(
                f"Worker {self.worker_id}: Citadel found {len(functions)} functions "
                f"and {len(includes)} includes in {file_path}"
            )

            return {"functions": functions, "includes": includes}
        except Exception as e:
            logger.debug(
                f"Worker {self.worker_id}: Citadel analysis failed for {file_path}: {e}"
            )
            return None

    def _chunk_function_body(
        self, body: str, max_chars: int = MAX_FUNCTION_BODY_CHARS
    ) -> list[str]:
        """Split large function body into chunks for processing.

        Attempts to split at logical boundaries (blank lines, paragraph headers).
        Falls back to character-based splitting if needed.

        Args:
            body: The function body text.
            max_chars: Maximum characters per chunk.

        Returns:
            List of chunks (each <= max_chars).
        """
        if len(body) <= max_chars:
            return [body]

        chunks = []
        current_chunk: list[str] = []
        current_size = 0

        # Try to split at blank lines or paragraph boundaries
        lines = body.split("\n")

        for line in lines:
            line_size = len(line) + 1  # +1 for newline

            # Check for paragraph boundary (COBOL paragraph header pattern)
            is_boundary = (
                line.strip().endswith(".")
                and len(line) - len(line.lstrip()) < 12
                and line.strip().replace("-", "").replace("_", "").isalnum()
            )

            if current_size + line_size > max_chars and current_chunk:
                # Save current chunk
                chunks.append("\n".join(current_chunk))
                current_chunk = []
                current_size = 0

            current_chunk.append(line)
            current_size += line_size

        # Don't forget the last chunk
        if current_chunk:
            chunks.append("\n".join(current_chunk))

        return chunks

    async def _enrich_paragraphs_with_citadel(
        self,
        template: DocumentationTemplate,
        file_path: str,
        citadel_context: dict,
    ) -> DocumentationTemplate:
        """Enrich template paragraphs with Citadel call references.

        For each paragraph in the template that matches a Citadel-identified
        function, adds structured outgoing_calls and incoming_calls references.
        These are programmatic (from static analysis), not LLM-generated.

        Args:
            template: The documentation template to enrich.
            file_path: Path to the source file (used for caller lookup).
            citadel_context: Context from _get_citadel_context() with functions/includes.

        Returns:
            The template with enriched paragraphs (modified in place).
        """
        if not template.paragraphs or not self._citadel:
            return template

        # Build lookup of function name -> outgoing calls (case-insensitive)
        func_calls: dict[str, list[dict]] = {}
        for func in citadel_context.get("functions", []):
            name = func.get("name", "")
            if name:
                func_calls[name.lower()] = func.get("calls", [])

        for para in template.paragraphs:
            para_name = para.paragraph_name
            if not para_name:
                continue

            para_name_lower = para_name.lower()

            # Add outgoing calls from Citadel
            if para_name_lower in func_calls:
                para.outgoing_calls = [
                    FunctionCall(
                        target=c.get("target", ""),
                        call_type=c.get("type", "performs"),
                        line=c.get("line"),
                    )
                    for c in func_calls[para_name_lower]
                    if c.get("target")
                ]

            # Add incoming calls (callers) from cross-file analysis
            try:
                callers = self._citadel.get_callers(file_path, para_name)
                para.incoming_calls = [
                    CallerReference(
                        file=c.get("file", ""),
                        function=c.get("function", ""),
                        line=c.get("line"),
                        call_type=c.get("type", "performs"),
                    )
                    for c in callers
                    if c.get("file") and c.get("function")
                ]
            except Exception:
                # Skip if callers lookup fails - don't block documentation
                pass

            # Get function body and chunk if needed for LLM processing
            try:
                body = self._citadel.get_function_body(file_path, para_name)
                if body and len(body) > MAX_FUNCTION_BODY_CHARS:
                    chunks = self._chunk_function_body(body)
                    # Store chunk count in metadata for potential future use
                    if not hasattr(para, "metadata") or para.metadata is None:
                        para.metadata = {}
                    para.metadata["body_chunks"] = len(chunks)
                    logger.debug(
                        f"Function {para_name} body split into {len(chunks)} chunks"
                    )
            except Exception:
                # Graceful handling - if chunking fails, continue without chunking
                pass

        logger.debug(
            f"Worker {self.worker_id}: Enriched {len(template.paragraphs)} paragraphs "
            f"with Citadel call references"
        )

        return template

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

        # Filter out tickets recently released due to file locks
        # This prevents infinite loops where a worker repeatedly claims and releases
        # the same ticket when the file is locked by another worker
        now = datetime.utcnow()
        expired_skips = [
            ticket_id
            for ticket_id, skip_time in self._lock_skipped_tickets.items()
            if (now - skip_time).total_seconds() >= self._lock_skip_duration
        ]
        for ticket_id in expired_skips:
            del self._lock_skipped_tickets[ticket_id]

        tickets = [
            t for t in tickets if t.ticket_id not in self._lock_skipped_tickets
        ]

        if not tickets:
            return None

        # Sort by priority (lower value = higher priority), then by created_at
        tickets.sort(key=lambda t: (t.cycle_number, t.created_at))

        # Try to claim the first available ticket
        for ticket in tickets:
            # Check file lock BEFORE claiming to avoid wasteful claim-then-release churn
            # This is an optimization - the actual lock is still acquired in _process_ticket()
            if self.file_lock_manager is not None:
                output_file = self._get_output_file_for_ticket(ticket)
                if await self.file_lock_manager.is_locked_by_other(
                    output_file, self.worker_id
                ):
                    # File is locked by another worker, skip this ticket
                    logger.debug(
                        f"Worker {self.worker_id}: Skipping ticket {ticket.ticket_id} - "
                        f"file {output_file} is locked by another worker"
                    )
                    continue

            if self.beads_client.claim_ticket(ticket.ticket_id, self.worker_id):
                logger.info(
                    f"Worker {self.worker_id}: Claimed ticket {ticket.ticket_id} "
                    f"({ticket.file_name})"
                )
                return ticket

        # All tickets were claimed by other workers or had locked files
        return None

    def _get_output_file_for_ticket(self, ticket: ProgramManagerTicket) -> str:
        """Get the output file path for a ticket.

        Used for file locking to prevent concurrent access.

        Args:
            ticket: The ticket to get the output file for.

        Returns:
            Absolute path to the output .doc.json file.
        """
        return str(self._get_doc_output_path(ticket.file_name).resolve())

    async def _process_ticket(self, ticket: ProgramManagerTicket) -> None:
        """Process a claimed ticket.

        Updates ticket to IN_PROGRESS, runs the ScribeAgent, and updates
        the final state (COMPLETED or BLOCKED).

        If a FileLockManager is configured, acquires a lock on the output
        file before processing and releases it when done (success or failure).
        If the lock cannot be acquired, the ticket is released back to the
        queue for another worker to pick up.

        Args:
            ticket: The claimed ticket to process.
        """
        self._update_state(WorkerState.PROCESSING, ticket.ticket_id)

        # Get the output file path for locking
        output_file = self._get_output_file_for_ticket(ticket)
        lock_acquired = False

        try:
            # Acquire file lock if manager is available
            if self.file_lock_manager is not None:
                lock_acquired = await self.file_lock_manager.acquire(
                    output_file, self.worker_id
                )
                if not lock_acquired:
                    # Another worker has the lock - release ticket back to queue
                    # and track it to prevent immediate re-claiming (infinite loop fix)
                    logger.info(
                        f"Worker {self.worker_id}: File {output_file} is locked, "
                        f"releasing ticket {ticket.ticket_id} for another worker"
                    )
                    self.beads_client.update_ticket_state(
                        ticket.ticket_id,
                        TicketState.CREATED,
                        reason="File locked by another worker, released for retry",
                    )
                    # Track this ticket to avoid re-claiming it immediately
                    # This prevents the infinite loop where we claim -> lock fail -> release -> claim
                    self._lock_skipped_tickets[ticket.ticket_id] = datetime.utcnow()
                    return

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

            # If exit_on_error is enabled, raise FatalWorkerError to terminate
            if self.exit_on_error:
                raise FatalWorkerError(
                    worker_id=self.worker_id,
                    ticket_id=ticket.ticket_id,
                    error=str(e),
                )

        finally:
            # Release file lock if we acquired one
            if self.file_lock_manager is not None and lock_acquired:
                await self.file_lock_manager.release(output_file, self.worker_id)
            self._update_state(WorkerState.IDLE)

    def _get_doc_output_path(self, file_name: str) -> Path:
        """Get the output path for a documentation file.

        The output path preserves the full filename (including extension) to
        avoid naming conflicts between files with the same stem but different
        extensions (e.g., XYZ100.cbl vs XYZ100.lst).

        Examples:
        - Input: "app/cobol/PROG.cbl" -> Output: "output/app/cobol/PROG.cbl.doc.json"
        - Input: "PROG.cbl" (flat) -> Output: "output/PROG.cbl.doc.json"
        - Input: "PROG.lst" (flat) -> Output: "output/PROG.lst.doc.json"

        Args:
            file_name: Source file name or relative path (e.g., "app/cobol/PROG.cbl").

        Returns:
            Path to the documentation JSON file.
        """
        rel_path = Path(file_name)

        # Use full filename (with extension) to avoid naming conflicts
        # e.g., XYZ100.cbl -> XYZ100.cbl.doc.json
        doc_filename = f"{rel_path.name}.doc.json"

        # If the path has a parent directory, mirror the structure
        if rel_path.parent != Path("."):
            return self.output_directory / rel_path.parent / doc_filename

        # Flat path (legacy or root-level files)
        return self.output_directory / doc_filename

    def _load_previous_template(self, file_name: str) -> DocumentationTemplate | None:
        """Load the previous iteration's documentation template.

        Args:
            file_name: Source file name or relative path.

        Returns:
            DocumentationTemplate if exists, None otherwise.
        """
        doc_path = self._get_doc_output_path(file_name)

        if not doc_path.exists():
            logger.debug(
                f"Worker {self.worker_id}: No previous template found at {doc_path}"
            )
            return None

        try:
            with open(doc_path) as f:
                data = json.load(f)
            logger.debug(
                f"Worker {self.worker_id}: Loaded previous template from {doc_path}"
            )
            return DocumentationTemplate.load_lenient(data)
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

            template = DocumentationTemplate.load_lenient(template_data)
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

    def _generate_markdown_from_template(
        self,
        template: DocumentationTemplate,
        file_name: str,
    ) -> str:
        """Generate human-readable markdown from a documentation template.

        Creates a well-formatted markdown document suitable for reading
        in documentation viewers, IDEs, and GitHub.

        Args:
            template: The documentation template to convert.
            file_name: Source file name for display in the header.

        Returns:
            Formatted markdown string.
        """
        parts = []

        # Header
        program_id = template.header.program_id if template.header else "Unknown"
        parts.append(f"# {program_id}")
        parts.append("")
        parts.append(f"**File**: `{file_name}`")
        if template.header:
            if template.header.file_type:
                parts.append(f"**Type**: {template.header.file_type}")
            if template.header.analyzed_at:
                parts.append(f"**Analyzed**: {template.header.analyzed_at}")
        parts.append("")

        # Purpose
        if template.purpose:
            parts.append("## Purpose")
            parts.append("")
            if template.purpose.summary:
                parts.append(template.purpose.summary)
                parts.append("")
            if template.purpose.business_context:
                parts.append(f"**Business Context**: {template.purpose.business_context}")
                parts.append("")

        # Inputs
        if template.inputs:
            parts.append("## Inputs")
            parts.append("")
            parts.append("| Name | Type | Description |")
            parts.append("|------|------|-------------|")
            for inp in template.inputs:
                name = inp.name or ""
                io_type = inp.io_type or ""
                description = inp.description or ""
                parts.append(f"| {name} | {io_type} | {description} |")
            parts.append("")

        # Outputs
        if template.outputs:
            parts.append("## Outputs")
            parts.append("")
            parts.append("| Name | Type | Description |")
            parts.append("|------|------|-------------|")
            for out in template.outputs:
                name = out.name or ""
                io_type = out.io_type or ""
                description = out.description or ""
                parts.append(f"| {name} | {io_type} | {description} |")
            parts.append("")

        # Called Programs
        if template.called_programs:
            parts.append("## Called Programs")
            parts.append("")
            parts.append("| Program | Call Type | Purpose |")
            parts.append("|---------|-----------|---------|")
            for prog in template.called_programs:
                program_name = prog.program_name or ""
                call_type = prog.call_type or ""
                purpose = prog.purpose or ""
                parts.append(f"| {program_name} | {call_type} | {purpose} |")
            parts.append("")

        # Business Rules
        if template.business_rules:
            parts.append("## Business Rules")
            parts.append("")
            for rule in template.business_rules:
                rule_id = rule.rule_id or "Rule"
                description = rule.description or ""
                parts.append(f"- **{rule_id}**: {description}")
            parts.append("")

        # Paragraphs/Functions
        if template.paragraphs:
            parts.append("## Paragraphs/Procedures")
            parts.append("")
            for para in template.paragraphs:
                para_name = para.paragraph_name or "Unknown"
                parts.append(f"### {para_name}")
                if para.purpose:
                    parts.append(para.purpose)
                if para.summary:
                    parts.append(para.summary)
                parts.append("")

        # Open Questions
        if template.open_questions:
            parts.append("## Open Questions")
            parts.append("")
            for q in template.open_questions:
                question = q.question or ""
                parts.append(f"- ? {question}")
                if q.context:
                    parts.append(f"  - Context: {q.context}")
            parts.append("")

        return "\n".join(parts)

    def _save_template(self, file_name: str, template: DocumentationTemplate) -> None:
        """Save the documentation template to the output directory.

        Creates parent directories as needed to mirror input structure.
        Creates a backup of the existing template before overwriting to protect
        against corruption from bad LLM responses.

        Args:
            file_name: Source file name or relative path (e.g., "app/cobol/PROG.cbl").
            template: The documentation template to save.
        """
        doc_path = self._get_doc_output_path(file_name)
        backup_path = Path(str(doc_path) + ".bak")

        try:
            # Create parent directories if they don't exist (for mirrored structure)
            doc_path.parent.mkdir(parents=True, exist_ok=True)

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

            # Also save human-readable markdown alongside the .doc.json
            # Note: .bak files are created separately and do not get .md files
            try:
                md_content = self._generate_markdown_from_template(template, file_name)
                # Change .doc.json to .md (remove both suffixes, add .md)
                md_path = doc_path.with_suffix("").with_suffix(".md")
                md_path.write_text(md_content, encoding="utf-8")
                logger.debug(f"Worker {self.worker_id}: Saved markdown to {md_path}")
            except Exception as md_error:
                # Don't fail the main save if markdown generation fails
                logger.warning(
                    f"Worker {self.worker_id}: Failed to save markdown for "
                    f"{doc_path}: {md_error}"
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

    def _validate_critical_sections(
        self,
        template: DocumentationTemplate,
        feedback_context: dict | None,
        detected_file_type: FileType | None = None,
    ) -> tuple[bool, list[str]]:
        """Validate that critical sections are not empty (IMPFB-005).

        Checks that sections marked as critical in the feedback context
        have actual content, not just placeholders or empty values.

        Sections are skipped based on file type where they don't apply:
        - COPYBOOK: skip called_programs, data_flow (include files don't execute)
        - JCL/PROC: skip called_programs (different execution paradigm)
        - BMS: skip most sections (screen map definitions)

        Args:
            template: The documentation template to validate.
            feedback_context: FeedbackContext dict with critical_sections list.
            detected_file_type: File type detected from extension (takes precedence
                over template.header.file_type which may not be set by LLM).

        Returns:
            Tuple of (is_valid, list_of_empty_sections).
            is_valid is True if all critical sections have content.
        """
        if not feedback_context:
            return True, []

        critical_sections = feedback_context.get("critical_sections", [])
        if not critical_sections:
            return True, []

        # Use detected file type (from extension) with fallback to template header
        # The detected_file_type is more reliable since it's determined from the
        # file extension, while template.header.file_type depends on LLM output
        file_type = detected_file_type
        if file_type is None and template.header and template.header.file_type:
            file_type = template.header.file_type

        # Sections to skip based on file type
        # These sections are legitimately empty for certain file types
        skip_sections_by_type: dict[str, set[str]] = {
            # COBOL: skip nothing - full programs with all sections
            # PLI: skip nothing - full programs like COBOL

            "COPYBOOK": {
                "called_programs",    # Don't execute, just included
                "data_flow",          # No execution flow
                "cics_operations",    # No CICS commands
                "sql_operations",     # No SQL execution
                "business_rules",     # Define data, not logic
                "error_handling",     # No error handling
                "inputs",             # Data definitions, not I/O
                "outputs",            # Data definitions, not I/O
            },

            "JCL": {
                "called_programs",    # EXECs programs, doesn't CALL
                "copybooks",          # No COBOL copybooks
                "cics_operations",    # Batch, not CICS
                "sql_operations",     # No embedded SQL
                "data_flow",          # Job steps, not data flow
                "business_rules",     # Job control, not business logic
                "error_handling",     # COND codes not traditional error handling
                # Keep: purpose, inputs, outputs
            },

            "PROC": {
                "called_programs",    # Same as JCL
                "copybooks",
                "cics_operations",
                "sql_operations",
                "data_flow",
                "business_rules",
                "error_handling",     # Same as JCL
            },

            "BMS": {
                "called_programs",    # Screen definitions only
                "data_flow",          # No execution
                "copybooks",          # Generates copybooks, doesn't use
                "sql_operations",     # No SQL
                "cics_operations",    # Used BY CICS, doesn't contain commands
                "business_rules",     # Just layout
                "error_handling",     # No error handling
                # Keep: purpose, inputs (fields), outputs (fields)
            },

            "LISTING": {
                # Compiler output - skip almost everything
                "called_programs",
                "data_flow",
                "copybooks",
                "sql_operations",
                "cics_operations",
                "business_rules",
                "error_handling",
                "inputs",
                "outputs",
                # Keep: purpose only
            },

            "OTHER": {
                # Unknown file type - be lenient
                "called_programs",
                "data_flow",
                "copybooks",
                "sql_operations",
                "cics_operations",
                "business_rules",
                "error_handling",
            },

            # New file types - be comprehensive to avoid validation loops
            "ASM": {
                # Assembler - low level, different paradigm
                "copybooks",          # Uses macros, not COBOL copybooks
                "cics_operations",    # Rarely has CICS macros
                "sql_operations",     # Rarely has embedded SQL
                "business_rules",     # Low-level, rules not explicit
                "error_handling",     # Uses return codes/ABEND, not structured
            },

            "REXX": {
                # Scripting language - simpler structure
                "copybooks",          # No COBOL copybooks
                "cics_operations",    # Usually batch/TSO
                "sql_operations",     # Rarely has EXECSQL
                "business_rules",     # Scripts, not business logic
                "error_handling",     # Uses SIGNAL, not structured handling
            },

            "CLIST": {
                # TSO command list scripting
                "copybooks",          # TSO scripting, no copybooks
                "cics_operations",    # Not online
                "sql_operations",     # No SQL
                "data_flow",          # Simple commands
                "business_rules",     # Utility scripts
                "error_handling",     # Simple error checking
                "called_programs",    # Invokes commands, not CALL
            },

            "NATURAL": {
                # Software AG 4GL - different paradigm from COBOL
                "copybooks",          # Has own LDA/PDA mechanism
                "cics_operations",    # Uses Natural screens, not CICS
                "sql_operations",     # Uses ADABAS, not SQL
                "error_handling",     # Uses ON ERROR, different pattern
            },

            "EASYTRIEVE": {
                # Report generator - simple utility
                "copybooks",          # Report definitions
                "cics_operations",    # Batch only
                "called_programs",    # Standalone utility
                "business_rules",     # Report formatting, not logic
                "error_handling",     # Minimal error handling
                "data_flow",          # Sequential file processing
            },

            "SORT": {
                # DFSORT control cards - very specific
                "called_programs",
                "copybooks",
                "cics_operations",
                "sql_operations",
                "business_rules",
                "error_handling",
                "data_flow",
                # Keep: purpose, inputs, outputs
            },

            "DDL": {
                # Database definitions
                "called_programs",
                "copybooks",
                "cics_operations",
                "data_flow",
                "business_rules",
                "error_handling",
                # Keep: purpose, inputs (tables), outputs (tables), sql_operations
            },

            "IMS": {
                # IMS database/program definitions
                "called_programs",
                "copybooks",
                "cics_operations",
                "sql_operations",
                "data_flow",
                "business_rules",
                "error_handling",
                # Keep: purpose, inputs, outputs
            },
        }

        # Get the string value of the file type enum
        file_type_str = file_type.value if hasattr(file_type, 'value') else str(file_type)
        skip_sections = skip_sections_by_type.get(file_type_str, set())

        empty_sections: list[str] = []

        # Map section names to template attributes
        section_checks = {
            "purpose": lambda t: t.purpose and t.purpose.summary and len(t.purpose.summary.strip()) > 10,
            "inputs": lambda t: t.inputs and len(t.inputs) > 0,
            "outputs": lambda t: t.outputs and len(t.outputs) > 0,
            "data_flow": lambda t: t.data_flow and (t.data_flow.internal_flow or t.data_flow.inputs or t.data_flow.outputs),
            "copybooks": lambda t: t.copybooks and len(t.copybooks) > 0,
            "sql_operations": lambda t: t.sql_operations and len(t.sql_operations) > 0,
            "cics_operations": lambda t: t.cics_operations and len(t.cics_operations) > 0,
            "business_rules": lambda t: t.business_rules and len(t.business_rules) > 0,
            "error_handling": lambda t: t.error_handling and (t.error_handling.error_codes or t.error_handling.error_routines),
            "called_programs": lambda t: t.called_programs and len(t.called_programs) > 0,
            "summary": lambda t: t.purpose and t.purpose.summary and len(t.purpose.summary.strip()) > 10,
        }

        for section in critical_sections:
            section_key = section.lower().replace(" ", "_")

            # Skip sections that don't apply to this file type
            if section_key in skip_sections:
                logger.debug(
                    f"Worker {self.worker_id}: Skipping {section} validation for {file_type} file"
                )
                continue

            check_func = section_checks.get(section_key)

            if check_func:
                try:
                    if not check_func(template):
                        empty_sections.append(section)
                except Exception as e:
                    logger.debug(f"Error checking section {section}: {e}")
                    empty_sections.append(section)

        is_valid = len(empty_sections) == 0
        if not is_valid:
            logger.warning(
                f"Worker {self.worker_id}: Critical sections empty: {', '.join(empty_sections)}"
            )

        return is_valid, empty_sections

    def _prepare_source_for_processing(
        self,
        source_code: str,
        ticket: ProgramManagerTicket,
        previous_template: DocumentationTemplate | None,
        challenger_questions: list[ChallengerQuestion] | None = None,
        chrome_tickets: list[ChromeTicket] | None = None,
    ) -> PreparedSource:
        """Prepare source code with centralized token limit handling.

        This method provides a single entry point for all source code preparation,
        delegating to SourceCodePreparer which decides whether to:
        - Pass through (if within token limit)
        - Sample (for updates/clarifications/chrome) - uses intelligent sampling
          when questions/tickets are provided
        - Chunk (for initial documentation of large files)

        Args:
            source_code: The raw source code to prepare.
            ticket: The ticket being processed (provides context).
            previous_template: Previous documentation template, if any.
            challenger_questions: Optional Challenger questions for intelligent
                sampling (CLARIFICATION tickets).
            chrome_tickets: Optional Chrome tickets for intelligent sampling
                (CHROME tickets).

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
        return self._source_preparer.prepare(
            source_code,
            context,
            challenger_questions=challenger_questions,
            chrome_tickets=chrome_tickets,
            previous_template=previous_template,
        )

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
                # Check if this is a discovery ticket (from call graph gap)
                if ticket.metadata.get("discovery"):
                    logger.info(
                        f"Worker {self.worker_id}: Discovery ticket for {ticket.program_id}, "
                        f"searching for file..."
                    )
                    return await self._handle_discovery_ticket(ticket, formatting_strict)

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

        # Load previous template - either for update iteration or augmentation (IMPFB-007)
        previous_template = None
        feedback_context = ticket.metadata.get("feedback_context")
        augment_existing = (
            feedback_context.get("augment_existing", True)
            if feedback_context else False
        )

        # Always check for existing documentation to augment
        if ticket.cycle_number > 1 or augment_existing:
            previous_template = self._load_previous_template(ticket.file_name)
            if previous_template:
                if ticket.cycle_number == 1 and augment_existing:
                    logger.info(
                        f"Worker {self.worker_id}: Augmenting existing documentation for "
                        f"{ticket.file_name} (per feedback context)"
                    )
                else:
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

        # feedback_context already extracted above for augmentation check (IMPFB-004/007)

        # Build Scribe input with prepared source
        scribe_input = ScribeInput(
            source_code=prepared.source_code,
            file_name=ticket.file_name,
            file_type=file_type,
            iteration=ticket.cycle_number,
            copybook_contents=ticket.metadata.get("copybook_contents", {}),
            previous_template=previous_template,
            formatting_strict=formatting_strict,
            feedback_context=feedback_context,
        )

        # Run the Scribe agent
        logger.debug(
            f"Worker {self.worker_id}: Running Scribe for {ticket.file_name}"
        )
        output = await self.scribe_agent.ainvoke(scribe_input)

        # Validate critical sections if feedback context provided (IMPFB-005)
        # Skip validation if source was sampled/truncated - the LLM may not have
        # seen all the code, so empty sections could be legitimate (the code for
        # those sections wasn't in the sample). This prevents false failures.
        if output.success and output.template and feedback_context:
            if prepared.was_modified and prepared.strategy_used == "sampling":
                logger.info(
                    f"Worker {self.worker_id}: Skipping critical section validation for "
                    f"{ticket.file_name} - source was sampled/truncated, LLM may "
                    f"not have seen all code sections"
                )
            else:
                is_valid, empty_sections = self._validate_critical_sections(
                    output.template, feedback_context, detected_file_type=file_type
                )
                if not is_valid:
                    logger.warning(
                        f"Worker {self.worker_id}: Critical sections validation failed for "
                        f"{ticket.file_name}: empty sections = {empty_sections}"
                    )
                    # Return failure so ticket can be retried or escalated
                    return ScribeOutput(
                        success=False,
                        error=f"Critical sections are empty: {', '.join(empty_sections)}",
                        template=output.template,  # Include template for debugging
                    )

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

        # Extract feedback context from ticket metadata (IMPFB-004)
        feedback_context = ticket.metadata.get("feedback_context")

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
                feedback_context=feedback_context,
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

    # =========================================================================
    # Discovery Ticket Handling
    # =========================================================================

    async def _handle_discovery_ticket(
        self,
        ticket: ProgramManagerTicket,
        formatting_strict: bool = False,
    ) -> ScribeOutput:
        """Handle a discovery ticket for a program found via call graph analysis.

        Discovery tickets are created when the call graph identifies a program
        that is called but not documented. This method searches for the program:
        1. First by file name with various extensions
        2. If not found, by searching for the symbol in all source files
        3. If found in another file, documents it as an internal routine
        4. If not found anywhere, documents it as external/missing

        Args:
            ticket: The discovery ticket to process.
            formatting_strict: If True, add extra JSON formatting instructions.

        Returns:
            ScribeOutput with documentation results.
        """
        program_id = ticket.program_id or ticket.file_name.split(".")[0]
        logger.info(
            f"Worker {self.worker_id}: Starting discovery for program {program_id}"
        )

        # Step 1: Search for file by name with various extensions
        found_file = self._search_file_by_name(program_id)
        if found_file:
            logger.info(
                f"Worker {self.worker_id}: Found file {found_file} for {program_id}"
            )
            # Update ticket with found file path and process normally
            ticket.metadata["file_path"] = str(found_file)
            ticket.metadata["discovery_resolved"] = "file_found"
            # Read the file and continue with normal documentation
            try:
                source_code = found_file.read_text(encoding="utf-8", errors="replace")
                # Continue with normal documentation processing
                return await self._process_documentation_with_source(
                    ticket, source_code, formatting_strict
                )
            except Exception as e:
                logger.error(f"Worker {self.worker_id}: Failed to read {found_file}: {e}")
                return ScribeOutput(success=False, error=f"Failed to read found file: {e}")

        # Step 2: Search for symbol in all source files
        symbol_matches = self._search_symbol_in_sources(program_id)
        if symbol_matches:
            logger.info(
                f"Worker {self.worker_id}: Found {program_id} as symbol in "
                f"{len(symbol_matches)} location(s)"
            )
            # Document as internal routine in parent file(s)
            return await self._document_as_internal_routine(
                ticket, program_id, symbol_matches
            )

        # Step 3: Not found anywhere - document as external/missing
        logger.info(
            f"Worker {self.worker_id}: {program_id} not found, documenting as external"
        )
        return await self._document_as_external(ticket, program_id)

    def _search_file_by_name(self, program_id: str) -> Path | None:
        """Search for a file matching the program ID.

        Searches the input directory (non-recursive) for files matching
        the program ID with various extensions.

        Args:
            program_id: The program identifier to search for.

        Returns:
            Path to the found file, or None if not found.
        """
        search_dir = self.input_directory
        program_upper = program_id.upper()
        program_lower = program_id.lower()

        for ext in self.DISCOVERY_EXTENSIONS:
            # Try exact case
            candidate = search_dir / f"{program_id}{ext}"
            if candidate.exists():
                return candidate
            # Try uppercase
            candidate = search_dir / f"{program_upper}{ext}"
            if candidate.exists():
                return candidate
            # Try lowercase
            candidate = search_dir / f"{program_lower}{ext}"
            if candidate.exists():
                return candidate
            # Try with uppercase extension
            candidate = search_dir / f"{program_id}{ext.upper()}"
            if candidate.exists():
                return candidate

        # Also check subdirectories (one level deep for organized projects)
        for subdir in search_dir.iterdir():
            if subdir.is_dir() and not subdir.name.startswith("."):
                for ext in self.DISCOVERY_EXTENSIONS:
                    candidate = subdir / f"{program_id}{ext}"
                    if candidate.exists():
                        return candidate
                    candidate = subdir / f"{program_upper}{ext}"
                    if candidate.exists():
                        return candidate
                    candidate = subdir / f"{program_lower}{ext}"
                    if candidate.exists():
                        return candidate

        return None

    def _search_symbol_in_sources(
        self, program_id: str
    ) -> list[tuple[Path, int, str, str]]:
        """Search for a symbol DEFINITION in all source files.

        Searches all source files in the input directory for definitions
        of the program ID (SECTION, paragraph, PROGRAM-ID, PROC, etc.).

        Note: This finds where the symbol is DEFINED, not where it's called.
        A CALL to XYZ does not qualify - we need the actual definition.

        Args:
            program_id: The program/symbol identifier to search for.

        Returns:
            List of (file_path, line_number, matched_line, match_type) tuples.
        """
        matches: list[tuple[Path, int, str, str]] = []
        search_dir = self.input_directory

        # Compile definition patterns for this symbol
        compiled_patterns: list[tuple[re.Pattern, str]] = []
        for pattern_template in self.SYMBOL_DEFINITION_PATTERNS:
            pattern_str = pattern_template.format(symbol=re.escape(program_id))
            try:
                compiled_patterns.append(
                    (re.compile(pattern_str, re.IGNORECASE | re.MULTILINE), pattern_template)
                )
            except re.error:
                continue

        # Search all source files
        for source_file in self._get_all_source_files(search_dir):
            try:
                content = source_file.read_text(encoding="utf-8", errors="replace")
                lines = content.split("\n")

                for line_num, line in enumerate(lines, start=1):
                    for pattern, pattern_type in compiled_patterns:
                        if pattern.search(line):
                            # Determine definition type from pattern
                            if "SECTION" in pattern_type:
                                match_type = "SECTION"
                            elif "PROGRAM-ID" in pattern_type:
                                match_type = "PROGRAM"
                            elif "PROC" in pattern_type:
                                match_type = "JCL_PROC"
                            elif "CSECT" in pattern_type:
                                match_type = "ASM_CSECT"
                            elif "START" in pattern_type or "ENTRY" in pattern_type:
                                match_type = "ASM_ENTRY"
                            elif r"\.\s*$" in pattern_type:
                                match_type = "PARAGRAPH"
                            else:
                                match_type = "DEFINITION"

                            matches.append((source_file, line_num, line.strip(), match_type))
                            break  # Only count first match per line

            except Exception as e:
                logger.debug(f"Worker {self.worker_id}: Could not read {source_file}: {e}")
                continue

        return matches

    def _get_all_source_files(self, directory: Path) -> list[Path]:
        """Get all source files in directory (including subdirectories).

        Args:
            directory: Directory to search.

        Returns:
            List of Path objects for all source files.
        """
        source_files: list[Path] = []

        for ext in self.DISCOVERY_EXTENSIONS:
            # Direct files
            source_files.extend(directory.glob(f"*{ext}"))
            source_files.extend(directory.glob(f"*{ext.upper()}"))
            # Subdirectory files (one level)
            source_files.extend(directory.glob(f"*/*{ext}"))
            source_files.extend(directory.glob(f"*/*{ext.upper()}"))

        return sorted(set(source_files))

    async def _document_as_internal_routine(
        self,
        ticket: ProgramManagerTicket,
        program_id: str,
        matches: list[tuple[Path, int, str, str]],
    ) -> ScribeOutput:
        """Document a program as an internal routine found in parent file(s).

        Updates the parent file's documentation to include this routine
        in the internal_routines section.

        Args:
            ticket: The discovery ticket being processed.
            program_id: The program/routine identifier.
            matches: List of (file_path, line_number, line, match_type) where found.

        Returns:
            ScribeOutput indicating success.
        """
        # Group matches by parent file
        files_updated: list[str] = []

        for file_path, line_num, line_content, match_type in matches:
            # Load existing documentation for the parent file
            rel_path = file_path.relative_to(self.input_directory)
            doc_path = self.output_directory / rel_path.parent / f"{rel_path.name}.doc.json"

            if doc_path.exists():
                try:
                    with open(doc_path) as f:
                        doc_data = json.load(f)

                    # Add to internal_routines if not already present
                    if "internal_routines" not in doc_data:
                        doc_data["internal_routines"] = []

                    # Check if already documented
                    existing = [r for r in doc_data["internal_routines"]
                               if r.get("name", "").upper() == program_id.upper()]
                    if not existing:
                        doc_data["internal_routines"].append({
                            "name": program_id,
                            "type": match_type,
                            "line_number": line_num,
                            "context": line_content[:100],
                            "discovered_from": ticket.metadata.get("source", "call_graph"),
                        })

                        # Save updated documentation
                        with open(doc_path, "w") as f:
                            json.dump(doc_data, f, indent=2, default=str)

                        files_updated.append(str(rel_path))
                        logger.info(
                            f"Worker {self.worker_id}: Added {program_id} to "
                            f"internal_routines in {rel_path}"
                        )

                except Exception as e:
                    logger.warning(
                        f"Worker {self.worker_id}: Failed to update {doc_path}: {e}"
                    )

        # Update ticket metadata
        ticket.metadata["discovery_resolved"] = "internal_routine"
        ticket.metadata["found_in_files"] = files_updated
        ticket.metadata["match_count"] = len(matches)

        return ScribeOutput(
            success=True,
            template=None,  # No separate template for internal routines
            discovery_result={
                "status": "internal_routine",
                "program_id": program_id,
                "found_in": files_updated,
                "match_count": len(matches),
            },
        )

    async def _document_as_external(
        self,
        ticket: ProgramManagerTicket,
        program_id: str,
    ) -> ScribeOutput:
        """Document a program as external/missing.

        Creates documentation noting that the program could not be found
        and providing guidance for resolution.

        Args:
            ticket: The discovery ticket being processed.
            program_id: The program identifier that was not found.

        Returns:
            ScribeOutput with external program documentation.
        """
        # Determine likely explanation based on name patterns
        likely_explanation = self._infer_program_type(program_id)

        # Get caller information from metadata
        called_from = ticket.metadata.get("called_from", [])
        if not called_from and ticket.parent_ticket_id:
            called_from = [ticket.parent_ticket_id]

        # Build search summary
        search_performed = [
            f"File search: {self.input_directory}/*{ext}"
            for ext in self.DISCOVERY_EXTENSIONS[:3]  # Sample
        ] + [
            "Symbol search: CALL, PERFORM, EXEC patterns in all source files"
        ]

        # Determine recommended action
        if likely_explanation == "system_utility":
            recommended_action = "Add to system utilities list in call_graph.py"
        elif likely_explanation == "external_library":
            recommended_action = "Verify with SME - may be from external library"
        else:
            recommended_action = "Locate source code or confirm as external dependency"

        # Create external documentation
        external_doc = {
            "header": {
                "program_id": program_id,
                "file_name": f"{program_id}.EXTERNAL",
                "program_type": "EXTERNAL",
            },
            "is_external": True,
            "status": "missing",
            "called_from": called_from,
            "search_performed": search_performed,
            "likely_explanation": likely_explanation,
            "recommended_action": recommended_action,
            "discovered_at": datetime.utcnow().isoformat(),
        }

        # Save external documentation
        doc_path = self.output_directory / f"{program_id}.external.json"
        try:
            with open(doc_path, "w") as f:
                json.dump(external_doc, f, indent=2)
            logger.info(
                f"Worker {self.worker_id}: Created external documentation for "
                f"{program_id} at {doc_path}"
            )
        except Exception as e:
            logger.error(f"Worker {self.worker_id}: Failed to save external doc: {e}")

        # Update ticket metadata
        ticket.metadata["discovery_resolved"] = "external"
        ticket.metadata["likely_explanation"] = likely_explanation

        return ScribeOutput(
            success=True,
            template=None,
            discovery_result={
                "status": "external",
                "program_id": program_id,
                "likely_explanation": likely_explanation,
                "recommended_action": recommended_action,
            },
        )

    def _infer_program_type(self, program_id: str) -> str:
        """Infer the likely type of a missing program based on naming patterns.

        Args:
            program_id: The program identifier.

        Returns:
            One of: "system_utility", "external_library", "missing_source"
        """
        upper_id = program_id.upper()

        # IBM system utility patterns
        system_prefixes = [
            "IEF", "IEB", "IEH", "IDC", "AMS",  # z/OS utilities
            "DFH",  # CICS
            "DSN",  # DB2
            "CSQ",  # MQ
            "IGY", "IGZ",  # COBOL runtime
            "CEE",  # Language Environment
            "DFS",  # IMS
            "HEWL", "IEWL",  # Linkage editor
            "IGYCRCTL", "IKJEFT",  # Common utilities
        ]

        for prefix in system_prefixes:
            if upper_id.startswith(prefix):
                return "system_utility"

        # Common external library patterns
        if upper_id.startswith(("LIB", "UTL", "CMN", "COM")):
            return "external_library"

        return "missing_source"

    async def _process_documentation_with_source(
        self,
        ticket: ProgramManagerTicket,
        source_code: str,
        formatting_strict: bool = False,
    ) -> ScribeOutput:
        """Process documentation ticket with provided source code.

        This is a helper that continues the normal documentation flow
        after source code has been loaded (used by discovery flow).

        Args:
            ticket: The ticket being processed.
            source_code: The loaded source code.
            formatting_strict: If True, add extra JSON formatting instructions.

        Returns:
            ScribeOutput with documentation results.
        """
        # Determine the source file path for Citadel analysis
        source_path = self.input_directory / ticket.file_name
        if ticket.metadata.get("file_path"):
            source_path = Path(ticket.metadata["file_path"])

        # Get Citadel context early for potential use in enrichment
        citadel_context = self._get_citadel_context(str(source_path))

        # Load previous template if this is an update iteration
        previous_template = self._load_previous_template(ticket)

        # Determine file type from filename
        file_type = self._determine_file_type(ticket.file_name)

        # Run preprocessor if applicable
        preprocessor_result = self._run_preprocessor(source_code, file_type)

        # Prepare source code with centralized handling
        prepared = self._prepare_source_for_processing(
            source_code, ticket, previous_template
        )

        # Build input for ScribeAgent
        scribe_input = ScribeInput(
            source_code=prepared.content,
            file_name=ticket.file_name,
            file_type=file_type,
            existing_template=previous_template,
            iteration=ticket.cycle_number,
            context={
                "batch_id": ticket.metadata.get("batch_id", "unknown"),
                "ticket_id": ticket.ticket_id,
                "discovery": ticket.metadata.get("discovery", False),
            },
            preprocessor_result=preprocessor_result,
        )

        # Run the Scribe agent
        try:
            result = await self.scribe_agent.run(
                scribe_input,
                use_mock=self.config.use_mock,
                formatting_strict=formatting_strict,
            )
        except Exception as e:
            logger.error(
                f"Worker {self.worker_id}: ScribeAgent failed for {ticket.file_name}: {e}"
            )
            return ScribeOutput(
                success=False,
                error=str(e),
            )

        # Enrich with Citadel call references if available
        if result.template and citadel_context:
            result.template = await self._enrich_paragraphs_with_citadel(
                result.template,
                str(source_path),
                citadel_context,
            )

        # Save the template to disk
        if result.template:
            self._save_template(ticket.file_name, result.template)

        return result

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
                previous_template = DocumentationTemplate.load_lenient(template_data)
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
            # Bug fix war_rig-vllu: Handle missing template gracefully
            # This can happen if the Imperator created a ticket for a callee
            # (external program) that doesn't exist in our codebase. Instead of
            # failing with an error, we return success=True to close the ticket
            # since there's no documentation to clarify.
            logger.warning(
                f"Worker {self.worker_id}: No previous template found for "
                f"clarification ticket {ticket.ticket_id} ({ticket.file_name}). "
                f"This may be a ticket for a non-existent file - closing gracefully."
            )
            return ScribeOutput(
                success=True,  # Mark as success to close the ticket
                template=None,
                open_questions=[
                    f"Clarification ticket for {ticket.file_name} could not be processed: "
                    f"no existing documentation found. The file may not exist in the codebase."
                ],
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
                # Bug fix war_rig-vllu: Handle missing source file gracefully
                logger.warning(
                    f"Worker {self.worker_id}: Source file not found for "
                    f"clarification ticket: {source_path}. Closing gracefully."
                )
                return ScribeOutput(
                    success=True,  # Mark as success to close the ticket
                    template=previous_template,  # Preserve existing template
                    open_questions=[
                        f"Source file {ticket.file_name} not found - clarification "
                        f"could not be processed."
                    ],
                )
            try:
                source_code = source_path.read_text(encoding="utf-8", errors="replace")
            except Exception as e:
                return ScribeOutput(
                    success=False,
                    error=f"Failed to read source file: {e}",
                )

        # Extract challenger questions from ticket metadata BEFORE source preparation
        # so they can be used for intelligent sampling
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

        # Prepare source code using centralized token limit handling
        # Pass challenger questions for intelligent sampling
        prepared = self._prepare_source_for_processing(
            source_code,
            ticket,
            previous_template,
            challenger_questions=challenger_questions,
        )
        if prepared.was_modified:
            logger.info(
                f"Worker {self.worker_id}: Source {prepared.strategy_used} for "
                f"clarification ticket ({ticket.file_name})"
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

        # Extract feedback context from ticket metadata (IMPFB-004)
        feedback_context = ticket.metadata.get("feedback_context")

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
            feedback_context=feedback_context,
        )

        logger.info(
            f"Worker {self.worker_id}: Processing clarification for {ticket.file_name} "
            f"with {len(challenger_questions)} questions"
            + (f" (with feedback context)" if feedback_context else "")
        )
        output = await self.scribe_agent.ainvoke(scribe_input)

        # Validate critical sections if feedback context provided (IMPFB-005)
        # Skip validation if source was sampled/truncated - the LLM may not have
        # seen all the code, so empty sections could be legitimate (the code for
        # those sections wasn't in the sample). This prevents false failures.
        if output.success and output.template and feedback_context:
            if prepared.was_modified and prepared.strategy_used == "sampling":
                logger.info(
                    f"Worker {self.worker_id}: Skipping critical section validation for "
                    f"{ticket.file_name} (clarification) - source was sampled/truncated, LLM may "
                    f"not have seen all code sections"
                )
            else:
                is_valid, empty_sections = self._validate_critical_sections(
                    output.template, feedback_context, detected_file_type=file_type
                )
                if not is_valid:
                    logger.warning(
                        f"Worker {self.worker_id}: Critical sections validation failed for "
                        f"{ticket.file_name} (clarification): empty sections = {empty_sections}"
                    )
                    return ScribeOutput(
                        success=False,
                        error=f"Critical sections are empty: {', '.join(empty_sections)}",
                        template=output.template,
                    )

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
                previous_template = DocumentationTemplate.load_lenient(template_data)
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
            # Bug fix war_rig-vllu: Handle missing template gracefully
            # This can happen if the Imperator created a chrome ticket for a callee
            # (external program) that doesn't exist in our codebase. Instead of
            # failing with an error, we return success=True to close the ticket
            # since there's no documentation to update.
            logger.warning(
                f"Worker {self.worker_id}: No previous template found for "
                f"chrome ticket {ticket.ticket_id} ({ticket.file_name}). "
                f"This may be a ticket for a non-existent file - closing gracefully."
            )
            return ScribeOutput(
                success=True,  # Mark as success to close the ticket
                template=None,
                open_questions=[
                    f"Chrome ticket for {ticket.file_name} could not be processed: "
                    f"no existing documentation found. The file may not exist in the codebase."
                ],
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
                # Bug fix war_rig-vllu: Handle missing source file gracefully
                logger.warning(
                    f"Worker {self.worker_id}: Source file not found for "
                    f"chrome ticket: {source_path}. Closing gracefully."
                )
                return ScribeOutput(
                    success=True,  # Mark as success to close the ticket
                    template=previous_template,  # Preserve existing template
                    open_questions=[
                        f"Source file {ticket.file_name} not found - chrome issue "
                        f"could not be addressed."
                    ],
                )
            try:
                source_code = source_path.read_text(encoding="utf-8", errors="replace")
            except Exception as e:
                return ScribeOutput(
                    success=False,
                    error=f"Failed to read source file: {e}",
                )

        # Extract chrome tickets from ticket metadata BEFORE source preparation
        # so they can be used for intelligent sampling
        chrome_tickets_list: list[ChromeTicket] = []
        chrome_data = ticket.metadata.get("chrome_tickets", [])
        for ct in chrome_data:
            if isinstance(ct, dict):
                chrome_tickets_list.append(ChromeTicket.model_validate(ct))
            elif isinstance(ct, ChromeTicket):
                chrome_tickets_list.append(ct)

        # Also check for single issue in issue_description
        if ticket.metadata.get("issue_description"):
            from war_rig.models.tickets import IssuePriority
            chrome_tickets_list.append(
                ChromeTicket(
                    ticket_id=f"CHR-{ticket.ticket_id[-8:]}",
                    description=ticket.metadata["issue_description"],
                    section=ticket.metadata.get("section"),
                    priority=IssuePriority.MEDIUM,
                    guidance=ticket.metadata.get("guidance"),
                )
            )

        # Prepare source code using centralized token limit handling
        # Pass chrome tickets for intelligent sampling
        prepared = self._prepare_source_for_processing(
            source_code,
            ticket,
            previous_template,
            chrome_tickets=chrome_tickets_list,
        )
        if prepared.was_modified:
            logger.info(
                f"Worker {self.worker_id}: Source {prepared.strategy_used} for "
                f"chrome ticket ({ticket.file_name})"
            )

        if not chrome_tickets_list:
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

        # Extract feedback context from ticket metadata (IMPFB-004)
        feedback_context = ticket.metadata.get("feedback_context")

        # Build Scribe input with prepared source and previous template
        scribe_input = ScribeInput(
            source_code=prepared.source_code,
            file_name=ticket.file_name,
            file_type=file_type,
            iteration=ticket.cycle_number,
            copybook_contents=ticket.metadata.get("copybook_contents", {}),
            previous_template=previous_template,
            chrome_tickets=chrome_tickets_list,
            formatting_strict=formatting_strict,
            feedback_context=feedback_context,
        )

        logger.info(
            f"Worker {self.worker_id}: Processing chrome for {ticket.file_name} "
            f"with {len(chrome_tickets_list)} issues"
            + (f" (with feedback context)" if feedback_context else "")
        )
        output = await self.scribe_agent.ainvoke(scribe_input)

        # Validate critical sections if feedback context provided (IMPFB-005)
        # Skip validation if source was sampled/truncated - the LLM may not have
        # seen all the code, so empty sections could be legitimate (the code for
        # those sections wasn't in the sample). This prevents false failures.
        if output.success and output.template and feedback_context:
            if prepared.was_modified and prepared.strategy_used == "sampling":
                logger.info(
                    f"Worker {self.worker_id}: Skipping critical section validation for "
                    f"{ticket.file_name} (chrome) - source was sampled/truncated, LLM may "
                    f"not have seen all code sections"
                )
            else:
                is_valid, empty_sections = self._validate_critical_sections(
                    output.template, feedback_context, detected_file_type=file_type
                )
                if not is_valid:
                    logger.warning(
                        f"Worker {self.worker_id}: Critical sections validation failed for "
                        f"{ticket.file_name} (chrome): empty sections = {empty_sections}"
                    )
                    return ScribeOutput(
                        success=False,
                        error=f"Critical sections are empty: {', '.join(empty_sections)}",
                        template=output.template,
                    )

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
        # Build metadata - only references, not full content to avoid bloat
        # Template is saved to disk (.doc.json) and loaded by Challenger from there
        validation_metadata: dict[str, Any] = {
            "parent_documentation_ticket": doc_ticket.ticket_id,
            "scribe_worker": self.worker_id,
        }

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
        file_lock_manager: FileLockManager | None = None,
        exit_on_error: bool | None = None,
        dependency_graph_path: Path | None = None,
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
            file_lock_manager: Optional centralized lock manager for file locking.
                When provided, workers will acquire locks on output files before
                processing and skip files that are locked by other workers.
                This prevents race conditions when multiple workers attempt to
                process tickets for the same output file.
            exit_on_error: If True, workers will raise FatalWorkerError on errors.
                Defaults to config.exit_on_error.
            dependency_graph_path: Optional path to Citadel dependency graph JSON.
                When provided, workers can use the graph to understand code
                relationships and improve documentation quality.
        """
        self.config = config
        self.beads_client = beads_client
        self.input_directory = input_directory or config.input_directory
        self.output_directory = output_directory or config.output_directory
        self.num_workers = num_workers or config.num_scribes
        self.poll_interval = poll_interval
        self.idle_timeout = idle_timeout
        self.file_lock_manager = file_lock_manager
        self.exit_on_error = exit_on_error if exit_on_error is not None else config.exit_on_error
        self.dependency_graph_path = dependency_graph_path

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
                file_lock_manager=self.file_lock_manager,
                exit_on_error=self.exit_on_error,
                dependency_graph_path=self.dependency_graph_path,
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
