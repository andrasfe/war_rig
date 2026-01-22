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
import random
from collections.abc import Callable
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import Any

from war_rig.agents.challenger import ChallengerAgent, ChallengerInput, ChallengerOutput
from war_rig.chunking import TokenEstimator
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
        # Resolve to absolute path for consistent file access
        self.output_directory = config.output_directory.resolve()

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

    def _get_doc_path(self, file_name: str) -> Path:
        """Get the documentation file path for a given file name.

        Handles both flat paths (legacy) and relative paths (new structure):
        - "PROG.cbl" -> output/PROG.doc.json
        - "app/cobol/PROG.cbl" -> output/app/cobol/PROG.doc.json

        Args:
            file_name: Source file name or relative path.

        Returns:
            Path to the documentation JSON file.
        """
        rel_path = Path(file_name)

        # If the path has a parent directory, mirror the structure
        if rel_path.parent != Path("."):
            return self.output_directory / rel_path.parent / f"{rel_path.stem}.doc.json"

        # Flat path (legacy or root-level files)
        return self.output_directory / f"{rel_path.stem}.doc.json"

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
        # Only log and set state if not already stopped
        if self.status.state == WorkerState.STOPPED:
            return
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
                # Reset any in-progress ticket so it can be retried
                if self.status.current_ticket_id:
                    logger.warning(
                        f"Worker {self.worker_id}: Resetting ticket {self.status.current_ticket_id} "
                        f"to CREATED due to cancellation"
                    )
                    try:
                        self.beads_client.update_ticket_state(
                            self.status.current_ticket_id,
                            TicketState.CREATED,
                            reason="Worker cancelled mid-processing, reset for retry",
                        )
                    except Exception as e:
                        logger.error(f"Failed to reset ticket on cancellation: {e}")
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

        # Check if this is a discovery ticket validation
        if ticket.metadata and ticket.metadata.get("discovery"):
            await self._process_discovery_validation(ticket)
            return

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
            # Issues found - check if we've exceeded max iterations
            validation_count = self._get_validation_count(ticket.file_name)
            max_iterations = self.config.max_iterations

            if validation_count >= max_iterations:
                # Force approval - max iterations reached
                logger.warning(
                    f"Worker {self.worker_id}: Max iterations ({max_iterations}) reached for "
                    f"{ticket.file_name} with {len(result.blocking_questions)} issues remaining. "
                    f"Force approving documentation."
                )
                self.beads_client.update_ticket_state(
                    ticket.ticket_id,
                    TicketState.COMPLETED,
                    reason=f"Force approved at max iterations ({validation_count}), "
                    f"{len(result.blocking_questions)} issues remaining",
                )
                self.status.tickets_validated += 1
            else:
                # Create REWORK ticket for Scribe
                logger.info(
                    f"Worker {self.worker_id}: Issues found in documentation for "
                    f"{ticket.file_name} (iteration {validation_count + 1}/{max_iterations}), "
                    f"creating rework ticket"
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
                state["template"] = DocumentationTemplate.load_lenient(template_data)
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
            # Template not in metadata - try loading from disk (.doc.json file)
            # This happens when PM creates VALIDATION tickets for files with existing docs
            # Handle both flat paths (legacy) and relative paths (new structure)
            doc_file = self._get_doc_path(ticket.file_name)
            if doc_file.exists():
                try:
                    with open(doc_file, "r", encoding="utf-8") as f:
                        template_data = json.load(f)
                    state["template"] = DocumentationTemplate.load_lenient(template_data)
                    logger.info(f"Loaded template from disk: {doc_file}")
                except Exception as e:
                    logger.error(f"Failed to load template from {doc_file}: {e}")
                    return None
            else:
                logger.warning(f"No template in metadata and {doc_file} not found")
                return None

        # Get source code
        state["source_code"] = metadata.get("source_code", "")
        if not state["source_code"]:
            # Source code not in metadata - try loading from disk
            file_path = metadata.get("file_path")
            if file_path:
                from pathlib import Path
                source_path = Path(file_path)
                if source_path.exists():
                    try:
                        with open(source_path, "r", encoding="utf-8", errors="replace") as f:
                            state["source_code"] = f.read()
                        logger.info(f"Loaded source code from disk: {file_path}")
                    except Exception as e:
                        logger.error(f"Failed to load source from {file_path}: {e}")
                        return None
                else:
                    logger.warning(f"Source file not found: {file_path}")
                    return None
            else:
                logger.warning("No source code in metadata and no file_path specified")
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

    def _sample_source_code(self, source_code: str, max_tokens: int) -> tuple[str, bool]:
        """Sample a random portion of source code if it exceeds token limit.

        Selects a random contiguous portion of the source code that fits within
        the token budget. This allows Challenger to validate against a representative
        sample when the full source is too large.

        Args:
            source_code: The full source code.
            max_tokens: Maximum tokens allowed for source code.

        Returns:
            Tuple of (sampled_code, was_sampled) where was_sampled indicates
            if sampling was needed.
        """
        estimator = TokenEstimator()
        source_tokens = estimator.estimate_source_tokens(source_code)

        if source_tokens <= max_tokens:
            return source_code, False

        # Need to sample - calculate what fraction we can keep
        lines = source_code.split("\n")
        total_lines = len(lines)

        # Estimate lines that fit (with some buffer)
        chars_per_token = 3.5  # Conservative for COBOL
        max_chars = int(max_tokens * chars_per_token * 0.9)  # 10% buffer

        # Calculate approximate lines we can include
        avg_chars_per_line = len(source_code) / total_lines if total_lines > 0 else 80
        lines_to_keep = int(max_chars / avg_chars_per_line)
        lines_to_keep = max(100, min(lines_to_keep, total_lines))  # At least 100 lines

        # Random start position (ensuring we don't go past the end)
        max_start = max(0, total_lines - lines_to_keep)
        start_line = random.randint(0, max_start) if max_start > 0 else 0

        # Extract the sample
        sampled_lines = lines[start_line:start_line + lines_to_keep]
        sampled_code = "\n".join(sampled_lines)

        # Add header indicating this is a sample
        header = (
            f"* NOTE: Source code sampled for validation (lines {start_line + 1}-"
            f"{start_line + lines_to_keep} of {total_lines})\n"
            f"* Full source: {source_tokens} tokens, sample: ~{estimator.estimate_source_tokens(sampled_code)} tokens\n\n"
        )

        return header + sampled_code, True

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
            # Check if source code needs sampling due to token limits
            source_code = state["source_code"]
            max_prompt_tokens = self.config.challenger.max_prompt_tokens
            # Reserve tokens for template, system prompt, etc.
            max_source_tokens = max_prompt_tokens - 6000  # More overhead for template

            sampled_code, was_sampled = self._sample_source_code(source_code, max_source_tokens)
            if was_sampled:
                logger.info(
                    f"Worker {self.worker_id}: Source code sampled for validation "
                    f"({ticket.file_name})"
                )

            # Build ChallengerInput
            challenger_input = ChallengerInput(
                template=state["template"],
                source_code=sampled_code,
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

    async def _process_discovery_validation(self, ticket: ProgramManagerTicket) -> None:
        """Process validation for a discovery ticket.

        Discovery tickets are for programs found in call graphs but not in source.
        Scribe either:
        - Found the symbol as an internal routine in a parent file
        - Documented it as external/missing

        This method validates that resolution.

        Args:
            ticket: The discovery validation ticket.
        """
        metadata = ticket.metadata or {}
        discovery_result = metadata.get("discovery_result", {})
        program_id = ticket.program_id

        logger.info(
            f"Worker {self.worker_id}: Validating discovery ticket for {program_id}"
        )

        status = discovery_result.get("status")
        issues = []

        if status == "internal_routine":
            # Scribe found the symbol in a parent file
            parent_file = discovery_result.get("parent_file")
            parent_doc = discovery_result.get("parent_doc")

            if not parent_file:
                issues.append("Discovery marked as internal_routine but no parent_file specified")
            else:
                # Verify the parent documentation exists
                parent_doc_path = self._get_doc_path(parent_file)
                if not parent_doc_path.exists():
                    issues.append(
                        f"Parent documentation {parent_doc_path} not found for "
                        f"internal routine {program_id}"
                    )
                else:
                    # Verify the symbol is mentioned in the parent doc
                    try:
                        with open(parent_doc_path, "r", encoding="utf-8") as f:
                            parent_content = f.read()
                        if program_id.upper() not in parent_content.upper():
                            issues.append(
                                f"Symbol {program_id} not found in parent documentation "
                                f"{parent_doc_path}"
                            )
                    except Exception as e:
                        issues.append(f"Error reading parent doc {parent_doc_path}: {e}")

        elif status == "external":
            # Scribe documented it as external/missing
            search_performed = discovery_result.get("search_performed", {})
            likely_explanation = discovery_result.get("likely_explanation", "")

            # Validate that a search was actually performed
            files_searched = search_performed.get("files_searched", 0)
            if files_searched == 0:
                issues.append("Discovery marked as external but no files were searched")

            # Quick spot-check: if there's a file with matching name, that's suspicious
            input_dir = metadata.get("input_directory")
            if input_dir:
                input_path = Path(input_dir)
                if input_path.exists():
                    # Look for any file with matching name
                    for ext in [".cbl", ".cob", ".cobol", ".prc", ".proc", ".jcl"]:
                        matches = list(input_path.glob(f"**/{program_id}{ext}"))
                        matches += list(input_path.glob(f"**/{program_id.lower()}{ext}"))
                        if matches:
                            issues.append(
                                f"File {matches[0]} exists but was marked as external. "
                                f"Re-verify discovery search."
                            )
                            break

        elif status == "system_utility":
            # Auto-classified as system utility - valid, just approve
            logger.info(
                f"Worker {self.worker_id}: {program_id} classified as system utility, approving"
            )

        else:
            issues.append(f"Unknown discovery status: {status}")

        # Process validation result
        if issues:
            logger.warning(
                f"Worker {self.worker_id}: Discovery validation found issues for {program_id}: "
                f"{issues}"
            )
            # Create clarification ticket for Scribe to re-check
            clarification_metadata = {
                "parent_validation_ticket": ticket.ticket_id,
                "challenger_questions": [
                    {
                        "question_id": f"disc-{i}",
                        "section": "discovery",
                        "question_type": "verification",
                        "question": issue,
                        "severity": "blocking",
                        "evidence": f"Discovery result: {discovery_result}",
                    }
                    for i, issue in enumerate(issues)
                ],
                "challenger_worker": self.worker_id,
                "file_path": metadata.get("file_path"),
                "discovery": True,
                "discovery_result": discovery_result,
            }

            self.beads_client.create_pm_ticket(
                ticket_type=TicketType.CLARIFICATION,
                file_name=ticket.file_name,
                program_id=program_id,
                cycle_number=ticket.cycle_number,
                parent_ticket_id=ticket.ticket_id,
                priority=BeadsPriority.MEDIUM,
                metadata=clarification_metadata,
            )

            self.beads_client.update_ticket_state(
                ticket.ticket_id,
                TicketState.COMPLETED,
                reason=f"Discovery validation found {len(issues)} issues, created clarification",
            )
            self.status.tickets_rejected += 1
        else:
            # Discovery validated successfully
            logger.info(
                f"Worker {self.worker_id}: Discovery validated for {program_id} "
                f"(status: {status})"
            )
            self.beads_client.update_ticket_state(
                ticket.ticket_id,
                TicketState.COMPLETED,
                reason=f"Discovery validated successfully (status: {status})",
            )
            self.status.tickets_validated += 1

    def _get_validation_count(self, file_name: str) -> int:
        """Count completed validation tickets for a file.

        Used to enforce max_iterations limit per file to prevent infinite
        validation/clarification loops.

        Args:
            file_name: The file to count validations for.

        Returns:
            Number of completed validation tickets for this file.
        """
        completed_tickets = self.beads_client.get_tickets_by_state(TicketState.COMPLETED)
        count = sum(
            1
            for t in completed_tickets
            if t.file_name == file_name
            and t.ticket_type == TicketType.VALIDATION
        )
        return count

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

        # Include file path so Scribe can load from disk (don't embed content to avoid bloat)
        if validation_ticket.metadata:
            rework_metadata["file_path"] = validation_ticket.metadata.get("file_path")
            rework_metadata["file_type"] = validation_ticket.metadata.get("file_type")

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
