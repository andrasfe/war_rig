"""Ticket-based orchestration engine for parallel documentation processing.

The TicketOrchestrator coordinates all components of the Program Manager workflow:
- ProgramManagerAgent for ticket creation and batch management
- ScribeWorkerPool for parallel documentation
- ChallengerWorkerPool for parallel validation
- ImperatorAgent for holistic review and approval

This module implements the orchestration flow described in
docs/program_manager_architecture.md Section 3.

Example:
    from war_rig.config import load_config
    from war_rig.orchestration.ticket_engine import TicketOrchestrator

    config = load_config()
    orchestrator = TicketOrchestrator(config)

    result = await orchestrator.run_batch(Path("./input"))
    if result.final_decision == "SATISFIED":
        print(f"Completed {len(result.completed_files)} files in {result.total_cycles} cycles")
"""

from __future__ import annotations

import asyncio
import logging
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import Any

from war_rig.agents.imperator import (
    FileDocumentation,
    HolisticReviewInput,
    HolisticReviewOutput,
    ImperatorAgent,
    ImperatorHolisticDecision,
)
from war_rig.agents.program_manager import (
    ClarificationRequest,
    ProgramManagerAgent,
)
from war_rig.beads import (
    BeadsClient,
    BeadsPriority,
    ProgramManagerTicket,
    TicketState,
    TicketType,
    get_beads_client,
)
from war_rig.config import WarRigConfig, load_config
from war_rig.models.assessments import ConfidenceLevel
from war_rig.workers.challenger_pool import ChallengerWorkerPool
from war_rig.workers.scribe_pool import ScribeWorkerPool

logger = logging.getLogger(__name__)


class OrchestrationStatus(str, Enum):
    """Current status of the orchestration workflow.

    Tracks the phase of batch processing for status reporting.
    """

    IDLE = "idle"
    INITIALIZING = "initializing"
    DOCUMENTING = "documenting"
    VALIDATING = "validating"
    REVIEWING = "reviewing"
    PROCESSING_FEEDBACK = "processing_feedback"
    COMPLETED = "completed"
    STOPPED = "stopped"
    FAILED = "failed"


@dataclass
class BatchResult:
    """Final results from a batch documentation run.

    Contains summary information about the completed batch including
    which files succeeded/failed, cycle counts, and output locations.

    Attributes:
        completed_files: List of file names that were successfully documented.
        failed_files: List of file names that failed processing.
        total_cycles: Number of orchestration cycles executed.
        final_decision: The Imperator's final holistic review decision.
        documentation_outputs: Mapping of file names to output paths.
        started_at: When batch processing started.
        completed_at: When batch processing finished.
        quality_notes: Quality observations from the Imperator.
        assumptions_made: System assumptions made during review.
    """

    completed_files: list[str] = field(default_factory=list)
    failed_files: list[str] = field(default_factory=list)
    total_cycles: int = 0
    final_decision: str = ""
    documentation_outputs: dict[str, Path] = field(default_factory=dict)
    started_at: datetime | None = None
    completed_at: datetime | None = None
    quality_notes: list[str] = field(default_factory=list)
    assumptions_made: list[dict[str, Any]] = field(default_factory=list)

    @property
    def success(self) -> bool:
        """Whether the batch completed successfully."""
        return self.final_decision in (
            "SATISFIED",
            "FORCED_COMPLETE",
            ImperatorHolisticDecision.SATISFIED.value,
            ImperatorHolisticDecision.FORCED_COMPLETE.value,
        )

    @property
    def duration_seconds(self) -> float:
        """Total duration of batch processing in seconds."""
        if self.started_at and self.completed_at:
            return (self.completed_at - self.started_at).total_seconds()
        return 0.0

    def to_dict(self) -> dict[str, Any]:
        """Convert to dictionary for serialization."""
        return {
            "completed_files": self.completed_files,
            "failed_files": self.failed_files,
            "total_cycles": self.total_cycles,
            "final_decision": self.final_decision,
            "documentation_outputs": {
                k: str(v) for k, v in self.documentation_outputs.items()
            },
            "started_at": self.started_at.isoformat() if self.started_at else None,
            "completed_at": self.completed_at.isoformat() if self.completed_at else None,
            "duration_seconds": self.duration_seconds,
            "success": self.success,
            "quality_notes": self.quality_notes,
            "assumptions_made": self.assumptions_made,
        }


@dataclass
class OrchestrationState:
    """Internal state tracking for the orchestrator.

    Tracks progress through the batch workflow including file states,
    cycle history, and intermediate results.
    """

    batch_id: str = ""
    cycle: int = 0
    status: OrchestrationStatus = OrchestrationStatus.IDLE
    started_at: datetime | None = None

    # File tracking
    total_files: int = 0
    documented_files: int = 0
    validated_files: int = 0

    # Cycle history
    clarification_history: list[ClarificationRequest] = field(default_factory=list)
    review_history: list[HolisticReviewOutput] = field(default_factory=list)

    # Current status message
    status_message: str = ""


class TicketOrchestrator:
    """Orchestrates ticket-based parallel documentation workflow.

    The TicketOrchestrator is the main entry point for batch documentation
    processing. It coordinates:

    1. ProgramManagerAgent - Creates tickets for source files
    2. ScribeWorkerPool - Parallel documentation workers
    3. ChallengerWorkerPool - Parallel validation workers
    4. ImperatorAgent - Holistic review and approval

    The workflow repeats until either:
    - Imperator is satisfied with all documentation
    - Maximum cycles reached (forced completion)
    - Stop requested by user

    Attributes:
        config: War Rig configuration.
        beads_client: Client for ticket operations.
        program_manager: Agent for batch initialization.
        imperator: Agent for holistic review.
        state: Current orchestration state.

    Example:
        orchestrator = TicketOrchestrator(config)

        # Start batch processing
        result = await orchestrator.run_batch(Path("./input"))

        # Check results
        if result.success:
            print(f"Documented {len(result.completed_files)} files")
        else:
            print(f"Failed: {result.failed_files}")

        # Get status during processing
        status = orchestrator.get_status()
        print(f"Cycle {status['cycle']}: {status['status']}")

        # Stop gracefully if needed
        await orchestrator.stop()
    """

    def __init__(
        self,
        config: WarRigConfig | None = None,
        beads_client: BeadsClient | None = None,
        use_mock: bool = False,
    ):
        """Initialize the ticket orchestrator.

        Args:
            config: War Rig configuration. If None, loads from environment.
            beads_client: BeadsClient for ticket operations. If None, uses default.
            use_mock: If True, use mock LLM responses (for testing).
        """
        self.config = config or load_config()
        self.use_mock = use_mock

        # Initialize beads client with persistent ticket storage
        if beads_client is None:
            tickets_file = self.config.output_directory / ".war_rig_tickets.json"
            # When beads is enabled, use output_directory as isolated beads_dir
            # This keeps War Rig's beads instance separate from the project's .beads/
            beads_dir = self.config.output_directory if self.config.beads_enabled else None
            self.beads_client = get_beads_client(
                enabled=self.config.beads_enabled,
                dry_run=self.config.beads_dry_run,
                tickets_file=tickets_file,
                beads_dir=beads_dir,
            )
        else:
            self.beads_client = beads_client

        # Initialize agents
        self.program_manager = ProgramManagerAgent(
            config=self.config,
            beads_client=self.beads_client,
        )
        self.imperator = ImperatorAgent(
            config=self.config.imperator,
            api_config=self.config.api,
        )

        # Worker pools (created on demand)
        self._scribe_pool: ScribeWorkerPool | None = None
        self._challenger_pool: ChallengerWorkerPool | None = None

        # Internal state
        self._state = OrchestrationState()
        self._stop_requested = False
        self._lock = asyncio.Lock()
        self._input_directory: Path | None = None

    @property
    def state(self) -> OrchestrationState:
        """Get the current orchestration state."""
        return self._state

    async def run_batch(self, input_dir: Path) -> BatchResult:
        """Run the complete batch documentation workflow.

        This is the main entry point for batch processing. It:
        1. Uses ProgramManagerAgent to create tickets for all files
        2. Starts ScribeWorkerPool and ChallengerWorkerPool
        3. Waits for all tickets to be processed
        4. Triggers Imperator holistic review
        5. If Imperator needs clarification, creates new tickets and restarts pools
        6. Repeats until max cycles or Imperator satisfied

        Args:
            input_dir: Directory containing source files to process.

        Returns:
            BatchResult with final results and statistics.

        Raises:
            ValueError: If input_dir does not exist.

        Example:
            result = await orchestrator.run_batch(Path("./input"))
            if result.success:
                for file_name, output_path in result.documentation_outputs.items():
                    print(f"{file_name} -> {output_path}")
        """
        if not input_dir.exists():
            raise ValueError(f"Input directory does not exist: {input_dir}")

        # Store input directory for worker pools
        self._input_directory = input_dir

        # Initialize result
        result = BatchResult(
            started_at=datetime.utcnow(),
        )

        # Reset state
        self._stop_requested = False
        self._state = OrchestrationState(
            status=OrchestrationStatus.INITIALIZING,
            started_at=datetime.utcnow(),
        )

        try:
            # Phase 1: Initialize batch with Program Manager
            logger.info(f"Initializing batch from {input_dir}")
            self._state.status_message = "Discovering source files..."

            tickets = self.program_manager.initialize_batch(input_dir)

            if not tickets:
                logger.warning("No source files found to process")
                result.final_decision = "NO_FILES"
                result.completed_at = datetime.utcnow()
                self._state.status = OrchestrationStatus.COMPLETED
                return result

            self._state.batch_id = self.program_manager.batch_id or ""
            self._state.total_files = len(tickets)
            logger.info(f"Batch {self._state.batch_id}: {len(tickets)} files to process")

            # Phase 2-6: Run cycles until completion
            max_cycles = self.config.pm_max_cycles

            while self._state.cycle < max_cycles and not self._stop_requested:
                self._state.cycle += 1
                logger.info(f"Starting cycle {self._state.cycle} of {max_cycles}")

                # Run documentation and validation
                await self._run_worker_cycle()

                if self._stop_requested:
                    break

                # Trigger holistic review
                review_result = await self._run_holistic_review()

                if review_result is None:
                    logger.error("Holistic review failed")
                    self._state.status = OrchestrationStatus.FAILED
                    result.final_decision = "ERROR"
                    break

                self._state.review_history.append(review_result)

                # Check if satisfied
                if review_result.decision == ImperatorHolisticDecision.SATISFIED:
                    logger.info("Imperator satisfied - batch complete")
                    result.final_decision = review_result.decision.value
                    result.quality_notes = review_result.quality_notes
                    break

                elif review_result.decision == ImperatorHolisticDecision.FORCED_COMPLETE:
                    logger.info("Forced completion at max cycles")
                    result.final_decision = review_result.decision.value
                    result.quality_notes = review_result.quality_notes
                    break

                elif review_result.decision == ImperatorHolisticDecision.NEEDS_CLARIFICATION:
                    if self._state.cycle >= max_cycles:
                        logger.info("Max cycles reached, forcing completion")
                        result.final_decision = ImperatorHolisticDecision.FORCED_COMPLETE.value
                        break

                    # Create clarification tickets for next cycle
                    logger.info(
                        f"Imperator needs clarification: "
                        f"{len(review_result.clarification_requests)} requests"
                    )
                    await self._handle_imperator_feedback(review_result)

            # Collect final results
            result = self._collect_results(result)
            result.completed_at = datetime.utcnow()
            self._state.status = OrchestrationStatus.COMPLETED

            logger.info(
                f"Batch complete: {len(result.completed_files)} files, "
                f"{result.total_cycles} cycles, decision={result.final_decision}"
            )

            return result

        except Exception as e:
            logger.error(f"Batch processing failed: {e}")
            self._state.status = OrchestrationStatus.FAILED
            self._state.status_message = str(e)
            result.final_decision = "ERROR"
            result.completed_at = datetime.utcnow()
            raise

        finally:
            # Ensure pools are stopped
            await self._stop_pools()

    def _is_documentation_in_progress(self) -> bool:
        """Check if documentation work might still produce validation tickets.

        Used by Challengers to know if they should wait for more work
        instead of timing out. Returns True if:
        - There are DOCUMENTATION tickets available (CREATED state)
        - There are DOCUMENTATION tickets being processed (IN_PROGRESS state)
        - Scribes are actively working

        Returns:
            True if documentation is still in progress.
        """
        # Check for available documentation tickets
        doc_available = self.beads_client.get_available_tickets(
            ticket_type=TicketType.DOCUMENTATION
        )
        if doc_available:
            return True

        # Check for in-progress documentation tickets
        doc_in_progress = [
            t for t in self.beads_client._pm_ticket_cache.values()
            if t.ticket_type == TicketType.DOCUMENTATION
            and t.state == TicketState.IN_PROGRESS
        ]
        if doc_in_progress:
            return True

        # Check if Scribe pool has active workers
        if self._scribe_pool:
            status = self._scribe_pool.get_status()
            if status.get("active_workers", 0) > 0:
                return True

        return False

    async def _run_worker_cycle(self) -> None:
        """Run one cycle with Scribe and Challenger workers in parallel.

        Creates both worker pools and starts them simultaneously, allowing
        Challengers to validate documents as soon as Scribes create them.
        This is more efficient than waiting for all documentation to complete
        before starting validation.

        The pipeline terminates when:
        - All DOCUMENTATION tickets are processed (completed or failed)
        - All VALIDATION tickets are processed (completed or failed)
        - Both worker pools are idle
        """
        self._state.status = OrchestrationStatus.DOCUMENTING
        self._state.status_message = "Running documentation and validation pipeline..."

        logger.info("Starting parallel Scribe and Challenger worker pools")

        # Create Scribe pool
        self._scribe_pool = ScribeWorkerPool(
            config=self.config,
            beads_client=self.beads_client,
            input_directory=self._input_directory,
            num_workers=self.config.num_scribes,
            poll_interval=2.0,
            idle_timeout=30.0,
        )

        # Create Challenger pool with upstream check
        # Challengers won't idle-timeout while Scribes might produce more work
        self._challenger_pool = ChallengerWorkerPool(
            num_workers=self.config.num_challengers,
            config=self.config,
            beads_client=self.beads_client,
            poll_interval=2.0,
            upstream_active_check=self._is_documentation_in_progress,
        )

        # Start both pools simultaneously
        await self._scribe_pool.start()
        await self._challenger_pool.start()

        logger.info("Both worker pools started, running pipeline...")

        # Wait for both pools to complete
        # Scribes will finish when no more DOCUMENTATION tickets
        # Challengers will finish when no more VALIDATION tickets AND no upstream activity
        scribe_wait = asyncio.create_task(self._scribe_pool.wait())
        challenger_wait = asyncio.create_task(self._challenger_pool.wait_for_completion())

        await asyncio.gather(scribe_wait, challenger_wait, return_exceptions=True)

        logger.info("Pipeline complete, all workers finished")

        # Stop pools
        await self._stop_pools()

        # Update state with progress
        self._update_progress()

    async def _run_holistic_review(self) -> HolisticReviewOutput | None:
        """Trigger and wait for Imperator holistic review.

        Collects all completed documentation and submits it to the
        Imperator for batch-level review.

        Returns:
            HolisticReviewOutput with the review decision and feedback.
        """
        self._state.status = OrchestrationStatus.REVIEWING
        self._state.status_message = "Running Imperator holistic review..."

        logger.info("Triggering holistic review")

        # Build review input from completed documentation
        review_input = self._build_holistic_review_input()

        if not review_input.file_documentation:
            logger.warning("No completed documentation to review")
            return HolisticReviewOutput(
                success=False,
                error="No documentation to review",
                decision=ImperatorHolisticDecision.NEEDS_CLARIFICATION,
            )

        # Run holistic review
        try:
            output = await self.imperator.holistic_review(
                review_input,
                use_mock=self.use_mock,
            )
            return output

        except Exception as e:
            logger.error(f"Holistic review failed: {e}")
            return HolisticReviewOutput(
                success=False,
                error=str(e),
                decision=ImperatorHolisticDecision.NEEDS_CLARIFICATION,
            )

    def _build_holistic_review_input(self) -> HolisticReviewInput:
        """Build input for Imperator holistic review.

        Collects all completed documentation, assessments, and cross-file
        analysis data into a HolisticReviewInput.

        Returns:
            HolisticReviewInput ready for the Imperator.
        """
        # Get completed documentation tickets
        completed_docs = self.beads_client.get_tickets_by_state(
            state=TicketState.COMPLETED,
            ticket_type=TicketType.DOCUMENTATION,
        )

        # Build file documentation list
        file_docs: list[FileDocumentation] = []
        per_file_confidence: dict[str, ConfidenceLevel] = {}
        per_file_issues: dict[str, list[str]] = {}

        for ticket in completed_docs:
            # In a real implementation, we would load the actual template
            # from ticket metadata or storage. For now, create placeholder.
            # The ScribeWorkerPool should store results in ticket metadata.
            file_docs.append(
                FileDocumentation(
                    file_name=ticket.file_name,
                    program_id=ticket.program_id or ticket.file_name.split(".")[0],
                    template=self._get_template_for_ticket(ticket),
                    iteration_count=ticket.cycle_number,
                )
            )
            per_file_confidence[ticket.file_name] = ConfidenceLevel.MEDIUM

        # Get previous clarification requests to avoid repetition
        previous_requests = self._state.clarification_history.copy()

        # Build cross-file analysis (simplified for now)
        # In full implementation, this would analyze actual program relationships
        shared_copybooks: dict[str, list[str]] = {}
        call_graph: dict[str, list[str]] = {}
        data_flow: dict[str, list[str]] = {}

        return HolisticReviewInput(
            batch_id=self._state.batch_id,
            cycle=self._state.cycle,
            file_documentation=file_docs,
            shared_copybooks=shared_copybooks,
            call_graph=call_graph,
            data_flow=data_flow,
            per_file_confidence=per_file_confidence,
            per_file_issues=per_file_issues,
            previous_clarification_requests=[
                # Convert our ClarificationRequest to Imperator's format
                self._convert_clarification_request(req)
                for req in previous_requests
            ],
            previous_chrome_tickets=[],
            resolution_status={},
            max_cycles=self.config.pm_max_cycles,
        )

    def _get_template_for_ticket(
        self,
        ticket: ProgramManagerTicket,
    ) -> Any:
        """Get the documentation template for a completed ticket.

        In a full implementation, this would retrieve the actual template
        from ticket metadata or a storage system. For now, returns a
        minimal placeholder.

        Args:
            ticket: The completed documentation ticket.

        Returns:
            DocumentationTemplate for the file.
        """
        # Import here to avoid circular dependency
        from war_rig.models.templates import (
            DocumentationTemplate,
            FileType,
            HeaderSection,
            ProgramType,
            PurposeSection,
        )

        # In real implementation, deserialize from ticket.metadata
        return DocumentationTemplate(
            header=HeaderSection(
                program_id=ticket.program_id or ticket.file_name.split(".")[0],
                file_name=ticket.file_name,
                file_type=FileType.COBOL,
                analyzed_by=self.config.rig_id,
                analyzed_at=datetime.utcnow(),
                iteration_count=ticket.cycle_number,
            ),
            purpose=PurposeSection(
                summary=f"Documentation for {ticket.file_name}",
                business_context="Generated by TicketOrchestrator",
                program_type=ProgramType.BATCH,
                citations=[],
            ),
        )

    def _convert_clarification_request(
        self,
        request: ClarificationRequest,
    ) -> Any:
        """Convert Program Manager's ClarificationRequest to Imperator's format.

        Args:
            request: ClarificationRequest from Program Manager.

        Returns:
            Imperator's ClarificationRequest model.
        """
        from war_rig.agents.imperator import (
            ClarificationRequest as ImperatorClarificationRequest,
        )
        from uuid import uuid4

        return ImperatorClarificationRequest(
            request_id=f"CLR-{uuid4().hex[:8].upper()}",
            file_name=request.file_name,
            question=request.issue_description,
            context=request.guidance or "",
            related_files=[],
            cycle_asked=self._state.cycle,
        )

    async def _handle_imperator_feedback(
        self,
        review_output: HolisticReviewOutput,
    ) -> None:
        """Process Imperator feedback and create new tickets.

        Converts the Imperator's feedback (clarification requests, Chrome
        tickets) into beads tickets for the next cycle.

        Args:
            review_output: The holistic review output with feedback.
        """
        self._state.status = OrchestrationStatus.PROCESSING_FEEDBACK
        self._state.status_message = "Processing Imperator feedback..."

        # Create clarification requests from review output
        clarification_requests: list[ClarificationRequest] = []

        for req in review_output.clarification_requests:
            clarification_requests.append(
                ClarificationRequest(
                    file_name=req.file_name,
                    issue_description=req.question,
                    section=None,
                    priority=BeadsPriority.MEDIUM,
                    guidance=req.context,
                )
            )
            # Add to history to avoid repetition
            self._state.clarification_history.append(
                ClarificationRequest(
                    file_name=req.file_name,
                    issue_description=req.question,
                    guidance=req.context,
                )
            )

        # Create tickets from file-level feedback (Chrome tickets)
        for file_name, chrome_tickets in review_output.file_feedback.items():
            for ticket in chrome_tickets:
                clarification_requests.append(
                    ClarificationRequest(
                        file_name=file_name,
                        issue_description=ticket.description,
                        section=ticket.section,
                        priority=self._map_issue_priority(ticket.priority),
                        guidance=ticket.guidance,
                    )
                )

        # Use Program Manager to create tickets
        if clarification_requests:
            logger.info(f"Creating {len(clarification_requests)} clarification tickets")
            self.program_manager.handle_clarifications(clarification_requests)

    def _map_issue_priority(self, priority: Any) -> BeadsPriority:
        """Map Imperator issue priority to BeadsPriority.

        Args:
            priority: IssuePriority from Chrome ticket.

        Returns:
            Corresponding BeadsPriority.
        """
        from war_rig.models.tickets import IssuePriority

        mapping = {
            IssuePriority.CRITICAL: BeadsPriority.CRITICAL,
            IssuePriority.HIGH: BeadsPriority.HIGH,
            IssuePriority.MEDIUM: BeadsPriority.MEDIUM,
        }
        return mapping.get(priority, BeadsPriority.MEDIUM)

    def _update_progress(self) -> None:
        """Update progress counters from ticket states."""
        # Count documented files
        completed_docs = self.beads_client.get_tickets_by_state(
            state=TicketState.COMPLETED,
            ticket_type=TicketType.DOCUMENTATION,
        )
        self._state.documented_files = len(completed_docs)

        # Count validated files
        completed_validations = self.beads_client.get_tickets_by_state(
            state=TicketState.COMPLETED,
            ticket_type=TicketType.VALIDATION,
        )
        self._state.validated_files = len(completed_validations)

    def _collect_results(self, result: BatchResult) -> BatchResult:
        """Collect final results from completed tickets.

        Gathers all completed documentation and builds the final
        BatchResult with success/failure lists and output paths.

        Args:
            result: BatchResult to populate.

        Returns:
            Updated BatchResult with collected data.
        """
        result.total_cycles = self._state.cycle

        # Get completed documentation tickets
        completed_docs = self.beads_client.get_tickets_by_state(
            state=TicketState.COMPLETED,
            ticket_type=TicketType.DOCUMENTATION,
        )

        for ticket in completed_docs:
            result.completed_files.append(ticket.file_name)
            # In real implementation, would set actual output path
            result.documentation_outputs[ticket.file_name] = (
                self.config.output_directory / f"{ticket.file_name}.md"
            )

        # Get failed/blocked tickets
        blocked_docs = self.beads_client.get_tickets_by_state(
            state=TicketState.BLOCKED,
            ticket_type=TicketType.DOCUMENTATION,
        )

        for ticket in blocked_docs:
            result.failed_files.append(ticket.file_name)

        # Collect assumptions from review history
        for review in self._state.review_history:
            for assumption in review.assumptions:
                result.assumptions_made.append({
                    "assumption_id": assumption.assumption_id,
                    "description": assumption.description,
                    "affected_files": assumption.affected_files,
                    "confidence": assumption.confidence.value,
                    "needs_verification": assumption.needs_verification,
                })

        return result

    async def _stop_pools(self) -> None:
        """Stop all worker pools gracefully."""
        if self._scribe_pool:
            try:
                await self._scribe_pool.stop()
            except Exception as e:
                logger.warning(f"Error stopping Scribe pool: {e}")
            self._scribe_pool = None

        if self._challenger_pool:
            try:
                await self._challenger_pool.stop()
            except Exception as e:
                logger.warning(f"Error stopping Challenger pool: {e}")
            self._challenger_pool = None

    def get_status(self) -> dict[str, Any]:
        """Get current orchestration status.

        Returns a dictionary with the current state of orchestration
        including cycle, status, file counts, and worker pool status.

        Returns:
            Dictionary with status information.

        Example:
            status = orchestrator.get_status()
            print(f"Cycle {status['cycle']}: {status['status']}")
            print(f"Files: {status['documented_files']}/{status['total_files']}")
        """
        # Update progress counters from actual ticket states
        self._update_progress()

        status: dict[str, Any] = {
            "batch_id": self._state.batch_id,
            "cycle": self._state.cycle,
            "max_cycles": self.config.pm_max_cycles,
            "status": self._state.status.value,
            "status_message": self._state.status_message,
            "total_files": self._state.total_files,
            "documented_files": self._state.documented_files,
            "validated_files": self._state.validated_files,
            "started_at": (
                self._state.started_at.isoformat() if self._state.started_at else None
            ),
        }

        # Add pool status if running
        if self._scribe_pool:
            status["scribe_pool"] = self._scribe_pool.get_status()

        if self._challenger_pool:
            status["challenger_pool"] = self._challenger_pool.get_status()

        return status

    async def stop(self) -> None:
        """Gracefully stop the orchestrator.

        Signals the orchestrator to stop processing after the current
        operation completes. Worker pools are stopped and resources
        are cleaned up.

        Example:
            # Start in background
            task = asyncio.create_task(orchestrator.run_batch(input_dir))

            # Later, stop gracefully
            await orchestrator.stop()
            await task  # Get result
        """
        logger.info("Stop requested for TicketOrchestrator")
        self._stop_requested = True
        self._state.status = OrchestrationStatus.STOPPED
        self._state.status_message = "Stop requested by user"

        await self._stop_pools()
