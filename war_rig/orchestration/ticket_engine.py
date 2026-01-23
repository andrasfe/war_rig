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
    ProgramSummary,
    SystemOverviewInput,
    SystemOverviewOutput,
)
from war_rig.analysis.call_graph import CallGraphAnalyzer, CallGraphAnalysis
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
from war_rig.utils.exceptions import FatalWorkerError, MaxTicketRetriesExceeded
from war_rig.utils.file_lock import FileLockManager
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
    ANALYZING_CALL_GRAPH = "analyzing_call_graph"
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
    rework_files: int = 0  # Chrome/rework tickets completed

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

        # Centralized file lock manager for concurrent workers (war_rig-qxw9)
        # Workers acquire locks before processing tickets to prevent race conditions
        # when multiple workers attempt to write to the same output file
        self._file_lock_manager = FileLockManager(lock_timeout=300.0)

        # Internal state
        self._state = OrchestrationState()
        self._stop_requested = False
        self._lock = asyncio.Lock()
        self._input_directory: Path | None = None

        # Feedback context from Imperator (distributed to new tickets)
        self._current_feedback_context: "FeedbackContext | None" = None

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
                # Persist current cycle for status display
                self.beads_client.set_current_cycle(self._state.cycle)
                logger.info(f"Starting cycle {self._state.cycle} of {max_cycles}")

                # Run documentation and validation
                await self._run_worker_cycle()

                if self._stop_requested:
                    break

                # Run call graph analysis to check for gaps
                call_graph_result = await self._run_call_graph_analysis()
                has_call_graph_gaps = (
                    call_graph_result
                    and call_graph_result.has_gaps()
                    and self._state.cycle < max_cycles
                )

                # Always run holistic review to capture feedback for next cycle
                review_result = await self._run_holistic_review()

                if review_result is None:
                    logger.error("Holistic review failed")
                    self._state.status = OrchestrationStatus.FAILED
                    result.final_decision = "ERROR"
                    break

                self._state.review_history.append(review_result)

                # Always capture feedback context for next cycle (even if we have gaps)
                # This ensures quality notes propagate to all subsequent work
                await self._handle_imperator_feedback(review_result)

                # If call graph has gaps, create tickets and continue to next cycle
                if has_call_graph_gaps:
                    missing = call_graph_result.get_missing_for_documentation()
                    if missing:
                        logger.info(
                            f"Call graph has {len(missing)} gaps - "
                            f"creating DOCUMENTATION tickets (feedback captured)"
                        )
                        self._create_documentation_tickets_for_missing(
                            missing, call_graph_result
                        )
                        # Continue to next cycle to document missing programs
                        continue

                # Check review decision for completion
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
                    # Check if there are actually any issues to address
                    total_issues = (
                        len(review_result.clarification_requests) +
                        sum(len(tickets) for tickets in review_result.file_feedback.values())
                    )

                    if total_issues == 0:
                        # No actual issues - treat as satisfied
                        logger.info("Imperator said NEEDS_CLARIFICATION but no issues - completing")
                        result.final_decision = ImperatorHolisticDecision.SATISFIED.value
                        result.quality_notes = review_result.quality_notes
                        break

                    if self._state.cycle >= max_cycles:
                        logger.info("Max cycles reached, forcing completion")
                        result.final_decision = ImperatorHolisticDecision.FORCED_COMPLETE.value
                        break

                    # Log that we're continuing with clarification work
                    logger.info(
                        f"Imperator needs clarification: {total_issues} issues "
                        f"({len(review_result.clarification_requests)} requests, "
                        f"{sum(len(t) for t in review_result.file_feedback.values())} chrome tickets)"
                    )

            # Collect final results
            result = self._collect_results(result)

            # Generate system overview if documentation was successful
            if result.completed_files and result.final_decision in (
                "SATISFIED",
                "FORCED_COMPLETE",
                ImperatorHolisticDecision.SATISFIED.value,
                ImperatorHolisticDecision.FORCED_COMPLETE.value,
            ):
                self._state.status_message = "Generating system overview..."
                self._create_system_overview_ticket()
                await self._process_system_overview()

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
            # Recover any orphaned tickets for next run
            orphaned_count = self.beads_client.reset_orphaned_tickets()
            if orphaned_count > 0:
                logger.info(f"Reset {orphaned_count} orphaned tickets for future retry")

    def _is_documentation_in_progress(self) -> bool:
        """Check if Scribe work might still produce validation tickets.

        Used by Challengers to know if they should wait for more work
        instead of timing out. Returns True if:
        - Scribe pool is running AND has active workers
        - There are IN_PROGRESS tickets being processed

        Note: Available tickets don't count if the scribe pool has stopped,
        since no one will process them in this cycle. They'll be picked up
        in the next cycle or marked as stuck.

        Returns:
            True if Scribe work is still in progress.
        """
        # First check if scribe pool is still running with active workers
        scribe_pool_active = False
        if self._scribe_pool:
            status = self._scribe_pool.get_status()
            # Pool returns active_count (processing) and idle_count (waiting for work)
            # Both mean workers are still running and may produce validation tickets
            scribe_pool_active = (
                status.get("active_count", 0) > 0 or status.get("idle_count", 0) > 0
            )

        # If scribe pool has no active workers, check if it's completely stopped
        if not scribe_pool_active and self._scribe_pool:
            status = self._scribe_pool.get_status()
            # If all workers are stopped, upstream is not active
            # (available tickets will be handled in next cycle)
            if status.get("num_workers", 0) == status.get("stopped_count", 0):
                return False

        # Ticket types that Scribes process
        scribe_ticket_types = [
            TicketType.DOCUMENTATION,
            TicketType.CLARIFICATION,
            TicketType.CHROME,
        ]

        # Check for in-progress tickets of any Scribe type
        # These are actively being worked on
        in_progress = [
            t for t in self.beads_client._pm_ticket_cache.values()
            if t.ticket_type in scribe_ticket_types
            and t.state == TicketState.IN_PROGRESS
        ]
        if in_progress:
            return True

        # If scribe pool is active, check for available tickets
        if scribe_pool_active:
            for ticket_type in scribe_ticket_types:
                available = self.beads_client.get_available_tickets(
                    ticket_type=ticket_type
                )
                if available:
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

        # Create Scribe pool with centralized file lock manager
        self._scribe_pool = ScribeWorkerPool(
            config=self.config,
            beads_client=self.beads_client,
            input_directory=self._input_directory,
            num_workers=self.config.num_scribes,
            poll_interval=2.0,
            idle_timeout=30.0,
            file_lock_manager=self._file_lock_manager,
            exit_on_error=self.config.exit_on_error,
        )

        # Create Challenger pool with upstream check and file lock manager
        # Challengers won't idle-timeout while Scribes might produce more work
        self._challenger_pool = ChallengerWorkerPool(
            num_workers=self.config.num_challengers,
            config=self.config,
            beads_client=self.beads_client,
            poll_interval=2.0,
            upstream_active_check=self._is_documentation_in_progress,
            file_lock_manager=self._file_lock_manager,
            exit_on_error=self.config.exit_on_error,
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

        # Gather results and check for FatalWorkerError
        results = await asyncio.gather(scribe_wait, challenger_wait, return_exceptions=True)

        # Re-raise FatalWorkerError if any worker encountered a fatal error
        for result in results:
            if isinstance(result, FatalWorkerError):
                logger.error(f"Fatal worker error: {result}")
                raise result

        logger.info("Pipeline complete, all workers finished")

        # Stop pools
        await self._stop_pools()

        # Check for orphaned tickets (CLAIMED/IN_PROGRESS that weren't completed)
        # This handles cases where workers crashed or timed out mid-processing
        orphaned_count = self.beads_client.reset_orphaned_tickets()
        if orphaned_count > 0:
            logger.info(
                f"Found {orphaned_count} orphaned tickets, restarting workers to process them"
            )
            # Recursively process the orphaned tickets
            await self._run_worker_cycle()
            return  # Skip update_progress, the recursive call handles it

        # Check for "universally failed" tickets - CREATED but all workers skipped them
        # This happens when all workers failed twice on a ticket and added it to _failed_tickets
        stuck_created = self.beads_client.get_available_tickets(
            ticket_type=TicketType.DOCUMENTATION
        )
        stuck_created.extend(
            self.beads_client.get_available_tickets(ticket_type=TicketType.CLARIFICATION)
        )
        stuck_created.extend(
            self.beads_client.get_available_tickets(ticket_type=TicketType.CHROME)
        )

        if stuck_created:
            # Mark these as BLOCKED - they failed on all workers
            for ticket in stuck_created:
                logger.warning(
                    f"Ticket {ticket.ticket_id} ({ticket.file_name}) universally failed - "
                    f"all workers skipped it. Marking as BLOCKED."
                )
                self.beads_client.update_ticket_state(
                    ticket.ticket_id,
                    TicketState.BLOCKED,
                    reason="All workers failed to process this ticket",
                )

        # Update state with progress
        self._update_progress()

        # Check for BLOCKED tickets and run automatic rescue with Super-Scribe
        await self._run_super_scribe_rescue()

        # Final validation: ensure no tickets are stuck in non-terminal states
        # Terminal states are: COMPLETED, BLOCKED, CANCELLED, MERGED
        # Non-terminal states are: CREATED, CLAIMED, IN_PROGRESS, REWORK
        await self._validate_cycle_complete(max_retries=3)

    async def _run_call_graph_analysis(self) -> CallGraphAnalysis | None:
        """Analyze the call graph to identify documentation gaps.

        Scans all completed documentation to build a call graph showing
        program relationships. Identifies:
        - Custom programs that are called but not documented (gaps)
        - System utilities (which don't need documentation)
        - External dependencies for SYSTEM_DESIGN.md

        Returns:
            CallGraphAnalysis with gap information, or None if analysis fails.
        """
        self._state.status = OrchestrationStatus.ANALYZING_CALL_GRAPH
        self._state.status_message = "Analyzing call graph for documentation gaps..."

        logger.info("Running call graph analysis")

        try:
            analyzer = CallGraphAnalyzer(doc_directory=self.config.output_directory)
            analysis = analyzer.analyze()

            # Log summary
            logger.info(
                f"Call graph: {len(analysis.documented_programs)} documented programs, "
                f"{len(analysis.external_dependencies)} external deps, "
                f"{len(analysis.system_utilities)} system utils, "
                f"{len(analysis.custom_missing)} custom missing"
            )

            if analysis.custom_missing:
                logger.info(f"Missing custom programs: {', '.join(analysis.custom_missing)}")

            # Generate SYSTEM_DESIGN.md for external dependencies
            if analysis.external_dependencies:
                system_design_path = self.config.output_directory / "SYSTEM_DESIGN.md"
                system_design_content = analyzer.generate_system_design_md(analysis)
                system_design_path.write_text(system_design_content, encoding="utf-8")
                logger.info(f"Generated SYSTEM_DESIGN.md with {len(analysis.external_dependencies)} external dependencies")

            # Also regenerate CALL_GRAPH.md for current state
            call_graph_path = self.config.output_directory / "CALL_GRAPH.md"
            call_graph_content = analyzer.generate_markdown_report(analysis)
            call_graph_path.write_text(call_graph_content, encoding="utf-8")
            logger.info(f"Updated CALL_GRAPH.md")

            return analysis

        except Exception as e:
            logger.error(f"Call graph analysis failed: {e}")
            return None

    def _create_documentation_tickets_for_missing(
        self,
        missing_programs: list[str],
        call_graph: CallGraphAnalysis,
    ) -> list[ProgramManagerTicket]:
        """Create DOCUMENTATION tickets for callers of undocumented programs.

        When a call graph gap is detected (e.g., XYZ calls SOME_CALL but SOME_CALL
        is not documented), we create tickets for the CALLER (XYZ), not the callee
        (SOME_CALL). The caller is the file that needs documentation about what it
        calls - the callee might be an external program or system utility that
        doesn't exist in our codebase.

        Bug fixes applied:
        - war_rig-k323: Create tickets for callers, not callees
        - war_rig-4o4l: Only create tickets for files that exist in input directory

        Args:
            missing_programs: List of program IDs (callees) that are not documented.
            call_graph: The full call graph analysis with caller information.

        Returns:
            List of created tickets for callers.
        """
        from uuid import uuid4

        created_tickets: list[ProgramManagerTicket] = []

        # Build a set of callers that need documentation tickets
        # For each missing callee, find which documented callers call it
        callers_needing_tickets: dict[str, set[str]] = {}  # caller -> set of missing callees it calls

        for missing_callee in missing_programs:
            # Find all documented programs that call this missing callee
            for program_id, program_info in call_graph.documented_programs.items():
                for call in program_info.calls:
                    if call.callee == missing_callee:
                        if program_id not in callers_needing_tickets:
                            callers_needing_tickets[program_id] = set()
                        callers_needing_tickets[program_id].add(missing_callee)

        for caller_id, missing_callees in callers_needing_tickets.items():
            # Check if ticket already exists for this caller
            existing = [
                t for t in self.beads_client._pm_ticket_cache.values()
                if t.program_id == caller_id
                and t.ticket_type == TicketType.DOCUMENTATION
                and t.state not in (TicketState.COMPLETED, TicketState.BLOCKED, TicketState.CANCELLED)
            ]

            if existing:
                logger.debug(f"Documentation ticket already exists for caller {caller_id}")
                continue

            # Get file name from the documented program info
            program_info = call_graph.documented_programs.get(caller_id)
            if not program_info:
                logger.warning(f"No program info found for caller {caller_id}")
                continue

            # Determine the file name - use the documented file_name or fall back to program_id.cbl
            file_name = program_info.file_name or f"{caller_id}.cbl"

            # Bug fix war_rig-4o4l: Verify the file exists before creating a ticket
            if self._input_directory:
                # Try common extensions
                file_exists = False
                for ext in [".cbl", ".CBL", ".cob", ".COB", ".jcl", ".JCL", ".prc", ".PRC", ""]:
                    # Check both the exact file_name and program_id with extension
                    candidates = [
                        self._input_directory / file_name,
                        self._input_directory / f"{caller_id}{ext}",
                    ]
                    for candidate in candidates:
                        if candidate.exists():
                            file_name = candidate.name
                            file_exists = True
                            break
                    if file_exists:
                        break

                if not file_exists:
                    logger.debug(
                        f"Skipping ticket for caller {caller_id}: file not found in input directory"
                    )
                    continue

            # Create new ticket for the CALLER
            ticket_id = f"DOC-{uuid4().hex[:8].upper()}"

            metadata: dict[str, Any] = {
                "batch_id": self._state.batch_id,
                "source": "call_graph_gap",
                "discovery": False,  # We know the file exists
                "created_at": datetime.utcnow().isoformat(),
                "priority": "high",  # Gap-filling is high priority
                "missing_callees": sorted(missing_callees),  # Document what calls are missing
            }

            # Embed feedback context if available (IMPFB-002)
            if self._current_feedback_context is not None:
                metadata["feedback_context"] = self._current_feedback_context.model_dump()

            ticket = ProgramManagerTicket(
                ticket_id=ticket_id,
                file_name=file_name,
                program_id=caller_id,
                ticket_type=TicketType.DOCUMENTATION,
                state=TicketState.CREATED,
                cycle_number=self._state.cycle,
                metadata=metadata,
            )

            # Add to cache
            self.beads_client._pm_ticket_cache[ticket_id] = ticket
            created_tickets.append(ticket)
            logger.info(
                f"Created documentation ticket {ticket_id} for caller {caller_id} "
                f"(calls missing: {', '.join(sorted(missing_callees))})"
            )

        # Save to disk
        if created_tickets:
            self.beads_client._save_to_disk()
            logger.info(f"Created {len(created_tickets)} documentation tickets for call graph gaps")
        else:
            logger.info(
                f"No documentation tickets created - callers of missing programs "
                f"either already have tickets or their files don't exist"
            )

        return created_tickets

    async def _validate_cycle_complete(self, max_retries: int = 3, retry_count: int = 0) -> None:
        """Ensure all tickets are in terminal states before proceeding.

        This keeps retrying the worker cycle until all tickets are either
        COMPLETED or BLOCKED. This prevents cycles from advancing with
        incomplete work.

        Args:
            max_retries: Maximum number of retry attempts before forcing BLOCKED.
            retry_count: Current retry attempt (used internally for recursion).
        """
        non_terminal_states = [
            TicketState.CREATED,
            TicketState.CLAIMED,
            TicketState.IN_PROGRESS,
            TicketState.REWORK,
        ]

        # Collect all non-terminal tickets (excluding Imperator-handled types)
        # SYSTEM_OVERVIEW and HOLISTIC_REVIEW are processed by Imperator, not Scribe/Challenger
        imperator_ticket_types = {TicketType.SYSTEM_OVERVIEW, TicketType.HOLISTIC_REVIEW}
        stuck_tickets = []
        for state in non_terminal_states:
            tickets = self.beads_client.get_tickets_by_state(state)
            for ticket in tickets:
                if ticket.ticket_type not in imperator_ticket_types:
                    stuck_tickets.append(ticket)

        if not stuck_tickets:
            logger.debug("All tickets in terminal states - cycle complete")
            return

        # Check if we've exceeded max retries
        if retry_count >= max_retries:
            logger.warning(
                f"Max retries ({max_retries}) exceeded - forcing {len(stuck_tickets)} "
                f"ticket(s) to BLOCKED state"
            )
            for ticket in stuck_tickets:
                logger.warning(
                    f"  Forcing BLOCKED: {ticket.ticket_id} ({ticket.file_name}) "
                    f"was in {ticket.state.value} state after {max_retries} retries"
                )
                self.beads_client.update_ticket_state(
                    ticket.ticket_id,
                    TicketState.BLOCKED,
                    reason=f"Forced BLOCKED after {max_retries} retries at cycle {self._state.cycle}",
                )
            return

        # Reset stuck tickets to CREATED so workers can retry them
        logger.info(
            f"Found {len(stuck_tickets)} incomplete ticket(s) - "
            f"retry {retry_count + 1}/{max_retries}"
        )
        for ticket in stuck_tickets:
            logger.info(
                f"  Resetting for retry: {ticket.ticket_id} ({ticket.file_name}) "
                f"was in {ticket.state.value} state"
            )
            # Reset to CREATED for retry (unless already CREATED)
            if ticket.state != TicketState.CREATED:
                self.beads_client.update_ticket_state(
                    ticket.ticket_id,
                    TicketState.CREATED,
                    reason=f"Reset for retry {retry_count + 1} at cycle {self._state.cycle}",
                )

        # Run worker pools again to process remaining tickets
        await self._run_worker_pools_only()

        # Recursively validate (will check if all complete now)
        await self._validate_cycle_complete(max_retries=max_retries, retry_count=retry_count + 1)

    async def _run_worker_pools_only(self) -> None:
        """Run worker pools without the full cycle overhead.

        This is used for retry loops to process remaining tickets without
        triggering orphan resets or super-scribe rescue (which would cause
        infinite recursion).
        """
        logger.info("Running worker pools for retry...")

        # Create and run Scribe pool with file lock manager
        scribe_pool = ScribeWorkerPool(
            config=self.config,
            beads_client=self.beads_client,
            input_directory=self._input_directory,
            num_workers=self.config.num_scribes,
            poll_interval=2.0,
            idle_timeout=30.0,
            file_lock_manager=self._file_lock_manager,
            exit_on_error=self.config.exit_on_error,
        )

        # Create and run Challenger pool with file lock manager
        # Use is_done() to check if scribes are still active - this correctly
        # handles workers that stopped naturally due to idle timeout (not just
        # via explicit stop() call which sets _stopped)
        challenger_pool = ChallengerWorkerPool(
            num_workers=self.config.num_challengers,
            config=self.config,
            beads_client=self.beads_client,
            poll_interval=2.0,
            upstream_active_check=lambda: not scribe_pool.is_done(),
            file_lock_manager=self._file_lock_manager,
            exit_on_error=self.config.exit_on_error,
        )

        # Start both pools
        await scribe_pool.start()
        await challenger_pool.start()

        # Wait for completion
        scribe_wait = asyncio.create_task(scribe_pool.wait())
        challenger_wait = asyncio.create_task(challenger_pool.wait_for_completion())
        results = await asyncio.gather(scribe_wait, challenger_wait, return_exceptions=True)

        # Stop pools
        await scribe_pool.stop()
        await challenger_pool.stop()

        # Re-raise FatalWorkerError if any worker encountered a fatal error
        for result in results:
            if isinstance(result, FatalWorkerError):
                logger.error(f"Fatal worker error during retry: {result}")
                raise result

        logger.info("Retry worker pools complete")

    async def _run_super_scribe_rescue(self) -> None:
        """Automatically rescue BLOCKED tickets using a stronger model (Super-Scribe).

        After the normal worker cycle completes, this method checks for any tickets
        that ended up in BLOCKED state (all workers failed). If found, it:
        1. Increments retry_count in ticket metadata
        2. Checks against max_ticket_retries limit (raises MaxTicketRetriesExceeded if exceeded)
        3. Resets them to CREATED state
        4. Creates an Opus-powered ScribeWorkerPool with 1 worker
        5. Runs the pool to attempt rescue

        This provides automatic escalation without requiring a separate command.
        The rescue model and worker count are configured via:
        - config.super_scribe_model (default: anthropic/claude-opus-4-20250514)
        - config.num_super_scribes (default: 1)
        - config.max_ticket_retries (default: 5) - fatal exit threshold

        Raises:
            MaxTicketRetriesExceeded: If any ticket exceeds max_ticket_retries and
                exit_on_error is True.
        """
        # Ticket types that can be rescued (not VALIDATION - that's for Challenger)
        rescue_ticket_types = [
            TicketType.DOCUMENTATION,
            TicketType.CHROME,
            TicketType.CLARIFICATION,
        ]

        # Find BLOCKED tickets
        blocked_tickets = self.beads_client.get_tickets_by_state(TicketState.BLOCKED)
        blocked_tickets = [
            t for t in blocked_tickets if t.ticket_type in rescue_ticket_types
        ]

        if not blocked_tickets:
            return  # No blocked tickets to rescue

        logger.info(
            f"Super-Scribe is rescuing {len(blocked_tickets)} blocked ticket(s)"
        )

        # Track retry counts and check for exceeded limits
        max_retries = self.config.max_ticket_retries
        tickets_exceeding_limit: list[ProgramManagerTicket] = []

        for ticket in blocked_tickets:
            # Get current retry count from metadata (default 0)
            current_retries = ticket.metadata.get("retry_count", 0) + 1

            logger.info(
                f"  - {ticket.ticket_id}: {ticket.file_name} ({ticket.ticket_type.value}) "
                f"[retry {current_retries}/{max_retries}]"
            )

            # Check if this ticket has exceeded max retries
            if current_retries > max_retries:
                tickets_exceeding_limit.append(ticket)
                logger.error(
                    f"Ticket {ticket.ticket_id} ({ticket.file_name}) has exceeded "
                    f"max retry limit: {current_retries} > {max_retries}"
                )

        # If any tickets exceeded the limit and exit_on_error is enabled, fail immediately
        if tickets_exceeding_limit and self.config.exit_on_error:
            # Pick the first one to report (all will be in the log)
            ticket = tickets_exceeding_limit[0]
            retry_count = ticket.metadata.get("retry_count", 0) + 1
            raise MaxTicketRetriesExceeded(
                ticket_id=ticket.ticket_id,
                file_name=ticket.file_name,
                retry_count=retry_count,
                max_retries=max_retries,
            )

        # Reset blocked tickets to CREATED so rescue workers can pick them up
        # Also increment retry_count in metadata
        reset_count = 0
        for ticket in blocked_tickets:
            # Skip tickets that exceeded the limit (they stay blocked)
            if ticket in tickets_exceeding_limit:
                logger.warning(
                    f"Skipping ticket {ticket.ticket_id} - exceeded max retries"
                )
                continue

            current_retries = ticket.metadata.get("retry_count", 0) + 1
            # Build metadata updates with retry count and feedback context (IMPFB-005)
            metadata_updates: dict = {"retry_count": current_retries}
            if self._current_feedback_context is not None:
                metadata_updates["feedback_context"] = self._current_feedback_context.model_dump()
            success = self.beads_client.update_ticket_state(
                ticket.ticket_id,
                TicketState.CREATED,
                reason=f"Reset for Super-Scribe rescue (retry {current_retries}/{max_retries})",
                metadata_updates=metadata_updates,
            )
            if success:
                reset_count += 1
                logger.debug(
                    f"Reset ticket {ticket.ticket_id} to CREATED (retry {current_retries})"
                )
            else:
                logger.warning(f"Failed to reset ticket {ticket.ticket_id}")

        if reset_count == 0:
            logger.warning("No tickets were reset for rescue. Skipping Super-Scribe.")
            return

        # Create a modified config with rescue model override
        # This swaps out the scribe_model so ScribeWorkerPool uses Opus
        config_dict = self.config.model_dump()
        config_dict["scribe_model"] = self.config.super_scribe_model
        config_dict["num_scribes"] = self.config.num_super_scribes
        rescue_config = WarRigConfig(**config_dict)

        logger.info(
            f"Starting Super-Scribe rescue pool with {rescue_config.num_scribes} "
            f"worker(s) using {rescue_config.scribe_model}"
        )

        # Create and run rescue pool with file lock manager
        rescue_pool = ScribeWorkerPool(
            config=rescue_config,
            beads_client=self.beads_client,
            input_directory=self._input_directory,
            output_directory=self.config.output_directory,
            num_workers=rescue_config.num_scribes,
            poll_interval=2.0,
            idle_timeout=30.0,
            file_lock_manager=self._file_lock_manager,
            exit_on_error=self.config.exit_on_error,
        )

        try:
            await rescue_pool.start()
            await rescue_pool.wait()
        except FatalWorkerError:
            # Re-raise fatal errors to terminate the orchestrator
            raise
        finally:
            await rescue_pool.stop()

        # Get rescue results
        rescue_status = rescue_pool.get_status()
        total_processed = rescue_status.get("total_processed", 0)
        total_failed = rescue_status.get("total_failed", 0)

        logger.info(
            f"Super-Scribe rescue complete: {total_processed} processed, "
            f"{total_failed} failed"
        )

        # Check if any tickets are still blocked
        still_blocked = self.beads_client.get_tickets_by_state(TicketState.BLOCKED)
        still_blocked = [
            t for t in still_blocked if t.ticket_type in rescue_ticket_types
        ]
        if still_blocked:
            logger.warning(
                f"{len(still_blocked)} ticket(s) still blocked after Super-Scribe rescue:"
            )

            # Check for tickets that have now exceeded the retry limit after this rescue attempt
            tickets_now_exceeding_limit: list[ProgramManagerTicket] = []
            for ticket in still_blocked:
                retry_count = ticket.metadata.get("retry_count", 0)
                logger.warning(
                    f"  - {ticket.ticket_id}: {ticket.file_name} "
                    f"[retries: {retry_count}/{max_retries}]"
                )
                if retry_count >= max_retries:
                    tickets_now_exceeding_limit.append(ticket)

            # If any tickets now exceed limit and exit_on_error is enabled, fail
            if tickets_now_exceeding_limit and self.config.exit_on_error:
                ticket = tickets_now_exceeding_limit[0]
                retry_count = ticket.metadata.get("retry_count", 0)
                logger.error(
                    f"Ticket {ticket.ticket_id} ({ticket.file_name}) is still blocked "
                    f"after {retry_count} retries including Super-Scribe escalation. "
                    f"Exiting due to exit_on_error=True."
                )
                raise MaxTicketRetriesExceeded(
                    ticket_id=ticket.ticket_id,
                    file_name=ticket.file_name,
                    retry_count=retry_count,
                    max_retries=max_retries,
                )

        # Check for pending validation tickets created by rescue
        # These need to be processed by Challengers before holistic review
        pending_validations = self.beads_client.get_available_tickets(
            ticket_type=TicketType.VALIDATION
        )
        if pending_validations:
            logger.info(
                f"Running Challengers for {len(pending_validations)} validation tickets "
                "created during Super-Scribe rescue"
            )
            self._state.status = OrchestrationStatus.VALIDATING
            self._state.status_message = "Running post-rescue validation..."

            # Run Challenger pool for validation with file lock manager
            if self._challenger_pool is None:
                self._challenger_pool = ChallengerWorkerPool(
                    num_workers=self.config.num_challengers,
                    config=self.config,
                    beads_client=self.beads_client,
                    poll_interval=2.0,
                    file_lock_manager=self._file_lock_manager,
                    exit_on_error=self.config.exit_on_error,
                )

            await self._challenger_pool.start()
            await self._challenger_pool.wait_for_completion()

            logger.info("Post-rescue validation complete")

    async def _run_holistic_review(self) -> HolisticReviewOutput | None:
        """Trigger and wait for Imperator holistic review.

        Collects all completed documentation and submits it to the
        Imperator for batch-level review. Only runs when all Scribe
        and Challenger work is complete.

        Returns:
            HolisticReviewOutput with the review decision and feedback.
        """
        # Check if there's still pending work - Imperator should wait
        # Must check ALL ticket types that Scribes and Challengers process
        scribe_types = [
            TicketType.DOCUMENTATION,
            TicketType.CLARIFICATION,
            TicketType.CHROME,
        ]
        challenger_types = [TicketType.VALIDATION]

        # Collect pending/in-progress counts for all relevant ticket types
        pending_scribe = []
        in_progress_scribe = []
        for ticket_type in scribe_types:
            pending_scribe.extend(
                self.beads_client.get_tickets_by_state(
                    state=TicketState.CREATED,
                    ticket_type=ticket_type,
                )
            )
            in_progress_scribe.extend(
                self.beads_client.get_tickets_by_state(
                    state=TicketState.IN_PROGRESS,
                    ticket_type=ticket_type,
                )
            )

        pending_val = []
        in_progress_val = []
        for ticket_type in challenger_types:
            pending_val.extend(
                self.beads_client.get_tickets_by_state(
                    state=TicketState.CREATED,
                    ticket_type=ticket_type,
                )
            )
            in_progress_val.extend(
                self.beads_client.get_tickets_by_state(
                    state=TicketState.IN_PROGRESS,
                    ticket_type=ticket_type,
                )
            )

        if pending_scribe or in_progress_scribe or pending_val or in_progress_val:
            logger.info(
                f"Skipping holistic review - work still pending: "
                f"{len(pending_scribe)} scribe pending, {len(in_progress_scribe)} scribe in progress, "
                f"{len(pending_val)} val pending, {len(in_progress_val)} val in progress"
            )
            return HolisticReviewOutput(
                success=True,
                decision=ImperatorHolisticDecision.NEEDS_CLARIFICATION,
                quality_notes=["Work still in progress, review deferred"],
            )

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
                output_directory=self.config.output_directory,
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
        tickets) into beads tickets for the next cycle. Also builds
        FeedbackContext from review output to embed in subsequent tickets.

        Args:
            review_output: The holistic review output with feedback.
        """
        self._state.status = OrchestrationStatus.PROCESSING_FEEDBACK
        self._state.status_message = "Processing Imperator feedback..."

        # Build feedback context from review output for distribution to tickets
        self._current_feedback_context = self._build_feedback_context(review_output)
        logger.info(
            f"Built feedback context with {len(self._current_feedback_context.quality_notes)} "
            f"quality notes, {len(self._current_feedback_context.critical_sections)} critical sections"
        )

        # Build a lookup of file paths only - workers load content from disk to avoid bloat
        file_path_lookup: dict[str, str] = {}
        for ticket in self.beads_client._pm_ticket_cache.values():
            if ticket.file_name not in file_path_lookup:
                if ticket.metadata.get("file_path"):
                    file_path_lookup[ticket.file_name] = ticket.metadata["file_path"]

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
                    file_path=file_path_lookup.get(req.file_name),
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
                        file_path=file_path_lookup.get(file_name),
                    )
                )

        # Use Program Manager to create tickets with feedback context
        if clarification_requests:
            logger.info(f"Creating {len(clarification_requests)} clarification tickets")
            self.program_manager.handle_clarifications(
                clarification_requests,
                feedback_context=self._current_feedback_context,
            )

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

    def _build_feedback_context(
        self,
        review_output: HolisticReviewOutput,
    ) -> "FeedbackContext":
        """Build FeedbackContext from Imperator review output.

        Extracts quality notes, identifies critical sections, and
        captures file-specific issues for distribution to tickets.

        Args:
            review_output: The holistic review output.

        Returns:
            FeedbackContext with parsed quality notes and settings.
        """
        from war_rig.models.tickets import FeedbackContext, QualityNote

        # Use structured quality notes if available (IMPFB-003)
        quality_notes: list[QualityNote] = []
        if review_output.quality_notes_structured:
            # Use structured notes directly - they have proper category/severity
            for i, note_dict in enumerate(review_output.quality_notes_structured):
                try:
                    note = QualityNote(
                        note_id=f"QN-{i:03d}",
                        category=note_dict.get("category", "other"),
                        severity=note_dict.get("severity", "medium"),
                        description=note_dict.get("description", ""),
                        affected_sections=note_dict.get("affected_sections", []),
                        affected_files=note_dict.get("affected_files", []),
                        guidance=note_dict.get("guidance"),
                        cycle_identified=self._state.cycle,
                    )
                    quality_notes.append(note)
                except Exception as e:
                    logger.warning(f"Failed to parse structured quality note: {e}")
        else:
            # Fall back to parsing string notes (legacy format)
            for i, note_str in enumerate(review_output.quality_notes or []):
                note = self._parse_quality_note(note_str, i)
                quality_notes.append(note)

        # Determine critical sections from notes
        critical_sections = self._identify_critical_sections(quality_notes)

        # Build file-specific issues from file_feedback
        previous_issues: dict[str, list[str]] = {}
        for file_name, chrome_tickets in (review_output.file_feedback or {}).items():
            previous_issues[file_name] = [t.description for t in chrome_tickets]

        return FeedbackContext(
            quality_notes=quality_notes,
            critical_sections=critical_sections,
            required_citations=True,  # Always require citations
            cross_reference_required=any(
                "cross-reference" in n.description.lower() for n in quality_notes
            ),
            previous_cycle_issues=previous_issues,
            augment_existing=True,  # Per user requirement
        )

    def _parse_quality_note(self, note_str: str, index: int) -> "QualityNote":
        """Parse a quality note string into a QualityNote object.

        Infers category and severity from note content.

        Args:
            note_str: The raw quality note string from Imperator.
            index: Index for generating note ID.

        Returns:
            Parsed QualityNote with inferred category and severity.
        """
        from war_rig.models.tickets import QualityNote

        note_lower = note_str.lower()

        # Infer category
        if "empty" in note_lower or "blank" in note_lower:
            category = "empty_section"
            severity = "critical"
        elif "citation" in note_lower or "cite" in note_lower:
            category = "missing_citation"
            severity = "high"
        elif "cross-reference" in note_lower or "cross reference" in note_lower:
            category = "no_cross_reference"
            severity = "medium"
        elif "confidence" in note_lower:
            category = "confidence_mismatch"
            severity = "medium"
        elif "redundant" in note_lower or "duplicate" in note_lower:
            category = "redundant_doc"
            severity = "high"
        elif "vague" in note_lower or "generic" in note_lower:
            category = "vague_content"
            severity = "medium"
        else:
            category = "other"
            severity = "medium"

        # Extract affected sections from note
        affected_sections = []
        section_keywords = [
            "inputs", "outputs", "data_flow", "data flow", "copybooks",
            "sql_operations", "sql operations", "cics_operations", "cics operations",
            "business_rules", "business rules", "error_handling", "error handling",
            "purpose", "summary",
        ]
        for section in section_keywords:
            if section in note_lower:
                # Normalize to underscore format
                affected_sections.append(section.replace(" ", "_"))

        return QualityNote(
            note_id=f"QN-{index:03d}",
            category=category,
            severity=severity,
            description=note_str,
            affected_sections=affected_sections,
            cycle_identified=self._state.cycle,
        )

    def _identify_critical_sections(self, notes: list["QualityNote"]) -> list[str]:
        """Identify sections that must be populated based on quality notes.

        If a section was noted as empty or incomplete, it becomes critical.

        Args:
            notes: List of parsed quality notes.

        Returns:
            List of section names that must be populated.
        """
        # Default critical sections
        critical = {"purpose", "inputs", "outputs"}

        # Add sections from empty_section notes
        for note in notes:
            if note.category == "empty_section" and note.affected_sections:
                critical.update(note.affected_sections)

        return list(critical)

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

        # Count rework/chrome tickets completed
        completed_chrome = self.beads_client.get_tickets_by_state(
            state=TicketState.COMPLETED,
            ticket_type=TicketType.CHROME,
        )
        self._state.rework_files = len(completed_chrome)

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
            "rework_files": self._state.rework_files,
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

    # =========================================================================
    # System Overview Generation
    # =========================================================================

    def _create_system_overview_ticket(self) -> ProgramManagerTicket | None:
        """Create a SYSTEM_OVERVIEW ticket if one doesn't exist.

        Called after all documentation is complete. Creates a single ticket
        for generating the system overview markdown file.

        Returns:
            The created ticket, or None if one already exists.
        """
        # Check if overview ticket already exists
        existing = [
            t for t in self.beads_client._pm_ticket_cache.values()
            if t.ticket_type == TicketType.SYSTEM_OVERVIEW
        ]
        if existing:
            logger.debug(f"System overview ticket already exists: {existing[0].ticket_id}")
            return existing[0]

        # Create the ticket
        from uuid import uuid4

        ticket_id = f"OVW-{uuid4().hex[:8].upper()}"
        ticket = ProgramManagerTicket(
            ticket_id=ticket_id,
            file_name="SYSTEM_OVERVIEW.md",
            program_id="SYSTEM_OVERVIEW",
            ticket_type=TicketType.SYSTEM_OVERVIEW,
            state=TicketState.CREATED,
            cycle_number=self._state.cycle,  # Use current orchestrator cycle
            metadata={
                "batch_id": self._state.batch_id,
                "system_name": "CardDemo",  # TODO: Make configurable
                "created_at": datetime.utcnow().isoformat(),
            },
        )

        # Add to cache
        self.beads_client._pm_ticket_cache[ticket_id] = ticket
        self.beads_client._save_to_disk()

        logger.info(f"Created SYSTEM_OVERVIEW ticket: {ticket_id}")
        return ticket

    async def _process_system_overview(self) -> SystemOverviewOutput | None:
        """Process the SYSTEM_OVERVIEW ticket by generating the overview document.

        Reads all completed documentation from the output directory,
        builds program summaries, and calls the Imperator to generate
        the system overview markdown.

        Returns:
            SystemOverviewOutput if successful, None otherwise.
        """
        import json

        # Find the overview ticket
        overview_tickets = [
            t for t in self.beads_client._pm_ticket_cache.values()
            if t.ticket_type == TicketType.SYSTEM_OVERVIEW
        ]

        if not overview_tickets:
            logger.warning("No SYSTEM_OVERVIEW ticket found")
            return None

        ticket = overview_tickets[0]

        # If already completed, skip
        if ticket.state == TicketState.COMPLETED:
            logger.info("SYSTEM_OVERVIEW ticket already completed")
            return None

        # Mark as in progress
        self.beads_client.update_ticket_state(
            ticket.ticket_id,
            TicketState.IN_PROGRESS,
            reason="Generating system overview (imperator)",
        )

        logger.info("Generating system overview from completed documentation...")

        # Collect program summaries from completed documentation
        programs: list[ProgramSummary] = []

        # Check multiple locations for documentation files
        # Priority: final/programs > final > root output directory
        doc_dirs = [
            self.config.output_directory / "final" / "programs",
            self.config.output_directory / "final",
            self.config.output_directory,  # Scribe saves here
        ]

        doc_files: list[Path] = []
        for doc_dir in doc_dirs:
            if doc_dir.exists():
                found = list(doc_dir.glob("*.doc.json"))
                if found:
                    doc_files = found
                    logger.debug(f"Found {len(found)} documentation files in {doc_dir}")
                    break

        for doc_file in doc_files:
                try:
                    doc_data = json.loads(doc_file.read_text(encoding="utf-8"))

                    # Extract relevant fields
                    header = doc_data.get("header", {})
                    purpose = doc_data.get("purpose", {})
                    called_programs = doc_data.get("called_programs", [])
                    inputs = doc_data.get("inputs", [])
                    outputs = doc_data.get("outputs", [])

                    # Build call lists
                    calls = [cp.get("program_name", "") for cp in called_programs if cp.get("program_name")]
                    input_names = [i.get("name", "") for i in inputs if i.get("name")][:5]
                    output_names = [o.get("name", "") for o in outputs if o.get("name")][:5]

                    programs.append(ProgramSummary(
                        file_name=header.get("file_name") or doc_file.stem,
                        program_id=header.get("program_id") or doc_file.stem.replace(".doc", ""),
                        program_type=purpose.get("program_type") or "UNKNOWN",
                        summary=purpose.get("summary") or "No summary available",
                        business_context=purpose.get("business_context") or "",
                        calls=calls,
                        called_by=[],  # Would need cross-reference to populate
                        inputs=input_names,
                        outputs=output_names,
                        json_path=f"./programs/{doc_file.name}",
                    ))

                except Exception as e:
                    logger.warning(f"Failed to read documentation {doc_file}: {e}")

        if not programs:
            logger.warning("No completed documentation found for system overview")
            self.beads_client.update_ticket_state(
                ticket.ticket_id,
                TicketState.BLOCKED,
                reason="No completed documentation found",
            )
            return None

        logger.info(f"Found {len(programs)} documented programs for overview")

        # Build cross-references (called_by)
        program_ids = {p.program_id for p in programs}
        for prog in programs:
            for called in prog.calls:
                # Find the called program and add this one to its called_by
                for other in programs:
                    if other.program_id == called:
                        if prog.program_id not in other.called_by:
                            other.called_by.append(prog.program_id)

        # Build input
        system_name = ticket.metadata.get("system_name", "CardDemo")
        input_data = SystemOverviewInput(
            batch_id=self._state.batch_id,
            system_name=system_name,
            programs=programs,
            total_files=len(programs),
        )

        # Generate overview
        try:
            output = await self.imperator.generate_system_overview(
                input_data,
                use_mock=self.use_mock,
            )

            if output.success:
                # Write the markdown file
                overview_path = self.config.output_directory / "SYSTEM_OVERVIEW.md"
                overview_path.write_text(output.markdown, encoding="utf-8")
                logger.info(f"Wrote system overview to {overview_path}")

                # Mark ticket as completed
                self.beads_client.update_ticket_state(
                    ticket.ticket_id,
                    TicketState.COMPLETED,
                    reason="System overview generated",
                    decision="WITNESSED",
                )

                return output
            else:
                logger.error(f"System overview generation failed: {output.error}")
                self.beads_client.update_ticket_state(
                    ticket.ticket_id,
                    TicketState.BLOCKED,
                    reason=f"Generation failed: {output.error}",
                )
                return None

        except Exception as e:
            logger.error(f"System overview generation failed: {e}")
            self.beads_client.update_ticket_state(
                ticket.ticket_id,
                TicketState.BLOCKED,
                reason=f"Generation error: {e}",
            )
            return None

    async def stop(self) -> None:
        """Gracefully stop the orchestrator.

        Signals the orchestrator to stop processing after the current
        operation completes. Worker pools are stopped and resources
        are cleaned up. Any orphaned tickets are reset for future retry.

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

        # Recover any orphaned tickets for future retry
        orphaned_count = self.beads_client.reset_orphaned_tickets()
        if orphaned_count > 0:
            logger.info(f"Reset {orphaned_count} orphaned tickets for future retry")
