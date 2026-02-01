"""Minion Scribe pool for parallel call semantics processing.

This module provides the MinionScribePool class which manages a pool of
lightweight LLM workers (using fast, cheap models like Haiku) to process
call semantics analysis in parallel.

The pool:
1. Takes a file with its Citadel context and working storage
2. Extracts call edges (caller -> callee pairs)
3. Distributes batches of edges across N parallel workers
4. Each worker uses CallSemanticsAnalyzer with a minion model
5. Aggregates results and returns all CallSemantics
6. Creates sub-tickets for each batch to enable tracking and retry

This provides significant speedup for files with many paragraphs by
parallelizing what was previously sequential LLM calls.

Example:
    from war_rig.workers.minion_scribe_pool import MinionScribePool
    from war_rig.config import load_config
    from war_rig.beads import BeadsClient

    config = load_config()
    beads_client = BeadsClient()
    pool = MinionScribePool(config, beads_client=beads_client)

    semantics = await pool.analyze_file(
        source_path=Path("PROGRAM.cbl"),
        citadel_context=functions_list,
        working_storage=ws_text,
        parent_ticket_id="mem-000001",
    )
"""

from __future__ import annotations

import asyncio
import logging
import time
from dataclasses import dataclass
from pathlib import Path
from typing import TYPE_CHECKING, Any

from war_rig.analysis.call_semantics import CallSemanticsAnalyzer
from war_rig.config import WarRigConfig
from war_rig.models.templates import CallSemantics
from war_rig.providers import LLMProvider, get_provider_from_env

if TYPE_CHECKING:
    from war_rig.beads import BeadsClient, ProgramManagerTicket

logger = logging.getLogger(__name__)


@dataclass
class BatchResult:
    """Result of processing a single batch.

    Attributes:
        batch_index: Index of the batch (0-based).
        semantics: List of CallSemantics for this batch.
        success: Whether the batch was processed successfully.
        error: Error message if processing failed.
        duration_seconds: Processing time in seconds.
        tokens_used: LLM tokens consumed (if available).
        sub_ticket_id: ID of the sub-ticket for this batch.
    """

    batch_index: int
    semantics: list[CallSemantics]
    success: bool
    error: str | None = None
    duration_seconds: float = 0.0
    tokens_used: int = 0
    sub_ticket_id: str | None = None


class MinionScribePool:
    """Pool of minion scribes for parallel call semantics analysis.

    This class manages a pool of workers that process call edge batches
    in parallel using fast, cheap LLM models. It distributes work across
    multiple concurrent workers to speed up call semantics inference.

    When a BeadsClient is provided, the pool creates sub-tickets for each
    batch to enable tracking and retry of failed batches.

    Attributes:
        config: War Rig configuration.
        num_workers: Number of parallel workers.
        batch_size: Number of call edges per LLM request.
        model: Model identifier for minion scribes.
        beads_client: Optional client for ticket operations.
    """

    def __init__(
        self,
        config: WarRigConfig,
        provider: LLMProvider | None = None,
        beads_client: BeadsClient | None = None,
    ):
        """Initialize the minion scribe pool.

        Args:
            config: War Rig configuration containing minion scribe settings.
            provider: Optional LLM provider instance. If not provided,
                creates one from environment variables.
            beads_client: Optional client for creating/managing sub-tickets.
                When provided, creates CALL_SEMANTICS sub-tickets for each
                batch with full tracking and retry capability.
        """
        self.config = config
        self.num_workers = config.num_minion_scribes
        self.batch_size = config.minion_scribe_batch_size
        self.model = config.minion_scribe_model
        self._provider = provider or get_provider_from_env()
        self._beads_client = beads_client

        logger.debug(
            f"MinionScribePool initialized: {self.num_workers} workers, "
            f"batch_size={self.batch_size}, model={self.model}, "
            f"beads_client={'enabled' if beads_client else 'disabled'}"
        )

    @property
    def provider(self) -> LLMProvider:
        """Get the LLM provider instance."""
        return self._provider

    @property
    def beads_client(self) -> BeadsClient | None:
        """Get the BeadsClient instance."""
        return self._beads_client

    async def analyze_file(
        self,
        source_path: Path,
        citadel_context: list[dict[str, Any]],
        working_storage: str | None = None,
        parent_ticket_id: str | None = None,
    ) -> list[CallSemantics]:
        """Analyze all calls in a file using parallel workers.

        Extracts call edges from Citadel context, distributes them across
        worker batches, processes in parallel, and aggregates results.

        When a parent_ticket_id is provided and beads_client is configured,
        creates CALL_SEMANTICS sub-tickets for each batch with tracking and
        retry capability.

        Args:
            source_path: Path to the COBOL source file.
            citadel_context: Context from Citadel with functions list.
                Each function dict should have: name, type, line, line_end, calls.
            working_storage: Optional DATA DIVISION text for context.
            parent_ticket_id: Optional parent DOCUMENTATION ticket ID.
                When provided with beads_client, creates sub-tickets for
                each batch.

        Returns:
            List of CallSemantics for each call edge.
        """
        if not citadel_context:
            logger.debug("No citadel context provided, returning empty semantics")
            return []

        # Extract call edges from citadel context
        call_edges = self._extract_call_edges(citadel_context)

        if not call_edges:
            logger.debug("No call edges found in citadel context")
            return []

        logger.info(
            f"MinionScribePool: Found {len(call_edges)} call edges in {source_path.name}"
        )

        # Split edges into batches
        batches = self._create_batches(call_edges)
        total_batches = len(batches)

        if total_batches == 0:
            return []

        logger.info(
            f"MinionScribePool: Processing {total_batches} batches with "
            f"{self.num_workers} parallel workers"
        )

        # Create a queue of work items
        work_queue: asyncio.Queue[tuple[int, list[tuple[str, str]]]] = asyncio.Queue()
        for i, batch in enumerate(batches):
            await work_queue.put((i, batch))

        # Results storage
        batch_results: list[BatchResult] = [
            BatchResult(batch_index=i, semantics=[], success=False)
            for i in range(total_batches)
        ]

        # Create worker tasks
        workers = [
            asyncio.create_task(
                self._worker(
                    worker_id=i + 1,
                    work_queue=work_queue,
                    batch_results=batch_results,
                    source_path=source_path,
                    citadel_context=citadel_context,
                    working_storage=working_storage,
                    total_batches=total_batches,
                    parent_ticket_id=parent_ticket_id,
                )
            )
            for i in range(min(self.num_workers, total_batches))
        ]

        # Wait for all workers to complete
        await asyncio.gather(*workers, return_exceptions=True)

        # Log any errors
        errors = [r for r in batch_results if not r.success]
        if errors:
            logger.warning(
                f"MinionScribePool: {len(errors)} batch(es) had errors: "
                f"{'; '.join(r.error or 'Unknown error' for r in errors[:3])}"
            )

        # Flatten results
        all_semantics: list[CallSemantics] = []
        for result in batch_results:
            all_semantics.extend(result.semantics)

        logger.info(
            f"MinionScribePool: Completed analysis with "
            f"{len(all_semantics)} call semantics"
        )

        return all_semantics

    def get_pending_batches(
        self,
        parent_ticket_id: str,
    ) -> list[ProgramManagerTicket]:
        """Get pending or failed sub-tickets for retry.

        Queries the beads_client for CALL_SEMANTICS sub-tickets that are
        in CREATED or BLOCKED state (failed batches that can be retried).

        Args:
            parent_ticket_id: ID of the parent DOCUMENTATION ticket.

        Returns:
            List of ProgramManagerTicket for pending/failed batches.
            Empty list if beads_client is not configured.
        """
        if not self._beads_client:
            return []

        from war_rig.beads import TicketState, TicketType

        # Get all tickets for this parent
        all_tickets = list(self._beads_client._pm_ticket_cache.values())

        # Filter for CALL_SEMANTICS sub-tickets of this parent
        # that are in a retriable state
        pending = [
            t
            for t in all_tickets
            if t.ticket_type == TicketType.CALL_SEMANTICS
            and t.parent_ticket_id == parent_ticket_id
            and t.state in (TicketState.CREATED, TicketState.BLOCKED)
        ]

        return pending

    def _create_sub_ticket(
        self,
        parent_ticket_id: str,
        batch_index: int,
        total_batches: int,
        call_edges: list[tuple[str, str]],
        file_name: str,
    ) -> str | None:
        """Create a CALL_SEMANTICS sub-ticket for a batch.

        Args:
            parent_ticket_id: ID of the parent DOCUMENTATION ticket.
            batch_index: Index of the batch (0-based).
            total_batches: Total number of batches.
            call_edges: List of (caller, callee) tuples in this batch.
            file_name: Name of the source file being processed.

        Returns:
            Ticket ID if created successfully, None otherwise.
        """
        if not self._beads_client:
            return None

        from war_rig.beads import BeadsPriority, TicketType

        try:
            ticket = self._beads_client.create_pm_ticket(
                ticket_type=TicketType.CALL_SEMANTICS,
                file_name=file_name,
                parent_ticket_id=parent_ticket_id,
                priority=BeadsPriority.LOW,
                metadata={
                    "batch_index": batch_index,
                    "total_batches": total_batches,
                    "call_edges": call_edges,
                    "model": self.model,
                },
            )
            if ticket:
                logger.debug(
                    f"Created CALL_SEMANTICS sub-ticket {ticket.ticket_id} "
                    f"for batch {batch_index + 1}/{total_batches}"
                )
                return ticket.ticket_id
        except Exception as e:
            logger.warning(f"Failed to create sub-ticket for batch {batch_index}: {e}")

        return None

    def _update_sub_ticket_success(
        self,
        sub_ticket_id: str,
        result: BatchResult,
    ) -> None:
        """Update a sub-ticket with successful results and close it.

        Args:
            sub_ticket_id: ID of the sub-ticket to update.
            result: The batch result with semantics and metadata.
        """
        if not self._beads_client:
            return

        from war_rig.beads import TicketState

        try:
            # Update with result metadata and close
            self._beads_client.update_ticket_state(
                sub_ticket_id,
                TicketState.COMPLETED,
                reason=f"Processed {len(result.semantics)} call semantics",
                metadata_updates={
                    "result": [s.model_dump() for s in result.semantics],
                    "duration_seconds": result.duration_seconds,
                    "tokens_used": result.tokens_used,
                },
            )
            logger.debug(f"Completed sub-ticket {sub_ticket_id}")
        except Exception as e:
            logger.warning(f"Failed to update sub-ticket {sub_ticket_id}: {e}")

    def _update_sub_ticket_failure(
        self,
        sub_ticket_id: str,
        error: str,
        duration_seconds: float,
    ) -> None:
        """Update a sub-ticket with failure details (leave open for retry).

        Args:
            sub_ticket_id: ID of the sub-ticket to update.
            error: Error message describing the failure.
            duration_seconds: Processing time before failure.
        """
        if not self._beads_client:
            return

        from war_rig.beads import TicketState

        try:
            # Update with error and set to BLOCKED (available for retry)
            self._beads_client.update_ticket_state(
                sub_ticket_id,
                TicketState.BLOCKED,
                reason=f"Batch failed: {error}",
                metadata_updates={
                    "error": error,
                    "duration_seconds": duration_seconds,
                },
            )
            logger.debug(f"Marked sub-ticket {sub_ticket_id} as BLOCKED")
        except Exception as e:
            logger.warning(f"Failed to update sub-ticket {sub_ticket_id}: {e}")

    async def _worker(
        self,
        worker_id: int,
        work_queue: asyncio.Queue[tuple[int, list[tuple[str, str]]]],
        batch_results: list[BatchResult],
        source_path: Path,
        citadel_context: list[dict[str, Any]],
        working_storage: str | None,
        total_batches: int,
        parent_ticket_id: str | None,
    ) -> None:
        """Worker coroutine that processes batches from the queue.

        Args:
            worker_id: Worker identifier for logging.
            work_queue: Queue of (batch_index, call_edges) tuples.
            batch_results: List to store results indexed by batch index.
            source_path: Path to the source file.
            citadel_context: Citadel context for function bodies.
            working_storage: Optional working storage text.
            total_batches: Total number of batches for logging.
            parent_ticket_id: Optional parent ticket ID for sub-tickets.
        """
        # Create analyzer for this worker with minion model
        analyzer = CallSemanticsAnalyzer(
            api_config=self.config.api,
            model=self.model,
            provider=self._provider,
            batch_size=self.batch_size,
        )

        while True:
            try:
                # Non-blocking get with immediate check
                batch_index, batch = work_queue.get_nowait()
            except asyncio.QueueEmpty:
                # No more work
                break

            # Create sub-ticket if beads_client and parent_ticket_id are available
            sub_ticket_id: str | None = None
            if self._beads_client and parent_ticket_id:
                sub_ticket_id = self._create_sub_ticket(
                    parent_ticket_id=parent_ticket_id,
                    batch_index=batch_index,
                    total_batches=total_batches,
                    call_edges=batch,
                    file_name=source_path.name,
                )

            start_time = time.time()
            try:
                logger.info(
                    f"MinionScribe-{worker_id}: Processing batch "
                    f"{batch_index + 1}/{total_batches} ({len(batch)} edges)"
                    + (f" [sub-ticket: {sub_ticket_id}]" if sub_ticket_id else "")
                )

                # Process this batch using the analyzer
                batch_semantics = await self._process_batch(
                    analyzer=analyzer,
                    batch=batch,
                    source_path=source_path,
                    citadel_context=citadel_context,
                    working_storage=working_storage,
                )

                duration = time.time() - start_time

                # Store results
                result = BatchResult(
                    batch_index=batch_index,
                    semantics=batch_semantics,
                    success=True,
                    duration_seconds=duration,
                    sub_ticket_id=sub_ticket_id,
                )
                batch_results[batch_index] = result

                logger.debug(
                    f"MinionScribe-{worker_id}: Completed batch {batch_index + 1} "
                    f"with {len(batch_semantics)} semantics in {duration:.2f}s"
                )

                # Update sub-ticket with success
                if sub_ticket_id:
                    self._update_sub_ticket_success(sub_ticket_id, result)

            except Exception as e:
                duration = time.time() - start_time
                error_msg = f"Batch {batch_index + 1} failed: {e}"
                logger.warning(f"MinionScribe-{worker_id}: {error_msg}")

                # Return empty semantics for failed batch
                result = BatchResult(
                    batch_index=batch_index,
                    semantics=self._empty_semantics_for_batch(batch),
                    success=False,
                    error=str(e),
                    duration_seconds=duration,
                    sub_ticket_id=sub_ticket_id,
                )
                batch_results[batch_index] = result

                # Update sub-ticket with failure
                if sub_ticket_id:
                    self._update_sub_ticket_failure(sub_ticket_id, str(e), duration)

            finally:
                work_queue.task_done()

    async def _process_batch(
        self,
        analyzer: CallSemanticsAnalyzer,
        batch: list[tuple[str, str]],
        source_path: Path,
        citadel_context: list[dict[str, Any]],
        working_storage: str | None,
    ) -> list[CallSemantics]:
        """Process a single batch of call edges.

        Args:
            analyzer: CallSemanticsAnalyzer instance to use.
            batch: List of (caller, callee) tuples.
            source_path: Path to the source file.
            citadel_context: Citadel context (used for function bodies).
            working_storage: Optional working storage text.

        Returns:
            List of CallSemantics for this batch.
        """
        # Get function bodies for this batch
        function_names = set()
        for caller, callee in batch:
            function_names.add(caller)
            function_names.add(callee)

        bodies = analyzer._get_function_bodies(source_path, list(function_names))

        # Truncate working storage if needed
        ws_context = analyzer._truncate_working_storage(working_storage)

        # Analyze the batch
        return await analyzer._analyze_batch(
            batch=batch,
            bodies=bodies,
            working_storage=ws_context,
            file_name=source_path.name,
        )

    def _extract_call_edges(
        self, citadel_context: list[dict[str, Any]]
    ) -> list[tuple[str, str]]:
        """Extract caller-callee pairs from Citadel function data.

        Args:
            citadel_context: List of function dicts from Citadel's get_functions().

        Returns:
            List of (caller_name, callee_name) tuples.
        """
        edges: list[tuple[str, str]] = []

        for func in citadel_context:
            caller_name = func.get("name", "")
            if not caller_name:
                continue

            calls = func.get("calls", [])
            for call in calls:
                callee_name = call.get("target", "")
                call_type = call.get("type", "").lower()

                # Only consider PERFORM calls for paragraph-to-paragraph semantics
                # Exclude CALL (external programs), includes, etc.
                if callee_name and call_type in ("performs", "perform", ""):
                    edges.append((caller_name, callee_name))

        return edges

    def _create_batches(
        self, call_edges: list[tuple[str, str]]
    ) -> list[list[tuple[str, str]]]:
        """Split call edges into batches for parallel processing.

        Args:
            call_edges: List of (caller, callee) tuples.

        Returns:
            List of batches, each containing up to batch_size edges.
        """
        batches: list[list[tuple[str, str]]] = []

        for i in range(0, len(call_edges), self.batch_size):
            batch = call_edges[i : i + self.batch_size]
            batches.append(batch)

        return batches

    def _empty_semantics_for_batch(
        self, batch: list[tuple[str, str]]
    ) -> list[CallSemantics]:
        """Create empty CallSemantics for a failed batch.

        Args:
            batch: List of (caller, callee) tuples.

        Returns:
            List of CallSemantics with empty inputs/outputs.
        """
        return [
            CallSemantics(
                caller=caller,
                callee=callee,
                inputs=[],
                outputs=[],
                purpose=None,
            )
            for caller, callee in batch
        ]
