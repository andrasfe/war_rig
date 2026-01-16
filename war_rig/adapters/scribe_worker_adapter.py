"""Worker adapter to integrate War Rig's ScribeAgent with Atlas orchestration.

This adapter implements Atlas's Worker interface, enabling Atlas to
dispatch chunk analysis work to War Rig's existing Scribe agent.

The adapter:
- Receives DOC_CHUNK work items from Atlas
- Extracts the relevant source code slice from artifacts
- Delegates to ScribeAgent for documentation generation
- Writes results back to the artifact store
"""

from __future__ import annotations

import json
import logging
from typing import TYPE_CHECKING, Any

from atlas.adapters.artifact_store import ArtifactStoreAdapter
from atlas.adapters.ticket_system import TicketSystemAdapter
from atlas.models.enums import WorkItemType
from atlas.models.work_item import DocChunkPayload, WorkItem
from atlas.workers.base import Worker

from war_rig.agents.scribe import ScribeAgent, ScribeInput, ScribeOutput
from war_rig.config import APIConfig, ScribeConfig
from war_rig.models.templates import FileType

if TYPE_CHECKING:
    from war_rig.preprocessors.base import PreprocessorResult

logger = logging.getLogger(__name__)


class ScribeWorkerAdapter(Worker):
    """Adapts War Rig's ScribeAgent to Atlas's Worker interface.

    This worker handles DOC_CHUNK work items by:
    1. Reading the source artifact slice defined by chunk_locator
    2. Invoking ScribeAgent to analyze the code
    3. Writing the documentation result to result_uri

    The adapter bridges Atlas's orchestration with War Rig's existing
    agent infrastructure.

    Attributes:
        scribe: The War Rig ScribeAgent instance.
        file_type_map: Maps artifact types to FileType enum.

    Example:
        >>> scribe = ScribeAgent(scribe_config, api_config)
        >>> worker = ScribeWorkerAdapter(
        ...     worker_id="scribe-1",
        ...     ticket_system=beads_adapter,
        ...     artifact_store=file_adapter,
        ...     scribe=scribe,
        ... )
        >>> await worker.poll_and_process(limit=5)
    """

    def __init__(
        self,
        worker_id: str,
        ticket_system: TicketSystemAdapter,
        artifact_store: ArtifactStoreAdapter,
        scribe: ScribeAgent,
    ) -> None:
        """Initialize the worker adapter.

        Args:
            worker_id: Unique identifier for this worker.
            ticket_system: Ticket system adapter (BeadsTicketAdapter).
            artifact_store: Artifact storage adapter (FileArtifactAdapter).
            scribe: War Rig's ScribeAgent instance.
        """
        super().__init__(worker_id, ticket_system, artifact_store)
        self.scribe = scribe

        # Map artifact types to War Rig FileType
        self.file_type_map: dict[str, FileType] = {
            "cobol": FileType.COBOL,
            "copybook": FileType.COPYBOOK,
            "jcl": FileType.JCL,
            "pli": FileType.PLI,
            "bms": FileType.BMS,
            "other": FileType.OTHER,
        }

    @property
    def supported_work_types(self) -> list[WorkItemType]:
        """Get supported work types.

        Returns:
            List containing DOC_CHUNK - this worker handles chunk analysis.
        """
        return [WorkItemType.DOC_CHUNK]

    async def process(self, work_item: WorkItem) -> dict[str, Any]:
        """Process a DOC_CHUNK work item.

        Reads the source chunk, invokes ScribeAgent, and writes results.

        Args:
            work_item: The chunk work item to process.

        Returns:
            Dictionary with processing results.

        Raises:
            ValueError: If payload is not DocChunkPayload.
            RuntimeError: If processing fails.
        """
        payload = work_item.payload
        if not isinstance(payload, DocChunkPayload):
            raise ValueError(
                f"Expected DocChunkPayload, got {type(payload).__name__}"
            )

        logger.info(
            f"Processing chunk {payload.chunk_id} for job {payload.job_id}"
        )

        # Read source artifact
        if not payload.artifact_ref:
            raise ValueError("Work item missing artifact_ref")

        source_content = await self.artifact_store.read(
            payload.artifact_ref.artifact_uri
        )
        source_text = source_content.decode("utf-8")

        # Extract chunk slice based on locator
        chunk_text = self._extract_chunk(
            source_text,
            payload.chunk_locator.start_line,
            payload.chunk_locator.end_line,
        )

        # Determine file type
        artifact_type = payload.artifact_ref.artifact_type.lower()
        file_type = self.file_type_map.get(artifact_type, FileType.OTHER)

        # Build ScribeInput
        scribe_input = ScribeInput(
            source_code=chunk_text,
            file_name=payload.artifact_ref.artifact_id,
            file_type=file_type,
            iteration=work_item.cycle_number,
        )

        # Invoke ScribeAgent
        scribe_output = await self.scribe.ainvoke(scribe_input)

        # Build result
        result = self._build_result(work_item, scribe_output)

        # Write result to artifact store
        await self.artifact_store.write_json(
            payload.result_uri,
            result,
            metadata={
                "chunk_id": payload.chunk_id,
                "job_id": payload.job_id,
                "work_id": work_item.work_id,
            },
        )

        logger.info(
            f"Chunk {payload.chunk_id} processed successfully, "
            f"result written to {payload.result_uri}"
        )

        return result

    def _extract_chunk(
        self,
        source: str,
        start_line: int | None,
        end_line: int | None,
    ) -> str:
        """Extract a slice of source code by line numbers.

        Args:
            source: Full source code text.
            start_line: Starting line (1-indexed, inclusive).
            end_line: Ending line (1-indexed, inclusive).

        Returns:
            Extracted source code slice.
        """
        lines = source.split("\n")

        # Default to full source if no locator
        if start_line is None and end_line is None:
            return source

        # Adjust for 1-indexed lines
        start_idx = (start_line or 1) - 1
        end_idx = end_line or len(lines)

        # Extract slice
        chunk_lines = lines[start_idx:end_idx]
        return "\n".join(chunk_lines)

    def _build_result(
        self,
        work_item: WorkItem,
        scribe_output: ScribeOutput,
    ) -> dict[str, Any]:
        """Build the chunk result dictionary.

        Args:
            work_item: The processed work item.
            scribe_output: ScribeAgent output.

        Returns:
            Result dictionary for artifact storage.
        """
        payload = work_item.payload

        result: dict[str, Any] = {
            "chunk_id": getattr(payload, "chunk_id", None),
            "work_id": work_item.work_id,
            "job_id": payload.job_id,
            "cycle_number": work_item.cycle_number,
            "success": scribe_output.success,
        }

        if scribe_output.success:
            if scribe_output.template:
                result["template"] = scribe_output.template.model_dump()
            if scribe_output.confidence:
                result["confidence"] = scribe_output.confidence.model_dump()
            result["responses"] = [
                r.model_dump() for r in scribe_output.responses
            ]
            result["open_questions"] = scribe_output.open_questions
        else:
            result["error"] = scribe_output.error

        return result

    async def _output_exists(self, work_item: WorkItem) -> bool:
        """Check if output already exists for idempotency.

        Args:
            work_item: The work item to check.

        Returns:
            True if valid output exists at result_uri.
        """
        payload = work_item.payload
        if not isinstance(payload, DocChunkPayload):
            return False

        result_uri = payload.result_uri
        if not result_uri:
            return False

        # Check if result artifact exists
        if await self.artifact_store.exists(result_uri):
            try:
                # Verify it's valid JSON with success marker
                result = await self.artifact_store.read_json(result_uri)
                return result.get("success", False)
            except Exception:
                return False

        return False


def create_scribe_worker(
    worker_id: str,
    ticket_system: TicketSystemAdapter,
    artifact_store: ArtifactStoreAdapter,
    scribe_config: ScribeConfig,
    api_config: APIConfig | None = None,
) -> ScribeWorkerAdapter:
    """Factory function to create a ScribeWorkerAdapter.

    Convenience function that creates both the ScribeAgent and
    the worker adapter in one call.

    Args:
        worker_id: Unique identifier for this worker.
        ticket_system: Ticket system adapter.
        artifact_store: Artifact storage adapter.
        scribe_config: Configuration for the ScribeAgent.
        api_config: API configuration (optional).

    Returns:
        Configured ScribeWorkerAdapter instance.
    """
    scribe = ScribeAgent(scribe_config, api_config)

    return ScribeWorkerAdapter(
        worker_id=worker_id,
        ticket_system=ticket_system,
        artifact_store=artifact_store,
        scribe=scribe,
    )
