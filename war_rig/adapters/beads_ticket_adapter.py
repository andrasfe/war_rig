"""Adapter to make BeadsClient compatible with Atlas TicketSystemAdapter.

This adapter bridges War Rig's BeadsClient ticket system to Atlas's
abstract TicketSystemAdapter interface, enabling Atlas orchestration
to use War Rig's existing ticket infrastructure.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

from atlas.adapters.ticket_system import TicketSystemAdapter
from atlas.models.enums import WorkItemStatus, WorkItemType
from atlas.models.work_item import WorkItem, WorkItemPayload

from war_rig.beads import (
    BeadsClient,
    ProgramManagerTicket,
    TicketState,
    TicketType,
)

if TYPE_CHECKING:
    from collections.abc import AsyncIterator


# Mapping between Atlas WorkItemStatus and War Rig TicketState
ATLAS_TO_BEADS_STATUS: dict[WorkItemStatus, TicketState] = {
    WorkItemStatus.NEW: TicketState.CREATED,
    WorkItemStatus.READY: TicketState.CREATED,
    WorkItemStatus.IN_PROGRESS: TicketState.IN_PROGRESS,
    WorkItemStatus.BLOCKED: TicketState.BLOCKED,
    WorkItemStatus.DONE: TicketState.COMPLETED,
    WorkItemStatus.FAILED: TicketState.BLOCKED,
    WorkItemStatus.CANCELED: TicketState.CANCELLED,
}

BEADS_TO_ATLAS_STATUS: dict[TicketState, WorkItemStatus] = {
    TicketState.CREATED: WorkItemStatus.READY,
    TicketState.CLAIMED: WorkItemStatus.IN_PROGRESS,
    TicketState.IN_PROGRESS: WorkItemStatus.IN_PROGRESS,
    TicketState.COMPLETED: WorkItemStatus.DONE,
    TicketState.REWORK: WorkItemStatus.READY,
    TicketState.BLOCKED: WorkItemStatus.BLOCKED,
    TicketState.CANCELLED: WorkItemStatus.CANCELED,
    TicketState.MERGED: WorkItemStatus.DONE,
}

# Mapping between Atlas WorkItemType and War Rig TicketType
ATLAS_TO_BEADS_TYPE: dict[WorkItemType, TicketType] = {
    WorkItemType.DOC_REQUEST: TicketType.DOCUMENTATION,
    WorkItemType.DOC_CHUNK: TicketType.DOCUMENTATION,
    WorkItemType.DOC_MERGE: TicketType.DOCUMENTATION,
    WorkItemType.DOC_CHALLENGE: TicketType.VALIDATION,
    WorkItemType.DOC_FOLLOWUP: TicketType.CLARIFICATION,
    WorkItemType.DOC_PATCH_MERGE: TicketType.CHROME,
    WorkItemType.DOC_FINALIZE: TicketType.DOCUMENTATION,
}

BEADS_TO_ATLAS_TYPE: dict[TicketType, WorkItemType] = {
    TicketType.DOCUMENTATION: WorkItemType.DOC_CHUNK,
    TicketType.VALIDATION: WorkItemType.DOC_CHALLENGE,
    TicketType.CLARIFICATION: WorkItemType.DOC_FOLLOWUP,
    TicketType.CHROME: WorkItemType.DOC_PATCH_MERGE,
}


class BeadsTicketAdapter(TicketSystemAdapter):
    """Adapts War Rig's BeadsClient to Atlas's TicketSystemAdapter.

    This adapter enables Atlas's reconciliation controller to use
    War Rig's existing ticket tracking infrastructure.

    Note:
        War Rig's BeadsClient uses synchronous methods while Atlas
        expects async. This adapter wraps sync calls appropriately.
    """

    def __init__(self, beads_client: BeadsClient) -> None:
        """Initialize the adapter.

        Args:
            beads_client: War Rig's BeadsClient instance.
        """
        self.beads = beads_client
        self._work_item_cache: dict[str, WorkItem] = {}

    async def create_work_item(self, work_item: WorkItem) -> str:
        """Create a work item in the beads system.

        Converts Atlas WorkItem to BeadsClient ProgramManagerTicket
        and creates it.
        """
        beads_type = ATLAS_TO_BEADS_TYPE.get(
            work_item.work_type, TicketType.DOCUMENTATION
        )

        # Extract file_name from payload or metadata
        file_name = ""
        if work_item.payload and work_item.payload.artifact_ref:
            file_name = work_item.payload.artifact_ref.artifact_id

        pm_ticket = self.beads.create_pm_ticket(
            ticket_type=beads_type,
            file_name=file_name,
            program_id=file_name.split(".")[0] if file_name else None,
            cycle_number=work_item.cycle_number or 1,
            metadata={
                "atlas_work_id": work_item.work_id,
                "atlas_work_type": work_item.work_type.value,
                "atlas_payload": work_item.payload.model_dump() if work_item.payload else {},
                "depends_on": work_item.depends_on,
                "idempotency_key": work_item.idempotency_key,
            },
        )

        if pm_ticket:
            # Cache for lookup
            self._work_item_cache[pm_ticket.ticket_id] = work_item
            work_item.work_id = pm_ticket.ticket_id
            return pm_ticket.ticket_id

        raise RuntimeError(f"Failed to create work item: {work_item.work_id}")

    async def get_work_item(self, work_id: str) -> WorkItem | None:
        """Retrieve a work item by ID."""
        # Check cache first
        if work_id in self._work_item_cache:
            return self._work_item_cache[work_id]

        # Look up in beads
        ticket = self.beads._pm_ticket_cache.get(work_id)
        if ticket is None:
            return None

        return self._pm_ticket_to_work_item(ticket)

    async def update_status(
        self,
        work_id: str,
        new_status: WorkItemStatus,
        *,
        expected_status: WorkItemStatus | None = None,
    ) -> bool:
        """Update work item status."""
        beads_state = ATLAS_TO_BEADS_STATUS.get(new_status, TicketState.CREATED)
        return self.beads.update_ticket_state(work_id, beads_state)

    async def claim_work_item(
        self,
        work_id: str,
        worker_id: str,
        lease_duration_seconds: int = 300,
    ) -> bool:
        """Claim a work item for processing."""
        return self.beads.claim_ticket(work_id, worker_id)

    async def release_work_item(
        self,
        work_id: str,
        worker_id: str,
        new_status: WorkItemStatus = WorkItemStatus.READY,
    ) -> bool:
        """Release a claimed work item."""
        beads_state = ATLAS_TO_BEADS_STATUS.get(new_status, TicketState.CREATED)
        return self.beads.update_ticket_state(work_id, beads_state)

    async def query_by_status(
        self,
        status: WorkItemStatus,
        work_type: WorkItemType | None = None,
        job_id: str | None = None,
        limit: int = 100,
    ) -> list[WorkItem]:
        """Query work items by status."""
        beads_state = ATLAS_TO_BEADS_STATUS.get(status, TicketState.CREATED)
        beads_type = ATLAS_TO_BEADS_TYPE.get(work_type) if work_type else None

        tickets = self.beads.get_tickets_by_state(beads_state, beads_type)

        results = []
        for t in tickets[:limit]:
            work_item = self._pm_ticket_to_work_item(t)
            if work_item is not None:
                results.append(work_item)
        return results

    async def query_by_job(
        self,
        job_id: str,
        work_type: WorkItemType | None = None,
    ) -> list[WorkItem]:
        """Query all work items for a job."""
        # Filter from cache by job_id in metadata
        results = []
        for ticket in self.beads._pm_ticket_cache.values():
            atlas_payload = ticket.metadata.get("atlas_payload", {})
            atlas_job_id = atlas_payload.get("job_id")
            if atlas_job_id == job_id:
                if work_type:
                    atlas_type = ticket.metadata.get("atlas_work_type")
                    if atlas_type != work_type.value:
                        continue
                work_item = self._pm_ticket_to_work_item(ticket)
                if work_item:
                    results.append(work_item)
        return results

    async def get_ready_work_items(
        self,
        work_type: WorkItemType | None = None,
        limit: int = 10,
    ) -> list[WorkItem]:
        """Get work items ready for processing."""
        beads_type = ATLAS_TO_BEADS_TYPE.get(work_type) if work_type else None
        tickets = self.beads.get_available_tickets(beads_type)

        results = []
        for t in tickets[:limit]:
            work_item = self._pm_ticket_to_work_item(t)
            if work_item is not None:
                results.append(work_item)
        return results

    async def check_dependencies_done(self, work_id: str) -> bool:
        """Check if all dependencies of a work item are DONE."""
        work_item = await self.get_work_item(work_id)
        if not work_item:
            return False

        for dep_id in work_item.depends_on:
            dep = await self.get_work_item(dep_id)
            if dep is None or dep.status != WorkItemStatus.DONE:
                return False

        return True

    async def find_by_idempotency_key(
        self,
        idempotency_key: str,
    ) -> WorkItem | None:
        """Find work item by idempotency key."""
        for ticket in self.beads._pm_ticket_cache.values():
            if ticket.metadata.get("idempotency_key") == idempotency_key:
                return self._pm_ticket_to_work_item(ticket)
        return None

    def _pm_ticket_to_work_item(
        self,
        ticket: ProgramManagerTicket,
    ) -> WorkItem | None:
        """Convert BeadsClient ticket to Atlas WorkItem."""
        # Check if this is an Atlas-managed ticket
        atlas_type_str = ticket.metadata.get("atlas_work_type")

        # If no atlas metadata, create a minimal WorkItem from beads ticket
        if not atlas_type_str:
            # Map beads ticket type to atlas type
            work_type = BEADS_TO_ATLAS_TYPE.get(
                ticket.ticket_type, WorkItemType.DOC_CHUNK
            )
            status = BEADS_TO_ATLAS_STATUS.get(ticket.state, WorkItemStatus.NEW)

            return WorkItem(
                work_id=ticket.ticket_id,
                work_type=work_type,
                status=status,
                payload=WorkItemPayload(),
                depends_on=ticket.metadata.get("depends_on", []),
                cycle_number=ticket.cycle_number,
            )

        # Reconstruct from atlas metadata
        try:
            work_type = WorkItemType(atlas_type_str)
        except ValueError:
            return None

        status = BEADS_TO_ATLAS_STATUS.get(ticket.state, WorkItemStatus.NEW)

        # Reconstruct payload
        atlas_payload = ticket.metadata.get("atlas_payload", {})
        payload = WorkItemPayload(**atlas_payload) if atlas_payload else WorkItemPayload()

        return WorkItem(
            work_id=ticket.ticket_id,
            work_type=work_type,
            status=status,
            payload=payload,
            depends_on=ticket.metadata.get("depends_on", []),
            cycle_number=ticket.cycle_number,
            idempotency_key=ticket.metadata.get("idempotency_key"),
        )
