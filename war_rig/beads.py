"""Beads integration for War Rig ticket tracking.

This module provides integration with the beads task tracking system,
allowing agents to create and manage tickets during the documentation process.

Tickets are created when:
- Challenger asks blocking questions
- Imperator issues Chrome tickets

Tickets are closed when:
- Documentation reaches WITNESSED or VALHALLA status
- Questions are satisfactorily answered

Program Manager Workflow:
- Uses extended ticket types (DOCUMENTATION, VALIDATION, etc.)
- Tracks ticket state through the workflow state machine
- Supports atomic ticket claiming for parallel workers
"""

import json
import logging
import os
import re
import subprocess
import threading
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)


class BeadsTicketType(str, Enum):
    """Types of beads tickets that can be created."""

    BUG = "bug"
    TASK = "task"
    FEATURE = "feature"


class BeadsPriority(int, Enum):
    """Priority levels for beads tickets (0=critical, 4=backlog)."""

    CRITICAL = 0
    HIGH = 1
    MEDIUM = 2
    LOW = 3
    BACKLOG = 4


class TicketType(str, Enum):
    """Ticket types for Program Manager workflow.

    These types track different work items in the parallel documentation process:
    - DOCUMENTATION: Initial documentation task for a source file
    - VALIDATION: Validation task for completed documentation
    - CLARIFICATION: Question from Challenger requiring Scribe response
    - CHROME: Issue ticket from Imperator requiring rework
    - HOLISTIC_REVIEW: Batch review task for Imperator
    """

    DOCUMENTATION = "documentation"
    VALIDATION = "validation"
    CLARIFICATION = "clarification"
    CHROME = "chrome"
    HOLISTIC_REVIEW = "holistic_review"


class TicketState(str, Enum):
    """States for Program Manager workflow tickets.

    State machine transitions:
    - CREATED -> CLAIMED: Worker picks ticket
    - CLAIMED -> IN_PROGRESS: Work begins
    - IN_PROGRESS -> COMPLETED: Work finished successfully
    - IN_PROGRESS -> BLOCKED: Dependency identified
    - COMPLETED -> REWORK: Imperator issues CHROME
    - REWORK -> CREATED: New cycle starts
    - BLOCKED -> CREATED: Dependency resolved
    - Any -> CANCELLED: Ticket no longer needed
    - Any -> MERGED: Combined into another ticket
    """

    CREATED = "created"
    CLAIMED = "claimed"
    IN_PROGRESS = "in_progress"
    COMPLETED = "completed"
    REWORK = "rework"
    BLOCKED = "blocked"
    CANCELLED = "cancelled"
    MERGED = "merged"


# Mapping from TicketState to beads CLI status values
# beads supports: open, in_progress, blocked, deferred, closed
TICKET_STATE_TO_BD_STATUS: dict[TicketState, str] = {
    TicketState.CREATED: "open",
    TicketState.CLAIMED: "in_progress",  # Claimed is a form of in_progress
    TicketState.IN_PROGRESS: "in_progress",
    TicketState.COMPLETED: "closed",
    TicketState.REWORK: "open",  # Rework returns to open for next cycle
    TicketState.BLOCKED: "blocked",
    TicketState.CANCELLED: "closed",
    TicketState.MERGED: "closed",
}

# Mapping from beads status back to possible TicketStates
# Note: This is ambiguous, so we use labels to disambiguate
BD_STATUS_TO_TICKET_STATES: dict[str, list[TicketState]] = {
    "open": [TicketState.CREATED, TicketState.REWORK],
    "in_progress": [TicketState.CLAIMED, TicketState.IN_PROGRESS],
    "blocked": [TicketState.BLOCKED],
    "closed": [TicketState.COMPLETED, TicketState.CANCELLED, TicketState.MERGED],
}


@dataclass
class BeadsTicket:
    """Represents a ticket in the beads system."""

    id: str
    title: str
    ticket_type: BeadsTicketType
    priority: BeadsPriority
    assignee: str | None = None
    labels: list[str] | None = None


@dataclass
class ProgramManagerTicket:
    """Extended ticket for Program Manager workflow.

    This dataclass represents a ticket used in the parallel documentation
    workflow, with additional fields for tracking workflow state, file
    associations, and cycle information.

    Attributes:
        ticket_id: Unique identifier for the ticket (e.g., "war_rig-abc123")
        ticket_type: Type of work item (DOCUMENTATION, VALIDATION, etc.)
        state: Current workflow state (CREATED, CLAIMED, IN_PROGRESS, etc.)
        file_name: Source file being documented
        program_id: Identifier for the program (extracted from file)
        cycle_number: Current batch cycle (1-based)
        worker_id: ID of worker that claimed the ticket
        parent_ticket_id: Link to parent ticket for derived tickets
        metadata: Additional JSON-serializable data
    """

    ticket_id: str
    ticket_type: TicketType
    state: TicketState
    file_name: str
    program_id: str | None = None
    cycle_number: int = 1
    worker_id: str | None = None
    parent_ticket_id: str | None = None
    metadata: dict[str, Any] = field(default_factory=dict)

    # Timestamps
    created_at: datetime = field(default_factory=datetime.utcnow)
    updated_at: datetime = field(default_factory=datetime.utcnow)
    claimed_at: datetime | None = None

    def to_labels(self) -> list[str]:
        """Generate labels for beads ticket from this PM ticket.

        Returns:
            List of labels encoding ticket type, state, and associations.
        """
        labels = [
            f"pm-type:{self.ticket_type.value}",
            f"pm-state:{self.state.value}",
            f"cycle:{self.cycle_number}",
        ]

        if self.file_name:
            labels.append(f"file:{self.file_name}")

        if self.program_id:
            labels.append(f"program:{self.program_id}")

        if self.worker_id:
            labels.append(f"worker:{self.worker_id}")

        if self.parent_ticket_id:
            labels.append(f"parent:{self.parent_ticket_id}")

        return labels

    @classmethod
    def from_labels(
        cls,
        ticket_id: str,
        labels: list[str],
        bd_status: str,
    ) -> "ProgramManagerTicket | None":
        """Reconstruct a PM ticket from beads labels.

        Args:
            ticket_id: The beads ticket ID.
            labels: List of labels from the beads ticket.
            bd_status: The beads status (open, in_progress, etc.)

        Returns:
            ProgramManagerTicket if labels indicate a PM ticket, None otherwise.
        """
        label_dict: dict[str, str] = {}
        for label in labels:
            if ":" in label:
                key, value = label.split(":", 1)
                label_dict[key] = value

        # Check if this is a PM ticket
        if "pm-type" not in label_dict:
            return None

        try:
            ticket_type = TicketType(label_dict["pm-type"])
        except ValueError:
            return None

        # Get state from label, falling back to inferring from bd_status
        state_str = label_dict.get("pm-state")
        if state_str:
            try:
                state = TicketState(state_str)
            except ValueError:
                # Fall back to inferring from bd_status
                possible_states = BD_STATUS_TO_TICKET_STATES.get(bd_status, [])
                state = possible_states[0] if possible_states else TicketState.CREATED
        else:
            possible_states = BD_STATUS_TO_TICKET_STATES.get(bd_status, [])
            state = possible_states[0] if possible_states else TicketState.CREATED

        return cls(
            ticket_id=ticket_id,
            ticket_type=ticket_type,
            state=state,
            file_name=label_dict.get("file", ""),
            program_id=label_dict.get("program"),
            cycle_number=int(label_dict.get("cycle", "1")),
            worker_id=label_dict.get("worker"),
            parent_ticket_id=label_dict.get("parent"),
        )


class BeadsClient:
    """Client for interacting with the beads task tracking system.

    This client wraps the `bd` CLI commands to create, update, and close
    tickets from within the War Rig workflow.

    Example:
        client = BeadsClient()
        ticket_id = client.create_ticket(
            title="CBACT04C: Missing business rule citation",
            ticket_type=BeadsTicketType.BUG,
            priority=BeadsPriority.HIGH,
            assignee="scribe",
        )
        # Later...
        client.close_ticket(ticket_id, reason="Documentation updated with citation")
    """

    def __init__(
        self,
        enabled: bool = True,
        dry_run: bool = False,
        tickets_file: Path | str | None = None,
        beads_dir: Path | str | None = None,
    ):
        """Initialize the beads client.

        Args:
            enabled: Whether to actually create tickets (False for testing).
            dry_run: Log commands instead of executing them.
            tickets_file: Path to persist tickets for crash recovery.
                If None, tickets are only kept in memory.
            beads_dir: Directory to run bd commands in (for isolated beads).
                If None, uses current working directory.
        """
        self.enabled = enabled
        self.dry_run = dry_run
        self._ticket_cache: dict[str, BeadsTicket] = {}
        # In-memory PM ticket tracking for when beads CLI is unavailable
        self._pm_ticket_cache: dict[str, ProgramManagerTicket] = {}
        self._pm_ticket_counter: int = 0
        # Track if bd CLI is available (checked on first use)
        self._bd_available: bool | None = None
        # Lock for thread-safe access to in-memory ticket cache
        self._lock = threading.Lock()
        # Persistence for crash recovery
        self._tickets_file: Path | None = Path(tickets_file) if tickets_file else None
        self._persist_lock = threading.Lock()
        # Directory for bd commands (isolated from project's .beads/)
        self._beads_dir: Path | None = Path(beads_dir) if beads_dir else None
        # Load existing tickets if file exists
        if self._tickets_file:
            self._load_from_disk()

    def _check_bd_available(self) -> bool:
        """Check if the bd CLI is available and beads is initialized.

        Returns:
            True if bd CLI is available and beads is initialized in beads_dir.
        """
        if self._bd_available is not None:
            return self._bd_available

        try:
            # Check if bd command exists
            result = subprocess.run(
                ["bd", "--version"],
                capture_output=True,
                text=True,
                timeout=5,
            )
            if result.returncode != 0:
                logger.info("bd CLI not working, using in-memory ticket tracking")
                self._bd_available = False
                return False

            logger.debug(f"bd CLI available: {result.stdout.strip()}")

            # If beads_dir is specified, check if .beads exists there
            if self._beads_dir:
                beads_init_path = self._beads_dir / ".beads"
                if not beads_init_path.exists():
                    logger.info(
                        f"Beads not initialized in {self._beads_dir}, "
                        "using in-memory tracking. Run scripts/init_beads_tracking.sh first."
                    )
                    self._bd_available = False
                    return False

            self._bd_available = True

        except (FileNotFoundError, subprocess.TimeoutExpired):
            logger.info("bd CLI not found, using in-memory ticket tracking")
            self._bd_available = False
        except Exception as e:
            logger.warning(f"Error checking bd CLI: {e}, using in-memory tracking")
            self._bd_available = False

        return self._bd_available

    def _load_from_disk(self) -> None:
        """Load tickets from disk for crash recovery.

        Resets any CLAIMED or IN_PROGRESS tickets back to CREATED,
        assuming they weren't completed before the crash.
        """
        if not self._tickets_file or not self._tickets_file.exists():
            return

        try:
            with open(self._tickets_file) as f:
                data = json.load(f)

            loaded_count = 0
            reset_count = 0

            for ticket_data in data.get("tickets", []):
                try:
                    # Parse timestamps if present
                    created_at = datetime.utcnow()
                    updated_at = datetime.utcnow()
                    if ticket_data.get("created_at"):
                        try:
                            created_at = datetime.fromisoformat(ticket_data["created_at"])
                        except (ValueError, TypeError):
                            pass
                    if ticket_data.get("updated_at"):
                        try:
                            updated_at = datetime.fromisoformat(ticket_data["updated_at"])
                        except (ValueError, TypeError):
                            pass

                    # Reconstruct the ticket
                    ticket = ProgramManagerTicket(
                        ticket_id=ticket_data["ticket_id"],
                        ticket_type=TicketType(ticket_data["ticket_type"]),
                        state=TicketState(ticket_data["state"]),
                        file_name=ticket_data["file_name"],
                        program_id=ticket_data.get("program_id"),
                        cycle_number=ticket_data.get("cycle_number", 1),
                        worker_id=ticket_data.get("worker_id"),
                        parent_ticket_id=ticket_data.get("parent_ticket_id"),
                        metadata=ticket_data.get("metadata", {}),
                        created_at=created_at,
                        updated_at=updated_at,
                    )

                    # Reset incomplete/failed tickets - assume crash interrupted them
                    # or they need retry
                    if ticket.state in (
                        TicketState.CLAIMED,
                        TicketState.IN_PROGRESS,
                        TicketState.BLOCKED,
                    ):
                        ticket.state = TicketState.CREATED
                        ticket.worker_id = None
                        ticket.claimed_at = None
                        reset_count += 1

                    self._pm_ticket_cache[ticket.ticket_id] = ticket
                    loaded_count += 1

                    # Track highest ticket counter for new ticket IDs
                    if ticket.ticket_id.startswith("mem-"):
                        try:
                            num = int(ticket.ticket_id.split("-")[1])
                            self._pm_ticket_counter = max(self._pm_ticket_counter, num)
                        except (IndexError, ValueError):
                            pass

                except (KeyError, ValueError) as e:
                    logger.warning(f"Skipping invalid ticket data: {e}")
                    continue

            logger.info(
                f"Loaded {loaded_count} tickets from disk "
                f"({reset_count} reset to CREATED for retry)"
            )

        except json.JSONDecodeError as e:
            logger.error(f"Failed to parse tickets file: {e}")
        except Exception as e:
            logger.error(f"Failed to load tickets from disk: {e}")

    def _save_to_disk(self) -> None:
        """Persist tickets to disk for crash recovery.

        Uses atomic write (write to temp, then rename) to prevent corruption.
        """
        if not self._tickets_file:
            return

        with self._persist_lock:
            try:
                # Ensure directory exists
                self._tickets_file.parent.mkdir(parents=True, exist_ok=True)

                # Build ticket data
                tickets_data = []
                with self._lock:
                    for ticket in self._pm_ticket_cache.values():
                        tickets_data.append({
                            "ticket_id": ticket.ticket_id,
                            "ticket_type": ticket.ticket_type.value,
                            "state": ticket.state.value,
                            "file_name": ticket.file_name,
                            "program_id": ticket.program_id,
                            "cycle_number": ticket.cycle_number,
                            "worker_id": ticket.worker_id,
                            "parent_ticket_id": ticket.parent_ticket_id,
                            "metadata": ticket.metadata,
                            "created_at": ticket.created_at.isoformat() if ticket.created_at else None,
                            "updated_at": ticket.updated_at.isoformat() if ticket.updated_at else None,
                        })

                data = {
                    "version": 1,
                    "saved_at": datetime.utcnow().isoformat(),
                    "ticket_count": len(tickets_data),
                    "tickets": tickets_data,
                }

                # Atomic write: write to temp file, then rename
                temp_file = self._tickets_file.with_suffix(".tmp")
                with open(temp_file, "w") as f:
                    json.dump(data, f, indent=2)

                # Atomic rename (on POSIX systems)
                os.replace(temp_file, self._tickets_file)

                logger.debug(f"Saved {len(tickets_data)} tickets to disk")

            except Exception as e:
                logger.error(f"Failed to save tickets to disk: {e}")

    def _run_bd(self, args: list[str]) -> tuple[bool, str]:
        """Run a bd command.

        Args:
            args: Arguments to pass to bd.

        Returns:
            Tuple of (success, output).
        """
        cmd = ["bd"] + args

        if self.dry_run:
            logger.info(f"[DRY RUN] Would execute: {' '.join(cmd)}")
            return True, "dry-run-ticket-id"

        if not self.enabled:
            logger.debug(f"Beads disabled, skipping: {' '.join(cmd)}")
            return True, ""

        # Check if bd CLI is available
        if not self._check_bd_available():
            logger.debug(f"bd CLI unavailable, skipping: {' '.join(cmd)}")
            return False, "bd not available"

        try:
            # Run bd in the beads_dir if specified (isolated from project .beads/)
            cwd = str(self._beads_dir) if self._beads_dir else None

            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=30,
                cwd=cwd,
            )

            if result.returncode == 0:
                return True, result.stdout.strip()
            else:
                logger.error(f"bd command failed: {result.stderr}")
                return False, result.stderr

        except subprocess.TimeoutExpired:
            logger.error("bd command timed out")
            return False, "timeout"
        except FileNotFoundError:
            logger.warning("bd command not found - beads not installed")
            self._bd_available = False
            return False, "bd not found"
        except Exception as e:
            logger.error(f"Error running bd: {e}")
            return False, str(e)

    def create_ticket(
        self,
        title: str,
        ticket_type: BeadsTicketType = BeadsTicketType.TASK,
        priority: BeadsPriority = BeadsPriority.MEDIUM,
        assignee: str | None = None,
        labels: list[str] | None = None,
        parent_id: str | None = None,
    ) -> str | None:
        """Create a new ticket in beads.

        Args:
            title: Ticket title/description.
            ticket_type: Type of ticket (bug, task, feature).
            priority: Priority level (0-4).
            assignee: Agent or person to assign to.
            labels: Optional labels/tags.
            parent_id: Optional parent ticket for hierarchy.

        Returns:
            Ticket ID if created successfully, None otherwise.
        """
        args = [
            "create",
            f"--title={title}",
            f"--type={ticket_type.value}",
            f"--priority={priority.value}",
        ]

        if assignee:
            args.append(f"--assignee={assignee}")

        if parent_id:
            args.append(f"--parent={parent_id}")

        success, output = self._run_bd(args)

        if success:
            # Parse ticket ID from output
            # bd create outputs something like "Created issue: war_rig-abc123"
            ticket_id = self._parse_ticket_id(output)
            if ticket_id:
                self._ticket_cache[ticket_id] = BeadsTicket(
                    id=ticket_id,
                    title=title,
                    ticket_type=ticket_type,
                    priority=priority,
                    assignee=assignee,
                    labels=labels,
                )
                logger.info(f"Created beads ticket: {ticket_id} - {title}")
                return ticket_id

        return None

    def close_ticket(self, ticket_id: str, reason: str | None = None) -> bool:
        """Close a ticket in beads.

        Args:
            ticket_id: ID of the ticket to close.
            reason: Optional reason for closing.

        Returns:
            True if closed successfully.
        """
        args = ["close", ticket_id]
        if reason:
            args.append(f"--reason={reason}")

        success, _ = self._run_bd(args)

        if success:
            logger.info(f"Closed beads ticket: {ticket_id}")
            if ticket_id in self._ticket_cache:
                del self._ticket_cache[ticket_id]

        return success

    def close_tickets(self, ticket_ids: list[str], reason: str | None = None) -> int:
        """Close multiple tickets at once.

        Args:
            ticket_ids: List of ticket IDs to close.
            reason: Optional reason for closing.

        Returns:
            Number of tickets successfully closed.
        """
        if not ticket_ids:
            return 0

        args = ["close"] + ticket_ids
        if reason:
            args.append(f"--reason={reason}")

        success, _ = self._run_bd(args)

        if success:
            logger.info(f"Closed {len(ticket_ids)} beads tickets")
            for tid in ticket_ids:
                if tid in self._ticket_cache:
                    del self._ticket_cache[tid]
            return len(ticket_ids)

        return 0

    def update_ticket(
        self,
        ticket_id: str,
        status: str | None = None,
        assignee: str | None = None,
    ) -> bool:
        """Update a ticket in beads.

        Args:
            ticket_id: ID of the ticket to update.
            status: New status (open, in_progress, closed).
            assignee: New assignee.

        Returns:
            True if updated successfully.
        """
        args = ["update", ticket_id]

        if status:
            args.append(f"--status={status}")
        if assignee:
            args.append(f"--assignee={assignee}")

        success, _ = self._run_bd(args)
        return success

    def add_dependency(self, ticket_id: str, depends_on: str) -> bool:
        """Add a dependency between tickets.

        Args:
            ticket_id: Ticket that depends on another.
            depends_on: Ticket that must be completed first.

        Returns:
            True if dependency added successfully.
        """
        args = ["dep", "add", ticket_id, depends_on]
        success, _ = self._run_bd(args)
        return success

    def _parse_ticket_id(self, output: str) -> str | None:
        """Parse ticket ID from bd command output.

        Args:
            output: Raw output from bd command.

        Returns:
            Ticket ID or None if not found.
        """
        if self.dry_run:
            return "dry-run-ticket"

        # Look for patterns like "war_rig-abc123" or "beads-xyz789"
        match = re.search(r"([a-zA-Z_-]+\-[a-zA-Z0-9]+(?:\.\d+)?)", output)
        if match:
            return match.group(1)

        # If output is just an ID
        if output and "-" in output:
            return output.split()[-1] if output.split() else output

        return None

    def get_open_tickets(self) -> list[str]:
        """Get list of open ticket IDs from cache.

        Returns:
            List of ticket IDs that are still open.
        """
        return list(self._ticket_cache.keys())

    # =========================================================================
    # Program Manager Workflow Methods
    # =========================================================================

    def _use_memory_cache(self) -> bool:
        """Check if we should use in-memory caching instead of bd CLI.

        Returns:
            True if we should use memory cache (beads disabled, dry run,
            or bd CLI unavailable).
        """
        if not self.enabled or self.dry_run:
            return True
        # Also use memory cache if bd CLI is not available
        return not self._check_bd_available()

    def create_pm_ticket(
        self,
        ticket_type: TicketType,
        file_name: str,
        program_id: str | None = None,
        cycle_number: int = 1,
        parent_ticket_id: str | None = None,
        priority: BeadsPriority = BeadsPriority.MEDIUM,
        metadata: dict[str, Any] | None = None,
    ) -> ProgramManagerTicket | None:
        """Create a Program Manager workflow ticket.

        Creates a ticket in beads with labels encoding the PM workflow state.
        The ticket is created in the CREATED state, ready for a worker to claim.

        When beads is disabled, tickets are tracked in memory.

        Args:
            ticket_type: Type of PM ticket (DOCUMENTATION, VALIDATION, etc.)
            file_name: Source file being documented.
            program_id: Identifier for the program.
            cycle_number: Current batch cycle (1-based).
            parent_ticket_id: Link to parent ticket for derived tickets.
            priority: Priority level for the ticket.
            metadata: Additional JSON-serializable data.

        Returns:
            ProgramManagerTicket if created successfully, None otherwise.

        Example:
            ticket = client.create_pm_ticket(
                ticket_type=TicketType.DOCUMENTATION,
                file_name="CBACT04C.cbl",
                program_id="CBACT04C",
                cycle_number=1,
            )
        """
        # If using memory cache, create and store with lock
        if self._use_memory_cache():
            with self._lock:
                self._pm_ticket_counter += 1
                memory_ticket_id = f"mem-{self._pm_ticket_counter:06d}"
                pm_ticket = ProgramManagerTicket(
                    ticket_id=memory_ticket_id,
                    ticket_type=ticket_type,
                    state=TicketState.CREATED,
                    file_name=file_name,
                    program_id=program_id,
                    cycle_number=cycle_number,
                    parent_ticket_id=parent_ticket_id,
                    metadata=metadata or {},
                )
                self._pm_ticket_cache[memory_ticket_id] = pm_ticket
            # Persist to disk for crash recovery
            self._save_to_disk()
            logger.info(
                f"Created PM ticket (memory): {memory_ticket_id} "
                f"({ticket_type.value}) for {file_name}"
            )
            return pm_ticket

        # For bd CLI mode, generate ticket ID placeholder
        self._pm_ticket_counter += 1
        memory_ticket_id = f"mem-{self._pm_ticket_counter:06d}"

        pm_ticket = ProgramManagerTicket(
            ticket_id=memory_ticket_id,
            ticket_type=ticket_type,
            state=TicketState.CREATED,
            file_name=file_name,
            program_id=program_id,
            cycle_number=cycle_number,
            parent_ticket_id=parent_ticket_id,
            metadata=metadata or {},
        )

        # Build title based on ticket type
        type_prefix = ticket_type.value.upper()
        if program_id:
            title = f"[{type_prefix}] {program_id}: {file_name}"
        else:
            title = f"[{type_prefix}] {file_name}"

        if cycle_number > 1:
            title = f"[Cycle {cycle_number}] {title}"

        # Get labels from PM ticket
        labels = pm_ticket.to_labels()

        # Create the beads ticket
        args = [
            "create",
            f"--title={title}",
            f"--type=task",
            f"--priority={priority.value}",
        ]

        # Use --labels with comma-separated values for bd create
        if labels:
            args.append(f"--labels={','.join(labels)}")

        if parent_ticket_id:
            args.append(f"--parent={parent_ticket_id}")

        success, output = self._run_bd(args)

        if success:
            ticket_id = self._parse_ticket_id(output)
            if ticket_id:
                pm_ticket.ticket_id = ticket_id
                # Also cache in memory for consistency
                self._pm_ticket_cache[ticket_id] = pm_ticket
                logger.info(
                    f"Created PM ticket: {ticket_id} "
                    f"({ticket_type.value}) for {file_name}"
                )
                return pm_ticket

        return None

    def claim_ticket(
        self,
        ticket_id: str,
        worker_id: str,
    ) -> bool:
        """Atomically claim a ticket for a worker.

        Uses bd's --claim flag to atomically set the assignee and status.
        This prevents race conditions when multiple workers try to claim
        the same ticket.

        When using memory cache, performs simple state check and update.

        Args:
            ticket_id: ID of the ticket to claim.
            worker_id: ID of the worker claiming the ticket.

        Returns:
            True if claim succeeded, False if already claimed or error.

        Example:
            if client.claim_ticket("war_rig-abc123", "scribe-1"):
                # Process the ticket
                pass
            else:
                # Ticket was claimed by another worker
                pass
        """
        # Memory cache mode - use lock for thread safety
        if self._use_memory_cache():
            with self._lock:
                ticket = self._pm_ticket_cache.get(ticket_id)
                if not ticket:
                    logger.warning(f"Ticket {ticket_id} not found in memory cache")
                    return False
                if ticket.state != TicketState.CREATED:
                    logger.debug(f"Ticket {ticket_id} already claimed (state={ticket.state})")
                    return False
                # Claim the ticket (atomic with lock held)
                ticket.state = TicketState.CLAIMED
                ticket.worker_id = worker_id
                ticket.claimed_at = datetime.utcnow()
            # Persist to disk for crash recovery
            self._save_to_disk()
            logger.info(f"Worker {worker_id} claimed ticket {ticket_id} (memory)")
            return True

        # Use bd update --claim for atomic claim
        args = [
            "update",
            ticket_id,
            "--claim",
            f"--assignee={worker_id}",
            f"--add-label=worker:{worker_id}",
            f"--remove-label=pm-state:{TicketState.CREATED.value}",
            f"--add-label=pm-state:{TicketState.CLAIMED.value}",
        ]

        success, output = self._run_bd(args)

        if success:
            logger.info(f"Worker {worker_id} claimed ticket {ticket_id}")
            # Update memory cache
            if ticket_id in self._pm_ticket_cache:
                self._pm_ticket_cache[ticket_id].state = TicketState.CLAIMED
                self._pm_ticket_cache[ticket_id].worker_id = worker_id
            return True
        else:
            # Check if failure was due to already claimed
            if "already claimed" in output.lower():
                logger.debug(f"Ticket {ticket_id} already claimed")
            else:
                logger.warning(f"Failed to claim ticket {ticket_id}: {output}")
            return False

    def update_ticket_state(
        self,
        ticket_id: str,
        new_state: TicketState,
        reason: str | None = None,
        metadata_updates: dict[str, Any] | None = None,
    ) -> bool:
        """Transition a ticket to a new workflow state.

        Updates both the beads status and the pm-state label to reflect
        the new state.

        When using memory cache, updates the in-memory ticket state.

        Args:
            ticket_id: ID of the ticket to update.
            new_state: New workflow state.
            reason: Optional reason for the transition.
            metadata_updates: Optional dict of metadata fields to update.
                These are merged into existing metadata (not replaced).

        Returns:
            True if update succeeded, False otherwise.

        Example:
            # Mark work as in progress
            client.update_ticket_state(
                "war_rig-abc123",
                TicketState.IN_PROGRESS,
            )

            # Mark as completed
            client.update_ticket_state(
                "war_rig-abc123",
                TicketState.COMPLETED,
                reason="Documentation approved",
            )

            # Reset for retry with updated metadata
            client.update_ticket_state(
                "war_rig-abc123",
                TicketState.CREATED,
                metadata_updates={"retry_count": 2, "last_error": "Parse failed"},
            )
        """
        # Memory cache mode - use lock for thread safety
        if self._use_memory_cache():
            with self._lock:
                ticket = self._pm_ticket_cache.get(ticket_id)
                if not ticket:
                    logger.warning(f"Ticket {ticket_id} not found in memory cache")
                    return False
                ticket.state = new_state
                ticket.updated_at = datetime.utcnow()
                # Update metadata if provided
                if metadata_updates:
                    ticket.metadata.update(metadata_updates)
            # Persist to disk for crash recovery
            self._save_to_disk()
            logger.info(f"Updated ticket {ticket_id} state to {new_state.value} (memory)")
            return True

        bd_status = TICKET_STATE_TO_BD_STATUS[new_state]

        # Build update command
        args = [
            "update",
            ticket_id,
            f"--status={bd_status}",
        ]

        # Update the state label - remove old states and add new one
        for state in TicketState:
            args.append(f"--remove-label=pm-state:{state.value}")
        args.append(f"--add-label=pm-state:{new_state.value}")

        success, _ = self._run_bd(args)

        if success:
            logger.info(f"Updated ticket {ticket_id} state to {new_state.value}")

            # Update memory cache and persist to disk
            if ticket_id in self._pm_ticket_cache:
                self._pm_ticket_cache[ticket_id].state = new_state
                self._save_to_disk()

            # If closing, also run close command with reason
            if new_state in (
                TicketState.COMPLETED,
                TicketState.CANCELLED,
                TicketState.MERGED,
            ):
                close_args = ["close", ticket_id]
                if reason:
                    close_args.append(f"--reason={reason}")
                self._run_bd(close_args)

            return True

        return False

    def get_available_tickets(
        self,
        ticket_type: TicketType | None = None,
        cycle_number: int | None = None,
    ) -> list[ProgramManagerTicket]:
        """List unclaimed tickets available for workers.

        Queries beads for tickets in the CREATED state that have not been
        claimed by any worker.

        When using memory cache, filters from in-memory ticket storage.

        Args:
            ticket_type: Filter by ticket type (optional).
            cycle_number: Filter by cycle number (optional).

        Returns:
            List of ProgramManagerTicket objects available for claiming.

        Example:
            # Get all available documentation tickets
            tickets = client.get_available_tickets(
                ticket_type=TicketType.DOCUMENTATION,
            )
            for ticket in tickets:
                if client.claim_ticket(ticket.ticket_id, worker_id):
                    process(ticket)
                    break
        """
        # Memory cache mode - use lock for thread safety
        if self._use_memory_cache():
            with self._lock:
                tickets = []
                for ticket in self._pm_ticket_cache.values():
                    # Must be in CREATED state
                    if ticket.state != TicketState.CREATED:
                        continue
                    # Filter by ticket type if specified
                    if ticket_type and ticket.ticket_type != ticket_type:
                        continue
                    # Filter by cycle number if specified
                    if cycle_number and ticket.cycle_number != cycle_number:
                        continue
                    tickets.append(ticket)
                return tickets

        args = ["list", "--json", "--status=open"]

        # Add label filters
        if ticket_type:
            args.append(f"--label=pm-type:{ticket_type.value}")
        else:
            # Must have pm-type label to be a PM ticket
            args.append("--label-any=pm-type:documentation,pm-type:validation,"
                       "pm-type:clarification,pm-type:chrome,pm-type:holistic_review")

        args.append(f"--label=pm-state:{TicketState.CREATED.value}")

        if cycle_number:
            args.append(f"--label=cycle:{cycle_number}")

        success, output = self._run_bd(args)

        if not success:
            logger.warning(f"Failed to list available tickets: {output}")
            return []

        return self._parse_pm_tickets_from_json(output)

    def get_tickets_by_state(
        self,
        state: TicketState,
        ticket_type: TicketType | None = None,
        cycle_number: int | None = None,
    ) -> list[ProgramManagerTicket]:
        """Query tickets by their workflow state.

        When using memory cache, filters from in-memory ticket storage.

        Args:
            state: The ticket state to filter by.
            ticket_type: Filter by ticket type (optional).
            cycle_number: Filter by cycle number (optional).

        Returns:
            List of ProgramManagerTicket objects in the specified state.

        Example:
            # Get all in-progress tickets
            in_progress = client.get_tickets_by_state(TicketState.IN_PROGRESS)

            # Get completed documentation tickets
            completed_docs = client.get_tickets_by_state(
                state=TicketState.COMPLETED,
                ticket_type=TicketType.DOCUMENTATION,
            )
        """
        # Memory cache mode - use lock for thread safety
        if self._use_memory_cache():
            with self._lock:
                tickets = []
                for ticket in self._pm_ticket_cache.values():
                    # Filter by state
                    if ticket.state != state:
                        continue
                    # Filter by ticket type if specified
                    if ticket_type and ticket.ticket_type != ticket_type:
                        continue
                    # Filter by cycle number if specified
                    if cycle_number and ticket.cycle_number != cycle_number:
                        continue
                    tickets.append(ticket)
                return tickets

        # Map state to bd status
        bd_status = TICKET_STATE_TO_BD_STATUS[state]

        args = ["list", "--json", f"--status={bd_status}"]

        # Filter by pm-state label for precision
        args.append(f"--label=pm-state:{state.value}")

        if ticket_type:
            args.append(f"--label=pm-type:{ticket_type.value}")

        if cycle_number:
            args.append(f"--label=cycle:{cycle_number}")

        success, output = self._run_bd(args)

        if not success:
            logger.warning(f"Failed to list tickets by state: {output}")
            return []

        return self._parse_pm_tickets_from_json(output)

    def _parse_pm_tickets_from_json(self, output: str) -> list[ProgramManagerTicket]:
        """Parse PM tickets from bd list --json output.

        Args:
            output: JSON output from bd list command.

        Returns:
            List of ProgramManagerTicket objects.
        """
        if not output or output.strip() == "":
            return []

        try:
            issues = json.loads(output)
        except json.JSONDecodeError:
            logger.warning(f"Failed to parse JSON from bd list: {output[:100]}...")
            return []

        if not isinstance(issues, list):
            return []

        tickets = []
        for issue in issues:
            ticket_id = issue.get("id", "")
            labels = issue.get("labels", [])
            bd_status = issue.get("status", "open")

            pm_ticket = ProgramManagerTicket.from_labels(
                ticket_id=ticket_id,
                labels=labels,
                bd_status=bd_status,
            )

            if pm_ticket:
                tickets.append(pm_ticket)

        return tickets

    def reset_orphaned_tickets(
        self,
        ticket_types: list[TicketType] | None = None,
    ) -> int:
        """Reset orphaned CLAIMED/IN_PROGRESS tickets back to CREATED.

        Call this after workers have finished to recover tickets that were
        claimed but never completed (e.g., due to worker crash or timeout).

        Args:
            ticket_types: Only reset tickets of these types. If None, resets all types.

        Returns:
            Number of tickets reset.

        Example:
            # After workers finish, recover any orphaned tickets
            reset_count = client.reset_orphaned_tickets()
            if reset_count > 0:
                logger.info(f"Reset {reset_count} orphaned tickets for retry")
        """
        reset_count = 0

        if self._use_memory_cache():
            with self._lock:
                for ticket in self._pm_ticket_cache.values():
                    # Reset CLAIMED or IN_PROGRESS tickets
                    if ticket.state in (TicketState.CLAIMED, TicketState.IN_PROGRESS):
                        # Filter by ticket type if specified
                        if ticket_types and ticket.ticket_type not in ticket_types:
                            continue

                        logger.info(
                            f"Resetting orphaned ticket {ticket.ticket_id} "
                            f"({ticket.file_name}) from {ticket.state.value} to created"
                        )
                        ticket.state = TicketState.CREATED
                        ticket.worker_id = None
                        ticket.claimed_at = None
                        reset_count += 1

                    # Also clean up data inconsistency: CREATED tickets with worker_id set
                    elif ticket.state == TicketState.CREATED and ticket.worker_id:
                        logger.debug(
                            f"Clearing stale worker_id from CREATED ticket {ticket.ticket_id}"
                        )
                        ticket.worker_id = None
                        ticket.claimed_at = None

            # Persist changes
            if reset_count > 0:
                self._save_to_disk()

            return reset_count

        # For bd CLI mode, query and update each ticket
        for state in (TicketState.CLAIMED, TicketState.IN_PROGRESS):
            tickets = self.get_tickets_by_state(state)
            for ticket in tickets:
                if ticket_types and ticket.ticket_type not in ticket_types:
                    continue

                logger.info(
                    f"Resetting orphaned ticket {ticket.ticket_id} "
                    f"({ticket.file_name}) from {state.value} to created"
                )
                if self.update_ticket_state(ticket.ticket_id, TicketState.CREATED):
                    reset_count += 1

        return reset_count


# Singleton instance for convenience
_default_client: BeadsClient | None = None


def get_beads_client(
    enabled: bool = True,
    dry_run: bool = False,
    tickets_file: Path | str | None = None,
    beads_dir: Path | str | None = None,
) -> BeadsClient:
    """Get or create the default beads client.

    Args:
        enabled: Whether to enable ticket creation.
        dry_run: Whether to log instead of execute.
        tickets_file: Path to persist tickets for crash recovery.
        beads_dir: Directory to run bd commands in (for isolated beads).
            If None, uses current working directory.

    Returns:
        BeadsClient instance.
    """
    global _default_client
    if _default_client is None:
        _default_client = BeadsClient(
            enabled=enabled,
            dry_run=dry_run,
            tickets_file=tickets_file,
            beads_dir=beads_dir,
        )
    return _default_client


def create_challenger_ticket(
    program_id: str,
    question: str,
    question_type: str,
    severity: str,
    team_id: int = 1,
    client: BeadsClient | None = None,
) -> str | None:
    """Create a beads ticket for a Challenger question.

    Args:
        program_id: ID of the program being documented.
        question: The question being asked.
        question_type: Type of question (CLARIFICATION, VERIFICATION, etc.).
        severity: Severity level (BLOCKING, IMPORTANT, MINOR).
        team_id: Team number for multi-team setups.
        client: Optional BeadsClient instance.

    Returns:
        Ticket ID if created, None otherwise.
    """
    client = client or get_beads_client()

    # Map severity to priority
    priority_map = {
        "BLOCKING": BeadsPriority.HIGH,
        "IMPORTANT": BeadsPriority.MEDIUM,
        "MINOR": BeadsPriority.LOW,
    }
    priority = priority_map.get(severity, BeadsPriority.MEDIUM)

    # Truncate question for title
    short_question = question[:80] + "..." if len(question) > 80 else question
    title = f"[T{team_id}] {program_id}: {question_type} - {short_question}"

    return client.create_ticket(
        title=title,
        ticket_type=BeadsTicketType.TASK,
        priority=priority,
        assignee="scribe",
        labels=[f"team-{team_id}", "challenger", program_id],
    )


def create_imperator_ticket(
    program_id: str,
    issue_type: str,
    description: str,
    section: str,
    priority: str,
    team_id: int = 1,
    client: BeadsClient | None = None,
) -> str | None:
    """Create a beads ticket for an Imperator Chrome ticket.

    Args:
        program_id: ID of the program being documented.
        issue_type: Type of issue (VAGUE, MISSING, WRONG, etc.).
        description: Description of the issue.
        section: Documentation section with the issue.
        priority: Priority level (CRITICAL, HIGH, MEDIUM).
        team_id: Team number for multi-team setups.
        client: Optional BeadsClient instance.

    Returns:
        Ticket ID if created, None otherwise.
    """
    client = client or get_beads_client()

    # Map priority to beads priority
    priority_map = {
        "CRITICAL": BeadsPriority.CRITICAL,
        "HIGH": BeadsPriority.HIGH,
        "MEDIUM": BeadsPriority.MEDIUM,
    }
    bd_priority = priority_map.get(priority, BeadsPriority.MEDIUM)

    # Truncate description for title
    short_desc = description[:60] + "..." if len(description) > 60 else description
    title = f"[T{team_id}] {program_id}/{section}: {issue_type} - {short_desc}"

    return client.create_ticket(
        title=title,
        ticket_type=BeadsTicketType.BUG,
        priority=bd_priority,
        assignee="scribe",
        labels=[f"team-{team_id}", "imperator", "chrome", program_id],
    )


def close_program_tickets(
    program_id: str,
    reason: str = "Documentation approved",
    client: BeadsClient | None = None,
) -> int:
    """Close all tickets related to a program.

    Args:
        program_id: ID of the program.
        reason: Reason for closing.
        client: Optional BeadsClient instance.

    Returns:
        Number of tickets closed.
    """
    client = client or get_beads_client()

    # Get tickets from cache that match program
    tickets_to_close = [
        tid for tid, ticket in client._ticket_cache.items()
        if ticket.labels and program_id in ticket.labels
    ]

    return client.close_tickets(tickets_to_close, reason=reason)
