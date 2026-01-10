"""Beads integration for War Rig ticket tracking.

This module provides integration with the beads task tracking system,
allowing agents to create and manage tickets during the documentation process.

Tickets are created when:
- Challenger asks blocking questions
- Imperator issues Chrome tickets

Tickets are closed when:
- Documentation reaches WITNESSED or VALHALLA status
- Questions are satisfactorily answered
"""

import logging
import subprocess
from dataclasses import dataclass
from enum import Enum
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


@dataclass
class BeadsTicket:
    """Represents a ticket in the beads system."""

    id: str
    title: str
    ticket_type: BeadsTicketType
    priority: BeadsPriority
    assignee: str | None = None
    labels: list[str] | None = None


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

    def __init__(self, enabled: bool = True, dry_run: bool = False):
        """Initialize the beads client.

        Args:
            enabled: Whether to actually create tickets (False for testing).
            dry_run: Log commands instead of executing them.
        """
        self.enabled = enabled
        self.dry_run = dry_run
        self._ticket_cache: dict[str, BeadsTicket] = {}

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

        try:
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=30,
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
        import re
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


# Singleton instance for convenience
_default_client: BeadsClient | None = None


def get_beads_client(enabled: bool = True, dry_run: bool = False) -> BeadsClient:
    """Get or create the default beads client.

    Args:
        enabled: Whether to enable ticket creation.
        dry_run: Whether to log instead of execute.

    Returns:
        BeadsClient instance.
    """
    global _default_client
    if _default_client is None:
        _default_client = BeadsClient(enabled=enabled, dry_run=dry_run)
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
