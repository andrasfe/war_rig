#!/usr/bin/env python3
"""War Rig Ticket Status Monitor - Real-time CLI Dashboard.

A CLI utility that displays War Rig ticket status in real-time with in-place
terminal updates. Reads from a tickets JSON file and presents ticket counts
by type and state, highlighting stuck tickets.

Usage:
    python scripts/war_rig_status.py output/.war_rig_tickets.json
"""

from __future__ import annotations

import argparse
import json
import sys
import time
from collections import defaultdict
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Any

from rich.console import Console, Group
from rich.live import Live
from rich.panel import Panel
from rich.table import Table
from rich.text import Text

# Constants
REFRESH_INTERVAL = 2.0  # seconds
MAX_TICKET_DISPLAY = 15  # Maximum tickets to show in detail list

# Color scheme for ticket states
STATE_STYLES: dict[str, str] = {
    "completed": "green",
    "in_progress": "yellow",
    "created": "blue",
    "claimed": "cyan",
    "blocked": "red",
    "rework": "orange1",
    "cancelled": "dim",
}

# States considered "stuck" (needing attention)
STUCK_STATES: set[str] = {"blocked", "in_progress", "claimed"}

# Known ticket types for consistent ordering
TICKET_TYPES: list[str] = ["documentation", "validation", "clarification", "chrome", "system_overview"]

# Known states for consistent ordering in display
TICKET_STATES: list[str] = ["created", "claimed", "in_progress", "completed", "blocked", "rework", "cancelled"]

# Known file types for consistent ordering
FILE_TYPES: list[str] = ["COBOL", "JCL", "COPYBOOK", "PROC"]


def format_bytes(size_bytes: int | None) -> str:
    """Format bytes as human-readable string.

    Args:
        size_bytes: Number of bytes, or None.

    Returns:
        Human-readable string like "117 KB" or "-" if None.
    """
    if size_bytes is None:
        return "-"

    if size_bytes < 1024:
        return f"{size_bytes} B"
    elif size_bytes < 1024 * 1024:
        return f"{size_bytes / 1024:.0f} KB"
    elif size_bytes < 1024 * 1024 * 1024:
        return f"{size_bytes / (1024 * 1024):.1f} MB"
    else:
        return f"{size_bytes / (1024 * 1024 * 1024):.2f} GB"


def format_duration(seconds: float) -> str:
    """Format duration as human-readable string.

    Args:
        seconds: Duration in seconds.

    Returns:
        Human-readable string like "5m", "2h 30m", "1d 3h".
    """
    if seconds < 60:
        return f"{int(seconds)}s"
    elif seconds < 3600:
        minutes = int(seconds / 60)
        return f"{minutes}m"
    elif seconds < 86400:
        hours = int(seconds / 3600)
        minutes = int((seconds % 3600) / 60)
        if minutes > 0:
            return f"{hours}h {minutes}m"
        return f"{hours}h"
    else:
        days = int(seconds / 86400)
        hours = int((seconds % 86400) / 3600)
        if hours > 0:
            return f"{days}d {hours}h"
        return f"{days}d"


def parse_timestamp(ts_str: str | None) -> datetime | None:
    """Parse ISO timestamp string to datetime.

    Args:
        ts_str: ISO format timestamp string or None.

    Returns:
        datetime object or None if parsing fails.
    """
    if not ts_str:
        return None
    try:
        # Handle various ISO formats
        if "T" in ts_str:
            # Remove timezone info if present (for simplicity)
            ts_str = ts_str.replace("Z", "").split("+")[0]
            return datetime.fromisoformat(ts_str)
        return datetime.fromisoformat(ts_str)
    except (ValueError, TypeError):
        return None


@dataclass
class TicketSummary:
    """Aggregated ticket statistics."""

    by_type: dict[str, dict[str, int]] = field(default_factory=dict)  # type -> state -> count
    by_state: dict[str, int] = field(default_factory=dict)  # state -> count
    by_file_type: dict[str, dict[str, int | float]] = field(default_factory=dict)  # file_type -> {count, size_bytes}
    by_cycle: dict[int, dict[str, int]] = field(default_factory=dict)  # cycle -> state -> count
    stuck_tickets: list[dict[str, Any]] = field(default_factory=list)
    active_work: list[dict[str, Any]] = field(default_factory=list)  # in_progress tickets
    all_tickets: list[dict[str, Any]] = field(default_factory=list)  # all tickets with details
    current_phase: str = "idle"
    phase_detail: str = ""
    total: int = 0
    total_size_bytes: int = 0
    batch_id: str | None = None
    last_updated: datetime = field(default_factory=datetime.now)
    max_cycle: int = 1  # Highest cycle number found in tickets
    json_saved_at: str | None = None  # Timestamp from JSON file
    by_decision: dict[str, int] = field(default_factory=dict)  # decision -> count (WITNESSED, VALHALLA, etc.)


def load_tickets(path: Path) -> dict[str, Any] | None:
    """Load tickets from JSON file.

    Args:
        path: Path to the tickets JSON file.

    Returns:
        Parsed JSON dict or None if file missing/invalid.
    """
    if not path.exists():
        return None

    try:
        with open(path, "r", encoding="utf-8") as f:
            return json.load(f)
    except json.JSONDecodeError:
        return None
    except OSError:
        return None


def summarize_tickets(data: dict[str, Any]) -> TicketSummary:
    """Aggregate ticket data into summary statistics.

    Args:
        data: Parsed JSON from tickets file.

    Returns:
        TicketSummary with aggregated counts.
    """
    summary = TicketSummary(last_updated=datetime.now())

    # Extract JSON metadata
    summary.json_saved_at = data.get("saved_at")

    tickets = data.get("tickets", [])
    summary.total = len(tickets)

    # Initialize counters
    by_type: dict[str, dict[str, int]] = defaultdict(lambda: defaultdict(int))
    by_state: dict[str, int] = defaultdict(int)
    by_file_type: dict[str, dict[str, int | float]] = defaultdict(lambda: {"count": 0, "size_bytes": 0})
    by_cycle: dict[int, dict[str, int]] = defaultdict(lambda: defaultdict(int))
    by_decision: dict[str, int] = defaultdict(int)
    stuck_tickets: list[dict[str, Any]] = []
    active_work: list[dict[str, Any]] = []
    all_tickets: list[dict[str, Any]] = []
    total_size_bytes = 0
    batch_id: str | None = None
    max_cycle = 1

    for ticket in tickets:
        ticket_type = ticket.get("ticket_type", "unknown")
        state = ticket.get("state", "unknown")
        metadata = ticket.get("metadata", {})

        program_id = ticket.get("program_id")
        worker_id = ticket.get("worker_id")

        # Extract metadata fields (with graceful fallbacks)
        file_type = metadata.get("file_type", "unknown")
        size_bytes = metadata.get("size_bytes")

        # Capture batch_id from first ticket with it
        if batch_id is None and metadata.get("batch_id"):
            batch_id = metadata.get("batch_id")

        by_type[ticket_type][state] += 1
        by_state[state] += 1

        # Aggregate by file type
        by_file_type[file_type]["count"] += 1
        if size_bytes is not None:
            by_file_type[file_type]["size_bytes"] += size_bytes
            total_size_bytes += size_bytes

        # Track max cycle number and aggregate by cycle
        cycle_num = ticket.get("cycle_number", 1) or 1
        if cycle_num > max_cycle:
            max_cycle = cycle_num
        by_cycle[cycle_num][state] += 1

        # Track Imperator decisions (WITNESSED, VALHALLA, CHROME, FORCED)
        decision = ticket.get("decision")
        if decision:
            by_decision[decision] += 1

        # Build comprehensive ticket info for all_tickets list
        ticket_info = {
            "ticket_id": ticket.get("ticket_id", "unknown"),
            "program_id": program_id or "-",
            "file_name": ticket.get("file_name", "unknown"),
            "state": state,
            "ticket_type": ticket_type,
            "file_type": file_type,
            "size_bytes": size_bytes,
            "cycle_number": cycle_num,
            "worker_id": worker_id,
        }
        all_tickets.append(ticket_info)

        # Collect stuck tickets
        if state in STUCK_STATES:
            stuck_tickets.append({
                "ticket_id": ticket.get("ticket_id", "unknown"),
                "file_name": ticket.get("file_name", "unknown"),
                "state": state,
                "worker_id": worker_id,
                "ticket_type": ticket_type,
                "updated_at": ticket.get("updated_at"),
            })

        # Collect active work (in_progress only, not blocked)
        if state in ("in_progress", "claimed"):
            active_work.append({
                "ticket_id": ticket.get("ticket_id", "unknown"),
                "file_name": ticket.get("file_name", "unknown"),
                "state": state,
                "worker_id": worker_id,
                "ticket_type": ticket_type,
            })

    # Convert defaultdicts to regular dicts
    summary.by_type = {k: dict(v) for k, v in by_type.items()}
    summary.by_state = dict(by_state)
    summary.by_file_type = {k: dict(v) for k, v in by_file_type.items()}
    summary.by_cycle = {k: dict(v) for k, v in by_cycle.items()}
    summary.by_decision = dict(by_decision)
    summary.stuck_tickets = stuck_tickets
    summary.active_work = active_work
    summary.all_tickets = all_tickets
    summary.total_size_bytes = total_size_bytes
    summary.batch_id = batch_id
    summary.max_cycle = max_cycle

    # Infer current phase from ticket states
    summary.current_phase, summary.phase_detail = infer_phase(by_type, by_state, active_work)

    return summary


def infer_phase(
    by_type: dict[str, dict[str, int]],
    by_state: dict[str, int],
    active_work: list[dict[str, Any]],
) -> tuple[str, str]:
    """Infer current workflow phase from ticket states.

    Returns:
        Tuple of (phase_name, detail_string)
    """
    # Check what's actively being worked on
    active_types = {w["ticket_type"] for w in active_work}

    if active_work:
        if "documentation" in active_types:
            files = [w["file_name"] for w in active_work if w["ticket_type"] == "documentation"]
            return "DOCUMENTING", f"Processing {len(files)} file(s): {', '.join(files[:3])}"

        if "validation" in active_types:
            files = [w["file_name"] for w in active_work if w["ticket_type"] == "validation"]
            return "VALIDATING", f"Validating {len(files)} file(s): {', '.join(files[:3])}"

        if "chrome" in active_types:
            files = [w["file_name"] for w in active_work if w["ticket_type"] == "chrome"]
            return "REWORKING", f"Reworking {len(files)} file(s): {', '.join(files[:3])}"

        if "clarification" in active_types:
            files = [w["file_name"] for w in active_work if w["ticket_type"] == "clarification"]
            return "CLARIFYING", f"Clarifying {len(files)} file(s): {', '.join(files[:3])}"

    # Check for blocked tickets being rescued (Super-Scribe)
    blocked = by_state.get("blocked", 0)
    if blocked > 0:
        # Check if any blocked are chrome/clarification (Super-Scribe targets)
        chrome_blocked = by_type.get("chrome", {}).get("blocked", 0)
        clar_blocked = by_type.get("clarification", {}).get("blocked", 0)
        if chrome_blocked + clar_blocked > 0:
            return "SUPER-SCRIBE NEEDED", f"{chrome_blocked + clar_blocked} blocked ticket(s) awaiting rescue"

    # Check for pending work
    created = by_state.get("created", 0)
    if created > 0:
        # What types have created tickets?
        pending_types = []
        for ttype in ["documentation", "validation", "chrome", "clarification"]:
            if by_type.get(ttype, {}).get("created", 0) > 0:
                pending_types.append(ttype)
        return "PENDING", f"{created} ticket(s) waiting: {', '.join(pending_types)}"

    # Check completion
    completed = by_state.get("completed", 0)
    total = sum(by_state.values())
    if total > 0 and completed == total:
        return "COMPLETE", "All tickets processed"

    if total > 0:
        pct = (completed / total) * 100
        return "IDLE", f"{pct:.0f}% complete, waiting for next phase"

    return "IDLE", "No tickets"


def build_type_table(summary: TicketSummary) -> Table:
    """Build rich Table showing counts by ticket type.

    Columns: Type | Created | In Progress | Completed | Blocked | Total
    """
    table = Table(title="TICKETS BY TYPE", title_style="bold", show_header=True, header_style="bold")

    # Add columns
    table.add_column("Type", style="bold")
    table.add_column("Created", justify="right", style=STATE_STYLES.get("created", ""))
    table.add_column("In Progress", justify="right", style=STATE_STYLES.get("in_progress", ""))
    table.add_column("Completed", justify="right", style=STATE_STYLES.get("completed", ""))
    table.add_column("Blocked", justify="right", style=STATE_STYLES.get("blocked", ""))
    table.add_column("Total", justify="right", style="bold")

    # Track totals for each state
    totals: dict[str, int] = defaultdict(int)
    grand_total = 0

    # Add rows for each known type
    all_types = set(TICKET_TYPES) | set(summary.by_type.keys())
    for ticket_type in TICKET_TYPES:
        if ticket_type not in all_types:
            continue
        states = summary.by_type.get(ticket_type, {})

        created = states.get("created", 0)
        in_progress = states.get("in_progress", 0) + states.get("claimed", 0)
        completed = states.get("completed", 0)
        blocked = states.get("blocked", 0)
        row_total = sum(states.values())

        totals["created"] += created
        totals["in_progress"] += in_progress
        totals["completed"] += completed
        totals["blocked"] += blocked
        grand_total += row_total

        table.add_row(
            ticket_type,
            str(created),
            str(in_progress),
            str(completed),
            str(blocked),
            str(row_total),
        )

    # Add any unknown types
    for ticket_type in sorted(summary.by_type.keys()):
        if ticket_type in TICKET_TYPES:
            continue
        states = summary.by_type[ticket_type]

        created = states.get("created", 0)
        in_progress = states.get("in_progress", 0) + states.get("claimed", 0)
        completed = states.get("completed", 0)
        blocked = states.get("blocked", 0)
        row_total = sum(states.values())

        totals["created"] += created
        totals["in_progress"] += in_progress
        totals["completed"] += completed
        totals["blocked"] += blocked
        grand_total += row_total

        table.add_row(
            ticket_type,
            str(created),
            str(in_progress),
            str(completed),
            str(blocked),
            str(row_total),
        )

    # Add totals row
    table.add_section()
    table.add_row(
        "TOTAL",
        str(totals["created"]),
        str(totals["in_progress"]),
        str(totals["completed"]),
        str(totals["blocked"]),
        str(grand_total),
        style="bold",
    )

    return table


def build_state_table(summary: TicketSummary) -> Table:
    """Build rich Table showing counts by state.

    Columns: State | Count | Percentage
    """
    table = Table(title="TICKETS BY STATE", title_style="bold", show_header=True, header_style="bold")

    table.add_column("State", style="bold")
    table.add_column("Count", justify="right")
    table.add_column("Percentage", justify="right")

    # Determine ordering: known states first, then any others
    all_states = set(TICKET_STATES) | set(summary.by_state.keys())
    ordered_states = [s for s in TICKET_STATES if s in all_states]
    ordered_states.extend(sorted(s for s in all_states if s not in TICKET_STATES))

    total = summary.total or 1  # Avoid division by zero

    for state in ordered_states:
        count = summary.by_state.get(state, 0)
        if count == 0:
            continue

        percentage = (count / total) * 100
        style = STATE_STYLES.get(state, "")

        table.add_row(
            state,
            str(count),
            f"{percentage:.1f}%",
            style=style,
        )

    return table


# Decision styles (Imperator verdicts)
DECISION_STYLES: dict[str, str] = {
    "WITNESSED": "green",
    "VALHALLA": "gold1",
    "CHROME": "yellow",
    "FORCED": "orange1",
}

# Decision ordering for display
DECISION_ORDER: list[str] = ["WITNESSED", "VALHALLA", "CHROME", "FORCED"]


def build_decision_table(summary: TicketSummary) -> Table | None:
    """Build rich Table showing counts by Imperator decision.

    Columns: Decision | Count | Percentage

    Returns None if no decisions recorded.
    """
    if not summary.by_decision:
        return None

    table = Table(title="IMPERATOR DECISIONS", title_style="bold", show_header=True, header_style="bold")

    table.add_column("Decision", style="bold")
    table.add_column("Count", justify="right")
    table.add_column("Percentage", justify="right")

    total = sum(summary.by_decision.values()) or 1  # Avoid division by zero

    # Show decisions in defined order, then any others
    shown = set()
    for decision in DECISION_ORDER:
        count = summary.by_decision.get(decision, 0)
        if count == 0:
            continue
        shown.add(decision)

        percentage = (count / total) * 100
        style = DECISION_STYLES.get(decision, "")

        table.add_row(
            decision,
            str(count),
            f"{percentage:.1f}%",
            style=style,
        )

    # Show any unknown decisions
    for decision, count in sorted(summary.by_decision.items()):
        if decision in shown:
            continue

        percentage = (count / total) * 100
        table.add_row(
            decision,
            str(count),
            f"{percentage:.1f}%",
        )

    return table


def build_cycle_table(summary: TicketSummary) -> Table | None:
    """Build rich Table showing tickets by cycle with state breakdown.

    Columns: Cycle | Created | In Progress | Completed | Blocked | Total

    Returns None if only 1 cycle exists.
    """
    if len(summary.by_cycle) <= 1:
        return None

    table = Table(title="TICKETS BY CYCLE", title_style="bold", show_header=True, header_style="bold")

    table.add_column("Cycle", style="bold", justify="center")
    table.add_column("Created", justify="right", style=STATE_STYLES.get("created", ""))
    table.add_column("In Progress", justify="right", style=STATE_STYLES.get("in_progress", ""))
    table.add_column("Completed", justify="right", style=STATE_STYLES.get("completed", ""))
    table.add_column("Blocked", justify="right", style=STATE_STYLES.get("blocked", ""))
    table.add_column("Total", justify="right", style="bold")

    for cycle in sorted(summary.by_cycle.keys()):
        states = summary.by_cycle[cycle]
        created = states.get("created", 0)
        in_progress = states.get("in_progress", 0) + states.get("claimed", 0)
        completed = states.get("completed", 0)
        blocked = states.get("blocked", 0)
        row_total = sum(states.values())

        # Highlight rows with in_progress or blocked tickets
        row_style = ""
        if in_progress > 0:
            row_style = "yellow"
        elif blocked > 0:
            row_style = "red"

        table.add_row(
            str(cycle),
            str(created),
            str(in_progress),
            str(completed),
            str(blocked),
            str(row_total),
            style=row_style,
        )

    return table


def build_stuck_panel(summary: TicketSummary) -> Panel | None:
    """Build Panel listing stuck tickets.

    Returns None if no stuck tickets.
    """
    if not summary.stuck_tickets:
        return None

    lines: list[Text] = []
    now = datetime.now()

    # Sort by state priority: blocked first, then in_progress, then claimed
    # Within each state, sort by duration (longest first)
    def sort_key(t: dict) -> tuple:
        state_priority = {"blocked": 0, "in_progress": 1, "claimed": 2}
        priority = state_priority.get(t["state"], 99)
        # Sort by duration descending (negative seconds)
        duration_sort = 0
        updated_at = parse_timestamp(t.get("updated_at"))
        if updated_at:
            duration_sort = -(now - updated_at).total_seconds()
        return (priority, duration_sort, t["ticket_id"])

    sorted_tickets = sorted(summary.stuck_tickets, key=sort_key)

    for ticket in sorted_tickets:
        state = ticket["state"]
        style = STATE_STYLES.get(state, "")
        worker = ticket["worker_id"] or "unassigned"

        line = Text()
        line.append(f"[{state:11s}]", style=style)
        line.append(f"  {ticket['ticket_id']:12s}")
        line.append(f"  {ticket['file_name']:20s}")
        line.append(f"  worker: {worker:12s}")

        # Show duration for all stuck tickets
        updated_at = parse_timestamp(ticket.get("updated_at"))
        state_verb = "blocked" if state == "blocked" else "stuck"
        if updated_at:
            duration_seconds = (now - updated_at).total_seconds()
            duration_str = format_duration(duration_seconds)
            # Color code based on duration
            if duration_seconds > 3600:  # > 1 hour
                line.append(f"  {state_verb} for ", style="dim")
                line.append(f"{duration_str}", style="bold red")
            elif duration_seconds > 600:  # > 10 min
                line.append(f"  {state_verb} for ", style="dim")
                line.append(f"{duration_str}", style="yellow")
            else:
                line.append(f"  {state_verb} for {duration_str}", style="dim")
        else:
            # No timestamp available
            line.append(f"  {state_verb} for ", style="dim")
            line.append("?", style="dim italic")

        lines.append(line)

    content = Text("\n").join(lines)

    return Panel(
        content,
        title=f"STUCK TICKETS ({len(summary.stuck_tickets)})",
        title_align="left",
        border_style="red" if any(t["state"] == "blocked" for t in summary.stuck_tickets) else "yellow",
    )


def build_summary_header(summary: TicketSummary, path: Path) -> Panel:
    """Build summary header panel with batch info and totals.

    Args:
        summary: Ticket summary with metadata.
        path: Path to the tickets file.

    Returns:
        Rich Panel with batch summary.
    """
    header_text = Text()
    header_text.append("WAR RIG TICKET STATUS\n", style="bold magenta")
    header_text.append(f"File: {path}\n", style="dim")

    # Show JSON file's saved_at timestamp for data freshness
    if summary.json_saved_at:
        header_text.append(f"JSON saved: {summary.json_saved_at}  ", style="dim")
    header_text.append(f"Refreshed: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n", style="dim")

    # Add batch info and cycle number
    # Note: max_cycle is the highest cycle_number found in tickets, not necessarily
    # the "current" cycle being processed. Show this clearly to avoid confusion.
    if summary.batch_id:
        header_text.append(f"\nBatch: ", style="bold")
        header_text.append(f"{summary.batch_id}", style="cyan")
        header_text.append(f"  |  Latest Cycle: ", style="bold")
        header_text.append(f"{summary.max_cycle}\n", style="cyan")
    else:
        header_text.append(f"\nLatest Cycle: ", style="bold")
        header_text.append(f"{summary.max_cycle}\n", style="cyan")

    # Ticket count breakdown
    header_text.append(f"Total Tickets: ", style="bold")
    header_text.append(f"{summary.total}", style="white")

    # State breakdown inline
    completed = summary.by_state.get("completed", 0)
    in_progress = summary.by_state.get("in_progress", 0) + summary.by_state.get("claimed", 0)
    blocked = summary.by_state.get("blocked", 0)
    created = summary.by_state.get("created", 0)

    # Calculate incomplete (pending reassignment/retry)
    incomplete = summary.total - completed
    if incomplete > 0 and summary.total > 0:
        pct_complete = (completed / summary.total) * 100
        header_text.append(f"  |  ", style="dim")
        header_text.append(f"Progress: ", style="bold")
        header_text.append(f"{pct_complete:.0f}%", style="green" if pct_complete > 80 else "yellow" if pct_complete > 50 else "red")
        header_text.append(f" ({incomplete} pending)", style="dim")

    header_text.append("  (", style="dim")
    if completed:
        header_text.append(f"{completed} completed", style="green")
        header_text.append(", ", style="dim")
    if in_progress:
        header_text.append(f"{in_progress} in progress", style="yellow")
        header_text.append(", ", style="dim")
    if blocked:
        header_text.append(f"{blocked} blocked", style="red")
        header_text.append(", ", style="dim")
    if created:
        header_text.append(f"{created} pending", style="blue")
    header_text.append(")\n", style="dim")

    # Total size
    if summary.total_size_bytes > 0:
        header_text.append(f"Total Size: ", style="bold")
        header_text.append(f"{format_bytes(summary.total_size_bytes)}\n", style="white")

    return Panel(header_text, border_style="magenta")


def build_file_type_table(summary: TicketSummary) -> Table:
    """Build rich Table showing counts and sizes by file type.

    Columns: File Type | Count | Total Size
    """
    table = Table(title="FILES BY TYPE", title_style="bold", show_header=True, header_style="bold")

    table.add_column("File Type", style="bold")
    table.add_column("Count", justify="right")
    table.add_column("Total Size", justify="right")

    # Determine ordering: known file types first, then any others
    all_file_types = set(FILE_TYPES) | set(summary.by_file_type.keys())
    ordered_types = [ft for ft in FILE_TYPES if ft in summary.by_file_type]
    ordered_types.extend(sorted(ft for ft in summary.by_file_type if ft not in FILE_TYPES))

    total_count = 0
    total_size = 0

    for file_type in ordered_types:
        info = summary.by_file_type.get(file_type, {"count": 0, "size_bytes": 0})
        count = int(info.get("count", 0))
        size_bytes = int(info.get("size_bytes", 0))

        if count == 0:
            continue

        total_count += count
        total_size += size_bytes

        table.add_row(
            file_type,
            str(count),
            format_bytes(size_bytes) if size_bytes > 0 else "-",
        )

    # Add totals row if multiple types
    if len(ordered_types) > 1:
        table.add_section()
        table.add_row(
            "TOTAL",
            str(total_count),
            format_bytes(total_size) if total_size > 0 else "-",
            style="bold",
        )

    return table


def build_ticket_list_table(summary: TicketSummary, max_display: int = MAX_TICKET_DISPLAY) -> Table | None:
    """Build rich Table showing detailed ticket list.

    Columns: ID | Program | File | State | Type | Size | Cycle | Worker

    Args:
        summary: Ticket summary with all_tickets list.
        max_display: Maximum number of tickets to display.

    Returns:
        Rich Table with ticket details, or None if no tickets.
    """
    if not summary.all_tickets:
        return None

    # Sort tickets: by state priority, then by ticket_id
    state_priority = {
        "in_progress": 0,
        "claimed": 1,
        "blocked": 2,
        "created": 3,
        "rework": 4,
        "completed": 5,
        "cancelled": 6,
    }
    sorted_tickets = sorted(
        summary.all_tickets,
        key=lambda t: (state_priority.get(t["state"], 99), t["ticket_id"]),
    )

    remaining = len(sorted_tickets) - max_display
    display_tickets = sorted_tickets[:max_display]

    title = f"TICKET DETAILS ({len(summary.all_tickets)} total)"
    if remaining > 0:
        title = f"TICKET DETAILS (showing {max_display} of {len(summary.all_tickets)})"

    table = Table(title=title, title_style="bold", show_header=True, header_style="bold")

    table.add_column("Ticket ID", style="dim")
    table.add_column("Program", style="bold")
    table.add_column("File", no_wrap=True)
    table.add_column("State", justify="center")
    table.add_column("File Type", justify="center")
    table.add_column("Size", justify="right")
    table.add_column("Cycle", justify="center")
    table.add_column("Worker", style="cyan")

    for ticket in display_tickets:
        state = ticket["state"]
        state_style = STATE_STYLES.get(state, "")

        # Format state with color
        state_text = Text(state, style=state_style)

        # Format cycle number
        cycle = ticket.get("cycle_number")
        cycle_str = str(cycle) if cycle is not None else "-"

        # Format worker
        worker = ticket.get("worker_id") or "-"

        # Format size
        size_bytes = ticket.get("size_bytes")
        size_str = format_bytes(size_bytes)

        table.add_row(
            ticket["ticket_id"],
            str(ticket.get("program_id", "-")),
            ticket["file_name"],
            state_text,
            ticket.get("file_type", "-"),
            size_str,
            cycle_str,
            worker,
        )

    # Add "... and X more" row if truncated
    if remaining > 0:
        table.add_section()
        table.add_row(
            f"... and {remaining} more",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            style="dim italic",
        )

    return table


def build_activity_panel(summary: TicketSummary) -> Panel:
    """Build Panel showing current workflow phase and active work.

    Args:
        summary: Ticket summary with phase information.

    Returns:
        Rich Panel with activity information.
    """
    # Phase styling
    phase_styles = {
        "DOCUMENTING": ("bold cyan", "cyan"),
        "VALIDATING": ("bold yellow", "yellow"),
        "REWORKING": ("bold orange1", "orange1"),
        "CLARIFYING": ("bold magenta", "magenta"),
        "SUPER-SCRIBE NEEDED": ("bold red", "red"),
        "PENDING": ("bold blue", "blue"),
        "COMPLETE": ("bold green", "green"),
        "IDLE": ("dim", "dim"),
    }

    text_style, border_style = phase_styles.get(summary.current_phase, ("", "white"))

    content = Text()
    content.append(f"{summary.current_phase}\n", style=text_style)
    content.append(summary.phase_detail, style="dim")

    # Show active workers if any
    if summary.active_work:
        content.append("\n\n")
        content.append("Active Workers:\n", style="bold")
        for work in summary.active_work[:5]:  # Limit to 5
            worker = work["worker_id"] or "unassigned"
            content.append(f"  {worker}: ", style="cyan")
            content.append(f"{work['file_name']} ", style="white")
            content.append(f"({work['ticket_type']})\n", style="dim")
        if len(summary.active_work) > 5:
            content.append(f"  ... and {len(summary.active_work) - 5} more\n", style="dim")

    return Panel(
        content,
        title="CURRENT ACTIVITY",
        title_align="left",
        border_style=border_style,
    )


def build_display(summary: TicketSummary | None, path: Path, error_msg: str | None = None) -> Group:
    """Compose full display from components.

    Args:
        summary: Ticket summary data, or None if unavailable.
        path: Path to the tickets file.
        error_msg: Optional error message to display.

    Returns:
        Rich Group containing all display components.
    """
    components: list[Any] = []

    if error_msg:
        # Simple header when no data available
        header_text = Text()
        header_text.append("WAR RIG TICKET STATUS\n", style="bold magenta")
        header_text.append(f"File: {path}\n", style="dim")
        header_text.append(f"Last updated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}", style="dim")
        components.append(Panel(header_text, border_style="magenta"))
        components.append(Panel(Text(error_msg, style="yellow"), title="Status", border_style="yellow"))
    elif summary:
        # Enhanced header with batch info and totals
        components.append(build_summary_header(summary, path))

        # Current activity panel
        components.append(build_activity_panel(summary))
        components.append(Text())  # Spacer

        # Ticket type breakdown table
        components.append(build_type_table(summary))
        components.append(Text())  # Spacer

        # Cycle breakdown table (shows per-cycle state counts)
        cycle_table = build_cycle_table(summary)
        if cycle_table:
            components.append(cycle_table)
            components.append(Text())  # Spacer

        # File type breakdown table (shows COBOL, JCL, etc. with sizes)
        if summary.by_file_type:
            components.append(build_file_type_table(summary))
            components.append(Text())  # Spacer

        # Comprehensive ticket list table
        ticket_list_table = build_ticket_list_table(summary)
        if ticket_list_table:
            components.append(ticket_list_table)
            components.append(Text())  # Spacer

        # State breakdown table
        components.append(build_state_table(summary))
        components.append(Text())  # Spacer

        # Imperator decisions table (if any decisions recorded)
        decision_table = build_decision_table(summary)
        if decision_table:
            components.append(decision_table)
            components.append(Text())  # Spacer

        # Stuck tickets panel (highlighted separately)
        stuck_panel = build_stuck_panel(summary)
        if stuck_panel:
            components.append(stuck_panel)
    else:
        # Fallback header when summary is None but no error
        header_text = Text()
        header_text.append("WAR RIG TICKET STATUS\n", style="bold magenta")
        header_text.append(f"File: {path}\n", style="dim")
        header_text.append(f"Last updated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}", style="dim")
        components.append(Panel(header_text, border_style="magenta"))

    # Footer
    footer = Text()
    footer.append(f"Refreshing every {REFRESH_INTERVAL:.0f}s", style="dim")
    footer.append(" | ", style="dim")
    footer.append("Press Ctrl+C to exit", style="dim")
    components.append(footer)

    return Group(*components)


def run_monitor(path: Path, refresh_interval: float = REFRESH_INTERVAL) -> None:
    """Main monitoring loop with in-place updates.

    Args:
        path: Path to the tickets JSON file.
        refresh_interval: Seconds between refreshes.
    """
    console = Console()
    last_mtime: float | None = None

    # Initial load
    data = load_tickets(path)
    if data:
        summary = summarize_tickets(data)
        last_mtime = path.stat().st_mtime
        display = build_display(summary, path)
    else:
        display = build_display(None, path, error_msg="Waiting for tickets file...")

    try:
        with Live(display, console=console, refresh_per_second=4, screen=True) as live:
            while True:
                time.sleep(refresh_interval)

                # Check if file has changed
                if path.exists():
                    current_mtime = path.stat().st_mtime
                    if last_mtime is None or current_mtime != last_mtime:
                        data = load_tickets(path)
                        if data:
                            summary = summarize_tickets(data)
                            display = build_display(summary, path)
                            last_mtime = current_mtime
                        else:
                            display = build_display(None, path, error_msg="Error parsing tickets file")
                    else:
                        # Update timestamp even if data unchanged
                        if data:
                            summary = summarize_tickets(data)
                            display = build_display(summary, path)
                else:
                    display = build_display(None, path, error_msg="Waiting for tickets file...")
                    last_mtime = None

                live.update(display)

    except KeyboardInterrupt:
        console.print("\n[dim]Exiting...[/dim]")


def main() -> None:
    """CLI entry point."""
    parser = argparse.ArgumentParser(
        description="War Rig Ticket Status Monitor - Real-time CLI Dashboard",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
    python scripts/war_rig_status.py output/.war_rig_tickets.json
    python scripts/war_rig_status.py /path/to/tickets.json
        """,
    )
    parser.add_argument(
        "file",
        type=Path,
        help="Path to the tickets JSON file",
    )
    parser.add_argument(
        "--refresh",
        type=float,
        default=REFRESH_INTERVAL,
        help=f"Refresh interval in seconds (default: {REFRESH_INTERVAL})",
    )

    args = parser.parse_args()

    # Validate path exists or is valid
    file_path: Path = args.file
    if not file_path.is_absolute():
        file_path = Path.cwd() / file_path

    # Note: File doesn't need to exist yet - we'll wait for it
    if file_path.exists() and file_path.is_dir():
        print(f"Error: {file_path} is a directory, not a file", file=sys.stderr)
        sys.exit(1)

    run_monitor(file_path, args.refresh)


if __name__ == "__main__":
    main()
