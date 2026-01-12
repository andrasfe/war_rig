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
TICKET_TYPES: list[str] = ["documentation", "validation", "clarification", "chrome"]

# Known states for consistent ordering in display
TICKET_STATES: list[str] = ["created", "claimed", "in_progress", "completed", "blocked", "rework", "cancelled"]


@dataclass
class TicketSummary:
    """Aggregated ticket statistics."""

    by_type: dict[str, dict[str, int]] = field(default_factory=dict)  # type -> state -> count
    by_state: dict[str, int] = field(default_factory=dict)  # state -> count
    stuck_tickets: list[dict[str, Any]] = field(default_factory=list)
    active_work: list[dict[str, Any]] = field(default_factory=list)  # in_progress tickets
    current_phase: str = "idle"
    phase_detail: str = ""
    total: int = 0
    last_updated: datetime = field(default_factory=datetime.now)


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

    tickets = data.get("tickets", [])
    summary.total = len(tickets)

    # Initialize counters
    by_type: dict[str, dict[str, int]] = defaultdict(lambda: defaultdict(int))
    by_state: dict[str, int] = defaultdict(int)
    stuck_tickets: list[dict[str, Any]] = []

    active_work: list[dict[str, Any]] = []

    for ticket in tickets:
        ticket_type = ticket.get("ticket_type", "unknown")
        state = ticket.get("state", "unknown")

        by_type[ticket_type][state] += 1
        by_state[state] += 1

        # Collect stuck tickets
        if state in STUCK_STATES:
            stuck_tickets.append({
                "ticket_id": ticket.get("ticket_id", "unknown"),
                "file_name": ticket.get("file_name", "unknown"),
                "state": state,
                "worker_id": ticket.get("worker_id"),
                "ticket_type": ticket_type,
            })

        # Collect active work (in_progress only, not blocked)
        if state in ("in_progress", "claimed"):
            active_work.append({
                "ticket_id": ticket.get("ticket_id", "unknown"),
                "file_name": ticket.get("file_name", "unknown"),
                "state": state,
                "worker_id": ticket.get("worker_id"),
                "ticket_type": ticket_type,
            })

    # Convert defaultdicts to regular dicts
    summary.by_type = {k: dict(v) for k, v in by_type.items()}
    summary.by_state = dict(by_state)
    summary.stuck_tickets = stuck_tickets
    summary.active_work = active_work

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
            str(created) if created else "-",
            str(in_progress) if in_progress else "-",
            str(completed) if completed else "-",
            str(blocked) if blocked else "-",
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
            str(created) if created else "-",
            str(in_progress) if in_progress else "-",
            str(completed) if completed else "-",
            str(blocked) if blocked else "-",
            str(row_total),
        )

    # Add totals row
    table.add_section()
    table.add_row(
        "TOTAL",
        str(totals["created"]) if totals["created"] else "-",
        str(totals["in_progress"]) if totals["in_progress"] else "-",
        str(totals["completed"]) if totals["completed"] else "-",
        str(totals["blocked"]) if totals["blocked"] else "-",
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


def build_stuck_panel(summary: TicketSummary) -> Panel | None:
    """Build Panel listing stuck tickets.

    Returns None if no stuck tickets.
    """
    if not summary.stuck_tickets:
        return None

    lines: list[Text] = []

    # Sort by state priority: blocked first, then in_progress, then claimed
    state_priority = {"blocked": 0, "in_progress": 1, "claimed": 2}
    sorted_tickets = sorted(
        summary.stuck_tickets,
        key=lambda t: (state_priority.get(t["state"], 99), t["ticket_id"]),
    )

    for ticket in sorted_tickets:
        state = ticket["state"]
        style = STATE_STYLES.get(state, "")
        worker = ticket["worker_id"] or "unassigned"

        line = Text()
        line.append(f"[{state}]", style=style)
        line.append(f"  {ticket['ticket_id']:12s}")
        line.append(f"  {ticket['file_name']:20s}")
        line.append(f"  worker: {worker}")
        lines.append(line)

    content = Text("\n").join(lines)

    return Panel(
        content,
        title=f"STUCK TICKETS ({len(summary.stuck_tickets)})",
        title_align="left",
        border_style="red" if any(t["state"] == "blocked" for t in summary.stuck_tickets) else "yellow",
    )


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

    # Header
    header_text = Text()
    header_text.append("WAR RIG TICKET STATUS\n", style="bold magenta")
    header_text.append(f"File: {path}\n", style="dim")
    header_text.append(f"Last updated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}", style="dim")

    components.append(Panel(header_text, border_style="magenta"))

    if error_msg:
        components.append(Panel(Text(error_msg, style="yellow"), title="Status", border_style="yellow"))
    elif summary:
        # Current activity panel
        components.append(build_activity_panel(summary))
        components.append(Text())  # Spacer

        # Type breakdown table
        components.append(build_type_table(summary))
        components.append(Text())  # Spacer

        # State breakdown table
        components.append(build_state_table(summary))
        components.append(Text())  # Spacer

        # Stuck tickets panel
        stuck_panel = build_stuck_panel(summary)
        if stuck_panel:
            components.append(stuck_panel)

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
