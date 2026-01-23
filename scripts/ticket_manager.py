#!/usr/bin/env python3
"""Ticket manager script for creating, editing, and deleting tickets in a JSONL file.

Requires Python 3.9+

Usage:
    python scripts/ticket_manager.py <jsonl_file> list [--status=<status>]
    python scripts/ticket_manager.py <jsonl_file> show <ticket_id>
    python scripts/ticket_manager.py <jsonl_file> create --title=<title> [--type=<type>] [--priority=<priority>] [--status=<status>]
    python scripts/ticket_manager.py <jsonl_file> edit <ticket_id> [--title=<title>] [--status=<status>] [--priority=<priority>]
    python scripts/ticket_manager.py <jsonl_file> delete <ticket_id> [--force]

Examples:
    # List all tickets
    python scripts/ticket_manager.py .beads/issues.jsonl list

    # List only open tickets
    python scripts/ticket_manager.py .beads/issues.jsonl list --status=open

    # Show a specific ticket
    python scripts/ticket_manager.py .beads/issues.jsonl show war_rig-abc1

    # Create a new ticket
    python scripts/ticket_manager.py .beads/issues.jsonl create --title="Fix bug" --type=bug --priority=1

    # Edit a ticket's status
    python scripts/ticket_manager.py .beads/issues.jsonl edit war_rig-abc1 --status=closed

    # Delete a ticket
    python scripts/ticket_manager.py .beads/issues.jsonl delete war_rig-abc1 --force
"""

from __future__ import annotations

import argparse
import json
import sys
from datetime import datetime, timezone
from pathlib import Path
import random
import string
from typing import Optional


def generate_ticket_id(prefix: str = "war_rig") -> str:
    """Generate a random ticket ID."""
    suffix = ''.join(random.choices(string.ascii_lowercase + string.digits, k=4))
    return f"{prefix}-{suffix}"


def load_tickets(jsonl_path: Path) -> list[dict]:
    """Load all tickets from a JSONL file."""
    tickets = []
    if jsonl_path.exists():
        with open(jsonl_path, 'r') as f:
            for line in f:
                line = line.strip()
                if line:
                    tickets.append(json.loads(line))
    return tickets


def save_tickets(jsonl_path: Path, tickets: list[dict]) -> None:
    """Save all tickets to a JSONL file."""
    with open(jsonl_path, 'w') as f:
        for ticket in tickets:
            f.write(json.dumps(ticket) + '\n')


def find_ticket(tickets: list[dict], ticket_id: str) -> tuple[int, Optional[dict]]:
    """Find a ticket by ID. Returns (index, ticket) or (-1, None) if not found."""
    for i, ticket in enumerate(tickets):
        if ticket.get('id') == ticket_id:
            return i, ticket
    return -1, None


def format_ticket(ticket: dict, verbose: bool = False) -> str:
    """Format a ticket for display."""
    tid = ticket.get('id', 'N/A')
    title = ticket.get('title', 'N/A')
    status = ticket.get('status', 'N/A')
    priority = ticket.get('priority', 'N/A')
    ttype = ticket.get('type', 'N/A')

    if verbose:
        lines = [
            f"ID:       {tid}",
            f"Title:    {title}",
            f"Status:   {status}",
            f"Priority: P{priority}" if isinstance(priority, int) else f"Priority: {priority}",
            f"Type:     {ttype}",
        ]
        if ticket.get('description'):
            lines.append(f"Description: {ticket['description']}")
        if ticket.get('created_at'):
            lines.append(f"Created:  {ticket['created_at']}")
        if ticket.get('updated_at'):
            lines.append(f"Updated:  {ticket['updated_at']}")
        if ticket.get('blocked_by'):
            lines.append(f"Blocked by: {', '.join(ticket['blocked_by'])}")
        if ticket.get('blocks'):
            lines.append(f"Blocks: {', '.join(ticket['blocks'])}")
        return '\n'.join(lines)
    else:
        return f"[{status:12}] [P{priority}] [{ttype:8}] {tid}: {title}"


def cmd_list(args, tickets: list[dict]) -> int:
    """List tickets."""
    filtered = tickets
    if args.status:
        filtered = [t for t in tickets if t.get('status') == args.status]

    if not filtered:
        print("No tickets found.")
        return 0

    print(f"Found {len(filtered)} ticket(s):\n")
    for ticket in filtered:
        print(format_ticket(ticket))
    return 0


def cmd_show(args, tickets: list[dict]) -> int:
    """Show a specific ticket."""
    _, ticket = find_ticket(tickets, args.ticket_id)
    if not ticket:
        print(f"Error: Ticket '{args.ticket_id}' not found.", file=sys.stderr)
        return 1

    print(format_ticket(ticket, verbose=True))
    return 0


def cmd_create(args, tickets: list[dict], jsonl_path: Path) -> int:
    """Create a new ticket."""
    now = datetime.now(timezone.utc).isoformat()

    ticket_id = generate_ticket_id()
    # Ensure unique ID
    while find_ticket(tickets, ticket_id)[1] is not None:
        ticket_id = generate_ticket_id()

    ticket = {
        'id': ticket_id,
        'title': args.title,
        'status': args.status or 'open',
        'type': args.type or 'task',
        'priority': int(args.priority) if args.priority else 2,
        'created_at': now,
        'updated_at': now,
    }

    if args.description:
        ticket['description'] = args.description

    tickets.append(ticket)
    save_tickets(jsonl_path, tickets)

    print(f"Created ticket: {ticket_id}")
    print(format_ticket(ticket, verbose=True))
    return 0


def cmd_edit(args, tickets: list[dict], jsonl_path: Path) -> int:
    """Edit an existing ticket."""
    idx, ticket = find_ticket(tickets, args.ticket_id)
    if not ticket:
        print(f"Error: Ticket '{args.ticket_id}' not found.", file=sys.stderr)
        return 1

    updated = False
    if args.title:
        ticket['title'] = args.title
        updated = True
    if args.status:
        ticket['status'] = args.status
        updated = True
    if args.priority:
        ticket['priority'] = int(args.priority)
        updated = True
    if args.type:
        ticket['type'] = args.type
        updated = True
    if args.description:
        ticket['description'] = args.description
        updated = True

    if not updated:
        print("No changes specified. Use --title, --status, --priority, --type, or --description.")
        return 1

    ticket['updated_at'] = datetime.now(timezone.utc).isoformat()
    tickets[idx] = ticket
    save_tickets(jsonl_path, tickets)

    print(f"Updated ticket: {args.ticket_id}")
    print(format_ticket(ticket, verbose=True))
    return 0


def cmd_delete(args, tickets: list[dict], jsonl_path: Path) -> int:
    """Delete a ticket."""
    idx, ticket = find_ticket(tickets, args.ticket_id)
    if not ticket:
        print(f"Error: Ticket '{args.ticket_id}' not found.", file=sys.stderr)
        return 1

    if not args.force:
        print(f"About to delete ticket:")
        print(format_ticket(ticket, verbose=True))
        response = input("\nAre you sure? (y/N): ")
        if response.lower() != 'y':
            print("Cancelled.")
            return 0

    tickets.pop(idx)
    save_tickets(jsonl_path, tickets)

    print(f"Deleted ticket: {args.ticket_id}")
    return 0


def main():
    parser = argparse.ArgumentParser(
        description='Manage tickets in a JSONL file.',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__
    )
    parser.add_argument('jsonl_file', type=Path, help='Path to the JSONL tickets file')

    subparsers = parser.add_subparsers(dest='command', required=True)

    # list command
    list_parser = subparsers.add_parser('list', help='List tickets')
    list_parser.add_argument('--status', help='Filter by status (open, closed, in_progress)')

    # show command
    show_parser = subparsers.add_parser('show', help='Show a specific ticket')
    show_parser.add_argument('ticket_id', help='Ticket ID to show')

    # create command
    create_parser = subparsers.add_parser('create', help='Create a new ticket')
    create_parser.add_argument('--title', required=True, help='Ticket title')
    create_parser.add_argument('--type', choices=['task', 'bug', 'feature', 'epic'], help='Ticket type (default: task)')
    create_parser.add_argument('--priority', type=int, choices=[0, 1, 2, 3, 4], help='Priority 0-4 (default: 2)')
    create_parser.add_argument('--status', choices=['open', 'in_progress', 'closed'], help='Status (default: open)')
    create_parser.add_argument('--description', help='Ticket description')

    # edit command
    edit_parser = subparsers.add_parser('edit', help='Edit an existing ticket')
    edit_parser.add_argument('ticket_id', help='Ticket ID to edit')
    edit_parser.add_argument('--title', help='New title')
    edit_parser.add_argument('--status', choices=['open', 'in_progress', 'closed'], help='New status')
    edit_parser.add_argument('--priority', type=int, choices=[0, 1, 2, 3, 4], help='New priority')
    edit_parser.add_argument('--type', choices=['task', 'bug', 'feature', 'epic'], help='New type')
    edit_parser.add_argument('--description', help='New description')

    # delete command
    delete_parser = subparsers.add_parser('delete', help='Delete a ticket')
    delete_parser.add_argument('ticket_id', help='Ticket ID to delete')
    delete_parser.add_argument('--force', '-f', action='store_true', help='Skip confirmation')

    args = parser.parse_args()

    jsonl_path = args.jsonl_file

    # Load tickets
    tickets = load_tickets(jsonl_path)

    # Execute command
    if args.command == 'list':
        return cmd_list(args, tickets)
    elif args.command == 'show':
        return cmd_show(args, tickets)
    elif args.command == 'create':
        return cmd_create(args, tickets, jsonl_path)
    elif args.command == 'edit':
        return cmd_edit(args, tickets, jsonl_path)
    elif args.command == 'delete':
        return cmd_delete(args, tickets, jsonl_path)

    return 0


if __name__ == '__main__':
    sys.exit(main())
