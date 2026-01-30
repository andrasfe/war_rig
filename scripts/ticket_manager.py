#!/usr/bin/env python3
"""Ticket manager script for War Rig's in-memory ticket system (.war_rig_tickets.json).

Usage:
    python scripts/ticket_manager.py <json_file> list [--state=<state>] [--type=<type>]
    python scripts/ticket_manager.py <json_file> show <ticket_id>
    python scripts/ticket_manager.py <json_file> create --file=<file_name> [--type=<type>] [--state=<state>]
    python scripts/ticket_manager.py <json_file> edit <ticket_id> [--state=<state>] [--type=<type>]
    python scripts/ticket_manager.py <json_file> delete <ticket_id> [--force]
    python scripts/ticket_manager.py <json_file> reset <ticket_id> [--force]
    python scripts/ticket_manager.py <json_file> stats

Examples:
    # List all tickets
    python scripts/ticket_manager.py output/.war_rig_tickets.json list

    # List only blocked tickets
    python scripts/ticket_manager.py output/.war_rig_tickets.json list --state=blocked

    # List validation tickets
    python scripts/ticket_manager.py output/.war_rig_tickets.json list --type=validation

    # Show a specific ticket
    python scripts/ticket_manager.py output/.war_rig_tickets.json show mem-000001

    # Create a new documentation ticket
    python scripts/ticket_manager.py output/.war_rig_tickets.json create --file=cobol/NEWPROG.cbl --type=documentation

    # Reset a blocked ticket to created
    python scripts/ticket_manager.py output/.war_rig_tickets.json reset mem-000042

    # Edit a ticket's state
    python scripts/ticket_manager.py output/.war_rig_tickets.json edit mem-000001 --state=completed

    # Delete a ticket
    python scripts/ticket_manager.py output/.war_rig_tickets.json delete mem-000001 --force

    # Show statistics
    python scripts/ticket_manager.py output/.war_rig_tickets.json stats

Requires Python 3.9+
"""

from __future__ import annotations

import argparse
import json
import sys
from datetime import datetime
from pathlib import Path


def load_tickets_file(json_path: Path) -> dict:
    """Load the War Rig tickets JSON file."""
    if not json_path.exists():
        return {
            "version": 1,
            "saved_at": datetime.now().isoformat(),
            "ticket_count": 0,
            "current_cycle": 1,
            "tickets": []
        }
    with open(json_path) as f:
        return json.load(f)


def save_tickets_file(json_path: Path, data: dict) -> None:
    """Save the War Rig tickets JSON file."""
    data["saved_at"] = datetime.now().isoformat()
    data["ticket_count"] = len(data["tickets"])
    with open(json_path, 'w') as f:
        json.dump(data, f, indent=2)


def find_ticket(tickets: list[dict], ticket_id: str) -> tuple[int, dict | None]:
    """Find a ticket by ID. Returns (index, ticket) or (-1, None) if not found."""
    for i, ticket in enumerate(tickets):
        if ticket.get('ticket_id') == ticket_id:
            return i, ticket
    return -1, None


def get_next_ticket_id(tickets: list[dict]) -> str:
    """Generate the next mem-XXXXXX ticket ID."""
    max_num = 0
    for ticket in tickets:
        tid = ticket.get('ticket_id', '')
        if tid.startswith('mem-'):
            try:
                num = int(tid.split('-')[1])
                max_num = max(max_num, num)
            except (ValueError, IndexError):
                pass
    return f"mem-{max_num + 1:06d}"


def format_ticket(ticket: dict, verbose: bool = False) -> str:
    """Format a ticket for display."""
    tid = ticket.get('ticket_id', 'N/A')
    ttype = ticket.get('ticket_type', 'N/A')
    state = ticket.get('state', 'N/A')
    file_name = ticket.get('file_name', 'N/A')
    program_id = ticket.get('program_id', 'N/A')
    cycle = ticket.get('cycle_number', 'N/A')
    worker = ticket.get('worker_id', 'N/A')

    if verbose:
        lines = [
            f"Ticket ID:    {tid}",
            f"Type:         {ttype}",
            f"State:        {state}",
            f"File:         {file_name}",
            f"Program ID:   {program_id}",
            f"Cycle:        {cycle}",
            f"Worker:       {worker}",
        ]
        if ticket.get('parent_ticket_id'):
            lines.append(f"Parent:       {ticket['parent_ticket_id']}")
        if ticket.get('created_at'):
            lines.append(f"Created:      {ticket['created_at']}")
        if ticket.get('updated_at'):
            lines.append(f"Updated:      {ticket['updated_at']}")
        if ticket.get('decision'):
            lines.append(f"Decision:     {ticket['decision']}")
        if ticket.get('metadata'):
            meta = ticket['metadata']
            lines.append("Metadata:")
            for k, v in meta.items():
                lines.append(f"  {k}: {v}")
        return '\n'.join(lines)
    else:
        return f"[{state:12}] [{ttype:14}] {tid}: {file_name}"


def cmd_list(args, data: dict) -> int:
    """List tickets."""
    tickets = data.get('tickets', [])
    filtered = tickets

    if args.state:
        filtered = [t for t in filtered if t.get('state') == args.state]
    if args.type:
        filtered = [t for t in filtered if t.get('ticket_type') == args.type]

    if not filtered:
        print("No tickets found.")
        return 0

    print(f"Found {len(filtered)} ticket(s) (total: {len(tickets)}, cycle: {data.get('current_cycle', '?')}):\n")
    for ticket in filtered:
        print(format_ticket(ticket))
    return 0


def cmd_show(args, data: dict) -> int:
    """Show a specific ticket."""
    tickets = data.get('tickets', [])
    _, ticket = find_ticket(tickets, args.ticket_id)
    if not ticket:
        print(f"Error: Ticket '{args.ticket_id}' not found.", file=sys.stderr)
        return 1

    print(format_ticket(ticket, verbose=True))
    return 0


def cmd_create(args, data: dict, json_path: Path) -> int:
    """Create a new ticket."""
    tickets = data.get('tickets', [])
    now = datetime.now().isoformat()

    ticket_id = get_next_ticket_id(tickets)
    file_name = args.file
    program_id = Path(file_name).stem.upper()

    ticket = {
        "ticket_id": ticket_id,
        "ticket_type": args.type or "documentation",
        "state": args.state or "created",
        "file_name": file_name,
        "program_id": program_id,
        "cycle_number": data.get('current_cycle', 1),
        "worker_id": None,
        "parent_ticket_id": None,
        "metadata": {},
        "created_at": now,
        "updated_at": now,
        "decision": None
    }

    tickets.append(ticket)
    data['tickets'] = tickets
    save_tickets_file(json_path, data)

    print(f"Created ticket: {ticket_id}")
    print(format_ticket(ticket, verbose=True))
    return 0


def cmd_edit(args, data: dict, json_path: Path) -> int:
    """Edit an existing ticket."""
    tickets = data.get('tickets', [])
    idx, ticket = find_ticket(tickets, args.ticket_id)
    if not ticket:
        print(f"Error: Ticket '{args.ticket_id}' not found.", file=sys.stderr)
        return 1

    updated = False
    if args.state:
        ticket['state'] = args.state
        updated = True
    if args.type:
        ticket['ticket_type'] = args.type
        updated = True
    if args.cycle:
        ticket['cycle_number'] = int(args.cycle)
        updated = True

    if not updated:
        print("No changes specified. Use --state, --type, or --cycle.")
        return 1

    ticket['updated_at'] = datetime.now().isoformat()
    tickets[idx] = ticket
    data['tickets'] = tickets
    save_tickets_file(json_path, data)

    print(f"Updated ticket: {args.ticket_id}")
    print(format_ticket(ticket, verbose=True))
    return 0


def cmd_reset(args, data: dict, json_path: Path) -> int:
    """Reset a ticket to 'created' state."""
    tickets = data.get('tickets', [])
    idx, ticket = find_ticket(tickets, args.ticket_id)
    if not ticket:
        print(f"Error: Ticket '{args.ticket_id}' not found.", file=sys.stderr)
        return 1

    old_state = ticket.get('state')
    if old_state == 'created' and not args.force:
        print("Ticket is already in 'created' state.")
        return 0

    if not args.force:
        print(f"About to reset ticket from '{old_state}' to 'created':")
        print(format_ticket(ticket, verbose=True))
        response = input("\nAre you sure? (y/N): ")
        if response.lower() != 'y':
            print("Cancelled.")
            return 0

    ticket['state'] = 'created'
    ticket['worker_id'] = None
    ticket['updated_at'] = datetime.now().isoformat()
    tickets[idx] = ticket
    data['tickets'] = tickets
    save_tickets_file(json_path, data)

    print(f"Reset ticket: {args.ticket_id} (was: {old_state})")
    return 0


def cmd_delete(args, data: dict, json_path: Path) -> int:
    """Delete a ticket."""
    tickets = data.get('tickets', [])
    idx, ticket = find_ticket(tickets, args.ticket_id)
    if not ticket:
        print(f"Error: Ticket '{args.ticket_id}' not found.", file=sys.stderr)
        return 1

    if not args.force:
        print("About to delete ticket:")
        print(format_ticket(ticket, verbose=True))
        response = input("\nAre you sure? (y/N): ")
        if response.lower() != 'y':
            print("Cancelled.")
            return 0

    tickets.pop(idx)
    data['tickets'] = tickets
    save_tickets_file(json_path, data)

    print(f"Deleted ticket: {args.ticket_id}")
    return 0


def cmd_stats(args, data: dict) -> int:
    """Show statistics about tickets."""
    tickets = data.get('tickets', [])

    print("War Rig Tickets Statistics")
    print("=" * 40)
    print(f"File:          {args.json_file}")
    print(f"Saved at:      {data.get('saved_at', 'N/A')}")
    print(f"Current cycle: {data.get('current_cycle', 'N/A')}")
    print(f"Total tickets: {len(tickets)}")
    print()

    # Count by state
    states = {}
    for t in tickets:
        state = t.get('state', 'unknown')
        states[state] = states.get(state, 0) + 1

    print("By State:")
    for state, count in sorted(states.items()):
        print(f"  {state:15} {count:4}")
    print()

    # Count by type
    types = {}
    for t in tickets:
        ttype = t.get('ticket_type', 'unknown')
        types[ttype] = types.get(ttype, 0) + 1

    print("By Type:")
    for ttype, count in sorted(types.items()):
        print(f"  {ttype:15} {count:4}")

    return 0


def main():
    parser = argparse.ArgumentParser(
        description='Manage War Rig tickets (.war_rig_tickets.json).',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__
    )
    parser.add_argument('json_file', type=Path, help='Path to .war_rig_tickets.json')

    subparsers = parser.add_subparsers(dest='command', required=True)

    # list command
    list_parser = subparsers.add_parser('list', help='List tickets')
    list_parser.add_argument('--state', help='Filter by state (created, in_progress, completed, blocked, etc.)')
    list_parser.add_argument('--type', help='Filter by type (documentation, validation, clarification, chrome, etc.)')

    # show command
    show_parser = subparsers.add_parser('show', help='Show a specific ticket')
    show_parser.add_argument('ticket_id', help='Ticket ID (e.g., mem-000001)')

    # create command
    create_parser = subparsers.add_parser('create', help='Create a new ticket')
    create_parser.add_argument('--file', required=True, help='File name (e.g., cobol/PROG.cbl)')
    create_parser.add_argument('--type', choices=['documentation', 'validation', 'clarification', 'chrome', 'holistic_review', 'system_overview'],
                               help='Ticket type (default: documentation)')
    create_parser.add_argument('--state', choices=['created', 'in_progress', 'completed', 'blocked'],
                               help='Initial state (default: created)')

    # edit command
    edit_parser = subparsers.add_parser('edit', help='Edit an existing ticket')
    edit_parser.add_argument('ticket_id', help='Ticket ID to edit')
    edit_parser.add_argument('--state', choices=['created', 'in_progress', 'completed', 'blocked'], help='New state')
    edit_parser.add_argument('--type', help='New ticket type')
    edit_parser.add_argument('--cycle', type=int, help='New cycle number')

    # reset command
    reset_parser = subparsers.add_parser('reset', help='Reset a ticket to created state')
    reset_parser.add_argument('ticket_id', help='Ticket ID to reset')
    reset_parser.add_argument('--force', '-f', action='store_true', help='Skip confirmation')

    # delete command
    delete_parser = subparsers.add_parser('delete', help='Delete a ticket')
    delete_parser.add_argument('ticket_id', help='Ticket ID to delete')
    delete_parser.add_argument('--force', '-f', action='store_true', help='Skip confirmation')

    # stats command
    subparsers.add_parser('stats', help='Show ticket statistics')

    args = parser.parse_args()

    json_path = args.json_file

    # Load tickets
    data = load_tickets_file(json_path)

    # Execute command
    if args.command == 'list':
        return cmd_list(args, data)
    elif args.command == 'show':
        return cmd_show(args, data)
    elif args.command == 'create':
        return cmd_create(args, data, json_path)
    elif args.command == 'edit':
        return cmd_edit(args, data, json_path)
    elif args.command == 'reset':
        return cmd_reset(args, data, json_path)
    elif args.command == 'delete':
        return cmd_delete(args, data, json_path)
    elif args.command == 'stats':
        return cmd_stats(args, data)

    return 0


if __name__ == '__main__':
    sys.exit(main())
