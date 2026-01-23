#!/usr/bin/env python3
"""Human-in-the-loop feedback injection script for War Rig.

This script allows human reviewers to inject feedback into War Rig tickets
before agents process them. Feedback can include quality issues, domain
context, and specific instructions.

Usage:
    python scripts/human_feedback.py <tickets_file>
    python scripts/human_feedback.py output/.war_rig_tickets.json

The script presents an interactive menu for adding feedback notes,
viewing current Imperator feedback, and injecting feedback into CREATED
tickets.

Examples:
    # Start the feedback console
    python scripts/human_feedback.py output/.war_rig_tickets.json

    # The console allows you to:
    # - View current Imperator feedback
    # - Add quality notes (document issues, domain context, instructions)
    # - Prioritize specific files
    # - Skip/cancel files
    # - Inject all feedback and exit
"""

from __future__ import annotations

import argparse
import sys
from datetime import datetime
from pathlib import Path

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from rich.console import Console
from rich.panel import Panel
from rich.prompt import Confirm, Prompt
from rich.table import Table
from rich.text import Text

from war_rig.feedback.injector import FeedbackInjector, InjectionResult
from war_rig.feedback.models import (
    HumanFeedbackCategory,
    HumanFeedbackContext,
    HumanFeedbackNote,
    HumanFeedbackSeverity,
)
from war_rig.models.tickets import FeedbackContext

# Available template sections for feedback targeting
TEMPLATE_SECTIONS = [
    "purpose",
    "inputs",
    "outputs",
    "called_programs",
    "calling_context",
    "business_rules",
    "data_flow",
    "copybooks_used",
    "paragraphs",
    "error_handling",
    "sql_operations",
    "cics_operations",
    "open_questions",
]


class HumanFeedbackConsole:
    """Interactive console for human feedback injection.

    Provides a Rich-based terminal UI for viewing Imperator feedback,
    adding human feedback notes, and injecting feedback into tickets.
    """

    def __init__(self, tickets_file: Path):
        """Initialize the feedback console.

        Args:
            tickets_file: Path to the .war_rig_tickets.json file.
        """
        self.tickets_file = tickets_file
        self.console = Console()
        self.injector = FeedbackInjector(tickets_file)

        # Human feedback to inject
        self.human_context = HumanFeedbackContext()

        # Load tickets
        self.injector.load()

    def run(self) -> int:
        """Run the interactive feedback console.

        Returns:
            Exit code (0 for success, non-zero for error).
        """
        self._print_header()

        # Check for CREATED tickets
        created_tickets = self.injector.get_created_tickets()
        if not created_tickets:
            self.console.print(
                "[yellow]No CREATED tickets found. "
                "Feedback can only be injected into tickets that haven't started processing.[/yellow]"
            )
            return 0

        self.console.print(
            f"Found [green]{len(created_tickets)}[/green] CREATED tickets ready for feedback injection.\n"
        )

        # Show ticket summary
        self._show_ticket_summary()

        # Main menu loop
        while True:
            choice = self._show_menu()

            if choice == "1":
                self._add_quality_note()
            elif choice == "2":
                self._prioritize_files()
            elif choice == "3":
                self._skip_files()
            elif choice == "4":
                self._override_critical_sections()
            elif choice == "5":
                self._add_global_instructions()
            elif choice == "6":
                self._view_current_feedback()
            elif choice == "7":
                self._view_feedback_queue()
            elif choice == "8":
                return self._inject_and_exit()
            elif choice == "q" or choice == "Q":
                return self._quit_without_injecting()
            else:
                self.console.print("[red]Invalid choice. Please try again.[/red]")

    def _print_header(self) -> None:
        """Print the console header."""
        header = Text("War Rig Human Feedback Console", style="bold blue")
        self.console.print(Panel(header, border_style="blue"))
        self.console.print()

    def _show_ticket_summary(self) -> None:
        """Show a summary of ticket states."""
        summary = self.injector.get_ticket_summary()

        table = Table(title="Ticket Summary", show_header=True)
        table.add_column("State", style="cyan")
        table.add_column("Count", justify="right")

        for state, count in sorted(summary["by_state"].items()):
            style = "green" if state == "created" else "dim"
            table.add_row(state, str(count), style=style)

        self.console.print(table)
        self.console.print(f"Current cycle: {summary['current_cycle']}\n")

    def _show_menu(self) -> str:
        """Show the main menu and get user choice.

        Returns:
            The user's menu choice as a string.
        """
        menu_text = """
[bold]--- Add Human Feedback ---[/bold]

[1] Add quality note
[2] Prioritize specific file(s)
[3] Skip/cancel file(s)
[4] Override critical sections
[5] Add global instructions

[bold]--- View ---[/bold]

[6] View current Imperator feedback (for a ticket)
[7] View pending human feedback queue

[bold]--- Actions ---[/bold]

[8] Inject feedback and exit
[q] Quit without injecting
"""
        self.console.print(menu_text)
        return Prompt.ask("Choice", default="8")

    def _add_quality_note(self) -> None:
        """Add a quality note through interactive prompts."""
        self.console.print("\n[bold]Add Quality Note[/bold]\n")

        # Category selection
        self.console.print("Categories:")
        self.console.print("  [1] quality_issue - Document quality problem")
        self.console.print("  [2] domain_context - Business/domain knowledge")
        self.console.print("  [3] instruction - Specific agent instruction")

        cat_choice = Prompt.ask("Select category", choices=["1", "2", "3"], default="3")
        category_map = {
            "1": HumanFeedbackCategory.QUALITY_ISSUE,
            "2": HumanFeedbackCategory.DOMAIN_CONTEXT,
            "3": HumanFeedbackCategory.INSTRUCTION,
        }
        category = category_map[cat_choice]

        # Severity selection
        self.console.print("\nSeverity:")
        self.console.print("  [1] critical - Must be addressed")
        self.console.print("  [2] high - Should be addressed")
        self.console.print("  [3] medium - Should be addressed")
        self.console.print("  [4] low - Nice to have")

        sev_choice = Prompt.ask("Select severity", choices=["1", "2", "3", "4"], default="3")
        severity_map = {
            "1": HumanFeedbackSeverity.CRITICAL,
            "2": HumanFeedbackSeverity.HIGH,
            "3": HumanFeedbackSeverity.MEDIUM,
            "4": HumanFeedbackSeverity.LOW,
        }
        severity = severity_map[sev_choice]

        # Description
        description = Prompt.ask("Description (the feedback text)")
        if not description.strip():
            self.console.print("[red]Description cannot be empty. Note not added.[/red]")
            return

        # Optional guidance
        guidance = Prompt.ask("Guidance (optional, press Enter to skip)", default="")
        guidance = guidance.strip() if guidance.strip() else None

        # Affected sections
        self.console.print(f"\nAvailable sections: {', '.join(TEMPLATE_SECTIONS)}")
        sections_input = Prompt.ask("Affected sections (comma-separated, or Enter for all)", default="")
        affected_sections = []
        if sections_input.strip():
            affected_sections = [s.strip() for s in sections_input.split(",") if s.strip()]

        # Affected files
        created_files = [t.get("file_name", "") for t in self.injector.get_created_tickets()]
        self.console.print(f"\nCREATED files: {', '.join(created_files[:10])}")
        if len(created_files) > 10:
            self.console.print(f"  ... and {len(created_files) - 10} more")

        files_input = Prompt.ask("Affected files (comma-separated, or Enter for all)", default="")
        affected_files = []
        if files_input.strip():
            affected_files = [f.strip() for f in files_input.split(",") if f.strip()]

        # Create the note
        note = HumanFeedbackNote(
            category=category,
            severity=severity,
            description=description,
            guidance=guidance,
            affected_sections=affected_sections,
            affected_files=affected_files,
        )

        self.human_context.notes.append(note)
        self.console.print(f"\n[green]Added note: {note.note_id}[/green]")
        self._show_note(note)

    def _prioritize_files(self) -> None:
        """Add files to the priority list."""
        self.console.print("\n[bold]Prioritize Files[/bold]\n")

        created_files = [t.get("file_name", "") for t in self.injector.get_created_tickets()]
        self.console.print(f"CREATED files: {', '.join(created_files)}")

        files_input = Prompt.ask("Files to prioritize (comma-separated)")
        if not files_input.strip():
            return

        files = [f.strip() for f in files_input.split(",") if f.strip()]
        for f in files:
            if f not in self.human_context.prioritize_files:
                self.human_context.prioritize_files.append(f)

        self.console.print(f"[green]Prioritized: {', '.join(files)}[/green]")

    def _skip_files(self) -> None:
        """Add files to the skip list (their tickets will be cancelled)."""
        self.console.print("\n[bold]Skip/Cancel Files[/bold]\n")
        self.console.print("[yellow]Warning: Skipped files will have their tickets CANCELLED.[/yellow]\n")

        created_files = [t.get("file_name", "") for t in self.injector.get_created_tickets()]
        self.console.print(f"CREATED files: {', '.join(created_files)}")

        files_input = Prompt.ask("Files to skip (comma-separated)")
        if not files_input.strip():
            return

        files = [f.strip() for f in files_input.split(",") if f.strip()]

        if Confirm.ask(f"Cancel tickets for {len(files)} file(s)?"):
            for f in files:
                if f not in self.human_context.skip_files:
                    self.human_context.skip_files.append(f)
            self.console.print(f"[green]Will skip: {', '.join(files)}[/green]")

    def _override_critical_sections(self) -> None:
        """Set critical sections override."""
        self.console.print("\n[bold]Override Critical Sections[/bold]\n")
        self.console.print(
            "Critical sections are template sections that MUST be populated.\n"
            "Setting this will REPLACE the Imperator's critical sections list.\n"
        )

        self.console.print(f"Available sections: {', '.join(TEMPLATE_SECTIONS)}")

        current = self.human_context.critical_sections_override
        if current:
            self.console.print(f"Current override: {', '.join(current)}")

        sections_input = Prompt.ask(
            "Critical sections (comma-separated, or 'clear' to remove override)",
            default="purpose,inputs,outputs",
        )

        if sections_input.lower() == "clear":
            self.human_context.critical_sections_override = None
            self.console.print("[green]Critical sections override cleared.[/green]")
        else:
            sections = [s.strip() for s in sections_input.split(",") if s.strip()]
            self.human_context.critical_sections_override = sections
            self.console.print(f"[green]Critical sections set to: {', '.join(sections)}[/green]")

    def _add_global_instructions(self) -> None:
        """Add global instructions that apply to all tickets."""
        self.console.print("\n[bold]Add Global Instructions[/bold]\n")
        self.console.print(
            "Global instructions are added to ALL CREATED tickets and override\n"
            "any existing instructions from Imperator.\n"
        )

        if self.human_context.global_instructions:
            self.console.print(f"Current instructions: {self.human_context.global_instructions}\n")

        instructions = Prompt.ask("Global instructions (or 'clear' to remove)")

        if instructions.lower() == "clear":
            self.human_context.global_instructions = None
            self.console.print("[green]Global instructions cleared.[/green]")
        elif instructions.strip():
            self.human_context.global_instructions = instructions.strip()
            self.console.print("[green]Global instructions set.[/green]")

    def _view_current_feedback(self) -> None:
        """View current Imperator feedback for a specific ticket."""
        self.console.print("\n[bold]View Current Imperator Feedback[/bold]\n")

        created_tickets = self.injector.get_created_tickets()
        if not created_tickets:
            self.console.print("[yellow]No CREATED tickets available.[/yellow]")
            return

        # List available tickets
        table = Table(show_header=True)
        table.add_column("#", style="dim")
        table.add_column("File", style="cyan")
        table.add_column("Program ID")
        table.add_column("Type")

        for i, ticket in enumerate(created_tickets, 1):
            table.add_row(
                str(i),
                ticket.get("file_name", "unknown"),
                ticket.get("program_id", "unknown"),
                ticket.get("ticket_type", "unknown"),
            )

        self.console.print(table)

        choice = Prompt.ask(f"Select ticket (1-{len(created_tickets)})", default="1")
        try:
            idx = int(choice) - 1
            if 0 <= idx < len(created_tickets):
                ticket = created_tickets[idx]
                self._show_ticket_feedback(ticket)
            else:
                self.console.print("[red]Invalid selection.[/red]")
        except ValueError:
            self.console.print("[red]Invalid input.[/red]")

    def _show_ticket_feedback(self, ticket: dict) -> None:
        """Display the Imperator feedback for a ticket."""
        file_name = ticket.get("file_name", "unknown")
        self.console.print(f"\n[bold]Feedback for {file_name}[/bold]\n")

        feedback_ctx = self.injector.get_current_feedback_context(ticket)

        if not feedback_ctx:
            self.console.print("[dim]No Imperator feedback context found.[/dim]")
            return

        # Quality Notes
        if feedback_ctx.quality_notes:
            self.console.print("[bold cyan]Quality Notes:[/bold cyan]")
            for note in feedback_ctx.quality_notes:
                severity_style = "red" if note.severity == "critical" else "yellow"
                self.console.print(
                    f"  [{severity_style}][{note.note_id}][/{severity_style}] "
                    f"({note.category}, {note.severity}) {note.description}"
                )
                if note.guidance:
                    self.console.print(f"    [dim]Guidance: {note.guidance}[/dim]")
        else:
            self.console.print("[dim]No quality notes.[/dim]")

        # Critical Sections
        if feedback_ctx.critical_sections:
            self.console.print(
                f"\n[bold cyan]Critical Sections:[/bold cyan] {', '.join(feedback_ctx.critical_sections)}"
            )

        # Settings
        self.console.print(f"\n[bold cyan]Settings:[/bold cyan]")
        self.console.print(f"  Required citations: {feedback_ctx.required_citations}")
        self.console.print(f"  Cross-reference required: {feedback_ctx.cross_reference_required}")
        self.console.print(f"  Augment existing: {feedback_ctx.augment_existing}")

        # Previous cycle issues
        if feedback_ctx.previous_cycle_issues:
            self.console.print(f"\n[bold cyan]Previous Cycle Issues:[/bold cyan]")
            for fname, issues in feedback_ctx.previous_cycle_issues.items():
                self.console.print(f"  {fname}:")
                for issue in issues:
                    self.console.print(f"    - {issue}")

    def _view_feedback_queue(self) -> None:
        """View the pending human feedback queue."""
        self.console.print("\n[bold]Pending Human Feedback Queue[/bold]\n")

        if not self.human_context.notes and not self.human_context.skip_files and not self.human_context.prioritize_files:
            self.console.print("[dim]No feedback queued yet.[/dim]")
            return

        # Quality Notes
        if self.human_context.notes:
            self.console.print(f"[bold cyan]Quality Notes ({len(self.human_context.notes)}):[/bold cyan]")
            for note in self.human_context.notes:
                self._show_note(note)

        # Prioritized files
        if self.human_context.prioritize_files:
            self.console.print(
                f"\n[bold cyan]Prioritized Files:[/bold cyan] {', '.join(self.human_context.prioritize_files)}"
            )

        # Skipped files
        if self.human_context.skip_files:
            self.console.print(
                f"\n[bold yellow]Skipped Files (will be cancelled):[/bold yellow] "
                f"{', '.join(self.human_context.skip_files)}"
            )

        # Critical sections override
        if self.human_context.critical_sections_override is not None:
            self.console.print(
                f"\n[bold cyan]Critical Sections Override:[/bold cyan] "
                f"{', '.join(self.human_context.critical_sections_override)}"
            )

        # Global instructions
        if self.human_context.global_instructions:
            self.console.print(
                f"\n[bold cyan]Global Instructions:[/bold cyan] {self.human_context.global_instructions}"
            )

    def _show_note(self, note: HumanFeedbackNote) -> None:
        """Display a single feedback note."""
        severity_style = "red" if note.severity == HumanFeedbackSeverity.CRITICAL else "yellow"
        self.console.print(
            f"  [{severity_style}][{note.note_id}][/{severity_style}] "
            f"({note.category.value}, {note.severity.value})"
        )
        self.console.print(f"    {note.description}")
        if note.guidance:
            self.console.print(f"    [dim]Guidance: {note.guidance}[/dim]")
        if note.affected_sections:
            self.console.print(f"    [dim]Sections: {', '.join(note.affected_sections)}[/dim]")
        if note.affected_files:
            self.console.print(f"    [dim]Files: {', '.join(note.affected_files)}[/dim]")

    def _inject_and_exit(self) -> int:
        """Inject feedback and exit the console.

        Returns:
            Exit code (0 for success, 1 for error).
        """
        self.console.print("\n[bold]Inject Feedback[/bold]\n")

        # Confirm injection
        total_notes = len(self.human_context.notes)
        total_skip = len(self.human_context.skip_files)
        total_priority = len(self.human_context.prioritize_files)
        has_overrides = (
            self.human_context.critical_sections_override is not None
            or self.human_context.global_instructions is not None
        )

        if total_notes == 0 and total_skip == 0 and total_priority == 0 and not has_overrides:
            self.console.print("[yellow]No feedback to inject. Exiting.[/yellow]")
            return 0

        self.console.print(f"Summary:")
        self.console.print(f"  Quality notes: {total_notes}")
        self.console.print(f"  Files to skip: {total_skip}")
        self.console.print(f"  Files to prioritize: {total_priority}")
        self.console.print(f"  Has section/instruction overrides: {has_overrides}")

        if not Confirm.ask("\nProceed with injection?"):
            return self._quit_without_injecting()

        # Perform injection
        result = self.injector.inject(self.human_context)

        if result.success:
            self.console.print("\n[green]Feedback injection successful![/green]")
            self.console.print(f"  Tickets modified: {result.tickets_modified}")
            self.console.print(f"  Tickets cancelled: {result.tickets_cancelled}")
            self.console.print(f"  Tickets skipped: {result.tickets_skipped}")

            if result.modified_files:
                self.console.print(f"\n[dim]Modified files: {', '.join(result.modified_files[:10])}")
                if len(result.modified_files) > 10:
                    self.console.print(f"  ... and {len(result.modified_files) - 10} more[/dim]")

            return 0
        else:
            self.console.print(f"\n[red]Feedback injection failed: {result.error}[/red]")
            return 1

    def _quit_without_injecting(self) -> int:
        """Quit without injecting feedback.

        Returns:
            Exit code 0.
        """
        if self.human_context.notes or self.human_context.skip_files or self.human_context.prioritize_files:
            if not Confirm.ask("You have pending feedback. Quit without injecting?"):
                return self.run()  # Go back to menu

        self.console.print("\n[dim]Exiting without injection.[/dim]")
        return 0


def main() -> int:
    """Main entry point for the human feedback script.

    Returns:
        Exit code (0 for success, non-zero for error).
    """
    parser = argparse.ArgumentParser(
        description="War Rig Human Feedback Injection Console",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    parser.add_argument(
        "tickets_file",
        type=Path,
        help="Path to .war_rig_tickets.json file",
    )

    args = parser.parse_args()

    # Validate tickets file
    if not args.tickets_file.exists():
        print(f"Error: Tickets file not found: {args.tickets_file}", file=sys.stderr)
        return 1

    # Run the console
    console = HumanFeedbackConsole(args.tickets_file)
    return console.run()


if __name__ == "__main__":
    sys.exit(main())
