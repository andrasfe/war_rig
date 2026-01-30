"""Feedback injector for merging human feedback into War Rig tickets.

This module provides the FeedbackInjector class that handles:
- Loading tickets from the .war_rig_tickets.json file
- Filtering CREATED tickets eligible for feedback injection
- Merging human feedback with existing Imperator feedback
- Saving updated tickets back to the file with atomic writes
"""

import json
import logging
import os
import tempfile
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Any

from war_rig.beads import TicketState
from war_rig.feedback.models import HumanFeedbackContext
from war_rig.models.tickets import FeedbackContext

logger = logging.getLogger(__name__)


@dataclass
class InjectionResult:
    """Result of a feedback injection operation.

    Attributes:
        success: Whether the injection completed successfully
        tickets_modified: Number of tickets that were modified
        tickets_skipped: Number of tickets that were skipped
        tickets_cancelled: Number of tickets that were cancelled
        error: Error message if injection failed
        modified_files: List of file names whose tickets were modified
    """

    success: bool = True
    tickets_modified: int = 0
    tickets_skipped: int = 0
    tickets_cancelled: int = 0
    error: str | None = None
    modified_files: list[str] = field(default_factory=list)


class FeedbackInjector:
    """Service for injecting human feedback into War Rig tickets.

    The FeedbackInjector reads tickets from the War Rig tickets file,
    identifies CREATED tickets eligible for feedback injection, merges
    human feedback with existing Imperator feedback (with human taking
    precedence), and saves the updated tickets.

    Example:
        injector = FeedbackInjector(Path("output/.war_rig_tickets.json"))

        # Get current state
        tickets = injector.get_created_tickets()

        # Inject feedback
        human_ctx = HumanFeedbackContext(
            notes=[
                HumanFeedbackNote(
                    category="instruction",
                    description="Document all SQL cursors explicitly",
                )
            ],
            critical_sections_override=["purpose", "inputs", "sql_operations"],
        )
        result = injector.inject(human_ctx)
    """

    def __init__(self, tickets_file: Path):
        """Initialize the feedback injector.

        Args:
            tickets_file: Path to the .war_rig_tickets.json file.
        """
        self.tickets_file = tickets_file
        self._data: dict[str, Any] | None = None

    def load(self) -> dict[str, Any]:
        """Load tickets from the JSON file.

        Returns:
            The parsed tickets data structure.

        Raises:
            FileNotFoundError: If the tickets file does not exist.
            json.JSONDecodeError: If the file contains invalid JSON.
        """
        if not self.tickets_file.exists():
            raise FileNotFoundError(f"Tickets file not found: {self.tickets_file}")

        with open(self.tickets_file) as f:
            self._data = json.load(f)

        return self._data

    def save(self) -> None:
        """Save tickets back to the JSON file.

        Uses atomic write (write to temp file, then rename) to prevent
        corruption if interrupted.

        Raises:
            RuntimeError: If no data has been loaded.
        """
        if self._data is None:
            raise RuntimeError("No data loaded. Call load() first.")

        # Update timestamp
        self._data["saved_at"] = datetime.utcnow().isoformat()
        self._data["ticket_count"] = len(self._data.get("tickets", []))

        # Atomic write: write to temp file in same directory, then rename
        temp_fd, temp_path = tempfile.mkstemp(
            dir=self.tickets_file.parent,
            prefix=".tickets_tmp_",
            suffix=".json",
        )
        try:
            with os.fdopen(temp_fd, "w") as f:
                json.dump(self._data, f, indent=2)

            # Atomic rename (on POSIX systems)
            os.replace(temp_path, self.tickets_file)
            logger.info(f"Saved {self._data['ticket_count']} tickets to {self.tickets_file}")
        except Exception:
            # Clean up temp file on failure
            if os.path.exists(temp_path):
                os.unlink(temp_path)
            raise

    def get_created_tickets(self) -> list[dict[str, Any]]:
        """Get all tickets in CREATED state eligible for feedback injection.

        Returns:
            List of ticket dictionaries in CREATED state.
        """
        if self._data is None:
            self.load()

        tickets = self._data.get("tickets", [])
        created_tickets = []

        for ticket in tickets:
            state = ticket.get("state", "")
            if state == TicketState.CREATED.value:
                created_tickets.append(ticket)

        return created_tickets

    def get_current_feedback_context(
        self,
        ticket: dict[str, Any],
    ) -> FeedbackContext | None:
        """Extract the current Imperator feedback context from a ticket.

        Args:
            ticket: The ticket dictionary.

        Returns:
            FeedbackContext if present in metadata, None otherwise.
        """
        metadata = ticket.get("metadata", {})
        ctx_data = metadata.get("feedback_context")

        if not ctx_data:
            return None

        try:
            return FeedbackContext.model_validate(ctx_data)
        except Exception as e:
            logger.warning(f"Failed to parse feedback context: {e}")
            return None

    def merge_feedback(
        self,
        imperator_ctx: FeedbackContext | None,
        human_ctx: HumanFeedbackContext,
        file_name: str,
    ) -> dict[str, Any]:
        """Merge human feedback with existing Imperator feedback.

        Human feedback takes precedence on conflicts:
        - Human QualityNote entries are prepended to the list
        - Human critical_sections_override replaces Imperator's if set
        - Human global_instructions override existing instructions

        Args:
            imperator_ctx: Existing Imperator feedback (may be None).
            human_ctx: Human feedback to merge.
            file_name: The file name for filtering file-specific notes.

        Returns:
            Merged feedback context as a dictionary.
        """
        # Start with Imperator context or empty defaults
        if imperator_ctx:
            merged = imperator_ctx.model_dump()
        else:
            merged = {
                "quality_notes": [],
                "critical_sections": [],
                "required_citations": True,
                "cross_reference_required": False,
                "previous_cycle_issues": {},
                "augment_existing": True,
            }

        # Get human notes applicable to this file
        human_notes = human_ctx.get_notes_for_file(file_name)

        # Convert human notes to QualityNote-compatible dicts and prepend
        human_quality_notes = [note.to_quality_note_dict() for note in human_notes]
        merged["quality_notes"] = human_quality_notes + merged.get("quality_notes", [])

        # Override critical sections if human specified them
        if human_ctx.critical_sections_override is not None:
            merged["critical_sections"] = human_ctx.critical_sections_override

        # Add global instructions if specified
        if human_ctx.global_instructions:
            merged["global_instructions"] = human_ctx.global_instructions

        return merged

    def inject(
        self,
        human_ctx: HumanFeedbackContext,
        target_files: list[str] | None = None,
    ) -> InjectionResult:
        """Inject human feedback into CREATED tickets.

        Args:
            human_ctx: Human feedback context to inject.
            target_files: If specified, only inject into these files.
                If None, inject into all CREATED tickets.

        Returns:
            InjectionResult with statistics about the injection.
        """
        result = InjectionResult()

        try:
            if self._data is None:
                self.load()

            tickets = self._data.get("tickets", [])
            modified_indices: list[int] = []

            for i, ticket in enumerate(tickets):
                state = ticket.get("state", "")
                file_name = ticket.get("file_name", "")

                # Only process CREATED tickets
                if state != TicketState.CREATED.value:
                    continue

                # Filter by target files if specified
                if target_files and file_name not in target_files:
                    result.tickets_skipped += 1
                    continue

                # Check if file should be skipped
                if human_ctx.should_skip_file(file_name):
                    # Cancel the ticket
                    ticket["state"] = TicketState.CANCELLED.value
                    ticket["updated_at"] = datetime.utcnow().isoformat()
                    if "metadata" not in ticket:
                        ticket["metadata"] = {}
                    ticket["metadata"]["cancelled_reason"] = "Skipped by human feedback"
                    result.tickets_cancelled += 1
                    modified_indices.append(i)
                    logger.info(f"Cancelled ticket for {file_name} (skipped by human)")
                    continue

                # Get current Imperator feedback
                imperator_ctx = self.get_current_feedback_context(ticket)

                # Merge feedback
                merged_ctx = self.merge_feedback(imperator_ctx, human_ctx, file_name)

                # Update ticket metadata
                if "metadata" not in ticket:
                    ticket["metadata"] = {}
                ticket["metadata"]["feedback_context"] = merged_ctx
                ticket["metadata"]["human_feedback_injected"] = True
                ticket["metadata"]["human_feedback_at"] = datetime.utcnow().isoformat()

                # Mark as prioritized if applicable
                if human_ctx.is_prioritized(file_name):
                    ticket["metadata"]["priority"] = "critical"

                ticket["updated_at"] = datetime.utcnow().isoformat()
                modified_indices.append(i)
                result.tickets_modified += 1
                result.modified_files.append(file_name)
                logger.info(f"Injected feedback into ticket for {file_name}")

            # Update injected_at timestamp
            human_ctx.injected_at = datetime.utcnow()

            # Save changes
            self.save()

            logger.info(
                f"Feedback injection complete: {result.tickets_modified} modified, "
                f"{result.tickets_cancelled} cancelled, {result.tickets_skipped} skipped"
            )

        except Exception as e:
            result.success = False
            result.error = str(e)
            logger.error(f"Feedback injection failed: {e}")

        return result

    def get_ticket_summary(self) -> dict[str, Any]:
        """Get a summary of current ticket state.

        Returns:
            Dictionary with ticket counts by state and type.
        """
        if self._data is None:
            self.load()

        tickets = self._data.get("tickets", [])

        by_state: dict[str, int] = {}
        by_type: dict[str, int] = {}
        created_files: list[str] = []

        for ticket in tickets:
            state = ticket.get("state", "unknown")
            ticket_type = ticket.get("ticket_type", "unknown")

            by_state[state] = by_state.get(state, 0) + 1
            by_type[ticket_type] = by_type.get(ticket_type, 0) + 1

            if state == TicketState.CREATED.value:
                created_files.append(ticket.get("file_name", "unknown"))

        return {
            "total_tickets": len(tickets),
            "by_state": by_state,
            "by_type": by_type,
            "created_files": created_files,
            "current_cycle": self._data.get("current_cycle", 1),
            "saved_at": self._data.get("saved_at"),
        }
