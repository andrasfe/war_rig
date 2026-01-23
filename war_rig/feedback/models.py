"""Models for human feedback injection in War Rig.

This module defines Pydantic models for capturing human feedback that can be
injected into War Rig tickets. Human feedback supplements or overrides the
Imperator-generated feedback context.

Models:
- HumanFeedbackCategory: Categories of human feedback
- HumanFeedbackNote: A single human feedback observation
- HumanFeedbackContext: Collection of human feedback for injection
"""

from datetime import datetime
from enum import Enum
from typing import Any
from uuid import uuid4

from pydantic import BaseModel, ConfigDict, Field


class HumanFeedbackCategory(str, Enum):
    """Categories of feedback that humans can provide.

    These categories align with the types of guidance that are most useful
    for agents during documentation.
    """

    QUALITY_ISSUE = "quality_issue"  # Document quality problem (typos, formatting, etc.)
    DOMAIN_CONTEXT = "domain_context"  # Business/domain knowledge the agent lacks
    INSTRUCTION = "instruction"  # Specific instruction for the agent


class HumanFeedbackSeverity(str, Enum):
    """Severity levels for human feedback notes."""

    CRITICAL = "critical"  # Must be addressed - blocks approval
    HIGH = "high"  # Should be addressed before review
    MEDIUM = "medium"  # Should be addressed
    LOW = "low"  # Nice to have


class HumanFeedbackNote(BaseModel):
    """A single feedback observation from a human reviewer.

    Human feedback notes are converted into QualityNote entries when injected
    into tickets. They are marked with source="human" to distinguish them from
    Imperator-generated notes.

    Attributes:
        note_id: Unique identifier for tracking
        category: Type of feedback (quality_issue, domain_context, instruction)
        severity: How important this feedback is
        description: The actual feedback text
        affected_sections: Template sections this applies to
        affected_files: Specific files this applies to (empty = all CREATED tickets)
        guidance: Optional specific guidance for addressing the feedback
        created_at: When this feedback was added
    """

    model_config = ConfigDict(extra="ignore")

    note_id: str = Field(
        default_factory=lambda: f"HFB-{uuid4().hex[:8].upper()}",
        description="Unique identifier for the note",
    )
    category: HumanFeedbackCategory = Field(
        description="Category of feedback",
    )
    severity: HumanFeedbackSeverity = Field(
        default=HumanFeedbackSeverity.MEDIUM,
        description="Severity level",
    )
    description: str = Field(
        description="Human-readable description of the feedback",
    )
    affected_sections: list[str] = Field(
        default_factory=list,
        description="Template sections affected (e.g., 'inputs', 'outputs')",
    )
    affected_files: list[str] = Field(
        default_factory=list,
        description="Files affected (empty = all CREATED tickets)",
    )
    guidance: str | None = Field(
        default=None,
        description="Specific guidance for addressing the feedback",
    )
    created_at: datetime = Field(
        default_factory=datetime.utcnow,
        description="When this feedback was created",
    )

    def to_quality_note_dict(self) -> dict[str, Any]:
        """Convert to a QualityNote-compatible dictionary.

        This allows human feedback to be merged with Imperator feedback
        in the FeedbackContext structure that agents read.

        Returns:
            Dictionary compatible with QualityNote model, with source="human".
        """
        # Map HumanFeedbackCategory to QualityNoteCategory-compatible values
        category_mapping = {
            HumanFeedbackCategory.QUALITY_ISSUE: "other",  # Generic quality issue
            HumanFeedbackCategory.DOMAIN_CONTEXT: "other",  # Domain context as other
            HumanFeedbackCategory.INSTRUCTION: "other",  # Instructions as other
        }

        return {
            "note_id": self.note_id,
            "category": category_mapping.get(self.category, "other"),
            "severity": self.severity.value,
            "description": self.description,
            "affected_sections": self.affected_sections,
            "affected_files": self.affected_files,
            "guidance": self.guidance,
            "cycle_identified": 0,  # Human feedback is pre-cycle
            "source": "human",  # Mark as human-provided
        }


class HumanFeedbackContext(BaseModel):
    """Collection of human feedback for injection into tickets.

    This model aggregates all human feedback to be injected. It includes
    both quality notes and override settings that take precedence over
    Imperator-generated feedback.

    Attributes:
        notes: List of human feedback notes
        critical_sections_override: If set, replaces Imperator's critical_sections
        global_instructions: If set, overrides any existing global instructions
        skip_files: Files to skip (not inject feedback into)
        prioritize_files: Files to mark as high priority
        injected_at: Timestamp when feedback was injected
    """

    model_config = ConfigDict(extra="ignore")

    notes: list[HumanFeedbackNote] = Field(
        default_factory=list,
        description="Human feedback notes to inject",
    )
    critical_sections_override: list[str] | None = Field(
        default=None,
        description="If set, replaces Imperator's critical_sections list",
    )
    global_instructions: str | None = Field(
        default=None,
        description="Global instructions that override existing ones",
    )
    skip_files: list[str] = Field(
        default_factory=list,
        description="Files to skip (cancel their tickets)",
    )
    prioritize_files: list[str] = Field(
        default_factory=list,
        description="Files to mark as high priority",
    )
    injected_at: datetime | None = Field(
        default=None,
        description="When feedback was injected into tickets",
    )

    def get_notes_for_file(self, file_name: str) -> list[HumanFeedbackNote]:
        """Get human feedback notes applicable to a specific file.

        Args:
            file_name: The file to get notes for.

        Returns:
            List of notes that apply to this file (global + file-specific).
        """
        applicable = []
        for note in self.notes:
            # Global notes (no specific files) apply to all
            if not note.affected_files:
                applicable.append(note)
            # File-specific notes
            elif file_name in note.affected_files:
                applicable.append(note)
        return applicable

    def should_skip_file(self, file_name: str) -> bool:
        """Check if a file should be skipped (not processed).

        Args:
            file_name: The file to check.

        Returns:
            True if the file should be skipped.
        """
        return file_name in self.skip_files

    def is_prioritized(self, file_name: str) -> bool:
        """Check if a file has been marked as high priority.

        Args:
            file_name: The file to check.

        Returns:
            True if the file is prioritized.
        """
        return file_name in self.prioritize_files
