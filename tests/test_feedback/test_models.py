"""Unit tests for human feedback models.

Tests:
- HumanFeedbackNote model validation and serialization
- HumanFeedbackContext model functionality
- get_notes_for_file filtering
- Conversion to QualityNote format
"""

import pytest
from datetime import datetime

from war_rig.feedback.models import (
    HumanFeedbackCategory,
    HumanFeedbackNote,
    HumanFeedbackContext,
    HumanFeedbackSeverity,
)


class TestHumanFeedbackNote:
    """Tests for HumanFeedbackNote model."""

    def test_create_minimal_note(self):
        """Test creating a HumanFeedbackNote with minimal fields."""
        note = HumanFeedbackNote(
            category=HumanFeedbackCategory.INSTRUCTION,
            description="Document all SQL cursors",
        )
        assert note.category == HumanFeedbackCategory.INSTRUCTION
        assert note.severity == HumanFeedbackSeverity.MEDIUM  # default
        assert note.description == "Document all SQL cursors"
        assert note.note_id.startswith("HFB-")
        assert note.affected_sections == []
        assert note.affected_files == []
        assert note.guidance is None

    def test_create_full_note(self):
        """Test creating a HumanFeedbackNote with all fields."""
        note = HumanFeedbackNote(
            note_id="HFB-TEST001",
            category=HumanFeedbackCategory.QUALITY_ISSUE,
            severity=HumanFeedbackSeverity.CRITICAL,
            description="Missing DB2 error handling documentation",
            affected_sections=["sql_operations", "error_handling"],
            affected_files=["CBACT04C.cbl", "CBACT05C.cbl"],
            guidance="Check SQLCODE handling after each EXEC SQL block",
        )
        assert note.note_id == "HFB-TEST001"
        assert note.category == HumanFeedbackCategory.QUALITY_ISSUE
        assert note.severity == HumanFeedbackSeverity.CRITICAL
        assert "sql_operations" in note.affected_sections
        assert len(note.affected_files) == 2
        assert note.guidance is not None

    def test_note_ignores_extra_fields(self):
        """Test that extra fields are ignored (lenient parsing)."""
        note = HumanFeedbackNote(
            category=HumanFeedbackCategory.DOMAIN_CONTEXT,
            description="Test note",
            extra_field="should be ignored",
        )
        assert not hasattr(note, "extra_field")

    def test_to_quality_note_dict(self):
        """Test conversion to QualityNote-compatible dictionary."""
        note = HumanFeedbackNote(
            note_id="HFB-TEST001",
            category=HumanFeedbackCategory.INSTRUCTION,
            severity=HumanFeedbackSeverity.HIGH,
            description="Document all SQL cursors",
            affected_sections=["sql_operations"],
            affected_files=["PROG.cbl"],
            guidance="Check DECLARE CURSOR statements",
        )

        quality_dict = note.to_quality_note_dict()

        assert quality_dict["note_id"] == "HFB-TEST001"
        assert quality_dict["category"] == "other"  # Instructions map to other
        assert quality_dict["severity"] == "high"
        assert quality_dict["description"] == "Document all SQL cursors"
        assert quality_dict["affected_sections"] == ["sql_operations"]
        assert quality_dict["affected_files"] == ["PROG.cbl"]
        assert quality_dict["guidance"] == "Check DECLARE CURSOR statements"
        assert quality_dict["source"] == "human"  # Key marker for human feedback
        assert quality_dict["cycle_identified"] == 0  # Pre-cycle

    def test_all_categories(self):
        """Test all category enum values."""
        expected = ["quality_issue", "domain_context", "instruction"]
        for cat_str in expected:
            cat = HumanFeedbackCategory(cat_str)
            note = HumanFeedbackNote(category=cat, description="Test")
            assert note.category == cat

    def test_all_severities(self):
        """Test all severity enum values."""
        expected = ["critical", "high", "medium", "low"]
        for sev_str in expected:
            sev = HumanFeedbackSeverity(sev_str)
            note = HumanFeedbackNote(
                category=HumanFeedbackCategory.INSTRUCTION,
                severity=sev,
                description="Test",
            )
            assert note.severity == sev


class TestHumanFeedbackContext:
    """Tests for HumanFeedbackContext model."""

    def test_create_empty_context(self):
        """Test creating an empty HumanFeedbackContext."""
        ctx = HumanFeedbackContext()
        assert ctx.notes == []
        assert ctx.critical_sections_override is None
        assert ctx.global_instructions is None
        assert ctx.skip_files == []
        assert ctx.prioritize_files == []
        assert ctx.injected_at is None

    def test_create_full_context(self):
        """Test creating a HumanFeedbackContext with all fields."""
        notes = [
            HumanFeedbackNote(
                category=HumanFeedbackCategory.INSTRUCTION,
                description="Instruction 1",
            ),
            HumanFeedbackNote(
                category=HumanFeedbackCategory.DOMAIN_CONTEXT,
                severity=HumanFeedbackSeverity.HIGH,
                description="Domain context note",
            ),
        ]
        ctx = HumanFeedbackContext(
            notes=notes,
            critical_sections_override=["purpose", "inputs", "sql_operations"],
            global_instructions="Always document SQLCODE checking",
            skip_files=["SKIP.cbl"],
            prioritize_files=["PRIORITY.cbl"],
        )
        assert len(ctx.notes) == 2
        assert ctx.critical_sections_override == ["purpose", "inputs", "sql_operations"]
        assert ctx.global_instructions == "Always document SQLCODE checking"
        assert "SKIP.cbl" in ctx.skip_files
        assert "PRIORITY.cbl" in ctx.prioritize_files

    def test_get_notes_for_file_global(self):
        """Test get_notes_for_file returns global notes (no affected_files)."""
        global_note = HumanFeedbackNote(
            category=HumanFeedbackCategory.INSTRUCTION,
            description="Global instruction",
            affected_files=[],  # Empty = applies to all
        )
        specific_note = HumanFeedbackNote(
            category=HumanFeedbackCategory.QUALITY_ISSUE,
            description="Specific issue",
            affected_files=["OTHER.cbl"],
        )
        ctx = HumanFeedbackContext(notes=[global_note, specific_note])

        notes = ctx.get_notes_for_file("ANY.cbl")
        assert len(notes) == 1
        assert notes[0].description == "Global instruction"

    def test_get_notes_for_file_specific(self):
        """Test get_notes_for_file returns file-specific notes."""
        specific_note = HumanFeedbackNote(
            category=HumanFeedbackCategory.DOMAIN_CONTEXT,
            description="Context for PROG.cbl",
            affected_files=["PROG.cbl"],
        )
        ctx = HumanFeedbackContext(notes=[specific_note])

        notes = ctx.get_notes_for_file("PROG.cbl")
        assert len(notes) == 1

        notes = ctx.get_notes_for_file("OTHER.cbl")
        assert len(notes) == 0

    def test_get_notes_for_file_combined(self):
        """Test get_notes_for_file returns both global and file-specific notes."""
        global_note = HumanFeedbackNote(
            category=HumanFeedbackCategory.INSTRUCTION,
            description="Global instruction",
            affected_files=[],
        )
        specific_note = HumanFeedbackNote(
            category=HumanFeedbackCategory.QUALITY_ISSUE,
            description="Specific issue",
            affected_files=["PROG.cbl"],
        )
        ctx = HumanFeedbackContext(notes=[global_note, specific_note])

        notes = ctx.get_notes_for_file("PROG.cbl")
        assert len(notes) == 2

        notes = ctx.get_notes_for_file("OTHER.cbl")
        assert len(notes) == 1  # Only global

    def test_should_skip_file(self):
        """Test should_skip_file method."""
        ctx = HumanFeedbackContext(skip_files=["SKIP1.cbl", "SKIP2.cbl"])

        assert ctx.should_skip_file("SKIP1.cbl") is True
        assert ctx.should_skip_file("SKIP2.cbl") is True
        assert ctx.should_skip_file("KEEP.cbl") is False

    def test_is_prioritized(self):
        """Test is_prioritized method."""
        ctx = HumanFeedbackContext(prioritize_files=["IMPORTANT.cbl"])

        assert ctx.is_prioritized("IMPORTANT.cbl") is True
        assert ctx.is_prioritized("NORMAL.cbl") is False

    def test_context_ignores_extra_fields(self):
        """Test that extra fields in HumanFeedbackContext are ignored."""
        ctx = HumanFeedbackContext(
            notes=[],
            extra_field="should be ignored",
        )
        assert not hasattr(ctx, "extra_field")


class TestHumanFeedbackSerialization:
    """Tests for serialization/deserialization of feedback models."""

    def test_note_round_trip(self):
        """Test that HumanFeedbackNote can be serialized and deserialized."""
        note = HumanFeedbackNote(
            category=HumanFeedbackCategory.INSTRUCTION,
            severity=HumanFeedbackSeverity.HIGH,
            description="Test instruction",
            affected_sections=["inputs"],
        )

        # Serialize to dict
        note_dict = note.model_dump()

        # Deserialize back
        note2 = HumanFeedbackNote.model_validate(note_dict)

        assert note2.category == HumanFeedbackCategory.INSTRUCTION
        assert note2.severity == HumanFeedbackSeverity.HIGH
        assert note2.description == "Test instruction"
        assert note2.affected_sections == ["inputs"]

    def test_context_round_trip(self):
        """Test that HumanFeedbackContext can be serialized and deserialized."""
        note = HumanFeedbackNote(
            category=HumanFeedbackCategory.QUALITY_ISSUE,
            description="Quality issue",
        )
        ctx = HumanFeedbackContext(
            notes=[note],
            critical_sections_override=["purpose", "inputs"],
            global_instructions="Test instructions",
            skip_files=["SKIP.cbl"],
            prioritize_files=["PRIORITY.cbl"],
        )

        # Serialize to dict
        ctx_dict = ctx.model_dump()

        # Deserialize back
        ctx2 = HumanFeedbackContext.model_validate(ctx_dict)

        assert len(ctx2.notes) == 1
        assert ctx2.notes[0].category == HumanFeedbackCategory.QUALITY_ISSUE
        assert ctx2.critical_sections_override == ["purpose", "inputs"]
        assert ctx2.global_instructions == "Test instructions"
        assert ctx2.skip_files == ["SKIP.cbl"]
        assert ctx2.prioritize_files == ["PRIORITY.cbl"]
