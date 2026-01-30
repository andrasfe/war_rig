"""Unit tests for FeedbackInjector.

Tests:
- Loading and saving tickets
- Identifying CREATED tickets
- Merging human feedback with Imperator feedback
- Injection with various scenarios
"""

import json
from datetime import datetime

import pytest

from war_rig.feedback.injector import FeedbackInjector
from war_rig.feedback.models import (
    HumanFeedbackCategory,
    HumanFeedbackContext,
    HumanFeedbackNote,
    HumanFeedbackSeverity,
)


@pytest.fixture
def sample_tickets_data():
    """Create sample tickets data for testing."""
    return {
        "version": 1,
        "saved_at": datetime.utcnow().isoformat(),
        "ticket_count": 4,
        "current_cycle": 1,
        "tickets": [
            {
                "ticket_id": "mem-000001",
                "ticket_type": "documentation",
                "state": "created",
                "file_name": "PROG1.cbl",
                "program_id": "PROG1",
                "cycle_number": 1,
                "metadata": {
                    "feedback_context": {
                        "quality_notes": [
                            {
                                "note_id": "QN-001",
                                "category": "empty_section",
                                "severity": "medium",
                                "description": "Imperator note for PROG1",
                                "affected_sections": ["inputs"],
                                "affected_files": [],
                            }
                        ],
                        "critical_sections": ["purpose", "inputs"],
                        "required_citations": True,
                    }
                },
            },
            {
                "ticket_id": "mem-000002",
                "ticket_type": "documentation",
                "state": "created",
                "file_name": "PROG2.cbl",
                "program_id": "PROG2",
                "cycle_number": 1,
                "metadata": {},  # No feedback context
            },
            {
                "ticket_id": "mem-000003",
                "ticket_type": "documentation",
                "state": "in_progress",  # Not CREATED
                "file_name": "PROG3.cbl",
                "program_id": "PROG3",
                "cycle_number": 1,
                "metadata": {},
            },
            {
                "ticket_id": "mem-000004",
                "ticket_type": "documentation",
                "state": "completed",  # Not CREATED
                "file_name": "PROG4.cbl",
                "program_id": "PROG4",
                "cycle_number": 1,
                "metadata": {},
            },
        ],
    }


@pytest.fixture
def tickets_file(sample_tickets_data, tmp_path):
    """Create a temporary tickets file for testing."""
    tickets_path = tmp_path / ".war_rig_tickets.json"
    with open(tickets_path, "w") as f:
        json.dump(sample_tickets_data, f)
    return tickets_path


class TestFeedbackInjectorLoading:
    """Tests for loading and saving tickets."""

    def test_load_tickets(self, tickets_file, sample_tickets_data):
        """Test loading tickets from file."""
        injector = FeedbackInjector(tickets_file)
        data = injector.load()

        assert data["version"] == 1
        assert data["ticket_count"] == 4
        assert len(data["tickets"]) == 4

    def test_load_nonexistent_file(self, tmp_path):
        """Test loading from non-existent file raises error."""
        injector = FeedbackInjector(tmp_path / "nonexistent.json")

        with pytest.raises(FileNotFoundError):
            injector.load()

    def test_save_tickets(self, tickets_file):
        """Test saving tickets back to file."""
        injector = FeedbackInjector(tickets_file)
        injector.load()

        # Modify and save
        injector._data["current_cycle"] = 2
        injector.save()

        # Reload and verify
        injector2 = FeedbackInjector(tickets_file)
        data = injector2.load()
        assert data["current_cycle"] == 2

    def test_save_atomic(self, tickets_file):
        """Test that save uses atomic write (temp file + rename)."""
        injector = FeedbackInjector(tickets_file)
        injector.load()

        # Save should not leave temp files
        injector.save()

        # Check no temp files exist
        temp_files = list(tickets_file.parent.glob(".tickets_tmp_*"))
        assert len(temp_files) == 0

    def test_save_without_load_raises(self, tickets_file):
        """Test that save without load raises error."""
        injector = FeedbackInjector(tickets_file)

        with pytest.raises(RuntimeError):
            injector.save()


class TestFeedbackInjectorQueries:
    """Tests for querying ticket state."""

    def test_get_created_tickets(self, tickets_file):
        """Test getting only CREATED tickets."""
        injector = FeedbackInjector(tickets_file)
        injector.load()

        created = injector.get_created_tickets()

        assert len(created) == 2
        file_names = [t["file_name"] for t in created]
        assert "PROG1.cbl" in file_names
        assert "PROG2.cbl" in file_names
        assert "PROG3.cbl" not in file_names  # in_progress
        assert "PROG4.cbl" not in file_names  # completed

    def test_get_current_feedback_context_exists(self, tickets_file):
        """Test extracting existing feedback context from ticket."""
        injector = FeedbackInjector(tickets_file)
        injector.load()

        created = injector.get_created_tickets()
        prog1_ticket = next(t for t in created if t["file_name"] == "PROG1.cbl")

        ctx = injector.get_current_feedback_context(prog1_ticket)

        assert ctx is not None
        assert len(ctx.quality_notes) == 1
        assert ctx.quality_notes[0].note_id == "QN-001"
        assert ctx.critical_sections == ["purpose", "inputs"]

    def test_get_current_feedback_context_none(self, tickets_file):
        """Test extracting feedback context when none exists."""
        injector = FeedbackInjector(tickets_file)
        injector.load()

        created = injector.get_created_tickets()
        prog2_ticket = next(t for t in created if t["file_name"] == "PROG2.cbl")

        ctx = injector.get_current_feedback_context(prog2_ticket)

        assert ctx is None

    def test_get_ticket_summary(self, tickets_file):
        """Test getting ticket summary."""
        injector = FeedbackInjector(tickets_file)
        injector.load()

        summary = injector.get_ticket_summary()

        assert summary["total_tickets"] == 4
        assert summary["by_state"]["created"] == 2
        assert summary["by_state"]["in_progress"] == 1
        assert summary["by_state"]["completed"] == 1
        assert summary["current_cycle"] == 1
        assert len(summary["created_files"]) == 2


class TestFeedbackMerging:
    """Tests for merging human and Imperator feedback."""

    def test_merge_with_existing_context(self, tickets_file):
        """Test merging human feedback with existing Imperator context."""
        injector = FeedbackInjector(tickets_file)
        injector.load()

        # Create human feedback
        human_note = HumanFeedbackNote(
            note_id="HFB-001",
            category=HumanFeedbackCategory.INSTRUCTION,
            severity=HumanFeedbackSeverity.HIGH,
            description="Human instruction",
        )
        human_ctx = HumanFeedbackContext(notes=[human_note])

        # Get existing Imperator context
        created = injector.get_created_tickets()
        prog1_ticket = next(t for t in created if t["file_name"] == "PROG1.cbl")
        imperator_ctx = injector.get_current_feedback_context(prog1_ticket)

        # Merge
        merged = injector.merge_feedback(imperator_ctx, human_ctx, "PROG1.cbl")

        # Human notes should be prepended
        assert len(merged["quality_notes"]) == 2
        assert merged["quality_notes"][0]["note_id"] == "HFB-001"
        assert merged["quality_notes"][0]["source"] == "human"
        assert merged["quality_notes"][1]["note_id"] == "QN-001"

        # Imperator settings should be preserved
        assert merged["critical_sections"] == ["purpose", "inputs"]

    def test_merge_without_existing_context(self, tickets_file):
        """Test merging human feedback when no Imperator context exists."""
        injector = FeedbackInjector(tickets_file)
        injector.load()

        human_note = HumanFeedbackNote(
            category=HumanFeedbackCategory.DOMAIN_CONTEXT,
            description="Domain context note",
        )
        human_ctx = HumanFeedbackContext(notes=[human_note])

        # Merge with None
        merged = injector.merge_feedback(None, human_ctx, "PROG2.cbl")

        assert len(merged["quality_notes"]) == 1
        assert merged["quality_notes"][0]["source"] == "human"
        assert merged["critical_sections"] == []  # Empty default
        assert merged["required_citations"] is True  # Default

    def test_merge_critical_sections_override(self, tickets_file):
        """Test that human critical_sections_override replaces Imperator's."""
        injector = FeedbackInjector(tickets_file)
        injector.load()

        human_ctx = HumanFeedbackContext(
            notes=[],
            critical_sections_override=["purpose", "sql_operations"],
        )

        created = injector.get_created_tickets()
        prog1_ticket = next(t for t in created if t["file_name"] == "PROG1.cbl")
        imperator_ctx = injector.get_current_feedback_context(prog1_ticket)

        merged = injector.merge_feedback(imperator_ctx, human_ctx, "PROG1.cbl")

        # Human override should replace Imperator's
        assert merged["critical_sections"] == ["purpose", "sql_operations"]
        assert "inputs" not in merged["critical_sections"]

    def test_merge_global_instructions(self, tickets_file):
        """Test that human global_instructions are added."""
        injector = FeedbackInjector(tickets_file)
        injector.load()

        human_ctx = HumanFeedbackContext(
            notes=[],
            global_instructions="Always document SQLCODE checking",
        )

        merged = injector.merge_feedback(None, human_ctx, "PROG.cbl")

        assert merged["global_instructions"] == "Always document SQLCODE checking"


class TestFeedbackInjection:
    """Tests for the inject operation."""

    def test_inject_all_created(self, tickets_file):
        """Test injecting feedback into all CREATED tickets."""
        injector = FeedbackInjector(tickets_file)
        injector.load()

        human_note = HumanFeedbackNote(
            category=HumanFeedbackCategory.INSTRUCTION,
            description="Global instruction",
        )
        human_ctx = HumanFeedbackContext(notes=[human_note])

        result = injector.inject(human_ctx)

        assert result.success is True
        assert result.tickets_modified == 2
        assert "PROG1.cbl" in result.modified_files
        assert "PROG2.cbl" in result.modified_files

    def test_inject_specific_files(self, tickets_file):
        """Test injecting feedback into specific files only."""
        injector = FeedbackInjector(tickets_file)
        injector.load()

        human_note = HumanFeedbackNote(
            category=HumanFeedbackCategory.INSTRUCTION,
            description="Specific instruction",
        )
        human_ctx = HumanFeedbackContext(notes=[human_note])

        result = injector.inject(human_ctx, target_files=["PROG1.cbl"])

        assert result.success is True
        assert result.tickets_modified == 1
        assert result.tickets_skipped == 1
        assert "PROG1.cbl" in result.modified_files
        assert "PROG2.cbl" not in result.modified_files

    def test_inject_skip_files(self, tickets_file):
        """Test that skip_files cancels tickets."""
        injector = FeedbackInjector(tickets_file)
        injector.load()

        human_ctx = HumanFeedbackContext(
            notes=[],
            skip_files=["PROG1.cbl"],
        )

        result = injector.inject(human_ctx)

        assert result.success is True
        assert result.tickets_cancelled == 1
        assert result.tickets_modified == 1  # PROG2.cbl still modified

        # Verify ticket was cancelled
        injector2 = FeedbackInjector(tickets_file)
        data = injector2.load()
        prog1 = next(t for t in data["tickets"] if t["file_name"] == "PROG1.cbl")
        assert prog1["state"] == "cancelled"
        assert prog1["metadata"]["cancelled_reason"] == "Skipped by human feedback"

    def test_inject_prioritize_files(self, tickets_file):
        """Test that prioritize_files marks tickets as critical priority."""
        injector = FeedbackInjector(tickets_file)
        injector.load()

        human_ctx = HumanFeedbackContext(
            notes=[],
            prioritize_files=["PROG1.cbl"],
        )

        result = injector.inject(human_ctx)

        assert result.success is True

        # Verify priority was set
        injector2 = FeedbackInjector(tickets_file)
        data = injector2.load()
        prog1 = next(t for t in data["tickets"] if t["file_name"] == "PROG1.cbl")
        assert prog1["metadata"]["priority"] == "critical"

    def test_inject_sets_metadata_flags(self, tickets_file):
        """Test that injection sets metadata flags."""
        injector = FeedbackInjector(tickets_file)
        injector.load()

        human_ctx = HumanFeedbackContext(notes=[])

        result = injector.inject(human_ctx)

        # Verify metadata flags
        injector2 = FeedbackInjector(tickets_file)
        data = injector2.load()
        prog1 = next(t for t in data["tickets"] if t["file_name"] == "PROG1.cbl")

        assert prog1["metadata"]["human_feedback_injected"] is True
        assert "human_feedback_at" in prog1["metadata"]

    def test_inject_does_not_modify_non_created(self, tickets_file):
        """Test that injection does not modify non-CREATED tickets."""
        injector = FeedbackInjector(tickets_file)
        injector.load()

        human_note = HumanFeedbackNote(
            category=HumanFeedbackCategory.INSTRUCTION,
            description="Should not apply",
        )
        human_ctx = HumanFeedbackContext(notes=[human_note])

        injector.inject(human_ctx)

        # Verify in_progress and completed tickets not modified
        injector2 = FeedbackInjector(tickets_file)
        data = injector2.load()

        prog3 = next(t for t in data["tickets"] if t["file_name"] == "PROG3.cbl")
        assert "human_feedback_injected" not in prog3.get("metadata", {})

        prog4 = next(t for t in data["tickets"] if t["file_name"] == "PROG4.cbl")
        assert "human_feedback_injected" not in prog4.get("metadata", {})

    def test_inject_empty_context(self, tickets_file):
        """Test injecting empty feedback context still updates metadata."""
        injector = FeedbackInjector(tickets_file)
        injector.load()

        human_ctx = HumanFeedbackContext()  # Empty

        result = injector.inject(human_ctx)

        assert result.success is True
        assert result.tickets_modified == 2

    def test_inject_file_specific_notes(self, tickets_file):
        """Test that file-specific notes only apply to those files."""
        injector = FeedbackInjector(tickets_file)
        injector.load()

        # Create file-specific note
        note_for_prog1 = HumanFeedbackNote(
            category=HumanFeedbackCategory.INSTRUCTION,
            description="Only for PROG1",
            affected_files=["PROG1.cbl"],
        )
        human_ctx = HumanFeedbackContext(notes=[note_for_prog1])

        injector.inject(human_ctx)

        # Reload and verify
        injector2 = FeedbackInjector(tickets_file)
        data = injector2.load()

        prog1 = next(t for t in data["tickets"] if t["file_name"] == "PROG1.cbl")
        prog1_notes = prog1["metadata"]["feedback_context"]["quality_notes"]
        # Should have human note + imperator note
        human_notes = [n for n in prog1_notes if n.get("source") == "human"]
        assert len(human_notes) == 1

        prog2 = next(t for t in data["tickets"] if t["file_name"] == "PROG2.cbl")
        prog2_notes = prog2["metadata"]["feedback_context"]["quality_notes"]
        # Should have no human notes (file-specific was not for this file)
        human_notes = [n for n in prog2_notes if n.get("source") == "human"]
        assert len(human_notes) == 0
