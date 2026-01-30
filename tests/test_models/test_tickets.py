"""Unit tests for ticket models including feedback loop functionality.

Tests:
- QualityNote model validation
- FeedbackContext model functionality
- get_notes_for_file filtering
- get_critical_notes filtering
"""

from war_rig.models.tickets import (
    FeedbackContext,
    QualityNote,
    QualityNoteCategory,
    QualityNoteSeverity,
)


class TestQualityNote:
    """Tests for QualityNote model."""

    def test_create_quality_note_minimal(self):
        """Test creating a QualityNote with minimal fields."""
        note = QualityNote(
            category="empty_section",
            description="The inputs section is empty",
        )
        assert note.category == "empty_section"
        assert note.severity == "medium"  # default
        assert note.description == "The inputs section is empty"
        assert note.note_id.startswith("QN-")

    def test_create_quality_note_full(self):
        """Test creating a QualityNote with all fields."""
        note = QualityNote(
            note_id="QN-TEST001",
            category="missing_citation",
            severity="critical",
            description="Claims without line number citations",
            affected_sections=["inputs", "outputs"],
            affected_files=["PROG1.cbl", "PROG2.cbl"],
            guidance="Add line number citations for all claims",
            cycle_identified=2,
        )
        assert note.note_id == "QN-TEST001"
        assert note.category == "missing_citation"
        assert note.severity == "critical"
        assert "inputs" in note.affected_sections
        assert len(note.affected_files) == 2
        assert note.guidance is not None

    def test_quality_note_ignores_extra_fields(self):
        """Test that extra fields are ignored (lenient parsing)."""
        note = QualityNote(
            category="other",
            description="Test note",
            extra_field="should be ignored",
            another_extra=123,
        )
        assert note.category == "other"
        assert not hasattr(note, "extra_field")


class TestFeedbackContext:
    """Tests for FeedbackContext model."""

    def test_create_empty_feedback_context(self):
        """Test creating an empty FeedbackContext."""
        ctx = FeedbackContext()
        assert ctx.quality_notes == []
        assert ctx.critical_sections == []
        assert ctx.required_citations is True
        assert ctx.cross_reference_required is False
        assert ctx.augment_existing is True

    def test_create_full_feedback_context(self):
        """Test creating a FeedbackContext with all fields."""
        notes = [
            QualityNote(
                category="empty_section",
                description="Empty inputs",
                affected_sections=["inputs"],
            ),
            QualityNote(
                category="missing_citation",
                severity="critical",
                description="No citations",
            ),
        ]
        ctx = FeedbackContext(
            quality_notes=notes,
            critical_sections=["inputs", "outputs", "purpose"],
            required_citations=True,
            cross_reference_required=True,
            previous_cycle_issues={"PROG.cbl": ["Issue 1", "Issue 2"]},
            augment_existing=True,
        )
        assert len(ctx.quality_notes) == 2
        assert "inputs" in ctx.critical_sections
        assert ctx.cross_reference_required is True
        assert len(ctx.previous_cycle_issues["PROG.cbl"]) == 2

    def test_get_notes_for_file_global(self):
        """Test get_notes_for_file returns global notes (no affected_files)."""
        global_note = QualityNote(
            category="missing_citation",
            description="Global issue",
            affected_files=[],  # Empty = applies to all
        )
        specific_note = QualityNote(
            category="empty_section",
            description="Specific issue",
            affected_files=["OTHER.cbl"],
        )
        ctx = FeedbackContext(quality_notes=[global_note, specific_note])

        notes = ctx.get_notes_for_file("ANY.cbl")
        assert len(notes) == 1
        assert notes[0].description == "Global issue"

    def test_get_notes_for_file_specific(self):
        """Test get_notes_for_file returns file-specific notes."""
        specific_note = QualityNote(
            category="empty_section",
            description="Specific issue for PROG.cbl",
            affected_files=["PROG.cbl"],
        )
        ctx = FeedbackContext(quality_notes=[specific_note])

        notes = ctx.get_notes_for_file("PROG.cbl")
        assert len(notes) == 1

        notes = ctx.get_notes_for_file("OTHER.cbl")
        assert len(notes) == 0

    def test_get_notes_for_file_combined(self):
        """Test get_notes_for_file returns both global and file-specific notes."""
        global_note = QualityNote(
            category="missing_citation",
            description="Global issue",
            affected_files=[],
        )
        specific_note = QualityNote(
            category="empty_section",
            description="Specific issue",
            affected_files=["PROG.cbl"],
        )
        ctx = FeedbackContext(quality_notes=[global_note, specific_note])

        notes = ctx.get_notes_for_file("PROG.cbl")
        assert len(notes) == 2

        notes = ctx.get_notes_for_file("OTHER.cbl")
        assert len(notes) == 1  # Only global

    def test_get_critical_notes(self):
        """Test get_critical_notes returns only critical severity notes."""
        critical_note = QualityNote(
            category="empty_section",
            severity="critical",
            description="Critical issue",
        )
        high_note = QualityNote(
            category="vague_content",
            severity="high",
            description="High priority issue",
        )
        medium_note = QualityNote(
            category="other",
            severity="medium",
            description="Medium issue",
        )
        ctx = FeedbackContext(quality_notes=[critical_note, high_note, medium_note])

        critical_notes = ctx.get_critical_notes()
        assert len(critical_notes) == 1
        assert critical_notes[0].severity == "critical"

    def test_feedback_context_ignores_extra_fields(self):
        """Test that extra fields in FeedbackContext are ignored."""
        ctx = FeedbackContext(
            quality_notes=[],
            extra_field="should be ignored",
        )
        assert not hasattr(ctx, "extra_field")


class TestQualityNoteCategories:
    """Tests for QualityNoteCategory enum values."""

    def test_all_categories_exist(self):
        """Test all expected categories are defined."""
        expected = [
            "empty_section",
            "missing_citation",
            "vague_content",
            "no_cross_reference",
            "confidence_mismatch",
            "redundant_doc",
            "other",
        ]
        for cat in expected:
            assert QualityNoteCategory(cat)

    def test_all_severities_exist(self):
        """Test all expected severities are defined."""
        expected = ["critical", "high", "medium", "low"]
        for sev in expected:
            assert QualityNoteSeverity(sev)


class TestFeedbackContextSerialization:
    """Tests for FeedbackContext serialization/deserialization."""

    def test_feedback_context_round_trip(self):
        """Test that FeedbackContext can be serialized and deserialized."""
        note = QualityNote(
            category="empty_section",
            severity="critical",
            description="Critical empty section",
            affected_sections=["inputs"],
        )
        ctx = FeedbackContext(
            quality_notes=[note],
            critical_sections=["inputs", "outputs"],
            required_citations=True,
        )

        # Serialize to dict
        ctx_dict = ctx.model_dump()

        # Deserialize back
        ctx2 = FeedbackContext.model_validate(ctx_dict)

        assert len(ctx2.quality_notes) == 1
        assert ctx2.quality_notes[0].category == "empty_section"
        assert ctx2.critical_sections == ["inputs", "outputs"]
