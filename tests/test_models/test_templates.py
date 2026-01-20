"""Tests for documentation template models."""

import pytest
from datetime import datetime

from war_rig.models.templates import (
    DocumentationTemplate,
    HeaderSection,
    PurposeSection,
    InputOutput,
    CalledProgram,
    BusinessRule,
    CopybookReference,
    FileType,
    ProgramType,
    IOType,
    CallType,
    CopybookLocation,
    FinalStatus,
)


class TestHeaderSection:
    """Tests for HeaderSection model."""

    def test_create_minimal_header(self):
        """Test creating header with required fields only."""
        header = HeaderSection(
            program_id="TESTPROG",
            file_name="TESTPROG.cbl",
            file_type=FileType.COBOL,
        )

        assert header.program_id == "TESTPROG"
        assert header.file_name == "TESTPROG.cbl"
        assert header.file_type == FileType.COBOL
        assert header.iteration_count == 1
        assert header.final_status is None

    def test_create_full_header(self):
        """Test creating header with all fields."""
        header = HeaderSection(
            program_id="TESTPROG",
            file_name="TESTPROG.cbl",
            file_type=FileType.COBOL,
            analyzed_by="TEST_RIG",
            iteration_count=3,
            final_status=FinalStatus.WITNESSED,
        )

        assert header.iteration_count == 3
        assert header.final_status == FinalStatus.WITNESSED

    def test_file_types(self):
        """Test all file type values."""
        for ft in FileType:
            header = HeaderSection(
                program_id="TEST",
                file_name=f"TEST.{ft.value.lower()}",
                file_type=ft,
            )
            assert header.file_type == ft


class TestPurposeSection:
    """Tests for PurposeSection model."""

    def test_create_purpose(self):
        """Test creating purpose section."""
        purpose = PurposeSection(
            summary="This program processes customer records",
            business_context="Part of daily batch",
            program_type=ProgramType.BATCH,
            citations=[1, 10, 20],
        )

        assert purpose.summary == "This program processes customer records"
        assert purpose.program_type == ProgramType.BATCH
        assert purpose.citations == [1, 10, 20]

    def test_program_types(self):
        """Test all program type values."""
        for pt in ProgramType:
            purpose = PurposeSection(
                summary="Test",
                program_type=pt,
            )
            assert purpose.program_type == pt


class TestInputOutput:
    """Tests for InputOutput model."""

    def test_create_input(self):
        """Test creating input definition."""
        inp = InputOutput(
            name="CUSTOMER-FILE",
            io_type=IOType.FILE_VSAM,
            description="Customer master file",
            copybook="CUSTREC",
            citation=[10, 20],
        )

        assert inp.name == "CUSTOMER-FILE"
        assert inp.io_type == IOType.FILE_VSAM
        assert inp.copybook == "CUSTREC"

    def test_io_types(self):
        """Test all IO type values."""
        for iot in IOType:
            io = InputOutput(
                name="TEST",
                io_type=iot,
                description="Test resource",
            )
            assert io.io_type == iot


class TestCalledProgram:
    """Tests for CalledProgram model."""

    def test_create_called_program(self):
        """Test creating called program entry."""
        called = CalledProgram(
            program_name="SUBPROG1",
            call_type=CallType.STATIC_CALL,
            purpose="Calculate interest",
            parameters=["WS-ACCOUNT", "WS-RATE"],
            citation=150,
        )

        assert called.program_name == "SUBPROG1"
        assert called.call_type == CallType.STATIC_CALL
        assert "WS-ACCOUNT" in called.parameters

    def test_call_types(self):
        """Test all call type values."""
        for ct in CallType:
            called = CalledProgram(
                program_name="TEST",
                call_type=ct,
            )
            assert called.call_type == ct


class TestBusinessRule:
    """Tests for BusinessRule model."""

    def test_create_business_rule(self):
        """Test creating business rule."""
        rule = BusinessRule(
            rule_id="BR001",
            description="Apply 5% markup for amounts over $1000",
            logic_summary="IF AMOUNT > 1000 THEN AMOUNT = AMOUNT * 1.05",
            conditions=["IN-AMOUNT > 1000"],
            citation=[45, 46, 47],
        )

        assert rule.rule_id == "BR001"
        assert "5% markup" in rule.description
        assert rule.citation == [45, 46, 47]


class TestCopybookReference:
    """Tests for CopybookReference model."""

    def test_create_copybook_reference(self):
        """Test creating copybook reference."""
        cb = CopybookReference(
            copybook_name="CUSTREC",
            purpose="Customer record layout",
            location=CopybookLocation.WORKING_STORAGE,
            citation=30,
        )

        assert cb.copybook_name == "CUSTREC"
        assert cb.location == CopybookLocation.WORKING_STORAGE

    def test_copybook_locations(self):
        """Test all copybook location values."""
        for loc in CopybookLocation:
            cb = CopybookReference(
                copybook_name="TEST",
                location=loc,
            )
            assert cb.location == loc


class TestDocumentationTemplate:
    """Tests for DocumentationTemplate model."""

    def test_create_minimal_template(self):
        """Test creating template with required fields only."""
        template = DocumentationTemplate(
            header=HeaderSection(
                program_id="TEST",
                file_name="TEST.cbl",
                file_type=FileType.COBOL,
            ),
            purpose=PurposeSection(
                summary="Test program",
                program_type=ProgramType.BATCH,
            ),
        )

        assert template.header.program_id == "TEST"
        assert template.purpose.summary == "Test program"
        assert template.inputs == []
        assert template.outputs == []

    def test_template_serialization(self, sample_template):
        """Test template JSON serialization."""
        json_str = sample_template.model_dump_json()
        assert "TESTPROG" in json_str
        assert "COBOL" in json_str

    def test_template_deserialization(self, sample_template):
        """Test template JSON deserialization."""
        json_str = sample_template.model_dump_json()
        restored = DocumentationTemplate.model_validate_json(json_str)

        assert restored.header.program_id == sample_template.header.program_id
        assert restored.purpose.summary == sample_template.purpose.summary


class TestLenientCoercion:
    """Tests for lenient type coercion from LLM responses."""

    def test_citation_from_string_with_numbers(self):
        """Test that citation accepts a string and extracts numbers."""
        io = InputOutput(
            name="TEST",
            io_type=IOType.FILE_SEQUENTIAL,
            description="Test",
            citation="Line 42 shows the read statement",
        )
        assert io.citation == [42]

    def test_citation_from_string_multiple_numbers(self):
        """Test extracting multiple numbers from a string."""
        io = InputOutput(
            name="TEST",
            io_type=IOType.FILE_SEQUENTIAL,
            description="Test",
            citation="Lines 10, 20, and 30",
        )
        assert io.citation == [10, 20, 30]

    def test_citation_from_string_no_numbers(self):
        """Test string with no numbers returns empty list."""
        io = InputOutput(
            name="TEST",
            io_type=IOType.FILE_SEQUENTIAL,
            description="Test",
            citation="No line numbers here",
        )
        assert io.citation == []

    def test_citation_from_mixed_list(self):
        """Test citation accepts mixed list of ints and strings."""
        io = InputOutput(
            name="TEST",
            io_type=IOType.FILE_SEQUENTIAL,
            description="Test",
            citation=[10, "line 20", 30],
        )
        assert io.citation == [10, 20, 30]

    def test_citation_from_none(self):
        """Test citation handles None gracefully."""
        io = InputOutput(
            name="TEST",
            io_type=IOType.FILE_SEQUENTIAL,
            description="Test",
            citation=None,
        )
        assert io.citation == []

    def test_citation_from_single_int(self):
        """Test citation accepts a single int."""
        io = InputOutput(
            name="TEST",
            io_type=IOType.FILE_SEQUENTIAL,
            description="Test",
            citation=42,
        )
        assert io.citation == [42]

    def test_conditions_from_string(self):
        """Test conditions field accepts a string and wraps it."""
        rule = BusinessRule(
            rule_id="BR001",
            description="Test rule",
            conditions="AMOUNT > 1000",
        )
        assert rule.conditions == ["AMOUNT > 1000"]

    def test_conditions_from_none(self):
        """Test conditions handles None gracefully."""
        rule = BusinessRule(
            rule_id="BR001",
            description="Test rule",
            conditions=None,
        )
        assert rule.conditions == []

    def test_parameters_from_string(self):
        """Test parameters accepts a string."""
        called = CalledProgram(
            program_name="SUBPROG",
            call_type=CallType.STATIC_CALL,
            parameters="WS-PARAM",
        )
        assert called.parameters == ["WS-PARAM"]
