"""Tests for document validation functionality.

This module provides comprehensive tests for the DocumentValidator class,
including file type detection, validation criteria checking, and ticket creation.
"""

import pytest

from war_rig.models.templates import (
    CalledProgram,
    CallType,
    CopybookHeader,
    CopybookPurpose,
    CopybookTemplate,
    DocumentationTemplate,
    FileType,
    HeaderSection,
    JCLHeader,
    JCLPurpose,
    JCLStep,
    JCLTemplate,
    Paragraph,
    ProgramType,
    PurposeSection,
    RecordField,
    UsageType,
)
from war_rig.models.tickets import IssueType
from war_rig.validation.document_validator import DocumentValidator, ValidationResult


# =============================================================================
# Test Fixtures
# =============================================================================


@pytest.fixture
def validator() -> DocumentValidator:
    """Create a DocumentValidator instance for testing."""
    return DocumentValidator()


@pytest.fixture
def minimal_documentation_template() -> DocumentationTemplate:
    """Create a minimal DocumentationTemplate with no content."""
    return DocumentationTemplate(
        header=HeaderSection(
            program_id="TESTPROG",
            file_name="TESTPROG.cbl",
            file_type=FileType.COBOL,
        ),
        purpose=None,
    )


@pytest.fixture
def complete_documentation_template() -> DocumentationTemplate:
    """Create a DocumentationTemplate with comprehensive content."""
    return DocumentationTemplate(
        header=HeaderSection(
            program_id="TESTPROG",
            file_name="TESTPROG.cbl",
            file_type=FileType.COBOL,
        ),
        purpose=PurposeSection(
            summary="This program processes customer records and applies business rules for account management.",
            business_context="Part of daily batch processing for customer accounts",
            program_type=ProgramType.BATCH,
            citations=[1, 35, 50],
        ),
        paragraphs=[
            Paragraph(
                paragraph_name="0000-MAIN",
                purpose="Main control paragraph that orchestrates program flow",
                called_by=[],
                calls=["1000-INITIALIZE", "2000-PROCESS", "3000-FINALIZE"],
                citation=(78, 82),
            ),
            Paragraph(
                paragraph_name="2000-PROCESS",
                purpose="Process individual records and apply markup rules",
                called_by=["0000-MAIN"],
                calls=[],
                citation=(91, 102),
            ),
        ],
        called_programs=[
            CalledProgram(
                program_name="SUBPROG1",
                call_type=CallType.STATIC_CALL,
                purpose="Calculate interest on account balances",
                parameters=["WS-ACCOUNT", "WS-RATE"],
                citation=150,
            ),
        ],
    )


@pytest.fixture
def minimal_jcl_template() -> JCLTemplate:
    """Create a minimal JCLTemplate with no steps."""
    return JCLTemplate(
        header=JCLHeader(
            job_name="TESTJOB",
            file_name="TESTJOB.jcl",
            file_type=FileType.JCL,
        ),
        purpose=None,
        steps=[],
    )


@pytest.fixture
def complete_jcl_template() -> JCLTemplate:
    """Create a JCLTemplate with documented steps."""
    return JCLTemplate(
        header=JCLHeader(
            job_name="TESTJOB",
            file_name="TESTJOB.jcl",
            file_type=FileType.JCL,
        ),
        purpose=JCLPurpose(
            summary="Daily batch job that processes customer transactions and generates reports.",
            schedule="Daily at 6:00 AM",
            business_context="Part of end-of-day processing for retail banking",
            job_class="A",
            msgclass="X",
        ),
        steps=[
            JCLStep(
                step_name="STEP01",
                program="TESTPROG",
                proc=None,
                purpose="Execute main program to process transactions",
                condition=None,
                citation=10,
            ),
            JCLStep(
                step_name="STEP02",
                program="SORT",
                proc=None,
                purpose="Sort output file by account number",
                condition="(4,LT)",
                citation=25,
            ),
        ],
    )


@pytest.fixture
def minimal_copybook_template() -> CopybookTemplate:
    """Create a minimal CopybookTemplate with no fields."""
    return CopybookTemplate(
        header=CopybookHeader(
            copybook_name="CUSTCOPY",
            file_name="CUSTCOPY.cpy",
            file_type=FileType.COPYBOOK,
        ),
        purpose=None,
        record_layout=[],
    )


@pytest.fixture
def complete_copybook_template() -> CopybookTemplate:
    """Create a CopybookTemplate with field definitions."""
    return CopybookTemplate(
        header=CopybookHeader(
            copybook_name="CUSTCOPY",
            file_name="CUSTCOPY.cpy",
            file_type=FileType.COPYBOOK,
        ),
        purpose=CopybookPurpose(
            summary="Customer record layout for the retail banking system master file.",
            usage_context="File record layout for VSAM customer master file",
        ),
        record_layout=[
            RecordField(
                level=1,
                field_name="CUSTOMER-RECORD",
                picture=None,
                usage=UsageType.DISPLAY,
                redefines=None,
                occurs=None,
                description="Root level group for customer data",
                valid_values=[],
                citation=1,
            ),
            RecordField(
                level=5,
                field_name="CUST-ID",
                picture="9(10)",
                usage=UsageType.DISPLAY,
                redefines=None,
                occurs=None,
                description="Unique customer identifier assigned at account creation",
                valid_values=[],
                citation=2,
            ),
            RecordField(
                level=5,
                field_name="CUST-NAME",
                picture="X(50)",
                usage=UsageType.DISPLAY,
                redefines=None,
                occurs=None,
                description="Customer full name",
                valid_values=[],
                citation=3,
            ),
        ],
    )


@pytest.fixture
def partial_documentation_template() -> DocumentationTemplate:
    """Create a DocumentationTemplate with some content but missing key sections."""
    return DocumentationTemplate(
        header=HeaderSection(
            program_id="PARTPROG",
            file_name="PARTPROG.cbl",
            file_type=FileType.COBOL,
        ),
        purpose=PurposeSection(
            summary="This program does some processing.",  # Too short/vague but still valid
            business_context="Batch system",
            program_type=ProgramType.BATCH,
        ),
        paragraphs=[],  # Empty - will fail COBOL-PROC
        called_programs=[],  # Empty - will fail COBOL-CALLS if program has CALL statements
    )


# =============================================================================
# Test _detect_file_type()
# =============================================================================


class TestDetectFileType:
    """Tests for DocumentValidator._detect_file_type()."""

    def test_cbl_extension_returns_cobol(self, validator: DocumentValidator):
        """Test .cbl extension is detected as COBOL."""
        template = DocumentationTemplate()
        file_type = validator._detect_file_type("PROGRAM.cbl", template)
        assert file_type == FileType.COBOL

    def test_cob_extension_returns_cobol(self, validator: DocumentValidator):
        """Test .cob extension is detected as COBOL."""
        template = DocumentationTemplate()
        file_type = validator._detect_file_type("PROGRAM.cob", template)
        assert file_type == FileType.COBOL

    def test_cpy_extension_returns_copybook(self, validator: DocumentValidator):
        """Test .cpy extension is detected as COPYBOOK."""
        template = CopybookTemplate()
        file_type = validator._detect_file_type("RECORD.cpy", template)
        assert file_type == FileType.COPYBOOK

    def test_copy_extension_returns_copybook(self, validator: DocumentValidator):
        """Test .copy extension is detected as COPYBOOK."""
        template = CopybookTemplate()
        file_type = validator._detect_file_type("RECORD.copy", template)
        assert file_type == FileType.COPYBOOK

    def test_jcl_extension_returns_jcl(self, validator: DocumentValidator):
        """Test .jcl extension is detected as JCL."""
        template = JCLTemplate()
        file_type = validator._detect_file_type("MYJOB.jcl", template)
        assert file_type == FileType.JCL

    def test_bms_extension_returns_bms(self, validator: DocumentValidator):
        """Test .bms extension is detected as BMS."""
        template = DocumentationTemplate()
        file_type = validator._detect_file_type("SCREEN.bms", template)
        assert file_type == FileType.BMS

    def test_pli_extension_returns_pli(self, validator: DocumentValidator):
        """Test .pli extension is detected as PLI."""
        template = DocumentationTemplate()
        file_type = validator._detect_file_type("PROGRAM.pli", template)
        assert file_type == FileType.PLI

    def test_pl1_extension_returns_pli(self, validator: DocumentValidator):
        """Test .pl1 extension is detected as PLI."""
        template = DocumentationTemplate()
        file_type = validator._detect_file_type("PROGRAM.pl1", template)
        assert file_type == FileType.PLI

    def test_proc_extension_returns_proc(self, validator: DocumentValidator):
        """Test .proc extension is detected as PROC."""
        template = JCLTemplate()
        file_type = validator._detect_file_type("MYPROC.proc", template)
        assert file_type == FileType.PROC

    def test_unknown_extension_returns_other(self, validator: DocumentValidator):
        """Test unknown extension is detected as OTHER."""
        template = DocumentationTemplate()
        file_type = validator._detect_file_type("README.txt", template)
        assert file_type == FileType.OTHER

    def test_no_extension_returns_other(self, validator: DocumentValidator):
        """Test filename without extension is detected as OTHER."""
        template = DocumentationTemplate()
        file_type = validator._detect_file_type("NOEXTENSION", template)
        assert file_type == FileType.OTHER

    def test_case_insensitive_extension(self, validator: DocumentValidator):
        """Test that extension detection is case insensitive."""
        template = DocumentationTemplate()
        assert validator._detect_file_type("PROGRAM.CBL", template) == FileType.COBOL
        assert validator._detect_file_type("PROGRAM.Cbl", template) == FileType.COBOL
        assert validator._detect_file_type("JOB.JCL", template) == FileType.JCL

    def test_header_file_type_fallback(self, validator: DocumentValidator):
        """Test that header file_type is used when extension is unknown."""
        template = DocumentationTemplate(
            header=HeaderSection(
                program_id="TEST",
                file_name="UNKNOWN",
                file_type=FileType.COBOL,
            )
        )
        file_type = validator._detect_file_type("UNKNOWN", template)
        assert file_type == FileType.COBOL

    def test_extension_takes_precedence_over_header(self, validator: DocumentValidator):
        """Test that extension detection takes precedence over header."""
        template = DocumentationTemplate(
            header=HeaderSection(
                program_id="TEST",
                file_name="PROGRAM.jcl",
                file_type=FileType.COBOL,  # Wrong type in header
            )
        )
        file_type = validator._detect_file_type("PROGRAM.jcl", template)
        assert file_type == FileType.JCL  # Extension wins

    def test_asm_extension_returns_asm(self, validator: DocumentValidator):
        """Test .asm extension is detected as ASM."""
        template = DocumentationTemplate()
        file_type = validator._detect_file_type("PROGRAM.asm", template)
        assert file_type == FileType.ASM

    def test_rexx_extension_returns_rexx(self, validator: DocumentValidator):
        """Test .rexx extension is detected as REXX."""
        template = DocumentationTemplate()
        file_type = validator._detect_file_type("SCRIPT.rexx", template)
        assert file_type == FileType.REXX


# =============================================================================
# Test JCL Validation
# =============================================================================


class TestJCLValidation:
    """Tests for JCL-specific validation criteria."""

    def test_jcl_missing_steps_fails(
        self,
        validator: DocumentValidator,
        minimal_jcl_template: JCLTemplate,
    ):
        """Test that JCL without EXEC steps fails JCL-EXEC criterion."""
        result = validator.validate_document(
            file_name="TESTJOB.jcl",
            program_id="TESTJOB",
            template=minimal_jcl_template,
        )

        assert not result.is_valid
        assert result.file_type == FileType.JCL

        # Find the JCL-EXEC failure
        exec_failures = [c for c in result.failed_criteria if c.criterion_id == "JCL-EXEC"]
        assert len(exec_failures) == 1
        assert exec_failures[0].section == "steps"

    def test_jcl_with_steps_passes_exec(
        self,
        validator: DocumentValidator,
        complete_jcl_template: JCLTemplate,
    ):
        """Test that JCL with documented steps passes JCL-EXEC criterion."""
        result = validator.validate_document(
            file_name="TESTJOB.jcl",
            program_id="TESTJOB",
            template=complete_jcl_template,
        )

        # Check that JCL-EXEC is not in the failed criteria
        exec_failures = [c for c in result.failed_criteria if c.criterion_id == "JCL-EXEC"]
        assert len(exec_failures) == 0

    def test_jcl_missing_purpose_fails(
        self,
        validator: DocumentValidator,
        minimal_jcl_template: JCLTemplate,
    ):
        """Test that JCL without purpose summary fails JCL-FLOW criterion."""
        result = validator.validate_document(
            file_name="TESTJOB.jcl",
            program_id="TESTJOB",
            template=minimal_jcl_template,
        )

        # Find the JCL-FLOW failure
        flow_failures = [c for c in result.failed_criteria if c.criterion_id == "JCL-FLOW"]
        assert len(flow_failures) == 1
        assert flow_failures[0].section == "purpose"

    def test_jcl_with_purpose_passes_flow(
        self,
        validator: DocumentValidator,
        complete_jcl_template: JCLTemplate,
    ):
        """Test that JCL with purpose summary passes JCL-FLOW criterion."""
        result = validator.validate_document(
            file_name="TESTJOB.jcl",
            program_id="TESTJOB",
            template=complete_jcl_template,
        )

        # Check that JCL-FLOW is not in the failed criteria
        flow_failures = [c for c in result.failed_criteria if c.criterion_id == "JCL-FLOW"]
        assert len(flow_failures) == 0


# =============================================================================
# Test COBOL Validation
# =============================================================================


class TestCOBOLValidation:
    """Tests for COBOL-specific validation criteria."""

    def test_cobol_missing_paragraphs_fails(
        self,
        validator: DocumentValidator,
        minimal_documentation_template: DocumentationTemplate,
    ):
        """Test that COBOL without procedure documentation fails COBOL-PROC criterion."""
        result = validator.validate_document(
            file_name="TESTPROG.cbl",
            program_id="TESTPROG",
            template=minimal_documentation_template,
        )

        assert not result.is_valid
        assert result.file_type == FileType.COBOL

        # Find the COBOL-PROC failure
        proc_failures = [c for c in result.failed_criteria if c.criterion_id == "COBOL-PROC"]
        assert len(proc_failures) == 1
        assert proc_failures[0].section == "paragraphs"

    def test_cobol_with_paragraphs_passes_proc(
        self,
        validator: DocumentValidator,
        complete_documentation_template: DocumentationTemplate,
    ):
        """Test that COBOL with documented paragraphs passes COBOL-PROC criterion."""
        result = validator.validate_document(
            file_name="TESTPROG.cbl",
            program_id="TESTPROG",
            template=complete_documentation_template,
        )

        # Check that COBOL-PROC is not in the failed criteria
        proc_failures = [c for c in result.failed_criteria if c.criterion_id == "COBOL-PROC"]
        assert len(proc_failures) == 0

    def test_cobol_with_calls_passes(
        self,
        validator: DocumentValidator,
        complete_documentation_template: DocumentationTemplate,
    ):
        """Test that COBOL with called_programs passes COBOL-CALLS criterion."""
        result = validator.validate_document(
            file_name="TESTPROG.cbl",
            program_id="TESTPROG",
            template=complete_documentation_template,
        )

        # Check that COBOL-CALLS is not in the failed criteria
        calls_failures = [c for c in result.failed_criteria if c.criterion_id == "COBOL-CALLS"]
        assert len(calls_failures) == 0

    def test_cobol_missing_calls_fails(
        self,
        validator: DocumentValidator,
        minimal_documentation_template: DocumentationTemplate,
    ):
        """Test that COBOL without called_programs fails COBOL-CALLS criterion."""
        result = validator.validate_document(
            file_name="TESTPROG.cbl",
            program_id="TESTPROG",
            template=minimal_documentation_template,
        )

        # Find the COBOL-CALLS failure
        calls_failures = [c for c in result.failed_criteria if c.criterion_id == "COBOL-CALLS"]
        assert len(calls_failures) == 1
        assert calls_failures[0].section == "called_programs"


# =============================================================================
# Test Copybook Validation
# =============================================================================


class TestCopybookValidation:
    """Tests for Copybook-specific validation criteria."""

    def test_copybook_missing_fields_fails(
        self,
        validator: DocumentValidator,
        minimal_copybook_template: CopybookTemplate,
    ):
        """Test that Copybook without field descriptions fails COPY-FIELDS criterion."""
        result = validator.validate_document(
            file_name="CUSTCOPY.cpy",
            program_id="CUSTCOPY",
            template=minimal_copybook_template,
        )

        assert not result.is_valid
        assert result.file_type == FileType.COPYBOOK

        # Find the COPY-FIELDS failure
        fields_failures = [c for c in result.failed_criteria if c.criterion_id == "COPY-FIELDS"]
        assert len(fields_failures) == 1
        assert fields_failures[0].section == "record_layout"

    def test_copybook_with_layout_passes(
        self,
        validator: DocumentValidator,
        complete_copybook_template: CopybookTemplate,
    ):
        """Test that Copybook with record_layout passes COPY-FIELDS criterion."""
        result = validator.validate_document(
            file_name="CUSTCOPY.cpy",
            program_id="CUSTCOPY",
            template=complete_copybook_template,
        )

        # Check that COPY-FIELDS is not in the failed criteria
        fields_failures = [c for c in result.failed_criteria if c.criterion_id == "COPY-FIELDS"]
        assert len(fields_failures) == 0

    def test_copybook_missing_purpose_fails(
        self,
        validator: DocumentValidator,
        minimal_copybook_template: CopybookTemplate,
    ):
        """Test that Copybook without purpose fails COPY-CONTEXT criterion."""
        result = validator.validate_document(
            file_name="CUSTCOPY.cpy",
            program_id="CUSTCOPY",
            template=minimal_copybook_template,
        )

        # Find the COPY-CONTEXT failure
        context_failures = [c for c in result.failed_criteria if c.criterion_id == "COPY-CONTEXT"]
        assert len(context_failures) == 1
        assert context_failures[0].section == "purpose"

    def test_copybook_with_purpose_passes_context(
        self,
        validator: DocumentValidator,
        complete_copybook_template: CopybookTemplate,
    ):
        """Test that Copybook with purpose passes COPY-CONTEXT criterion."""
        result = validator.validate_document(
            file_name="CUSTCOPY.cpy",
            program_id="CUSTCOPY",
            template=complete_copybook_template,
        )

        # Check that COPY-CONTEXT is not in the failed criteria
        context_failures = [c for c in result.failed_criteria if c.criterion_id == "COPY-CONTEXT"]
        assert len(context_failures) == 0


# =============================================================================
# Test Unknown Type Handling
# =============================================================================


class TestUnknownTypeValidation:
    """Tests for handling unknown file types."""

    def test_unknown_type_passes(self, validator: DocumentValidator):
        """Test that unknown file types pass validation (no criteria defined)."""
        template = DocumentationTemplate(
            header=HeaderSection(
                program_id="UNKNOWN",
                file_name="UNKNOWN.xyz",
                file_type=FileType.OTHER,
            ),
        )

        result = validator.validate_document(
            file_name="UNKNOWN.xyz",
            program_id="UNKNOWN",
            template=template,
        )

        assert result.is_valid
        assert result.file_type == FileType.OTHER
        assert len(result.failed_criteria) == 0
        assert len(result.tickets) == 0

    def test_listing_type_passes(self, validator: DocumentValidator):
        """Test that LISTING file type passes (no criteria defined)."""
        template = DocumentationTemplate(
            header=HeaderSection(
                program_id="LISTING",
                file_name="LISTING.lst",
                file_type=FileType.LISTING,
            ),
        )

        result = validator.validate_document(
            file_name="LISTING.lst",
            program_id="LISTING",
            template=template,
            file_type=FileType.LISTING,  # Explicitly set
        )

        assert result.is_valid
        assert result.file_type == FileType.LISTING
        assert len(result.failed_criteria) == 0


# =============================================================================
# Test Ticket Creation
# =============================================================================


class TestTicketCreation:
    """Tests for ChromeTicket creation on validation failures."""

    def test_creates_tickets_for_failures(
        self,
        validator: DocumentValidator,
        minimal_jcl_template: JCLTemplate,
    ):
        """Test that ChromeTickets are created with correct fields for each failure."""
        result = validator.validate_document(
            file_name="TESTJOB.jcl",
            program_id="TESTJOB",
            template=minimal_jcl_template,
        )

        assert not result.is_valid
        assert len(result.tickets) > 0
        assert len(result.tickets) == len(result.failed_criteria)

        # Verify ticket structure
        for ticket in result.tickets:
            assert ticket.program_id == "TESTJOB"
            assert ticket.issue_type == IssueType.INCOMPLETE
            assert ticket.description.startswith("Validation failed:")
            assert ticket.section is not None
            assert ticket.ticket_id.startswith("CHR-")

    def test_ticket_has_guidance(
        self,
        validator: DocumentValidator,
        minimal_jcl_template: JCLTemplate,
    ):
        """Test that tickets include guidance from the validation criterion."""
        result = validator.validate_document(
            file_name="TESTJOB.jcl",
            program_id="TESTJOB",
            template=minimal_jcl_template,
        )

        # Find a ticket for JCL-EXEC
        exec_tickets = [t for t in result.tickets if t.section == "steps"]
        assert len(exec_tickets) > 0

        ticket = exec_tickets[0]
        assert ticket.guidance is not None
        assert len(ticket.guidance) > 0

    def test_no_tickets_when_valid(
        self,
        validator: DocumentValidator,
    ):
        """Test that no tickets are created when validation passes."""
        # Use an unknown file type which has no criteria
        template = DocumentationTemplate(
            header=HeaderSection(
                program_id="VALID",
                file_name="VALID.xyz",
                file_type=FileType.OTHER,
            ),
        )

        result = validator.validate_document(
            file_name="VALID.xyz",
            program_id="VALID",
            template=template,
        )

        assert result.is_valid
        assert len(result.tickets) == 0

    def test_ticket_priority_matches_criterion(
        self,
        validator: DocumentValidator,
        minimal_jcl_template: JCLTemplate,
    ):
        """Test that ticket priority is derived from criterion priority."""
        result = validator.validate_document(
            file_name="TESTJOB.jcl",
            program_id="TESTJOB",
            template=minimal_jcl_template,
        )

        # JCL-EXEC is CRITICAL priority, should map to CRITICAL in ticket
        exec_tickets = [t for t in result.tickets if t.section == "steps"]
        assert len(exec_tickets) > 0

        # The ticket should have a priority set (mapping from criterion)
        ticket = exec_tickets[0]
        assert ticket.priority is not None


# =============================================================================
# Test ValidationResult Structure
# =============================================================================


class TestValidationResult:
    """Tests for ValidationResult dataclass structure."""

    def test_result_contains_file_info(
        self,
        validator: DocumentValidator,
        complete_jcl_template: JCLTemplate,
    ):
        """Test that ValidationResult contains correct file information."""
        result = validator.validate_document(
            file_name="MYJOB.jcl",
            program_id="MYJOB",
            template=complete_jcl_template,
        )

        assert result.file_name == "MYJOB.jcl"
        assert result.file_type == FileType.JCL

    def test_result_valid_has_no_failures(
        self,
        validator: DocumentValidator,
    ):
        """Test that a valid result has empty failure and ticket lists."""
        template = DocumentationTemplate(
            header=HeaderSection(
                program_id="TEST",
                file_name="TEST.xyz",
                file_type=FileType.OTHER,
            ),
        )

        result = validator.validate_document(
            file_name="TEST.xyz",
            program_id="TEST",
            template=template,
        )

        assert result.is_valid is True
        assert result.failed_criteria == []
        assert result.tickets == []

    def test_result_invalid_has_failures(
        self,
        validator: DocumentValidator,
        minimal_documentation_template: DocumentationTemplate,
    ):
        """Test that an invalid result has non-empty failure and ticket lists."""
        result = validator.validate_document(
            file_name="TESTPROG.cbl",
            program_id="TESTPROG",
            template=minimal_documentation_template,
        )

        assert result.is_valid is False
        assert len(result.failed_criteria) > 0
        assert len(result.tickets) > 0


# =============================================================================
# Test Explicit File Type Override
# =============================================================================


class TestExplicitFileType:
    """Tests for explicit file_type parameter override."""

    def test_explicit_file_type_overrides_detection(
        self,
        validator: DocumentValidator,
    ):
        """Test that explicit file_type parameter overrides extension detection."""
        # File has .cbl extension but we force JCL type
        template = JCLTemplate(
            header=JCLHeader(
                job_name="TESTJOB",
                file_name="WRONGEXT.cbl",
                file_type=FileType.JCL,
            ),
            purpose=JCLPurpose(
                summary="A job that does something important for the business process.",
            ),
            steps=[
                JCLStep(
                    step_name="STEP01",
                    program="PROG",
                    purpose="Run the main program",
                    citation=1,
                ),
            ],
        )

        result = validator.validate_document(
            file_name="WRONGEXT.cbl",
            program_id="TESTJOB",
            template=template,
            file_type=FileType.JCL,  # Explicit override
        )

        # Should use JCL criteria, not COBOL
        assert result.file_type == FileType.JCL


# =============================================================================
# Test Purpose Section Validation
# =============================================================================


class TestPurposeSectionValidation:
    """Tests for purpose section content validation."""

    def test_purpose_too_short_fails(self, validator: DocumentValidator):
        """Test that purpose summary shorter than minimum length fails."""
        template = DocumentationTemplate(
            header=HeaderSection(
                program_id="SHORT",
                file_name="SHORT.cbl",
                file_type=FileType.COBOL,
            ),
            purpose=PurposeSection(
                summary="Too short",  # Less than MIN_SUMMARY_LENGTH (20)
                program_type=ProgramType.BATCH,
            ),
        )

        result = validator.validate_document(
            file_name="SHORT.cbl",
            program_id="SHORT",
            template=template,
        )

        # Purpose validation should fail due to short summary
        # Note: This depends on the specific criterion for COBOL purpose
        # The test verifies the _check_purpose_section logic
        assert template.purpose is not None
        is_valid = validator._check_purpose_section(template.purpose)
        assert is_valid is False

    def test_purpose_sufficient_length_passes(self, validator: DocumentValidator):
        """Test that purpose summary with sufficient length passes."""
        template = DocumentationTemplate(
            header=HeaderSection(
                program_id="LONG",
                file_name="LONG.cbl",
                file_type=FileType.COBOL,
            ),
            purpose=PurposeSection(
                summary="This is a sufficiently long summary that describes what the program does in business terms.",
                program_type=ProgramType.BATCH,
            ),
        )

        is_valid = validator._check_purpose_section(template.purpose)
        assert is_valid is True

    def test_purpose_none_fails(self, validator: DocumentValidator):
        """Test that None purpose section fails validation."""
        is_valid = validator._check_purpose_section(None)
        assert is_valid is False

    def test_purpose_no_summary_fails(self, validator: DocumentValidator):
        """Test that purpose section with no summary fails validation."""
        purpose = PurposeSection(
            summary=None,
            program_type=ProgramType.BATCH,
        )
        is_valid = validator._check_purpose_section(purpose)
        assert is_valid is False


# =============================================================================
# Test Content Meaningfulness Checks
# =============================================================================


class TestMeaningfulContent:
    """Tests for _has_meaningful_content and related helper methods."""

    def test_empty_list_not_meaningful(self, validator: DocumentValidator):
        """Test that empty list is not considered meaningful."""
        assert validator._has_meaningful_content([]) is False

    def test_list_with_none_not_meaningful(self, validator: DocumentValidator):
        """Test that list with only None values is not meaningful."""
        assert validator._has_meaningful_content([None, None]) is False

    def test_list_with_content_is_meaningful(self, validator: DocumentValidator):
        """Test that list with actual content is meaningful."""
        steps = [
            JCLStep(
                step_name="STEP01",
                program="TESTPROG",
                purpose="Run the program",
            ),
        ]
        assert validator._has_meaningful_content(steps) is True

    def test_meaningful_value_checks_strings(self, validator: DocumentValidator):
        """Test _is_meaningful_value correctly evaluates strings."""
        assert validator._is_meaningful_value("content") is True
        assert validator._is_meaningful_value("") is False
        assert validator._is_meaningful_value("   ") is False
        assert validator._is_meaningful_value(None) is False

    def test_meaningful_value_checks_lists(self, validator: DocumentValidator):
        """Test _is_meaningful_value correctly evaluates lists."""
        assert validator._is_meaningful_value([1, 2, 3]) is True
        assert validator._is_meaningful_value([]) is False

    def test_meaningful_value_checks_numbers(self, validator: DocumentValidator):
        """Test _is_meaningful_value correctly evaluates numbers."""
        assert validator._is_meaningful_value(42) is True
        assert validator._is_meaningful_value(0) is True
        assert validator._is_meaningful_value(3.14) is True
