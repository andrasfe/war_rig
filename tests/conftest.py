"""Pytest fixtures for War Rig tests.

This module provides shared fixtures used across all test modules.
"""

import pytest
from pathlib import Path
from datetime import datetime

from war_rig.config import WarRigConfig, ScribeConfig, ChallengerConfig, ImperatorConfig
from war_rig.models.templates import (
    DocumentationTemplate,
    HeaderSection,
    PurposeSection,
    FileType,
    ProgramType,
    FinalStatus,
)
from war_rig.models.assessments import (
    ConfidenceAssessment,
    ChallengerAssessment,
    SectionAssessment,
    ConfidenceLevel,
    ValidationLevel,
)
from war_rig.models.tickets import (
    ChromeTicket,
    ChallengerQuestion,
    ScribeResponse,
    IssueType,
    IssuePriority,
    QuestionType,
    QuestionSeverity,
    ActionTaken,
)


# =============================================================================
# Sample Source Code Fixtures
# =============================================================================


@pytest.fixture
def sample_cobol_source() -> str:
    """Simple COBOL program for testing."""
    return """       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       AUTHOR. TEST AUTHOR.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE ASSIGN TO 'INPUT.DAT'
               ORGANIZATION IS SEQUENTIAL.
           SELECT OUTFILE ASSIGN TO 'OUTPUT.DAT'
               ORGANIZATION IS SEQUENTIAL.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  INFILE.
       01  IN-RECORD.
           05  IN-ACCOUNT-NUM     PIC 9(10).
           05  IN-AMOUNT          PIC 9(7)V99.
      *
       FD  OUTFILE.
       01  OUT-RECORD.
           05  OUT-ACCOUNT-NUM    PIC 9(10).
           05  OUT-AMOUNT         PIC 9(7)V99.
      *
       WORKING-STORAGE SECTION.
       01  WS-EOF-FLAG            PIC X VALUE 'N'.
           88 EOF                  VALUE 'Y'.
       01  WS-RECORD-COUNT        PIC 9(5) VALUE 0.
      *
       COPY CUSTCOPY.
      *
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS UNTIL EOF
           PERFORM 3000-FINALIZE
           STOP RUN.
      *
       1000-INITIALIZE.
           OPEN INPUT INFILE
           OPEN OUTPUT OUTFILE
           READ INFILE
               AT END SET EOF TO TRUE
           END-READ.
      *
       2000-PROCESS.
           MOVE IN-ACCOUNT-NUM TO OUT-ACCOUNT-NUM
           IF IN-AMOUNT > 1000
               COMPUTE OUT-AMOUNT = IN-AMOUNT * 1.05
           ELSE
               MOVE IN-AMOUNT TO OUT-AMOUNT
           END-IF
           WRITE OUT-RECORD
           ADD 1 TO WS-RECORD-COUNT
           READ INFILE
               AT END SET EOF TO TRUE
           END-READ.
      *
       3000-FINALIZE.
           CLOSE INFILE
           CLOSE OUTFILE
           DISPLAY 'RECORDS PROCESSED: ' WS-RECORD-COUNT.
"""


@pytest.fixture
def sample_jcl_source() -> str:
    """Simple JCL job for testing."""
    return """//TESTJOB  JOB (ACCT),'TEST JOB',CLASS=A,MSGCLASS=X
//*
//STEP01   EXEC PGM=TESTPROG
//INFILE   DD DSN=TEST.INPUT.FILE,DISP=SHR
//OUTFILE  DD DSN=TEST.OUTPUT.FILE,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=8000)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//*
//STEP02   EXEC PGM=SORT,COND=(4,LT)
//SORTIN   DD DSN=TEST.OUTPUT.FILE,DISP=SHR
//SORTOUT  DD DSN=TEST.SORTED.FILE,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(5,2),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,10,CH,A)
/*
//
"""


@pytest.fixture
def sample_copybook_source() -> str:
    """Simple copybook for testing."""
    return """      * CUSTOMER RECORD LAYOUT
       01  CUSTOMER-RECORD.
           05  CUST-ID            PIC 9(10).
           05  CUST-NAME.
               10  CUST-FIRST     PIC X(20).
               10  CUST-LAST      PIC X(30).
           05  CUST-ADDRESS.
               10  CUST-STREET    PIC X(50).
               10  CUST-CITY      PIC X(30).
               10  CUST-STATE     PIC XX.
               10  CUST-ZIP       PIC 9(5).
           05  CUST-BALANCE       PIC S9(9)V99 COMP-3.
           05  CUST-STATUS        PIC X.
               88  CUST-ACTIVE    VALUE 'A'.
               88  CUST-INACTIVE  VALUE 'I'.
               88  CUST-SUSPENDED VALUE 'S'.
"""


# =============================================================================
# Configuration Fixtures
# =============================================================================


@pytest.fixture
def test_config() -> WarRigConfig:
    """Test configuration with mock-friendly settings."""
    return WarRigConfig(
        rig_id="TEST_RIG",
        max_iterations=2,
        max_questions_per_round=3,
        max_chrome_tickets=3,
        scribe=ScribeConfig(
            model="claude-sonnet-4-20250514",
            temperature=0.1,
            max_tokens=1000,
        ),
        challenger=ChallengerConfig(
            model="claude-sonnet-4-20250514",
            temperature=0.1,
            max_tokens=1000,
        ),
        imperator=ImperatorConfig(
            model="claude-sonnet-4-20250514",
            temperature=0.1,
            max_tokens=1000,
        ),
    )


@pytest.fixture
def tmp_output_dir(tmp_path: Path) -> Path:
    """Temporary output directory for testing."""
    output_dir = tmp_path / "output"
    output_dir.mkdir()
    return output_dir


# =============================================================================
# Model Fixtures
# =============================================================================


@pytest.fixture
def sample_template() -> DocumentationTemplate:
    """Sample documentation template for testing."""
    return DocumentationTemplate(
        header=HeaderSection(
            program_id="TESTPROG",
            file_name="TESTPROG.cbl",
            file_type=FileType.COBOL,
            analyzed_by="TEST_RIG",
            analyzed_at=datetime(2024, 1, 1, 12, 0, 0),
            iteration_count=1,
        ),
        purpose=PurposeSection(
            summary="Test program that processes input records and applies business rules",
            business_context="Part of the batch processing system",
            program_type=ProgramType.BATCH,
            citations=[1, 35, 50],
        ),
    )


@pytest.fixture
def sample_confidence() -> ConfidenceAssessment:
    """Sample confidence assessment for testing."""
    return ConfidenceAssessment(
        program_id="TESTPROG",
        iteration=1,
        overall_confidence=ConfidenceLevel.MEDIUM,
        section_confidence={
            "purpose": ConfidenceLevel.HIGH,
            "inputs": ConfidenceLevel.MEDIUM,
            "outputs": ConfidenceLevel.MEDIUM,
        },
        low_confidence_sections=["business_rules"],
        reasoning="Good understanding of structure, some uncertainty on business rules",
    )


@pytest.fixture
def sample_challenger_assessment() -> ChallengerAssessment:
    """Sample challenger assessment for testing."""
    return ChallengerAssessment(
        program_id="TESTPROG",
        iteration=1,
        section_assessments=[
            SectionAssessment(
                section_name="purpose",
                validation_level=ValidationLevel.SOLID,
                issues=[],
                suggestions=[],
            ),
            SectionAssessment(
                section_name="inputs",
                validation_level=ValidationLevel.SOLID,
                issues=[],
                suggestions=[],
            ),
            SectionAssessment(
                section_name="outputs",
                validation_level=ValidationLevel.SHAKY,
                issues=["Missing description for OUT-RECORD"],
                suggestions=["Add field-level descriptions"],
            ),
        ],
        overall_assessment="Generally good, minor improvements needed",
        key_concerns=["Output documentation could be more detailed"],
    )


@pytest.fixture
def sample_chrome_ticket() -> ChromeTicket:
    """Sample Chrome ticket for testing."""
    return ChromeTicket(
        ticket_id="CHR-TEST0001",
        program_id="TESTPROG",
        section="purpose",
        issue_type=IssueType.VAGUE,
        description="Purpose section is too generic",
        guidance="Add specific business context",
        priority=IssuePriority.HIGH,
    )


@pytest.fixture
def sample_challenger_question() -> ChallengerQuestion:
    """Sample Challenger question for testing."""
    return ChallengerQuestion(
        question_id="Q-TEST0001",
        section="inputs",
        question_type=QuestionType.COMPLETENESS,
        question="What validation is performed on IN-AMOUNT?",
        evidence=[45, 46, 47],
        severity=QuestionSeverity.IMPORTANT,
        iteration=1,
    )


@pytest.fixture
def sample_scribe_response() -> ScribeResponse:
    """Sample Scribe response for testing."""
    return ScribeResponse(
        question_id="Q-TEST0001",
        response="IN-AMOUNT is validated in the 2000-PROCESS paragraph. Values over 1000 receive a 5% markup.",
        action_taken=ActionTaken.DEFENDED,
        citation=[45, 46, 47, 48],
        iteration=1,
    )


# =============================================================================
# Async Test Helpers
# =============================================================================


@pytest.fixture
def event_loop_policy():
    """Configure event loop for async tests."""
    import asyncio
    return asyncio.DefaultEventLoopPolicy()
