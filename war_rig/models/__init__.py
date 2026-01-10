"""Pydantic models for War Rig data structures.

This package contains all the data models used throughout the War Rig system:

- templates: Documentation template models for different file types
- tickets: Chrome tickets, Challenger questions, and Scribe responses
- assessments: Confidence and validation assessment models

Example:
    from war_rig.models import (
        DocumentationTemplate,
        ChromeTicket,
        ChallengerQuestion,
        ConfidenceLevel,
    )

    template = DocumentationTemplate(
        header=HeaderSection(program_id="CBACT04C", ...),
        purpose=PurposeSection(...),
        ...
    )
"""

from war_rig.models.assessments import (
    ChallengerAssessment,
    ConfidenceAssessment,
    ConfidenceLevel,
    SectionAssessment,
    ValidationLevel,
)
from war_rig.models.templates import (
    BusinessRule,
    CalledProgram,
    CallingContext,
    CICSOperation,
    CopybookReference,
    CopybookTemplate,
    DataFlow,
    DocumentationTemplate,
    ErrorHandler,
    FileType,
    HeaderSection,
    InputOutput,
    JCLDDStatement,
    JCLDependencies,
    JCLStep,
    JCLTemplate,
    OpenQuestion,
    Paragraph,
    ProgramType,
    PurposeSection,
    RecordField,
    SQLOperation,
)
from war_rig.models.tickets import (
    ActionTaken,
    ChromeTicket,
    ChallengerQuestion,
    IssuePriority,
    IssueType,
    QuestionSeverity,
    QuestionType,
    ScribeResponse,
)

__all__ = [
    # Assessment models
    "ChallengerAssessment",
    "ConfidenceAssessment",
    "ConfidenceLevel",
    "SectionAssessment",
    "ValidationLevel",
    # Template models
    "BusinessRule",
    "CalledProgram",
    "CallingContext",
    "CICSOperation",
    "CopybookReference",
    "CopybookTemplate",
    "DataFlow",
    "DocumentationTemplate",
    "ErrorHandler",
    "FileType",
    "HeaderSection",
    "InputOutput",
    "JCLDDStatement",
    "JCLDependencies",
    "JCLStep",
    "JCLTemplate",
    "OpenQuestion",
    "Paragraph",
    "ProgramType",
    "PurposeSection",
    "RecordField",
    "SQLOperation",
    # Ticket models
    "ActionTaken",
    "ChromeTicket",
    "ChallengerQuestion",
    "IssuePriority",
    "IssueType",
    "QuestionSeverity",
    "QuestionType",
    "ScribeResponse",
]
