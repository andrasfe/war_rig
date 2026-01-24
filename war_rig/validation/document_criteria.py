"""Document type validation criteria for War Rig.

This module defines validation criteria for different mainframe file types.
Each file type has specific documentation requirements that the Scribe agent
must satisfy. The Witness agent uses these criteria to validate documentation
quality and provide actionable feedback.

Criteria are organized by file type and include:
- Criterion ID: Unique identifier for the criterion (e.g., JCL-EXEC)
- Section: Which template section this criterion applies to
- Description: What documentation is required
- Guidance: How to fix if the criterion is not met
- Priority: CRITICAL, HIGH, or MEDIUM importance level
"""

from dataclasses import dataclass, field
from enum import Enum

from war_rig.models.templates import FileType


class Priority(str, Enum):
    """Priority level for validation criteria."""

    CRITICAL = "CRITICAL"  # Must be present for documentation to be useful
    HIGH = "HIGH"  # Should be present for complete documentation
    MEDIUM = "MEDIUM"  # Nice to have, improves documentation quality


@dataclass(frozen=True)
class ValidationCriterion:
    """A single validation criterion for documentation quality.

    Attributes:
        criterion_id: Unique identifier (e.g., "JCL-EXEC", "COBOL-PROC")
        section: Template section this applies to (e.g., "steps", "paragraphs")
        description: What documentation is required
        guidance: How to fix if the criterion is not met
        priority: Importance level (CRITICAL, HIGH, MEDIUM)
    """

    criterion_id: str
    section: str
    description: str
    guidance: str
    priority: Priority


@dataclass
class DocumentTypeCriteria:
    """Collection of validation criteria for a specific file type.

    Attributes:
        file_type: The FileType these criteria apply to
        criteria: List of ValidationCriterion instances
    """

    file_type: FileType
    criteria: list[ValidationCriterion] = field(default_factory=list)

    def get_critical_criteria(self) -> list[ValidationCriterion]:
        """Return only CRITICAL priority criteria."""
        return [c for c in self.criteria if c.priority == Priority.CRITICAL]

    def get_by_section(self, section: str) -> list[ValidationCriterion]:
        """Return criteria for a specific section."""
        return [c for c in self.criteria if c.section == section]

    def get_by_id(self, criterion_id: str) -> ValidationCriterion | None:
        """Return a specific criterion by ID."""
        for criterion in self.criteria:
            if criterion.criterion_id == criterion_id:
                return criterion
        return None


# =============================================================================
# JCL Criteria
# =============================================================================

JCL_CRITERIA = DocumentTypeCriteria(
    file_type=FileType.JCL,
    criteria=[
        ValidationCriterion(
            criterion_id="JCL-EXEC",
            section="steps",
            description=(
                "Every EXEC statement must be documented with step_name, "
                "program or proc name, and a clear purpose description"
            ),
            guidance=(
                "For each //stepname EXEC PGM=program or EXEC proc, document: "
                "(1) what the step accomplishes in business terms, "
                "(2) any COND parameters that control execution, "
                "(3) the line number citation"
            ),
            priority=Priority.CRITICAL,
        ),
        ValidationCriterion(
            criterion_id="JCL-DD",
            section="dd_statements",
            description=(
                "All DD statements with DSN= must document the dataset name, "
                "disposition, and purpose of the file"
            ),
            guidance=(
                "For each DD statement: identify the DSN, explain DISP parameters "
                "(NEW/OLD/SHR/MOD), describe what data the file contains and how "
                "it's used by the step. Group related DDs logically."
            ),
            priority=Priority.HIGH,
        ),
        ValidationCriterion(
            criterion_id="JCL-FLOW",
            section="purpose",
            description=(
                "Job purpose must explain the overall business function, "
                "not just list technical steps"
            ),
            guidance=(
                "The purpose.summary should answer: What business process does "
                "this job support? What is the end result? When does it run? "
                "Avoid simply restating the step names."
            ),
            priority=Priority.CRITICAL,
        ),
        ValidationCriterion(
            criterion_id="JCL-DEPS",
            section="dependencies",
            description=(
                "Input and output datasets must be identified to understand "
                "job sequencing and data flow"
            ),
            guidance=(
                "Populate dependencies.input_datasets with DSNs that must exist "
                "before the job runs (DISP=SHR or DISP=OLD). Populate "
                "dependencies.output_datasets with DSNs created (DISP=NEW) or "
                "modified (DISP=MOD) by the job."
            ),
            priority=Priority.HIGH,
        ),
    ],
)


# =============================================================================
# COBOL Criteria
# =============================================================================

COBOL_CRITERIA = DocumentTypeCriteria(
    file_type=FileType.COBOL,
    criteria=[
        ValidationCriterion(
            criterion_id="COBOL-PROC",
            section="paragraphs",
            description=(
                "Key paragraphs must be documented with their purpose and "
                "control flow relationships"
            ),
            guidance=(
                "Document each significant paragraph with: (1) what it accomplishes, "
                "(2) which paragraphs PERFORM it (called_by), (3) which paragraphs "
                "it PERFORMs (calls), (4) line number range. Focus on business "
                "logic paragraphs, not utility ones."
            ),
            priority=Priority.HIGH,
        ),
        ValidationCriterion(
            criterion_id="COBOL-CALLS",
            section="called_programs",
            description=(
                "All CALL statements must document the target program, "
                "call type, and parameters passed"
            ),
            guidance=(
                "For each CALL statement: identify if it's static (CALL 'PROG') "
                "or dynamic (CALL WS-PROG-NAME), document parameters from USING "
                "clause, explain why the call is made. Include CICS LINK/XCTL."
            ),
            priority=Priority.CRITICAL,
        ),
        ValidationCriterion(
            criterion_id="COBOL-DATA",
            section="data_flow",
            description=(
                "Data flow must trace how data moves through the program "
                "from inputs to outputs"
            ),
            guidance=(
                "Document: (1) reads_from - what files/tables/parameters provide "
                "input data and which fields, (2) writes_to - what destinations "
                "receive output and which fields, (3) transforms - key field "
                "transformations with business meaning."
            ),
            priority=Priority.HIGH,
        ),
        ValidationCriterion(
            criterion_id="COBOL-COPY",
            section="copybooks_used",
            description=(
                "All COPY statements must identify the copybook and explain "
                "what data structure it provides"
            ),
            guidance=(
                "For each COPY statement: name the copybook, describe what "
                "data structure it defines (record layout, commarea, etc.), "
                "indicate where it's included (WORKING-STORAGE, LINKAGE, FILE)."
            ),
            priority=Priority.MEDIUM,
        ),
    ],
)


# =============================================================================
# Copybook Criteria
# =============================================================================

COPYBOOK_CRITERIA = DocumentTypeCriteria(
    file_type=FileType.COPYBOOK,
    criteria=[
        ValidationCriterion(
            criterion_id="COPY-FIELDS",
            section="record_layout",
            description=(
                "Field definitions must include level, name, picture, and "
                "business description for key fields"
            ),
            guidance=(
                "Document each field with: level number, field name, PIC clause, "
                "USAGE if not DISPLAY. Add description explaining business meaning "
                "for fields that aren't self-evident. Note any REDEFINES relationships."
            ),
            priority=Priority.CRITICAL,
        ),
        ValidationCriterion(
            criterion_id="COPY-CONTEXT",
            section="purpose",
            description=(
                "Purpose must explain what data structure this defines and "
                "where it's typically used"
            ),
            guidance=(
                "The purpose should answer: Is this a file record, commarea, "
                "working storage structure, or parameter block? What business "
                "entity does it represent? What programs typically use it?"
            ),
            priority=Priority.HIGH,
        ),
        ValidationCriterion(
            criterion_id="COPY-KEYS",
            section="key_fields",
            description=(
                "Key fields that are primary keys, foreign keys, or business "
                "identifiers must be highlighted"
            ),
            guidance=(
                "Identify fields that serve as: record keys, account numbers, "
                "customer IDs, status codes, or other business identifiers. "
                "Explain their significance and any valid value constraints."
            ),
            priority=Priority.MEDIUM,
        ),
    ],
)


# =============================================================================
# PL/I Criteria
# =============================================================================

PLI_CRITERIA = DocumentTypeCriteria(
    file_type=FileType.PLI,
    criteria=[
        ValidationCriterion(
            criterion_id="PLI-PROC",
            section="paragraphs",
            description=(
                "Procedures and their entry points must be documented with "
                "purpose and parameters"
            ),
            guidance=(
                "Document each PROCEDURE with: purpose, parameters (DCL statements), "
                "what it returns or accomplishes, and relationships to other "
                "procedures. Include internal procedures and their call hierarchy."
            ),
            priority=Priority.HIGH,
        ),
        ValidationCriterion(
            criterion_id="PLI-CALLS",
            section="called_programs",
            description=(
                "All CALL statements must document external program invocations"
            ),
            guidance=(
                "For each CALL to an external program: identify the program name, "
                "document parameters passed, explain the purpose of the call. "
                "Distinguish between CALL and FETCH for dynamic loading."
            ),
            priority=Priority.CRITICAL,
        ),
        ValidationCriterion(
            criterion_id="PLI-DATA",
            section="data_flow",
            description=(
                "Structure declarations and data flow must be documented "
                "for complex programs"
            ),
            guidance=(
                "Document: DECLARE statements for major structures, how data "
                "flows from GET/READ statements through processing to PUT/WRITE "
                "statements. Include BASED and CONTROLLED variables."
            ),
            priority=Priority.HIGH,
        ),
    ],
)


# =============================================================================
# BMS (Basic Mapping Support) Criteria
# =============================================================================

BMS_CRITERIA = DocumentTypeCriteria(
    file_type=FileType.BMS,
    criteria=[
        ValidationCriterion(
            criterion_id="BMS-MAP",
            section="purpose",
            description=(
                "Map purpose must explain what screen this defines and "
                "its role in the application"
            ),
            guidance=(
                "Document: what business function this screen serves, which "
                "transaction ID uses it, how it fits into the screen flow "
                "(is it a menu, data entry, inquiry, or confirmation screen?)."
            ),
            priority=Priority.CRITICAL,
        ),
        ValidationCriterion(
            criterion_id="BMS-FIELDS",
            section="record_layout",
            description=(
                "Screen fields must document their position, attributes, "
                "and business purpose"
            ),
            guidance=(
                "For each DFHMDF: document the field name, position (row, col), "
                "length, attributes (protected, bright, etc.), and what data "
                "it displays or accepts. Group related fields logically."
            ),
            priority=Priority.HIGH,
        ),
    ],
)


# =============================================================================
# PROC (JCL Procedure) Criteria
# =============================================================================

PROC_CRITERIA = DocumentTypeCriteria(
    file_type=FileType.PROC,
    criteria=[
        ValidationCriterion(
            criterion_id="PROC-STEPS",
            section="steps",
            description=(
                "Procedure steps must be documented like JCL steps, with "
                "emphasis on reusability"
            ),
            guidance=(
                "Document each step with: what it does, what programs it executes, "
                "and how symbolic parameters affect its behavior. Explain the "
                "common use cases for this procedure."
            ),
            priority=Priority.CRITICAL,
        ),
        ValidationCriterion(
            criterion_id="PROC-PARAMS",
            section="purpose",
            description=(
                "Symbolic parameters must be documented with their defaults "
                "and valid values"
            ),
            guidance=(
                "List all symbolic parameters (&PARAM) with: default value, "
                "what the parameter controls, valid value ranges or options. "
                "Explain which parameters are required vs optional."
            ),
            priority=Priority.HIGH,
        ),
    ],
)


# =============================================================================
# Criteria Registry
# =============================================================================

CRITERIA_REGISTRY: dict[FileType, DocumentTypeCriteria] = {
    FileType.JCL: JCL_CRITERIA,
    FileType.COBOL: COBOL_CRITERIA,
    FileType.COPYBOOK: COPYBOOK_CRITERIA,
    FileType.PLI: PLI_CRITERIA,
    FileType.BMS: BMS_CRITERIA,
    FileType.PROC: PROC_CRITERIA,
}


def get_criteria_for_type(file_type: FileType) -> DocumentTypeCriteria | None:
    """Get validation criteria for a specific file type.

    Args:
        file_type: The FileType to get criteria for

    Returns:
        DocumentTypeCriteria instance if criteria exist for this type,
        None otherwise (e.g., for LISTING, OTHER, etc.)
    """
    return CRITERIA_REGISTRY.get(file_type)
