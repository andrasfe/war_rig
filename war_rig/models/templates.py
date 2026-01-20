"""Documentation template models for War Rig.

This module defines the Pydantic models for documentation templates that
the Scribe agent fills out. Templates vary by file type:

- DocumentationTemplate: For COBOL/PL/I programs
- CopybookTemplate: For copybook record layouts
- JCLTemplate: For JCL jobs

All templates follow the structure defined in the War Rig specification v3.0.
"""

import re
from datetime import datetime
from enum import Enum
from typing import Annotated, Any

from pydantic import BaseModel, BeforeValidator, Field, field_validator


def coerce_to_int_list(v: Any) -> list[int]:
    """Coerce any value to a list of integers, extracting numbers from strings."""
    if v is None:
        return []
    if isinstance(v, int):
        return [v]
    if isinstance(v, str):
        numbers = re.findall(r"\d+", v)
        return [int(n) for n in numbers] if numbers else []
    if isinstance(v, list):
        result = []
        for item in v:
            if isinstance(item, int):
                result.append(item)
            elif isinstance(item, (str, float)):
                try:
                    result.append(int(item) if isinstance(item, float) else int(float(item)))
                except (ValueError, TypeError):
                    numbers = re.findall(r"\d+", str(item))
                    result.extend(int(n) for n in numbers)
        return result
    return []


def coerce_to_str_list(v: Any) -> list[str]:
    """Coerce any value to a list of strings."""
    if v is None:
        return []
    if isinstance(v, str):
        return [v] if v.strip() else []
    if isinstance(v, list):
        return [str(item) for item in v if item is not None]
    return [str(v)]


# Lenient type aliases - accept any input and coerce to the expected type
LenientIntList = Annotated[list[int], BeforeValidator(coerce_to_int_list)]
LenientStrList = Annotated[list[str], BeforeValidator(coerce_to_str_list)]


class FileType(str, Enum):
    """Classification of mainframe source file types."""

    COBOL = "COBOL"
    PLI = "PLI"
    JCL = "JCL"
    COPYBOOK = "COPYBOOK"
    PROC = "PROC"
    BMS = "BMS"
    LISTING = "LISTING"
    OTHER = "OTHER"


class ProgramType(str, Enum):
    """Classification of program execution context."""

    BATCH = "BATCH"
    ONLINE_CICS = "ONLINE_CICS"
    SUBROUTINE = "SUBROUTINE"
    UTILITY = "UTILITY"


class IOType(str, Enum):
    """Classification of input/output resources."""

    FILE_SEQUENTIAL = "FILE_SEQUENTIAL"
    FILE_VSAM = "FILE_VSAM"
    DB2_TABLE = "DB2_TABLE"
    IMS_SEGMENT = "IMS_SEGMENT"
    PARAMETER = "PARAMETER"
    CICS_COMMAREA = "CICS_COMMAREA"
    CICS_MAP = "CICS_MAP"
    CICS_QUEUE = "CICS_QUEUE"
    REPORT = "REPORT"
    RETURN_CODE = "RETURN_CODE"
    OTHER = "OTHER"


class CallType(str, Enum):
    """Classification of program call types."""

    STATIC_CALL = "STATIC_CALL"
    DYNAMIC_CALL = "DYNAMIC_CALL"
    CICS_LINK = "CICS_LINK"
    CICS_XCTL = "CICS_XCTL"


class FinalStatus(str, Enum):
    """Final documentation approval status."""

    WITNESSED = "WITNESSED"
    FORCED = "FORCED"
    VALHALLA = "VALHALLA"


class CopybookLocation(str, Enum):
    """Location where a copybook is included."""

    WORKING_STORAGE = "WORKING_STORAGE"
    LINKAGE = "LINKAGE"
    FILE_SECTION = "FILE_SECTION"
    LOCAL_STORAGE = "LOCAL_STORAGE"
    OTHER = "OTHER"


class UsageType(str, Enum):
    """COBOL data item usage types."""

    DISPLAY = "DISPLAY"
    COMP = "COMP"
    COMP_1 = "COMP-1"
    COMP_2 = "COMP-2"
    COMP_3 = "COMP-3"
    COMP_4 = "COMP-4"
    COMP_5 = "COMP-5"
    POINTER = "POINTER"
    INDEX = "INDEX"
    OTHER = "OTHER"


# =============================================================================
# Program Documentation Template Components
# =============================================================================


class HeaderSection(BaseModel):
    """Documentation header with metadata about the analyzed file."""

    program_id: str = Field(
        ...,
        description="Program name extracted from source (e.g., PROGRAM-ID)",
    )
    file_name: str = Field(
        ...,
        description="Source file path",
    )
    file_type: FileType = Field(
        ...,
        description="Classification of the source file",
    )
    analyzed_by: str = Field(
        default="WAR_RIG",
        description="War Rig identifier that produced this documentation",
    )
    analyzed_at: datetime = Field(
        default_factory=datetime.utcnow,
        description="Timestamp of analysis",
    )
    iteration_count: int = Field(
        default=1,
        ge=1,
        description="Number of refinement cycles",
    )
    final_status: FinalStatus | None = Field(
        default=None,
        description="Final approval status (set by Imperator)",
    )


class PurposeSection(BaseModel):
    """Description of what the program does and its business context."""

    summary: str = Field(
        ...,
        description="2-3 sentence description of what this program does",
    )
    business_context: str | None = Field(
        default=None,
        description="What business process this serves",
    )
    program_type: ProgramType = Field(
        ...,
        description="Classification of execution context",
    )
    citations: LenientIntList = Field(
        default_factory=list,
        description="Line numbers supporting the summary",
    )


class InputOutput(BaseModel):
    """Description of a program input or output resource."""

    name: str = Field(
        ...,
        description="File, table, or parameter name",
    )
    io_type: IOType = Field(
        ...,
        description="Classification of the resource type",
    )
    description: str = Field(
        ...,
        description="What this input/output contains",
    )
    copybook: str | None = Field(
        default=None,
        description="Associated copybook name",
    )
    citation: LenientIntList = Field(
        default_factory=list,
        description="Line numbers where this is read/written",
    )


class CalledProgram(BaseModel):
    """Description of a program called by this program."""

    program_name: str = Field(
        ...,
        description="Name of the called program",
    )
    call_type: CallType = Field(
        ...,
        description="Type of call (STATIC, DYNAMIC, CICS_LINK, CICS_XCTL)",
    )
    purpose: str | None = Field(
        default=None,
        description="Why this program is called",
    )
    parameters: LenientStrList = Field(
        default_factory=list,
        description="Parameters passed (USING clause items)",
    )
    citation: int | None = Field(
        default=None,
        description="Line number of the CALL/LINK/XCTL statement",
    )


class CallingContext(BaseModel):
    """Information about how this program is invoked."""

    called_by: LenientStrList = Field(
        default_factory=list,
        description="Program names known to call this (may be populated later)",
    )
    entry_points: LenientStrList = Field(
        default_factory=list,
        description="Transaction IDs that invoke this (for CICS)",
    )
    linkage_section: LenientStrList = Field(
        default_factory=list,
        description="Key fields in LINKAGE SECTION",
    )


class BusinessRule(BaseModel):
    """Description of a business rule implemented in the program."""

    rule_id: str = Field(
        ...,
        description="Sequential identifier for the rule",
    )
    description: str = Field(
        ...,
        description="Plain English description of the rule",
    )
    logic_summary: str | None = Field(
        default=None,
        description="Brief explanation of implementation",
    )
    conditions: LenientStrList = Field(
        default_factory=list,
        description="Key conditions (IF statements) involved",
    )
    citation: LenientIntList = Field(
        default_factory=list,
        description="Line numbers where rule is implemented",
    )


class DataFlowRead(BaseModel):
    """Description of data read by the program."""

    source: str = Field(..., description="Source of the data")
    fields_used: LenientStrList = Field(
        default_factory=list,
        description="Fields read from this source",
    )
    citation: LenientIntList = Field(
        default_factory=list,
        description="Line numbers where read occurs",
    )


class DataFlowWrite(BaseModel):
    """Description of data written by the program."""

    destination: str = Field(..., description="Destination for the data")
    fields_written: LenientStrList = Field(
        default_factory=list,
        description="Fields written to this destination",
    )
    citation: LenientIntList = Field(
        default_factory=list,
        description="Line numbers where write occurs",
    )


class DataTransform(BaseModel):
    """Description of a data transformation."""

    input_field: str = Field(..., description="Source field")
    output_field: str = Field(..., description="Target field")
    transformation_description: str = Field(
        ...,
        description="Description of the transformation",
    )
    citation: LenientIntList = Field(
        default_factory=list,
        description="Line numbers where transformation occurs",
    )

    @field_validator("citation", mode="before")
    @classmethod
    def coerce_citation_to_list(cls, v):
        """Accept both int and list[int] for citation."""
        if isinstance(v, int):
            return [v]
        return v


class DataFlow(BaseModel):
    """Aggregated data flow information for the program."""

    reads_from: list[DataFlowRead] = Field(
        default_factory=list,
        description="Data sources read by the program",
    )
    writes_to: list[DataFlowWrite] = Field(
        default_factory=list,
        description="Data destinations written by the program",
    )
    transforms: list[DataTransform] = Field(
        default_factory=list,
        description="Data transformations performed",
    )


class CopybookReference(BaseModel):
    """Reference to a copybook used by the program."""

    copybook_name: str = Field(
        ...,
        description="Name of the copybook",
    )
    purpose: str | None = Field(
        default=None,
        description="What data structure it defines",
    )
    location: CopybookLocation = Field(
        ...,
        description="Where the copybook is included",
    )
    citation: int | None = Field(
        default=None,
        description="Line number of COPY statement",
    )


class Paragraph(BaseModel):
    """Description of a key paragraph in the program."""

    paragraph_name: str = Field(
        ...,
        description="Paragraph name",
    )
    purpose: str = Field(
        ...,
        description="What this paragraph does",
    )
    called_by: LenientStrList = Field(
        default_factory=list,
        description="Paragraphs that PERFORM this",
    )
    calls: LenientStrList = Field(
        default_factory=list,
        description="Paragraphs this PERFORMs",
    )
    citation: tuple[int, int] | None = Field(
        default=None,
        description="Line number range (start, end)",
    )


class ErrorHandler(BaseModel):
    """Description of an error handling case."""

    condition: str = Field(
        ...,
        description="What error condition is handled",
    )
    action: str = Field(
        ...,
        description="What happens (ABEND, return code, etc.)",
    )
    citation: LenientIntList = Field(
        default_factory=list,
        description="Line numbers",
    )


class SQLOperation(BaseModel):
    """Description of an embedded SQL operation."""

    operation: str = Field(
        ...,
        description="SQL operation type (SELECT, INSERT, UPDATE, DELETE, CURSOR)",
    )
    table: str | None = Field(
        default=None,
        description="Table name (may be None for complex queries or cursors)",
    )
    purpose: str | None = Field(
        default=None,
        description="Why this operation is performed",
    )
    citation: int | None = Field(
        default=None,
        description="Line number",
    )


class CICSOperation(BaseModel):
    """Description of a CICS command."""

    command: str = Field(
        ...,
        description="CICS command (RECEIVE, SEND, READ, WRITE, LINK, etc.)",
    )
    resource: str | None = Field(
        default=None,
        description="Map name, file name, queue name, etc.",
    )
    purpose: str | None = Field(
        default=None,
        description="Why this operation is performed",
    )
    citation: int | None = Field(
        default=None,
        description="Line number",
    )


class OpenQuestion(BaseModel):
    """An unresolved question about the program."""

    question: str = Field(
        ...,
        description="What remains unclear",
    )
    context: str | None = Field(
        default=None,
        description="Why it could not be determined",
    )
    suggestion: str | None = Field(
        default=None,
        description="How it might be resolved",
    )


class DocumentationTemplate(BaseModel):
    """Complete documentation template for COBOL/PL/I programs.

    This is the primary output of the Scribe agent and represents
    comprehensive documentation of a mainframe program.
    """

    header: HeaderSection = Field(
        ...,
        description="Metadata about the analyzed file",
    )
    purpose: PurposeSection = Field(
        ...,
        description="What the program does",
    )
    inputs: list[InputOutput] = Field(
        default_factory=list,
        description="Program inputs",
    )
    outputs: list[InputOutput] = Field(
        default_factory=list,
        description="Program outputs",
    )
    called_programs: list[CalledProgram] = Field(
        default_factory=list,
        description="Programs called by this program",
    )
    calling_context: CallingContext = Field(
        default_factory=CallingContext,
        description="How this program is invoked",
    )
    business_rules: list[BusinessRule] = Field(
        default_factory=list,
        description="Business rules implemented",
    )
    data_flow: DataFlow = Field(
        default_factory=DataFlow,
        description="Data flow through the program",
    )
    copybooks_used: list[CopybookReference] = Field(
        default_factory=list,
        description="Copybooks included",
    )
    paragraphs: list[Paragraph] = Field(
        default_factory=list,
        description="Key paragraphs",
    )
    error_handling: list[ErrorHandler] = Field(
        default_factory=list,
        description="Error handling cases",
    )
    sql_operations: list[SQLOperation] = Field(
        default_factory=list,
        description="Embedded SQL operations (if DB2)",
    )
    cics_operations: list[CICSOperation] = Field(
        default_factory=list,
        description="CICS commands (if CICS)",
    )
    open_questions: list[OpenQuestion] = Field(
        default_factory=list,
        description="Unresolved questions",
    )


# =============================================================================
# Copybook Template
# =============================================================================


class RecordField(BaseModel):
    """Description of a field in a copybook record layout."""

    level: int = Field(
        ...,
        ge=1,
        le=88,
        description="COBOL level number (01, 05, 10, etc.)",
    )
    field_name: str = Field(
        ...,
        description="Field name",
    )
    picture: str | None = Field(
        default=None,
        description="PIC clause",
    )
    usage: UsageType = Field(
        default=UsageType.DISPLAY,
        description="Usage clause (DISPLAY, COMP, COMP-3, etc.)",
    )
    redefines: str | None = Field(
        default=None,
        description="What this field redefines",
    )
    occurs: tuple[int, int] | None = Field(
        default=None,
        description="Array bounds (min, max) if OCCURS",
    )
    description: str | None = Field(
        default=None,
        description="Business meaning of this field",
    )
    valid_values: LenientStrList = Field(
        default_factory=list,
        description="Known valid values",
    )
    citation: int | None = Field(
        default=None,
        description="Line number",
    )


class KeyField(BaseModel):
    """Description of a notable field in a copybook."""

    field_name: str = Field(
        ...,
        description="Name of the key field",
    )
    significance: str = Field(
        ...,
        description="Why this field is important",
    )
    related_programs: LenientStrList = Field(
        default_factory=list,
        description="Programs that use this field",
    )


class CopybookHeader(BaseModel):
    """Header section for copybook documentation."""

    copybook_name: str = Field(
        ...,
        description="Name of the copybook",
    )
    file_name: str = Field(
        ...,
        description="Source file path",
    )
    file_type: FileType = Field(
        default=FileType.COPYBOOK,
        description="Always COPYBOOK for this template",
    )
    analyzed_at: datetime = Field(
        default_factory=datetime.utcnow,
        description="Timestamp of analysis",
    )


class CopybookPurpose(BaseModel):
    """Purpose section for copybook documentation."""

    summary: str = Field(
        ...,
        description="What data structure this defines",
    )
    usage_context: str | None = Field(
        default=None,
        description="File record, commarea, working storage, etc.",
    )


class CopybookTemplate(BaseModel):
    """Documentation template for copybooks.

    Copybooks define data structures and are simpler than full programs.
    """

    header: CopybookHeader = Field(
        ...,
        description="Copybook metadata",
    )
    purpose: CopybookPurpose = Field(
        ...,
        description="What this copybook defines",
    )
    record_layout: list[RecordField] = Field(
        default_factory=list,
        description="Field definitions",
    )
    key_fields: list[KeyField] = Field(
        default_factory=list,
        description="Notable fields",
    )
    used_by: LenientStrList = Field(
        default_factory=list,
        description="Programs that COPY this copybook",
    )


# =============================================================================
# JCL Template
# =============================================================================


class JCLHeader(BaseModel):
    """Header section for JCL job documentation."""

    job_name: str = Field(
        ...,
        description="JOB name",
    )
    file_name: str = Field(
        ...,
        description="Source file path",
    )
    file_type: FileType = Field(
        default=FileType.JCL,
        description="Always JCL for this template",
    )
    analyzed_at: datetime = Field(
        default_factory=datetime.utcnow,
        description="Timestamp of analysis",
    )


class JCLPurpose(BaseModel):
    """Purpose section for JCL job documentation."""

    summary: str = Field(
        ...,
        description="What this job accomplishes",
    )
    schedule: str | None = Field(
        default=None,
        description="When this runs (if known)",
    )
    business_context: str | None = Field(
        default=None,
        description="What business process this serves",
    )
    job_class: str | None = Field(
        default=None,
        description="Execution class",
    )
    msgclass: str | None = Field(
        default=None,
        description="Output class",
    )


class JCLStep(BaseModel):
    """Description of a JCL EXEC step."""

    step_name: str = Field(
        ...,
        description="EXEC step name",
    )
    program: str | None = Field(
        default=None,
        description="PGM= value (program executed)",
    )
    proc: str | None = Field(
        default=None,
        description="PROC= value (if procedure call)",
    )
    purpose: str | None = Field(
        default=None,
        description="What this step accomplishes",
    )
    condition: str | None = Field(
        default=None,
        description="COND= parameter",
    )
    citation: int | None = Field(
        default=None,
        description="Line number",
    )


class JCLDDStatement(BaseModel):
    """Description of a DD statement."""

    step_name: str = Field(
        ...,
        description="Which step this belongs to",
    )
    dd_name: str = Field(
        ...,
        description="DD name",
    )
    dataset: str | None = Field(
        default=None,
        description="DSN= value",
    )
    disposition: str | None = Field(
        default=None,
        description="DISP= value",
    )
    dcb: str | None = Field(
        default=None,
        description="DCB parameters",
    )
    purpose: str | None = Field(
        default=None,
        description="What this file is for",
    )
    citation: int | None = Field(
        default=None,
        description="Line number",
    )


class JCLDependencies(BaseModel):
    """Job dependency information."""

    input_datasets: LenientStrList = Field(
        default_factory=list,
        description="Datasets that must exist before job runs",
    )
    output_datasets: LenientStrList = Field(
        default_factory=list,
        description="Datasets created/modified by job",
    )
    predecessor_jobs: LenientStrList = Field(
        default_factory=list,
        description="Jobs that must complete first (if known)",
    )
    successor_jobs: LenientStrList = Field(
        default_factory=list,
        description="Jobs that depend on this (if known)",
    )


class JCLRestartRecovery(BaseModel):
    """Restart and recovery information."""

    restart_points: LenientStrList = Field(
        default_factory=list,
        description="Steps that can be restarted",
    )
    abend_handling: str | None = Field(
        default=None,
        description="How failures are handled",
    )


class JCLTemplate(BaseModel):
    """Documentation template for JCL jobs."""

    header: JCLHeader = Field(
        ...,
        description="Job metadata",
    )
    purpose: JCLPurpose = Field(
        ...,
        description="What this job does",
    )
    steps: list[JCLStep] = Field(
        default_factory=list,
        description="EXEC steps",
    )
    dd_statements: list[JCLDDStatement] = Field(
        default_factory=list,
        description="DD statements",
    )
    dependencies: JCLDependencies = Field(
        default_factory=JCLDependencies,
        description="Job dependencies",
    )
    restart_recovery: JCLRestartRecovery = Field(
        default_factory=JCLRestartRecovery,
        description="Restart and recovery info",
    )
