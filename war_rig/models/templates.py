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


def coerce_optional_int(v: Any) -> int | None:
    """Coerce a value to int or None.  Handles LLM quirks like ``[]``."""
    if v is None:
        return None
    if isinstance(v, int):
        return v
    if isinstance(v, float):
        return int(v)
    if isinstance(v, list):
        # [] -> None;  [42] -> 42
        ints = [x for x in v if isinstance(x, (int, float))]
        return int(ints[0]) if ints else None
    if isinstance(v, str):
        nums = re.findall(r"\d+", v)
        return int(nums[0]) if nums else None
    return None


def coerce_optional_int_pair(v: Any) -> tuple[int, int] | None:
    """Coerce a value to a (start, end) int pair or None."""
    if v is None:
        return None
    if isinstance(v, tuple) and len(v) == 2:
        if v[0] is not None and v[1] is not None:
            return (int(v[0]), int(v[1]))
        return None
    if isinstance(v, list):
        ints = [int(x) for x in v if x is not None and x != ""]
        if len(ints) >= 2:
            return (ints[0], ints[1])
        return None
    return None


LenientOptionalInt = Annotated[int | None, BeforeValidator(coerce_optional_int)]
LenientIntPair = Annotated[tuple[int, int] | None, BeforeValidator(coerce_optional_int_pair)]


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
    ASM = "ASM"  # Assembler/HLASM
    REXX = "REXX"  # REXX scripting
    CLIST = "CLIST"  # TSO command lists
    NATURAL = "NATURAL"  # Software AG 4GL
    EASYTRIEVE = "EASYTRIEVE"  # Report generator
    SORT = "SORT"  # DFSORT control cards
    DDL = "DDL"  # DB2 database definitions
    IMS = "IMS"  # IMS DBD/PSB definitions
    OTHER = "OTHER"


def _enum_missing_fallback(cls, value: object):  # type: ignore[no-untyped-def]
    """Shared _missing_ for template enums — normalise then fall back to OTHER."""
    if isinstance(value, str):
        upper = value.upper().replace(" ", "_").replace("-", "_")
        for member in cls:
            if member.value == upper:
                return member
    # Fall back to OTHER if the enum has it, otherwise None.
    return cls.__members__.get("OTHER")


class ProgramType(str, Enum):
    """Classification of program execution context."""

    BATCH = "BATCH"
    ONLINE_CICS = "ONLINE_CICS"
    SUBROUTINE = "SUBROUTINE"
    UTILITY = "UTILITY"
    BMS = "BMS"
    COPYBOOK = "COPYBOOK"
    JCL = "JCL"
    DDL = "DDL"
    DBD = "DBD"
    PSB = "PSB"
    ASSEMBLER = "ASSEMBLER"
    REXX = "REXX"
    PLI = "PLI"
    SQL = "SQL"
    DB2 = "DB2"
    OTHER = "OTHER"

    _missing_ = classmethod(_enum_missing_fallback)  # type: ignore[assignment]


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

    _missing_ = classmethod(_enum_missing_fallback)  # type: ignore[assignment]


class CallType(str, Enum):
    """Classification of program call types."""

    STATIC_CALL = "STATIC_CALL"
    DYNAMIC_CALL = "DYNAMIC_CALL"
    CICS_LINK = "CICS_LINK"
    CICS_XCTL = "CICS_XCTL"
    OTHER = "OTHER"

    _missing_ = classmethod(_enum_missing_fallback)  # type: ignore[assignment]


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

    _missing_ = classmethod(_enum_missing_fallback)  # type: ignore[assignment]


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

    _missing_ = classmethod(_enum_missing_fallback)  # type: ignore[assignment]


# =============================================================================
# Program Documentation Template Components
# =============================================================================


class HeaderSection(BaseModel):
    """Documentation header with metadata about the analyzed file."""

    program_id: str | None = Field(
        default=None,
        description="Program name extracted from source (e.g., PROGRAM-ID)",
    )
    file_name: str | None = Field(
        default=None,
        description="Source file path",
    )
    file_type: FileType | None = Field(
        default=None,
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

    summary: str | None = Field(
        default=None,
        description="2-3 sentence description of what this program does",
    )
    business_context: str | None = Field(
        default=None,
        description="What business process this serves",
    )
    program_type: ProgramType | None = Field(
        default=None,
        description="Classification of execution context (None for copybooks)",
    )
    citations: LenientIntList = Field(
        default_factory=list,
        description="Line numbers supporting the summary",
    )


class InputOutput(BaseModel):
    """Description of a program input or output resource."""

    name: str | None = Field(
        default=None,
        description="File, table, or parameter name",
    )
    io_type: IOType | None = Field(
        default=None,
        description="Classification of the resource type",
    )
    description: str | None = Field(
        default=None,
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

    program_name: str | None = Field(
        default=None,
        description="Name of the called program",
    )
    call_type: CallType | None = Field(
        default=None,
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
    citation: LenientOptionalInt = Field(
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

    rule_id: str | None = Field(
        default=None,
        description="Sequential identifier for the rule",
    )
    description: str | None = Field(
        default=None,
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

    source: str | None = Field(default=None, description="Source of the data")
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

    destination: str | None = Field(default=None, description="Destination for the data")
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

    input_field: str | None = Field(default=None, description="Source field")
    output_field: str | None = Field(default=None, description="Target field")
    transformation_description: str | None = Field(
        default=None,
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

    copybook_name: str | None = Field(
        default=None,
        description="Name of the copybook",
    )
    purpose: str | None = Field(
        default=None,
        description="What data structure it defines",
    )
    location: CopybookLocation | None = Field(
        default=None,
        description="Where the copybook is included",
    )
    citation: LenientOptionalInt = Field(
        default=None,
        description="Line number of COPY statement",
    )


class FunctionCall(BaseModel):
    """A call reference from Citadel analysis.

    Represents an outgoing call from a paragraph/function to another
    function, program, or paragraph. This is programmatic data from
    Citadel's static analysis, not LLM-generated.
    """

    target: str = Field(
        description="Name of called function/program/paragraph",
    )
    call_type: str = Field(
        default="performs",
        description="Type of call: 'performs', 'calls', 'includes', 'links', 'xctl', etc.",
    )
    line: int | None = Field(
        default=None,
        description="Line number where the call occurs",
    )


class CallerReference(BaseModel):
    """An incoming call reference from another file/function.

    Represents an incoming call to this paragraph/function from another
    location. This is programmatic data from Citadel's cross-file analysis,
    not LLM-generated.
    """

    file: str = Field(
        description="Source file path containing the caller",
    )
    function: str = Field(
        description="Name of the calling function/paragraph",
    )
    line: int | None = Field(
        default=None,
        description="Line number of the call in the source file",
    )
    call_type: str = Field(
        default="performs",
        description="Type of call: 'performs', 'calls', 'includes', etc.",
    )


class CallSemantics(BaseModel):
    """Data flow semantics for a paragraph call.

    Captures the data flow between caller and callee paragraphs,
    including what variables are passed in and what gets modified.
    This is LLM-inferred documentation about the semantic meaning
    of inter-paragraph calls.
    """

    caller: str = Field(..., description="Name of the calling paragraph")
    callee: str = Field(..., description="Name of the called paragraph")
    inputs: list[str] = Field(
        default_factory=list,
        description="Variables/data passed into the call",
    )
    outputs: list[str] = Field(
        default_factory=list,
        description="Variables/data returned or modified",
    )
    purpose: str | None = Field(
        default=None,
        description="Brief description of what the call accomplishes",
    )


class Paragraph(BaseModel):
    """Description of a key paragraph/function in the program.

    Contains both LLM-generated documentation (summary, purpose) and
    Citadel-provided programmatic call references (outgoing_calls, incoming_calls).
    """

    paragraph_name: str | None = Field(
        default=None,
        description="Paragraph name",
    )
    summary: str | None = Field(
        default=None,
        description="LLM-generated prose summary of what this paragraph does",
    )
    purpose: str | None = Field(
        default=None,
        description="Business purpose or role of this paragraph",
    )
    called_by: LenientStrList = Field(
        default_factory=list,
        description="Paragraphs that PERFORM this (legacy simple list)",
    )
    calls: LenientStrList = Field(
        default_factory=list,
        description="Paragraphs this PERFORMs (legacy simple list)",
    )
    citation: LenientIntPair = Field(
        default=None,
        description="Line number range (start, end)",
    )
    # Citadel-provided structured call references (programmatic, not LLM)
    outgoing_calls: list[FunctionCall] = Field(
        default_factory=list,
        description="Citadel-provided outgoing call references with full metadata",
    )
    incoming_calls: list[CallerReference] = Field(
        default_factory=list,
        description="Citadel-provided incoming call references from other files/functions",
    )
    is_dead_code: bool = Field(
        default=False,
        description="Whether this paragraph is dead code (never referenced)",
    )
    dead_code_reason: str | None = Field(
        default=None,
        description="Explanation of why this is considered dead code",
    )
    metadata: dict[str, Any] | None = Field(
        default=None,
        description="Additional metadata for processing (e.g., body_chunks count)",
    )


class ErrorHandler(BaseModel):
    """Description of an error handling case."""

    condition: str | None = Field(
        default=None,
        description="What error condition is handled",
    )
    action: str | None = Field(
        default=None,
        description="What happens (ABEND, return code, etc.)",
    )
    citation: LenientIntList = Field(
        default_factory=list,
        description="Line numbers",
    )


class SQLOperation(BaseModel):
    """Description of an embedded SQL operation."""

    operation: str | None = Field(
        default=None,
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
    citation: LenientOptionalInt = Field(
        default=None,
        description="Line number",
    )


class CICSOperation(BaseModel):
    """Description of a CICS command."""

    command: str | None = Field(
        default=None,
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
    citation: LenientOptionalInt = Field(
        default=None,
        description="Line number",
    )


class OpenQuestion(BaseModel):
    """An unresolved question about the program."""

    question: str | None = Field(
        default=None,
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


class ResolvedQuestion(BaseModel):
    """A question resolved by automated CodeWhisper analysis."""

    original_question: str | None = Field(
        default=None,
        description="The original question text",
    )
    original_context: str | None = Field(
        default=None,
        description="Original context for why the question was asked",
    )
    answer: str | None = Field(
        default=None,
        description="The resolved answer",
    )
    confidence: str = Field(
        default="MEDIUM",
        description="Answer confidence: HIGH, MEDIUM, LOW",
    )
    resolved_by: str = Field(
        default="CODEWHISPER",
        description="What resolved this question",
    )
    cycle_resolved: int = Field(
        default=1,
        description="Which cycle this was resolved in",
    )
    tool_calls_used: int = Field(
        default=0,
        description="Number of tool calls used to resolve",
    )


class DeadCodeItem(BaseModel):
    """An artifact identified as dead code by static analysis."""

    name: str = Field(description="Name of the unreferenced artifact")
    artifact_type: str = Field(description="Type: paragraph, copybook, program, etc.")
    line: int | None = Field(default=None, description="Line number in source")
    reason: str = Field(description="Why this is considered dead code")


class DocumentationTemplate(BaseModel):
    """Complete documentation template for COBOL/PL/I programs.

    This is the primary output of the Scribe agent and represents
    comprehensive documentation of a mainframe program.
    """

    header: HeaderSection | None = Field(
        default=None,
        description="Metadata about the analyzed file",
    )
    purpose: PurposeSection | None = Field(
        default=None,
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
    resolved_questions: list[ResolvedQuestion] = Field(
        default_factory=list,
        description="Questions resolved by automated analysis",
    )
    dead_code: list[DeadCodeItem] = Field(
        default_factory=list,
        description="Artifacts identified as dead code by static analysis",
    )
    call_semantics: list[CallSemantics] = Field(
        default_factory=list,
        description="Data flow semantics for paragraph calls (LLM-inferred)",
    )
    flow_diagram: str | None = Field(
        default=None,
        description="Mermaid flowchart of internal control flow (COBOL only)",
    )
    copybooks_not_found: list[str] = Field(
        default_factory=list,
        description="Copybooks referenced but not found during analysis",
    )

    @classmethod
    def load_lenient(cls, data: dict) -> "DocumentationTemplate":
        """Load template data leniently - log validation errors but don't fail.

        First attempts model_validate() which handles nested model construction.
        On failure, logs the error and recursively constructs nested models.
        """
        import logging
        logger = logging.getLogger(__name__)

        # Try validation first - this properly constructs nested models
        try:
            return cls.model_validate(data)
        except Exception as e:
            logger.warning(f"Template validation warning (loading anyway): {e}")
            # Fall back to deep construct without validation
            return cls._deep_construct(data)

    @classmethod
    def _deep_construct(cls, data: dict) -> "DocumentationTemplate":
        """Recursively construct template with nested models."""
        constructed = {}
        for key, value in data.items():
            if key == "header" and isinstance(value, dict):
                constructed[key] = HeaderSection.model_construct(**value)
            elif key == "purpose" and isinstance(value, dict):
                constructed[key] = PurposeSection.model_construct(**value)
            elif key == "calling_context" and isinstance(value, dict):
                constructed[key] = CallingContext.model_construct(**value)
            elif key == "data_flow" and isinstance(value, dict):
                constructed[key] = DataFlow.model_construct(**value)
            elif key in ("inputs", "outputs") and isinstance(value, list):
                constructed[key] = [InputOutput.model_construct(**v) if isinstance(v, dict) else v for v in value]
            elif key == "called_programs" and isinstance(value, list):
                constructed[key] = [CalledProgram.model_construct(**v) if isinstance(v, dict) else v for v in value]
            elif key == "business_rules" and isinstance(value, list):
                constructed[key] = [BusinessRule.model_construct(**v) if isinstance(v, dict) else v for v in value]
            elif key == "copybooks_used" and isinstance(value, list):
                constructed[key] = [CopybookReference.model_construct(**v) if isinstance(v, dict) else v for v in value]
            elif key == "paragraphs" and isinstance(value, list):
                constructed[key] = [cls._construct_paragraph(v) if isinstance(v, dict) else v for v in value]
            elif key == "error_handling" and isinstance(value, list):
                constructed[key] = [ErrorHandler.model_construct(**v) if isinstance(v, dict) else v for v in value]
            elif key == "sql_operations" and isinstance(value, list):
                constructed[key] = [SQLOperation.model_construct(**v) if isinstance(v, dict) else v for v in value]
            elif key == "cics_operations" and isinstance(value, list):
                constructed[key] = [CICSOperation.model_construct(**v) if isinstance(v, dict) else v for v in value]
            elif key == "open_questions" and isinstance(value, list):
                constructed[key] = [OpenQuestion.model_construct(**v) if isinstance(v, dict) else v for v in value]
            elif key == "resolved_questions" and isinstance(value, list):
                constructed[key] = [
                    ResolvedQuestion.model_construct(**v) if isinstance(v, dict) else v
                    for v in value
                ]
            elif key == "dead_code" and isinstance(value, list):
                constructed[key] = [DeadCodeItem.model_construct(**v) if isinstance(v, dict) else v for v in value]
            elif key == "call_semantics" and isinstance(value, list):
                constructed[key] = [CallSemantics.model_construct(**v) if isinstance(v, dict) else v for v in value]
            else:
                constructed[key] = value
        return cls.model_construct(**constructed)

    @classmethod
    def _construct_paragraph(cls, data: dict) -> "Paragraph":
        """Construct a Paragraph with nested FunctionCall and CallerReference models."""
        constructed = dict(data)

        # Handle outgoing_calls list
        if "outgoing_calls" in data and isinstance(data["outgoing_calls"], list):
            constructed["outgoing_calls"] = [
                FunctionCall.model_construct(**c) if isinstance(c, dict) else c
                for c in data["outgoing_calls"]
            ]

        # Handle incoming_calls list
        if "incoming_calls" in data and isinstance(data["incoming_calls"], list):
            constructed["incoming_calls"] = [
                CallerReference.model_construct(**c) if isinstance(c, dict) else c
                for c in data["incoming_calls"]
            ]

        return Paragraph.model_construct(**constructed)


# =============================================================================
# Copybook Template
# =============================================================================


class RecordField(BaseModel):
    """Description of a field in a copybook record layout."""

    level: int | None = Field(
        default=None,
        ge=1,
        le=88,
        description="COBOL level number (01, 05, 10, etc.)",
    )
    field_name: str | None = Field(
        default=None,
        description="Field name",
    )
    picture: str | None = Field(
        default=None,
        description="PIC clause",
    )
    usage: UsageType | None = Field(
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
    citation: LenientOptionalInt = Field(
        default=None,
        description="Line number",
    )


class KeyField(BaseModel):
    """Description of a notable field in a copybook."""

    field_name: str | None = Field(
        default=None,
        description="Name of the key field",
    )
    significance: str | None = Field(
        default=None,
        description="Why this field is important",
    )
    related_programs: LenientStrList = Field(
        default_factory=list,
        description="Programs that use this field",
    )


class CopybookHeader(BaseModel):
    """Header section for copybook documentation."""

    copybook_name: str | None = Field(
        default=None,
        description="Name of the copybook",
    )
    file_name: str | None = Field(
        default=None,
        description="Source file path",
    )
    file_type: FileType | None = Field(
        default=FileType.COPYBOOK,
        description="Always COPYBOOK for this template",
    )
    analyzed_at: datetime = Field(
        default_factory=datetime.utcnow,
        description="Timestamp of analysis",
    )


class CopybookPurpose(BaseModel):
    """Purpose section for copybook documentation."""

    summary: str | None = Field(
        default=None,
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

    header: CopybookHeader | None = Field(
        default=None,
        description="Copybook metadata",
    )
    purpose: CopybookPurpose | None = Field(
        default=None,
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

    job_name: str | None = Field(
        default=None,
        description="JOB name",
    )
    file_name: str | None = Field(
        default=None,
        description="Source file path",
    )
    file_type: FileType | None = Field(
        default=FileType.JCL,
        description="Always JCL for this template",
    )
    analyzed_at: datetime = Field(
        default_factory=datetime.utcnow,
        description="Timestamp of analysis",
    )


class JCLPurpose(BaseModel):
    """Purpose section for JCL job documentation."""

    summary: str | None = Field(
        default=None,
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

    step_name: str | None = Field(
        default=None,
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
    citation: LenientOptionalInt = Field(
        default=None,
        description="Line number",
    )


class JCLDDStatement(BaseModel):
    """Description of a DD statement."""

    step_name: str | None = Field(
        default=None,
        description="Which step this belongs to",
    )
    dd_name: str | None = Field(
        default=None,
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
    citation: LenientOptionalInt = Field(
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

    header: JCLHeader | None = Field(
        default=None,
        description="Job metadata",
    )
    purpose: JCLPurpose | None = Field(
        default=None,
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
