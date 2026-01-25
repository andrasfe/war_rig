"""
Artifact specification schema models.

This module defines the Pydantic models for artifact specifications,
including enumerations for artifact types, categories, and relationships,
as well as the complete ArtifactSpec model that drives extraction.
"""

from enum import Enum
from typing import Literal

from pydantic import BaseModel, Field


class ArtifactCategory(str, Enum):
    """Top-level categorization of artifacts."""

    CODE = "code"
    DATA = "data"
    INTERFACE = "interface"


class ArtifactType(str, Enum):
    """Specific artifact types within categories."""

    # Code artifacts
    PROGRAM = "program"  # Executable unit (COBOL program, main class)
    PARAGRAPH = "paragraph"  # COBOL paragraph/section
    PROCEDURE = "procedure"  # JCL PROC, stored procedure
    COPYBOOK = "copybook"  # Included source fragment
    MACRO = "macro"  # Assembler macro, C macro
    FUNCTION = "function"  # Standalone function
    CLASS = "class"  # OOP class
    METHOD = "method"  # Class method
    MODULE = "module"  # Python module, etc.

    # Data artifacts
    TABLE = "table"  # Database table
    VIEW = "view"  # Database view
    FILE = "file"  # VSAM, sequential, GDG
    RECORD_LAYOUT = "record_layout"  # Copybook structure, FD
    COLUMN = "column"  # Table column
    INDEX = "index"  # Database index
    SEGMENT = "segment"  # IMS segment
    DATASET = "dataset"  # JCL dataset reference

    # Interface artifacts
    TRANSACTION = "transaction"  # CICS transaction
    SCREEN = "screen"  # BMS map, screen definition
    QUEUE = "queue"  # MQ queue, TS queue
    SERVICE = "service"  # Web service, API endpoint
    MAP = "map"  # BMS map


class RelationshipType(str, Enum):
    """Types of relationships between artifacts."""

    # Code-to-code relationships
    CALLS = "calls"  # CALL, LINK, XCTL, function call
    INCLUDES = "includes"  # COPY, INCLUDE, import
    EXECUTES = "executes"  # JCL EXEC PGM=
    INHERITS = "inherits"  # OOP inheritance
    IMPORTS = "imports"  # Module import
    PERFORMS = "performs"  # COBOL PERFORM

    # Code-to-data relationships
    READS = "reads"  # SELECT, READ, GET
    WRITES = "writes"  # INSERT, WRITE, PUT
    UPDATES = "updates"  # UPDATE, REWRITE
    DELETES = "deletes"  # DELETE
    DEFINES = "defines"  # DDL CREATE, FD
    USES_LAYOUT = "uses_layout"  # Program uses copybook for record
    REFERENCES = "references"  # Generic data reference

    # Interface relationships
    RECEIVES_FROM = "receives_from"  # RECEIVE MAP
    SENDS_TO = "sends_to"  # SEND MAP
    TRIGGERS = "triggers"  # Transaction starts program
    ENQUEUES = "enqueues"  # WRITEQ, MQ PUT
    DEQUEUES = "dequeues"  # READQ, MQ GET


class CommentSyntax(BaseModel):
    """How comments are written in this language."""

    line_prefix: str | None = None  # e.g., "//" or "#"
    block_start: str | None = None  # e.g., "/*"
    block_end: str | None = None  # e.g., "*/"
    # For COBOL-style fixed-format
    fixed_column: int | None = None  # e.g., column 7
    fixed_indicator: str | None = None  # e.g., "*"
    # Columns to strip from each line (e.g., 1-6 for COBOL sequence numbers)
    strip_columns_start: int | None = None  # 1-indexed, inclusive
    strip_columns_end: int | None = None  # 1-indexed, inclusive


class StringSyntax(BaseModel):
    """How string literals are written."""

    delimiters: list[str] = Field(default_factory=lambda: ['"', "'"])
    escape_char: str = "\\"
    triple_quoted: bool = False  # Python-style """


class ContinuationSyntax(BaseModel):
    """How line continuation works."""

    trailing_char: str | None = None  # e.g., "\\" at end of line
    leading_char: str | None = None  # e.g., "-" in column 7 for COBOL
    leading_column: int | None = None  # Column for continuation indicator


class ScopeSyntax(BaseModel):
    """How code blocks/scopes are delimited."""

    start_pattern: str  # Regex for scope start
    end_pattern: str  # Regex for scope end
    name_pattern: str | None = None  # Regex to extract scope name


class ExtractionPattern(BaseModel):
    """A single pattern for extracting artifacts or references."""

    name: str  # Descriptive name for debugging
    pattern: str  # Regex pattern

    # Capture group handling
    capture_groups: list[int] = Field(default_factory=lambda: [1])  # Which groups form the identifier
    join_with: str = ""  # Separator when joining groups

    # What this pattern extracts
    artifact_type: ArtifactType | None = None
    relationship_type: RelationshipType | None = None

    # Context constraints
    must_be_in_scope: str | None = None  # Only match inside this scope type
    must_not_follow: str | None = None  # Negative lookbehind pattern
    must_not_precede: str | None = None  # Negative lookahead pattern

    # Additional extractions from same match
    column_pattern: str | None = None  # Extract column names (for SQL)
    target_type_hint: ArtifactType | None = None  # Expected type of target

    # String handling
    unmask_capture: bool = False  # If True, restore original string from placeholder

    # Regex flags
    ignore_case: bool = False
    multiline: bool = False
    dotall: bool = False


class NamingConvention(BaseModel):
    """Naming conventions for alias resolution."""

    case: Literal["upper", "lower", "preserve"] = "preserve"
    max_length: int | None = None  # Truncation length
    strip_chars: str = ""  # Characters to remove
    replace_map: dict[str, str] = Field(default_factory=dict)  # Character replacements
    common_abbreviations: dict[str, list[str]] = Field(
        default_factory=dict
    )  # CUSTOMER -> [CUST, CUSTMR]


class ArtifactSpec(BaseModel):
    """
    Complete specification for analyzing one artifact type.

    This is the core data structure. Specs are:
    - Generated by LLM for unknown languages
    - Shipped as built-ins for common languages
    - Cached for reuse
    - Human-editable for customization
    """

    # Metadata
    spec_version: str = "1.0"
    spec_id: str  # Unique identifier, e.g., "cobol-v1"
    language: str  # Human-readable name
    description: str  # What this spec covers

    # File matching
    file_extensions: list[str]  # e.g., [".cbl", ".cob"]
    file_patterns: list[str] = Field(default_factory=list)  # Glob patterns, e.g., ["JCL*"]

    # Categorization
    category: ArtifactCategory
    primary_artifact_type: ArtifactType  # Main thing this file defines

    # Syntax definitions
    comments: CommentSyntax
    strings: StringSyntax = Field(default_factory=StringSyntax)
    continuation: ContinuationSyntax | None = None
    scope: ScopeSyntax | None = None

    # Extraction patterns
    definition_patterns: list[ExtractionPattern]  # What this file defines
    reference_patterns: list[ExtractionPattern]  # What this file references
    preprocessor_patterns: list[ExtractionPattern] = Field(
        default_factory=list
    )  # COPY, INCLUDE, etc.

    # File-level artifact creation
    create_file_artifact: bool = False  # If True, create an artifact from the file stem

    # Naming conventions for resolution
    naming: NamingConvention = Field(default_factory=NamingConvention)

    # Known aliases (manually maintained)
    known_aliases: dict[str, str] = Field(default_factory=dict)  # alias -> canonical


class AliasRule(BaseModel):
    """
    Rule for resolving aliases between artifact types.

    Example: COBOL SQL reference "CUSTMAST" -> DDL table "CUSTOMER_MASTER"
    """

    source_type: ArtifactType | None = None  # None means any source type
    target_type: ArtifactType
    transformations: list[str]  # Ordered transforms to try
    # Supported: uppercase, lowercase, remove_hyphens, remove_underscores,
    #            truncate_N, add_prefix_X, add_suffix_X, abbreviations
    confidence_base: float = 0.9  # Base confidence if match found
