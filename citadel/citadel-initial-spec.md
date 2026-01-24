# Universal Dependency Graph Builder

## Specification for Claude Code Implementation

---

## 1. Project Overview

### 1.1 Purpose

Build a universal dependency graph extraction tool that analyzes source code repositories containing mixed languages—with emphasis on mainframe artifacts—to produce a comprehensive graph of code-to-code, code-to-data, and interface relationships.

The tool uses LLM-generated language/artifact specifications combined with deterministic parsing for reliability and performance. The LLM identifies patterns once per language; all subsequent extraction is regex-based and deterministic.

### 1.2 Key Design Principles

1. **Hybrid approach**: LLM for language understanding, deterministic parsing for extraction
2. **Spec-driven**: All extraction rules live in cacheable, human-editable YAML specs
3. **Universal**: Handle any language by generating specs on demand
4. **Mainframe-aware**: First-class support for COBOL, JCL, DB2, CICS, copybooks
5. **Data-layer integration**: Track code-to-data relationships, bind copybooks to DDL
6. **Incremental**: Support re-analysis of changed files without full rebuild

### 1.3 Monorepo Context

This project lives alongside existing projects in the monorepo:

```
monorepo/
├── llm-provider/           # Shared LLM abstraction (existing)
├── war-rig/                # Documentation system (existing)
└── dependency-graph/       # THIS PROJECT
```

Uses shared `.env` for configuration with a new entry for this tool's LLM settings.

---

## 2. Architecture

### 2.1 High-Level Flow

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                              ORCHESTRATOR                                    │
│  Coordinates all phases, manages parallelism, tracks progress               │
└─────────────────────────────────┬───────────────────────────────────────────┘
                                  │
         ┌────────────────────────┼────────────────────────┐
         ▼                        ▼                        ▼
┌─────────────────┐      ┌─────────────────┐      ┌─────────────────┐
│   DISCOVERY     │      │  SPEC MANAGER   │      │   LLM ADAPTER   │
│                 │      │                 │      │                 │
│ - File walking  │      │ - Built-in load │      │ - llm-provider  │
│ - Type detect   │      │ - Cache lookup  │      │ - Spec gen      │
│ - Grouping      │      │ - Validation    │      │ - Disambiguate  │
└────────┬────────┘      └────────┬────────┘      └────────┬────────┘
         │                        │                        │
         └────────────────────────┼────────────────────────┘
                                  ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                            PARSER ENGINE                                     │
│  Deterministic extraction using specs                                        │
│  - Preprocessor (comments, strings, continuations)                          │
│  - Scope tracker (which artifact contains current line)                     │
│  - Definition extractor (what this file defines)                            │
│  - Reference extractor (what this file references)                          │
└─────────────────────────────────┬───────────────────────────────────────────┘
                                  │
                                  ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                          ARTIFACT REGISTRY                                   │
│  Central index of all discovered artifacts                                   │
│  - Canonical name index                                                      │
│  - Alias index                                                               │
│  - Type index                                                                │
│  - Fuzzy match index                                                         │
└─────────────────────────────────┬───────────────────────────────────────────┘
                                  │
                                  ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                            RESOLVER                                          │
│  Binds references to artifacts                                               │
│  - Exact match                                                               │
│  - Alias rules                                                               │
│  - Transformations                                                           │
│  - Fuzzy match                                                               │
│  - LLM disambiguation (optional)                                             │
└─────────────────────────────────┬───────────────────────────────────────────┘
                                  │
                                  ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                          GRAPH BUILDER                                       │
│  Assembles final output                                                      │
│  - Artifact nodes                                                            │
│  - Relationship edges                                                        │
│  - Unresolved tracking                                                       │
│  - Statistics                                                                │
└─────────────────────────────────┬───────────────────────────────────────────┘
                                  │
                                  ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                            EXPORTERS                                         │
│  Output formats: JSON, DOT, Cypher, CSV                                     │
└─────────────────────────────────────────────────────────────────────────────┘
```

### 2.2 Processing Phases

```
Phase 1: Discovery
├── Walk source tree respecting excludes
├── Match files to known specs by extension
├── Queue unknown files for spec generation
└── Output: Map of spec_name -> [file_paths]

Phase 2: Spec Resolution
├── Load built-in specs for known languages
├── Check cache for previously generated specs
├── Generate new specs via LLM for unknowns
├── Validate all specs against schema
└── Output: Complete spec for every file group

Phase 3: Parsing (parallel)
├── For each file:
│   ├── Preprocess (strip comments, handle continuations)
│   ├── Extract definitions (what this file defines)
│   ├── Extract references (what this file uses)
│   └── Extract preprocessor directives (COPY, INCLUDE)
└── Output: Per-file parse results

Phase 4: Registration
├── Register all artifacts in central registry
├── Process COPY/INCLUDE to resolve copybook contents
├── Build alias indices
└── Output: Populated artifact registry

Phase 5: Resolution
├── For each raw reference:
│   ├── Attempt exact match
│   ├── Attempt alias match
│   ├── Attempt transformation match
│   ├── Attempt fuzzy match
│   └── Queue for LLM if ambiguous
├── Special pass: bind copybooks to tables
└── Output: Resolved relationships + unresolved list

Phase 6: LLM Disambiguation (optional, batched)
├── Batch ambiguous references
├── Query LLM with context and candidates
└── Output: Additional resolved relationships

Phase 7: Assembly
├── Build final graph structure
├── Compute statistics
├── Validate consistency
└── Output: DependencyGraph

Phase 8: Export
├── Write requested format(s)
└── Output: Files on disk
```

---

## 3. Directory Structure

```
dependency-graph/
├── README.md
├── pyproject.toml
├── src/
│   └── depgraph/
│       ├── __init__.py
│       ├── cli.py                    # Click-based CLI
│       ├── config.py                 # .env loading, configuration
│       ├── orchestrator.py           # Main coordination logic
│       │
│       ├── discovery/
│       │   ├── __init__.py
│       │   └── file_walker.py        # Directory traversal, file typing
│       │
│       ├── specs/
│       │   ├── __init__.py
│       │   ├── schema.py             # Pydantic models for specs
│       │   ├── manager.py            # Load, cache, validate specs
│       │   └── generator.py          # LLM-based spec generation
│       │
│       ├── parser/
│       │   ├── __init__.py
│       │   ├── preprocessor.py       # Comment/string stripping
│       │   ├── engine.py             # Pattern-based extraction
│       │   └── scope_tracker.py      # Track nested scopes
│       │
│       ├── resolver/
│       │   ├── __init__.py
│       │   ├── registry.py           # Artifact registry
│       │   ├── alias_resolver.py     # Alias transformation rules
│       │   └── cross_reference.py    # Reference-to-artifact binding
│       │
│       ├── graph/
│       │   ├── __init__.py
│       │   ├── model.py              # Graph data models
│       │   ├── builder.py            # Graph assembly
│       │   └── exporters.py          # JSON, DOT, Cypher, CSV export
│       │
│       └── llm/
│           ├── __init__.py
│           └── adapter.py            # Wrapper around llm-provider
│
├── specs/
│   └── builtin/                      # Pre-built specs (version controlled)
│       ├── cobol.yaml
│       ├── copybook.yaml
│       ├── jcl.yaml
│       ├── db2_ddl.yaml
│       ├── cics_bms.yaml
│       ├── python.yaml
│       ├── java.yaml
│       └── sql_generic.yaml
│
├── tests/
│   ├── __init__.py
│   ├── conftest.py
│   ├── fixtures/
│   │   └── samples/                  # Sample source files for testing
│   │       ├── cobol/
│   │       ├── jcl/
│   │       ├── ddl/
│   │       ├── copybook/
│   │       ├── python/
│   │       └── mixed/
│   ├── test_discovery.py
│   ├── test_specs.py
│   ├── test_parser.py
│   ├── test_resolver.py
│   ├── test_graph.py
│   └── test_integration.py
│
└── cache/                            # Runtime cache (gitignored)
    ├── specs/                        # Generated specs
    └── parse_results/                # Cached parse results by file hash
```

---

## 4. Data Models

### 4.1 Enumerations

```python
# src/depgraph/specs/schema.py

from enum import Enum

class ArtifactCategory(str, Enum):
    """Top-level categorization of artifacts."""
    CODE = "code"
    DATA = "data"
    INTERFACE = "interface"


class ArtifactType(str, Enum):
    """Specific artifact types within categories."""
    
    # Code artifacts
    PROGRAM = "program"              # Executable unit (COBOL program, main class)
    PARAGRAPH = "paragraph"          # COBOL paragraph/section
    PROCEDURE = "procedure"          # JCL PROC, stored procedure
    COPYBOOK = "copybook"            # Included source fragment
    MACRO = "macro"                  # Assembler macro, C macro
    FUNCTION = "function"            # Standalone function
    CLASS = "class"                  # OOP class
    METHOD = "method"                # Class method
    MODULE = "module"                # Python module, etc.
    
    # Data artifacts
    TABLE = "table"                  # Database table
    VIEW = "view"                    # Database view
    FILE = "file"                    # VSAM, sequential, GDG
    RECORD_LAYOUT = "record_layout"  # Copybook structure, FD
    COLUMN = "column"                # Table column
    INDEX = "index"                  # Database index
    SEGMENT = "segment"              # IMS segment
    DATASET = "dataset"              # JCL dataset reference
    
    # Interface artifacts
    TRANSACTION = "transaction"      # CICS transaction
    SCREEN = "screen"                # BMS map, screen definition
    QUEUE = "queue"                  # MQ queue, TS queue
    SERVICE = "service"              # Web service, API endpoint
    MAP = "map"                      # BMS map


class RelationshipType(str, Enum):
    """Types of relationships between artifacts."""
    
    # Code-to-code relationships
    CALLS = "calls"                  # CALL, LINK, XCTL, function call
    INCLUDES = "includes"            # COPY, INCLUDE, import
    EXECUTES = "executes"            # JCL EXEC PGM=
    INHERITS = "inherits"            # OOP inheritance
    IMPORTS = "imports"              # Module import
    PERFORMS = "performs"            # COBOL PERFORM
    
    # Code-to-data relationships
    READS = "reads"                  # SELECT, READ, GET
    WRITES = "writes"                # INSERT, WRITE, PUT
    UPDATES = "updates"              # UPDATE, REWRITE
    DELETES = "deletes"              # DELETE
    DEFINES = "defines"              # DDL CREATE, FD
    USES_LAYOUT = "uses_layout"      # Program uses copybook for record
    REFERENCES = "references"        # Generic data reference
    
    # Interface relationships
    RECEIVES_FROM = "receives_from"  # RECEIVE MAP
    SENDS_TO = "sends_to"            # SEND MAP
    TRIGGERS = "triggers"            # Transaction starts program
    ENQUEUES = "enqueues"            # WRITEQ, MQ PUT
    DEQUEUES = "dequeues"            # READQ, MQ GET
```

### 4.2 Artifact Specification Schema

```python
# src/depgraph/specs/schema.py (continued)

from pydantic import BaseModel, Field
from typing import Literal


class CommentSyntax(BaseModel):
    """How comments are written in this language."""
    line_prefix: str | None = None           # e.g., "//" or "#"
    block_start: str | None = None           # e.g., "/*"
    block_end: str | None = None             # e.g., "*/"
    # For COBOL-style fixed-format
    fixed_column: int | None = None          # e.g., column 7
    fixed_indicator: str | None = None       # e.g., "*"


class StringSyntax(BaseModel):
    """How string literals are written."""
    delimiters: list[str] = ['"', "'"]
    escape_char: str = "\\"
    triple_quoted: bool = False              # Python-style """


class ContinuationSyntax(BaseModel):
    """How line continuation works."""
    trailing_char: str | None = None         # e.g., "\\" at end of line
    leading_char: str | None = None          # e.g., "-" in column 7 for COBOL
    leading_column: int | None = None        # Column for continuation indicator


class ScopeSyntax(BaseModel):
    """How code blocks/scopes are delimited."""
    start_pattern: str                       # Regex for scope start
    end_pattern: str                         # Regex for scope end
    name_pattern: str | None = None          # Regex to extract scope name


class ExtractionPattern(BaseModel):
    """A single pattern for extracting artifacts or references."""
    
    name: str                                # Descriptive name for debugging
    pattern: str                             # Regex pattern
    
    # Capture group handling
    capture_groups: list[int] = [1]          # Which groups form the identifier
    join_with: str = ""                      # Separator when joining groups
    
    # What this pattern extracts
    artifact_type: ArtifactType | None = None
    relationship_type: RelationshipType | None = None
    
    # Context constraints
    must_be_in_scope: str | None = None      # Only match inside this scope type
    must_not_follow: str | None = None       # Negative lookbehind pattern
    must_not_precede: str | None = None      # Negative lookahead pattern
    
    # Additional extractions from same match
    column_pattern: str | None = None        # Extract column names (for SQL)
    target_type_hint: ArtifactType | None = None  # Expected type of target
    
    # Regex flags
    ignore_case: bool = False
    multiline: bool = False
    dotall: bool = False


class NamingConvention(BaseModel):
    """Naming conventions for alias resolution."""
    case: Literal["upper", "lower", "preserve"] = "preserve"
    max_length: int | None = None            # Truncation length
    strip_chars: str = ""                    # Characters to remove
    replace_map: dict[str, str] = {}         # Character replacements
    common_abbreviations: dict[str, list[str]] = {}  # CUSTOMER -> [CUST, CUSTMR]


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
    spec_id: str                             # Unique identifier, e.g., "cobol-v1"
    language: str                            # Human-readable name
    description: str                         # What this spec covers
    
    # File matching
    file_extensions: list[str]               # e.g., [".cbl", ".cob"]
    file_patterns: list[str] = []            # Glob patterns, e.g., ["JCL*"]
    
    # Categorization
    category: ArtifactCategory
    primary_artifact_type: ArtifactType      # Main thing this file defines
    
    # Syntax definitions
    comments: CommentSyntax
    strings: StringSyntax
    continuation: ContinuationSyntax | None = None
    scope: ScopeSyntax | None = None
    
    # Extraction patterns
    definition_patterns: list[ExtractionPattern]   # What this file defines
    reference_patterns: list[ExtractionPattern]    # What this file references
    preprocessor_patterns: list[ExtractionPattern] = []  # COPY, INCLUDE, etc.
    
    # Naming conventions for resolution
    naming: NamingConvention = Field(default_factory=NamingConvention)
    
    # Known aliases (manually maintained)
    known_aliases: dict[str, str] = {}       # alias -> canonical


class AliasRule(BaseModel):
    """
    Rule for resolving aliases between artifact types.
    
    Example: COBOL SQL reference "CUSTMAST" -> DDL table "CUSTOMER_MASTER"
    """
    source_type: ArtifactType
    target_type: ArtifactType
    transformations: list[str]               # Ordered transforms to try
    # Supported: uppercase, lowercase, remove_hyphens, remove_underscores,
    #            truncate_N, add_prefix_X, add_suffix_X, abbreviations
    confidence_base: float = 0.9             # Base confidence if match found
```

### 4.3 Graph Model

```python
# src/depgraph/graph/model.py

from pydantic import BaseModel, Field
from datetime import datetime
from typing import Any


class SourceLocation(BaseModel):
    """Location in source code."""
    file_path: str
    line_start: int
    line_end: int | None = None
    column_start: int | None = None
    column_end: int | None = None
    
    def __str__(self) -> str:
        if self.line_end and self.line_end != self.line_start:
            return f"{self.file_path}:{self.line_start}-{self.line_end}"
        return f"{self.file_path}:{self.line_start}"


class Artifact(BaseModel):
    """
    A node in the dependency graph.
    
    Represents any identifiable entity: program, table, copybook, etc.
    """
    
    # Identity
    id: str                                  # "{type}::{canonical_name}"
    artifact_type: ArtifactType
    category: ArtifactCategory
    canonical_name: str
    
    # Aliases and naming
    aliases: list[str] = []
    display_name: str | None = None          # Human-friendly name
    
    # Source info
    defined_in: SourceLocation | None = None
    language: str
    
    # Type-specific attributes
    attributes: dict[str, Any] = {}
    # Examples:
    #   TABLE: {"columns": ["COL1", "COL2"], "primary_key": ["COL1"]}
    #   PROGRAM: {"entry_point": "MAIN", "compiler_options": [...]}
    #   RECORD_LAYOUT: {"fields": [...], "total_length": 500}
    
    # For record layouts: mapping to data artifact
    maps_to: str | None = None               # Artifact ID of table/file
    field_mappings: dict[str, str] = {}      # local_field -> canonical_column


class Relationship(BaseModel):
    """
    An edge in the dependency graph.
    
    Represents a relationship between two artifacts.
    """
    
    # Identity
    id: str                                  # Auto-generated UUID
    
    # Endpoints
    from_artifact: str                       # Artifact ID
    to_artifact: str                         # Artifact ID
    relationship_type: RelationshipType
    
    # Evidence
    location: SourceLocation                 # Where this relationship is expressed
    evidence_text: str                       # The actual source text
    
    # For data relationships
    columns_accessed: list[str] = []
    access_mode: str | None = None           # For files: INPUT, OUTPUT, I-O
    
    # Resolution metadata
    confidence: float = 1.0
    resolution_method: str = "exact"         # exact, alias, fuzzy, llm
    is_resolved: bool = True


class UnresolvedReference(BaseModel):
    """
    A reference that could not be resolved to a known artifact.
    """
    
    reference_text: str                      # What was referenced
    expected_type: ArtifactType | None = None
    location: SourceLocation
    containing_artifact: str | None = None   # Which artifact contains this ref
    
    # Resolution attempt info
    candidates: list[str] = []               # Artifact IDs that might match
    best_score: float = 0.0
    reason: str                              # Why resolution failed


class GraphStatistics(BaseModel):
    """Statistics about the dependency graph."""
    
    files_analyzed: int
    files_skipped: int
    files_failed: int
    
    artifacts_by_type: dict[str, int]
    artifacts_total: int
    
    relationships_by_type: dict[str, int]
    relationships_total: int
    
    unresolved_count: int
    resolution_rate: float                   # Percentage resolved
    
    languages_detected: list[str]
    specs_used: dict[str, str]               # language -> spec_id


class DependencyGraph(BaseModel):
    """
    The complete dependency graph output.
    
    This is the final deliverable of the analysis.
    """
    
    # Metadata
    version: str = "1.0"
    generated_at: datetime = Field(default_factory=datetime.utcnow)
    source_root: str
    
    # Graph data
    artifacts: dict[str, Artifact]           # ID -> Artifact
    relationships: list[Relationship]
    unresolved: list[UnresolvedReference]
    
    # Statistics
    statistics: GraphStatistics
    
    # Reproducibility
    config_hash: str                         # Hash of config used
    specs_hashes: dict[str, str]             # language -> spec content hash
```

### 4.4 Internal Models

```python
# src/depgraph/parser/engine.py (internal models)

class RawReference(BaseModel):
    """A reference before resolution (internal use)."""
    
    raw_text: str
    pattern_name: str                        # Which pattern matched
    expected_type: ArtifactType | None
    relationship_type: RelationshipType
    location: SourceLocation
    containing_artifact: str | None          # ID of artifact containing this
    columns_mentioned: list[str] = []
    context_lines: list[str] = []            # Surrounding code for LLM


class FileParseResult(BaseModel):
    """Result of parsing a single file (internal use)."""
    
    file_path: str
    language: str
    spec_id: str
    
    artifacts_defined: list[Artifact]
    references_found: list[RawReference]
    preprocessor_directives: list[dict]      # COPY/INCLUDE info
    
    errors: list[str]
    warnings: list[str]
```

---

## 5. Component Specifications

### 5.1 Configuration (`config.py`)

Loads from shared `.env` file in monorepo root.

**Environment Variables:**

```bash
# .env (in monorepo root)

# Existing llm-provider vars...
LLM_PROVIDER=anthropic
ANTHROPIC_API_KEY=sk-...

# New: dependency-graph specific
DEPGRAPH_LLM_MODEL=claude-sonnet-4-20250514
DEPGRAPH_CACHE_DIR=.cache/depgraph
DEPGRAPH_PARALLEL_FILES=4
DEPGRAPH_LLM_DISAMBIGUATION=true
DEPGRAPH_MAX_LLM_CALLS=100
```

**Config Class:**

```python
# src/depgraph/config.py

from pydantic_settings import BaseSettings
from pathlib import Path


class Config(BaseSettings):
    """Configuration loaded from .env."""
    
    # LLM settings (prefixed for this tool)
    depgraph_llm_model: str = "claude-sonnet-4-20250514"
    depgraph_cache_dir: Path = Path(".cache/depgraph")
    depgraph_parallel_files: int = 4
    depgraph_llm_disambiguation: bool = True
    depgraph_max_llm_calls: int = 100
    
    # Inherited from llm-provider
    llm_provider: str = "anthropic"
    anthropic_api_key: str | None = None
    
    class Config:
        env_file = ".env"
        env_file_encoding = "utf-8"


def load_config() -> Config:
    """Load configuration from .env file."""
    return Config()
```

### 5.2 File Discovery (`discovery/file_walker.py`)

```python
class FileDiscovery:
    """
    Traverses source directory and groups files by artifact spec.
    """
    
    def __init__(
        self,
        root: Path,
        spec_manager: SpecManager,
        exclude_patterns: list[str] | None = None
    ):
        """
        Args:
            root: Source directory root
            spec_manager: For looking up specs by extension
            exclude_patterns: Glob patterns to exclude
        """
    
    def discover(self) -> DiscoveryResult:
        """
        Walk directory tree and categorize files.
        
        Returns:
            DiscoveryResult with files grouped by spec
        """
    
    def identify_file(self, path: Path) -> str | None:
        """
        Identify which spec applies to a file.
        
        Returns:
            Spec ID or None if unknown
        """


class DiscoveryResult(BaseModel):
    """Result of file discovery."""
    
    files_by_spec: dict[str, list[Path]]     # spec_id -> files
    unknown_files: list[Path]                 # Files needing spec generation
    excluded_files: list[Path]                # Files matching exclude patterns
    total_files: int
```

**Default Exclude Patterns:**

```python
DEFAULT_EXCLUDES = [
    "**/.git/**",
    "**/.svn/**",
    "**/node_modules/**",
    "**/__pycache__/**",
    "**/target/**",
    "**/.cache/**",
    "**/dist/**",
    "**/build/**",
    "**/*.pyc",
    "**/*.class",
]
```

### 5.3 Spec Manager (`specs/manager.py`)

```python
class SpecManager:
    """
    Manages artifact specifications: loading, caching, validation.
    """
    
    def __init__(
        self,
        builtin_dir: Path,
        cache_dir: Path,
        llm_adapter: LLMAdapter | None = None
    ):
        """
        Args:
            builtin_dir: Directory with built-in specs
            cache_dir: Directory for cached generated specs
            llm_adapter: For generating new specs (optional)
        """
    
    def get_spec(self, spec_id: str) -> ArtifactSpec | None:
        """Get spec by ID, checking builtin then cache."""
    
    def get_spec_for_extension(self, ext: str) -> ArtifactSpec | None:
        """Get spec that handles a file extension."""
    
    def get_spec_for_file(self, path: Path) -> ArtifactSpec | None:
        """Get spec for a specific file (checks patterns too)."""
    
    async def generate_spec(
        self,
        sample_files: list[Path],
        hints: dict | None = None
    ) -> ArtifactSpec:
        """
        Generate a new spec using LLM.
        
        Args:
            sample_files: Representative files of unknown type
            hints: Optional hints about expected language/type
            
        Returns:
            Generated and validated spec
        """
    
    def validate_spec(self, spec: ArtifactSpec) -> list[str]:
        """
        Validate spec against schema and test patterns.
        
        Returns:
            List of validation errors (empty if valid)
        """
    
    def cache_spec(self, spec: ArtifactSpec) -> None:
        """Save generated spec to cache."""
    
    def list_available_specs(self) -> list[str]:
        """List all available spec IDs."""
    
    def get_spec_hash(self, spec_id: str) -> str:
        """Get content hash for reproducibility tracking."""
```

### 5.4 Spec Generator (`specs/generator.py`)

```python
class SpecGenerator:
    """
    Generates artifact specs using LLM.
    """
    
    def __init__(self, llm_adapter: LLMAdapter):
        """
        Args:
            llm_adapter: Wrapper around llm-provider
        """
    
    async def generate(
        self,
        sample_files: list[tuple[Path, str]],  # (path, content)
        hints: dict | None = None
    ) -> ArtifactSpec:
        """
        Generate a spec from sample files.
        
        Process:
        1. Build prompt with samples and few-shot examples
        2. Request structured YAML output
        3. Parse and validate result
        4. Test patterns against samples
        5. Iterate if needed
        """
    
    async def refine(
        self,
        spec: ArtifactSpec,
        failures: list[dict]
    ) -> ArtifactSpec:
        """
        Refine a spec based on extraction failures.
        
        Args:
            spec: Existing spec that had issues
            failures: Examples of what failed
        """
```

**Spec Generation Prompt Structure:**

```
SYSTEM:
You are an expert at analyzing programming languages and defining extraction patterns.
Given sample files, produce a YAML specification for extracting artifacts and references.

The spec must follow this schema:
[Schema definition]

Here are examples of good specs:
[COBOL spec example]
[Python spec example]

USER:
Analyze these sample files and generate a specification:

File 1: {filename}
```
{content}
```

File 2: {filename}
```
{content}
```

Hints: {hints if provided}

Generate a complete, valid YAML spec.
```

### 5.5 Parser Engine (`parser/engine.py`)

```python
class ParserEngine:
    """
    Deterministic pattern-based extraction engine.
    """
    
    def __init__(self):
        """Initialize parser with compiled pattern cache."""
    
    def parse_file(
        self,
        file_path: Path,
        content: str,
        spec: ArtifactSpec
    ) -> FileParseResult:
        """
        Parse a single file using its spec.
        
        Process:
        1. Preprocess (strip comments, handle continuations)
        2. Build scope tree
        3. Extract definitions
        4. Extract references
        5. Extract preprocessor directives
        """
    
    def _preprocess(
        self,
        content: str,
        spec: ArtifactSpec
    ) -> tuple[str, dict[int, int]]:
        """
        Strip comments and strings, handle line continuation.
        
        Returns:
            (cleaned_content, line_number_mapping)
        """
    
    def _extract_definitions(
        self,
        content: str,
        spec: ArtifactSpec,
        file_path: Path
    ) -> list[Artifact]:
        """Extract all artifact definitions from content."""
    
    def _extract_references(
        self,
        content: str,
        spec: ArtifactSpec,
        file_path: Path,
        defined_artifacts: list[Artifact]
    ) -> list[RawReference]:
        """Extract all references from content."""
```

### 5.6 Preprocessor (`parser/preprocessor.py`)

```python
class Preprocessor:
    """
    Handles comment stripping, string skipping, line continuation.
    """
    
    def __init__(self, spec: ArtifactSpec):
        """Initialize with language spec."""
    
    def process(self, content: str) -> PreprocessedSource:
        """
        Clean source for pattern matching.
        
        Returns:
            PreprocessedSource with cleaned content and mappings
        """
    
    def _strip_line_comments(self, content: str) -> str:
        """Remove line comments (// or # style)."""
    
    def _strip_block_comments(self, content: str) -> str:
        """Remove block comments (/* */ style)."""
    
    def _strip_fixed_column_comments(self, content: str) -> str:
        """Remove COBOL-style column-based comments."""
    
    def _mask_strings(self, content: str) -> str:
        """Replace string contents with placeholders."""
    
    def _join_continuations(self, content: str) -> str:
        """Join continued lines."""


class PreprocessedSource(BaseModel):
    """Result of preprocessing."""
    
    cleaned: str                             # Cleaned content
    original: str                            # Original content
    line_map: dict[int, int]                 # cleaned_line -> original_line
    masked_strings: list[tuple[int, int, str]]  # Positions and original strings
```

### 5.7 Artifact Registry (`resolver/registry.py`)

```python
class ArtifactRegistry:
    """
    Central registry of all discovered artifacts.
    
    Provides fast lookup by name, alias, and type.
    """
    
    def __init__(self):
        """Initialize empty registry with indices."""
    
    def register(self, artifact: Artifact) -> None:
        """
        Add artifact to registry.
        
        Updates all indices.
        """
    
    def register_alias(
        self,
        alias: str,
        canonical_id: str,
        confidence: float = 1.0
    ) -> None:
        """Register an alias mapping."""
    
    def lookup(
        self,
        name: str,
        expected_type: ArtifactType | None = None
    ) -> list[tuple[Artifact, float]]:
        """
        Find artifacts matching a name.
        
        Returns:
            List of (artifact, confidence) ordered by confidence desc
        """
    
    def lookup_exact(self, artifact_id: str) -> Artifact | None:
        """Get artifact by exact ID."""
    
    def lookup_fuzzy(
        self,
        name: str,
        expected_type: ArtifactType | None = None,
        threshold: float = 0.7
    ) -> list[tuple[Artifact, float]]:
        """
        Fuzzy match against artifact names.
        
        Uses normalized Levenshtein distance.
        """
    
    def get_by_type(self, artifact_type: ArtifactType) -> list[Artifact]:
        """Get all artifacts of a given type."""
    
    def get_all(self) -> list[Artifact]:
        """Get all registered artifacts."""
    
    def size(self) -> int:
        """Number of registered artifacts."""
```

### 5.8 Alias Resolver (`resolver/alias_resolver.py`)

```python
class AliasResolver:
    """
    Resolves references to artifacts using transformation rules.
    """
    
    def __init__(
        self,
        registry: ArtifactRegistry,
        rules: list[AliasRule]
    ):
        """
        Args:
            registry: Artifact registry to search
            rules: Alias transformation rules
        """
    
    def resolve(
        self,
        reference: RawReference
    ) -> tuple[str | None, float, str]:
        """
        Attempt to resolve a reference.
        
        Returns:
            (artifact_id, confidence, method)
            Returns (None, 0.0, reason) if unresolved
        """
    
    def _try_exact(self, name: str, expected_type: ArtifactType | None) -> str | None:
        """Try exact match."""
    
    def _try_alias(self, name: str, expected_type: ArtifactType | None) -> tuple[str, float] | None:
        """Try registered aliases."""
    
    def _try_transformations(
        self,
        name: str,
        source_type: ArtifactType | None,
        target_type: ArtifactType | None
    ) -> tuple[str, float] | None:
        """Apply transformation rules."""
    
    def _try_fuzzy(
        self,
        name: str,
        expected_type: ArtifactType | None,
        threshold: float = 0.8
    ) -> tuple[str, float] | None:
        """Try fuzzy matching."""


# Built-in transformations
TRANSFORMATIONS = {
    "uppercase": lambda s: s.upper(),
    "lowercase": lambda s: s.lower(),
    "remove_hyphens": lambda s: s.replace("-", ""),
    "remove_underscores": lambda s: s.replace("_", ""),
    "hyphens_to_underscores": lambda s: s.replace("-", "_"),
    "underscores_to_hyphens": lambda s: s.replace("_", "-"),
    "truncate_8": lambda s: s[:8],
    "truncate_6": lambda s: s[:6],
    # Add more as needed
}
```

### 5.9 Cross-Reference Resolver (`resolver/cross_reference.py`)

```python
class CrossReferenceResolver:
    """
    Builds relationships from raw references.
    """
    
    def __init__(
        self,
        registry: ArtifactRegistry,
        alias_resolver: AliasResolver,
        llm_adapter: LLMAdapter | None = None,
        max_llm_calls: int = 100
    ):
        """
        Args:
            registry: Artifact registry
            alias_resolver: For resolving aliases
            llm_adapter: For disambiguation (optional)
            max_llm_calls: Budget for LLM disambiguation
        """
    
    def resolve_all(
        self,
        raw_references: list[RawReference]
    ) -> tuple[list[Relationship], list[UnresolvedReference]]:
        """
        Resolve all references.
        
        Returns:
            (resolved_relationships, unresolved_references)
        """
    
    async def resolve_with_llm(
        self,
        unresolved: list[UnresolvedReference]
    ) -> list[Relationship]:
        """
        Use LLM to disambiguate unresolved references.
        
        Batches calls for efficiency.
        """
    
    def bind_layouts_to_tables(self) -> int:
        """
        Special pass to bind record layouts (copybooks) to tables.
        
        Uses field name matching and naming conventions.
        
        Returns:
            Number of bindings created
        """
```

### 5.10 Graph Builder (`graph/builder.py`)

```python
class GraphBuilder:
    """
    Assembles the final dependency graph.
    """
    
    def __init__(self, source_root: Path, config: Config):
        """
        Args:
            source_root: Root directory that was analyzed
            config: Configuration used
        """
    
    def add_artifacts(self, artifacts: list[Artifact]) -> None:
        """Add artifacts to graph."""
    
    def add_relationships(self, relationships: list[Relationship]) -> None:
        """Add relationships to graph."""
    
    def add_unresolved(self, unresolved: list[UnresolvedReference]) -> None:
        """Track unresolved references."""
    
    def set_specs_used(self, specs: dict[str, str]) -> None:
        """Record which specs were used (for reproducibility)."""
    
    def build(self) -> DependencyGraph:
        """
        Finalize and return the graph.
        
        Computes statistics, validates consistency.
        """
    
    def _compute_statistics(self) -> GraphStatistics:
        """Compute graph statistics."""
    
    def _validate(self) -> list[str]:
        """
        Validate graph consistency.
        
        Checks:
        - All relationship endpoints exist
        - No duplicate artifact IDs
        - Required fields populated
        """
```

### 5.11 Exporters (`graph/exporters.py`)

```python
class GraphExporter:
    """
    Exports dependency graph in various formats.
    """
    
    def export_json(
        self,
        graph: DependencyGraph,
        path: Path,
        pretty: bool = True
    ) -> None:
        """Export as JSON."""
    
    def export_dot(
        self,
        graph: DependencyGraph,
        path: Path,
        cluster_by: str = "category"  # category, language, file
    ) -> None:
        """Export as GraphViz DOT."""
    
    def export_cypher(
        self,
        graph: DependencyGraph,
        path: Path
    ) -> None:
        """Export as Cypher statements for Neo4j."""
    
    def export_csv(
        self,
        graph: DependencyGraph,
        directory: Path
    ) -> None:
        """
        Export as CSV files.
        
        Creates:
        - artifacts.csv
        - relationships.csv
        - unresolved.csv
        """
    
    def export_summary(
        self,
        graph: DependencyGraph
    ) -> str:
        """Generate human-readable summary."""
```

### 5.12 LLM Adapter (`llm/adapter.py`)

```python
class LLMAdapter:
    """
    Wrapper around llm-provider for dependency-graph specific operations.
    """
    
    def __init__(self, config: Config):
        """
        Initialize with config.
        
        Creates llm-provider client with appropriate settings.
        """
    
    async def generate_spec(
        self,
        samples: list[tuple[str, str]],      # (filename, content)
        hints: dict | None = None
    ) -> str:
        """
        Generate artifact spec from samples.
        
        Returns:
            YAML string of generated spec
        """
    
    async def disambiguate(
        self,
        reference: UnresolvedReference,
        candidates: list[Artifact]
    ) -> tuple[str | None, float]:
        """
        Use LLM to choose best candidate.
        
        Returns:
            (artifact_id, confidence) or (None, 0.0)
        """
    
    async def identify_language(
        self,
        filename: str,
        content_sample: str
    ) -> str:
        """
        Identify language/type from file content.
        
        Returns:
            Language identifier string
        """
```

### 5.13 Orchestrator (`orchestrator.py`)

```python
class Orchestrator:
    """
    Main coordinator for dependency graph extraction.
    """
    
    def __init__(self, config: Config):
        """
        Initialize orchestrator with config.
        
        Creates all component instances.
        """
    
    async def analyze(
        self,
        source_root: Path,
        output_path: Path | None = None,
        output_format: str = "json"
    ) -> DependencyGraph:
        """
        Main entry point: analyze source and produce graph.
        
        Args:
            source_root: Directory to analyze
            output_path: Where to write output (optional)
            output_format: json, dot, cypher, csv
            
        Returns:
            Complete dependency graph
        """
    
    async def analyze_incremental(
        self,
        source_root: Path,
        changed_files: list[Path],
        existing_graph: DependencyGraph
    ) -> DependencyGraph:
        """
        Incremental analysis of changed files.
        
        Reuses existing artifacts for unchanged files.
        """
    
    async def _run_discovery(self, source_root: Path) -> DiscoveryResult:
        """Phase 1: Discover files."""
    
    async def _resolve_specs(self, discovery: DiscoveryResult) -> dict[str, ArtifactSpec]:
        """Phase 2: Get specs for all file types."""
    
    async def _parse_files(
        self,
        files_by_spec: dict[str, list[Path]],
        specs: dict[str, ArtifactSpec]
    ) -> list[FileParseResult]:
        """Phase 3: Parse all files (parallel)."""
    
    def _register_artifacts(self, parse_results: list[FileParseResult]) -> ArtifactRegistry:
        """Phase 4: Build artifact registry."""
    
    async def _resolve_references(
        self,
        parse_results: list[FileParseResult],
        registry: ArtifactRegistry
    ) -> tuple[list[Relationship], list[UnresolvedReference]]:
        """Phase 5 & 6: Resolve references, disambiguate."""
    
    def _build_graph(
        self,
        registry: ArtifactRegistry,
        relationships: list[Relationship],
        unresolved: list[UnresolvedReference],
        source_root: Path,
        specs: dict[str, ArtifactSpec]
    ) -> DependencyGraph:
        """Phase 7: Assemble final graph."""
```

### 5.14 CLI (`cli.py`)

```python
"""
Command-line interface for dependency-graph.

Usage:
    depgraph analyze <source> [options]
    depgraph spec list
    depgraph spec show <spec_id>
    depgraph spec generate <samples>... [--hints <hints>]
    depgraph export <graph> --format <fmt> --output <path>
    depgraph stats <graph>

Commands:
    analyze     Analyze source directory and produce dependency graph
    spec        Manage artifact specifications
    export      Export graph to different format
    stats       Show statistics for a graph

Options:
    -o, --output <path>       Output path [default: depgraph.json]
    -f, --format <fmt>        Output format: json, dot, cypher, csv
    -p, --parallel <n>        Parallelism for file processing
    -v, --verbose             Verbose output
    --no-llm                  Disable LLM features
    --stats                   Print statistics after analysis

Examples:
    depgraph analyze ./src -o graph.json --stats
    depgraph analyze ./mainframe -p 8 -v
    depgraph spec show cobol
    depgraph export graph.json -f dot -o graph.dot
"""

import click
from pathlib import Path


@click.group()
@click.version_option()
def cli():
    """Universal Dependency Graph Builder."""
    pass


@cli.command()
@click.argument("source", type=click.Path(exists=True))
@click.option("-o", "--output", default="depgraph.json", help="Output path")
@click.option("-f", "--format", "fmt", default="json", 
              type=click.Choice(["json", "dot", "cypher", "csv"]))
@click.option("-p", "--parallel", default=4, help="Parallelism level")
@click.option("-v", "--verbose", is_flag=True)
@click.option("--no-llm", is_flag=True, help="Disable LLM features")
@click.option("--stats", is_flag=True, help="Print statistics")
def analyze(source, output, fmt, parallel, verbose, no_llm, stats):
    """Analyze source directory and produce dependency graph."""
    pass


@cli.group()
def spec():
    """Manage artifact specifications."""
    pass


@spec.command("list")
def spec_list():
    """List available specs."""
    pass


@spec.command("show")
@click.argument("spec_id")
def spec_show(spec_id):
    """Show a specific spec."""
    pass


@spec.command("generate")
@click.argument("samples", nargs=-1, type=click.Path(exists=True))
@click.option("--hints", help="Hints about expected language")
def spec_generate(samples, hints):
    """Generate spec from sample files."""
    pass


@cli.command()
@click.argument("graph", type=click.Path(exists=True))
@click.option("-f", "--format", "fmt", required=True,
              type=click.Choice(["json", "dot", "cypher", "csv"]))
@click.option("-o", "--output", required=True, help="Output path")
def export(graph, fmt, output):
    """Export graph to different format."""
    pass


@cli.command()
@click.argument("graph", type=click.Path(exists=True))
def stats(graph):
    """Show statistics for a graph."""
    pass
```

---

## 6. Built-in Specs

### 6.1 COBOL (`specs/builtin/cobol.yaml`)

```yaml
spec_version: "1.0"
spec_id: cobol
language: COBOL
description: COBOL programs with embedded SQL and CICS

file_extensions:
  - .cbl
  - .cob
  - .cobol
  - .CBL
  - .COB

file_patterns: []

category: code
primary_artifact_type: program

comments:
  fixed_column: 7
  fixed_indicator: "*"
  line_prefix: null
  block_start: null
  block_end: null

strings:
  delimiters: ["'", '"']
  escape_char: ""
  triple_quoted: false

continuation:
  trailing_char: null
  leading_char: "-"
  leading_column: 7

scope:
  start_pattern: "(PROCEDURE|DATA|WORKING-STORAGE|LINKAGE|FILE)\\s+DIVISION"
  end_pattern: "(PROCEDURE|DATA|WORKING-STORAGE|LINKAGE|FILE)\\s+DIVISION|\\Z"
  name_pattern: null

definition_patterns:
  - name: program_id
    pattern: "PROGRAM-ID\\.\\s+([A-Za-z0-9-]+)"
    capture_groups: [1]
    artifact_type: program
    
  - name: paragraph
    pattern: "^\\s{7}([A-Z0-9-]+)\\s*\\."
    capture_groups: [1]
    artifact_type: paragraph
    must_be_in_scope: PROCEDURE DIVISION
    
  - name: section
    pattern: "^\\s{7}([A-Z0-9-]+)\\s+SECTION\\."
    capture_groups: [1]
    artifact_type: paragraph

reference_patterns:
  - name: call_literal
    pattern: "CALL\\s+['\"]([A-Za-z0-9-]+)['\"]"
    capture_groups: [1]
    relationship_type: calls
    target_type_hint: program
    
  - name: call_variable
    pattern: "CALL\\s+([A-Z0-9-]+)"
    capture_groups: [1]
    relationship_type: calls
    must_not_follow: "CALL\\s+['\"]"
    
  - name: perform
    pattern: "PERFORM\\s+([A-Z0-9-]+)"
    capture_groups: [1]
    relationship_type: performs
    target_type_hint: paragraph
    
  - name: exec_sql_select
    pattern: "EXEC\\s+SQL\\s+SELECT\\s+.+?\\s+FROM\\s+([A-Za-z0-9_]+)"
    capture_groups: [1]
    relationship_type: reads
    target_type_hint: table
    ignore_case: true
    dotall: true
    
  - name: exec_sql_insert
    pattern: "EXEC\\s+SQL\\s+INSERT\\s+INTO\\s+([A-Za-z0-9_]+)"
    capture_groups: [1]
    relationship_type: writes
    target_type_hint: table
    ignore_case: true
    
  - name: exec_sql_update
    pattern: "EXEC\\s+SQL\\s+UPDATE\\s+([A-Za-z0-9_]+)"
    capture_groups: [1]
    relationship_type: updates
    target_type_hint: table
    ignore_case: true
    
  - name: exec_sql_delete
    pattern: "EXEC\\s+SQL\\s+DELETE\\s+FROM\\s+([A-Za-z0-9_]+)"
    capture_groups: [1]
    relationship_type: deletes
    target_type_hint: table
    ignore_case: true
    
  - name: cics_link
    pattern: "EXEC\\s+CICS\\s+LINK\\s+PROGRAM\\s*\\(\\s*['\"]?([A-Za-z0-9-]+)['\"]?\\s*\\)"
    capture_groups: [1]
    relationship_type: calls
    target_type_hint: program
    ignore_case: true
    
  - name: cics_xctl
    pattern: "EXEC\\s+CICS\\s+XCTL\\s+PROGRAM\\s*\\(\\s*['\"]?([A-Za-z0-9-]+)['\"]?\\s*\\)"
    capture_groups: [1]
    relationship_type: calls
    target_type_hint: program
    ignore_case: true
    
  - name: cics_read
    pattern: "EXEC\\s+CICS\\s+READ\\s+.+?FILE\\s*\\(\\s*['\"]?([A-Za-z0-9-]+)['\"]?\\s*\\)"
    capture_groups: [1]
    relationship_type: reads
    target_type_hint: file
    ignore_case: true
    dotall: true
    
  - name: cics_write
    pattern: "EXEC\\s+CICS\\s+WRITE\\s+.+?FILE\\s*\\(\\s*['\"]?([A-Za-z0-9-]+)['\"]?\\s*\\)"
    capture_groups: [1]
    relationship_type: writes
    target_type_hint: file
    ignore_case: true
    dotall: true
    
  - name: cics_send_map
    pattern: "EXEC\\s+CICS\\s+SEND\\s+MAP\\s*\\(\\s*['\"]?([A-Za-z0-9-]+)['\"]?\\s*\\)"
    capture_groups: [1]
    relationship_type: sends_to
    target_type_hint: map
    ignore_case: true
    
  - name: cics_receive_map
    pattern: "EXEC\\s+CICS\\s+RECEIVE\\s+MAP\\s*\\(\\s*['\"]?([A-Za-z0-9-]+)['\"]?\\s*\\)"
    capture_groups: [1]
    relationship_type: receives_from
    target_type_hint: map
    ignore_case: true

preprocessor_patterns:
  - name: copy
    pattern: "COPY\\s+([A-Za-z0-9-]+)"
    capture_groups: [1]
    relationship_type: includes
    target_type_hint: copybook

naming:
  case: upper
  max_length: 8
  strip_chars: ""
  replace_map: {}
  common_abbreviations:
    CUSTOMER: [CUST, CUSTMR, CST]
    ACCOUNT: [ACCT, ACC]
    TRANSACTION: [TRANS, TXN, TRAN]
    NUMBER: [NUM, NBR, NO]
    BALANCE: [BAL]
    MASTER: [MSTR, MST]
```

### 6.2 Copybook (`specs/builtin/copybook.yaml`)

```yaml
spec_version: "1.0"
spec_id: copybook
language: COBOL Copybook
description: COBOL copybook record layouts

file_extensions:
  - .cpy
  - .copy
  - .CPY
  - .COPY

file_patterns:
  - "**/copylib/**"
  - "**/COPYLIB/**"

category: data
primary_artifact_type: record_layout

comments:
  fixed_column: 7
  fixed_indicator: "*"

strings:
  delimiters: ["'", '"']
  escape_char: ""

continuation:
  leading_char: "-"
  leading_column: 7

definition_patterns:
  - name: record_01_level
    pattern: "^\\s*01\\s+([A-Z0-9-]+)\\s*\\."
    capture_groups: [1]
    artifact_type: record_layout
    
  - name: field_definition
    pattern: "^\\s*(\\d{2})\\s+([A-Z0-9-]+)\\s+PIC"
    capture_groups: [1, 2]
    join_with: ":"
    artifact_type: column

reference_patterns: []

preprocessor_patterns:
  - name: copy_nested
    pattern: "COPY\\s+([A-Za-z0-9-]+)"
    capture_groups: [1]
    relationship_type: includes
    target_type_hint: copybook

naming:
  case: upper
  strip_chars: "-"
  common_abbreviations:
    CUSTOMER: [CUST, CST]
    ACCOUNT: [ACCT, ACC]
```

### 6.3 JCL (`specs/builtin/jcl.yaml`)

```yaml
spec_version: "1.0"
spec_id: jcl
language: JCL
description: IBM Job Control Language

file_extensions:
  - .jcl
  - .JCL
  - .proc
  - .PROC

file_patterns:
  - "JCL*"
  - "PROC*"

category: code
primary_artifact_type: procedure

comments:
  line_prefix: "//*"

strings:
  delimiters: ["'"]
  escape_char: ""

continuation:
  trailing_char: null
  leading_char: null

definition_patterns:
  - name: job
    pattern: "^//([A-Z0-9@#$]+)\\s+JOB\\s"
    capture_groups: [1]
    artifact_type: procedure
    
  - name: proc
    pattern: "^//([A-Z0-9@#$]+)\\s+PROC\\s"
    capture_groups: [1]
    artifact_type: procedure
    
  - name: step
    pattern: "^//([A-Z0-9@#$]+)\\s+EXEC\\s"
    capture_groups: [1]
    artifact_type: paragraph

reference_patterns:
  - name: exec_pgm
    pattern: "EXEC\\s+PGM=([A-Z0-9@#$]+)"
    capture_groups: [1]
    relationship_type: executes
    target_type_hint: program
    
  - name: exec_proc
    pattern: "EXEC\\s+([A-Z0-9@#$]+)(?!\\s*=)"
    capture_groups: [1]
    relationship_type: calls
    target_type_hint: procedure
    must_not_follow: "PGM="
    
  - name: dd_dsn
    pattern: "DD\\s+.*DSN=([A-Z0-9@#$.]+)"
    capture_groups: [1]
    relationship_type: references
    target_type_hint: dataset
    
  - name: dd_dsn_quoted
    pattern: "DD\\s+.*DSN='([A-Z0-9@#$.()+-]+)'"
    capture_groups: [1]
    relationship_type: references
    target_type_hint: dataset

preprocessor_patterns:
  - name: include
    pattern: "//\\s+INCLUDE\\s+MEMBER=([A-Z0-9@#$]+)"
    capture_groups: [1]
    relationship_type: includes
    target_type_hint: procedure

naming:
  case: upper
  max_length: 8
```

### 6.4 DB2 DDL (`specs/builtin/db2_ddl.yaml`)

```yaml
spec_version: "1.0"
spec_id: db2_ddl
language: DB2 DDL
description: DB2 Data Definition Language

file_extensions:
  - .sql
  - .ddl
  - .SQL
  - .DDL

file_patterns:
  - "**/ddl/**"
  - "**/DDL/**"

category: data
primary_artifact_type: table

comments:
  line_prefix: "--"
  block_start: "/*"
  block_end: "*/"

strings:
  delimiters: ["'"]
  escape_char: "'"

definition_patterns:
  - name: create_table
    pattern: "CREATE\\s+TABLE\\s+(?:[A-Z0-9_]+\\.)?([A-Z0-9_]+)"
    capture_groups: [1]
    artifact_type: table
    ignore_case: true
    
  - name: create_view
    pattern: "CREATE\\s+VIEW\\s+(?:[A-Z0-9_]+\\.)?([A-Z0-9_]+)"
    capture_groups: [1]
    artifact_type: view
    ignore_case: true
    
  - name: create_index
    pattern: "CREATE\\s+(?:UNIQUE\\s+)?INDEX\\s+(?:[A-Z0-9_]+\\.)?([A-Z0-9_]+)"
    capture_groups: [1]
    artifact_type: index
    ignore_case: true
    
  - name: column_def
    pattern: "^\\s+([A-Z0-9_]+)\\s+(?:CHAR|VARCHAR|INTEGER|DECIMAL|DATE|TIMESTAMP|SMALLINT|BIGINT)"
    capture_groups: [1]
    artifact_type: column
    ignore_case: true

reference_patterns:
  - name: foreign_key
    pattern: "REFERENCES\\s+(?:[A-Z0-9_]+\\.)?([A-Z0-9_]+)"
    capture_groups: [1]
    relationship_type: references
    target_type_hint: table
    ignore_case: true
    
  - name: view_select_from
    pattern: "AS\\s+SELECT\\s+.+?\\s+FROM\\s+(?:[A-Z0-9_]+\\.)?([A-Z0-9_]+)"
    capture_groups: [1]
    relationship_type: reads
    target_type_hint: table
    ignore_case: true
    dotall: true

naming:
  case: upper
  common_abbreviations:
    CUSTOMER: [CUST, CUSTMR]
    ACCOUNT: [ACCT, ACC]
    TRANSACTION: [TRANS, TXN]
```

### 6.5 Python (`specs/builtin/python.yaml`)

```yaml
spec_version: "1.0"
spec_id: python
language: Python
description: Python source files

file_extensions:
  - .py
  - .pyw

category: code
primary_artifact_type: module

comments:
  line_prefix: "#"
  block_start: null
  block_end: null

strings:
  delimiters: ["'", '"']
  escape_char: "\\"
  triple_quoted: true

scope:
  start_pattern: "^(class|def)\\s+([A-Za-z_][A-Za-z0-9_]*)\\s*[:(]"
  end_pattern: "^(?=class\\s|def\\s|\\S)"
  name_pattern: "^(?:class|def)\\s+([A-Za-z_][A-Za-z0-9_]*)"

definition_patterns:
  - name: class_def
    pattern: "^class\\s+([A-Za-z_][A-Za-z0-9_]*)\\s*[:(]"
    capture_groups: [1]
    artifact_type: class
    
  - name: function_def
    pattern: "^def\\s+([A-Za-z_][A-Za-z0-9_]*)\\s*\\("
    capture_groups: [1]
    artifact_type: function
    
  - name: method_def
    pattern: "^\\s+def\\s+([A-Za-z_][A-Za-z0-9_]*)\\s*\\(self"
    capture_groups: [1]
    artifact_type: method

reference_patterns:
  - name: import_module
    pattern: "^import\\s+([A-Za-z_][A-Za-z0-9_.]*)"
    capture_groups: [1]
    relationship_type: imports
    target_type_hint: module
    
  - name: from_import
    pattern: "^from\\s+([A-Za-z_][A-Za-z0-9_.]*)\\s+import"
    capture_groups: [1]
    relationship_type: imports
    target_type_hint: module
    
  - name: function_call
    pattern: "([A-Za-z_][A-Za-z0-9_]*)\\s*\\("
    capture_groups: [1]
    relationship_type: calls
    target_type_hint: function
    
  - name: sqlalchemy_table
    pattern: "Table\\s*\\(\\s*['\"]([A-Za-z0-9_]+)['\"]"
    capture_groups: [1]
    relationship_type: references
    target_type_hint: table
    
  - name: execute_sql
    pattern: "\\.execute\\s*\\(\\s*['\"]\\s*(?:SELECT|INSERT|UPDATE|DELETE)\\s+.+?(?:FROM|INTO|UPDATE)\\s+([A-Za-z0-9_]+)"
    capture_groups: [1]
    relationship_type: references
    target_type_hint: table
    ignore_case: true

naming:
  case: preserve
```

---

## 7. Alias Resolution Rules

Built-in rules for cross-artifact-type resolution:

```yaml
# Loaded by AliasResolver

alias_rules:
  # COBOL SQL to DDL tables
  - source_type: null  # Any source
    target_type: table
    transformations:
      - uppercase
      - remove_hyphens
      - truncate_8
    confidence_base: 0.85
    
  # Copybook to table
  - source_type: record_layout
    target_type: table
    transformations:
      - uppercase
      - remove_hyphens
      - remove_suffixes  # -REC, -RECORD, etc.
    confidence_base: 0.75
    
  # JCL dataset to file
  - source_type: dataset
    target_type: file
    transformations:
      - extract_last_qualifier  # PROD.DATA.CUSTMAST -> CUSTMAST
      - uppercase
    confidence_base: 0.8
    
  # CICS file to VSAM
  - source_type: null
    target_type: file
    transformations:
      - uppercase
      - truncate_8
    confidence_base: 0.8
```

---

## 8. Integration Points

### 8.1 War Rig Integration

The dependency graph serves as ground truth for War Rig documentation:

```python
# Example: War Rig validation using dependency graph

from depgraph import DependencyGraph, RelationshipType

def validate_program_documentation(
    program_name: str,
    documentation: dict,
    graph: DependencyGraph
) -> list[str]:
    """
    Validate that documentation covers all dependencies.
    
    Returns list of validation errors.
    """
    errors = []
    program_id = f"program::{program_name}"
    
    # Get all data relationships for this program
    data_rels = [
        r for r in graph.relationships
        if r.from_artifact == program_id
        and r.relationship_type in [
            RelationshipType.READS,
            RelationshipType.WRITES,
            RelationshipType.UPDATES,
            RelationshipType.DELETES
        ]
    ]
    
    # Check each table is documented
    for rel in data_rels:
        table = graph.artifacts.get(rel.to_artifact)
        if table and table.canonical_name not in documentation.get("data_dependencies", []):
            errors.append(
                f"Missing data dependency: {table.canonical_name} "
                f"({rel.relationship_type.value} at {rel.location})"
            )
    
    # Get all program calls
    call_rels = [
        r for r in graph.relationships
        if r.from_artifact == program_id
        and r.relationship_type == RelationshipType.CALLS
    ]
    
    # Check each called program is documented
    for rel in call_rels:
        called = graph.artifacts.get(rel.to_artifact)
        if called and called.canonical_name not in documentation.get("called_programs", []):
            errors.append(
                f"Missing called program: {called.canonical_name} "
                f"(at {rel.location})"
            )
    
    return errors
```

### 8.2 LLM Provider Integration

```python
# Example: Using shared llm-provider

from llm_provider import create_provider, Message

class LLMAdapter:
    def __init__(self, config: Config):
        self.provider = create_provider(
            provider_name=config.llm_provider,
            model=config.depgraph_llm_model
        )
    
    async def generate_spec(self, samples: list[tuple[str, str]]) -> str:
        messages = [
            Message(
                role="user",
                content=self._build_spec_prompt(samples)
            )
        ]
        
        response = await self.provider.complete(
            messages=messages,
            system=SPEC_GENERATION_SYSTEM_PROMPT,
            max_tokens=4096
        )
        
        return response.content
```

---

## 9. Error Handling

### 9.1 Error Categories

```python
class DepgraphError(Exception):
    """Base exception for dependency-graph."""
    pass

class SpecError(DepgraphError):
    """Error in artifact specification."""
    pass

class ParseError(DepgraphError):
    """Error parsing source file."""
    pass

class ResolutionError(DepgraphError):
    """Error resolving reference."""
    pass

class ConfigError(DepgraphError):
    """Configuration error."""
    pass
```

### 9.2 Error Handling Strategy

| Situation | Behavior |
|-----------|----------|
| Invalid spec YAML | Raise SpecError, halt |
| File read error | Log warning, skip file, continue |
| Pattern match error | Log warning, skip pattern, continue |
| LLM call failure | Retry 3x with backoff, then skip |
| Encoding error | Try fallback encodings, then skip |
| Circular includes | Detect, log warning, break cycle |
| Unknown file type | Queue for spec generation or skip |

---

## 10. Performance Considerations

### 10.1 Parallelism

- File parsing: Parallel by file (configurable workers)
- LLM calls: Batched and rate-limited
- Resolution: Single-threaded (registry is shared state)

### 10.2 Caching

| What | Where | Invalidation |
|------|-------|--------------|
| Generated specs | `cache/specs/` | Manual or on failure |
| Parse results | `cache/parse_results/` | File content hash change |
| LLM responses | In-memory per run | End of run |

### 10.3 Memory Management

- Stream large files line-by-line for preprocessing
- Don't hold all file contents in memory simultaneously
- Use generators where possible for artifact iteration

### 10.4 Target Performance

- 100+ files/second for deterministic parsing
- < 5% of runtime in LLM calls for typical codebases
- Handle 10,000+ file repositories
- Incremental re-analysis in seconds for small changes

---

## 11. Testing Strategy

### 11.1 Unit Tests

- Spec schema validation
- Individual pattern extraction
- Preprocessor (comment stripping, continuation handling)
- Alias transformations
- Registry lookup operations

### 11.2 Integration Tests

- End-to-end on sample repositories
- Multi-language repository
- Incremental analysis
- Error recovery

### 11.3 Test Fixtures

```
tests/fixtures/samples/
├── cobol/
│   ├── CUSTINQ.cbl      # Program with SQL and CICS
│   ├── ACCTUPD.cbl      # Program with CALL statements
│   └── expected.json     # Expected extraction results
├── jcl/
│   ├── DAILYJOB.jcl     # Job with EXEC PGM and PROC calls
│   └── expected.json
├── ddl/
│   ├── customer.sql     # Table definitions
│   └── expected.json
├── copybook/
│   ├── CUSTREC.cpy      # Customer record layout
│   └── expected.json
├── python/
│   ├── main.py          # Python with imports and calls
│   └── expected.json
└── mixed/
    ├── ...              # Full mini-application
    └── expected_graph.json
```

---

## 12. Deliverables

### 12.1 Milestone 1: Foundation
- [ ] Project structure with pyproject.toml
- [ ] Configuration loading from .env
- [ ] Data models (Pydantic schemas)
- [ ] File discovery component
- [ ] Spec manager with built-in loading
- [ ] Basic CLI skeleton

### 12.2 Milestone 2: Parser Engine
- [ ] Preprocessor (comments, strings, continuations)
- [ ] Pattern-based extraction engine
- [ ] Scope tracking
- [ ] COBOL and Python specs working
- [ ] Unit tests for parser

### 12.3 Milestone 3: Resolution
- [ ] Artifact registry with indices
- [ ] Alias resolver with transformations
- [ ] Cross-reference resolver
- [ ] Graph builder
- [ ] JSON exporter

### 12.4 Milestone 4: LLM Integration
- [ ] LLM adapter using llm-provider
- [ ] Spec generator
- [ ] Disambiguation for ambiguous references
- [ ] Spec refinement on failures

### 12.5 Milestone 5: Mainframe Complete
- [ ] JCL spec and testing
- [ ] DB2 DDL spec and testing
- [ ] CICS/BMS spec and testing
- [ ] Copybook-to-table binding
- [ ] Full mainframe integration test

### 12.6 Milestone 6: Production Ready
- [ ] All exporters (DOT, Cypher, CSV)
- [ ] Incremental analysis
- [ ] Performance optimization
- [ ] Comprehensive test coverage
- [ ] Documentation
- [ ] War Rig integration helpers

---

## 13. Usage Examples

### 13.1 Basic Analysis

```bash
# Analyze a mainframe codebase
depgraph analyze ./mainframe-app -o graph.json --stats

# Output:
# Discovered 1,247 files
# Using specs: cobol, copybook, jcl, db2_ddl
# Parsing... ████████████████████ 100%
# Resolving references...
# 
# Statistics:
#   Artifacts: 523
#     Programs: 156
#     Tables: 89
#     Copybooks: 134
#     ...
#   Relationships: 2,341
#     calls: 412
#     reads: 678
#     ...
#   Resolution rate: 94.2%
#   Unresolved: 137
#
# Output written to graph.json
```

### 13.2 Working with Specs

```bash
# List available specs
depgraph spec list

# Show a spec
depgraph spec show cobol

# Generate spec for unknown files
depgraph spec generate ./samples/weird*.src --hints "looks like PL/I"
```

### 13.3 Export to Neo4j

```bash
# Export for Neo4j import
depgraph export graph.json -f cypher -o import.cypher

# Then in Neo4j:
# :source import.cypher
```

### 13.4 Programmatic Usage

```python
from depgraph import Orchestrator, Config
from pathlib import Path
import asyncio

async def analyze_codebase():
    config = Config()
    orchestrator = Orchestrator(config)
    
    graph = await orchestrator.analyze(
        source_root=Path("./mainframe-app"),
        output_path=Path("graph.json")
    )
    
    # Find all programs that write to CUSTOMER table
    customer_writers = [
        r.from_artifact
        for r in graph.relationships
        if r.to_artifact == "table::CUSTOMER"
        and r.relationship_type.value in ["writes", "updates", "deletes"]
    ]
    
    print(f"Programs modifying CUSTOMER: {customer_writers}")

asyncio.run(analyze_codebase())
```

---

## 14. Notes for Implementation

1. **Use `uv`** for dependency management (consistent with monorepo)

2. **Follow existing patterns** from `llm-provider` and `war-rig` for:
   - Project structure
   - Logging approach
   - Error handling conventions
   - Test organization

3. **Pydantic everywhere** for data validation and serialization

4. **Async by default** for I/O operations and LLM calls

5. **Rich CLI output** using the `rich` library for progress bars and tables

6. **Extensive DEBUG logging** for troubleshooting pattern issues

7. **Human-editable specs** - users will customize; keep YAML clean and documented

8. **Cache aggressively** - LLM calls are expensive; specs should be generated once

9. **Fail gracefully** - one bad file shouldn't stop the whole analysis

10. **Reproducibility** - hash configs and specs so results can be reproduced
