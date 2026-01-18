# Atlas Chunking Embedding Architecture

**Ticket:** war_rig-i70v
**Author:** War Rig Architecture Team
**Date:** 2026-01-18
**Status:** PROPOSED

## Executive Summary

This document describes the architecture for embedding Atlas chunking functionality directly into War Rig. The goal is to eliminate the external dependency on the `atlas` package while preserving all chunking capabilities needed for processing large COBOL files that exceed context budgets.

---

## 1. Current State Analysis

### 1.1 Current Integration Points

War Rig currently integrates with Atlas through the following touchpoints:

| Component | Location | Atlas Dependency |
|-----------|----------|------------------|
| TicketOrchestrator | `war_rig/orchestration/ticket_engine.py` | `atlas.splitter.get_splitter()`, `atlas.models.manifest.SplitterProfile` |
| ScribeWorkerAdapter | `war_rig/adapters/scribe_worker_adapter.py` | `atlas.adapters.*`, `atlas.models.work_item.*`, `atlas.workers.base.Worker` |
| FileArtifactAdapter | `war_rig/adapters/file_artifact_adapter.py` | `atlas.adapters.artifact_store.ArtifactStoreAdapter` |
| BeadsTicketAdapter | `war_rig/adapters/beads_ticket_adapter.py` | `atlas.adapters.ticket_system.TicketSystemAdapter` |

### 1.2 Atlas Components Used

From the Atlas package, War Rig uses:

**Splitter Module:**
- `Splitter` - Abstract base class
- `SplitResult` - Result dataclass
- `COBOLSplitter` - COBOL-aware semantic splitter
- `LineBasedSplitter` - Fallback line-based splitter
- `SplitterRegistry` - Plugin architecture for splitter lookup
- `get_splitter()` - Convenience function

**Models Module:**
- `SplitterProfile` - Chunking configuration
- `ChunkSpec` - Chunk specification
- `ChunkKind` - Chunk type enumeration
- `ChunkLocator` - Locator for chunk within artifact
- `DocChunkPayload` - Work item payload
- `WorkItem` - Work item structure
- `WorkItemStatus` - Status enumeration
- `WorkItemType` - Type enumeration
- `ArtifactRef` - Artifact reference

### 1.3 Problems with Current Approach

1. **External Dependency:** Requires Atlas package to be installed and maintained separately
2. **Version Coupling:** Atlas updates may break War Rig integration
3. **Deployment Complexity:** Two packages to deploy and configure
4. **Circular Dependency Risk:** War Rig adapters depend on Atlas abstractions

---

## 2. Proposed Architecture

### 2.1 Module Structure

```
war_rig/
    chunking/                          # NEW: Embedded chunking package
        __init__.py                    # Public API exports
        enums.py                       # ChunkKind, WorkItemType, WorkItemStatus
        models/
            __init__.py
            chunk.py                   # ChunkSpec, ChunkLocator, SplitResult
            profile.py                 # SplitterProfile
            artifact.py                # ArtifactRef
            work_item.py               # WorkItem, DocChunkPayload, etc.
        splitters/
            __init__.py
            base.py                    # Splitter ABC
            cobol.py                   # COBOLSplitter
            line_based.py              # LineBasedSplitter
            registry.py                # SplitterRegistry, get_splitter()
```

### 2.2 Package Diagram

```
+--------------------------------------------------+
|                    war_rig                        |
+--------------------------------------------------+
|                                                  |
|  +--------------------+    +------------------+  |
|  |   orchestration/   |    |    adapters/     |  |
|  |   ticket_engine.py |--->| scribe_worker    |  |
|  +--------------------+    | file_artifact    |  |
|          |                 | beads_ticket     |  |
|          v                 | analysis_router  |  |
|  +--------------------+    +------------------+  |
|  |     chunking/      |<----------+              |
|  +--------------------+           |              |
|  | enums.py           |           |              |
|  | models/            |           |              |
|  |   chunk.py         |           |              |
|  |   profile.py       |           |              |
|  |   work_item.py     |           |              |
|  | splitters/         |           |              |
|  |   base.py          |           |              |
|  |   cobol.py         |           |              |
|  |   line_based.py    |           |              |
|  |   registry.py      |           |              |
|  +--------------------+           |              |
|          |                        |              |
|          v                        |              |
|  +--------------------+    +------+----------+   |
|  |     models/        |    |    workers/     |   |
|  |   templates.py     |    | scribe_pool.py  |   |
|  |   assessments.py   |    | challenger_pool |   |
|  +--------------------+    +-----------------+   |
|                                                  |
+--------------------------------------------------+
```

### 2.3 Key Interfaces

#### 2.3.1 Enumerations (`war_rig/chunking/enums.py`)

```python
from enum import Enum

class ChunkKind(str, Enum):
    """Classification of chunk content types."""
    IDENTIFICATION_DIVISION = "identification_division"
    ENVIRONMENT_DIVISION = "environment_division"
    DATA_DIVISION = "data_division"
    PROCEDURE_DIVISION = "procedure_division"
    PROCEDURE_PART = "procedure_part"
    WORKING_STORAGE = "working_storage"
    FILE_SECTION = "file_section"
    LINKAGE_SECTION = "linkage_section"
    GENERIC = "generic"
    MIXED = "mixed"


class WorkItemStatus(str, Enum):
    """Canonical status model for work items."""
    NEW = "new"
    READY = "ready"
    IN_PROGRESS = "in_progress"
    BLOCKED = "blocked"
    DONE = "done"
    FAILED = "failed"
    CANCELED = "canceled"


class WorkItemType(str, Enum):
    """Canonical work types for chunked processing."""
    DOC_CHUNK = "doc_chunk"
    DOC_MERGE = "doc_merge"
    DOC_CHALLENGE = "doc_challenge"
    DOC_FOLLOWUP = "doc_followup"
```

#### 2.3.2 Chunk Models (`war_rig/chunking/models/chunk.py`)

```python
from dataclasses import dataclass, field
from pydantic import BaseModel, Field
from war_rig.chunking.enums import ChunkKind

@dataclass
class SplitResult:
    """Result of splitting a source artifact."""
    chunks: list["ChunkSpec"]
    total_lines: int = 0
    total_estimated_tokens: int = 0
    semantic_boundaries_found: int = 0
    warnings: list[str] = field(default_factory=list)


class ChunkSpec(BaseModel):
    """Specification for a single chunk."""
    chunk_id: str
    chunk_kind: ChunkKind = ChunkKind.GENERIC
    start_line: int
    end_line: int
    division: str | None = None
    section: str | None = None
    paragraphs: list[str] = Field(default_factory=list)
    estimated_tokens: int = 0
    result_uri: str | None = None
    metadata: dict[str, Any] = Field(default_factory=dict)


class ChunkLocator(BaseModel):
    """Locator for a chunk within an artifact."""
    start_line: int | None = None
    end_line: int | None = None
    division: str | None = None
    section: str | None = None
    paragraphs: list[str] = Field(default_factory=list)
```

#### 2.3.3 Splitter Profile (`war_rig/chunking/models/profile.py`)

```python
from pydantic import BaseModel, Field
from war_rig.chunking.enums import ChunkKind

class SplitterProfile(BaseModel):
    """Configuration for the chunking strategy."""
    name: str = "default"
    prefer_semantic: bool = True
    max_chunk_tokens: int = 3500
    overlap_lines: int = 10
    chunk_kinds: list[ChunkKind] = Field(default_factory=list)
    custom_config: dict[str, Any] = Field(default_factory=dict)
```

#### 2.3.4 Work Item Models (`war_rig/chunking/models/work_item.py`)

```python
from pydantic import BaseModel, Field
from war_rig.chunking.enums import WorkItemStatus, WorkItemType
from war_rig.chunking.models.artifact import ArtifactRef
from war_rig.chunking.models.chunk import ChunkLocator

class WorkItemPayload(BaseModel):
    """Base payload for work items."""
    job_id: str
    artifact_ref: ArtifactRef | None = None
    manifest_uri: str | None = None


class DocChunkPayload(WorkItemPayload):
    """Payload for DOC_CHUNK work items."""
    chunk_id: str
    chunk_locator: ChunkLocator
    result_uri: str


class WorkItem(BaseModel):
    """A unit of work for chunked processing."""
    work_id: str
    work_type: WorkItemType
    status: WorkItemStatus = WorkItemStatus.NEW
    payload: WorkItemPayload
    parent_work_id: str | None = None
    depends_on: list[str] = Field(default_factory=list)
    cycle_number: int = 1
    idempotency_key: str | None = None
    created_at: str | None = None
    updated_at: str | None = None
    metadata: dict[str, Any] = Field(default_factory=dict)
```

#### 2.3.5 Splitter Interface (`war_rig/chunking/splitters/base.py`)

```python
from abc import ABC, abstractmethod
from war_rig.chunking.models.chunk import SplitResult, ChunkSpec
from war_rig.chunking.models.profile import SplitterProfile
from war_rig.chunking.enums import ChunkKind


class Splitter(ABC):
    """Abstract interface for source code splitting."""

    @classmethod
    @abstractmethod
    def get_artifact_types(cls) -> list[str]:
        """Return artifact types this splitter handles."""
        pass

    @abstractmethod
    def split(
        self,
        source: str,
        profile: SplitterProfile,
        artifact_id: str,
    ) -> SplitResult:
        """Split source code into chunks."""
        pass

    @abstractmethod
    def estimate_tokens(self, text: str) -> int:
        """Estimate token count for text."""
        pass

    @abstractmethod
    def detect_semantic_boundaries(
        self,
        source: str,
    ) -> list[tuple[int, str, ChunkKind]]:
        """Detect semantic boundaries in source code."""
        pass

    def generate_chunk_id(
        self,
        artifact_id: str,
        chunk_kind: ChunkKind,
        index: int,
        name: str | None = None,
    ) -> str:
        """Generate a stable chunk ID."""
        base = artifact_id.replace(".", "_").lower()
        kind = chunk_kind.value
        if name:
            safe_name = name.replace("-", "_").replace(" ", "_").lower()
            return f"{base}_{kind}_{safe_name}"
        return f"{base}_{kind}_{index:03d}"

    def validate_chunk(
        self,
        chunk: ChunkSpec,
        profile: SplitterProfile,
    ) -> list[str]:
        """Validate chunk against profile constraints."""
        errors: list[str] = []
        if chunk.estimated_tokens > profile.max_chunk_tokens:
            errors.append(
                f"Chunk {chunk.chunk_id} exceeds token limit: "
                f"{chunk.estimated_tokens} > {profile.max_chunk_tokens}"
            )
        if chunk.start_line > chunk.end_line:
            errors.append(
                f"Chunk {chunk.chunk_id} has invalid line range"
            )
        return errors
```

#### 2.3.6 COBOL Splitter (`war_rig/chunking/splitters/cobol.py`)

```python
import re
from dataclasses import dataclass, field
from typing import NamedTuple
from war_rig.chunking.splitters.base import Splitter, SplitResult
from war_rig.chunking.models.chunk import ChunkSpec
from war_rig.chunking.models.profile import SplitterProfile
from war_rig.chunking.enums import ChunkKind


class SemanticBoundary(NamedTuple):
    """A semantic boundary in COBOL source code."""
    line_number: int
    name: str
    kind: ChunkKind
    level: int


@dataclass
class COBOLStructure:
    """Parsed COBOL program structure."""
    boundaries: list[SemanticBoundary] = field(default_factory=list)
    divisions: dict[str, SemanticBoundary] = field(default_factory=dict)
    sections: dict[str, SemanticBoundary] = field(default_factory=dict)
    paragraphs: dict[str, SemanticBoundary] = field(default_factory=dict)
    total_lines: int = 0


class COBOLSplitter(Splitter):
    """COBOL-aware source code splitter.

    Parses COBOL structure and creates chunks at semantic boundaries
    (divisions, sections, paragraphs) while respecting token budgets.
    """

    DIVISION_PATTERN = re.compile(
        r"^\s*(IDENTIFICATION|ENVIRONMENT|DATA|PROCEDURE)\s+DIVISION\s*\.?\s*$",
        re.IGNORECASE | re.MULTILINE,
    )
    SECTION_PATTERN = re.compile(
        r"^\s*([A-Z0-9][A-Z0-9\-]*)\s+SECTION\s*\.?\s*$",
        re.IGNORECASE | re.MULTILINE,
    )
    PARAGRAPH_PATTERN = re.compile(
        r"^\s*([A-Z][A-Z0-9\-]*)\s*\.\s*$",
        re.IGNORECASE | re.MULTILINE,
    )

    DIVISION_TO_KIND: dict[str, ChunkKind] = {
        "IDENTIFICATION": ChunkKind.IDENTIFICATION_DIVISION,
        "ENVIRONMENT": ChunkKind.ENVIRONMENT_DIVISION,
        "DATA": ChunkKind.DATA_DIVISION,
        "PROCEDURE": ChunkKind.PROCEDURE_DIVISION,
    }

    SECTION_TO_KIND: dict[str, ChunkKind] = {
        "FILE": ChunkKind.FILE_SECTION,
        "WORKING-STORAGE": ChunkKind.WORKING_STORAGE,
        "LINKAGE": ChunkKind.LINKAGE_SECTION,
    }

    CHARS_PER_TOKEN = 4

    @classmethod
    def get_artifact_types(cls) -> list[str]:
        return ["cobol", "copybook"]

    def split(
        self,
        source: str,
        profile: SplitterProfile,
        artifact_id: str,
    ) -> SplitResult:
        # Implementation ported from Atlas
        ...

    def estimate_tokens(self, text: str) -> int:
        return max(1, len(text) // self.CHARS_PER_TOKEN)

    def detect_semantic_boundaries(
        self,
        source: str,
    ) -> list[tuple[int, str, ChunkKind]]:
        # Implementation ported from Atlas
        ...
```

#### 2.3.7 Line-Based Splitter (`war_rig/chunking/splitters/line_based.py`)

```python
from war_rig.chunking.splitters.base import Splitter, SplitResult
from war_rig.chunking.models.chunk import ChunkSpec
from war_rig.chunking.models.profile import SplitterProfile
from war_rig.chunking.enums import ChunkKind


class LineBasedSplitter(Splitter):
    """Simple line-count-based splitter for generic artifacts.

    This splitter divides source content into chunks based on line count,
    without any semantic awareness. Serves as fallback for unknown types.
    """

    CHARS_PER_TOKEN: int = 4
    DEFAULT_CHARS_PER_LINE: int = 40

    @classmethod
    def get_artifact_types(cls) -> list[str]:
        return []  # Fallback splitter

    def split(
        self,
        source: str,
        profile: SplitterProfile,
        artifact_id: str,
    ) -> SplitResult:
        # Implementation ported from Atlas
        ...

    def estimate_tokens(self, text: str) -> int:
        return max(1, len(text) // self.CHARS_PER_TOKEN)

    def detect_semantic_boundaries(
        self,
        source: str,
    ) -> list[tuple[int, str, ChunkKind]]:
        return []  # No semantic awareness
```

#### 2.3.8 Splitter Registry (`war_rig/chunking/splitters/registry.py`)

```python
import logging
from typing import Type, TypeVar
from war_rig.chunking.splitters.base import Splitter

logger = logging.getLogger(__name__)

T = TypeVar("T", bound=Splitter)


class SplitterNotFoundError(Exception):
    """Raised when no splitter is found for an artifact type."""
    pass


class SplitterRegistry:
    """Registry for mapping artifact types to splitter implementations."""

    def __init__(self, fallback_class: Type[Splitter] | None = None) -> None:
        self._splitter_classes: dict[str, Type[Splitter]] = {}
        self._splitter_instances: dict[str, Splitter] = {}
        self._configs: dict[str, dict] = {}

        from war_rig.chunking.splitters.line_based import LineBasedSplitter
        self._fallback_class: Type[Splitter] = fallback_class or LineBasedSplitter

    def register(
        self,
        splitter_class: Type[T],
        artifact_types: list[str] | None = None,
    ) -> None:
        """Register a splitter class for its declared artifact types."""
        types_to_register = artifact_types or splitter_class.get_artifact_types()
        for artifact_type in types_to_register:
            normalized_type = artifact_type.lower().strip()
            self._splitter_classes[normalized_type] = splitter_class
            self._splitter_instances.pop(normalized_type, None)

    def get_splitter(
        self,
        artifact_type: str,
        use_fallback: bool = True,
    ) -> Splitter:
        """Get a splitter instance for the given artifact type."""
        normalized_type = artifact_type.lower().strip()

        if normalized_type in self._splitter_instances:
            return self._splitter_instances[normalized_type]

        splitter_class = self._splitter_classes.get(normalized_type)

        if splitter_class is None:
            if use_fallback:
                splitter_class = self._fallback_class
            else:
                raise SplitterNotFoundError(
                    f"No splitter registered for artifact type '{artifact_type}'"
                )

        splitter = splitter_class()
        self._splitter_instances[normalized_type] = splitter
        return splitter


# Global default registry
_default_registry: SplitterRegistry | None = None


def get_default_registry() -> SplitterRegistry:
    """Get the default global splitter registry."""
    global _default_registry
    if _default_registry is None:
        _default_registry = SplitterRegistry()
        _register_builtin_splitters(_default_registry)
    return _default_registry


def _register_builtin_splitters(registry: SplitterRegistry) -> None:
    """Register all built-in splitters with the registry."""
    from war_rig.chunking.splitters.cobol import COBOLSplitter
    registry.register(COBOLSplitter)


def get_splitter(artifact_type: str) -> Splitter:
    """Get a splitter for the given artifact type."""
    return get_default_registry().get_splitter(artifact_type)
```

#### 2.3.9 Package Public API (`war_rig/chunking/__init__.py`)

```python
"""War Rig embedded chunking for large file processing.

This package provides chunking functionality for processing large source
files that exceed context budgets. It includes:

- COBOL-aware semantic splitter (COBOLSplitter)
- Line-based fallback splitter (LineBasedSplitter)
- Splitter registry with plugin architecture
- Work item models for chunked processing

Usage:
    from war_rig.chunking import get_splitter, SplitterProfile

    splitter = get_splitter("cobol")
    profile = SplitterProfile(max_chunk_tokens=3500)
    result = splitter.split(source, profile, "PROGRAM.cbl")
"""

from war_rig.chunking.enums import ChunkKind, WorkItemStatus, WorkItemType
from war_rig.chunking.models.artifact import ArtifactRef
from war_rig.chunking.models.chunk import ChunkLocator, ChunkSpec, SplitResult
from war_rig.chunking.models.profile import SplitterProfile
from war_rig.chunking.models.work_item import (
    DocChunkPayload,
    WorkItem,
    WorkItemPayload,
)
from war_rig.chunking.splitters.base import Splitter
from war_rig.chunking.splitters.cobol import COBOLSplitter, COBOLStructure, SemanticBoundary
from war_rig.chunking.splitters.line_based import LineBasedSplitter
from war_rig.chunking.splitters.registry import (
    SplitterNotFoundError,
    SplitterRegistry,
    get_default_registry,
    get_splitter,
)

__all__ = [
    # Enums
    "ChunkKind",
    "WorkItemStatus",
    "WorkItemType",
    # Models
    "ArtifactRef",
    "ChunkLocator",
    "ChunkSpec",
    "DocChunkPayload",
    "SplitResult",
    "SplitterProfile",
    "WorkItem",
    "WorkItemPayload",
    # Splitters
    "COBOLSplitter",
    "COBOLStructure",
    "LineBasedSplitter",
    "SemanticBoundary",
    "Splitter",
    # Registry
    "SplitterNotFoundError",
    "SplitterRegistry",
    "get_default_registry",
    "get_splitter",
]
```

---

## 3. Data Flow Diagrams

### 3.1 Chunked File Processing Flow

```
                     +-------------------+
                     |  TicketOrchestrator|
                     +-------------------+
                              |
                              | route_files_for_processing()
                              v
                     +-------------------+
                     |  AnalysisRouter   |
                     +-------------------+
                              |
              +---------------+---------------+
              |                               |
              v                               v
    +------------------+            +------------------+
    | DIRECT (< budget)|            | CHUNKED (> budget)|
    +------------------+            +------------------+
              |                               |
              v                               v
    +------------------+            +------------------+
    | Normal Pipeline  |            | _process_chunked |
    | (ScribeWorkerPool)|           | _files()         |
    +------------------+            +------------------+
                                              |
                                              v
                                    +------------------+
                                    | get_splitter()   |
                                    | (from chunking)  |
                                    +------------------+
                                              |
                                              v
                                    +------------------+
                                    | COBOLSplitter or |
                                    | LineBasedSplitter|
                                    +------------------+
                                              |
                                              | split() -> SplitResult
                                              v
                                    +------------------+
                                    | Create WorkItems |
                                    | (DocChunkPayload)|
                                    +------------------+
                                              |
                                              v
                                    +------------------+
                                    | ScribeWorker     |
                                    | process(WorkItem)|
                                    +------------------+
                                              |
                                              v
                                    +------------------+
                                    | Merge Results    |
                                    | -> DocumentationTemplate
                                    +------------------+
```

### 3.2 Splitter Resolution Flow

```
                    +-------------------+
                    | get_splitter(type)|
                    +-------------------+
                             |
                             v
                    +-------------------+
                    | SplitterRegistry  |
                    +-------------------+
                             |
              +--------------+--------------+
              |              |              |
              v              v              v
        +---------+    +---------+    +---------+
        | "cobol" |    |"copybook"|    | other   |
        +---------+    +---------+    +---------+
              |              |              |
              v              v              v
        +---------+    +---------+    +---------+
        | COBOL   |    | COBOL   |    | LineBased|
        | Splitter|    | Splitter|    | Splitter |
        +---------+    +---------+    +---------+
                                    (fallback)
```

### 3.3 COBOL Semantic Splitting Flow

```
                    +-------------------+
                    | COBOL Source      |
                    +-------------------+
                             |
                             v
                    +-------------------+
                    | _parse_structure()|
                    +-------------------+
                             |
                             v
                    +-------------------+
                    | COBOLStructure    |
                    | - divisions       |
                    | - sections        |
                    | - paragraphs      |
                    +-------------------+
                             |
              +--------------+--------------+
              |                             |
              v                             v
    +------------------+           +------------------+
    | prefer_semantic  |           | !prefer_semantic |
    | = True           |           | or no boundaries |
    +------------------+           +------------------+
              |                             |
              v                             v
    +------------------+           +------------------+
    | _split_semantic()|           | _split_by_lines()|
    +------------------+           +------------------+
              |                             |
              |    +-------------------+    |
              +--->| Check token budget|<---+
                   +-------------------+
                             |
              +--------------+--------------+
              |                             |
              v                             v
    +------------------+           +------------------+
    | <= max_chunk_tokens|         | > max_chunk_tokens|
    | Create single chunk|         | _split_oversized_|
    +------------------+           | chunk()          |
                                   +------------------+
                                            |
                                            v
                                   +------------------+
                                   | Sub-chunks at    |
                                   | paragraph bounds |
                                   +------------------+
```

---

## 4. Migration Steps

### Phase 1: Create Chunking Package Structure (Week 1)

1. **Create directory structure:**
   ```bash
   mkdir -p war_rig/chunking/models
   mkdir -p war_rig/chunking/splitters
   touch war_rig/chunking/__init__.py
   touch war_rig/chunking/enums.py
   touch war_rig/chunking/models/__init__.py
   touch war_rig/chunking/models/chunk.py
   touch war_rig/chunking/models/profile.py
   touch war_rig/chunking/models/artifact.py
   touch war_rig/chunking/models/work_item.py
   touch war_rig/chunking/splitters/__init__.py
   touch war_rig/chunking/splitters/base.py
   touch war_rig/chunking/splitters/cobol.py
   touch war_rig/chunking/splitters/line_based.py
   touch war_rig/chunking/splitters/registry.py
   ```

2. **Port enumerations** from `atlas/models/enums.py` to `war_rig/chunking/enums.py`

3. **Port models** with minimal modifications:
   - `ChunkSpec`, `ChunkLocator` -> `war_rig/chunking/models/chunk.py`
   - `SplitterProfile` -> `war_rig/chunking/models/profile.py`
   - `ArtifactRef` -> `war_rig/chunking/models/artifact.py`
   - `WorkItem`, payloads -> `war_rig/chunking/models/work_item.py`

### Phase 2: Port Splitters (Week 2)

1. **Port base splitter interface** from `atlas/splitter/base.py`:
   - Copy `Splitter` ABC, `SplitResult` dataclass
   - Update imports to use `war_rig.chunking.models`

2. **Port COBOL splitter** from `atlas/splitter/cobol.py`:
   - Copy `COBOLSplitter`, `COBOLStructure`, `SemanticBoundary`
   - Comprehensive test coverage for edge cases

3. **Port line-based splitter** from `atlas/splitter/line_based.py`:
   - Copy `LineBasedSplitter`
   - Simple implementation, minimal testing needed

4. **Port registry** from `atlas/splitter/registry.py`:
   - Copy `SplitterRegistry`, `get_default_registry()`, `get_splitter()`
   - Update built-in splitter registration

### Phase 3: Update Adapters (Week 3)

1. **Update ScribeWorkerAdapter:**
   ```python
   # OLD
   from atlas.adapters.artifact_store import ArtifactStoreAdapter
   from atlas.adapters.ticket_system import TicketSystemAdapter
   from atlas.models.work_item import DocChunkPayload, WorkItem
   from atlas.workers.base import Worker

   # NEW
   from war_rig.chunking.models.work_item import DocChunkPayload, WorkItem
   # Remove Worker base class, inline functionality
   ```

2. **Simplify adapter hierarchy:**
   - Remove dependency on `atlas.adapters.*` abstract bases
   - Inline necessary functionality directly into War Rig adapters
   - FileArtifactAdapter becomes standalone implementation
   - BeadsTicketAdapter becomes standalone implementation

3. **Update TicketOrchestrator:**
   ```python
   # OLD
   from atlas.splitter import get_splitter
   from atlas.models.manifest import SplitterProfile

   # NEW
   from war_rig.chunking import get_splitter, SplitterProfile
   ```

### Phase 4: Testing and Validation (Week 4)

1. **Unit tests for chunking package:**
   - Test splitter registry registration and lookup
   - Test COBOL splitter with various COBOL structures
   - Test line-based splitter with edge cases
   - Test chunk validation logic

2. **Integration tests:**
   - End-to-end test: large COBOL file -> chunks -> merged documentation
   - Test with actual COBOL sample files from test fixtures

3. **Regression tests:**
   - Verify existing War Rig functionality unchanged
   - Compare output with Atlas-based processing

### Phase 5: Remove Atlas Dependency (Week 5)

1. **Update pyproject.toml:**
   ```toml
   [project]
   dependencies = [
       # Remove: "atlas>=1.0.0",
   ]
   ```

2. **Remove old imports:**
   - Search and replace all `from atlas.*` imports
   - Verify no remaining references to atlas package

3. **Update documentation:**
   - Update README.md installation instructions
   - Update deployment documentation

---

## 5. Design Decisions

### 5.1 Why Copy Instead of Refactor?

The Atlas chunking code is well-designed and battle-tested. Rather than reimplementing from scratch:

1. **Reduced risk:** Proven algorithms, known edge cases handled
2. **Faster delivery:** Copy-and-adapt vs design-from-scratch
3. **Consistency:** Same behavior, just different package location

### 5.2 Why Flatten the Adapter Hierarchy?

The current adapter pattern (`Worker`, `ArtifactStoreAdapter`, `TicketSystemAdapter`) adds indirection without clear benefit for War Rig's use case:

1. **Single implementation:** War Rig only has one implementation of each
2. **Simpler debugging:** Fewer layers to trace through
3. **Reduced coupling:** No need to maintain interface compatibility

### 5.3 Why Keep SplitterRegistry?

The registry pattern enables future extensibility:

1. **Plugin architecture:** Easy to add new splitters (JCL, PL/I, etc.)
2. **Runtime configuration:** Swap splitters without code changes
3. **Testing:** Easy to inject mock splitters

### 5.4 Why Pydantic for Models?

Consistency with War Rig's existing model patterns:

1. **Validation:** Automatic field validation
2. **Serialization:** Easy JSON serialization/deserialization
3. **Consistency:** Matches `war_rig.models.templates` patterns

---

## 6. Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Subtle bugs in ported COBOL splitter | Medium | High | Comprehensive test coverage with real COBOL samples |
| Missing edge cases in semantic parsing | Medium | Medium | Test with diverse COBOL dialects (COBOL-85, COBOL-2002) |
| Performance regression | Low | Medium | Benchmark before/after with large files |
| Breaking changes to existing workflows | Low | High | Maintain API compatibility, regression tests |
| Future Atlas updates not incorporated | Medium | Low | Document divergence, evaluate updates quarterly |

---

## 7. Success Criteria

1. **Functional equivalence:** Chunked processing produces identical results to Atlas-based processing
2. **Zero external dependencies:** War Rig runs without atlas package installed
3. **Test coverage:** >90% coverage on chunking package
4. **Documentation:** All public APIs documented with docstrings
5. **Performance:** No measurable regression in processing time

---

## 8. Future Enhancements

### 8.1 Additional Splitters

- **JCLSplitter:** Split JCL at step boundaries
- **PLISplitter:** PL/I-aware semantic splitting
- **BMSSplitter:** BMS map-aware splitting

### 8.2 Enhanced Merge Strategies

- **HierarchicalMerge:** Multi-level merge DAG for very large files
- **IncrementalMerge:** Update merged documentation without full reprocessing

### 8.3 Caching Layer

- **ChunkCache:** Cache chunk results for repeated processing
- **SplitCache:** Cache split boundaries for unchanged sources

---

## Appendix A: File-by-File Mapping

| Atlas Source | War Rig Target |
|--------------|----------------|
| `atlas/models/enums.py` | `war_rig/chunking/enums.py` |
| `atlas/models/manifest.py` (partial) | `war_rig/chunking/models/profile.py`, `war_rig/chunking/models/chunk.py` |
| `atlas/models/artifact.py` | `war_rig/chunking/models/artifact.py` |
| `atlas/models/work_item.py` | `war_rig/chunking/models/work_item.py` |
| `atlas/splitter/base.py` | `war_rig/chunking/splitters/base.py` |
| `atlas/splitter/cobol.py` | `war_rig/chunking/splitters/cobol.py` |
| `atlas/splitter/line_based.py` | `war_rig/chunking/splitters/line_based.py` |
| `atlas/splitter/registry.py` | `war_rig/chunking/splitters/registry.py` |
| `atlas/splitter/__init__.py` | `war_rig/chunking/__init__.py` |

---

## Appendix B: Import Mapping

```python
# Adapters migration
"from atlas.adapters.artifact_store import ArtifactStoreAdapter"
-> Remove (inline functionality)

"from atlas.adapters.ticket_system import TicketSystemAdapter"
-> Remove (inline functionality)

"from atlas.workers.base import Worker"
-> Remove (inline functionality)

# Models migration
"from atlas.models.enums import ChunkKind, WorkItemStatus, WorkItemType"
-> "from war_rig.chunking import ChunkKind, WorkItemStatus, WorkItemType"

"from atlas.models.manifest import SplitterProfile, ChunkSpec"
-> "from war_rig.chunking import SplitterProfile, ChunkSpec"

"from atlas.models.work_item import DocChunkPayload, WorkItem, ChunkLocator"
-> "from war_rig.chunking import DocChunkPayload, WorkItem, ChunkLocator"

"from atlas.models.artifact import ArtifactRef"
-> "from war_rig.chunking import ArtifactRef"

# Splitter migration
"from atlas.splitter import get_splitter"
-> "from war_rig.chunking import get_splitter"

"from atlas.splitter import COBOLSplitter, LineBasedSplitter"
-> "from war_rig.chunking import COBOLSplitter, LineBasedSplitter"
```

---

## Appendix C: Test Plan Outline

### Unit Tests

```
tests/chunking/
    test_enums.py
    models/
        test_chunk.py
        test_profile.py
        test_artifact.py
        test_work_item.py
    splitters/
        test_base.py
        test_cobol.py
        test_line_based.py
        test_registry.py
```

### Integration Tests

```
tests/integration/
    test_chunked_processing.py      # End-to-end chunked file processing
    test_migration_parity.py        # Compare Atlas vs embedded results
```

### Test Fixtures

```
tests/fixtures/cobol/
    small_program.cbl               # < context budget
    large_program.cbl               # > context budget, needs chunking
    nested_paragraphs.cbl           # Complex paragraph structure
    minimal_structure.cbl           # Missing divisions
    cics_program.cbl                # CICS commands
    db2_program.cbl                 # Embedded SQL
```
