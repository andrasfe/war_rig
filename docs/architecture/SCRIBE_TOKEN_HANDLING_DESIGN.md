# Scribe Token Limit Handling - Architecture Design

## Executive Summary

This document proposes a clean architecture for token limit enforcement in the Scribe worker pool. The current implementation has token limit logic scattered across multiple code paths, leading to inconsistency and maintenance challenges. The proposed design centralizes token management into a single abstraction layer.

## Current State Analysis

### Current Token Limit Enforcement Points

The current `scribe_pool.py` enforces token limits in **four separate places**:

1. **Cycle 1 DOCUMENTATION** (lines 697-717): Uses chunking for large files
2. **Cycle > 1 DOCUMENTATION** (lines 729-736): Uses sampling via `_sample_source_code()`
3. **CLARIFICATION tickets** (lines 981-988): Uses sampling
4. **CHROME tickets** (lines 1167-1174): Uses sampling

### Problems with Current Approach

1. **DRY Violation**: Same sampling logic repeated 3 times with identical parameters
2. **Magic Numbers**: `max_prompt_tokens - 4000` hardcoded in 4 places
3. **Module-Level Function**: `_sample_source_code()` is a standalone function without clear ownership
4. **Inconsistent Token Budget**: Each code path independently calculates `max_source_tokens`
5. **No Single Source of Truth**: Token limit policy scattered across the file

### Current Decision Logic

```
Is it Cycle 1 DOCUMENTATION?
  YES -> Is source > token limit?
           YES -> Use CHUNKING (semantic split, process each, merge)
           NO  -> Process directly
  NO  -> Is source > token limit?
           YES -> Use SAMPLING (random contiguous portion)
           NO  -> Process directly
```

This logic is **correct** but implemented inconsistently.

## Proposed Architecture

### Design Principles

1. **Single Responsibility**: One class handles all source code preparation
2. **Strategy Pattern**: Chunking and sampling are interchangeable strategies
3. **Configuration Driven**: Token limits from config, not hardcoded
4. **Testable**: Each strategy independently testable
5. **Extensible**: Easy to add new strategies (e.g., intelligent summarization)

### Component Diagram

```
                    +------------------------+
                    |    ScribeWorker        |
                    +------------------------+
                              |
                              | uses
                              v
                    +------------------------+
                    | SourceCodePreparer     |
                    +------------------------+
                    | - config: ScribeConfig |
                    | - estimator: TokenEst  |
                    +------------------------+
                    | + prepare()            |
                    | + needs_preparation()  |
                    +------------------------+
                              |
              +---------------+---------------+
              |               |               |
              v               v               v
    +-------------+   +-------------+   +-------------+
    | PassThrough |   | Sampling    |   | Chunking    |
    | Strategy    |   | Strategy    |   | Strategy    |
    +-------------+   +-------------+   +-------------+
```

### New Class: `SourceCodePreparer`

```python
@dataclass
class PreparedSource:
    """Result of source code preparation for Scribe processing.

    Attributes:
        source_code: The prepared source (possibly sampled/chunked).
        strategy_used: The strategy that was applied.
        was_modified: True if source was altered from original.
        chunks: If chunked, the list of CodeChunks for separate processing.
        metadata: Additional info about the preparation (e.g., sample range).
    """
    source_code: str
    strategy_used: Literal["passthrough", "sampling", "chunking"]
    was_modified: bool
    chunks: list[CodeChunk] | None = None
    metadata: dict[str, Any] = field(default_factory=dict)


class SourceCodePreparer:
    """Prepares source code to fit within token limits.

    This class centralizes all token limit enforcement for Scribe.
    It decides whether to pass through, sample, or chunk based on
    the processing context and token budget.

    The decision logic:
    - Cycle 1 DOCUMENTATION with large files: CHUNKING
    - All other cases with large files: SAMPLING
    - Files within limit: PASSTHROUGH

    Usage:
        preparer = SourceCodePreparer(config.scribe)
        result = preparer.prepare(
            source_code=source,
            context=PreparationContext(
                ticket_type=TicketType.DOCUMENTATION,
                cycle_number=1,
                has_previous_template=False,
            )
        )

        if result.strategy_used == "chunking":
            # Process each chunk separately, then merge
            for chunk in result.chunks:
                output = await scribe_agent.ainvoke(...)
        else:
            # Process the (possibly sampled) source directly
            output = await scribe_agent.ainvoke(source_code=result.source_code)
    """

    # Reserve tokens for system prompt, schema, instructions, response buffer
    OVERHEAD_TOKENS: int = 4000

    def __init__(
        self,
        config: ScribeConfig,
        estimator: TokenEstimator | None = None,
    ):
        self.config = config
        self.estimator = estimator or TokenEstimator()

    @property
    def max_source_tokens(self) -> int:
        """Maximum tokens available for source code after overhead."""
        return self.config.max_prompt_tokens - self.OVERHEAD_TOKENS

    def prepare(
        self,
        source_code: str,
        context: PreparationContext,
    ) -> PreparedSource:
        """Prepare source code for Scribe processing.

        Args:
            source_code: The raw source code.
            context: Context about the processing scenario.

        Returns:
            PreparedSource with the appropriate strategy applied.
        """
        source_tokens = self.estimator.estimate_source_tokens(source_code)

        # If within limit, pass through unchanged
        if source_tokens <= self.max_source_tokens:
            return PreparedSource(
                source_code=source_code,
                strategy_used="passthrough",
                was_modified=False,
            )

        # Decide strategy based on context
        if self._should_use_chunking(context):
            return self._apply_chunking(source_code, context)
        else:
            return self._apply_sampling(source_code, source_tokens)

    def _should_use_chunking(self, context: PreparationContext) -> bool:
        """Determine if chunking should be used.

        Chunking is used for initial documentation (cycle 1) because:
        - We need to see the entire file to build a complete template
        - Each chunk builds partial documentation, then merged

        Sampling is used for updates because:
        - We already have a template to update
        - A representative sample is sufficient for targeted updates
        """
        return (
            context.ticket_type == TicketType.DOCUMENTATION
            and context.cycle_number == 1
            and not context.has_previous_template
        )


@dataclass
class PreparationContext:
    """Context for source code preparation decisions.

    Attributes:
        ticket_type: Type of ticket being processed.
        cycle_number: Current iteration/cycle number.
        has_previous_template: Whether a previous template exists.
        file_type: Type of source file (for chunking strategy).
    """
    ticket_type: TicketType
    cycle_number: int
    has_previous_template: bool
    file_type: FileType = FileType.OTHER
```

### Refactored ScribeWorker Usage

Before (current - scattered logic):

```python
# In _process_documentation_ticket:
if ticket.cycle_number == 1:
    source_tokens = estimator.estimate_source_tokens(source_code)
    if source_tokens > max_source_tokens:
        return await self._process_chunked_documentation(...)

# Later in same method:
if ticket.cycle_number > 1:
    source_code, was_sampled = _sample_source_code(source_code, max_source_tokens)

# In _process_clarification_ticket:
source_code, was_sampled = _sample_source_code(source_code, max_source_tokens)

# In _process_chrome_ticket:
source_code, was_sampled = _sample_source_code(source_code, max_source_tokens)
```

After (clean - single entry point):

```python
class ScribeWorker:
    def __init__(self, ...):
        ...
        self._source_preparer = SourceCodePreparer(config.scribe)

    async def _prepare_source_for_processing(
        self,
        source_code: str,
        ticket: ProgramManagerTicket,
        previous_template: DocumentationTemplate | None,
    ) -> PreparedSource:
        """Prepare source code with centralized token limit handling."""
        context = PreparationContext(
            ticket_type=ticket.ticket_type,
            cycle_number=ticket.cycle_number,
            has_previous_template=previous_template is not None,
            file_type=self._determine_file_type(ticket.file_name),
        )
        return self._source_preparer.prepare(source_code, context)
```

### Updated Processing Flow

```python
async def _process_documentation_ticket(self, ticket, formatting_strict=False):
    # Load source code (unchanged)
    source_code = self._load_source_code(ticket)

    # Load previous template if exists
    previous_template = None
    if ticket.cycle_number > 1:
        previous_template = self._load_previous_template(ticket.file_name)

    # SINGLE TOKEN LIMIT ENFORCEMENT POINT
    prepared = await self._prepare_source_for_processing(
        source_code, ticket, previous_template
    )

    if prepared.strategy_used == "chunking":
        return await self._process_chunked(ticket, prepared.chunks, formatting_strict)

    # For passthrough or sampling, process the prepared source
    return await self._process_direct(
        ticket, prepared.source_code, previous_template, formatting_strict
    )
```

## Strategy Details

### Passthrough Strategy

**When**: Source fits within token limit.

**Action**: Return source unchanged.

```python
def _apply_passthrough(self, source_code: str) -> PreparedSource:
    return PreparedSource(
        source_code=source_code,
        strategy_used="passthrough",
        was_modified=False,
    )
```

### Sampling Strategy

**When**: Source exceeds limit AND (cycle > 1 OR has previous template OR CLARIFICATION/CHROME ticket).

**Rationale**: We already have a template; a representative sample is sufficient for targeted updates.

**Algorithm**:
1. Calculate target line count based on token ratio
2. Pick a random starting point (for diversity across retries)
3. Extract contiguous lines
4. Add header noting the sample range
5. Truncate if still over character limit

```python
def _apply_sampling(self, source_code: str, source_tokens: int) -> PreparedSource:
    lines = source_code.split("\n")
    total_lines = len(lines)

    # Calculate how many lines we can include
    ratio = self.max_source_tokens / source_tokens
    target_lines = max(10, int(total_lines * ratio))

    # Random start for diversity
    max_start = max(0, total_lines - target_lines)
    start_line = random.randint(0, max_start) if max_start > 0 else 0

    # Extract sample
    sampled_lines = lines[start_line:start_line + target_lines]
    sampled_code = "\n".join(sampled_lines)

    # Add informative header
    header = (
        f"* NOTE: Source code sampled (lines {start_line + 1}-"
        f"{start_line + len(sampled_lines)} of {total_lines})\n"
        f"* Full source exceeds token limit ({source_tokens} tokens)\n\n"
    )

    return PreparedSource(
        source_code=header + sampled_code,
        strategy_used="sampling",
        was_modified=True,
        metadata={
            "original_lines": total_lines,
            "sampled_lines": len(sampled_lines),
            "start_line": start_line,
            "original_tokens": source_tokens,
        },
    )
```

### Chunking Strategy

**When**: Cycle 1 DOCUMENTATION with large file (no previous template).

**Rationale**: Initial documentation needs to see the entire file to build complete template.

**Algorithm**:
1. Select appropriate chunker (COBOLChunker or GenericChunker)
2. Split at semantic boundaries (divisions/sections for COBOL)
3. Return list of chunks for separate processing
4. Caller merges results using ChunkMerger

```python
def _apply_chunking(
    self,
    source_code: str,
    context: PreparationContext,
) -> PreparedSource:
    # Select chunker based on file type
    if context.file_type == FileType.COBOL:
        chunker = COBOLChunker(self.estimator)
    else:
        chunker = GenericChunker(self.estimator)

    # Chunk the source
    result = chunker.chunk(
        source_code=source_code,
        max_tokens=self.max_source_tokens,
        file_name="source",  # Used for logging
    )

    return PreparedSource(
        source_code=source_code,  # Original preserved
        strategy_used="chunking",
        was_modified=False,  # Original not modified, just split
        chunks=result.chunks,
        metadata={
            "chunk_count": result.chunk_count,
            "chunking_strategy": result.chunking_strategy,
        },
    )
```

## File Organization

### New Files

```
war_rig/
  workers/
    source_preparer.py    # New: SourceCodePreparer, PreparedSource, PreparationContext
```

### Modified Files

```
war_rig/
  workers/
    scribe_pool.py        # Refactored to use SourceCodePreparer
```

### Deleted Code

- Remove `_sample_source_code()` module-level function from `scribe_pool.py`
- Remove inline token estimation code from processing methods
- Remove duplicated `max_prompt_tokens - 4000` calculations

## Implementation Plan

### Phase 1: Create Source Preparer (New File)

1. Create `/home/andras/war_rig/war_rig/workers/source_preparer.py`
2. Implement `PreparationContext` dataclass
3. Implement `PreparedSource` dataclass
4. Implement `SourceCodePreparer` class with all strategies
5. Add comprehensive unit tests

### Phase 2: Integrate into ScribeWorker

1. Add `SourceCodePreparer` to `ScribeWorker.__init__`
2. Create `_prepare_source_for_processing()` helper method
3. Refactor `_process_documentation_ticket()` to use preparer
4. Refactor `_process_clarification_ticket()` to use preparer
5. Refactor `_process_chrome_ticket()` to use preparer
6. Remove `_sample_source_code()` function

### Phase 3: Cleanup and Testing

1. Remove all inline token limit calculations
2. Update/add integration tests
3. Verify all ticket types work correctly
4. Update documentation

## Testing Strategy

### Unit Tests for SourceCodePreparer

```python
class TestSourceCodePreparer:
    def test_passthrough_when_under_limit(self):
        """Source under token limit returns unchanged."""

    def test_sampling_for_clarification_ticket(self):
        """CLARIFICATION tickets use sampling strategy."""

    def test_sampling_for_chrome_ticket(self):
        """CHROME tickets use sampling strategy."""

    def test_sampling_for_cycle_gt_1_documentation(self):
        """Cycle > 1 DOCUMENTATION uses sampling."""

    def test_chunking_for_cycle_1_documentation(self):
        """Cycle 1 DOCUMENTATION uses chunking for large files."""

    def test_sampling_adds_header(self):
        """Sampled source includes informative header."""

    def test_chunking_preserves_original(self):
        """Chunking returns original source with chunks list."""
```

### Integration Tests

```python
class TestScribeWorkerTokenHandling:
    async def test_large_file_cycle_1_uses_chunking(self):
        """Large file on cycle 1 triggers chunked processing."""

    async def test_large_file_cycle_2_uses_sampling(self):
        """Large file on cycle 2 uses sampling."""

    async def test_clarification_large_file_uses_sampling(self):
        """CLARIFICATION with large source uses sampling."""
```

## Migration Notes

### Backward Compatibility

- No external API changes
- Internal refactoring only
- Same behavior, cleaner implementation

### Configuration

- `config.scribe.max_prompt_tokens` remains the single configuration point
- `OVERHEAD_TOKENS` (4000) moved to `SourceCodePreparer` as class constant
- Can be made configurable if needed in future

## Conclusion

This design:

1. **Centralizes** token limit logic in `SourceCodePreparer`
2. **Eliminates** code duplication (3 sampling calls -> 1 strategy)
3. **Documents** the policy decision (chunking vs sampling) in one place
4. **Enables** easy addition of new strategies (e.g., intelligent summarization)
5. **Improves** testability with isolated components
6. **Maintains** current behavior while cleaning up implementation

The refactoring is low-risk as it preserves existing behavior while improving code quality and maintainability.
