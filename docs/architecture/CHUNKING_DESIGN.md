# Large File Chunking Design

## Problem Statement

War Rig's Scribe agent sends entire source files to the LLM, which fails for large COBOL files (~35K tokens) that exceed context limits. The `scribe_max_prompt_tokens` config field (default 15000) exists but is not used by any code.

**Related Tickets:**
- war_rig-dowu: Investigation
- war_rig-7gsl: Root cause analysis
- war_rig-cr7p: Fix implementation

## Design Goals

1. **Prevent context overflow** - Never send prompts exceeding the configured token budget
2. **Preserve semantic integrity** - Split at meaningful COBOL boundaries, not arbitrary line counts
3. **Minimize complexity** - Simple solution first, with extension points for future sophistication
4. **Backward compatible** - Small files continue to work exactly as before

## Architecture Overview

```
                    ScribeWorker
                         |
                         v
              +--------------------+
              |  ChunkingStrategy  |  <-- NEW: Decides how to handle file
              +--------------------+
                    |         |
         +----------+         +----------+
         |                               |
         v                               v
    [Small File]                   [Large File]
    Process directly               Split into chunks
         |                               |
         v                               v
    ScribeAgent                   ScribeAgent (per chunk)
         |                               |
         v                               v
    Single template              Multiple partial templates
                                         |
                                         v
                                  +----------------+
                                  | ChunkMerger    |  <-- NEW: Combines results
                                  +----------------+
                                         |
                                         v
                                  Unified template
```

## Component Design

### 1. Token Estimator (war_rig/chunking/estimator.py)

Simple token estimation using character-based heuristics. More accurate than nothing, cheaper than tokenization.

```python
class TokenEstimator:
    """Estimates token counts for prompt construction."""

    CHARS_PER_TOKEN = 4  # Conservative estimate for COBOL

    def estimate_tokens(self, text: str) -> int:
        """Estimate token count for text."""

    def estimate_prompt_tokens(self, scribe_input: ScribeInput) -> int:
        """Estimate total tokens for a complete Scribe prompt."""
```

### 2. COBOL Chunker (war_rig/chunking/cobol_chunker.py)

Splits COBOL files at semantic boundaries (divisions, sections, paragraphs).

```python
@dataclass
class CodeChunk:
    """A semantically-bounded code chunk."""
    chunk_id: str
    content: str
    start_line: int
    end_line: int
    context_type: str  # "division", "section", "paragraph", "block"
    parent_context: str | None  # What division/section this belongs to
    estimated_tokens: int

class COBOLChunker:
    """Splits COBOL code at semantic boundaries."""

    def needs_chunking(self, source_code: str, max_tokens: int) -> bool:
        """Check if file exceeds token budget."""

    def chunk(self, source_code: str, max_tokens: int) -> list[CodeChunk]:
        """Split code into semantically-bounded chunks."""
```

**Chunking Strategy (Priority Order):**
1. Try to keep entire PROCEDURE DIVISION together (most important for logic)
2. If too large, split at SECTION boundaries
3. If sections too large, split at paragraph boundaries
4. If paragraphs too large, split at statement boundaries (last resort)
5. Always include header context (IDENTIFICATION, DATA DIVISION summaries)

### 3. Chunk Merger (war_rig/chunking/merger.py)

Combines multiple partial templates into a unified result.

```python
class ChunkMerger:
    """Merges multiple chunk documentation into unified template."""

    def merge(
        self,
        chunks: list[CodeChunk],
        chunk_outputs: list[ScribeOutput],
        file_name: str,
    ) -> DocumentationTemplate:
        """Merge chunk outputs into single unified template."""
```

**Merge Strategy:**
- **header/purpose**: Use from first chunk, augment with others
- **inputs/outputs**: Deduplicate by name, merge citations
- **paragraphs**: Combine all, adjust citation line numbers
- **business_rules**: Combine all, deduplicate by description
- **copybooks_used**: Deduplicate by name
- **error_handling**: Combine all

### 4. Integration Point: ScribeWorker

Modify `_process_documentation_ticket()` to use chunking when needed:

```python
async def _process_documentation_ticket(self, ticket: ProgramManagerTicket) -> ScribeOutput:
    source_code = self._load_source_code(ticket)

    # NEW: Check if chunking needed
    chunker = COBOLChunker()
    max_tokens = self.config.scribe.max_prompt_tokens

    if chunker.needs_chunking(source_code, max_tokens):
        return await self._process_chunked_file(ticket, source_code)
    else:
        return await self._process_direct(ticket, source_code)
```

## Configuration

Use existing config field (no changes needed):

```python
# config.py - already exists
class ScribeConfig(ModelConfig):
    max_prompt_tokens: int = Field(
        default=15000,
        description="Maximum tokens for Scribe prompt (will truncate to fit)",
    )
```

Environment variable: `SCRIBE_MAX_PROMPT_TOKENS=15000`

## Token Budget Allocation

For a 15000 token budget:

| Component | Tokens | Notes |
|-----------|--------|-------|
| System prompt | ~2000 | Fixed, comprehensive |
| Template schema | ~1500 | JSON schema example |
| Preprocessor hints | ~500 | Structural info |
| Context (copybooks) | ~2000 | Referenced copybooks |
| Source code | ~8000 | Main content |
| Instructions | ~500 | Task description |
| Buffer | ~500 | Safety margin |

For chunked files, each chunk gets ~8000 tokens of source, with shared context (system prompt, schema, copybooks) included in each call.

## Alternatives Considered

### 1. Simple Truncation (Rejected)
- **Pro**: Trivial to implement
- **Con**: Loses critical code at end of file (often the actual business logic)

### 2. Multiple Tickets per Chunk (Rejected)
- **Pro**: Fits existing ticket model
- **Con**: Complicates orchestration, requires cross-ticket merging
- **Con**: Each chunk validated separately, misses cross-chunk issues

### 3. Two-Pass Analysis (Considered for Future)
- Pass 1: Quick scan of entire file for structure
- Pass 2: Detailed analysis of each section
- **Pro**: Better understanding of context
- **Con**: More complex, 2x API calls

### 4. Sliding Window with Overlap (Rejected)
- **Pro**: Ensures no information lost at boundaries
- **Con**: Duplicate processing, merge complexity

## Implementation Phases

### Phase 1: Minimum Viable (This Ticket)
1. Implement `TokenEstimator` with simple heuristic
2. Implement `COBOLChunker` with division/section/paragraph splitting
3. Implement `ChunkMerger` with basic deduplication
4. Integrate into `ScribeWorker._process_documentation_ticket()`
5. Add logging for chunk decisions

### Phase 2: Refinement (Future)
1. Add actual tokenizer for accurate counts (tiktoken)
2. Improve merge quality with LLM-assisted reconciliation
3. Add chunk-level confidence tracking
4. Handle inter-chunk references (e.g., PERFORM targets across chunks)

### Phase 3: Advanced (Future)
1. Adaptive chunking based on code complexity
2. Dependency-aware chunking (keep related paragraphs together)
3. Incremental re-documentation (only re-process changed chunks)

## Testing Strategy

1. **Unit Tests**: TokenEstimator, COBOLChunker, ChunkMerger
2. **Integration Tests**: End-to-end with mock ScribeAgent
3. **Regression Tests**: Ensure small files unchanged
4. **Real-World Test**: Run against CardDemo's largest files

## Files to Create/Modify

### New Files
- `war_rig/chunking/__init__.py`
- `war_rig/chunking/estimator.py`
- `war_rig/chunking/cobol_chunker.py`
- `war_rig/chunking/merger.py`
- `war_rig/chunking/models.py`
- `tests/test_chunking/`

### Modified Files
- `war_rig/workers/scribe_pool.py` - Add chunking integration
- `war_rig/agents/scribe.py` - Add chunk context to prompts (optional)

## Success Criteria

1. Files under 15K tokens: No change in behavior
2. Files over 15K tokens: Successfully documented without context overflow
3. Merged documentation: All sections populated, no duplicates, correct citations
4. Performance: <2x increase in API calls for chunked files
