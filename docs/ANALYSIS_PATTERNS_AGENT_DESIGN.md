# Analysis Patterns Integration Design

## Status: Draft

**Author:** War Rig Architecture Team
**Date:** 2025-01-29
**Ticket:** N/A (pre-implementation design)

---

## 1. Executive Summary

This document outlines the design for integrating Citadel's new analysis patterns
(data_flow, control_flow, error_handling) into the Scribe and Challenger documentation
agents. The goal is to provide agents with **actionable insights** derived from static
analysis, not raw pattern dumps that would overwhelm LLM context windows.

### Key Design Principles

1. **Summarize, don't dump**: Transform raw matches into aggregated insights
2. **Paragraph-level focus**: Align pattern insights with paragraph documentation
3. **Actionable guidance**: Tell agents *what to document*, not just *what exists*
4. **Bounded context**: Stay within token budgets via intelligent filtering
5. **Differentiated use**: Scribe uses for writing, Challenger uses for validation

---

## 2. Current State

### 2.1 Citadel SDK Capability

The `Citadel.get_analysis_patterns(file_path)` method returns a `FileAnalysisPatternResult`:

```python
@dataclass
class FileAnalysisPatternResult:
    file_path: str
    language: str
    categories: dict[str, AnalysisCategoryResult]
    total_matches: int
    coverage_pct: float
    required_missing: list[str]
    error: str | None
```

Each `AnalysisCategoryResult` contains:

```python
@dataclass
class AnalysisCategoryResult:
    category: str  # "data_flow", "control_flow", "error_handling"
    matches: list[AnalysisPatternMatchSDK]
    match_count: int
    patterns_matched: dict[str, int]  # pattern_name -> count
```

Individual matches look like:

```python
@dataclass
class AnalysisPatternMatchSDK:
    pattern_name: str      # e.g., "move_simple", "if_perform"
    category: str          # e.g., "data_flow"
    captured: list[str]    # e.g., ["SOURCE-VAR", "TARGET-VAR"]
    line: int | None       # line number
    context: list[str]     # surrounding code lines
```

### 2.2 Current Agent Integration

**ScribeInput** has:
- `citadel_outline: list[dict] | None` - paragraph names, line ranges, calls

**ChallengerInput** has:
- `citadel_context: dict | None` - paragraph bodies for cross-reference validation

### 2.3 The Problem

A typical COBOL file may have:
- 500+ data_flow matches (MOVE, COMPUTE, SET, ADD, etc.)
- 100+ control_flow matches (IF/PERFORM, EVALUATE, loops)
- 50+ error_handling matches (file status, exception handlers)

Passing all matches to an LLM would:
1. Exceed token limits
2. Overwhelm the model with noise
3. Not provide actionable guidance

---

## 3. Proposed Solution

### 3.1 Aggregation Strategy

Transform raw matches into **paragraph-level summaries** with bounded size.

#### Layer 1: Pattern Aggregation (per file)

Aggregate raw matches into a file-level summary:

```python
@dataclass
class FilePatternSummary:
    """Aggregated pattern statistics for a file."""

    # Top-level metrics
    total_data_flow_ops: int
    total_control_flow_ops: int
    total_error_handlers: int

    # Key variables (most frequently used in data flow)
    key_variables: list[str]  # Top 10 by frequency

    # Control flow complexity indicators
    has_nested_conditionals: bool
    has_loops: bool
    has_go_to: bool
    max_nesting_depth: int

    # Error handling coverage
    file_status_checks: list[str]  # File names with status checks
    exception_handlers: list[str]  # ON EXCEPTION, INVALID KEY, etc.
    abend_calls: list[str]         # Calls to abend routines
```

#### Layer 2: Paragraph-Level Insights

Associate patterns with paragraphs by line range:

```python
@dataclass
class ParagraphPatternInsight:
    """Actionable insights for a single paragraph."""

    paragraph_name: str
    line_start: int
    line_end: int

    # Data flow summary
    variables_read: list[str]      # Variables consumed (max 5)
    variables_written: list[str]   # Variables modified (max 5)
    key_transformations: list[str] # COMPUTE/ADD/SUBTRACT targets

    # Control flow summary
    decision_points: int           # Number of IF/EVALUATE
    performs_paragraphs: list[str] # Paragraphs called (from citadel_outline)
    has_loop: bool                 # Contains PERFORM UNTIL/VARYING

    # Error handling
    handles_errors: bool           # Has error checking logic
    error_types: list[str]         # e.g., ["file_status", "db2_sqlcode"]

    # Complexity indicator
    complexity: str                # "simple", "moderate", "complex"
```

#### Layer 3: Documentation Hints

Transform insights into natural language guidance:

```python
@dataclass
class DocumentationHint:
    """Specific documentation guidance for Scribe."""

    paragraph_name: str
    hints: list[str]  # 2-4 sentences of guidance

    # Example hints:
    # - "This paragraph reads CUSTOMER-REC and ACCOUNT-STATUS, and writes
    #    to TRANSACTION-LOG. Document the transformation logic."
    # - "Contains 3 decision points with FILE-STATUS checks. Describe the
    #    error handling for each file operation."
    # - "Loops over input records using PERFORM UNTIL. Explain termination
    #    condition and accumulator variables."
```

### 3.2 Token Budget Management

#### Size Constraints

| Component | Max Tokens | Rationale |
|-----------|------------|-----------|
| File-level summary | 200 | Quick overview |
| Per-paragraph insight | 100 | 30 paragraphs = 3000 tokens max |
| Documentation hint | 50 | Natural language is compact |
| **Total budget** | ~4000 | Leaves room for source code |

#### Filtering Rules

1. **Variable filtering**: Only include variables appearing 3+ times
2. **Paragraph focus**: Only top 20 paragraphs by complexity
3. **Pattern sampling**: If >100 matches in a category, sample representatives
4. **Null suppression**: Omit empty/trivial entries

### 3.3 Data Flow

```
                                  get_analysis_patterns()
                                           |
                                           v
                              +------------------------+
                              |  Raw Pattern Matches   |
                              |  (hundreds/thousands)  |
                              +------------------------+
                                           |
                                           v
                              +------------------------+
                              |   PatternAggregator    |
                              +------------------------+
                              | - aggregate_to_file()  |
                              | - aggregate_to_paras() |
                              | - generate_hints()     |
                              +------------------------+
                                     |           |
                              +------+           +------+
                              |                        |
                              v                        v
                    +------------------+    +------------------+
                    | ScribeInput      |    | ChallengerInput  |
                    +------------------+    +------------------+
                    | pattern_insights |    | pattern_facts    |
                    | doc_hints        |    | validation_cues  |
                    +------------------+    +------------------+
```

---

## 4. New Field Specifications

### 4.1 ScribeInput Extensions

```python
class ScribeInput(AgentInput):
    # ... existing fields ...

    pattern_insights: PatternInsightsForScribe | None = Field(
        default=None,
        description="Aggregated analysis pattern insights to guide documentation",
    )
```

Where:

```python
@dataclass
class PatternInsightsForScribe:
    """Analysis pattern insights formatted for Scribe consumption."""

    # File-level overview (helps with purpose/summary sections)
    file_summary: dict[str, Any]
    # Keys: key_variables, complexity_indicators, error_handling_coverage

    # Per-paragraph documentation hints (aligns with citadel_outline)
    paragraph_hints: list[dict[str, Any]]
    # Each: {paragraph_name, hints: [str], variables: {read, write}, complexity}

    # Critical patterns to document (things the Scribe MUST address)
    critical_patterns: list[str]
    # e.g., ["Document the COMPUTE at line 234 that calculates BALANCE"]
```

### 4.2 ChallengerInput Extensions

```python
class ChallengerInput(AgentInput):
    # ... existing fields ...

    pattern_facts: PatternFactsForChallenger | None = Field(
        default=None,
        description="Ground-truth pattern facts for validation cross-reference",
    )
```

Where:

```python
@dataclass
class PatternFactsForChallenger:
    """Analysis pattern facts for Challenger validation."""

    # Per-paragraph ground truth (for cross-referencing documentation)
    paragraph_facts: list[dict[str, Any]]
    # Each: {
    #   paragraph_name,
    #   data_flow_count,  # Number of MOVE/COMPUTE/etc ops
    #   control_flow_count,  # Number of IF/PERFORM/etc
    #   error_handling_count,  # Number of error handlers
    #   key_operations: [str],  # Most important operations to verify
    # }

    # Validation checkpoints (things Challenger should verify)
    validation_cues: list[str]
    # e.g., ["PROCESS-RECORD has 5 MOVE operations - verify data_flow describes these"]

    # Pattern coverage expectations
    expected_coverage: dict[str, int]
    # {paragraph_name: expected_data_flow_mentions}
```

---

## 5. Transformation Logic

### 5.1 PatternAggregator Class

```python
class PatternAggregator:
    """Transforms raw Citadel patterns into agent-consumable insights."""

    def __init__(
        self,
        max_variables: int = 10,
        max_paragraphs: int = 20,
        max_hints_per_paragraph: int = 4,
    ):
        self.max_variables = max_variables
        self.max_paragraphs = max_paragraphs
        self.max_hints_per_paragraph = max_hints_per_paragraph

    def aggregate_for_scribe(
        self,
        pattern_result: FileAnalysisPatternResult,
        citadel_outline: list[dict],
    ) -> PatternInsightsForScribe:
        """Transform patterns into Scribe-friendly insights."""
        ...

    def aggregate_for_challenger(
        self,
        pattern_result: FileAnalysisPatternResult,
        citadel_context: dict,
    ) -> PatternFactsForChallenger:
        """Transform patterns into Challenger validation facts."""
        ...

    def _associate_patterns_with_paragraphs(
        self,
        matches: list[AnalysisPatternMatchSDK],
        paragraphs: list[dict],
    ) -> dict[str, list[AnalysisPatternMatchSDK]]:
        """Group pattern matches by containing paragraph (via line ranges)."""
        ...

    def _extract_variable_frequency(
        self,
        data_flow_matches: list[AnalysisPatternMatchSDK],
    ) -> dict[str, int]:
        """Count variable occurrences across data flow patterns."""
        ...

    def _compute_paragraph_complexity(
        self,
        para_matches: list[AnalysisPatternMatchSDK],
    ) -> str:
        """Classify paragraph as simple/moderate/complex based on pattern density."""
        ...

    def _generate_documentation_hint(
        self,
        paragraph_name: str,
        para_matches: dict[str, list[AnalysisPatternMatchSDK]],
    ) -> list[str]:
        """Generate natural language documentation hints for a paragraph."""
        ...
```

### 5.2 Aggregation Rules

#### Data Flow Aggregation

| Pattern Name | Captured Groups | Aggregation |
|--------------|-----------------|-------------|
| `move_simple` | [source, target] | Track target as "written", source as "read" |
| `move_corresponding` | [source, target] | Track both as group moves |
| `compute_expression` | [target, expression] | Track target as "computed" |
| `add_subtract` | [var1, var2, result] | Track result as "modified" |
| `initialize_statement` | [target] | Track target as "initialized" |

#### Control Flow Aggregation

| Pattern Name | Captured Groups | Aggregation |
|--------------|-----------------|-------------|
| `if_perform` | [paragraph] | Increment decision_points, add to performs |
| `evaluate_when` | [subject] | Increment decision_points |
| `perform_until` | [paragraph, condition] | Mark has_loop = True |
| `go_to` | [target] | Mark has_go_to = True |

#### Error Handling Aggregation

| Pattern Name | Captured Groups | Aggregation |
|--------------|-----------------|-------------|
| `file_status_check` | [file_status_var] | Add to file_status_checks |
| `on_exception` | [] | Increment exception_handlers |
| `invalid_key` | [] | Add "invalid_key" to error_types |
| `sqlcode_check` | [sqlcode] | Add "db2_sqlcode" to error_types |

### 5.3 Hint Generation Templates

```python
HINT_TEMPLATES = {
    "data_flow_heavy": (
        "This paragraph modifies {write_count} variables ({top_writes}). "
        "Document what transformations are applied and why."
    ),
    "control_flow_complex": (
        "Contains {decision_count} decision points. "
        "Explain the business logic that determines each branch."
    ),
    "error_handling": (
        "Handles {error_types} errors. "
        "Document the recovery/abend behavior for each error condition."
    ),
    "loop_processing": (
        "Loops over records using PERFORM {loop_type}. "
        "Describe the termination condition and what happens each iteration."
    ),
    "simple_delegation": (
        "Primarily delegates to {called_paragraphs}. "
        "Explain the orchestration purpose and sequencing."
    ),
}
```

---

## 6. Agent Prompt Integration

### 6.1 Scribe System Prompt Additions

Add to the system prompt after the "## Core Principles" section:

```
## Analysis Pattern Insights

When pattern_insights is provided, use these to ensure complete documentation:

1. **File Summary**: The key_variables and complexity_indicators help you
   understand what this program manipulates. Reference these in Purpose.

2. **Paragraph Hints**: Each paragraph has specific documentation guidance.
   Address ALL hints provided - they indicate important logic detected by
   static analysis that must be documented.

3. **Critical Patterns**: These are specific operations that MUST be
   documented. If a COMPUTE is marked critical, explain what it calculates
   and why.

4. **Variables**: When a paragraph reads from or writes to specific variables,
   mention these in your paragraph documentation with their business meaning.
```

### 6.2 Scribe User Prompt Additions

In `_build_user_prompt()`, add after the citadel_outline section:

```python
# Pattern insights for documentation guidance
if input_data.pattern_insights:
    parts.append("## Analysis Pattern Insights")
    parts.append("")

    # File-level summary
    summary = input_data.pattern_insights.get("file_summary", {})
    if summary.get("key_variables"):
        vars_list = ", ".join(summary["key_variables"][:10])
        parts.append(f"**Key Variables**: {vars_list}")
    if summary.get("complexity_indicators"):
        indicators = summary["complexity_indicators"]
        parts.append(f"**Complexity**: {indicators}")
    parts.append("")

    # Per-paragraph hints
    para_hints = input_data.pattern_insights.get("paragraph_hints", [])
    if para_hints:
        parts.append("### Paragraph Documentation Hints")
        parts.append("")
        for hint_item in para_hints[:20]:  # Limit to 20 paragraphs
            para_name = hint_item.get("paragraph_name", "?")
            hints = hint_item.get("hints", [])
            parts.append(f"**{para_name}**:")
            for hint in hints[:4]:  # Max 4 hints per paragraph
                parts.append(f"  - {hint}")
        parts.append("")

    # Critical patterns
    critical = input_data.pattern_insights.get("critical_patterns", [])
    if critical:
        parts.append("### Critical Patterns (MUST Document)")
        for pattern in critical[:10]:
            parts.append(f"- {pattern}")
        parts.append("")
```

### 6.3 Challenger System Prompt Additions

Add to the system prompt after the "## Assessment Levels" section:

```
## Pattern-Based Validation

When pattern_facts is provided, use these ground-truth facts for validation:

1. **Paragraph Facts**: Compare documented data_flow/control_flow against
   actual counts. If documentation says "modifies 2 variables" but facts
   show 8 data_flow operations, ask about the discrepancy.

2. **Validation Cues**: These are specific checks derived from static
   analysis. Use them to identify potential gaps in documentation.

3. **Coverage Expectations**: If a paragraph has significant pattern
   matches but the documentation is terse, flag it as incomplete.

Generate VERIFICATION questions when documentation contradicts facts.
Generate COMPLETENESS questions when documentation omits detected patterns.
```

### 6.4 Challenger User Prompt Additions

In `_build_user_prompt()`, add after the citadel_context section:

```python
# Pattern facts for validation
if input_data.pattern_facts:
    parts.append("## Analysis Pattern Facts (Ground Truth)")
    parts.append("")
    parts.append("Cross-reference documentation against these static analysis facts:")
    parts.append("")

    # Per-paragraph facts
    para_facts = input_data.pattern_facts.get("paragraph_facts", [])
    if para_facts:
        for fact in para_facts[:20]:
            para = fact.get("paragraph_name", "?")
            df_count = fact.get("data_flow_count", 0)
            cf_count = fact.get("control_flow_count", 0)
            eh_count = fact.get("error_handling_count", 0)
            parts.append(
                f"- **{para}**: {df_count} data ops, {cf_count} control ops, "
                f"{eh_count} error handlers"
            )
            key_ops = fact.get("key_operations", [])
            if key_ops:
                ops_str = "; ".join(key_ops[:3])
                parts.append(f"  Key operations: {ops_str}")
        parts.append("")

    # Validation cues
    cues = input_data.pattern_facts.get("validation_cues", [])
    if cues:
        parts.append("### Validation Checkpoints")
        for cue in cues[:10]:
            parts.append(f"- {cue}")
        parts.append("")
```

---

## 7. Example Transformations

### 7.1 Raw Pattern Input (truncated)

```python
{
  "data_flow": [
    {"name": "move_simple", "captured": ["WS-CUSTOMER-ID", "OUT-CUST-ID"], "line": 150},
    {"name": "move_simple", "captured": ["WS-ACCOUNT-NUM", "OUT-ACCT-NUM"], "line": 151},
    {"name": "move_simple", "captured": ["WS-BALANCE", "OUT-BALANCE"], "line": 152},
    {"name": "compute_expression", "captured": ["WS-NEW-BALANCE", "WS-BALANCE + WS-AMOUNT"], "line": 175},
    # ... 200 more matches ...
  ],
  "control_flow": [
    {"name": "if_perform", "captured": ["3100-PROCESS-RECORD"], "line": 200},
    {"name": "if_perform", "captured": ["9000-ERROR-HANDLER"], "line": 210},
    {"name": "perform_until", "captured": ["2000-READ-LOOP", "WS-EOF = 'Y'"], "line": 195},
    # ... 50 more matches ...
  ],
  "error_handling": [
    {"name": "file_status_check", "captured": ["INPUT-FILE-STATUS"], "line": 180},
    {"name": "file_status_check", "captured": ["OUTPUT-FILE-STATUS"], "line": 220},
    # ... 20 more matches ...
  ]
}
```

### 7.2 Transformed Scribe Output

```python
PatternInsightsForScribe(
    file_summary={
        "key_variables": [
            "WS-BALANCE", "WS-CUSTOMER-ID", "WS-ACCOUNT-NUM",
            "WS-AMOUNT", "WS-NEW-BALANCE", "WS-EOF"
        ],
        "complexity_indicators": {
            "total_data_ops": 203,
            "decision_points": 52,
            "has_loops": True,
            "error_coverage": "good"  # 2+ file status checks
        }
    },
    paragraph_hints=[
        {
            "paragraph_name": "2000-PROCESS-DATA",
            "hints": [
                "Modifies WS-NEW-BALANCE via COMPUTE. Document the calculation logic.",
                "Contains loop over records. Explain termination condition WS-EOF.",
                "Checks INPUT-FILE-STATUS. Describe error handling behavior."
            ],
            "variables": {
                "read": ["WS-BALANCE", "WS-AMOUNT", "WS-CUSTOMER-ID"],
                "write": ["WS-NEW-BALANCE", "OUT-BALANCE", "OUT-CUST-ID"]
            },
            "complexity": "complex"
        },
        # ... more paragraphs ...
    ],
    critical_patterns=[
        "Document COMPUTE WS-NEW-BALANCE at line 175 (balance calculation)",
        "Document loop termination: WS-EOF = 'Y' at line 195"
    ]
)
```

### 7.3 Transformed Challenger Output

```python
PatternFactsForChallenger(
    paragraph_facts=[
        {
            "paragraph_name": "2000-PROCESS-DATA",
            "data_flow_count": 45,
            "control_flow_count": 12,
            "error_handling_count": 2,
            "key_operations": [
                "COMPUTE WS-NEW-BALANCE",
                "MOVE to OUT-* fields",
                "FILE-STATUS checks for INPUT-FILE"
            ]
        },
        # ... more paragraphs ...
    ],
    validation_cues=[
        "2000-PROCESS-DATA has 45 data flow ops - verify paragraph describes data transformations",
        "ERROR-HANDLER paragraph handles 2 file statuses - verify both are documented",
        "Main loop at 195 uses WS-EOF - verify termination condition documented"
    ],
    expected_coverage={
        "2000-PROCESS-DATA": 3,  # Expect 3+ data_flow mentions
        "9000-ERROR-HANDLER": 2,  # Expect 2+ error handling mentions
    }
)
```

---

## 8. Implementation Plan

### Phase 1: Core Aggregator (Week 1)

1. Create `PatternAggregator` class in `war_rig/analysis/pattern_aggregator.py`
2. Implement `_associate_patterns_with_paragraphs()` using line ranges
3. Implement `aggregate_for_scribe()` and `aggregate_for_challenger()`
4. Add unit tests for aggregation logic

### Phase 2: Model Updates (Week 1)

1. Add `PatternInsightsForScribe` dataclass to `war_rig/models/`
2. Add `PatternFactsForChallenger` dataclass to `war_rig/models/`
3. Extend `ScribeInput` with `pattern_insights` field
4. Extend `ChallengerInput` with `pattern_facts` field

### Phase 3: Worker Integration (Week 2)

1. Update `ScribeWorker._get_citadel_context()` to also call `get_analysis_patterns()`
2. Add `PatternAggregator` instantiation in workers
3. Pass aggregated insights to agent inputs
4. Update `ChallengerWorker` similarly

### Phase 4: Prompt Engineering (Week 2)

1. Update Scribe system prompt with pattern guidance
2. Update Scribe user prompt builder
3. Update Challenger system prompt with validation guidance
4. Update Challenger user prompt builder

### Phase 5: Testing & Tuning (Week 3)

1. Integration tests with real COBOL files
2. Token budget validation (stay under 4000 tokens)
3. Quality assessment: do hints improve documentation?
4. Adjust aggregation thresholds based on results

---

## 9. Risks and Mitigations

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Token budget overflow | Medium | High | Hard cap on hints, aggressive filtering |
| LLM ignores hints | Medium | Medium | A/B test with/without, iterate on prompts |
| Pattern noise | Low | Medium | Quality threshold on pattern matches |
| Performance overhead | Low | Low | Cache aggregation results per file |
| Citadel unavailable | Low | Low | Graceful degradation (proceed without) |

---

## 10. Success Metrics

1. **Coverage improvement**: % of paragraphs with data_flow documentation increases
2. **Challenger efficiency**: Fewer COMPLETENESS questions post-integration
3. **Token efficiency**: Pattern insights add <4000 tokens on average
4. **Quality correlation**: High complexity paragraphs get longer documentation

---

## 11. Open Questions

1. **Caching**: Should we cache aggregated insights per-file in a dedicated store,
   or recompute on each ticket? Trade-off: memory vs. CPU.

2. **Cross-file patterns**: Some data flows span multiple programs (via CALL).
   Should we include cross-file hints when the dependency graph is available?

3. **Pattern priority**: Which patterns are most valuable? Should we weight
   error_handling higher than data_flow for hint generation?

4. **Iteration behavior**: On cycle 2+, should we reduce hints to only patterns
   not yet documented, or always show the full set?

---

## 12. Appendix: Full Pattern Categories

### A. Data Flow Patterns (COBOL)

| Pattern | Description | Captured |
|---------|-------------|----------|
| `move_simple` | Simple MOVE | source, target |
| `move_corresponding` | MOVE CORRESPONDING | source, target |
| `compute_expression` | COMPUTE | target, expression |
| `add_statement` | ADD | sources, target |
| `subtract_statement` | SUBTRACT | sources, target |
| `multiply_statement` | MULTIPLY | sources, target |
| `divide_statement` | DIVIDE | sources, target |
| `initialize_statement` | INITIALIZE | target |
| `set_statement` | SET | target, value |
| `string_unstring` | STRING/UNSTRING | source, target |
| `inspect_statement` | INSPECT | target |

### B. Control Flow Patterns (COBOL)

| Pattern | Description | Captured |
|---------|-------------|----------|
| `if_perform` | IF ... PERFORM | paragraph |
| `if_call` | IF ... CALL | program |
| `evaluate_when` | EVALUATE ... WHEN | subject |
| `perform_simple` | PERFORM paragraph | paragraph |
| `perform_until` | PERFORM ... UNTIL | paragraph, condition |
| `perform_varying` | PERFORM ... VARYING | paragraph, iterator |
| `perform_times` | PERFORM ... TIMES | paragraph, count |
| `go_to` | GO TO | target |
| `stop_run` | STOP RUN | exit_code |
| `exit_paragraph` | EXIT | |

### C. Error Handling Patterns (COBOL)

| Pattern | Description | Captured |
|---------|-------------|----------|
| `file_status_check` | IF FILE-STATUS | status_var |
| `on_exception` | ON EXCEPTION | |
| `not_on_exception` | NOT ON EXCEPTION | |
| `invalid_key` | INVALID KEY | |
| `at_end` | AT END | |
| `sqlcode_check` | IF SQLCODE | sqlcode |
| `abend_call` | CALL ABEND routine | abend_code |
| `return_code` | MOVE to RETURN-CODE | value |

---

## 13. References

- Citadel SDK: `/Users/andraslferenczi/war_rig/citadel/src/citadel/sdk.py`
- ScribeInput: `/Users/andraslferenczi/war_rig/war_rig/agents/scribe.py`
- ChallengerInput: `/Users/andraslferenczi/war_rig/war_rig/agents/challenger.py`
- ScribeWorker: `/Users/andraslferenczi/war_rig/war_rig/workers/scribe_pool.py`
- ChallengerWorker: `/Users/andraslferenczi/war_rig/war_rig/workers/challenger_pool.py`
