"""Prompt templates for the three-pass summarization pipeline.

When a COBOL file exceeds the practical context window for a single Scribe
pass, the summarization pipeline compresses it through three LLM passes:

- **Pass 1** (Bundle Summary): structured extraction per paragraph bundle.
- **Pass 2** (Coherence Merge): groups of bundle summaries merged into
  mid-level segment summaries.
- **Pass 3** (Root Summary): all segments merged into a canonical file-level
  summary with verifiable claims.

Plus a validation pass where claims are verified against evidence.

All user prompt templates use :meth:`str.format` substitution.  The provider
layer expects simple ``Message(role, content)`` pairs, so each constant here
is a plain string ready for that interface.
"""

from __future__ import annotations

# ---------------------------------------------------------------------------
# Pass 1: Bundle summary extraction
# ---------------------------------------------------------------------------

BUNDLE_SUMMARY_SYSTEM_PROMPT: str = (
    "You are a mainframe code analysis specialist. Extract structured "
    "summaries from COBOL paragraph documentation bundles. Preserve "
    "verbatim: all field names, paragraph names, PERFORM targets, and "
    "numeric literals with business significance. Never paraphrase "
    "technical identifiers."
)

BUNDLE_SUMMARY_USER_PROMPT_TEMPLATE: str = """\
Analyze the following COBOL paragraph bundle and produce a structured \
JSON summary.

**Bundle ID:** {bundle_id}
**Paragraphs:** {paragraph_names}

### Documentation Template (JSON)

```json
{template_json}
```

### Source Excerpt

```cobol
{source_excerpt}
```

### Instructions

1. Read the documentation template and source excerpt carefully.
2. Identify every data item that is read, written, or transformed.
3. List all PERFORM targets invoked by these paragraphs and all \
paragraphs that call into this bundle.
4. Describe each conditional branch with its business meaning.
5. Note any anomalies (dead code, unreachable paths, suspicious \
patterns).

Respond with **only** a JSON object matching this schema — no \
commentary, no markdown fences:

{{
  "bundle_id": "{bundle_id}",
  "paragraph_names": [...],
  "functional_summary": "Concise description of what this bundle does",
  "data_items_read": ["FIELD-A", "FIELD-B", ...],
  "data_items_written": ["FIELD-C", ...],
  "data_items_transformed": ["FIELD-D", ...],
  "perform_calls": ["TARGET-PARA-1", ...],
  "perform_callers": ["CALLER-PARA-1", ...],
  "conditional_branches": [
    {{
      "condition": "IF WS-STATUS = '00'",
      "business_meaning": "Successful database read",
      "outcome_true": "Continue processing",
      "outcome_false": "Branch to error handler"
    }}
  ],
  "anomalies": ["Description of anomaly if any"]
}}"""

# ---------------------------------------------------------------------------
# Pass 2: Coherence merging
# ---------------------------------------------------------------------------

COHERENCE_MERGE_SYSTEM_PROMPT: str = (
    "You are a mainframe documentation specialist performing coherence "
    "merging. Actively resolve coreferences (identify same data items "
    "across bundles), link argument threading (connect validation "
    "routines to the calculations they protect), and flag "
    "contradictions/redundancies. This is NOT concatenation."
)

COHERENCE_MERGE_USER_PROMPT_TEMPLATE: str = """\
Merge the following bundle summaries into a single coherent segment \
summary.

**Segment ID:** {segment_id}

### Bundle Summaries

```json
{bundle_summaries_json}
```

### Instructions

1. Identify data items that appear across multiple bundles under the \
same or aliased names — resolve them as coreferences.
2. Trace argument threading: connect validation routines to the \
calculations they protect, and link error handlers to the operations \
they guard.
3. Build a data-flow graph showing how data moves between paragraphs.
4. Build a PERFORM graph showing caller/callee relationships.
5. Flag any contradictions (conflicting descriptions of the same item) \
and redundancies (duplicate information across bundles).

Respond with **only** a JSON object matching this schema — no \
commentary, no markdown fences:

{{
  "segment_id": "{segment_id}",
  "bundle_ids": ["bundle-id-1", "bundle-id-2", ...],
  "functional_area": "Short name for the functional area",
  "summary": "Coherent description of this segment's purpose and behavior",
  "data_flows": [
    {{
      "source": "SOURCE-PARA",
      "target": "TARGET-PARA",
      "data_item": "WS-FIELD-NAME",
      "description": "How the data moves and is transformed"
    }}
  ],
  "perform_graph": [
    {{
      "caller": "CALLING-PARA",
      "callee": "CALLED-PARA",
      "purpose": "Why the call is made"
    }}
  ],
  "resolved_coreferences": [
    "WS-STATUS in bundle-1 is the same as WS-RETURN-CODE in bundle-2"
  ],
  "flagged_contradictions": [
    "Bundle-1 says X but bundle-3 says Y about the same item"
  ],
  "flagged_redundancies": [
    "Bundles 2 and 4 both describe the same validation logic"
  ]
}}"""

# ---------------------------------------------------------------------------
# Pass 3: Root (file-level) summary
# ---------------------------------------------------------------------------

ROOT_SUMMARY_SYSTEM_PROMPT: str = (
    "You are a mainframe documentation specialist creating a canonical "
    "file-level summary. Synthesize all segment summaries into a single "
    "coherent view. Generate verifiable claims \u2014 concrete, checkable "
    "assertions about the program."
)

ROOT_SUMMARY_USER_PROMPT_TEMPLATE: str = """\
Synthesize the following segment summaries into a file-level summary \
for the program.

**Program ID:** {program_id}
**File Name:** {file_name}

### Segment Summaries

```json
{segment_summaries_json}
```

### Instructions

1. Describe the overall business function of this program.
2. Summarize the primary data flows across all segments.
3. Produce a call graph summary covering the major PERFORM paths.
4. Identify risk areas (complex logic, error-prone patterns, missing \
error handling).
5. Note technical debt items (dead code, duplicated logic, deprecated \
patterns).
6. List migration considerations (features that require special \
attention during modernization).
7. Generate verifiable claims: concrete, checkable assertions about \
the program. Each claim must have a unique ID (C-001, C-002, ...), a \
category, and references to the bundle IDs that provide evidence.

Respond with **only** a JSON object matching this schema — no \
commentary, no markdown fences:

{{
  "business_function": "What this program does in business terms",
  "primary_data_flows": [
    "Data flow description 1",
    "Data flow description 2"
  ],
  "call_graph_summary": [
    "MAIN-PARA -> INIT-PARA -> VALIDATE-PARA",
    "MAIN-PARA -> PROCESS-PARA -> CALC-PARA"
  ],
  "risk_areas": [
    "Description of risk area"
  ],
  "technical_debt": [
    "Description of technical debt item"
  ],
  "migration_considerations": [
    "Description of migration consideration"
  ],
  "claims": [
    {{
      "claim_id": "C-001",
      "claim": "A concrete, verifiable assertion about the program",
      "category": "PERFORM_RELATIONSHIP|DATA_FLOW|IO_OPERATION|EXTERNAL_CALL|BUSINESS_RULE|ERROR_HANDLING|DEAD_CODE|OTHER",
      "evidence_bundle_ids": ["bundle-id-1", "bundle-id-2"]
    }}
  ]
}}"""

# ---------------------------------------------------------------------------
# Claim verification (validation pass)
# ---------------------------------------------------------------------------

CLAIM_VERIFICATION_SYSTEM_PROMPT: str = (
    "You are a validation specialist checking claims against source "
    "evidence. For each claim, determine if it is CONFIRMED, REFUTED, "
    "or UNVERIFIABLE based on the evidence provided."
)

CLAIM_VERIFICATION_USER_PROMPT_TEMPLATE: str = """\
Verify the following {claim_count} claim(s) against the provided evidence.

### Claims and Evidence

{claims_and_evidence}

### Instructions

For each claim, compare the assertion against the bundle summaries and \
source excerpts provided as evidence. Determine the verdict:

- **CONFIRMED**: The evidence clearly supports the claim.
- **REFUTED**: The evidence contradicts the claim.
- **UNVERIFIABLE**: The available evidence is insufficient to confirm \
or refute the claim.

Respond with **only** a JSON array — no commentary, no markdown fences:

[
  {{
    "claim_id": "C-001",
    "verdict": "CONFIRMED",
    "notes": "Explanation of why this verdict was reached"
  }}
]"""

# ---------------------------------------------------------------------------
# Context injection for Scribe prompts
# ---------------------------------------------------------------------------

CONTEXT_INJECTION_TEMPLATE: str = """\
## Program-Level Context

The following summary describes the overall structure and purpose of \
this file.
Use it to understand how the paragraph(s) you are documenting fit \
into the bigger picture.

{summary_text}"""
