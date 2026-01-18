# Formatting Error Recovery Architecture

**Ticket:** war_rig-jnxi
**Status:** Design
**Author:** Architecture Review
**Date:** 2026-01-18

---

## 1. Executive Summary

This document describes the architecture for intelligent formatting error recovery in War Rig. When Scribe or Challenger agents produce malformed output that fails Pydantic validation, the system captures the raw output and routes it to Super-Scribe for correction rather than discarding it entirely.

### Problem Statement

Currently, when an LLM produces output with formatting errors (invalid enum values, malformed JSON, wrong field types), the entire response is discarded and the ticket fails:

```
ValidationError: 1 validation error for DocumentationTemplate
purpose.program_type
  Input should be 'BATCH', 'ONLINE_CICS', 'SUBROUTINE' or 'UTILITY' [type=enum, input_value='ONLINE', input_type=str]
```

This wastes:
- The semantic content in the malformed response (business rules, IO analysis, etc.)
- API costs from the failed LLM call
- Time spent waiting for the response

### Proposed Solution

Introduce a `FORMATTING_FIX` ticket type that captures malformed output and routes it to Super-Scribe for correction. Super-Scribe, using a stronger model (Claude Opus), can:
1. Parse the malformed JSON
2. Identify validation errors
3. Correct specific fields while preserving semantic content
4. Return valid output

---

## 2. Error Detection Points

### 2.1 Current Validation Locations

Validation errors occur at these points in the codebase:

| Location | File | Method | Validates |
|----------|------|--------|-----------|
| Scribe Response Parse | `war_rig/agents/scribe.py` | `_parse_response()` | `DocumentationTemplate` |
| Challenger Response Parse | `war_rig/agents/challenger.py` | `_parse_response()` | `ChallengerQuestion`, `ChallengerAssessment` |
| Template Load | `war_rig/workers/scribe_pool.py` | `_load_previous_template()` | `DocumentationTemplate` |
| Ticket State Load | `war_rig/workers/challenger_pool.py` | `_load_documentation_state()` | `DocumentationTemplate` |

### 2.2 Current Error Handling Flow

```
LLM Response
    |
    v
JSON Extraction (regex)
    |
    +--> No JSON found --> Return error output (success=False)
    |
    v
json.loads()
    |
    +--> JSONDecodeError --> Attempt repair --> Retry parse
    |                                              |
    |                                              +--> Still fails --> Return error
    |
    v
Pydantic model_validate()
    |
    +--> ValidationError --> Return error output (success=False)  <-- CAPTURE POINT
    |
    v
Return success output
```

### 2.3 Common Validation Errors

Based on the template schemas in `war_rig/models/templates.py`:

| Field | Valid Values | Common Errors |
|-------|--------------|---------------|
| `purpose.program_type` | `BATCH`, `ONLINE_CICS`, `SUBROUTINE`, `UTILITY` | `"ONLINE"`, `"CICS"`, `"BATCH_PROGRAM"` |
| `header.file_type` | `COBOL`, `PLI`, `JCL`, `COPYBOOK`, `PROC`, `BMS`, `OTHER` | `"CBL"`, `"COBOL_PROGRAM"` |
| `inputs[].io_type` | `FILE_SEQUENTIAL`, `FILE_VSAM`, `DB2_TABLE`, etc. | `"FILE"`, `"VSAM"`, `"INPUT_FILE"` |
| `called_programs[].call_type` | `STATIC_CALL`, `DYNAMIC_CALL`, `CICS_LINK`, `CICS_XCTL` | `"CALL"`, `"STATIC"` |
| `copybooks_used[].location` | `WORKING_STORAGE`, `LINKAGE`, `FILE_SECTION`, etc. | `"WORKING-STORAGE"`, `"WS"` |
| `paragraphs[].citation` | `tuple[int, int]` (2 elements) | `[100]` (1 element), `100` (int) |
| `*.citation` (various) | `list[int]` or `int` depending on field | Mixed types |

---

## 3. New Ticket Type: FORMATTING_FIX

### 3.1 Ticket Type Definition

Add to `war_rig/beads.py`:

```python
class TicketType(str, Enum):
    """Ticket types for Program Manager workflow."""

    DOCUMENTATION = "documentation"
    VALIDATION = "validation"
    CLARIFICATION = "clarification"
    CHROME = "chrome"
    HOLISTIC_REVIEW = "holistic_review"
    FORMATTING_FIX = "formatting_fix"  # NEW: Malformed output correction
```

### 3.2 Ticket Metadata Schema

The `FORMATTING_FIX` ticket carries all information needed for correction:

```python
@dataclass
class FormattingFixMetadata:
    """Metadata for FORMATTING_FIX tickets."""

    # Original context
    source_ticket_id: str           # Parent ticket that generated malformed output
    source_ticket_type: TicketType  # DOCUMENTATION, CLARIFICATION, etc.
    file_name: str                  # Source file being documented
    program_id: str | None          # Program identifier

    # The malformed output
    raw_response: str               # Complete LLM response text
    extracted_json: str | None      # JSON portion if extraction succeeded

    # Validation error details
    validation_errors: list[ValidationErrorDetail]

    # Original input context (for Super-Scribe reference)
    source_code_hash: str           # Hash to verify source hasn't changed
    source_code_lines: int          # Line count for context

    # Processing history
    original_worker_id: str         # Worker that produced malformed output
    original_model: str             # Model that generated the response
    retry_count: int                # Number of formatting fix attempts


@dataclass
class ValidationErrorDetail:
    """Details about a specific validation error."""

    field_path: str         # e.g., "purpose.program_type"
    error_type: str         # e.g., "enum", "type_error", "missing"
    error_message: str      # Human-readable error
    input_value: Any        # What the LLM provided
    expected: str | None    # What was expected (for enums, types)
```

### 3.3 Ticket Priority

`FORMATTING_FIX` tickets should have elevated priority since:
1. They represent work already invested (LLM call completed)
2. Fixing is typically cheaper than regenerating
3. Blocking downstream validation

```python
# In create_formatting_fix_ticket():
priority = BeadsPriority.HIGH  # Process before new DOCUMENTATION tickets
```

---

## 4. Error Capture Implementation

### 4.1 Enhanced Error Output

Modify `AgentOutput` base class to capture malformed responses:

```python
# In war_rig/agents/base.py

class AgentOutput(BaseModel):
    """Base class for agent outputs."""

    success: bool = True
    error: str | None = None

    # NEW: Capture malformed output for recovery
    raw_response: str | None = Field(
        default=None,
        description="Raw LLM response when validation failed",
    )
    validation_errors: list[dict[str, Any]] = Field(
        default_factory=list,
        description="Pydantic validation error details",
    )
    recoverable: bool = Field(
        default=False,
        description="Whether this failure can be recovered via FORMATTING_FIX",
    )
```

### 4.2 Modified Parse Logic in ScribeAgent

```python
# In war_rig/agents/scribe.py

def _parse_response(self, response: str, input_data: ScribeInput) -> ScribeOutput:
    """Parse the LLM response into ScribeOutput."""
    try:
        # Try to extract JSON from response
        json_match = re.search(r"\{[\s\S]*\}", response)
        if not json_match:
            return ScribeOutput(
                success=False,
                error="No JSON object found in response",
                raw_response=response,
                recoverable=False,  # Can't fix if no JSON structure
            )

        json_str = json_match.group()

        # First try parsing as-is
        try:
            data = json.loads(json_str)
        except json.JSONDecodeError as e:
            # Try to repair common JSON errors
            repaired = self._repair_json(json_str)
            try:
                data = json.loads(repaired)
            except json.JSONDecodeError:
                return ScribeOutput(
                    success=False,
                    error=f"JSON parse error: {e}",
                    raw_response=response,
                    recoverable=True,  # JSON repair might work with stronger model
                )

        # Parse template - THIS IS WHERE VALIDATION ERRORS OCCUR
        template = None
        if "template" in data:
            try:
                template = DocumentationTemplate.model_validate(data["template"])
            except ValidationError as e:
                # CAPTURE FOR RECOVERY
                validation_errors = [
                    {
                        "field": ".".join(str(x) for x in err["loc"]),
                        "type": err["type"],
                        "message": err["msg"],
                        "input": err.get("input"),
                    }
                    for err in e.errors()
                ]
                return ScribeOutput(
                    success=False,
                    error=f"Template validation failed: {e.error_count()} errors",
                    raw_response=response,
                    validation_errors=validation_errors,
                    recoverable=True,  # Pydantic errors are fixable
                )

        # ... rest of parsing ...

    except Exception as e:
        logger.error(f"Failed to parse Scribe response: {e}")
        return ScribeOutput(
            success=False,
            error=f"Failed to parse response: {e}",
            raw_response=response,
            recoverable=False,  # Unknown errors not recoverable
        )
```

### 4.3 Modified Worker Error Handling

In `war_rig/workers/scribe_pool.py`, modify `_process_ticket()`:

```python
async def _process_ticket(self, ticket: ProgramManagerTicket) -> None:
    """Process a claimed ticket."""
    self._update_state(WorkerState.PROCESSING, ticket.ticket_id)

    try:
        # ... existing processing logic ...

        if result.success:
            # ... existing success handling ...
        else:
            # Check if this is a recoverable formatting error
            if result.recoverable and result.raw_response:
                # Create FORMATTING_FIX ticket instead of retrying with same model
                formatting_ticket = self._create_formatting_fix_ticket(
                    source_ticket=ticket,
                    result=result,
                )

                if formatting_ticket:
                    logger.info(
                        f"Worker {self.worker_id}: Created FORMATTING_FIX ticket "
                        f"{formatting_ticket.ticket_id} for {ticket.file_name}"
                    )
                    # Mark original ticket as BLOCKED pending formatting fix
                    self.beads_client.update_ticket_state(
                        ticket.ticket_id,
                        TicketState.BLOCKED,
                        reason=f"Pending formatting fix: {formatting_ticket.ticket_id}",
                        metadata_updates={
                            "formatting_fix_ticket": formatting_ticket.ticket_id,
                        },
                    )
                    self._status.tickets_processed += 1  # Count as processed
                    return

            # Fall back to existing retry logic if not recoverable
            # ... existing failure handling ...
```

---

## 5. Super-Scribe Formatting Fix Workflow

### 5.1 Super-Scribe Agent Enhancement

Create dedicated formatting fix capability in Super-Scribe:

```python
# In war_rig/agents/super_scribe.py (new file)

class FormattingFixInput(AgentInput):
    """Input for Super-Scribe formatting fix."""

    raw_response: str = Field(
        ...,
        description="The malformed LLM response to fix",
    )
    validation_errors: list[dict[str, Any]] = Field(
        ...,
        description="Pydantic validation errors to address",
    )
    target_schema: str = Field(
        ...,
        description="Name of the target schema (DocumentationTemplate, etc.)",
    )
    file_name: str = Field(
        ...,
        description="Source file for context",
    )
    # Optional: Include source code excerpt for semantic verification
    source_excerpt: str | None = Field(
        default=None,
        description="Relevant source code lines for verification",
    )


class FormattingFixOutput(AgentOutput):
    """Output from Super-Scribe formatting fix."""

    corrected_json: str | None = Field(
        default=None,
        description="The corrected JSON string",
    )
    corrections_made: list[dict[str, Any]] = Field(
        default_factory=list,
        description="List of corrections applied",
    )
    template: DocumentationTemplate | None = Field(
        default=None,
        description="Parsed and validated template (if DocumentationTemplate)",
    )


class SuperScribeAgent(BaseAgent[FormattingFixInput, FormattingFixOutput]):
    """Super-Scribe agent for handling difficult cases and formatting fixes.

    Super-Scribe uses a stronger model (Claude Opus) to:
    1. Fix malformed JSON from weaker models
    2. Correct enum/type validation errors
    3. Handle complex edge cases that Sonnet struggles with
    """

    def __init__(
        self,
        config: ScribeConfig,  # Uses elevated model from config.super_scribe_model
        api_config: APIConfig | None = None,
    ):
        super().__init__(config, api_config, name="SuperScribe")

    def _build_system_prompt(self) -> str:
        return """You are Super-Scribe, a specialist in fixing malformed documentation output.

Your task is to correct JSON formatting and validation errors while preserving the semantic content.

## Principles

1. **Preserve semantics**: Never change the meaning of documented information
2. **Fix only what's broken**: Don't rewrite sections that are valid
3. **Use exact valid values**: When fixing enums, use the exact valid option
4. **Maintain structure**: Keep the JSON structure intact

## Common Fixes

### Enum Corrections
- program_type: BATCH, ONLINE_CICS, SUBROUTINE, UTILITY
- file_type: COBOL, PLI, JCL, COPYBOOK, PROC, BMS, OTHER
- io_type: FILE_SEQUENTIAL, FILE_VSAM, DB2_TABLE, IMS_SEGMENT, PARAMETER,
           CICS_COMMAREA, CICS_MAP, CICS_QUEUE, REPORT, RETURN_CODE, OTHER
- call_type: STATIC_CALL, DYNAMIC_CALL, CICS_LINK, CICS_XCTL
- location: WORKING_STORAGE, LINKAGE, FILE_SECTION, LOCAL_STORAGE, OTHER

### Type Corrections
- paragraphs[].citation must be [start_line, end_line] (exactly 2 integers)
- called_programs[].citation must be single integer (not list)
- copybooks_used[].citation must be single integer (not list)
- inputs/outputs/business_rules citation must be list[int]

### JSON Syntax
- Remove trailing commas
- Escape special characters in strings
- Ensure all property names are quoted

## Output Format

Return a JSON object with:
- "corrected_json": The fixed JSON (as a JSON-encoded string)
- "corrections_made": List of {field, original, corrected, reason}

Return ONLY valid JSON."""

    def _build_user_prompt(self, input_data: FormattingFixInput) -> str:
        parts = []

        parts.append(f"## File: {input_data.file_name}")
        parts.append(f"## Target Schema: {input_data.target_schema}")
        parts.append("")

        parts.append("## Validation Errors to Fix")
        for err in input_data.validation_errors:
            parts.append(f"- {err['field']}: {err['message']}")
            if err.get('input'):
                parts.append(f"  Input was: {err['input']}")
        parts.append("")

        parts.append("## Malformed Response to Correct")
        parts.append("```json")
        parts.append(input_data.raw_response)
        parts.append("```")
        parts.append("")

        if input_data.source_excerpt:
            parts.append("## Source Code Context")
            parts.append("```")
            parts.append(input_data.source_excerpt)
            parts.append("```")

        parts.append("")
        parts.append("Fix the validation errors and return corrected JSON.")

        return "\n".join(parts)
```

### 5.2 Super-Scribe Worker Pool

Add `FORMATTING_FIX` to Super-Scribe's compatible ticket types:

```python
# In war_rig/workers/super_scribe_pool.py (new file)

class SuperScribeWorker:
    """Worker that handles FORMATTING_FIX tickets using Super-Scribe."""

    COMPATIBLE_TICKET_TYPES = [
        TicketType.FORMATTING_FIX,
        # Also handles escalated DOCUMENTATION/CLARIFICATION/CHROME
        TicketType.DOCUMENTATION,
        TicketType.CLARIFICATION,
        TicketType.CHROME,
    ]

    async def _process_formatting_fix_ticket(
        self,
        ticket: ProgramManagerTicket,
    ) -> FormattingFixOutput:
        """Process a FORMATTING_FIX ticket."""

        # Extract metadata
        raw_response = ticket.metadata.get("raw_response", "")
        validation_errors = ticket.metadata.get("validation_errors", [])
        target_schema = ticket.metadata.get("target_schema", "DocumentationTemplate")
        source_ticket_id = ticket.metadata.get("source_ticket_id")

        # Build input
        fix_input = FormattingFixInput(
            raw_response=raw_response,
            validation_errors=validation_errors,
            target_schema=target_schema,
            file_name=ticket.file_name,
        )

        # Run Super-Scribe
        output = await self.super_scribe_agent.ainvoke(fix_input)

        if output.success and output.template:
            # Save the corrected template
            self._save_template(ticket.file_name, output.template)

            # Unblock the source ticket if it exists
            if source_ticket_id:
                self._resume_source_ticket(source_ticket_id, output.template)

        return output

    def _resume_source_ticket(
        self,
        source_ticket_id: str,
        corrected_template: DocumentationTemplate,
    ) -> None:
        """Resume the source ticket with corrected output."""

        # Update source ticket metadata with corrected template
        self.beads_client.update_ticket_state(
            source_ticket_id,
            TicketState.COMPLETED,
            reason="Formatting fixed by Super-Scribe",
            metadata_updates={
                "template": corrected_template.model_dump(mode="json"),
                "corrected_by_super_scribe": True,
            },
        )

        # Create VALIDATION ticket for the corrected documentation
        self._create_validation_ticket(source_ticket_id, corrected_template)
```

---

## 6. State Machine for Affected Tickets

### 6.1 Extended State Transitions

```
DOCUMENTATION Ticket (normal flow):
  CREATED --> CLAIMED --> IN_PROGRESS --> COMPLETED --> [VALIDATION created]
                              |
                        (validation error, recoverable)
                              |
                              v
                          BLOCKED --[formatting_fix_ticket]--> FORMATTING_FIX created
                              |
                        (fix completed)
                              |
                              v
                          COMPLETED --> [VALIDATION created]


FORMATTING_FIX Ticket (new):
  CREATED --> CLAIMED --> IN_PROGRESS --> COMPLETED
                              |                |
                        (fix failed)     (source ticket unblocked)
                              |
                              v
                          BLOCKED (manual intervention needed)
```

### 6.2 Ticket Relationships

```
+-------------------+
| DOCUMENTATION     |
| ticket-001        |
| state: BLOCKED    |
+--------+----------+
         |
         | metadata.formatting_fix_ticket
         v
+-------------------+
| FORMATTING_FIX    |
| ticket-002        |
| state: IN_PROGRESS|
+--------+----------+
         |
         | metadata.source_ticket_id
         v
+-------------------+
| DOCUMENTATION     |
| ticket-001        |
| (referenced)      |
+-------------------+
```

### 6.3 State Machine Diagram

```
                    DOCUMENTATION Ticket
                    ===================
                           |
                      [CREATED]
                           |
                     worker claims
                           |
                      [CLAIMED]
                           |
                    processing starts
                           |
                    [IN_PROGRESS]
                           |
            +--------------+---------------+
            |              |               |
         success     recoverable      non-recoverable
            |          error              error
            |              |               |
            v              v               v
       [COMPLETED]    [BLOCKED]      [BLOCKED]
            |              |          (no fix ticket)
            |              |
            |        create FORMATTING_FIX
            |              |
            |              v
            |      FORMATTING_FIX Ticket
            |      ====================
            |              |
            |         [CREATED]
            |              |
            |        Super-Scribe claims
            |              |
            |         [CLAIMED]
            |              |
            |       [IN_PROGRESS]
            |              |
            |       +------+------+
            |       |             |
            |    success       failure
            |       |             |
            |       v             v
            |  [COMPLETED]   [BLOCKED]
            |       |        (escalate to human)
            |       |
            |   unblock source
            |       |
            +<------+
            |
            v
       [COMPLETED]
            |
      create VALIDATION
            |
            v
      VALIDATION Ticket
      =================
           ...
```

---

## 7. Integration with Existing Workflow

### 7.1 TicketOrchestrator Changes

Modify `war_rig/orchestration/ticket_engine.py`:

```python
class TicketOrchestrator:

    async def _run_worker_cycle(self) -> None:
        """Run one cycle with all worker pools."""

        # ... existing Scribe and Challenger pools ...

        # NEW: Add Super-Scribe pool for formatting fixes
        super_scribe_pool = SuperScribeWorkerPool(
            config=self._create_super_scribe_config(),
            beads_client=self.beads_client,
            input_directory=self._input_directory,
            num_workers=self.config.num_super_scribes,
            poll_interval=2.0,
            idle_timeout=30.0,
        )

        await super_scribe_pool.start()

        # Wait for all pools including Super-Scribe
        await asyncio.gather(
            scribe_wait,
            challenger_wait,
            super_scribe_pool.wait(),
            return_exceptions=True,
        )

    async def _run_super_scribe_rescue(self) -> None:
        """Enhanced rescue to include FORMATTING_FIX tickets."""

        # Check for both BLOCKED tickets AND pending FORMATTING_FIX
        formatting_fix_tickets = self.beads_client.get_available_tickets(
            ticket_type=TicketType.FORMATTING_FIX
        )

        if formatting_fix_tickets:
            logger.info(
                f"Processing {len(formatting_fix_tickets)} FORMATTING_FIX tickets"
            )
            # Super-Scribe pool handles these naturally
```

### 7.2 Processing Order

1. **Scribe Pool** processes `DOCUMENTATION`, `CLARIFICATION`, `CHROME`
2. **Challenger Pool** processes `VALIDATION` (waits for Scribe)
3. **Super-Scribe Pool** processes `FORMATTING_FIX` (continuous)
4. When `FORMATTING_FIX` completes, source ticket is unblocked
5. New `VALIDATION` ticket created for corrected output

### 7.3 Configuration Additions

Add to `war_rig/config.py`:

```python
class WarRigConfig(BaseSettings):
    # ... existing config ...

    # Formatting fix configuration
    enable_formatting_recovery: bool = Field(
        default=True,
        description="Enable FORMATTING_FIX ticket creation for recoverable errors",
    )
    max_formatting_fix_attempts: int = Field(
        default=2,
        ge=1,
        le=5,
        description="Maximum attempts to fix formatting before giving up",
    )
    formatting_fix_timeout: int = Field(
        default=120,
        ge=30,
        le=600,
        description="Timeout in seconds for formatting fix attempts",
    )
```

---

## 8. Metrics and Observability

### 8.1 New Metrics to Track

```python
@dataclass
class FormattingRecoveryMetrics:
    """Metrics for formatting error recovery."""

    # Volume
    total_validation_errors: int = 0
    recoverable_errors: int = 0
    formatting_fix_tickets_created: int = 0

    # Success rates
    fixes_successful: int = 0
    fixes_failed: int = 0

    # Efficiency
    tokens_saved: int = 0  # Estimated tokens not regenerated
    api_cost_saved: float = 0.0  # Estimated cost savings

    # Error analysis
    errors_by_field: dict[str, int] = field(default_factory=dict)
    errors_by_type: dict[str, int] = field(default_factory=dict)
```

### 8.2 Logging

```python
# Log validation errors with recovery intent
logger.warning(
    f"Validation error in {ticket.file_name}: {error_count} errors. "
    f"Recoverable: {result.recoverable}. "
    f"Creating FORMATTING_FIX ticket."
)

# Log corrections made
logger.info(
    f"Super-Scribe fixed {len(corrections)} errors in {ticket.file_name}: "
    f"{[c['field'] for c in corrections]}"
)
```

---

## 9. Error Handling and Edge Cases

### 9.1 Non-Recoverable Errors

Some errors cannot be fixed by formatting correction:

| Error Type | Example | Handling |
|------------|---------|----------|
| No JSON in response | LLM returned prose | Retry with formatting_strict |
| Completely malformed JSON | Truncated response | Retry with formatting_strict |
| Missing required sections | No "template" key | Retry with formatting_strict |
| Semantic errors | Wrong file analyzed | Mark as BLOCKED, manual review |

### 9.2 Circular Dependencies

Prevent infinite loops:

```python
# In create_formatting_fix_ticket():
if ticket.metadata.get("is_formatting_fix_retry"):
    logger.error(
        f"FORMATTING_FIX ticket {ticket.ticket_id} failed again. "
        "Manual intervention required."
    )
    return None  # Don't create another fix ticket

# Mark the fix ticket
metadata["is_formatting_fix_retry"] = True
```

### 9.3 Timeout Handling

```python
async def _process_formatting_fix_ticket(self, ticket: ProgramManagerTicket):
    try:
        async with asyncio.timeout(self.config.formatting_fix_timeout):
            output = await self.super_scribe_agent.ainvoke(fix_input)
    except asyncio.TimeoutError:
        logger.error(f"Formatting fix timed out for {ticket.ticket_id}")
        self.beads_client.update_ticket_state(
            ticket.ticket_id,
            TicketState.BLOCKED,
            reason="Formatting fix timed out",
        )
```

---

## 10. Implementation Plan

### Phase 1: Core Infrastructure (1-2 days)

1. Add `FORMATTING_FIX` to `TicketType` enum
2. Extend `AgentOutput` with recovery fields
3. Modify `ScribeAgent._parse_response()` to capture errors
4. Add `create_formatting_fix_ticket()` method to `ScribeWorker`

### Phase 2: Super-Scribe Agent (1-2 days)

1. Create `SuperScribeAgent` with formatting fix capability
2. Build dedicated system prompt for error correction
3. Add validation/testing for common error patterns

### Phase 3: Worker Integration (1-2 days)

1. Create `SuperScribeWorkerPool`
2. Modify `ScribeWorker` to create fix tickets
3. Implement `_resume_source_ticket()` logic
4. Update `TicketOrchestrator` to include Super-Scribe pool

### Phase 4: Testing and Metrics (1 day)

1. Add unit tests for error capture
2. Add integration tests for fix workflow
3. Implement metrics collection
4. Add logging for observability

---

## 11. Open Questions

1. **Should FORMATTING_FIX tickets be created immediately or batched?**
   - Immediate: Lower latency, more API calls
   - Batched: Fewer calls, but delays workflow

2. **Should Super-Scribe always use Opus, or fallback to Sonnet for simple fixes?**
   - Cost vs reliability tradeoff
   - Could use error count/type to decide

3. **What's the maximum raw_response size to store in ticket metadata?**
   - Large responses may exceed storage limits
   - Consider storing in artifact store instead

4. **Should we pre-validate JSON structure before Pydantic validation?**
   - Could catch more errors earlier
   - Adds complexity to parsing logic

---

## Appendix A: Example FORMATTING_FIX Ticket

```json
{
  "ticket_id": "mem-000042",
  "ticket_type": "formatting_fix",
  "state": "created",
  "file_name": "CBACT04C.cbl",
  "program_id": "CBACT04C",
  "cycle_number": 1,
  "metadata": {
    "source_ticket_id": "mem-000015",
    "source_ticket_type": "documentation",
    "target_schema": "DocumentationTemplate",
    "raw_response": "{\"template\": {\"header\": {...}, \"purpose\": {\"program_type\": \"ONLINE\", ...}}}",
    "validation_errors": [
      {
        "field": "purpose.program_type",
        "type": "enum",
        "message": "Input should be 'BATCH', 'ONLINE_CICS', 'SUBROUTINE' or 'UTILITY'",
        "input": "ONLINE"
      }
    ],
    "original_worker_id": "scribe-2",
    "original_model": "anthropic/claude-sonnet-4-20250514",
    "retry_count": 0
  }
}
```

## Appendix B: Super-Scribe Correction Example

**Input (malformed):**
```json
{
  "template": {
    "purpose": {
      "program_type": "ONLINE",
      "summary": "Processes customer transactions"
    },
    "paragraphs": [
      {"paragraph_name": "0000-MAIN", "citation": 100}
    ]
  }
}
```

**Validation Errors:**
- `purpose.program_type`: Input should be 'BATCH', 'ONLINE_CICS', 'SUBROUTINE' or 'UTILITY'
- `paragraphs[0].citation`: Input should be a valid tuple

**Output (corrected):**
```json
{
  "corrected_json": "{\"template\": {\"purpose\": {\"program_type\": \"ONLINE_CICS\", \"summary\": \"Processes customer transactions\"}, \"paragraphs\": [{\"paragraph_name\": \"0000-MAIN\", \"citation\": [100, 150]}]}}",
  "corrections_made": [
    {
      "field": "purpose.program_type",
      "original": "ONLINE",
      "corrected": "ONLINE_CICS",
      "reason": "Mapped partial enum value to valid ONLINE_CICS"
    },
    {
      "field": "paragraphs[0].citation",
      "original": 100,
      "corrected": [100, 150],
      "reason": "Converted single int to tuple [start, end], estimated end"
    }
  ]
}
```
