# Imperator Feedback Loop Improvements - Implementation Tickets

**Created**: 2026-01-22
**Feature**: Imperator Feedback Loop Improvements
**Total Tickets**: 8

## Executive Summary

The Imperator holistic review identified critical quality issues in generated documentation:
- Content largely empty; critical sections (inputs, outputs, data flow, copybooks, SQL/CICS operations) blank
- No cross-reference information between programs
- Redundant copies of program documentation created instead of augmenting existing docs
- No business rules, error handling, or sample JCL documented
- Confidence scores do not match actual documentation quality

This ticket set implements a feedback loop where Imperator quality notes are:
1. Embedded in ticket metadata for DOCUMENTATION and VALIDATION tickets (global context)
2. Explicitly addressed by Scribe in output
3. Validated by Challenger with critical section checks
4. Used to prevent duplicate documentation (augment instead of create)

## Dependency Graph

```
[IMPFB-001: Feedback Metadata Model] ----+
                                         |
[IMPFB-002: PM Feedback Distribution] ---+--> [IMPFB-004: Scribe Feedback Processing]
                                         |
[IMPFB-003: Imperator Feedback Capture] -+
                                              |
                                              v
[IMPFB-005: Critical Section Validation] <----+
                                              |
[IMPFB-006: Challenger Feedback Validation] --+
                                              |
[IMPFB-007: Duplicate Doc Detection] ---------+--> [IMPFB-008: Integration Testing]
```

- Ticket 1 (Metadata Model) is foundational - defines data structures
- Tickets 2, 3 can be developed in parallel after Ticket 1
- Ticket 4 depends on Tickets 1-3 (Scribe needs metadata to process)
- Tickets 5, 6 depend on Ticket 4 (validate what Scribe produces)
- Ticket 7 is independent but should integrate with Ticket 4
- Ticket 8 is integration testing for all components

---

## Ticket 1: Feedback Metadata Model

**ID**: IMPFB-001
**Type**: Feature
**Priority**: High (1)
**Estimated Effort**: Small
**Dependencies**: None

### Summary
Define data structures for storing Imperator quality feedback in ticket metadata, enabling feedback to flow from Imperator through Program Manager to Scribe/Challenger.

### Files to Modify
- `/home/andras/war_rig/war_rig/models/tickets.py`
- `/home/andras/war_rig/war_rig/beads.py`

### Context
Currently, Imperator feedback (quality_notes, chrome tickets) is processed in the orchestrator but not persisted in a structured way that downstream agents can access. We need a standardized metadata schema so feedback flows through the ticket system.

### Requirements

#### Part A: Add Feedback Models (`tickets.py`)

1. **Add `QualityNote` model** (after `DialogueRound` class, around line 340):
   ```python
   class QualityNote(BaseModel):
       """A quality observation from Imperator holistic review.

       Quality notes identify systemic issues across documentation that
       should be addressed in subsequent cycles.
       """

       note_id: str = Field(
           default_factory=lambda: f"QN-{uuid4().hex[:8].upper()}",
           description="Unique identifier for the note",
       )
       category: str = Field(
           description="Category: 'empty_section', 'missing_citation', 'vague_content', "
                       "'no_cross_reference', 'confidence_mismatch', 'redundant_doc'",
       )
       severity: str = Field(
           default="medium",
           description="Severity: 'critical', 'high', 'medium', 'low'",
       )
       description: str = Field(
           description="Human-readable description of the quality issue",
       )
       affected_sections: list[str] = Field(
           default_factory=list,
           description="Template sections affected (e.g., 'inputs', 'outputs', 'data_flow')",
       )
       affected_files: list[str] = Field(
           default_factory=list,
           description="Files affected (empty = all files)",
       )
       guidance: str | None = Field(
           default=None,
           description="Specific guidance for addressing the issue",
       )
       cycle_identified: int = Field(
           default=1,
           description="Cycle when this note was first identified",
       )
   ```

2. **Add `FeedbackContext` model**:
   ```python
   class FeedbackContext(BaseModel):
       """Aggregated feedback context for a documentation cycle.

       Contains all Imperator feedback that should be applied globally
       to documentation and validation tickets.
       """

       quality_notes: list[QualityNote] = Field(
           default_factory=list,
           description="Quality observations from Imperator",
       )
       critical_sections: list[str] = Field(
           default_factory=list,
           description="Sections that MUST be populated (not empty)",
       )
       required_citations: bool = Field(
           default=True,
           description="Whether citations are required for all claims",
       )
       cross_reference_required: bool = Field(
           default=False,
           description="Whether cross-program references must be documented",
       )
       previous_cycle_issues: dict[str, list[str]] = Field(
           default_factory=dict,
           description="File-specific issues from previous cycle",
       )
       augment_existing: bool = Field(
           default=True,
           description="Always augment existing docs instead of creating new",
       )
   ```

#### Part B: Update ProgramManagerTicket (`beads.py`)

3. **Add `feedback_context` to ProgramManagerTicket metadata schema** (around line 160):
   - Document that `metadata["feedback_context"]` can contain serialized `FeedbackContext`
   - Add helper method to extract feedback context:
   ```python
   def get_feedback_context(self) -> FeedbackContext | None:
       """Extract feedback context from metadata if present."""
       from war_rig.models.tickets import FeedbackContext
       ctx_data = self.metadata.get("feedback_context")
       if ctx_data:
           if isinstance(ctx_data, dict):
               return FeedbackContext.model_validate(ctx_data)
           elif isinstance(ctx_data, FeedbackContext):
               return ctx_data
       return None
   ```

### Acceptance Criteria
- [ ] `QualityNote` model defined with all specified fields
- [ ] `FeedbackContext` model defined with all specified fields
- [ ] Both models are JSON-serializable (for ticket metadata storage)
- [ ] `get_feedback_context()` helper works on ProgramManagerTicket
- [ ] Models have appropriate defaults for optional fields
- [ ] Unit tests for model serialization/deserialization

### Technical Notes
- Use Pydantic for validation and serialization
- Keep models lightweight - they're stored in JSON metadata
- Consider adding `model_config = ConfigDict(extra='ignore')` for forward compatibility

---

## Ticket 2: Program Manager Feedback Distribution

**ID**: IMPFB-002
**Type**: Feature
**Priority**: High (1)
**Estimated Effort**: Medium
**Dependencies**: IMPFB-001

### Summary
Modify ProgramManagerAgent and TicketOrchestrator to embed feedback context in DOCUMENTATION and VALIDATION ticket metadata when creating or updating tickets.

### Files to Modify
- `/home/andras/war_rig/war_rig/agents/program_manager.py`
- `/home/andras/war_rig/war_rig/orchestration/ticket_engine.py`

### Context
After Imperator holistic review produces quality_notes, these need to be distributed to all subsequent DOCUMENTATION and VALIDATION tickets so Scribe and Challenger agents can access and act on them.

### Requirements

#### Part A: TicketOrchestrator Updates (`ticket_engine.py`)

1. **Add `_current_feedback_context` instance variable** (in `__init__`, around line 265):
   ```python
   # Feedback context from Imperator (distributed to new tickets)
   self._current_feedback_context: FeedbackContext | None = None
   ```

2. **Update `_handle_imperator_feedback()` method** (around line 1223) to capture and store feedback context:
   ```python
   async def _handle_imperator_feedback(
       self,
       review_output: HolisticReviewOutput,
   ) -> None:
       """Process Imperator feedback and create new tickets.

       Also builds FeedbackContext from review output to embed in
       subsequent tickets.
       """
       # Build feedback context from review output
       self._current_feedback_context = self._build_feedback_context(review_output)

       # ... existing clarification ticket creation logic ...
   ```

3. **Add `_build_feedback_context()` method**:
   ```python
   def _build_feedback_context(
       self,
       review_output: HolisticReviewOutput,
   ) -> FeedbackContext:
       """Build FeedbackContext from Imperator review output.

       Extracts quality notes, identifies critical sections, and
       captures file-specific issues for distribution to tickets.
       """
       from war_rig.models.tickets import FeedbackContext, QualityNote

       # Convert quality_notes strings to QualityNote objects
       quality_notes = []
       for i, note_str in enumerate(review_output.quality_notes):
           # Parse note string to extract category and severity
           note = self._parse_quality_note(note_str, i)
           quality_notes.append(note)

       # Determine critical sections from notes
       critical_sections = self._identify_critical_sections(quality_notes)

       # Build file-specific issues from file_feedback
       previous_issues = {}
       for file_name, chrome_tickets in review_output.file_feedback.items():
           previous_issues[file_name] = [t.description for t in chrome_tickets]

       return FeedbackContext(
           quality_notes=quality_notes,
           critical_sections=critical_sections,
           required_citations=True,  # Always require citations
           cross_reference_required=any("cross-reference" in n.description.lower() for n in quality_notes),
           previous_cycle_issues=previous_issues,
           augment_existing=True,  # Per user requirement
       )
   ```

4. **Add `_parse_quality_note()` helper**:
   ```python
   def _parse_quality_note(self, note_str: str, index: int) -> QualityNote:
       """Parse a quality note string into a QualityNote object.

       Infers category and severity from note content.
       """
       from war_rig.models.tickets import QualityNote

       note_lower = note_str.lower()

       # Infer category
       if "empty" in note_lower or "blank" in note_lower:
           category = "empty_section"
           severity = "critical"
       elif "citation" in note_lower or "cite" in note_lower:
           category = "missing_citation"
           severity = "high"
       elif "cross-reference" in note_lower or "cross reference" in note_lower:
           category = "no_cross_reference"
           severity = "medium"
       elif "confidence" in note_lower:
           category = "confidence_mismatch"
           severity = "medium"
       elif "redundant" in note_lower or "duplicate" in note_lower:
           category = "redundant_doc"
           severity = "high"
       elif "vague" in note_lower or "generic" in note_lower:
           category = "vague_content"
           severity = "medium"
       else:
           category = "other"
           severity = "medium"

       # Extract affected sections from note
       affected_sections = []
       for section in ["inputs", "outputs", "data_flow", "copybooks", "sql_operations",
                       "cics_operations", "business_rules", "error_handling"]:
           if section.replace("_", " ") in note_lower or section in note_lower:
               affected_sections.append(section)

       return QualityNote(
           note_id=f"QN-{index:03d}",
           category=category,
           severity=severity,
           description=note_str,
           affected_sections=affected_sections,
           cycle_identified=self._state.cycle,
       )
   ```

5. **Add `_identify_critical_sections()` helper**:
   ```python
   def _identify_critical_sections(self, notes: list[QualityNote]) -> list[str]:
       """Identify sections that must be populated based on quality notes.

       If a section was noted as empty or incomplete, it becomes critical.
       """
       # Default critical sections
       critical = {"purpose", "inputs", "outputs"}

       # Add sections from empty_section notes
       for note in notes:
           if note.category == "empty_section" and note.affected_sections:
               critical.update(note.affected_sections)

       return list(critical)
   ```

#### Part B: Embed Context in New Tickets

6. **Modify `_handle_imperator_feedback()` to pass context** when creating clarification tickets:
   ```python
   # Add feedback_context to metadata
   clarification_metadata["feedback_context"] = self._current_feedback_context.model_dump()
   ```

7. **Update `_create_documentation_tickets_for_missing()`** (around line 674) to include feedback context:
   ```python
   metadata={
       "batch_id": self._state.batch_id,
       "source": "call_graph_gap",
       "discovery": True,
       "created_at": datetime.utcnow().isoformat(),
       "priority": "high",
       # Add feedback context
       "feedback_context": (
           self._current_feedback_context.model_dump()
           if self._current_feedback_context else None
       ),
   }
   ```

### Acceptance Criteria
- [ ] FeedbackContext is built from HolisticReviewOutput quality_notes
- [ ] Quality notes are parsed with category, severity, and affected_sections
- [ ] Critical sections are identified from empty_section notes
- [ ] File-specific issues are captured from file_feedback
- [ ] DOCUMENTATION tickets include feedback_context in metadata
- [ ] CLARIFICATION tickets include feedback_context in metadata
- [ ] Context is properly JSON-serialized in ticket metadata
- [ ] Unit tests for _parse_quality_note and _identify_critical_sections

### Technical Notes
- Keep quality note parsing heuristic-based; LLM-based extraction would be overkill
- Consider caching feedback context to avoid repeated parsing
- Ensure backward compatibility with tickets created before this feature

---

## Ticket 3: Imperator Feedback Capture

**ID**: IMPFB-003
**Type**: Feature
**Priority**: High (1)
**Estimated Effort**: Medium
**Dependencies**: IMPFB-001

### Summary
Enhance Imperator's holistic review to generate structured quality notes that categorize issues by type and identify affected sections, improving feedback quality for downstream agents.

### Files to Modify
- `/home/andras/war_rig/war_rig/agents/imperator.py`

### Context
Currently, Imperator's quality_notes are free-form strings. To enable better feedback processing, we need Imperator to produce more structured output that identifies issue categories, affected sections, and severity.

### Requirements

1. **Update HolisticReviewOutput model** (around line 150) to include structured notes:
   ```python
   class StructuredQualityNote(BaseModel):
       """Structured quality observation for better downstream processing."""

       category: str = Field(
           description="Category: empty_section, missing_citation, vague_content, "
                       "no_cross_reference, confidence_mismatch, redundant_doc"
       )
       severity: str = Field(
           description="Severity: critical, high, medium, low"
       )
       description: str = Field(
           description="Human-readable description"
       )
       affected_sections: list[str] = Field(
           default_factory=list,
           description="Template sections affected"
       )
       affected_files: list[str] = Field(
           default_factory=list,
           description="Files affected (empty = all)"
       )
       remediation: str | None = Field(
           default=None,
           description="Suggested fix"
       )
   ```

2. **Add `structured_quality_notes` field to HolisticReviewOutput**:
   ```python
   structured_quality_notes: list[StructuredQualityNote] = Field(
       default_factory=list,
       description="Structured quality observations with categories and affected sections",
   )
   ```

3. **Update `_build_holistic_review_prompt()` method** (around line 350) to request structured output:
   ```python
   # Add to prompt instructions
   """
   ## Quality Notes Format

   For each quality issue, provide a structured note with:
   - category: One of [empty_section, missing_citation, vague_content,
                       no_cross_reference, confidence_mismatch, redundant_doc]
   - severity: One of [critical, high, medium, low]
   - description: Clear description of the issue
   - affected_sections: List of template sections (e.g., ["inputs", "outputs"])
   - affected_files: List of files affected, or empty for all files
   - remediation: Suggested fix

   Example:
   {
     "category": "empty_section",
     "severity": "critical",
     "description": "The 'inputs' section is empty or contains only placeholders",
     "affected_sections": ["inputs"],
     "affected_files": [],
     "remediation": "Analyze SELECT statements and file declarations to identify inputs"
   }
   """
   ```

4. **Update `_parse_holistic_review_response()` method** (around line 480) to parse structured notes:
   ```python
   # Parse structured_quality_notes if present
   structured_notes = []
   if "structured_quality_notes" in data:
       for note_data in data["structured_quality_notes"]:
           try:
               note = StructuredQualityNote.model_validate(note_data)
               structured_notes.append(note)
           except ValidationError as e:
               logger.warning(f"Invalid structured note: {e}")

   # Fall back to converting quality_notes strings if no structured notes
   if not structured_notes and data.get("quality_notes"):
       for note_str in data["quality_notes"]:
           structured_notes.append(StructuredQualityNote(
               category="other",
               severity="medium",
               description=note_str,
           ))

   output.structured_quality_notes = structured_notes
   ```

5. **Add `_identify_duplicate_documentation()` method** for redundant doc detection:
   ```python
   def _identify_duplicate_documentation(
       self,
       file_docs: list[FileDocumentation],
   ) -> list[tuple[str, str]]:
       """Identify potential duplicate documentation across files.

       Checks for:
       - Same program_id documented multiple times
       - Highly similar purpose summaries

       Returns:
           List of (file1, file2) tuples that may be duplicates.
       """
       duplicates = []

       # Check for same program_id
       program_to_files: dict[str, list[str]] = {}
       for doc in file_docs:
           pid = doc.program_id.upper()
           if pid not in program_to_files:
               program_to_files[pid] = []
           program_to_files[pid].append(doc.file_name)

       for pid, files in program_to_files.items():
           if len(files) > 1:
               for i in range(len(files)):
                   for j in range(i + 1, len(files)):
                       duplicates.append((files[i], files[j]))

       return duplicates
   ```

6. **Call duplicate detection in `holistic_review()` and add to structured notes**:
   ```python
   # Detect duplicates
   duplicates = self._identify_duplicate_documentation(input_data.file_documentation)
   if duplicates:
       for file1, file2 in duplicates:
           structured_notes.append(StructuredQualityNote(
               category="redundant_doc",
               severity="high",
               description=f"Potential duplicate documentation: {file1} and {file2}",
               affected_files=[file1, file2],
               remediation=f"Merge documentation from {file2} into {file1}",
           ))
   ```

### Acceptance Criteria
- [ ] StructuredQualityNote model defined with all fields
- [ ] HolisticReviewOutput includes structured_quality_notes field
- [ ] Prompt instructs Imperator to produce structured notes
- [ ] Response parsing handles both structured and string notes
- [ ] Duplicate documentation detection implemented
- [ ] Duplicate findings added to structured_quality_notes
- [ ] Backward compatible with existing quality_notes strings
- [ ] Unit tests for structured note parsing and duplicate detection

### Technical Notes
- The prompt update should request JSON format for structured notes
- Keep existing quality_notes field for backward compatibility
- Duplicate detection is conservative - flags potential duplicates for review

---

## Ticket 4: Scribe Feedback Processing

**ID**: IMPFB-004
**Type**: Feature
**Priority**: High (1)
**Estimated Effort**: Large
**Dependencies**: IMPFB-001, IMPFB-002, IMPFB-003

### Summary
Modify ScribeWorker to read feedback context from ticket metadata, explicitly address each quality note in output, and fail validation if critical sections are empty.

### Files to Modify
- `/home/andras/war_rig/war_rig/workers/scribe_pool.py`
- `/home/andras/war_rig/war_rig/agents/scribe.py`

### Context
Scribe currently processes documentation without awareness of Imperator feedback. With feedback context embedded in tickets, Scribe must read this context, adjust its behavior, and explicitly address each quality note.

### Requirements

#### Part A: ScribeWorker Updates (`scribe_pool.py`)

1. **Add `_extract_feedback_context()` method** to ScribeWorker (after `_load_documentation_state()`):
   ```python
   def _extract_feedback_context(
       self,
       ticket: ProgramManagerTicket,
   ) -> FeedbackContext | None:
       """Extract feedback context from ticket metadata.

       Args:
           ticket: The ticket being processed.

       Returns:
           FeedbackContext if present in metadata, None otherwise.
       """
       from war_rig.models.tickets import FeedbackContext

       metadata = ticket.metadata or {}
       ctx_data = metadata.get("feedback_context")

       if not ctx_data:
           return None

       try:
           if isinstance(ctx_data, dict):
               return FeedbackContext.model_validate(ctx_data)
           return ctx_data
       except Exception as e:
           logger.warning(f"Failed to parse feedback context: {e}")
           return None
   ```

2. **Update `_process_documentation_ticket()` method** (around line 300) to pass feedback context:
   ```python
   # Extract feedback context
   feedback_context = self._extract_feedback_context(ticket)

   # Build ScribeInput with feedback context
   scribe_input = ScribeInput(
       source_code=source_code,
       file_name=ticket.file_name,
       file_type=file_type,
       preprocessor_result=preprocessor_result,
       copybook_contents=copybook_contents,
       previous_template=previous_template,
       iteration=ticket.cycle_number,
       feedback_context=feedback_context,  # NEW FIELD
   )
   ```

3. **Add critical section validation** after Scribe output:
   ```python
   def _validate_critical_sections(
       self,
       template: DocumentationTemplate,
       feedback_context: FeedbackContext | None,
   ) -> list[str]:
       """Validate that critical sections are not empty.

       Returns list of section names that are empty but should be populated.
       """
       empty_sections = []

       # Default critical sections
       critical = {"purpose", "inputs", "outputs"}

       # Add sections from feedback context
       if feedback_context and feedback_context.critical_sections:
           critical.update(feedback_context.critical_sections)

       # Check each critical section
       for section in critical:
           if section == "purpose":
               if not template.purpose or not template.purpose.summary:
                   empty_sections.append("purpose")
           elif section == "inputs":
               if not template.inputs:
                   empty_sections.append("inputs")
           elif section == "outputs":
               if not template.outputs:
                   empty_sections.append("outputs")
           elif section == "data_flow":
               if not template.data_flow or (
                   not template.data_flow.reads_from and
                   not template.data_flow.writes_to
               ):
                   empty_sections.append("data_flow")
           elif section == "business_rules":
               if not template.business_rules:
                   empty_sections.append("business_rules")
           elif section == "error_handling":
               if not template.error_handling:
                   empty_sections.append("error_handling")
           elif section == "copybooks_used":
               # Only critical if source has COPY statements
               pass  # Skip for now, can be enhanced

       return empty_sections
   ```

4. **Fail validation if critical sections empty**:
   ```python
   # After getting Scribe output
   if output.success and output.template:
       empty_critical = self._validate_critical_sections(
           output.template,
           feedback_context,
       )

       if empty_critical:
           logger.warning(
               f"Documentation for {ticket.file_name} has empty critical sections: "
               f"{empty_critical}"
           )
           # Mark as needing rework, don't create validation ticket
           self.beads_client.update_ticket_state(
               ticket.ticket_id,
               TicketState.BLOCKED,
               reason=f"Critical sections empty: {', '.join(empty_critical)}",
               metadata_updates={
                   "empty_critical_sections": empty_critical,
                   "needs_deeper_analysis": True,
               },
           )
           return
   ```

#### Part B: ScribeAgent Updates (`scribe.py`)

5. **Add `feedback_context` field to ScribeInput** (around line 75):
   ```python
   feedback_context: FeedbackContext | None = Field(
       default=None,
       description="Feedback context from Imperator with quality notes to address",
   )
   ```

6. **Update `_build_user_prompt()` method** (around line 340) to include feedback instructions:
   ```python
   # Add feedback context section
   if input_data.feedback_context:
       parts.append("## IMPERATOR QUALITY FEEDBACK")
       parts.append("")
       parts.append("You MUST address each of the following quality issues in your output:")
       parts.append("")

       for i, note in enumerate(input_data.feedback_context.quality_notes, 1):
           parts.append(f"### Issue {i}: {note.category}")
           parts.append(f"- **Severity**: {note.severity}")
           parts.append(f"- **Description**: {note.description}")
           if note.affected_sections:
               parts.append(f"- **Affected Sections**: {', '.join(note.affected_sections)}")
           if note.guidance:
               parts.append(f"- **Guidance**: {note.guidance}")
           parts.append("")

       parts.append("## CRITICAL SECTIONS")
       parts.append("")
       parts.append("The following sections MUST be populated with real content (not empty):")
       for section in input_data.feedback_context.critical_sections:
           parts.append(f"- {section}")
       parts.append("")
       parts.append("If you cannot find information for a critical section, explicitly state ")
       parts.append("what you searched for and why it could not be determined.")
       parts.append("")

       if input_data.feedback_context.augment_existing:
           parts.append("## IMPORTANT: AUGMENT EXISTING DOCUMENTATION")
           parts.append("")
           parts.append("If documentation already exists for this program, you MUST augment it ")
           parts.append("rather than creating new documentation from scratch. Preserve existing ")
           parts.append("content and add missing information.")
           parts.append("")
   ```

7. **Add feedback response tracking to ScribeOutput** (around line 95):
   ```python
   feedback_responses: dict[str, str] = Field(
       default_factory=dict,
       description="How each quality note was addressed (note_id -> response)",
   )
   addressed_notes: list[str] = Field(
       default_factory=list,
       description="List of note_ids that were successfully addressed",
   )
   ```

8. **Update output parsing** to capture feedback responses:
   ```python
   # In _parse_response()
   feedback_responses = data.get("feedback_responses", {})
   addressed_notes = data.get("addressed_notes", [])

   return ScribeOutput(
       success=True,
       template=template,
       # ... existing fields ...
       feedback_responses=feedback_responses,
       addressed_notes=addressed_notes,
   )
   ```

### Acceptance Criteria
- [ ] ScribeWorker extracts feedback_context from ticket metadata
- [ ] ScribeInput has feedback_context field
- [ ] User prompt includes all quality notes with "MUST address" instruction
- [ ] Critical sections are listed in prompt
- [ ] Critical section validation implemented in ScribeWorker
- [ ] Documentation fails validation if critical sections empty
- [ ] ScribeOutput tracks which notes were addressed
- [ ] "Augment existing" instruction added when flag is set
- [ ] Unit tests for feedback extraction and critical section validation

### Technical Notes
- Critical section validation should be configurable per file type
- Consider adding retry logic for critical section failures
- Feedback responses should be logged for debugging

---

## Ticket 5: Critical Section Validation Rules

**ID**: IMPFB-005
**Type**: Feature
**Priority**: High (1)
**Estimated Effort**: Medium
**Dependencies**: IMPFB-004

### Summary
Implement comprehensive validation rules for critical sections that go beyond "not empty" to ensure actual content quality.

### Files to Modify
- `/home/andras/war_rig/war_rig/workers/scribe_pool.py`
- `/home/andras/war_rig/war_rig/models/templates.py`

### Context
Simply checking if a section is non-empty is insufficient. A section could contain placeholder text like "N/A" or "To be determined". We need validation rules that ensure actual, meaningful content.

### Requirements

1. **Add validation helper methods to DocumentationTemplate** (`templates.py`):
   ```python
   def is_section_meaningful(self, section_name: str) -> bool:
       """Check if a section has meaningful (non-placeholder) content.

       Validates that the section:
       - Is not empty
       - Does not contain only placeholder text
       - Has citations where required

       Args:
           section_name: Name of the section to validate.

       Returns:
           True if section has meaningful content.
       """
       PLACEHOLDER_PATTERNS = [
           r"^n/a$", r"^none$", r"^tbd$", r"^to be determined$",
           r"^unknown$", r"^not applicable$", r"^pending$",
           r"^\[.*\]$",  # Bracketed placeholders like [TBD]
           r"^-+$",  # Dashes only
       ]

       section = getattr(self, section_name, None)
       if section is None:
           return False

       # Handle different section types
       if section_name == "purpose":
           if not section.summary:
               return False
           # Check for placeholder patterns
           summary_lower = section.summary.lower().strip()
           for pattern in PLACEHOLDER_PATTERNS:
               if re.match(pattern, summary_lower):
                   return False
           # Must have at least one citation
           if not section.citations:
               return False
           return True

       elif section_name in ("inputs", "outputs"):
           if not section:  # Empty list
               return False
           # At least one item must have description and citation
           for item in section:
               if item.description and item.citation:
                   return True
           return False

       elif section_name == "business_rules":
           if not section:
               return False
           # At least one rule must have description and citation
           for rule in section:
               if rule.description and rule.citation:
                   return True
           return False

       elif section_name == "error_handling":
           if not section:
               return False
           # At least one entry must have condition and action
           for entry in section:
               if entry.condition and entry.action:
                   return True
           return False

       elif section_name == "data_flow":
           if not section:
               return False
           # Must have either reads_from or writes_to
           has_reads = section.reads_from and len(section.reads_from) > 0
           has_writes = section.writes_to and len(section.writes_to) > 0
           return has_reads or has_writes

       return True  # Unknown sections pass by default
   ```

2. **Add citation validation**:
   ```python
   def validate_citations(self) -> list[str]:
       """Check that all sections have required citations.

       Returns:
           List of section names missing citations.
       """
       missing_citations = []

       # Purpose must have citations
       if self.purpose and not self.purpose.citations:
           missing_citations.append("purpose")

       # Each input/output should have citation
       if self.inputs:
           for i, inp in enumerate(self.inputs):
               if not inp.citation:
                   missing_citations.append(f"inputs[{i}]")

       if self.outputs:
           for i, out in enumerate(self.outputs):
               if not out.citation:
                   missing_citations.append(f"outputs[{i}]")

       # Business rules must have citations
       if self.business_rules:
           for i, rule in enumerate(self.business_rules):
               if not rule.citation:
                   missing_citations.append(f"business_rules[{i}]")

       return missing_citations
   ```

3. **Update ScribeWorker validation** (`scribe_pool.py`):
   ```python
   def _validate_critical_sections(
       self,
       template: DocumentationTemplate,
       feedback_context: FeedbackContext | None,
   ) -> tuple[list[str], list[str]]:
       """Validate critical sections for meaningful content.

       Returns:
           Tuple of (empty_sections, missing_citations).
       """
       empty_sections = []

       # Default critical sections
       critical = {"purpose", "inputs", "outputs"}

       # Add sections from feedback context
       if feedback_context and feedback_context.critical_sections:
           critical.update(feedback_context.critical_sections)

       # Check each critical section for meaningful content
       for section in critical:
           if not template.is_section_meaningful(section):
               empty_sections.append(section)

       # Check citations if required
       missing_citations = []
       if feedback_context and feedback_context.required_citations:
           missing_citations = template.validate_citations()

       return empty_sections, missing_citations
   ```

4. **Add quality score calculation**:
   ```python
   def calculate_quality_score(self) -> dict[str, Any]:
       """Calculate documentation quality score.

       Returns:
           Dict with overall score and per-section scores.
       """
       scores = {
           "purpose": 0,
           "inputs": 0,
           "outputs": 0,
           "business_rules": 0,
           "error_handling": 0,
           "data_flow": 0,
       }

       # Score each section (0-100)
       if self.is_section_meaningful("purpose"):
           scores["purpose"] = 100 if self.purpose.citations else 70

       if self.inputs:
           cited = sum(1 for i in self.inputs if i.citation)
           scores["inputs"] = int((cited / len(self.inputs)) * 100) if self.inputs else 0

       if self.outputs:
           cited = sum(1 for o in self.outputs if o.citation)
           scores["outputs"] = int((cited / len(self.outputs)) * 100) if self.outputs else 0

       if self.business_rules:
           cited = sum(1 for r in self.business_rules if r.citation)
           scores["business_rules"] = int((cited / len(self.business_rules)) * 100)

       if self.error_handling:
           scores["error_handling"] = 100

       if self.data_flow:
           has_content = bool(self.data_flow.reads_from or self.data_flow.writes_to)
           scores["data_flow"] = 100 if has_content else 0

       # Calculate overall score (weighted)
       weights = {
           "purpose": 0.2,
           "inputs": 0.2,
           "outputs": 0.2,
           "business_rules": 0.15,
           "error_handling": 0.1,
           "data_flow": 0.15,
       }

       overall = sum(scores[k] * weights[k] for k in weights)

       return {
           "overall": round(overall),
           "sections": scores,
           "missing_citations": self.validate_citations(),
       }
   ```

### Acceptance Criteria
- [ ] `is_section_meaningful()` detects placeholder content
- [ ] `validate_citations()` identifies sections missing citations
- [ ] `calculate_quality_score()` produces meaningful scores
- [ ] ScribeWorker uses enhanced validation
- [ ] Placeholder patterns are configurable
- [ ] Quality score is logged/stored for reporting
- [ ] Unit tests for validation methods

### Technical Notes
- Placeholder patterns should be case-insensitive
- Consider file-type-specific validation (JCL has different sections than COBOL)
- Quality score can be stored in ticket metadata for reporting

---

## Ticket 6: Challenger Feedback Validation

**ID**: IMPFB-006
**Type**: Feature
**Priority**: Medium (2)
**Estimated Effort**: Medium
**Dependencies**: IMPFB-004

### Summary
Enhance ChallengerWorker to validate that Scribe addressed all quality notes from feedback context and that critical sections contain meaningful content.

### Files to Modify
- `/home/andras/war_rig/war_rig/workers/challenger_pool.py`
- `/home/andras/war_rig/war_rig/agents/challenger.py`

### Context
Challenger validates documentation quality. With feedback context available, Challenger should verify that Scribe addressed each quality note and that the documentation quality has improved.

### Requirements

#### Part A: ChallengerWorker Updates (`challenger_pool.py`)

1. **Add feedback context extraction** (similar to ScribeWorker):
   ```python
   def _extract_feedback_context(
       self,
       ticket: ProgramManagerTicket,
   ) -> FeedbackContext | None:
       """Extract feedback context from ticket metadata."""
       # Same implementation as ScribeWorker
   ```

2. **Add `_validate_feedback_addressed()` method**:
   ```python
   def _validate_feedback_addressed(
       self,
       template: DocumentationTemplate,
       feedback_context: FeedbackContext,
       scribe_output_metadata: dict | None,
   ) -> list[str]:
       """Validate that Scribe addressed all quality notes.

       Args:
           template: The documentation template.
           feedback_context: Feedback from Imperator.
           scribe_output_metadata: Metadata from Scribe's output (may contain addressed_notes).

       Returns:
           List of unaddressed note descriptions.
       """
       unaddressed = []
       addressed_ids = set()

       # Get notes Scribe claims to have addressed
       if scribe_output_metadata:
           addressed_ids = set(scribe_output_metadata.get("addressed_notes", []))

       for note in feedback_context.quality_notes:
           # Check if Scribe claimed to address it
           if note.note_id in addressed_ids:
               continue

           # Verify by checking affected sections
           if note.category == "empty_section":
               for section in note.affected_sections:
                   if not template.is_section_meaningful(section):
                       unaddressed.append(
                           f"Section '{section}' still empty despite feedback"
                       )

           elif note.category == "missing_citation":
               missing = template.validate_citations()
               if missing:
                   unaddressed.append(
                       f"Citations still missing: {', '.join(missing[:3])}"
                   )

           elif note.category == "vague_content":
               # Harder to validate automatically - flag for manual review
               unaddressed.append(
                   f"Vague content issue not addressed: {note.description[:50]}..."
               )

       return unaddressed
   ```

3. **Update `_validate_documentation()` method** to include feedback validation:
   ```python
   # After normal validation
   if feedback_context:
       unaddressed = self._validate_feedback_addressed(
           state["template"],
           feedback_context,
           state.get("scribe_metadata"),
       )

       if unaddressed:
           for issue in unaddressed:
               issues_found.append(issue)
           # Create blocking question for unaddressed feedback
           blocking_questions.append({
               "question_id": f"FB-{ticket.ticket_id[-8:]}",
               "question": f"Imperator feedback not addressed: {'; '.join(unaddressed[:3])}",
               "severity": "BLOCKING",
               "section": "feedback",
               "question_type": "COMPLETENESS",
           })
   ```

#### Part B: ChallengerAgent Updates (`challenger.py`)

4. **Add `feedback_context` to ChallengerInput** (around line 80):
   ```python
   feedback_context: FeedbackContext | None = Field(
       default=None,
       description="Feedback context from Imperator to validate against",
   )
   ```

5. **Update `_build_user_prompt()` method** to include feedback validation instructions:
   ```python
   # Add feedback validation section
   if input_data.feedback_context:
       parts.append("## FEEDBACK VALIDATION REQUIREMENTS")
       parts.append("")
       parts.append("Verify that the Scribe addressed each of these quality issues:")
       parts.append("")

       for note in input_data.feedback_context.quality_notes:
           parts.append(f"- [{note.severity.upper()}] {note.description}")
           if note.affected_sections:
               parts.append(f"  Check sections: {', '.join(note.affected_sections)}")
       parts.append("")

       parts.append("Critical sections that MUST be populated:")
       for section in input_data.feedback_context.critical_sections:
           parts.append(f"- {section}")
       parts.append("")

       parts.append("If any feedback was not addressed, create a BLOCKING question.")
       parts.append("")
   ```

### Acceptance Criteria
- [ ] ChallengerWorker extracts feedback context from ticket metadata
- [ ] ChallengerInput has feedback_context field
- [ ] Prompt includes feedback validation instructions
- [ ] `_validate_feedback_addressed()` checks each quality note
- [ ] Unaddressed feedback creates BLOCKING questions
- [ ] Critical section validation is enforced by Challenger
- [ ] Unit tests for feedback validation

### Technical Notes
- Challenger should not duplicate Scribe's validation but verify it
- Consider making some feedback validation "soft" (warning) vs "hard" (blocking)
- Log which notes were verified for debugging

---

## Ticket 7: Duplicate Documentation Prevention

**ID**: IMPFB-007
**Type**: Feature
**Priority**: High (1)
**Estimated Effort**: Medium
**Dependencies**: IMPFB-004

### Summary
Ensure Scribe always augments existing documentation instead of creating new copies, and implement cleanup logic for already-duplicated documentation.

### Files to Modify
- `/home/andras/war_rig/war_rig/workers/scribe_pool.py`
- `/home/andras/war_rig/war_rig/orchestration/ticket_engine.py`

### Context
The Imperator identified redundant copies of program documentation. Per user requirement, Scribe should ALWAYS augment existing documentation instead of creating new. Additionally, Imperator should create cleanup tickets for identified duplicates.

### Requirements

#### Part A: ScribeWorker - Always Augment (`scribe_pool.py`)

1. **Update `_process_documentation_ticket()` to always check for existing docs**:
   ```python
   # At the start of _process_documentation_ticket()

   # ALWAYS check for existing documentation first
   existing_template = self._load_previous_template(ticket)

   if existing_template:
       logger.info(
           f"Found existing documentation for {ticket.file_name}, "
           f"will augment (not replace)"
       )
       # Pass existing template even for initial documentation tickets
       previous_template = existing_template
   ```

2. **Add `_check_for_duplicate_docs()` method**:
   ```python
   def _check_for_duplicate_docs(
       self,
       program_id: str,
       current_file: str,
   ) -> list[Path]:
       """Check for other documentation files with the same program_id.

       Args:
           program_id: The program identifier.
           current_file: The file being documented (to exclude).

       Returns:
           List of paths to potentially duplicate documentation files.
       """
       duplicates = []

       # Search output directory for .doc.json files
       for doc_file in self.output_directory.glob("**/*.doc.json"):
           if doc_file.stem == Path(current_file).stem:
               continue  # Skip current file

           try:
               with open(doc_file, "r", encoding="utf-8") as f:
                   doc_data = json.load(f)

               doc_program_id = doc_data.get("header", {}).get("program_id", "")
               if doc_program_id.upper() == program_id.upper():
                   duplicates.append(doc_file)
           except Exception as e:
               logger.debug(f"Error reading {doc_file}: {e}")

       return duplicates
   ```

3. **Merge duplicate content when found**:
   ```python
   def _merge_duplicate_docs(
       self,
       primary: DocumentationTemplate,
       duplicate: DocumentationTemplate,
   ) -> DocumentationTemplate:
       """Merge content from duplicate doc into primary.

       Preserves primary content but adds any unique content from duplicate.
       """
       # Merge inputs (avoid duplicates by name)
       primary_input_names = {i.name.upper() for i in primary.inputs}
       for inp in duplicate.inputs:
           if inp.name.upper() not in primary_input_names:
               primary.inputs.append(inp)

       # Merge outputs (avoid duplicates by name)
       primary_output_names = {o.name.upper() for o in primary.outputs}
       for out in duplicate.outputs:
           if out.name.upper() not in primary_output_names:
               primary.outputs.append(out)

       # Merge business rules (avoid duplicates by rule_id)
       primary_rule_ids = {r.rule_id for r in primary.business_rules}
       for rule in duplicate.business_rules:
           if rule.rule_id not in primary_rule_ids:
               primary.business_rules.append(rule)

       # Merge called_programs
       primary_calls = {c.program_name.upper() for c in primary.called_programs}
       for call in duplicate.called_programs:
           if call.program_name.upper() not in primary_calls:
               primary.called_programs.append(call)

       # Merge copybooks
       primary_copybooks = {c.copybook_name.upper() for c in primary.copybooks_used}
       for cb in duplicate.copybooks_used:
           if cb.copybook_name.upper() not in primary_copybooks:
               primary.copybooks_used.append(cb)

       return primary
   ```

#### Part B: TicketOrchestrator - Cleanup Tickets (`ticket_engine.py`)

4. **Add `_create_cleanup_tickets_for_duplicates()`**:
   ```python
   def _create_cleanup_tickets_for_duplicates(
       self,
       duplicates: list[tuple[str, str]],
   ) -> list[ProgramManagerTicket]:
       """Create cleanup tickets for identified duplicate documentation.

       Args:
           duplicates: List of (primary_file, duplicate_file) tuples.

       Returns:
           List of created cleanup tickets.
       """
       from uuid import uuid4

       cleanup_tickets = []

       for primary, duplicate in duplicates:
           ticket_id = f"CLN-{uuid4().hex[:8].upper()}"

           ticket = ProgramManagerTicket(
               ticket_id=ticket_id,
               ticket_type=TicketType.CHROME,  # Use CHROME for rework tasks
               state=TicketState.CREATED,
               file_name=duplicate,  # The file to be cleaned up
               program_id=Path(duplicate).stem,
               cycle_number=self._state.cycle,
               metadata={
                   "cleanup_type": "duplicate_merge",
                   "primary_file": primary,
                   "duplicate_file": duplicate,
                   "action": f"Merge content from {duplicate} into {primary}, then delete {duplicate}",
                   "batch_id": self._state.batch_id,
                   "created_at": datetime.utcnow().isoformat(),
               },
           )

           self.beads_client._pm_ticket_cache[ticket_id] = ticket
           cleanup_tickets.append(ticket)
           logger.info(f"Created cleanup ticket {ticket_id} to merge {duplicate} into {primary}")

       if cleanup_tickets:
           self.beads_client._save_to_disk()

       return cleanup_tickets
   ```

5. **Call duplicate cleanup from `_handle_imperator_feedback()`**:
   ```python
   # After building feedback context
   if review_output.structured_quality_notes:
       duplicates = [
           (n.affected_files[0], n.affected_files[1])
           for n in review_output.structured_quality_notes
           if n.category == "redundant_doc" and len(n.affected_files) >= 2
       ]

       if duplicates:
           self._create_cleanup_tickets_for_duplicates(duplicates)
   ```

### Acceptance Criteria
- [ ] ScribeWorker always checks for existing documentation first
- [ ] Existing documentation is augmented, not replaced
- [ ] Duplicate detection finds docs with same program_id
- [ ] Content merging preserves unique content from both docs
- [ ] Cleanup tickets created for identified duplicates
- [ ] Cleanup tickets have proper metadata (primary_file, duplicate_file)
- [ ] Unit tests for merge logic and duplicate detection

### Technical Notes
- Merging should be conservative - prefer primary content when conflicts exist
- Keep backup of duplicate before deleting (or mark as "merged" rather than delete)
- Consider adding a "merged_from" field to track lineage

---

## Ticket 8: Integration Testing

**ID**: IMPFB-008
**Type**: Testing
**Priority**: Medium (2)
**Estimated Effort**: Medium
**Dependencies**: IMPFB-001 through IMPFB-007

### Summary
Create comprehensive integration tests that validate the complete feedback loop flow from Imperator review through Scribe/Challenger processing.

### Files to Create/Modify
- `/home/andras/war_rig/tests/test_feedback_loop/` (new directory)
- `/home/andras/war_rig/tests/test_feedback_loop/test_feedback_flow.py`
- `/home/andras/war_rig/tests/test_feedback_loop/test_critical_sections.py`
- `/home/andras/war_rig/tests/test_feedback_loop/test_duplicate_prevention.py`

### Context
The feedback loop involves multiple components working together. Integration tests ensure the complete flow works correctly.

### Requirements

#### Part A: Feedback Flow Integration Tests

1. **Create `test_feedback_flow.py`**:
   ```python
   """Integration tests for Imperator feedback flow."""

   import pytest
   from war_rig.models.tickets import FeedbackContext, QualityNote
   from war_rig.orchestration.ticket_engine import TicketOrchestrator
   from war_rig.agents.imperator import HolisticReviewOutput, StructuredQualityNote

   class TestFeedbackFlow:
       """Test complete feedback flow from Imperator to Scribe/Challenger."""

       def test_feedback_context_built_from_review(self):
           """Verify FeedbackContext is correctly built from HolisticReviewOutput."""
           pass

       def test_feedback_embedded_in_documentation_tickets(self):
           """Verify DOCUMENTATION tickets include feedback_context in metadata."""
           pass

       def test_feedback_embedded_in_validation_tickets(self):
           """Verify VALIDATION tickets include feedback_context in metadata."""
           pass

       def test_scribe_receives_feedback_context(self):
           """Verify ScribeAgent receives and processes feedback_context."""
           pass

       def test_challenger_receives_feedback_context(self):
           """Verify ChallengerAgent receives and validates against feedback_context."""
           pass
   ```

#### Part B: Critical Section Tests

2. **Create `test_critical_sections.py`**:
   ```python
   """Tests for critical section validation."""

   import pytest
   from war_rig.models.templates import DocumentationTemplate
   from war_rig.workers.scribe_pool import ScribeWorker

   class TestCriticalSectionValidation:
       """Test critical section validation logic."""

       def test_empty_section_detected(self):
           """Verify empty sections are correctly identified."""
           pass

       def test_placeholder_content_detected(self):
           """Verify placeholder content (TBD, N/A) is detected as empty."""
           pass

       def test_missing_citations_detected(self):
           """Verify missing citations are identified."""
           pass

       def test_quality_score_calculation(self):
           """Verify quality score calculation is accurate."""
           pass

       def test_validation_fails_for_empty_critical_sections(self):
           """Verify documentation fails validation with empty critical sections."""
           pass
   ```

#### Part C: Duplicate Prevention Tests

3. **Create `test_duplicate_prevention.py`**:
   ```python
   """Tests for duplicate documentation prevention."""

   import pytest
   from pathlib import Path
   from war_rig.workers.scribe_pool import ScribeWorker
   from war_rig.orchestration.ticket_engine import TicketOrchestrator

   class TestDuplicatePrevention:
       """Test duplicate documentation prevention and cleanup."""

       def test_existing_doc_detected(self):
           """Verify existing documentation is found before processing."""
           pass

       def test_existing_doc_augmented_not_replaced(self):
           """Verify Scribe augments existing doc instead of replacing."""
           pass

       def test_duplicate_docs_detected_by_program_id(self):
           """Verify duplicate docs with same program_id are detected."""
           pass

       def test_content_merge_preserves_unique_content(self):
           """Verify merge operation preserves unique content from both docs."""
           pass

       def test_cleanup_tickets_created_for_duplicates(self):
           """Verify cleanup tickets are created for identified duplicates."""
           pass
   ```

#### Part D: End-to-End Test

4. **Create `test_e2e_feedback_loop.py`**:
   ```python
   """End-to-end test for complete feedback loop."""

   import pytest
   from pathlib import Path
   from war_rig.orchestration.ticket_engine import TicketOrchestrator
   from war_rig.config import load_config

   class TestEndToEndFeedbackLoop:
       """Test complete feedback loop from low-quality doc to improved version."""

       @pytest.mark.integration
       async def test_feedback_improves_documentation_quality(self):
           """
           End-to-end test:
           1. Start with documentation that has empty sections
           2. Imperator review identifies issues
           3. Feedback is distributed to tickets
           4. Scribe addresses feedback
           5. Challenger validates feedback was addressed
           6. Quality score improves
           """
           pass
   ```

### Acceptance Criteria
- [ ] All unit tests for feedback models pass
- [ ] Integration tests for feedback flow pass
- [ ] Critical section validation tests cover all edge cases
- [ ] Duplicate prevention tests cover merge scenarios
- [ ] End-to-end test demonstrates quality improvement
- [ ] Tests run in CI/CD pipeline
- [ ] Test coverage > 80% for new code

### Technical Notes
- Use pytest fixtures for common test setup
- Mock LLM calls for unit tests, use real calls for integration tests
- Consider using pytest-asyncio for async test support
- Add test data files with sample documentation

---

## Open Questions

1. **Feedback Persistence Across Sessions**: Should feedback context be persisted to disk so it survives orchestrator restarts? Currently it's in-memory only.

2. **Feedback Expiry**: Should quality notes expire after N cycles if not addressed? Or persist indefinitely?

3. **Cross-Reference Implementation**: The user mentioned wanting cross-reference information. Should this be a separate ticket or included in the critical sections work?

4. **Sample JCL Generation**: The user mentioned wanting sample JCL in documentation. This may require a separate ticket as it's a distinct feature.

5. **Confidence Score Recalculation**: The user noted confidence scores don't match quality. Should there be explicit logic to recalculate confidence based on the quality score?

---

## Implementation Order Recommendation

1. **Phase 1 (Foundation)**: Ticket 1 (Metadata Model) - must be first
   - Defines data structures used by all other tickets

2. **Phase 2 (Distribution)**: Tickets 2, 3 in parallel
   - Ticket 2: PM distributes feedback to tickets
   - Ticket 3: Imperator captures structured feedback

3. **Phase 3 (Processing)**: Ticket 4 (Scribe Feedback Processing)
   - Core behavior change for Scribe

4. **Phase 4 (Validation)**: Tickets 5, 6, 7 in parallel
   - Ticket 5: Critical section validation rules
   - Ticket 6: Challenger feedback validation
   - Ticket 7: Duplicate prevention

5. **Phase 5 (Testing)**: Ticket 8
   - Integration testing after all features complete

Total estimated effort: ~2-3 weeks with focused development
