# War Rig: Mainframe Documentation System
## Agent Specification v3.0

---

## Overview

War Rig is a multi-agent system for documenting legacy mainframe codebases (COBOL, PL/I, JCL). Skimmer agents work in pairs — one documents while the other validates — cycling until the documentation meets quality standards. A reviewer agent oversees the process and decides when documentation is complete.

---

## Architecture Summary

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                              WAR RIG                                        │
│                                                                             │
│   ┌─────────────────────────────────────────────────────────────────────┐   │
│   │                        WAR BOY PAIR                                 │   │
│   │                                                                     │   │
│   │    ┌──────────────┐              ┌──────────────┐                   │   │
│   │    │  SCRIBE      │              │  CHALLENGER  │                   │   │
│   │    │              │              │              │                   │   │
│   │    │  Fills the   │◄────────────►│  Questions   │                   │   │
│   │    │  template    │   dialogue   │  & validates │                   │   │
│   │    │              │              │              │                   │   │
│   │    └──────┬───────┘              └──────┬───────┘                   │   │
│   │           │                             │                           │   │
│   │           └──────────────┬──────────────┘                           │   │
│   │                          │                                          │   │
│   │                          ▼                                          │   │
│   │                 ┌────────────────┐                                  │   │
│   │                 │  Draft Doc     │                                  │   │
│   │                 └────────┬───────┘                                  │   │
│   │                          │                                          │   │
│   └──────────────────────────┼──────────────────────────────────────────┘   │
│                              │                                              │
│                              ▼                                              │
│                     ┌────────────────┐                                      │
│                     │   IMPERATOR    │                                      │
│                     │   (Reviewer)   │                                      │
│                     └────────┬───────┘                                      │
│                              │                                              │
│               ┌──────────────┼──────────────┐                               │
│               ▼              ▼              ▼                               │
│        ┌──────────┐   ┌──────────┐   ┌──────────┐                           │
│        │WITNESSED │   │  CHROME  │   │ VALHALLA │                           │
│        │(approved)│   │ (tickets)│   │ (final)  │                           │
│        └──────────┘   └────┬─────┘   └──────────┘                           │
│                            │                                                │
│                            │ Back to War Boy Pair                           │
│                            └────────────────────────────────────────────────┘
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## Agents

### 1. Scribe (Documenter War Boy)

**Purpose:** Analyze mainframe source code and fill out the documentation template.

**Inputs:**
- Source file content (COBOL, PL/I, JCL, or copybook)
- File type classification
- Structural hints from preprocessor (paragraphs, PERFORM targets, CALL targets, file operations)
- Any open Chrome tickets from previous iterations
- Copybook contents (resolved if referenced)
- Challenger's questions from previous round (if any)

**Outputs:**
- Completed documentation template
- Confidence level (HIGH, MEDIUM, LOW) per section
- Open questions (things that could not be determined)
- Responses to Challenger's questions

**Behavior:**
- First pass: Analyze code, fill template as completely as possible
- Subsequent passes: Address Chrome tickets and Challenger questions
- Must cite line numbers for every factual claim
- Must not invent information; mark unknowns as UNKNOWN with explanation

**Constraints:**
- Must complete template even if sections are marked UNKNOWN
- Cannot see other War Rigs' outputs (future multi-team mode)

---

### 2. Challenger (Validator War Boy)

**Purpose:** Review Scribe's documentation, ask probing questions, and identify gaps or errors.

**Inputs:**
- Scribe's completed template
- Original source file content
- Structural hints from preprocessor
- Previous round's questions and Scribe's responses

**Outputs:**
- List of questions for Scribe
- List of potential issues found
- Validation assessment: SOLID, SHAKY, or WRONG per section
- Suggested corrections (with citations)

**Behavior:**
- Review each template section for accuracy and completeness
- Cross-check claims against source code
- Ask clarifying questions about vague or suspicious entries
- Identify contradictions between sections
- Verify that citations are accurate

**Question Types:**
- CLARIFICATION: "What do you mean by X?"
- VERIFICATION: "Line 234 says Y, but you documented Z"
- COMPLETENESS: "What about the error handling at line 456?"
- CHALLENGE: "Are you sure this is an input, not an output?"

**Constraints:**
- Maximum 5 questions per round
- Must provide evidence (line numbers) for challenges
- Cannot modify documentation directly

---

### 3. Imperator (Reviewer)

**Purpose:** Oversee the War Boy pair, decide when documentation is complete, issue Chrome tickets for major issues.

**Inputs:**
- Current documentation draft
- Challenger's assessment
- Scribe's responses to questions
- Original source file content
- Iteration count

**Outputs:**
- Decision: WITNESSED (approved), CHROME (needs work), or VALHALLA (exceptional)
- If CHROME: List of tickets specifying what needs improvement
- If WITNESSED/VALHALLA: Final approved documentation

**Behavior:**
- Evaluate if Scribe and Challenger have converged
- Check that critical sections are complete
- Identify issues the pair missed
- Decide when further iteration is unlikely to improve quality

**Acceptance Criteria:**
- Purpose section: Specific, not generic boilerplate
- All inputs and outputs identified
- At least one business rule documented with citation
- Copybooks listed match structural hints
- Challenger assessment is SOLID on critical sections
- No unresolved WRONG assessments

**Constraints:**
- Maximum 3 iterations before forced approval
- Can override Challenger if evidence supports Scribe
- Must approve after max iterations (with quality flag)

---

## Workflow

### Single Program Flow

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                                                                             │
│  ITERATION 1                                                                │
│  ──────────                                                                 │
│  1. Scribe receives: source code, structural hints, copybooks               │
│  2. Scribe produces: initial documentation template                         │
│  3. Challenger receives: Scribe's template, source code                     │
│  4. Challenger produces: questions, validation assessment                   │
│  5. Scribe receives: Challenger's questions                                 │
│  6. Scribe produces: responses, updated template                            │
│  7. Imperator receives: template, assessment, dialogue                      │
│  8. Imperator decides: WITNESSED, CHROME, or needs more iteration           │
│                                                                             │
│  If CHROME or more iteration needed:                                        │
│  ─────────────────────────────────────                                      │
│  ITERATION 2                                                                │
│  1. Scribe receives: Chrome tickets, previous questions                     │
│  2. Scribe produces: updated template, responses                            │
│  3. Challenger receives: updated template                                   │
│  4. Challenger produces: new questions, updated assessment                  │
│  5. ... repeat until WITNESSED or max iterations                            │
│                                                                             │
│  FINAL                                                                      │
│  ─────                                                                      │
│  Documentation stored in VALHALLA (if exceptional) or output directory      │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## Data Structures

### Chrome Ticket

Issued by Imperator when documentation needs significant improvement.

**Fields:**
- ticket_id: Unique identifier
- program_id: Target program
- section: Which template section needs work
- issue_type: One of [VAGUE, MISSING, CONTRADICTORY, UNCITED, INCOMPLETE, WRONG]
- description: Specific description of the problem
- guidance: Hint for resolution
- priority: One of [CRITICAL, HIGH, MEDIUM]

---

### Challenger Question

Issued by Challenger to probe Scribe's documentation.

**Fields:**
- question_id: Unique identifier
- section: Which template section this relates to
- question_type: One of [CLARIFICATION, VERIFICATION, COMPLETENESS, CHALLENGE]
- question: The actual question
- evidence: Line numbers or citations supporting the question
- severity: One of [BLOCKING, IMPORTANT, MINOR]

---

### Scribe Response

Scribe's answer to a Challenger question.

**Fields:**
- question_id: Reference to the question
- response: The answer
- action_taken: One of [UPDATED, DEFENDED, ACKNOWLEDGED]
- updated_section: If UPDATED, which section changed
- citation: Supporting evidence

---

## Documentation Template

### Header
- program_id: Program name from source
- file_name: Source file path
- file_type: One of [COBOL, PLI, JCL, COPYBOOK, PROC, BMS, OTHER]
- analyzed_by: War Rig identifier
- iteration_count: Number of refinement cycles
- final_status: WITNESSED, FORCED, or VALHALLA

### Purpose
- summary: 2-3 sentence description of what this program does
- business_context: What business process this serves
- program_type: One of [BATCH, ONLINE_CICS, SUBROUTINE, UTILITY]
- citations: Line numbers supporting the summary

### Inputs
Array of:
- name: File, table, or parameter name
- type: One of [FILE_SEQUENTIAL, FILE_VSAM, DB2_TABLE, IMS_SEGMENT, PARAMETER, CICS_COMMAREA, CICS_MAP, CICS_QUEUE, OTHER]
- description: What this input contains
- copybook: Associated copybook name (if any)
- citation: Line numbers where input is read/received

### Outputs
Array of:
- name: File, table, or result name
- type: One of [FILE_SEQUENTIAL, FILE_VSAM, DB2_TABLE, IMS_SEGMENT, REPORT, CICS_COMMAREA, CICS_MAP, CICS_QUEUE, RETURN_CODE, OTHER]
- description: What this output contains
- copybook: Associated copybook name (if any)
- citation: Line numbers where output is written/sent

### Called Programs
Array of:
- program_name: Name of called program
- call_type: One of [STATIC_CALL, DYNAMIC_CALL, CICS_LINK, CICS_XCTL]
- purpose: Why this program is called
- parameters: What is passed (USING clause)
- citation: Line number of CALL/LINK/XCTL statement

### Calling Context
- called_by: Array of program names known to call this (may be populated later)
- entry_points: For CICS, which transaction IDs invoke this
- linkage_section: Key fields in LINKAGE SECTION

### Business Rules
Array of:
- rule_id: Sequential identifier
- description: Plain English description of the rule
- logic_summary: Brief explanation of implementation
- conditions: Key conditions (IF statements) involved
- citation: Line numbers where rule is implemented

### Data Flow
- reads_from: Array of {source, fields_used, citation}
- writes_to: Array of {destination, fields_written, citation}
- transforms: Array of {input_field, output_field, transformation_description, citation}

### Copybooks Used
Array of:
- copybook_name: Name of copybook
- purpose: What data structure it defines
- location: WORKING-STORAGE, LINKAGE, FILE SECTION
- citation: Line number of COPY statement

### Paragraphs
Array of key paragraphs:
- paragraph_name: Name
- purpose: What this paragraph does
- called_by: Which paragraphs PERFORM this
- calls: Which paragraphs this PERFORMs
- citation: Line number range

### Error Handling
Array of:
- condition: What error condition is handled
- action: What happens (ABEND, return code, etc.)
- citation: Line numbers

### SQL Operations (if DB2)
Array of:
- operation: SELECT, INSERT, UPDATE, DELETE, CURSOR
- table: Table name
- purpose: Why this operation
- citation: Line number

### CICS Operations (if CICS)
Array of:
- command: RECEIVE, SEND, READ, WRITE, LINK, etc.
- resource: Map name, file name, queue name, etc.
- purpose: Why this operation
- citation: Line number

### Open Questions
Array of:
- question: What remains unclear
- context: Why it could not be determined
- suggestion: How it might be resolved

### Confidence Assessment
- overall_confidence: HIGH, MEDIUM, or LOW
- low_confidence_sections: Array of section names
- challenger_assessment: Summary of Challenger's final evaluation
- reasoning: Brief explanation

---

## Template: Copybook

### Header
- copybook_name: Name
- file_name: Source file path
- file_type: COPYBOOK

### Purpose
- summary: What data structure this defines
- usage_context: File record, commarea, working storage, etc.

### Record Layout
Array of:
- level: COBOL level number (01, 05, 10, etc.)
- field_name: Name
- picture: PIC clause
- usage: DISPLAY, COMP, COMP-3, etc.
- redefines: What it redefines (if applicable)
- occurs: Array bounds (if applicable)
- description: Business meaning
- valid_values: Known valid values (if determinable)
- citation: Line number

### Key Fields
Array of notable fields:
- field_name: Name
- significance: Why this field is important
- related_programs: Programs that use this field

### Used By
Array of program names that COPY this copybook

---

## Template: JCL Job

### Header
- job_name: JOB name
- file_name: Source file path
- file_type: JCL

### Purpose
- summary: What this job accomplishes
- schedule: When this runs (if known)
- business_context: What business process this serves
- job_class: Execution class
- msgclass: Output class

### Steps
Array of:
- step_name: EXEC step name
- program: PGM= value (program executed)
- proc: PROC= value (if procedure call)
- purpose: What this step accomplishes
- condition: COND= parameter (if any)
- citation: Line number

### DD Statements
Array of:
- step_name: Which step this belongs to
- dd_name: DD name
- dataset: DSN= value
- disposition: DISP= value
- dcb: DCB parameters
- purpose: What this file is for
- citation: Line number

### Dependencies
- input_datasets: Datasets that must exist before job runs
- output_datasets: Datasets created/modified by job
- predecessor_jobs: Jobs that must complete first (if known)
- successor_jobs: Jobs that depend on this (if known)

### Restart/Recovery
- restart_points: Steps that can be restarted
- abend_handling: How failures are handled

---

## Preprocessing

Before agents run, a preprocessor extracts structural information. This is deterministic (no LLM).

### Preprocessor Outputs: COBOL

```json
{
  "program_id": "CBACT04C",
  "file_type": "COBOL",
  "divisions": ["IDENTIFICATION", "ENVIRONMENT", "DATA", "PROCEDURE"],
  "paragraphs": [
    {"name": "0000-MAIN", "start_line": 100, "end_line": 150},
    {"name": "1000-PROCESS", "start_line": 152, "end_line": 200}
  ],
  "performs": [
    {"from": "0000-MAIN", "to": "1000-PROCESS", "thru": null, "line": 120},
    {"from": "0000-MAIN", "to": "2000-READ", "thru": "2000-EXIT", "line": 125}
  ],
  "calls": [
    {"program": "CBSTM03B", "using": ["WS-COMMAREA"], "line": 180}
  ],
  "copybooks": [
    {"name": "CVACT01Y", "line": 45},
    {"name": "CVACT02Y", "line": 46}
  ],
  "files": [
    {"name": "TCATBAL-FILE", "type": "INDEXED", "line": 30}
  ],
  "sql_statements": [
    {"operation": "SELECT", "table": "ACCOUNT", "line": 250}
  ],
  "cics_commands": [
    {"command": "READ", "resource": "CARDFILE", "line": 300}
  ]
}
```

### Preprocessor Outputs: JCL

```json
{
  "job_name": "CBACT04J",
  "file_type": "JCL",
  "steps": [
    {"name": "STEP01", "program": "CBACT04C", "line": 10}
  ],
  "dd_statements": [
    {"step": "STEP01", "dd_name": "TCATBALF", "dataset": "AWS.M2.CARDDEMO.TCATBAL", "line": 15}
  ],
  "procs_called": [],
  "datasets_referenced": [
    "AWS.M2.CARDDEMO.TCATBAL",
    "AWS.M2.CARDDEMO.XREFFILE"
  ]
}
```

---

## Test Case: AWS CardDemo

### Repository
`https://github.com/aws-samples/aws-mainframe-modernization-carddemo`

### Description
A credit card management application built by AWS for mainframe modernization testing. Contains realistic COBOL programs, copybooks, JCL, and CICS transactions.

### Structure
```
aws-mainframe-modernization-carddemo/
├── app/
│   ├── cbl/              # COBOL programs (~50+)
│   │   ├── CBACT01C.cbl  # Account processing
│   │   ├── CBACT02C.cbl  # Account update
│   │   ├── CBACT03C.cbl  # Account inquiry
│   │   ├── CBACT04C.cbl  # Interest calculator
│   │   ├── CBSTM03B.cbl  # Statement subroutine
│   │   ├── COSGN00C.cbl  # Sign-on (CICS)
│   │   ├── COCRDLIC.cbl  # Credit card list (CICS)
│   │   └── ...
│   ├── cpy/              # Copybooks
│   │   ├── CVACT01Y.cpy  # Account record layout
│   │   ├── CVACT02Y.cpy  # Transaction record
│   │   └── ...
│   ├── bms/              # CICS screen maps
│   │   └── ...
│   └── jcl/              # Job control
│       └── ...
├── data/                 # Sample data files
└── samples/              # Sample JCL templates
```

### Test Programs (Recommended Order)

**Phase 1: Simple Batch**
1. `CBACT04C.cbl` - Interest calculator (batch, file I/O, calls subroutine)
2. `CBSTM03B.cbl` - Statement subroutine (called by CBACT04C)

**Phase 2: Batch with DB2**
3. `CBACT01C.cbl` - Account processing (batch, DB2)
4. `CBACT02C.cbl` - Account update (batch, DB2)

**Phase 3: CICS Online**
5. `COSGN00C.cbl` - Sign-on program (CICS, maps)
6. `COCRDLIC.cbl` - Credit card list (CICS, file browse)

**Phase 4: JCL**
7. Sample JCL from `samples/` directory

### Success Criteria

| Metric | Target |
|--------|--------|
| Template completion | 100% of sections filled or marked UNKNOWN |
| Citation accuracy | 90%+ of claims have valid line citations |
| Challenger convergence | SOLID assessment on Purpose, Inputs, Outputs |
| Iteration efficiency | Average < 2 iterations to WITNESSED |
| Business rule extraction | At least 1 rule per program |

---

## Configuration

### War Rig Configuration
```yaml
war_rig:
  rig_id: "ALPHA"
  
  scribe:
    model: "claude-sonnet-4-20250514"
    temperature: 0.3
    max_tokens: 4000
  
  challenger:
    model: "claude-sonnet-4-20250514"
    temperature: 0.5
    max_tokens: 2000
  
  imperator:
    model: "claude-sonnet-4-20250514"
    temperature: 0.2
    max_tokens: 2000
  
  max_iterations: 3
  max_questions_per_round: 5
  max_chrome_tickets: 5
```

### System Configuration
```yaml
system:
  input_directory: "./aws-mainframe-modernization-carddemo/app"
  output_directory: "./output"
  checkpoint_frequency: "per_program"
  
  preprocessing:
    extract_paragraphs: true
    extract_performs: true
    extract_calls: true
    extract_copybooks: true
    extract_sql: true
    extract_cics: true
  
  file_extensions:
    cobol: [".cbl", ".cob", ".CBL", ".COB"]
    copybook: [".cpy", ".CPY", ".copy", ".COPY"]
    jcl: [".jcl", ".JCL"]
    bms: [".bms", ".BMS"]
```

---

## File Structure

```
output/
├── analysis/
│   ├── inventory.json              # All files categorized
│   ├── cobol_structures.json       # Preprocessor output
│   ├── jcl_structures.json
│   └── copybook_structures.json
│
├── war_rig/
│   ├── iterations/
│   │   ├── CBACT04C_iter1.json     # Scribe's first draft
│   │   ├── CBACT04C_iter1_challenge.json  # Challenger's questions
│   │   ├── CBACT04C_iter2.json     # Updated draft
│   │   └── CBACT04C_final.json     # Final approved
│   ├── chrome/
│   │   └── CBACT04C_chrome.json    # Any Chrome tickets
│   └── dialogues/
│       └── CBACT04C_dialogue.json  # Full Scribe-Challenger exchange
│
├── valhalla/
│   ├── CBACT04C.json               # Exceptional quality
│   └── ...
│
├── final/
│   ├── programs/
│   │   ├── CBACT04C.json           # Final documentation
│   │   └── CBACT04C.md             # Human-readable version
│   ├── copybooks/
│   │   └── CVACT01Y.json
│   ├── jcl/
│   │   └── ...
│   └── index.md                    # Summary of all documented programs
│
└── metrics/
    ├── iteration_counts.json
    ├── challenger_assessments.json
    └── coverage.json
```

---

## Metrics

### Per-Program Metrics
- iterations_to_witnessed: How many cycles before approval
- challenger_final_assessment: SOLID/SHAKY/WRONG counts by section
- questions_asked: Total questions from Challenger
- questions_resolved: Questions that led to template updates
- chrome_tickets: Number of Imperator tickets
- confidence_score: Final assessed confidence

### System-Wide Metrics
- total_programs_documented: Count
- average_iterations: Mean cycles to approval
- challenger_effectiveness: % of questions that improved documentation
- coverage: % of inventory successfully documented
- valhalla_rate: % achieving exceptional quality

---

## Error Handling

### Agent Failures
- If Scribe produces invalid template: Challenger flags, Imperator issues Chrome
- If Scribe exceeds token limit: Split into sections
- If Challenger fails: Skip validation, Imperator reviews directly
- If Imperator fails: Default to WITNESSED after max iterations

### Recovery
- All state persisted after each step
- Resume capability from any checkpoint
- Failed programs logged for retry

---

## Appendix A: Naming Reference

| Term | Role | Description |
|------|------|-------------|
| War Rig | Team | Complete documentation unit |
| War Boy | Worker Agent | Generic term for Scribe or Challenger |
| Scribe | Documenter | Fills the template |
| Challenger | Validator | Questions and validates |
| Imperator | Reviewer | Approves or rejects |
| Chrome | Refinement Ticket | Major issue to fix |
| Witnessed | Approved | Documentation accepted |
| Valhalla | Exceptional | Outstanding quality |

---

## Appendix B: Future Extensions

### Multi-Team Mode (Deferred)
- Multiple War Rigs process same program independently
- Thunderdome agent compares outputs
- Consensus = high confidence
- Conflicts = Warrants for human review

### Human-in-the-Loop
- Warrants queued for SME review
- SME responses fed back to improve future runs

### Cross-Reference Pass
- After all programs documented:
  - Populate "called_by" fields
  - Build program dependency graph
  - Link copybooks to programs
  - Generate system overview
