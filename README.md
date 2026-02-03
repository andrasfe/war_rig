# War Rig: Mainframe Documentation System

> **Note**: This is a personal project for self-educating myself on agentic coding techniques. It is in no way related to any work I perform with my employer.

A multi-agent system for documenting legacy mainframe codebases (COBOL, PL/I, JCL, Assembler, REXX, and more) using parallel worker pools and ticket-based orchestration.

## Overview

War Rig uses AI agents to automatically document legacy COBOL code:

- **Scribe**: Reads source files and generates structured documentation templates
- **Challenger**: Reviews documentation and raises questions or issues
- **Imperator**: Makes final approval decisions in batch reviews

A ticket system coordinates the workflow, passing files through documentation → validation → refinement cycles until the Imperator grants final approval.

## Architecture

### Ticket-Based Workflow

```
                    ┌─────────────────┐
                    │  DOCUMENTATION  │  Scribe analyzes source file
                    │     ticket      │  and fills documentation template
                    └────────┬────────┘
                             │
                             ▼
                    ┌─────────────────┐
                    │   VALIDATION    │  Challenger reviews the
                    │     ticket      │  documentation for issues
                    └────────┬────────┘
                             │
              ┌──────────────┼──────────────┐
              │              │              │
              ▼              ▼              ▼
     ┌─────────────┐  ┌───────────┐  ┌─────────────┐
     │CLARIFICATION│  │  CHROME   │  │   Approved  │
     │   ticket    │  │  ticket   │  │  (no issues)│
     └──────┬──────┘  └─────┬─────┘  └─────────────┘
            │               │
            └───────┬───────┘
                    │
                    ▼
            Back to Scribe
           (address issues)
                    │
                    ▼
            ┌───────────────┐
            │   VALIDATION  │  Re-validate after fixes
            └───────────────┘
```

### Batch Processing with Worker Pools

```
         ┌──────────────────────────────────────────────┐
         │         Create tickets for all files         │
         └──────────────────────────────────────────────┘
                              │
         ┌────────────────────┼────────────────────┐
         │                    │                    │
         ▼                    ▼                    ▼
   ┌───────────┐        ┌───────────┐        ┌───────────┐
   │ Scribe #1 │        │ Scribe #2 │        │ Scribe #3 │
   │  (async)  │        │  (async)  │        │  (async)  │
   └───────────┘        └───────────┘        └───────────┘
         │                    │                    │
         └────────────────────┼────────────────────┘
                              │
         ┌────────────────────┼────────────────────┐
         │                    │                    │
         ▼                    ▼                    ▼
   ┌─────────────┐      ┌─────────────┐
   │Challenger #1│      │Challenger #2│
   │   (async)   │      │   (async)   │
   └─────────────┘      └─────────────┘
                              │
                              ▼
         ┌──────────────────────────────────────────────┐
         │         HOLISTIC_REVIEW (Imperator)          │
         │      Batch review of all documentation       │
         └──────────────────────────────────────────────┘
                              │
              ┌───────────────┼───────────────┐
              ▼               ▼               ▼
        ┌──────────┐   ┌──────────┐   ┌──────────┐
        │WITNESSED │   │ VALHALLA │   │  FORCED  │
        │(approved)│   │(excellent)│  │(max iter)│
        └──────────┘   └──────────┘   └──────────┘
```

### Ticket Types

| Type | Description |
|------|-------------|
| **DOCUMENTATION** | Initial documentation task for a source file |
| **VALIDATION** | Challenger reviews completed documentation |
| **CLARIFICATION** | Question from Challenger requiring Scribe response |
| **CHROME** | Issue from review requiring Scribe rework |
| **HOLISTIC_REVIEW** | Batch review task for Imperator |
| **SYSTEM_OVERVIEW** | Final system overview generation |

### Final Decisions (Imperator)

Named after *Mad Max: Fury Road* terminology:

| Decision | Meaning |
|----------|---------|
| **WITNESSED** | Documentation approved - meets quality standards |
| **VALHALLA** | Exceptional quality - above and beyond expectations |
| **CHROME** | Issues found, needs rework (creates CHROME tickets) |
| **FORCED** | Approved after max iterations despite remaining issues |

## Supported File Types

War Rig supports documentation of various mainframe file types:

| File Type | Description | Extensions |
|-----------|-------------|------------|
| **COBOL** | COBOL programs | .cbl, .cob |
| **PLI** | PL/I programs | .pli, .pl1 |
| **JCL** | Job Control Language | .jcl |
| **PROC** | JCL procedures | .proc, .prc |
| **COPYBOOK** | COBOL copybooks | .cpy, .copy |
| **BMS** | CICS screen maps | .bms |
| **ASM** | Assembler/HLASM | .asm, .hlasm |
| **REXX** | REXX scripts | .rexx, .rex, .exec |
| **CLIST** | TSO command lists | .clist |
| **NATURAL** | Software AG 4GL | .nsp, .nsn |
| **EASYTRIEVE** | Report generator | .ezt, .ezy |
| **SORT** | DFSORT control cards | .srt, .sort |
| **DDL** | DB2 database definitions | .ddl, .sql |
| **DB2** | DB2 DDL files | .db2 |
| **IMS** | IMS DBD/PSB definitions | .dbd, .psb |
| **DATACARD** | Utility control cards | .dc |

### Datacards (Utility Control Cards)

Datacard files (.dc) contain control statements for mainframe utilities. War Rig auto-detects the utility type and generates a consolidated catalog:

| Utility Type | Description |
|--------------|-------------|
| **DB2 UNLOAD/LOAD** | Database unload/load operations |
| **IDCAMS REPRO** | Copy sequential/VSAM datasets |
| **IDCAMS DEFINE** | Create VSAM cluster/GDG |
| **IDCAMS DELETE** | Delete dataset/cluster |
| **DFSORT** | Sort/merge records |
| **ICETOOL** | Advanced data transformation |
| **IEBCOPY** | Copy PDS members |
| **IEBGENER** | Copy sequential datasets |

Unlike other file types, datacards produce a single consolidated `DATACARDS.md` catalog rather than individual documentation files.

Each file type has a tailored validation skip matrix - the system knows which documentation sections are applicable for each type (e.g., COPYBOOK files skip `called_programs` and `data_flow` validation since they don't execute code).

## Installation

```bash
# Install everything (war_rig + citadel) with dev dependencies
./scripts/setup.sh

# Production only (no dev dependencies)
./scripts/setup.sh --prod
```

Requires [uv](https://docs.astral.sh/uv/) for package management.

## Configuration

Create a `.env` file:

```bash
# API Provider
API_PROVIDER=openrouter
OPENROUTER_API_KEY=sk-or-v1-your-key-here

# Model Assignments
SCRIBE_MODEL=anthropic/claude-sonnet-4-20250514
CHALLENGER_MODEL=openai/gpt-4o-2024-11-20
IMPERATOR_MODEL=anthropic/claude-sonnet-4-20250514

# Worker Pools
NUM_SCRIBES=3
NUM_CHALLENGERS=2

# Workflow Settings
MAX_ITERATIONS=3
PM_MAX_CYCLES=5

# Error Handling
EXIT_ON_ERROR=true        # Stop on first error (default: true)
MAX_TICKET_RETRIES=5      # Max retries per ticket before fatal exit (default: 5)

# Paths
INPUT_DIRECTORY=./input
OUTPUT_DIRECTORY=./output
```

### Exit on Error Mode

When `EXIT_ON_ERROR=true` (the default), War Rig will stop processing immediately when any error occurs. This is useful for debugging and ensuring issues are addressed before continuing. Set to `false` for batch processing where you want to continue despite individual file failures.

### Citadel-Guided Processing (Large Files)

For large COBOL files, War Rig uses Citadel-guided batched processing to document paragraphs in smaller chunks:

```bash
# Thresholds for triggering batched processing
CITADEL_GUIDED_THRESHOLD_LINES=2000      # Files > this use batched processing (default: 2000)
CITADEL_GUIDED_THRESHOLD_PARAGRAPHS=15   # Files > this use batched processing (default: 15)

# Batch size control
CITADEL_MAX_PARAGRAPHS_PER_BATCH=10      # Max paragraphs per LLM call (default: 10)
```

**When to adjust these settings:**

| Setting | When to Change |
|---------|----------------|
| `CITADEL_GUIDED_THRESHOLD_*` | Lower to force batching on smaller files; raise to process more in single pass |
| `CITADEL_MAX_PARAGRAPHS_PER_BATCH` | Lower for slower models (o3: try 5); raise for faster models with large context |

**Reasoning models (o3, o1):** These models are slower and may timeout or produce incomplete output with large batches. Set `CITADEL_MAX_PARAGRAPHS_PER_BATCH=5` for best results. Temperature is automatically set to 1.0 for these models (required by the API).

### Call Semantics and Minion Scribes

Call semantics analysis infers **data flow between paragraphs** - what variables are passed into each PERFORM call and what gets modified. This requires additional LLM calls per file.

For large files with many PERFORM calls, this can mean 20+ sequential LLM calls. To speed this up, War Rig uses a **Minion Scribe Pool** - a pool of fast, cheap LLMs (Haiku by default) that process call semantics batches in parallel.

```bash
# Enable/disable call semantics (default: true)
ENABLE_CALL_SEMANTICS=true

# Minion scribe pool configuration
MINION_SCRIBE_MODEL=anthropic/claude-3-haiku-20240307  # Fast, cheap model
NUM_MINION_SCRIBES=4                                    # Parallel workers (1-10)
MINION_SCRIBE_BATCH_SIZE=5                              # Call edges per LLM request
```

**Architecture:**

```
ScribeWorkerPool
    └── MinionScribePool (shared across all ScribeWorkers)
            │
    ┌───────┼───────┬───────┐
    │       │       │       │
  Minion1 Minion2 Minion3 Minion4  (process batches in parallel)
```

**Sub-ticket tracking:** Each call semantics batch creates a `CALL_SEMANTICS` sub-ticket linked to the parent `DOCUMENTATION` ticket. This provides:
- Full observability of minion work
- Retry capability for failed batches
- Duration and token usage tracking per batch

**Caching for resume:** Call semantics results are cached to `output/cache/call_semantics/{PROGRAM_ID}.json`. If processing crashes and you re-run, cached results are loaded automatically, skipping the LLM calls. To clear the cache: `rm -rf output/cache/call_semantics/`

**Output:** Call semantics appear in the markdown output as an **Inter-Paragraph Data Flow** table:

```markdown
## Inter-Paragraph Data Flow

| Caller | Callee | Inputs | Outputs | Purpose |
|--------|--------|--------|---------|---------|
| 0100-MAIN | 2000-VALIDATE | WS-ACCT-ID | WS-STATUS | Validates account |
```

**When to disable:** Set `ENABLE_CALL_SEMANTICS=false` if you:
- Don't need data flow analysis between paragraphs
- Want faster processing with fewer LLM calls
- Are hitting rate limits

You still get paragraph documentation, call graphs, and all other analysis - just without the "what data flows in/out of each PERFORM" detail.

### Challenger Paragraph Sampling

For large files with many paragraphs, Challenger validation can consume 40-50K tokens. To reduce this, Challenger samples a subset of paragraphs using a hybrid strategy:

1. **Entry point** - Always includes main entry (0000-/0100-/MAIN)
2. **High-complexity** - Paragraphs with SQL, CICS, or many calls
3. **Random** - Additional random paragraphs for coverage

```bash
# Number of paragraphs to sample for validation (default: 5)
CHALLENGER_PARAGRAPH_SAMPLE_SIZE=5
```

This reduces Challenger context from ~48K to ~5K tokens (~90% reduction). The Challenger validates the sampled paragraphs deeply, and any issues found are fixed by the Scribe for those paragraphs only.

**Trade-off:** Issues in non-sampled paragraphs won't be caught. For critical files, increase the sample size or set to a high value to validate more.

### Max Ticket Retries (Endless Loop Prevention)

The `MAX_TICKET_RETRIES` setting (default: 5) prevents endless retry loops when a ticket repeatedly fails to process. This safeguard applies to:

1. **Normal Scribe failures**: When a ticket fails twice (initial + strict formatting retry), it gets reset for other workers
2. **Super-Scribe escalation**: When all regular Scribes fail, blocked tickets are escalated to Super-Scribe (Opus model)
3. **Retry tracking**: Each rescue attempt increments the ticket's `retry_count` in metadata

When a ticket exceeds `MAX_TICKET_RETRIES` and `EXIT_ON_ERROR=true`:
- The system raises `MaxTicketRetriesExceeded` with details about the failing ticket
- Processing stops immediately to prevent resource waste on a persistently failing file
- The error message includes the ticket ID, file name, and retry count for debugging

This is particularly useful for catching:
- Malformed source files that no LLM can parse correctly
- Edge cases in the documentation template schema
- Persistent API or network issues affecting specific files

Set `MAX_TICKET_RETRIES` higher (up to 20) for more tolerance, or set `EXIT_ON_ERROR=false` to continue processing other files despite retry limit violations.

## Usage

### Analyze a Single File

```bash
war-rig analyze path/to/PROGRAM.cbl

# Or run as a module
python -m war_rig analyze path/to/PROGRAM.cbl
```

### Process a Directory

```bash
war-rig batch path/to/source/directory
war-rig batch path/to/source/ --type COBOL --limit 10

# Resume a batch run (skip already-completed files)
war-rig batch path/to/source/ --resume
```

### Process Datacards

Generate a consolidated catalog of utility control cards (.dc files):

```bash
# Process datacards standalone
war-rig datacards path/to/source/

# With custom output directory
war-rig datacards path/to/source/ --output ./docs
```

Datacards are also automatically processed during `batch` runs.

### Generate Agent Skills

Convert War Rig documentation into [Agent Skills format](https://agentskills.io/specification) for progressive AI agent discovery:

```bash
# Generate skills from documentation output
war-rig skills ./output

# Custom output directory
war-rig skills ./output --output ./my-skills

# Generate skills automatically during batch processing
war-rig batch ./source --skills

# With custom skills output directory
war-rig batch ./source --skills --skills-output ./my-skills
```

**Output Structure:**
```
skills-output/
├── system-overview/
│   └── SKILL.md              # Index skill - start here for discovery
├── call-graph/
│   └── SKILL.md              # Program call relationships
├── datacards/
│   └── SKILL.md              # Utility control cards (if present)
└── programs/
    ├── cbact01c/
    │   ├── SKILL.md          # Summary, business rules, when to use
    │   └── references/
    │       └── REFERENCE.md  # Full technical details
    └── ...
```

**What Each Skill Contains:**
- **system-overview**: High-level system description, program catalog grouped by type, architecture diagram
- **call-graph**: Mermaid diagram showing program call relationships
- **program skills**: Purpose, business rules, inputs/outputs, copybooks, "When to Use" guidance
- **references**: Detailed paragraphs, data flow, error handling, SQL/CICS operations

Skills follow the Agent Skills spec for progressive disclosure - agents load metadata first (~100 tokens), then full instructions (<5000 tokens), then references as needed.

### Process with Parallel Workers

```bash
python scripts/run_carddemo.py
python scripts/run_carddemo.py --num-scribes 5 --num-challengers 3
```

### Monitor Processing Status

```bash
# Real-time status dashboard with live updates
python scripts/war_rig_status.py output/.war_rig_tickets.json

# Single snapshot (no live updates)
python scripts/war_rig_status.py output/.war_rig_tickets.json --once
```

### Manage Tickets

```bash
# List all tickets
python scripts/ticket_manager.py output/.war_rig_tickets.json list

# List blocked tickets only
python scripts/ticket_manager.py output/.war_rig_tickets.json list --state=blocked

# Show ticket details
python scripts/ticket_manager.py output/.war_rig_tickets.json show mem-000001

# Reset a blocked ticket to retry
python scripts/ticket_manager.py output/.war_rig_tickets.json reset mem-000042

# View statistics
python scripts/ticket_manager.py output/.war_rig_tickets.json stats
```

### Human-in-the-Loop Feedback

Inject human feedback into pending tickets to guide agent behavior:

```bash
python scripts/human_feedback.py output/.war_rig_tickets.json
```

**Features:**
- Shows current Imperator feedback before prompting for human input
- Injects feedback only into `CREATED` state tickets (not yet claimed by agents)
- Human feedback overrides Imperator feedback on conflicts
- Supports multiple feedback categories and severity levels

**Interactive Menu:**
```
=== War Rig Human Feedback Console ===

Found 5 CREATED tickets ready for feedback injection.

[1] Add quality note        - Add feedback with category/severity
[2] Prioritize file(s)      - Move specific files to front of queue
[3] Skip/cancel file(s)     - Cancel tickets for specific files
[4] Override critical sections
[5] Add global instructions

[6] View Imperator feedback - See current AI feedback for a ticket
[7] View pending queue      - See queued human feedback

[8] Inject feedback and exit
[q] Quit without injecting
```

**Feedback Categories:**
| Category | Description |
|----------|-------------|
| `quality_issue` | Document quality problem to address |
| `domain_context` | Business/domain knowledge agents should know |
| `instruction` | Specific instruction for agents to follow |

**Severity Levels:** `critical`, `high`, `medium`, `low`

**Example Workflow:**
1. Start the feedback console
2. View current Imperator feedback for context
3. Add domain knowledge (e.g., "CUST-BALANCE uses COMP-3 packed decimal")
4. Prioritize specific files that need attention first
5. Skip test stub files that shouldn't be documented
6. Inject feedback - agents will see it when they claim tickets

## Output Structure

```
output/
├── PROGRAM.doc.json       # Documentation template (JSON)
├── PROGRAM.doc.md         # Human-readable documentation
├── DATACARDS.md           # Consolidated datacard catalog (if .dc files present)
├── SYSTEM_OVERVIEW.md     # System-wide overview (generated after batch)
├── analysis/              # Preprocessor outputs
└── metrics/               # Processing metrics
```

## Development

```bash
# Run tests
uv run pytest tests/ -x

# Type checking
uv run mypy war_rig

# Linting
uv run ruff check .

# Formatting
uv run ruff format .
```

## Project Structure

```
war_rig/
├── __main__.py         # Module entry point (python -m war_rig)
├── agents/             # AI agent implementations
│   ├── scribe.py       # Documentation generation
│   ├── challenger.py   # Documentation validation
│   └── imperator.py    # Final review decisions
├── workers/            # Parallel worker pools
│   ├── scribe_pool.py
│   └── challenger_pool.py
├── models/             # Pydantic data models
│   ├── templates.py    # Documentation templates
│   └── assessments.py  # Validation enums with case-insensitive handling
├── feedback/           # Human feedback injection
│   ├── models.py       # HumanFeedbackNote, HumanFeedbackContext
│   └── injector.py     # FeedbackInjector for ticket updates
├── sampling/           # Intelligent source code sampling
│   ├── analyzer.py     # RelevanceAnalyzer for hint extraction
│   ├── composer.py     # WindowComposer for merging windows
│   ├── orchestrator.py # Main entry point
│   └── strategies/     # Sampling strategies (5 implementations)
├── processors/         # Special file type processors
│   └── datacard.py     # Datacard catalog generator
├── skills/             # Agent Skills format converter
│   ├── generator.py    # Main SkillsGenerator class
│   ├── naming.py       # Skill name normalization
│   ├── program_skill.py    # Program documentation → skill
│   ├── overview_skill.py   # System overview → index skill
│   ├── call_graph_skill.py # Call graph → skill
│   └── datacard_skill.py   # Datacards → skill
├── orchestration/      # Workflow coordination
│   └── ticket_engine.py
├── utils/              # Utility modules
│   └── file_lock.py    # File locking for concurrent workers
├── beads.py            # Ticket tracking system
└── cli.py              # Command-line interface

scripts/
├── setup.sh            # Install all projects (war_rig + citadel)
├── generate_skills.py  # Standalone Agent Skills generator
├── human_feedback.py   # Human-in-the-loop feedback injection
├── ticket_manager.py   # CLI for managing tickets
├── war_rig_status.py   # Real-time status monitor
└── run_carddemo.py     # CardDemo batch processor

citadel/                # Dependency graph extraction tool (path dependency)
├── src/citadel/        # Source code
└── tests/              # Tests
```

## Technical Features

### File Locking for Concurrent Workers

When running multiple Scribe or Challenger workers in parallel, War Rig uses a centralized file locking mechanism (`war_rig/utils/file_lock.py`) to prevent race conditions. Features include:

- Coroutine-level synchronization using `asyncio.Lock` (workers run as async tasks)
- **Lock-before-claim pattern**: Workers check file lock availability before claiming tickets, reducing wasteful claim-release churn
- Automatic lock expiration (default 5 minutes) for crash recovery
- Locks identified by normalized absolute file paths
- Fallback skip tracking for rare race conditions

**Lock ordering (deadlock prevention):**
1. FileLockManager per-file locks (checked first)
2. BeadsClient ticket state locks
3. BeadsClient persistence locks

### Case-Insensitive Enum Handling

LLM outputs sometimes return enum values with inconsistent casing. War Rig's assessment enums (`ConfidenceLevel`, `ValidationLevel`) implement `_missing_()` to handle case-insensitive matching, ensuring robust parsing of AI responses.

### Validation Skip Matrix

Different file types have different applicable documentation sections. The validation system maintains a skip matrix that knows which sections to skip for each file type:

| File Type | Skipped Sections | Kept Sections |
|-----------|------------------|---------------|
| **COBOL/PLI** | (none) | All sections validated |
| **COPYBOOK** | `called_programs`, `data_flow`, `cics_operations`, `sql_operations`, `business_rules`, `error_handling` | `purpose`, `inputs`, `outputs`, `copybooks` |
| **JCL/PROC** | `called_programs`, `copybooks`, `cics_operations`, `sql_operations`, `data_flow`, `business_rules`, `error_handling` | `purpose`, `inputs`, `outputs` |
| **BMS** | `called_programs`, `data_flow`, `copybooks`, `sql_operations`, `cics_operations`, `business_rules`, `error_handling` | `purpose`, `inputs`, `outputs` |
| **LISTING** | All except `purpose` | `purpose` only |
| **ASM** | `copybooks`, `cics_operations`, `sql_operations`, `business_rules`, `error_handling` | `purpose`, `inputs`, `outputs`, `data_flow`, `called_programs` |
| **REXX** | `copybooks`, `cics_operations`, `sql_operations`, `business_rules`, `error_handling` | `purpose`, `inputs`, `outputs`, `data_flow`, `called_programs` |
| **CLIST** | `copybooks`, `cics_operations`, `sql_operations`, `data_flow`, `business_rules`, `error_handling`, `called_programs` | `purpose`, `inputs`, `outputs` |
| **NATURAL** | `copybooks`, `cics_operations`, `sql_operations`, `error_handling` | `purpose`, `inputs`, `outputs`, `data_flow`, `business_rules`, `called_programs` |
| **EASYTRIEVE** | `copybooks`, `cics_operations`, `called_programs`, `business_rules`, `error_handling`, `data_flow` | `purpose`, `inputs`, `outputs`, `sql_operations` |
| **SORT** | `called_programs`, `copybooks`, `cics_operations`, `sql_operations`, `business_rules`, `error_handling`, `data_flow` | `purpose`, `inputs`, `outputs` |
| **DDL** | `called_programs`, `copybooks`, `cics_operations`, `data_flow`, `business_rules`, `error_handling` | `purpose`, `inputs`, `outputs`, `sql_operations` |
| **IMS** | `called_programs`, `copybooks`, `cics_operations`, `sql_operations`, `data_flow`, `business_rules`, `error_handling` | `purpose`, `inputs`, `outputs` |
| **OTHER** | `called_programs`, `data_flow`, `copybooks`, `sql_operations`, `cics_operations`, `business_rules`, `error_handling` | `purpose`, `inputs`, `outputs` |

This prevents false validation failures (and endless loops) when sections are legitimately empty for certain file types.

### Human Feedback Injection

The human feedback system allows operators to inject domain knowledge and instructions into pending tickets:

- **Feedback merging**: Human feedback is prepended to existing Imperator feedback, with human overriding on conflicts
- **Selective targeting**: Feedback can target all tickets, specific files, or specific template sections
- **Priority control**: Files can be prioritized (moved to front of queue) or skipped (cancelled)
- **Source tracking**: Human notes are marked with `source: "human"` for traceability
- **Atomic updates**: Ticket file updates use atomic writes (temp file + rename) for safety

The feedback uses the same `FeedbackContext` structure that agents already read, requiring no agent code changes.

### Per-Document Type Validation

Per-document type validation ensures documentation meets type-specific structural requirements before LLM review. The validation system validates JCL, COBOL, Copybook, PLI, BMS, and PROC documents against specific criteria, creating actionable feedback when documentation is incomplete.

**Key Validation Criteria by Type:**

| File Type | Key Criteria |
|-----------|--------------|
| **JCL** | EXEC steps documented with purpose, DD statements with dataset info, job flow explanation, input/output dependencies |
| **COBOL** | Procedure division paragraphs, CALL statements with parameters, data flow tracing, copybook usage |
| **Copybook** | Field descriptions with PIC clauses, usage context (file record, commarea, etc.), key field identification |
| **PLI** | Procedures with parameters, external CALLs, data structure declarations |
| **BMS** | Screen purpose and transaction context, field positions and attributes |
| **PROC** | Procedure steps with reusability focus, symbolic parameter documentation |

**Priority Levels:**
- `CRITICAL`: Must be present for documentation to be useful
- `HIGH`: Should be present for complete documentation
- `MEDIUM`: Nice to have, improves documentation quality

**Integration:** Validation runs automatically during the Imperator holistic review phase. Failed criteria generate ChromeTickets with specific guidance for the Scribe to address.

**Location:** Validation modules are in `war_rig/validation/` (`document_criteria.py` for criteria definitions, `document_validator.py` for validation logic).

### Intelligent Source Sampling

When source code exceeds token limits for CLARIFICATION/CHROME tickets, War Rig uses intelligent sampling to select relevant portions based on the questions being asked:

**Strategy Chain (priority order):**

| Priority | Strategy | Triggers | Relevance |
|----------|----------|----------|-----------|
| 1 | LineCitation | Line numbers in `evidence` or question text | 1.0 |
| 2 | SectionReference | `section` field → lookup template citations | 0.9 |
| 3 | IdentifierMention | COBOL identifiers in question text → grep source | 0.7 |
| 4 | SemanticRegion | Topic inference → COBOL division boundaries | 0.6 |
| 5 | RandomFallback | No hints found (last resort) | 0.1 |

**How it works:**
1. `RelevanceAnalyzer` extracts hints from challenger questions (line numbers, sections, identifiers)
2. Each applicable strategy generates `SourceWindow` objects pointing to relevant code regions
3. `WindowComposer` merges overlapping windows, prioritizes by relevance, fits within token budget
4. Result includes non-contiguous windows with context margins (±5 lines)

**Example:** If Challenger asks "Why is WS-ACCOUNT-BALANCE set incorrectly at line 456?", the system:
- LineCitationStrategy creates a window around line 456
- IdentifierMentionStrategy searches for "WS-ACCOUNT-BALANCE" throughout the source
- Both windows are merged and included in the sample

This ensures the LLM sees the relevant code to answer questions, rather than random portions.

### Custom LLM Providers

War Rig uses the `llm-providers` package for LLM connectivity. You can add custom providers by implementing the `LLMProvider` protocol.

**1. Implement the Protocol**

```python
from llm_providers import LLMProvider, Message, CompletionResponse

class MyCustomProvider:
    """Custom provider for my LLM backend."""

    def __init__(self, api_key: str, default_model: str = "my-model"):
        self._api_key = api_key
        self._default_model = default_model

    @property
    def default_model(self) -> str:
        return self._default_model

    async def complete(
        self,
        messages: list[Message],
        model: str | None = None,
        temperature: float = 0.7,
        **kwargs,
    ) -> CompletionResponse:
        resolved_model = model or self._default_model

        # Your API call logic here
        content = await self._call_api(messages, resolved_model, temperature)

        return CompletionResponse(
            content=content,
            model=resolved_model,
            tokens_used=0,  # Optional: track token usage
            raw_response={},  # Optional: include raw API response
        )
```

**2. Register Your Provider**

```python
from llm_providers import register_provider

# Register at runtime
register_provider("myprovider", MyCustomProvider)

# Now use it
provider = create_provider("myprovider", api_key="...")
```

**3. Plugin Registration (Entry Points)**

For automatic discovery, register via `pyproject.toml`:

```toml
[project.entry-points."llm_providers.providers"]
myprovider = "mypackage.provider:MyCustomProvider"
```

The provider will be automatically available:

```python
from llm_providers import create_provider, get_available_providers

# See all providers including your custom one
print(get_available_providers())  # ['openrouter', 'anthropic', 'openai', 'myprovider']

# Create your provider
provider = create_provider("myprovider", api_key="...")
```

**4. Use in War Rig**

Set your provider in `.env`:

```bash
API_PROVIDER=myprovider
MYPROVIDER_API_KEY=your-key-here
```

**Built-in Providers:**

| Provider | Models | Key Variable |
|----------|--------|--------------|
| `openrouter` | Any model via OpenRouter | `OPENROUTER_API_KEY` |
| `anthropic` | Claude models direct | `ANTHROPIC_API_KEY` |
| `openai` | GPT models direct | `OPENAI_API_KEY` |

**Streaming Support:**

For best results, implement streaming in your provider to enable stall detection (the system monitors for hung API calls):

```python
async def complete(self, messages, model=None, temperature=0.7, **kwargs):
    # Use streaming for progress visibility
    content_chunks = []
    async for chunk in self._stream_api(messages, model, temperature):
        content_chunks.append(chunk)
    return CompletionResponse(content="".join(content_chunks), ...)
```

## License

MIT License
