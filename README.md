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
| **IMS** | IMS DBD/PSB definitions | .dbd, .psb |

Each file type has a tailored validation skip matrix - the system knows which documentation sections are applicable for each type (e.g., COPYBOOK files skip `called_programs` and `data_flow` validation since they don't execute code).

## Installation

```bash
# Install in development mode
pip install -e .

# Install with dev dependencies
pip install -e ".[dev]"
```

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
EXIT_ON_ERROR=true  # Stop on first error (default: true)

# Paths
INPUT_DIRECTORY=./input
OUTPUT_DIRECTORY=./output
```

### Exit on Error Mode

When `EXIT_ON_ERROR=true` (the default), War Rig will stop processing immediately when any error occurs. This is useful for debugging and ensuring issues are addressed before continuing. Set to `false` for batch processing where you want to continue despite individual file failures.

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

## Output Structure

```
output/
├── PROGRAM.doc.json       # Documentation template (JSON)
├── PROGRAM.doc.md         # Human-readable documentation
├── analysis/              # Preprocessor outputs
└── metrics/               # Processing metrics
```

## Development

```bash
# Run tests
pytest

# Type checking
mypy war_rig

# Linting
ruff check war_rig
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
├── orchestration/      # Workflow coordination
│   └── ticket_engine.py
├── utils/              # Utility modules
│   └── file_lock.py    # File locking for concurrent workers
├── beads.py            # Ticket tracking system
└── cli.py              # Command-line interface

scripts/
├── ticket_manager.py   # CLI for managing tickets
├── war_rig_status.py   # Real-time status monitor
└── run_carddemo.py     # CardDemo batch processor
```

## Technical Features

### File Locking for Concurrent Workers

When running multiple Scribe or Challenger workers in parallel, War Rig uses a centralized file locking mechanism (`war_rig/utils/file_lock.py`) to prevent race conditions. Features include:

- Thread-safe lock management using `asyncio.Lock`
- Automatic lock expiration (default 5 minutes) for crash recovery
- Workers check file availability before claiming tickets
- Locks identified by normalized absolute file paths

### Case-Insensitive Enum Handling

LLM outputs sometimes return enum values with inconsistent casing. War Rig's assessment enums (`ConfidenceLevel`, `ValidationLevel`) implement `_missing_()` to handle case-insensitive matching, ensuring robust parsing of AI responses.

### Validation Skip Matrix

Different file types have different applicable documentation sections. The validation system maintains a skip matrix that knows which sections to skip for each file type:

- **COBOL/PLI**: Full validation (no sections skipped)
- **COPYBOOK**: Skip `called_programs`, `data_flow`, `cics_operations`, `sql_operations`, `business_rules`, `error_handling`
- **JCL/PROC**: Skip `called_programs`, `copybooks`, `cics_operations`, `sql_operations`, `data_flow`, `business_rules`
- **BMS**: Skip most sections except `purpose`, `inputs`, `outputs`
- **SORT**: Skip most sections except `purpose`, `inputs`, `outputs`
- **DDL**: Skip program-related sections, keep `sql_operations`

This prevents false validation failures when sections are legitimately empty for certain file types.

## License

MIT License
