# War Rig: Mainframe Documentation System

> **Note**: This is a personal project for self-educating myself on agentic coding techniques. It is in no way related to any work I perform with my employer.

A multi-agent system for documenting legacy mainframe codebases (COBOL, PL/I, JCL) using parallel worker pools and ticket-based orchestration.

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

# Paths
INPUT_DIRECTORY=./input
OUTPUT_DIRECTORY=./output
```

## Usage

### Analyze a Single File

```bash
war-rig analyze path/to/PROGRAM.cbl
```

### Process a Directory

```bash
war-rig batch path/to/source/directory
war-rig batch path/to/source/ --type COBOL --limit 10
```

### Process with Parallel Workers

```bash
python scripts/run_carddemo.py
python scripts/run_carddemo.py --num-scribes 5 --num-challengers 3
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
├── agents/             # AI agent implementations
│   ├── scribe.py       # Documentation generation
│   ├── challenger.py   # Documentation validation
│   └── imperator.py    # Final review decisions
├── workers/            # Parallel worker pools
│   ├── scribe_pool.py
│   └── challenger_pool.py
├── models/             # Pydantic data models
│   └── templates.py    # Documentation templates
├── orchestration/      # Workflow coordination
│   └── ticket_engine.py
├── beads.py            # Ticket tracking system
└── cli.py              # Command-line interface
```

## License

MIT License
