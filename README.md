# War Rig: Mainframe Documentation System

> **Note**: This is a personal project for self-educating myself on novel vibe-coding techniques. It is in no way related to any work I perform with my employer.

A multi-agent system for documenting legacy mainframe codebases (COBOL, PL/I, JCL) using LangGraph orchestration and parallel worker pools.

## Overview

War Rig uses multiple AI agents working in parallel to document entire codebases:

- **Scribe Pool**: Multiple Scribes (default 3) analyze source code and fill documentation templates in parallel
- **Challenger Pool**: Multiple Challengers (default 2) validate documentation and request corrections
- **Imperator**: Reviews documentation and issues final verdicts (WITNESSED, VALHALLA, CHROME, FORCED)
- **Super-Scribe**: Automatic escalation using a stronger model (Opus) for tickets that fail on all normal workers

## Architecture

### Per-File Processing (War Boy Pair)

Each file is processed through the Scribe-Challenger-Imperator workflow:

```
    +----------------------------------------------------------+
    |                      WAR BOY PAIR                        |
    |                                                          |
    |    +------------+              +------------+            |
    |    |  SCRIBE    |<------------>| CHALLENGER |            |
    |    |            |   dialogue   |            |            |
    |    +------+-----+              +------+-----+            |
    |           |                           |                  |
    |           +-------------+-------------+                  |
    |                         v                                |
    |                  +--------------+                        |
    |                  |  Draft Doc   |                        |
    |                  +------+-------+                        |
    +-------------------------|--------------------------------+
                              v
                     +----------------+
                     |   IMPERATOR    |
                     |   (Per-File)   |
                     +-------+--------+
                             |
              +--------------+--------------+
              v              v              v
        +----------+   +----------+   +----------+
        |WITNESSED |   |  CHROME  |   | VALHALLA |
        |(approved)|   | (rework) |   |(exceptional)
        +----------+   +----+-----+   +----------+
                            |
                            v
                    Back to Scribe
```

### Batch Processing Flow

```
            +----------------------------------+
            |  Create tickets for all files   |
            +----------------------------------+
                              |
         +--------------------+--------------------+
         |                    |                    |
         v                    v                    v
   +-----------+        +-----------+        +-----------+
   | Scribe #1 |        | Scribe #2 |        | Scribe #3 |
   |  (async)  |        |  (async)  |        |  (async)  |
   +-----------+        +-----------+        +-----------+
         |                    |                    |
         +--------------------+--------------------+
                              |
                              v
            +----------------------------------+
            |     Challenger Validation        |
            +----------------------------------+
                              |
                              v
            +----------------------------------+
            |     Imperator Final Review       |
            +----------------------------------+
                              |
              +--------------+--------------+
              v              v              v
        +----------+   +----------+   +----------+
        |WITNESSED |   |  CHROME  |   | FORCED   |
        | (done)   |   | (cycle)  |   | (max)    |
        +----------+   +----+-----+   +----------+
                            |
                            | Rework cycle
                            +---> Back to Scribes
```

## Installation

### Using Poetry (recommended)

```bash
poetry install
```

### Using pip

```bash
pip install -e .
```

## Configuration

Create a `.env` file (see `.env.example`):

```bash
# API Provider
API_PROVIDER=openrouter
OPENROUTER_API_KEY=sk-or-v1-your-key-here
OPENROUTER_BASE_URL=https://openrouter.ai/api/v1

# Model Assignments (REQUIRED - no defaults)
SCRIBE_MODEL=anthropic/claude-sonnet-4-20250514
CHALLENGER_MODEL=openai/gpt-4o-2024-11-20
IMPERATOR_MODEL=anthropic/claude-sonnet-4-20250514

# Parallel Worker Pools
NUM_SCRIBES=3
NUM_CHALLENGERS=2
NUM_SUPER_SCRIBES=1

# Super-Scribe Model (automatic rescue for blocked tickets)
SUPER_SCRIBE_MODEL=anthropic/claude-opus-4-20250514

# Workflow Settings
MAX_ITERATIONS=3
MAX_QUESTIONS_PER_ROUND=5
MAX_CHROME_TICKETS=5
PM_MAX_CYCLES=5

# Paths
INPUT_DIRECTORY=./aws-mainframe-modernization-carddemo/app
OUTPUT_DIRECTORY=./output
```

## Usage

### Analyze a Single File

```bash
war-rig analyze path/to/PROGRAM.cbl
war-rig analyze path/to/PROGRAM.cbl --verbose
```

### Process a Directory (Sequential)

```bash
war-rig batch path/to/source/directory
war-rig batch path/to/source/ --type COBOL --limit 10
war-rig batch path/to/source/ --resume  # Skip already-completed files
```

### Process with Parallel Workers

```bash
# Default settings (3 scribes, 2 challengers)
python scripts/run_carddemo.py

# Custom worker counts
python scripts/run_carddemo.py --num-scribes 5 --num-challengers 3 --max-cycles 10

# Process specific directory
python scripts/run_carddemo.py --input-dir /path/to/cobol/files
```

### Mock Mode (No API Key)

For testing without an API key:

```bash
war-rig analyze path/to/PROGRAM.cbl --mock
python scripts/run_carddemo.py --mock
```

## AWS CardDemo Integration

War Rig includes integration with the [AWS CardDemo](https://github.com/aws-samples/aws-mainframe-modernization-carddemo) sample mainframe application.

### Setup

```bash
# Clone CardDemo repository
git clone https://github.com/aws-samples/aws-mainframe-modernization-carddemo.git

# Run with mock agents (testing)
python scripts/run_carddemo.py --mock

# Run with real LLM (production)
python scripts/run_carddemo.py
```

## Output Structure

```
output/
├── .war_rig_tickets.json  # Ticket state tracking
├── analysis/              # Preprocessor outputs
├── war_rig/
│   ├── iterations/        # Draft versions
│   ├── chrome/            # Issue tickets
│   └── dialogues/         # Agent exchanges
├── valhalla/              # Exceptional quality docs
├── final/                 # Approved documentation
│   ├── programs/          # Program documentation (.json + .md)
│   ├── copybooks/         # Copybook documentation
│   └── jcl/               # JCL documentation
└── metrics/               # Processing metrics
```

## Documentation Quality Levels (Imperator Decisions)

The Imperator agent reviews documentation and issues one of four verdicts, named after *Mad Max: Fury Road* terminology:

| Decision | Color | Meaning |
|----------|-------|---------|
| **WITNESSED** | Green | Documentation approved - meets quality standards. ("Witness me!" = recognized/approved) |
| **VALHALLA** | Gold | Exceptional quality - above and beyond expectations. (The ultimate reward for War Boys) |
| **CHROME** | Yellow | Issues found, needs rework. Chrome tickets are created for the Scribe to address. (War Boys spray chrome before glorious acts - shiny but needs polish) |
| **FORCED** | Orange | Approved after max iterations despite remaining issues. A pragmatic approval when iteration limits are reached. |

### Decision Flow

1. Scribe creates documentation draft
2. Challenger validates and may request corrections
3. Imperator reviews the final draft:
   - **WITNESSED/VALHALLA**: Documentation complete, written to `final/`
   - **CHROME**: Scribe must address issues, then re-submit
   - **FORCED**: After max iterations, approve with known issues

## Ticket Tracking

War Rig uses in-memory ticket tracking with JSON file persistence (`.war_rig_tickets.json`). This provides:

- Crash recovery (tickets restored on restart)
- Real-time status monitoring via the status script
- Tracking of Imperator decisions per file

Ticket states:
- **created**: Waiting to be claimed by a worker
- **claimed**: Worker has picked up the ticket
- **in_progress**: Worker is actively processing
- **completed**: Successfully finished (with decision: WITNESSED/VALHALLA/FORCED)
- **blocked**: Failed on all workers, awaiting Super-Scribe rescue

## Failure Handling and Retry Strategy

War Rig implements a resilient retry strategy designed for multi-LLM environments where different workers may use different models.

### Retry Flow

```
Worker A (e.g., Claude):
  1. Process ticket → FAIL (JSON parse error)
  2. Retry with enhanced formatting prompt → FAIL
  3. Reset ticket to CREATED, add to worker's failed list
  4. Move to next ticket

Worker B (e.g., GPT-4):
  1. Picks up same ticket (fresh, no retry baggage)
  2. Process ticket → SUCCESS
```

### Key Principles

1. **One Enhanced Retry**: On first failure, retry immediately with `formatting_strict=True` which adds explicit JSON formatting instructions to the prompt

2. **Per-Worker Failed Tracking**: Each worker maintains a local `_failed_tickets` set to avoid re-picking tickets it already failed on

3. **Clean Ticket Reset**: Failed tickets reset to CREATED with no retry metadata - looks fresh for the next worker

4. **No Permanent Blocking**: Tickets are never permanently blocked within a run - other workers (potentially with different LLMs) can always try

5. **Template Backup Safety**: Before overwriting existing documentation, the previous version is backed up to `.doc.json.bak` to protect against parseable-but-garbage responses

### Super-Scribe Automatic Rescue

When tickets fail on all normal Scribe workers and get marked as BLOCKED, the Super-Scribe automatically kicks in:

1. Detects BLOCKED tickets (DOCUMENTATION, CHROME, CLARIFICATION types)
2. Resets them to CREATED state
3. Processes with a stronger model (Opus by default)
4. Logs results and any tickets still blocked after rescue

Configure via:
- `SUPER_SCRIBE_MODEL`: Model to use (default: `anthropic/claude-opus-4-20250514`)
- `NUM_SUPER_SCRIBES`: Number of rescue workers (default: 1, typically kept low due to cost)

## Utility Scripts

### Real-Time Status Monitor

Monitor ticket processing in real-time with an in-place updating display:

```bash
python scripts/war_rig_status.py output/.war_rig_tickets.json
```

Features:
- Ticket counts by type and state
- Imperator decisions breakdown (WITNESSED, VALHALLA, CHROME, FORCED)
- Progress percentage with pending count
- Stuck ticket detection with duration tracking
- Active workers display
- Auto-refresh every 2 seconds

### Generate System Overview

Create a consolidated markdown summary from all documentation:

```bash
python scripts/generate_summary.py output/
```

Generates `output/SYSTEM_OVERVIEW.md` with program summaries grouped by type.

### Generate Call Graph

Create a visual call graph showing program dependencies:

```bash
python scripts/generate_call_graph.py output/
```

Generates `output/CALL_GRAPH.md` with:
- Mermaid flowchart diagram
- Entry points and leaf nodes
- Call chains
- External dependencies

## Development

### Running Tests

```bash
pytest
```

### Type Checking

```bash
mypy war_rig
```

### Linting

```bash
ruff check war_rig
```

## Project Structure

```
war_rig/
├── __init__.py
├── cli.py              # Command-line interface
├── config.py           # Configuration management
├── beads.py            # Ticket tracking (in-memory + JSON persistence)
├── adapters/           # Integration adapters
│   ├── analysis_router.py
│   ├── beads_ticket_adapter.py
│   ├── file_artifact_adapter.py
│   └── scribe_worker_adapter.py
├── agents/             # AI agent implementations
│   ├── base.py         # Base agent class
│   ├── scribe.py       # Documentation agent
│   ├── challenger.py   # Validation agent
│   ├── imperator.py    # Review agent
│   └── program_manager.py
├── chunking/           # Code chunking for large files
│   ├── base.py
│   ├── cobol.py
│   ├── line_based.py
│   └── models.py
├── models/             # Pydantic data models
│   ├── assessments.py
│   ├── templates.py
│   └── tickets.py
├── orchestration/      # LangGraph workflow
│   ├── graph.py        # Per-file workflow
│   ├── nodes.py        # Graph nodes
│   ├── state.py        # State management
│   └── ticket_engine.py # Batch orchestration
├── preprocessors/      # Code structure extraction
│   ├── base.py
│   ├── cobol.py
│   ├── copybook.py
│   └── jcl.py
├── workers/            # Parallel worker pools
│   ├── scribe_pool.py
│   └── challenger_pool.py
└── io/                 # File I/O operations
    ├── reader.py
    └── writer.py

scripts/
├── run_carddemo.py     # Batch processing script
├── war_rig_status.py   # Real-time status monitor
├── generate_summary.py # System overview generator
└── generate_call_graph.py # Call graph generator
```

## License

MIT License
