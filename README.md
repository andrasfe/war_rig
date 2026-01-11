# War Rig: Mainframe Documentation System

A multi-agent system for documenting legacy mainframe codebases (COBOL, PL/I, JCL) using LangGraph orchestration and parallel worker pools.

## Overview

War Rig uses multiple AI agents working in parallel to document entire codebases:

- **Program Manager**: Orchestrates the batch workflow, creates tickets for all source files
- **Scribe Pool**: Multiple Scribes (default 3) analyze source code and fill documentation templates in parallel
- **Challenger Pool**: Multiple Challengers (default 2) validate documentation and request corrections
- **Imperator**: Reviews all documentation holistically and identifies cross-program issues

## Architecture

### Program Manager Workflow

```
                         PROGRAM MANAGER
                              |
                              v
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
            |      VALIDATION TICKETS          |
            +----------------------------------+
                              |
              +---------------+---------------+
              |                               |
              v                               v
       +-------------+                 +-------------+
       |Challenger #1|                 |Challenger #2|
       |   (async)   |                 |   (async)   |
       +-------------+                 +-------------+
              |                               |
              +---------------+---------------+
                              |
                              v
            +----------------------------------+
            |     All tickets completed?       |
            +----------------------------------+
                              |
                       Yes    |    No (rework tickets)
                              |    +---> Back to Scribes
                              v
                     +----------------+
                     |   IMPERATOR    |
                     | Holistic Review|
                     +-------+--------+
                             |
              +--------------+--------------+
              v              v              v
        +----------+   +----------+   +----------+
        | SATISFIED|   | CLARIFY  |   | FORCED   |
        |  (done)  |   | (cycle)  |   | (max)    |
        +----------+   +----+-----+   +----------+
                            |
                            | New cycle with
                            | clarification tickets
                            +----------------------->
```

### Per-File Processing (War Boy Pair)

Each Scribe processes files using the existing War Boy pair workflow:

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
        |(approved)|   | (tickets)|   | (final)  |
        +----------+   +----+-----+   +----------+
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
NUM_TEAMS=1

# Program Manager
PM_MAX_CYCLES=5

# Workflow Settings
MAX_ITERATIONS=3
MAX_QUESTIONS_PER_ROUND=5
MAX_CHROME_TICKETS=5

# Beads Integration (task tracking)
BEADS_ENABLED=true
BEADS_DRY_RUN=false

# Paths
INPUT_DIRECTORY=./aws-mainframe-modernization-carddemo/app
OUTPUT_DIRECTORY=./output
```

## Usage

### Analyze a Single File

```bash
war-rig analyze path/to/PROGRAM.cbl
```

### Process a Directory (Sequential)

```bash
war-rig batch path/to/source/directory
```

### Process with Program Manager (Parallel)

```bash
python scripts/run_carddemo.py --pm-mode
```

### Mock Mode (No API Key)

For testing without an API key:

```bash
war-rig analyze path/to/PROGRAM.cbl --mock
python scripts/run_carddemo.py --mock --phase 1
```

## AWS CardDemo Integration

War Rig includes integration with the [AWS CardDemo](https://github.com/aws-samples/aws-mainframe-modernization-carddemo) sample mainframe application.

### Setup

```bash
# Clone CardDemo repository
git clone https://github.com/aws-samples/aws-mainframe-modernization-carddemo.git

# Run with mock agents (testing)
python scripts/run_carddemo.py --mock --phase 1

# Run with real LLM (production)
python scripts/run_carddemo.py --phase 1

# Run with Program Manager mode (parallel processing)
python scripts/run_carddemo.py --pm-mode
```

### Available Phases

- **Phase 1**: Simple batch programs (CBACT04C, CBSTM03B)
- **Phase 2**: Batch with DB2 (CBACT01C, CBACT02C)
- **Phase 3**: CICS online (COSGN00C, COCRDLIC)

## Output Structure

```
output/
├── analysis/           # Preprocessor outputs
├── war_rig/
│   ├── iterations/     # Draft versions
│   ├── chrome/         # Issue tickets
│   └── dialogues/      # Agent exchanges
├── valhalla/           # Exceptional quality docs
├── final/              # Approved documentation
│   ├── programs/       # Program documentation
│   ├── copybooks/      # Copybook documentation
│   └── jcl/            # JCL documentation
└── metrics/            # Processing metrics
```

## Documentation Quality Levels

- **WITNESSED**: Documentation meets quality standards
- **CHROME**: Issues found, needs refinement (creates tickets)
- **VALHALLA**: Exceptional quality documentation
- **FORCED**: Approved after max iterations despite issues

## Beads Task Tracking

War Rig integrates with [beads](https://github.com/beads-project/beads) for task tracking:

- Tickets are created for each source file
- Challengers create tickets for blocking questions
- Imperator creates Chrome tickets for issues
- All tickets are closed when documentation is approved

```bash
# View ready work
bd ready

# View all open tickets
bd list --status=open

# View project statistics
bd stats
```

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

### Enhanced Formatting Prompt

On retry, the following instructions are added:

```
## CRITICAL: JSON Formatting Requirements
Your previous response had JSON formatting errors. Please ensure:
1. Output ONLY valid JSON - no markdown code blocks, no extra text
2. All strings must be properly escaped (use \n for newlines, \\ for backslashes)
3. No trailing commas after the last element in arrays or objects
4. All property names must be in double quotes
5. No comments in JSON
6. Verify your response is parseable JSON before submitting
```

### Multi-LLM Architecture Support

This approach is designed for future multi-LLM deployments:

- Different Scribe workers can use different models (Claude, GPT-4, etc.)
- A ticket that fails on one model may succeed on another
- No arbitrary retry limits that would block all attempts
- Workers only skip tickets they personally failed on

### What Happens to Persistently Failing Tickets

If all workers fail on a ticket:
1. Ticket remains CREATED (available)
2. Run completes (all workers idle with no claimable tickets)
3. On restart, ticket is still CREATED
4. Manual investigation can determine why (bad source code, impossible task, etc.)

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
├── config.py           # Configuration management
├── models/             # Pydantic data models
├── preprocessors/      # Code structure extraction
├── agents/             # AI agent implementations
│   ├── scribe.py       # Documentation agent
│   ├── challenger.py   # Validation agent
│   ├── imperator.py    # Review agent (per-file & holistic)
│   └── program_manager.py  # Batch orchestration agent
├── workers/            # Parallel worker pools
│   ├── scribe_pool.py  # Async Scribe workers
│   └── challenger_pool.py  # Async Challenger workers
├── orchestration/      # LangGraph workflow
│   ├── graph.py        # Per-file workflow
│   ├── nodes.py        # Graph nodes
│   └── state.py        # State management
├── beads.py            # Beads task tracking integration
├── io/                 # File I/O operations
└── cli.py              # Command-line interface

docs/
└── program_manager_architecture.md  # Architecture design doc
```

## License

MIT License
