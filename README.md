# War Rig: Mainframe Documentation System

A multi-agent system for documenting legacy mainframe codebases (COBOL, PL/I, JCL) using LangGraph orchestration.

## Overview

War Rig uses a pair of AI agents working in tandem:
- **Scribe**: Analyzes source code and fills out documentation templates
- **Challenger**: Validates the documentation, asking probing questions
- **Imperator**: Reviews the pair's work and decides when documentation is complete

## Architecture

```
                              WAR RIG
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
                     |   (Reviewer)   |
                     +-------+--------+
                             |
              +--------------+--------------+
              v              v              v
        +----------+   +----------+   +----------+
        |WITNESSED |   |  CHROME  |   | VALHALLA |
        |(approved)|   | (tickets)|   | (final)  |
        +----------+   +----+-----+   +----------+
                            |
                            | Back to War Boy Pair
                            +----------------------->
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

Create a `war_rig.yaml` configuration file:

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

system:
  input_directory: "./input"
  output_directory: "./output"
  checkpoint_frequency: "per_program"
```

Set your Anthropic API key:

```bash
export ANTHROPIC_API_KEY="your-key-here"
```

## Usage

### Analyze a Single File

```bash
war-rig analyze path/to/PROGRAM.cbl
```

### Process a Directory

```bash
war-rig batch path/to/source/directory
```

### Check Processing Status

```bash
war-rig status
```

### Mock Mode (No API Key)

For testing without an API key:

```bash
war-rig analyze path/to/PROGRAM.cbl --mock
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
export ANTHROPIC_API_KEY="your-key-here"
python scripts/run_carddemo.py --phase 1
```

### Available Phases

- **Phase 1**: Simple batch programs (CBACT04C, CBSTM03B)
- **Phase 2**: Batch with DB2 (CBACT01C, CBACT02C)
- **Phase 3**: CICS online (COSGN00C, COCRDLIC)

## Output Structure

```
output/
+-- analysis/           # Preprocessor outputs
+-- war_rig/
|   +-- iterations/     # Draft versions
|   +-- chrome/         # Issue tickets
|   +-- dialogues/      # Agent exchanges
+-- valhalla/           # Exceptional quality docs
+-- final/              # Approved documentation
|   +-- programs/       # Program documentation
|   +-- copybooks/      # Copybook documentation
|   +-- jcl/            # JCL documentation
+-- metrics/            # Processing metrics
```

## Documentation Quality Levels

- **WITNESSED**: Documentation meets quality standards
- **CHROME**: Issues found, needs refinement (creates tickets)
- **VALHALLA**: Exceptional quality documentation

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
+-- config.py           # Configuration management
+-- models/             # Pydantic data models
+-- preprocessors/      # Code structure extraction
+-- agents/             # AI agent implementations
+-- orchestration/      # LangGraph workflow
+-- io/                 # File I/O operations
+-- cli.py              # Command-line interface
```

## License

MIT License
