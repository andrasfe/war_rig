# Citadel

Universal Dependency Graph Builder for analyzing source code repositories.

## Overview

Citadel extracts dependency graphs from mixed-language codebases, with emphasis on mainframe artifacts (COBOL, JCL, DB2, CICS). It produces comprehensive graphs of code-to-code, code-to-data, and interface relationships.

### Features

- **Multi-language support**: COBOL, JCL, DB2 DDL, CICS BMS, Python (extensible via specs)
- **Rich artifact extraction**: Programs, copybooks, tables, procedures, screens, and more
- **Relationship tracking**: CALL, PERFORM, COPY, EXEC SQL, EXEC CICS, imports
- **Multiple export formats**: JSON, DOT (GraphViz), Cypher (Neo4j), CSV, Markdown, Mermaid
- **Dead code detection**: Find unreferenced artifacts (paragraphs, programs, copybooks)
- **Flow diagrams**: Generate Mermaid flowcharts of intra-file COBOL control flow
- **SDK for agents**: Simple Python API for programmatic analysis

### Sample Results

Tested on AWS CardDemo mainframe sample:

| Metric | Before | After (Citadel) |
|--------|--------|-----------------|
| Artifacts | 13 | 261 |
| Relationships | 5 | 250 |
| Languages | 1 | 5 |

## Installation

```bash
cd citadel
uv pip install -e .
```

For development:

```bash
uv pip install -e ".[dev]"
```

## SDK Usage (for Agents)

The SDK provides a simple API for agents to analyze source files and extract artifacts with their callouts.

### Basic Usage

```python
from citadel import Citadel, analyze_file, get_functions

# Initialize (reuse for multiple files)
citadel = Citadel()

# Full analysis with artifacts and callouts
result = citadel.analyze_file("program.cbl")

print(f"File: {result.file_path}")
print(f"Language: {result.language}")
print(f"Includes: {result.preprocessor_includes}")

for artifact in result.artifacts:
    print(f"{artifact.name} ({artifact.type})")
    for call in artifact.callouts:
        print(f"  -> {call.target} ({call.relationship})")
```

### Simplified Dict Format (for JSON responses)

```python
from citadel import get_functions

# Returns list of dicts - ideal for agent responses
funcs = citadel.get_functions("program.cbl")

# Output:
# [
#   {
#     "name": "COSGN00C",
#     "type": "program",
#     "line": 23,
#     "calls": [
#       {"target": "SEND-SCREEN", "type": "performs", "line": 83},
#       {"target": "COCOM01Y", "type": "includes", "line": 48}
#     ]
#   }
# ]
```

### Get All Callouts

```python
# Get all references/calls from a single file
callouts = citadel.get_callouts("program.cbl")

# Output:
# [
#   {"from": "COSGN00C", "to": "SEND-SCREEN", "type": "performs", "line": 83},
#   {"from": "COSGN00C", "to": "COCOM01Y", "type": "includes", "line": 48}
# ]

# Get callouts from a DIRECTORY with resolution status
callouts = citadel.get_callouts("./samples")

# Output includes `resolved` field indicating if target exists:
# [
#   {"from": "BATCMP", "to": "BUILDBAT", "type": "calls", "line": 42, "resolved": True},
#   {"from": "STEP05", "to": "REPROC", "type": "calls", "line": 23, "resolved": False}
# ]

# Filter to show only call relationships
for c in callouts:
    if c.get("type") == "calls":
        status = "✓" if c.get("resolved") else "✗"
        print(f"{status} {c['from']} -> {c['to']}")
```

### Get Included Files

```python
# Get preprocessor includes (COPY, import, #include)
includes = citadel.get_includes("program.cbl")

# Output: ["COCOM01Y", "DFHAID", "CSUSR01Y", ...]
```

### Get Function Body Text

```python
# Get the actual source code of a specific function/paragraph
body = citadel.get_function_body("program.cbl", "PROCESS-ENTER-KEY")

# Returns ONLY the function body text:
#        PROCESS-ENTER-KEY.
#
#            EXEC CICS RECEIVE
#                      MAP('COSGN0A')
#                      MAPSET('COSGN00')
#            END-EXEC.
#            ...

# Returns None if function not found
body = citadel.get_function_body("program.cbl", "NONEXISTENT")  # None
```

This is useful for agents that need to inspect or analyze specific functions without loading the entire file.

### Find Callers of a Function

```python
# Find all places that call a specific function/paragraph
callers = citadel.get_callers("UTILS.cbl", "CALCULATE-TAX")

# Output:
# [
#   {
#     "file": "/path/to/MAINPROG.cbl",
#     "function": "PROCESS-ORDER",  # The function making the call
#     "line": 125,                   # Line where the call occurs
#     "type": "performs"             # Type of call (performs, calls, includes)
#   },
#   {
#     "file": "/path/to/BILLING.cbl",
#     "function": "CALC-TOTALS",
#     "line": 89,
#     "type": "performs"
#   }
# ]

# Search in specific directories
callers = citadel.get_callers(
    "copybooks/CUSTOMER-REC.cpy",
    "CUSTOMER-REC",
    search_paths=["./app/cbl", "./app/copybooks"]
)
```

For COBOL, this finds:
- `PERFORM` statements referencing paragraphs/sections
- `CALL` statements referencing programs
- `COPY` statements referencing copybooks

### Generate Sequence Diagrams

```python
# Generate Mermaid sequence diagrams showing call chains
diagrams = citadel.get_sequence_diagrams("./src")

# Output: list of Mermaid sequence diagram strings
for diagram in diagrams:
    print(diagram)

# Example output:
# sequenceDiagram
#     participant MAINPROG as MAINPROG
#     participant SUBPROG as SUBPROG
#     participant UTILITY as UTILITY
#     MAINPROG->>SUBPROG: calls
#     SUBPROG->>UTILITY: calls

# Control diagram generation
diagrams = citadel.get_sequence_diagrams(
    "./src",
    max_diagrams=5,           # Limit number of diagrams
    min_sequence_length=2,    # Minimum calls in chain (2 = A->B->C)
)

# Can also use a pre-generated dependency graph
diagrams = citadel.get_sequence_diagrams("./output/graph.json")
```

The sequence finder algorithm:
- Finds longest call chains using DFS from entry points
- Handles cycles without infinite loops
- Discovers multiple sequences from disconnected subgraphs
- Includes both resolved and unresolved call relationships

### Detect Dead Code

```python
# Find artifacts that are never referenced by anything else
dead = citadel.get_dead_code("./src")

# Output:
# [
#   {
#     "name": "5000-EXIT",
#     "type": "paragraph",
#     "file": "/path/to/MAINPROG.cbl",
#     "line": 324,
#     "reason": "Paragraph '5000-EXIT' is never PERFORMed..."
#   },
#   {
#     "name": "OLD-UTILITY",
#     "type": "program",
#     "file": "/path/to/OLDUTIL.cbl",
#     "line": 23,
#     "reason": "Program 'OLD-UTILITY' is never CALLed..."
#   }
# ]

# Filter by type
dead_paragraphs = [d for d in dead if d["type"] == "paragraph"]
dead_copybooks = [d for d in dead if d["type"] == "copybook"]

# Can also use a pre-generated dependency graph
dead = citadel.get_dead_code("./output/graph.json")
```

The dead code detector:
- Finds artifacts with zero incoming edges in the dependency graph
- Excludes entry points: programs, JCL procedures, and the first paragraph per COBOL file (execution starts there)
- Reports file path and line number for each dead artifact
- Provides type-specific reason messages

### Generate Flow Diagrams

```python
# Generate Mermaid flowchart of internal control flow for a COBOL file
diagram = citadel.get_flow_diagram("program.cbl")

# Example output:
# flowchart TD
#     MAIN_PARA[MAIN-PARA]
#     PROCESS_ENTER_KEY[PROCESS-ENTER-KEY]
#     SEND_SIGNON_SCREEN[SEND-SIGNON-SCREEN]
#     POPULATE_HEADER_INFO[POPULATE-HEADER-INFO]
#     READ_USER_SEC_FILE[READ-USER-SEC-FILE]
#     WS_USRSEC_FILE[(WS-USRSEC-FILE)]
#     MAIN_PARA --> PROCESS_ENTER_KEY
#     MAIN_PARA --> SEND_SIGNON_SCREEN
#     PROCESS_ENTER_KEY --> READ_USER_SEC_FILE
#     READ_USER_SEC_FILE -.->|reads| WS_USRSEC_FILE
#     SEND_SIGNON_SCREEN --> POPULATE_HEADER_INFO

# Start from a specific paragraph (only shows reachable subgraph)
diagram = citadel.get_flow_diagram("program.cbl", paragraph="PROCESS-ENTER-KEY")

# Hide external calls (only show internal PERFORM flow)
diagram = citadel.get_flow_diagram("program.cbl", include_external=False)
```

The flow diagram generator:
- Shows PERFORM relationships between paragraphs within a single file
- External calls (CALL, EXEC SQL, EXEC CICS, reads/writes) appear as distinctively shaped leaf nodes
- Node shapes: `[rectangle]` for paragraphs, `([stadium])` for programs, `[(cylinder)]` for tables/files, `{{hexagon}}` for maps/screens
- Solid arrows for internal PERFORMs, dashed arrows for external calls
- De-duplicates edges and handles cycles

### One-off Convenience Functions

```python
from citadel import (
    analyze_file, get_functions, get_function_body,
    get_callers, get_sequence_diagrams, get_dead_code,
    get_flow_diagram,
)

# No need to create Citadel instance
result = analyze_file("program.cbl")
funcs = get_functions("program.cbl")
body = get_function_body("program.cbl", "MAIN-PARA")
callers = get_callers("UTILS.cbl", "CALCULATE-TAX")
diagrams = get_sequence_diagrams("./src")
dead = get_dead_code("./src")
diagram = get_flow_diagram("program.cbl")
```

### SDK Classes

| Class | Description |
|-------|-------------|
| `Citadel` | Main SDK class - reuse for multiple files |
| `FileAnalysisResult` | Full analysis result with artifacts and callouts |
| `FileArtifact` | An artifact (program, function, class) with its callouts |
| `Callout` | A reference from one artifact to another |

### FileAnalysisResult Properties

```python
result = citadel.analyze_file("program.cbl")

result.file_path              # Path to analyzed file
result.language               # Detected language (e.g., "COBOL")
result.artifacts              # List of FileArtifact objects
result.file_level_callouts    # Callouts not in any artifact
result.preprocessor_includes  # List of included file names
result.error                  # Error message if analysis failed

result.to_dict()              # Convert to JSON-serializable dict
result.get_all_callouts()     # All callouts from all artifacts
result.get_artifact_by_name("MAIN")  # Find artifact by name
```

## CLI Usage

### Analyze a Codebase

```bash
# Basic analysis to JSON
citadel analyze ./src -o graph.json

# With statistics
citadel analyze ./src -o graph.json --stats

# Direct to Mermaid
citadel analyze ./src -f mermaid -o graph.mmd

# Verbose output
citadel analyze ./src -o graph.json -v
```

### Export to Different Formats

```bash
# Export existing graph to different formats
citadel export graph.json -f markdown -o graph.md
citadel export graph.json -f mermaid -o graph.mmd
citadel export graph.json -f dot -o graph.dot
citadel export graph.json -f cypher -o graph.cypher
citadel export graph.json -f csv -o graph_csv/
```

### Supported Export Formats

| Format | Extension | Description |
|--------|-----------|-------------|
| `json` | `.json` | Full graph data, machine-readable |
| `markdown` | `.md` | Human-readable summary with Mermaid diagram |
| `mermaid` | `.mmd` | Standalone Mermaid flowchart |
| `dot` | `.dot` | GraphViz DOT format |
| `cypher` | `.cypher` | Neo4j Cypher statements |
| `csv` | directory | CSV files (artifacts.csv, relationships.csv) |

### Spec Management

```bash
# List available language specs
citadel spec list

# Show spec details
citadel spec show cobol
citadel spec show jcl --yaml

# View graph statistics
citadel stats graph.json
```

## Supported Languages

| Language | File Extensions | Artifacts Extracted |
|----------|-----------------|---------------------|
| COBOL | `.cbl`, `.cob`, `.cobol` | Programs, paragraphs, sections |
| Copybook | `.cpy`, `.copy` | Copybooks, record layouts |
| JCL | `.jcl`, `.proc` | Jobs, procedures, steps |
| DB2 DDL | `.sql`, `.ddl` | Tables, views, indexes, columns |
| CICS BMS | `.bms` | Maps, screens |
| Python | `.py` | Classes, functions, methods |

## Configuration

Configuration is loaded from environment variables (optionally via `.env` file):

```bash
CITADEL_LLM_MODEL=claude-sonnet-4-20250514
CITADEL_CACHE_DIR=.cache/citadel
CITADEL_PARALLEL_FILES=4
CITADEL_LLM_DISAMBIGUATION=true
CITADEL_MAX_LLM_CALLS=100
```

## Architecture

```
citadel/
├── src/citadel/
│   ├── sdk.py           # SDK for programmatic access
│   ├── cli.py           # Command-line interface
│   ├── orchestrator.py  # Main analysis coordinator
│   ├── analysis/        # Graph analysis (sequence finder, dead code, flow diagrams)
│   ├── discovery/       # File discovery
│   ├── specs/           # Language specifications
│   ├── parser/          # Pattern-based parsing
│   ├── resolver/        # Reference resolution
│   └── graph/           # Graph building and export
└── specs/builtin/       # Built-in language specs (YAML)
```

## Development

Run tests:

```bash
cd citadel
uv run pytest
```

Run tests with coverage:

```bash
uv run pytest --cov=citadel
```

## Example: Analyzing CardDemo

```bash
cd citadel

# Full analysis
uv run python -m citadel.cli analyze ../aws-mainframe-modernization-carddemo \
  -o ./output/carddemo.json --stats

# Generate Mermaid diagram
uv run python -m citadel.cli export ./output/carddemo.json \
  -f mermaid -o ./output/carddemo.mmd

# Generate Markdown report
uv run python -m citadel.cli export ./output/carddemo.json \
  -f markdown -o ./output/carddemo.md
```

### Using SDK

```python
from citadel import Citadel

citadel = Citadel()

# Analyze a specific COBOL program
result = citadel.analyze_file("../aws-mainframe-modernization-carddemo/app/cbl/COSGN00C.cbl")

print(f"Program: {result.artifacts[0].name}")
print(f"Includes: {result.preprocessor_includes}")
print(f"Calls: {len(result.artifacts[0].callouts)}")

# Get simplified output
funcs = citadel.get_functions("../aws-mainframe-modernization-carddemo/app/cbl/COSGN00C.cbl")
print(funcs)
```
