# Citadel

Universal Dependency Graph Builder for analyzing source code repositories.

## Overview

Citadel extracts dependency graphs from mixed-language codebases, with emphasis on mainframe artifacts (COBOL, JCL, DB2, CICS). It produces comprehensive graphs of code-to-code, code-to-data, and interface relationships.

## Installation

```bash
uv pip install -e .
```

For development:

```bash
uv pip install -e ".[dev]"
```

## Usage

```bash
# Analyze a codebase
citadel analyze ./src -o graph.json

# List available specs
citadel spec list

# Show a specific spec
citadel spec show cobol
```

## Configuration

Configuration is loaded from environment variables (optionally via `.env` file):

```bash
CITADEL_LLM_MODEL=claude-sonnet-4-20250514
CITADEL_CACHE_DIR=.cache/citadel
CITADEL_PARALLEL_FILES=4
CITADEL_LLM_DISAMBIGUATION=true
CITADEL_MAX_LLM_CALLS=100
```

## Development

Run tests:

```bash
pytest
```
