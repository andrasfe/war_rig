# CodeWhisper

Interactive chatbot CLI for exploring mainframe codebases using LangGraph and skills.

## Overview

CodeWhisper is a conversational AI assistant that helps developers understand and explore mainframe codebases (COBOL, PL/I, JCL, etc.). It uses a skills-based knowledge system where documentation about programs, subsystems, and concepts is organized into loadable "skills" that the agent can access on-demand.

## Features

- **Interactive REPL**: Natural language conversations about your codebase
- **Skills System**: Structured documentation that the agent can search and load
- **Code Search**: Find patterns and references across source files
- **File Reading**: Examine source code with line numbers
- **LangGraph Architecture**: Flexible agent with tool calling capabilities

## Installation

```bash
pip install codewhisper
```

Or with uv:

```bash
uv add codewhisper
```

## Quick Start

### Interactive Mode

```bash
# Start the chatbot
codewhisper --skills-dir ./example_output/skills --code-dir ./src

# You'll see a prompt where you can ask questions:
You> What does CBPAUP0C do?

CodeWhisper> CBPAUP0C is a batch IMS program that handles cleanup of expired
authorizations. It traverses the authorization database, reads pending
authorization summary segments, and deletes expired detail records based
on a configurable expiry period...
```

### Single Query Mode

```bash
codewhisper -s ./skills -c ./src -q "How does the fraud marking work?"
```

## Configuration

### Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `CODEWHISPER_SKILLS_DIR` | Path to skills directory | `./skills` |
| `CODEWHISPER_CODE_DIR` | Path to source code | `.` |
| `CODEWHISPER_MODEL` | LLM model identifier | `anthropic/claude-sonnet-4-20250514` |
| `CODEWHISPER_PROVIDER` | LLM provider | `openrouter` |

### CLI Options

```
codewhisper [OPTIONS]

Options:
  -s, --skills-dir PATH   Skills directory with SKILL.md files
  -c, --code-dir PATH     Source code directory to explore
  -m, --model TEXT        LLM model to use
  -p, --provider TEXT     LLM provider (openrouter, anthropic, openai)
  -q, --query TEXT        Single query (non-interactive mode)
  -v, --verbose          Enable verbose logging
  --help                 Show this message and exit
```

## Skills Format

Skills are markdown files with YAML frontmatter:

```markdown
---
name: cbpaup0c
description: Batch cleanup of expired authorizations
---

# CBPAUP0C

**Type:** COBOL (BATCH)

## Purpose

This program traverses the authorization database and deletes
expired authorization records...

## Called Programs

- PAUDBUNL (Dynamic call)

## Inputs

- SYSIN (Parameters): Expiry days configuration
```

## Architecture

CodeWhisper uses a LangGraph StateGraph for the conversational agent:

```
[User Query] -> [Agent] -> [Tools?] -> [Response]
                   |           |
                   v           v
              [Reasoning]  [Skill Search]
                          [Code Search]
                          [File Read]
```

The agent follows a ReAct-style pattern:
1. Analyze the user's question
2. Decide which tools to use
3. Gather information from skills and code
4. Synthesize a helpful response

## Development

```bash
# Install development dependencies
uv sync --all-extras

# Run tests
pytest tests/

# Type checking
mypy src/codewhisper

# Linting
ruff check src/codewhisper
```

## License

MIT
