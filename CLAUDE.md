# War Rig - Project Context

## Overview

War Rig is a multi-agent mainframe documentation system. It uses AI agents (Scribe, Challenger, Imperator) to automatically document legacy mainframe codebases (COBOL, PL/I, JCL, Assembler, REXX) through ticket-based orchestration and parallel worker pools.

## Tech Stack

- **Python 3.11+** with **uv** for package management
- **LangGraph** for agent orchestration
- **LangChain** (Anthropic + OpenAI providers)
- **Pydantic v2** for models and settings
- **beads** (`bd` CLI) for issue tracking

## Project Structure

```
war_rig/
  agents/         # AI agents (Scribe, Challenger, Imperator)
  analysis/       # Code analysis and parsing
  chunking/       # Source file chunking
  workers/        # Parallel worker pool execution
  models/         # Pydantic data models
  orchestration/  # Ticket-based workflow coordination
  providers/      # LLM provider abstraction
  preprocessors/  # Source file preprocessing
  validation/     # Output validation
  feedback/       # Feedback loops
  sampling/       # Sampling strategies
  io/             # I/O utilities
  utils/          # Shared utilities
  cli.py          # CLI entry point
  config.py       # Configuration
tests/            # Test suite
scripts/          # Helper scripts
citadel/          # Documentation generation tool
docs/             # Project documentation
```

## Common Commands

```bash
# Development
uv sync --all-extras          # Install all dependencies
uv run pytest tests/ -x       # Run tests (stop on first failure)
uv run ruff check .           # Lint
uv run ruff format .          # Format
uv run mypy war_rig/          # Type check

# Issue tracking (beads)
bd ready                      # Show issues ready to work
bd show <id>                  # View issue details
bd create --title="..." --type=task --priority=2
bd update <id> --status=in_progress
bd close <id>
bd sync                       # Sync with git remote
```

## Code Conventions

- **Linting**: ruff (line-length 88, pycodestyle + pyflakes + isort + bugbear)
- **Type checking**: mypy strict mode (disallow_untyped_defs, strict_optional)
- **Docstrings**: Google style
- **Testing**: pytest with pytest-asyncio (asyncio_mode = auto)
- **Imports**: isort via ruff, `war_rig` as known first-party

## Git Workflow

- See `AGENTS.md` for session completion protocol
- Always run `bd sync` before pushing
- Reference issue numbers in commit messages
