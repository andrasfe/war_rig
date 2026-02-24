# Skill Generator

Generate Agent Skills from War Rig documentation output. Skills are structured
markdown files that AI coding agents can reference when answering questions
about a documented codebase.

## Quick Start

```bash
# 1. Run War Rig to document your codebase
#    (produces output/ with cbl/, jcl/, cpy/ subdirectories)

# 2. Generate skills from the documentation
codewhisper generate-skills --docs-dir ./output/documentation

# 3. Skills are written to ./output/skills-documentation/
#    Point your agent at this directory.
```

## CLI Reference

```
codewhisper generate-skills [OPTIONS]
```

| Option | Short | Required | Default | Description |
|--------|-------|----------|---------|-------------|
| `--docs-dir` | `-d` | Yes | - | War Rig documentation directory (contains `cbl/`, `jcl/`, etc.) |
| `--output-dir` | `-o` | No | `<docs_dir>/../skills-documentation` | Output directory for generated skills |
| `--system-name` | - | No | `"System"` | System name used in the top-level skill title |
| `--force` | `-f` | No | `false` | Overwrite existing skills output directory |
| `--verbose` | `-v` | No | `false` | Show detailed progress messages |

### Examples

```bash
# Basic: generate from default War Rig output
codewhisper generate-skills -d ./output/documentation

# Named system with custom output path
codewhisper generate-skills \
  -d /data/carddemo/documentation \
  -o /data/carddemo/skills \
  --system-name "CardDemo"

# Regenerate (overwrite existing)
codewhisper generate-skills -d ./output/documentation --force -v
```

## SKILL.md Format

Each generated skill follows the Agent Skills format: a markdown file with
YAML frontmatter providing metadata.

### Top-Level Skill

The top-level `SKILL.md` in the output root provides a system overview and
links to per-category skills:

```markdown
---
name: system-overview
description: System documentation overview
---

# System Overview

[Executive summary extracted from the documentation README.md]

## Categories

- [COBOL](cobol/SKILL.md) - COBOL program documentation (8 files)
- [JCL](jcl/SKILL.md) - JCL job documentation (5 files)
- [COPYBOOK](copybook/SKILL.md) - Copybook documentation (9 files)
```

### Category Skill

Each category (`cobol/`, `jcl/`, etc.) gets its own `SKILL.md` with a
summary table linking to the full documentation:

```markdown
---
name: cobol
description: COBOL program documentation
---

# COBOL Documentation

| Program | Description | Documentation |
|---------|-------------|---------------|
| CBACT01C | Account file utility program... | [Full docs](../documentation/cbl/CBACT01C.cbl.md) |
| COSGN00C | Sign-on program that validates... | [Full docs](../documentation/cbl/COSGN00C.cbl.md) |
```

## Supported Categories

The generator recognizes these subdirectories in the documentation output:

| Directory | Category | Column Header |
|-----------|----------|---------------|
| `cbl/` | COBOL programs | Program |
| `jcl/` | JCL jobs | Job |
| `cpy/` | Copybooks (data structures) | Name |
| `cpy-bms/` | BMS copybooks (screen maps) | Name |
| `bms/` | BMS maps (screen definitions) | Name |
| `ddl/` | DDL (database definitions) | Name |
| `ims/` | IMS (DB/PSB definitions) | Name |

## How It Works

1. **Discovery** -- Scans `--docs-dir` for known subdirectories (`cbl/`, `jcl/`, etc.)
2. **Summary extraction** -- For each `.md` file, extracts the first meaningful
   paragraph (skipping YAML frontmatter, headers, tables, and code blocks).
   Uses the Citadel SDK if available, otherwise falls back to text parsing.
3. **Category generation** -- Creates a `SKILL.md` per category with a markdown
   table of programs and truncated descriptions linking to the full documentation.
4. **Top-level generation** -- Creates the root `SKILL.md` with an executive
   summary (from the documentation `README.md` if present) and a category index.

## Troubleshooting

**"Input directory does not exist"**
The `--docs-dir` path must point to the War Rig documentation output directory.
This is typically `output/documentation/` relative to where War Rig ran.

**"Output directory already exists"**
Pass `--force` to overwrite, or choose a different `--output-dir`.

**Empty or missing descriptions**
The generator extracts summaries from the documentation markdown files. If a
file has no extractable summary (e.g., only contains tables or code blocks),
the description column will be empty. Re-running War Rig documentation may
produce better summaries.

**"No categories found"**
The documentation directory must contain at least one recognized subdirectory
(`cbl/`, `jcl/`, `cpy/`, etc.) with `.md` files. Check that War Rig completed
successfully and produced documentation output.
