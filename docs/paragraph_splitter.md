# COBOL Paragraph Splitter

Splits large COBOL source files into per-paragraph markdown files and adds clickable source links to the generated documentation. This makes individual paragraphs viewable on GitHub (which refuses to render files above a certain size) and lets readers jump from documentation straight to the relevant source.

## How it works

1. **Split**: Reads `.doc.json` templates to get paragraph names and `citation` line ranges, then extracts each paragraph from the source `.cbl` file into its own `.cbl.md` file (wrapped in `` ```cobol `` fences for syntax highlighting).

2. **Link**: Patches the `.cbl.md` documentation to insert `> [Source: ...]` links after each `### PARAGRAPH-NAME` heading, pointing to the corresponding split file.

### Output structure

```
output/cbl/
  PROG.cbl.doc.json       # documentation template (input)
  PROG.cbl.md             # documentation markdown (patched with links)
  PROG.cbl.d/             # split paragraph directory
    MAIN-PARA.cbl.md      # ```cobol ... ``` wrapped source
    1000-INIT.cbl.md
    9999-EXIT.cbl.md
```

### What the links look like in documentation

```markdown
### MAIN-PARA
> [Source: MAIN-PARA.cbl.md](PROG.cbl.d/MAIN-PARA.cbl.md)

Main logic description...
```

## Pipeline integration (automatic)

When the documentation pipeline runs, `split_and_link()` is called automatically from `ScribePool._save_template()` after writing the `.doc.json` and `.cbl.md` files. No manual intervention is needed for new documentation runs.

## Adding links to existing documentation

If you have existing documentation output that was generated before the splitter was integrated (or you need to regenerate after changes), use the batch runner:

```python
from pathlib import Path
from war_rig.io.paragraph_splitter import run_batch_split

run_batch_split(
    input_dir=Path("path/to/cobol/source"),   # directory with .cbl files
    output_dir=Path("path/to/doc/output"),     # directory with .doc.json and .cbl.md files
)
```

This will:
- Scan `output_dir` for `*.cbl.doc.json` files
- Match each to its source `.cbl` file in `input_dir` (case-insensitive search)
- Split all paragraphs into `{PROG}.cbl.d/` directories
- Patch all `.cbl.md` files with source links

The operation is **idempotent** -- running it multiple times will not duplicate links or overwrite files with identical content.

### Step-by-step functions

If you need more control, use the individual functions:

```python
from pathlib import Path
from war_rig.io.paragraph_splitter import (
    split_all_in_directory,
    patch_all_markdown_in_directory,
)

# Step 1: Split paragraphs
results = split_all_in_directory(
    source_dir=Path("path/to/cobol/source"),
    doc_dir=Path("path/to/doc/output"),
)
# results: {"PROG.cbl": [Path("PROG.cbl.d/MAIN-PARA.cbl.md"), ...]}

# Step 2: Patch markdown with links
patched_count = patch_all_markdown_in_directory(
    doc_dir=Path("path/to/doc/output"),
)
```

### Single-file usage

```python
from pathlib import Path
from war_rig.io.paragraph_splitter import split_and_link

created = split_and_link(
    source_path=Path("input/cbl/PROG.cbl"),
    doc_json_path=Path("output/cbl/PROG.cbl.doc.json"),
    md_path=Path("output/cbl/PROG.cbl.md"),
)
```

## Notes

- Only COBOL files (`.cbl`, `.cob`, `.cobol`) are processed; other file types are skipped.
- Paragraphs with missing or invalid citations (e.g. COBOL sequence numbers instead of line numbers) are skipped with a warning.
- Paragraph names are sanitized for filesystem safety (unsafe characters replaced with `_`).
- Split files use `.cbl.md` extension with `` ```cobol `` fencing so GitHub renders them with syntax highlighting.
