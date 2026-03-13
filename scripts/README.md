# War Rig Scripts

Utility scripts for managing and maintaining War Rig documentation workflows.

## Output Maintenance

### `regenerate_markdown.py`
Regenerate markdown files from existing `.doc.json` templates without reprocessing.

Useful when markdown generation logic has been updated (e.g., adding new sections like Inter-Paragraph Data Flow) and you want to refresh the markdown without rerunning the full documentation pipeline.

```bash
# Regenerate all markdown in output directory
python scripts/regenerate_markdown.py --output-dir ./output

# Preview without writing
python scripts/regenerate_markdown.py --output-dir ./output --dry-run

# Verbose output with paragraph counts
python scripts/regenerate_markdown.py --output-dir ./output --verbose
```

### `fix_stub_paragraphs.py`
Find and fix files with stub paragraphs that weren't properly documented.

Scans for paragraphs with placeholder text like "[Citadel] Paragraph identified by static analysis" and creates re-documentation tickets for affected files.

```bash
# Report files with stubs (no changes)
python scripts/fix_stub_paragraphs.py --output-dir ./output --report-only

# Preview ticket creation
python scripts/fix_stub_paragraphs.py --output-dir ./output --dry-run

# Create tickets to re-document files with stubs
python scripts/fix_stub_paragraphs.py --output-dir ./output --input-dir ./source
```

## Analysis & Reporting

### `generate_call_graph.py`
Generate call graph visualizations from Citadel analysis.

### `generate_summary.py`
Generate summary reports from documentation output.

### `war_rig_status.py`
Check status of War Rig processing jobs.

### `validate_cobol_analysis_patterns.py`
Validate COBOL analysis patterns against test cases.

### `redoc_paragraphs.py`
Re-document specific paragraphs in a COBOL program by name.

Marks named paragraphs as stubs in the `.doc.json` so the scribe resume path re-processes only those paragraphs on the next run. Optionally creates a DOCUMENTATION ticket for `--no-new-tickets` batch runs.

```bash
# List all paragraphs and their status
python scripts/redoc_paragraphs.py COPAUA0C.cbl -o ./output --list

# Mark specific paragraphs for re-documentation (dry run)
python scripts/redoc_paragraphs.py COPAUA0C.cbl -o ./output \
    -p MAIN-PARA -p 6000-MAKE-DECISION --dry-run

# Mark paragraphs and create a ticket
python scripts/redoc_paragraphs.py COPAUA0C.cbl -o ./output \
    -p MAIN-PARA -p 6000-MAKE-DECISION --ticket

# Then rerun without creating new tickets
war-rig run --no-new-tickets
```

## Ticket Management

### `ticket_manager.py`
Manage documentation tickets in the beads system.

### `init_beads_tracking.sh`
Initialize beads issue tracking for a project.

## Development & Testing

### `run_carddemo.py`
Run War Rig against the CardDemo sample application.

### `test_chunking.py`
Test chunking strategies for large files.

### `generate_skills.py`
Generate skill definitions for the documentation agents.

### `human_feedback.py`
Process human feedback on generated documentation.

## System Administration

### `setup.sh`
Initial setup script for War Rig environment.

### `kill_workers.sh`
Kill running worker processes.

```bash
# Stop all scribe and challenger workers
./scripts/kill_workers.sh
```

## Common Workflows

### Fix documentation without full rerun

1. **Regenerate markdown only** (data is in .doc.json):
   ```bash
   python scripts/regenerate_markdown.py --output-dir ./output
   ```

2. **Fix files with stub paragraphs**:
   ```bash
   # Check which files need fixing
   python scripts/fix_stub_paragraphs.py --output-dir ./output --report-only

   # Create tickets and reprocess
   python scripts/fix_stub_paragraphs.py --output-dir ./output --input-dir ./source
   # Then run the scribe pool
   ```

3. **Re-document specific paragraphs**:
   ```bash
   # See which paragraphs exist
   python scripts/redoc_paragraphs.py FILE.cbl -o ./output --list

   # Mark bad paragraphs as stubs and create a ticket
   python scripts/redoc_paragraphs.py FILE.cbl -o ./output \
       -p PARA-NAME -p OTHER-PARA --ticket

   # Rerun (only the stubbed paragraphs get re-processed)
   war-rig run --no-new-tickets
   ```

4. **Reprocess a single file**:
   ```bash
   # Delete the doc file
   rm output/path/to/FILE.cbl.doc.json

   # Create a ticket manually or let the next run pick it up
   ```
