# Beads Integration for War Rig

War Rig uses the [beads](https://github.com/anthropics/beads) task tracking system for persistent ticket management across sessions.

## Overview

War Rig has two ticket tracking modes:

1. **In-Memory Tracking (Default)**: Tickets are stored in memory with persistence to disk (`.war_rig_tickets.json`) for crash recovery. This is the default mode and requires no additional setup.

2. **Beads CLI Tracking**: When enabled, War Rig uses the `bd` CLI for full beads integration, allowing you to view and manage tickets using standard beads commands.

## Quick Start

### Using Default In-Memory Tracking

By default, War Rig uses in-memory tracking with disk persistence. No setup required:

```bash
# Run War Rig normally
war_rig run ./input
```

Tickets are persisted to `<output_dir>/.war_rig_tickets.json` and automatically recovered on restart.

### Enabling Beads CLI Integration

To use full beads CLI integration:

1. **Install beads CLI**:
   ```bash
   # See https://github.com/anthropics/beads for installation
   pip install beads
   ```

2. **Initialize beads in output directory**:
   ```bash
   ./scripts/init_beads_tracking.sh ./output
   ```

3. **Enable beads in War Rig**:
   ```bash
   # Set environment variable
   export BEADS_ENABLED=true

   # Or in .env file
   echo "BEADS_ENABLED=true" >> .env
   ```

4. **Run War Rig**:
   ```bash
   war_rig run ./input
   ```

## Configuration

| Environment Variable | Default | Description |
|---------------------|---------|-------------|
| `BEADS_ENABLED` | `false` | Enable beads CLI integration |
| `BEADS_DRY_RUN` | `false` | Log commands instead of executing them |

## Beads Initialization Script

The `scripts/init_beads_tracking.sh` script sets up beads in your output directory:

```bash
# Initialize in default output directory (./output)
./scripts/init_beads_tracking.sh

# Initialize in custom directory
./scripts/init_beads_tracking.sh /path/to/output
```

The script:
- Creates the output directory if needed
- Initializes a git repository (beads uses git for storage)
- Runs `bd init` to set up beads tracking
- Configures the project name as `war_rig_tickets`

## Directory Isolation

**Important**: War Rig's beads instance is isolated from your project's beads.

When `BEADS_ENABLED=true`:
- War Rig creates/uses `.beads/` inside the output directory
- All `bd` commands are executed with `cwd=<output_dir>`
- Your project's `.beads/` directory (if any) is not affected

This means:
- `./output/.beads/` - War Rig's ticket tracking (when enabled)
- `./.beads/` - Your project's beads (unaffected)

## Viewing Tickets

### When using in-memory tracking (default):

```bash
# View raw ticket data
cat ./output/.war_rig_tickets.json | jq .
```

### When using beads CLI:

```bash
# Navigate to output directory
cd ./output

# List all tickets
bd list

# Show ready tickets (no blockers)
bd ready

# View ticket details
bd show <ticket-id>

# Get project statistics
bd stats
```

## Crash Recovery

War Rig automatically handles crash recovery:

1. Tickets are persisted to disk after every state change
2. On restart, tickets are loaded from disk
3. Incomplete tickets (CLAIMED, IN_PROGRESS, BLOCKED) are reset to CREATED
4. Workers can then claim and retry the work

This ensures no work is lost and agents can resume from where they left off.

## Ticket Lifecycle

```
CREATED  →  CLAIMED  →  IN_PROGRESS  →  COMPLETED
    ↑          ↓              ↓
    └──────────┴──── BLOCKED ─┘
```

- **CREATED**: Ready for a worker to claim
- **CLAIMED**: Worker has claimed the ticket
- **IN_PROGRESS**: Work is being performed
- **COMPLETED**: Work finished successfully
- **BLOCKED**: Dependency identified, needs resolution

On crash/restart, CLAIMED, IN_PROGRESS, and BLOCKED tickets are reset to CREATED.

## Ticket Types

| Type | Description |
|------|-------------|
| `DOCUMENTATION` | Initial documentation task for a source file |
| `VALIDATION` | Validation task for completed documentation |
| `CLARIFICATION` | Question from Challenger requiring Scribe response |
| `CHROME` | Issue ticket from Imperator requiring rework |
| `HOLISTIC_REVIEW` | Batch review task for Imperator |

## Thread Safety

The in-memory ticket tracking is thread-safe using `threading.Lock()` for all critical sections. This allows multiple worker threads to safely claim and update tickets concurrently.

## Troubleshooting

### "bd CLI not found"
Install beads: `pip install beads`

### "Beads not initialized in output directory"
Run the initialization script: `./scripts/init_beads_tracking.sh ./output`

### Tickets not persisting
Check that the output directory exists and is writable. Verify `.war_rig_tickets.json` is being created.

### Interference with project's beads
Ensure `BEADS_ENABLED=true` is set. War Rig only uses isolated beads when explicitly enabled.
