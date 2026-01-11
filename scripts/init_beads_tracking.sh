#!/bin/bash
# Initialize beads ticket tracking for War Rig
#
# This script sets up a beads instance in the output directory for
# persistent ticket tracking across War Rig sessions.
#
# Usage:
#   ./scripts/init_beads_tracking.sh [output_dir]
#
# Arguments:
#   output_dir - Directory for War Rig output (default: ./output)
#
# Prerequisites:
#   - beads CLI (bd) must be installed
#   - git must be installed (beads uses git for storage)

set -e

OUTPUT_DIR="${1:-./output}"
BEADS_DIR="${OUTPUT_DIR}/.beads"

echo "=== War Rig Beads Initialization ==="
echo ""

# Check if bd is installed
if ! command -v bd &> /dev/null; then
    echo "ERROR: beads CLI (bd) is not installed."
    echo ""
    echo "Install beads first. See: https://github.com/anthropics/beads"
    exit 1
fi

echo "bd version: $(bd --version 2>/dev/null || echo 'unknown')"
echo "Output directory: ${OUTPUT_DIR}"
echo ""

# Create output directory if it doesn't exist
mkdir -p "${OUTPUT_DIR}"

# Check if beads is already initialized
if [ -d "${BEADS_DIR}" ]; then
    echo "Beads already initialized in ${OUTPUT_DIR}"
    echo ""
    echo "Current status:"
    cd "${OUTPUT_DIR}" && bd stats 2>/dev/null || echo "  (no issues yet)"
    exit 0
fi

# Initialize beads in the output directory
echo "Initializing beads in ${OUTPUT_DIR}..."
cd "${OUTPUT_DIR}"

# Initialize git if not already a git repo
if [ ! -d ".git" ]; then
    git init --quiet
    echo "  - Initialized git repository"
fi

# Initialize beads
bd init 2>/dev/null || {
    echo "ERROR: Failed to initialize beads"
    exit 1
}
echo "  - Initialized beads tracking"

# Configure beads for War Rig
# Set project name
if [ -f ".beads/config.toml" ]; then
    # Update project name if config exists
    sed -i.bak 's/^name = .*/name = "war_rig_tickets"/' .beads/config.toml 2>/dev/null || true
    rm -f .beads/config.toml.bak
fi

echo ""
echo "=== Beads Initialization Complete ==="
echo ""
echo "Beads is now set up in: ${OUTPUT_DIR}/.beads"
echo ""
echo "To use beads with War Rig:"
echo "  1. Set BEADS_ENABLED=true in your .env file"
echo "  2. Run War Rig as normal"
echo ""
echo "To view tickets:"
echo "  cd ${OUTPUT_DIR} && bd list"
echo ""
echo "To sync with remote (if configured):"
echo "  cd ${OUTPUT_DIR} && bd sync"
