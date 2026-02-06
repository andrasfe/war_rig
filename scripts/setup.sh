#!/usr/bin/env bash
# Setup script for War Rig (includes citadel and codewhisper as dependencies).
#
# Usage:
#   ./scripts/setup.sh          # Install with dev dependencies
#   ./scripts/setup.sh --prod   # Install without dev dependencies
#
# Supports both uv and pip. Uses uv if available, falls back to pip.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

PROD_ONLY=false
if [[ "${1:-}" == "--prod" ]]; then
    PROD_ONLY=true
fi

echo "=== Setting up War Rig ==="
cd "$ROOT_DIR"

if command -v uv &> /dev/null; then
    echo "Using uv..."
    if $PROD_ONLY; then
        uv sync
    else
        uv sync --all-extras
    fi
else
    echo "uv not found, using pip..."

    # Install local packages first (uv.sources equivalents)
    echo "Installing local packages..."
    pip install -e ./citadel
    pip install -e ./codewhisper

    # Install war_rig itself
    if $PROD_ONLY; then
        pip install -e .
    else
        pip install -e ".[dev]"
    fi
fi

echo ""
echo "=== Setup complete ==="
