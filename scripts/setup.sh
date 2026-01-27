#!/usr/bin/env bash
# Setup script for War Rig (includes citadel as a dependency).
#
# Usage:
#   ./scripts/setup.sh          # Install with dev dependencies
#   ./scripts/setup.sh --prod   # Install without dev dependencies

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

PROD_ONLY=false
if [[ "${1:-}" == "--prod" ]]; then
    PROD_ONLY=true
fi

# Check for uv
if ! command -v uv &> /dev/null; then
    echo "Error: uv is not installed. Install it with: curl -LsSf https://astral.sh/uv/install.sh | sh"
    exit 1
fi

echo "=== Setting up War Rig ==="

cd "$ROOT_DIR"
if $PROD_ONLY; then
    uv sync
else
    uv sync --all-extras
fi

echo ""
echo "=== Setup complete ==="
