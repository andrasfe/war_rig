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

# ── ProLeap COBOL parser (JDK 17 + fat JAR) ──────────────────────
echo ""
echo "=== Setting up ProLeap COBOL parser ==="

PROLEAP_DIR="$ROOT_DIR/citadel/proleap-wrapper"
FAT_JAR="$PROLEAP_DIR/target/proleap-wrapper-fat.jar"

# Check if fat JAR already exists
if [[ -f "$FAT_JAR" ]]; then
    echo "ProLeap fat JAR already built: $FAT_JAR"
else
    # Ensure JDK 17+ is available
    find_java17() {
        for env in JAVA17_HOME JAVA_HOME; do
            local val="${!env:-}"
            if [[ -n "$val" ]] && "$val/bin/java" -version 2>&1 | grep -qE '"(1[7-9]|2[0-9])\.'; then
                echo "$val"; return 0
            fi
        done
        if command -v java >/dev/null 2>&1; then
            local ver
            ver="$(java -version 2>&1 | head -1 | sed 's/.*"\([0-9]*\)\..*/\1/')"
            if [[ "$ver" -ge 17 ]] 2>/dev/null; then
                local java_bin
                java_bin="$(readlink -f "$(command -v java)")"
                echo "$(dirname "$(dirname "$java_bin")")"; return 0
            fi
        fi
        [[ -x "/tmp/jdk-17.0.2/bin/java" ]] && echo "/tmp/jdk-17.0.2" && return 0
        return 1
    }

    if ! find_java17 >/dev/null 2>&1; then
        echo "JDK 17+ not found, downloading OpenJDK 17..."
        ARCH="$(uname -m)"
        case "$ARCH" in
            x86_64|amd64) JDK_ARCH="x64" ;;
            aarch64|arm64) JDK_ARCH="aarch64" ;;
            *) echo "WARNING: Unsupported architecture $ARCH, skipping JDK download"; JDK_ARCH="" ;;
        esac
        if [[ -n "$JDK_ARCH" ]]; then
            JDK_URL="https://download.java.net/java/GA/jdk17.0.2/dfd4a8d0985749f896bed50d7138ee7f/8/GPL/openjdk-17.0.2_linux-${JDK_ARCH}_bin.tar.gz"
            curl -fsSL "$JDK_URL" | tar -xz -C /tmp
            echo "JDK 17 installed to /tmp/jdk-17.0.2"
        fi
    fi

    # Build the fat JAR (handles Maven download + ProLeap build from source)
    if [[ -f "$PROLEAP_DIR/build-jar.sh" ]]; then
        echo "Building ProLeap fat JAR..."
        bash "$PROLEAP_DIR/build-jar.sh"
    else
        echo "WARNING: $PROLEAP_DIR/build-jar.sh not found, skipping ProLeap build"
    fi
fi

echo ""
echo "=== Setup complete ==="
