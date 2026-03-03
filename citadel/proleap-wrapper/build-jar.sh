#!/usr/bin/env bash
# Build the ProLeap wrapper fat JAR.
#
# Requires: JDK 17+ and Maven 3.6+.
# If JAVA_HOME points to JDK < 17, set JAVA17_HOME to a JDK 17+ install.
# If mvn is not on PATH, the script downloads Maven automatically.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

TARGET_JAR="$SCRIPT_DIR/target/proleap-wrapper-fat.jar"

# ── JDK 17 detection ────────────────────────────────────────────────
find_java17() {
    # 1. Explicit override
    if [[ -n "${JAVA17_HOME:-}" ]] && "$JAVA17_HOME/bin/java" -version 2>&1 | grep -q '"17\.\|"18\.\|"19\.\|"20\.\|"21\.\|"22\.\|"23\.\|"24\.\|"25\.'; then
        echo "$JAVA17_HOME"
        return 0
    fi
    # 2. JAVA_HOME if >= 17
    if [[ -n "${JAVA_HOME:-}" ]] && "$JAVA_HOME/bin/java" -version 2>&1 | grep -q '"17\.\|"18\.\|"19\.\|"20\.\|"21\.\|"22\.\|"23\.\|"24\.\|"25\.'; then
        echo "$JAVA_HOME"
        return 0
    fi
    # 3. java on PATH
    if command -v java >/dev/null 2>&1; then
        local ver
        ver="$(java -version 2>&1 | head -1 | sed 's/.*"\([0-9]*\)\..*/\1/')"
        if [[ "$ver" -ge 17 ]] 2>/dev/null; then
            # Derive JAVA_HOME from java binary
            local java_bin
            java_bin="$(readlink -f "$(command -v java)")"
            echo "$(dirname "$(dirname "$java_bin")")"
            return 0
        fi
    fi
    # 4. Well-known local path from our download
    if [[ -x "/tmp/jdk-17.0.2/bin/java" ]]; then
        echo "/tmp/jdk-17.0.2"
        return 0
    fi
    return 1
}

JAVA17_HOME_RESOLVED="$(find_java17)" || {
    echo >&2 "ERROR: JDK 17+ not found. Set JAVA17_HOME or install JDK 17."
    exit 1
}
export JAVA_HOME="$JAVA17_HOME_RESOLVED"
export PATH="$JAVA_HOME/bin:$PATH"
echo "Using JDK: $JAVA_HOME ($(java -version 2>&1 | head -1))"

# ── Maven detection ─────────────────────────────────────────────────
MVN_LOCAL="$SCRIPT_DIR/.maven"
MVN_VERSION="3.9.6"

find_mvn() {
    if command -v mvn >/dev/null 2>&1; then
        command -v mvn
        return 0
    fi
    if [[ -x "$MVN_LOCAL/bin/mvn" ]]; then
        echo "$MVN_LOCAL/bin/mvn"
        return 0
    fi
    return 1
}

MVN="$(find_mvn)" || {
    echo "Maven not found, downloading $MVN_VERSION..."
    curl -fsSL "https://archive.apache.org/dist/maven/maven-3/${MVN_VERSION}/binaries/apache-maven-${MVN_VERSION}-bin.tar.gz" \
        | tar -xz -C "$SCRIPT_DIR"
    mv "$SCRIPT_DIR/apache-maven-${MVN_VERSION}" "$MVN_LOCAL"
    MVN="$MVN_LOCAL/bin/mvn"
    echo "Maven installed at $MVN_LOCAL"
}

echo "Using Maven: $MVN"

# ── ProLeap dependency (not on Maven Central) ───────────────────────
PROLEAP_JAR="$HOME/.m2/repository/io/github/uwol/proleap-cobol-parser/4.0.0/proleap-cobol-parser-4.0.0.jar"
if [[ ! -f "$PROLEAP_JAR" ]]; then
    echo "ProLeap not in local Maven repo, building from source..."
    PROLEAP_TMP="/tmp/proleap-cobol-parser"
    if [[ ! -d "$PROLEAP_TMP" ]]; then
        git clone --depth 1 https://github.com/uwol/proleap-cobol-parser.git "$PROLEAP_TMP"
    fi
    "$MVN" install -q -DskipTests -f "$PROLEAP_TMP/pom.xml"
    echo "ProLeap installed to local Maven repo"
fi

# ── Build ────────────────────────────────────────────────────────────
"$MVN" package -q -DskipTests -f "$SCRIPT_DIR/pom.xml"

if [[ -f "$TARGET_JAR" ]]; then
    echo "Built: $TARGET_JAR"
else
    echo >&2 "ERROR: Build succeeded but JAR not found at $TARGET_JAR"
    exit 1
fi
