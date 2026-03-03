"""Bridge to ProLeap COBOL parser for grammar-based AST generation.

Calls the ProLeap wrapper JAR via subprocess and deserializes JSON output
into the same ``ParagraphSyntaxTree`` / ``SyntaxNode`` objects used by
``syntax_tree.py``.

Raises ``RuntimeError`` when Java 17+ or the fat JAR are unavailable.

Environment variables:
    PROLEAP_JAR_PATH
        Explicit path to the fat JAR.  When unset the bridge looks for
        ``proleap-wrapper/target/proleap-wrapper-fat.jar`` relative to
        the citadel package root.
    PROLEAP_JAVA_HOME
        Path to a JDK 17+ install.  Falls back to ``JAVA_HOME`` and
        then ``java`` on ``PATH``.
    PROLEAP_TIMEOUT
        Subprocess timeout in seconds (default 120).
"""

from __future__ import annotations

import json
import logging
import os
import shutil
import subprocess
from pathlib import Path
from typing import Any

from citadel.cobol.proleap_preprocessor import preprocess_for_proleap
from citadel.cobol.syntax_tree import (
    ParagraphSyntaxTree,
    StatementType,
    SyntaxNode,
    format_file_ast,
)

logger = logging.getLogger(__name__)

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

_CITADEL_ROOT = Path(__file__).resolve().parent.parent.parent.parent
_DEFAULT_JAR = _CITADEL_ROOT / "proleap-wrapper" / "target" / "proleap-wrapper-fat.jar"
_BUILD_SCRIPT = _CITADEL_ROOT / "proleap-wrapper" / "build-jar.sh"
_POM_XML = _CITADEL_ROOT / "proleap-wrapper" / "pom.xml"

_TIMEOUT = int(os.environ.get("PROLEAP_TIMEOUT", "120"))

# Map JSON type strings → StatementType enum values
_TYPE_MAP: dict[str, StatementType] = {
    "PARAGRAPH": StatementType.PARAGRAPH,
    "IF": StatementType.IF,
    "ELSE": StatementType.ELSE,
    "EVALUATE": StatementType.EVALUATE,
    "WHEN": StatementType.WHEN,
    "PERFORM_INLINE": StatementType.PERFORM_INLINE,
    "PERFORM": StatementType.PERFORM,
    "PERFORM_THRU": StatementType.PERFORM_THRU,
    "PERFORM_UNTIL": StatementType.PERFORM_UNTIL,
    "MOVE": StatementType.MOVE,
    "COMPUTE": StatementType.COMPUTE,
    "ADD": StatementType.ADD,
    "SUBTRACT": StatementType.SUBTRACT,
    "MULTIPLY": StatementType.MULTIPLY,
    "DIVIDE": StatementType.DIVIDE,
    "SET": StatementType.SET,
    "INITIALIZE": StatementType.INITIALIZE,
    "STRING": StatementType.STRING_STMT,
    "UNSTRING": StatementType.UNSTRING,
    "INSPECT": StatementType.INSPECT,
    "ACCEPT": StatementType.ACCEPT,
    "DISPLAY": StatementType.DISPLAY,
    "READ": StatementType.READ_STMT,
    "WRITE": StatementType.WRITE_STMT,
    "REWRITE": StatementType.REWRITE,
    "OPEN": StatementType.OPEN,
    "CLOSE": StatementType.CLOSE,
    "SORT": StatementType.SORT,
    "SEARCH": StatementType.SEARCH,
    "EXEC_SQL": StatementType.EXEC_SQL,
    "EXEC_CICS": StatementType.EXEC_CICS,
    "EXEC_DLI": StatementType.EXEC_DLI,
    "EXEC_OTHER": StatementType.EXEC_OTHER,
    "CALL": StatementType.CALL,
    "GOBACK": StatementType.GOBACK,
    "STOP_RUN": StatementType.STOP_RUN,
    "GO_TO": StatementType.GO_TO,
    "EXIT": StatementType.EXIT,
    "CONTINUE": StatementType.CONTINUE_STMT,
    "ALTER": StatementType.ALTER,
    "UNKNOWN": StatementType.UNKNOWN,
}


# ---------------------------------------------------------------------------
# Java / JAR discovery
# ---------------------------------------------------------------------------


def _find_java() -> str | None:
    """Find a Java 17+ binary.

    Search order: PROLEAP_JAVA_HOME → JAVA_HOME → java on PATH →
    well-known local path.
    """
    candidates: list[str] = []

    for env in ("PROLEAP_JAVA_HOME", "JAVA_HOME"):
        val = os.environ.get(env)
        if val:
            candidates.append(os.path.join(val, "bin", "java"))

    java_on_path = shutil.which("java")
    if java_on_path:
        candidates.append(java_on_path)

    # Well-known local download path
    candidates.append("/tmp/jdk-17.0.2/bin/java")

    for java_bin in candidates:
        if not os.path.isfile(java_bin):
            continue
        try:
            proc = subprocess.run(
                [java_bin, "-version"],
                capture_output=True,
                text=True,
                timeout=10,
            )
            output = proc.stderr + proc.stdout
            # Parse version: look for "17." or higher
            for line in output.splitlines():
                if '"' in line:
                    ver_str = line.split('"')[1]
                    major = int(ver_str.split(".")[0])
                    if major >= 17:
                        return java_bin
        except Exception:
            continue

    return None


def _find_jar() -> Path | None:
    """Find the ProLeap wrapper fat JAR."""
    explicit = os.environ.get("PROLEAP_JAR_PATH")
    if explicit:
        p = Path(explicit)
        return p if p.is_file() else None

    if _DEFAULT_JAR.is_file():
        return _DEFAULT_JAR

    return None


def _auto_build() -> bool:
    """Attempt to auto-build the fat JAR if pom.xml and build script exist."""
    if not _POM_XML.is_file() or not _BUILD_SCRIPT.is_file():
        return False

    logger.info("ProLeap: auto-building fat JAR via %s", _BUILD_SCRIPT)
    try:
        subprocess.run(
            ["bash", str(_BUILD_SCRIPT)],
            capture_output=True,
            text=True,
            timeout=300,
            cwd=str(_BUILD_SCRIPT.parent),
        )
    except Exception:
        logger.debug("ProLeap auto-build failed", exc_info=True)
        return False

    return _DEFAULT_JAR.is_file()


# ---------------------------------------------------------------------------
# Parse via subprocess
# ---------------------------------------------------------------------------


def parse_proleap(
    source_path: str | Path,
    copybook_dirs: list[str | Path] | None = None,
) -> tuple[dict[str, ParagraphSyntaxTree], str]:
    """Parse a COBOL file via ProLeap and return AST structures.

    Args:
        source_path: Path to the COBOL source file.
        copybook_dirs: Directories to search for copybooks.

    Returns:
        Tuple of (paragraph_name → ParagraphSyntaxTree, full_ast_text).

    Raises:
        RuntimeError: If ProLeap parsing fails.
    """
    java_bin = _find_java()
    if java_bin is None:
        raise RuntimeError(
            "ProLeap requires Java 17+. Set PROLEAP_JAVA_HOME or JAVA_HOME, "
            "or install JDK 17+ on PATH."
        )

    jar = _find_jar()
    if jar is None and _auto_build():
        jar = _find_jar()
    if jar is None:
        raise RuntimeError(
            f"ProLeap fat JAR not found at {_DEFAULT_JAR}. "
            "Run citadel/proleap-wrapper/build-jar.sh or set PROLEAP_JAR_PATH."
        )

    pp = preprocess_for_proleap(source_path)
    effective_source = str(pp.output_path)

    try:
        cmd: list[str] = [java_bin, "-jar", str(jar), effective_source]
        if copybook_dirs:
            for d in copybook_dirs:
                cmd.append(f"--copybook-dir={d}")

        logger.debug("ProLeap command: %s", " ".join(cmd))

        proc = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            timeout=_TIMEOUT,
        )

        if proc.returncode != 0:
            raise RuntimeError(
                f"ProLeap failed (exit {proc.returncode}): {proc.stderr[:500]}"
            )

        if not proc.stdout.strip():
            raise RuntimeError("ProLeap returned empty output")

        data = json.loads(proc.stdout)
        trees = _deserialize_paragraphs(data)
        full_text = format_file_ast(trees)
        return trees, full_text
    finally:
        pp.output_path.unlink(missing_ok=True)


# ---------------------------------------------------------------------------
# JSON → ParagraphSyntaxTree deserialization
# ---------------------------------------------------------------------------


def _deserialize_paragraphs(
    data: dict[str, Any],
) -> dict[str, ParagraphSyntaxTree]:
    """Convert ProLeap JSON output to ParagraphSyntaxTree objects."""
    trees: dict[str, ParagraphSyntaxTree] = {}

    for para_json in data.get("paragraphs", []):
        name = para_json.get("name", "")
        if not name:
            continue

        statements_json = para_json.get("statements", [])
        children = [_json_to_node(s) for s in statements_json]

        line_start = para_json.get("line_start", 0)
        line_end = para_json.get("line_end", 0)

        root = SyntaxNode(
            node_type=StatementType.PARAGRAPH,
            source_text="",
            line_start=line_start,
            line_end=line_end,
            children=children,
            attributes={"name": name},
        )

        # Count statements and max depth
        stmt_count = _count_statements(root)
        max_depth = _max_depth(root)

        trees[name] = ParagraphSyntaxTree(
            paragraph_name=name,
            root=root,
            statement_count=stmt_count,
            max_nesting_depth=max_depth,
        )

    return trees


def _json_to_node(data: dict[str, Any]) -> SyntaxNode:
    """Recursively convert a JSON statement dict to a SyntaxNode."""
    type_str = data.get("type", "UNKNOWN")
    node_type = _TYPE_MAP.get(type_str, StatementType.UNKNOWN)

    children_json = data.get("children", [])
    children = [_json_to_node(c) for c in children_json]

    attributes = data.get("attributes", {})
    # Filter out empty string values
    attributes = {k: v for k, v in attributes.items() if v}

    return SyntaxNode(
        node_type=node_type,
        source_text=data.get("text", ""),
        line_start=data.get("line_start", 0),
        line_end=data.get("line_end", 0),
        children=children,
        attributes=attributes,
    )


def _count_statements(node: SyntaxNode) -> int:
    """Count non-root statement nodes."""
    count = 0
    for child in node.children:
        count += 1
        count += _count_statements(child)
    return count


def _max_depth(node: SyntaxNode, depth: int = 0) -> int:
    """Compute maximum nesting depth."""
    if not node.children:
        return depth
    return max(_max_depth(c, depth + 1) for c in node.children)
