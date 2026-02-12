"""Mermaid diagram validation utilities.

Validates mermaid diagram content and sanitizes markdown documents
by removing invalid mermaid code blocks before output.

When Node.js and the mermaid npm package are available, diagrams are
validated through the real ``mermaid.parse()`` API.  Otherwise the
validator falls back to a lightweight regex check (type-keyword only).
"""

from __future__ import annotations

import json
import logging
import re
import shutil
import subprocess
from collections.abc import Sequence
from pathlib import Path

logger = logging.getLogger(__name__)

# Known mermaid diagram type keywords (first token on the type line).
_KNOWN_TYPES: set[str] = {
    "graph",
    "flowchart",
    "sequenceDiagram",
    "classDiagram",
    "stateDiagram",
    "stateDiagram-v2",
    "erDiagram",
    "journey",
    "gantt",
    "pie",
    "gitGraph",
    "mindmap",
    "timeline",
    "sankey",
    "block-beta",
    "xychart-beta",
    "quadrantChart",
    "requirementDiagram",
    "C4Context",
    "C4Container",
    "C4Component",
    "C4Deployment",
    "packet-beta",
    "kanban",
    "architecture-beta",
}

_MERMAID_BLOCK_RE = re.compile(
    r"```mermaid[ \t]*\r?\n(.*?)```",
    re.DOTALL,
)

# ---------------------------------------------------------------------------
# Node.js availability (cached)
# ---------------------------------------------------------------------------

_mermaid_node_available: bool | None = None

_SCRIPT_PATH = Path(__file__).with_name("_mermaid_parse.mjs")


def _check_mermaid_node() -> bool:
    """Probe whether Node.js + mermaid are usable for validation.

    Checks: ``node`` binary exists, the helper script exists, and
    ``node_modules/`` has been installed.  Then runs a tiny smoke-test
    diagram through the script to confirm everything works end-to-end.

    The result is cached in :data:`_mermaid_node_available`.
    """
    global _mermaid_node_available  # noqa: PLW0603

    if _mermaid_node_available is not None:
        return _mermaid_node_available

    # 1. node binary
    if shutil.which("node") is None:
        logger.debug("mermaid node validation: node binary not found")
        _mermaid_node_available = False
        return False

    # 2. helper script
    if not _SCRIPT_PATH.is_file():
        logger.debug("mermaid node validation: script %s not found", _SCRIPT_PATH)
        _mermaid_node_available = False
        return False

    # 3. node_modules
    node_modules = _SCRIPT_PATH.parent / "node_modules"
    if not node_modules.is_dir():
        logger.debug("mermaid node validation: node_modules not installed")
        _mermaid_node_available = False
        return False

    # 4. smoke test
    try:
        results = _validate_mermaid_batch(["flowchart TD\n  A --> B"])
        if results and results[0]:
            _mermaid_node_available = True
            logger.debug("mermaid node validation: available")
            return True
    except Exception:
        logger.debug("mermaid node validation: smoke test failed", exc_info=True)

    _mermaid_node_available = False
    return False


# ---------------------------------------------------------------------------
# Regex-only fallback
# ---------------------------------------------------------------------------


def _is_valid_mermaid_regex(content: str) -> bool:
    """Regex fallback: check that the first meaningful line starts with a known type."""
    if not content or not content.strip():
        return False

    for line in content.splitlines():
        stripped = line.strip()
        if not stripped or stripped.startswith("%%"):
            continue
        first_token = stripped.split()[0]
        return first_token in _KNOWN_TYPES
    return False


# ---------------------------------------------------------------------------
# Node subprocess validation
# ---------------------------------------------------------------------------


def _validate_mermaid_batch(diagrams: Sequence[str]) -> list[bool]:
    """Send *diagrams* to the Node.js helper and return per-diagram booleans.

    Raises on subprocess / JSON errors so callers can fall back.
    """
    payload = json.dumps({"diagrams": list(diagrams)})
    timeout = max(5, 2 * len(diagrams))

    proc = subprocess.run(
        ["node", str(_SCRIPT_PATH)],
        input=payload,
        capture_output=True,
        text=True,
        timeout=timeout,
        cwd=str(_SCRIPT_PATH.parent),
    )
    if proc.returncode != 0:
        raise RuntimeError(f"mermaid parse script failed: {proc.stderr[:200]}")

    data = json.loads(proc.stdout)
    return [r["valid"] for r in data["results"]]


def _validate_mermaid_node(content: str) -> bool:
    """Validate a single diagram via Node, falling back to regex on error."""
    try:
        results = _validate_mermaid_batch([content])
        return results[0]
    except Exception:
        logger.debug("node validation failed, falling back to regex", exc_info=True)
        return _is_valid_mermaid_regex(content)


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


def is_valid_mermaid(content: str) -> bool:
    """Check whether *content* looks like a valid mermaid diagram.

    When Node.js and the mermaid npm package are installed, the diagram
    is validated through ``mermaid.parse()`` for full syntax checking.
    Otherwise falls back to a regex check on the first keyword.

    Args:
        content: Raw diagram content **without** the surrounding
            ````` mermaid``/````` `` fences.

    Returns:
        ``True`` when the content is considered valid.
    """
    if _check_mermaid_node():
        return _validate_mermaid_node(content)
    return _is_valid_mermaid_regex(content)


def sanitize_mermaid_blocks(markdown: str) -> str:
    """Remove invalid mermaid code blocks from *markdown*.

    Scans for ```` ```mermaid ... ``` ```` fenced blocks, validates each
    one with :func:`is_valid_mermaid`, and replaces invalid blocks with
    an HTML comment.

    When Node.js validation is available, all blocks are batch-validated
    in a single subprocess call for efficiency.

    Args:
        markdown: Full markdown document text.

    Returns:
        The markdown with invalid mermaid blocks replaced.
    """
    matches = list(_MERMAID_BLOCK_RE.finditer(markdown))
    if not matches:
        return markdown

    diagrams = [m.group(1) for m in matches]

    # Try batch validation via Node
    if _check_mermaid_node():
        try:
            valids = _validate_mermaid_batch(diagrams)
        except Exception:
            logger.debug(
                "batch node validation failed, falling back to regex", exc_info=True
            )
            valids = [_is_valid_mermaid_regex(d) for d in diagrams]
    else:
        valids = [_is_valid_mermaid_regex(d) for d in diagrams]

    # Replace invalid blocks in reverse order to preserve offsets
    result = markdown
    for match, valid in reversed(list(zip(matches, valids, strict=True))):
        if not valid:
            inner = match.group(1)
            preview = inner.strip()[:80].replace("\n", " ")
            logger.warning("Removing invalid mermaid block: %s", preview)
            result = (
                result[: match.start()]
                + "<!-- Invalid mermaid diagram removed -->"
                + result[match.end() :]
            )

    return result
