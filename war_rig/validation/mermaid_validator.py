"""Mermaid diagram validation utilities.

Validates mermaid diagram content and sanitizes markdown documents
by removing invalid mermaid code blocks before output.
"""

from __future__ import annotations

import logging
import re

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


def is_valid_mermaid(content: str) -> bool:
    """Check whether *content* looks like a valid mermaid diagram.

    Validates that the first non-empty, non-comment line starts with a
    recognised mermaid diagram type keyword.

    Args:
        content: Raw diagram content **without** the surrounding
            ````` mermaid``/````` `` fences.

    Returns:
        ``True`` when the content begins with a known type declaration.
    """
    if not content or not content.strip():
        return False

    for line in content.splitlines():
        stripped = line.strip()
        if not stripped or stripped.startswith("%%"):
            continue
        first_token = stripped.split()[0]
        return first_token in _KNOWN_TYPES
    # All lines were blank or comments
    return False


def sanitize_mermaid_blocks(markdown: str) -> str:
    """Remove invalid mermaid code blocks from *markdown*.

    Scans for ```` ```mermaid ... ``` ```` fenced blocks, validates each
    one with :func:`is_valid_mermaid`, and replaces invalid blocks with
    an HTML comment.

    Args:
        markdown: Full markdown document text.

    Returns:
        The markdown with invalid mermaid blocks replaced.
    """

    def _replace(match: re.Match[str]) -> str:
        inner = match.group(1)
        if is_valid_mermaid(inner):
            return match.group(0)
        preview = inner.strip()[:80].replace("\n", " ")
        logger.warning("Removing invalid mermaid block: %s", preview)
        return "<!-- Invalid mermaid diagram removed -->"

    return _MERMAID_BLOCK_RE.sub(_replace, markdown)
