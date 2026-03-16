"""
Flow diagram generator for COBOL file analysis.

This module generates Mermaid flowcharts showing the internal control flow
(PERFORM relationships between paragraphs) within a single COBOL file.
External calls (CALL, reads, writes, EXEC SQL, EXEC CICS, includes) appear
as leaf nodes with different shapes but are NOT recursively followed.

Algorithm
---------
1. Accept a ``FileAnalysisResult`` (from ``sdk.analyze_file()``) and an
   optional starting paragraph name.
2. Build an adjacency map: paragraph -> set of performed paragraphs (only
   ``performs`` relationship type, only targets that exist as paragraphs
   in this file).
3. If a starting paragraph is given, BFS from that paragraph to collect
   only reachable paragraphs.
4. If no starting paragraph is given, include all paragraphs.
5. Generate Mermaid flowchart with:
   - Solid arrows (``-->``) for internal PERFORM calls between paragraphs.
   - Dashed arrows (``-.->``) for external/outgoing calls.
   - Node shapes: ``[rectangle]`` for internal paragraphs,
     ``([stadium])`` for external program calls,
     ``[(cylinder)]`` for file/table reads/writes,
     ``{{hexagon}}`` for maps/screens.
   - Edge labels for external call types.
   - De-duplicated edges and graceful cycle handling.

Time Complexity: O(V + E) where V = paragraphs, E = callouts.
Space Complexity: O(V + E) for adjacency maps and output.
"""

from __future__ import annotations

import logging
import re
from collections import deque
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from citadel.sdk import FileAnalysisResult

logger = logging.getLogger(__name__)

# Relationship types that represent internal PERFORM calls.
_INTERNAL_REL_TYPES = {"performs"}

# Relationship types for external calls that should appear as leaf nodes.
_EXTERNAL_REL_TYPES = {
    "calls",
    "reads",
    "writes",
    "sends_to",
    "receives_from",
    "includes",
    "updates",
    "deletes",
}

# Artifact types that indicate the top-level container (program, etc.)
# These should NOT appear as nodes in the flow diagram.
_CONTAINER_TYPES = {"program", "class", "module"}

# Target types that map to specific Mermaid node shapes.
_PROGRAM_TARGET_TYPES = {"program", "procedure"}
_DATA_TARGET_TYPES = {"table", "dataset", "file", "queue", "database"}
_SCREEN_TARGET_TYPES = {"screen", "map", "mapset"}

# Relationship types that hint at the target category when target_type
# is missing or generic.
_DATA_REL_TYPES = {"reads", "writes", "updates", "deletes"}
_SCREEN_REL_TYPES = {"sends_to", "receives_from"}
_CALL_REL_TYPES = {"calls"}


def _make_node_id(name: str) -> str:
    """Convert a paragraph or artifact name to a valid Mermaid node ID.

    Mermaid node IDs must consist of alphanumeric characters and underscores.
    This function replaces all non-alphanumeric characters with underscores
    and ensures the result is non-empty.

    Args:
        name: The original paragraph or artifact name.

    Returns:
        A sanitized string suitable for use as a Mermaid node ID.
    """
    sanitized = re.sub(r"[^a-zA-Z0-9]", "_", name)
    if not sanitized:
        sanitized = "node"
    return sanitized


def _make_node_label(name: str) -> str:
    """Create a safe Mermaid label by escaping quotes.

    Args:
        name: The display name for the node.

    Returns:
        A string safe to embed inside Mermaid node definitions.
    """
    return name.replace('"', "'")


def _classify_external_node(
    relationship: str,
    target_type: str | None,
) -> str:
    """Determine the Mermaid node shape category for an external call.

    Returns one of: ``"program"``, ``"data"``, ``"screen"``, or ``"other"``.

    Args:
        relationship: The callout relationship type.
        target_type: The target artifact type, if known.

    Returns:
        A classification string for choosing the Mermaid shape.
    """
    # First, check target_type if available.
    if target_type:
        tt = target_type.lower()
        if tt in _PROGRAM_TARGET_TYPES:
            return "program"
        if tt in _DATA_TARGET_TYPES:
            return "data"
        if tt in _SCREEN_TARGET_TYPES:
            return "screen"

    # Fall back to relationship type heuristics.
    rel = relationship.lower()
    if rel in _CALL_REL_TYPES:
        return "program"
    if rel in _DATA_REL_TYPES:
        return "data"
    if rel in _SCREEN_REL_TYPES:
        return "screen"

    return "other"


def _format_node_definition(node_id: str, label: str, category: str) -> str:
    """Generate a Mermaid node definition with the appropriate shape.

    Args:
        node_id: The sanitized Mermaid node ID.
        label: The display label for the node.
        category: One of ``"internal"``, ``"program"``, ``"data"``,
            ``"screen"``, or ``"other"``.

    Returns:
        A Mermaid node definition string (e.g., ``nodeId[Label]``).
    """
    safe_label = _make_node_label(label)
    if category == "internal":
        return f'{node_id}["{safe_label}"]'
    elif category == "program":
        return f'{node_id}(["{safe_label}"])'
    elif category == "data":
        return f'{node_id}[("{safe_label}")]'
    elif category == "screen":
        return f'{node_id}{{{{"{safe_label}"}}}}'
    else:
        return f'{node_id}["{safe_label}"]'


def generate_flow_diagram_from_ast(
    ast_data: dict,
    start_paragraph: str | None = None,
    include_external: bool = True,
    title: str | None = None,
) -> str:
    """Generate a Mermaid flowchart from a pre-parsed ``.ast`` JSON file.

    This is the preferred path for COBOL files — it reads the AST produced
    by ProLeap/Cobalt rather than re-parsing the source with regex.

    The output is identical in format to ``generate_flow_diagram()`` but
    derived from typed AST statement nodes (``PERFORM_THRU``, ``CALL``,
    ``EXEC_CICS``, etc.) instead of regex-extracted callouts.

    Args:
        ast_data: Parsed content of a ``.ast`` JSON file with keys
            ``program_id`` and ``paragraphs``.
        start_paragraph: Optional starting paragraph name for BFS filter.
        include_external: Whether to show external calls as leaf nodes.
        title: Optional title comment for the diagram.

    Returns:
        A Mermaid flowchart string.
    """
    paragraphs = ast_data.get("paragraphs", [])
    if not paragraphs:
        return "flowchart TD\n    %% No paragraphs found"

    para_names: set[str] = set()
    canonical: dict[str, str] = {}
    for p in paragraphs:
        name = p.get("name", "")
        if name:
            para_names.add(name.lower())
            canonical[name.lower()] = name

    # Statement types that represent internal PERFORM calls
    _PERFORM_TYPES = {"PERFORM_THRU", "PERFORM", "PERFORM_UNTIL"}
    # Statement types for external calls
    _CALL_TYPES = {"CALL"}
    _EXEC_TYPES = {"EXEC_CICS", "EXEC_DLI", "EXEC_SQL", "EXEC_OTHER"}
    _IO_TYPES = {"READ_STMT", "WRITE_STMT", "REWRITE"}

    adjacency: dict[str, set[str]] = {}
    ext_callouts: dict[str, list[tuple[str, str, str | None]]] = {}

    def _collect(para_lower: str, stmts: list) -> None:
        for stmt in stmts:
            stype = stmt.get("type", "")
            attrs = stmt.get("attributes", {})
            target = attrs.get("target", "")

            if stype in _PERFORM_TYPES and target:
                t_lower = target.lower()
                if t_lower in para_names:
                    adjacency.setdefault(para_lower, set()).add(t_lower)
            elif stype in _CALL_TYPES and target:
                ext_callouts.setdefault(para_lower, []).append(
                    (target, "calls", "program")
                )
            elif stype in _EXEC_TYPES:
                raw = attrs.get("raw_text", stmt.get("text", stype))
                # Extract a short label from the raw EXEC text
                label = raw.split()[1] if len(raw.split()) > 1 else stype
                category = "screen" if stype == "EXEC_CICS" else "data"
                ext_callouts.setdefault(para_lower, []).append(
                    (label, stype.lower().replace("_", " "), category)
                )
            elif stype in _IO_TYPES and target:
                ext_callouts.setdefault(para_lower, []).append(
                    (target, "reads" if "READ" in stype else "writes", "data")
                )

            # Recurse into children (IF, EVALUATE, PERFORM INLINE, etc.)
            children = stmt.get("children")
            if children:
                _collect(para_lower, children)

    for p in paragraphs:
        name_lower = p["name"].lower()
        adjacency.setdefault(name_lower, set())
        ext_callouts.setdefault(name_lower, [])
        _collect(name_lower, p.get("statements", []))

    # BFS filter if start_paragraph given
    if start_paragraph is not None:
        s_lower = start_paragraph.lower()
        if s_lower not in para_names:
            raise ValueError(
                f"Paragraph '{start_paragraph}' not found. "
                f"Available: {sorted(canonical.values())}"
            )
        included = _bfs_reachable(s_lower, adjacency)
    else:
        included = set(para_names)

    if not included:
        return "flowchart TD\n    %% No paragraphs found"

    # Generate Mermaid (same format as generate_flow_diagram)
    lines: list[str] = ["flowchart TD"]
    if title:
        lines.append(f"    %% Title: {title}")

    defined: set[str] = set()
    edges: list[str] = []
    seen_int: set[tuple[str, str]] = set()
    seen_ext: set[tuple[str, str, str]] = set()

    for pl in sorted(included):
        pname = canonical.get(pl, pl)
        nid = _make_node_id(pname)

        if nid not in defined:
            lines.append(f"    {_format_node_definition(nid, pname, 'internal')}")
            defined.add(nid)

        for tl in sorted(adjacency.get(pl, set())):
            if tl not in included:
                continue
            ek = (pl, tl)
            if ek in seen_int:
                continue
            seen_int.add(ek)
            tname = canonical.get(tl, tl)
            tid = _make_node_id(tname)
            if tid not in defined:
                lines.append(
                    f"    {_format_node_definition(tid, tname, 'internal')}"
                )
                defined.add(tid)
            edges.append(f"    {nid} --> {tid}")

        if include_external:
            for tgt, rel, ttype in ext_callouts.get(pl, []):
                cat = _classify_external_node(rel, ttype)
                ext_id = _make_node_id(tgt) + "__ext"
                ek_ext = (pl, tgt.lower(), rel.lower())
                if ek_ext in seen_ext:
                    continue
                seen_ext.add(ek_ext)
                if ext_id not in defined:
                    lines.append(
                        f"    {_format_node_definition(ext_id, tgt, cat)}"
                    )
                    defined.add(ext_id)
                edges.append(f"    {nid} -.->|{rel}| {ext_id}")

    lines.extend(edges)
    return "\n".join(lines)


def generate_flow_diagram(
    analysis_result: FileAnalysisResult,
    start_paragraph: str | None = None,
    include_external: bool = True,
    title: str | None = None,
) -> str:
    """Generate a Mermaid flowchart for a COBOL file's internal control flow.

    Builds a directed graph of PERFORM relationships between paragraphs,
    then renders it as a Mermaid ``flowchart TD`` diagram. External calls
    (CALL, reads, writes, EXEC SQL, EXEC CICS, includes) appear as
    distinctively shaped leaf nodes connected by dashed arrows.

    Args:
        analysis_result: A ``FileAnalysisResult`` obtained from
            ``Citadel.analyze_file()``.
        start_paragraph: Optional starting paragraph name. When provided,
            only paragraphs reachable from this paragraph (via BFS over
            PERFORM edges) are included. When ``None``, all paragraphs
            are shown.
        include_external: Whether to show external calls (CALL, reads,
            writes, etc.) as leaf nodes. Defaults to ``True``.
        title: Optional title to display at the top of the diagram.

    Returns:
        A Mermaid flowchart string suitable for rendering or embedding
        in Markdown.

    Raises:
        ValueError: If ``start_paragraph`` is specified but does not
            exist in the analysis result.
    """
    # Step 1: Identify all paragraph artifacts and build lookup maps.
    paragraph_names: set[str] = set()
    paragraph_artifacts: dict[str, object] = {}  # name_lower -> FileArtifact

    for artifact in analysis_result.artifacts:
        if artifact.type in _CONTAINER_TYPES:
            continue
        paragraph_names.add(artifact.name.lower())
        paragraph_artifacts[artifact.name.lower()] = artifact

    # Step 2: Build adjacency map for internal PERFORM edges.
    # adjacency[source_lower] = set of target_lower paragraph names
    adjacency: dict[str, set[str]] = {}
    # Also track external callouts per paragraph.
    # external_callouts[source_lower] = list of (target_name, relationship, target_type)
    external_callouts: dict[str, list[tuple[str, str, str | None]]] = {}

    for artifact in analysis_result.artifacts:
        if artifact.type in _CONTAINER_TYPES:
            continue

        src_lower = artifact.name.lower()
        adjacency.setdefault(src_lower, set())
        external_callouts.setdefault(src_lower, [])

        for callout in artifact.callouts:
            rel = callout.relationship.lower()
            target_lower = callout.target.lower() if callout.target else ""

            if rel in _INTERNAL_REL_TYPES and target_lower in paragraph_names:
                adjacency[src_lower].add(target_lower)
            elif rel in _EXTERNAL_REL_TYPES:
                external_callouts[src_lower].append(
                    (callout.target, callout.relationship, callout.target_type)
                )

    # Step 3: Determine which paragraphs to include.
    if start_paragraph is not None:
        start_lower = start_paragraph.lower()
        if start_lower not in paragraph_names:
            raise ValueError(
                f"Paragraph '{start_paragraph}' not found in "
                f"{analysis_result.file_path}. Available paragraphs: "
                f"{sorted(a.name for a in analysis_result.artifacts if a.type not in _CONTAINER_TYPES)}"
            )
        included_paragraphs = _bfs_reachable(start_lower, adjacency)
    else:
        included_paragraphs = set(paragraph_names)

    if not included_paragraphs:
        return "flowchart TD\n    %% No paragraphs found"

    # Step 4: Generate Mermaid output.
    lines: list[str] = ["flowchart TD"]

    if title:
        # Mermaid does not natively support a title directive for flowcharts
        # in all renderers, so we include it as a comment and a subgraph
        # title pattern.
        lines.append(f"    %% Title: {title}")

    # Collect all node definitions and edges.
    defined_nodes: set[str] = set()
    edges: list[str] = []
    # Track de-duplicated edges.
    seen_internal_edges: set[tuple[str, str]] = set()
    seen_external_edges: set[tuple[str, str, str]] = set()

    # Build a canonical-name lookup (lower -> original case).
    canonical_name: dict[str, str] = {}
    for artifact in analysis_result.artifacts:
        if artifact.type not in _CONTAINER_TYPES:
            canonical_name[artifact.name.lower()] = artifact.name

    # Sort included paragraphs for deterministic output.
    for para_lower in sorted(included_paragraphs):
        para_name = canonical_name.get(para_lower, para_lower)
        node_id = _make_node_id(para_name)

        # Define the paragraph node.
        if node_id not in defined_nodes:
            lines.append(
                f"    {_format_node_definition(node_id, para_name, 'internal')}"
            )
            defined_nodes.add(node_id)

        # Internal PERFORM edges.
        for target_lower in sorted(adjacency.get(para_lower, set())):
            if target_lower not in included_paragraphs:
                continue

            edge_key = (para_lower, target_lower)
            if edge_key in seen_internal_edges:
                continue
            seen_internal_edges.add(edge_key)

            target_name = canonical_name.get(target_lower, target_lower)
            target_id = _make_node_id(target_name)

            # Ensure the target node is defined.
            if target_id not in defined_nodes:
                lines.append(
                    f"    {_format_node_definition(target_id, target_name, 'internal')}"
                )
                defined_nodes.add(target_id)

            edges.append(f"    {node_id} --> {target_id}")

        # External callout edges.
        if include_external:
            for target_name, rel, target_type in external_callouts.get(
                para_lower, []
            ):
                category = _classify_external_node(rel, target_type)
                ext_node_id = _make_node_id(target_name) + "__ext"

                edge_key_ext = (para_lower, target_name.lower(), rel.lower())
                if edge_key_ext in seen_external_edges:
                    continue
                seen_external_edges.add(edge_key_ext)

                if ext_node_id not in defined_nodes:
                    lines.append(
                        f"    {_format_node_definition(ext_node_id, target_name, category)}"
                    )
                    defined_nodes.add(ext_node_id)

                edges.append(f"    {node_id} -.->|{rel}| {ext_node_id}")

    # Append all edges after node definitions for readability.
    lines.extend(edges)

    return "\n".join(lines)


def _bfs_reachable(
    start: str,
    adjacency: dict[str, set[str]],
) -> set[str]:
    """Find all paragraphs reachable from ``start`` via BFS.

    Handles cycles gracefully by tracking visited nodes.

    Args:
        start: The starting paragraph name (lowercase).
        adjacency: Adjacency map of paragraph-level PERFORM edges.

    Returns:
        Set of reachable paragraph names (lowercase), including ``start``.
    """
    visited: set[str] = set()
    queue: deque[str] = deque([start])

    while queue:
        current = queue.popleft()
        if current in visited:
            continue
        visited.add(current)

        for neighbor in adjacency.get(current, set()):
            if neighbor not in visited:
                queue.append(neighbor)

    return visited
