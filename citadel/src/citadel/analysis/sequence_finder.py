"""
Sequence finder for dependency graph analysis.

This module provides algorithms for finding the longest call sequences
in a dependency graph, suitable for generating Mermaid sequence diagrams.

Mathematical Background
-----------------------
The problem of finding longest paths in a directed graph is NP-hard in the
general case (when cycles are present). However, we can efficiently find
longest simple paths (paths without repeated vertices) using DFS with
memoization and cycle detection.

Algorithm Overview
------------------
1. Build an adjacency list from the edge list, filtering for call-type relationships
2. Identify entry points: artifacts with no incoming call edges (potential sequence starts)
3. For each entry point, perform DFS to find the longest path from that node
4. Use memoization to cache the longest path from each node (in a DAG subgraph)
5. Track visited nodes in the current path to detect and avoid cycles
6. Return all maximal-length sequences from different connected components

Time Complexity: O(V + E) for DAG portions, potentially O(V * E) with cycles due
to path enumeration, but cycle detection prevents infinite recursion.

Space Complexity: O(V + E) for the adjacency list, O(V) for recursion stack and
memoization cache.
"""

from __future__ import annotations

from collections import defaultdict
from dataclasses import dataclass, field
from typing import Iterator


@dataclass
class _GraphContext:
    """
    Internal context for graph traversal.

    Holds the adjacency list and provides efficient lookup methods.
    """

    # adjacency_list[source] = [(target, relationship_type), ...]
    adjacency_list: dict[str, list[tuple[str, str]]] = field(default_factory=dict)

    # reverse_adjacency[target] = [source, ...]  (for finding entry points)
    reverse_adjacency: dict[str, set[str]] = field(default_factory=dict)

    # All nodes in the graph
    nodes: set[str] = field(default_factory=set)


def _build_graph_context(
    edges: list[dict],
    relationship_types: list[str],
) -> _GraphContext:
    """
    Build adjacency lists from edge data.

    Args:
        edges: List of edge dictionaries with 'source', 'target', 'relationship_type'.
        relationship_types: List of relationship types to include.

    Returns:
        _GraphContext with populated adjacency lists.

    Time Complexity: O(E) where E is the number of edges.
    Space Complexity: O(E) for the adjacency lists.
    """
    ctx = _GraphContext(
        adjacency_list=defaultdict(list),
        reverse_adjacency=defaultdict(set),
        nodes=set(),
    )

    rel_type_set = set(relationship_types)

    for edge in edges:
        rel_type = edge.get("relationship_type", "")

        # Handle both string and enum-like objects
        if hasattr(rel_type, "value"):
            rel_type = rel_type.value

        if rel_type not in rel_type_set:
            continue

        source = edge.get("source") or edge.get("from_artifact")
        target = edge.get("target") or edge.get("to_artifact")

        if not source or not target:
            continue

        ctx.adjacency_list[source].append((target, rel_type))
        ctx.reverse_adjacency[target].add(source)
        ctx.nodes.add(source)
        ctx.nodes.add(target)

    return ctx


def _find_entry_points(ctx: _GraphContext) -> list[str]:
    """
    Find potential entry points for call sequences.

    An entry point is a node with no incoming call edges, making it a
    natural starting point for a sequence diagram. These are typically
    top-level programs that are not called by other programs.

    Args:
        ctx: Graph context with adjacency information.

    Returns:
        List of artifact IDs that have no incoming call edges.

    Time Complexity: O(V) where V is the number of vertices.
    """
    entry_points = []

    for node in ctx.nodes:
        if node not in ctx.reverse_adjacency or len(ctx.reverse_adjacency[node]) == 0:
            entry_points.append(node)

    # Sort for deterministic output
    return sorted(entry_points)


def _dfs_longest_path(
    node: str,
    ctx: _GraphContext,
    path_visited: set[str],
    memo: dict[str, list[tuple[str, str, str]]],
) -> list[tuple[str, str, str]]:
    """
    Find the longest path from a given node using DFS with memoization.

    This function performs depth-first search to find the longest simple
    path (no repeated vertices) starting from the given node.

    Cycle Handling:
        - path_visited tracks nodes in the current DFS path to detect cycles
        - When a cycle is detected, we return an empty path for that branch
        - Memoization is only applied for fully explored subgraphs

    Args:
        node: Current node being explored.
        ctx: Graph context with adjacency information.
        path_visited: Set of nodes already in the current path (for cycle detection).
        memo: Memoization cache for longest paths from explored nodes.

    Returns:
        List of (caller, callee, relationship_type) tuples representing the
        longest path from this node.

    Invariants:
        - The returned path starts with an edge from 'node'
        - No vertex appears more than once in the path
        - The path is maximal (no longer simple path exists from 'node')
    """
    # Check memoization cache (only valid if node is not in current path)
    if node in memo and node not in path_visited:
        return memo[node]

    # No outgoing edges = end of sequence
    if node not in ctx.adjacency_list or not ctx.adjacency_list[node]:
        return []

    # Mark as visited in current path
    path_visited.add(node)

    best_path: list[tuple[str, str, str]] = []

    for target, rel_type in ctx.adjacency_list[node]:
        # Skip if target already in path (cycle detection)
        if target in path_visited:
            continue

        # Recursively find longest path from target
        sub_path = _dfs_longest_path(target, ctx, path_visited, memo)

        # Build candidate path: edge to target + longest path from target
        candidate = [(node, target, rel_type)] + sub_path

        # Keep the longest path
        if len(candidate) > len(best_path):
            best_path = candidate

    # Unmark from current path (backtracking)
    path_visited.remove(node)

    # Memoize result (safe since we've backtracked)
    memo[node] = best_path

    return best_path


def _find_all_maximal_sequences(
    ctx: _GraphContext,
) -> list[list[tuple[str, str, str]]]:
    """
    Find all maximal-length sequences from entry points.

    This function explores each entry point and collects the longest
    sequences. For disconnected subgraphs, it ensures we find sequences
    from each component.

    Args:
        ctx: Graph context with adjacency information.

    Returns:
        List of sequences, where each sequence is a list of
        (caller, callee, relationship_type) tuples.

    Algorithm:
        1. Identify entry points (nodes with no incoming edges)
        2. For each entry point, find the longest path via DFS
        3. Track which nodes have been covered
        4. For uncovered nodes with outgoing edges, also find paths
           (handles cycles where every node has an incoming edge)
    """
    sequences: list[list[tuple[str, str, str]]] = []
    covered_nodes: set[str] = set()
    memo: dict[str, list[tuple[str, str, str]]] = {}

    # First, explore from all entry points
    entry_points = _find_entry_points(ctx)

    for entry in entry_points:
        if entry in covered_nodes:
            continue

        path_visited: set[str] = set()
        sequence = _dfs_longest_path(entry, ctx, path_visited, memo)

        if sequence:
            sequences.append(sequence)
            # Mark all nodes in this sequence as covered
            for caller, callee, _ in sequence:
                covered_nodes.add(caller)
                covered_nodes.add(callee)

    # Handle nodes in cycles that have no true entry points
    # These are nodes with outgoing edges that haven't been covered
    remaining = [
        node
        for node in ctx.nodes
        if node not in covered_nodes and node in ctx.adjacency_list
    ]

    for node in sorted(remaining):
        if node in covered_nodes:
            continue

        path_visited: set[str] = set()
        sequence = _dfs_longest_path(node, ctx, path_visited, memo)

        if sequence:
            sequences.append(sequence)
            for caller, callee, _ in sequence:
                covered_nodes.add(caller)
                covered_nodes.add(callee)

    return sequences


def _get_sequence_participants(
    sequence: list[tuple[str, str, str]],
) -> list[str]:
    """
    Extract unique participants from a sequence in order of appearance.

    Args:
        sequence: List of (caller, callee, relationship_type) tuples.

    Returns:
        Ordered list of unique participant artifact IDs.
    """
    seen: set[str] = set()
    participants: list[str] = []

    for caller, callee, _ in sequence:
        if caller not in seen:
            seen.add(caller)
            participants.append(caller)
        if callee not in seen:
            seen.add(callee)
            participants.append(callee)

    return participants


def find_longest_sequences(
    artifacts: dict[str, dict],
    edges: list[dict],
    relationship_types: list[str] | None = None,
    min_length: int = 1,
    max_sequences: int | None = None,
) -> list[list[tuple[str, str, str]]]:
    """
    Find longest call sequences from a dependency graph.

    This function identifies the longest call chains in the graph, suitable
    for generating Mermaid sequence diagrams. It handles cycles gracefully,
    explores multiple entry points, and returns sequences from disconnected
    subgraphs.

    Mathematical Formulation:
        Given a directed graph G = (V, E) where V is the set of artifacts
        and E is the set of call-type relationships, find a collection of
        simple paths P = {p1, p2, ...} such that:
        1. Each pi is a maximal simple path (cannot be extended)
        2. Paths are chosen to maximize coverage of edges
        3. Paths do not share their starting vertices

    Args:
        artifacts: Dictionary mapping artifact_id to artifact properties.
            Expected keys include 'name', 'type', 'defined_in', etc.
            This parameter is accepted for API consistency but the algorithm
            primarily uses the edges for path finding.

        edges: List of edge dictionaries. Each edge should have:
            - 'source' or 'from_artifact': Source artifact ID
            - 'target' or 'to_artifact': Target artifact ID
            - 'relationship_type': Type of relationship (string or enum)

        relationship_types: List of relationship types to consider as "calls".
            Defaults to ["calls", "executes", "performs"].

        min_length: Minimum number of edges in a sequence.
            Sequences shorter than this are excluded. Default is 1.
            Note: A sequence diagram needs at least 2 participants (1 edge).

        max_sequences: Maximum number of sequences to return.
            If None, returns all sequences. Sequences are sorted by length
            (longest first) before truncation.

    Returns:
        List of sequences, where each sequence is an ordered list of
        (caller, callee, relationship_type) tuples representing the call chain.

        Sequences are sorted by length in descending order (longest first).

    Examples:
        >>> artifacts = {
        ...     "prog::A": {"name": "A", "type": "program"},
        ...     "prog::B": {"name": "B", "type": "program"},
        ...     "prog::C": {"name": "C", "type": "program"},
        ... }
        >>> edges = [
        ...     {"source": "prog::A", "target": "prog::B", "relationship_type": "calls"},
        ...     {"source": "prog::B", "target": "prog::C", "relationship_type": "calls"},
        ... ]
        >>> sequences = find_longest_sequences(artifacts, edges)
        >>> len(sequences)
        1
        >>> sequences[0]
        [('prog::A', 'prog::B', 'calls'), ('prog::B', 'prog::C', 'calls')]

    Complexity:
        Time: O(V + E) for DAG graphs, O(V * E) worst case with many cycles
        Space: O(V + E) for adjacency list and memoization

    Notes:
        - Cycles are handled by tracking the current path and avoiding revisits
        - Entry points are nodes with no incoming call-type edges
        - The algorithm uses memoization for efficiency on DAG portions
        - For disconnected graphs, sequences from each component are included
    """
    if relationship_types is None:
        relationship_types = ["calls", "executes", "performs"]

    # Build graph context
    ctx = _build_graph_context(edges, relationship_types)

    if not ctx.nodes:
        return []

    # Find all maximal sequences
    sequences = _find_all_maximal_sequences(ctx)

    # Filter by minimum length
    sequences = [seq for seq in sequences if len(seq) >= min_length]

    # Sort by length (longest first)
    sequences.sort(key=len, reverse=True)

    # Truncate if max_sequences specified
    if max_sequences is not None:
        sequences = sequences[:max_sequences]

    return sequences


def sequences_to_mermaid(
    sequences: list[list[tuple[str, str, str]]],
    artifacts: dict[str, dict] | None = None,
    title: str | None = None,
) -> str:
    """
    Convert call sequences to Mermaid sequence diagram syntax.

    This is a helper function that generates Mermaid markup from the
    sequences returned by find_longest_sequences.

    Args:
        sequences: List of sequences from find_longest_sequences.
        artifacts: Optional artifact dictionary for display names.
            If provided, uses 'name' or 'display_name' instead of IDs.
        title: Optional title for the sequence diagram.

    Returns:
        Mermaid sequence diagram as a string.

    Example:
        >>> sequences = [[('A', 'B', 'calls'), ('B', 'C', 'executes')]]
        >>> print(sequences_to_mermaid(sequences, title="Call Flow"))
        sequenceDiagram
            title Call Flow
            A->>B: calls
            B->>C: executes
    """
    if not sequences:
        return "sequenceDiagram\n    Note right of Start: No sequences found"

    lines = ["sequenceDiagram"]

    if title:
        lines.append(f"    title {title}")

    def get_display_name(artifact_id: str) -> str:
        """Get a display-friendly name for an artifact."""
        if artifacts and artifact_id in artifacts:
            art = artifacts[artifact_id]
            name = art.get("display_name") or art.get("name") or art.get("canonical_name")
            if name:
                return name
        # Extract name from ID (e.g., "program::FOO" -> "FOO")
        if "::" in artifact_id:
            return artifact_id.split("::")[-1]
        return artifact_id

    # Track all participants for declaration
    all_participants: dict[str, str] = {}  # id -> display_name

    for sequence in sequences:
        for caller, callee, _ in sequence:
            if caller not in all_participants:
                all_participants[caller] = get_display_name(caller)
            if callee not in all_participants:
                all_participants[callee] = get_display_name(callee)

    # Declare participants
    for artifact_id, display_name in all_participants.items():
        # Sanitize name for Mermaid (remove special chars)
        safe_name = "".join(c if c.isalnum() or c == "_" else "_" for c in display_name)
        lines.append(f"    participant {safe_name} as {display_name}")

    # Add sequence arrows
    for i, sequence in enumerate(sequences):
        if i > 0:
            lines.append("")  # Blank line between sequences
            lines.append(f"    Note over {get_display_name(sequence[0][0])}: New sequence")

        for caller, callee, rel_type in sequence:
            caller_name = "".join(
                c if c.isalnum() or c == "_" else "_"
                for c in get_display_name(caller)
            )
            callee_name = "".join(
                c if c.isalnum() or c == "_" else "_"
                for c in get_display_name(callee)
            )
            lines.append(f"    {caller_name}->>{callee_name}: {rel_type}")

    return "\n".join(lines)


def find_sequences_containing(
    artifact_id: str,
    artifacts: dict[str, dict],
    edges: list[dict],
    relationship_types: list[str] | None = None,
    context_depth: int = 2,
) -> list[list[tuple[str, str, str]]]:
    """
    Find call sequences that contain a specific artifact.

    This is useful for generating a focused sequence diagram around
    a particular program or function of interest.

    Args:
        artifact_id: The artifact to include in sequences.
        artifacts: Dictionary mapping artifact_id to artifact properties.
        edges: List of edge dictionaries.
        relationship_types: Types of relationships to follow.
        context_depth: How many levels of callers/callees to include.

    Returns:
        List of sequences that include the specified artifact,
        with context_depth levels of ancestors and descendants.
    """
    if relationship_types is None:
        relationship_types = ["calls", "executes", "performs"]

    # Build graph context
    ctx = _build_graph_context(edges, relationship_types)

    if artifact_id not in ctx.nodes:
        return []

    # Find ancestors (callers) up to context_depth
    ancestors: set[str] = set()
    frontier = {artifact_id}

    for _ in range(context_depth):
        new_frontier: set[str] = set()
        for node in frontier:
            if node in ctx.reverse_adjacency:
                for parent in ctx.reverse_adjacency[node]:
                    if parent not in ancestors:
                        ancestors.add(parent)
                        new_frontier.add(parent)
        frontier = new_frontier

    # Find descendants (callees) up to context_depth
    descendants: set[str] = set()
    frontier = {artifact_id}

    for _ in range(context_depth):
        new_frontier: set[str] = set()
        for node in frontier:
            if node in ctx.adjacency_list:
                for child, _ in ctx.adjacency_list[node]:
                    if child not in descendants:
                        descendants.add(child)
                        new_frontier.add(child)
        frontier = new_frontier

    # Build subgraph with relevant nodes
    relevant_nodes = ancestors | descendants | {artifact_id}

    # Filter edges to only those between relevant nodes
    subgraph_edges = [
        edge
        for edge in edges
        if (
            (edge.get("source") or edge.get("from_artifact")) in relevant_nodes
            and (edge.get("target") or edge.get("to_artifact")) in relevant_nodes
        )
    ]

    # Find sequences in the subgraph
    return find_longest_sequences(
        artifacts=artifacts,
        edges=subgraph_edges,
        relationship_types=relationship_types,
    )


def iter_all_simple_paths(
    source: str,
    ctx: _GraphContext,
    max_length: int | None = None,
) -> Iterator[list[tuple[str, str, str]]]:
    """
    Iterate over all simple paths from a source node.

    This generator yields all possible simple paths (no repeated vertices)
    starting from the source node. Useful when you need all paths rather
    than just the longest.

    Warning: This can generate exponentially many paths for dense graphs.
    Use max_length to limit exploration.

    Args:
        source: Starting node.
        ctx: Graph context with adjacency information.
        max_length: Maximum path length (number of edges). None for unlimited.

    Yields:
        Paths as lists of (caller, callee, relationship_type) tuples.
    """
    stack: list[tuple[str, list[tuple[str, str, str]], set[str]]] = [
        (source, [], {source})
    ]

    while stack:
        current, path, visited = stack.pop()

        # Yield current path if non-empty
        if path:
            yield path

        # Check max length
        if max_length is not None and len(path) >= max_length:
            continue

        # Explore neighbors
        if current in ctx.adjacency_list:
            for neighbor, rel_type in ctx.adjacency_list[current]:
                if neighbor not in visited:
                    new_path = path + [(current, neighbor, rel_type)]
                    new_visited = visited | {neighbor}
                    stack.append((neighbor, new_path, new_visited))
