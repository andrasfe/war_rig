#!/usr/bin/env python3
"""Generate a call graph from individual doc.json files.

Analyzes called_programs from each documentation file to build a comprehensive
call graph showing program relationships, entry points, and external dependencies.

Usage:
    python scripts/generate_call_graph.py output/
    python scripts/generate_call_graph.py output/ --output output/CALL_GRAPH.md
"""

import argparse
import json
import re
from collections import defaultdict
from datetime import datetime
from pathlib import Path


# Pattern to detect dynamic program names (contains parentheses with variables)
DYNAMIC_PROGRAM_PATTERN = re.compile(r"\([A-Z0-9-]+\)")


def sanitize_mermaid_id(name: str) -> str:
    """Sanitize a name to be a valid Mermaid node ID.

    Mermaid interprets hyphens as minus operators, so we replace them with underscores.
    We also need to handle parentheses and other special characters.
    """
    # Replace hyphens with underscores
    sanitized = name.replace("-", "_")
    # Remove parentheses and their contents for node IDs (keep in labels)
    sanitized = re.sub(r"\([^)]*\)", "", sanitized)
    # Remove any remaining non-alphanumeric characters except underscores
    sanitized = re.sub(r"[^A-Za-z0-9_]", "", sanitized)
    return sanitized


def load_documentation(doc_dir: Path) -> list[dict]:
    """Load all .doc.json files from directory."""
    docs = []
    for doc_file in sorted(doc_dir.glob("*.doc.json")):
        try:
            data = json.loads(doc_file.read_text())
            data["_source_file"] = doc_file.name
            docs.append(data)
        except (json.JSONDecodeError, IOError) as e:
            print(f"Warning: Could not load {doc_file}: {e}")
    return docs


def get_program_id(doc: dict) -> str:
    """Extract program ID from documentation."""
    if "header" in doc and "program_id" in doc["header"]:
        return doc["header"]["program_id"]
    if "program_id" in doc:
        return doc["program_id"]
    return doc.get("_source_file", "Unknown").replace(".doc.json", "")


def is_dynamic_call(program_name: str) -> bool:
    """Check if a program name represents a dynamic call (variable-based)."""
    return bool(DYNAMIC_PROGRAM_PATTERN.search(program_name))


def extract_base_name(program_name: str) -> str:
    """Extract the base variable/array name from a dynamic call."""
    # For names like "CDEMO-MENU-OPT-PGMNAME(WS-OPTION)", return as-is for display
    return program_name


def build_call_graph(docs: list[dict]) -> dict:
    """Build call graph data structures from documentation.

    Returns a dict containing:
        - documented_programs: set of all documented program IDs
        - adjacency: dict mapping caller -> list of (callee, call_type, is_dynamic)
        - reverse_adjacency: dict mapping callee -> list of callers
        - dynamic_calls: list of (caller, callee_pattern, call_type)
        - external_programs: set of programs called but not documented
    """
    documented_programs = set()
    adjacency: dict[str, list[tuple[str, str, bool]]] = defaultdict(list)
    reverse_adjacency: dict[str, list[str]] = defaultdict(list)
    dynamic_calls: list[tuple[str, str, str]] = []
    all_callees: set[str] = set()

    # First pass: collect all documented program IDs
    for doc in docs:
        program_id = get_program_id(doc)
        documented_programs.add(program_id)

    # Second pass: extract called_programs relationships
    for doc in docs:
        caller = get_program_id(doc)
        called_programs = doc.get("called_programs", [])

        if not called_programs:
            continue

        for call_info in called_programs:
            if isinstance(call_info, dict):
                callee = call_info.get("program_name", "")
                call_type = call_info.get("call_type", "UNKNOWN")
            elif isinstance(call_info, str):
                callee = call_info
                call_type = "UNKNOWN"
            else:
                continue

            if not callee:
                continue

            is_dynamic = is_dynamic_call(callee)

            if is_dynamic:
                dynamic_calls.append((caller, callee, call_type))
            else:
                adjacency[caller].append((callee, call_type, False))
                reverse_adjacency[callee].append(caller)
                all_callees.add(callee)

    # Identify external programs (called but not documented)
    external_programs = all_callees - documented_programs

    return {
        "documented_programs": documented_programs,
        "adjacency": dict(adjacency),
        "reverse_adjacency": dict(reverse_adjacency),
        "dynamic_calls": dynamic_calls,
        "external_programs": external_programs,
    }


def find_entry_points(graph: dict) -> set[str]:
    """Find programs that have no callers (entry points).

    Entry points are documented programs that are never called by other
    documented programs (excluding dynamic calls which we can't trace).
    """
    documented = graph["documented_programs"]
    reverse_adj = graph["reverse_adjacency"]

    entry_points = set()
    for program in documented:
        callers = reverse_adj.get(program, [])
        # Filter to only documented callers
        documented_callers = [c for c in callers if c in documented]
        if not documented_callers:
            entry_points.add(program)

    return entry_points


def find_leaf_nodes(graph: dict) -> set[str]:
    """Find programs that don't call any other programs (leaf nodes)."""
    documented = graph["documented_programs"]
    adjacency = graph["adjacency"]

    leaf_nodes = set()
    for program in documented:
        calls = adjacency.get(program, [])
        # Filter to only documented callees
        documented_calls = [c for c, _, _ in calls if c in documented]
        if not documented_calls:
            leaf_nodes.add(program)

    return leaf_nodes


def build_call_chains(graph: dict, max_depth: int = 10) -> list[list[str]]:
    """Build representative call chains starting from entry points.

    Returns a list of call chains, where each chain is a list of program names.
    """
    entry_points = find_entry_points(graph)
    adjacency = graph["adjacency"]
    documented = graph["documented_programs"]

    chains = []
    visited_chains: set[tuple[str, ...]] = set()

    def dfs_chain(current: str, path: list[str], depth: int) -> None:
        if depth > max_depth:
            return

        calls = adjacency.get(current, [])
        documented_calls = [(c, ct) for c, ct, _ in calls if c in documented]

        if not documented_calls:
            # This is a leaf - record the chain if it has length > 1
            if len(path) > 1:
                chain_tuple = tuple(path)
                if chain_tuple not in visited_chains:
                    visited_chains.add(chain_tuple)
                    chains.append(path.copy())
            return

        for callee, _ in documented_calls:
            if callee not in path:  # Avoid cycles
                path.append(callee)
                dfs_chain(callee, path, depth + 1)
                path.pop()

    for entry in sorted(entry_points):
        dfs_chain(entry, [entry], 0)

    return chains


def generate_mermaid_diagram(graph: dict) -> str:
    """Generate a Mermaid flowchart diagram of the call graph."""
    lines = ["```mermaid", "flowchart TD"]

    documented = graph["documented_programs"]
    adjacency = graph["adjacency"]
    dynamic_calls = graph["dynamic_calls"]
    external_programs = graph["external_programs"]
    entry_points = find_entry_points(graph)
    leaf_nodes = find_leaf_nodes(graph)

    # Define node styles
    lines.append("")
    lines.append("    %% Node definitions")

    # Add documented program nodes
    # Use sanitized IDs for Mermaid compatibility, but show original names in labels
    for program in sorted(documented):
        node_id = sanitize_mermaid_id(program)
        if program in entry_points:
            # Entry points get special styling (rounded rectangle)
            lines.append(f"    {node_id}([{program}])")
        elif program in leaf_nodes:
            # Leaf nodes get special styling (stadium shape)
            lines.append(f"    {node_id}[/{program}/]")
        else:
            lines.append(f"    {node_id}[{program}]")

    # Add external program nodes
    for program in sorted(external_programs):
        node_id = sanitize_mermaid_id(program)
        # External programs get asymmetric shape (flag)
        lines.append(f"    {node_id}>{program}]")

    lines.append("")
    lines.append("    %% Call relationships")

    # Add edges for regular calls
    for caller in sorted(adjacency.keys()):
        caller_id = sanitize_mermaid_id(caller)
        for callee, call_type, _ in adjacency[caller]:
            callee_id = sanitize_mermaid_id(callee)
            # Determine edge style based on call type
            if "XCTL" in call_type:
                # XCTL = transfer control (dashed line)
                lines.append(f"    {caller_id} -.->|XCTL| {callee_id}")
            elif "LINK" in call_type:
                # LINK = call and return (solid line)
                lines.append(f"    {caller_id} -->|LINK| {callee_id}")
            elif "DYNAMIC" in call_type:
                # Dynamic call
                lines.append(f"    {caller_id} -->|dynamic| {callee_id}")
            else:
                # Default solid line
                lines.append(f"    {caller_id} --> {callee_id}")

    # Add dynamic calls (to a placeholder node)
    if dynamic_calls:
        lines.append("")
        lines.append("    %% Dynamic calls")
        lines.append("    DYNAMIC_TARGETS{{Dynamic Targets}}")

        dynamic_callers = set()
        for caller, pattern, call_type in dynamic_calls:
            if caller not in dynamic_callers:
                dynamic_callers.add(caller)
                caller_id = sanitize_mermaid_id(caller)
                lines.append(f"    {caller_id} -.->|dynamic| DYNAMIC_TARGETS")

    # Add styling
    lines.append("")
    lines.append("    %% Styling")
    if entry_points:
        sanitized_entries = [sanitize_mermaid_id(p) for p in sorted(entry_points)]
        lines.append("    classDef entryPoint fill:#90EE90,stroke:#228B22")
        lines.append(f"    class {','.join(sanitized_entries)} entryPoint")

    if external_programs:
        sanitized_externals = [sanitize_mermaid_id(p) for p in sorted(external_programs)]
        lines.append("    classDef external fill:#FFB6C1,stroke:#DC143C")
        lines.append(f"    class {','.join(sanitized_externals)} external")

    if leaf_nodes - external_programs:
        internal_leaves = leaf_nodes - external_programs
        if internal_leaves:
            sanitized_leaves = [sanitize_mermaid_id(p) for p in sorted(internal_leaves)]
            lines.append("    classDef leafNode fill:#87CEEB,stroke:#4682B4")
            lines.append(f"    class {','.join(sanitized_leaves)} leafNode")

    lines.append("```")

    return "\n".join(lines)


def generate_text_summary(graph: dict, docs: list[dict]) -> str:
    """Generate a text summary of the call graph analysis."""
    lines = []

    documented = graph["documented_programs"]
    adjacency = graph["adjacency"]
    dynamic_calls = graph["dynamic_calls"]
    external_programs = graph["external_programs"]
    entry_points = find_entry_points(graph)
    leaf_nodes = find_leaf_nodes(graph)
    chains = build_call_chains(graph)

    # Entry Points Section
    lines.append("## Entry Points")
    lines.append("")
    lines.append("Programs with no documented callers (potential application entry points):")
    lines.append("")

    if entry_points:
        # Get program summaries for context
        program_summaries = {}
        for doc in docs:
            pid = get_program_id(doc)
            if "purpose" in doc and isinstance(doc["purpose"], dict):
                program_summaries[pid] = doc["purpose"].get("summary", "")[:100]
            elif "summary" in doc:
                program_summaries[pid] = doc["summary"][:100]

        for program in sorted(entry_points):
            summary = program_summaries.get(program, "No summary available")
            if len(summary) > 80:
                summary = summary[:77] + "..."
            lines.append(f"- **{program}**: {summary}")
    else:
        lines.append("*No entry points found (all programs are called by other programs)*")

    lines.append("")

    # Leaf Nodes Section
    lines.append("## Leaf Nodes")
    lines.append("")
    lines.append("Programs that don't call other documented programs:")
    lines.append("")

    internal_leaves = leaf_nodes - external_programs
    if internal_leaves:
        for program in sorted(internal_leaves):
            lines.append(f"- {program}")
    else:
        lines.append("*No leaf nodes found*")

    lines.append("")

    # Call Chains Section
    lines.append("## Call Chains")
    lines.append("")
    lines.append("Representative call chains from entry points to leaf nodes:")
    lines.append("")

    if chains:
        # Group chains by entry point
        chains_by_entry: dict[str, list[list[str]]] = defaultdict(list)
        for chain in chains:
            if chain:
                chains_by_entry[chain[0]].append(chain)

        for entry in sorted(chains_by_entry.keys()):
            lines.append(f"### From {entry}")
            lines.append("")

            # Show up to 5 chains per entry point
            entry_chains = chains_by_entry[entry][:5]
            for chain in entry_chains:
                chain_str = " -> ".join(chain)
                lines.append(f"- `{chain_str}`")

            if len(chains_by_entry[entry]) > 5:
                lines.append(f"- *... and {len(chains_by_entry[entry]) - 5} more chains*")

            lines.append("")
    else:
        lines.append("*No call chains found*")
        lines.append("")

    # External Dependencies Section
    lines.append("## External Dependencies")
    lines.append("")
    lines.append("Programs called but not documented in this codebase:")
    lines.append("")

    if external_programs:
        # Group by who calls them
        external_callers: dict[str, list[str]] = defaultdict(list)
        for caller, calls in adjacency.items():
            for callee, call_type, _ in calls:
                if callee in external_programs:
                    external_callers[callee].append(f"{caller} ({call_type})")

        for program in sorted(external_programs):
            callers = external_callers.get(program, [])
            callers_str = ", ".join(callers) if callers else "Unknown"
            lines.append(f"- **{program}**: Called by {callers_str}")
    else:
        lines.append("*No external dependencies found*")

    lines.append("")

    # Dynamic Calls Section
    lines.append("## Dynamic Calls")
    lines.append("")
    lines.append("Calls where the target program is determined at runtime:")
    lines.append("")

    if dynamic_calls:
        for caller, pattern, call_type in sorted(dynamic_calls, key=lambda x: x[0]):
            lines.append(f"- **{caller}** -> `{pattern}` ({call_type})")
    else:
        lines.append("*No dynamic calls found*")

    lines.append("")

    # Statistics Section
    lines.append("## Statistics")
    lines.append("")
    lines.append("| Metric | Count |")
    lines.append("|--------|-------|")
    lines.append(f"| Total Documented Programs | {len(documented)} |")
    lines.append(f"| Entry Points | {len(entry_points)} |")
    lines.append(f"| Leaf Nodes | {len(internal_leaves)} |")
    lines.append(f"| External Dependencies | {len(external_programs)} |")
    lines.append(f"| Dynamic Calls | {len(dynamic_calls)} |")

    # Count total static calls
    total_calls = sum(len(calls) for calls in adjacency.values())
    lines.append(f"| Total Static Calls | {total_calls} |")

    lines.append("")

    return "\n".join(lines)


def generate_adjacency_list(graph: dict) -> str:
    """Generate a detailed adjacency list view."""
    lines = []

    documented = graph["documented_programs"]
    adjacency = graph["adjacency"]
    reverse_adjacency = graph["reverse_adjacency"]

    lines.append("## Detailed Adjacency List")
    lines.append("")
    lines.append("### Programs and Their Calls")
    lines.append("")

    for program in sorted(documented):
        calls = adjacency.get(program, [])
        callers = reverse_adjacency.get(program, [])

        lines.append(f"#### {program}")
        lines.append("")

        if callers:
            caller_list = ", ".join(sorted(callers))
            lines.append(f"**Called by:** {caller_list}")
        else:
            lines.append("**Called by:** *(none - entry point)*")

        if calls:
            lines.append("**Calls:**")
            for callee, call_type, is_dynamic in calls:
                dynamic_marker = " [DYNAMIC]" if is_dynamic else ""
                lines.append(f"  - {callee} ({call_type}){dynamic_marker}")
        else:
            lines.append("**Calls:** *(none - leaf node)*")

        lines.append("")

    return "\n".join(lines)


def generate_markdown(docs: list[dict], graph: dict) -> str:
    """Generate the complete CALL_GRAPH.md content."""
    lines = []

    # Header
    lines.append("# CardDemo Call Graph")
    lines.append("")
    lines.append(f"*Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}*")
    lines.append("")
    lines.append(f"**Programs Analyzed:** {len(graph['documented_programs'])}")
    lines.append("")
    lines.append("---")
    lines.append("")

    # Table of Contents
    lines.append("## Table of Contents")
    lines.append("")
    lines.append("- [Visual Call Graph](#visual-call-graph)")
    lines.append("- [Entry Points](#entry-points)")
    lines.append("- [Leaf Nodes](#leaf-nodes)")
    lines.append("- [Call Chains](#call-chains)")
    lines.append("- [External Dependencies](#external-dependencies)")
    lines.append("- [Dynamic Calls](#dynamic-calls)")
    lines.append("- [Statistics](#statistics)")
    lines.append("- [Detailed Adjacency List](#detailed-adjacency-list)")
    lines.append("")
    lines.append("---")
    lines.append("")

    # Visual Call Graph (Mermaid)
    lines.append("## Visual Call Graph")
    lines.append("")
    lines.append("Legend:")
    lines.append("- **Green (rounded)**: Entry points (no callers)")
    lines.append("- **Blue (slanted)**: Leaf nodes (no callees)")
    lines.append("- **Pink (arrow shape)**: External dependencies (not documented)")
    lines.append("- **Solid arrows**: CALL/LINK (returns to caller)")
    lines.append("- **Dashed arrows**: XCTL (transfers control)")
    lines.append("")
    lines.append(generate_mermaid_diagram(graph))
    lines.append("")
    lines.append("---")
    lines.append("")

    # Text Summary
    lines.append(generate_text_summary(graph, docs))
    lines.append("---")
    lines.append("")

    # Detailed Adjacency List
    lines.append(generate_adjacency_list(graph))

    return "\n".join(lines)


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Generate call graph from documentation files"
    )
    parser.add_argument(
        "doc_dir",
        type=Path,
        help="Directory containing .doc.json files",
    )
    parser.add_argument(
        "--output",
        "-o",
        type=Path,
        default=None,
        help="Output file path (default: <doc_dir>/CALL_GRAPH.md)",
    )

    args = parser.parse_args()

    if not args.doc_dir.exists():
        print(f"Error: Directory not found: {args.doc_dir}")
        return 1

    # Load documentation
    docs = load_documentation(args.doc_dir)
    if not docs:
        print(f"Error: No .doc.json files found in {args.doc_dir}")
        return 1

    print(f"Loaded {len(docs)} documentation files")

    # Build call graph
    graph = build_call_graph(docs)
    print(f"Found {len(graph['documented_programs'])} documented programs")
    print(f"Found {len(graph['external_programs'])} external dependencies")
    print(f"Found {len(graph['dynamic_calls'])} dynamic calls")

    # Generate markdown
    content = generate_markdown(docs, graph)

    # Write output
    output_path = args.output or (args.doc_dir / "CALL_GRAPH.md")
    output_path.write_text(content)
    print(f"Generated: {output_path}")

    return 0


if __name__ == "__main__":
    exit(main())
