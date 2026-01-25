#!/usr/bin/env python3
"""Generate a call graph from Citadel static analysis or documentation files.

Uses Citadel's dependency graph for accurate static analysis of source code.
Falls back to doc.json parsing if Citadel graph is not available.

Usage:
    # From source directory (uses Citadel)
    python scripts/generate_call_graph.py --source ../aws-mainframe-modernization-carddemo/samples

    # From existing dependency graph
    python scripts/generate_call_graph.py --graph output/dependency_graph.json

    # Fallback: from doc.json files only
    python scripts/generate_call_graph.py output/
"""

import argparse
import sys
from pathlib import Path


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Generate call graph from Citadel analysis or documentation files"
    )
    parser.add_argument(
        "doc_dir",
        type=Path,
        nargs="?",
        default=None,
        help="Directory containing .doc.json files (for fallback/metadata)",
    )
    parser.add_argument(
        "--source",
        "-s",
        type=Path,
        default=None,
        help="Source directory to analyze with Citadel (recommended)",
    )
    parser.add_argument(
        "--graph",
        "-g",
        type=Path,
        default=None,
        help="Path to existing Citadel dependency_graph.json",
    )
    parser.add_argument(
        "--output",
        "-o",
        type=Path,
        default=None,
        help="Output file path (default: CALL_GRAPH.md in doc_dir or current dir)",
    )

    args = parser.parse_args()

    # Validate arguments
    if not args.source and not args.graph and not args.doc_dir:
        parser.error("Must provide --source, --graph, or doc_dir")

    # Import here to avoid import errors if dependencies missing
    try:
        from war_rig.analysis.call_graph import CallGraphAnalyzer
    except ImportError:
        print("Error: war_rig package not installed. Run from project root with:")
        print("  uv run python scripts/generate_call_graph.py ...")
        return 1

    # Determine dependency graph path
    dependency_graph_path = None

    if args.source:
        # Generate Citadel graph from source
        print(f"Analyzing source directory: {args.source}")
        try:
            from citadel import Citadel
            from citadel.orchestrator import Orchestrator as CitadelOrchestrator
            import asyncio

            # Generate to temp location or output dir
            if args.doc_dir:
                graph_path = args.doc_dir / "dependency_graph.json"
            else:
                graph_path = Path("dependency_graph.json")

            orchestrator = CitadelOrchestrator()
            asyncio.run(orchestrator.analyze(
                source_root=args.source,
                output_path=graph_path,
                output_format="json",
            ))
            dependency_graph_path = graph_path
            print(f"Generated dependency graph: {graph_path}")

        except ImportError:
            print("Warning: Citadel not installed. Install with: uv pip install -e ./citadel")
            print("Falling back to doc.json parsing...")
        except Exception as e:
            print(f"Warning: Citadel analysis failed: {e}")
            print("Falling back to doc.json parsing...")

    elif args.graph:
        if not args.graph.exists():
            print(f"Error: Graph file not found: {args.graph}")
            return 1
        dependency_graph_path = args.graph
        print(f"Using existing dependency graph: {args.graph}")

    # Determine doc directory (for metadata/summaries)
    doc_dir = args.doc_dir or Path(".")
    if not doc_dir.exists():
        doc_dir.mkdir(parents=True, exist_ok=True)

    # Run analysis
    analyzer = CallGraphAnalyzer(doc_directory=doc_dir)
    analysis = analyzer.analyze(dependency_graph_path=dependency_graph_path)

    print(f"Found {len(analysis.documented_programs)} programs")
    print(f"Found {analysis.total_calls} call relationships")
    print(f"Found {len(analysis.external_dependencies)} external dependencies")
    if analysis.custom_missing:
        print(f"Missing custom programs: {', '.join(sorted(analysis.custom_missing))}")

    # Generate markdown
    content = analyzer.generate_markdown_report(analysis)

    # Write output
    if args.output:
        output_path = args.output
    elif args.doc_dir:
        output_path = args.doc_dir / "CALL_GRAPH.md"
    else:
        output_path = Path("CALL_GRAPH.md")

    output_path.write_text(content)
    print(f"Generated: {output_path}")

    return 0


if __name__ == "__main__":
    sys.exit(main())
