#!/usr/bin/env python3
"""Generate a consolidated SYSTEM_OVERVIEW.md from individual doc.json files.

Usage:
    python scripts/generate_summary.py output/
    python scripts/generate_summary.py output/ --output output/SYSTEM_OVERVIEW.md
"""

import argparse
import json
from datetime import datetime
from pathlib import Path


def load_documentation(doc_dir: Path) -> list[dict]:
    """Load all .doc.json files from directory."""
    docs = []
    for doc_file in sorted(doc_dir.glob("*.doc.json")):
        try:
            data = json.loads(doc_file.read_text())
            data["_source_file"] = doc_file.name
            docs.append(data)
        except (OSError, json.JSONDecodeError) as e:
            print(f"Warning: Could not load {doc_file}: {e}")
    return docs


def get_program_type(doc: dict) -> str:
    """Extract program type from documentation."""
    # Try different locations - check for None explicitly
    if doc.get("program_type"):
        return doc["program_type"]
    if "purpose" in doc and isinstance(doc["purpose"], dict):
        ptype = doc["purpose"].get("program_type")
        if ptype:
            return ptype
    return "Unknown"


def get_summary(doc: dict) -> str:
    """Extract summary from documentation."""
    if "purpose" in doc and isinstance(doc["purpose"], dict):
        summary = doc["purpose"].get("summary")
        if summary:
            return summary
    if doc.get("summary"):
        return doc["summary"]
    return "No summary available"


def get_program_id(doc: dict) -> str:
    """Extract program ID from documentation."""
    program_id = doc.get("program_id")
    if program_id:
        return program_id
    source_file = doc.get("_source_file", "Unknown")
    return source_file.replace(".doc.json", "") if source_file else "Unknown"


def categorize_programs(docs: list[dict]) -> dict[str, list[dict]]:
    """Group programs by type."""
    categories: dict[str, list[dict]] = {}
    for doc in docs:
        ptype = get_program_type(doc)
        if ptype not in categories:
            categories[ptype] = []
        categories[ptype].append(doc)
    return categories


def generate_markdown(docs: list[dict], ticket_file: Path | None = None) -> str:
    """Generate the SYSTEM_OVERVIEW.md content."""
    lines = []

    # Header
    lines.append("# CardDemo System Overview")
    lines.append("")
    lines.append(f"*Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}*")
    lines.append("")
    lines.append(f"**Total Programs Documented:** {len(docs)}")
    lines.append("")

    # Ticket status if available
    if ticket_file and ticket_file.exists():
        try:
            ticket_data = json.loads(ticket_file.read_text())
            tickets = ticket_data.get("tickets", [])
            completed = sum(1 for t in tickets if t.get("state") == "completed")
            blocked = sum(1 for t in tickets if t.get("state") == "blocked")
            total = len(tickets)
            lines.append(f"**Documentation Status:** {completed}/{total} tickets completed ({blocked} blocked)")
            lines.append("")
        except Exception:
            pass

    lines.append("---")
    lines.append("")

    # Table of Contents
    lines.append("## Table of Contents")
    lines.append("")
    categories = categorize_programs(docs)
    for category in sorted(categories.keys()):
        anchor = category.lower().replace(" ", "-").replace("/", "-")
        lines.append(f"- [{category}](#{anchor}) ({len(categories[category])} programs)")
    lines.append("")
    lines.append("---")
    lines.append("")

    # Programs by category
    for category in sorted(categories.keys()):
        lines.append(f"## {category}")
        lines.append("")

        for doc in sorted(categories[category], key=lambda d: get_program_id(d)):
            program_id = get_program_id(doc)
            summary = get_summary(doc)

            lines.append(f"### {program_id}")
            lines.append("")
            lines.append(summary)
            lines.append("")

            # Key details
            if "purpose" in doc and isinstance(doc["purpose"], dict):
                purpose = doc["purpose"]
                if purpose.get("business_context"):
                    lines.append(f"**Business Context:** {purpose['business_context']}")
                if purpose.get("technical_role"):
                    lines.append(f"**Technical Role:** {purpose['technical_role']}")
                lines.append("")

            # Inputs/Outputs summary
            if "data_flow" in doc and isinstance(doc["data_flow"], dict):
                data_flow = doc["data_flow"]
                inputs = data_flow.get("inputs", [])
                outputs = data_flow.get("outputs", [])
                if inputs or outputs:
                    if inputs:
                        input_names = [i.get("name", "?") if isinstance(i, dict) else str(i) for i in inputs[:5]]
                        lines.append(f"**Inputs:** {', '.join(input_names)}")
                    if outputs:
                        output_names = [o.get("name", "?") if isinstance(o, dict) else str(o) for o in outputs[:5]]
                        lines.append(f"**Outputs:** {', '.join(output_names)}")
                    lines.append("")

            # Called programs
            if "dependencies" in doc and isinstance(doc["dependencies"], dict):
                deps = doc["dependencies"]
                called = deps.get("called_programs", [])
                if called:
                    called_names = [c.get("program_id", "?") if isinstance(c, dict) else str(c) for c in called[:5]]
                    if len(called) > 5:
                        called_names.append(f"... and {len(called) - 5} more")
                    lines.append(f"**Calls:** {', '.join(called_names)}")
                    lines.append("")

            lines.append("---")
            lines.append("")

    # Cross-cutting concerns
    lines.append("## Cross-Cutting Observations")
    lines.append("")

    # Common patterns
    lines.append("### Program Types Distribution")
    lines.append("")
    lines.append("| Type | Count |")
    lines.append("|------|-------|")
    for category in sorted(categories.keys()):
        lines.append(f"| {category} | {len(categories[category])} |")
    lines.append("")

    return "\n".join(lines)


def main():
    parser = argparse.ArgumentParser(
        description="Generate consolidated system overview from documentation"
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
        help="Output file path (default: <doc_dir>/SYSTEM_OVERVIEW.md)",
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

    # Generate markdown
    ticket_file = args.doc_dir / ".war_rig_tickets.json"
    content = generate_markdown(docs, ticket_file)

    # Write output
    output_path = args.output or (args.doc_dir / "SYSTEM_OVERVIEW.md")
    output_path.write_text(content)
    print(f"Generated: {output_path}")

    return 0


if __name__ == "__main__":
    exit(main())
