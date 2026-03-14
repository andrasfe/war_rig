#!/usr/bin/env python3
"""Re-document specific paragraphs in a COBOL program.

Marks named paragraphs as stubs in the .doc.json so the scribe resume
path re-processes only those paragraphs on the next run.  Optionally
creates a DOCUMENTATION ticket so ``--no-new-tickets`` runs pick it up.

Usage:
    # List paragraphs in a file
    python scripts/redoc_paragraphs.py COPAUA0C.cbl --output-dir ./output --list

    # Mark specific paragraphs for re-documentation
    python scripts/redoc_paragraphs.py COPAUA0C.cbl --output-dir ./output \
        --para MAIN-PARA --para 2000-MAIN-PROCESS

    # Mark paragraphs and create a ticket
    python scripts/redoc_paragraphs.py COPAUA0C.cbl --output-dir ./output \
        --para MAIN-PARA --ticket

    # Dry run (show what would change)
    python scripts/redoc_paragraphs.py COPAUA0C.cbl --output-dir ./output \
        --para MAIN-PARA --dry-run
"""

from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path

# Add project root to path
project_root = Path(__file__).parent.parent
sys.path.insert(0, str(project_root))

STUB_PURPOSE = "[Citadel] Paragraph identified by static analysis"

_HEADING_RE = re.compile(
    r"^###\s+(?:~~)?([A-Za-z0-9_-]+?)(?:~~)?\s*(?:\(Dead Code\))?\s*$"
)


def find_doc_json(output_dir: Path, file_name: str) -> Path | None:
    """Find the .doc.json for a given file name."""
    stem = file_name
    if stem.endswith(".doc.json"):
        stem = stem[: -len(".doc.json")]
    elif stem.endswith(".doc"):
        stem = stem[: -len(".doc")]

    # Direct match
    for candidate in output_dir.rglob(f"{stem}.doc.json"):
        return candidate

    # Case-insensitive fallback
    for candidate in output_dir.rglob("*.doc.json"):
        if candidate.stem.replace(".doc", "").upper() == stem.upper():
            return candidate

    return None


def find_md(output_dir: Path, file_name: str) -> Path | None:
    """Find the .md for a given file name."""
    stem = file_name
    for suffix in (".doc.json", ".doc", ".md"):
        if stem.endswith(suffix):
            stem = stem[: -len(suffix)]

    for candidate in output_dir.rglob(f"{stem}.md"):
        if ".doc." not in candidate.name:
            return candidate

    return None


def list_paragraphs(doc_json_path: Path) -> list[dict]:
    """List all paragraphs from a .doc.json with their stub status."""
    data = json.loads(doc_json_path.read_text(encoding="utf-8"))
    result = []
    for p in data.get("paragraphs", []):
        name = p.get("paragraph_name", "")
        purpose = p.get("purpose", "") or ""
        is_stub = purpose.startswith(STUB_PURPOSE)
        result.append({
            "name": name,
            "purpose": purpose[:80],
            "is_stub": is_stub,
        })
    return result


def list_paragraphs_from_md(md_path: Path) -> list[str]:
    """Extract paragraph names from ### headings in the .md file."""
    names = []
    for line in md_path.read_text(encoding="utf-8").splitlines():
        m = _HEADING_RE.match(line)
        if m:
            names.append(m.group(1))
    return names


def mark_as_stubs(
    doc_json_path: Path,
    para_names: list[str],
    dry_run: bool = False,
) -> list[str]:
    """Set the purpose of named paragraphs to the stub marker.

    Returns the list of paragraphs that were actually modified.
    """
    data = json.loads(doc_json_path.read_text(encoding="utf-8"))
    target_set = {n.upper() for n in para_names}
    modified = []

    for p in data.get("paragraphs", []):
        name = p.get("paragraph_name", "")
        if name.upper() in target_set:
            old_purpose = p.get("purpose", "")
            if old_purpose.startswith(STUB_PURPOSE):
                continue  # already a stub
            p["purpose"] = STUB_PURPOSE
            # Clear summary too so the md gets a clean re-render
            p.pop("summary", None)
            modified.append(name)

    if modified and not dry_run:
        doc_json_path.write_text(
            json.dumps(data, indent=2, ensure_ascii=False),
            encoding="utf-8",
        )

    return modified


def create_ticket(
    file_name: str,
    para_names: list[str],
    output_dir: Path,
    source_path: Path | None = None,
) -> str | None:
    """Create a DOCUMENTATION ticket for the file.

    Args:
        file_name: Relative file name as the scribe expects it
            (e.g. ``cbl/COPAUA0C.cbl``).
        para_names: Paragraph names to re-document.
        output_dir: Output directory (tickets file written here).
        source_path: Optional resolved path to the source file.
    """
    from war_rig.beads import BeadsPriority, TicketType, get_beads_client

    client = get_beads_client(
        tickets_file=output_dir / ".war_rig_tickets.json",
    )
    program_id = Path(file_name).stem.split(".")[0].upper()

    metadata: dict = {
        "stub_paragraphs": para_names,
        "stub_count": len(para_names),
        "redocumentation": True,
    }
    if source_path:
        metadata["file_path"] = str(source_path)

    ticket = client.create_pm_ticket(
        ticket_type=TicketType.DOCUMENTATION,
        file_name=file_name,
        program_id=program_id,
        cycle_number=1,
        priority=BeadsPriority.HIGH,
        metadata=metadata,
    )
    return ticket.ticket_id if ticket else None


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Re-document specific paragraphs in a COBOL program.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    parser.add_argument(
        "file",
        help="COBOL file name (e.g. COPAUA0C.cbl)",
    )
    parser.add_argument(
        "--output-dir", "-o",
        type=Path,
        default=Path("output"),
        help="Directory containing .doc.json files (default: ./output)",
    )
    parser.add_argument(
        "--para", "-p",
        action="append",
        default=[],
        dest="paragraphs",
        help="Paragraph name to re-document (repeatable)",
    )
    parser.add_argument(
        "--list", "-l",
        action="store_true",
        help="List all paragraphs and their status",
    )
    parser.add_argument(
        "--ticket", "-t",
        action="store_true",
        help="Create a DOCUMENTATION ticket after marking stubs",
    )
    parser.add_argument(
        "--dry-run", "-n",
        action="store_true",
        help="Show what would change without modifying files",
    )

    args = parser.parse_args()
    file_name = args.file

    doc_json = find_doc_json(args.output_dir, file_name)
    if not doc_json:
        print(f"Error: No .doc.json found for '{file_name}' in {args.output_dir}", file=sys.stderr)
        sys.exit(1)

    print(f"Found: {doc_json}")

    # --list: show all paragraphs
    if args.list:
        paragraphs = list_paragraphs(doc_json)
        if not paragraphs:
            print("  No paragraphs found.")
            return
        max_name = max(len(p["name"]) for p in paragraphs)
        for p in paragraphs:
            marker = " [STUB]" if p["is_stub"] else ""
            print(f"  {p['name']:<{max_name}}  {p['purpose']}{marker}")
        stubs = sum(1 for p in paragraphs if p["is_stub"])
        print(f"\n  {len(paragraphs)} paragraphs, {stubs} stubs")
        return

    if not args.paragraphs:
        print("Error: Specify paragraphs with --para NAME (or use --list to see them)", file=sys.stderr)
        sys.exit(1)

    # Validate paragraph names exist
    known = list_paragraphs(doc_json)
    known_names = {p["name"].upper() for p in known}
    bad = [n for n in args.paragraphs if n.upper() not in known_names]
    if bad:
        print(f"Error: Unknown paragraph(s): {', '.join(bad)}", file=sys.stderr)
        print(f"  Use --list to see available paragraphs", file=sys.stderr)
        sys.exit(1)

    # Mark as stubs
    prefix = "[DRY RUN] " if args.dry_run else ""
    modified = mark_as_stubs(doc_json, args.paragraphs, dry_run=args.dry_run)

    if modified:
        print(f"{prefix}Marked {len(modified)} paragraph(s) as stubs:")
        for name in modified:
            print(f"  - {name}")
    else:
        already = [n for n in args.paragraphs if n.upper() in {
            p["name"].upper() for p in known if p["is_stub"]
        }]
        if already:
            print(f"Already stubs: {', '.join(already)}")
        else:
            print("No paragraphs modified.")

    # Derive scribe-compatible file_name from .doc.json path
    # e.g. output/cbl/PROG.cbl.doc.json -> cbl/PROG.cbl
    rel_file_name = str(doc_json.relative_to(args.output_dir)).replace(".doc.json", "")

    # Create ticket
    if args.ticket and not args.dry_run:
        ticket_id = create_ticket(rel_file_name, args.paragraphs, args.output_dir)
        if ticket_id:
            print(f"Created ticket: {ticket_id}")
        else:
            print("Warning: Failed to create ticket", file=sys.stderr)
    elif args.ticket and args.dry_run:
        print(f"{prefix}Would create DOCUMENTATION ticket for {rel_file_name}")

    if modified and not args.dry_run and not args.ticket:
        print(f"\nRun with --ticket to create a ticket, or rerun:")
        print(f"  war-rig run --file {rel_file_name}")


if __name__ == "__main__":
    main()
