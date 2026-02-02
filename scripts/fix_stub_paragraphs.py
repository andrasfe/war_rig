#!/usr/bin/env python3
"""Find and fix files with stub paragraphs that need proper documentation.

This script scans .doc.json files for paragraphs that only have placeholder
text like "[Citadel] Paragraph identified by static analysis" and creates
re-documentation tickets for those files.

Usage:
    python scripts/fix_stub_paragraphs.py [--output-dir DIR] [--dry-run] [--verbose]

Examples:
    # Scan and report files with stubs
    python scripts/fix_stub_paragraphs.py --dry-run

    # Create tickets to re-document files with stubs
    python scripts/fix_stub_paragraphs.py

    # Specify custom output directory
    python scripts/fix_stub_paragraphs.py --output-dir ./my_output
"""

import argparse
import json
import sys
from pathlib import Path

# Add project root to path
project_root = Path(__file__).parent.parent
sys.path.insert(0, str(project_root))

from war_rig.models.templates import DocumentationTemplate

# Stub patterns that indicate a paragraph wasn't properly documented
STUB_PATTERNS = [
    "[Citadel] Paragraph identified by static analysis",
    "[Citadel]",
    "Paragraph identified by static analysis",
]


def is_stub_paragraph(paragraph) -> bool:
    """Check if a paragraph is a stub (not properly documented).

    Args:
        paragraph: A Paragraph object or dict.

    Returns:
        True if this is a stub paragraph.
    """
    # Get purpose - handle both object and dict
    if hasattr(paragraph, 'purpose'):
        purpose = paragraph.purpose or ""
    elif isinstance(paragraph, dict):
        purpose = paragraph.get('purpose', '') or ""
    else:
        return False

    # Check if purpose matches any stub pattern
    for pattern in STUB_PATTERNS:
        if pattern in purpose:
            return True

    # Also check for empty/minimal purpose
    if len(purpose.strip()) < 10:
        return True

    return False


def analyze_file(doc_path: Path) -> dict:
    """Analyze a .doc.json file for stub paragraphs.

    Args:
        doc_path: Path to the .doc.json file.

    Returns:
        Dict with analysis results.
    """
    try:
        with open(doc_path) as f:
            data = json.load(f)

        template = DocumentationTemplate.load_lenient(data)
        paragraphs = template.paragraphs or []

        total = len(paragraphs)
        stubs = []
        documented = []

        for para in paragraphs:
            name = para.paragraph_name if hasattr(para, 'paragraph_name') else para.get('paragraph_name', '')
            if is_stub_paragraph(para):
                stubs.append(name)
            else:
                documented.append(name)

        return {
            'path': doc_path,
            'total_paragraphs': total,
            'stub_count': len(stubs),
            'documented_count': len(documented),
            'stub_names': stubs,
            'file_name': doc_path.stem.replace('.doc', ''),
            'needs_fix': len(stubs) > 0,
        }
    except Exception as e:
        return {
            'path': doc_path,
            'error': str(e),
            'needs_fix': False,
        }


def find_files_with_stubs(output_dir: Path, verbose: bool = False) -> list[dict]:
    """Find all files with stub paragraphs.

    Args:
        output_dir: Directory to scan.
        verbose: Print details for each file.

    Returns:
        List of analysis results for files needing fixes.
    """
    results = []

    for doc_path in output_dir.rglob("*.doc.json"):
        if ".bak" in str(doc_path):
            continue

        analysis = analyze_file(doc_path)

        if 'error' in analysis:
            print(f"Error reading {doc_path}: {analysis['error']}", file=sys.stderr)
            continue

        if verbose:
            status = "NEEDS FIX" if analysis['needs_fix'] else "OK"
            print(f"{analysis['file_name']}: {analysis['documented_count']}/{analysis['total_paragraphs']} documented [{status}]")
            if analysis['stub_names'] and verbose:
                print(f"  Stubs: {', '.join(analysis['stub_names'][:5])}" +
                      (f"... (+{len(analysis['stub_names'])-5} more)" if len(analysis['stub_names']) > 5 else ""))

        if analysis['needs_fix']:
            results.append(analysis)

    return results


def create_redoc_tickets(files_to_fix: list[dict], input_dir: Path, dry_run: bool = False) -> int:
    """Create re-documentation tickets for files with stubs.

    Args:
        files_to_fix: List of file analysis results.
        input_dir: Directory containing source files.
        dry_run: If True, don't create tickets.

    Returns:
        Number of tickets created.
    """
    if dry_run:
        print("\n[DRY RUN] Would create tickets for:")
        for f in files_to_fix:
            print(f"  - {f['file_name']} ({f['stub_count']} stubs)")
        return 0

    try:
        from war_rig.beads import BeadsClient, TicketType, BeadsPriority
    except ImportError:
        print("Error: Could not import beads. Make sure war_rig is installed.", file=sys.stderr)
        return 0

    client = BeadsClient()
    created = 0

    for f in files_to_fix:
        file_name = f['file_name']
        doc_path = f['path']

        # Try to find the source file
        source_path = None
        for ext in ['', '.cbl', '.cob', '.CBL', '.COB']:
            candidate = input_dir / (file_name.replace('.cbl', '').replace('.CBL', '') + ext)
            if candidate.exists():
                source_path = candidate
                break

        # Also search recursively
        if not source_path:
            for candidate in input_dir.rglob(f"*{file_name}*"):
                if candidate.is_file() and candidate.suffix.lower() in ['.cbl', '.cob', '.cobol']:
                    source_path = candidate
                    break

        metadata = {
            'stub_paragraphs': f['stub_names'],
            'stub_count': f['stub_count'],
            'redocumentation': True,
        }

        if source_path:
            metadata['file_path'] = str(source_path)

        try:
            # Delete the existing doc file so it gets regenerated fresh
            doc_path.unlink()
            md_path = doc_path.with_suffix('').with_suffix('.md')
            if md_path.exists():
                md_path.unlink()

            ticket = client.create_pm_ticket(
                ticket_type=TicketType.DOCUMENTATION,
                file_name=file_name,
                program_id=file_name.split('.')[0].upper(),
                cycle_number=1,
                priority=BeadsPriority.HIGH,
                metadata=metadata,
            )
            print(f"Created ticket {ticket.ticket_id} for {file_name} ({f['stub_count']} stubs)")
            created += 1
        except Exception as e:
            print(f"Error creating ticket for {file_name}: {e}", file=sys.stderr)

    return created


def main():
    parser = argparse.ArgumentParser(
        description="Find and fix files with stub paragraphs.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=Path("output"),
        help="Directory containing .doc.json files (default: ./output)",
    )
    parser.add_argument(
        "--input-dir",
        type=Path,
        default=None,
        help="Directory containing source files (for ticket metadata)",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Don't create tickets, just report what would be done",
    )
    parser.add_argument(
        "--verbose", "-v",
        action="store_true",
        help="Print details for each file",
    )
    parser.add_argument(
        "--report-only",
        action="store_true",
        help="Only report files with stubs, don't create tickets",
    )

    args = parser.parse_args()

    if not args.output_dir.exists():
        print(f"Error: Output directory not found: {args.output_dir}", file=sys.stderr)
        sys.exit(1)

    print(f"Scanning {args.output_dir} for files with stub paragraphs...\n")

    files_with_stubs = find_files_with_stubs(args.output_dir, verbose=args.verbose)

    if not files_with_stubs:
        print("\nNo files with stub paragraphs found.")
        return

    # Summary
    total_stubs = sum(f['stub_count'] for f in files_with_stubs)
    print(f"\nFound {len(files_with_stubs)} files with {total_stubs} total stub paragraphs:")
    for f in files_with_stubs:
        pct = (f['stub_count'] / f['total_paragraphs'] * 100) if f['total_paragraphs'] > 0 else 0
        print(f"  - {f['file_name']}: {f['stub_count']}/{f['total_paragraphs']} stubs ({pct:.0f}%)")

    if args.report_only:
        return

    # Create tickets
    input_dir = args.input_dir or args.output_dir.parent
    print(f"\nCreating re-documentation tickets (input_dir: {input_dir})...")

    created = create_redoc_tickets(files_with_stubs, input_dir, dry_run=args.dry_run)

    if not args.dry_run:
        print(f"\nCreated {created} tickets. Run the scribe pool to process them.")


if __name__ == "__main__":
    main()
