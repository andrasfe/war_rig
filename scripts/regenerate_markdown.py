#!/usr/bin/env python3
"""Regenerate markdown files from existing .doc.json templates.

This script reads all .doc.json files in the output directory and regenerates
their corresponding .md files. Useful when the markdown generation logic has
been updated (e.g., adding new sections like Inter-Paragraph Data Flow) and
you want to refresh the markdown without reprocessing the documentation.

Usage:
    python scripts/regenerate_markdown.py [--output-dir OUTPUT_DIR] [--dry-run] [--verbose]

Examples:
    # Regenerate all markdown in default output directory
    python scripts/regenerate_markdown.py

    # Regenerate from a specific directory
    python scripts/regenerate_markdown.py --output-dir ./my_output

    # Preview what would be regenerated without writing
    python scripts/regenerate_markdown.py --dry-run
"""

import argparse
import json
import sys
from pathlib import Path

# Add project root to path
project_root = Path(__file__).parent.parent
sys.path.insert(0, str(project_root))

from war_rig.io.writer import DocumentationWriter
from war_rig.models.templates import DocumentationTemplate


def regenerate_markdown(
    output_dir: Path,
    dry_run: bool = False,
    verbose: bool = False,
) -> tuple[int, int]:
    """Regenerate markdown files from .doc.json templates.

    Args:
        output_dir: Directory containing .doc.json files.
        dry_run: If True, don't write files, just report what would be done.
        verbose: If True, print details for each file.

    Returns:
        Tuple of (files_processed, files_with_call_semantics).
    """
    writer = DocumentationWriter()
    count = 0
    with_call_semantics = 0

    for doc_file in output_dir.rglob("*.doc.json"):
        # Skip backup files
        if ".bak" in str(doc_file):
            continue

        try:
            with open(doc_file) as f:
                data = json.load(f)

            template = DocumentationTemplate.load_lenient(data)
            md_content = writer.template_to_markdown(template)
            md_file = doc_file.with_suffix("").with_suffix(".md")

            para_count = len(template.paragraphs) if template.paragraphs else 0
            cs_count = len(template.call_semantics) if template.call_semantics else 0

            if cs_count > 0:
                with_call_semantics += 1

            if dry_run:
                print(f"[DRY RUN] Would regenerate: {md_file.name}")
                if verbose:
                    print(f"  - Paragraphs: {para_count}")
                    print(f"  - Call semantics: {cs_count}")
            else:
                with open(md_file, "w") as f:
                    f.write(md_content)

                if verbose:
                    print(f"Regenerated: {md_file.name}")
                    print(f"  - Paragraphs: {para_count}")
                    print(f"  - Call semantics: {cs_count}")
                else:
                    status = f" (+{cs_count} call semantics)" if cs_count > 0 else ""
                    print(f"Regenerated: {md_file.name}{status}")

            count += 1

        except Exception as e:
            print(f"Error processing {doc_file}: {e}", file=sys.stderr)

    return count, with_call_semantics


def main():
    parser = argparse.ArgumentParser(
        description="Regenerate markdown files from .doc.json templates.",
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
        "--dry-run",
        action="store_true",
        help="Don't write files, just show what would be done",
    )
    parser.add_argument(
        "--verbose",
        "-v",
        action="store_true",
        help="Print detailed information for each file",
    )

    args = parser.parse_args()

    if not args.output_dir.exists():
        print(f"Error: Output directory not found: {args.output_dir}", file=sys.stderr)
        sys.exit(1)

    print(f"Scanning {args.output_dir} for .doc.json files...")
    if args.dry_run:
        print("[DRY RUN MODE - no files will be written]\n")

    count, with_cs = regenerate_markdown(
        args.output_dir,
        dry_run=args.dry_run,
        verbose=args.verbose,
    )

    print(f"\n{'Would regenerate' if args.dry_run else 'Regenerated'} {count} markdown files")
    if with_cs > 0:
        print(f"  - {with_cs} files have Inter-Paragraph Data Flow sections")


if __name__ == "__main__":
    main()
