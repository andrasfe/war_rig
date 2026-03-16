#!/usr/bin/env python3
"""Regenerate markdown files from existing .doc.json templates.

This script reads all .doc.json files in the output directory and regenerates
their corresponding .md files. Useful when the markdown generation logic has
been updated (e.g., adding new sections like Inter-Paragraph Data Flow) and
you want to refresh the markdown without reprocessing the documentation.

When --input-dir is provided, the script also loads .ast files (Cobalt ASTs)
and includes per-paragraph AST fragments in the generated markdown.

Usage:
    python scripts/regenerate_markdown.py [--output-dir OUTPUT_DIR] [--input-dir INPUT_DIR] [--dry-run] [--verbose]

Examples:
    # Regenerate all markdown in default output directory
    python scripts/regenerate_markdown.py

    # Regenerate with AST fragments from input directory
    python scripts/regenerate_markdown.py --output-dir ./output --input-dir ./input

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


class MinimalConfig:
    """Minimal config for DocumentationWriter."""

    def __init__(self, output_dir: Path):
        self.output_directory = output_dir


def _load_ast_for_file(
    file_name: str, input_dir: Path | None,
) -> dict[str, str] | None:
    """Try to load paragraph ASTs from a .ast file.

    Searches input_dir for a matching .ast file (e.g., PROG.cbl.ast).

    Returns:
        Dict mapping uppercase paragraph name to AST string, or None.
    """
    if input_dir is None:
        return None

    try:
        from citadel import Citadel
    except ImportError:
        return None

    # The .ast file lives alongside the source in input_dir
    # file_name can be "subdir/PROG.cbl" — search by basename
    basename = Path(file_name).name
    ast_candidates = list(input_dir.rglob(f"{basename}.ast"))
    if not ast_candidates:
        # Try case-insensitive
        for candidate in input_dir.rglob("*.ast"):
            if candidate.name.lower() == f"{basename.lower()}.ast":
                ast_candidates = [candidate]
                break

    if not ast_candidates:
        return None

    try:
        citadel = Citadel()
        paragraph_asts, _ = citadel.load_cobol_ast(ast_candidates[0])
        return paragraph_asts
    except Exception:
        return None


def _inject_asts(md_content: str, paragraph_asts: dict[str, str]) -> str:
    """Insert AST fragments after paragraph headings in markdown.

    Matches lines like ``### PARAGRAPH-NAME`` and inserts a fenced code
    block with the AST immediately after.
    """
    import re

    heading_re = re.compile(
        r"^(### (?:~~)?([A-Za-z0-9_-]+?)(?:~~)?\s*(?:\(Dead Code\))?\s*)$"
    )
    lines = md_content.split("\n")
    result: list[str] = []
    for line in lines:
        result.append(line)
        m = heading_re.match(line)
        if m:
            para_name = m.group(2).upper()
            ast_text = paragraph_asts.get(para_name, "")
            if ast_text:
                result.append("")
                result.append("```")
                result.append(ast_text)
                result.append("```")
    return "\n".join(result)


def _find_source_for_doc(
    doc_file: Path, file_name: str, input_dir: Path | None,
) -> Path | None:
    """Locate the COBOL source file for a given .doc.json.

    Searches input_dir (if provided) for the source file by basename,
    falling back to looking next to the doc_file itself.
    """
    basename = Path(file_name).name
    cobol_extensions = {".cbl", ".cob", ".cobol"}
    if Path(basename).suffix.lower() not in cobol_extensions:
        return None

    if input_dir is not None:
        # Exact match first
        candidates = list(input_dir.rglob(basename))
        if candidates:
            return candidates[0]
        # Case-insensitive fallback
        for candidate in input_dir.rglob("*"):
            if candidate.name.lower() == basename.lower() and candidate.is_file():
                return candidate

    # Try next to the doc file (output dir may contain a copy)
    beside = doc_file.parent / basename
    if beside.exists():
        return beside

    return None


def regenerate_markdown(
    output_dir: Path,
    input_dir: Path | None = None,
    dry_run: bool = False,
    verbose: bool = False,
) -> tuple[int, int]:
    """Regenerate markdown files from .doc.json templates.

    Args:
        output_dir: Directory containing .doc.json files.
        input_dir: Directory containing source + .ast files (optional).
        dry_run: If True, don't write files, just report what would be done.
        verbose: If True, print details for each file.

    Returns:
        Tuple of (files_processed, files_with_call_semantics).
    """
    config = MinimalConfig(output_dir)
    writer = DocumentationWriter(config)
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
            md_content = writer._template_to_markdown(template)

            # Inject AST fragments if available
            file_name = template.header.file_name if template.header else doc_file.stem
            paragraph_asts = _load_ast_for_file(file_name, input_dir)
            if paragraph_asts:
                md_content = _inject_asts(md_content, paragraph_asts)
                ast_label = f", {len(paragraph_asts)} ASTs"
            else:
                ast_label = ""

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

                # Re-split paragraph snippets using AST-aware splitter
                split_label = ""
                source_path = _find_source_for_doc(doc_file, file_name, input_dir)
                if source_path is not None:
                    try:
                        from war_rig.io.paragraph_splitter import split_and_link

                        created = split_and_link(source_path, doc_file, md_file)
                        if created:
                            split_label = f", {len(created)} snippets"
                    except Exception as split_err:
                        if verbose:
                            print(f"  - Split failed: {split_err}")

                if verbose:
                    print(f"Regenerated: {md_file.name}")
                    print(f"  - Paragraphs: {para_count}{ast_label}")
                    print(f"  - Call semantics: {cs_count}")
                    if split_label:
                        print(f"  - Snippets: {split_label.lstrip(', ')}")
                else:
                    status = f" (+{cs_count} call semantics)" if cs_count > 0 else ""
                    print(f"Regenerated: {md_file.name}{status}{ast_label}{split_label}")

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
        "--input-dir",
        type=Path,
        default=None,
        help="Directory containing source + .ast files (for AST fragments in markdown)",
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
    if args.input_dir:
        print(f"Loading ASTs from {args.input_dir}")
    if args.dry_run:
        print("[DRY RUN MODE - no files will be written]\n")

    count, with_cs = regenerate_markdown(
        args.output_dir,
        input_dir=args.input_dir,
        dry_run=args.dry_run,
        verbose=args.verbose,
    )

    print(f"\n{'Would regenerate' if args.dry_run else 'Regenerated'} {count} markdown files")
    if with_cs > 0:
        print(f"  - {with_cs} files have Inter-Paragraph Data Flow sections")


if __name__ == "__main__":
    main()
