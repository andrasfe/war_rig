#!/usr/bin/env python3
"""Generate Agent Skills from War Rig documentation output.

This script converts War Rig documentation into Agent Skills format
for progressive discovery by AI agents. Skills are organized by category
(COBOL, JCL, IMS, etc.) with program summaries and links to full docs.

Usage:
    python scripts/generate_skills.py ./output/documentation
    python scripts/generate_skills.py ./output/documentation --output-dir ./my-skills
    python scripts/generate_skills.py ./output/documentation -v

The input directory should contain subdirectories like cbl/, jcl/, ims/
with .md documentation files.
"""

import argparse
import logging
import sys
from pathlib import Path


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Generate Agent Skills from War Rig documentation output"
    )
    parser.add_argument(
        "input_dir",
        type=Path,
        help="Path to documentation directory (e.g., ./output/documentation)"
    )
    parser.add_argument(
        "--output-dir", "-o",
        type=Path,
        default=None,
        help="Output directory for skills (default: skills-{input_name} at same level)"
    )
    parser.add_argument(
        "--system-name", "-n",
        type=str,
        default="System",
        help="System name for the top-level skill title (default: System)"
    )
    parser.add_argument(
        "--verbose", "-v",
        action="store_true",
        help="Enable verbose logging"
    )

    args = parser.parse_args()

    # Setup logging
    level = logging.DEBUG if args.verbose else logging.INFO
    logging.basicConfig(
        level=level,
        format="%(levelname)s: %(message)s"
    )

    # Resolve input directory - handle both ./output and ./output/documentation
    input_dir = args.input_dir.resolve()
    if not input_dir.exists():
        logging.error(f"Input directory does not exist: {input_dir}")
        return 1

    # If user passed the parent output dir, look for documentation subdir
    doc_subdir = input_dir / "documentation"
    if doc_subdir.exists() and doc_subdir.is_dir():
        logging.info(f"Found documentation subdirectory, using: {doc_subdir}")
        input_dir = doc_subdir

    try:
        from war_rig.skills import SkillsGenerator

        generator = SkillsGenerator(
            input_dir,
            args.output_dir,
            system_name=args.system_name,
        )
        result = generator.generate_with_result()

        print(f"Skills generated at: {result.output_dir}")
        print(f"  Categories: {len(result.categories_created)}")
        print(f"  Files processed: {result.files_processed}")

        if result.errors:
            print(f"  Errors: {len(result.errors)}")
            for error in result.errors:
                print(f"    - {error}")
            return 1

        return 0
    except Exception as e:
        logging.error(f"Failed to generate skills: {e}")
        if args.verbose:
            import traceback
            traceback.print_exc()
        return 1


if __name__ == "__main__":
    sys.exit(main())
