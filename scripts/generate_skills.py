#!/usr/bin/env python3
"""Generate Agent Skills from War Rig documentation output.

This script converts War Rig documentation into Agent Skills format
for progressive discovery by AI agents.

Usage:
    python scripts/generate_skills.py ./output
    python scripts/generate_skills.py ./output --output-dir ./my-skills
    python scripts/generate_skills.py ./output -v
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
        help="Path to War Rig output directory (e.g., ./output)"
    )
    parser.add_argument(
        "--output-dir", "-o",
        type=Path,
        default=None,
        help="Output directory for skills (default: skills-{input_name} at same level)"
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

    try:
        from war_rig.skills import SkillsGenerator

        generator = SkillsGenerator(args.input_dir, args.output_dir)
        output_path = generator.generate()
        print(f"Skills generated at: {output_path}")
        return 0
    except Exception as e:
        logging.error(f"Failed to generate skills: {e}")
        return 1


if __name__ == "__main__":
    sys.exit(main())
