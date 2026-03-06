"""Cobalt CLI — parse COBOL files and output AST JSON."""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

from cobalt.generator import parse_cobol_ast


def main() -> None:
    parser = argparse.ArgumentParser(
        prog="cobalt",
        description="Parse COBOL source files and output AST JSON.",
    )
    parser.add_argument(
        "source",
        help="Path to the COBOL source file (.cbl)",
    )
    parser.add_argument(
        "--copybook-dir",
        action="append",
        default=[],
        dest="copybook_dirs",
        help="Directory to search for copybooks (repeatable)",
    )
    parser.add_argument(
        "--text",
        action="store_true",
        help="Output formatted AST text instead of JSON",
    )
    parser.add_argument(
        "-o", "--output",
        help="Write output to file instead of stdout",
    )

    args = parser.parse_args()

    source_path = Path(args.source)
    if not source_path.exists():
        print(f"Error: file not found: {source_path}", file=sys.stderr)
        sys.exit(1)

    trees, full_text, raw_json = parse_cobol_ast(
        source_path,
        copybook_dirs=args.copybook_dirs or None,
    )

    output = full_text if args.text else raw_json

    if args.output:
        Path(args.output).write_text(output, encoding="utf-8")
    else:
        print(output)


if __name__ == "__main__":
    main()
