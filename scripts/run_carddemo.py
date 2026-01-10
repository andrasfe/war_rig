#!/usr/bin/env python3
"""Run War Rig on AWS CardDemo sample programs.

This script processes the CardDemo COBOL programs and generates documentation.
It can run in mock mode for testing or with real LLM calls for production.

Usage:
    # Mock mode (no API key needed)
    python scripts/run_carddemo.py --mock

    # Real mode (requires ANTHROPIC_API_KEY)
    export ANTHROPIC_API_KEY=your-key
    python scripts/run_carddemo.py

    # Single program
    python scripts/run_carddemo.py --program CBACT04C

    # All Phase 1 programs
    python scripts/run_carddemo.py --phase 1
"""

import argparse
import asyncio
import logging
import os
import sys
from pathlib import Path

# Add project root to path
sys.path.insert(0, str(Path(__file__).parent.parent))

from war_rig.config import SystemConfig, WarRigConfig
from war_rig.io.reader import SourceReader
from war_rig.io.writer import DocumentationWriter
from war_rig.orchestration.graph import create_war_rig_graph

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger(__name__)

# CardDemo program phases from spec
PHASES = {
    1: ["CBACT04C", "CBSTM03B"],  # Simple Batch
    2: ["CBACT01C", "CBACT02C"],  # Batch with DB2
    3: ["COSGN00C", "COCRDLIC"],  # CICS Online
    4: [],  # JCL (not implemented yet)
}

CARDDEMO_PATH = Path("aws-mainframe-modernization-carddemo/app")


async def process_program(
    program_id: str,
    graph,
    reader: SourceReader,
    writer: DocumentationWriter,
    use_mock: bool = False,
) -> bool:
    """Process a single program through War Rig.

    Returns:
        True if successful, False otherwise.
    """
    # Find the source file
    cbl_path = CARDDEMO_PATH / "cbl" / f"{program_id}.cbl"
    if not cbl_path.exists():
        logger.error(f"Source file not found: {cbl_path}")
        return False

    logger.info(f"Processing {program_id}...")

    try:
        # Read source
        source_code = reader.read_file(cbl_path)

        # Resolve copybooks
        copybook_contents = {}
        cpy_dir = CARDDEMO_PATH / "cpy"
        if cpy_dir.exists():
            # Simple copybook resolution - could be enhanced
            for cpy_file in cpy_dir.glob("*.cpy"):
                copybook_contents[cpy_file.stem] = reader.read_file(cpy_file)

        # Run War Rig
        result = await graph.ainvoke(
            source_code=source_code,
            file_name=f"{program_id}.cbl",
            copybook_contents=copybook_contents,
            use_mock=use_mock,
        )

        # Write outputs
        if result.get("final_template"):
            output_paths = writer.write_result(result)
            logger.info(f"✓ {program_id}: {result.get('decision', 'UNKNOWN')}")
            logger.info(f"  Output: {output_paths.get('final_json', 'N/A')}")
            return True
        else:
            logger.error(f"✗ {program_id}: No template generated")
            return False

    except Exception as e:
        logger.exception(f"✗ {program_id}: {e}")
        return False


async def main():
    parser = argparse.ArgumentParser(
        description="Run War Rig on AWS CardDemo programs"
    )
    parser.add_argument(
        "--mock",
        action="store_true",
        help="Use mock agents (no API key required)",
    )
    parser.add_argument(
        "--program",
        type=str,
        help="Process a single program by ID (e.g., CBACT04C)",
    )
    parser.add_argument(
        "--phase",
        type=int,
        choices=[1, 2, 3, 4],
        help="Process all programs in a phase",
    )
    parser.add_argument(
        "--all",
        action="store_true",
        help="Process all available programs",
    )
    args = parser.parse_args()

    # Check for API key if not in mock mode
    if not args.mock and not os.environ.get("ANTHROPIC_API_KEY"):
        logger.error("ANTHROPIC_API_KEY not set. Use --mock for testing.")
        sys.exit(1)

    # Check CardDemo exists
    if not CARDDEMO_PATH.exists():
        logger.error(
            f"CardDemo not found at {CARDDEMO_PATH}. "
            "Run: git clone https://github.com/aws-samples/aws-mainframe-modernization-carddemo.git"
        )
        sys.exit(1)

    # Determine programs to process
    programs = []
    if args.program:
        programs = [args.program]
    elif args.phase:
        programs = PHASES.get(args.phase, [])
    elif args.all:
        programs = [p for phase_progs in PHASES.values() for p in phase_progs]
    else:
        # Default: Phase 1
        programs = PHASES[1]
        logger.info("No programs specified, defaulting to Phase 1")

    if not programs:
        logger.error("No programs to process")
        sys.exit(1)

    logger.info(f"Processing {len(programs)} program(s): {', '.join(programs)}")
    logger.info(f"Mode: {'MOCK' if args.mock else 'REAL LLM'}")

    # Initialize components
    config = WarRigConfig()
    system_config = SystemConfig(
        input_directory=CARDDEMO_PATH,
        output_directory=Path("output"),
    )
    graph = create_war_rig_graph(config)
    reader = SourceReader(system_config)
    writer = DocumentationWriter(system_config)

    # Process programs
    results = []
    for program_id in programs:
        success = await process_program(
            program_id, graph, reader, writer, use_mock=args.mock
        )
        results.append((program_id, success))

    # Summary
    print("\n" + "=" * 50)
    print("SUMMARY")
    print("=" * 50)
    success_count = sum(1 for _, s in results if s)
    for program_id, success in results:
        status = "✓" if success else "✗"
        print(f"  {status} {program_id}")
    print(f"\nTotal: {success_count}/{len(results)} succeeded")

    if success_count == len(results):
        print("\n✓ All programs documented successfully!")
        return 0
    else:
        print("\n⚠ Some programs failed")
        return 1


if __name__ == "__main__":
    sys.exit(asyncio.run(main()))
