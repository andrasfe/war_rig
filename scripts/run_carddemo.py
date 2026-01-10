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

    # Program Manager mode (parallel ticket-based processing)
    python scripts/run_carddemo.py --pm-mode --mock

    # PM mode with custom worker counts
    python scripts/run_carddemo.py --pm-mode --num-scribes 5 --num-challengers 3 --max-cycles 10
"""

import argparse
import asyncio
import logging
import os
import signal
import sys
from pathlib import Path
from typing import Any

# Add project root to path
sys.path.insert(0, str(Path(__file__).parent.parent))

from war_rig.config import SystemConfig, WarRigConfig
from war_rig.io.reader import SourceReader
from war_rig.io.writer import DocumentationWriter
from war_rig.orchestration.graph import create_war_rig_graph
from war_rig.orchestration.ticket_engine import (
    BatchResult,
    OrchestrationStatus,
    TicketOrchestrator,
)

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


# =============================================================================
# Program Manager Mode Functions
# =============================================================================


def format_status_line(status: dict[str, Any]) -> str:
    """Format a single-line status update for the orchestrator.

    Args:
        status: Status dictionary from TicketOrchestrator.get_status().

    Returns:
        Formatted status string.
    """
    cycle = status.get("cycle", 0)
    max_cycles = status.get("max_cycles", 5)
    state = status.get("status", "unknown")
    total = status.get("total_files", 0)
    documented = status.get("documented_files", 0)
    validated = status.get("validated_files", 0)

    # Worker counts
    scribe_pool = status.get("scribe_pool", {})
    challenger_pool = status.get("challenger_pool", {})

    active_scribes = scribe_pool.get("active_count", 0)
    active_challengers = challenger_pool.get("active_count", 0)
    total_workers = active_scribes + active_challengers

    return (
        f"[Cycle {cycle}/{max_cycles}] {state.upper():<20} | "
        f"Files: {documented}/{total} documented, {validated} validated | "
        f"Workers: {total_workers} active ({active_scribes}S/{active_challengers}C)"
    )


def print_cycle_summary(status: dict[str, Any], cycle: int) -> None:
    """Print a summary after each orchestration cycle.

    Args:
        status: Status dictionary from TicketOrchestrator.get_status().
        cycle: Completed cycle number.
    """
    print(f"\n{'─' * 60}")
    print(f"Cycle {cycle} Complete")
    print(f"{'─' * 60}")

    total = status.get("total_files", 0)
    documented = status.get("documented_files", 0)
    validated = status.get("validated_files", 0)

    print(f"  Files documented: {documented}/{total}")
    print(f"  Files validated:  {validated}/{total}")

    # Show worker statistics if available
    scribe_pool = status.get("scribe_pool", {})
    if scribe_pool:
        print(f"  Scribe processed: {scribe_pool.get('total_processed', 0)}")
        print(f"  Scribe failed:    {scribe_pool.get('total_failed', 0)}")

    challenger_pool = status.get("challenger_pool", {})
    if challenger_pool:
        print(f"  Challenger processed: {challenger_pool.get('total_processed', 0)}")
        print(f"  Challenger failed:    {challenger_pool.get('total_failed', 0)}")


def print_batch_result(result: BatchResult) -> None:
    """Print the final batch result summary.

    Args:
        result: BatchResult from TicketOrchestrator.run_batch().
    """
    print("\n" + "=" * 60)
    print("BATCH RESULT SUMMARY")
    print("=" * 60)

    print(f"\nFinal Decision: {result.final_decision}")
    print(f"Total Cycles:   {result.total_cycles}")
    print(f"Duration:       {result.duration_seconds:.1f}s")

    if result.completed_files:
        print(f"\nCompleted Files ({len(result.completed_files)}):")
        for f in result.completed_files:
            print(f"  [OK] {f}")

    if result.failed_files:
        print(f"\nFailed Files ({len(result.failed_files)}):")
        for f in result.failed_files:
            print(f"  [FAIL] {f}")

    if result.quality_notes:
        print("\nQuality Notes:")
        for note in result.quality_notes:
            print(f"  - {note}")

    if result.assumptions_made:
        print(f"\nAssumptions Made ({len(result.assumptions_made)}):")
        for assumption in result.assumptions_made[:5]:  # Show first 5
            print(f"  - {assumption.get('description', 'N/A')}")
        if len(result.assumptions_made) > 5:
            print(f"  ... and {len(result.assumptions_made) - 5} more")

    print("\n" + "=" * 60)
    if result.success:
        print("Batch completed SUCCESSFULLY")
    else:
        print("Batch completed with ISSUES")
    print("=" * 60)


async def run_pm_mode(
    input_dir: Path,
    use_mock: bool,
    num_scribes: int,
    num_challengers: int,
    max_cycles: int,
) -> int:
    """Run War Rig in Program Manager mode with parallel processing.

    Args:
        input_dir: Directory containing source files to process.
        use_mock: Whether to use mock LLM responses.
        num_scribes: Number of Scribe workers.
        num_challengers: Number of Challenger workers.
        max_cycles: Maximum orchestration cycles.

    Returns:
        Exit code (0 for success, 1 for failure).
    """
    print("\n" + "=" * 60)
    print("PROGRAM MANAGER MODE")
    print("=" * 60)
    print(f"Input directory:  {input_dir}")
    print(f"Scribe workers:   {num_scribes}")
    print(f"Challenger workers: {num_challengers}")
    print(f"Max cycles:       {max_cycles}")
    print(f"Mock mode:        {use_mock}")
    print("=" * 60 + "\n")

    # Create config with overridden values
    config = WarRigConfig()
    # Override worker counts via object mutation (config is Pydantic model)
    config_dict = config.model_dump()
    config_dict["num_scribes"] = num_scribes
    config_dict["num_challengers"] = num_challengers
    config_dict["pm_max_cycles"] = max_cycles
    config = WarRigConfig(**config_dict)

    # Create orchestrator
    orchestrator = TicketOrchestrator(
        config=config,
        use_mock=use_mock,
    )

    # Set up graceful interrupt handling
    stop_event = asyncio.Event()

    def handle_interrupt() -> None:
        """Handle Ctrl+C gracefully."""
        print("\n\nInterrupt received, stopping gracefully...")
        stop_event.set()
        # Create a task to stop the orchestrator
        asyncio.create_task(orchestrator.stop())

    # Register signal handler
    loop = asyncio.get_event_loop()
    for sig in (signal.SIGINT, signal.SIGTERM):
        try:
            loop.add_signal_handler(sig, handle_interrupt)
        except NotImplementedError:
            # Windows doesn't support add_signal_handler
            pass

    # Start progress display task
    async def display_progress() -> None:
        """Periodically display progress updates."""
        last_cycle = 0
        while not stop_event.is_set():
            status = orchestrator.get_status()

            # Print status line
            print(f"\r{format_status_line(status)}", end="", flush=True)

            # Print cycle summary when cycle changes
            current_cycle = status.get("cycle", 0)
            if current_cycle > last_cycle and last_cycle > 0:
                print()  # New line after status
                print_cycle_summary(status, last_cycle)
            last_cycle = current_cycle

            # Check if completed
            if status.get("status") in ("completed", "stopped", "failed"):
                break

            await asyncio.sleep(2.0)

    # Run orchestrator with progress display
    progress_task = asyncio.create_task(display_progress())

    try:
        result = await orchestrator.run_batch(input_dir)
    except KeyboardInterrupt:
        print("\n\nInterrupted by user")
        await orchestrator.stop()
        result = BatchResult(final_decision="INTERRUPTED")
    finally:
        stop_event.set()
        progress_task.cancel()
        try:
            await progress_task
        except asyncio.CancelledError:
            pass

    # Print final result
    print()  # Clear the status line
    print_batch_result(result)

    return 0 if result.success else 1


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

    # Program Manager mode arguments
    parser.add_argument(
        "--pm-mode",
        action="store_true",
        help="Use Program Manager mode with TicketOrchestrator for parallel processing",
    )
    parser.add_argument(
        "--num-scribes",
        type=int,
        default=3,
        help="Number of parallel Scribe workers (default: 3)",
    )
    parser.add_argument(
        "--num-challengers",
        type=int,
        default=2,
        help="Number of parallel Challenger workers (default: 2)",
    )
    parser.add_argument(
        "--max-cycles",
        type=int,
        default=5,
        help="Maximum orchestration cycles before forced completion (default: 5)",
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

    # Program Manager mode: use TicketOrchestrator for parallel processing
    if args.pm_mode:
        logger.info("Starting Program Manager mode...")
        return await run_pm_mode(
            input_dir=CARDDEMO_PATH / "cbl",
            use_mock=args.mock,
            num_scribes=args.num_scribes,
            num_challengers=args.num_challengers,
            max_cycles=args.max_cycles,
        )

    # Sequential mode (original behavior)
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
        status = "OK" if success else "FAIL"
        print(f"  [{status}] {program_id}")
    print(f"\nTotal: {success_count}/{len(results)} succeeded")

    if success_count == len(results):
        print("\nAll programs documented successfully!")
        return 0
    else:
        print("\nSome programs failed")
        return 1


if __name__ == "__main__":
    sys.exit(asyncio.run(main()))
