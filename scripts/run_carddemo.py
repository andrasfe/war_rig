#!/usr/bin/env python3
"""Run War Rig on AWS CardDemo sample programs.

This script processes the CardDemo COBOL programs using the Program Manager
architecture with parallel Scribe and Challenger workers.

Usage:
    # Mock mode (no API key needed)
    python scripts/run_carddemo.py --mock

    # Real mode (requires API key in .env or environment)
    python scripts/run_carddemo.py

    # Custom worker counts
    python scripts/run_carddemo.py --num-scribes 5 --num-challengers 3 --max-cycles 10

    # Process specific directory
    python scripts/run_carddemo.py --input-dir /path/to/cobol/files

Note: Super-Scribe rescue mode for BLOCKED tickets is now automatic.
When tickets fail on all normal workers, the orchestrator automatically
escalates them to an Opus-powered "Super-Scribe" for rescue attempts.
Configure via RESCUE_MODEL and RESCUE_NUM_WORKERS in .env.
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

from war_rig.config import WarRigConfig
from war_rig.orchestration.ticket_engine import (
    BatchResult,
    TicketOrchestrator,
)

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger(__name__)

CARDDEMO_PATH = Path("aws-mainframe-modernization-carddemo/app")


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
    rework = status.get("rework_files", 0)

    # Worker counts
    scribe_pool = status.get("scribe_pool", {})
    challenger_pool = status.get("challenger_pool", {})

    active_scribes = scribe_pool.get("active_count", 0)
    active_challengers = challenger_pool.get("active_count", 0)
    total_workers = active_scribes + active_challengers

    # Build rework suffix only if there are rework items
    rework_str = f", {rework} rework" if rework > 0 else ""

    return (
        f"[Cycle {cycle}/{max_cycles}] {state.upper():<20} | "
        f"Files: {documented}/{total} documented, {validated} validated{rework_str} | "
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


async def run_orchestrator(
    input_dir: Path,
    use_mock: bool,
    num_scribes: int,
    num_challengers: int,
    max_cycles: int,
) -> int:
    """Run War Rig with the Program Manager architecture.

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
    print("WAR RIG - PROGRAM MANAGER MODE")
    print("=" * 60)
    print(f"Input directory:    {input_dir}")
    print(f"Scribe workers:     {num_scribes}")
    print(f"Challenger workers: {num_challengers}")
    print(f"Max cycles:         {max_cycles}")
    print(f"Mock mode:          {use_mock}")
    print("=" * 60 + "\n")

    # Create config with overridden values
    config = WarRigConfig()
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
        description="Run War Rig on mainframe source code using Program Manager architecture"
    )
    parser.add_argument(
        "--mock",
        action="store_true",
        help="Use mock agents (no API key required)",
    )
    parser.add_argument(
        "--input-dir",
        type=Path,
        default=None,
        help="Directory containing source files (default: CardDemo cbl directory)",
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

    # Determine input directory
    if args.input_dir:
        input_dir = args.input_dir
    else:
        # Default to CardDemo
        if not CARDDEMO_PATH.exists():
            logger.error(
                f"CardDemo not found at {CARDDEMO_PATH}. "
                "Either specify --input-dir or run: "
                "git clone https://github.com/aws-samples/aws-mainframe-modernization-carddemo.git"
            )
            sys.exit(1)
        input_dir = CARDDEMO_PATH / "cbl"

    if not input_dir.exists():
        logger.error(f"Input directory not found: {input_dir}")
        sys.exit(1)

    # Check for API key if not in mock mode
    if not args.mock:
        # Load .env if it exists
        from dotenv import load_dotenv
        load_dotenv()

        if not os.environ.get("OPENROUTER_API_KEY") and not os.environ.get("ANTHROPIC_API_KEY"):
            logger.error(
                "No API key found. Set OPENROUTER_API_KEY or ANTHROPIC_API_KEY in .env or environment, "
                "or use --mock for testing."
            )
            sys.exit(1)

    # Run the orchestrator (Super-Scribe rescue is now automatic)
    logger.info("Starting War Rig...")
    return await run_orchestrator(
        input_dir=input_dir,
        use_mock=args.mock,
        num_scribes=args.num_scribes,
        num_challengers=args.num_challengers,
        max_cycles=args.max_cycles,
    )


if __name__ == "__main__":
    sys.exit(asyncio.run(main()))
