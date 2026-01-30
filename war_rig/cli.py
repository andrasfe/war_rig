"""Command-line interface for War Rig.

This module provides the CLI entry point for the War Rig documentation
system. It uses Typer for a modern, type-annotated CLI experience.

Commands:
    analyze: Document a single source file
    batch: Process a directory of source files
    status: Show processing status
    init: Initialize output directories
    skills: Generate Agent Skills from documentation output

Example:
    $ war-rig analyze path/to/PROGRAM.cbl
    $ war-rig batch path/to/source/directory
    $ war-rig status
    $ war-rig skills ./output
"""

import asyncio
import logging
import sys
from pathlib import Path
from typing import Annotated, Optional

import typer
from rich.console import Console
from rich.logging import RichHandler
from rich.panel import Panel
from rich.progress import Progress, SpinnerColumn, TextColumn
from rich.table import Table

from war_rig.beads import TicketState, TicketType, get_beads_client
from war_rig.config import WarRigConfig, load_config
from war_rig.io.reader import SourceReader
from war_rig.io.writer import DocumentationWriter
from war_rig.models.templates import FileType
from war_rig.orchestration.graph import create_war_rig_graph
from war_rig.orchestration.ticket_engine import TicketOrchestrator
from war_rig.processors.datacard import process_datacards
from war_rig.utils import setup_error_file_handler

# Set up Typer app
app = typer.Typer(
    name="war-rig",
    help="War Rig: Multi-agent mainframe documentation system",
    add_completion=False,
)

# Set up Rich console
console = Console()


def setup_logging(verbose: bool = False) -> None:
    """Configure logging with Rich handler.

    Args:
        verbose: Whether to enable debug logging.
    """
    level = logging.DEBUG if verbose else logging.INFO

    logging.basicConfig(
        level=level,
        format="%(message)s",
        datefmt="[%X]",
        handlers=[RichHandler(console=console, rich_tracebacks=True)],
    )

    # Set up error file logging (captures ALL ERROR level logs to ERR_FILE)
    setup_error_file_handler()


def load_config_with_fallback(config_path: Optional[Path]) -> WarRigConfig:
    """Load configuration with fallback to defaults.

    Args:
        config_path: Optional path to config file.

    Returns:
        Loaded configuration.
    """
    try:
        if config_path:
            return WarRigConfig.from_yaml(config_path)
        return load_config()
    except Exception as e:
        console.print(f"[yellow]Warning: Could not load config: {e}[/yellow]")
        console.print("[yellow]Using default configuration[/yellow]")
        return WarRigConfig()


@app.command()
def analyze(
    file_path: Annotated[
        Path,
        typer.Argument(
            help="Path to the source file to analyze",
            exists=True,
            file_okay=True,
            dir_okay=False,
            readable=True,
        ),
    ],
    config: Annotated[
        Optional[Path],
        typer.Option(
            "--config",
            "-c",
            help="Path to configuration file",
        ),
    ] = None,
    output: Annotated[
        Optional[Path],
        typer.Option(
            "--output",
            "-o",
            help="Output directory (overrides config)",
        ),
    ] = None,
    mock: Annotated[
        bool,
        typer.Option(
            "--mock",
            "-m",
            help="Use mock agents (for testing)",
        ),
    ] = False,
    verbose: Annotated[
        bool,
        typer.Option(
            "--verbose",
            "-v",
            help="Enable verbose logging",
        ),
    ] = False,
) -> None:
    """Analyze and document a single source file.

    This command reads a mainframe source file (COBOL, JCL, etc.) and
    generates comprehensive documentation using the War Rig agent system.

    Example:
        $ war-rig analyze path/to/PROGRAM.cbl
        $ war-rig analyze path/to/JOB.jcl --output ./docs
    """
    setup_logging(verbose)

    # Load configuration
    cfg = load_config_with_fallback(config)
    if output:
        cfg.output_directory = output

    console.print(Panel.fit(
        f"[bold blue]War Rig[/bold blue] - Analyzing: {file_path.name}",
        border_style="blue",
    ))

    # Read source file
    reader = SourceReader(cfg.system)
    try:
        source_code = reader.read_file(file_path)
    except Exception as e:
        console.print(f"[red]Error reading file: {e}[/red]")
        raise typer.Exit(1)

    # Create graph and run
    graph = create_war_rig_graph(cfg)

    with Progress(
        SpinnerColumn(),
        TextColumn("[progress.description]{task.description}"),
        console=console,
    ) as progress:
        task = progress.add_task("Analyzing...", total=None)

        try:
            result = asyncio.run(
                graph.ainvoke(
                    source_code=source_code,
                    file_name=file_path.name,
                    source_file_path=str(file_path.resolve()),
                    use_mock=mock,
                )
            )
        except Exception as e:
            progress.stop()
            console.print(f"[red]Analysis failed: {e}[/red]")
            raise typer.Exit(1)

        progress.update(task, description="Writing output...")

    # Write results
    writer = DocumentationWriter(cfg.system)
    outputs = writer.write_result(result)

    # Update ticket state to COMPLETED if we have a ticket for this file
    tickets_file = cfg.output_directory / ".war_rig_tickets.json"
    if tickets_file.exists():
        beads_client = get_beads_client(
            enabled=cfg.beads_enabled if hasattr(cfg, 'beads_enabled') else True,
            tickets_file=tickets_file,
        )
        # Find ticket by file name
        for state in [TicketState.CREATED, TicketState.IN_PROGRESS, TicketState.CLAIMED]:
            tickets = beads_client.get_tickets_by_state(
                state=state,
                ticket_type=TicketType.DOCUMENTATION,
            )
            for ticket in tickets:
                if ticket.file_name == file_path.name:
                    decision = result.get("decision", "UNKNOWN")
                    beads_client.update_ticket_state(
                        ticket.ticket_id,
                        TicketState.COMPLETED,
                        reason=f"Documentation {decision}",
                        decision=decision,
                    )
                    break

    # Display results
    decision = result.get("decision", "UNKNOWN")
    color = {
        "WITNESSED": "green",
        "VALHALLA": "gold1",
        "CHROME": "yellow",
        "FORCED": "orange1",
    }.get(decision, "white")

    console.print()
    console.print(f"[bold {color}]Decision: {decision}[/bold {color}]")
    console.print(f"Iterations: {result.get('iteration', 0)}")
    console.print(f"Tokens used: {result.get('tokens_used', 0)}")

    if result.get("error"):
        console.print(f"[yellow]Warning: {result['error']}[/yellow]")

    console.print()
    console.print("[bold]Output files:[/bold]")
    for name, path in outputs.items():
        console.print(f"  {name}: {path}")

    # Generate system overview if we have completed documentation
    programs_dir = cfg.output_directory / "final" / "programs"
    if programs_dir.exists() and list(programs_dir.glob("*.json")):
        console.print("\n[cyan]Generating system overview...[/cyan]")
        try:
            _generate_overview_internal(cfg, mock=mock)
            console.print(f"[green]System overview written to: {cfg.output_directory / 'SYSTEM_OVERVIEW.md'}[/green]")
        except Exception as e:
            console.print(f"[yellow]Warning: Could not generate overview: {e}[/yellow]")


@app.command()
def batch(
    directory: Annotated[
        Path,
        typer.Argument(
            help="Directory containing source files",
            exists=True,
            file_okay=False,
            dir_okay=True,
            readable=True,
        ),
    ],
    config: Annotated[
        Optional[Path],
        typer.Option(
            "--config",
            "-c",
            help="Path to configuration file",
        ),
    ] = None,
    output: Annotated[
        Optional[Path],
        typer.Option(
            "--output",
            "-o",
            help="Output directory (overrides config)",
        ),
    ] = None,
    file_type: Annotated[
        Optional[str],
        typer.Option(
            "--type",
            "-t",
            help="File type filter (COBOL, JCL, COPYBOOK)",
        ),
    ] = None,
    limit: Annotated[
        Optional[int],
        typer.Option(
            "--limit",
            "-l",
            help="Maximum files to process",
        ),
    ] = None,
    mock: Annotated[
        bool,
        typer.Option(
            "--mock",
            "-m",
            help="Use mock agents (for testing)",
        ),
    ] = False,
    resume: Annotated[
        bool,
        typer.Option(
            "--resume",
            "-r",
            help="Skip files that already have output in final/programs/",
        ),
    ] = False,
    verbose: Annotated[
        bool,
        typer.Option(
            "--verbose",
            "-v",
            help="Enable verbose logging",
        ),
    ] = False,
    skills_output: Annotated[
        bool,
        typer.Option(
            "--skills",
            help="Generate Agent Skills after batch processing",
        ),
    ] = False,
    skills_dir: Annotated[
        Optional[Path],
        typer.Option(
            "--skills-output",
            help="Output directory for skills (default: skills-{output_name})",
        ),
    ] = None,
) -> None:
    """Process a directory of source files.

    This command discovers and processes all recognized source files
    in a directory, generating documentation for each.

    Example:
        $ war-rig batch path/to/source/
        $ war-rig batch path/to/source/ --type COBOL --limit 10
        $ war-rig batch path/to/source/ --resume  # Skip already-processed files
        $ war-rig batch path/to/source/ --skills  # Generate Agent Skills after processing
    """
    setup_logging(verbose)

    # Load configuration
    cfg = load_config_with_fallback(config)
    if output:
        cfg.output_directory = output

    console.print(Panel.fit(
        f"[bold blue]War Rig[/bold blue] - Batch Processing: {directory}",
        border_style="blue",
    ))

    # Discover files
    reader = SourceReader(cfg.system)

    type_filter = None
    if file_type:
        try:
            type_filter = [FileType(file_type.upper())]
        except ValueError:
            console.print(f"[red]Unknown file type: {file_type}[/red]")
            raise typer.Exit(1)

    files = list(reader.discover_files(directory, file_types=type_filter))
    total_discovered = len(files)

    # Filter out already-processed files if --resume is enabled
    if resume:
        output_dir = cfg.output_directory
        programs_dir = output_dir / "final" / "programs"
        if programs_dir.exists():
            completed = {p.stem for p in programs_dir.glob("*.json")}
            original_count = len(files)
            files = [f for f in files if f.stem not in completed]
            skipped = original_count - len(files)
            if skipped > 0:
                console.print(f"[cyan]Resuming: skipping {skipped} already-completed files[/cyan]")

    if limit:
        files = files[:limit]

    if not files:
        if resume and total_discovered > 0:
            console.print("[green]All files already processed![/green]")
        else:
            console.print("[yellow]No files found to process[/yellow]")
        raise typer.Exit(0)

    console.print(f"Found {len(files)} files to process")

    # Use TicketOrchestrator for batch processing
    # This ensures Imperator only reviews after all Scribe/Challenger work is done
    orchestrator = TicketOrchestrator(
        config=cfg,
        use_mock=mock,
    )

    console.print("\n[cyan]Starting batch processing with parallel workers...[/cyan]")
    console.print("[dim]Scribes document, Challengers validate, then Imperator reviews holistically[/dim]\n")

    try:
        batch_result = asyncio.run(orchestrator.run_batch(directory))

        # Display summary
        console.print()

        table = Table(title="Processing Summary")
        table.add_column("Status", style="bold")
        table.add_column("Count", justify="right")

        table.add_row("[green]Completed[/green]", str(len(batch_result.completed_files)))
        table.add_row("[red]Failed[/red]", str(len(batch_result.failed_files)))
        table.add_row("[cyan]Total Cycles[/cyan]", str(batch_result.total_cycles))
        table.add_row("[blue]Final Decision[/blue]", batch_result.final_decision or "N/A")

        console.print(table)

        if batch_result.failed_files:
            console.print("\n[red]Failed files:[/red]")
            for file_name, error in batch_result.failed_files.items():
                console.print(f"  {file_name}: {error}")

        if batch_result.quality_notes:
            console.print(f"\n[cyan]Quality Notes:[/cyan] {batch_result.quality_notes}")

    except Exception as e:
        console.print(f"\n[red]Batch processing failed: {e}[/red]")
        raise typer.Exit(1)

    # Process datacards (utility control statements) if any exist
    try:
        datacard_path = process_datacards(directory, cfg.output_directory)
        if datacard_path:
            console.print(f"\n[green]Datacard catalog written to: {datacard_path}[/green]")
    except Exception as e:
        console.print(f"[yellow]Warning: Could not process datacards: {e}[/yellow]")

    # Generate system overview if we have completed documentation
    # Check total completed (current run + previously completed via --resume)
    programs_dir = cfg.output_directory / "final" / "programs"
    if programs_dir.exists():
        all_completed = list(programs_dir.glob("*.json"))
        if all_completed:
            console.print("\n[cyan]Generating system overview...[/cyan]")
            try:
                _generate_overview_internal(cfg, mock=mock)
                console.print(f"[green]System overview written to: {cfg.output_directory / 'SYSTEM_OVERVIEW.md'}[/green]")
            except Exception as e:
                console.print(f"[yellow]Warning: Could not generate overview: {e}[/yellow]")

    # Generate Agent Skills if --skills flag is set
    if skills_output:
        programs_dir = cfg.output_directory / "final" / "programs"
        if programs_dir.exists() and list(programs_dir.glob("*.json")):
            console.print("\n[cyan]Generating Agent Skills...[/cyan]")
            try:
                from war_rig.skills import SkillsGenerator

                generator = SkillsGenerator(cfg.output_directory, skills_dir)
                skills_path = generator.generate()
                console.print(f"[green]Skills generated at: {skills_path}[/green]")
            except Exception as e:
                console.print(f"[yellow]Warning: Could not generate skills: {e}[/yellow]")
        else:
            console.print("[yellow]Skipping skills generation: no completed documentation[/yellow]")


@app.command()
def status(
    output: Annotated[
        Optional[Path],
        typer.Option(
            "--output",
            "-o",
            help="Output directory to check",
        ),
    ] = None,
    config: Annotated[
        Optional[Path],
        typer.Option(
            "--config",
            "-c",
            help="Path to configuration file",
        ),
    ] = None,
) -> None:
    """Show processing status and statistics.

    This command displays the current state of the output directory,
    including counts of processed files and their statuses.

    Example:
        $ war-rig status
        $ war-rig status --output ./docs
    """
    # Load configuration
    cfg = load_config_with_fallback(config)
    if output:
        cfg.output_directory = output

    output_dir = cfg.output_directory

    console.print(Panel.fit(
        f"[bold blue]War Rig[/bold blue] - Status: {output_dir}",
        border_style="blue",
    ))

    if not output_dir.exists():
        console.print("[yellow]Output directory does not exist[/yellow]")
        raise typer.Exit(0)

    # Count files in various directories
    final_programs = list((output_dir / "final" / "programs").glob("*.json"))
    final_copybooks = list((output_dir / "final" / "copybooks").glob("*.json"))
    final_jcl = list((output_dir / "final" / "jcl").glob("*.json"))
    valhalla = list((output_dir / "valhalla").glob("*.json"))
    chrome = list((output_dir / "war_rig" / "chrome").glob("*.json"))

    table = Table(title="Documentation Status")
    table.add_column("Category", style="bold")
    table.add_column("Count", justify="right")

    table.add_row("Programs", str(len(final_programs)))
    table.add_row("Copybooks", str(len(final_copybooks)))
    table.add_row("JCL", str(len(final_jcl)))
    table.add_row("[gold1]Valhalla[/gold1]", str(len(valhalla)))
    table.add_row("[yellow]Open Chrome Tickets[/yellow]", str(len(chrome)))

    console.print(table)

    # Show recent files
    if final_programs:
        console.print("\n[bold]Recent Programs:[/bold]")
        recent = sorted(final_programs, key=lambda p: p.stat().st_mtime, reverse=True)[:5]
        for path in recent:
            console.print(f"  {path.stem}")


@app.command()
def init(
    output: Annotated[
        Optional[Path],
        typer.Option(
            "--output",
            "-o",
            help="Output directory to initialize",
        ),
    ] = None,
    config: Annotated[
        Optional[Path],
        typer.Option(
            "--config",
            "-c",
            help="Path to configuration file",
        ),
    ] = None,
) -> None:
    """Initialize output directories.

    This command creates the directory structure needed for War Rig output.

    Example:
        $ war-rig init
        $ war-rig init --output ./docs
    """
    # Load configuration
    cfg = load_config_with_fallback(config)
    if output:
        cfg.output_directory = output

    console.print(Panel.fit(
        f"[bold blue]War Rig[/bold blue] - Initializing: {cfg.output_directory}",
        border_style="blue",
    ))

    # Create writer (which creates directories)
    writer = DocumentationWriter(cfg.system)

    console.print("[green]Output directories initialized successfully[/green]")
    console.print(f"\nOutput directory: {cfg.output_directory}")


@app.command()
def overview(
    config: Annotated[
        Optional[Path],
        typer.Option(
            "--config",
            "-c",
            help="Path to configuration file",
        ),
    ] = None,
    output: Annotated[
        Optional[Path],
        typer.Option(
            "--output",
            "-o",
            help="Output directory (overrides config)",
        ),
    ] = None,
    system_name: Annotated[
        str,
        typer.Option(
            "--name",
            "-n",
            help="Name of the system being documented",
        ),
    ] = "CardDemo",
    mock: Annotated[
        bool,
        typer.Option(
            "--mock",
            "-m",
            help="Use mock output (for testing)",
        ),
    ] = False,
    verbose: Annotated[
        bool,
        typer.Option(
            "--verbose",
            "-v",
            help="Enable verbose logging",
        ),
    ] = False,
) -> None:
    """Generate system overview from completed documentation.

    This command reads all completed documentation from output/final/programs/
    and generates a SYSTEM_OVERVIEW.md file that synthesizes the individual
    program documentation into a coherent narrative.

    Example:
        $ war-rig overview
        $ war-rig overview --name "My System"
        $ war-rig overview --output ./docs
    """
    setup_logging(verbose)

    # Load configuration
    cfg = load_config_with_fallback(config)
    if output:
        cfg.output_directory = output

    console.print(Panel.fit(
        f"[bold blue]War Rig[/bold blue] - Generating System Overview",
        border_style="blue",
    ))

    programs_dir = cfg.output_directory / "final" / "programs"
    if not programs_dir.exists():
        console.print(f"[red]No documentation found at {programs_dir}[/red]")
        raise typer.Exit(1)

    doc_count = len(list(programs_dir.glob("*.json")))
    console.print(f"Found {doc_count} documented programs")
    console.print("Generating system overview...")

    try:
        _generate_overview_internal(cfg, system_name=system_name, mock=mock)
        console.print(f"[green]System overview written to: {cfg.output_directory / 'SYSTEM_OVERVIEW.md'}[/green]")
    except Exception as e:
        console.print(f"[red]Error generating overview: {e}[/red]")
        raise typer.Exit(1)


@app.command()
def datacards(
    directory: Annotated[
        Path,
        typer.Argument(
            help="Directory containing datacard files (.dc)",
            exists=True,
            file_okay=False,
            dir_okay=True,
            readable=True,
        ),
    ],
    output: Annotated[
        Optional[Path],
        typer.Option(
            "--output",
            "-o",
            help="Output directory (overrides config)",
        ),
    ] = None,
    config: Annotated[
        Optional[Path],
        typer.Option(
            "--config",
            "-c",
            help="Path to configuration file",
        ),
    ] = None,
    verbose: Annotated[
        bool,
        typer.Option(
            "--verbose",
            "-v",
            help="Enable verbose logging",
        ),
    ] = False,
) -> None:
    """Process datacard files and generate a catalog.

    This command scans a directory for datacard files (.dc), auto-detects
    the utility type for each file (UNLOAD, REPRO, SORT, etc.), and generates
    a single consolidated DATACARDS.md catalog.

    Example:
        $ war-rig datacards path/to/source/
        $ war-rig datacards path/to/source/ --output ./docs
    """
    setup_logging(verbose)

    # Load configuration
    cfg = load_config_with_fallback(config)
    if output:
        cfg.output_directory = output

    console.print(Panel.fit(
        f"[bold blue]War Rig[/bold blue] - Processing Datacards: {directory}",
        border_style="blue",
    ))

    try:
        datacard_path = process_datacards(directory, cfg.output_directory)
        if datacard_path:
            console.print(f"\n[green]Datacard catalog written to: {datacard_path}[/green]")
        else:
            console.print("[yellow]No datacard files (.dc) found[/yellow]")
    except Exception as e:
        console.print(f"[red]Error processing datacards: {e}[/red]")
        raise typer.Exit(1) from None


@app.command()
def skills(
    directory: Annotated[
        Path,
        typer.Argument(
            help="Path to War Rig output directory",
            exists=True,
            file_okay=False,
            dir_okay=True,
            readable=True,
        ),
    ],
    output: Annotated[
        Optional[Path],
        typer.Option(
            "--output",
            "-o",
            help="Output directory for skills (default: skills-{input_name})",
        ),
    ] = None,
    verbose: Annotated[
        bool,
        typer.Option(
            "--verbose",
            "-v",
            help="Enable verbose logging",
        ),
    ] = False,
) -> None:
    """Generate Agent Skills from War Rig documentation.

    This command converts War Rig documentation output into Agent Skills format
    for progressive discovery by AI agents.

    Example:
        $ war-rig skills ./output
        $ war-rig skills ./output --output ./my-skills
    """
    setup_logging(verbose)

    console.print(Panel.fit(
        f"[bold blue]War Rig[/bold blue] - Generating Agent Skills: {directory}",
        border_style="blue",
    ))

    try:
        from war_rig.skills import SkillsGenerator

        generator = SkillsGenerator(directory, output)
        skills_path = generator.generate()
        console.print(f"\n[green]Skills generated at: {skills_path}[/green]")
    except Exception as e:
        console.print(f"[red]Error generating skills: {e}[/red]")
        raise typer.Exit(1) from None


@app.command()
def version() -> None:
    """Show War Rig version information."""
    from war_rig import __version__

    console.print(f"[bold blue]War Rig[/bold blue] version {__version__}")


def _generate_overview_internal(
    cfg: WarRigConfig,
    system_name: str = "CardDemo",
    mock: bool = False,
) -> None:
    """Internal helper to generate system overview.

    Args:
        cfg: War Rig configuration.
        system_name: Name of the system being documented.
        mock: Whether to use mock output.

    Raises:
        Exception: If overview generation fails.
    """
    import json

    from war_rig.agents.imperator import (
        ImperatorAgent,
        ProgramSummary,
        SystemOverviewInput,
    )

    output_dir = cfg.output_directory
    programs_dir = output_dir / "final" / "programs"

    if not programs_dir.exists():
        raise FileNotFoundError(f"No documentation found at {programs_dir}")

    # Collect program summaries
    programs: list[ProgramSummary] = []

    doc_files = list(programs_dir.glob("*.doc.json")) + list(programs_dir.glob("*.json"))
    # Deduplicate by stem
    seen_stems: set[str] = set()
    unique_files = []
    for f in doc_files:
        stem = f.stem.replace(".doc", "")
        if stem not in seen_stems:
            seen_stems.add(stem)
            unique_files.append(f)

    if not unique_files:
        raise FileNotFoundError("No documentation files found")

    for doc_file in unique_files:
        try:
            doc_data = json.loads(doc_file.read_text(encoding="utf-8"))

            header = doc_data.get("header", {})
            purpose = doc_data.get("purpose", {})
            called_programs = doc_data.get("called_programs", [])
            inputs = doc_data.get("inputs", [])
            outputs_data = doc_data.get("outputs", [])

            calls = [cp.get("program_name", "") for cp in called_programs if cp.get("program_name")]
            input_names = [i.get("name", "") for i in inputs if i.get("name")][:5]
            output_names = [o.get("name", "") for o in outputs_data if o.get("name")][:5]

            programs.append(ProgramSummary(
                file_name=header.get("file_name", doc_file.stem),
                program_id=header.get("program_id", doc_file.stem.replace(".doc", "")),
                program_type=purpose.get("program_type", "UNKNOWN"),
                summary=purpose.get("summary", "No summary available"),
                business_context=purpose.get("business_context", ""),
                calls=calls,
                called_by=[],
                inputs=input_names,
                outputs=output_names,
                json_path=f"./final/programs/{doc_file.name}",
            ))
        except Exception:
            pass  # Skip unparseable files

    if not programs:
        raise ValueError("No valid documentation found")

    # Build cross-references (called_by)
    for prog in programs:
        for called in prog.calls:
            for other in programs:
                if other.program_id == called:
                    if prog.program_id not in other.called_by:
                        other.called_by.append(prog.program_id)

    # Create input
    input_data = SystemOverviewInput(
        batch_id="cli-overview",
        system_name=system_name,
        programs=programs,
        total_files=len(programs),
    )

    # Generate overview
    imperator = ImperatorAgent(
        config=cfg.imperator,
        api_config=cfg.api,
    )

    result = asyncio.run(
        imperator.generate_system_overview(input_data, use_mock=mock)
    )

    if result.success:
        overview_path = output_dir / "SYSTEM_OVERVIEW.md"
        overview_path.write_text(result.markdown, encoding="utf-8")
    else:
        raise RuntimeError(f"Failed to generate overview: {result.error}")


def main() -> None:
    """Main entry point for the CLI."""
    app()


if __name__ == "__main__":
    main()
