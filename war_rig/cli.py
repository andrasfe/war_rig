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
from pathlib import Path
from typing import Annotated

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


def load_config_with_fallback(config_path: Path | None) -> WarRigConfig:
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
        Path | None,
        typer.Option(
            "--config",
            "-c",
            help="Path to configuration file",
        ),
    ] = None,
    output: Annotated[
        Path | None,
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
        Path | None,
        typer.Option(
            "--config",
            "-c",
            help="Path to configuration file",
        ),
    ] = None,
    output: Annotated[
        Path | None,
        typer.Option(
            "--output",
            "-o",
            help="Output directory (overrides config)",
        ),
    ] = None,
    file_type: Annotated[
        str | None,
        typer.Option(
            "--type",
            "-t",
            help="File type filter (COBOL, JCL, COPYBOOK)",
        ),
    ] = None,
    limit: Annotated[
        int | None,
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
        Path | None,
        typer.Option(
            "--skills-output",
            help="Output directory for skills (default: skills-{output_name})",
        ),
    ] = None,
    no_new_tickets: Annotated[
        bool,
        typer.Option(
            "--no-new-tickets",
            help="Skip ticket creation, work only with existing tickets from a previous run",
        ),
    ] = False,
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

    if no_new_tickets:
        cfg.skip_ticket_creation = True
        console.print("[cyan]--no-new-tickets: working with existing tickets only[/cyan]")

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
        # New generator expects documentation directory with cbl/, jcl/, etc. subdirs
        docs_dir = cfg.output_directory / "documentation"
        if not docs_dir.exists():
            docs_dir = cfg.output_directory  # Fall back to output dir itself

        # Check if there are any .md documentation files
        has_docs = any(docs_dir.glob("*/*.md"))
        if has_docs:
            console.print("\n[cyan]Generating Agent Skills...[/cyan]")
            try:
                from war_rig.skills import SkillsGenerator

                generator = SkillsGenerator(docs_dir, skills_dir)
                result = generator.generate_with_result()
                console.print(f"[green]Skills generated at: {result.output_dir}[/green]")
                console.print(f"  Categories: {len(result.categories_created)}, Files: {result.files_processed}")
            except Exception as e:
                console.print(f"[yellow]Warning: Could not generate skills: {e}[/yellow]")
        else:
            console.print("[yellow]Skipping skills generation: no documentation files found[/yellow]")


@app.command()
def status(
    output: Annotated[
        Path | None,
        typer.Option(
            "--output",
            "-o",
            help="Output directory to check",
        ),
    ] = None,
    config: Annotated[
        Path | None,
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
        Path | None,
        typer.Option(
            "--output",
            "-o",
            help="Output directory to initialize",
        ),
    ] = None,
    config: Annotated[
        Path | None,
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
        Path | None,
        typer.Option(
            "--config",
            "-c",
            help="Path to configuration file",
        ),
    ] = None,
    output: Annotated[
        Path | None,
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
        "[bold blue]War Rig[/bold blue] - Generating System Overview",
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
        Path | None,
        typer.Option(
            "--output",
            "-o",
            help="Output directory (overrides config)",
        ),
    ] = None,
    config: Annotated[
        Path | None,
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
            help="Path to War Rig documentation directory (or parent output dir)",
            exists=True,
            file_okay=False,
            dir_okay=True,
            readable=True,
        ),
    ],
    output: Annotated[
        Path | None,
        typer.Option(
            "--output",
            "-o",
            help="Output directory for skills (default: skills-{input_name})",
        ),
    ] = None,
    system_name: Annotated[
        str,
        typer.Option(
            "--system-name",
            "-n",
            help="System name for top-level skill title",
        ),
    ] = "System",
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
    for progressive discovery by AI agents. Skills are organized by category
    (COBOL, JCL, IMS, etc.) with program summaries and links to full docs.

    Example:
        $ war-rig skills ./output/documentation
        $ war-rig skills ./output  # Auto-detects documentation/ subdir
        $ war-rig skills ./output --output ./my-skills --system-name "CardDemo"
    """
    setup_logging(verbose)

    # If user passed parent output dir, look for documentation subdir
    input_dir = directory.resolve()
    doc_subdir = input_dir / "documentation"
    if doc_subdir.exists() and doc_subdir.is_dir():
        console.print(f"[dim]Found documentation subdirectory, using: {doc_subdir}[/dim]")
        input_dir = doc_subdir

    console.print(Panel.fit(
        f"[bold blue]War Rig[/bold blue] - Generating Agent Skills: {input_dir}",
        border_style="blue",
    ))

    try:
        from war_rig.skills import SkillsGenerator

        generator = SkillsGenerator(input_dir, output, system_name=system_name)
        result = generator.generate_with_result()

        console.print(f"\n[green]Skills generated at: {result.output_dir}[/green]")
        console.print(f"  Categories: {len(result.categories_created)}")
        console.print(f"  Files processed: {result.files_processed}")

        if result.errors:
            console.print(f"[yellow]  Warnings: {len(result.errors)}[/yellow]")
    except Exception as e:
        console.print(f"[red]Error generating skills: {e}[/red]")
        raise typer.Exit(1) from None


@app.command(name="generate-readme")
def generate_readme(
    config: Annotated[
        Path | None,
        typer.Option(
            "--config",
            "-c",
            help="Path to configuration file",
        ),
    ] = None,
    output: Annotated[
        Path | None,
        typer.Option(
            "--output",
            "-o",
            help="Output directory (overrides config)",
        ),
    ] = None,
    source: Annotated[
        Path | None,
        typer.Option(
            "--source",
            "-s",
            help="Source directory for Citadel analysis (optional)",
        ),
    ] = None,
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
    """Generate README.md from existing documentation.

    This command reads completed documentation from output/final/programs/
    and generates a comprehensive README.md system design document using
    Imperator's generate_system_design method.

    Example:
        $ war-rig generate-readme
        $ war-rig generate-readme --output ./docs
        $ war-rig generate-readme --source ./src  # Include call graph from source
    """
    setup_logging(verbose)

    # Load configuration
    cfg = load_config_with_fallback(config)
    if output:
        cfg.output_directory = output

    console.print(Panel.fit(
        "[bold blue]War Rig[/bold blue] - Generating README.md",
        border_style="blue",
    ))

    # Check for documentation in various locations
    output_dir = cfg.output_directory
    doc_count = 0

    # Check final/programs first
    programs_dir = output_dir / "final" / "programs"
    if programs_dir.exists():
        doc_count = len(list(programs_dir.glob("*.json")))
    else:
        # Check type-organized structure
        for subdir in ["cbl", "cpy", "ims", "jcl", "bms", "ddl", "asm", "pli"]:
            type_dir = output_dir / subdir
            if type_dir.exists():
                doc_count += len(list(type_dir.glob("*.doc.json")))

    if doc_count == 0:
        console.print(f"[red]No documentation found in {output_dir}[/red]")
        console.print("[dim]Checked: final/programs/, cbl/, cpy/, ims/, jcl/, etc.[/dim]")
        raise typer.Exit(1)

    console.print(f"Found {doc_count} documented programs")
    console.print("Building README.md...")

    try:
        _generate_readme_internal(
            cfg,
            source_dir=source,
            mock=mock,
        )
        readme_path = cfg.output_directory / "README.md"
        console.print(f"[green]README.md written to: {readme_path}[/green]")
    except Exception as e:
        console.print(f"[red]Error generating README: {e}[/red]")
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


def _generate_readme_internal(
    cfg: WarRigConfig,
    source_dir: Path | None = None,
    mock: bool = False,
) -> None:
    """Internal helper to generate README.md from existing documentation.

    Args:
        cfg: War Rig configuration.
        source_dir: Optional source directory for Citadel analysis.
        mock: Whether to use mock output.

    Raises:
        Exception: If README generation fails.
    """
    import json

    from war_rig.agents.imperator import (
        FileDocumentation,
        HolisticReviewInput,
        ImperatorAgent,
    )
    from war_rig.models.assessments import ConfidenceLevel
    from war_rig.models.templates import DocumentationTemplate

    output_dir = cfg.output_directory

    # Find documentation files - check multiple possible structures:
    # 1. final/programs/*.json (standard structure)
    # 2. cbl/*.doc.json, cpy/*.doc.json, etc. (type-organized structure)
    # 3. *.doc.json in root (flat structure)
    doc_files: list[Path] = []

    programs_dir = output_dir / "final" / "programs"
    if programs_dir.exists():
        doc_files = list(programs_dir.glob("*.doc.json")) + list(programs_dir.glob("*.json"))
    else:
        # Try type-organized structure (cbl/, cpy/, ims/, jcl/, etc.)
        for subdir in ["cbl", "cpy", "ims", "jcl", "bms", "ddl", "asm", "pli"]:
            type_dir = output_dir / subdir
            if type_dir.exists():
                doc_files.extend(type_dir.glob("*.doc.json"))

        # Also check root for flat structure
        if not doc_files:
            doc_files = list(output_dir.glob("*.doc.json"))

    if not doc_files:
        raise FileNotFoundError(
            f"No documentation found. Checked:\n"
            f"  - {output_dir}/final/programs/\n"
            f"  - {output_dir}/cbl/, cpy/, ims/, jcl/, etc.\n"
            f"  - {output_dir}/*.doc.json"
        )

    # Collect file documentation
    file_docs: list[FileDocumentation] = []
    per_file_confidence: dict[str, ConfidenceLevel] = {}

    # Deduplicate by stem
    seen_stems: set[str] = set()
    unique_files = []
    for f in doc_files:
        # Handle stems like "PROGRAM.cbl.doc" -> "PROGRAM.cbl"
        stem = f.stem
        if stem.endswith(".doc"):
            stem = stem[:-4]
        if stem not in seen_stems:
            seen_stems.add(stem)
            unique_files.append(f)

    for doc_file in unique_files:
        try:
            doc_data = json.loads(doc_file.read_text(encoding="utf-8"))

            # Load template from JSON - use model_validate for proper nested object hydration
            try:
                template = DocumentationTemplate.model_validate(doc_data)
            except Exception:
                # Fall back to lenient construction if validation fails
                template = DocumentationTemplate.model_construct(**doc_data)

            header = doc_data.get("header", {})
            file_name = header.get("file_name", doc_file.stem)
            program_id = header.get("program_id", doc_file.stem.replace(".doc", ""))

            file_docs.append(
                FileDocumentation(
                    file_name=file_name,
                    program_id=program_id,
                    template=template,
                    iteration_count=header.get("iteration_count", 1),
                )
            )
            per_file_confidence[file_name] = ConfidenceLevel.MEDIUM

        except Exception as e:
            console.print(f"[yellow]Warning: Skipping {doc_file.name}: {e}[/yellow]")

    if not file_docs:
        raise ValueError("No valid documentation found")

    # Build cross-file analysis using Citadel (if source_dir provided)
    shared_copybooks: dict[str, list[str]] = {}
    call_graph: dict[str, list[str]] = {}
    data_flow: dict[str, list[str]] = {}
    call_graph_markdown: str | None = None

    # Load CALL_GRAPH.md if it exists
    call_graph_path = output_dir / "CALL_GRAPH.md"
    if call_graph_path.exists():
        try:
            call_graph_markdown = call_graph_path.read_text(encoding="utf-8")
            console.print(f"[dim]Loaded CALL_GRAPH.md ({len(call_graph_markdown)} chars)[/dim]")
        except Exception:
            pass

    # Run Citadel analysis if source directory provided
    if source_dir and source_dir.exists():
        try:
            from citadel import Citadel

            citadel = Citadel()
            callouts = citadel.get_callouts(source_dir)

            for callout in callouts:
                if "error" in callout:
                    continue

                from_prog = callout.get("from", "").upper()
                to_target = callout.get("to", "").upper()
                rel_type = callout.get("type", "")

                if not from_prog or not to_target:
                    continue

                if rel_type == "includes":
                    if to_target not in shared_copybooks:
                        shared_copybooks[to_target] = []
                    if from_prog not in shared_copybooks[to_target]:
                        shared_copybooks[to_target].append(from_prog)

                elif rel_type in ("calls", "executes", "performs"):
                    if from_prog not in call_graph:
                        call_graph[from_prog] = []
                    if to_target not in call_graph[from_prog]:
                        call_graph[from_prog].append(to_target)

                elif rel_type in ("reads", "writes", "updates", "deletes"):
                    if from_prog not in data_flow:
                        data_flow[from_prog] = []
                    if to_target not in data_flow[from_prog]:
                        data_flow[from_prog].append(to_target)

            console.print(
                f"[dim]Citadel: {len(shared_copybooks)} copybooks, "
                f"{len(call_graph)} callers[/dim]"
            )

        except ImportError:
            console.print("[dim]Citadel not available for cross-file analysis[/dim]")
        except Exception as e:
            console.print(f"[yellow]Warning: Citadel analysis failed: {e}[/yellow]")

    # Generate sequence diagrams from dependency graph
    sequence_diagrams: list[str] = []
    dep_graph_path = output_dir / "dependency_graph.json"
    if dep_graph_path.exists():
        try:
            from citadel import Citadel

            citadel = Citadel()
            sequence_diagrams = citadel.get_sequence_diagrams(
                dep_graph_path,
                min_sequence_length=2,  # At least 2 calls in sequence
                max_diagrams=10,  # Limit to top 10 flows
            )
            if sequence_diagrams:
                console.print(f"[dim]Generated {len(sequence_diagrams)} sequence diagrams[/dim]")
        except ImportError:
            console.print("[dim]Citadel not available for sequence diagrams[/dim]")
        except Exception as e:
            console.print(f"[yellow]Warning: Sequence diagram generation failed: {e}[/yellow]")

    # Build HolisticReviewInput
    review_input = HolisticReviewInput(
        batch_id="cli-readme",
        cycle=1,
        file_documentation=file_docs,
        shared_copybooks=shared_copybooks,
        call_graph=call_graph,
        call_graph_markdown=call_graph_markdown,
        data_flow=data_flow,
        per_file_confidence=per_file_confidence,
        per_file_issues={},
        previous_clarification_requests=[],
        previous_chrome_tickets=[],
        resolution_status={},
        max_cycles=5,
    )

    # Generate README using Imperator
    imperator = ImperatorAgent(
        config=cfg.imperator,
        api_config=cfg.api,
    )

    # Check for existing README to enhance
    existing_readme: str | None = None
    readme_path = output_dir / "README.md"
    if readme_path.exists():
        try:
            existing_readme = readme_path.read_text(encoding="utf-8")
            console.print("[dim]Found existing README.md - will enhance[/dim]")
        except Exception:
            pass

    result = asyncio.run(
        imperator.generate_system_design(
            review_input,
            existing_content=existing_readme,
            use_mock=mock,
            sequence_diagrams=sequence_diagrams if sequence_diagrams else None,
        )
    )

    if result.success:
        readme_path.write_text(result.markdown, encoding="utf-8")
    else:
        raise RuntimeError(f"Failed to generate README: {result.error}")


@app.command(name="enrich-kg")
def enrich_kg(
    output_dir: Annotated[
        Path,
        typer.Option(
            "--output-dir",
            help="Directory containing .doc.json files",
            exists=True,
            file_okay=False,
            dir_okay=True,
        ),
    ],
    config_path: Annotated[
        Path | None,
        typer.Option(
            "--config",
            help="Path to configuration file",
        ),
    ] = None,
    verbose: Annotated[
        bool,
        typer.Option("--verbose", "-v", help="Enable verbose logging"),
    ] = False,
) -> None:
    """Enrich the knowledge graph from existing .doc.json documentation files.

    Discovers all .doc.json files under the output directory, extracts
    KG triples from their structured fields (called_programs, copybooks,
    data_flow, inputs/outputs, paragraphs), and ingests them into the
    knowledge graph. This improves KG health without re-running LLM calls.
    """
    import json as json_mod

    from war_rig.knowledge_graph.manager import KnowledgeGraphManager
    from war_rig.models.templates import DocumentationTemplate

    setup_logging(verbose)

    cfg = load_config_with_fallback(config_path)
    cfg.output_directory = output_dir

    # Force KG enabled for enrichment
    cfg.knowledge_graph_enabled = True

    async def _run_enrichment() -> None:
        manager = KnowledgeGraphManager(cfg)
        await manager.initialize()

        doc_files = sorted(output_dir.rglob("*.doc.json"))
        if not doc_files:
            console.print("[yellow]No .doc.json files found[/yellow]")
            await manager.close()
            return

        console.print(
            f"[bold]Found {len(doc_files)} .doc.json files[/bold]"
        )

        total_triples = 0
        with Progress(
            SpinnerColumn(),
            TextColumn("[progress.description]{task.description}"),
            console=console,
        ) as progress:
            task = progress.add_task("Enriching KG...", total=len(doc_files))

            for doc_file in doc_files:
                try:
                    with open(doc_file) as f:
                        data = json_mod.load(f)
                    template = DocumentationTemplate.load_lenient(data)
                    triples = await manager.ingest_documentation_template(
                        template
                    )
                    count = len(triples)
                    total_triples += count
                    if count > 0:
                        prog_name = (
                            template.header.program_id
                            if template.header
                            else doc_file.stem
                        )
                        progress.console.print(
                            f"  {prog_name}: {count} triples"
                        )
                except Exception as e:
                    progress.console.print(
                        f"  [red]Error processing {doc_file.name}: {e}[/red]"
                    )
                progress.advance(task)

        await manager.close()

        stats_table = Table(title="KG Enrichment Summary")
        stats_table.add_column("Metric", style="bold")
        stats_table.add_column("Value", justify="right")
        stats_table.add_row("Files processed", str(len(doc_files)))
        stats_table.add_row("Total triples ingested", str(total_triples))
        console.print(stats_table)

    try:
        asyncio.run(_run_enrichment())
    except Exception as e:
        console.print(f"[red]Error during KG enrichment: {e}[/red]")
        raise typer.Exit(1) from None


def main() -> None:
    """Main entry point for the CLI."""
    app()


if __name__ == "__main__":
    main()
