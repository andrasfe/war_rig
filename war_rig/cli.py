"""Command-line interface for War Rig.

This module provides the CLI entry point for the War Rig documentation
system. It uses Typer for a modern, type-annotated CLI experience.

Commands:
    analyze: Document a single source file
    batch: Process a directory of source files
    status: Show processing status
    init: Initialize output directories

Example:
    $ war-rig analyze path/to/PROGRAM.cbl
    $ war-rig batch path/to/source/directory
    $ war-rig status
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

from war_rig.config import WarRigConfig, load_config
from war_rig.io.reader import SourceReader
from war_rig.io.writer import DocumentationWriter
from war_rig.models.templates import FileType
from war_rig.orchestration.graph import create_war_rig_graph

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
        cfg.system.output_directory = output

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
    verbose: Annotated[
        bool,
        typer.Option(
            "--verbose",
            "-v",
            help="Enable verbose logging",
        ),
    ] = False,
) -> None:
    """Process a directory of source files.

    This command discovers and processes all recognized source files
    in a directory, generating documentation for each.

    Example:
        $ war-rig batch path/to/source/
        $ war-rig batch path/to/source/ --type COBOL --limit 10
    """
    setup_logging(verbose)

    # Load configuration
    cfg = load_config_with_fallback(config)
    if output:
        cfg.system.output_directory = output

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

    if limit:
        files = files[:limit]

    if not files:
        console.print("[yellow]No files found to process[/yellow]")
        raise typer.Exit(0)

    console.print(f"Found {len(files)} files to process")

    # Create graph
    graph = create_war_rig_graph(cfg)
    writer = DocumentationWriter(cfg.system)

    results = []
    errors = []

    with Progress(console=console) as progress:
        task = progress.add_task("Processing...", total=len(files))

        for source_file in files:
            progress.update(task, description=f"Processing {source_file.name}...")

            try:
                source_code = reader.read_source_file(source_file)
                result = asyncio.run(
                    graph.ainvoke(
                        source_code=source_code,
                        file_name=source_file.name,
                        use_mock=mock,
                    )
                )
                results.append(result)
                writer.write_result(result)
            except Exception as e:
                errors.append((source_file.name, str(e)))

            progress.advance(task)

    # Write index
    if results:
        index_path = writer.write_index(results)
        console.print(f"\nIndex written to: {index_path}")

    # Display summary
    console.print()

    table = Table(title="Processing Summary")
    table.add_column("Status", style="bold")
    table.add_column("Count", justify="right")

    witnessed = sum(1 for r in results if r.get("decision") == "WITNESSED")
    valhalla = sum(1 for r in results if r.get("decision") == "VALHALLA")
    forced = sum(1 for r in results if r.get("decision") == "FORCED")
    chrome = sum(1 for r in results if r.get("decision") == "CHROME")

    table.add_row("[green]Witnessed[/green]", str(witnessed))
    table.add_row("[gold1]Valhalla[/gold1]", str(valhalla))
    table.add_row("[orange1]Forced[/orange1]", str(forced))
    table.add_row("[yellow]Chrome (incomplete)[/yellow]", str(chrome))
    table.add_row("[red]Errors[/red]", str(len(errors)))

    console.print(table)

    if errors:
        console.print("\n[red]Errors:[/red]")
        for name, error in errors:
            console.print(f"  {name}: {error}")


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
        cfg.system.output_directory = output

    output_dir = cfg.system.output_directory

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
        cfg.system.output_directory = output

    console.print(Panel.fit(
        f"[bold blue]War Rig[/bold blue] - Initializing: {cfg.system.output_directory}",
        border_style="blue",
    ))

    # Create writer (which creates directories)
    writer = DocumentationWriter(cfg.system)

    console.print("[green]Output directories initialized successfully[/green]")
    console.print(f"\nOutput directory: {cfg.system.output_directory}")


@app.command()
def version() -> None:
    """Show War Rig version information."""
    from war_rig import __version__

    console.print(f"[bold blue]War Rig[/bold blue] version {__version__}")


def main() -> None:
    """Main entry point for the CLI."""
    app()


if __name__ == "__main__":
    main()
