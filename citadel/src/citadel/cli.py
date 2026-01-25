"""
Command-line interface for Citadel.

Usage:
    citadel analyze <source> [options]
    citadel spec list
    citadel spec show <spec_id>
    citadel spec generate <samples>... [--hints <hints>]
    citadel export <graph> --format <fmt> --output <path>
    citadel stats <graph>
    citadel cache clear
    citadel cache info

Commands:
    analyze     Analyze source directory and produce dependency graph
    spec        Manage artifact specifications
    export      Export graph to different format
    stats       Show statistics for a graph
    cache       Manage Citadel cache

Options:
    -o, --output <path>       Output path [default: citadel-graph.json]
    -f, --format <fmt>        Output format: json, dot, cypher, csv, markdown
    -p, --parallel <n>        Parallelism for file processing
    -v, --verbose             Verbose output
    --no-llm                  Disable LLM features
    --stats                   Print statistics after analysis
"""

from __future__ import annotations

import asyncio
import json
import logging
import sys
from pathlib import Path
from typing import TYPE_CHECKING

import click
from rich.console import Console
from rich.logging import RichHandler
from rich.panel import Panel
from rich.progress import Progress, SpinnerColumn, TextColumn, TimeElapsedColumn
from rich.table import Table
from rich.text import Text

from citadel.config import CitadelConfig, get_specs_cache_dir, load_config
from citadel.graph.exporters import GraphExporter
from citadel.graph.model import DependencyGraph
from citadel.orchestrator import Orchestrator
from citadel.specs.manager import SpecManager

if TYPE_CHECKING:
    from citadel.specs.schema import ArtifactSpec

# Initialize rich console
console = Console()


def setup_logging(verbose: bool) -> None:
    """Configure logging with rich handler."""
    level = logging.DEBUG if verbose else logging.INFO
    logging.basicConfig(
        level=level,
        format="%(message)s",
        datefmt="[%X]",
        handlers=[RichHandler(console=console, rich_tracebacks=True)],
    )


def get_spec_manager(config: CitadelConfig | None = None) -> SpecManager:
    """Get a configured SpecManager instance."""
    if config is None:
        config = load_config()

    builtin_dir = config.builtin_specs_dir
    cache_dir = get_specs_cache_dir(config)

    return SpecManager(builtin_dir=builtin_dir, cache_dir=cache_dir)


def load_graph_from_file(graph_path: Path) -> DependencyGraph:
    """
    Load a dependency graph from a JSON file.

    Args:
        graph_path: Path to the JSON graph file.

    Returns:
        Parsed DependencyGraph instance.

    Raises:
        click.ClickException: If the file cannot be read or parsed.
    """
    try:
        with open(graph_path, encoding="utf-8") as f:
            data = json.load(f)
        return DependencyGraph.model_validate(data)
    except FileNotFoundError:
        raise click.ClickException(f"Graph file not found: {graph_path}")
    except json.JSONDecodeError as e:
        raise click.ClickException(f"Invalid JSON in graph file: {e}")
    except Exception as e:
        raise click.ClickException(f"Failed to load graph: {e}")


@click.group()
@click.version_option(package_name="citadel")
@click.option("-v", "--verbose", is_flag=True, help="Enable verbose output")
@click.pass_context
def cli(ctx: click.Context, verbose: bool) -> None:
    """Citadel - Universal Dependency Graph Builder."""
    ctx.ensure_object(dict)
    ctx.obj["verbose"] = verbose
    setup_logging(verbose)


@cli.command()
@click.argument("source", type=click.Path(exists=True, path_type=Path))
@click.option(
    "-o", "--output",
    default="citadel-graph.json",
    type=click.Path(path_type=Path),
    help="Output path for the dependency graph"
)
@click.option(
    "-f", "--format", "fmt",
    default="json",
    type=click.Choice(["json", "dot", "cypher", "csv", "markdown", "mermaid"]),
    help="Output format"
)
@click.option(
    "-p", "--parallel",
    default=None,
    type=int,
    help="Parallelism level (overrides config)"
)
@click.option("-v", "--verbose", is_flag=True, help="Verbose output")
@click.option("--no-llm", is_flag=True, help="Disable LLM features")
@click.option("--stats", is_flag=True, help="Print statistics after analysis")
@click.pass_context
def analyze(
    ctx: click.Context,
    source: Path,
    output: Path,
    fmt: str,
    parallel: int | None,
    verbose: bool,
    no_llm: bool,
    stats: bool,
) -> None:
    """Analyze source directory and produce dependency graph."""
    # Merge verbose flag from group and command
    verbose = verbose or ctx.obj.get("verbose", False)
    setup_logging(verbose)

    logger = logging.getLogger(__name__)

    # Load configuration
    config = load_config()

    # Override config with CLI options
    if parallel is not None:
        config.parallel_files = parallel
    if no_llm:
        config.llm_disambiguation = False

    # Resolve source path
    source = source.resolve()

    console.print(Panel.fit(
        f"[bold blue]Citadel Analysis[/bold blue]\n"
        f"Source: [cyan]{source}[/cyan]\n"
        f"Output: [cyan]{output}[/cyan] ([yellow]{fmt}[/yellow])",
        border_style="blue"
    ))

    try:
        # Run analysis with progress indicator
        with Progress(
            SpinnerColumn(),
            TextColumn("[progress.description]{task.description}"),
            TimeElapsedColumn(),
            console=console,
        ) as progress:
            task = progress.add_task("Analyzing source files...", total=None)

            # Create orchestrator and run analysis
            orchestrator = Orchestrator(config)

            # Run the async analysis
            graph_dict = asyncio.run(
                orchestrator.analyze(
                    source_root=source,
                    output_path=output if fmt == "json" else None,
                    output_format=fmt,
                )
            )

            progress.update(task, description="Analysis complete")

        # Convert dict to DependencyGraph model if needed
        if isinstance(graph_dict, dict):
            # The orchestrator currently returns a dict stub
            # Create a minimal graph for demonstration
            from datetime import datetime
            from citadel.graph.model import GraphStatistics

            graph = DependencyGraph(
                version=graph_dict.get("version", "1.0"),
                source_root=str(source),
                generated_at=datetime.utcnow(),
                artifacts={},
                relationships=[],
                unresolved=[],
                statistics=GraphStatistics(
                    files_analyzed=graph_dict.get("statistics", {}).get("files_analyzed", 0),
                    files_skipped=0,
                    files_failed=0,
                    artifacts_total=graph_dict.get("statistics", {}).get("artifacts_total", 0),
                    relationships_total=graph_dict.get("statistics", {}).get("relationships_total", 0),
                    unresolved_count=0,
                    resolution_rate=1.0,
                ),
                config_hash="",
            )
        else:
            graph = graph_dict

        # Export the graph
        exporter = GraphExporter()

        if fmt == "json":
            exporter.export_json(graph, output)
        elif fmt == "dot":
            exporter.export_dot(graph, output)
        elif fmt == "cypher":
            exporter.export_cypher(graph, output)
        elif fmt == "csv":
            # CSV exports to a directory
            csv_dir = output.parent / output.stem if output.suffix else output
            exporter.export_csv(graph, csv_dir)
            output = csv_dir
        elif fmt == "markdown":
            exporter.export_markdown(graph, output)
        elif fmt == "mermaid":
            exporter.export_mermaid(graph, output)

        console.print(f"\n[green]Graph exported to:[/green] {output}")

        # Print statistics if requested
        if stats:
            _print_graph_statistics(graph)

    except FileNotFoundError as e:
        console.print(f"[red]Error:[/red] {e}")
        raise SystemExit(1)
    except ValueError as e:
        console.print(f"[red]Configuration error:[/red] {e}")
        raise SystemExit(1)
    except Exception as e:
        logger.exception("Analysis failed")
        console.print(f"[red]Analysis failed:[/red] {e}")
        raise SystemExit(1)


@cli.group()
def spec() -> None:
    """Manage artifact specifications."""
    pass


@spec.command("list")
@click.pass_context
def spec_list(ctx: click.Context) -> None:
    """List available specs."""
    verbose = ctx.obj.get("verbose", False)
    setup_logging(verbose)

    try:
        manager = get_spec_manager()
        spec_ids = manager.list_available_specs()

        if not spec_ids:
            console.print("[yellow]No specs found.[/yellow]")
            console.print(
                "Check that builtin specs are installed in the configured directory."
            )
            return

        # Create a rich table
        table = Table(title="Available Artifact Specifications")
        table.add_column("Spec ID", style="cyan", no_wrap=True)
        table.add_column("Language", style="green")
        table.add_column("Extensions", style="yellow")
        table.add_column("Source", style="blue")
        table.add_column("Category", style="magenta")

        for spec_id in spec_ids:
            spec = manager.get_spec(spec_id)
            if spec:
                extensions = ", ".join(spec.file_extensions[:3])
                if len(spec.file_extensions) > 3:
                    extensions += f" (+{len(spec.file_extensions) - 3})"

                source = "builtin" if manager.is_builtin(spec_id) else "cached"

                table.add_row(
                    spec_id,
                    spec.language,
                    extensions,
                    source,
                    spec.category.value,
                )

        console.print(table)
        console.print(f"\n[dim]Total: {len(spec_ids)} specs[/dim]")

        # Show handled extensions
        extensions = manager.get_extensions_handled()
        if extensions:
            ext_display = ", ".join(extensions[:10])
            if len(extensions) > 10:
                ext_display += f" ... and {len(extensions) - 10} more"
            console.print(f"[dim]File extensions handled: {ext_display}[/dim]")

    except Exception as e:
        console.print(f"[red]Error listing specs:[/red] {e}")
        raise SystemExit(1)


@spec.command("show")
@click.argument("spec_id")
@click.option("--yaml", "as_yaml", is_flag=True, help="Output raw YAML")
@click.pass_context
def spec_show(ctx: click.Context, spec_id: str, as_yaml: bool) -> None:
    """Show details of a specific spec."""
    verbose = ctx.obj.get("verbose", False)
    setup_logging(verbose)

    try:
        manager = get_spec_manager()
        spec = manager.get_spec(spec_id)

        if spec is None:
            console.print(f"[red]Spec not found:[/red] {spec_id}")

            # Suggest similar specs
            available = manager.list_available_specs()
            if available:
                suggestions = [s for s in available if spec_id.lower() in s.lower()]
                if suggestions:
                    console.print(f"\n[yellow]Did you mean:[/yellow] {', '.join(suggestions)}")

            raise SystemExit(1)

        if as_yaml:
            # Output raw YAML
            import yaml
            spec_dict = spec.model_dump(mode="json", exclude_none=True)
            console.print(yaml.safe_dump(spec_dict, default_flow_style=False, sort_keys=False))
            return

        # Rich formatted output
        _display_spec_details(spec, spec_id, manager)

    except SystemExit:
        raise
    except Exception as e:
        console.print(f"[red]Error showing spec:[/red] {e}")
        raise SystemExit(1)


def _display_spec_details(spec: "ArtifactSpec", spec_id: str, manager: SpecManager) -> None:
    """Display formatted spec details."""
    # Header
    console.print(Panel.fit(
        f"[bold blue]{spec.language}[/bold blue] ({spec_id})\n"
        f"[dim]{spec.description}[/dim]",
        title="Artifact Specification",
        border_style="blue"
    ))

    # Basic info table
    info_table = Table(show_header=False, box=None, padding=(0, 2))
    info_table.add_column("Property", style="bold cyan")
    info_table.add_column("Value")

    info_table.add_row("Spec Version", spec.spec_version)
    info_table.add_row("Category", spec.category.value)
    info_table.add_row("Primary Type", spec.primary_artifact_type.value)
    info_table.add_row("Source", "builtin" if manager.is_builtin(spec_id) else "cached")

    source_path = manager.get_spec_source(spec_id)
    if source_path:
        info_table.add_row("File", str(source_path))

    console.print(info_table)
    console.print()

    # File matching
    console.print("[bold]File Matching[/bold]")
    console.print(f"  Extensions: [yellow]{', '.join(spec.file_extensions)}[/yellow]")
    if spec.file_patterns:
        console.print(f"  Patterns: [yellow]{', '.join(spec.file_patterns)}[/yellow]")
    console.print()

    # Syntax info
    console.print("[bold]Syntax[/bold]")
    if spec.comments.line_prefix:
        console.print(f"  Line comments: [green]{spec.comments.line_prefix}[/green]")
    if spec.comments.block_start:
        console.print(
            f"  Block comments: [green]{spec.comments.block_start}...{spec.comments.block_end}[/green]"
        )
    if spec.comments.fixed_column is not None:
        console.print(
            f"  Fixed-column comments: column [green]{spec.comments.fixed_column}[/green], "
            f"indicator [green]{spec.comments.fixed_indicator}[/green]"
        )
    console.print()

    # Definition patterns
    if spec.definition_patterns:
        console.print(f"[bold]Definition Patterns[/bold] ({len(spec.definition_patterns)})")
        for pattern in spec.definition_patterns:
            artifact_type = pattern.artifact_type.value if pattern.artifact_type else "N/A"
            console.print(f"  - [cyan]{pattern.name}[/cyan] -> {artifact_type}")
        console.print()

    # Reference patterns
    if spec.reference_patterns:
        console.print(f"[bold]Reference Patterns[/bold] ({len(spec.reference_patterns)})")
        for pattern in spec.reference_patterns:
            rel_type = pattern.relationship_type.value if pattern.relationship_type else "N/A"
            console.print(f"  - [cyan]{pattern.name}[/cyan] -> {rel_type}")
        console.print()

    # Preprocessor patterns
    if spec.preprocessor_patterns:
        console.print(f"[bold]Preprocessor Patterns[/bold] ({len(spec.preprocessor_patterns)})")
        for pattern in spec.preprocessor_patterns:
            rel_type = pattern.relationship_type.value if pattern.relationship_type else "N/A"
            console.print(f"  - [cyan]{pattern.name}[/cyan] -> {rel_type}")
        console.print()

    # Naming conventions
    if spec.naming:
        console.print("[bold]Naming Convention[/bold]")
        console.print(f"  Case: [yellow]{spec.naming.case}[/yellow]")
        if spec.naming.max_length:
            console.print(f"  Max Length: [yellow]{spec.naming.max_length}[/yellow]")
        if spec.naming.common_abbreviations:
            console.print(
                f"  Abbreviations: [yellow]{len(spec.naming.common_abbreviations)} defined[/yellow]"
            )


@spec.command("generate")
@click.argument("samples", nargs=-1, type=click.Path(exists=True, path_type=Path))
@click.option("--hints", help="Hints about expected language")
@click.pass_context
def spec_generate(ctx: click.Context, samples: tuple[Path, ...], hints: str | None) -> None:
    """Generate spec from sample files."""
    verbose = ctx.obj.get("verbose", False)
    setup_logging(verbose)

    if not samples:
        console.print("[red]Error:[/red] At least one sample file is required.")
        raise SystemExit(1)

    console.print(f"[yellow]Generating spec from {len(samples)} sample file(s)...[/yellow]")

    if hints:
        console.print(f"[dim]Hints: {hints}[/dim]")

    # Show sample files
    for sample in samples:
        console.print(f"  - {sample}")

    # TODO: Implement LLM-based spec generation
    console.print(
        "\n[yellow]Note:[/yellow] LLM-based spec generation is not yet implemented.\n"
        "For now, you can create specs manually as YAML files."
    )


@cli.command()
@click.argument("graph", type=click.Path(exists=True, path_type=Path))
@click.option(
    "-f", "--format", "fmt",
    required=True,
    type=click.Choice(["json", "dot", "cypher", "csv", "markdown", "mermaid"]),
    help="Output format"
)
@click.option(
    "-o", "--output",
    required=True,
    type=click.Path(path_type=Path),
    help="Output path"
)
@click.option(
    "--cluster-by",
    default="category",
    type=click.Choice(["category", "language", "file"]),
    help="How to cluster nodes in DOT output"
)
@click.pass_context
def export(
    ctx: click.Context,
    graph: Path,
    fmt: str,
    output: Path,
    cluster_by: str,
) -> None:
    """Export graph to different format."""
    verbose = ctx.obj.get("verbose", False)
    setup_logging(verbose)

    console.print(f"Loading graph from [cyan]{graph}[/cyan]...")

    try:
        # Load the graph
        dep_graph = load_graph_from_file(graph)

        # Export
        exporter = GraphExporter()

        with Progress(
            SpinnerColumn(),
            TextColumn("[progress.description]{task.description}"),
            console=console,
        ) as progress:
            task = progress.add_task(f"Exporting to {fmt}...", total=None)

            if fmt == "json":
                exporter.export_json(dep_graph, output)
            elif fmt == "dot":
                exporter.export_dot(dep_graph, output, cluster_by=cluster_by)
            elif fmt == "cypher":
                exporter.export_cypher(dep_graph, output)
            elif fmt == "csv":
                # CSV exports to a directory
                csv_dir = output.parent / output.stem if output.suffix else output
                exporter.export_csv(dep_graph, csv_dir)
                output = csv_dir
            elif fmt == "markdown":
                exporter.export_markdown(dep_graph, output)
            elif fmt == "mermaid":
                exporter.export_mermaid(dep_graph, output)

            progress.update(task, description="Export complete")

        console.print(f"[green]Exported to:[/green] {output}")

        # Show summary
        console.print(
            f"[dim]({dep_graph.statistics.artifacts_total} artifacts, "
            f"{dep_graph.statistics.relationships_total} relationships)[/dim]"
        )

    except click.ClickException:
        raise
    except Exception as e:
        console.print(f"[red]Export failed:[/red] {e}")
        raise SystemExit(1)


@cli.group()
def cache() -> None:
    """Manage Citadel cache."""
    pass


@cache.command("clear")
@click.option("--yes", "-y", is_flag=True, help="Skip confirmation prompt")
@click.pass_context
def cache_clear(ctx: click.Context, yes: bool) -> None:
    """Clear all cached data (specs, parse results)."""
    verbose = ctx.obj.get("verbose", False)
    setup_logging(verbose)

    from citadel.sdk import Citadel

    citadel = Citadel()
    cache_dir = citadel.config.cache_dir

    if not cache_dir.exists():
        console.print("[yellow]Cache directory does not exist.[/yellow]")
        return

    if not yes:
        console.print(f"Cache directory: [cyan]{cache_dir}[/cyan]")
        if not click.confirm("Clear all cached data?"):
            console.print("[dim]Cancelled.[/dim]")
            return

    citadel.clear_cache()
    console.print("[green]Cache cleared successfully.[/green]")


@cache.command("info")
@click.pass_context
def cache_info(ctx: click.Context) -> None:
    """Show cache location and size."""
    verbose = ctx.obj.get("verbose", False)
    setup_logging(verbose)

    from citadel.sdk import Citadel

    citadel = Citadel()
    cache_dir = citadel.config.cache_dir

    console.print(f"Cache directory: [cyan]{cache_dir}[/cyan]")

    if not cache_dir.exists():
        console.print("[yellow]Cache directory does not exist.[/yellow]")
        return

    # Calculate cache size
    total_size = 0
    file_count = 0
    for path in cache_dir.rglob("*"):
        if path.is_file():
            total_size += path.stat().st_size
            file_count += 1

    # Format size
    if total_size < 1024:
        size_str = f"{total_size} B"
    elif total_size < 1024 * 1024:
        size_str = f"{total_size / 1024:.1f} KB"
    else:
        size_str = f"{total_size / (1024 * 1024):.1f} MB"

    console.print(f"Cache size: [green]{size_str}[/green] ({file_count} files)")


@cli.command()
@click.argument("graph", type=click.Path(exists=True, path_type=Path))
@click.option("--json", "as_json", is_flag=True, help="Output statistics as JSON")
@click.pass_context
def stats(ctx: click.Context, graph: Path, as_json: bool) -> None:
    """Show statistics for a graph."""
    verbose = ctx.obj.get("verbose", False)
    setup_logging(verbose)

    try:
        # Load the graph
        dep_graph = load_graph_from_file(graph)

        if as_json:
            # Output as JSON for scripting
            stats_dict = dep_graph.statistics.model_dump(mode="json")
            console.print_json(data=stats_dict)
        else:
            _print_graph_statistics(dep_graph)

    except click.ClickException:
        raise
    except Exception as e:
        console.print(f"[red]Failed to load graph:[/red] {e}")
        raise SystemExit(1)


def _print_graph_statistics(graph: DependencyGraph) -> None:
    """Print formatted graph statistics using rich."""
    stats = graph.statistics

    # Header panel
    console.print(Panel.fit(
        f"[bold]Source:[/bold] {graph.source_root}\n"
        f"[bold]Generated:[/bold] {graph.generated_at.isoformat()}",
        title="[bold blue]Dependency Graph Statistics[/bold blue]",
        border_style="blue"
    ))

    # File analysis summary
    console.print("\n[bold]File Analysis[/bold]")
    file_table = Table(show_header=False, box=None, padding=(0, 2))
    file_table.add_column("Metric", style="cyan")
    file_table.add_column("Count", justify="right")

    file_table.add_row("Files Analyzed", str(stats.files_analyzed))
    file_table.add_row("Files Skipped", str(stats.files_skipped))
    file_table.add_row("Files Failed", str(stats.files_failed))
    console.print(file_table)

    # Artifacts summary
    console.print("\n[bold]Artifacts[/bold]")
    if stats.artifacts_by_type:
        artifact_table = Table(show_header=True, header_style="bold")
        artifact_table.add_column("Type", style="cyan")
        artifact_table.add_column("Count", justify="right", style="green")

        for artifact_type, count in sorted(stats.artifacts_by_type.items()):
            artifact_table.add_row(artifact_type, str(count))

        artifact_table.add_row(
            "[bold]Total[/bold]",
            f"[bold]{stats.artifacts_total}[/bold]",
            style="bold"
        )
        console.print(artifact_table)
    else:
        console.print(f"  Total: [green]{stats.artifacts_total}[/green]")

    # Relationships summary
    console.print("\n[bold]Relationships[/bold]")
    if stats.relationships_by_type:
        rel_table = Table(show_header=True, header_style="bold")
        rel_table.add_column("Type", style="cyan")
        rel_table.add_column("Count", justify="right", style="green")

        for rel_type, count in sorted(stats.relationships_by_type.items()):
            rel_table.add_row(rel_type, str(count))

        rel_table.add_row(
            "[bold]Total[/bold]",
            f"[bold]{stats.relationships_total}[/bold]",
            style="bold"
        )
        console.print(rel_table)
    else:
        console.print(f"  Total: [green]{stats.relationships_total}[/green]")

    # Resolution metrics
    console.print("\n[bold]Resolution[/bold]")
    resolution_color = "green" if stats.resolution_rate >= 0.9 else (
        "yellow" if stats.resolution_rate >= 0.7 else "red"
    )

    resolution_table = Table(show_header=False, box=None, padding=(0, 2))
    resolution_table.add_column("Metric", style="cyan")
    resolution_table.add_column("Value", justify="right")

    resolution_table.add_row("Resolved", str(stats.relationships_total))
    resolution_table.add_row("Unresolved", str(stats.unresolved_count))
    resolution_table.add_row(
        "Resolution Rate",
        Text(f"{stats.resolution_rate:.1%}", style=resolution_color)
    )
    console.print(resolution_table)

    # Languages detected
    if stats.languages_detected:
        console.print("\n[bold]Languages Detected[/bold]")
        lang_table = Table(show_header=True, header_style="bold")
        lang_table.add_column("Language", style="cyan")
        lang_table.add_column("Spec Used", style="yellow")

        for lang in stats.languages_detected:
            spec_id = stats.specs_used.get(lang, "unknown")
            lang_table.add_row(lang, spec_id)

        console.print(lang_table)


if __name__ == "__main__":
    cli()
