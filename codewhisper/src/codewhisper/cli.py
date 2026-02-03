"""CodeWhisper CLI - Interactive chatbot for mainframe code exploration.

This module provides the command-line interface for CodeWhisper, including
an interactive REPL mode for conversational code exploration.

Example:
    # Start interactive mode
    $ codewhisper --skills-dir ./skills --code-dir ./src

    # Single query mode
    $ codewhisper --query "What does CBPAUP0C do?"
"""

from __future__ import annotations

import asyncio
import logging
from pathlib import Path
from typing import TYPE_CHECKING, Annotated

import typer
from rich.console import Console
from rich.markdown import Markdown
from rich.panel import Panel
from rich.prompt import Prompt

from codewhisper.config import AgentConfig


def _load_dotenv() -> None:
    """Load .env file from current directory or parent directories."""
    try:
        from dotenv import load_dotenv

        # Try current directory first, then walk up to find .env
        cwd = Path.cwd()
        for parent in [cwd, *cwd.parents]:
            env_file = parent / ".env"
            if env_file.exists():
                load_dotenv(env_file)
                return
        # Fallback to default behavior (looks in cwd)
        load_dotenv()
    except ImportError:
        pass  # python-dotenv not installed, skip


# Load .env on module import
_load_dotenv()

if TYPE_CHECKING:
    from codewhisper.skills.index import SkillsIndex

def version_callback(value: bool) -> None:
    """Show version and exit."""
    if value:
        from codewhisper import __version__
        console.print(f"codewhisper {__version__}")
        raise typer.Exit()


# CLI application
app = typer.Typer(
    name="codewhisper",
    help="Interactive chatbot for exploring mainframe codebases",
    add_completion=False,
)


def cli() -> None:
    """Entry point for CLI."""
    app()

# Rich console for formatted output
console = Console()

# Logger
logger = logging.getLogger(__name__)


def setup_logging(verbose: bool) -> None:
    """Configure logging based on verbosity.

    Args:
        verbose: If True, set DEBUG level; otherwise INFO.
    """
    level = logging.DEBUG if verbose else logging.INFO
    logging.basicConfig(
        level=level,
        format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
    )


async def run_interactive(config: AgentConfig) -> None:
    """Run the interactive REPL loop.

    Args:
        config: Agent configuration.
    """
    from codewhisper.agent.graph import create_agent

    console.print(
        Panel.fit(
            "[bold blue]CodeWhisper[/bold blue]\n"
            "Interactive mainframe code exploration chatbot\n\n"
            f"Skills: {config.skills_dir}\n"
            f"Code: {config.code_dir}\n"
            f"Model: {config.model}\n\n"
            "Type [bold]/help[/bold] for commands, [bold]/quit[/bold] to exit.",
            title="Welcome",
        )
    )

    # Initialize the agent
    try:
        with console.status("[bold]Loading skills and initializing agent...[/bold]"):
            agent = create_agent(config)
        console.print(f"[dim]Loaded {len(agent.skills_index)} skills[/dim]")
    except Exception as e:
        console.print(f"[red]Failed to initialize agent:[/red] {e}")
        return

    while True:
        try:
            # Get user input
            user_input = Prompt.ask("\n[bold green]You[/bold green]")

            # Handle special commands
            cmd = user_input.strip().lower()

            if cmd in ("/quit", "/exit", "/q"):
                console.print("[dim]Goodbye![/dim]")
                break

            if cmd == "/help":
                _show_help()
                continue

            if cmd == "/clear":
                console.clear()
                continue

            if cmd == "/reset":
                await agent.reset()
                console.print("[dim]Conversation reset[/dim]")
                continue

            if cmd == "/skills":
                _list_skills(agent.skills_index)
                continue

            if cmd.startswith("/load "):
                skill_name = user_input.strip()[6:].strip()
                _load_skill_interactive(agent.skills_index, skill_name)
                continue

            if cmd.startswith("/search "):
                query = user_input.strip()[8:].strip()
                _search_skills_interactive(agent.skills_index, query)
                continue

            if not user_input.strip():
                continue

            # Send to agent and get response
            with console.status("[bold]Thinking...[/bold]"):
                try:
                    response = await agent.chat(user_input)
                except Exception as e:
                    logger.exception("Error during chat")
                    console.print(f"[red]Error:[/red] {e}")
                    continue

            console.print("\n[bold blue]CodeWhisper[/bold blue]")
            console.print(Markdown(response))

        except KeyboardInterrupt:
            console.print("\n[dim]Use /quit to exit[/dim]")
            continue
        except EOFError:
            console.print("[dim]Goodbye![/dim]")
            break


def _show_help() -> None:
    """Display help information."""
    help_text = """
## Commands

- `/help` - Show this help message
- `/clear` - Clear the screen
- `/quit` or `/exit` - Exit the chatbot
- `/reset` - Reset conversation history
- `/skills` - List all available skills
- `/load <skill>` - Show a specific skill's content
- `/search <query>` - Search skills by keyword

## Tips

- Ask about specific programs: "What does CBPAUP0C do?"
- Search for patterns: "Find all programs that call MQGET"
- Understand relationships: "How does COPAUS0C interact with the database?"
- Get overviews: "Explain the authorization subsystem"
- Ask follow-up questions: The agent remembers conversation context
"""
    console.print(Panel(Markdown(help_text), title="Help"))


def _list_skills(skills_index: SkillsIndex) -> None:
    """List all available skills.

    Args:
        skills_index: The skills index to list from.
    """
    from rich.table import Table


    skills = skills_index.get_all()

    if not skills:
        console.print("[dim]No skills loaded[/dim]")
        return

    table = Table(title=f"Available Skills ({len(skills)})")
    table.add_column("Name", style="cyan")
    table.add_column("Description", style="dim")

    for skill in sorted(skills, key=lambda s: s.name):
        # Truncate description
        desc = skill.description[:80] + "..." if len(skill.description) > 80 else skill.description
        table.add_row(skill.name, desc)

    console.print(table)


def _load_skill_interactive(skills_index: SkillsIndex, skill_name: str) -> None:
    """Load and display a skill's content.

    Args:
        skills_index: The skills index.
        skill_name: Name of the skill to load.
    """

    skill = skills_index.get(skill_name)

    if skill is None:
        console.print(f"[red]Skill not found:[/red] {skill_name}")
        # Suggest similar skills
        suggestions = [
            s.name for s in skills_index.get_all()
            if skill_name.lower() in s.name.lower()
        ][:5]
        if suggestions:
            console.print(f"[dim]Did you mean: {', '.join(suggestions)}?[/dim]")
        return

    console.print(Panel(
        Markdown(f"**{skill.name}**\n\n{skill.description}\n\n---\n\n{skill.content[:2000]}..."),
        title=f"Skill: {skill.name}",
    ))


def _search_skills_interactive(skills_index: SkillsIndex, query: str) -> None:
    """Search skills by keyword and display results.

    Args:
        skills_index: The skills index.
        query: Search query.
    """
    from rich.table import Table


    results = skills_index.search(query, limit=10)

    if not results:
        console.print(f"[dim]No skills found matching '{query}'[/dim]")
        return

    table = Table(title=f"Search Results for '{query}'")
    table.add_column("Score", style="yellow", justify="right")
    table.add_column("Name", style="cyan")
    table.add_column("Description", style="dim")

    for result in results:
        desc = result.skill.description[:60] + "..." if len(result.skill.description) > 60 else result.skill.description
        table.add_row(f"{result.score:.1f}", result.skill.name, desc)

    console.print(table)


async def run_single_query(config: AgentConfig, query: str) -> None:
    """Run a single query and exit.

    Args:
        config: Agent configuration.
        query: The query to process.
    """
    from codewhisper.agent.graph import create_agent

    # Initialize the agent
    try:
        with console.status("[bold]Loading skills and initializing agent...[/bold]"):
            agent = create_agent(config)
    except Exception as e:
        console.print(f"[red]Failed to initialize agent:[/red] {e}")
        return

    console.print(f"[bold]Query:[/bold] {query}\n")

    # Run the query
    try:
        with console.status("[bold]Thinking...[/bold]"):
            response = await agent.chat(query)
    except Exception as e:
        logger.exception("Error during chat")
        console.print(f"[red]Error:[/red] {e}")
        return

    console.print("[bold blue]CodeWhisper[/bold blue]")
    console.print(Markdown(response))


@app.command()
def main(
    skills_dir: Annotated[
        Path | None,
        typer.Option(
            "--skills-dir",
            "-s",
            help="Path to the skills directory containing SKILL.md files (required)",
            envvar="CODEWHISPER_SKILLS_DIR",
        ),
    ] = None,
    code_dir: Annotated[
        Path | None,
        typer.Option(
            "--code-dir",
            "-c",
            help="Path to the source code directory to explore",
            envvar="CODEWHISPER_CODE_DIR",
        ),
    ] = None,
    model: Annotated[
        str | None,
        typer.Option(
            "--model",
            "-m",
            help="LLM model to use (e.g., anthropic/claude-sonnet-4-20250514)",
        ),
    ] = None,
    provider: Annotated[
        str | None,
        typer.Option(
            "--provider",
            "-p",
            help="LLM provider (openrouter, anthropic, openai)",
        ),
    ] = None,
    query: Annotated[
        str | None,
        typer.Option(
            "--query",
            "-q",
            help="Single query to run (non-interactive mode)",
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
    version: Annotated[
        bool,
        typer.Option(
            "--version",
            "-V",
            help="Show version and exit",
            callback=version_callback,
            is_eager=True,
        ),
    ] = False,
) -> None:
    """CodeWhisper - Interactive chatbot for mainframe code exploration.

    Start an interactive session to ask questions about your codebase,
    or run a single query with the --query option.

    Examples:

        # Interactive mode
        codewhisper --skills-dir ./skills --code-dir ./src

        # Single query
        codewhisper -s ./skills -c ./src -q "What does CBPAUP0C do?"
    """
    setup_logging(verbose)

    # Require skills_dir
    if skills_dir is None:
        console.print("[red]Error:[/red] --skills-dir is required")
        console.print("[dim]Specify the path to the skills directory containing SKILL.md files[/dim]")
        raise typer.Exit(1)

    # Build configuration from CLI args
    config_kwargs: dict[str, object] = {
        "skills_dir": skills_dir,
        "verbose": verbose,
    }
    if code_dir is not None:
        config_kwargs["code_dir"] = code_dir
    if model:
        config_kwargs["model"] = model
    if provider:
        config_kwargs["provider"] = provider

    try:
        config = AgentConfig(**config_kwargs)
    except Exception as e:
        console.print(f"[red]Configuration error:[/red] {e}")
        raise typer.Exit(1) from None

    # Validate paths exist
    if not config.skills_dir.exists():
        console.print(f"[red]Skills directory not found:[/red] {config.skills_dir}")
        raise typer.Exit(1)

    if not config.code_dir.exists():
        console.print(f"[red]Code directory not found:[/red] {config.code_dir}")
        raise typer.Exit(1)

    # Run in appropriate mode
    if query:
        asyncio.run(run_single_query(config, query))
    else:
        asyncio.run(run_interactive(config))


if __name__ == "__main__":
    app()
