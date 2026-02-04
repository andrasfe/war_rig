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
import re
import time
from datetime import datetime
from pathlib import Path
from typing import TYPE_CHECKING, Annotated

import typer
from dotenv import load_dotenv
from rich.console import Console
from rich.markdown import Markdown
from rich.panel import Panel
from rich.prompt import Prompt
from rich.table import Table

# Load .env file from current directory or parent directories
load_dotenv()

from codewhisper.config import AgentConfig

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


class ConversationTracker:
    """Track conversation state for display purposes."""

    def __init__(self, max_history: int = 20):
        """Initialize the conversation tracker.

        Args:
            max_history: Maximum turns to track.
        """
        self.turn_count: int = 0
        self.max_history: int = max_history
        self.summaries: list[str] = []

    def increment(self) -> None:
        """Record a new conversation turn."""
        self.turn_count += 1

    def add_summary(self, user_input: str) -> None:
        """Add a summary of the user's question.

        Args:
            user_input: The user's input text.
        """
        # Create a brief summary (first 50 chars)
        summary = user_input[:50].strip()
        if len(user_input) > 50:
            summary += "..."
        self.summaries.append(summary)
        # Keep only recent summaries
        if len(self.summaries) > self.max_history:
            self.summaries = self.summaries[-self.max_history :]

    def reset(self) -> None:
        """Reset the conversation tracker."""
        self.turn_count = 0
        self.summaries = []

    def get_status(self) -> str:
        """Get a status string for the prompt.

        Returns:
            Status string like "Turn 3/20".
        """
        return f"Turn {self.turn_count}/{self.max_history}"

    def get_summary(self) -> str:
        """Get a summary of the conversation.

        Returns:
            Formatted summary of recent turns.
        """
        if not self.summaries:
            return "No conversation history yet."

        lines = [f"**Conversation Summary** ({self.turn_count} turns)\n"]
        for i, summary in enumerate(self.summaries, 1):
            lines.append(f"{i}. {summary}")
        return "\n".join(lines)


def _extract_mermaid_diagrams(text: str) -> list[tuple[str, str]]:
    """Extract Mermaid diagrams from text.

    Args:
        text: The text containing potential Mermaid diagrams.

    Returns:
        List of tuples (diagram_type, diagram_content).
    """
    pattern = r"```mermaid\s*\n(.*?)```"
    matches = re.findall(pattern, text, re.DOTALL)
    diagrams = []
    for match in matches:
        # Detect diagram type from first line
        first_line = match.strip().split("\n")[0].strip().lower()
        if first_line.startswith("flowchart"):
            dtype = "flowchart"
        elif first_line.startswith("sequencediagram") or first_line.startswith(
            "sequence"
        ):
            dtype = "sequence"
        elif first_line.startswith("graph"):
            dtype = "graph"
        else:
            dtype = "diagram"
        diagrams.append((dtype, match.strip()))
    return diagrams


def _save_mermaid_diagram(
    diagram: str, output_dir: Path, prefix: str = "diagram"
) -> Path:
    """Save a Mermaid diagram to a file.

    Args:
        diagram: The Mermaid diagram content.
        output_dir: Directory to save to.
        prefix: Filename prefix.

    Returns:
        Path to the saved file.
    """
    output_dir.mkdir(parents=True, exist_ok=True)
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    filename = f"{prefix}_{timestamp}.mmd"
    filepath = output_dir / filename
    filepath.write_text(diagram)
    return filepath


def _format_response_with_diagrams(response: str, code_dir: Path) -> str:
    """Format response and handle Mermaid diagrams.

    Args:
        response: The raw response text.
        code_dir: Code directory for saving diagrams.

    Returns:
        Formatted response with diagram file references.
    """
    diagrams = _extract_mermaid_diagrams(response)
    if not diagrams:
        return response

    # Save diagrams and create references
    diagram_dir = code_dir / ".codewhisper" / "diagrams"
    saved_paths = []

    for i, (dtype, content) in enumerate(diagrams):
        prefix = f"{dtype}_{i + 1}" if len(diagrams) > 1 else dtype
        filepath = _save_mermaid_diagram(content, diagram_dir, prefix)
        saved_paths.append(filepath)

    # Add note about saved diagrams
    if saved_paths:
        note = "\n\n---\n**Diagrams saved:**\n"
        for path in saved_paths:
            note += f"- `{path}`\n"
        note += (
            "\nTip: Use a Mermaid viewer or VS Code extension to render these diagrams."
        )
        return response + note

    return response


async def _run_citadel_analyze(code_dir: Path, file_path: str) -> str:
    """Run citadel analyze on a file.

    Args:
        code_dir: Base code directory.
        file_path: Path to the file to analyze.

    Returns:
        Formatted analysis result.
    """
    try:
        from citadel.sdk import Citadel  # type: ignore[import-untyped]

        citadel = Citadel()

        full_path = (code_dir / file_path).resolve()
        if not full_path.exists():
            return f"[red]File not found:[/red] {file_path}"

        result = citadel.analyze_file(full_path)

        if result.error:
            return f"[red]Analysis error:[/red] {result.error}"

        lines = [
            f"**File:** {result.file_path}",
            f"**Language:** {result.language}",
            f"**Artifacts:** {len(result.artifacts)}",
            "",
        ]

        for artifact in result.artifacts:
            callout_summary = ""
            if artifact.callouts:
                call_types: dict[str, int] = {}
                for c in artifact.callouts:
                    call_types[c.relationship] = call_types.get(c.relationship, 0) + 1
                callout_summary = " | " + ", ".join(
                    f"{k}: {v}" for k, v in sorted(call_types.items())
                )
            lines.append(
                f"- **{artifact.name}** ({artifact.type})"
                f" lines {artifact.line_start}-{artifact.line_end or '?'}{callout_summary}"
            )

        return "\n".join(lines)

    except ImportError:
        return "[red]Error:[/red] Citadel is not installed. Run: pip install citadel"
    except Exception as e:
        logger.exception("Citadel analyze error")
        return f"[red]Error:[/red] {e}"


async def _run_citadel_functions(code_dir: Path, file_path: str) -> str:
    """List functions in a file using citadel.

    Args:
        code_dir: Base code directory.
        file_path: Path to the file.

    Returns:
        Formatted function list.
    """
    try:
        from citadel.sdk import Citadel  # type: ignore[import-untyped]

        citadel = Citadel()

        full_path = (code_dir / file_path).resolve()
        if not full_path.exists():
            return f"[red]File not found:[/red] {file_path}"

        functions = citadel.get_functions(full_path)

        if not functions:
            return f"No functions found in {file_path}"

        if "error" in functions[0]:
            return f"[red]Error:[/red] {functions[0]['error']}"

        table = Table(title=f"Functions in {file_path}")
        table.add_column("Name", style="cyan")
        table.add_column("Type", style="dim")
        table.add_column("Lines", justify="right")
        table.add_column("Calls", justify="right")

        for func in functions:
            line_range = f"{func.get('line', '?')}-{func.get('line_end', '?')}"
            call_count = len(func.get("calls", []))
            table.add_row(
                func["name"],
                func.get("type", "?"),
                line_range,
                str(call_count),
            )

        # Return as markdown since we can't return rich objects directly
        lines = [f"**Functions in {file_path}**\n"]
        for func in functions:
            line_range = f"{func.get('line', '?')}-{func.get('line_end', '?')}"
            calls = func.get("calls", [])
            lines.append(
                f"- **{func['name']}** ({func.get('type', '?')}) "
                f"lines {line_range}, {len(calls)} calls"
            )
            if calls:
                for call in calls[:5]:
                    lines.append(f"  - {call['type']}: {call['target']}")
                if len(calls) > 5:
                    lines.append(f"  - ... and {len(calls) - 5} more")

        return "\n".join(lines)

    except ImportError:
        return "[red]Error:[/red] Citadel is not installed. Run: pip install citadel"
    except Exception as e:
        logger.exception("Citadel functions error")
        return f"[red]Error:[/red] {e}"


async def _run_citadel_callers(
    code_dir: Path, file_path: str, function_name: str
) -> str:
    """Find callers of a function using citadel.

    Args:
        code_dir: Base code directory.
        file_path: Path to the file containing the function.
        function_name: Name of the function to find callers for.

    Returns:
        Formatted callers list.
    """
    try:
        from citadel.sdk import Citadel  # type: ignore[import-untyped]

        citadel = Citadel()

        full_path = (code_dir / file_path).resolve()
        if not full_path.exists():
            return f"[red]File not found:[/red] {file_path}"

        callers = citadel.get_callers(full_path, function_name, [code_dir])

        if not callers:
            return f"No callers found for `{function_name}` in codebase"

        lines = [f"**Callers of {function_name}**\n"]
        for caller in callers:
            rel_path = Path(caller["file"]).relative_to(code_dir.resolve())
            func = caller.get("function") or "(file-level)"
            line = caller.get("line") or "?"
            call_type = caller.get("type", "references")
            lines.append(f"- `{rel_path}:{line}` - **{func}** ({call_type})")

        return "\n".join(lines)

    except ImportError:
        return "[red]Error:[/red] Citadel is not installed. Run: pip install citadel"
    except Exception as e:
        logger.exception("Citadel callers error")
        return f"[red]Error:[/red] {e}"


async def _run_citadel_flow(code_dir: Path, file_path: str) -> str:
    """Generate a flow diagram for a file using citadel.

    Args:
        code_dir: Base code directory.
        file_path: Path to the file.

    Returns:
        Mermaid diagram or path to saved file.
    """
    try:
        from citadel.sdk import Citadel  # type: ignore[import-untyped]

        citadel = Citadel()

        full_path = (code_dir / file_path).resolve()
        if not full_path.exists():
            return f"[red]File not found:[/red] {file_path}"

        diagram = citadel.get_flow_diagram(full_path)

        # Save diagram to file
        diagram_dir = code_dir / ".codewhisper" / "diagrams"
        filepath = _save_mermaid_diagram(
            diagram, diagram_dir, f"flow_{Path(file_path).stem}"
        )

        lines = [
            f"**Flow diagram generated for {file_path}**",
            "",
            f"Saved to: `{filepath}`",
            "",
            "```mermaid",
            diagram,
            "```",
        ]
        return "\n".join(lines)

    except ImportError:
        return "[red]Error:[/red] Citadel is not installed. Run: pip install citadel"
    except Exception as e:
        logger.exception("Citadel flow error")
        return f"[red]Error:[/red] {e}"


async def _run_citadel_dead_code(code_dir: Path) -> str:
    """Find dead code in the codebase using citadel.

    Args:
        code_dir: Code directory to analyze.

    Returns:
        Formatted dead code report.
    """
    try:
        from citadel.sdk import Citadel  # type: ignore[import-untyped]

        citadel = Citadel()

        dead_items = citadel.get_dead_code(code_dir)

        if not dead_items:
            return "No dead code found in codebase."

        lines = [f"**Dead Code Analysis** ({len(dead_items)} items)\n"]

        # Group by type
        by_type: dict[str, list] = {}
        for item in dead_items:
            art_type = item.get("type", "unknown")
            by_type.setdefault(art_type, []).append(item)

        for art_type, items in sorted(by_type.items()):
            lines.append(f"\n### {art_type.title()} ({len(items)})")
            for item in items[:10]:  # Limit display
                file_path = item.get("file", "?")
                if file_path and file_path != "?":
                    try:
                        rel_path = Path(file_path).relative_to(code_dir.resolve())
                        file_path = str(rel_path)
                    except ValueError:
                        pass
                lines.append(f"- **{item['name']}** in `{file_path}`")
            if len(items) > 10:
                lines.append(f"  ... and {len(items) - 10} more")

        return "\n".join(lines)

    except ImportError:
        return "[red]Error:[/red] Citadel is not installed. Run: pip install citadel"
    except Exception as e:
        logger.exception("Citadel dead code error")
        return f"[red]Error:[/red] {e}"


async def _run_citadel_sequence(code_dir: Path) -> str:
    """Generate sequence diagrams for the codebase using citadel.

    Args:
        code_dir: Code directory to analyze.

    Returns:
        Mermaid diagrams or paths to saved files.
    """
    try:
        from citadel.sdk import Citadel  # type: ignore[import-untyped]

        citadel = Citadel()

        diagrams = citadel.get_sequence_diagrams(code_dir, max_diagrams=3)

        if not diagrams:
            return "No significant call sequences found in codebase."

        # Save diagrams
        diagram_dir = code_dir / ".codewhisper" / "diagrams"
        saved_paths = []
        lines = [f"**Sequence Diagrams** ({len(diagrams)} generated)\n"]

        for i, diagram in enumerate(diagrams):
            filepath = _save_mermaid_diagram(diagram, diagram_dir, f"sequence_{i + 1}")
            saved_paths.append(filepath)
            lines.append(f"### Sequence {i + 1}")
            lines.append(f"Saved to: `{filepath}`")
            lines.append("")
            lines.append("```mermaid")
            lines.append(diagram)
            lines.append("```")
            lines.append("")

        return "\n".join(lines)

    except ImportError:
        return "[red]Error:[/red] Citadel is not installed. Run: pip install citadel"
    except Exception as e:
        logger.exception("Citadel sequence error")
        return f"[red]Error:[/red] {e}"


async def _handle_citadel_command(
    cmd: str, user_input: str, code_dir: Path
) -> str | None:
    """Handle citadel quick commands.

    Args:
        cmd: The lowercase command.
        user_input: The full user input.
        code_dir: The code directory.

    Returns:
        Result string if command was handled, None otherwise.
    """
    parts = user_input.strip().split(maxsplit=2)

    if cmd.startswith("/analyze "):
        if len(parts) < 2:
            return "[yellow]Usage:[/yellow] /analyze <file>"
        return await _run_citadel_analyze(code_dir, parts[1])

    elif cmd.startswith("/functions "):
        if len(parts) < 2:
            return "[yellow]Usage:[/yellow] /functions <file>"
        return await _run_citadel_functions(code_dir, parts[1])

    elif cmd.startswith("/callers "):
        if len(parts) < 3:
            return "[yellow]Usage:[/yellow] /callers <file> <function_name>"
        return await _run_citadel_callers(code_dir, parts[1], parts[2])

    elif cmd.startswith("/flow "):
        if len(parts) < 2:
            return "[yellow]Usage:[/yellow] /flow <file>"
        return await _run_citadel_flow(code_dir, parts[1])

    elif cmd == "/dead":
        return await _run_citadel_dead_code(code_dir)

    elif cmd == "/sequence":
        return await _run_citadel_sequence(code_dir)

    return None


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

    # Initialize conversation tracker
    tracker = ConversationTracker(max_history=config.max_history)

    while True:
        try:
            # Build prompt with turn indicator
            turn_status = tracker.get_status() if tracker.turn_count > 0 else ""
            if turn_status:
                prompt_text = f"[bold green]You[/bold green] [dim]({turn_status})[/dim]"
            else:
                prompt_text = "[bold green]You[/bold green]"

            # Get user input
            user_input = Prompt.ask(f"\n{prompt_text}")

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
                tracker.reset()
                console.print("[dim]Conversation reset[/dim]")
                continue

            if cmd == "/skills":
                _list_skills(agent.skills_index)
                continue

            if cmd == "/status":
                console.print(Panel(Markdown(tracker.get_summary()), title="Status"))
                continue

            if cmd == "/summary":
                console.print(Panel(Markdown(tracker.get_summary()), title="Summary"))
                continue

            if cmd.startswith("/load "):
                skill_name = user_input.strip()[6:].strip()
                _load_skill_interactive(agent.skills_index, skill_name)
                continue

            if cmd.startswith("/search "):
                query = user_input.strip()[8:].strip()
                _search_skills_interactive(agent.skills_index, query)
                continue

            # Handle citadel quick commands
            citadel_commands = [
                "/analyze ",
                "/functions ",
                "/callers ",
                "/flow ",
                "/dead",
                "/sequence",
            ]
            is_citadel_cmd = any(
                cmd.startswith(c) or cmd == c.strip() for c in citadel_commands
            )
            if is_citadel_cmd:
                result = await _handle_citadel_command(cmd, user_input, config.code_dir)
                if result:
                    console.print()
                    console.print(Markdown(result))
                    continue

            if not user_input.strip():
                continue

            # Track this turn
            tracker.increment()
            tracker.add_summary(user_input)

            # Send to agent and get response with elapsed time display
            start_time = time.time()
            current_input = user_input  # Capture for closure

            try:
                with console.status("[bold]Thinking...[/bold]") as status:
                    # Create a task for the chat
                    async def chat_with_progress(msg: str = current_input) -> str:
                        return await agent.chat(msg)

                    # Run the task while updating status
                    task = asyncio.create_task(chat_with_progress())

                    while not task.done():
                        elapsed = time.time() - start_time
                        status.update(f"[bold]Thinking...[/bold] ({elapsed:.1f}s)")
                        await asyncio.sleep(0.1)

                    response = await task

            except Exception as e:
                elapsed = time.time() - start_time
                logger.exception("Error during chat")
                console.print(
                    f"[red]Error after {elapsed:.1f}s:[/red] {e}\n"
                    "[dim]Try rephrasing your question or use /reset to start fresh.[/dim]"
                )
                continue

            elapsed = time.time() - start_time

            # Format response (handle Mermaid diagrams)
            formatted_response = _format_response_with_diagrams(
                response, config.code_dir
            )

            console.print(
                f"\n[bold blue]CodeWhisper[/bold blue] [dim]({elapsed:.1f}s)[/dim]"
            )
            console.print(Markdown(formatted_response))

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

### Navigation
- `/help` - Show this help message
- `/clear` - Clear the screen
- `/quit` or `/exit` - Exit the chatbot
- `/reset` - Reset conversation history

### Skills
- `/skills` - List all available skills
- `/load <skill>` - Show a specific skill's content
- `/search <query>` - Search skills by keyword

### Conversation
- `/status` - Show conversation status (turn count)
- `/summary` - Show conversation summary

### Citadel Code Analysis
- `/analyze <file>` - Quick file analysis (artifacts and callouts)
- `/functions <file>` - List functions/paragraphs in file
- `/callers <file> <function>` - Find callers of a function
- `/flow <file>` - Generate flow diagram (Mermaid)
- `/dead` - Find dead code in codebase
- `/sequence` - Generate sequence diagrams for call chains

## Tips

- Ask about specific programs: "What does CBPAUP0C do?"
- Search for patterns: "Find all programs that call MQGET"
- Understand relationships: "How does COPAUS0C interact with the database?"
- Get overviews: "Explain the authorization subsystem"
- Ask follow-up questions: The agent remembers conversation context

## Diagrams

Mermaid diagrams are automatically saved to `.codewhisper/diagrams/`.
Use a Mermaid viewer or VS Code extension to render them.
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
        desc = (
            skill.description[:80] + "..."
            if len(skill.description) > 80
            else skill.description
        )
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
            s.name
            for s in skills_index.get_all()
            if skill_name.lower() in s.name.lower()
        ][:5]
        if suggestions:
            console.print(f"[dim]Did you mean: {', '.join(suggestions)}?[/dim]")
        return

    console.print(
        Panel(
            Markdown(
                f"**{skill.name}**\n\n{skill.description}\n\n---\n\n{skill.content[:2000]}..."
            ),
            title=f"Skill: {skill.name}",
        )
    )


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
        desc = (
            result.skill.description[:60] + "..."
            if len(result.skill.description) > 60
            else result.skill.description
        )
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

    # Run the query with elapsed time
    start_time = time.time()
    current_query = query  # Capture for closure
    try:
        with console.status("[bold]Thinking...[/bold]") as status:
            # Create a task for the chat
            async def chat_with_progress(msg: str = current_query) -> str:
                return await agent.chat(msg)

            task = asyncio.create_task(chat_with_progress())

            while not task.done():
                elapsed = time.time() - start_time
                status.update(f"[bold]Thinking...[/bold] ({elapsed:.1f}s)")
                await asyncio.sleep(0.1)

            response = await task

    except Exception as e:
        elapsed = time.time() - start_time
        logger.exception("Error during chat")
        console.print(f"[red]Error after {elapsed:.1f}s:[/red] {e}")
        return

    elapsed = time.time() - start_time

    # Format response
    formatted_response = _format_response_with_diagrams(response, config.code_dir)

    console.print(f"[bold blue]CodeWhisper[/bold blue] [dim]({elapsed:.1f}s)[/dim]")
    console.print(Markdown(formatted_response))


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
        console.print(
            "[dim]Specify the path to the skills directory containing SKILL.md files[/dim]"
        )
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
        config = AgentConfig(**config_kwargs)  # type: ignore[arg-type]
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
