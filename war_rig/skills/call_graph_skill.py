"""Call graph skill generator for Agent Skills format.

This module provides the CallGraphSkillGenerator class which creates the
call graph skill showing program call relationships and dependencies.

Example:
    >>> generator = CallGraphSkillGenerator(Path("./skills"))
    >>> skill_dir = generator.generate(Path("./output/CALL_GRAPH.md"))
"""

import logging
from pathlib import Path

from war_rig.skills.naming import create_skill_frontmatter

logger = logging.getLogger(__name__)

# Fixed skill name for call graph
CALL_GRAPH_SKILL_NAME = "call-graph"


class CallGraphSkillGenerationError(Exception):
    """Raised when call graph skill generation fails."""

    pass


class CallGraphSkillGenerator:
    """Generates the call graph skill.

    The call graph skill provides visualization and documentation of
    program call relationships, helping agents understand the system's
    execution flow and dependencies.

    Attributes:
        output_dir: Root skills output directory.

    Example:
        >>> generator = CallGraphSkillGenerator(Path("./skills"))
        >>> skill_dir = generator.generate(Path("./output/CALL_GRAPH.md"))
    """

    def __init__(self, output_dir: Path) -> None:
        """Initialize the CallGraphSkillGenerator.

        Args:
            output_dir: Root skills output directory (e.g., ./skills).
                       Call graph skill will be created at output_dir/call-graph/.
        """
        self.output_dir = output_dir.resolve()
        logger.debug(
            f"CallGraphSkillGenerator initialized: output_dir={self.output_dir}"
        )

    def generate(self, call_graph_md_path: Path) -> Path:
        """Generate the call graph skill.

        Creates:
            call-graph/SKILL.md - Main skill file with call graph visualization
                                  and relationship documentation.

        Args:
            call_graph_md_path: Path to CALL_GRAPH.md from War Rig output.

        Returns:
            Path to the created skill directory.

        Raises:
            CallGraphSkillGenerationError: If generation fails.
        """
        logger.info(f"Generating call graph skill from: {call_graph_md_path}")

        # Read the call graph content
        try:
            call_graph_content = call_graph_md_path.read_text(encoding="utf-8")
        except Exception as e:
            raise CallGraphSkillGenerationError(
                f"Failed to read {call_graph_md_path}: {e}"
            ) from e

        # Create skill directory
        skill_dir = self.output_dir / CALL_GRAPH_SKILL_NAME
        skill_dir.mkdir(parents=True, exist_ok=True)

        # Generate SKILL.md
        skill_md_path = skill_dir / "SKILL.md"
        skill_content = self._generate_skill_md(call_graph_content)
        skill_md_path.write_text(skill_content, encoding="utf-8")

        logger.info(f"Generated call graph skill at: {skill_dir}")
        return skill_dir

    def _generate_skill_md(self, call_graph_content: str) -> str:
        """Generate the complete SKILL.md content.

        Args:
            call_graph_content: Raw content from CALL_GRAPH.md.

        Returns:
            Complete SKILL.md content with frontmatter and body.
        """
        description = self._extract_description(call_graph_content)
        frontmatter = create_skill_frontmatter(CALL_GRAPH_SKILL_NAME, description)
        body = self._format_skill_body(call_graph_content)
        return f"{frontmatter}\n\n{body}"

    def _extract_description(self, call_graph_content: str) -> str:
        """Extract a concise description from the call graph.

        Args:
            call_graph_content: Raw content from CALL_GRAPH.md.

        Returns:
            Concise description string (max 1024 chars).
        """
        # Extract statistics if available
        lines = call_graph_content.split("\n")
        programs_count = ""
        entry_points = ""

        for line in lines:
            stripped = line.strip()
            if "**Programs Analyzed:**" in stripped:
                programs_count = stripped.replace("**Programs Analyzed:**", "").strip()
            elif "Entry Points" in stripped and "|" in stripped:
                # In statistics table
                continue
            elif stripped.startswith("| Entry Points |"):
                parts = stripped.split("|")
                if len(parts) > 2:
                    entry_points = parts[2].strip()

        if programs_count:
            desc = f"Call graph analysis showing program relationships. {programs_count} programs analyzed."
            if entry_points:
                desc += f" {entry_points} entry points identified."
            return desc

        return (
            "Call graph showing program call relationships, "
            "entry points, and external dependencies."
        )

    def _format_skill_body(self, call_graph_content: str) -> str:
        """Format the main SKILL.md body content.

        Args:
            call_graph_content: Raw content from CALL_GRAPH.md.

        Returns:
            Formatted markdown body content.
        """
        sections: list[str] = []

        # Title
        sections.append("# Call Graph")
        sections.append("")

        # Purpose section
        sections.append("## About This Skill")
        sections.append("")
        sections.append(
            "This skill provides a visual call graph showing how programs call "
            "each other within the system. Use this to understand execution flow, "
            "identify entry points, and trace program dependencies."
        )
        sections.append("")

        # When to use section
        sections.append("## When to Use This Skill")
        sections.append("")
        sections.append("Use this skill when you need to:")
        sections.append("- Understand which programs call which other programs")
        sections.append("- Identify system entry points (JCL jobs, transactions)")
        sections.append("- Find external dependencies that need documentation")
        sections.append("- Trace the impact of changes to a specific program")
        sections.append("- Understand the overall system execution flow")
        sections.append("")

        # Include the call graph content (cleaned up)
        sections.append("## Call Graph Analysis")
        sections.append("")
        cleaned_content = self._clean_call_graph_content(call_graph_content)
        sections.append(cleaned_content)
        sections.append("")

        # Related skills
        sections.append("## Related Skills")
        sections.append("")
        sections.append("- **system-overview**: High-level system documentation")
        sections.append(
            "- Individual program skills: Load `programs/{program-name}` for details on specific programs"
        )

        return "\n".join(sections)

    def _clean_call_graph_content(self, content: str) -> str:
        """Clean up the call graph content for inclusion.

        Removes the main header and metadata lines.

        Args:
            content: Raw call graph content.

        Returns:
            Cleaned content string.
        """
        lines = content.strip().split("\n")
        cleaned_lines: list[str] = []
        skip_header = True

        for line in lines:
            stripped = line.strip()

            # Skip the main header (we provide our own)
            if skip_header and stripped.startswith("# "):
                skip_header = False
                continue

            # Skip generated timestamp lines
            if stripped.startswith("*Generated:") and stripped.endswith("*"):
                continue

            cleaned_lines.append(line)

        return "\n".join(cleaned_lines).strip()
