"""System overview skill generator for Agent Skills format.

This module provides the OverviewSkillGenerator class which creates the
system overview skill that serves as the index for progressive discovery,
helping agents find relevant programs within the documented system.

Example:
    >>> generator = OverviewSkillGenerator(Path("./skills"))
    >>> skill_dir = generator.generate(
    ...     Path("./output/SYSTEM_OVERVIEW.md"),
    ...     program_summaries
    ... )
"""

import logging
from pathlib import Path

from war_rig.skills.naming import create_skill_frontmatter

logger = logging.getLogger(__name__)

# Fixed skill name for system overview
SYSTEM_OVERVIEW_SKILL_NAME = "system-overview"


class OverviewSkillGenerationError(Exception):
    """Raised when overview skill generation fails."""

    pass


class OverviewSkillGenerator:
    """Generates the system overview skill for progressive discovery.

    The system overview skill serves as the main index for agent discovery,
    providing a high-level view of the documented system and a catalog of
    all available program skills.

    Attributes:
        output_dir: Root skills output directory.

    Example:
        >>> generator = OverviewSkillGenerator(Path("./skills"))
        >>> program_summaries = [
        ...     {"name": "cbact01c", "program_id": "CBACT01C",
        ...      "description": "Account processing", "type": "BATCH"}
        ... ]
        >>> skill_dir = generator.generate(
        ...     Path("./output/SYSTEM_OVERVIEW.md"),
        ...     program_summaries
        ... )
    """

    def __init__(self, output_dir: Path) -> None:
        """Initialize the OverviewSkillGenerator.

        Args:
            output_dir: Root skills output directory (e.g., ./skills).
                       Overview skill will be created at output_dir/system-overview/.
        """
        self.output_dir = output_dir.resolve()
        logger.debug(
            f"OverviewSkillGenerator initialized: output_dir={self.output_dir}"
        )

    def generate(
        self, overview_md_path: Path, program_summaries: list[dict[str, str]]
    ) -> Path:
        """Generate the system overview skill.

        Creates:
            system-overview/SKILL.md - Main skill file with system overview
                                       and program catalog.

        Args:
            overview_md_path: Path to SYSTEM_OVERVIEW.md from War Rig output.
            program_summaries: List of dicts with {name, program_id, description, type}
                              for all documented programs.

        Returns:
            Path to the created skill directory.

        Raises:
            OverviewSkillGenerationError: If generation fails.
        """
        logger.info(f"Generating system overview skill from: {overview_md_path}")

        # Read the system overview content
        try:
            overview_content = overview_md_path.read_text(encoding="utf-8")
        except Exception as e:
            raise OverviewSkillGenerationError(
                f"Failed to read {overview_md_path}: {e}"
            ) from e

        # Create skill directory
        skill_dir = self.output_dir / SYSTEM_OVERVIEW_SKILL_NAME
        skill_dir.mkdir(parents=True, exist_ok=True)

        # Generate SKILL.md
        skill_md_path = skill_dir / "SKILL.md"
        skill_content = self._generate_skill_md(overview_content, program_summaries)
        skill_md_path.write_text(skill_content, encoding="utf-8")

        logger.info(f"Generated system overview skill at: {skill_dir}")
        return skill_dir

    def _generate_skill_md(
        self, overview_content: str, program_summaries: list[dict[str, str]]
    ) -> str:
        """Generate the complete SKILL.md content.

        Args:
            overview_content: Raw content from SYSTEM_OVERVIEW.md.
            program_summaries: List of program summary dicts.

        Returns:
            Complete SKILL.md content with frontmatter and body.
        """
        description = self._extract_description(overview_content)
        frontmatter = create_skill_frontmatter(SYSTEM_OVERVIEW_SKILL_NAME, description)
        body = self._format_skill_body(overview_content, program_summaries)
        return f"{frontmatter}\n\n{body}"

    def _extract_description(self, overview_content: str) -> str:
        """Extract a concise description from the overview.

        Args:
            overview_content: Raw content from SYSTEM_OVERVIEW.md.

        Returns:
            Concise description string (max 1024 chars).
        """
        # Try to extract the first meaningful paragraph
        lines = overview_content.strip().split("\n")

        description_parts: list[str] = []
        in_content = False

        for line in lines:
            stripped = line.strip()

            # Skip empty lines and headers at the start
            if not stripped:
                if in_content and description_parts:
                    # End of first paragraph
                    break
                continue

            # Skip markdown headers
            if stripped.startswith("#"):
                in_content = True
                continue

            # Skip metadata lines
            if stripped.startswith("*") and stripped.endswith("*"):
                continue

            # Start collecting content
            in_content = True
            description_parts.append(stripped)

            # Stop after a reasonable length
            if len(" ".join(description_parts)) > 500:
                break

        if description_parts:
            desc = " ".join(description_parts)
            # Truncate if needed
            if len(desc) > 1024:
                desc = desc[:1021] + "..."
            return desc

        return "System overview and program catalog for progressive discovery."

    def _format_skill_body(
        self, overview_content: str, program_summaries: list[dict[str, str]]
    ) -> str:
        """Format the main SKILL.md body content.

        Args:
            overview_content: Raw content from SYSTEM_OVERVIEW.md.
            program_summaries: List of program summary dicts.

        Returns:
            Formatted markdown body content.
        """
        sections: list[str] = []

        # Title
        sections.append("# System Overview")
        sections.append("")

        # Purpose section
        sections.append("## About This Skill")
        sections.append("")
        sections.append(
            "This skill provides a high-level overview of the documented system "
            "and serves as an index for discovering specific program documentation. "
            "Use this as your starting point when exploring the codebase."
        )
        sections.append("")

        # When to use section
        sections.append("## When to Use This Skill")
        sections.append("")
        sections.append("Use this skill when you need to:")
        sections.append("- Understand the overall system architecture")
        sections.append("- Find which program handles a specific function")
        sections.append("- Get an overview before diving into specific programs")
        sections.append("- Understand how programs relate to each other")
        sections.append("")

        # Include the original overview content (cleaned up)
        sections.append("## System Documentation")
        sections.append("")
        cleaned_content = self._clean_overview_content(overview_content)
        sections.append(cleaned_content)
        sections.append("")

        # Program catalog section
        if program_summaries:
            sections.append("## Program Catalog")
            sections.append("")
            sections.append(
                "The following programs are documented. "
                "Use the program name to load its specific skill for detailed information."
            )
            sections.append("")

            # Group programs by type
            programs_by_type: dict[str, list[dict[str, str]]] = {}
            for prog in program_summaries:
                prog_type = prog.get("type", "Other")
                if prog_type not in programs_by_type:
                    programs_by_type[prog_type] = []
                programs_by_type[prog_type].append(prog)

            # Output each type group
            for prog_type in sorted(programs_by_type.keys()):
                progs = programs_by_type[prog_type]
                sections.append(f"### {prog_type} Programs")
                sections.append("")
                sections.append("| Program | Skill | Description |")
                sections.append("|---------|-------|-------------|")
                for prog in sorted(progs, key=lambda x: x.get("program_id", "")):
                    program_id = prog.get("program_id", "")
                    skill_name = prog.get("name", "")
                    description = prog.get("description", "")
                    # Escape pipe characters in description
                    description = description.replace("|", "\\|")
                    sections.append(
                        f"| {program_id} | `{skill_name}` | {description} |"
                    )
                sections.append("")

        # Related skills
        sections.append("## Related Skills")
        sections.append("")
        sections.append("- **call-graph**: View program call relationships")
        sections.append(
            "- **datacards**: View data structure documentation (if available)"
        )
        sections.append(
            "- Individual program skills: Load `programs/{program-name}` for details"
        )

        return "\n".join(sections)

    def _clean_overview_content(self, content: str) -> str:
        """Clean up the overview content for inclusion.

        Removes metadata lines and ensures proper formatting.

        Args:
            content: Raw overview content.

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
