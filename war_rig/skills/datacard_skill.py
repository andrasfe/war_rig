"""Datacards skill generator for Agent Skills format.

This module provides the DatacardSkillGenerator class which creates the
datacards catalog skill documenting data structures and their relationships.

Example:
    >>> generator = DatacardSkillGenerator(Path("./skills"))
    >>> skill_dir = generator.generate(Path("./output/DATACARDS.md"))
"""

import logging
from pathlib import Path

from war_rig.skills.naming import create_skill_frontmatter

logger = logging.getLogger(__name__)

# Fixed skill name for datacards
DATACARDS_SKILL_NAME = "datacards"


class DatacardSkillGenerationError(Exception):
    """Raised when datacard skill generation fails."""

    pass


class DatacardSkillGenerator:
    """Generates the datacards catalog skill.

    The datacards skill provides documentation of data structures,
    copybooks, file layouts, and database schemas used within the system.

    Attributes:
        output_dir: Root skills output directory.

    Example:
        >>> generator = DatacardSkillGenerator(Path("./skills"))
        >>> skill_dir = generator.generate(Path("./output/DATACARDS.md"))
    """

    def __init__(self, output_dir: Path) -> None:
        """Initialize the DatacardSkillGenerator.

        Args:
            output_dir: Root skills output directory (e.g., ./skills).
                       Datacards skill will be created at output_dir/datacards/.
        """
        self.output_dir = output_dir.resolve()
        logger.debug(
            f"DatacardSkillGenerator initialized: output_dir={self.output_dir}"
        )

    def generate(self, datacards_md_path: Path) -> Path:
        """Generate the datacards skill.

        Creates:
            datacards/SKILL.md - Main skill file with data structure catalog.

        Args:
            datacards_md_path: Path to DATACARDS.md from War Rig output.

        Returns:
            Path to the created skill directory.

        Raises:
            DatacardSkillGenerationError: If generation fails.
        """
        logger.info(f"Generating datacards skill from: {datacards_md_path}")

        # Read the datacards content
        try:
            datacards_content = datacards_md_path.read_text(encoding="utf-8")
        except Exception as e:
            raise DatacardSkillGenerationError(
                f"Failed to read {datacards_md_path}: {e}"
            ) from e

        # Create skill directory
        skill_dir = self.output_dir / DATACARDS_SKILL_NAME
        skill_dir.mkdir(parents=True, exist_ok=True)

        # Generate SKILL.md
        skill_md_path = skill_dir / "SKILL.md"
        skill_content = self._generate_skill_md(datacards_content)
        skill_md_path.write_text(skill_content, encoding="utf-8")

        logger.info(f"Generated datacards skill at: {skill_dir}")
        return skill_dir

    def _generate_skill_md(self, datacards_content: str) -> str:
        """Generate the complete SKILL.md content.

        Args:
            datacards_content: Raw content from DATACARDS.md.

        Returns:
            Complete SKILL.md content with frontmatter and body.
        """
        description = self._extract_description(datacards_content)
        frontmatter = create_skill_frontmatter(DATACARDS_SKILL_NAME, description)
        body = self._format_skill_body(datacards_content)
        return f"{frontmatter}\n\n{body}"

    def _extract_description(self, datacards_content: str) -> str:
        """Extract a concise description from the datacards.

        Args:
            datacards_content: Raw content from DATACARDS.md.

        Returns:
            Concise description string (max 1024 chars).
        """
        # Count data structures mentioned
        lines = datacards_content.split("\n")
        datacard_count = 0

        for line in lines:
            stripped = line.strip()
            # Count level-2 or level-3 headers as potential datacards
            if stripped.startswith("## ") or stripped.startswith("### "):
                datacard_count += 1

        if datacard_count > 0:
            return (
                f"Data structure catalog documenting {datacard_count} "
                "copybooks, file layouts, and database schemas."
            )

        return (
            "Data structure catalog documenting copybooks, file layouts, "
            "database schemas, and record definitions used in the system."
        )

    def _format_skill_body(self, datacards_content: str) -> str:
        """Format the main SKILL.md body content.

        Args:
            datacards_content: Raw content from DATACARDS.md.

        Returns:
            Formatted markdown body content.
        """
        sections: list[str] = []

        # Title
        sections.append("# Datacards Catalog")
        sections.append("")

        # Purpose section
        sections.append("## About This Skill")
        sections.append("")
        sections.append(
            "This skill provides a catalog of data structures used within the system, "
            "including copybooks, file layouts, database tables, and record definitions. "
            "Use this to understand data formats and field definitions."
        )
        sections.append("")

        # When to use section
        sections.append("## When to Use This Skill")
        sections.append("")
        sections.append("Use this skill when you need to:")
        sections.append("- Understand the layout of a specific data structure")
        sections.append("- Find field definitions and data types")
        sections.append("- Identify relationships between data structures")
        sections.append("- Understand file or database record formats")
        sections.append("- Map data flows between programs")
        sections.append("")

        # Include the datacards content (cleaned up)
        sections.append("## Data Structure Catalog")
        sections.append("")
        cleaned_content = self._clean_datacards_content(datacards_content)
        sections.append(cleaned_content)
        sections.append("")

        # Related skills
        sections.append("## Related Skills")
        sections.append("")
        sections.append("- **system-overview**: High-level system documentation")
        sections.append("- **call-graph**: Program call relationships")
        sections.append(
            "- Individual program skills: See which programs use specific data structures"
        )

        return "\n".join(sections)

    def _clean_datacards_content(self, content: str) -> str:
        """Clean up the datacards content for inclusion.

        Removes the main header and metadata lines.

        Args:
            content: Raw datacards content.

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
