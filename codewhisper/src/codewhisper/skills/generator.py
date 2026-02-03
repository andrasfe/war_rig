"""Skill generator for CodeWhisper.

This module generates hierarchical SKILL.md files from War Rig's documentation
output. The generated skills provide context to the LLM agent by organizing
program summaries and documentation links by category.

The generator creates:
- A top-level SKILL.md with executive summary and category links
- Per-category SKILL.md files with program summaries and doc links

Example:
    generator = SkillGenerator(SkillGeneratorConfig(
        docs_dir=Path("./example_output/documentation"),
        output_dir=Path("./example_output/code-skills"),
        relative_docs_path="../documentation",
    ))
    result = generator.generate()
    print(f"Created {len(result.categories_created)} category skills")
"""

from __future__ import annotations

import logging
import re
from dataclasses import dataclass, field
from pathlib import Path

from citadel.sdk import get_markdown_summary as citadel_get_markdown_summary

logger = logging.getLogger(__name__)


# Mapping from documentation directory names to friendly category names
CATEGORY_MAPPING: dict[str, str] = {
    "cbl": "cobol",
    "jcl": "jcl",
    "cpy": "copybook",
    "cpy-bms": "bms-copybook",
    "bms": "bms",
    "ddl": "ddl",
    "ims": "ims",
}

# Friendly descriptions for each category
CATEGORY_DESCRIPTIONS: dict[str, str] = {
    "cobol": "COBOL program documentation",
    "jcl": "JCL job documentation",
    "copybook": "Copybook documentation (shared data structures)",
    "bms-copybook": "BMS copybook documentation (screen mappings)",
    "bms": "BMS map documentation (screen definitions)",
    "ddl": "DDL documentation (database definitions)",
    "ims": "IMS documentation (database/PSB definitions)",
}


@dataclass
class SkillGeneratorConfig:
    """Configuration for skill generation.

    Attributes:
        docs_dir: Input documentation directory containing subdirectories
            like cbl/, jcl/, etc. with .md documentation files.
        output_dir: Output skills directory where SKILL.md files will be
            created in a hierarchical structure.
        relative_docs_path: Relative path from skills directory to docs
            directory (e.g., "../documentation"). Used to construct links.
        system_name: Optional system name for the top-level skill title.
            Defaults to "System".
    """

    docs_dir: Path
    output_dir: Path
    relative_docs_path: str = "../documentation"
    system_name: str = "System"


@dataclass
class GenerationResult:
    """Result of skill generation.

    Attributes:
        top_level_created: Whether the top-level SKILL.md was created.
        categories_created: List of category names that had skills generated.
        files_processed: Total number of documentation files processed.
        errors: List of error messages encountered during generation.
    """

    top_level_created: bool = False
    categories_created: list[str] = field(default_factory=list)
    files_processed: int = 0
    errors: list[str] = field(default_factory=list)


def get_markdown_summary(file_path: Path, max_chars: int = 200) -> str | None:
    """Extract a summary from a markdown documentation file.

    This is a wrapper around citadel.sdk.get_markdown_summary() that returns
    just the summary string for use in skill generation.

    Args:
        file_path: Path to the markdown documentation file.
        max_chars: Maximum characters for the summary.

    Returns:
        The extracted summary string, or None if no summary could be found.
    """
    try:
        result = citadel_get_markdown_summary(file_path, max_chars=max_chars)
        summary = result.get("summary", "")
        return summary if summary else None
    except Exception as e:
        logger.warning("Failed to extract summary from %s: %s", file_path, e)
        return None


class SkillGenerator:
    """Generates hierarchical SKILL.md files from documentation.

    This class reads documentation files from a War Rig output directory
    and generates SKILL.md files organized by category (COBOL, JCL, etc.)
    that can be used by CodeWhisper's agent to understand the codebase.

    The generated structure is:
        output_dir/
            SKILL.md               # Top-level with executive summary
            cobol/SKILL.md         # Program summaries + links
            jcl/SKILL.md           # Job summaries + links
            ...

    Attributes:
        config: Configuration for skill generation.

    Example:
        config = SkillGeneratorConfig(
            docs_dir=Path("./documentation"),
            output_dir=Path("./skills"),
            relative_docs_path="../documentation",
        )
        generator = SkillGenerator(config)
        result = generator.generate()
    """

    def __init__(self, config: SkillGeneratorConfig) -> None:
        """Initialize the skill generator.

        Args:
            config: Configuration for skill generation.

        Raises:
            ValueError: If docs_dir doesn't exist or is not a directory.
        """
        self.config = config

        if not self.config.docs_dir.exists():
            raise ValueError(f"Documentation directory does not exist: {config.docs_dir}")

        if not self.config.docs_dir.is_dir():
            raise ValueError(f"Documentation path is not a directory: {config.docs_dir}")

    def generate(self) -> GenerationResult:
        """Generate all skills from the documentation.

        Creates the output directory structure and generates:
        - Top-level SKILL.md with executive summary
        - Per-category SKILL.md files with file summaries

        Returns:
            GenerationResult with summary of what was created.

        Raises:
            OSError: If output directory cannot be created.
        """
        result = GenerationResult()

        # Create output directory
        try:
            self.config.output_dir.mkdir(parents=True, exist_ok=True)
        except OSError as e:
            result.errors.append(f"Failed to create output directory: {e}")
            return result

        # Discover categories
        categories = self._discover_categories()
        logger.info("Discovered %d categories: %s", len(categories), list(categories.keys()))

        # Generate category skills first (we need their info for top-level)
        category_info: dict[str, dict[str, str | int]] = {}
        for docs_subdir, category_name in categories.items():
            try:
                info = self._generate_category_skill(category_name, docs_subdir)
                file_count = int(info["file_count"])
                if file_count > 0:
                    category_info[category_name] = info
                    result.categories_created.append(category_name)
                    result.files_processed += file_count
            except Exception as e:
                error_msg = f"Failed to generate {category_name} skill: {e}"
                logger.error(error_msg)
                result.errors.append(error_msg)

        # Generate top-level skill
        try:
            self._generate_top_level_skill(category_info)
            result.top_level_created = True
        except Exception as e:
            error_msg = f"Failed to generate top-level skill: {e}"
            logger.error(error_msg)
            result.errors.append(error_msg)

        logger.info(
            "Generation complete: %d categories, %d files processed",
            len(result.categories_created),
            result.files_processed,
        )

        return result

    def _discover_categories(self) -> dict[str, str]:
        """Discover available categories from documentation subdirectories.

        Returns:
            Dictionary mapping docs subdirectory name to category name.
        """
        categories: dict[str, str] = {}

        for subdir in self.config.docs_dir.iterdir():
            if not subdir.is_dir():
                continue

            # Skip special directories
            if subdir.name.startswith(".") or subdir.name == "cache":
                continue

            # Map to friendly category name
            category_name = CATEGORY_MAPPING.get(subdir.name, subdir.name)
            categories[subdir.name] = category_name

        return categories

    def _generate_top_level_skill(
        self,
        category_info: dict[str, dict[str, str | int]],
    ) -> None:
        """Generate the root SKILL.md with executive summary and category links.

        Args:
            category_info: Dictionary of category names to info dicts containing
                'file_count' and 'description'.
        """
        # Try to extract executive summary from README.md
        readme_path = self.config.docs_dir / "README.md"
        executive_summary = self._extract_executive_summary(readme_path)

        # Build content
        lines: list[str] = [
            "---",
            "name: system-overview",
            f"description: {self.config.system_name} documentation overview",
            "---",
            "",
            f"# {self.config.system_name} Overview",
            "",
        ]

        if executive_summary:
            lines.extend([executive_summary, "", ""])

        # Add categories section
        lines.extend(["## Categories", ""])

        for category_name in sorted(category_info.keys()):
            info = category_info[category_name]
            description = CATEGORY_DESCRIPTIONS.get(
                category_name,
                f"{category_name.upper()} documentation",
            )
            file_count = info.get("file_count", 0)
            lines.append(
                f"- [{category_name.upper()}]({category_name}/SKILL.md) - "
                f"{description} ({file_count} files)"
            )

        lines.append("")

        # Write file
        output_path = self.config.output_dir / "SKILL.md"
        output_path.write_text("\n".join(lines), encoding="utf-8")
        logger.info("Generated top-level skill: %s", output_path)

    def _extract_executive_summary(self, readme_path: Path) -> str | None:
        """Extract executive summary from README.md.

        Looks for the "Executive Summary" section and extracts the first
        2-3 paragraphs.

        Args:
            readme_path: Path to the README.md file.

        Returns:
            Executive summary text, or None if not found.
        """
        if not readme_path.exists():
            logger.debug("README.md not found at %s", readme_path)
            return None

        try:
            content = readme_path.read_text(encoding="utf-8")
        except Exception as e:
            logger.warning("Failed to read README.md: %s", e)
            return None

        # Look for Executive Summary section
        match = re.search(
            r"##\s*\d*\.?\s*Executive Summary\s*\n+(.*?)(?=\n###|\n##\s*\d|\Z)",
            content,
            re.DOTALL | re.IGNORECASE,
        )

        if not match:
            # Try without section number
            match = re.search(
                r"##\s*Executive Summary\s*\n+(.*?)(?=\n###|\n##|\Z)",
                content,
                re.DOTALL | re.IGNORECASE,
            )

        if match:
            summary_text = match.group(1).strip()
            # Take first 2-3 paragraphs
            paragraphs = [p.strip() for p in summary_text.split("\n\n") if p.strip()]
            if paragraphs:
                return "\n\n".join(paragraphs[:3])

        return None

    def _generate_category_skill(
        self,
        category: str,
        docs_subdir: str,
    ) -> dict[str, str | int]:
        """Generate a category SKILL.md with file summaries and links.

        Args:
            category: Friendly category name (e.g., "cobol").
            docs_subdir: Subdirectory name in docs (e.g., "cbl").

        Returns:
            Dictionary with 'file_count' and 'description'.
        """
        docs_path = self.config.docs_dir / docs_subdir
        if not docs_path.exists():
            logger.warning("Category docs path does not exist: %s", docs_path)
            return {"file_count": 0, "description": ""}

        # Find all .md files in the category
        md_files = sorted(docs_path.glob("*.md"))
        if not md_files:
            logger.debug("No markdown files in %s", docs_path)
            return {"file_count": 0, "description": ""}

        # Create output directory for category
        category_output = self.config.output_dir / category
        category_output.mkdir(parents=True, exist_ok=True)

        # Get description
        description = CATEGORY_DESCRIPTIONS.get(
            category,
            f"{category.upper()} documentation",
        )

        # Build content
        lines: list[str] = [
            "---",
            f"name: {category}",
            f"description: {description}",
            "---",
            "",
            f"# {category.upper()} Documentation",
            "",
        ]

        # Add summary table
        if category in ("cobol", "jcl"):
            lines.append(f"| {'Program' if category == 'cobol' else 'Job'} | Description | Documentation |")
        else:
            lines.append("| Name | Description | Documentation |")
        lines.append("|---------|-------------|---------------|")

        file_count = 0
        for md_file in md_files:
            # Skip README.md and similar
            if md_file.name.lower() in ("readme.md",):
                continue

            # Extract file info
            file_name = self._extract_program_name(md_file)
            summary = get_markdown_summary(md_file)

            if summary is None:
                summary = "(No description available)"

            # Escape any pipe characters in summary
            summary = summary.replace("|", "\\|")

            # Build link to documentation
            doc_link = f"{self.config.relative_docs_path}/{docs_subdir}/{md_file.name}"

            lines.append(f"| {file_name} | {summary} | [Full docs]({doc_link}) |")
            file_count += 1

        lines.append("")

        # Write file
        output_path = category_output / "SKILL.md"
        output_path.write_text("\n".join(lines), encoding="utf-8")
        logger.info("Generated %s skill: %s (%d files)", category, output_path, file_count)

        return {"file_count": file_count, "description": description}

    def _extract_program_name(self, md_file: Path) -> str:
        """Extract the program/file name from a documentation file.

        Removes common suffixes like .cbl.md, .CBL.md, etc.

        Args:
            md_file: Path to the markdown documentation file.

        Returns:
            Clean program/file name.
        """
        name = md_file.name

        # Remove .md suffix first
        if name.endswith(".md"):
            name = name[:-3]

        # Remove common source file extensions
        common_extensions = [
            ".cbl", ".CBL", ".cob", ".COB",
            ".jcl", ".JCL",
            ".cpy", ".CPY",
            ".bms", ".BMS",
            ".ddl", ".DDL",
            ".dbd", ".DBD",
            ".psb", ".PSB",
        ]
        for ext in common_extensions:
            if name.endswith(ext):
                name = name[: -len(ext)]
                break

        return name
