"""Skills loader for CodeWhisper.

This module handles loading skill files from disk. Skills are markdown
files with YAML frontmatter containing metadata like name and description.

The expected format is:

    ---
    name: skill-name
    description: Brief description of the skill
    ---

    # Skill Content

    Markdown content here...

Example:
    loader = SkillsLoader(Path("./skills"))
    skills = loader.load_all()
    for skill in skills:
        print(f"{skill.name}: {skill.description}")
"""

from __future__ import annotations

import logging
from collections.abc import Iterator
from dataclasses import dataclass
from pathlib import Path

import frontmatter

logger = logging.getLogger(__name__)


@dataclass
class Skill:
    """A loaded skill with metadata and content.

    Attributes:
        name: Unique identifier for the skill.
        description: Brief description of what the skill covers.
        content: Full markdown content (without frontmatter).
        file_path: Path to the source file.
        tags: Optional list of tags for categorization.
    """

    name: str
    description: str
    content: str
    file_path: Path
    tags: list[str] | None = None

    @classmethod
    def from_file(cls, file_path: Path) -> Skill | None:
        """Load a skill from a markdown file with frontmatter.

        Args:
            file_path: Path to the skill file.

        Returns:
            Loaded Skill, or None if parsing fails.

        TODO: Handle encoding issues gracefully
        TODO: Validate required frontmatter fields
        """
        try:
            # Parse the frontmatter
            post = frontmatter.load(file_path)

            # Extract metadata
            name = post.get("name", file_path.stem)
            description = post.get("description", "")
            tags = post.get("tags")

            if isinstance(tags, str):
                tags = [t.strip() for t in tags.split(",")]

            return cls(
                name=name,
                description=description,
                content=post.content,
                file_path=file_path,
                tags=tags,
            )

        except Exception as e:
            logger.warning(f"Failed to load skill from {file_path}: {e}")
            return None

    def __str__(self) -> str:
        """String representation of the skill."""
        return f"Skill({self.name}: {self.description[:50]}...)"


class SkillsLoader:
    """Loader for skill files from a directory.

    Recursively finds and loads all SKILL.md files from the given
    directory and its subdirectories.

    Attributes:
        skills_dir: Base directory containing skills.

    Example:
        loader = SkillsLoader(Path("./example_output/skills"))
        skills = loader.load_all()

        # Or iterate lazily
        for skill in loader.iter_skills():
            print(skill.name)
    """

    # File patterns to search for
    SKILL_PATTERNS = ["SKILL.md", "skill.md", "*.skill.md"]

    def __init__(self, skills_dir: Path):
        """Initialize the loader.

        Args:
            skills_dir: Path to the skills directory.

        Raises:
            ValueError: If skills_dir doesn't exist.
        """
        self.skills_dir = Path(skills_dir)

        if not self.skills_dir.exists():
            raise ValueError(f"Skills directory does not exist: {skills_dir}")

        if not self.skills_dir.is_dir():
            raise ValueError(f"Skills path is not a directory: {skills_dir}")

        logger.debug(f"SkillsLoader initialized: {skills_dir}")

    def iter_skills(self) -> Iterator[Skill]:
        """Iterate over all skills in the directory.

        Yields skills as they are loaded, allowing for lazy processing.

        Yields:
            Loaded Skill objects.

        TODO: Add progress logging for large skill sets
        """
        # Find all skill files
        skill_files = self._find_skill_files()

        for file_path in skill_files:
            skill = Skill.from_file(file_path)
            if skill is not None:
                yield skill

    def load_all(self) -> list[Skill]:
        """Load all skills from the directory.

        Returns:
            List of all loaded skills.
        """
        skills = list(self.iter_skills())
        logger.info(f"Loaded {len(skills)} skills from {self.skills_dir}")
        return skills

    def load_by_name(self, name: str) -> Skill | None:
        """Load a specific skill by name.

        Searches for a skill matching the given name.

        Args:
            name: Skill name to find.

        Returns:
            The skill if found, None otherwise.

        TODO: Optimize with caching or pre-indexing
        """
        name_lower = name.lower()

        for skill in self.iter_skills():
            if skill.name.lower() == name_lower:
                return skill

        return None

    def _find_skill_files(self) -> list[Path]:
        """Find all skill files in the directory.

        Returns:
            List of paths to skill files.
        """
        skill_files: list[Path] = []

        for pattern in self.SKILL_PATTERNS:
            skill_files.extend(self.skills_dir.rglob(pattern))

        # Deduplicate and sort
        unique_files = sorted(set(skill_files))
        logger.debug(f"Found {len(unique_files)} skill files")

        return unique_files

    def get_skill_paths(self) -> dict[str, Path]:
        """Get a mapping of skill names to file paths.

        This is useful for building an index without loading full content.

        Returns:
            Dictionary mapping skill names to file paths.

        TODO: Implement efficient metadata-only loading
        """
        result: dict[str, Path] = {}

        for skill in self.iter_skills():
            result[skill.name] = skill.file_path

        return result
