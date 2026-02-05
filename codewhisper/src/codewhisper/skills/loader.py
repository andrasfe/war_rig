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

    By default, only loads the root SKILL.md file for progressive disclosure.
    Category skills can be loaded on-demand using load_category().

    Attributes:
        skills_dir: Base directory containing skills.

    Example:
        loader = SkillsLoader(Path("./example_output/skills"))

        # Load only root skill (default)
        root_skill = loader.load_root()

        # Load a specific category when needed
        cobol_skill = loader.load_category("cobol")

        # Or load all (not recommended for large skill sets)
        all_skills = loader.load_all(recursive=True)
    """

    # File patterns to search for
    SKILL_PATTERNS = ["SKILL.md", "skill.md"]

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

    def load_root(self) -> Skill | None:
        """Load only the root SKILL.md file.

        This is the preferred method for initial loading - provides
        the overview without loading all category details.

        Returns:
            The root skill, or None if not found.
        """
        for pattern in self.SKILL_PATTERNS:
            root_path = self.skills_dir / pattern
            if root_path.exists():
                skill = Skill.from_file(root_path)
                if skill:
                    logger.info(f"Loaded root skill: {skill.name}")
                    return skill

        logger.warning(f"No root SKILL.md found in {self.skills_dir}")
        return None

    def load_category(self, category: str) -> Skill | None:
        """Load a specific category's SKILL.md.

        Use this for on-demand loading when user asks about a category.

        Args:
            category: Category name (e.g., "cobol", "jcl").

        Returns:
            The category skill, or None if not found.
        """
        category_dir = self.skills_dir / category

        if not category_dir.is_dir():
            logger.warning(f"Category directory not found: {category}")
            return None

        for pattern in self.SKILL_PATTERNS:
            skill_path = category_dir / pattern
            if skill_path.exists():
                skill = Skill.from_file(skill_path)
                if skill:
                    logger.info(f"Loaded category skill: {skill.name}")
                    return skill

        logger.warning(f"No SKILL.md found in category: {category}")
        return None

    def list_categories(self) -> list[str]:
        """List available category directories.

        Returns:
            List of category names that have SKILL.md files.
        """
        categories = []
        for subdir in self.skills_dir.iterdir():
            if not subdir.is_dir() or subdir.name.startswith("."):
                continue
            # Check if it has a skill file
            for pattern in self.SKILL_PATTERNS:
                if (subdir / pattern).exists():
                    categories.append(subdir.name)
                    break
        return sorted(categories)

    def iter_skills(self, recursive: bool = False) -> Iterator[Skill]:
        """Iterate over skills in the directory.

        Args:
            recursive: If False (default), only yields root skill.
                      If True, yields all skills recursively.

        Yields:
            Loaded Skill objects.
        """
        skill_files = self._find_skill_files(recursive=recursive)

        for file_path in skill_files:
            skill = Skill.from_file(file_path)
            if skill is not None:
                yield skill

    def load_all(self, recursive: bool = False) -> list[Skill]:
        """Load skills from the directory.

        Args:
            recursive: If False (default), only loads root skill.
                      If True, loads all skills recursively.

        Returns:
            List of loaded skills.
        """
        skills = list(self.iter_skills(recursive=recursive))
        logger.info(f"Loaded {len(skills)} skills from {self.skills_dir}")
        return skills

    def load_by_name(self, name: str) -> Skill | None:
        """Load a specific skill by name.

        Searches all skills (recursive) for a matching name.

        Args:
            name: Skill name to find.

        Returns:
            The skill if found, None otherwise.
        """
        name_lower = name.lower()

        for skill in self.iter_skills(recursive=True):
            if skill.name.lower() == name_lower:
                return skill

        return None

    def _find_skill_files(self, recursive: bool = False) -> list[Path]:
        """Find skill files in the directory.

        Args:
            recursive: If False, only finds root-level skill files.
                      If True, finds all skill files recursively.

        Returns:
            List of paths to skill files.
        """
        skill_files: list[Path] = []

        for pattern in self.SKILL_PATTERNS:
            if recursive:
                skill_files.extend(self.skills_dir.rglob(pattern))
            else:
                # Only root level
                root_file = self.skills_dir / pattern
                if root_file.exists():
                    skill_files.append(root_file)

        # Deduplicate and sort - use samefile() to handle case-insensitive filesystems
        # where SKILL.md and skill.md may refer to the same file
        unique_files: list[Path] = []
        for f in skill_files:
            # Check if this file is the same as any already added (handles case-insensitive FS)
            is_duplicate = any(f.samefile(existing) for existing in unique_files)
            if not is_duplicate:
                unique_files.append(f)
        unique_files.sort()
        logger.debug(f"Found {len(unique_files)} skill files (recursive={recursive})")

        return unique_files

    def get_skill_paths(self, recursive: bool = True) -> dict[str, Path]:
        """Get a mapping of skill names to file paths.

        This is useful for building an index without loading full content.

        Args:
            recursive: If True, includes all skills. If False, only root.

        Returns:
            Dictionary mapping skill names to file paths.
        """
        result: dict[str, Path] = {}

        for skill in self.iter_skills(recursive=recursive):
            result[skill.name] = skill.file_path

        return result
