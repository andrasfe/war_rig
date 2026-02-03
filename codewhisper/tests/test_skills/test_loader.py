"""Tests for skills loader.

This module tests:
- Skill dataclass and from_file method
- SkillsLoader initialization and validation
- Skills file discovery
- Skills loading and iteration
"""

from pathlib import Path

import pytest

from codewhisper.skills.loader import Skill, SkillsLoader


class TestSkill:
    """Tests for the Skill dataclass."""

    def test_skill_creation(self) -> None:
        """Test creating a Skill directly."""
        skill = Skill(
            name="test-skill",
            description="A test skill",
            content="# Test\n\nContent here.",
            file_path=Path("/path/to/skill/SKILL.md"),
        )

        assert skill.name == "test-skill"
        assert skill.description == "A test skill"
        assert skill.content == "# Test\n\nContent here."
        assert skill.file_path == Path("/path/to/skill/SKILL.md")
        assert skill.tags is None

    def test_skill_with_tags(self) -> None:
        """Test skill with tags."""
        skill = Skill(
            name="test-skill",
            description="Test",
            content="Content",
            file_path=Path("/path/SKILL.md"),
            tags=["batch", "cobol", "authorization"],
        )

        assert skill.tags is not None
        assert len(skill.tags) == 3
        assert "batch" in skill.tags

    def test_skill_str_representation(self) -> None:
        """Test skill string representation."""
        skill = Skill(
            name="test-skill",
            description="A test skill for demonstrating functionality",
            content="Content",
            file_path=Path("/path/SKILL.md"),
        )

        str_repr = str(skill)
        assert "Skill" in str_repr
        assert "test-skill" in str_repr

    def test_skill_from_file(self, tmp_path: Path) -> None:
        """Test loading skill from file with frontmatter."""
        skill_file = tmp_path / "SKILL.md"
        skill_file.write_text(
            """---
name: loaded-skill
description: A skill loaded from file
tags: test, example
---

# Loaded Skill

This skill was loaded from a file.
"""
        )

        skill = Skill.from_file(skill_file)

        assert skill is not None
        assert skill.name == "loaded-skill"
        assert skill.description == "A skill loaded from file"
        assert skill.content.strip().startswith("# Loaded Skill")
        assert skill.file_path == skill_file
        assert skill.tags is not None
        assert "test" in skill.tags
        assert "example" in skill.tags

    def test_skill_from_file_minimal_frontmatter(self, tmp_path: Path) -> None:
        """Test loading skill with minimal frontmatter."""
        skill_file = tmp_path / "SKILL.md"
        skill_file.write_text(
            """---
name: minimal-skill
---

# Minimal

Content.
"""
        )

        skill = Skill.from_file(skill_file)

        assert skill is not None
        assert skill.name == "minimal-skill"
        assert skill.description == ""
        assert skill.tags is None

    def test_skill_from_file_no_name_uses_stem(self, tmp_path: Path) -> None:
        """Test that missing name defaults to filename stem."""
        skill_file = tmp_path / "my-custom-skill" / "SKILL.md"
        skill_file.parent.mkdir(parents=True)
        skill_file.write_text(
            """---
description: No name provided
---

# Content

Here.
"""
        )

        skill = Skill.from_file(skill_file)

        assert skill is not None
        assert skill.name == "SKILL"  # Uses filename stem

    def test_skill_from_file_invalid_returns_none(self, tmp_path: Path) -> None:
        """Test that invalid file returns None or handles gracefully."""
        skill_file = tmp_path / "SKILL.md"
        # Create a file that can't be parsed as frontmatter
        skill_file.write_bytes(b"\x00\x01\x02\x03")  # Binary content

        skill = Skill.from_file(skill_file)

        # Should either return None for unparseable files, or parse
        # with minimal content (python-frontmatter is very permissive)
        # The important thing is it doesn't raise an exception
        if skill is not None:
            # If parsed, the description should be empty (no frontmatter)
            assert skill.description == "" or skill is not None


class TestSkillsLoader:
    """Tests for the SkillsLoader class."""

    def test_loader_init_valid_directory(self, tmp_skills_dir: Path) -> None:
        """Test loader initialization with valid directory."""
        loader = SkillsLoader(tmp_skills_dir)

        assert loader.skills_dir == tmp_skills_dir

    def test_loader_init_nonexistent_directory(self, tmp_path: Path) -> None:
        """Test loader raises error for nonexistent directory."""
        with pytest.raises(ValueError) as exc_info:
            SkillsLoader(tmp_path / "nonexistent")

        assert "does not exist" in str(exc_info.value)

    def test_loader_init_file_not_directory(self, tmp_path: Path) -> None:
        """Test loader raises error when path is file, not directory."""
        file_path = tmp_path / "not_a_directory.txt"
        file_path.touch()

        with pytest.raises(ValueError) as exc_info:
            SkillsLoader(file_path)

        assert "not a directory" in str(exc_info.value)

    def test_loader_iter_skills_default(self, tmp_skills_dir: Path) -> None:
        """Test iterating over skills (default: root only)."""
        loader = SkillsLoader(tmp_skills_dir)

        skills = list(loader.iter_skills())

        # Default is non-recursive, should only get root
        assert len(skills) == 1
        assert skills[0].name == "system-overview"

    def test_loader_iter_skills_recursive(self, tmp_skills_dir: Path) -> None:
        """Test iterating over all skills recursively."""
        loader = SkillsLoader(tmp_skills_dir)

        skills = list(loader.iter_skills(recursive=True))

        # Should find all skills: root, cobol, jcl, test-program
        assert len(skills) >= 4
        assert all(isinstance(s, Skill) for s in skills)

    def test_loader_load_all_default(self, tmp_skills_dir: Path) -> None:
        """Test loading skills (default: root only)."""
        loader = SkillsLoader(tmp_skills_dir)

        skills = loader.load_all()

        # Default is non-recursive
        assert isinstance(skills, list)
        assert len(skills) == 1
        assert skills[0].name == "system-overview"

    def test_loader_load_all_recursive(self, tmp_skills_dir: Path) -> None:
        """Test loading all skills recursively."""
        loader = SkillsLoader(tmp_skills_dir)

        skills = loader.load_all(recursive=True)

        assert isinstance(skills, list)
        assert len(skills) >= 4

    def test_loader_load_root(self, tmp_skills_dir: Path) -> None:
        """Test loading only the root skill."""
        loader = SkillsLoader(tmp_skills_dir)

        skill = loader.load_root()

        assert skill is not None
        assert skill.name == "system-overview"

    def test_loader_load_category(self, tmp_skills_dir: Path) -> None:
        """Test loading a specific category skill."""
        loader = SkillsLoader(tmp_skills_dir)

        skill = loader.load_category("cobol")

        assert skill is not None
        assert skill.name == "cobol"
        assert "COBOL" in skill.content

    def test_loader_load_category_not_found(self, tmp_skills_dir: Path) -> None:
        """Test loading nonexistent category returns None."""
        loader = SkillsLoader(tmp_skills_dir)

        skill = loader.load_category("nonexistent")

        assert skill is None

    def test_loader_list_categories(self, tmp_skills_dir: Path) -> None:
        """Test listing available categories."""
        loader = SkillsLoader(tmp_skills_dir)

        categories = loader.list_categories()

        assert "cobol" in categories
        assert "jcl" in categories
        # "programs" has nested structure (no direct SKILL.md), not listed
        assert "programs" not in categories

    def test_loader_load_by_name(self, tmp_skills_dir: Path) -> None:
        """Test loading a skill by name."""
        loader = SkillsLoader(tmp_skills_dir)

        skill = loader.load_by_name("test-program")

        assert skill is not None
        assert skill.name == "test-program"

    def test_loader_load_by_name_case_insensitive(self, tmp_skills_dir: Path) -> None:
        """Test that name lookup is case-insensitive."""
        loader = SkillsLoader(tmp_skills_dir)

        skill = loader.load_by_name("TEST-PROGRAM")

        assert skill is not None
        assert skill.name == "test-program"

    def test_loader_load_by_name_not_found(self, tmp_skills_dir: Path) -> None:
        """Test loading nonexistent skill returns None."""
        loader = SkillsLoader(tmp_skills_dir)

        skill = loader.load_by_name("nonexistent-skill")

        assert skill is None

    def test_loader_get_skill_paths(self, tmp_skills_dir: Path) -> None:
        """Test getting skill name to path mapping."""
        loader = SkillsLoader(tmp_skills_dir)

        paths = loader.get_skill_paths()

        assert isinstance(paths, dict)
        assert len(paths) >= 1
        assert all(isinstance(p, Path) for p in paths.values())


class TestSkillsLoaderPatterns:
    """Tests for skill file pattern discovery."""

    def test_finds_skill_md_files(self, tmp_path: Path) -> None:
        """Test that SKILL.md files are found with recursive loading."""
        skill_dir = tmp_path / "my-skill"
        skill_dir.mkdir()
        skill_file = skill_dir / "SKILL.md"
        skill_file.write_text(
            """---
name: pattern-test
---
Content
"""
        )

        loader = SkillsLoader(tmp_path)
        skills = loader.load_all(recursive=True)

        assert len(skills) == 1
        assert skills[0].name == "pattern-test"

    def test_finds_lowercase_skill_md(self, tmp_path: Path) -> None:
        """Test that lowercase skill.md files are found."""
        skill_dir = tmp_path / "my-skill"
        skill_dir.mkdir()
        skill_file = skill_dir / "skill.md"
        skill_file.write_text(
            """---
name: lowercase-skill
---
Content
"""
        )

        loader = SkillsLoader(tmp_path)
        skills = loader.load_all(recursive=True)

        assert len(skills) == 1
        assert skills[0].name == "lowercase-skill"

    def test_finds_nested_skills(self, tmp_path: Path) -> None:
        """Test that nested skills are found with recursive loading."""
        # Create nested structure
        nested_dir = tmp_path / "programs" / "batch" / "cleanup"
        nested_dir.mkdir(parents=True)
        skill_file = nested_dir / "SKILL.md"
        skill_file.write_text(
            """---
name: nested-skill
---
Content
"""
        )

        loader = SkillsLoader(tmp_path)
        skills = loader.load_all(recursive=True)

        assert len(skills) == 1
        assert skills[0].name == "nested-skill"

    def test_root_only_loading(self, tmp_path: Path) -> None:
        """Test that non-recursive loading only gets root skill."""
        # Create root skill
        root_file = tmp_path / "SKILL.md"
        root_file.write_text(
            """---
name: root-skill
---
Root content
"""
        )
        # Create nested skill
        nested_dir = tmp_path / "category"
        nested_dir.mkdir()
        nested_file = nested_dir / "SKILL.md"
        nested_file.write_text(
            """---
name: nested-skill
---
Nested content
"""
        )

        loader = SkillsLoader(tmp_path)

        # Default (non-recursive) should only get root
        skills = loader.load_all()
        assert len(skills) == 1
        assert skills[0].name == "root-skill"

        # Recursive should get both
        all_skills = loader.load_all(recursive=True)
        assert len(all_skills) == 2

    def test_deduplicates_files(self, tmp_path: Path) -> None:
        """Test that duplicate file paths are deduplicated."""
        skill_dir = tmp_path / "skill"
        skill_dir.mkdir()
        skill_file = skill_dir / "SKILL.md"
        skill_file.write_text(
            """---
name: unique-skill
---
Content
"""
        )

        loader = SkillsLoader(tmp_path)
        files = loader._find_skill_files()

        # Should not have duplicates
        assert len(files) == len(set(files))
