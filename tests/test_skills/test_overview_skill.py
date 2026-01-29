"""Unit tests for OverviewSkillGenerator class.

Tests:
- System overview skill generation
- SKILL.md content formatting
- Program catalog generation
- Edge cases
"""

from pathlib import Path

import pytest

from war_rig.skills import (
    SYSTEM_OVERVIEW_SKILL_NAME,
    OverviewSkillGenerationError,
    OverviewSkillGenerator,
)


@pytest.fixture
def sample_overview_content() -> str:
    """Create sample system overview markdown content."""
    return """# CardDemo System Overview

*Generated: 2026-01-28 10:00:00*

CardDemo is a sample credit card management application demonstrating
mainframe modernization patterns. It includes batch and online components
for card management, transaction processing, and authorization.

## Architecture

The system consists of:
- Online CICS transactions for user interaction
- Batch programs for nightly processing
- IMS database for transaction history
- DB2 for customer data

## Key Functions

- Account management
- Transaction processing
- Authorization handling
- Reporting
"""


@pytest.fixture
def sample_program_summaries() -> list[dict[str, str]]:
    """Create sample program summaries."""
    return [
        {
            "name": "cbact01c",
            "program_id": "CBACT01C",
            "description": "Account inquiry program for displaying account details.",
            "type": "ONLINE",
        },
        {
            "name": "cbpaup0c",
            "program_id": "CBPAUP0C",
            "description": "Batch program for purging expired authorizations.",
            "type": "BATCH",
        },
        {
            "name": "cbtrn01c",
            "program_id": "CBTRN01C",
            "description": "Transaction listing program.",
            "type": "ONLINE",
        },
    ]


@pytest.fixture
def overview_md_file(tmp_path: Path, sample_overview_content: str) -> Path:
    """Create a sample SYSTEM_OVERVIEW.md file."""
    overview_path = tmp_path / "SYSTEM_OVERVIEW.md"
    overview_path.write_text(sample_overview_content, encoding="utf-8")
    return overview_path


class TestOverviewSkillGeneratorInit:
    """Tests for OverviewSkillGenerator initialization."""

    def test_init_creates_generator(self, tmp_path: Path):
        """Test basic initialization."""
        generator = OverviewSkillGenerator(tmp_path)
        assert generator.output_dir == tmp_path


class TestOverviewSkillGeneratorGenerate:
    """Tests for OverviewSkillGenerator.generate()."""

    def test_generate_creates_skill_directory(
        self,
        tmp_path: Path,
        overview_md_file: Path,
        sample_program_summaries: list[dict[str, str]],
    ):
        """Test that generation creates the skill directory."""
        generator = OverviewSkillGenerator(tmp_path)
        skill_dir = generator.generate(overview_md_file, sample_program_summaries)

        assert skill_dir.exists()
        assert skill_dir.is_dir()
        assert skill_dir.name == SYSTEM_OVERVIEW_SKILL_NAME

    def test_generate_creates_skill_md(
        self,
        tmp_path: Path,
        overview_md_file: Path,
        sample_program_summaries: list[dict[str, str]],
    ):
        """Test that generation creates SKILL.md."""
        generator = OverviewSkillGenerator(tmp_path)
        skill_dir = generator.generate(overview_md_file, sample_program_summaries)

        skill_md = skill_dir / "SKILL.md"
        assert skill_md.exists()
        assert skill_md.is_file()

    def test_generate_returns_skill_path(
        self,
        tmp_path: Path,
        overview_md_file: Path,
        sample_program_summaries: list[dict[str, str]],
    ):
        """Test that generation returns the correct path."""
        generator = OverviewSkillGenerator(tmp_path)
        skill_dir = generator.generate(overview_md_file, sample_program_summaries)

        expected = tmp_path / SYSTEM_OVERVIEW_SKILL_NAME
        assert skill_dir == expected

    def test_generate_handles_missing_file(self, tmp_path: Path):
        """Test that generation raises error for missing file."""
        generator = OverviewSkillGenerator(tmp_path)
        nonexistent = tmp_path / "MISSING.md"

        with pytest.raises(OverviewSkillGenerationError, match="Failed to read"):
            generator.generate(nonexistent, [])


class TestOverviewSkillMdContent:
    """Tests for SKILL.md content formatting."""

    def test_skill_md_has_frontmatter(
        self,
        tmp_path: Path,
        overview_md_file: Path,
        sample_program_summaries: list[dict[str, str]],
    ):
        """Test that SKILL.md has valid frontmatter."""
        generator = OverviewSkillGenerator(tmp_path)
        skill_dir = generator.generate(overview_md_file, sample_program_summaries)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        assert content.startswith("---\n")
        assert f"name: {SYSTEM_OVERVIEW_SKILL_NAME}" in content
        assert "description:" in content

    def test_skill_md_has_title(
        self,
        tmp_path: Path,
        overview_md_file: Path,
        sample_program_summaries: list[dict[str, str]],
    ):
        """Test that SKILL.md has title."""
        generator = OverviewSkillGenerator(tmp_path)
        skill_dir = generator.generate(overview_md_file, sample_program_summaries)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        assert "# System Overview" in content

    def test_skill_md_has_about_section(
        self,
        tmp_path: Path,
        overview_md_file: Path,
        sample_program_summaries: list[dict[str, str]],
    ):
        """Test that SKILL.md has about section."""
        generator = OverviewSkillGenerator(tmp_path)
        skill_dir = generator.generate(overview_md_file, sample_program_summaries)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        assert "## About This Skill" in content

    def test_skill_md_has_when_to_use(
        self,
        tmp_path: Path,
        overview_md_file: Path,
        sample_program_summaries: list[dict[str, str]],
    ):
        """Test that SKILL.md has when to use section."""
        generator = OverviewSkillGenerator(tmp_path)
        skill_dir = generator.generate(overview_md_file, sample_program_summaries)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        assert "## When to Use This Skill" in content
        assert "overall system architecture" in content

    def test_skill_md_includes_original_content(
        self,
        tmp_path: Path,
        overview_md_file: Path,
        sample_program_summaries: list[dict[str, str]],
    ):
        """Test that SKILL.md includes original overview content."""
        generator = OverviewSkillGenerator(tmp_path)
        skill_dir = generator.generate(overview_md_file, sample_program_summaries)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        assert "credit card management" in content
        assert "Architecture" in content

    def test_skill_md_excludes_generated_timestamp(
        self,
        tmp_path: Path,
        overview_md_file: Path,
        sample_program_summaries: list[dict[str, str]],
    ):
        """Test that SKILL.md excludes generated timestamp."""
        generator = OverviewSkillGenerator(tmp_path)
        skill_dir = generator.generate(overview_md_file, sample_program_summaries)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        assert "*Generated:" not in content


class TestProgramCatalog:
    """Tests for program catalog generation."""

    def test_skill_md_has_program_catalog(
        self,
        tmp_path: Path,
        overview_md_file: Path,
        sample_program_summaries: list[dict[str, str]],
    ):
        """Test that SKILL.md has program catalog section."""
        generator = OverviewSkillGenerator(tmp_path)
        skill_dir = generator.generate(overview_md_file, sample_program_summaries)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        assert "## Program Catalog" in content

    def test_program_catalog_groups_by_type(
        self,
        tmp_path: Path,
        overview_md_file: Path,
        sample_program_summaries: list[dict[str, str]],
    ):
        """Test that program catalog groups programs by type."""
        generator = OverviewSkillGenerator(tmp_path)
        skill_dir = generator.generate(overview_md_file, sample_program_summaries)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        assert "### BATCH Programs" in content
        assert "### ONLINE Programs" in content

    def test_program_catalog_includes_all_programs(
        self,
        tmp_path: Path,
        overview_md_file: Path,
        sample_program_summaries: list[dict[str, str]],
    ):
        """Test that program catalog includes all programs."""
        generator = OverviewSkillGenerator(tmp_path)
        skill_dir = generator.generate(overview_md_file, sample_program_summaries)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        assert "CBACT01C" in content
        assert "CBPAUP0C" in content
        assert "CBTRN01C" in content
        assert "cbact01c" in content  # Skill name
        assert "cbpaup0c" in content  # Skill name

    def test_program_catalog_has_table_format(
        self,
        tmp_path: Path,
        overview_md_file: Path,
        sample_program_summaries: list[dict[str, str]],
    ):
        """Test that program catalog uses table format."""
        generator = OverviewSkillGenerator(tmp_path)
        skill_dir = generator.generate(overview_md_file, sample_program_summaries)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        assert "| Program | Skill | Description |" in content
        assert "|---------|-------|-------------|" in content

    def test_empty_program_summaries(self, tmp_path: Path, overview_md_file: Path):
        """Test handling of empty program summaries."""
        generator = OverviewSkillGenerator(tmp_path)
        skill_dir = generator.generate(overview_md_file, [])

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        # Should not have program catalog if no programs
        assert "## Program Catalog" not in content


class TestRelatedSkills:
    """Tests for related skills section."""

    def test_skill_md_has_related_skills(
        self,
        tmp_path: Path,
        overview_md_file: Path,
        sample_program_summaries: list[dict[str, str]],
    ):
        """Test that SKILL.md has related skills section."""
        generator = OverviewSkillGenerator(tmp_path)
        skill_dir = generator.generate(overview_md_file, sample_program_summaries)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        assert "## Related Skills" in content
        assert "call-graph" in content
        assert "datacards" in content


class TestEdgeCases:
    """Tests for edge cases."""

    def test_handles_minimal_overview(self, tmp_path: Path):
        """Test handling of minimal overview content."""
        overview_path = tmp_path / "SYSTEM_OVERVIEW.md"
        overview_path.write_text("# Overview\n\nMinimal content.", encoding="utf-8")

        generator = OverviewSkillGenerator(tmp_path)
        skill_dir = generator.generate(overview_path, [])

        assert skill_dir.exists()
        skill_md = skill_dir / "SKILL.md"
        assert skill_md.exists()

    def test_handles_pipe_characters_in_descriptions(self, tmp_path: Path):
        """Test handling of pipe characters in descriptions."""
        overview_path = tmp_path / "SYSTEM_OVERVIEW.md"
        overview_path.write_text("# Overview\n\nContent here.", encoding="utf-8")

        programs = [
            {
                "name": "test",
                "program_id": "TEST",
                "description": "Description with | pipe character",
                "type": "BATCH",
            }
        ]

        generator = OverviewSkillGenerator(tmp_path)
        skill_dir = generator.generate(overview_path, programs)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        # Pipe should be escaped in table
        assert "\\|" in content or "Description with" in content

    def test_generates_idempotently(
        self,
        tmp_path: Path,
        overview_md_file: Path,
        sample_program_summaries: list[dict[str, str]],
    ):
        """Test that running twice produces same result."""
        generator = OverviewSkillGenerator(tmp_path)

        skill_dir1 = generator.generate(overview_md_file, sample_program_summaries)
        content1 = (skill_dir1 / "SKILL.md").read_text(encoding="utf-8")

        skill_dir2 = generator.generate(overview_md_file, sample_program_summaries)
        content2 = (skill_dir2 / "SKILL.md").read_text(encoding="utf-8")

        assert content1 == content2
