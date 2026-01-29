"""Unit tests for DatacardSkillGenerator class.

Tests:
- Datacard skill generation
- SKILL.md content formatting
- Edge cases
"""

from pathlib import Path

import pytest

from war_rig.skills import (
    DATACARDS_SKILL_NAME,
    DatacardSkillGenerationError,
    DatacardSkillGenerator,
)


@pytest.fixture
def sample_datacards_content() -> str:
    """Create sample datacards markdown content."""
    return """# Datacards Catalog

*Generated: 2026-01-28 10:00:00*

This catalog documents the data structures used in the CardDemo system.

## CUSTOMER-RECORD

**Copybook:** CUSTCOPY

Layout for customer master record.

| Field | Type | Length | Description |
|-------|------|--------|-------------|
| CUST-ID | PIC 9(10) | 10 | Customer identifier |
| CUST-NAME | PIC X(30) | 30 | Customer name |
| CUST-ADDR | PIC X(50) | 50 | Customer address |

## TRANSACTION-RECORD

**Copybook:** TRANCOPY

Layout for transaction history record.

| Field | Type | Length | Description |
|-------|------|--------|-------------|
| TRAN-ID | PIC 9(12) | 12 | Transaction identifier |
| TRAN-AMT | PIC 9(9)V99 | 11 | Transaction amount |
| TRAN-DATE | PIC 9(8) | 8 | Transaction date (YYYYMMDD) |

## ACCOUNT-SUMMARY

**Copybook:** ACCTSUM

Layout for account summary segment.

| Field | Type | Length | Description |
|-------|------|--------|-------------|
| ACCT-ID | PIC 9(11) | 11 | Account number |
| ACCT-BAL | PIC S9(9)V99 | 11 | Account balance |
"""


@pytest.fixture
def datacards_md_file(tmp_path: Path, sample_datacards_content: str) -> Path:
    """Create a sample DATACARDS.md file."""
    datacards_path = tmp_path / "DATACARDS.md"
    datacards_path.write_text(sample_datacards_content, encoding="utf-8")
    return datacards_path


class TestDatacardSkillGeneratorInit:
    """Tests for DatacardSkillGenerator initialization."""

    def test_init_creates_generator(self, tmp_path: Path):
        """Test basic initialization."""
        generator = DatacardSkillGenerator(tmp_path)
        assert generator.output_dir == tmp_path


class TestDatacardSkillGeneratorGenerate:
    """Tests for DatacardSkillGenerator.generate()."""

    def test_generate_creates_skill_directory(
        self, tmp_path: Path, datacards_md_file: Path
    ):
        """Test that generation creates the skill directory."""
        generator = DatacardSkillGenerator(tmp_path)
        skill_dir = generator.generate(datacards_md_file)

        assert skill_dir.exists()
        assert skill_dir.is_dir()
        assert skill_dir.name == DATACARDS_SKILL_NAME

    def test_generate_creates_skill_md(self, tmp_path: Path, datacards_md_file: Path):
        """Test that generation creates SKILL.md."""
        generator = DatacardSkillGenerator(tmp_path)
        skill_dir = generator.generate(datacards_md_file)

        skill_md = skill_dir / "SKILL.md"
        assert skill_md.exists()
        assert skill_md.is_file()

    def test_generate_returns_skill_path(self, tmp_path: Path, datacards_md_file: Path):
        """Test that generation returns the correct path."""
        generator = DatacardSkillGenerator(tmp_path)
        skill_dir = generator.generate(datacards_md_file)

        expected = tmp_path / DATACARDS_SKILL_NAME
        assert skill_dir == expected

    def test_generate_handles_missing_file(self, tmp_path: Path):
        """Test that generation raises error for missing file."""
        generator = DatacardSkillGenerator(tmp_path)
        nonexistent = tmp_path / "MISSING.md"

        with pytest.raises(DatacardSkillGenerationError, match="Failed to read"):
            generator.generate(nonexistent)


class TestDatacardSkillMdContent:
    """Tests for SKILL.md content formatting."""

    def test_skill_md_has_frontmatter(self, tmp_path: Path, datacards_md_file: Path):
        """Test that SKILL.md has valid frontmatter."""
        generator = DatacardSkillGenerator(tmp_path)
        skill_dir = generator.generate(datacards_md_file)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        assert content.startswith("---\n")
        assert f"name: {DATACARDS_SKILL_NAME}" in content
        assert "description:" in content

    def test_skill_md_has_title(self, tmp_path: Path, datacards_md_file: Path):
        """Test that SKILL.md has title."""
        generator = DatacardSkillGenerator(tmp_path)
        skill_dir = generator.generate(datacards_md_file)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        assert "# Datacards Catalog" in content

    def test_skill_md_has_about_section(self, tmp_path: Path, datacards_md_file: Path):
        """Test that SKILL.md has about section."""
        generator = DatacardSkillGenerator(tmp_path)
        skill_dir = generator.generate(datacards_md_file)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        assert "## About This Skill" in content

    def test_skill_md_has_when_to_use(self, tmp_path: Path, datacards_md_file: Path):
        """Test that SKILL.md has when to use section."""
        generator = DatacardSkillGenerator(tmp_path)
        skill_dir = generator.generate(datacards_md_file)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        assert "## When to Use This Skill" in content
        assert "data structure" in content.lower()

    def test_skill_md_includes_data_structures(
        self, tmp_path: Path, datacards_md_file: Path
    ):
        """Test that SKILL.md includes the data structures."""
        generator = DatacardSkillGenerator(tmp_path)
        skill_dir = generator.generate(datacards_md_file)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        assert "CUSTOMER-RECORD" in content
        assert "TRANSACTION-RECORD" in content
        assert "ACCOUNT-SUMMARY" in content

    def test_skill_md_includes_field_tables(
        self, tmp_path: Path, datacards_md_file: Path
    ):
        """Test that SKILL.md includes field tables."""
        generator = DatacardSkillGenerator(tmp_path)
        skill_dir = generator.generate(datacards_md_file)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        assert "| Field | Type | Length | Description |" in content
        assert "CUST-ID" in content
        assert "TRAN-AMT" in content

    def test_skill_md_excludes_generated_timestamp(
        self, tmp_path: Path, datacards_md_file: Path
    ):
        """Test that SKILL.md excludes generated timestamp."""
        generator = DatacardSkillGenerator(tmp_path)
        skill_dir = generator.generate(datacards_md_file)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        assert "*Generated:" not in content


class TestRelatedSkills:
    """Tests for related skills section."""

    def test_skill_md_has_related_skills(self, tmp_path: Path, datacards_md_file: Path):
        """Test that SKILL.md has related skills section."""
        generator = DatacardSkillGenerator(tmp_path)
        skill_dir = generator.generate(datacards_md_file)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        assert "## Related Skills" in content
        assert "system-overview" in content
        assert "call-graph" in content


class TestDescriptionExtraction:
    """Tests for description extraction."""

    def test_extracts_datacard_count(self, tmp_path: Path, datacards_md_file: Path):
        """Test that description includes count of data structures."""
        generator = DatacardSkillGenerator(tmp_path)
        skill_dir = generator.generate(datacards_md_file)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        # Description should mention data structures
        lines = content.split("\n")
        for line in lines:
            if line.startswith("description:"):
                # Should have a count of data structures (3 in our sample)
                assert "3" in line or "data structure" in line.lower()
                break


class TestEdgeCases:
    """Tests for edge cases."""

    def test_handles_minimal_datacards(self, tmp_path: Path):
        """Test handling of minimal datacards content."""
        datacards_path = tmp_path / "DATACARDS.md"
        datacards_path.write_text(
            "# Datacards\n\nNo data structures documented yet.", encoding="utf-8"
        )

        generator = DatacardSkillGenerator(tmp_path)
        skill_dir = generator.generate(datacards_path)

        assert skill_dir.exists()
        skill_md = skill_dir / "SKILL.md"
        assert skill_md.exists()

    def test_handles_no_field_tables(self, tmp_path: Path):
        """Test handling of datacards without field tables."""
        datacards_path = tmp_path / "DATACARDS.md"
        datacards_path.write_text(
            "# Datacards\n\n## RECORD-A\n\nSimple record description.",
            encoding="utf-8",
        )

        generator = DatacardSkillGenerator(tmp_path)
        skill_dir = generator.generate(datacards_path)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        assert "RECORD-A" in content

    def test_handles_special_characters(self, tmp_path: Path):
        """Test handling of special characters in datacards."""
        datacards_path = tmp_path / "DATACARDS.md"
        datacards_path.write_text(
            "# Datacards\n\n## RECORD<TYPE>\n\nRecord with <special> & chars.",
            encoding="utf-8",
        )

        generator = DatacardSkillGenerator(tmp_path)
        skill_dir = generator.generate(datacards_path)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        # Content should be present
        assert "RECORD" in content

    def test_generates_idempotently(self, tmp_path: Path, datacards_md_file: Path):
        """Test that running twice produces same result."""
        generator = DatacardSkillGenerator(tmp_path)

        skill_dir1 = generator.generate(datacards_md_file)
        content1 = (skill_dir1 / "SKILL.md").read_text(encoding="utf-8")

        skill_dir2 = generator.generate(datacards_md_file)
        content2 = (skill_dir2 / "SKILL.md").read_text(encoding="utf-8")

        assert content1 == content2
