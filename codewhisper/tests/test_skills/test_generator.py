"""Tests for skill generator.

This module tests:
- SkillGeneratorConfig dataclass
- GenerationResult dataclass
- get_markdown_summary function
- SkillGenerator class
  - Initialization and validation
  - Category discovery
  - Top-level skill generation
  - Category skill generation
  - Full generation workflow
"""

from pathlib import Path

import pytest

from codewhisper.skills.generator import (
    CATEGORY_DESCRIPTIONS,
    CATEGORY_MAPPING,
    GenerationResult,
    SkillGenerator,
    SkillGeneratorConfig,
    get_markdown_summary,
)


class TestSkillGeneratorConfig:
    """Tests for the SkillGeneratorConfig dataclass."""

    def test_config_creation(self, tmp_path: Path) -> None:
        """Test creating a config with all fields."""
        docs_dir = tmp_path / "docs"
        output_dir = tmp_path / "skills"
        docs_dir.mkdir()

        config = SkillGeneratorConfig(
            docs_dir=docs_dir,
            output_dir=output_dir,
            relative_docs_path="../docs",
            system_name="Test System",
        )

        assert config.docs_dir == docs_dir
        assert config.output_dir == output_dir
        assert config.relative_docs_path == "../docs"
        assert config.system_name == "Test System"

    def test_config_defaults(self, tmp_path: Path) -> None:
        """Test config with default values."""
        docs_dir = tmp_path / "docs"
        output_dir = tmp_path / "skills"
        docs_dir.mkdir()

        config = SkillGeneratorConfig(
            docs_dir=docs_dir,
            output_dir=output_dir,
        )

        assert config.relative_docs_path == "../documentation"
        assert config.system_name == "System"


class TestGenerationResult:
    """Tests for the GenerationResult dataclass."""

    def test_result_defaults(self) -> None:
        """Test result with default values."""
        result = GenerationResult()

        assert result.top_level_created is False
        assert result.categories_created == []
        assert result.files_processed == 0
        assert result.errors == []

    def test_result_with_values(self) -> None:
        """Test result with populated values."""
        result = GenerationResult(
            top_level_created=True,
            categories_created=["cobol", "jcl"],
            files_processed=15,
            errors=["Error 1"],
        )

        assert result.top_level_created is True
        assert len(result.categories_created) == 2
        assert result.files_processed == 15
        assert len(result.errors) == 1


class TestGetMarkdownSummary:
    """Tests for the get_markdown_summary wrapper function.

    Note: This tests the wrapper around citadel.sdk.get_markdown_summary().
    Detailed paragraph extraction logic is tested in citadel's own test suite.
    """

    def test_extract_from_first_paragraph(self, tmp_path: Path) -> None:
        """Test extracting summary from first paragraph."""
        md_file = tmp_path / "test.md"
        md_file.write_text(
            """# TESTPROG

This is the main processing program for the system.

## Details

More information here.
"""
        )

        summary = get_markdown_summary(md_file)

        assert summary is not None
        assert "main processing program" in summary

    def test_truncates_long_summary(self, tmp_path: Path) -> None:
        """Test that long summaries are truncated."""
        md_file = tmp_path / "test.md"
        long_text = "A" * 300
        md_file.write_text(f"# TESTPROG\n\n{long_text}\n")

        summary = get_markdown_summary(md_file)

        assert summary is not None
        assert len(summary) <= 200
        assert summary.endswith("...")

    def test_returns_none_for_nonexistent_file(self, tmp_path: Path) -> None:
        """Test that nonexistent file returns None."""
        nonexistent = tmp_path / "nonexistent.md"

        summary = get_markdown_summary(nonexistent)

        assert summary is None

    def test_returns_none_for_empty_summary(self, tmp_path: Path) -> None:
        """Test that empty summary returns None."""
        md_file = tmp_path / "test.md"
        # File with only a heading and no content
        md_file.write_text("# TESTPROG\n")

        summary = get_markdown_summary(md_file)

        assert summary is None


class TestSkillGenerator:
    """Tests for the SkillGenerator class."""

    @pytest.fixture
    def sample_docs_dir(self, tmp_path: Path) -> Path:
        """Create a sample documentation directory structure."""
        docs_dir = tmp_path / "documentation"
        docs_dir.mkdir()

        # Create README.md with executive summary
        readme = docs_dir / "README.md"
        readme.write_text(
            """# System Design Document

## 1. Executive Summary

This system manages authorization data for financial transactions.
It processes authorization requests, validates them against databases,
and records decisions for auditing.

The major capabilities include batch processing and online transaction handling.

### 2. Architecture Overview

Details about architecture...
"""
        )

        # Create cbl directory with COBOL docs
        cbl_dir = docs_dir / "cbl"
        cbl_dir.mkdir()

        (cbl_dir / "TESTPROG.cbl.md").write_text(
            """# TESTPROG

Main batch processing program for authorization cleanup.

## Purpose

This program handles batch processing of authorization records.

## Inputs

| Name | Type |
|------|------|
| INPUT | File |
"""
        )

        (cbl_dir / "AUTHPROC.cbl.md").write_text(
            """# AUTHPROC

## Purpose

Processes authorization requests from the queue.
"""
        )

        # Create jcl directory with JCL docs
        jcl_dir = docs_dir / "jcl"
        jcl_dir.mkdir()

        (jcl_dir / "RUNJOB.jcl.md").write_text(
            """# RUNJOB

## Purpose

JCL to execute the batch authorization job.
"""
        )

        # Create cpy directory with copybook docs
        cpy_dir = docs_dir / "cpy"
        cpy_dir.mkdir()

        (cpy_dir / "AUTHCPY.cpy.md").write_text(
            """# AUTHCPY

## Purpose

Copybook containing authorization data structures.
"""
        )

        return docs_dir

    def test_init_valid_directory(self, sample_docs_dir: Path, tmp_path: Path) -> None:
        """Test generator initialization with valid directory."""
        config = SkillGeneratorConfig(
            docs_dir=sample_docs_dir,
            output_dir=tmp_path / "skills",
        )
        generator = SkillGenerator(config)

        assert generator.config.docs_dir == sample_docs_dir

    def test_init_nonexistent_directory(self, tmp_path: Path) -> None:
        """Test generator raises error for nonexistent docs directory."""
        config = SkillGeneratorConfig(
            docs_dir=tmp_path / "nonexistent",
            output_dir=tmp_path / "skills",
        )

        with pytest.raises(ValueError) as exc_info:
            SkillGenerator(config)

        assert "does not exist" in str(exc_info.value)

    def test_init_file_not_directory(self, tmp_path: Path) -> None:
        """Test generator raises error when docs path is a file."""
        file_path = tmp_path / "not_a_directory.txt"
        file_path.touch()

        config = SkillGeneratorConfig(
            docs_dir=file_path,
            output_dir=tmp_path / "skills",
        )

        with pytest.raises(ValueError) as exc_info:
            SkillGenerator(config)

        assert "not a directory" in str(exc_info.value)

    def test_discover_categories(self, sample_docs_dir: Path, tmp_path: Path) -> None:
        """Test category discovery from docs structure."""
        config = SkillGeneratorConfig(
            docs_dir=sample_docs_dir,
            output_dir=tmp_path / "skills",
        )
        generator = SkillGenerator(config)

        categories = generator._discover_categories()

        assert "cbl" in categories
        assert categories["cbl"] == "cobol"
        assert "jcl" in categories
        assert categories["jcl"] == "jcl"
        assert "cpy" in categories
        assert categories["cpy"] == "copybook"

    def test_discover_categories_skips_hidden_dirs(
        self, sample_docs_dir: Path, tmp_path: Path
    ) -> None:
        """Test that hidden directories are skipped."""
        # Create a hidden directory
        hidden_dir = sample_docs_dir / ".hidden"
        hidden_dir.mkdir()
        (hidden_dir / "test.md").write_text("# Hidden")

        # Create a cache directory
        cache_dir = sample_docs_dir / "cache"
        cache_dir.mkdir()
        (cache_dir / "test.md").write_text("# Cache")

        config = SkillGeneratorConfig(
            docs_dir=sample_docs_dir,
            output_dir=tmp_path / "skills",
        )
        generator = SkillGenerator(config)

        categories = generator._discover_categories()

        assert ".hidden" not in categories
        assert "cache" not in categories

    def test_generate_creates_output_directory(
        self, sample_docs_dir: Path, tmp_path: Path
    ) -> None:
        """Test that generate creates the output directory."""
        output_dir = tmp_path / "skills" / "nested" / "path"

        config = SkillGeneratorConfig(
            docs_dir=sample_docs_dir,
            output_dir=output_dir,
        )
        generator = SkillGenerator(config)

        generator.generate()

        assert output_dir.exists()

    def test_generate_creates_top_level_skill(
        self, sample_docs_dir: Path, tmp_path: Path
    ) -> None:
        """Test that top-level SKILL.md is created."""
        output_dir = tmp_path / "skills"

        config = SkillGeneratorConfig(
            docs_dir=sample_docs_dir,
            output_dir=output_dir,
            system_name="Authorization System",
        )
        generator = SkillGenerator(config)

        result = generator.generate()

        assert result.top_level_created is True
        skill_path = output_dir / "SKILL.md"
        assert skill_path.exists()

        content = skill_path.read_text()
        assert "name: system-overview" in content
        assert "Authorization System" in content

    def test_generate_extracts_executive_summary(
        self, sample_docs_dir: Path, tmp_path: Path
    ) -> None:
        """Test that executive summary is extracted from README."""
        output_dir = tmp_path / "skills"

        config = SkillGeneratorConfig(
            docs_dir=sample_docs_dir,
            output_dir=output_dir,
        )
        generator = SkillGenerator(config)

        generator.generate()

        content = (output_dir / "SKILL.md").read_text()
        assert "authorization data" in content

    def test_generate_creates_category_skills(
        self, sample_docs_dir: Path, tmp_path: Path
    ) -> None:
        """Test that category SKILL.md files are created."""
        output_dir = tmp_path / "skills"

        config = SkillGeneratorConfig(
            docs_dir=sample_docs_dir,
            output_dir=output_dir,
        )
        generator = SkillGenerator(config)

        result = generator.generate()

        assert "cobol" in result.categories_created
        assert "jcl" in result.categories_created
        assert "copybook" in result.categories_created

        # Check COBOL skill
        cobol_skill = output_dir / "cobol" / "SKILL.md"
        assert cobol_skill.exists()
        content = cobol_skill.read_text()
        assert "name: cobol" in content
        assert "TESTPROG" in content
        assert "batch processing" in content.lower()

    def test_generate_category_skill_format(
        self, sample_docs_dir: Path, tmp_path: Path
    ) -> None:
        """Test category skill markdown format."""
        output_dir = tmp_path / "skills"

        config = SkillGeneratorConfig(
            docs_dir=sample_docs_dir,
            output_dir=output_dir,
            relative_docs_path="../documentation",
        )
        generator = SkillGenerator(config)

        generator.generate()

        content = (output_dir / "cobol" / "SKILL.md").read_text()

        # Check YAML frontmatter
        assert content.startswith("---")
        assert "name: cobol" in content
        assert "description:" in content

        # Check table format
        assert "| Program |" in content or "| Name |" in content
        assert "|---------|" in content

        # Check links
        assert "[Full docs](" in content
        assert "../documentation/cbl/" in content

    def test_generate_returns_result(
        self, sample_docs_dir: Path, tmp_path: Path
    ) -> None:
        """Test that generate returns proper result."""
        output_dir = tmp_path / "skills"

        config = SkillGeneratorConfig(
            docs_dir=sample_docs_dir,
            output_dir=output_dir,
        )
        generator = SkillGenerator(config)

        result = generator.generate()

        assert result.top_level_created is True
        assert len(result.categories_created) >= 3
        assert result.files_processed >= 4
        assert len(result.errors) == 0

    def test_generate_handles_empty_category(self, tmp_path: Path) -> None:
        """Test handling of empty category directory."""
        docs_dir = tmp_path / "documentation"
        docs_dir.mkdir()

        # Create empty cbl directory
        cbl_dir = docs_dir / "cbl"
        cbl_dir.mkdir()

        output_dir = tmp_path / "skills"

        config = SkillGeneratorConfig(
            docs_dir=docs_dir,
            output_dir=output_dir,
        )
        generator = SkillGenerator(config)

        result = generator.generate()

        # Empty category should not be in created list
        assert "cobol" not in result.categories_created

    def test_extract_program_name(
        self, sample_docs_dir: Path, tmp_path: Path
    ) -> None:
        """Test program name extraction from file names."""
        config = SkillGeneratorConfig(
            docs_dir=sample_docs_dir,
            output_dir=tmp_path / "skills",
        )
        generator = SkillGenerator(config)

        # Test various file name formats
        assert generator._extract_program_name(Path("TESTPROG.cbl.md")) == "TESTPROG"
        assert generator._extract_program_name(Path("MYJOB.JCL.md")) == "MYJOB"
        assert generator._extract_program_name(Path("AUTHCPY.cpy.md")) == "AUTHCPY"
        assert generator._extract_program_name(Path("SCREEN.bms.md")) == "SCREEN"
        assert generator._extract_program_name(Path("TABLE.ddl.md")) == "TABLE"
        assert generator._extract_program_name(Path("MYPSB.psb.md")) == "MYPSB"
        assert generator._extract_program_name(Path("MYDBD.dbd.md")) == "MYDBD"


class TestCategoryMappings:
    """Tests for category mapping constants."""

    def test_all_mappings_have_descriptions(self) -> None:
        """Test that all mapped categories have descriptions."""
        for category_name in CATEGORY_MAPPING.values():
            assert category_name in CATEGORY_DESCRIPTIONS

    def test_expected_mappings_exist(self) -> None:
        """Test that expected mappings are present."""
        assert CATEGORY_MAPPING["cbl"] == "cobol"
        assert CATEGORY_MAPPING["jcl"] == "jcl"
        assert CATEGORY_MAPPING["cpy"] == "copybook"
        assert CATEGORY_MAPPING["cpy-bms"] == "bms-copybook"
        assert CATEGORY_MAPPING["bms"] == "bms"
        assert CATEGORY_MAPPING["ddl"] == "ddl"
        assert CATEGORY_MAPPING["ims"] == "ims"


class TestSkillGeneratorIntegration:
    """Integration tests using real-like documentation structure."""

    def test_full_generation_workflow(self, tmp_path: Path) -> None:
        """Test complete generation workflow end-to-end."""
        # Create documentation structure similar to real output
        docs_dir = tmp_path / "documentation"
        docs_dir.mkdir()

        # README with executive summary
        (docs_dir / "README.md").write_text(
            """# System Design Document

## 1. Executive Summary

This is a card authorization system that processes credit card transactions.
It validates cards, checks limits, and records authorization decisions.

The system uses COBOL for batch processing and CICS for online transactions.

### 2. Architecture Overview

The system follows a layered architecture.
"""
        )

        # COBOL programs
        cbl = docs_dir / "cbl"
        cbl.mkdir()
        (cbl / "AUTHPROC.cbl.md").write_text(
            """# AUTHPROC

## Purpose

Main authorization processing program that validates transactions.
"""
        )
        (cbl / "BATCHCLEAN.CBL.md").write_text(
            """# BATCHCLEAN

## Purpose

Batch cleanup program for expired authorizations.
"""
        )

        # JCL jobs
        jcl = docs_dir / "jcl"
        jcl.mkdir()
        (jcl / "DAILYRUN.JCL.md").write_text(
            """# DAILYRUN

## Purpose

Daily batch job to process accumulated transactions.
"""
        )

        # Copybooks
        cpy = docs_dir / "cpy"
        cpy.mkdir()
        (cpy / "AUTHDATA.cpy.md").write_text(
            """# AUTHDATA

## Purpose

Data structures for authorization records.
"""
        )

        # IMS
        ims = docs_dir / "ims"
        ims.mkdir()
        (ims / "AUTHDB.dbd.md").write_text(
            """# AUTHDB

## Purpose

Database definition for authorization storage.
"""
        )

        # Generate skills
        output_dir = tmp_path / "code-skills"
        config = SkillGeneratorConfig(
            docs_dir=docs_dir,
            output_dir=output_dir,
            relative_docs_path="../documentation",
            system_name="Card Authorization System",
        )
        generator = SkillGenerator(config)
        result = generator.generate()

        # Verify result
        assert result.top_level_created is True
        assert result.files_processed == 5
        assert len(result.errors) == 0
        assert set(result.categories_created) == {"cobol", "jcl", "copybook", "ims"}

        # Verify top-level skill
        top_skill = output_dir / "SKILL.md"
        assert top_skill.exists()
        top_content = top_skill.read_text()
        assert "Card Authorization System" in top_content
        assert "credit card transactions" in top_content
        assert "[COBOL](cobol/SKILL.md)" in top_content

        # Verify category skills
        cobol_skill = output_dir / "cobol" / "SKILL.md"
        assert cobol_skill.exists()
        cobol_content = cobol_skill.read_text()
        assert "AUTHPROC" in cobol_content
        assert "BATCHCLEAN" in cobol_content
        assert "[Full docs](../documentation/cbl/" in cobol_content

        jcl_skill = output_dir / "jcl" / "SKILL.md"
        assert jcl_skill.exists()
        jcl_content = jcl_skill.read_text()
        assert "DAILYRUN" in jcl_content
