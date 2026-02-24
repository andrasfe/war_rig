"""Unit tests for SkillsGenerator class.

Tests:
- SkillsGenerator initialization
- Input directory validation
- Output directory naming
- Category discovery
- Skill generation
"""

import json
from pathlib import Path

import pytest

from war_rig.skills import (
    CATEGORY_MAPPING,
    GenerationResult,
    InputDirectoryNotFoundError,
    InvalidInputDirectoryError,
    SkillsGenerator,
    SkillsGeneratorError,
    get_markdown_summary,
)
from war_rig.skills.generator import _load_file_summary, format_program_section


class TestSkillsGeneratorInit:
    """Tests for SkillsGenerator initialization."""

    def test_init_with_valid_directory(self, tmp_path: Path):
        """Test initialization with a valid documentation directory."""
        # Create minimal documentation structure
        cbl_dir = tmp_path / "cbl"
        cbl_dir.mkdir()
        (cbl_dir / "TESTPROG.cbl.md").write_text("# TESTPROG\n\nTest program.")

        generator = SkillsGenerator(tmp_path)

        assert generator.input_dir == tmp_path
        assert generator.output_dir == tmp_path.parent / f"skills-{tmp_path.name}"

    def test_init_with_custom_output_dir(self, tmp_path: Path):
        """Test initialization with custom output directory."""
        input_dir = tmp_path / "documentation"
        output_dir = tmp_path / "custom-skills"

        # Create minimal structure
        cbl_dir = input_dir / "cbl"
        cbl_dir.mkdir(parents=True)
        (cbl_dir / "TEST.cbl.md").write_text("# TEST\n\nTest.")

        generator = SkillsGenerator(input_dir, output_dir=output_dir)

        assert generator.output_dir == output_dir

    def test_init_nonexistent_directory_raises(self, tmp_path: Path):
        """Test that nonexistent input directory raises error."""
        nonexistent = tmp_path / "does-not-exist"

        with pytest.raises(InputDirectoryNotFoundError, match="does not exist"):
            SkillsGenerator(nonexistent)

    def test_init_file_instead_of_directory_raises(self, tmp_path: Path):
        """Test that file path instead of directory raises error."""
        file_path = tmp_path / "somefile.txt"
        file_path.write_text("content")

        with pytest.raises(InvalidInputDirectoryError, match="not a directory"):
            SkillsGenerator(file_path)

    def test_init_with_system_name(self, tmp_path: Path):
        """Test initialization with custom system name."""
        cbl_dir = tmp_path / "cbl"
        cbl_dir.mkdir()
        (cbl_dir / "TEST.cbl.md").write_text("# TEST\n\nTest.")

        generator = SkillsGenerator(tmp_path, system_name="CardDemo")

        assert generator.system_name == "CardDemo"


class TestSkillsGeneratorGenerate:
    """Tests for SkillsGenerator.generate() method."""

    def test_generate_creates_output_directory(self, tmp_path: Path):
        """Test that generate creates the output directory."""
        input_dir = tmp_path / "documentation"
        cbl_dir = input_dir / "cbl"
        cbl_dir.mkdir(parents=True)
        (cbl_dir / "TEST.cbl.md").write_text("# TEST\n\nTest program.")

        generator = SkillsGenerator(input_dir)
        result = generator.generate()

        assert result.exists()
        assert result.is_dir()

    def test_generate_creates_category_subdirectories(self, tmp_path: Path):
        """Test that generate creates category subdirectories."""
        input_dir = tmp_path / "documentation"

        # Create multiple category directories
        for subdir in ["cbl", "jcl", "cpy"]:
            cat_dir = input_dir / subdir
            cat_dir.mkdir(parents=True)
            (cat_dir / f"TEST.{subdir}.md").write_text(f"# TEST\n\nTest {subdir}.")

        generator = SkillsGenerator(input_dir)
        result = generator.generate()

        # Check mapped category names
        assert (result / "cobol").is_dir()
        assert (result / "jcl").is_dir()
        assert (result / "copybook").is_dir()

    def test_generate_creates_top_level_skill(self, tmp_path: Path):
        """Test that generate creates top-level SKILL.md."""
        input_dir = tmp_path / "documentation"
        cbl_dir = input_dir / "cbl"
        cbl_dir.mkdir(parents=True)
        (cbl_dir / "TEST.cbl.md").write_text("# TEST\n\nTest program.")

        generator = SkillsGenerator(input_dir)
        result = generator.generate()

        skill_file = result / "SKILL.md"
        assert skill_file.exists()
        content = skill_file.read_text()
        assert "system-overview" in content
        assert "Categories" in content

    def test_generate_creates_category_skill_files(self, tmp_path: Path):
        """Test that generate creates SKILL.md in each category."""
        input_dir = tmp_path / "documentation"
        cbl_dir = input_dir / "cbl"
        cbl_dir.mkdir(parents=True)
        (cbl_dir / "PROG1.cbl.md").write_text("# PROG1\n\nFirst program.")
        (cbl_dir / "PROG2.cbl.md").write_text("# PROG2\n\nSecond program.")

        generator = SkillsGenerator(input_dir)
        result = generator.generate()

        skill_file = result / "cobol" / "SKILL.md"
        assert skill_file.exists()
        content = skill_file.read_text()
        assert "PROG1" in content
        assert "PROG2" in content
        assert "Full docs" in content

    def test_generate_returns_output_path(self, tmp_path: Path):
        """Test that generate returns the output directory path."""
        input_dir = tmp_path / "documentation"
        cbl_dir = input_dir / "cbl"
        cbl_dir.mkdir(parents=True)
        (cbl_dir / "TEST.cbl.md").write_text("# TEST\n\nTest.")

        generator = SkillsGenerator(input_dir)
        result = generator.generate()

        assert result == generator.output_dir

    def test_generate_with_result_returns_details(self, tmp_path: Path):
        """Test that generate_with_result returns GenerationResult."""
        input_dir = tmp_path / "documentation"

        cbl_dir = input_dir / "cbl"
        cbl_dir.mkdir(parents=True)
        (cbl_dir / "PROG1.cbl.md").write_text("# PROG1\n\nFirst.")
        (cbl_dir / "PROG2.cbl.md").write_text("# PROG2\n\nSecond.")

        jcl_dir = input_dir / "jcl"
        jcl_dir.mkdir(parents=True)
        (jcl_dir / "JOB1.jcl.md").write_text("# JOB1\n\nJob.")

        generator = SkillsGenerator(input_dir)
        result = generator.generate_with_result()

        assert isinstance(result, GenerationResult)
        assert result.top_level_created is True
        assert "cobol" in result.categories_created
        assert "jcl" in result.categories_created
        assert result.files_processed == 3
        assert result.errors == []

    def test_generate_skips_empty_categories(self, tmp_path: Path):
        """Test that empty category directories are skipped."""
        input_dir = tmp_path / "documentation"

        # Create a category with files
        cbl_dir = input_dir / "cbl"
        cbl_dir.mkdir(parents=True)
        (cbl_dir / "TEST.cbl.md").write_text("# TEST\n\nTest.")

        # Create an empty category
        empty_dir = input_dir / "jcl"
        empty_dir.mkdir()

        generator = SkillsGenerator(input_dir)
        result = generator.generate_with_result()

        assert "cobol" in result.categories_created
        assert "jcl" not in result.categories_created


class TestCategoryDiscovery:
    """Tests for category discovery."""

    def test_discover_categories_maps_known_dirs(self, tmp_path: Path):
        """Test that known directory names are mapped to friendly names."""
        for docs_name, expected_name in CATEGORY_MAPPING.items():
            cat_dir = tmp_path / docs_name
            cat_dir.mkdir(exist_ok=True)
            (cat_dir / "TEST.md").write_text("# TEST\n\nTest.")

        generator = SkillsGenerator(tmp_path)
        categories = generator._discover_categories()

        for docs_name, expected_name in CATEGORY_MAPPING.items():
            assert categories.get(docs_name) == expected_name

    def test_discover_categories_skips_hidden_dirs(self, tmp_path: Path):
        """Test that hidden directories are skipped."""
        hidden_dir = tmp_path / ".hidden"
        hidden_dir.mkdir()
        (hidden_dir / "TEST.md").write_text("# TEST\n\nTest.")

        cbl_dir = tmp_path / "cbl"
        cbl_dir.mkdir()
        (cbl_dir / "TEST.md").write_text("# TEST\n\nTest.")

        generator = SkillsGenerator(tmp_path)
        categories = generator._discover_categories()

        assert ".hidden" not in categories
        assert "cbl" in categories

    def test_discover_categories_skips_cache_dir(self, tmp_path: Path):
        """Test that cache directory is skipped."""
        cache_dir = tmp_path / "cache"
        cache_dir.mkdir()
        (cache_dir / "TEST.md").write_text("# TEST\n\nTest.")

        cbl_dir = tmp_path / "cbl"
        cbl_dir.mkdir()
        (cbl_dir / "TEST.md").write_text("# TEST\n\nTest.")

        generator = SkillsGenerator(tmp_path)
        categories = generator._discover_categories()

        assert "cache" not in categories
        assert "cbl" in categories

    def test_discover_categories_uses_dir_name_for_unknown(self, tmp_path: Path):
        """Test that unknown directories use their name as category."""
        custom_dir = tmp_path / "custom-type"
        custom_dir.mkdir()
        (custom_dir / "TEST.md").write_text("# TEST\n\nTest.")

        generator = SkillsGenerator(tmp_path)
        categories = generator._discover_categories()

        assert categories.get("custom-type") == "custom-type"


class TestMarkdownSummary:
    """Tests for get_markdown_summary function."""

    def test_extracts_first_paragraph(self, tmp_path: Path):
        """Test that first paragraph is extracted as summary."""
        md_file = tmp_path / "test.md"
        md_file.write_text("# Title\n\nThis is the first paragraph.\n\nSecond paragraph.")

        summary = get_markdown_summary(md_file)

        assert summary is not None
        assert "first paragraph" in summary

    def test_skips_yaml_frontmatter(self, tmp_path: Path):
        """Test that YAML frontmatter is skipped."""
        md_file = tmp_path / "test.md"
        md_file.write_text("---\nname: test\n---\n\n# Title\n\nActual content here.")

        summary = get_markdown_summary(md_file)

        assert summary is not None
        assert "name:" not in summary
        assert "Actual content" in summary

    def test_truncates_long_summaries(self, tmp_path: Path):
        """Test that long summaries are truncated."""
        md_file = tmp_path / "test.md"
        long_text = "# Title\n\n" + "This is a very long paragraph. " * 50
        md_file.write_text(long_text)

        summary = get_markdown_summary(md_file, max_chars=100)

        assert summary is not None
        assert len(summary) <= 103  # max_chars + "..."

    def test_returns_none_for_nonexistent_file(self, tmp_path: Path):
        """Test that None is returned for nonexistent file."""
        md_file = tmp_path / "nonexistent.md"

        summary = get_markdown_summary(md_file)

        assert summary is None


class TestSkillContent:
    """Tests for generated skill content."""

    def test_top_level_skill_has_frontmatter(self, tmp_path: Path):
        """Test that top-level skill has proper frontmatter."""
        input_dir = tmp_path / "documentation"
        cbl_dir = input_dir / "cbl"
        cbl_dir.mkdir(parents=True)
        (cbl_dir / "TEST.cbl.md").write_text("# TEST\n\nTest.")

        generator = SkillsGenerator(input_dir, system_name="TestSystem")
        result = generator.generate()

        content = (result / "SKILL.md").read_text()
        assert "---" in content
        assert "name: system-overview" in content
        assert "TestSystem" in content

    def test_category_skill_has_table(self, tmp_path: Path):
        """Test that category skill has markdown table."""
        input_dir = tmp_path / "documentation"
        cbl_dir = input_dir / "cbl"
        cbl_dir.mkdir(parents=True)
        (cbl_dir / "PROG1.cbl.md").write_text("# PROG1\n\nProgram one.")
        (cbl_dir / "PROG2.cbl.md").write_text("# PROG2\n\nProgram two.")

        generator = SkillsGenerator(input_dir)
        result = generator.generate()

        content = (result / "cobol" / "SKILL.md").read_text()
        assert "|" in content  # Has table
        assert "Program" in content  # Has header
        assert "PROG1" in content
        assert "PROG2" in content

    def test_category_skill_has_doc_links(self, tmp_path: Path):
        """Test that category skill has links to documentation."""
        input_dir = tmp_path / "documentation"
        cbl_dir = input_dir / "cbl"
        cbl_dir.mkdir(parents=True)
        (cbl_dir / "TEST.cbl.md").write_text("# TEST\n\nTest program.")

        generator = SkillsGenerator(input_dir, relative_docs_path="../docs")
        result = generator.generate()

        content = (result / "cobol" / "SKILL.md").read_text()
        assert "../docs/cbl/TEST.cbl.md" in content

    def test_extracts_executive_summary_from_readme(self, tmp_path: Path):
        """Test that executive summary is extracted from README.md."""
        input_dir = tmp_path / "documentation"
        cbl_dir = input_dir / "cbl"
        cbl_dir.mkdir(parents=True)
        (cbl_dir / "TEST.cbl.md").write_text("# TEST\n\nTest.")

        readme = input_dir / "README.md"
        readme.write_text(
            "# System\n\n## Executive Summary\n\nThis is the executive summary.\n\n## Other Section"
        )

        generator = SkillsGenerator(input_dir)
        result = generator.generate()

        content = (result / "SKILL.md").read_text()
        assert "executive summary" in content.lower()


class TestSkillsGeneratorErrors:
    """Tests for SkillsGenerator error handling."""

    def test_error_inheritance(self):
        """Test that specific errors inherit from base error."""
        assert issubclass(InputDirectoryNotFoundError, SkillsGeneratorError)
        assert issubclass(InvalidInputDirectoryError, SkillsGeneratorError)

    def test_error_messages_are_descriptive(self, tmp_path: Path):
        """Test that error messages contain useful information."""
        nonexistent = tmp_path / "nonexistent"

        with pytest.raises(InputDirectoryNotFoundError) as exc_info:
            SkillsGenerator(nonexistent)

        assert str(nonexistent) in str(exc_info.value)


class TestSkillsGeneratorIntegration:
    """Integration tests for SkillsGenerator."""

    def test_full_workflow_with_multiple_categories(self, tmp_path: Path):
        """Test complete workflow with multiple categories."""
        input_dir = tmp_path / "documentation"

        # Create COBOL programs
        cbl_dir = input_dir / "cbl"
        cbl_dir.mkdir(parents=True)
        (cbl_dir / "CBACT01C.cbl.md").write_text("# CBACT01C\n\nAccount processing.")
        (cbl_dir / "CBPAUP0C.cbl.md").write_text("# CBPAUP0C\n\nPayment processing.")

        # Create JCL jobs
        jcl_dir = input_dir / "jcl"
        jcl_dir.mkdir()
        (jcl_dir / "DAILYJOB.jcl.md").write_text("# DAILYJOB\n\nDaily batch job.")

        # Create IMS definitions
        ims_dir = input_dir / "ims"
        ims_dir.mkdir()
        (ims_dir / "CUSTDB.dbd.md").write_text("# CUSTDB\n\nCustomer database.")

        # Create README with executive summary
        (input_dir / "README.md").write_text(
            "# CardDemo\n\n## Executive Summary\n\nCardDemo is a credit card demo app.\n\n## Details"
        )

        generator = SkillsGenerator(input_dir, system_name="CardDemo")
        result = generator.generate_with_result()

        # Verify result
        assert result.top_level_created is True
        assert set(result.categories_created) == {"cobol", "jcl", "ims"}
        assert result.files_processed == 4
        assert result.errors == []

        # Verify output structure
        output_dir = result.output_dir
        assert output_dir is not None
        assert (output_dir / "SKILL.md").exists()
        assert (output_dir / "cobol" / "SKILL.md").exists()
        assert (output_dir / "jcl" / "SKILL.md").exists()
        assert (output_dir / "ims" / "SKILL.md").exists()

        # Verify content
        top_content = (output_dir / "SKILL.md").read_text()
        assert "CardDemo" in top_content
        assert "credit card demo" in top_content.lower()
        assert "COBOL" in top_content
        assert "JCL" in top_content

    def test_idempotent_generation(self, tmp_path: Path):
        """Test that running generate twice is safe."""
        input_dir = tmp_path / "documentation"
        cbl_dir = input_dir / "cbl"
        cbl_dir.mkdir(parents=True)
        (cbl_dir / "TEST.cbl.md").write_text("# TEST\n\nTest.")

        generator = SkillsGenerator(input_dir)

        # Run twice
        result1 = generator.generate()
        result2 = generator.generate()

        assert result1 == result2
        assert result1.exists()


def _write_summary_json(md_file: Path, summary_data: dict) -> Path:
    """Helper: write a .summary.json file alongside a .md file."""
    summary_path = md_file.parent / (md_file.stem + ".summary.json")
    summary_path.write_text(json.dumps(summary_data), encoding="utf-8")
    return summary_path


class TestLoadFileSummary:
    """Tests for _load_file_summary function."""

    def test_returns_none_when_no_summary_file(self, tmp_path: Path):
        md_file = tmp_path / "TESTPROG.cbl.md"
        md_file.write_text("# TESTPROG\n\nTest.")

        assert _load_file_summary(md_file) is None

    def test_loads_valid_summary(self, tmp_path: Path):
        md_file = tmp_path / "TESTPROG.cbl.md"
        md_file.write_text("# TESTPROG\n\nTest.")

        _write_summary_json(md_file, {
            "program_id": "TESTPROG",
            "file_name": "TESTPROG.cbl",
            "business_function": "Manages customer accounts",
            "primary_data_flows": ["Read ACCTFILE", "Write RPTFILE"],
        })

        result = _load_file_summary(md_file)
        assert result is not None
        assert result.business_function == "Manages customer accounts"
        assert len(result.primary_data_flows) == 2

    def test_returns_none_on_invalid_json(self, tmp_path: Path):
        md_file = tmp_path / "TESTPROG.cbl.md"
        md_file.write_text("# TESTPROG\n\nTest.")

        summary_path = md_file.parent / "TESTPROG.cbl.summary.json"
        summary_path.write_text("not valid json {{{")

        assert _load_file_summary(md_file) is None


class TestFormatProgramSection:
    """Tests for format_program_section function."""

    def test_returns_none_when_no_summary(self, tmp_path: Path):
        md_file = tmp_path / "TESTPROG.cbl.md"
        md_file.write_text("# TESTPROG\n\nFallback.")

        result = format_program_section("TESTPROG", md_file, "../docs/cbl/TESTPROG.cbl.md")
        assert result is None

    def test_returns_none_when_empty_business_function(self, tmp_path: Path):
        md_file = tmp_path / "TESTPROG.cbl.md"
        md_file.write_text("# TESTPROG\n\nFallback.")
        _write_summary_json(md_file, {
            "program_id": "TESTPROG",
            "file_name": "TESTPROG.cbl",
            "business_function": "",
        })

        result = format_program_section("TESTPROG", md_file, "../docs/cbl/TESTPROG.cbl.md")
        assert result is None

    def test_includes_business_function_as_heading(self, tmp_path: Path):
        md_file = tmp_path / "TESTPROG.cbl.md"
        md_file.write_text("# TESTPROG\n\nFallback.")
        _write_summary_json(md_file, {
            "program_id": "TESTPROG",
            "file_name": "TESTPROG.cbl",
            "business_function": "Processes daily credit card transactions",
        })

        result = format_program_section("TESTPROG", md_file, "../docs/cbl/TESTPROG.cbl.md")
        assert result is not None
        assert "### TESTPROG" in result
        assert "credit card transactions" in result

    def test_includes_segment_summaries_with_detail(self, tmp_path: Path):
        md_file = tmp_path / "TESTPROG.cbl.md"
        md_file.write_text("# TESTPROG\n\nFallback.")
        _write_summary_json(md_file, {
            "program_id": "TESTPROG",
            "file_name": "TESTPROG.cbl",
            "business_function": "Payment processing",
            "segment_summaries": [
                {"segment_id": "s1", "functional_area": "Input validation",
                 "summary": "Validates all payment fields against business rules"},
                {"segment_id": "s2", "functional_area": "Payment routing",
                 "summary": "Routes transactions to appropriate payment processor"},
            ],
        })

        result = format_program_section("TESTPROG", md_file, "../docs/cbl/TESTPROG.cbl.md")
        assert result is not None
        assert "**Input validation**: Validates all payment fields" in result
        assert "**Payment routing**: Routes transactions" in result

    def test_includes_data_flows(self, tmp_path: Path):
        md_file = tmp_path / "TESTPROG.cbl.md"
        md_file.write_text("# TESTPROG\n\nFallback.")
        _write_summary_json(md_file, {
            "program_id": "TESTPROG",
            "file_name": "TESTPROG.cbl",
            "business_function": "Account processing",
            "primary_data_flows": [
                "Read customer records from CUSTFILE",
                "Write transaction log to TRANLOG",
            ],
        })

        result = format_program_section("TESTPROG", md_file, "../docs/cbl/TESTPROG.cbl.md")
        assert result is not None
        assert "**Data**: Read customer records from CUSTFILE" in result
        assert "- Write transaction log to TRANLOG" in result

    def test_includes_call_graph(self, tmp_path: Path):
        md_file = tmp_path / "TESTPROG.cbl.md"
        md_file.write_text("# TESTPROG\n\nFallback.")
        _write_summary_json(md_file, {
            "program_id": "TESTPROG",
            "file_name": "TESTPROG.cbl",
            "business_function": "Batch processing",
            "call_graph_summary": [
                "MAIN-PARA -> VALIDATE-INPUT",
                "VALIDATE-INPUT -> PROCESS-RECORD",
            ],
        })

        result = format_program_section("TESTPROG", md_file, "../docs/cbl/TESTPROG.cbl.md")
        assert result is not None
        assert "**Calls**: MAIN-PARA -> VALIDATE-INPUT" in result

    def test_includes_risk_areas(self, tmp_path: Path):
        md_file = tmp_path / "TESTPROG.cbl.md"
        md_file.write_text("# TESTPROG\n\nFallback.")
        _write_summary_json(md_file, {
            "program_id": "TESTPROG",
            "file_name": "TESTPROG.cbl",
            "business_function": "Account processing",
            "risk_areas": ["No error handling for file I/O"],
        })

        result = format_program_section("TESTPROG", md_file, "../docs/cbl/TESTPROG.cbl.md")
        assert result is not None
        assert "**Risks**: No error handling for file I/O" in result

    def test_includes_technical_debt(self, tmp_path: Path):
        md_file = tmp_path / "TESTPROG.cbl.md"
        md_file.write_text("# TESTPROG\n\nFallback.")
        _write_summary_json(md_file, {
            "program_id": "TESTPROG",
            "file_name": "TESTPROG.cbl",
            "business_function": "Legacy processing",
            "technical_debt": ["Uses deprecated GOTO statements"],
        })

        result = format_program_section("TESTPROG", md_file, "../docs/cbl/TESTPROG.cbl.md")
        assert result is not None
        assert "**Debt**: Uses deprecated GOTO statements" in result

    def test_includes_migration_considerations(self, tmp_path: Path):
        md_file = tmp_path / "TESTPROG.cbl.md"
        md_file.write_text("# TESTPROG\n\nFallback.")
        _write_summary_json(md_file, {
            "program_id": "TESTPROG",
            "file_name": "TESTPROG.cbl",
            "business_function": "File processing",
            "migration_considerations": ["VSAM KSDS needs key-value store replacement"],
        })

        result = format_program_section("TESTPROG", md_file, "../docs/cbl/TESTPROG.cbl.md")
        assert result is not None
        assert "**Migration**: VSAM KSDS needs key-value store" in result

    def test_includes_doc_link(self, tmp_path: Path):
        md_file = tmp_path / "TESTPROG.cbl.md"
        md_file.write_text("# TESTPROG\n\nFallback.")
        _write_summary_json(md_file, {
            "program_id": "TESTPROG",
            "file_name": "TESTPROG.cbl",
            "business_function": "Something useful",
        })

        result = format_program_section("TESTPROG", md_file, "../docs/cbl/TESTPROG.cbl.md")
        assert result is not None
        assert "[Full documentation](../docs/cbl/TESTPROG.cbl.md)" in result

    def test_truncates_very_long_section(self, tmp_path: Path):
        md_file = tmp_path / "TESTPROG.cbl.md"
        md_file.write_text("# TESTPROG\n\nFallback.")
        _write_summary_json(md_file, {
            "program_id": "TESTPROG",
            "file_name": "TESTPROG.cbl",
            "business_function": "X" * 200,
            "primary_data_flows": [f"Flow item {i} " * 20 for i in range(20)],
            "call_graph_summary": [f"Call {i} " * 20 for i in range(20)],
            "risk_areas": [f"Risk {i} " * 20 for i in range(20)],
        })

        result = format_program_section("TESTPROG", md_file, "../docs/cbl/TESTPROG.cbl.md", max_chars=500)
        assert result is not None
        assert len(result) <= 600  # Some slack for the trailing link
        assert "[Full documentation]" in result


class TestRichDescriptionInSkillGeneration:
    """Tests that skill generation uses rich section format from .summary.json."""

    def test_category_skill_uses_section_format(self, tmp_path: Path):
        """When .summary.json exists, uses section format not table row."""
        input_dir = tmp_path / "documentation"
        cbl_dir = input_dir / "cbl"
        cbl_dir.mkdir(parents=True)

        md_file = cbl_dir / "CBACT01C.cbl.md"
        md_file.write_text("# CBACT01C\n\nThin first paragraph.")

        _write_summary_json(md_file, {
            "program_id": "CBACT01C",
            "file_name": "CBACT01C.cbl",
            "business_function": "Manages credit card account lifecycle including creation, updates, and closure",
            "primary_data_flows": [
                "Read account master from ACCTFILE",
                "Write audit trail to AUDITLOG",
            ],
            "segment_summaries": [
                {"segment_id": "s1", "functional_area": "Account validation",
                 "summary": "Validates account data against business rules"},
                {"segment_id": "s2", "functional_area": "Balance updates",
                 "summary": "Applies debits and credits to account balances"},
            ],
        })

        generator = SkillsGenerator(input_dir)
        result = generator.generate()

        content = (result / "cobol" / "SKILL.md").read_text()
        # Section format, not table
        assert "### CBACT01C" in content
        assert "credit card account lifecycle" in content
        assert "ACCTFILE" in content
        assert "Account validation" in content
        assert "Thin first paragraph" not in content

    def test_mixed_files_section_and_table(self, tmp_path: Path):
        """Files with summaries get sections; without get table rows."""
        input_dir = tmp_path / "documentation"
        cbl_dir = input_dir / "cbl"
        cbl_dir.mkdir(parents=True)

        # File WITH summary -> section
        md1 = cbl_dir / "RICH.cbl.md"
        md1.write_text("# RICH\n\nThin line for rich.")
        _write_summary_json(md1, {
            "program_id": "RICH",
            "file_name": "RICH.cbl",
            "business_function": "Rich program with full pipeline summary",
        })

        # File WITHOUT summary -> table row
        md2 = cbl_dir / "SLIM.cbl.md"
        md2.write_text("# SLIM\n\nThis slim program does simple things.")

        generator = SkillsGenerator(input_dir)
        result = generator.generate()

        content = (result / "cobol" / "SKILL.md").read_text()
        # Rich file gets section format
        assert "### RICH" in content
        assert "Rich program with full pipeline summary" in content
        # Slim file gets table format
        assert "## Other Programs" in content
        assert "| SLIM |" in content
        assert "slim program does simple things" in content.lower()

    def test_all_slim_files_use_table_only(self, tmp_path: Path):
        """When no summaries exist, output is table-only (no 'Other' header)."""
        input_dir = tmp_path / "documentation"
        cbl_dir = input_dir / "cbl"
        cbl_dir.mkdir(parents=True)

        (cbl_dir / "PROG1.cbl.md").write_text("# PROG1\n\nFirst program.")
        (cbl_dir / "PROG2.cbl.md").write_text("# PROG2\n\nSecond program.")

        generator = SkillsGenerator(input_dir)
        result = generator.generate()

        content = (result / "cobol" / "SKILL.md").read_text()
        assert "| PROG1 |" in content
        assert "| PROG2 |" in content
        assert "## Other Programs" not in content
        assert "## Programs" not in content
