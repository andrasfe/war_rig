"""Unit tests for SkillsGenerator class.

Tests:
- SkillsGenerator initialization
- Input directory validation
- Output directory naming
- Directory structure creation
- Documentation file discovery
"""

from pathlib import Path

import pytest

from war_rig.skills import (
    InputDirectoryNotFoundError,
    InvalidInputDirectoryError,
    SkillsGenerator,
    SkillsGeneratorError,
)


class TestSkillsGeneratorInit:
    """Tests for SkillsGenerator initialization."""

    def test_init_with_valid_directory(self, tmp_path: Path):
        """Test initialization with a valid War Rig output directory."""
        # Create minimal War Rig output structure
        programs_dir = tmp_path / "final" / "programs"
        programs_dir.mkdir(parents=True)
        (programs_dir / "TESTPROG.cbl.doc.json").write_text("{}")

        generator = SkillsGenerator(tmp_path)

        assert generator.input_dir == tmp_path
        assert generator.output_dir == tmp_path.parent / f"skills-{tmp_path.name}"

    def test_init_with_custom_output_dir(self, tmp_path: Path):
        """Test initialization with custom output directory."""
        input_dir = tmp_path / "input"
        output_dir = tmp_path / "custom-output"

        # Create minimal structure
        programs_dir = input_dir / "final" / "programs"
        programs_dir.mkdir(parents=True)
        (programs_dir / "TEST.cbl.doc.json").write_text("{}")

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

    def test_init_empty_directory_raises(self, tmp_path: Path):
        """Test that empty directory without War Rig structure raises error."""
        with pytest.raises(InvalidInputDirectoryError, match="lacks War Rig output"):
            SkillsGenerator(tmp_path)

    def test_init_with_system_overview_only(self, tmp_path: Path):
        """Test initialization with only SYSTEM_OVERVIEW.md present."""
        (tmp_path / "SYSTEM_OVERVIEW.md").write_text("# System Overview")

        generator = SkillsGenerator(tmp_path)
        assert generator.input_dir == tmp_path

    def test_init_with_call_graph_only(self, tmp_path: Path):
        """Test initialization with only CALL_GRAPH.md present."""
        (tmp_path / "CALL_GRAPH.md").write_text("# Call Graph")

        generator = SkillsGenerator(tmp_path)
        assert generator.input_dir == tmp_path


class TestSkillsGeneratorGenerate:
    """Tests for SkillsGenerator.generate() method."""

    def test_generate_creates_output_directory(self, tmp_path: Path):
        """Test that generate creates the output directory."""
        input_dir = tmp_path / "input"
        programs_dir = input_dir / "final" / "programs"
        programs_dir.mkdir(parents=True)
        (programs_dir / "TEST.cbl.doc.json").write_text("{}")

        generator = SkillsGenerator(input_dir)
        result = generator.generate()

        assert result.exists()
        assert result.is_dir()

    def test_generate_creates_standard_subdirectories(self, tmp_path: Path):
        """Test that generate creates standard subdirectories."""
        input_dir = tmp_path / "input"
        programs_dir = input_dir / "final" / "programs"
        programs_dir.mkdir(parents=True)
        (programs_dir / "TEST.cbl.doc.json").write_text("{}")

        generator = SkillsGenerator(input_dir)
        result = generator.generate()

        assert (result / "system-overview").is_dir()
        assert (result / "call-graph").is_dir()
        assert (result / "programs").is_dir()

    def test_generate_creates_datacards_when_present(self, tmp_path: Path):
        """Test that datacards directory is created when DATACARDS.md exists."""
        input_dir = tmp_path / "input"
        programs_dir = input_dir / "final" / "programs"
        programs_dir.mkdir(parents=True)
        (programs_dir / "TEST.cbl.doc.json").write_text("{}")
        (input_dir / "DATACARDS.md").write_text("# Datacards")

        generator = SkillsGenerator(input_dir)
        result = generator.generate()

        assert (result / "datacards").is_dir()

    def test_generate_no_datacards_when_missing(self, tmp_path: Path):
        """Test that datacards directory is not created when DATACARDS.md is absent."""
        input_dir = tmp_path / "input"
        programs_dir = input_dir / "final" / "programs"
        programs_dir.mkdir(parents=True)
        (programs_dir / "TEST.cbl.doc.json").write_text("{}")

        generator = SkillsGenerator(input_dir)
        result = generator.generate()

        assert not (result / "datacards").exists()

    def test_generate_returns_output_path(self, tmp_path: Path):
        """Test that generate returns the output directory path."""
        input_dir = tmp_path / "input"
        programs_dir = input_dir / "final" / "programs"
        programs_dir.mkdir(parents=True)
        (programs_dir / "TEST.cbl.doc.json").write_text("{}")

        generator = SkillsGenerator(input_dir)
        result = generator.generate()

        assert result == generator.output_dir


class TestSkillsGeneratorFindDocs:
    """Tests for SkillsGenerator document finding methods."""

    def test_find_program_docs_returns_doc_json_files(self, tmp_path: Path):
        """Test that _find_program_docs returns .doc.json files."""
        input_dir = tmp_path / "input"
        programs_dir = input_dir / "final" / "programs"
        programs_dir.mkdir(parents=True)

        # Create some doc files
        (programs_dir / "PROG1.cbl.doc.json").write_text("{}")
        (programs_dir / "PROG2.cbl.doc.json").write_text("{}")
        (programs_dir / "OTHER.txt").write_text("not a doc")

        generator = SkillsGenerator(input_dir)
        docs = generator._find_program_docs()

        assert len(docs) == 2
        assert all(p.suffix == ".json" for p in docs)
        assert all("doc.json" in p.name for p in docs)

    def test_find_program_docs_returns_sorted(self, tmp_path: Path):
        """Test that _find_program_docs returns sorted list."""
        input_dir = tmp_path / "input"
        programs_dir = input_dir / "final" / "programs"
        programs_dir.mkdir(parents=True)

        (programs_dir / "ZEBRA.cbl.doc.json").write_text("{}")
        (programs_dir / "ALPHA.cbl.doc.json").write_text("{}")
        (programs_dir / "MIDDLE.cbl.doc.json").write_text("{}")

        generator = SkillsGenerator(input_dir)
        docs = generator._find_program_docs()

        names = [p.name for p in docs]
        assert names == sorted(names)

    def test_find_program_docs_empty_when_no_programs_dir(self, tmp_path: Path):
        """Test _find_program_docs returns empty when programs dir missing."""
        (tmp_path / "SYSTEM_OVERVIEW.md").write_text("# Overview")

        generator = SkillsGenerator(tmp_path)
        docs = generator._find_program_docs()

        assert docs == []

    def test_has_system_overview_true(self, tmp_path: Path):
        """Test _has_system_overview returns True when file exists."""
        (tmp_path / "SYSTEM_OVERVIEW.md").write_text("# Overview")

        generator = SkillsGenerator(tmp_path)
        assert generator._has_system_overview() is True

    def test_has_system_overview_false(self, tmp_path: Path):
        """Test _has_system_overview returns False when file missing."""
        programs_dir = tmp_path / "final" / "programs"
        programs_dir.mkdir(parents=True)
        (programs_dir / "TEST.cbl.doc.json").write_text("{}")

        generator = SkillsGenerator(tmp_path)
        assert generator._has_system_overview() is False

    def test_has_call_graph_true(self, tmp_path: Path):
        """Test _has_call_graph returns True when file exists."""
        (tmp_path / "CALL_GRAPH.md").write_text("# Call Graph")

        generator = SkillsGenerator(tmp_path)
        assert generator._has_call_graph() is True

    def test_has_call_graph_false(self, tmp_path: Path):
        """Test _has_call_graph returns False when file missing."""
        (tmp_path / "SYSTEM_OVERVIEW.md").write_text("# Overview")

        generator = SkillsGenerator(tmp_path)
        assert generator._has_call_graph() is False

    def test_has_datacards_true(self, tmp_path: Path):
        """Test _has_datacards returns True when file exists."""
        (tmp_path / "DATACARDS.md").write_text("# Datacards")
        (tmp_path / "SYSTEM_OVERVIEW.md").write_text(
            "# Overview"
        )  # Need valid structure

        generator = SkillsGenerator(tmp_path)
        assert generator._has_datacards() is True

    def test_has_datacards_false(self, tmp_path: Path):
        """Test _has_datacards returns False when file missing."""
        (tmp_path / "SYSTEM_OVERVIEW.md").write_text("# Overview")

        generator = SkillsGenerator(tmp_path)
        assert generator._has_datacards() is False


class TestSkillsGeneratorPathAccessors:
    """Tests for SkillsGenerator path accessor methods."""

    def test_get_system_overview_path_exists(self, tmp_path: Path):
        """Test get_system_overview_path returns path when file exists."""
        (tmp_path / "SYSTEM_OVERVIEW.md").write_text("# Overview")

        generator = SkillsGenerator(tmp_path)
        path = generator.get_system_overview_path()

        assert path is not None
        assert path.name == "SYSTEM_OVERVIEW.md"
        assert path.is_file()

    def test_get_system_overview_path_missing(self, tmp_path: Path):
        """Test get_system_overview_path returns None when file missing."""
        programs_dir = tmp_path / "final" / "programs"
        programs_dir.mkdir(parents=True)
        (programs_dir / "TEST.cbl.doc.json").write_text("{}")

        generator = SkillsGenerator(tmp_path)
        assert generator.get_system_overview_path() is None

    def test_get_call_graph_path_exists(self, tmp_path: Path):
        """Test get_call_graph_path returns path when file exists."""
        (tmp_path / "CALL_GRAPH.md").write_text("# Call Graph")

        generator = SkillsGenerator(tmp_path)
        path = generator.get_call_graph_path()

        assert path is not None
        assert path.name == "CALL_GRAPH.md"

    def test_get_call_graph_path_missing(self, tmp_path: Path):
        """Test get_call_graph_path returns None when file missing."""
        (tmp_path / "SYSTEM_OVERVIEW.md").write_text("# Overview")

        generator = SkillsGenerator(tmp_path)
        assert generator.get_call_graph_path() is None

    def test_get_datacards_path_exists(self, tmp_path: Path):
        """Test get_datacards_path returns path when file exists."""
        (tmp_path / "DATACARDS.md").write_text("# Datacards")
        (tmp_path / "SYSTEM_OVERVIEW.md").write_text("# Overview")

        generator = SkillsGenerator(tmp_path)
        path = generator.get_datacards_path()

        assert path is not None
        assert path.name == "DATACARDS.md"

    def test_get_datacards_path_missing(self, tmp_path: Path):
        """Test get_datacards_path returns None when file missing."""
        (tmp_path / "SYSTEM_OVERVIEW.md").write_text("# Overview")

        generator = SkillsGenerator(tmp_path)
        assert generator.get_datacards_path() is None


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

    def test_invalid_structure_error_is_descriptive(self, tmp_path: Path):
        """Test that invalid structure error contains guidance."""
        with pytest.raises(InvalidInputDirectoryError) as exc_info:
            SkillsGenerator(tmp_path)

        error_msg = str(exc_info.value)
        assert "doc.json" in error_msg or "SYSTEM_OVERVIEW" in error_msg


class TestSkillsGeneratorIntegration:
    """Integration tests for SkillsGenerator."""

    def test_full_workflow_with_all_files(self, tmp_path: Path):
        """Test complete workflow with all documentation types."""
        input_dir = tmp_path / "war-rig-output"

        # Create complete War Rig output structure
        programs_dir = input_dir / "final" / "programs"
        programs_dir.mkdir(parents=True)

        (programs_dir / "CBACT01C.cbl.doc.json").write_text('{"program": "CBACT01C"}')
        (programs_dir / "CBPAUP0C.cbl.doc.json").write_text('{"program": "CBPAUP0C"}')

        (input_dir / "SYSTEM_OVERVIEW.md").write_text("# CardDemo System Overview")
        (input_dir / "CALL_GRAPH.md").write_text("# Call Graph\n\nCBACT01C -> CBPAUP0C")
        (input_dir / "DATACARDS.md").write_text("# Data Cards\n\nCUSTOMER-RECORD")

        generator = SkillsGenerator(input_dir)
        output_dir = generator.generate()

        # Verify output structure
        assert output_dir.exists()
        assert (output_dir / "system-overview").is_dir()
        assert (output_dir / "call-graph").is_dir()
        assert (output_dir / "datacards").is_dir()
        assert (output_dir / "programs").is_dir()

        # Verify documentation was found
        docs = generator._find_program_docs()
        assert len(docs) == 2

    def test_idempotent_generation(self, tmp_path: Path):
        """Test that running generate twice is safe."""
        input_dir = tmp_path / "input"
        programs_dir = input_dir / "final" / "programs"
        programs_dir.mkdir(parents=True)
        (programs_dir / "TEST.cbl.doc.json").write_text("{}")

        generator = SkillsGenerator(input_dir)

        # Run twice
        result1 = generator.generate()
        result2 = generator.generate()

        assert result1 == result2
        assert result1.exists()
