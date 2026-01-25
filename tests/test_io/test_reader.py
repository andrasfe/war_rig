"""Tests for source file reading and discovery."""

import tempfile
from pathlib import Path

import pytest

from war_rig.config import SystemConfig, FileExtensionsConfig
from war_rig.io.reader import SourceFile, SourceReader
from war_rig.models.templates import FileType


@pytest.fixture
def temp_directory():
    """Create a temporary directory with nested structure."""
    with tempfile.TemporaryDirectory() as tmpdir:
        root = Path(tmpdir)

        # Create nested directory structure like carddemo
        (root / "app" / "cobol").mkdir(parents=True)
        (root / "app" / "copybook").mkdir(parents=True)
        (root / "app" / "jcl").mkdir(parents=True)
        (root / "app" / "bms").mkdir(parents=True)

        # Create sample files
        (root / "app" / "cobol" / "CBACT01C.cbl").write_text("IDENTIFICATION DIVISION.")
        (root / "app" / "cobol" / "CBACT02C.cbl").write_text("IDENTIFICATION DIVISION.")
        (root / "app" / "copybook" / "CSSETATY.cpy").write_text("01 CSSETATY.")
        (root / "app" / "jcl" / "JOBCARD.jcl").write_text("//JOBCARD JOB")
        (root / "app" / "bms" / "COSGN00.bms").write_text("COSGN00 DFHMSD")

        # Also create a root-level file
        (root / "README.md").write_text("# Test")
        (root / "ROOTPROG.cbl").write_text("IDENTIFICATION DIVISION.")

        yield root


@pytest.fixture
def system_config(temp_directory):
    """Create a SystemConfig for testing."""
    return SystemConfig(
        input_directory=temp_directory,
        output_directory=temp_directory / "output",
        file_extensions=FileExtensionsConfig(),
    )


class TestSourceFile:
    """Tests for SourceFile model."""

    def test_create_source_file(self):
        """Test creating a SourceFile with all fields."""
        sf = SourceFile(
            path=Path("/test/app/cobol/PROG.cbl"),
            name="PROG.cbl",
            relative_path="app/cobol/PROG.cbl",
            file_type=FileType.COBOL,
            size_bytes=100,
        )

        assert sf.name == "PROG.cbl"
        assert sf.relative_path == "app/cobol/PROG.cbl"
        assert sf.stem == "PROG"

    def test_default_relative_path(self):
        """Test that relative_path defaults to empty string."""
        sf = SourceFile(
            path=Path("/test/PROG.cbl"),
            name="PROG.cbl",
            file_type=FileType.COBOL,
        )

        assert sf.relative_path == ""


class TestSourceReaderDiscoverFiles:
    """Tests for SourceReader.discover_files with relative paths."""

    def test_discover_files_returns_relative_paths(self, system_config, temp_directory):
        """Test that discover_files populates relative_path correctly."""
        reader = SourceReader(system_config)
        files = list(reader.discover_files(temp_directory))

        # Should find COBOL files
        cobol_files = [f for f in files if f.file_type == FileType.COBOL]
        assert len(cobol_files) >= 2

        # Check that relative paths are correct
        for sf in cobol_files:
            assert sf.relative_path != ""
            assert sf.relative_path.endswith(".cbl")
            # Relative path should include directory structure
            if "CBACT01C" in sf.name:
                assert sf.relative_path == "app/cobol/CBACT01C.cbl"

    def test_discover_files_nested_structure(self, system_config, temp_directory):
        """Test that nested directories are correctly represented."""
        reader = SourceReader(system_config)
        files = list(reader.discover_files(temp_directory))

        # Build a map of relative paths
        rel_paths = {f.relative_path for f in files}

        # Should have files from different subdirectories
        assert "app/cobol/CBACT01C.cbl" in rel_paths
        assert "app/copybook/CSSETATY.cpy" in rel_paths
        assert "app/jcl/JOBCARD.jcl" in rel_paths

    def test_discover_files_root_level(self, system_config, temp_directory):
        """Test that root-level files have simple relative paths."""
        reader = SourceReader(system_config)
        files = list(reader.discover_files(temp_directory))

        root_cobol = [f for f in files if f.name == "ROOTPROG.cbl"]
        assert len(root_cobol) == 1
        assert root_cobol[0].relative_path == "ROOTPROG.cbl"

    def test_discover_files_preserves_name(self, system_config, temp_directory):
        """Test that name field still contains just the basename."""
        reader = SourceReader(system_config)
        files = list(reader.discover_files(temp_directory))

        for sf in files:
            # name should be just the filename (no directory)
            assert "/" not in sf.name
            assert sf.name == Path(sf.relative_path).name


class TestOutputPathMirroring:
    """Tests for output path generation that mirrors input structure."""

    def test_get_doc_output_path_nested(self):
        """Test output path for nested input files."""
        from war_rig.workers.scribe_pool import ScribeWorker
        from unittest.mock import MagicMock

        # Create a mock worker to test the path generation
        config = MagicMock()
        config.output_directory = Path("/output")
        beads = MagicMock()

        # Use a partial mock - we just want to test _get_doc_output_path
        worker = ScribeWorker.__new__(ScribeWorker)
        worker.output_directory = Path("/output")

        # Test nested path - new naming convention includes source extension
        result = worker._get_doc_output_path("app/cobol/CBACT01C.cbl")
        assert result == Path("/output/app/cobol/CBACT01C.cbl.doc.json")

    def test_get_doc_output_path_flat(self):
        """Test output path for flat (legacy) input files."""
        from war_rig.workers.scribe_pool import ScribeWorker

        worker = ScribeWorker.__new__(ScribeWorker)
        worker.output_directory = Path("/output")

        # Test flat path - new naming convention includes source extension
        result = worker._get_doc_output_path("PROG.cbl")
        assert result == Path("/output/PROG.cbl.doc.json")

    def test_challenger_get_doc_path_nested(self):
        """Test ChallengerWorker doc path for nested files."""
        from war_rig.workers.challenger_pool import ChallengerWorker

        worker = ChallengerWorker.__new__(ChallengerWorker)
        worker.output_directory = Path("/output")

        # New naming convention includes source extension
        result = worker._get_doc_path("app/cobol/CBACT01C.cbl")
        assert result == Path("/output/app/cobol/CBACT01C.cbl.doc.json")

    def test_challenger_get_doc_path_flat(self):
        """Test ChallengerWorker doc path for flat files."""
        from war_rig.workers.challenger_pool import ChallengerWorker

        worker = ChallengerWorker.__new__(ChallengerWorker)
        worker.output_directory = Path("/output")

        # New naming convention includes source extension
        result = worker._get_doc_path("PROG.cbl")
        assert result == Path("/output/PROG.cbl.doc.json")
