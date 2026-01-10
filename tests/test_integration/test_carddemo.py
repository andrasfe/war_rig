"""Integration tests with AWS CardDemo.

These tests verify the War Rig system works end-to-end with real
mainframe source code from the AWS CardDemo sample application.
"""

import pytest
from pathlib import Path

from war_rig.config import SystemConfig, WarRigConfig
from war_rig.io.reader import SourceReader
from war_rig.io.writer import DocumentationWriter
from war_rig.orchestration.graph import create_war_rig_graph, analyze_file
from war_rig.preprocessors.cobol import COBOLPreprocessor

CARDDEMO_PATH = Path("aws-mainframe-modernization-carddemo/app")


@pytest.fixture
def carddemo_available():
    """Check if CardDemo is available."""
    if not CARDDEMO_PATH.exists():
        pytest.skip("CardDemo not cloned - run: git clone https://github.com/aws-samples/aws-mainframe-modernization-carddemo.git")
    return CARDDEMO_PATH


@pytest.fixture
def system_config(carddemo_available, tmp_path):
    """Create system config for tests."""
    return SystemConfig(
        input_directory=carddemo_available,
        output_directory=tmp_path / "output",
    )


@pytest.fixture
def war_rig_graph():
    """Create a War Rig graph for testing."""
    return create_war_rig_graph(WarRigConfig())


class TestCOBOLPreprocessing:
    """Test COBOL preprocessing with real CardDemo files."""

    def test_preprocess_cbact04c(self, carddemo_available):
        """Test preprocessing CBACT04C (interest calculator)."""
        source_path = carddemo_available / "cbl" / "CBACT04C.cbl"
        assert source_path.exists(), f"CBACT04C.cbl not found"

        source = source_path.read_text()
        preprocessor = COBOLPreprocessor()
        result = preprocessor.process(source, "CBACT04C.cbl")

        assert result.program_id == "CBACT04C"
        assert result.file_type == "COBOL"
        assert len(result.paragraphs) > 0
        assert len(result.copybooks) > 0

    def test_preprocess_cbstm03b(self, carddemo_available):
        """Test preprocessing CBSTM03B (statement subroutine)."""
        source_path = carddemo_available / "cbl" / "CBSTM03B.cbl"
        assert source_path.exists(), f"CBSTM03B.cbl not found"

        source = source_path.read_text()
        preprocessor = COBOLPreprocessor()
        result = preprocessor.process(source, "CBSTM03B.cbl")

        assert result.program_id == "CBSTM03B"


class TestWarRigWorkflow:
    """Test the full War Rig workflow with mock agents."""

    @pytest.mark.asyncio
    async def test_analyze_cbact04c_mock(self, carddemo_available):
        """Test analyzing CBACT04C with mock agents."""
        source_path = carddemo_available / "cbl" / "CBACT04C.cbl"
        source = source_path.read_text()

        result = await analyze_file(
            source_code=source,
            file_name="CBACT04C.cbl",
            use_mock=True,
        )

        assert result["decision"] in ["WITNESSED", "VALHALLA", "FORCED"]
        assert result["final_template"] is not None
        assert result["iteration"] <= 3  # Max iterations

    @pytest.mark.asyncio
    async def test_analyze_cbstm03b_mock(self, carddemo_available):
        """Test analyzing CBSTM03B with mock agents."""
        source_path = carddemo_available / "cbl" / "CBSTM03B.cbl"
        source = source_path.read_text()

        result = await analyze_file(
            source_code=source,
            file_name="CBSTM03B.cbl",
            use_mock=True,
        )

        assert result["decision"] in ["WITNESSED", "VALHALLA", "FORCED"]
        assert result["final_template"] is not None


class TestOutputGeneration:
    """Test documentation output generation."""

    @pytest.mark.asyncio
    async def test_write_documentation(self, system_config, carddemo_available):
        """Test writing documentation output."""
        source_path = carddemo_available / "cbl" / "CBACT04C.cbl"
        source = source_path.read_text()

        # Run analysis
        result = await analyze_file(
            source_code=source,
            file_name="CBACT04C.cbl",
            use_mock=True,
        )

        # Write output
        writer = DocumentationWriter(system_config)
        output_paths = writer.write_result(result)

        # Verify outputs exist
        assert "final_json" in output_paths
        assert output_paths["final_json"].exists()
        assert "final_md" in output_paths
        assert output_paths["final_md"].exists()


class TestSourceReader:
    """Test source file reading."""

    def test_read_cobol_file(self, system_config, carddemo_available):
        """Test reading a COBOL file."""
        reader = SourceReader(system_config)
        source = reader.read_file(carddemo_available / "cbl" / "CBACT04C.cbl")

        assert "IDENTIFICATION DIVISION" in source
        assert "PROGRAM-ID" in source

    def test_discover_cobol_files(self, system_config, carddemo_available):
        """Test discovering COBOL files in directory."""
        reader = SourceReader(system_config)
        files = list(reader.discover_files(carddemo_available / "cbl"))

        assert len(files) > 0
        cobol_files = [f for f in files if f.file_type.value == "COBOL"]
        assert len(cobol_files) > 10  # CardDemo has many COBOL files
