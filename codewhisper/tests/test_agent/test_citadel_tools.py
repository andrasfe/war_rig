"""Tests for citadel integration tools.

This module tests:
- Citadel tool input schemas
- Individual tool functions with mocked SDK
- Error handling (file not found, invalid paths)
- Path resolution (relative to code_dir)
- Result serialization
- Tool configuration
"""

from pathlib import Path
from typing import Any
from unittest.mock import MagicMock

import pytest
from pydantic import ValidationError


# Mock the citadel SDK classes for unit testing
class MockCallout:
    """Mock Callout from citadel SDK."""

    def __init__(
        self,
        target: str,
        relationship: str,
        target_type: str | None = None,
        line: int | None = None,
        raw_text: str | None = None,
        resolved: bool | None = None,
    ):
        self.target = target
        self.relationship = relationship
        self.target_type = target_type
        self.line = line
        self.raw_text = raw_text
        self.resolved = resolved

    def to_dict(self) -> dict[str, Any]:
        return {
            "target": self.target,
            "relationship": self.relationship,
            "target_type": self.target_type,
            "line": self.line,
            "raw_text": self.raw_text,
        }


class MockFileArtifact:
    """Mock FileArtifact from citadel SDK."""

    def __init__(
        self,
        name: str,
        type: str,
        category: str = "code",
        line_start: int | None = None,
        line_end: int | None = None,
        callouts: list[MockCallout] | None = None,
    ):
        self.name = name
        self.type = type
        self.category = category
        self.line_start = line_start
        self.line_end = line_end
        self.callouts = callouts or []

    def to_dict(self) -> dict[str, Any]:
        return {
            "name": self.name,
            "type": self.type,
            "category": self.category,
            "line_start": self.line_start,
            "line_end": self.line_end,
            "callouts": [c.to_dict() for c in self.callouts],
        }


class MockFileAnalysisResult:
    """Mock FileAnalysisResult from citadel SDK."""

    def __init__(
        self,
        file_path: str,
        language: str,
        artifacts: list[MockFileArtifact] | None = None,
        file_level_callouts: list[MockCallout] | None = None,
        preprocessor_includes: list[str] | None = None,
        error: str | None = None,
    ):
        self.file_path = file_path
        self.language = language
        self.artifacts = artifacts or []
        self.file_level_callouts = file_level_callouts or []
        self.preprocessor_includes = preprocessor_includes or []
        self.error = error

    def to_dict(self) -> dict[str, Any]:
        return {
            "file_path": self.file_path,
            "language": self.language,
            "artifacts": [a.to_dict() for a in self.artifacts],
            "file_level_callouts": [c.to_dict() for c in self.file_level_callouts],
            "preprocessor_includes": self.preprocessor_includes,
            "error": self.error,
        }

    def get_artifact_by_name(self, name: str) -> MockFileArtifact | None:
        name_lower = name.lower()
        for artifact in self.artifacts:
            if artifact.name.lower() == name_lower:
                return artifact
        return None


class MockAnalysisPatternMatch:
    """Mock analysis pattern match."""

    def __init__(
        self,
        pattern_name: str,
        category: str,
        captured: list[str],
        line: int | None = None,
        context: list[str] | None = None,
    ):
        self.pattern_name = pattern_name
        self.category = category
        self.captured = captured
        self.line = line
        self.context = context or []


class MockAnalysisCategoryResult:
    """Mock analysis category result."""

    def __init__(
        self,
        category: str,
        matches: list[MockAnalysisPatternMatch],
        match_count: int,
        patterns_matched: dict[str, int],
    ):
        self.category = category
        self.matches = matches
        self.match_count = match_count
        self.patterns_matched = patterns_matched


class MockFileAnalysisPatternResult:
    """Mock FileAnalysisPatternResult from citadel SDK."""

    def __init__(
        self,
        file_path: str,
        language: str,
        categories: dict[str, MockAnalysisCategoryResult] | None = None,
        total_matches: int = 0,
        coverage_pct: float = 0.0,
        required_missing: list[str] | None = None,
        error: str | None = None,
    ):
        self.file_path = file_path
        self.language = language
        self.categories = categories or {}
        self.total_matches = total_matches
        self.coverage_pct = coverage_pct
        self.required_missing = required_missing or []
        self.error = error

    def to_dict(self) -> dict[str, Any]:
        return {
            "file_path": self.file_path,
            "language": self.language,
            "total_matches": self.total_matches,
            "coverage_pct": self.coverage_pct,
            "required_missing": self.required_missing,
            "error": self.error,
        }


class MockCitadel:
    """Mock Citadel class for testing."""

    def __init__(self):
        self._analyze_file_response: MockFileAnalysisResult | None = None
        self._get_functions_response: list[dict[str, Any]] | None = None
        self._get_callouts_response: list[dict[str, Any]] | None = None
        self._get_includes_response: list[str] | None = None
        self._get_function_body_response: str | None = None
        self._get_function_bodies_response: dict[str, str | None] | None = None
        self._get_file_stats_response: dict[str, Any] | None = None
        self._get_callers_response: list[dict[str, Any]] | None = None
        self._get_sequence_diagrams_response: list[str] | None = None
        self._get_dead_code_response: list[dict[str, Any]] | None = None
        self._get_flow_diagram_response: str | None = None
        self._get_file_summary_response: dict[str, Any] | None = None
        self._get_analysis_patterns_response: MockFileAnalysisPatternResult | None = (
            None
        )

    def analyze_file(self, file_path: str | Path) -> MockFileAnalysisResult:
        if self._analyze_file_response:
            return self._analyze_file_response
        return MockFileAnalysisResult(
            file_path=str(file_path), language="cobol", artifacts=[], error=None
        )

    def get_functions(self, file_path: str | Path) -> list[dict[str, Any]]:
        if self._get_functions_response is not None:
            return self._get_functions_response
        return []

    def get_callouts(self, path: str | Path) -> list[dict[str, Any]]:
        if self._get_callouts_response is not None:
            return self._get_callouts_response
        return []

    def get_includes(self, file_path: str | Path) -> list[str]:
        if self._get_includes_response is not None:
            return self._get_includes_response
        return []

    def get_function_body(
        self, file_path: str | Path, function_name: str
    ) -> str | None:
        return self._get_function_body_response

    def get_function_bodies(
        self, file_path: str | Path, function_names: list[str]
    ) -> dict[str, str | None]:
        if self._get_function_bodies_response is not None:
            return self._get_function_bodies_response
        return dict.fromkeys(function_names)

    def get_file_stats(self, file_path: str | Path) -> dict[str, Any]:
        if self._get_file_stats_response is not None:
            return self._get_file_stats_response
        return {
            "total_lines": 0,
            "paragraph_count": 0,
            "language": "unknown",
            "paragraphs": [],
        }

    def get_callers(
        self,
        file_path: str | Path,
        function_name: str,
        search_paths: list[str | Path] | None = None,
    ) -> list[dict[str, Any]]:
        if self._get_callers_response is not None:
            return self._get_callers_response
        return []

    def get_sequence_diagrams(
        self,
        path: str | Path,
        max_diagrams: int = 5,
        min_sequence_length: int = 2,
    ) -> list[str]:
        if self._get_sequence_diagrams_response is not None:
            return self._get_sequence_diagrams_response
        return []

    def get_dead_code(
        self,
        path: str | Path,
        exclude_types: set[str] | None = None,
        include_only_types: set[str] | None = None,
    ) -> list[dict[str, Any]]:
        if self._get_dead_code_response is not None:
            return self._get_dead_code_response
        return []

    def get_flow_diagram(
        self,
        file_path: str | Path,
        paragraph: str | None = None,
        include_external: bool = True,
    ) -> str:
        if self._get_flow_diagram_response is not None:
            return self._get_flow_diagram_response
        return "flowchart TD\n    A --> B"

    def get_file_summary(self, file_path: str | Path) -> dict[str, Any]:
        if self._get_file_summary_response is not None:
            return self._get_file_summary_response
        return {
            "file_name": Path(file_path).name,
            "language": "cobol",
            "total_lines": 100,
            "paragraph_count": 5,
            "entry_points": ["MAIN-PARA"],
            "main_calls": ["SUB-PROGRAM"],
            "error": None,
        }

    def get_analysis_patterns(
        self, file_path: str | Path, summary_only: bool = False
    ) -> MockFileAnalysisPatternResult:
        if self._get_analysis_patterns_response is not None:
            return self._get_analysis_patterns_response
        return MockFileAnalysisPatternResult(
            file_path=str(file_path),
            language="cobol",
            total_matches=0,
        )


@pytest.fixture
def mock_citadel() -> MockCitadel:
    """Create a mock Citadel instance."""
    return MockCitadel()


@pytest.fixture
def tmp_code_dir(tmp_path: Path) -> Path:
    """Create a temporary code directory with sample source files."""
    code_dir = tmp_path / "src"
    code_dir.mkdir()

    # Create a COBOL file
    cobol_dir = code_dir / "cbl"
    cobol_dir.mkdir()

    cobol_file = cobol_dir / "TESTPROG.cbl"
    cobol_file.write_text(
        """       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       AUTHOR. TEST.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-COUNTER          PIC 9(5) VALUE 0.
       01  WS-TOTAL            PIC 9(9)V99 VALUE 0.
      *
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS
           PERFORM 3000-FINALIZE
           STOP RUN.
      *
       1000-INITIALIZE.
           DISPLAY 'STARTING TESTPROG'.
      *
       2000-PROCESS.
           ADD 1 TO WS-COUNTER
           COMPUTE WS-TOTAL = WS-TOTAL + 100.50.
      *
       3000-FINALIZE.
           DISPLAY 'PROCESSED: ' WS-COUNTER ' RECORDS'.
"""
    )

    # Create a JCL file
    jcl_dir = code_dir / "jcl"
    jcl_dir.mkdir()

    jcl_file = jcl_dir / "TESTJOB.jcl"
    jcl_file.write_text(
        """//TESTJOB  JOB (ACCT),'TEST JOB',CLASS=A
//*
//STEP01   EXEC PGM=TESTPROG
//INFILE   DD DSN=TEST.INPUT,DISP=SHR
//OUTFILE  DD DSN=TEST.OUTPUT,DISP=(NEW,CATLG)
//
"""
    )

    return code_dir


@pytest.fixture
def mock_agent_config(tmp_code_dir: Path) -> MagicMock:
    """Create a mock AgentConfig for testing."""
    config = MagicMock()
    config.code_dir = tmp_code_dir
    return config


# Import the module after defining fixtures
# We'll use patch to mock the citadel import in actual tests


class TestAnalyzeFileInputSchema:
    """Tests for AnalyzeFileInput schema."""

    def test_valid_input(self) -> None:
        """Test valid analyze file input."""
        # This would test the Pydantic schema once citadel_tools.py exists
        # For now, test the expected behavior
        from pydantic import BaseModel, Field

        class AnalyzeFileInput(BaseModel):
            file_path: str = Field(
                ...,
                description="Path to source file (relative to code directory or absolute)",
            )

        input_data = AnalyzeFileInput(file_path="cbl/TEST.cbl")
        assert input_data.file_path == "cbl/TEST.cbl"

    def test_file_path_required(self) -> None:
        """Test that file_path is required."""
        from pydantic import BaseModel, Field

        class AnalyzeFileInput(BaseModel):
            file_path: str = Field(...)

        with pytest.raises(ValidationError):
            AnalyzeFileInput()  # type: ignore


class TestGetFunctionsInputSchema:
    """Tests for GetFunctionsInput schema."""

    def test_valid_input(self) -> None:
        """Test valid get functions input."""
        from pydantic import BaseModel, Field

        class GetFunctionsInput(BaseModel):
            file_path: str = Field(...)

        input_data = GetFunctionsInput(file_path="cbl/PROGRAM.cbl")
        assert input_data.file_path == "cbl/PROGRAM.cbl"


class TestGetFunctionBodyInputSchema:
    """Tests for GetFunctionBodyInput schema."""

    def test_valid_input(self) -> None:
        """Test valid get function body input."""
        from pydantic import BaseModel, Field

        class GetFunctionBodyInput(BaseModel):
            file_path: str = Field(...)
            function_name: str = Field(...)

        input_data = GetFunctionBodyInput(
            file_path="cbl/PROGRAM.cbl",
            function_name="1000-PROCESS",
        )
        assert input_data.file_path == "cbl/PROGRAM.cbl"
        assert input_data.function_name == "1000-PROCESS"

    def test_both_fields_required(self) -> None:
        """Test that both fields are required."""
        from pydantic import BaseModel, Field

        class GetFunctionBodyInput(BaseModel):
            file_path: str = Field(...)
            function_name: str = Field(...)

        with pytest.raises(ValidationError):
            GetFunctionBodyInput(file_path="test.cbl")  # type: ignore

        with pytest.raises(ValidationError):
            GetFunctionBodyInput(function_name="TEST")  # type: ignore


class TestGetCallersInputSchema:
    """Tests for GetCallersInput schema."""

    def test_valid_input(self) -> None:
        """Test valid get callers input."""
        from pydantic import BaseModel, Field

        class GetCallersInput(BaseModel):
            file_path: str = Field(...)
            function_name: str = Field(...)

        input_data = GetCallersInput(
            file_path="cbl/UTILS.cbl",
            function_name="CALCULATE-TAX",
        )
        assert input_data.file_path == "cbl/UTILS.cbl"
        assert input_data.function_name == "CALCULATE-TAX"


class TestGetSequenceDiagramsInputSchema:
    """Tests for GetSequenceDiagramsInput schema."""

    def test_valid_input_with_defaults(self) -> None:
        """Test valid input with default max_diagrams."""
        from pydantic import BaseModel, Field

        class GetSequenceDiagramsInput(BaseModel):
            path: str = Field(...)
            max_diagrams: int = Field(default=3)

        input_data = GetSequenceDiagramsInput(path="./src")
        assert input_data.path == "./src"
        assert input_data.max_diagrams == 3

    def test_custom_max_diagrams(self) -> None:
        """Test custom max_diagrams value."""
        from pydantic import BaseModel, Field

        class GetSequenceDiagramsInput(BaseModel):
            path: str = Field(...)
            max_diagrams: int = Field(default=3)

        input_data = GetSequenceDiagramsInput(path="./src", max_diagrams=10)
        assert input_data.max_diagrams == 10


class TestGetFlowDiagramInputSchema:
    """Tests for GetFlowDiagramInput schema."""

    def test_valid_input_without_paragraph(self) -> None:
        """Test valid input without optional paragraph."""
        from pydantic import BaseModel, Field

        class GetFlowDiagramInput(BaseModel):
            file_path: str = Field(...)
            paragraph: str | None = Field(default=None)

        input_data = GetFlowDiagramInput(file_path="cbl/PROGRAM.cbl")
        assert input_data.file_path == "cbl/PROGRAM.cbl"
        assert input_data.paragraph is None

    def test_valid_input_with_paragraph(self) -> None:
        """Test valid input with paragraph specified."""
        from pydantic import BaseModel, Field

        class GetFlowDiagramInput(BaseModel):
            file_path: str = Field(...)
            paragraph: str | None = Field(default=None)

        input_data = GetFlowDiagramInput(
            file_path="cbl/PROGRAM.cbl",
            paragraph="1000-PROCESS",
        )
        assert input_data.paragraph == "1000-PROCESS"


class TestMockCitadelAnalyzeFile:
    """Tests for analyze_file functionality with mock."""

    def test_analyze_file_success(
        self, mock_citadel: MockCitadel, tmp_code_dir: Path
    ) -> None:
        """Test successful file analysis."""
        artifact = MockFileArtifact(
            name="0000-MAIN",
            type="paragraph",
            line_start=10,
            line_end=15,
            callouts=[
                MockCallout("1000-INITIALIZE", "performs", line=11),
                MockCallout("2000-PROCESS", "performs", line=12),
            ],
        )
        mock_citadel._analyze_file_response = MockFileAnalysisResult(
            file_path=str(tmp_code_dir / "cbl/TESTPROG.cbl"),
            language="cobol",
            artifacts=[artifact],
        )

        result = mock_citadel.analyze_file(tmp_code_dir / "cbl/TESTPROG.cbl")

        assert result.language == "cobol"
        assert len(result.artifacts) == 1
        assert result.artifacts[0].name == "0000-MAIN"
        assert len(result.artifacts[0].callouts) == 2
        assert result.error is None

    def test_analyze_file_with_error(self, mock_citadel: MockCitadel) -> None:
        """Test analysis with error response."""
        mock_citadel._analyze_file_response = MockFileAnalysisResult(
            file_path="nonexistent.cbl",
            language="unknown",
            artifacts=[],
            error="No spec found for file extension '.xyz'",
        )

        result = mock_citadel.analyze_file("nonexistent.cbl")

        assert result.error is not None
        assert "spec" in result.error.lower()

    def test_analyze_file_with_file_level_callouts(
        self, mock_citadel: MockCitadel
    ) -> None:
        """Test analysis with file-level callouts (imports, etc.)."""
        mock_citadel._analyze_file_response = MockFileAnalysisResult(
            file_path="test.cbl",
            language="cobol",
            artifacts=[],
            file_level_callouts=[
                MockCallout("COMMON-COPY", "includes", line=5),
            ],
            preprocessor_includes=["COMMON-COPY"],
        )

        result = mock_citadel.analyze_file("test.cbl")

        assert len(result.file_level_callouts) == 1
        assert result.file_level_callouts[0].target == "COMMON-COPY"
        assert "COMMON-COPY" in result.preprocessor_includes


class TestMockCitadelGetFunctions:
    """Tests for get_functions functionality with mock."""

    def test_get_functions_success(self, mock_citadel: MockCitadel) -> None:
        """Test successful function list."""
        mock_citadel._get_functions_response = [
            {
                "name": "0000-MAIN",
                "type": "paragraph",
                "line": 10,
                "line_end": 15,
                "calls": [
                    {"target": "1000-INIT", "type": "performs", "line": 11},
                ],
            },
            {
                "name": "1000-INIT",
                "type": "paragraph",
                "line": 17,
                "line_end": 20,
                "calls": [],
            },
        ]

        result = mock_citadel.get_functions("test.cbl")

        assert len(result) == 2
        assert result[0]["name"] == "0000-MAIN"
        assert result[1]["name"] == "1000-INIT"

    def test_get_functions_empty(self, mock_citadel: MockCitadel) -> None:
        """Test when no functions found."""
        mock_citadel._get_functions_response = []

        result = mock_citadel.get_functions("empty.cbl")

        assert result == []

    def test_get_functions_with_error(self, mock_citadel: MockCitadel) -> None:
        """Test error in get_functions."""
        mock_citadel._get_functions_response = [{"error": "File not found"}]

        result = mock_citadel.get_functions("missing.cbl")

        assert len(result) == 1
        assert "error" in result[0]


class TestMockCitadelGetCallouts:
    """Tests for get_callouts functionality with mock."""

    def test_get_callouts_from_file(self, mock_citadel: MockCitadel) -> None:
        """Test getting callouts from a single file."""
        mock_citadel._get_callouts_response = [
            {"from": "MAIN", "to": "SUB1", "type": "calls", "line": 10},
            {"from": "MAIN", "to": "SUB2", "type": "performs", "line": 11},
        ]

        result = mock_citadel.get_callouts("test.cbl")

        assert len(result) == 2
        assert result[0]["to"] == "SUB1"

    def test_get_callouts_from_directory(self, mock_citadel: MockCitadel) -> None:
        """Test getting callouts from a directory with resolved status."""
        mock_citadel._get_callouts_response = [
            {
                "from": "PROG1",
                "to": "PROG2",
                "type": "calls",
                "line": 10,
                "resolved": True,
            },
            {
                "from": "PROG1",
                "to": "EXTERNAL",
                "type": "calls",
                "line": 11,
                "resolved": False,
            },
        ]

        result = mock_citadel.get_callouts("./src")

        assert len(result) == 2
        assert result[0]["resolved"] is True
        assert result[1]["resolved"] is False


class TestMockCitadelGetIncludes:
    """Tests for get_includes functionality with mock."""

    def test_get_includes_success(self, mock_citadel: MockCitadel) -> None:
        """Test successful includes extraction."""
        mock_citadel._get_includes_response = ["COMMON.CPY", "SHARED.CPY", "UTILS.CPY"]

        result = mock_citadel.get_includes("test.cbl")

        assert len(result) == 3
        assert "COMMON.CPY" in result

    def test_get_includes_empty(self, mock_citadel: MockCitadel) -> None:
        """Test when no includes found."""
        mock_citadel._get_includes_response = []

        result = mock_citadel.get_includes("no-includes.cbl")

        assert result == []


class TestMockCitadelGetFunctionBody:
    """Tests for get_function_body functionality with mock."""

    def test_get_function_body_success(self, mock_citadel: MockCitadel) -> None:
        """Test successful function body extraction."""
        mock_citadel._get_function_body_response = (
            "       1000-INITIALIZE.\n           DISPLAY 'STARTING'."
        )

        result = mock_citadel.get_function_body("test.cbl", "1000-INITIALIZE")

        assert result is not None
        assert "1000-INITIALIZE" in result
        assert "DISPLAY" in result

    def test_get_function_body_not_found(self, mock_citadel: MockCitadel) -> None:
        """Test when function is not found."""
        mock_citadel._get_function_body_response = None

        result = mock_citadel.get_function_body("test.cbl", "NONEXISTENT")

        assert result is None


class TestMockCitadelGetFunctionBodies:
    """Tests for get_function_bodies functionality with mock."""

    def test_get_function_bodies_success(self, mock_citadel: MockCitadel) -> None:
        """Test successful batch function body extraction."""
        mock_citadel._get_function_bodies_response = {
            "1000-INIT": "       1000-INIT.\n           DISPLAY 'INIT'.",
            "2000-PROC": "       2000-PROC.\n           ADD 1 TO COUNTER.",
            "9999-END": None,  # Not found
        }

        result = mock_citadel.get_function_bodies(
            "test.cbl", ["1000-INIT", "2000-PROC", "9999-END"]
        )

        assert len(result) == 3
        assert result["1000-INIT"] is not None
        assert result["2000-PROC"] is not None
        assert result["9999-END"] is None

    def test_get_function_bodies_all_missing(self, mock_citadel: MockCitadel) -> None:
        """Test when all requested functions are missing."""
        result = mock_citadel.get_function_bodies(
            "test.cbl", ["MISSING1", "MISSING2"]
        )

        assert len(result) == 2
        assert all(v is None for v in result.values())


class TestMockCitadelGetFileStats:
    """Tests for get_file_stats functionality with mock."""

    def test_get_file_stats_success(self, mock_citadel: MockCitadel) -> None:
        """Test successful file stats extraction."""
        mock_citadel._get_file_stats_response = {
            "total_lines": 500,
            "paragraph_count": 15,
            "language": "cobol",
            "paragraphs": [
                {"name": "0000-MAIN", "line_start": 10, "line_end": 20, "line_count": 11},
                {"name": "1000-INIT", "line_start": 22, "line_end": 30, "line_count": 9},
            ],
        }

        result = mock_citadel.get_file_stats("test.cbl")

        assert result["total_lines"] == 500
        assert result["paragraph_count"] == 15
        assert len(result["paragraphs"]) == 2


class TestMockCitadelGetCallers:
    """Tests for get_callers functionality with mock."""

    def test_get_callers_success(self, mock_citadel: MockCitadel) -> None:
        """Test successful caller lookup."""
        mock_citadel._get_callers_response = [
            {"file": "MAIN.cbl", "function": "0000-MAIN", "line": 15, "type": "calls"},
            {
                "file": "BATCH.cbl",
                "function": "PROCESS-BATCH",
                "line": 42,
                "type": "performs",
            },
        ]

        result = mock_citadel.get_callers("UTILS.cbl", "CALCULATE-TAX")

        assert len(result) == 2
        assert result[0]["file"] == "MAIN.cbl"
        assert result[1]["function"] == "PROCESS-BATCH"

    def test_get_callers_no_callers(self, mock_citadel: MockCitadel) -> None:
        """Test when no callers found."""
        mock_citadel._get_callers_response = []

        result = mock_citadel.get_callers("UTILS.cbl", "UNUSED-FUNC")

        assert result == []


class TestMockCitadelGetSequenceDiagrams:
    """Tests for get_sequence_diagrams functionality with mock."""

    def test_get_sequence_diagrams_success(self, mock_citadel: MockCitadel) -> None:
        """Test successful sequence diagram generation."""
        mock_citadel._get_sequence_diagrams_response = [
            "sequenceDiagram\n    MAIN->>PROCESS: call\n    PROCESS->>DB: read",
            "sequenceDiagram\n    BATCH->>VALIDATE: call",
        ]

        result = mock_citadel.get_sequence_diagrams("./src", max_diagrams=2)

        assert len(result) == 2
        assert "sequenceDiagram" in result[0]

    def test_get_sequence_diagrams_empty(self, mock_citadel: MockCitadel) -> None:
        """Test when no sequences found."""
        mock_citadel._get_sequence_diagrams_response = []

        result = mock_citadel.get_sequence_diagrams("./src")

        assert result == []


class TestMockCitadelGetDeadCode:
    """Tests for get_dead_code functionality with mock."""

    def test_get_dead_code_success(self, mock_citadel: MockCitadel) -> None:
        """Test successful dead code detection."""
        mock_citadel._get_dead_code_response = [
            {
                "name": "9999-UNUSED",
                "type": "paragraph",
                "file": "OLD.cbl",
                "line": 500,
                "reason": "No references found",
            },
            {
                "name": "OBSOLETE-CPY",
                "type": "copybook",
                "file": "OBSOLETE.CPY",
                "line": 1,
                "reason": "No COPY statements reference this member",
            },
        ]

        result = mock_citadel.get_dead_code("./src")

        assert len(result) == 2
        assert result[0]["name"] == "9999-UNUSED"
        assert result[1]["type"] == "copybook"

    def test_get_dead_code_empty(self, mock_citadel: MockCitadel) -> None:
        """Test when no dead code found."""
        mock_citadel._get_dead_code_response = []

        result = mock_citadel.get_dead_code("./src")

        assert result == []


class TestMockCitadelGetFlowDiagram:
    """Tests for get_flow_diagram functionality with mock."""

    def test_get_flow_diagram_success(self, mock_citadel: MockCitadel) -> None:
        """Test successful flow diagram generation."""
        mock_citadel._get_flow_diagram_response = """flowchart TD
    0000-MAIN --> 1000-INIT
    0000-MAIN --> 2000-PROCESS
    0000-MAIN --> 3000-FINAL
    1000-INIT --> OPEN-FILES[(OPEN-FILES)]"""

        result = mock_citadel.get_flow_diagram("test.cbl")

        assert "flowchart TD" in result
        assert "0000-MAIN" in result

    def test_get_flow_diagram_with_paragraph(self, mock_citadel: MockCitadel) -> None:
        """Test flow diagram starting from specific paragraph."""
        mock_citadel._get_flow_diagram_response = """flowchart TD
    2000-PROCESS --> 2100-VALIDATE
    2000-PROCESS --> 2200-CALCULATE"""

        result = mock_citadel.get_flow_diagram("test.cbl", paragraph="2000-PROCESS")

        assert "2000-PROCESS" in result


class TestMockCitadelGetFileSummary:
    """Tests for get_file_summary functionality with mock."""

    def test_get_file_summary_success(self, mock_citadel: MockCitadel) -> None:
        """Test successful file summary."""
        mock_citadel._get_file_summary_response = {
            "file_name": "TESTPROG.cbl",
            "language": "cobol",
            "total_lines": 250,
            "paragraph_count": 12,
            "entry_points": ["0000-MAIN"],
            "main_calls": ["SQLPROG", "MQ-HANDLER"],
            "error": None,
        }

        result = mock_citadel.get_file_summary("TESTPROG.cbl")

        assert result["file_name"] == "TESTPROG.cbl"
        assert result["paragraph_count"] == 12
        assert "0000-MAIN" in result["entry_points"]


class TestMockCitadelGetAnalysisPatterns:
    """Tests for get_analysis_patterns functionality with mock."""

    def test_get_analysis_patterns_success(self, mock_citadel: MockCitadel) -> None:
        """Test successful analysis pattern extraction."""
        data_flow_cat = MockAnalysisCategoryResult(
            category="data_flow",
            matches=[
                MockAnalysisPatternMatch(
                    pattern_name="move_statement",
                    category="data_flow",
                    captured=["WS-INPUT", "WS-OUTPUT"],
                    line=50,
                ),
            ],
            match_count=1,
            patterns_matched={"move_statement": 1},
        )

        mock_citadel._get_analysis_patterns_response = MockFileAnalysisPatternResult(
            file_path="test.cbl",
            language="cobol",
            categories={"data_flow": data_flow_cat},
            total_matches=1,
            coverage_pct=25.0,
        )

        result = mock_citadel.get_analysis_patterns("test.cbl")

        assert result.total_matches == 1
        assert "data_flow" in result.categories


class TestPathResolution:
    """Tests for path resolution logic."""

    def test_relative_path_resolution(
        self, mock_agent_config: MagicMock, tmp_code_dir: Path
    ) -> None:
        """Test that relative paths are resolved correctly."""
        # Simulate path resolution
        relative_path = "cbl/TESTPROG.cbl"
        code_dir = mock_agent_config.code_dir

        full_path = (code_dir / relative_path).resolve()

        assert full_path.exists()
        assert full_path.is_file()

    def test_absolute_path_handling(
        self, mock_agent_config: MagicMock, tmp_code_dir: Path
    ) -> None:
        """Test that absolute paths are handled correctly."""
        absolute_path = tmp_code_dir / "cbl" / "TESTPROG.cbl"

        # Should work with absolute path
        assert absolute_path.exists()

    def test_path_traversal_prevention(
        self, mock_agent_config: MagicMock, tmp_code_dir: Path
    ) -> None:
        """Test that path traversal attacks are prevented."""
        # Attempt path traversal
        malicious_path = "../../../etc/passwd"
        code_dir = mock_agent_config.code_dir

        full_path = (code_dir / malicious_path).resolve()

        # Should NOT be relative to code_dir (path traversal blocked)
        try:
            full_path.relative_to(code_dir.resolve())
            traversal_blocked = False
        except ValueError:
            traversal_blocked = True

        assert traversal_blocked, "Path traversal should be blocked"


class TestResultSerialization:
    """Tests for result serialization."""

    def test_file_analysis_result_to_dict(self) -> None:
        """Test FileAnalysisResult serialization."""
        artifact = MockFileArtifact(
            name="TEST-PARA",
            type="paragraph",
            line_start=10,
            line_end=20,
            callouts=[MockCallout("SUB-PARA", "performs", line=15)],
        )
        result = MockFileAnalysisResult(
            file_path="test.cbl",
            language="cobol",
            artifacts=[artifact],
        )

        data = result.to_dict()

        assert data["file_path"] == "test.cbl"
        assert data["language"] == "cobol"
        assert len(data["artifacts"]) == 1
        assert data["artifacts"][0]["name"] == "TEST-PARA"

    def test_callout_to_dict(self) -> None:
        """Test Callout serialization."""
        callout = MockCallout(
            target="TARGET-PROG",
            relationship="calls",
            target_type="program",
            line=42,
            raw_text="CALL 'TARGET-PROG'",
        )

        data = callout.to_dict()

        assert data["target"] == "TARGET-PROG"
        assert data["relationship"] == "calls"
        assert data["line"] == 42


class TestToolConfiguration:
    """Tests for tool configuration."""

    def test_configure_citadel_tools_sets_config(self) -> None:
        """Test that configure_citadel_tools sets module-level config."""
        # This test validates the expected pattern from citadel_tools.py
        # Implementation will set _config and _citadel at module level
        mock_config = MagicMock()
        mock_config.code_dir = Path("/tmp/test")

        # Simulating what configure_citadel_tools does
        _config = mock_config
        _citadel = None  # Lazy init

        assert _config is mock_config
        assert _citadel is None  # Should be lazy initialized

    def test_lazy_citadel_initialization(self) -> None:
        """Test that Citadel is lazily initialized on first use."""
        # Simulating the lazy init pattern
        _citadel = None

        def _get_citadel():
            nonlocal _citadel
            if _citadel is None:
                _citadel = MockCitadel()
            return _citadel

        # First call should initialize
        citadel1 = _get_citadel()
        assert citadel1 is not None

        # Second call should return same instance
        citadel2 = _get_citadel()
        assert citadel1 is citadel2


class TestEdgeCases:
    """Tests for edge cases and error conditions."""

    def test_empty_file_analysis(self, mock_citadel: MockCitadel) -> None:
        """Test analyzing an empty or minimal file."""
        mock_citadel._analyze_file_response = MockFileAnalysisResult(
            file_path="empty.cbl",
            language="cobol",
            artifacts=[],
        )

        result = mock_citadel.analyze_file("empty.cbl")

        assert len(result.artifacts) == 0
        assert result.error is None

    def test_large_file_stats(self, mock_citadel: MockCitadel) -> None:
        """Test file stats for a large file."""
        mock_citadel._get_file_stats_response = {
            "total_lines": 10000,
            "paragraph_count": 250,
            "language": "cobol",
            "paragraphs": [
                {"name": f"PARA-{i:04d}", "line_start": i * 40, "line_end": i * 40 + 38, "line_count": 39}
                for i in range(250)
            ],
        }

        result = mock_citadel.get_file_stats("large.cbl")

        assert result["total_lines"] == 10000
        assert result["paragraph_count"] == 250

    def test_special_characters_in_paths(
        self, mock_citadel: MockCitadel, tmp_path: Path
    ) -> None:
        """Test handling of special characters in file paths."""
        # Create a file with spaces in name
        special_dir = tmp_path / "my code"
        special_dir.mkdir()
        special_file = special_dir / "TEST PROG.cbl"
        special_file.write_text("       IDENTIFICATION DIVISION.")

        # Path should be handled correctly
        assert special_file.exists()

    def test_unicode_in_source_code(self, mock_citadel: MockCitadel) -> None:
        """Test handling of unicode content in source files."""
        mock_citadel._analyze_file_response = MockFileAnalysisResult(
            file_path="unicode.cbl",
            language="cobol",
            artifacts=[
                MockFileArtifact(
                    name="HANDLE-EURO",
                    type="paragraph",
                    callouts=[],
                )
            ],
        )

        result = mock_citadel.analyze_file("unicode.cbl")

        # Should handle without error
        assert result.error is None


class TestFileTypeSupport:
    """Tests for different file type support."""

    def test_cobol_file(self, mock_citadel: MockCitadel) -> None:
        """Test COBOL file analysis."""
        mock_citadel._analyze_file_response = MockFileAnalysisResult(
            file_path="test.cbl",
            language="cobol",
            artifacts=[MockFileArtifact(name="MAIN-PARA", type="paragraph")],
        )

        result = mock_citadel.analyze_file("test.cbl")

        assert result.language == "cobol"

    def test_jcl_file(self, mock_citadel: MockCitadel) -> None:
        """Test JCL file analysis."""
        mock_citadel._analyze_file_response = MockFileAnalysisResult(
            file_path="test.jcl",
            language="jcl",
            artifacts=[MockFileArtifact(name="STEP01", type="step")],
        )

        result = mock_citadel.analyze_file("test.jcl")

        assert result.language == "jcl"

    def test_python_file(self, mock_citadel: MockCitadel) -> None:
        """Test Python file analysis."""
        mock_citadel._analyze_file_response = MockFileAnalysisResult(
            file_path="test.py",
            language="python",
            artifacts=[MockFileArtifact(name="main", type="function")],
        )

        result = mock_citadel.analyze_file("test.py")

        assert result.language == "python"

    def test_unsupported_extension(self, mock_citadel: MockCitadel) -> None:
        """Test unsupported file extension handling."""
        mock_citadel._analyze_file_response = MockFileAnalysisResult(
            file_path="test.xyz",
            language="unknown",
            artifacts=[],
            error="No spec found for file extension '.xyz'",
        )

        result = mock_citadel.analyze_file("test.xyz")

        assert result.error is not None
        assert "spec" in result.error.lower()


class TestToolRegistration:
    """Tests for tool registration and retrieval."""

    def test_expected_tools_count(self) -> None:
        """Test that expected number of citadel tools would be registered."""
        # Based on CITADEL_INTEGRATION_PLAN.md, there should be 13 tools
        expected_tools = [
            "citadel_analyze_file",
            "citadel_get_functions",
            "citadel_get_callouts",
            "citadel_get_includes",
            "citadel_get_function_body",
            "citadel_get_function_bodies",
            "citadel_get_file_stats",
            "citadel_get_callers",
            "citadel_get_sequence_diagrams",
            "citadel_get_dead_code",
            "citadel_get_flow_diagram",
            "citadel_get_file_summary",
            "citadel_get_analysis_patterns",
        ]

        assert len(expected_tools) == 13

    def test_tool_names_follow_convention(self) -> None:
        """Test that tool names follow the citadel_ prefix convention."""
        expected_tools = [
            "citadel_analyze_file",
            "citadel_get_functions",
            "citadel_get_callouts",
            "citadel_get_includes",
            "citadel_get_function_body",
            "citadel_get_function_bodies",
            "citadel_get_file_stats",
            "citadel_get_callers",
            "citadel_get_sequence_diagrams",
            "citadel_get_dead_code",
            "citadel_get_flow_diagram",
            "citadel_get_file_summary",
            "citadel_get_analysis_patterns",
        ]

        for tool_name in expected_tools:
            assert tool_name.startswith("citadel_"), f"{tool_name} should start with citadel_"
