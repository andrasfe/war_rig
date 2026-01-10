"""Tests for Scribe agent."""

import pytest

from war_rig.agents.scribe import ScribeAgent, ScribeInput, ScribeOutput
from war_rig.config import ScribeConfig
from war_rig.models.templates import FileType
from war_rig.preprocessors.cobol import COBOLPreprocessor


class TestScribeAgent:
    """Tests for ScribeAgent."""

    @pytest.fixture
    def scribe_config(self) -> ScribeConfig:
        """Create scribe configuration."""
        return ScribeConfig(
            model="claude-sonnet-4-20250514",
            temperature=0.1,
            max_tokens=1000,
        )

    @pytest.fixture
    def scribe(self, scribe_config) -> ScribeAgent:
        """Create scribe agent instance."""
        return ScribeAgent(scribe_config)

    def test_create_scribe(self, scribe):
        """Test scribe creation."""
        assert scribe.name == "Scribe"
        assert scribe.config.temperature == 0.1

    def test_build_system_prompt(self, scribe):
        """Test system prompt generation."""
        prompt = scribe._build_system_prompt()
        assert "Scribe" in prompt
        assert "documentation" in prompt.lower()
        assert "citation" in prompt.lower()

    def test_build_user_prompt(self, scribe, sample_cobol_source):
        """Test user prompt generation."""
        input_data = ScribeInput(
            source_code=sample_cobol_source,
            file_name="TESTPROG.cbl",
            file_type=FileType.COBOL,
            iteration=1,
        )

        prompt = scribe._build_user_prompt(input_data)

        assert "TESTPROG.cbl" in prompt
        assert "COBOL" in prompt
        assert "Iteration: 1" in prompt
        assert sample_cobol_source[:50] in prompt or "IDENTIFICATION" in prompt

    def test_build_user_prompt_with_questions(self, scribe, sample_cobol_source, sample_challenger_question):
        """Test user prompt with Challenger questions."""
        input_data = ScribeInput(
            source_code=sample_cobol_source,
            file_name="TESTPROG.cbl",
            file_type=FileType.COBOL,
            iteration=2,
            challenger_questions=[sample_challenger_question],
        )

        prompt = scribe._build_user_prompt(input_data)

        assert "Challenger Questions" in prompt
        assert sample_challenger_question.question in prompt

    def test_mock_output(self, scribe, sample_cobol_source):
        """Test mock output generation."""
        preprocessor = COBOLPreprocessor()
        preprocess_result = preprocessor.process(sample_cobol_source, "TESTPROG.cbl")

        input_data = ScribeInput(
            source_code=sample_cobol_source,
            file_name="TESTPROG.cbl",
            file_type=FileType.COBOL,
            preprocessor_result=preprocess_result,
            iteration=1,
        )

        output = scribe.create_mock_output(input_data)

        assert isinstance(output, ScribeOutput)
        assert output.success is True
        assert output.template is not None
        assert output.template.header.program_id == "TESTPROG"
        assert output.confidence is not None

    def test_error_output(self, scribe, sample_cobol_source):
        """Test error output generation."""
        input_data = ScribeInput(
            source_code=sample_cobol_source,
            file_name="TESTPROG.cbl",
            file_type=FileType.COBOL,
            iteration=1,
        )

        output = scribe._create_error_output("Test error", input_data)

        assert isinstance(output, ScribeOutput)
        assert output.success is False
        assert output.error == "Test error"


class TestScribeInput:
    """Tests for ScribeInput model."""

    def test_create_minimal_input(self, sample_cobol_source):
        """Test creating input with required fields."""
        inp = ScribeInput(
            source_code=sample_cobol_source,
            file_name="TEST.cbl",
            file_type=FileType.COBOL,
        )

        assert inp.source_code == sample_cobol_source
        assert inp.file_name == "TEST.cbl"
        assert inp.iteration == 1  # Default

    def test_create_full_input(self, sample_cobol_source, sample_template, sample_challenger_question, sample_chrome_ticket):
        """Test creating input with all fields."""
        inp = ScribeInput(
            source_code=sample_cobol_source,
            file_name="TEST.cbl",
            file_type=FileType.COBOL,
            previous_template=sample_template,
            challenger_questions=[sample_challenger_question],
            chrome_tickets=[sample_chrome_ticket],
            iteration=2,
        )

        assert inp.iteration == 2
        assert inp.previous_template is not None
        assert len(inp.challenger_questions) == 1
        assert len(inp.chrome_tickets) == 1


class TestScribeOutput:
    """Tests for ScribeOutput model."""

    def test_create_success_output(self, sample_template, sample_confidence):
        """Test creating successful output."""
        output = ScribeOutput(
            success=True,
            template=sample_template,
            confidence=sample_confidence,
            responses=[],
            open_questions=["What about edge cases?"],
            tokens_used=1000,
        )

        assert output.success is True
        assert output.template is not None
        assert output.tokens_used == 1000

    def test_create_error_output(self):
        """Test creating error output."""
        output = ScribeOutput(
            success=False,
            error="Something went wrong",
        )

        assert output.success is False
        assert output.error == "Something went wrong"
        assert output.template is None
