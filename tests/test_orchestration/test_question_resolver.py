"""Tests for the QuestionResolver.

Verifies automatic open question resolution using CodeWhisper SDK,
including component template questions and README inline questions.
"""

from __future__ import annotations

import json
from pathlib import Path
from unittest.mock import AsyncMock, MagicMock, patch

import pytest

from war_rig.agents.imperator import (
    HolisticReviewOutput,
    ImperatorHolisticDecision,
    InlineQuestion,
)
from war_rig.config import QuestionResolutionConfig
from war_rig.models.templates import (
    ResolvedQuestion,
)
from war_rig.orchestration.question_resolver import (
    QuestionResolver,
)

# =============================================================================
# Fixtures
# =============================================================================


@pytest.fixture
def config() -> QuestionResolutionConfig:
    """Default enabled configuration."""
    return QuestionResolutionConfig(
        enabled=True,
        max_questions_per_cycle=10,
        timeout_per_question=120,
        min_confidence="MEDIUM",
        codewhisper_max_iterations=8,
        codewhisper_temperature=0.2,
        codewhisper_max_tokens=2048,
        resolve_readme_questions=True,
    )


@pytest.fixture
def disabled_config() -> QuestionResolutionConfig:
    """Disabled configuration."""
    return QuestionResolutionConfig(enabled=False)


@pytest.fixture
def output_dir(tmp_path: Path) -> Path:
    """Create output directory with sample .doc.json files."""
    out = tmp_path / "output"
    out.mkdir()
    return out


@pytest.fixture
def input_dir(tmp_path: Path) -> Path:
    """Create input directory."""
    inp = tmp_path / "input"
    inp.mkdir()
    return inp


@pytest.fixture
def mock_review_result() -> HolisticReviewOutput:
    """Create a minimal HolisticReviewOutput."""
    return HolisticReviewOutput(
        decision=ImperatorHolisticDecision.NEEDS_CLARIFICATION,
        system_design_questions=[],
    )


@pytest.fixture
def sample_doc_json() -> dict:
    """Sample .doc.json data with open questions."""
    return {
        "header": {
            "file_name": "TESTPROG.cbl",
            "program_id": "TESTPROG",
            "language": "COBOL",
            "analyzed_by": "WAR-RIG",
        },
        "purpose": {
            "summary": "Test program for validation",
        },
        "open_questions": [
            {
                "question": "What triggers the batch job?",
                "context": "No JCL reference found",
                "suggestion": "Check JCL libraries",
            },
            {
                "question": "What is the DB2 tablespace?",
                "context": "SQL detected but tablespace unclear",
            },
        ],
    }


def _mock_completion(content: str, tool_calls: int = 2) -> MagicMock:
    """Create a mock CompletionResult."""
    result = MagicMock()
    result.content = content
    result.tool_calls_made = tool_calls
    result.iterations = 1
    return result


def _mock_sdk(answer: str = "Found at line 42 in PERFORM MAIN-PROCESS.") -> MagicMock:
    """Create a mock CodeWhisper SDK."""
    sdk = MagicMock()
    sdk.reset = MagicMock()
    sdk.complete = AsyncMock(return_value=_mock_completion(answer))
    return sdk


# =============================================================================
# Tests: _collect_component_questions
# =============================================================================


class TestCollectComponentQuestions:
    """Tests for collecting questions from .doc.json files."""

    def test_finds_questions_from_doc_json(
        self, config: QuestionResolutionConfig, output_dir: Path,
        input_dir: Path, sample_doc_json: dict,
    ) -> None:
        resolver = QuestionResolver(config, output_dir, input_dir, cycle=1)

        doc_path = output_dir / "TESTPROG.doc.json"
        doc_path.write_text(json.dumps(sample_doc_json), encoding="utf-8")

        questions = resolver._collect_component_questions()
        assert len(questions) == 2
        assert questions[0].question == "What triggers the batch job?"
        assert questions[0].file_name == "TESTPROG"
        assert questions[0].question_index == 0
        assert questions[1].question_index == 1

    def test_skips_files_without_questions(
        self, config: QuestionResolutionConfig, output_dir: Path,
        input_dir: Path,
    ) -> None:
        resolver = QuestionResolver(config, output_dir, input_dir, cycle=1)

        doc = {"header": {"file_name": "CLEAN.cbl"}, "open_questions": []}
        (output_dir / "CLEAN.doc.json").write_text(
            json.dumps(doc), encoding="utf-8"
        )

        questions = resolver._collect_component_questions()
        assert len(questions) == 0

    def test_handles_malformed_json(
        self, config: QuestionResolutionConfig, output_dir: Path,
        input_dir: Path,
    ) -> None:
        resolver = QuestionResolver(config, output_dir, input_dir, cycle=1)

        (output_dir / "BAD.doc.json").write_text("not json", encoding="utf-8")

        questions = resolver._collect_component_questions()
        assert len(questions) == 0


# =============================================================================
# Tests: _collect_readme_questions
# =============================================================================


class TestCollectReadmeQuestions:
    """Tests for collecting inline questions from README."""

    def test_parses_question_markers_from_readme(
        self, config: QuestionResolutionConfig, output_dir: Path,
        input_dir: Path,
    ) -> None:
        resolver = QuestionResolver(config, output_dir, input_dir, cycle=1)

        readme = output_dir / "README.md"
        readme.write_text(
            "# System Design\n\n"
            "## Architecture\n\n"
            "The system processes batches.\n"
            "❓ QUESTION: How are batches scheduled?\n"
            "More text here.\n"
            "❓ QUESTION: What is the retry policy?\n",
            encoding="utf-8",
        )

        review = HolisticReviewOutput(
            decision=ImperatorHolisticDecision.NEEDS_CLARIFICATION,
            system_design_questions=[],
        )

        questions = resolver._collect_readme_questions(review)
        assert len(questions) == 2
        assert questions[0].question == "How are batches scheduled?"
        assert questions[1].question == "What is the retry policy?"

    def test_parses_bold_question_markers(
        self, config: QuestionResolutionConfig, output_dir: Path,
        input_dir: Path,
    ) -> None:
        resolver = QuestionResolver(config, output_dir, input_dir, cycle=1)

        readme = output_dir / "README.md"
        readme.write_text(
            "- ❓ **QUESTION**: What compiler version?\n"
            "- ❓ **QUESTION**: What load library naming?\n",
            encoding="utf-8",
        )

        review = HolisticReviewOutput(
            decision=ImperatorHolisticDecision.NEEDS_CLARIFICATION,
            system_design_questions=[],
        )

        questions = resolver._collect_readme_questions(review)
        assert len(questions) == 2
        assert questions[0].question == "What compiler version?"

    def test_includes_system_design_questions(
        self, config: QuestionResolutionConfig, output_dir: Path,
        input_dir: Path,
    ) -> None:
        resolver = QuestionResolver(config, output_dir, input_dir, cycle=1)

        review = HolisticReviewOutput(
            decision=ImperatorHolisticDecision.NEEDS_CLARIFICATION,
            system_design_questions=[
                InlineQuestion(
                    question_id="Q001",
                    question_text="What is the DB2 version?",
                    context="Database section",
                    related_files=["DBPROG.cbl"],
                ),
            ],
        )

        questions = resolver._collect_readme_questions(review)
        assert len(questions) == 1
        assert questions[0].question == "What is the DB2 version?"

    def test_deduplicates_questions(
        self, config: QuestionResolutionConfig, output_dir: Path,
        input_dir: Path,
    ) -> None:
        resolver = QuestionResolver(config, output_dir, input_dir, cycle=1)

        readme = output_dir / "README.md"
        readme.write_text(
            "❓ QUESTION: What is the DB2 version?\n",
            encoding="utf-8",
        )

        review = HolisticReviewOutput(
            decision=ImperatorHolisticDecision.NEEDS_CLARIFICATION,
            system_design_questions=[
                InlineQuestion(
                    question_id="Q001",
                    question_text="What is the DB2 version?",
                    context="Database section",
                ),
            ],
        )

        questions = resolver._collect_readme_questions(review)
        # Should not duplicate - InlineQuestion already captured it
        assert len(questions) == 1


# =============================================================================
# Tests: _assess_answer_quality
# =============================================================================


class TestAssessAnswerQuality:
    """Tests for answer quality assessment."""

    def test_high_confidence_with_line_numbers(
        self, config: QuestionResolutionConfig, output_dir: Path,
        input_dir: Path,
    ) -> None:
        resolver = QuestionResolver(config, output_dir, input_dir, cycle=1)
        is_good, confidence = resolver._assess_answer_quality(
            "Found at line 42 in MAIN-PROCESS paragraph."
        )
        assert is_good is True
        assert confidence == "HIGH"

    def test_high_confidence_with_cobol_identifiers(
        self, config: QuestionResolutionConfig, output_dir: Path,
        input_dir: Path,
    ) -> None:
        resolver = QuestionResolver(config, output_dir, input_dir, cycle=1)
        is_good, confidence = resolver._assess_answer_quality(
            "The PERFORM PROCESS-BATCH section handles this."
        )
        assert is_good is True
        assert confidence == "HIGH"

    def test_medium_confidence_default(
        self, config: QuestionResolutionConfig, output_dir: Path,
        input_dir: Path,
    ) -> None:
        resolver = QuestionResolver(config, output_dir, input_dir, cycle=1)
        is_good, confidence = resolver._assess_answer_quality(
            "The batch is triggered by a timer service."
        )
        assert is_good is True
        assert confidence == "MEDIUM"

    def test_inconclusive_prefix(
        self, config: QuestionResolutionConfig, output_dir: Path,
        input_dir: Path,
    ) -> None:
        resolver = QuestionResolver(config, output_dir, input_dir, cycle=1)
        is_good, confidence = resolver._assess_answer_quality(
            "INCONCLUSIVE: Could not find relevant code."
        )
        assert is_good is False
        assert confidence == "LOW"

    def test_inconclusive_markdown_bold(
        self, config: QuestionResolutionConfig, output_dir: Path,
        input_dir: Path,
    ) -> None:
        resolver = QuestionResolver(config, output_dir, input_dir, cycle=1)
        is_good, confidence = resolver._assess_answer_quality(
            "**INCONCLUSIVE:** The procedure is not defined in the accessible source code."
        )
        assert is_good is False
        assert confidence == "LOW"

    def test_hedging_language(
        self, config: QuestionResolutionConfig, output_dir: Path,
        input_dir: Path,
    ) -> None:
        resolver = QuestionResolver(config, output_dir, input_dir, cycle=1)
        is_good, confidence = resolver._assess_answer_quality(
            "It is unclear from the code what triggers this."
        )
        assert is_good is False
        assert confidence == "LOW"

    def test_empty_answer(
        self, config: QuestionResolutionConfig, output_dir: Path,
        input_dir: Path,
    ) -> None:
        resolver = QuestionResolver(config, output_dir, input_dir, cycle=1)
        is_good, confidence = resolver._assess_answer_quality("")
        assert is_good is False
        assert confidence == "LOW"

    def test_codewhisper_max_iterations_rejected(
        self, config: QuestionResolutionConfig, output_dir: Path,
        input_dir: Path,
    ) -> None:
        resolver = QuestionResolver(config, output_dir, input_dir, cycle=1)
        is_good, confidence = resolver._assess_answer_quality(
            "I reached the maximum number of iterations (8) while processing "
            "your request. During my analysis, I made 5 tool calls using: "
            "load_skill, search_skills. You may want to ask a more specific "
            "follow-up question to continue the analysis."
        )
        assert is_good is False
        assert confidence == "LOW"

    def test_codewhisper_tool_calls_noise_rejected(
        self, config: QuestionResolutionConfig, output_dir: Path,
        input_dir: Path,
    ) -> None:
        resolver = QuestionResolver(config, output_dir, input_dir, cycle=1)
        is_good, confidence = resolver._assess_answer_quality(
            "During my analysis, I made 3 tool calls using: search_skills, "
            "load_skill. Unfortunately I could not complete the investigation."
        )
        assert is_good is False
        assert confidence == "LOW"

    def test_codewhisper_followup_noise_rejected(
        self, config: QuestionResolutionConfig, output_dir: Path,
        input_dir: Path,
    ) -> None:
        resolver = QuestionResolver(config, output_dir, input_dir, cycle=1)
        is_good, confidence = resolver._assess_answer_quality(
            "You may want to ask a more specific follow-up question to "
            "continue the analysis."
        )
        assert is_good is False
        assert confidence == "LOW"


# =============================================================================
# Tests: _update_template
# =============================================================================


class TestUpdateTemplate:
    """Tests for template update (open -> resolved)."""

    def test_moves_question_from_open_to_resolved(
        self, config: QuestionResolutionConfig, output_dir: Path,
        input_dir: Path, sample_doc_json: dict,
    ) -> None:
        resolver = QuestionResolver(config, output_dir, input_dir, cycle=1)

        doc_path = output_dir / "TESTPROG.doc.json"
        doc_path.write_text(json.dumps(sample_doc_json), encoding="utf-8")

        resolved = ResolvedQuestion(
            original_question="What triggers the batch job?",
            original_context="No JCL reference found",
            answer="Triggered by PERFORM BATCH-INIT at line 100.",
            confidence="HIGH",
            cycle_resolved=1,
            tool_calls_used=3,
        )

        with patch.object(resolver, "_regenerate_markdown"):
            resolver._update_template(doc_path, 0, resolved)

        updated = json.loads(doc_path.read_text(encoding="utf-8"))
        assert len(updated["open_questions"]) == 1  # Was 2, now 1
        assert len(updated["resolved_questions"]) == 1
        assert updated["resolved_questions"][0]["answer"] == (
            "Triggered by PERFORM BATCH-INIT at line 100."
        )


# =============================================================================
# Tests: _update_readme
# =============================================================================


class TestUpdateReadme:
    """Tests for README question marker replacement."""

    def test_replaces_question_marker_with_answer(
        self, config: QuestionResolutionConfig, output_dir: Path,
        input_dir: Path,
    ) -> None:
        resolver = QuestionResolver(config, output_dir, input_dir, cycle=1)

        readme = output_dir / "README.md"
        readme.write_text(
            "# System Design\n\n"
            "❓ QUESTION: How are batches scheduled?\n\n"
            "More text.\n",
            encoding="utf-8",
        )

        resolver._update_readme(
            {"How are batches scheduled?": "Batches run via JCL CRON at midnight."}
        )

        content = readme.read_text(encoding="utf-8")
        assert "❓ QUESTION:" not in content
        assert "Batches run via JCL CRON at midnight." in content

    def test_replaces_bold_question_marker(
        self, config: QuestionResolutionConfig, output_dir: Path,
        input_dir: Path,
    ) -> None:
        resolver = QuestionResolver(config, output_dir, input_dir, cycle=1)

        readme = output_dir / "README.md"
        readme.write_text(
            "- ❓ **QUESTION**: What compiler version?\n",
            encoding="utf-8",
        )

        resolver._update_readme(
            {"What compiler version?": "IGYCRCTL V6.4"}
        )

        content = readme.read_text(encoding="utf-8")
        assert "❓" not in content
        assert "IGYCRCTL V6.4" in content


# =============================================================================
# Tests: resolve_all (integration)
# =============================================================================


class TestResolveAll:
    """Integration tests for the full resolve_all flow."""

    @pytest.mark.asyncio
    async def test_max_questions_cap(
        self, config: QuestionResolutionConfig, output_dir: Path,
        input_dir: Path, mock_review_result: HolisticReviewOutput,
    ) -> None:
        config.max_questions_per_cycle = 1

        # Create file with 3 questions
        doc = {
            "header": {"file_name": "BIG.cbl", "program_id": "BIG"},
            "open_questions": [
                {"question": f"Question {i}?"} for i in range(3)
            ],
        }
        (output_dir / "BIG.doc.json").write_text(
            json.dumps(doc), encoding="utf-8"
        )

        mock_sdk = _mock_sdk("Found at line 10 in MAIN-PARA.")
        resolver = QuestionResolver(config, output_dir, input_dir, cycle=1)

        with patch.object(resolver, "_create_sdk", return_value=mock_sdk), \
             patch.object(resolver, "_update_template"), \
             patch.object(resolver, "_regenerate_markdown"):
            result = await resolver.resolve_all(mock_review_result)

        # Only 1 should be attempted due to cap
        assert result.component_questions_found == 3
        assert result.component_questions_resolved <= 1

    @pytest.mark.asyncio
    async def test_graceful_degradation_sdk_error(
        self, config: QuestionResolutionConfig, output_dir: Path,
        input_dir: Path, mock_review_result: HolisticReviewOutput,
        sample_doc_json: dict,
    ) -> None:
        (output_dir / "TESTPROG.doc.json").write_text(
            json.dumps(sample_doc_json), encoding="utf-8"
        )

        resolver = QuestionResolver(config, output_dir, input_dir, cycle=1)

        with patch.object(
            resolver, "_create_sdk", side_effect=RuntimeError("No provider")
        ):
            result = await resolver.resolve_all(mock_review_result)

        assert result.component_questions_found == 2
        assert result.component_questions_resolved == 0
        assert len(result.errors) == 1
        assert "SDK creation failed" in result.errors[0]

    @pytest.mark.asyncio
    async def test_timeout_handling(
        self, config: QuestionResolutionConfig, output_dir: Path,
        input_dir: Path, mock_review_result: HolisticReviewOutput,
        sample_doc_json: dict,
    ) -> None:
        config.timeout_per_question = 1  # Very short

        (output_dir / "TESTPROG.doc.json").write_text(
            json.dumps(sample_doc_json), encoding="utf-8"
        )

        # SDK that times out
        import asyncio

        async def slow_complete(*args, **kwargs):
            await asyncio.sleep(10)
            return _mock_completion("answer")

        mock_sdk = MagicMock()
        mock_sdk.reset = MagicMock()
        mock_sdk.complete = slow_complete

        resolver = QuestionResolver(config, output_dir, input_dir, cycle=1)

        with patch.object(resolver, "_create_sdk", return_value=mock_sdk):
            result = await resolver.resolve_all(mock_review_result)

        # Should not crash, questions remain unresolved
        assert result.component_questions_resolved == 0

    @pytest.mark.asyncio
    async def test_disabled_config_is_noop(
        self, disabled_config: QuestionResolutionConfig, output_dir: Path,
        input_dir: Path, mock_review_result: HolisticReviewOutput,
    ) -> None:
        resolver = QuestionResolver(
            disabled_config, output_dir, input_dir, cycle=1
        )

        result = await resolver.resolve_all(mock_review_result)

        assert result.component_questions_found == 0
        assert result.component_questions_resolved == 0
        assert result.readme_questions_found == 0
        assert result.readme_questions_resolved == 0

    @pytest.mark.asyncio
    async def test_full_resolution_flow(
        self, config: QuestionResolutionConfig, output_dir: Path,
        input_dir: Path, sample_doc_json: dict,
    ) -> None:
        """End-to-end: question gets resolved, template updated, readme updated."""
        (output_dir / "TESTPROG.doc.json").write_text(
            json.dumps(sample_doc_json), encoding="utf-8"
        )

        readme = output_dir / "README.md"
        readme.write_text(
            "# Design\n❓ QUESTION: What DB2 plan is used?\n",
            encoding="utf-8",
        )

        review = HolisticReviewOutput(
            decision=ImperatorHolisticDecision.NEEDS_CLARIFICATION,
            system_design_questions=[],
        )

        mock_sdk = _mock_sdk("The PERFORM BATCH-INIT at line 100 handles this.")
        resolver = QuestionResolver(config, output_dir, input_dir, cycle=1)

        with patch.object(resolver, "_create_sdk", return_value=mock_sdk), \
             patch.object(resolver, "_regenerate_markdown"):
            result = await resolver.resolve_all(review)

        assert result.component_questions_resolved == 2
        assert result.readme_questions_resolved == 1

        # Verify template was updated
        updated = json.loads(
            (output_dir / "TESTPROG.doc.json").read_text(encoding="utf-8")
        )
        assert len(updated["open_questions"]) == 0
        assert len(updated["resolved_questions"]) == 2

        # Verify README was updated
        readme_content = readme.read_text(encoding="utf-8")
        assert "❓ QUESTION:" not in readme_content
