"""Tests for TokenEstimator."""

import pytest

from war_rig.chunking.estimator import PromptTokenEstimate, TokenEstimator


class TestTokenEstimator:
    """Tests for TokenEstimator."""

    def test_estimate_tokens_empty(self):
        """Empty text should return 0 tokens."""
        estimator = TokenEstimator()
        assert estimator.estimate_tokens("") == 0

    def test_estimate_tokens_simple(self):
        """Simple text should estimate based on character count."""
        estimator = TokenEstimator()
        # 40 chars / 4 chars per token = 10 tokens
        text = "a" * 40
        assert estimator.estimate_tokens(text) == 10

    def test_estimate_tokens_cobol_ratio(self):
        """COBOL text should use lower chars-per-token ratio."""
        estimator = TokenEstimator()
        # 35 chars / 3.5 chars per token = 10 tokens
        text = "a" * 35
        assert estimator.estimate_tokens(text, is_cobol=True) == 10

    def test_estimate_source_tokens_detects_cobol(self):
        """Should detect COBOL based on content."""
        estimator = TokenEstimator()
        cobol_code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       PROCEDURE DIVISION.
           DISPLAY "HELLO".
           STOP RUN.
        """
        # Should use COBOL ratio
        tokens = estimator.estimate_source_tokens(cobol_code)
        assert tokens > 0

    def test_estimate_prompt_tokens_structure(self):
        """Should return PromptTokenEstimate with all fields."""
        estimator = TokenEstimator()

        # TODO: Create a mock ScribeInput for testing
        # For now, test that the estimate structure is correct
        estimate = PromptTokenEstimate()
        assert estimate.total == 0
        assert estimate.source_code_percentage == 0.0

    def test_prompt_token_breakdown(self):
        """PromptTokenEstimate should sum components correctly."""
        estimate = PromptTokenEstimate(
            system_prompt=2000,
            template_schema=1500,
            source_code=5000,
            instructions=500,
        )
        assert estimate.total == 9000
        assert estimate.source_code_percentage == pytest.approx(55.56, rel=0.01)


class TestCustomRatios:
    """Tests for custom character-per-token ratios."""

    def test_custom_default_ratio(self):
        """Should use custom default ratio when specified."""
        estimator = TokenEstimator(chars_per_token=5.0)
        # 50 chars / 5 chars per token = 10 tokens
        assert estimator.estimate_tokens("a" * 50) == 10

    def test_custom_cobol_ratio(self):
        """Should use custom COBOL ratio when specified."""
        estimator = TokenEstimator(cobol_chars_per_token=5.0)
        # 50 chars / 5 chars per token = 10 tokens
        assert estimator.estimate_tokens("a" * 50, is_cobol=True) == 10
