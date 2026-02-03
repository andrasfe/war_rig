"""Tests for the minion processor.

This module tests:
- MinionProcessor initialization and configuration
- Tool result summarization logic
- Threshold behavior
- Graceful error handling
"""

from unittest.mock import AsyncMock, MagicMock, patch

import pytest

from codewhisper.agent.minion import (
    DEFAULT_MINION_MODEL,
    MINION_THRESHOLD,
    MinionProcessor,
    ToolResultSummary,
)


class TestToolResultSummary:
    """Tests for the ToolResultSummary model."""

    def test_create_summary(self) -> None:
        """Test creating a tool result summary."""
        summary = ToolResultSummary(
            key_points=["Found 3 functions", "Uses CICS calls"],
            summary="The file contains authorization logic.",
        )

        assert len(summary.key_points) == 2
        assert "3 functions" in summary.key_points[0]
        assert "authorization" in summary.summary


class TestMinionProcessorInit:
    """Tests for MinionProcessor initialization."""

    def test_init_defaults(self, monkeypatch: pytest.MonkeyPatch) -> None:
        """Test initialization with defaults."""
        # Clear env var to test fallback
        monkeypatch.delenv("MINION_SCRIBE_MODEL", raising=False)

        processor = MinionProcessor()

        assert processor.model_name == DEFAULT_MINION_MODEL
        assert processor.threshold == MINION_THRESHOLD

    def test_init_from_env(self, monkeypatch: pytest.MonkeyPatch) -> None:
        """Test initialization from environment variable."""
        monkeypatch.setenv("MINION_SCRIBE_MODEL", "test/custom-model")

        processor = MinionProcessor()

        assert processor.model_name == "test/custom-model"

    def test_init_explicit_model(self) -> None:
        """Test initialization with explicit model name."""
        processor = MinionProcessor(model_name="explicit/model")

        assert processor.model_name == "explicit/model"

    def test_init_explicit_threshold(self) -> None:
        """Test initialization with explicit threshold."""
        processor = MinionProcessor(threshold=5000)

        assert processor.threshold == 5000


class TestMinionProcessorSummarize:
    """Tests for summarization logic."""

    @pytest.fixture
    def processor(self) -> MinionProcessor:
        """Create a processor for testing."""
        return MinionProcessor(threshold=100)

    async def test_small_result_passes_through(
        self,
        processor: MinionProcessor,
    ) -> None:
        """Test that small results are not summarized."""
        small_result = "Found 2 matches."

        result = await processor.summarize_result("search_code", small_result)

        assert result == small_result

    async def test_exact_threshold_passes_through(
        self,
        processor: MinionProcessor,
    ) -> None:
        """Test that results at exactly threshold pass through."""
        # Create a result exactly at threshold (100 chars)
        exact_result = "x" * 100

        result = await processor.summarize_result("tool", exact_result)

        assert result == exact_result

    async def test_large_result_triggers_summarization(
        self,
        processor: MinionProcessor,
        monkeypatch: pytest.MonkeyPatch,
    ) -> None:
        """Test that large results are summarized."""
        monkeypatch.setenv("OPENROUTER_API_KEY", "test-key")

        # Create mock LLM response
        mock_summary = ToolResultSummary(
            key_points=["Point 1", "Point 2"],
            summary="This is the summary.",
        )

        mock_llm = MagicMock()
        mock_llm.ainvoke = AsyncMock(return_value=mock_summary)

        # Inject mock LLM
        processor._llm = mock_llm

        large_result = "x" * 200  # Above threshold

        result = await processor.summarize_result("read_file", large_result)

        # Verify summarization was called
        mock_llm.ainvoke.assert_called_once()

        # Verify result contains summary
        assert "[Summarized from 200 chars]" in result
        assert "Point 1" in result
        assert "Point 2" in result
        assert "This is the summary." in result

    async def test_summarization_error_returns_original(
        self,
        processor: MinionProcessor,
        monkeypatch: pytest.MonkeyPatch,
    ) -> None:
        """Test graceful fallback on summarization error."""
        monkeypatch.setenv("OPENROUTER_API_KEY", "test-key")

        # Create mock LLM that raises an error
        mock_llm = MagicMock()
        mock_llm.ainvoke = AsyncMock(side_effect=Exception("API error"))

        processor._llm = mock_llm

        large_result = "x" * 200

        result = await processor.summarize_result("tool", large_result)

        # Should return original on error
        assert result == large_result


class TestMinionProcessorLLM:
    """Tests for LLM creation and caching."""

    def test_llm_property_creates_on_first_access(
        self,
        monkeypatch: pytest.MonkeyPatch,
    ) -> None:
        """Test that LLM is created on first access."""
        monkeypatch.setenv("OPENROUTER_API_KEY", "test-key")

        processor = MinionProcessor()
        assert processor._llm is None

        # Access llm property - patch at langchain_openai module level
        with patch("langchain_openai.ChatOpenAI") as mock_chat:
            mock_instance = MagicMock()
            mock_instance.with_structured_output = MagicMock(
                return_value=mock_instance
            )
            mock_chat.return_value = mock_instance

            llm = processor.llm

            assert llm is not None
            mock_chat.assert_called_once()

    def test_llm_requires_api_key(
        self,
        monkeypatch: pytest.MonkeyPatch,
    ) -> None:
        """Test that LLM creation requires API key."""
        monkeypatch.delenv("OPENROUTER_API_KEY", raising=False)

        processor = MinionProcessor()

        with pytest.raises(ValueError, match="OPENROUTER_API_KEY"):
            _ = processor.llm

    def test_llm_property_caches_result(
        self,
        monkeypatch: pytest.MonkeyPatch,
    ) -> None:
        """Test that LLM is cached after first creation."""
        monkeypatch.setenv("OPENROUTER_API_KEY", "test-key")

        processor = MinionProcessor()

        with patch("langchain_openai.ChatOpenAI") as mock_chat:
            mock_instance = MagicMock()
            mock_instance.with_structured_output = MagicMock(
                return_value=mock_instance
            )
            mock_chat.return_value = mock_instance

            llm1 = processor.llm
            llm2 = processor.llm

            # Should only create once
            mock_chat.assert_called_once()
            assert llm1 is llm2
