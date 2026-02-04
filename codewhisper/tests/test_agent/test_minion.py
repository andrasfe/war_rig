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
    DEFAULT_MINION_THRESHOLD_TOKENS,
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

    def test_parse_from_text_valid(self) -> None:
        """Test parsing summary from well-formatted text."""
        text = """KEY POINTS:
- First key point
- Second key point
- Third key point

SUMMARY:
This is the summary of the tool output."""

        result = ToolResultSummary.parse_from_text(text)

        assert len(result.key_points) == 3
        assert "First key point" in result.key_points[0]
        assert "Second key point" in result.key_points[1]
        assert "Third key point" in result.key_points[2]
        assert "summary of the tool output" in result.summary

    def test_parse_from_text_asterisk_bullets(self) -> None:
        """Test parsing with asterisk bullets."""
        text = """KEY POINTS:
* Point one
* Point two

SUMMARY:
Summary text here."""

        result = ToolResultSummary.parse_from_text(text)

        assert len(result.key_points) == 2
        assert "Point one" in result.key_points[0]

    def test_parse_from_text_fallback(self) -> None:
        """Test fallback when parsing fails."""
        text = "Some text without proper formatting"

        result = ToolResultSummary.parse_from_text(text)

        # Should fall back to using text as summary
        assert result.summary == text
        assert result.key_points == []

    def test_parse_from_text_long_fallback_truncates(self) -> None:
        """Test fallback truncates long text."""
        text = "x" * 600

        result = ToolResultSummary.parse_from_text(text)

        assert len(result.summary) <= 503  # 500 + "..."
        assert result.summary.endswith("...")


class TestMinionProcessorInit:
    """Tests for MinionProcessor initialization."""

    def test_init_defaults(self, monkeypatch: pytest.MonkeyPatch) -> None:
        """Test initialization with defaults."""
        # Clear env vars to test fallback
        monkeypatch.delenv("MINION_SCRIBE_MODEL", raising=False)
        monkeypatch.delenv("MINION_CONTEXT_THRESHOLD", raising=False)

        processor = MinionProcessor()

        assert processor.model_name == DEFAULT_MINION_MODEL
        # Default threshold: 8000 tokens * 4 chars/token = 32000 chars
        assert processor.threshold == DEFAULT_MINION_THRESHOLD_TOKENS * 4

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
    ) -> None:
        """Test that large results are summarized."""
        # Create mock provider response
        mock_response = MagicMock()
        mock_response.content = """KEY POINTS:
- Point 1
- Point 2

SUMMARY:
This is the summary."""

        mock_provider = MagicMock()
        mock_provider.complete = AsyncMock(return_value=mock_response)

        # Inject mock provider
        processor._provider = mock_provider

        large_result = "x" * 200  # Above threshold

        result = await processor.summarize_result("read_file", large_result)

        # Verify complete was called
        mock_provider.complete.assert_called_once()

        # Verify result contains summary
        assert "[Summarized from 200 chars]" in result
        assert "Point 1" in result
        assert "Point 2" in result
        assert "This is the summary." in result

    async def test_summarization_error_returns_original(
        self,
        processor: MinionProcessor,
    ) -> None:
        """Test graceful fallback on summarization error."""
        # Create mock provider that raises an error
        mock_provider = MagicMock()
        mock_provider.complete = AsyncMock(side_effect=Exception("API error"))

        processor._provider = mock_provider

        large_result = "x" * 200

        result = await processor.summarize_result("tool", large_result)

        # Should return original on error
        assert result == large_result


class TestMinionProcessorProvider:
    """Tests for provider creation and caching."""

    def test_provider_property_creates_on_first_access(
        self,
        monkeypatch: pytest.MonkeyPatch,
    ) -> None:
        """Test that provider is created on first access."""
        # Set up provider config - default is openrouter
        monkeypatch.delenv("LLM_PROVIDER", raising=False)
        monkeypatch.setenv("OPENROUTER_API_KEY", "test-key")

        processor = MinionProcessor()
        assert processor._provider is None

        # Access provider property
        with patch("codewhisper.agent.minion.get_provider_from_env") as mock_get_provider:
            mock_provider = MagicMock()
            mock_get_provider.return_value = mock_provider

            provider = processor.provider

            assert provider is not None
            mock_get_provider.assert_called_once()

    def test_provider_property_caches_result(
        self,
        monkeypatch: pytest.MonkeyPatch,
    ) -> None:
        """Test that provider is cached after first creation."""
        # Set up provider config - default is openrouter
        monkeypatch.delenv("LLM_PROVIDER", raising=False)
        monkeypatch.setenv("OPENROUTER_API_KEY", "test-key")

        processor = MinionProcessor()

        with patch("codewhisper.agent.minion.get_provider_from_env") as mock_get_provider:
            mock_provider = MagicMock()
            mock_get_provider.return_value = mock_provider

            # First access creates
            provider1 = processor.provider
            # Second access returns cached
            provider2 = processor.provider

            # Should only create once
            mock_get_provider.assert_called_once()
            assert provider1 is provider2

    def test_init_with_explicit_provider(self) -> None:
        """Test initialization with explicit provider."""
        mock_provider = MagicMock()

        processor = MinionProcessor(provider=mock_provider)

        assert processor._provider is mock_provider
        assert processor.provider is mock_provider
