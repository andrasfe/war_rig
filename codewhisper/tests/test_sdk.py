"""Tests for the main CodeWhisper SDK class.

This module tests:
- CodeWhisper class initialization
- CodeWhisperConfig dataclass
- CompletionResult dataclass
- complete() method with mock provider
- reset() clears history
- Property accessors
"""

from pathlib import Path
from typing import Any
from unittest.mock import AsyncMock, MagicMock, patch

import pytest

from codewhisper.core.message import Message
from codewhisper.sdk import CodeWhisper, CodeWhisperConfig, CompletionResult


class MockLLMProvider:
    """Mock LLM provider for testing CodeWhisper SDK.

    Provides configurable responses without making real API calls.
    """

    def __init__(self, responses: list[dict[str, Any]] | None = None) -> None:
        """Initialize with optional response sequence.

        Args:
            responses: List of response dicts to return in sequence.
                If None, returns a default response.
        """
        self.responses = responses or [
            {"content": "Default mock response", "tool_calls": []}
        ]
        self.call_count = 0
        self.calls: list[dict[str, Any]] = []

    async def complete(
        self,
        messages: list[dict[str, Any]],
        tools: list[dict[str, Any]] | None = None,
        **kwargs: Any,
    ) -> dict[str, Any]:
        """Return the next configured response."""
        self.calls.append({
            "messages": messages,
            "tools": tools,
            "kwargs": kwargs,
        })

        if self.call_count >= len(self.responses):
            return {"content": "Exhausted responses", "tool_calls": []}

        response = self.responses[self.call_count]
        self.call_count += 1
        return response


class TestCodeWhisperConfig:
    """Tests for CodeWhisperConfig dataclass."""

    def test_default_values(self) -> None:
        """Test default configuration values."""
        config = CodeWhisperConfig()

        assert config.max_iterations == 10
        assert config.max_history == 20
        assert config.temperature == 0.3
        assert config.max_tokens == 4096
        assert config.use_minion is True
        assert config.minion_threshold == 32000

    def test_custom_values(self) -> None:
        """Test creating config with custom values."""
        config = CodeWhisperConfig(
            max_iterations=15,
            max_history=50,
            temperature=0.7,
            max_tokens=8192,
            use_minion=False,
            minion_threshold=16000,
        )

        assert config.max_iterations == 15
        assert config.max_history == 50
        assert config.temperature == 0.7
        assert config.max_tokens == 8192
        assert config.use_minion is False
        assert config.minion_threshold == 16000


class TestCompletionResult:
    """Tests for CompletionResult dataclass."""

    def test_create_simple_result(self) -> None:
        """Test creating a simple completion result."""
        result = CompletionResult(content="Hello, world!")

        assert result.content == "Hello, world!"
        assert result.tool_calls_made == 0
        assert result.iterations == 0
        assert result.messages == []

    def test_create_full_result(self) -> None:
        """Test creating a result with all fields."""
        messages = [
            Message(role="user", content="Hi"),
            Message(role="assistant", content="Hello!"),
        ]

        result = CompletionResult(
            content="Response text",
            tool_calls_made=5,
            iterations=3,
            messages=messages,
        )

        assert result.content == "Response text"
        assert result.tool_calls_made == 5
        assert result.iterations == 3
        assert len(result.messages) == 2


class TestCodeWhisperInitialization:
    """Tests for CodeWhisper initialization."""

    @pytest.fixture
    def tmp_code_dir(self, tmp_path: Path) -> Path:
        """Create a temporary code directory."""
        code_dir = tmp_path / "src"
        code_dir.mkdir()
        (code_dir / "main.py").write_text("print('hello')")
        return code_dir

    @pytest.fixture
    def tmp_docs_dir(self, tmp_path: Path) -> Path:
        """Create a temporary documents directory."""
        docs_dir = tmp_path / "docs"
        docs_dir.mkdir()
        skill_file = docs_dir / "SKILL.md"
        skill_file.write_text(
            """---
name: test-skill
description: A test skill
---

# Test Skill

Content here.
"""
        )
        return docs_dir

    def test_init_minimal(self, tmp_code_dir: Path) -> None:
        """Test initialization with minimal arguments."""
        provider = MockLLMProvider()

        sdk = CodeWhisper(
            llm_provider=provider,
            code_dir=tmp_code_dir,
        )

        assert sdk.code_dir == tmp_code_dir.resolve()
        assert sdk.documents_dir is None
        assert sdk.config is not None
        assert len(sdk.conversation_history) == 0

    def test_init_with_documents_dir(
        self, tmp_code_dir: Path, tmp_docs_dir: Path
    ) -> None:
        """Test initialization with documents directory."""
        provider = MockLLMProvider()

        sdk = CodeWhisper(
            llm_provider=provider,
            code_dir=tmp_code_dir,
            documents_dir=tmp_docs_dir,
        )

        assert sdk.documents_dir == tmp_docs_dir.resolve()

    def test_init_with_config(self, tmp_code_dir: Path) -> None:
        """Test initialization with custom config."""
        provider = MockLLMProvider()
        config = CodeWhisperConfig(
            max_iterations=5,
            temperature=0.5,
        )

        sdk = CodeWhisper(
            llm_provider=provider,
            code_dir=tmp_code_dir,
            config=config,
        )

        assert sdk.config.max_iterations == 5
        assert sdk.config.temperature == 0.5

    def test_init_with_skills_filter(
        self, tmp_code_dir: Path, tmp_docs_dir: Path
    ) -> None:
        """Test initialization with skills filter."""
        provider = MockLLMProvider()

        sdk = CodeWhisper(
            llm_provider=provider,
            code_dir=tmp_code_dir,
            documents_dir=tmp_docs_dir,
            skills=["cobol", "jcl"],
        )

        assert sdk is not None
        # Skills are lazily loaded, so we just verify initialization

    def test_init_with_minion_provider(self, tmp_code_dir: Path) -> None:
        """Test initialization with separate minion provider."""
        main_provider = MockLLMProvider()
        minion_provider = MockLLMProvider()

        sdk = CodeWhisper(
            llm_provider=main_provider,
            code_dir=tmp_code_dir,
            minion_provider=minion_provider,
        )

        assert sdk is not None


class TestCodeWhisperProperties:
    """Tests for CodeWhisper property accessors."""

    @pytest.fixture
    def sdk(self, tmp_path: Path) -> CodeWhisper:
        """Create a CodeWhisper instance for testing."""
        code_dir = tmp_path / "src"
        code_dir.mkdir()
        return CodeWhisper(
            llm_provider=MockLLMProvider(),
            code_dir=code_dir,
        )

    def test_code_dir_property(self, sdk: CodeWhisper) -> None:
        """Test code_dir property returns resolved path."""
        assert sdk.code_dir.is_absolute()
        assert sdk.code_dir.name == "src"

    def test_documents_dir_property_none(self, sdk: CodeWhisper) -> None:
        """Test documents_dir property when not set."""
        assert sdk.documents_dir is None

    def test_config_property(self, sdk: CodeWhisper) -> None:
        """Test config property returns config object."""
        config = sdk.config

        assert isinstance(config, CodeWhisperConfig)
        assert config.max_iterations > 0

    def test_tool_registry_property(self, sdk: CodeWhisper) -> None:
        """Test tool_registry property returns registry."""
        from codewhisper.tools.registry import ToolRegistry

        registry = sdk.tool_registry

        assert isinstance(registry, ToolRegistry)

    def test_conversation_history_property(self, sdk: CodeWhisper) -> None:
        """Test conversation_history returns a copy."""
        history = sdk.conversation_history

        assert isinstance(history, list)
        assert len(history) == 0

        # Should be a copy, not the internal list
        history.append(Message(role="user", content="test"))
        assert len(sdk.conversation_history) == 0


class TestCodeWhisperComplete:
    """Tests for complete() method."""

    @pytest.fixture
    def sdk_with_provider(self, tmp_path: Path) -> tuple[CodeWhisper, MockLLMProvider]:
        """Create SDK with accessible mock provider."""
        code_dir = tmp_path / "src"
        code_dir.mkdir()

        provider = MockLLMProvider([
            {"content": "First response", "tool_calls": []},
            {"content": "Second response", "tool_calls": []},
        ])

        sdk = CodeWhisper(
            llm_provider=provider,
            code_dir=code_dir,
            config=CodeWhisperConfig(use_minion=False),
        )

        return sdk, provider

    async def test_simple_complete(
        self, sdk_with_provider: tuple[CodeWhisper, MockLLMProvider]
    ) -> None:
        """Test simple completion without tools."""
        sdk, provider = sdk_with_provider

        result = await sdk.complete("Hello, assistant!")

        assert result.content == "First response"
        assert result.iterations == 1
        assert result.tool_calls_made == 0
        assert len(result.messages) == 2  # user + assistant

    async def test_complete_with_context(
        self, sdk_with_provider: tuple[CodeWhisper, MockLLMProvider]
    ) -> None:
        """Test completion with additional context."""
        sdk, provider = sdk_with_provider

        result = await sdk.complete(
            "Explain this error",
            context="Error: File not found",
        )

        # Verify the context was included in the message
        assert len(provider.calls) == 1
        messages = provider.calls[0]["messages"]
        # Handle both dict and Message object formats
        user_messages = [
            m for m in messages
            if (m.role if hasattr(m, "role") else m.get("role")) == "user"
        ]
        assert len(user_messages) > 0
        user_message = user_messages[0]
        content = user_message.content if hasattr(user_message, "content") else user_message.get("content", "")
        assert "Error: File not found" in content
        assert "Explain this error" in content

    async def test_complete_builds_history(
        self, sdk_with_provider: tuple[CodeWhisper, MockLLMProvider]
    ) -> None:
        """Test that multiple completions build conversation history."""
        sdk, provider = sdk_with_provider

        # First call
        result1 = await sdk.complete("First question")
        assert len(sdk.conversation_history) == 2

        # Second call should include history
        result2 = await sdk.complete("Follow-up question")
        assert len(sdk.conversation_history) == 4

    async def test_complete_reset_conversation(
        self, sdk_with_provider: tuple[CodeWhisper, MockLLMProvider]
    ) -> None:
        """Test reset_conversation parameter clears history."""
        sdk, provider = sdk_with_provider

        # Build some history
        await sdk.complete("First")
        assert len(sdk.conversation_history) > 0

        # Reset and complete
        await sdk.complete("Fresh start", reset_conversation=True)

        # Should only have messages from the new conversation
        assert len(sdk.conversation_history) == 2


class TestCodeWhisperReset:
    """Tests for reset() method."""

    @pytest.fixture
    def sdk(self, tmp_path: Path) -> CodeWhisper:
        """Create a CodeWhisper instance."""
        code_dir = tmp_path / "src"
        code_dir.mkdir()
        return CodeWhisper(
            llm_provider=MockLLMProvider(),
            code_dir=code_dir,
            config=CodeWhisperConfig(use_minion=False),
        )

    async def test_reset_clears_history(self, sdk: CodeWhisper) -> None:
        """Test that reset() clears conversation history."""
        # Build up some history
        await sdk.complete("Question 1")
        await sdk.complete("Question 2")
        assert len(sdk.conversation_history) > 0

        # Reset
        sdk.reset()

        assert len(sdk.conversation_history) == 0

    def test_reset_preserves_config(self, sdk: CodeWhisper) -> None:
        """Test that reset() does not affect configuration."""
        original_config = sdk.config

        sdk.reset()

        assert sdk.config is original_config

    def test_reset_preserves_tool_registry(self, sdk: CodeWhisper) -> None:
        """Test that reset() does not affect tool registry."""
        original_registry = sdk.tool_registry

        sdk.reset()

        assert sdk.tool_registry is original_registry


class TestCodeWhisperAddSystemMessage:
    """Tests for add_system_message() method."""

    @pytest.fixture
    def sdk(self, tmp_path: Path) -> CodeWhisper:
        """Create a CodeWhisper instance."""
        code_dir = tmp_path / "src"
        code_dir.mkdir()
        return CodeWhisper(
            llm_provider=MockLLMProvider(),
            code_dir=code_dir,
            config=CodeWhisperConfig(use_minion=False),
        )

    def test_add_system_message(self, sdk: CodeWhisper) -> None:
        """Test adding a system message."""
        sdk.add_system_message("You are a COBOL expert.")

        history = sdk.conversation_history
        assert len(history) == 1
        assert history[0].role == "system"
        assert history[0].content == "You are a COBOL expert."

    def test_multiple_system_messages(self, sdk: CodeWhisper) -> None:
        """Test adding multiple system messages."""
        sdk.add_system_message("First instruction")
        sdk.add_system_message("Second instruction")

        history = sdk.conversation_history
        assert len(history) == 2
        assert all(m.role == "system" for m in history)


class TestCodeWhisperHistoryTrimming:
    """Tests for conversation history trimming."""

    @pytest.fixture
    def sdk_small_history(self, tmp_path: Path) -> CodeWhisper:
        """Create SDK with small max_history for testing."""
        code_dir = tmp_path / "src"
        code_dir.mkdir()
        return CodeWhisper(
            llm_provider=MockLLMProvider([
                {"content": f"Response {i}", "tool_calls": []}
                for i in range(20)
            ]),
            code_dir=code_dir,
            config=CodeWhisperConfig(
                max_history=6,
                use_minion=False,
            ),
        )

    async def test_trims_to_max_history(
        self, sdk_small_history: CodeWhisper
    ) -> None:
        """Test that history is trimmed to max_history."""
        # Make many completions
        for i in range(10):
            await sdk_small_history.complete(f"Question {i}")

        # History should be trimmed
        assert len(sdk_small_history.conversation_history) <= 6

    async def test_preserves_system_messages_when_trimming(
        self, sdk_small_history: CodeWhisper
    ) -> None:
        """Test that system messages are preserved during trimming."""
        sdk_small_history.add_system_message("Important instruction")

        # Make many completions
        for i in range(10):
            await sdk_small_history.complete(f"Question {i}")

        # System message should still be present
        history = sdk_small_history.conversation_history
        system_messages = [m for m in history if m.role == "system"]
        assert len(system_messages) >= 1
        assert "Important instruction" in system_messages[0].content


class TestCodeWhisperMinionIntegration:
    """Tests for minion processor integration in SDK."""

    @pytest.fixture
    def sdk_with_minion(self, tmp_path: Path) -> CodeWhisper:
        """Create SDK with minion enabled."""
        code_dir = tmp_path / "src"
        code_dir.mkdir()
        return CodeWhisper(
            llm_provider=MockLLMProvider([
                {"content": "Response with minion", "tool_calls": []},
            ]),
            code_dir=code_dir,
            config=CodeWhisperConfig(
                use_minion=True,
                minion_threshold=1000,
            ),
        )

    async def test_minion_config_passed(self, sdk_with_minion: CodeWhisper) -> None:
        """Test that minion configuration is used."""
        # Just verify the SDK initializes properly with minion config
        assert sdk_with_minion.config.use_minion is True
        assert sdk_with_minion.config.minion_threshold == 1000


class TestCodeWhisperSkillsIntegration:
    """Tests for skills/documents integration."""

    @pytest.fixture
    def sdk_with_skills(self, tmp_path: Path) -> CodeWhisper:
        """Create SDK with skills directory."""
        code_dir = tmp_path / "src"
        code_dir.mkdir()

        docs_dir = tmp_path / "docs"
        docs_dir.mkdir()

        # Create a valid skill file
        skill_file = docs_dir / "SKILL.md"
        skill_file.write_text(
            """---
name: test-skill
description: Test skill for SDK testing
---

# Test Skill

This is a test skill.
"""
        )

        return CodeWhisper(
            llm_provider=MockLLMProvider(),
            code_dir=code_dir,
            documents_dir=docs_dir,
            config=CodeWhisperConfig(use_minion=False),
        )

    def test_skills_dir_configured(self, sdk_with_skills: CodeWhisper) -> None:
        """Test that skills directory is properly configured."""
        assert sdk_with_skills.documents_dir is not None
        assert sdk_with_skills.documents_dir.exists()

    def test_skills_index_loaded_when_documents_dir_provided(
        self, sdk_with_skills: CodeWhisper
    ) -> None:
        """Test that skills index is loaded when documents_dir is provided."""
        # When documents_dir is provided, skills index is loaded eagerly
        # to register knowledge tools (search_skills, load_skill)
        assert sdk_with_skills._skills_index is not None
