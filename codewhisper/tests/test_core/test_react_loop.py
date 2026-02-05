"""Tests for ReActLoop in codewhisper.core.react_loop.

This module tests:
- ReActLoop initialization
- Simple completion without tools
- Completion with single tool call
- Completion with multiple tool calls
- Max iterations handling
- Minion integration (optional summarization)
"""

import asyncio
from dataclasses import dataclass
from typing import Any
from unittest.mock import AsyncMock, MagicMock

import pytest

from codewhisper.core.message import Message, ToolCall, ToolResult
from codewhisper.core.react_loop import (
    LLMProvider,
    MinionProcessor,
    ReActConfig,
    ReActLoop,
    ReActResult,
)
from codewhisper.core.tool_protocol import ToolDefinition
from codewhisper.tools.registry import ToolRegistry


class MockLLMProvider:
    """Mock LLM provider for testing.

    Provides pre-configured responses in sequence, allowing tests to
    control the conversation flow.
    """

    def __init__(self, responses: list[dict[str, Any]]) -> None:
        """Initialize with a list of responses to return.

        Args:
            responses: List of response dictionaries. Each response should have
                'content' and optionally 'tool_calls' fields.
        """
        self.responses = responses
        self.call_count = 0
        self.calls: list[dict[str, Any]] = []

    async def complete(
        self,
        messages: list[dict[str, Any]],
        tools: list[dict[str, Any]] | None = None,
        **kwargs: Any,
    ) -> dict[str, Any]:
        """Return the next pre-configured response.

        Records the call for later inspection.
        """
        self.calls.append({
            "messages": messages,
            "tools": tools,
            "kwargs": kwargs,
        })

        if self.call_count >= len(self.responses):
            # Return a final response if we've exhausted responses
            return {"content": "No more responses configured", "tool_calls": []}

        response = self.responses[self.call_count]
        self.call_count += 1
        return response


class MockMinionProcessor:
    """Mock minion processor for testing summarization."""

    def __init__(self, summarize: bool = True) -> None:
        """Initialize mock minion.

        Args:
            summarize: If True, actually shortens content. If False, passes through.
        """
        self.summarize = summarize
        self.calls: list[tuple[str, str]] = []

    async def maybe_summarize(self, tool_name: str, result: str) -> str:
        """Record the call and optionally summarize."""
        self.calls.append((tool_name, result))

        if self.summarize and len(result) > 50:
            return f"[Summarized: {len(result)} chars] {result[:30]}..."
        return result


@pytest.fixture
def simple_registry() -> ToolRegistry:
    """Create a registry with simple test tools."""
    registry = ToolRegistry()

    def echo_handler(message: str) -> str:
        return f"Echo: {message}"

    def add_handler(a: int, b: int) -> str:
        return str(a + b)

    async def async_search_handler(query: str) -> str:
        await asyncio.sleep(0.001)
        return f"Found results for: {query}"

    registry.register(
        ToolDefinition(
            name="echo",
            description="Echo a message",
            parameters={
                "type": "object",
                "properties": {"message": {"type": "string"}},
                "required": ["message"],
            },
            handler=echo_handler,
        )
    )
    registry.register(
        ToolDefinition(
            name="add",
            description="Add two numbers",
            parameters={
                "type": "object",
                "properties": {
                    "a": {"type": "integer"},
                    "b": {"type": "integer"},
                },
                "required": ["a", "b"],
            },
            handler=add_handler,
        )
    )
    registry.register(
        ToolDefinition(
            name="search",
            description="Search for something",
            parameters={
                "type": "object",
                "properties": {"query": {"type": "string"}},
                "required": ["query"],
            },
            handler=async_search_handler,
        )
    )
    return registry


class TestReActConfig:
    """Tests for ReActConfig dataclass."""

    def test_default_values(self) -> None:
        """Test default configuration values."""
        config = ReActConfig()

        assert config.max_iterations == 10
        assert config.temperature == 0.3
        assert config.max_tokens == 4096
        assert config.parallel_tool_execution is True

    def test_custom_values(self) -> None:
        """Test creating config with custom values."""
        config = ReActConfig(
            max_iterations=5,
            temperature=0.7,
            max_tokens=8192,
            parallel_tool_execution=False,
        )

        assert config.max_iterations == 5
        assert config.temperature == 0.7
        assert config.max_tokens == 8192
        assert config.parallel_tool_execution is False


class TestReActResult:
    """Tests for ReActResult dataclass."""

    def test_create_result(self) -> None:
        """Test creating a result object."""
        history = [
            Message(role="user", content="Hello"),
            Message(role="assistant", content="Hi there!"),
        ]

        result = ReActResult(
            response="Hi there!",
            history=history,
            iterations=1,
            tool_calls_made=0,
        )

        assert result.response == "Hi there!"
        assert len(result.history) == 2
        assert result.iterations == 1
        assert result.tool_calls_made == 0
        assert result.reached_max_iterations is False

    def test_result_with_max_iterations(self) -> None:
        """Test result indicating max iterations reached."""
        result = ReActResult(
            response="Stopped early",
            history=[],
            iterations=10,
            tool_calls_made=15,
            reached_max_iterations=True,
        )

        assert result.reached_max_iterations is True
        assert result.iterations == 10


class TestReActLoopInitialization:
    """Tests for ReActLoop initialization."""

    def test_init_basic(self, simple_registry: ToolRegistry) -> None:
        """Test basic initialization."""
        provider = MockLLMProvider([])

        loop = ReActLoop(
            llm_provider=provider,
            tool_registry=simple_registry,
            system_prompt="You are a test assistant.",
        )

        # Should not raise
        assert loop is not None

    def test_init_with_config(self, simple_registry: ToolRegistry) -> None:
        """Test initialization with custom config."""
        provider = MockLLMProvider([])
        config = ReActConfig(max_iterations=5)

        loop = ReActLoop(
            llm_provider=provider,
            tool_registry=simple_registry,
            system_prompt="Test",
            config=config,
        )

        assert loop is not None

    def test_init_with_minion(self, simple_registry: ToolRegistry) -> None:
        """Test initialization with minion processor."""
        provider = MockLLMProvider([])
        minion = MockMinionProcessor()

        loop = ReActLoop(
            llm_provider=provider,
            tool_registry=simple_registry,
            system_prompt="Test",
            minion_processor=minion,
        )

        assert loop is not None


class TestReActLoopSimpleCompletion:
    """Tests for simple completions without tool calls."""

    async def test_simple_response(self, simple_registry: ToolRegistry) -> None:
        """Test a simple response without tool calls."""
        provider = MockLLMProvider([
            {"content": "Hello! How can I help you?", "tool_calls": []},
        ])

        loop = ReActLoop(
            llm_provider=provider,
            tool_registry=simple_registry,
            system_prompt="You are helpful.",
        )

        result = await loop.run(
            user_message="Hi there",
            history=[],
        )

        assert result.response == "Hello! How can I help you?"
        assert result.iterations == 1
        assert result.tool_calls_made == 0
        assert result.reached_max_iterations is False

    async def test_response_with_history(self, simple_registry: ToolRegistry) -> None:
        """Test response that builds on conversation history."""
        provider = MockLLMProvider([
            {"content": "Python is a programming language.", "tool_calls": []},
        ])

        loop = ReActLoop(
            llm_provider=provider,
            tool_registry=simple_registry,
            system_prompt="Test",
        )

        # Provide some history
        history = [
            Message(role="user", content="What is Python?"),
            Message(role="assistant", content="Python is great!"),
        ]

        result = await loop.run(
            user_message="Tell me more",
            history=history,
        )

        assert result.response == "Python is a programming language."
        # History should include original + new user + assistant response
        assert len(result.history) == 4

    async def test_empty_content_response(self, simple_registry: ToolRegistry) -> None:
        """Test handling of empty content response."""
        provider = MockLLMProvider([
            {"content": "", "tool_calls": []},
        ])

        loop = ReActLoop(
            llm_provider=provider,
            tool_registry=simple_registry,
            system_prompt="Test",
        )

        result = await loop.run(user_message="Test", history=[])

        assert result.response == ""
        assert result.iterations == 1


class TestReActLoopWithToolCalls:
    """Tests for completions with tool calls."""

    async def test_single_tool_call(self, simple_registry: ToolRegistry) -> None:
        """Test completion with a single tool call."""
        provider = MockLLMProvider([
            # First response: make a tool call
            {
                "content": "",
                "tool_calls": [
                    {
                        "id": "call_1",
                        "name": "echo",
                        "arguments": {"message": "Hello World"},
                    }
                ],
            },
            # Second response: final answer after tool result
            {
                "content": "The echo returned: Echo: Hello World",
                "tool_calls": [],
            },
        ])

        loop = ReActLoop(
            llm_provider=provider,
            tool_registry=simple_registry,
            system_prompt="Test",
        )

        result = await loop.run(
            user_message="Please echo Hello World",
            history=[],
        )

        assert result.response == "The echo returned: Echo: Hello World"
        assert result.iterations == 2
        assert result.tool_calls_made == 1

    async def test_multiple_tool_calls_single_response(
        self, simple_registry: ToolRegistry
    ) -> None:
        """Test response with multiple tool calls at once."""
        provider = MockLLMProvider([
            # First response: multiple tool calls
            {
                "content": "",
                "tool_calls": [
                    {
                        "id": "call_1",
                        "name": "add",
                        "arguments": {"a": 5, "b": 3},
                    },
                    {
                        "id": "call_2",
                        "name": "echo",
                        "arguments": {"message": "test"},
                    },
                ],
            },
            # Second response: final answer
            {
                "content": "5+3=8 and echo says 'test'",
                "tool_calls": [],
            },
        ])

        loop = ReActLoop(
            llm_provider=provider,
            tool_registry=simple_registry,
            system_prompt="Test",
        )

        result = await loop.run(user_message="Calculate and echo", history=[])

        assert result.tool_calls_made == 2
        assert result.iterations == 2

    async def test_chained_tool_calls(self, simple_registry: ToolRegistry) -> None:
        """Test multiple iterations of tool calls."""
        provider = MockLLMProvider([
            # First: call add
            {
                "content": "",
                "tool_calls": [{"id": "c1", "name": "add", "arguments": {"a": 1, "b": 2}}],
            },
            # Second: call echo with result
            {
                "content": "",
                "tool_calls": [
                    {"id": "c2", "name": "echo", "arguments": {"message": "result was 3"}}
                ],
            },
            # Third: final response
            {
                "content": "Done! 1+2=3",
                "tool_calls": [],
            },
        ])

        loop = ReActLoop(
            llm_provider=provider,
            tool_registry=simple_registry,
            system_prompt="Test",
        )

        result = await loop.run(user_message="Chain operations", history=[])

        assert result.iterations == 3
        assert result.tool_calls_made == 2

    async def test_tool_call_with_async_handler(
        self, simple_registry: ToolRegistry
    ) -> None:
        """Test tool call with async handler."""
        provider = MockLLMProvider([
            {
                "content": "",
                "tool_calls": [
                    {"id": "c1", "name": "search", "arguments": {"query": "python"}}
                ],
            },
            {"content": "Found results for python", "tool_calls": []},
        ])

        loop = ReActLoop(
            llm_provider=provider,
            tool_registry=simple_registry,
            system_prompt="Test",
        )

        result = await loop.run(user_message="Search for python", history=[])

        assert result.tool_calls_made == 1
        assert "Found results" in result.response


class TestReActLoopMaxIterations:
    """Tests for max iterations handling."""

    async def test_reaches_max_iterations(self, simple_registry: ToolRegistry) -> None:
        """Test that loop stops at max iterations."""
        # Create responses that always make tool calls
        infinite_responses = [
            {
                "content": "",
                "tool_calls": [
                    {"id": f"c{i}", "name": "echo", "arguments": {"message": f"call {i}"}}
                ],
            }
            for i in range(20)
        ]

        provider = MockLLMProvider(infinite_responses)
        config = ReActConfig(max_iterations=3)

        loop = ReActLoop(
            llm_provider=provider,
            tool_registry=simple_registry,
            system_prompt="Test",
            config=config,
        )

        result = await loop.run(user_message="Keep calling", history=[])

        assert result.reached_max_iterations is True
        assert result.iterations == 3
        assert result.tool_calls_made == 3

    async def test_max_iterations_summary(self, simple_registry: ToolRegistry) -> None:
        """Test that max iterations generates a summary response."""
        responses = [
            {
                "content": "Working on it...",
                "tool_calls": [
                    {"id": "c1", "name": "echo", "arguments": {"message": "test"}}
                ],
            },
            {
                "content": "",
                "tool_calls": [
                    {"id": "c2", "name": "add", "arguments": {"a": 1, "b": 1}}
                ],
            },
        ]

        provider = MockLLMProvider(responses)
        config = ReActConfig(max_iterations=2)

        loop = ReActLoop(
            llm_provider=provider,
            tool_registry=simple_registry,
            system_prompt="Test",
            config=config,
        )

        result = await loop.run(user_message="Do stuff", history=[])

        assert result.reached_max_iterations is True
        assert "maximum number of iterations" in result.response.lower()


class TestReActLoopMinionIntegration:
    """Tests for minion processor integration."""

    async def test_minion_summarizes_large_result(
        self, simple_registry: ToolRegistry
    ) -> None:
        """Test that minion summarizes large tool results."""
        provider = MockLLMProvider([
            {
                "content": "",
                "tool_calls": [
                    {"id": "c1", "name": "echo", "arguments": {"message": "x" * 100}}
                ],
            },
            {"content": "Got the summarized result", "tool_calls": []},
        ])

        minion = MockMinionProcessor(summarize=True)

        loop = ReActLoop(
            llm_provider=provider,
            tool_registry=simple_registry,
            system_prompt="Test",
            minion_processor=minion,
        )

        result = await loop.run(user_message="Echo long text", history=[])

        # Minion should have been called
        assert len(minion.calls) == 1
        assert minion.calls[0][0] == "echo"  # tool name

        # Result should be summarized in history
        tool_messages = [
            m for m in result.history if m.role == "tool"
        ]
        assert len(tool_messages) == 1
        assert "[Summarized:" in tool_messages[0].content

    async def test_minion_passes_small_result(
        self, simple_registry: ToolRegistry
    ) -> None:
        """Test that minion passes through small results."""
        provider = MockLLMProvider([
            {
                "content": "",
                "tool_calls": [
                    {"id": "c1", "name": "echo", "arguments": {"message": "hi"}}
                ],
            },
            {"content": "Got: Echo: hi", "tool_calls": []},
        ])

        minion = MockMinionProcessor(summarize=True)

        loop = ReActLoop(
            llm_provider=provider,
            tool_registry=simple_registry,
            system_prompt="Test",
            minion_processor=minion,
        )

        result = await loop.run(user_message="Echo hi", history=[])

        # Minion should have been called
        assert len(minion.calls) == 1

        # But result should not be summarized (too short)
        tool_messages = [m for m in result.history if m.role == "tool"]
        assert "[Summarized:" not in tool_messages[0].content

    async def test_minion_skips_errors(self, simple_registry: ToolRegistry) -> None:
        """Test that minion does not summarize error results."""
        provider = MockLLMProvider([
            {
                "content": "",
                "tool_calls": [
                    {"id": "c1", "name": "nonexistent", "arguments": {}}
                ],
            },
            {"content": "Tool failed", "tool_calls": []},
        ])

        minion = MockMinionProcessor(summarize=True)

        loop = ReActLoop(
            llm_provider=provider,
            tool_registry=simple_registry,
            system_prompt="Test",
            minion_processor=minion,
        )

        await loop.run(user_message="Call bad tool", history=[])

        # Minion should not be called for errors
        assert len(minion.calls) == 0


class TestReActLoopErrorHandling:
    """Tests for error handling in ReActLoop."""

    async def test_llm_exception_returns_error(
        self, simple_registry: ToolRegistry
    ) -> None:
        """Test that LLM exceptions are caught and returned as response."""
        provider = MagicMock()
        provider.complete = AsyncMock(side_effect=RuntimeError("API Error"))

        loop = ReActLoop(
            llm_provider=provider,
            tool_registry=simple_registry,
            system_prompt="Test",
        )

        result = await loop.run(user_message="Test", history=[])

        assert "Error" in result.response
        assert result.iterations == 1

    async def test_tool_error_continues_loop(
        self, simple_registry: ToolRegistry
    ) -> None:
        """Test that tool execution errors don't break the loop."""
        provider = MockLLMProvider([
            {
                "content": "",
                "tool_calls": [
                    # This tool doesn't exist
                    {"id": "c1", "name": "missing_tool", "arguments": {}}
                ],
            },
            {"content": "The tool was not found, but I can continue", "tool_calls": []},
        ])

        loop = ReActLoop(
            llm_provider=provider,
            tool_registry=simple_registry,
            system_prompt="Test",
        )

        result = await loop.run(user_message="Call missing tool", history=[])

        # Should complete despite tool error
        assert result.response == "The tool was not found, but I can continue"
        assert result.tool_calls_made == 1


class TestReActLoopParseResponse:
    """Tests for response parsing with different formats."""

    async def test_parse_openai_style_response(
        self, simple_registry: ToolRegistry
    ) -> None:
        """Test parsing OpenAI-style response object."""

        @dataclass
        class MockFunction:
            name: str
            arguments: str  # OpenAI returns JSON string

        @dataclass
        class MockToolCall:
            id: str
            function: MockFunction

        @dataclass
        class MockMessage:
            content: str | None
            tool_calls: list[MockToolCall] | None

        @dataclass
        class MockChoice:
            message: MockMessage

        @dataclass
        class MockResponse:
            choices: list[MockChoice]

        # Create a realistic OpenAI-style response
        openai_response = MockResponse(
            choices=[
                MockChoice(
                    message=MockMessage(
                        content=None,
                        tool_calls=[
                            MockToolCall(
                                id="call_abc123",
                                function=MockFunction(
                                    name="echo",
                                    arguments='{"message": "test"}',
                                ),
                            )
                        ],
                    )
                )
            ]
        )

        provider = MagicMock()
        provider.complete = AsyncMock(
            side_effect=[
                openai_response,
                {"content": "Done", "tool_calls": []},
            ]
        )

        loop = ReActLoop(
            llm_provider=provider,
            tool_registry=simple_registry,
            system_prompt="Test",
        )

        result = await loop.run(user_message="Test", history=[])

        assert result.tool_calls_made == 1
        assert result.response == "Done"


class TestLLMProviderProtocol:
    """Tests for LLMProvider protocol compliance."""

    def test_mock_provider_is_llm_provider(self) -> None:
        """Test that MockLLMProvider satisfies LLMProvider protocol."""
        provider = MockLLMProvider([])

        # Should satisfy the protocol (structural typing)
        assert isinstance(provider, LLMProvider)

    def test_custom_provider_protocol(self) -> None:
        """Test that any class with complete method satisfies protocol."""

        class CustomProvider:
            async def complete(
                self,
                messages: list[dict[str, Any]],
                tools: list[dict[str, Any]] | None = None,
                **kwargs: Any,
            ) -> dict[str, Any]:
                return {"content": "test", "tool_calls": []}

        provider = CustomProvider()
        assert isinstance(provider, LLMProvider)


class TestMinionProcessorProtocol:
    """Tests for MinionProcessor protocol compliance."""

    def test_mock_minion_is_protocol(self) -> None:
        """Test that MockMinionProcessor satisfies MinionProcessor protocol."""
        minion = MockMinionProcessor()

        # Should satisfy the protocol (structural typing)
        assert isinstance(minion, MinionProcessor)
