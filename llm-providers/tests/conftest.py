"""Pytest fixtures for llm-providers tests.

This module provides shared fixtures for testing the llm-providers package.
"""

import os
from unittest.mock import MagicMock

import pytest

from llm_providers.protocol import CompletionResponse, Message


@pytest.fixture
def sample_messages() -> list[Message]:
    """Sample messages for testing."""
    return [
        Message(role="system", content="You are a helpful assistant."),
        Message(role="user", content="Hello, how are you?"),
    ]


@pytest.fixture
def sample_completion_response() -> CompletionResponse:
    """Sample completion response for testing."""
    return CompletionResponse(
        content="I'm doing well, thank you for asking!",
        model="test-model",
        tokens_used=25,
        raw_response={"id": "test-123", "model": "test-model"},
    )


@pytest.fixture
def mock_openai_response() -> MagicMock:
    """Mock OpenAI API response object."""
    mock_response = MagicMock()
    mock_response.id = "chatcmpl-123"
    mock_response.model = "gpt-4o"
    mock_response.choices = [MagicMock()]
    mock_response.choices[0].message.content = "Mocked response content"
    mock_response.usage = MagicMock()
    mock_response.usage.prompt_tokens = 50
    mock_response.usage.completion_tokens = 25
    return mock_response


@pytest.fixture
def mock_anthropic_response() -> MagicMock:
    """Mock Anthropic API response object."""
    mock_response = MagicMock()
    mock_response.id = "msg-123"
    mock_response.model = "claude-sonnet-4-20250514"
    mock_response.content = [MagicMock()]
    mock_response.content[0].text = "Mocked Anthropic response"
    mock_response.usage = MagicMock()
    mock_response.usage.input_tokens = 40
    mock_response.usage.output_tokens = 30
    mock_response.stop_reason = "end_turn"
    return mock_response


@pytest.fixture
def clean_env() -> dict[str, str]:
    """Clean environment without any API keys."""
    # Save original environment
    original_env = os.environ.copy()

    # Remove any LLM-related environment variables
    keys_to_remove = [
        "OPENROUTER_API_KEY",
        "ANTHROPIC_API_KEY",
        "OPENAI_API_KEY",
        "LLM_PROVIDER",
        "LLM_DEFAULT_MODEL",
        "LLM_TIMEOUT",
    ]
    for key in keys_to_remove:
        os.environ.pop(key, None)

    yield {}

    # Restore original environment
    os.environ.clear()
    os.environ.update(original_env)


@pytest.fixture
def openrouter_env() -> dict[str, str]:
    """Environment variables for OpenRouter provider."""
    return {
        "OPENROUTER_API_KEY": "sk-or-test-key-12345",
        "LLM_PROVIDER": "openrouter",
    }


@pytest.fixture
def anthropic_env() -> dict[str, str]:
    """Environment variables for Anthropic provider."""
    return {
        "ANTHROPIC_API_KEY": "sk-ant-test-key-12345",
        "LLM_PROVIDER": "anthropic",
    }


@pytest.fixture
def openai_env() -> dict[str, str]:
    """Environment variables for OpenAI provider."""
    return {
        "OPENAI_API_KEY": "sk-test-key-12345",
        "LLM_PROVIDER": "openai",
    }
