"""Minion module for summarizing large tool results.

This module provides the MinionProcessor for summarizing large tool outputs
before they reach the main agent, reducing token usage and improving context
efficiency.

The minion processor uses a smaller, faster LLM to extract key information
from tool results that exceed a configurable size threshold.

Example:
    from codewhisper.minion import MinionProcessor, MinionConfig
    from war_rig.providers import get_provider_from_env

    # Create provider and processor
    provider = get_provider_from_env()
    processor = MinionProcessor(
        llm_provider=provider,
        config=MinionConfig(threshold=16000),
    )

    # Summarize large results
    result = await processor.maybe_summarize("read_file", large_content)
"""

from codewhisper.minion.processor import (
    LLMProvider,
    MinionConfig,
    MinionProcessor,
    ToolResultSummary,
)

__all__ = [
    "LLMProvider",
    "MinionConfig",
    "MinionProcessor",
    "ToolResultSummary",
]
