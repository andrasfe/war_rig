"""Minion processor for summarizing large tool results.

This module provides a lightweight LLM processor that summarizes large tool
outputs before they reach the main agent, reducing token usage and improving
context efficiency.

The minion uses a smaller, faster model (configured via MINION_SCRIBE_MODEL)
to extract key information from tool results that exceed a size threshold.

Example:
    processor = MinionProcessor()
    summary = await processor.summarize_result("read_file", large_file_content)
"""

from __future__ import annotations

import logging
import os
from typing import TYPE_CHECKING

from pydantic import BaseModel, Field

if TYPE_CHECKING:
    from langchain_core.runnables import Runnable

logger = logging.getLogger(__name__)

# Default threshold: 8000 tokens (suitable for claude-3-haiku)
DEFAULT_MINION_THRESHOLD_TOKENS = 8000


def _get_minion_threshold() -> int:
    """Get minion context threshold from env var.

    Returns threshold in characters (~4 chars per token).
    Reads MINION_CONTEXT_THRESHOLD env var (in tokens).
    Default: 8000 tokens = 32000 chars.
    """
    tokens = int(
        os.environ.get("MINION_CONTEXT_THRESHOLD", str(DEFAULT_MINION_THRESHOLD_TOKENS))
    )
    return tokens * 4  # Convert tokens to approximate chars

# Default model if MINION_SCRIBE_MODEL is not set
DEFAULT_MINION_MODEL = "anthropic/claude-3-haiku-20240307"


class ToolResultSummary(BaseModel):
    """Structured output for tool result summarization.

    Attributes:
        key_points: List of key findings from the tool result.
        summary: Concise narrative summary of the tool output.
    """

    key_points: list[str] = Field(
        ...,
        description="Key findings or important items from the tool result",
    )
    summary: str = Field(
        ...,
        description="Concise narrative summary of the tool output",
    )


class MinionProcessor:
    """Processor that uses a smaller model to summarize large tool results.

    The MinionProcessor intercepts tool outputs that exceed a character
    threshold and uses a fast, inexpensive model to extract key information.
    This reduces token usage in the main agent while preserving essential
    information.

    Attributes:
        model_name: The model identifier to use for summarization.
        threshold: Character count above which to summarize.

    Example:
        processor = MinionProcessor()

        # Large file content gets summarized
        result = await processor.summarize_result(
            "read_file",
            very_long_file_content
        )

        # Small results pass through unchanged
        result = await processor.summarize_result(
            "search_skills",
            "Found 2 skills: auth, payments"
        )
    """

    def __init__(
        self,
        model_name: str | None = None,
        threshold: int | None = None,
    ):
        """Initialize the minion processor.

        Args:
            model_name: Model to use for summarization. If not provided,
                reads from MINION_SCRIBE_MODEL env var, falling back to
                claude-3-haiku.
            threshold: Character count above which to trigger summarization.
        """
        self.model_name = model_name or os.environ.get(
            "MINION_SCRIBE_MODEL",
            DEFAULT_MINION_MODEL,
        )
        self.threshold = threshold if threshold is not None else _get_minion_threshold()
        self._llm: Runnable[str, ToolResultSummary] | None = None

        logger.debug(f"MinionProcessor initialized: model={self.model_name}")

    def _create_llm(self) -> "Runnable[str, ToolResultSummary]":
        """Create the LLM for summarization based on API_PROVIDER.

        Respects the same provider configuration as the main agent.

        Returns:
            Configured runnable with structured output.
        """
        from langchain_core.language_models import BaseChatModel

        provider = os.environ.get("API_PROVIDER", "openrouter")

        llm: BaseChatModel
        if provider == "anthropic":
            from langchain_anthropic import ChatAnthropic

            api_key = os.environ.get("ANTHROPIC_API_KEY", "")
            if not api_key:
                raise ValueError(
                    "ANTHROPIC_API_KEY environment variable not set. "
                    "Required for minion processing with anthropic provider."
                )

            llm = ChatAnthropic(
                model=self.model_name,
                temperature=0.0,
                max_tokens=1024,
                api_key=api_key,
            )
        elif provider == "openai":
            from langchain_openai import ChatOpenAI

            api_key = os.environ.get("OPENAI_API_KEY", "")
            if not api_key:
                raise ValueError(
                    "OPENAI_API_KEY environment variable not set. "
                    "Required for minion processing with openai provider."
                )

            llm = ChatOpenAI(
                model=self.model_name,
                temperature=0.0,
                max_completion_tokens=1024,
                api_key=api_key,
            )
        else:  # openrouter (default)
            from langchain_openai import ChatOpenAI

            api_key = os.environ.get("OPENROUTER_API_KEY", "")
            if not api_key:
                raise ValueError(
                    "OPENROUTER_API_KEY environment variable not set. "
                    "Required for minion processing with openrouter provider."
                )

            llm = ChatOpenAI(
                model=self.model_name,
                temperature=0.0,
                max_completion_tokens=1024,
                base_url="https://openrouter.ai/api/v1",
                api_key=api_key,
            )

        return llm.with_structured_output(ToolResultSummary)  # type: ignore[return-value]

    @property
    def llm(self) -> "Runnable[str, ToolResultSummary]":
        """Get or create the LLM instance.

        Returns:
            Configured runnable with structured output.
        """
        if self._llm is None:
            self._llm = self._create_llm()
        return self._llm

    async def summarize_result(self, tool_name: str, result: str) -> str:
        """Summarize a tool result if it exceeds the threshold.

        If the result is below the threshold, it passes through unchanged.
        If summarization fails for any reason, the original result is
        returned as a fallback.

        Args:
            tool_name: Name of the tool that produced the result.
            result: The tool's output to potentially summarize.

        Returns:
            Either the original result (if small or on error) or a
            formatted summary with key points.
        """
        # Pass through small results unchanged
        if len(result) <= self.threshold:
            return result

        logger.debug(
            f"Summarizing {tool_name} result: {len(result)} chars "
            f"(threshold: {self.threshold})"
        )

        try:
            prompt = (
                f"Summarize this {tool_name} tool output for a code exploration agent. "
                f"Extract the most important information that would help understand "
                f"the codebase.\n\n"
                f"Tool output:\n{result}"
            )

            summary_result = await self.llm.ainvoke(prompt)

            # Format the summary
            key_points_text = "\n".join(
                f"- {point}" for point in summary_result.key_points
            )
            formatted = (
                f"[Summarized from {len(result)} chars]\n\n"
                f"Key findings:\n{key_points_text}\n\n"
                f"Summary: {summary_result.summary}"
            )

            logger.debug(
                f"Summarization complete: {len(result)} -> {len(formatted)} chars"
            )
            return formatted

        except Exception as e:
            # Graceful fallback: return original on any error
            logger.warning(
                f"Minion summarization failed for {tool_name}, "
                f"returning original: {e}"
            )
            return result
