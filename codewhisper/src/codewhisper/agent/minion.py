"""Minion processor for summarizing large tool results.

This module provides a lightweight LLM processor that summarizes large tool
outputs before they reach the main agent, reducing token usage and improving
context efficiency.

The minion uses a smaller, faster model (configured via MINION_SCRIBE_MODEL)
to extract key information from tool results that exceed a size threshold.

Uses llm_providers directly instead of LangChain.

Example:
    from codewhisper.agent.minion import MinionProcessor

    processor = MinionProcessor()
    summary = await processor.summarize_result("read_file", large_file_content)
"""

from __future__ import annotations

import logging
import os
from dataclasses import dataclass
from typing import TYPE_CHECKING

from dotenv import load_dotenv

from llm_providers import Message, get_provider_from_env

# Load .env file from current directory or parent directories
load_dotenv()

if TYPE_CHECKING:
    from llm_providers import LLMProvider

logger = logging.getLogger(__name__)

# Default threshold: 8000 tokens (suitable for claude-3-haiku)
DEFAULT_MINION_THRESHOLD_TOKENS = 8000

# Default model if MINION_SCRIBE_MODEL is not set
DEFAULT_MINION_MODEL = "anthropic/claude-3-haiku-20240307"


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


@dataclass
class ToolResultSummary:
    """Structured output for tool result summarization.

    Attributes:
        key_points: List of key findings from the tool result.
        summary: Concise narrative summary of the tool output.
    """

    key_points: list[str]
    summary: str

    @classmethod
    def parse_from_text(cls, text: str) -> "ToolResultSummary":
        """Parse summary from LLM response text.

        Expects format:
        KEY POINTS:
        - point 1
        - point 2

        SUMMARY:
        summary text here

        Args:
            text: LLM response text.

        Returns:
            Parsed ToolResultSummary.
        """
        key_points: list[str] = []
        summary = ""

        # Split by sections
        lines = text.strip().split("\n")
        current_section = None

        for line in lines:
            line_stripped = line.strip()

            if line_stripped.upper().startswith("KEY POINTS"):
                current_section = "key_points"
                continue
            elif line_stripped.upper().startswith("SUMMARY"):
                current_section = "summary"
                continue

            if current_section == "key_points":
                # Extract bullet points
                if line_stripped.startswith("-") or line_stripped.startswith("*"):
                    point = line_stripped.lstrip("-*").strip()
                    if point:
                        key_points.append(point)
            elif current_section == "summary":
                if line_stripped:
                    summary = summary + " " + line_stripped if summary else line_stripped

        # Fallback if parsing failed
        if not key_points and not summary:
            summary = text[:500] + "..." if len(text) > 500 else text

        return cls(key_points=key_points, summary=summary.strip())


class MinionProcessor:
    """Processor that uses a smaller model to summarize large tool results.

    The MinionProcessor intercepts tool outputs that exceed a character
    threshold and uses a fast, inexpensive model to extract key information.
    This reduces token usage in the main agent while preserving essential
    information.

    Uses llm_providers directly instead of LangChain.

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
        provider: "LLMProvider | None" = None,
    ):
        """Initialize the minion processor.

        Args:
            model_name: Model to use for summarization. If not provided,
                reads from MINION_SCRIBE_MODEL env var, falling back to
                claude-3-haiku.
            threshold: Character count above which to trigger summarization.
            provider: Optional LLM provider. If None, creates from environment.
        """
        self.model_name = model_name or os.environ.get(
            "MINION_SCRIBE_MODEL",
            DEFAULT_MINION_MODEL,
        )
        self.threshold = threshold if threshold is not None else _get_minion_threshold()
        self._provider = provider

        logger.debug(
            f"MinionProcessor initialized: model={self.model_name}, "
            f"threshold={self.threshold} chars"
        )

    @property
    def provider(self) -> "LLMProvider":
        """Get the LLM provider, creating if needed.

        Returns:
            Configured LLM provider.
        """
        if self._provider is None:
            self._provider = get_provider_from_env()
        return self._provider

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
            # Build summarization prompt
            prompt = f"""Summarize this {tool_name} tool output for a code exploration agent.
Extract the most important information that would help understand the codebase.

Respond in this exact format:
KEY POINTS:
- point 1
- point 2
- (add more as needed)

SUMMARY:
A concise paragraph summarizing the key information.

Tool output:
{result}"""

            messages = [
                Message(role="system", content="You are a helpful assistant that summarizes tool outputs concisely."),
                Message(role="user", content=prompt),
            ]

            # Call the LLM
            response = await self.provider.complete(
                messages=messages,
                model=self.model_name,
                temperature=0.0,
                max_tokens=1024,
            )

            # Parse the response
            summary_result = ToolResultSummary.parse_from_text(response.content)

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
