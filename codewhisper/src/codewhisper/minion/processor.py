"""Minion processor for summarizing large tool results.

This module provides a lightweight LLM processor that summarizes large tool
outputs before they reach the main agent, reducing token usage and improving
context efficiency.

The minion uses a smaller, faster model to extract key information from tool
results that exceed a size threshold.

Uses war_rig.providers-compatible protocol directly - no LangChain dependency.

Example:
    from codewhisper.minion import MinionProcessor, MinionConfig

    processor = MinionProcessor(llm_provider=my_provider)
    summary = await processor.maybe_summarize("read_file", large_file_content)
"""

from __future__ import annotations

import logging
from dataclasses import dataclass
from typing import TYPE_CHECKING, Any, Protocol, runtime_checkable

if TYPE_CHECKING:
    pass

logger = logging.getLogger(__name__)


@runtime_checkable
class LLMProvider(Protocol):
    """Protocol for LLM providers compatible with war_rig.providers.

    Any class implementing the complete method can be used as an LLM provider
    for the minion processor. This uses structural subtyping (duck typing).
    """

    async def complete(
        self,
        messages: list[dict[str, Any]],
        **kwargs: Any,
    ) -> Any:
        """Send messages and get a completion response.

        Args:
            messages: List of messages in OpenAI format (role, content, etc.).
            **kwargs: Provider-specific parameters (temperature, max_tokens, etc.).

        Returns:
            Response object from the provider.
        """
        ...


@dataclass
class MinionConfig:
    """Minion processor configuration.

    Attributes:
        threshold: Character count above which to summarize results.
            Default is 32000 characters (~8000 tokens).
        max_summary_tokens: Maximum tokens for the summary response.
            Default is 1024.
        temperature: Sampling temperature for summarization. Lower values
            (0.0-0.3) are more deterministic. Default is 0.0.

    Example:
        config = MinionConfig(
            threshold=16000,  # Summarize at ~4000 tokens
            max_summary_tokens=512,
        )
    """

    threshold: int = 32000  # Characters (~8000 tokens)
    max_summary_tokens: int = 1024
    temperature: float = 0.0


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
    def parse_from_text(cls, text: str) -> ToolResultSummary:
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
        current_section: str | None = None

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
    """Summarizes large tool results using a (potentially cheaper) LLM.

    Uses war_rig.providers-compatible protocol directly - no LangChain dependency.

    The MinionProcessor intercepts tool outputs that exceed a character
    threshold and uses a fast, inexpensive model to extract key information.
    This reduces token usage in the main agent while preserving essential
    information.

    Attributes:
        _llm: The LLM provider for summarization calls.
        _config: Configuration options for the processor.

    Example:
        from war_rig.providers import get_provider_from_env

        provider = get_provider_from_env()
        processor = MinionProcessor(
            llm_provider=provider,
            config=MinionConfig(threshold=16000),
        )

        # Large file content gets summarized
        result = await processor.maybe_summarize(
            "read_file",
            very_long_file_content
        )

        # Small results pass through unchanged
        result = await processor.maybe_summarize(
            "search_skills",
            "Found 2 skills: auth, payments"
        )
    """

    # System prompt for summarization
    _SYSTEM_PROMPT = "You are a helpful assistant that summarizes tool outputs concisely."

    # User prompt template for summarization
    _SUMMARIZE_PROMPT_TEMPLATE = """Summarize this {tool_name} tool output for a code exploration agent.
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

    def __init__(
        self,
        llm_provider: LLMProvider,
        config: MinionConfig | None = None,
    ) -> None:
        """Initialize the minion processor.

        Args:
            llm_provider: The LLM provider for summarization calls.
                Must implement the LLMProvider protocol.
            config: Optional configuration. Defaults to MinionConfig().
        """
        self._llm = llm_provider
        self._config = config or MinionConfig()

        logger.debug(
            "MinionProcessor initialized: threshold=%d chars, max_tokens=%d",
            self._config.threshold,
            self._config.max_summary_tokens,
        )

    @property
    def threshold(self) -> int:
        """Get the character threshold for summarization.

        Returns:
            The character count above which results are summarized.
        """
        return self._config.threshold

    @property
    def config(self) -> MinionConfig:
        """Get the current configuration.

        Returns:
            The MinionConfig instance.
        """
        return self._config

    async def maybe_summarize(
        self,
        tool_name: str,
        result: str,
    ) -> str:
        """Summarize result if it exceeds threshold.

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
        if len(result) <= self._config.threshold:
            return result
        return await self._summarize(tool_name, result)

    async def _summarize(self, tool_name: str, result: str) -> str:
        """Perform summarization using LLM.

        Builds the summarization prompt, calls the LLM, and formats
        the response into the standard KEY POINTS + SUMMARY format.

        Args:
            tool_name: Name of the tool that produced the result.
            result: The tool's output to summarize.

        Returns:
            Formatted summary string, or original result on error.
        """
        logger.debug(
            "Summarizing %s result: %d chars (threshold: %d)",
            tool_name,
            len(result),
            self._config.threshold,
        )

        try:
            # Build summarization prompt
            prompt = self._SUMMARIZE_PROMPT_TEMPLATE.format(
                tool_name=tool_name,
                result=result,
            )

            messages = [
                {"role": "system", "content": self._SYSTEM_PROMPT},
                {"role": "user", "content": prompt},
            ]

            # Call the LLM
            response = await self._llm.complete(
                messages=messages,
                temperature=self._config.temperature,
                max_tokens=self._config.max_summary_tokens,
            )

            # Extract content from response - handle various response formats
            content = self._extract_content(response)

            # Parse the response into structured format
            summary_result = ToolResultSummary.parse_from_text(content)

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
                "Summarization complete: %d -> %d chars",
                len(result),
                len(formatted),
            )
            return formatted

        except Exception as e:
            # Graceful fallback: return original on any error
            logger.warning(
                "Minion summarization failed for %s, returning original: %s",
                tool_name,
                e,
            )
            return result

    def _extract_content(self, response: Any) -> str:
        """Extract text content from various LLM response formats.

        Handles different response structures from various providers:
        - OpenAI-style responses with choices[0].message.content
        - Dict responses with direct 'content' field
        - Object responses with content attribute

        Args:
            response: The raw response from the LLM provider.

        Returns:
            The extracted text content.
        """
        # OpenAI-style response with choices
        if hasattr(response, "choices") and response.choices:
            message = response.choices[0].message
            if hasattr(message, "content"):
                return message.content or ""
            return str(message)

        # Dict response
        if isinstance(response, dict):
            return str(response.get("content", ""))

        # Direct content attribute
        if hasattr(response, "content"):
            content = response.content
            return content if isinstance(content, str) else str(content)

        # Fallback to string conversion
        return str(response)
