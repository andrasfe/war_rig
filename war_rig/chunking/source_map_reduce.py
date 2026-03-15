"""Map-reduce pipeline for oversized paragraph source code.

When a single paragraph's source exceeds the token budget, truncation
loses the second half entirely.  This module instead:

1. **Map**: splits source into overlapping windows and summarises each
   with a lightweight LLM call.
2. **Reduce**: assembles the window summaries plus a representative
   source excerpt into a compressed replacement for ``source_code``.

Window sizes and trigger thresholds are derived from the configured
``max_prompt_tokens`` so they scale automatically with the LLM's
context window.

Ticket: war_rig-mapreduce

Example::

    from war_rig.chunking.source_map_reduce import SourceMapReducer

    reducer = SourceMapReducer(provider, model="my-model", max_prompt_tokens=65000)
    if reducer.needs_map_reduce(huge_source):
        result = await reducer.reduce(huge_source, paragraph_name="9000-MAIN-LOOP")
        scribe_input.source_code = result.reduced_source

See Also:
    - war_rig.chunking.estimator: Token estimation utilities
    - war_rig.summarization.bundle_summarizer: Similar parallel LLM pattern
    - war_rig.workers.scribe_pool: Integration point
"""

from __future__ import annotations

import asyncio
import logging
from dataclasses import dataclass, field

from war_rig.chunking.estimator import TokenEstimator
from war_rig.providers.protocol import LLMProvider, Message

logger = logging.getLogger(__name__)

# ---------------------------------------------------------------------------
# Prompts
# ---------------------------------------------------------------------------

MAP_WINDOW_SYSTEM_PROMPT: str = (
    "You are a mainframe code analysis specialist. Summarize COBOL source "
    "code windows into structured technical notes. Preserve verbatim: all "
    "field names (e.g., WS-FIELD-A), paragraph names, PERFORM targets, "
    "CALL targets, SQL statements, CICS commands, and numeric literals "
    "with business significance. Never paraphrase technical identifiers."
)

MAP_WINDOW_USER_PROMPT_TEMPLATE: str = """\
Summarize this COBOL source window. This is window {window_id} of \
{total_windows} from paragraph {paragraph_name} ({total_lines} total lines).

**Lines {line_start}-{line_end}:**

```cobol
{source_text}
```

Produce a structured summary covering:
1. **Data flow**: Which fields are read, written, or transformed (use exact names)
2. **Control flow**: PERFORM calls, GO TO targets, EVALUATE/IF branches
3. **External interactions**: CALL, SQL, CICS, file I/O operations
4. **Business logic**: Conditions, calculations, validation rules
5. **Anomalies**: Dead code, unreachable paths, unusual patterns

Be concise but preserve all technical identifiers verbatim. Target ~200 words.\
"""


# ---------------------------------------------------------------------------
# Data classes
# ---------------------------------------------------------------------------


@dataclass
class WindowSummary:
    """Summary of one source window from the map phase."""

    window_id: int
    line_start: int
    line_end: int
    total_lines: int
    summary: str


@dataclass
class MapReduceResult:
    """Output of the source map-reduce pipeline."""

    original_lines: int
    original_tokens: int
    num_windows: int
    window_summaries: list[WindowSummary] = field(default_factory=list)
    representative_excerpt: str = ""
    reduced_source: str = ""
    tokens_saved: int = 0


# ---------------------------------------------------------------------------
# SourceMapReducer
# ---------------------------------------------------------------------------


class SourceMapReducer:
    """Map-reduce pipeline for oversized paragraph source code.

    When a single paragraph's source exceeds the token budget, this class:

    1. Splits source into overlapping windows sized to fit a map-phase prompt
    2. Summarises each window with a lightweight LLM call (map)
    3. Assembles summaries + a representative excerpt (reduce)

    The result replaces raw ``source_code`` in the ``ScribeInput``,
    preserving information that truncation would lose.

    Window sizes and the trigger threshold are derived from
    ``max_prompt_tokens`` so they scale with the LLM context window.
    """

    # Fraction of available source tokens that triggers map-reduce.
    TRIGGER_RATIO: float = 0.65

    # Fraction of map-phase prompt budget allocated to source window.
    WINDOW_SOURCE_RATIO: float = 0.75

    # Overlap between adjacent windows as fraction of window size.
    OVERLAP_RATIO: float = 0.10

    # Map-phase prompt overhead (system + instructions + response buffer).
    MAP_OVERHEAD_TOKENS: int = 1500

    # Fraction of the Scribe source budget reserved for the representative
    # excerpt in the reduced output.
    EXCERPT_RATIO: float = 0.20

    # Approximate characters per COBOL source line.
    COBOL_CHARS_PER_LINE: int = 72

    def __init__(
        self,
        provider: LLMProvider,
        model: str,
        max_prompt_tokens: int,
        temperature: float = 1.0,
        max_concurrency: int = 5,
    ) -> None:
        self._provider = provider
        self._model = model
        self._max_prompt_tokens = max_prompt_tokens
        self._temperature = temperature
        self._max_concurrency = max_concurrency
        self._estimator = TokenEstimator()

    # -- public API ---------------------------------------------------------

    def needs_map_reduce(self, source_code: str) -> bool:
        """Check whether *source_code* exceeds the trigger threshold.

        The threshold is ``TRIGGER_RATIO * max_prompt_tokens``.  If the
        estimated token count of *source_code* exceeds that, map-reduce
        should be applied.
        """
        threshold = int(self._max_prompt_tokens * self.TRIGGER_RATIO)
        tokens = self._estimator.estimate_source_tokens(source_code)
        return tokens > threshold

    async def reduce(
        self,
        source_code: str,
        paragraph_name: str = "",
    ) -> MapReduceResult:
        """Run the full map-reduce pipeline.

        1. Split source into overlapping windows.
        2. Summarise each window in parallel (bounded concurrency).
        3. Select a representative source excerpt from the beginning.
        4. Assemble reduced source text.

        Args:
            source_code: Full raw source of the oversized paragraph.
            paragraph_name: Paragraph name for context in prompts/headers.

        Returns:
            MapReduceResult with the compressed source replacement.
        """
        source_lines = source_code.split("\n")
        total_lines = len(source_lines)
        original_tokens = self._estimator.estimate_source_tokens(source_code)

        # 1. Split
        windows = self._split_windows(source_lines)
        total_windows = len(windows)

        logger.info(
            "SourceMapReducer: splitting %d-line paragraph %r into %d windows",
            total_lines,
            paragraph_name,
            total_windows,
        )

        # 2. Map (parallel with bounded concurrency)
        semaphore = asyncio.Semaphore(self._max_concurrency)

        async def _bounded_summarise(
            wid: int, start: int, end: int,
        ) -> WindowSummary:
            async with semaphore:
                return await self._summarize_window(
                    window_id=wid + 1,
                    source_text="\n".join(source_lines[start:end]),
                    line_start=start + 1,
                    line_end=end,
                    total_lines=total_lines,
                    total_windows=total_windows,
                    paragraph_name=paragraph_name,
                )

        summaries = await asyncio.gather(
            *(
                _bounded_summarise(i, start, end)
                for i, (start, end) in enumerate(windows)
            )
        )

        # 3. Representative excerpt
        excerpt_tokens = int(self._max_prompt_tokens * self.EXCERPT_RATIO)
        excerpt = self._select_representative_excerpt(
            source_lines, excerpt_tokens,
        )

        # 4. Assemble
        reduced = self._assemble_reduced_source(
            list(summaries), excerpt, paragraph_name, total_lines,
        )

        reduced_tokens = self._estimator.estimate_source_tokens(reduced)

        return MapReduceResult(
            original_lines=total_lines,
            original_tokens=original_tokens,
            num_windows=total_windows,
            window_summaries=list(summaries),
            representative_excerpt=excerpt,
            reduced_source=reduced,
            tokens_saved=max(0, original_tokens - reduced_tokens),
        )

    # -- internals ----------------------------------------------------------

    def _compute_window_params(self) -> tuple[int, int]:
        """Derive (window_lines, overlap_lines) from the token budget."""
        window_token_budget = int(
            (self._max_prompt_tokens - self.MAP_OVERHEAD_TOKENS)
            * self.WINDOW_SOURCE_RATIO
        )
        window_chars = int(
            window_token_budget * self._estimator.cobol_chars_per_token
        )
        window_lines = max(100, window_chars // self.COBOL_CHARS_PER_LINE)
        overlap_lines = max(10, int(window_lines * self.OVERLAP_RATIO))
        return window_lines, overlap_lines

    def _split_windows(
        self, source_lines: list[str],
    ) -> list[tuple[int, int]]:
        """Split source into overlapping ``(start, end)`` ranges.

        Indices are 0-based; *end* is exclusive.
        """
        total = len(source_lines)
        window_lines, overlap_lines = self._compute_window_params()
        stride = window_lines - overlap_lines

        windows: list[tuple[int, int]] = []
        pos = 0
        while pos < total:
            end = min(pos + window_lines, total)
            windows.append((pos, end))
            if end >= total:
                break
            pos += stride

        return windows

    async def _summarize_window(
        self,
        window_id: int,
        source_text: str,
        line_start: int,
        line_end: int,
        total_lines: int,
        total_windows: int,
        paragraph_name: str,
    ) -> WindowSummary:
        """Map phase: summarise a single source window via LLM."""
        user_content = MAP_WINDOW_USER_PROMPT_TEMPLATE.format(
            window_id=window_id,
            total_windows=total_windows,
            paragraph_name=paragraph_name or "(unnamed)",
            total_lines=total_lines,
            line_start=line_start,
            line_end=line_end,
            source_text=source_text,
        )

        messages = [
            Message(role="system", content=MAP_WINDOW_SYSTEM_PROMPT),
            Message(role="user", content=user_content),
        ]

        try:
            response = await self._provider.complete(
                messages=messages,
                model=self._model,
                temperature=self._temperature,
            )
            summary_text = response.content.strip()
            logger.debug(
                "SourceMapReducer: window %d/%d summarised (%d chars, %d tokens)",
                window_id,
                total_windows,
                len(summary_text),
                response.tokens_used,
            )
        except Exception:
            logger.exception(
                "SourceMapReducer: window %d/%d failed, using line-range fallback",
                window_id,
                total_windows,
            )
            # Fallback: first + last lines of the window
            lines = source_text.split("\n")
            head = "\n".join(lines[:20])
            tail = "\n".join(lines[-20:]) if len(lines) > 40 else ""
            summary_text = (
                f"[Map-reduce fallback — LLM call failed]\n"
                f"First 20 lines:\n{head}"
            )
            if tail:
                summary_text += f"\n...\nLast 20 lines:\n{tail}"

        return WindowSummary(
            window_id=window_id,
            line_start=line_start,
            line_end=line_end,
            total_lines=total_lines,
            summary=summary_text,
        )

    def _select_representative_excerpt(
        self, source_lines: list[str], max_tokens: int,
    ) -> str:
        """Select a representative source excerpt from the beginning.

        The beginning of a COBOL paragraph typically contains the most
        structurally important code (initialisation, key conditionals).
        """
        max_chars = int(max_tokens * self._estimator.cobol_chars_per_token)
        collected: list[str] = []
        char_count = 0

        for line in source_lines:
            if char_count + len(line) + 1 > max_chars:
                break
            collected.append(line)
            char_count += len(line) + 1  # +1 for newline

        if len(collected) < len(source_lines):
            collected.append(
                f"      * ... ({len(source_lines) - len(collected)} "
                f"more lines omitted)"
            )

        return "\n".join(collected)

    @staticmethod
    def _assemble_reduced_source(
        window_summaries: list[WindowSummary],
        representative_excerpt: str,
        paragraph_name: str,
        total_lines: int,
    ) -> str:
        """Combine window summaries + excerpt into the reduced source text."""
        parts: list[str] = [
            f"## Source Analysis for {paragraph_name or '(unnamed)'} "
            f"({total_lines} lines)",
            "",
            "This paragraph exceeds the context window. Below are structured "
            "summaries of sequential source windows followed by a representative "
            "source excerpt from the beginning of the paragraph.",
            "",
        ]

        for ws in window_summaries:
            parts.append(
                f"### Window {ws.window_id} of {len(window_summaries)} "
                f"(lines {ws.line_start}-{ws.line_end})"
            )
            parts.append(ws.summary)
            parts.append("")

        parts.append(
            f"### Representative Source Excerpt (first lines)"
        )
        parts.append("```cobol")
        parts.append(representative_excerpt)
        parts.append("```")

        return "\n".join(parts)
