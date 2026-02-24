"""Pass 1: Parallel bundle summarization for large COBOL files.

This module provides the :class:`BundleSummarizer` which processes individual
paragraph bundles in parallel, producing structured :class:`BundleSummary`
extractions for each bundle.  This is the first pass of the recursive
summarization pipeline.

Each bundle corresponds to a contiguous group of COBOL paragraphs.  The
summarizer sends the paragraph template JSON and optional source excerpt
to the LLM, requesting a structured extraction covering functional summary,
data items, PERFORM relationships, conditional branches, and anomalies.

Example:
    from war_rig.summarization.bundle_summarizer import (
        BundleInput,
        BundleSummarizer,
    )

    summarizer = BundleSummarizer(provider=my_provider, model="my-model")
    bundles = [
        BundleInput(
            bundle_id="BDL-001",
            paragraph_names=["0100-INIT", "0200-PROCESS"],
            template_json=template.model_dump_json(),
            source_excerpt=source_text,
        ),
    ]
    results = await summarizer.summarize_bundles(bundles)
"""

from __future__ import annotations

import asyncio
import json
import logging
import re
from typing import Any

from war_rig.models.summaries import BundleSummary
from war_rig.providers.protocol import LLMProvider, Message
from war_rig.summarization.prompts import (
    BUNDLE_SUMMARY_SYSTEM_PROMPT,
    BUNDLE_SUMMARY_USER_PROMPT_TEMPLATE,
)

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _extract_json(text: str) -> dict[str, Any]:
    """Extract a JSON object from LLM response text.

    Applies three strategies in order:

    1. Parse the entire text as JSON directly.
    2. Extract the first ``json`` fenced code block and parse it.
    3. Find the outermost ``{...}`` substring and parse it.

    Args:
        text: Raw text that may contain a JSON object.

    Returns:
        Parsed dictionary, or an empty dict if all strategies fail.
    """
    # Strategy 1: direct parse
    try:
        result = json.loads(text)
        if isinstance(result, dict):
            return result
    except (json.JSONDecodeError, TypeError):
        pass

    # Strategy 2: fenced code block ```json ... ```
    fence_match = re.search(r"```json\s*\n?(.*?)\n?\s*```", text, re.DOTALL)
    if fence_match:
        try:
            result = json.loads(fence_match.group(1))
            if isinstance(result, dict):
                return result
        except (json.JSONDecodeError, TypeError):
            pass

    # Strategy 3: outermost braces
    first_brace = text.find("{")
    last_brace = text.rfind("}")
    if first_brace != -1 and last_brace > first_brace:
        try:
            result = json.loads(text[first_brace : last_brace + 1])
            if isinstance(result, dict):
                return result
        except (json.JSONDecodeError, TypeError):
            pass

    return {}


# ---------------------------------------------------------------------------
# Input DTO
# ---------------------------------------------------------------------------


class BundleInput:
    """Input for a single bundle summarization request.

    This is a plain data-transfer object (not a Pydantic model) to keep the
    public API lightweight and avoid unnecessary validation overhead on the
    caller side.

    Attributes:
        bundle_id: Unique identifier for this bundle (e.g. ``BDL-001``).
        paragraph_names: Ordered list of paragraph names in this bundle.
        template_json: Serialised documentation template JSON for context.
        source_excerpt: Optional raw COBOL source excerpt for the bundle.
    """

    def __init__(
        self,
        bundle_id: str,
        paragraph_names: list[str],
        template_json: str,
        source_excerpt: str = "",
    ) -> None:
        self.bundle_id = bundle_id
        self.paragraph_names = paragraph_names
        self.template_json = template_json
        self.source_excerpt = source_excerpt


# ---------------------------------------------------------------------------
# Summarizer
# ---------------------------------------------------------------------------


class BundleSummarizer:
    """Parallel bundle summarizer (Pass 1).

    Creates structured :class:`BundleSummary` extractions for each paragraph
    bundle by calling the configured LLM provider.

    Attributes:
        _provider: LLM provider for completion calls.
        _model: Model identifier to use.
        _temperature: Sampling temperature.
    """

    def __init__(
        self,
        provider: LLMProvider,
        model: str,
        temperature: float = 1.0,
    ) -> None:
        """Initialise the bundle summarizer.

        Args:
            provider: LLM provider instance satisfying the
                :class:`~war_rig.providers.protocol.LLMProvider` protocol.
            model: Model identifier string (e.g.
                ``"anthropic/claude-sonnet-4-20250514"``).
            temperature: Sampling temperature for LLM calls.
        """
        self._provider = provider
        self._model = model
        self._temperature = temperature

    # -- public API ---------------------------------------------------------

    async def summarize_bundle(self, bundle: BundleInput) -> BundleSummary:
        """Summarize a single paragraph bundle.

        Builds a system + user message pair from the prompt templates,
        calls the LLM provider, and parses the JSON response into a
        :class:`BundleSummary`.  On any failure the method returns a
        minimal fallback summary rather than raising.

        Args:
            bundle: Input data describing the bundle to summarize.

        Returns:
            A :class:`BundleSummary` populated from the LLM response, or
            a fallback instance when parsing fails.
        """
        logger.debug(
            "BundleSummarizer: building prompt for bundle %s (%d paragraphs)",
            bundle.bundle_id,
            len(bundle.paragraph_names),
        )

        user_content = BUNDLE_SUMMARY_USER_PROMPT_TEMPLATE.format(
            bundle_id=bundle.bundle_id,
            paragraph_names=", ".join(bundle.paragraph_names),
            template_json=bundle.template_json,
            source_excerpt=bundle.source_excerpt,
        )

        messages = [
            Message(role="system", content=BUNDLE_SUMMARY_SYSTEM_PROMPT),
            Message(role="user", content=user_content),
        ]

        try:
            response = await self._provider.complete(
                messages=messages,
                model=self._model,
                temperature=self._temperature,
            )
        except Exception:
            logger.exception(
                "BundleSummarizer: provider.complete() failed for bundle %s",
                bundle.bundle_id,
            )
            return self._fallback_summary(bundle, raw_text="")

        logger.debug(
            "BundleSummarizer: received response for bundle %s (%d chars, %d tokens)",
            bundle.bundle_id,
            len(response.content),
            response.tokens_used,
        )

        return self._parse_response(bundle, response.content)

    async def summarize_bundles(
        self, bundles: list[BundleInput]
    ) -> list[BundleSummary]:
        """Summarize all bundles in parallel.

        Each bundle is processed as an independent asyncio task.  If any
        single bundle fails, the exception is **not** propagated -- the
        corresponding task returns a fallback summary (handled inside
        :meth:`summarize_bundle`).

        Args:
            bundles: List of bundle inputs to process.

        Returns:
            List of :class:`BundleSummary` results in the same order as
            the input bundles.
        """
        tasks = [self.summarize_bundle(b) for b in bundles]
        results: list[BundleSummary] = list(
            await asyncio.gather(*tasks, return_exceptions=False)
        )

        logger.info(
            "BundleSummarizer: completed %d/%d bundle summaries",
            len(results),
            len(bundles),
        )
        return results

    # -- internal -----------------------------------------------------------

    def _parse_response(self, bundle: BundleInput, content: str) -> BundleSummary:
        """Parse the LLM response into a :class:`BundleSummary`.

        Attempts JSON extraction; on failure falls back to a minimal
        summary that preserves the raw text in ``functional_summary``.

        Args:
            bundle: The original bundle input (for metadata).
            content: Raw LLM response text.

        Returns:
            Parsed or fallback :class:`BundleSummary`.
        """
        data = _extract_json(content)

        if not data:
            logger.warning(
                "BundleSummarizer: failed to extract JSON for bundle %s; "
                "using raw text as functional_summary",
                bundle.bundle_id,
            )
            return self._fallback_summary(bundle, raw_text=content)

        # Ensure identifying fields are present
        data.setdefault("bundle_id", bundle.bundle_id)
        data.setdefault("paragraph_names", bundle.paragraph_names)

        try:
            summary = BundleSummary.model_validate(data)
        except Exception:
            logger.warning(
                "BundleSummarizer: Pydantic validation failed for "
                "bundle %s; using raw text as functional_summary",
                bundle.bundle_id,
                exc_info=True,
            )
            return self._fallback_summary(bundle, raw_text=content)

        logger.info(
            "BundleSummarizer: bundle %s summarized -- %d data items "
            "read, %d written, %d perform calls",
            summary.bundle_id,
            len(summary.data_items_read),
            len(summary.data_items_written),
            len(summary.perform_calls),
        )
        return summary

    @staticmethod
    def _fallback_summary(bundle: BundleInput, *, raw_text: str) -> BundleSummary:
        """Create a minimal fallback summary.

        Used when the LLM response cannot be parsed as valid JSON.

        Args:
            bundle: The original bundle input.
            raw_text: The raw LLM response text (stored as
                ``functional_summary``).

        Returns:
            A :class:`BundleSummary` with metadata preserved.
        """
        return BundleSummary(
            bundle_id=bundle.bundle_id,
            paragraph_names=bundle.paragraph_names,
            functional_summary=raw_text.strip() if raw_text else "",
        )
