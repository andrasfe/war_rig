"""Pass 3: Root file-level summary with verifiable claims.

This module provides the :class:`RootSummarizer` which produces the
canonical :class:`FileSummary` for an entire COBOL source file.  It
receives the segment summaries from Pass 2 and the original bundle
summaries from Pass 1, then asks the LLM to synthesise a top-level
business function description, primary data flows, call graph summary,
risk areas, technical debt notes, migration considerations, and a set
of :class:`VerifiableClaim` assertions that the Challenger can later
validate against the source.

Example:
    from war_rig.summarization.root_summarizer import RootSummarizer

    summarizer = RootSummarizer(
        provider=my_provider,
        model="my-model",
    )
    file_summary = await summarizer.summarize(
        program_id="PROG001",
        file_name="PROG001.cbl",
        segments=segment_summaries,
        bundles=bundle_summaries,
    )
"""

from __future__ import annotations

import json
import logging
import re
from typing import Any

from war_rig.models.summaries import (
    BundleSummary,
    FileSummary,
    SegmentSummary,
    VerifiableClaim,
)
from war_rig.providers.protocol import LLMProvider, Message
from war_rig.summarization.prompts import (
    ROOT_SUMMARY_SYSTEM_PROMPT,
    ROOT_SUMMARY_USER_PROMPT_TEMPLATE,
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
# Summarizer
# ---------------------------------------------------------------------------


class RootSummarizer:
    """Root file-level summarizer (Pass 3).

    Produces the canonical :class:`FileSummary` by synthesising all
    segment summaries and bundle summaries into a single top-level
    narrative with verifiable claims.

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
        """Initialise the root summarizer.

        Args:
            provider: LLM provider instance satisfying the
                :class:`~war_rig.providers.protocol.LLMProvider` protocol.
            model: Model identifier string.
            temperature: Sampling temperature for LLM calls.
        """
        self._provider = provider
        self._model = model
        self._temperature = temperature

    # -- public API ---------------------------------------------------------

    async def summarize(
        self,
        program_id: str,
        file_name: str,
        segments: list[SegmentSummary],
        bundles: list[BundleSummary],
    ) -> FileSummary:
        """Create the root file-level summary from segment summaries.

        Builds a prompt containing all segment summaries, calls the LLM
        for a top-level synthesis, then attaches the full bundle and
        segment lists for drill-down.

        Args:
            program_id: Program identifier (e.g. ``"PROG001"``).
            file_name: Source file name (e.g. ``"PROG001.cbl"``).
            segments: Segment summaries produced by Pass 2.
            bundles: Bundle summaries produced by Pass 1 (attached
                to the result for traceability).

        Returns:
            A :class:`FileSummary` with ``passes_completed=3``, the
            full bundle and segment lists attached, and verifiable
            claims extracted from the LLM response.
        """
        logger.debug(
            "RootSummarizer: building prompt for %s (%d segments, %d bundles)",
            program_id,
            len(segments),
            len(bundles),
        )

        segments_json = json.dumps(
            [s.model_dump(mode="json") for s in segments],
            indent=2,
        )

        user_content = ROOT_SUMMARY_USER_PROMPT_TEMPLATE.format(
            program_id=program_id,
            file_name=file_name,
            segment_summaries_json=segments_json,
        )

        messages = [
            Message(role="system", content=ROOT_SUMMARY_SYSTEM_PROMPT),
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
                "RootSummarizer: provider.complete() failed for %s",
                program_id,
            )
            return self._fallback_summary(program_id, file_name, segments, bundles)

        logger.debug(
            "RootSummarizer: received response for %s (%d chars, %d tokens)",
            program_id,
            len(response.content),
            response.tokens_used,
        )

        return self._parse_response(
            program_id, file_name, segments, bundles, response.content
        )

    # -- internal -----------------------------------------------------------

    def _parse_response(
        self,
        program_id: str,
        file_name: str,
        segments: list[SegmentSummary],
        bundles: list[BundleSummary],
        content: str,
    ) -> FileSummary:
        """Parse the LLM response into a :class:`FileSummary`.

        Attempts JSON extraction; on failure falls back to a minimal
        summary that concatenates segment narratives.

        Args:
            program_id: Program identifier.
            file_name: Source file name.
            segments: Segment summaries from Pass 2.
            bundles: Bundle summaries from Pass 1.
            content: Raw LLM response text.

        Returns:
            Parsed or fallback :class:`FileSummary`.
        """
        data = _extract_json(content)

        if not data:
            logger.warning(
                "RootSummarizer: failed to extract JSON for %s; using fallback",
                program_id,
            )
            return self._fallback_summary(program_id, file_name, segments, bundles)

        # Ensure identifying fields are present
        data.setdefault("program_id", program_id)
        data.setdefault("file_name", file_name)

        # Parse claims sub-objects before top-level validation so that
        # raw dicts get properly coerced into VerifiableClaim instances.
        raw_claims = data.pop("claims", [])
        claims: list[VerifiableClaim] = []
        for raw_claim in raw_claims:
            if isinstance(raw_claim, dict):
                try:
                    claims.append(VerifiableClaim.model_validate(raw_claim))
                except Exception:
                    logger.warning(
                        "RootSummarizer: skipping invalid claim for %s: %s",
                        program_id,
                        raw_claim,
                    )
            elif isinstance(raw_claim, VerifiableClaim):
                claims.append(raw_claim)

        # Remove nested summaries from LLM output -- we attach the real
        # ones from earlier passes below.
        data.pop("bundle_summaries", None)
        data.pop("segment_summaries", None)
        data.pop("passes_completed", None)
        data.pop("timestamp", None)

        try:
            file_summary = FileSummary.model_validate(data)
        except Exception:
            logger.warning(
                "RootSummarizer: Pydantic validation failed for %s; using fallback",
                program_id,
                exc_info=True,
            )
            return self._fallback_summary(program_id, file_name, segments, bundles)

        # Attach the claims, bundle/segment lists, and mark completed
        file_summary.claims = claims
        file_summary.bundle_summaries = bundles
        file_summary.segment_summaries = segments
        file_summary.passes_completed = 3

        logger.info(
            "RootSummarizer: %s summarized -- business_function=%r, "
            "%d claims, %d risk areas, %d data flows",
            program_id,
            file_summary.business_function,
            len(file_summary.claims),
            len(file_summary.risk_areas),
            len(file_summary.primary_data_flows),
        )
        return file_summary

    @staticmethod
    def _fallback_summary(
        program_id: str,
        file_name: str,
        segments: list[SegmentSummary],
        bundles: list[BundleSummary],
    ) -> FileSummary:
        """Create a minimal fallback file summary.

        Concatenates segment summaries into the ``business_function``
        field and attaches all prior-pass data so that downstream
        consumers still have access to the bundle- and segment-level
        detail.

        Args:
            program_id: Program identifier.
            file_name: Source file name.
            segments: Segment summaries from Pass 2.
            bundles: Bundle summaries from Pass 1.

        Returns:
            A :class:`FileSummary` with ``passes_completed=3`` and
            concatenated segment narratives.
        """
        combined = " ".join(s.summary for s in segments if s.summary)
        return FileSummary(
            program_id=program_id,
            file_name=file_name,
            business_function=combined.strip(),
            bundle_summaries=bundles,
            segment_summaries=segments,
            passes_completed=3,
        )
