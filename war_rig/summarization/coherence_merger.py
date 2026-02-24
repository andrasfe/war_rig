"""Pass 2: Coherence merging of bundle summaries into segment summaries.

This module provides the :class:`CoherenceMerger` which groups adjacent
bundle summaries and merges each group into a single
:class:`SegmentSummary`.  During merging the LLM is asked to resolve
coreferences, thread argument flows, flag contradictions, and produce a
unified narrative for the functional area covered by the group.

The grouping strategy is pluggable via :class:`BundleGroupingStrategy`;
the default is :class:`AdjacentSequentialGrouping` which creates groups
of ``group_size`` adjacent bundles.

Example:
    from war_rig.summarization.coherence_merger import CoherenceMerger

    merger = CoherenceMerger(
        provider=my_provider,
        model="my-model",
        group_size=6,
    )
    segments = await merger.merge_bundles(bundle_summaries)
"""

from __future__ import annotations

import json
import logging
import re
from typing import Any

from war_rig.models.summaries import BundleSummary, SegmentSummary
from war_rig.providers.protocol import LLMProvider, Message
from war_rig.summarization.grouping import (
    AdjacentSequentialGrouping,
    BundleGroupingStrategy,
)
from war_rig.summarization.prompts import (
    COHERENCE_MERGE_SYSTEM_PROMPT,
    COHERENCE_MERGE_USER_PROMPT_TEMPLATE,
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
# Merger
# ---------------------------------------------------------------------------


class CoherenceMerger:
    """Coherence merger for bundle-to-segment compression (Pass 2).

    Groups bundle summaries using a configurable strategy, then sends each
    group to the LLM for coherence merging.  The result is a list of
    :class:`SegmentSummary` objects, each covering a contiguous functional
    area of the program.

    Attributes:
        _provider: LLM provider for completion calls.
        _model: Model identifier to use.
        _temperature: Sampling temperature.
        _group_size: Target number of bundles per segment group.
        _grouping: Strategy used to partition bundles into groups.
    """

    def __init__(
        self,
        provider: LLMProvider,
        model: str,
        temperature: float = 1.0,
        group_size: int = 6,
        grouping_strategy: BundleGroupingStrategy | None = None,
    ) -> None:
        """Initialise the coherence merger.

        Args:
            provider: LLM provider instance satisfying the
                :class:`~war_rig.providers.protocol.LLMProvider` protocol.
            model: Model identifier string.
            temperature: Sampling temperature for LLM calls.
            group_size: Target number of bundles per merged segment.
            grouping_strategy: Strategy for partitioning bundles into
                groups.  Defaults to :class:`AdjacentSequentialGrouping`.
        """
        self._provider = provider
        self._model = model
        self._temperature = temperature
        self._group_size = group_size
        self._grouping: BundleGroupingStrategy = (
            grouping_strategy or AdjacentSequentialGrouping()
        )

    # -- public API ---------------------------------------------------------

    async def merge_bundles(self, bundles: list[BundleSummary]) -> list[SegmentSummary]:
        """Group and merge bundles into segment summaries.

        Bundles are first partitioned using the configured grouping
        strategy, then each group is merged into a single
        :class:`SegmentSummary` via an LLM call.

        Args:
            bundles: Ordered list of bundle summaries from Pass 1.

        Returns:
            Ordered list of segment summaries, one per group.
        """
        groups = self._grouping.group(bundles, self._group_size)

        logger.debug(
            "CoherenceMerger: %d bundles grouped into %d segments (group_size=%d)",
            len(bundles),
            len(groups),
            self._group_size,
        )

        segments: list[SegmentSummary] = []
        for i, group in enumerate(groups):
            segment_id = f"SEG-{i + 1:03d}"
            seg = await self._merge_group(segment_id, group)
            segments.append(seg)

        logger.info(
            "CoherenceMerger: merged %d bundles into %d segments",
            len(bundles),
            len(segments),
        )
        return segments

    # -- internal -----------------------------------------------------------

    async def _merge_group(
        self, segment_id: str, bundles: list[BundleSummary]
    ) -> SegmentSummary:
        """Merge a single group of bundles into a segment summary.

        Serialises each bundle summary to JSON, builds the prompt, calls
        the LLM, and parses the result.  On failure returns a fallback
        segment with concatenated functional summaries.

        Args:
            segment_id: Identifier for the resulting segment
                (e.g. ``SEG-001``).
            bundles: Bundle summaries to merge.

        Returns:
            A :class:`SegmentSummary` populated from the LLM response,
            or a fallback instance when parsing fails.
        """
        bundle_ids = [b.bundle_id for b in bundles]

        logger.debug(
            "CoherenceMerger: merging group %s (%d bundles: %s)",
            segment_id,
            len(bundles),
            ", ".join(bundle_ids),
        )

        # Serialise each bundle summary for the prompt
        bundles_json = json.dumps(
            [b.model_dump(mode="json") for b in bundles],
            indent=2,
        )

        user_content = COHERENCE_MERGE_USER_PROMPT_TEMPLATE.format(
            segment_id=segment_id,
            bundle_summaries_json=bundles_json,
        )

        messages = [
            Message(role="system", content=COHERENCE_MERGE_SYSTEM_PROMPT),
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
                "CoherenceMerger: provider.complete() failed for segment %s",
                segment_id,
            )
            return self._fallback_segment(segment_id, bundles)

        logger.debug(
            "CoherenceMerger: received response for segment %s (%d chars, %d tokens)",
            segment_id,
            len(response.content),
            response.tokens_used,
        )

        return self._parse_response(segment_id, bundles, response.content)

    def _parse_response(
        self,
        segment_id: str,
        bundles: list[BundleSummary],
        content: str,
    ) -> SegmentSummary:
        """Parse the LLM response into a :class:`SegmentSummary`.

        Attempts JSON extraction; on failure falls back to a minimal
        segment that concatenates the bundle functional summaries.

        Args:
            segment_id: Identifier for this segment.
            bundles: Original bundles in this group (for metadata).
            content: Raw LLM response text.

        Returns:
            Parsed or fallback :class:`SegmentSummary`.
        """
        bundle_ids = [b.bundle_id for b in bundles]
        data = _extract_json(content)

        if not data:
            logger.warning(
                "CoherenceMerger: failed to extract JSON for "
                "segment %s; using fallback",
                segment_id,
            )
            return self._fallback_segment(segment_id, bundles)

        # Ensure identifying fields are present
        data.setdefault("segment_id", segment_id)
        data.setdefault("bundle_ids", bundle_ids)

        try:
            segment = SegmentSummary.model_validate(data)
        except Exception:
            logger.warning(
                "CoherenceMerger: Pydantic validation failed for "
                "segment %s; using fallback",
                segment_id,
                exc_info=True,
            )
            return self._fallback_segment(segment_id, bundles)

        logger.info(
            "CoherenceMerger: segment %s merged -- area=%r, "
            "%d data flows, %d perform links, "
            "%d coreferences resolved, %d contradictions flagged",
            segment.segment_id,
            segment.functional_area,
            len(segment.data_flows),
            len(segment.perform_graph),
            len(segment.resolved_coreferences),
            len(segment.flagged_contradictions),
        )
        return segment

    @staticmethod
    def _fallback_segment(
        segment_id: str, bundles: list[BundleSummary]
    ) -> SegmentSummary:
        """Create a minimal fallback segment summary.

        Concatenates the ``functional_summary`` fields of the input
        bundles to preserve as much information as possible when the LLM
        call or parse step fails.

        Args:
            segment_id: Identifier for this segment.
            bundles: Original bundles in this group.

        Returns:
            A :class:`SegmentSummary` with concatenated summaries.
        """
        combined_summary = " ".join(
            b.functional_summary for b in bundles if b.functional_summary
        )
        return SegmentSummary(
            segment_id=segment_id,
            bundle_ids=[b.bundle_id for b in bundles],
            functional_area="",
            summary=combined_summary.strip(),
        )
