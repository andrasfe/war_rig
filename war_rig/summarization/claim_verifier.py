"""Claim verification for recursive summarization pipeline.

Checks verifiable claims from the root summary (Pass 3) against
bundle-level evidence and optionally raw COBOL source.  Each claim
carries ``evidence_bundle_ids`` that point back to specific
:class:`~war_rig.models.summaries.BundleSummary` objects; the verifier
collects the relevant evidence, optionally augments it with source
lines, and asks the LLM to render a verdict per claim.

The verifier is consumed by :class:`~war_rig.summarization.validation_loop.ValidationLoop`
which re-summarizes when refuted claims are detected.
"""

from __future__ import annotations

import json
import logging
import re
from typing import Any

from war_rig.models.summaries import (
    BundleSummary,
    ClaimVerdict,
    VerifiableClaim,
)
from war_rig.providers.protocol import CompletionResponse, LLMProvider, Message
from war_rig.summarization.prompts import (
    CLAIM_VERIFICATION_SYSTEM_PROMPT,
    CLAIM_VERIFICATION_USER_PROMPT_TEMPLATE,
)

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# JSON extraction helper
# ---------------------------------------------------------------------------


def _extract_json(text: str) -> dict[str, Any] | list[Any]:
    """Extract a JSON object or array from potentially noisy LLM output.

    Tries three strategies in order:

    1. Direct ``json.loads`` on the stripped text.
    2. Fenced ``\u0060\u0060\u0060json ... \u0060\u0060\u0060`` code blocks.
    3. Greedy brace / bracket matching (first ``{`` to last ``}``
       or first ``[`` to last ``]``).

    Args:
        text: Raw LLM response text.

    Returns:
        Parsed JSON as a ``dict`` or ``list``.  Returns an empty dict on
        total parse failure.
    """
    stripped = text.strip()

    # Strategy 1: direct parse
    try:
        parsed = json.loads(stripped)
        if isinstance(parsed, (dict, list)):
            return parsed
    except (json.JSONDecodeError, ValueError):
        pass

    # Strategy 2: fenced code blocks
    fence_match = re.search(r"```(?:json)?\s*\n?([\s\S]*?)```", stripped)
    if fence_match:
        try:
            parsed = json.loads(fence_match.group(1).strip())
            if isinstance(parsed, (dict, list)):
                return parsed
        except (json.JSONDecodeError, ValueError):
            pass

    # Strategy 3: greedy brace / bracket matching
    for open_ch, close_ch in [("{", "}"), ("[", "]")]:
        first = stripped.find(open_ch)
        last = stripped.rfind(close_ch)
        if first != -1 and last > first:
            try:
                parsed = json.loads(stripped[first : last + 1])
                if isinstance(parsed, (dict, list)):
                    return parsed
            except (json.JSONDecodeError, ValueError):
                pass

    logger.debug("_extract_json: failed to extract JSON from response")
    return {}


# ---------------------------------------------------------------------------
# Evidence builder
# ---------------------------------------------------------------------------


def _build_evidence_block(
    claim: VerifiableClaim,
    bundles_by_id: dict[str, BundleSummary],
    source_lines: list[str] | None,
) -> str:
    """Assemble evidence text for a single claim.

    Gathers functional summaries from the bundles referenced by the claim
    and optionally includes the relevant source line ranges.

    Args:
        claim: The claim to gather evidence for.
        bundles_by_id: Mapping of ``bundle_id`` to ``BundleSummary``.
        source_lines: Full source split into lines (1-indexed via list
            index + 1), or ``None`` if source is unavailable.

    Returns:
        A formatted evidence string ready for prompt injection.
    """
    parts: list[str] = []

    for bid in claim.evidence_bundle_ids:
        bundle = bundles_by_id.get(bid)
        if bundle is None:
            continue

        parts.append(f"--- Bundle {bid} ---")
        parts.append(f"Paragraphs: {', '.join(bundle.paragraph_names)}")
        if bundle.functional_summary:
            parts.append(f"Summary: {bundle.functional_summary}")
        if bundle.perform_calls:
            parts.append(f"PERFORM calls: {', '.join(bundle.perform_calls)}")
        if bundle.data_items_read:
            parts.append(f"Reads: {', '.join(bundle.data_items_read)}")
        if bundle.data_items_written:
            parts.append(f"Writes: {', '.join(bundle.data_items_written)}")
        if bundle.anomalies:
            parts.append(f"Anomalies: {', '.join(bundle.anomalies)}")

        # Attach source excerpt when available
        if source_lines and bundle.source_line_start > 0 and bundle.source_line_end > 0:
            start = max(0, bundle.source_line_start - 1)
            end = min(len(source_lines), bundle.source_line_end)
            excerpt = "".join(source_lines[start:end])
            parts.append(
                f"Source (lines {bundle.source_line_start}-{bundle.source_line_end}):"
            )
            parts.append(excerpt.rstrip())

        parts.append("")  # blank line between bundles

    return "\n".join(parts).strip() if parts else "(no evidence found)"


# ---------------------------------------------------------------------------
# ClaimVerifier
# ---------------------------------------------------------------------------


class ClaimVerifier:
    """Verifies claims from the root summary against bundle evidence.

    Uses the LLM as a judge to compare each claim against concrete
    evidence extracted from bundle summaries (and optionally raw COBOL
    source).  The result is an updated list of
    :class:`~war_rig.models.summaries.VerifiableClaim` objects with
    ``verdict`` and ``verification_notes`` populated.

    Args:
        provider: LLM provider for verification calls.
        model: Model identifier to use.
        temperature: Sampling temperature.  Defaults to ``1.0`` (required
            by reasoning models such as o3).
    """

    def __init__(
        self,
        provider: LLMProvider,
        model: str,
        temperature: float = 1.0,
    ) -> None:
        self._provider = provider
        self._model = model
        self._temperature = temperature

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    async def verify_claims(
        self,
        claims: list[VerifiableClaim],
        bundle_summaries: list[BundleSummary],
        source_lines: list[str] | None = None,
    ) -> list[VerifiableClaim]:
        """Verify a list of claims against bundle evidence.

        All claims are submitted in a single LLM call for efficiency.
        The LLM is asked to return a JSON array of verdicts keyed by
        ``claim_id``.

        Args:
            claims: Claims to verify (typically from
                :attr:`FileSummary.claims`).
            bundle_summaries: All bundle summaries from the file.
            source_lines: Optional full source for deeper verification.

        Returns:
            The *same* claim objects with ``verdict`` and
            ``verification_notes`` updated in place.  The list is
            returned for convenience.
        """
        if not claims:
            logger.debug("verify_claims: no claims to verify")
            return claims

        bundles_by_id: dict[str, BundleSummary] = {
            b.bundle_id: b for b in bundle_summaries
        }

        # Build per-claim evidence blocks
        evidence_blocks: list[str] = []
        for claim in claims:
            evidence = _build_evidence_block(claim, bundles_by_id, source_lines)
            evidence_blocks.append(
                f"### Claim [{claim.claim_id}]: {claim.claim}\n"
                f"Category: {claim.category.value}\n"
                f"Evidence bundles: "
                f"{', '.join(claim.evidence_bundle_ids)}\n\n"
                f"{evidence}"
            )

        all_evidence = "\n\n".join(evidence_blocks)

        user_content = CLAIM_VERIFICATION_USER_PROMPT_TEMPLATE.format(
            claims_and_evidence=all_evidence,
            claim_count=len(claims),
        )

        logger.debug(
            "verify_claims: built prompt with %d claims, evidence length=%d chars",
            len(claims),
            len(all_evidence),
        )

        messages = [
            Message(role="system", content=CLAIM_VERIFICATION_SYSTEM_PROMPT),
            Message(role="user", content=user_content),
        ]

        verdicts = await self._call_llm(messages, len(claims))
        self._apply_verdicts(claims, verdicts)

        confirmed = sum(1 for c in claims if c.verdict == ClaimVerdict.CONFIRMED)
        refuted = sum(1 for c in claims if c.verdict == ClaimVerdict.REFUTED)
        unverifiable = sum(1 for c in claims if c.verdict == ClaimVerdict.UNVERIFIABLE)
        logger.info(
            "verify_claims: %d confirmed, %d refuted, %d unverifiable",
            confirmed,
            refuted,
            unverifiable,
        )

        return claims

    # ------------------------------------------------------------------
    # Internals
    # ------------------------------------------------------------------

    async def _call_llm(
        self,
        messages: list[Message],
        expected_count: int,
    ) -> list[dict[str, Any]]:
        """Call the LLM and parse the verdict array.

        Args:
            messages: Prompt messages for the verification call.
            expected_count: Expected number of verdicts (for logging).

        Returns:
            A list of verdict dicts, each containing at least
            ``claim_id``, ``verdict``, and ``notes``.  Returns an empty
            list on failure.
        """
        try:
            response: CompletionResponse = await self._provider.complete(
                messages=messages,
                model=self._model,
                temperature=self._temperature,
            )
        except Exception:
            logger.warning(
                "ClaimVerifier: LLM call failed, marking all claims as UNVERIFIABLE",
                exc_info=True,
            )
            return []

        logger.debug(
            "ClaimVerifier: LLM returned %d chars, %d tokens",
            len(response.content),
            response.tokens_used,
        )

        parsed = _extract_json(response.content)

        # Accept either a top-level list or {"verdicts": [...]}
        if isinstance(parsed, list):
            verdicts = parsed
        elif isinstance(parsed, dict):
            verdicts = parsed.get("verdicts", [])
            if not isinstance(verdicts, list):
                verdicts = []
        else:
            verdicts = []

        if len(verdicts) != expected_count:
            logger.debug(
                "ClaimVerifier: expected %d verdicts but got %d",
                expected_count,
                len(verdicts),
            )

        return verdicts

    @staticmethod
    def _apply_verdicts(
        claims: list[VerifiableClaim],
        verdicts: list[dict[str, Any]],
    ) -> None:
        """Map verdict dicts back onto claim objects.

        If a verdict for a given ``claim_id`` is missing or unparseable,
        the claim is marked as :attr:`ClaimVerdict.UNVERIFIABLE`.

        Args:
            claims: Claims to update.
            verdicts: Parsed verdict dicts from the LLM.
        """
        verdict_map: dict[str, dict[str, Any]] = {}
        for v in verdicts:
            cid = v.get("claim_id", "")
            if cid:
                verdict_map[cid] = v

        for claim in claims:
            entry = verdict_map.get(claim.claim_id)
            if entry is None:
                claim.verdict = ClaimVerdict.UNVERIFIABLE
                claim.verification_notes = "No verdict returned by verifier."
                continue

            raw_verdict = str(entry.get("verdict", "")).upper().strip()
            try:
                claim.verdict = ClaimVerdict(raw_verdict)
            except ValueError:
                claim.verdict = ClaimVerdict.UNVERIFIABLE

            notes = entry.get("notes") or entry.get("reasoning") or ""
            claim.verification_notes = str(notes) if notes else None
