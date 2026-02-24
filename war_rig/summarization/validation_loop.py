"""Iterative validation loop for recursive summarization.

Implements the verify-then-re-summarize cycle that ensures the root
summary (Pass 3) is consistent with the underlying bundle evidence.

Each iteration:

1. **Verify** -- Run :class:`ClaimVerifier` over all claims in the
   current :class:`~war_rig.models.summaries.FileSummary`.
2. **Convergence check** -- If no claims are
   :attr:`~war_rig.models.summaries.ClaimVerdict.REFUTED`, the summary
   is considered stable.
3. **Re-summarize** -- If refuted claims exist, regenerate the root
   summary via :class:`RootSummarizer`, incorporating the feedback
   from verification.

The loop stops when it converges *or* when ``max_iterations`` is
reached, whichever comes first.
"""

from __future__ import annotations

import logging

from war_rig.models.summaries import ClaimVerdict, FileSummary
from war_rig.summarization.claim_verifier import ClaimVerifier
from war_rig.summarization.root_summarizer import RootSummarizer

logger = logging.getLogger(__name__)


class ValidationLoop:
    """Iterative claim verification with re-summarization on discrepancy.

    Args:
        verifier: Claim verifier instance.
        root_summarizer: Root summarizer used to regenerate the summary
            when refuted claims are detected.
        max_iterations: Maximum number of verify/re-summarize cycles.
            Defaults to ``3``.
    """

    def __init__(
        self,
        verifier: ClaimVerifier,
        root_summarizer: RootSummarizer,
        max_iterations: int = 3,
    ) -> None:
        self._verifier = verifier
        self._root_summarizer = root_summarizer
        self._max_iterations = max_iterations

    async def validate(
        self,
        summary: FileSummary,
        source_lines: list[str] | None = None,
    ) -> FileSummary:
        """Run the validation loop until stable or budget exhausted.

        Args:
            summary: The file-level summary produced by Pass 3.
            source_lines: Optional full source for deeper claim
                verification.

        Returns:
            The (possibly regenerated) :class:`FileSummary` with
            ``validation_iterations`` set to the number of iterations
            actually performed and all claim verdicts populated.
        """
        iteration = 0

        for iteration in range(self._max_iterations):
            logger.info(
                "Validation iteration %d/%d for %s: %d claims",
                iteration + 1,
                self._max_iterations,
                summary.program_id,
                len(summary.claims),
            )

            # ----- Step 1: verify all claims -----
            updated_claims = await self._verifier.verify_claims(
                summary.claims,
                summary.bundle_summaries,
                source_lines,
            )
            summary.claims = updated_claims

            # ----- Step 2: convergence check -----
            refuted = [c for c in summary.claims if c.verdict == ClaimVerdict.REFUTED]
            if not refuted:
                confirmed = summary.claims_verified
                unverifiable = sum(
                    1 for c in summary.claims if c.verdict == ClaimVerdict.UNVERIFIABLE
                )
                logger.info(
                    "Validation converged for %s after %d iteration(s): "
                    "%d confirmed, %d unverifiable",
                    summary.program_id,
                    iteration + 1,
                    confirmed,
                    unverifiable,
                )
                break

            # ----- Step 3: re-summarize -----
            logger.info(
                "%d refuted claims for %s, re-summarizing",
                len(refuted),
                summary.program_id,
            )
            summary = await self._root_summarizer.summarize(
                program_id=summary.program_id,
                file_name=summary.file_name,
                segments=summary.segment_summaries,
                bundles=summary.bundle_summaries,
            )

        summary.validation_iterations = iteration + 1
        return summary
