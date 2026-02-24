"""Top-level orchestrator for multi-pass recursive summarization.

Wires together the four stages of the summarization pipeline:

- **Pass 1** -- Bundle summaries via :class:`BundleSummarizer`
  (parallel, uses a cheaper / faster model by default).
- **Pass 2** -- Coherence merging via :class:`CoherenceMerger`
  (groups adjacent bundles into mid-level segments).
- **Pass 3** -- Root summary via :class:`RootSummarizer`
  (produces file-level summary with verifiable claims).
- **Validation** -- Iterative claim verification and re-summarization
  via :class:`ValidationLoop`.

Example::

    pipeline = SummarizationPipeline(
        provider, model="anthropic/claude-sonnet-4-20250514"
    )
    summary = await pipeline.run(
        program_id="CBACT04C",
        file_name="CBACT04C.cbl",
        chunk_templates=[template1, template2, ...],
        source_lines=source_code.splitlines(keepends=True),
    )
"""

from __future__ import annotations

import json
import logging
from pathlib import Path

from war_rig.models.summaries import FileSummary
from war_rig.models.templates import DocumentationTemplate
from war_rig.providers.protocol import LLMProvider
from war_rig.summarization.bundle_summarizer import (
    BundleInput,
    BundleSummarizer,
)
from war_rig.summarization.claim_verifier import ClaimVerifier
from war_rig.summarization.coherence_merger import CoherenceMerger
from war_rig.summarization.root_summarizer import RootSummarizer
from war_rig.summarization.validation_loop import ValidationLoop

logger = logging.getLogger(__name__)


class SummarizationPipeline:
    """Orchestrates multi-pass recursive summarization for large COBOL files.

    The pipeline converts a sequence of Scribe chunk templates into a
    single :class:`~war_rig.models.summaries.FileSummary` by running
    four stages in sequence.

    Args:
        provider: LLM provider instance.
        model: Primary model identifier (used for Passes 2, 3, and
            validation).
        pass1_model: Optional cheaper model for Pass 1 bundle
            extraction.  Defaults to *model*.
        temperature: Sampling temperature.  Defaults to ``1.0``
            (safe for reasoning models like o3).
        merge_group_size: Number of adjacent bundles grouped per
            coherence merge step.  Defaults to ``6``.
        max_validation_iterations: Maximum verify/re-summarize cycles.
            Defaults to ``3``.
    """

    def __init__(
        self,
        provider: LLMProvider,
        model: str,
        pass1_model: str | None = None,
        temperature: float = 1.0,
        merge_group_size: int = 6,
        max_validation_iterations: int = 3,
    ) -> None:
        self._provider = provider
        self._model = model
        self._pass1_model = pass1_model or model
        self._temperature = temperature
        self._merge_group_size = merge_group_size
        self._max_validation_iterations = max_validation_iterations

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    async def run(
        self,
        program_id: str,
        file_name: str,
        chunk_templates: list[tuple[str, DocumentationTemplate]],
        source_lines: list[str] | None = None,
    ) -> FileSummary:
        """Run the full summarization pipeline.

        Args:
            program_id: Program identifier (e.g. ``"CBACT04C"``).
            file_name: Source file name (e.g. ``"CBACT04C.cbl"``).
            chunk_templates: Ordered list of ``(chunk_id, template)``
                pairs produced by the Scribe chunk processing stage.
            source_lines: Optional full source (line list) for claim
                verification.  Pass
                ``source_code.splitlines(keepends=True)`` for best
                results.

        Returns:
            A fully populated :class:`FileSummary` with three levels of
            summaries and verified claims.
        """
        logger.info(
            "Starting summarization pipeline for %s (%d chunks)",
            program_id,
            len(chunk_templates),
        )

        # ---- Pass 1: Bundle summaries (parallel) ----
        bundle_summarizer = BundleSummarizer(
            self._provider,
            self._pass1_model,
            self._temperature,
        )
        bundle_inputs = self._build_bundle_inputs(chunk_templates)
        bundle_summaries = await bundle_summarizer.summarize_bundles(bundle_inputs)
        logger.info("Pass 1 complete: %d bundle summaries", len(bundle_summaries))

        # ---- Pass 2: Coherence merging ----
        merger = CoherenceMerger(
            self._provider,
            self._model,
            self._temperature,
            group_size=self._merge_group_size,
        )
        segment_summaries = await merger.merge_bundles(bundle_summaries)
        logger.info(
            "Pass 2 complete: %d segment summaries",
            len(segment_summaries),
        )

        # ---- Pass 3: Root summary ----
        root_summarizer = RootSummarizer(
            self._provider,
            self._model,
            self._temperature,
        )
        file_summary = await root_summarizer.summarize(
            program_id,
            file_name,
            segment_summaries,
            bundle_summaries,
        )
        logger.info(
            "Pass 3 complete: root summary with %d claims",
            len(file_summary.claims),
        )

        # ---- Validation loop ----
        verifier = ClaimVerifier(self._provider, self._model, self._temperature)
        loop = ValidationLoop(
            verifier, root_summarizer, self._max_validation_iterations
        )
        file_summary = await loop.validate(file_summary, source_lines)

        logger.info(
            "Summarization complete for %s: %d bundles, %d segments, "
            "%d claims (%d confirmed, %d refuted)",
            program_id,
            len(bundle_summaries),
            len(segment_summaries),
            len(file_summary.claims),
            file_summary.claims_verified,
            file_summary.claims_refuted,
        )
        return file_summary

    # ------------------------------------------------------------------
    # Bundle input builder
    # ------------------------------------------------------------------

    @staticmethod
    def _build_bundle_inputs(
        chunk_templates: list[tuple[str, DocumentationTemplate]],
    ) -> list[BundleInput]:
        """Convert chunk templates into :class:`BundleInput` objects.

        Each ``BundleInput`` carries the chunk ID, the paragraph names
        extracted from the template, and the full template JSON for the
        LLM prompt.

        Args:
            chunk_templates: Ordered ``(chunk_id, template)`` pairs.

        Returns:
            List of :class:`BundleInput` ready for
            :meth:`BundleSummarizer.summarize_bundles`.
        """
        inputs: list[BundleInput] = []
        for chunk_id, template in chunk_templates:
            para_names = [
                p.paragraph_name
                for p in (template.paragraphs or [])
                if p.paragraph_name
            ]
            template_json = template.model_dump_json(exclude_none=True)
            inputs.append(
                BundleInput(
                    bundle_id=chunk_id,
                    paragraph_names=para_names,
                    template_json=template_json,
                )
            )
        return inputs

    # ------------------------------------------------------------------
    # Persistence helpers
    # ------------------------------------------------------------------

    @staticmethod
    def save_summary(summary: FileSummary, output_path: Path) -> None:
        """Serialize a :class:`FileSummary` to a ``.summary.json`` file.

        Args:
            summary: The summary to persist.
            output_path: Destination path (typically
                ``<program>.summary.json``).
        """
        output_path.write_text(
            summary.model_dump_json(indent=2, exclude_none=True),
            encoding="utf-8",
        )
        logger.info("Saved summary to %s", output_path)

    @staticmethod
    def load_summary(path: Path) -> FileSummary:
        """Deserialize a :class:`FileSummary` from a ``.summary.json`` file.

        Args:
            path: Path to the JSON file.

        Returns:
            Validated :class:`FileSummary` instance.

        Raises:
            FileNotFoundError: If *path* does not exist.
            pydantic.ValidationError: If the JSON does not match the
                :class:`FileSummary` schema.
        """
        data = json.loads(path.read_text(encoding="utf-8"))
        return FileSummary.model_validate(data)
