"""Recursive document summarization for large COBOL files.

When a COBOL source file exceeds the practical context window for a
single Scribe pass, this package provides a structured multi-pass
compression strategy:

- **Pass 1** — Bundle summaries (parallel): structured extraction per
  paragraph bundle.
- **Pass 2** — Coherence merging: groups of bundle summaries merged
  into mid-level segment summaries.
- **Pass 3** — Root summary: all segments merged into a canonical
  file-level summary with verifiable claims.
- **Validation** — Challenger-driven claim verification with
  re-summarization loop.

The :class:`SummarizationPipeline` orchestrates all passes.
"""

from war_rig.summarization.pipeline import SummarizationPipeline

__all__ = ["SummarizationPipeline"]
