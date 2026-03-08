"""Lightweight cross-file context for prompt injection.

Replaces the Knowledge Graph with an in-memory adjacency map built from
the Citadel ``CallGraphAnalysis``.  Provides ``get_context(program_name)``
returning a token-budgeted markdown block describing a program's
relationships (calls, called-by, copybooks, datasets).

No database, no conflict detection, no provenance tracking — just a dict
look-up and a formatter.

Example:
    from war_rig.analysis.call_graph import CallGraphAnalyzer
    from war_rig.analysis.cross_file_context import CrossFileContext

    analysis = CallGraphAnalyzer(doc_directory=output_dir).analyze(dep_graph)
    ctx = CrossFileContext.from_call_graph(analysis)
    scribe_context = ctx.get_context("PAUDBUNL")
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from war_rig.analysis.call_graph import CallGraphAnalysis

logger = logging.getLogger(__name__)

# Budget defaults
_DEFAULT_MAX_TOKENS = 500
_CHARS_PER_TOKEN = 4
_MAX_PREVIEW = 3


@dataclass
class ProgramRelationships:
    """Adjacency entry for a single program."""

    calls: list[str] = field(default_factory=list)
    called_by: list[str] = field(default_factory=list)
    copybooks: list[str] = field(default_factory=list)
    datasets_read: list[str] = field(default_factory=list)
    datasets_written: list[str] = field(default_factory=list)
    summary: str | None = None
    file_type: str | None = None


class CrossFileContext:
    """In-memory adjacency map providing cross-file context for agents.

    Built once at batch start from ``CallGraphAnalysis``, then queried
    per-file during Scribe/Challenger processing.

    Args:
        max_tokens: Token budget for formatted context blocks.
    """

    def __init__(self, max_tokens: int = _DEFAULT_MAX_TOKENS) -> None:
        self._max_tokens = max_tokens
        self._programs: dict[str, ProgramRelationships] = {}

    # ── Construction ──────────────────────────────────────────────

    @classmethod
    def from_call_graph(
        cls,
        analysis: CallGraphAnalysis,
        max_tokens: int = _DEFAULT_MAX_TOKENS,
    ) -> CrossFileContext:
        """Build context from a completed ``CallGraphAnalysis``.

        Extracts calls, called-by, copybooks, and datasets from the
        analysis which was itself built from ``dependency_graph.json``
        and ``.doc.json`` files.

        Args:
            analysis: Completed call graph analysis.
            max_tokens: Token budget per context block.

        Returns:
            Populated CrossFileContext.
        """
        ctx = cls(max_tokens=max_tokens)

        for program_id, info in analysis.documented_programs.items():
            rels = ProgramRelationships(
                calls=[c.callee for c in info.calls],
                called_by=list(info.called_by),
                summary=info.summary,
                file_type=info.file_type,
            )
            ctx._programs[program_id.upper()] = rels

        # Build called_by from the calls relationships (fill gaps where
        # called_by isn't populated from docs)
        for program_id, rels in ctx._programs.items():
            for callee in rels.calls:
                callee_upper = callee.upper()
                if callee_upper in ctx._programs:
                    if program_id not in ctx._programs[callee_upper].called_by:
                        ctx._programs[callee_upper].called_by.append(program_id)

        logger.info(
            "CrossFileContext built: %d programs", len(ctx._programs)
        )
        return ctx

    def enrich_from_doc(
        self,
        program_id: str,
        copybooks: list[str] | None = None,
        datasets_read: list[str] | None = None,
        datasets_written: list[str] | None = None,
    ) -> None:
        """Enrich a program's relationships with doc-derived metadata.

        Called during worker processing when copybook/dataset info becomes
        available from preprocessors or templates.

        Args:
            program_id: Program name (case-insensitive).
            copybooks: Copybook names used by this program.
            datasets_read: Dataset names read by this program.
            datasets_written: Dataset names written by this program.
        """
        key = program_id.upper()
        if key not in self._programs:
            self._programs[key] = ProgramRelationships()
        rels = self._programs[key]
        if copybooks:
            rels.copybooks = list(dict.fromkeys(rels.copybooks + copybooks))
        if datasets_read:
            rels.datasets_read = list(
                dict.fromkeys(rels.datasets_read + datasets_read)
            )
        if datasets_written:
            rels.datasets_written = list(
                dict.fromkeys(rels.datasets_written + datasets_written)
            )

    # ── Context formatting ────────────────────────────────────────

    def get_context(self, program_name: str) -> str:
        """Get formatted cross-file context for a program.

        Returns a markdown block matching the shape previously produced
        by the knowledge graph's ``ContextFormatter``.

        Args:
            program_name: Program to get context for (case-insensitive).

        Returns:
            Formatted markdown string, or empty string if no context.
        """
        key = program_name.upper()
        rels = self._programs.get(key)
        if rels is None:
            return ""

        groups = self._build_groups(rels)
        if not groups:
            return ""

        body = self._format_groups(groups)

        header = (
            "## System Context (cross-file relationships)\n\n"
            "The program you are documenting exists in this context:\n\n"
        )
        footer = (
            "\n\n_This context is derived from static analysis. "
            "Use it to inform your documentation but verify against "
            "the source code._"
        )
        return header + body + footer

    def get_challenger_context(self, program_name: str) -> str:
        """Get context with cross-check hints for the Challenger.

        Args:
            program_name: Program to get context for.

        Returns:
            Formatted markdown with cross-check hints appended.
        """
        base = self.get_context(program_name)
        if not base:
            return ""

        key = program_name.upper()
        rels = self._programs.get(key)
        if rels is None:
            return base

        hints: list[str] = ["\n\n### Cross-Check Hints"]
        datasets = sorted(set(rels.datasets_read + rels.datasets_written))
        if datasets:
            hints.append(f"- Expected datasets: {', '.join(datasets)}")
        if rels.calls:
            hints.append(f"- Expected calls: {', '.join(sorted(set(rels.calls)))}")
        if rels.copybooks:
            hints.append(
                f"- Shared copybooks: {', '.join(sorted(set(rels.copybooks)))}"
            )

        if len(hints) > 1:
            return base + "\n".join(hints)
        return base

    # ── Internals ─────────────────────────────────────────────────

    @staticmethod
    def _build_groups(rels: ProgramRelationships) -> dict[str, list[str]]:
        """Group relationships by category."""
        groups: dict[str, list[str]] = {}
        if rels.calls:
            groups["CALLS OUT TO"] = rels.calls
        if rels.called_by:
            groups["CALLED BY"] = rels.called_by
        if rels.datasets_read:
            groups["READS FROM"] = rels.datasets_read
        if rels.datasets_written:
            groups["WRITES TO"] = rels.datasets_written
        if rels.copybooks:
            groups["INCLUDES"] = rels.copybooks
        return groups

    def _format_groups(self, groups: dict[str, list[str]]) -> str:
        """Format relationship groups, summarizing if over budget."""
        lines: list[str] = []
        for category, items in groups.items():
            lines.append(f"{category}:")
            for item in items:
                lines.append(f"  - {item}")

        body = "\n".join(lines)

        max_chars = self._max_tokens * _CHARS_PER_TOKEN
        if len(body) > max_chars:
            body = self._summarize_groups(groups)

        return body

    @staticmethod
    def _summarize_groups(groups: dict[str, list[str]]) -> str:
        """Produce a compressed summary when full listing exceeds budget."""
        lines: list[str] = []
        for category, items in groups.items():
            preview = ", ".join(items[:_MAX_PREVIEW])
            if len(items) > _MAX_PREVIEW:
                preview += ", ..."
            lines.append(f"{category}: {len(items)} items ({preview})")
        return "\n".join(lines)

    @property
    def program_count(self) -> int:
        """Number of programs in the context map."""
        return len(self._programs)
