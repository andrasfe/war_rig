"""High-level facade for knowledge graph operations in the pipeline.

The KnowledgeGraphManager provides a clean, minimal interface for the
orchestration code to interact with the knowledge graph subsystem. All
graph operations are guarded by the enabled check and wrapped in
try/except to ensure the graph never breaks the main pipeline.

This module is the ONLY integration point that orchestration/worker code
needs to import. It encapsulates store lifecycle, triple extraction,
context formatting, and convergence checking.

Example:
    from war_rig.knowledge_graph.manager import KnowledgeGraphManager

    manager = KnowledgeGraphManager(config)
    await manager.initialize()

    # After preprocessing
    await manager.ingest_preprocessor(preprocessor_result)

    # Before Scribe runs
    context = await manager.get_scribe_context("ACCT0100")

    # After Scribe completes
    await manager.ingest_scribe_output(scribe_raw_output, "pass_1", "ACCT0100.cbl")

    # Cleanup
    await manager.close()
"""

from __future__ import annotations

import logging
from pathlib import Path

from war_rig.config import WarRigConfig
from war_rig.knowledge_graph.conflicts import ConflictDetector
from war_rig.knowledge_graph.context import ContextFormatter
from war_rig.knowledge_graph.extractors import TripleExtractor
from war_rig.knowledge_graph.ingestion import TripleIngestionCoordinator
from war_rig.knowledge_graph.models import Entity, EntityType
from war_rig.knowledge_graph.parser import TripleOutputParser
from war_rig.knowledge_graph.queries import GraphQueryHelper
from war_rig.knowledge_graph.sqlite_store import SQLiteGraphStore
from war_rig.preprocessors.base import PreprocessorResult

logger = logging.getLogger(__name__)


class KnowledgeGraphManager:
    """High-level facade for knowledge graph operations in the pipeline.

    Encapsulates the graph store lifecycle, triple ingestion from
    preprocessors and Scribe output, context formatting for prompt
    injection, and convergence tracking between passes.

    All public methods are safe to call even when the graph is disabled
    (they return empty/no-op results). All operations are wrapped in
    try/except to prevent graph failures from crashing the pipeline.

    Args:
        config: War Rig configuration with knowledge_graph settings.
    """

    def __init__(self, config: WarRigConfig) -> None:
        """Initialize the knowledge graph manager.

        Args:
            config: War Rig configuration.
        """
        self._config = config
        self._store: SQLiteGraphStore | None = None
        self._extractor = TripleExtractor()
        self._parser = TripleOutputParser()
        self._conflict_detector: ConflictDetector | None = None
        self._coordinator: TripleIngestionCoordinator | None = None
        self._context_formatter: ContextFormatter | None = None
        self._query_helper: GraphQueryHelper | None = None

    @property
    def enabled(self) -> bool:
        """Whether the knowledge graph subsystem is enabled."""
        return self._config.knowledge_graph_enabled

    async def initialize(self) -> None:
        """Initialize the graph store and internal components.

        Call this at pipeline start. Safe to call when disabled (no-op).
        Creates the SQLite database and initializes all components.
        """
        if not self.enabled:
            logger.debug("Knowledge graph disabled, skipping initialization")
            return

        try:
            # Place the knowledge graph DB inside the output directory
            # so it stays with the project output (not the repo root).
            db_path = str(
                self._config.output_directory / "knowledge_graph.db"
            )
            # Ensure parent directory exists
            db_dir = Path(db_path).parent
            db_dir.mkdir(parents=True, exist_ok=True)

            self._store = SQLiteGraphStore(db_path)
            await self._store.initialize()

            self._conflict_detector = ConflictDetector(self._store)
            self._coordinator = TripleIngestionCoordinator(
                self._store,
                self._extractor,
                self._parser,
                self._conflict_detector,
            )
            self._context_formatter = ContextFormatter(
                max_tokens=self._config.knowledge_graph_max_context_tokens,
            )
            self._query_helper = GraphQueryHelper(self._store)

            logger.info(
                "Knowledge graph initialized (db=%s, max_context_tokens=%d, "
                "neighborhood_hops=%d, convergence_threshold=%.2f)",
                db_path,
                self._config.knowledge_graph_max_context_tokens,
                self._config.knowledge_graph_neighborhood_hops,
                self._config.knowledge_graph_convergence_threshold,
            )
        except Exception:
            logger.warning(
                "Failed to initialize knowledge graph, continuing without it",
                exc_info=True,
            )
            self._store = None

    async def close(self) -> None:
        """Close the graph store and release resources.

        Safe to call when disabled or not initialized.
        """
        if self._store is not None:
            try:
                await self._store.close()
                logger.debug("Knowledge graph store closed")
            except Exception:
                logger.warning(
                    "Failed to close knowledge graph store", exc_info=True
                )
            self._store = None

    async def ingest_preprocessor(
        self,
        preprocessor_result: PreprocessorResult,
        source_pass: str = "preprocess",
    ) -> None:
        """Ingest triples from preprocessor output.

        Extracts deterministic triples from COBOL/JCL preprocessor
        results and stores them as ground-truth in the graph.

        Args:
            preprocessor_result: Output from any War Rig preprocessor.
            source_pass: Pass identifier for provenance tracking.
        """
        if not self.enabled:
            return
        if not self._config.knowledge_graph_extract_from_preprocessors:
            return
        if self._coordinator is None:
            return

        try:
            triples = await self._coordinator.ingest_from_preprocessor(
                preprocessor_result, source_pass
            )
            if triples:
                logger.debug(
                    "Ingested %d preprocessor triples for %s",
                    len(triples),
                    preprocessor_result.file_name,
                )
        except Exception:
            logger.warning(
                "Failed to ingest preprocessor triples for %s",
                preprocessor_result.file_name,
                exc_info=True,
            )

    async def get_scribe_context(self, program_name: str) -> str:
        """Get graph context for Scribe prompt injection.

        Queries the neighborhood around the given program and formats
        it as a structured text block for prompt injection.

        Args:
            program_name: Name of the program being documented.

        Returns:
            Formatted context string, or empty string if no context
            is available (disabled, not initialized, or entity not found).
        """
        if not self.enabled or self._store is None or self._context_formatter is None:
            return ""

        try:
            neighborhood = await self._store.get_neighborhood(
                program_name,
                EntityType.PROGRAM,
                hops=self._config.knowledge_graph_neighborhood_hops,
            )
            return self._context_formatter.format(neighborhood)
        except Exception:
            logger.warning(
                "Failed to get Scribe context for %s",
                program_name,
                exc_info=True,
            )
            return ""

    async def get_challenger_context(self, program_name: str) -> str:
        """Get graph context for Challenger prompt injection.

        Similar to get_scribe_context but includes additional cross-check
        hints (expected datasets, calls, copybooks) for structural
        validation.

        Args:
            program_name: Name of the program being validated.

        Returns:
            Formatted context string with cross-check hints, or empty
            string if no context is available.
        """
        if not self.enabled or self._store is None or self._context_formatter is None:
            return ""
        if not self._config.knowledge_graph_challenger_cross_check:
            return ""

        try:
            neighborhood = await self._store.get_neighborhood(
                program_name,
                EntityType.PROGRAM,
                hops=self._config.knowledge_graph_neighborhood_hops,
            )
            return self._context_formatter.format_for_challenger(neighborhood)
        except Exception:
            logger.warning(
                "Failed to get Challenger context for %s",
                program_name,
                exc_info=True,
            )
            return ""

    async def ingest_scribe_output(
        self,
        scribe_output: str,
        source_pass: str,
        source_artifact: str,
    ) -> None:
        """Parse and ingest triples from Scribe LLM output.

        Parses the structured triple block from Scribe output and
        ingests the triples into the graph with conflict detection.

        Args:
            scribe_output: Full text output from the Scribe agent.
            source_pass: Which War Rig pass produced this output.
            source_artifact: Source file the Scribe was analyzing.
        """
        if not self.enabled:
            return
        if not self._config.knowledge_graph_emit_triples_from_scribe:
            return
        if self._coordinator is None:
            return

        try:
            triples = await self._coordinator.ingest_from_scribe_output(
                scribe_output, source_pass, source_artifact
            )
            if triples:
                logger.debug(
                    "Ingested %d Scribe triples for %s (pass %s)",
                    len(triples),
                    source_artifact,
                    source_pass,
                )
        except Exception:
            logger.warning(
                "Failed to ingest Scribe triples for %s",
                source_artifact,
                exc_info=True,
            )

    async def check_convergence(
        self, pass_from: str, pass_to: str
    ) -> bool:
        """Check if the graph has converged between passes.

        Convergence is reached when the triple delta drops below the
        configured threshold (default 5%).

        Args:
            pass_from: Earlier pass identifier.
            pass_to: Later pass identifier.

        Returns:
            True if change rate is below threshold, False if not
            converged or if the check fails.
        """
        if not self.enabled or self._query_helper is None:
            return False

        try:
            threshold = self._config.knowledge_graph_convergence_threshold
            converged = await self._query_helper.check_convergence(
                pass_from, pass_to, threshold
            )
            if converged:
                logger.info(
                    "Knowledge graph converged between %s and %s "
                    "(threshold=%.2f)",
                    pass_from,
                    pass_to,
                    threshold,
                )
            return converged
        except Exception:
            logger.warning(
                "Failed to check convergence between %s and %s",
                pass_from,
                pass_to,
                exc_info=True,
            )
            return False

    async def get_imperator_summary(
        self, current_pass: str | None = None
    ) -> str:
        """Get a compact text summary of KG health for Imperator prompt injection.

        Builds a ~200 token markdown block summarizing entity counts,
        triple confirmation rates, unresolved conflicts, and optionally
        convergence delta for the current pass.

        Args:
            current_pass: Current pass identifier (e.g. "pass_2"). If
                provided and cycle > 1, includes convergence delta info.

        Returns:
            Formatted markdown block, or empty string if disabled,
            not initialized, or an error occurs.
        """
        if not self.enabled or self._store is None or self._query_helper is None:
            return ""

        try:
            # Entity breakdown
            breakdown = await self._query_helper.get_entity_breakdown()
            total_entities = sum(breakdown.values())

            # Confirmation stats
            conf_stats = await self._query_helper.get_confirmation_stats()
            total_triples = conf_stats["total"]
            confirmation_rate = conf_stats["confirmation_rate"]
            avg_corroboration = conf_stats["avg_corroboration"]

            # Unresolved conflicts
            conflicts = await self._store.get_unresolved_conflicts()
            conflict_count = len(conflicts)

            # Format entity breakdown as compact list
            type_parts = []
            for type_name, count in sorted(
                breakdown.items(), key=lambda x: x[1], reverse=True
            ):
                type_parts.append(f"{count} {type_name.lower()}s")
            type_summary = ", ".join(type_parts) if type_parts else "none"

            # Build summary lines
            lines = ["## Knowledge Graph Health"]
            lines.append(
                f"- {total_entities} entities ({type_summary})"
            )
            lines.append(
                f"- {int(total_triples)} triples: "
                f"{confirmation_rate:.0%} confirmed, "
                f"avg corroboration {avg_corroboration}x"
            )
            lines.append(f"- {conflict_count} unresolved conflicts")

            # Convergence delta (only if we have a pass > 1)
            if current_pass is not None:
                # Extract pass number to compute previous pass
                pass_num = self._extract_pass_number(current_pass)
                if pass_num is not None and pass_num > 1:
                    prev_pass = f"pass_{pass_num - 1}"
                    try:
                        delta = await self._store.compute_delta(
                            prev_pass, current_pass
                        )
                        rate_pct = delta.change_rate * 100
                        status = (
                            "converged" if delta.has_converged else "not converged"
                        )
                        lines.append(
                            f"- Convergence: {rate_pct:.1f}% delta ({status})"
                        )
                    except Exception:
                        logger.debug(
                            "Could not compute convergence delta for %s",
                            current_pass,
                        )

            return "\n".join(lines)

        except Exception:
            logger.warning(
                "Failed to build Imperator KG summary", exc_info=True
            )
            return ""

    @staticmethod
    def _extract_pass_number(pass_id: str) -> int | None:
        """Extract the numeric pass number from a pass identifier.

        Args:
            pass_id: Pass identifier such as "pass_2".

        Returns:
            The numeric portion, or None if not parseable.
        """
        parts = pass_id.rsplit("_", 1)
        if len(parts) == 2:
            try:
                return int(parts[1])
            except ValueError:
                return None
        return None

    async def get_system_summary(self, max_tokens: int = 400) -> str:
        """Get a system-level KG summary for README generation.

        Unlike get_imperator_summary() (which is for review decisions),
        this provides architectural context: most-connected hub entities,
        data flow hotspots, key relationships.

        Args:
            max_tokens: Approximate token budget for the output. The
                summary is capped at roughly ``max_tokens * 4`` characters.

        Returns:
            Compact markdown string, or empty string if the graph is
            unavailable or empty.
        """
        if not self.enabled or self._store is None:
            return ""

        try:
            entities = await self._store.get_all_entities()
            triples = await self._store.get_all_triples()

            if not entities or not triples:
                return ""

            # Build entity lookup
            entity_by_id: dict[int, Entity] = {
                e.id: e for e in entities if e.id is not None
            }

            # Count connections per entity (subject + object appearances)
            conn_count: dict[int, dict[str, int]] = {}
            # Track dataset readers/writers and copybook includers
            dataset_readers: dict[str, list[str]] = {}
            dataset_writers: dict[str, list[str]] = {}
            copybook_includers: dict[str, list[str]] = {}

            for t in triples:
                # Connection counts
                for eid in (t.subject_id, t.object_id):
                    if eid not in conn_count:
                        conn_count[eid] = {
                            "calls": 0,
                            "called_by": 0,
                            "reads": 0,
                            "writes": 0,
                            "includes": 0,
                            "other": 0,
                        }

                pred = t.predicate.value if hasattr(t.predicate, "value") else str(t.predicate)

                if pred == "CALLS":
                    conn_count[t.subject_id]["calls"] += 1
                    conn_count[t.object_id]["called_by"] += 1
                elif pred == "READS":
                    conn_count[t.subject_id]["reads"] += 1
                    obj = entity_by_id.get(t.object_id)
                    subj = entity_by_id.get(t.subject_id)
                    if obj and subj:
                        dataset_readers.setdefault(obj.name, []).append(subj.name)
                elif pred == "WRITES":
                    conn_count[t.subject_id]["writes"] += 1
                    obj = entity_by_id.get(t.object_id)
                    subj = entity_by_id.get(t.subject_id)
                    if obj and subj:
                        dataset_writers.setdefault(obj.name, []).append(subj.name)
                elif pred == "INCLUDES":
                    conn_count[t.subject_id]["includes"] += 1
                    obj = entity_by_id.get(t.object_id)
                    subj = entity_by_id.get(t.subject_id)
                    if obj and subj:
                        copybook_includers.setdefault(obj.name, []).append(subj.name)
                else:
                    conn_count[t.subject_id]["other"] += 1

            lines: list[str] = ["## System Architecture (from Knowledge Graph)", ""]

            # Hub programs: top 5 by total connections
            program_conns: list[tuple[str, int, dict[str, int]]] = []
            for eid, counts in conn_count.items():
                ent = entity_by_id.get(eid)
                if ent and ent.entity_type.value == "PROGRAM":
                    total = sum(counts.values())
                    if total > 0:
                        program_conns.append((ent.name, total, counts))

            program_conns.sort(key=lambda x: x[1], reverse=True)

            if program_conns:
                lines.append("### Hub Programs (most connections)")
                for name, total, counts in program_conns[:5]:
                    detail_parts = []
                    for label in ("calls", "called_by", "reads", "writes", "includes"):
                        if counts.get(label, 0) > 0:
                            detail_parts.append(f"{label} {counts[label]}")
                    detail = ", ".join(detail_parts)
                    lines.append(f"- {name}: {total} relationships ({detail})")
                lines.append("")

            # Data flow hotspots: datasets with most readers/writers
            dataset_activity: list[tuple[str, int, int]] = []
            all_datasets = set(dataset_readers.keys()) | set(dataset_writers.keys())
            for ds in all_datasets:
                r_count = len(dataset_readers.get(ds, []))
                w_count = len(dataset_writers.get(ds, []))
                if r_count + w_count > 1:
                    dataset_activity.append((ds, r_count, w_count))

            dataset_activity.sort(key=lambda x: x[1] + x[2], reverse=True)

            if dataset_activity:
                lines.append("### Data Flow Hotspots")
                for ds, r_count, w_count in dataset_activity[:5]:
                    parts = []
                    if r_count:
                        parts.append(f"read by {r_count} programs")
                    if w_count:
                        parts.append(f"written by {w_count}")
                    lines.append(f"- {ds}: {', '.join(parts)}")
                lines.append("")

            # Shared copybooks: top 5 by includer count
            copybook_shared: list[tuple[str, int]] = [
                (cb, len(progs))
                for cb, progs in copybook_includers.items()
                if len(progs) > 1
            ]
            copybook_shared.sort(key=lambda x: x[1], reverse=True)

            if copybook_shared:
                lines.append("### Shared Resources")
                for cb, count in copybook_shared[:5]:
                    lines.append(f"- {cb}: included by {count} programs")
                lines.append("")

            summary = "\n".join(lines).rstrip()

            # Cap at approximate token budget (4 chars per token)
            char_budget = max_tokens * 4
            if len(summary) > char_budget:
                summary = summary[:char_budget].rsplit("\n", 1)[0]

            return summary

        except Exception:
            logger.warning(
                "Failed to build system summary for README", exc_info=True
            )
            return ""

    async def get_stats(self) -> dict[str, int]:
        """Get basic graph statistics.

        Returns:
            Dictionary with entity_count and triple_count, or zeros
            if the graph is not available.
        """
        if not self.enabled or self._store is None:
            return {"entity_count": 0, "triple_count": 0}

        try:
            entities = await self._store.get_all_entities()
            triple_count = await self._store.get_triple_count()
            return {
                "entity_count": len(entities),
                "triple_count": triple_count,
            }
        except Exception:
            logger.warning("Failed to get graph stats", exc_info=True)
            return {"entity_count": 0, "triple_count": 0}
