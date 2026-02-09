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
from war_rig.knowledge_graph.models import EntityType
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
            db_path = self._config.knowledge_graph_db_path
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
