"""Triple ingestion coordination.

This module orchestrates the ingestion of triples from multiple sources
into the knowledge graph store. It handles:

1. Preprocessor-derived triples (ground truth, high confidence)
2. LLM-emitted triples from Scribe output (supplementary, needs validation)
3. Batch ingestion with conflict detection

The coordinator is the single entry point for all triple ingestion,
ensuring consistent provenance tracking and conflict detection.

Example:
    coordinator = TripleIngestionCoordinator(store, extractor, parser, conflict_detector)
    triples = await coordinator.ingest_from_scribe_output(
        scribe_output="...",
        source_pass="pass_1",
        source_artifact="ACCT0100.cbl",
    )
"""

import logging

from war_rig.knowledge_graph.conflicts import ConflictDetector
from war_rig.knowledge_graph.extractors import TripleExtractor
from war_rig.knowledge_graph.models import RawTriple, Triple
from war_rig.knowledge_graph.parser import TripleOutputParser
from war_rig.knowledge_graph.store import KnowledgeGraphStore
from war_rig.preprocessors.base import PreprocessorResult

logger = logging.getLogger(__name__)


class TripleIngestionCoordinator:
    """Coordinates triple ingestion from all sources.

    Sits between the pipeline stages and the graph store, handling
    entity resolution, batch writes, and conflict detection.

    Args:
        store: The knowledge graph store to write into.
        extractor: Deterministic triple extractor for preprocessor output.
        parser: LLM output parser for Scribe-emitted triples.
        conflict_detector: Conflict detection engine.
    """

    def __init__(
        self,
        store: KnowledgeGraphStore,
        extractor: TripleExtractor,
        parser: TripleOutputParser,
        conflict_detector: ConflictDetector,
    ) -> None:
        """Initialize the ingestion coordinator.

        Args:
            store: Knowledge graph store.
            extractor: Deterministic triple extractor.
            parser: LLM triple output parser.
            conflict_detector: Conflict detection engine.
        """
        self._store = store
        self._extractor = extractor
        self._parser = parser
        self._conflict_detector = conflict_detector

    async def ingest_from_preprocessor(
        self,
        preprocessor_result: PreprocessorResult,
        source_pass: str = "preprocess",
    ) -> list[Triple]:
        """Extract and ingest triples from preprocessor output.

        These triples are treated as ground truth since they come from
        deterministic parsing. They are tagged with provenance "preprocess"
        and automatically confirmed upon corroboration.

        Args:
            preprocessor_result: Output from COBOL/JCL/etc. preprocessor.
            source_pass: Pass identifier (default "preprocess").

        Returns:
            List of ingested Triple objects.
        """
        raw_triples = self._extractor.extract(preprocessor_result, source_pass)
        if not raw_triples:
            logger.debug(
                "No triples extracted from preprocessor for %s",
                preprocessor_result.file_name,
            )
            return []

        triples = await self._store.ingest_raw_triples(
            raw_triples, confirmed=True
        )
        logger.info(
            "Ingested %d preprocessor triples for %s (from %d raw)",
            len(triples),
            preprocessor_result.file_name,
            len(raw_triples),
        )
        return triples

    async def ingest_from_scribe_output(
        self,
        scribe_output: str,
        source_pass: str,
        source_artifact: str,
    ) -> list[Triple]:
        """Parse and ingest triples from Scribe LLM output.

        These triples are supplementary — they need validation by the
        Challenger or corroboration across passes to be confirmed.
        After ingestion, conflict detection runs on the new triples.

        Args:
            scribe_output: Full text output from the Scribe agent.
            source_pass: Which War Rig pass produced this output.
            source_artifact: Source file the Scribe was analyzing.

        Returns:
            List of ingested Triple objects.
        """
        raw_triples = self._parser.parse(scribe_output, source_pass, source_artifact)
        if not raw_triples:
            logger.debug(
                "No triples parsed from Scribe output for %s (pass %s)",
                source_artifact,
                source_pass,
            )
            return []

        triples = await self._store.ingest_raw_triples(raw_triples)

        # Run conflict detection on newly ingested triples
        conflicts = await self._conflict_detector.detect_conflicts(triples)
        if conflicts:
            logger.warning(
                "Detected %d conflicts from Scribe triples for %s (pass %s)",
                len(conflicts),
                source_artifact,
                source_pass,
            )

        logger.info(
            "Ingested %d Scribe triples for %s (pass %s, %d conflicts)",
            len(triples),
            source_artifact,
            source_pass,
            len(conflicts),
        )
        return triples

    async def ingest_raw_triples(
        self,
        raw_triples: list[RawTriple],
        confirmed: bool = False,
    ) -> list[Triple]:
        """Directly ingest a batch of raw triples.

        Low-level method for when triples are already parsed/extracted.
        Runs conflict detection after ingestion.

        Args:
            raw_triples: Pre-parsed raw triples to ingest.
            confirmed: If True, mark new triples as confirmed on first
                insertion (for ground-truth sources).

        Returns:
            List of ingested Triple objects.
        """
        if not raw_triples:
            return []

        triples = await self._store.ingest_raw_triples(
            raw_triples, confirmed=confirmed
        )

        conflicts = await self._conflict_detector.detect_conflicts(triples)
        if conflicts:
            logger.warning(
                "Detected %d conflicts from raw triple ingestion",
                len(conflicts),
            )

        return triples


async def ingest_preprocessor_triples(
    store: KnowledgeGraphStore,
    preprocessor_result: PreprocessorResult,
    source_pass: str = "preprocess",
) -> list[Triple]:
    """Convenience function to ingest preprocessor triples without full coordinator.

    Creates a TripleExtractor, extracts triples, and ingests them into
    the store. This is the simplest integration point for pipeline code
    that just needs to seed the graph from preprocessor output.

    Args:
        store: The knowledge graph store.
        preprocessor_result: Output from any War Rig preprocessor.
        source_pass: Pass identifier for provenance (default "preprocess").

    Returns:
        List of ingested Triple objects.
    """
    extractor = TripleExtractor()
    raw_triples = extractor.extract(preprocessor_result, source_pass)
    if not raw_triples:
        return []

    triples = await store.ingest_raw_triples(raw_triples, confirmed=True)
    logger.info(
        "ingest_preprocessor_triples: %d triples for %s",
        len(triples),
        preprocessor_result.file_name,
    )
    return triples
