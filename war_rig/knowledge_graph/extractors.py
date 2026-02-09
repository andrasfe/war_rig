"""Deterministic triple extraction from preprocessor output.

This module extracts knowledge graph triples from the structural information
already produced by War Rig's preprocessors (COBOL, JCL, etc.). These triples
are treated as ground truth because they come from deterministic parsing,
not LLM inference.

The JCL pre-parser is particularly valuable — it can seed the graph with
reliable structural data before any LLM processing begins, enabling
graph context even on Pass 1.

Supported extractions:
- COBOL: CALLS, READS, WRITES, INCLUDES, PERFORMS, QUERIES, MODIFIES
- JCL: CONTAINS_STEP, EXECUTES, DEFINES_INPUT, DEFINES_OUTPUT

Example:
    from war_rig.preprocessors.cobol import COBOLStructure
    from war_rig.knowledge_graph.extractors import TripleExtractor

    extractor = TripleExtractor()
    raw_triples = extractor.extract_from_cobol(cobol_structure)
"""

import logging

from war_rig.knowledge_graph.models import EntityType, RawTriple, RelationType
from war_rig.preprocessors.base import PreprocessorResult
from war_rig.preprocessors.cobol import COBOLStructure
from war_rig.preprocessors.jcl import JCLStructure

logger = logging.getLogger(__name__)

# SQL operations that read data (SELECT, CURSOR declarations)
_SQL_READ_OPS = {"SELECT", "CURSOR"}

# SQL operations that modify data
_SQL_WRITE_OPS = {"INSERT", "UPDATE", "DELETE"}


class TripleExtractor:
    """Extracts deterministic triples from preprocessor output.

    This is a stateless utility — it takes preprocessor results and
    returns lists of RawTriple objects ready for ingestion into the store.

    Ground truth triples from preprocessors are never marked as unconfirmed;
    they serve as the baseline for validating LLM-emitted triples.
    """

    def extract(
        self,
        preprocessor_result: PreprocessorResult,
        source_pass: str = "preprocess",
    ) -> list[RawTriple]:
        """Extract triples from any preprocessor result.

        Dispatches to the appropriate extraction method based on the
        preprocessor result type.

        Args:
            preprocessor_result: Output from any War Rig preprocessor.
            source_pass: Pass identifier for provenance tracking.

        Returns:
            List of extracted RawTriple objects.
        """
        if isinstance(preprocessor_result, COBOLStructure):
            return self.extract_from_cobol(preprocessor_result, source_pass)
        if isinstance(preprocessor_result, JCLStructure):
            return self.extract_from_jcl(preprocessor_result, source_pass)

        logger.warning(
            "No triple extractor for preprocessor type %s (file: %s)",
            type(preprocessor_result).__name__,
            preprocessor_result.file_name,
        )
        return []

    def extract_from_cobol(
        self,
        structure: COBOLStructure,
        source_pass: str = "preprocess",
    ) -> list[RawTriple]:
        """Extract triples from COBOL preprocessor output.

        Extractions (per spec Section 4.2):
        - CALL statements -> PROGRAM CALLS PROGRAM
        - COPY statements -> PROGRAM INCLUDES COPYBOOK
        - File definitions -> PROGRAM READS DATASET (direction unknown from FD)
        - PERFORM statements -> PARAGRAPH PERFORMS PARAGRAPH
        - EXEC SQL SELECT/CURSOR -> PROGRAM QUERIES DB_TABLE
        - EXEC SQL INSERT/UPDATE/DELETE -> PROGRAM MODIFIES DB_TABLE

        Args:
            structure: COBOL preprocessor output.
            source_pass: Pass identifier for provenance.

        Returns:
            List of extracted RawTriple objects.
        """
        triples: list[RawTriple] = []
        program_name = structure.program_id
        source_artifact = structure.file_name

        if not program_name:
            logger.warning(
                "COBOL structure has no program_id, skipping extraction: %s",
                source_artifact,
            )
            return triples

        # CALL statements -> PROGRAM CALLS PROGRAM
        seen_calls: set[str] = set()
        for call in structure.calls:
            if call.program not in seen_calls:
                seen_calls.add(call.program)
                triples.append(RawTriple(
                    subject_type=EntityType.PROGRAM,
                    subject_name=program_name,
                    predicate=RelationType.CALLS,
                    object_type=EntityType.PROGRAM,
                    object_name=call.program,
                    source_pass=source_pass,
                    source_artifact=source_artifact,
                ))

        # COPY statements -> PROGRAM INCLUDES COPYBOOK
        seen_copybooks: set[str] = set()
        for copybook in structure.copybooks:
            if copybook.name not in seen_copybooks:
                seen_copybooks.add(copybook.name)
                triples.append(RawTriple(
                    subject_type=EntityType.PROGRAM,
                    subject_name=program_name,
                    predicate=RelationType.INCLUDES,
                    object_type=EntityType.COPYBOOK,
                    object_name=copybook.name,
                    source_pass=source_pass,
                    source_artifact=source_artifact,
                ))

        # File definitions -> PROGRAM READS DATASET
        # The preprocessor extracts SELECT/FD statements but not I/O direction,
        # so we emit READS as the conservative default. The Scribe's LLM triples
        # will refine with READS/WRITES based on OPEN INPUT/OUTPUT analysis.
        seen_files: set[str] = set()
        for file_def in structure.files:
            if file_def.name not in seen_files:
                seen_files.add(file_def.name)
                triples.append(RawTriple(
                    subject_type=EntityType.PROGRAM,
                    subject_name=program_name,
                    predicate=RelationType.READS,
                    object_type=EntityType.DATASET,
                    object_name=file_def.name,
                    source_pass=source_pass,
                    source_artifact=source_artifact,
                ))

        # PERFORM statements -> PARAGRAPH PERFORMS PARAGRAPH
        seen_performs: set[tuple[str, str]] = set()
        for perform in structure.performs:
            key = (perform.from_paragraph, perform.to_paragraph)
            if key not in seen_performs:
                seen_performs.add(key)
                triples.append(RawTriple(
                    subject_type=EntityType.PARAGRAPH,
                    subject_name=perform.from_paragraph,
                    predicate=RelationType.PERFORMS,
                    object_type=EntityType.PARAGRAPH,
                    object_name=perform.to_paragraph,
                    source_pass=source_pass,
                    source_artifact=source_artifact,
                ))

        # SQL statements -> PROGRAM QUERIES/MODIFIES DB_TABLE
        seen_sql: set[tuple[str, str]] = set()
        for sql in structure.sql_statements:
            if not sql.table:
                continue
            op = sql.operation.upper()
            if op in _SQL_READ_OPS:
                predicate = RelationType.QUERIES
            elif op in _SQL_WRITE_OPS:
                predicate = RelationType.MODIFIES
            else:
                continue
            key = (predicate.value, sql.table)
            if key not in seen_sql:
                seen_sql.add(key)
                triples.append(RawTriple(
                    subject_type=EntityType.PROGRAM,
                    subject_name=program_name,
                    predicate=predicate,
                    object_type=EntityType.DB_TABLE,
                    object_name=sql.table,
                    source_pass=source_pass,
                    source_artifact=source_artifact,
                ))

        logger.info(
            "Extracted %d triples from COBOL %s (%s)",
            len(triples),
            program_name,
            source_artifact,
        )
        return triples

    def extract_from_jcl(
        self,
        structure: JCLStructure,
        source_pass: str = "preprocess",
    ) -> list[RawTriple]:
        """Extract triples from JCL preprocessor output.

        Extractions (per spec Section 4.2, 10.2):
        - JOB card -> JCL_JOB entity
        - EXEC steps -> JCL_JOB CONTAINS_STEP JCL_STEP
        - PGM= references -> JCL_STEP EXECUTES PROGRAM
        - DD with DISP=SHR/OLD -> JCL_STEP DEFINES_INPUT DATASET
        - DD with DISP=(NEW,CATLG) -> JCL_STEP DEFINES_OUTPUT DATASET
        - INCLUDE members -> JCL_JOB INCLUDES COPYBOOK

        Args:
            structure: JCL preprocessor output.
            source_pass: Pass identifier for provenance.

        Returns:
            List of extracted RawTriple objects.
        """
        triples: list[RawTriple] = []
        job_name = structure.job_name
        source_artifact = structure.file_name

        if not job_name:
            logger.warning(
                "JCL structure has no job_name, skipping extraction: %s",
                source_artifact,
            )
            return triples

        # Build step name lookup for DD association
        step_names: set[str] = set()

        # EXEC steps -> JCL_JOB CONTAINS_STEP JCL_STEP
        for step in structure.steps:
            step_names.add(step.name)
            triples.append(RawTriple(
                subject_type=EntityType.JCL_JOB,
                subject_name=job_name,
                predicate=RelationType.CONTAINS_STEP,
                object_type=EntityType.JCL_STEP,
                object_name=step.name,
                source_pass=source_pass,
                source_artifact=source_artifact,
            ))

            # PGM= references -> JCL_STEP EXECUTES PROGRAM
            if step.program:
                triples.append(RawTriple(
                    subject_type=EntityType.JCL_STEP,
                    subject_name=step.name,
                    predicate=RelationType.EXECUTES,
                    object_type=EntityType.PROGRAM,
                    object_name=step.program,
                    source_pass=source_pass,
                    source_artifact=source_artifact,
                ))

        # DD statements -> JCL_STEP DEFINES_INPUT/DEFINES_OUTPUT DATASET
        for dd in structure.dd_statements:
            if not dd.dataset:
                continue
            # Skip SYSOUT DDs — they're print output, not dataset references
            if dd.sysout:
                continue

            relation = self._classify_dd_disposition(dd.disposition)
            if relation is None:
                continue

            triples.append(RawTriple(
                subject_type=EntityType.JCL_STEP,
                subject_name=dd.step_name,
                predicate=relation,
                object_type=EntityType.DATASET,
                object_name=dd.dataset,
                source_pass=source_pass,
                source_artifact=source_artifact,
            ))

        # INCLUDE members -> JCL_JOB INCLUDES COPYBOOK
        for member in structure.include_members:
            triples.append(RawTriple(
                subject_type=EntityType.JCL_JOB,
                subject_name=job_name,
                predicate=RelationType.INCLUDES,
                object_type=EntityType.COPYBOOK,
                object_name=member,
                source_pass=source_pass,
                source_artifact=source_artifact,
            ))

        logger.info(
            "Extracted %d triples from JCL %s (%s)",
            len(triples),
            job_name,
            source_artifact,
        )
        return triples

    def _classify_dd_disposition(self, disposition: str | None) -> RelationType | None:
        """Classify a DD disposition as input or output.

        Args:
            disposition: The DISP= value from JCL (e.g., "SHR", "(NEW,CATLG,DELETE)").

        Returns:
            DEFINES_INPUT, DEFINES_OUTPUT, or None if ambiguous/missing.
        """
        if not disposition:
            return None

        disp_upper = disposition.upper().strip()

        # Simple dispositions
        if disp_upper in ("SHR", "OLD"):
            return RelationType.DEFINES_INPUT

        # Parenthesized dispositions: (status, normal, abnormal)
        # Strip parens for analysis
        inner = disp_upper.strip("()")
        parts = [p.strip() for p in inner.split(",")]

        if not parts:
            return None

        status = parts[0]

        if status == "SHR" or status == "OLD":
            return RelationType.DEFINES_INPUT
        if status == "NEW":
            return RelationType.DEFINES_OUTPUT
        if status == "MOD":
            # MOD appends to existing — it's effectively an output
            return RelationType.DEFINES_OUTPUT

        logger.debug(
            "Ambiguous DD disposition, skipping: %s",
            disposition,
        )
        return None
