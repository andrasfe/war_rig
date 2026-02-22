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
from pathlib import Path

from war_rig.knowledge_graph.models import EntityType, RawTriple, RelationType
from war_rig.models.templates import DocumentationTemplate, IOType
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

    def extract_from_citadel_context(
        self,
        citadel_context: dict,
        file_name: str,
        source_pass: str = "citadel",
    ) -> list[RawTriple]:
        """Extract triples from Citadel static analysis context.

        Uses the function call graph and includes from Citadel's deterministic
        analysis as an alternative to preprocessor-based extraction. This is
        the primary KG seeding path for Citadel-guided processing.

        Citadel context structure:
        - functions: list of {name, type, line, calls: [{target, type, line}]}
        - includes: list of include file names

        Citadel call types mapped to KG relations:
        - "calls" -> PROGRAM CALLS PROGRAM
        - "performs" -> PARAGRAPH PERFORMS PARAGRAPH
        - "includes" -> PROGRAM INCLUDES COPYBOOK
        - "reads" -> PROGRAM READS DATASET
        - "writes" -> PROGRAM WRITES DATASET

        Args:
            citadel_context: Dictionary from _get_citadel_context() with
                functions and includes keys.
            file_name: Source file name for provenance tracking.
            source_pass: Pass identifier for provenance.

        Returns:
            List of extracted RawTriple objects.
        """
        triples: list[RawTriple] = []
        functions = citadel_context.get("functions", [])
        includes = citadel_context.get("includes", [])

        # Derive program name from file_name (strip extension, uppercase)
        program_name = Path(file_name).stem.upper()

        # Map Citadel relationship types to KG relation/entity pairs
        _CALL_TYPE_MAP: dict[str, tuple[RelationType, EntityType, EntityType]] = {
            "calls": (RelationType.CALLS, EntityType.PROGRAM, EntityType.PROGRAM),
            "performs": (RelationType.PERFORMS, EntityType.PARAGRAPH, EntityType.PARAGRAPH),
            "reads": (RelationType.READS, EntityType.PROGRAM, EntityType.DATASET),
            "writes": (RelationType.WRITES, EntityType.PROGRAM, EntityType.DATASET),
            "updates": (RelationType.MODIFIES, EntityType.PROGRAM, EntityType.DB_TABLE),
            "deletes": (RelationType.MODIFIES, EntityType.PROGRAM, EntityType.DB_TABLE),
            "executes": (RelationType.EXECUTES, EntityType.PROGRAM, EntityType.PROGRAM),
        }

        # Track seen triples to avoid duplicates
        seen: set[tuple[str, str, str]] = set()

        for func in functions:
            func_name = func.get("name", "")
            if not func_name:
                continue

            for call in func.get("calls", []):
                target = call.get("target", "")
                call_type = call.get("type", "").lower()
                if not target or not call_type:
                    continue

                mapping = _CALL_TYPE_MAP.get(call_type)
                if mapping is None:
                    continue

                predicate, subject_type, object_type = mapping

                # For PERFORMS, subject is the paragraph name
                if predicate == RelationType.PERFORMS:
                    subject_name = func_name.upper()
                else:
                    subject_name = program_name

                key = (predicate.value, subject_name, target.upper())
                if key in seen:
                    continue
                seen.add(key)

                triples.append(RawTriple(
                    subject_type=subject_type,
                    subject_name=subject_name,
                    predicate=predicate,
                    object_type=object_type,
                    object_name=target.upper(),
                    source_pass=source_pass,
                    source_artifact=file_name,
                ))

        # Includes -> PROGRAM INCLUDES COPYBOOK
        seen_includes: set[str] = set()
        for include in includes:
            if not include:
                continue
            inc_upper = include.upper()
            if inc_upper in seen_includes:
                continue
            seen_includes.add(inc_upper)
            triples.append(RawTriple(
                subject_type=EntityType.PROGRAM,
                subject_name=program_name,
                predicate=RelationType.INCLUDES,
                object_type=EntityType.COPYBOOK,
                object_name=inc_upper,
                source_pass=source_pass,
                source_artifact=file_name,
            ))

        logger.info(
            "Extracted %d triples from Citadel context for %s (%s)",
            len(triples),
            program_name,
            file_name,
        )
        return triples

    def extract_from_template(
        self,
        template: DocumentationTemplate,
        source_pass: str = "doc_enrichment",
    ) -> list[RawTriple]:
        """Extract triples from a DocumentationTemplate (.doc.json).

        Mines structured fields that the Scribe already populated
        (called_programs, copybooks_used, data_flow, paragraphs,
        inputs, outputs) and converts them to KG triples. This allows
        KG enrichment from existing documentation without re-running
        LLM calls.

        Args:
            template: The documentation template to extract from.
            source_pass: Pass identifier for provenance tracking.

        Returns:
            List of extracted RawTriple objects, deduplicated by
            (predicate, subject_name, object_name).
        """
        # Derive program name from header or return empty
        program_name: str | None = None
        source_artifact: str | None = None
        if template.header:
            program_name = template.header.program_id
            source_artifact = template.header.file_name
        if not program_name:
            logger.debug(
                "Template has no program_id in header, skipping extraction"
            )
            return []

        triples: list[RawTriple] = []
        seen: set[tuple[str, str, str]] = set()

        def _add(
            pred: RelationType,
            subj_type: EntityType,
            subj_name: str,
            obj_type: EntityType,
            obj_name: str,
        ) -> None:
            key = (pred.value, subj_name, obj_name)
            if key in seen:
                return
            seen.add(key)
            triples.append(RawTriple(
                subject_type=subj_type,
                subject_name=subj_name,
                predicate=pred,
                object_type=obj_type,
                object_name=obj_name,
                source_pass=source_pass,
                source_artifact=source_artifact,
            ))

        # called_programs[].program_name -> PROGRAM CALLS PROGRAM
        for cp in template.called_programs:
            if cp.program_name:
                _add(
                    RelationType.CALLS,
                    EntityType.PROGRAM, program_name,
                    EntityType.PROGRAM, cp.program_name,
                )

        # copybooks_used[].copybook_name -> PROGRAM INCLUDES COPYBOOK
        for cb in template.copybooks_used:
            if cb.copybook_name:
                _add(
                    RelationType.INCLUDES,
                    EntityType.PROGRAM, program_name,
                    EntityType.COPYBOOK, cb.copybook_name,
                )

        # paragraphs[].outgoing_calls -> varies by call_type
        for para in template.paragraphs:
            para_name = para.paragraph_name or ""
            for call in para.outgoing_calls:
                if not call.target:
                    continue
                ct = call.call_type.lower() if call.call_type else "performs"
                if ct == "performs":
                    _add(
                        RelationType.PERFORMS,
                        EntityType.PARAGRAPH, para_name,
                        EntityType.PARAGRAPH, call.target,
                    )
                elif ct == "calls":
                    _add(
                        RelationType.CALLS,
                        EntityType.PROGRAM, program_name,
                        EntityType.PROGRAM, call.target,
                    )
                elif ct == "includes":
                    _add(
                        RelationType.INCLUDES,
                        EntityType.PROGRAM, program_name,
                        EntityType.COPYBOOK, call.target,
                    )

        # data_flow.reads_from[].source -> PROGRAM READS DATASET
        for read in template.data_flow.reads_from:
            if read.source:
                _add(
                    RelationType.READS,
                    EntityType.PROGRAM, program_name,
                    EntityType.DATASET, read.source,
                )

        # data_flow.writes_to[].destination -> PROGRAM WRITES DATASET
        for write in template.data_flow.writes_to:
            if write.destination:
                _add(
                    RelationType.WRITES,
                    EntityType.PROGRAM, program_name,
                    EntityType.DATASET, write.destination,
                )

        # inputs[] / outputs[] -> type-dependent triples
        _FILE_TYPES = {IOType.FILE_SEQUENTIAL, IOType.FILE_VSAM}

        for inp in template.inputs:
            if not inp.name:
                continue
            if inp.io_type == IOType.DB2_TABLE:
                _add(
                    RelationType.QUERIES,
                    EntityType.PROGRAM, program_name,
                    EntityType.DB_TABLE, inp.name,
                )
            elif inp.io_type in _FILE_TYPES:
                _add(
                    RelationType.READS,
                    EntityType.PROGRAM, program_name,
                    EntityType.DATASET, inp.name,
                )

        for out in template.outputs:
            if not out.name:
                continue
            if out.io_type == IOType.DB2_TABLE:
                _add(
                    RelationType.MODIFIES,
                    EntityType.PROGRAM, program_name,
                    EntityType.DB_TABLE, out.name,
                )
            elif out.io_type in _FILE_TYPES:
                _add(
                    RelationType.WRITES,
                    EntityType.PROGRAM, program_name,
                    EntityType.DATASET, out.name,
                )

        logger.info(
            "Extracted %d triples from template for %s",
            len(triples),
            program_name,
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
