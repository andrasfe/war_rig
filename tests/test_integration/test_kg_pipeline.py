"""Integration tests for knowledge graph pipeline.

Tests the full flow: preprocess -> extract triples -> store -> query -> format context.
Uses real SQLiteGraphStore (with tmp_path) and real components.
"""

import pytest

from war_rig.knowledge_graph.context import ContextFormatter
from war_rig.knowledge_graph.extractors import TripleExtractor
from war_rig.knowledge_graph.models import EntityType, RawTriple, RelationType
from war_rig.knowledge_graph.parser import TripleOutputParser
from war_rig.knowledge_graph.sqlite_store import SQLiteGraphStore
from war_rig.preprocessors.base import (
    CallInfo,
    CopybookInfo,
    FileInfo,
    PerformInfo,
    SQLStatementInfo,
)
from war_rig.preprocessors.cobol import COBOLStructure
from war_rig.preprocessors.jcl import JCLDDInfo, JCLStepInfo, JCLStructure


def _make_cobol_structure(
    program_id: str = "ACCT0100",
    file_name: str = "ACCT0100.cbl",
    calls: list[CallInfo] | None = None,
    copybooks: list[CopybookInfo] | None = None,
    files: list[FileInfo] | None = None,
    performs: list[PerformInfo] | None = None,
    sql_statements: list[SQLStatementInfo] | None = None,
) -> COBOLStructure:
    """Create a COBOLStructure with sensible defaults."""
    return COBOLStructure(
        file_name=file_name,
        program_id=program_id,
        line_count=200,
        calls=calls or [],
        copybooks=copybooks or [],
        files=files or [],
        performs=performs or [],
        sql_statements=sql_statements or [],
    )


def _make_jcl_structure(
    job_name: str = "ACCTJOB1",
    file_name: str = "ACCTJOB1.jcl",
    steps: list[JCLStepInfo] | None = None,
    dd_statements: list[JCLDDInfo] | None = None,
    include_members: list[str] | None = None,
) -> JCLStructure:
    """Create a JCLStructure with sensible defaults."""
    return JCLStructure(
        file_name=file_name,
        job_name=job_name,
        program_id=job_name,
        line_count=80,
        steps=steps or [],
        dd_statements=dd_statements or [],
        include_members=include_members or [],
    )


@pytest.fixture()
async def store(tmp_path):
    """Create and initialize a SQLiteGraphStore for testing."""
    db_path = tmp_path / "test_pipeline.db"
    s = SQLiteGraphStore(db_path)
    await s.initialize()
    yield s
    await s.close()


class TestFullCobolPipeline:
    """Full COBOL pipeline: extract -> ingest -> store -> query -> format."""

    async def test_full_cobol_pipeline(self, store):
        """COBOL structure with calls, copybooks, performs, files, sql
        flows through extract -> ingest -> query neighborhood -> format context."""
        cobol = _make_cobol_structure(
            program_id="ACCT0100",
            file_name="ACCT0100.cbl",
            calls=[
                CallInfo(program="ACCT0200", line=50),
                CallInfo(program="ACCT0300", line=75),
            ],
            copybooks=[
                CopybookInfo(name="ACCTCPY1", line=10),
                CopybookInfo(name="ACCTCPY2", line=15),
            ],
            performs=[
                PerformInfo(
                    from_paragraph="1000-INIT",
                    to_paragraph="2000-PROCESS",
                    line=30,
                ),
            ],
            files=[
                FileInfo(name="ACCT-FILE", line=5),
            ],
            sql_statements=[
                SQLStatementInfo(operation="SELECT", table="ACCT_TABLE", line=60),
                SQLStatementInfo(operation="UPDATE", table="ACCT_TABLE", line=80),
            ],
        )

        # Extract triples from preprocessor output
        extractor = TripleExtractor()
        raw_triples = extractor.extract_from_cobol(cobol, source_pass="preprocess")

        # Should extract: 2 CALLS + 2 INCLUDES + 1 READS + 1 PERFORMS + 1 QUERIES + 1 MODIFIES = 8
        assert len(raw_triples) == 8

        # Ingest raw triples into the store
        stored_triples = await store.ingest_raw_triples(raw_triples)
        assert len(stored_triples) == 8

        # Verify stored entities
        entities = await store.get_all_entities()
        entity_names = {e.name for e in entities}
        assert "ACCT0100" in entity_names
        assert "ACCT0200" in entity_names
        assert "ACCT0300" in entity_names
        assert "ACCTCPY1" in entity_names
        assert "ACCTCPY2" in entity_names
        assert "ACCT-FILE" in entity_names
        assert "ACCT_TABLE" in entity_names
        assert "1000-INIT" in entity_names
        assert "2000-PROCESS" in entity_names

        # Verify stored triples by predicate
        all_triples = await store.get_all_triples()
        predicates = [t.predicate for t in all_triples]
        assert predicates.count(RelationType.CALLS) == 2
        assert predicates.count(RelationType.INCLUDES) == 2
        assert predicates.count(RelationType.READS) == 1
        assert predicates.count(RelationType.PERFORMS) == 1
        assert predicates.count(RelationType.QUERIES) == 1
        assert predicates.count(RelationType.MODIFIES) == 1

        # Query neighborhood around ACCT0100
        neighborhood = await store.get_neighborhood(
            "ACCT0100", EntityType.PROGRAM, hops=2
        )
        assert neighborhood is not None
        assert neighborhood.target.name == "ACCT0100"
        # Should include the target and at least the directly connected entities
        neighborhood_names = {e.name for e in neighborhood.entities}
        assert "ACCT0200" in neighborhood_names
        assert "ACCT0300" in neighborhood_names
        assert "ACCTCPY1" in neighborhood_names

        # Format context
        formatter = ContextFormatter(max_tokens=500)
        context = formatter.format(neighborhood)
        assert context != ""
        assert "System Context" in context
        assert "knowledge graph" in context


class TestFullJclPipeline:
    """Full JCL pipeline: extract -> ingest -> store -> query -> format."""

    async def test_full_jcl_pipeline(self, store):
        """JCL structure with steps, programs, datasets, includes
        flows through the full pipeline."""
        jcl = _make_jcl_structure(
            job_name="ACCTJOB1",
            file_name="ACCTJOB1.jcl",
            steps=[
                JCLStepInfo(name="STEP01", program="ACCT0100", line=5),
                JCLStepInfo(name="STEP02", program="ACCT0200", line=15),
            ],
            dd_statements=[
                JCLDDInfo(
                    step_name="STEP01",
                    dd_name="INPUT1",
                    dataset="PROD.ACCT.MASTER",
                    disposition="SHR",
                    line=8,
                ),
                JCLDDInfo(
                    step_name="STEP01",
                    dd_name="OUTPUT1",
                    dataset="PROD.ACCT.REPORT",
                    disposition="(NEW,CATLG,DELETE)",
                    line=10,
                ),
                JCLDDInfo(
                    step_name="STEP02",
                    dd_name="SYSOUT",
                    dataset=None,
                    sysout="*",
                    line=20,
                ),
            ],
            include_members=["JCLINCL1"],
        )

        # Extract triples from JCL
        extractor = TripleExtractor()
        raw_triples = extractor.extract_from_jcl(jcl, source_pass="preprocess")

        # Expected: 2 CONTAINS_STEP + 2 EXECUTES + 1 DEFINES_INPUT + 1 DEFINES_OUTPUT + 1 INCLUDES = 7
        assert len(raw_triples) == 7

        # Ingest
        stored_triples = await store.ingest_raw_triples(raw_triples)
        assert len(stored_triples) == 7

        # Verify entities
        entities = await store.get_all_entities()
        entity_names = {e.name for e in entities}
        assert "ACCTJOB1" in entity_names
        assert "STEP01" in entity_names
        assert "STEP02" in entity_names
        assert "ACCT0100" in entity_names
        assert "ACCT0200" in entity_names
        assert "PROD.ACCT.MASTER" in entity_names
        assert "PROD.ACCT.REPORT" in entity_names
        assert "JCLINCL1" in entity_names

        # Query neighborhood around the JCL job
        neighborhood = await store.get_neighborhood(
            "ACCTJOB1", EntityType.JCL_JOB, hops=2
        )
        assert neighborhood is not None
        assert neighborhood.target.name == "ACCTJOB1"
        # 2-hop should reach the programs via CONTAINS_STEP -> EXECUTES
        neighborhood_names = {e.name for e in neighborhood.entities}
        assert "STEP01" in neighborhood_names
        assert "ACCT0100" in neighborhood_names

        # Format context
        formatter = ContextFormatter(max_tokens=500)
        context = formatter.format(neighborhood)
        assert context != ""
        assert "System Context" in context


class TestScribeOutputIngestion:
    """Scribe output parsing -> ingestion -> verification."""

    async def test_scribe_output_ingestion(self, store):
        """Parse a fake Scribe output with a triples block, ingest, and verify."""
        scribe_output = (
            "# Documentation for ACCT0100\n\n"
            "This program processes account transactions.\n\n"
            "```triples\n"
            "PROGRAM:ACCT0100 | CALLS | PROGRAM:ACCT0200\n"
            "PROGRAM:ACCT0100 | READS | DATASET:ACCT.MASTER\n"
            "PROGRAM:ACCT0100 | WRITES | DATASET:ACCT.REPORT\n"
            "PROGRAM:ACCT0100 | INCLUDES | COPYBOOK:ACCTCPY1\n"
            "PROGRAM:ACCT0100 | QUERIES | DB_TABLE:ACCT_TBL\n"
            "```\n\n"
            "End of documentation."
        )

        parser = TripleOutputParser()
        raw_triples = parser.parse(
            scribe_output,
            source_pass="pass_1",
            source_artifact="ACCT0100.cbl",
        )

        assert len(raw_triples) == 5

        # Ingest into the store
        stored_triples = await store.ingest_raw_triples(raw_triples)
        assert len(stored_triples) == 5

        # Verify entities created
        entities = await store.get_all_entities()
        entity_names = {e.name for e in entities}
        assert "ACCT0100" in entity_names
        assert "ACCT0200" in entity_names
        assert "ACCT.MASTER" in entity_names
        assert "ACCT.REPORT" in entity_names
        assert "ACCTCPY1" in entity_names
        assert "ACCT_TBL" in entity_names

        # Verify triples are stored with correct provenance
        all_triples = await store.get_all_triples()
        assert len(all_triples) == 5
        for t in all_triples:
            assert t.source_pass == "pass_1"
            assert t.source_artifact == "ACCT0100.cbl"


class TestCrossModuleRelationships:
    """Cross-module relationship verification."""

    async def test_cross_module_relationships(self, store):
        """Seed graph with COBOL program ACCT0100 that calls ACCT0200,
        then add JCL job that executes ACCT0100.
        Verify 2-hop neighborhood from ACCT0100 includes the JCL context."""
        extractor = TripleExtractor()

        # Add COBOL triples for ACCT0100
        cobol = _make_cobol_structure(
            program_id="ACCT0100",
            calls=[CallInfo(program="ACCT0200", line=50)],
        )
        cobol_triples = extractor.extract_from_cobol(cobol)
        await store.ingest_raw_triples(cobol_triples)

        # Add JCL triples for a job that executes ACCT0100
        jcl = _make_jcl_structure(
            job_name="DAILYJOB",
            file_name="DAILYJOB.jcl",
            steps=[JCLStepInfo(name="STEP01", program="ACCT0100", line=5)],
        )
        jcl_triples = extractor.extract_from_jcl(jcl)
        await store.ingest_raw_triples(jcl_triples)

        # Get 2-hop neighborhood from ACCT0100
        neighborhood = await store.get_neighborhood(
            "ACCT0100", EntityType.PROGRAM, hops=2
        )
        assert neighborhood is not None
        neighborhood_names = {e.name for e in neighborhood.entities}

        # 1-hop: ACCT0200 (via CALLS), STEP01 (via EXECUTES inverse)
        assert "ACCT0200" in neighborhood_names
        assert "STEP01" in neighborhood_names

        # 2-hop: DAILYJOB (via STEP01 <- CONTAINS_STEP)
        assert "DAILYJOB" in neighborhood_names


class TestContextRespectsTokenBudget:
    """Context formatter token budget enforcement."""

    async def test_context_respects_token_budget(self, store):
        """Seed graph with many entities (30+ triples),
        verify formatted context fits within token budget."""
        # Create 35 triples: ACCT0100 calls 35 different programs
        raw_triples = []
        for i in range(35):
            raw_triples.append(
                RawTriple(
                    subject_type=EntityType.PROGRAM,
                    subject_name="ACCT0100",
                    predicate=RelationType.CALLS,
                    object_type=EntityType.PROGRAM,
                    object_name=f"SUBPROG{i:03d}",
                    source_pass="preprocess",
                    source_artifact="ACCT0100.cbl",
                )
            )

        await store.ingest_raw_triples(raw_triples)

        # Get neighborhood
        neighborhood = await store.get_neighborhood(
            "ACCT0100", EntityType.PROGRAM, hops=1
        )
        assert neighborhood is not None
        assert len(neighborhood.triples) == 35

        # Format with a small budget
        budget_tokens = 200
        formatter = ContextFormatter(max_tokens=budget_tokens)
        context = formatter.format(neighborhood)

        assert context != ""
        # Estimate tokens: ~4 chars per token
        estimated_tokens = len(context) // 4
        # The formatter should summarize to stay within budget
        # Allow some overhead from header/footer
        assert estimated_tokens < budget_tokens * 2, (
            f"Context too large: ~{estimated_tokens} tokens vs {budget_tokens} budget"
        )
