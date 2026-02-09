"""Unit tests for TripleExtractor.

Tests:
- COBOL extraction: CALLS, INCLUDES, READS (files), PERFORMS, QUERIES, MODIFIES
- JCL extraction: CONTAINS_STEP, EXECUTES, DEFINES_INPUT, DEFINES_OUTPUT, INCLUDES
- DD disposition classification
- Edge cases: empty structures, missing program_id/job_name, deduplication
- Dispatch: extract() routes to correct method based on type
"""

from war_rig.knowledge_graph.extractors import TripleExtractor
from war_rig.knowledge_graph.models import EntityType, RelationType
from war_rig.preprocessors.base import (
    CallInfo,
    CopybookInfo,
    FileInfo,
    PerformInfo,
    SourceLocation,
    SQLStatementInfo,
)
from war_rig.preprocessors.cobol import COBOLStructure
from war_rig.preprocessors.jcl import JCLDDInfo, JCLStepInfo, JCLStructure


def _make_cobol_structure(
    program_id: str | None = "ACCT0100",
    file_name: str = "ACCT0100.cbl",
    calls: list[CallInfo] | None = None,
    copybooks: list[CopybookInfo] | None = None,
    files: list[FileInfo] | None = None,
    performs: list[PerformInfo] | None = None,
    sql_statements: list[SQLStatementInfo] | None = None,
) -> COBOLStructure:
    """Create a COBOLStructure with minimal boilerplate."""
    return COBOLStructure(
        file_name=file_name,
        program_id=program_id,
        line_count=100,
        calls=calls or [],
        copybooks=copybooks or [],
        files=files or [],
        performs=performs or [],
        sql_statements=sql_statements or [],
    )


def _make_jcl_structure(
    job_name: str | None = "ACCTJOB1",
    file_name: str = "ACCTJOB1.jcl",
    steps: list[JCLStepInfo] | None = None,
    dd_statements: list[JCLDDInfo] | None = None,
    include_members: list[str] | None = None,
) -> JCLStructure:
    """Create a JCLStructure with minimal boilerplate."""
    return JCLStructure(
        file_name=file_name,
        job_name=job_name,
        program_id=job_name,
        line_count=50,
        steps=steps or [],
        dd_statements=dd_statements or [],
        include_members=include_members or [],
    )


class TestExtractFromCobol:
    """Tests for COBOL triple extraction."""

    def test_calls_extraction(self):
        structure = _make_cobol_structure(
            calls=[
                CallInfo(program="ACCT0200", line=10),
                CallInfo(program="ACCT0300", line=20),
            ],
        )
        extractor = TripleExtractor()
        triples = extractor.extract_from_cobol(structure)

        calls_triples = [t for t in triples if t.predicate == RelationType.CALLS]
        assert len(calls_triples) == 2
        assert calls_triples[0].subject_type == EntityType.PROGRAM
        assert calls_triples[0].subject_name == "ACCT0100"
        assert calls_triples[0].object_type == EntityType.PROGRAM
        assert calls_triples[0].object_name == "ACCT0200"
        assert calls_triples[1].object_name == "ACCT0300"

    def test_includes_extraction(self):
        structure = _make_cobol_structure(
            copybooks=[
                CopybookInfo(name="ACCTCPY1", line=5),
                CopybookInfo(name="ACCTCPY2", line=15),
            ],
        )
        extractor = TripleExtractor()
        triples = extractor.extract_from_cobol(structure)

        includes = [t for t in triples if t.predicate == RelationType.INCLUDES]
        assert len(includes) == 2
        assert includes[0].subject_type == EntityType.PROGRAM
        assert includes[0].object_type == EntityType.COPYBOOK
        assert includes[0].object_name == "ACCTCPY1"

    def test_reads_from_files(self):
        structure = _make_cobol_structure(
            files=[FileInfo(name="MASTER-FILE", line=8)],
        )
        extractor = TripleExtractor()
        triples = extractor.extract_from_cobol(structure)

        reads = [t for t in triples if t.predicate == RelationType.READS]
        assert len(reads) == 1
        assert reads[0].subject_type == EntityType.PROGRAM
        assert reads[0].object_type == EntityType.DATASET
        assert reads[0].object_name == "MASTER-FILE"

    def test_performs_extraction(self):
        structure = _make_cobol_structure(
            performs=[
                PerformInfo(
                    from_paragraph="1000-MAIN",
                    to_paragraph="2000-PROCESS",
                    line=30,
                ),
            ],
        )
        extractor = TripleExtractor()
        triples = extractor.extract_from_cobol(structure)

        performs = [t for t in triples if t.predicate == RelationType.PERFORMS]
        assert len(performs) == 1
        assert performs[0].subject_type == EntityType.PARAGRAPH
        assert performs[0].subject_name == "1000-MAIN"
        assert performs[0].object_type == EntityType.PARAGRAPH
        assert performs[0].object_name == "2000-PROCESS"

    def test_queries_extraction(self):
        structure = _make_cobol_structure(
            sql_statements=[
                SQLStatementInfo(operation="SELECT", table="ACCOUNT_TBL", line=50),
                SQLStatementInfo(operation="CURSOR", table="TRANS_TBL", line=55),
            ],
        )
        extractor = TripleExtractor()
        triples = extractor.extract_from_cobol(structure)

        queries = [t for t in triples if t.predicate == RelationType.QUERIES]
        assert len(queries) == 2
        assert queries[0].subject_type == EntityType.PROGRAM
        assert queries[0].object_type == EntityType.DB_TABLE
        tables = {t.object_name for t in queries}
        assert "ACCOUNT_TBL" in tables
        assert "TRANS_TBL" in tables

    def test_modifies_extraction(self):
        structure = _make_cobol_structure(
            sql_statements=[
                SQLStatementInfo(operation="INSERT", table="ACCOUNT_TBL", line=60),
                SQLStatementInfo(operation="UPDATE", table="TRANS_TBL", line=65),
                SQLStatementInfo(operation="DELETE", table="HIST_TBL", line=70),
            ],
        )
        extractor = TripleExtractor()
        triples = extractor.extract_from_cobol(structure)

        modifies = [t for t in triples if t.predicate == RelationType.MODIFIES]
        assert len(modifies) == 3
        tables = {t.object_name for t in modifies}
        assert tables == {"ACCOUNT_TBL", "TRANS_TBL", "HIST_TBL"}

    def test_sql_without_table_skipped(self):
        structure = _make_cobol_structure(
            sql_statements=[
                SQLStatementInfo(operation="SELECT", table=None, line=50),
            ],
        )
        extractor = TripleExtractor()
        triples = extractor.extract_from_cobol(structure)
        assert len(triples) == 0

    def test_unknown_sql_operation_skipped(self):
        structure = _make_cobol_structure(
            sql_statements=[
                SQLStatementInfo(operation="COMMIT", table="TBL", line=50),
            ],
        )
        extractor = TripleExtractor()
        triples = extractor.extract_from_cobol(structure)
        assert len(triples) == 0

    def test_source_provenance(self):
        structure = _make_cobol_structure(
            calls=[CallInfo(program="ACCT0200", line=10)],
        )
        extractor = TripleExtractor()
        triples = extractor.extract_from_cobol(structure, source_pass="pass_1")

        assert triples[0].source_pass == "pass_1"
        assert triples[0].source_artifact == "ACCT0100.cbl"

    def test_deduplication_calls(self):
        structure = _make_cobol_structure(
            calls=[
                CallInfo(program="ACCT0200", line=10),
                CallInfo(program="ACCT0200", line=20),
            ],
        )
        extractor = TripleExtractor()
        triples = extractor.extract_from_cobol(structure)

        calls_triples = [t for t in triples if t.predicate == RelationType.CALLS]
        assert len(calls_triples) == 1

    def test_deduplication_copybooks(self):
        structure = _make_cobol_structure(
            copybooks=[
                CopybookInfo(name="ACCTCPY1", line=5),
                CopybookInfo(name="ACCTCPY1", line=15),
            ],
        )
        extractor = TripleExtractor()
        triples = extractor.extract_from_cobol(structure)

        includes = [t for t in triples if t.predicate == RelationType.INCLUDES]
        assert len(includes) == 1

    def test_deduplication_files(self):
        structure = _make_cobol_structure(
            files=[
                FileInfo(name="MASTER-FILE", line=5),
                FileInfo(name="MASTER-FILE", line=15),
            ],
        )
        extractor = TripleExtractor()
        triples = extractor.extract_from_cobol(structure)

        reads = [t for t in triples if t.predicate == RelationType.READS]
        assert len(reads) == 1

    def test_deduplication_performs(self):
        structure = _make_cobol_structure(
            performs=[
                PerformInfo(from_paragraph="1000-MAIN", to_paragraph="2000-PROC", line=10),
                PerformInfo(from_paragraph="1000-MAIN", to_paragraph="2000-PROC", line=20),
            ],
        )
        extractor = TripleExtractor()
        triples = extractor.extract_from_cobol(structure)

        performs = [t for t in triples if t.predicate == RelationType.PERFORMS]
        assert len(performs) == 1

    def test_deduplication_sql(self):
        structure = _make_cobol_structure(
            sql_statements=[
                SQLStatementInfo(operation="SELECT", table="TBL", line=50),
                SQLStatementInfo(operation="SELECT", table="TBL", line=60),
            ],
        )
        extractor = TripleExtractor()
        triples = extractor.extract_from_cobol(structure)

        queries = [t for t in triples if t.predicate == RelationType.QUERIES]
        assert len(queries) == 1


class TestExtractFromCobolEdgeCases:
    """Edge case tests for COBOL extraction."""

    def test_missing_program_id_returns_empty(self):
        structure = _make_cobol_structure(
            program_id=None,
            calls=[CallInfo(program="ACCT0200", line=10)],
        )
        extractor = TripleExtractor()
        triples = extractor.extract_from_cobol(structure)
        assert triples == []

    def test_empty_structure_returns_empty(self):
        structure = _make_cobol_structure()
        extractor = TripleExtractor()
        triples = extractor.extract_from_cobol(structure)
        assert triples == []


class TestExtractFromJcl:
    """Tests for JCL triple extraction."""

    def test_contains_step(self):
        structure = _make_jcl_structure(
            steps=[
                JCLStepInfo(name="STEP01", program="ACCT0100", line=5),
                JCLStepInfo(name="STEP02", program="ACCT0200", line=10),
            ],
        )
        extractor = TripleExtractor()
        triples = extractor.extract_from_jcl(structure)

        contains = [t for t in triples if t.predicate == RelationType.CONTAINS_STEP]
        assert len(contains) == 2
        assert contains[0].subject_type == EntityType.JCL_JOB
        assert contains[0].subject_name == "ACCTJOB1"
        assert contains[0].object_type == EntityType.JCL_STEP
        assert contains[0].object_name == "STEP01"

    def test_executes(self):
        structure = _make_jcl_structure(
            steps=[JCLStepInfo(name="STEP01", program="ACCT0100", line=5)],
        )
        extractor = TripleExtractor()
        triples = extractor.extract_from_jcl(structure)

        executes = [t for t in triples if t.predicate == RelationType.EXECUTES]
        assert len(executes) == 1
        assert executes[0].subject_type == EntityType.JCL_STEP
        assert executes[0].subject_name == "STEP01"
        assert executes[0].object_type == EntityType.PROGRAM
        assert executes[0].object_name == "ACCT0100"

    def test_step_without_program_no_executes(self):
        structure = _make_jcl_structure(
            steps=[JCLStepInfo(name="STEP01", program=None, proc="MYPROC", line=5)],
        )
        extractor = TripleExtractor()
        triples = extractor.extract_from_jcl(structure)

        executes = [t for t in triples if t.predicate == RelationType.EXECUTES]
        assert len(executes) == 0
        # But CONTAINS_STEP should still be emitted
        contains = [t for t in triples if t.predicate == RelationType.CONTAINS_STEP]
        assert len(contains) == 1

    def test_defines_input_shr(self):
        structure = _make_jcl_structure(
            steps=[JCLStepInfo(name="STEP01", program="PGM1", line=5)],
            dd_statements=[
                JCLDDInfo(
                    step_name="STEP01",
                    dd_name="INPUT01",
                    dataset="ACCT.INPUT.FILE",
                    disposition="SHR",
                    line=10,
                ),
            ],
        )
        extractor = TripleExtractor()
        triples = extractor.extract_from_jcl(structure)

        defines_input = [t for t in triples if t.predicate == RelationType.DEFINES_INPUT]
        assert len(defines_input) == 1
        assert defines_input[0].subject_type == EntityType.JCL_STEP
        assert defines_input[0].subject_name == "STEP01"
        assert defines_input[0].object_type == EntityType.DATASET
        assert defines_input[0].object_name == "ACCT.INPUT.FILE"

    def test_defines_input_old(self):
        structure = _make_jcl_structure(
            dd_statements=[
                JCLDDInfo(
                    step_name="STEP01",
                    dd_name="INPUT01",
                    dataset="ACCT.INPUT.FILE",
                    disposition="OLD",
                    line=10,
                ),
            ],
        )
        extractor = TripleExtractor()
        triples = extractor.extract_from_jcl(structure)

        defines_input = [t for t in triples if t.predicate == RelationType.DEFINES_INPUT]
        assert len(defines_input) == 1

    def test_defines_output_new_catlg(self):
        structure = _make_jcl_structure(
            dd_statements=[
                JCLDDInfo(
                    step_name="STEP01",
                    dd_name="OUTPUT01",
                    dataset="ACCT.OUTPUT.FILE",
                    disposition="(NEW,CATLG,DELETE)",
                    line=10,
                ),
            ],
        )
        extractor = TripleExtractor()
        triples = extractor.extract_from_jcl(structure)

        defines_output = [
            t for t in triples if t.predicate == RelationType.DEFINES_OUTPUT
        ]
        assert len(defines_output) == 1
        assert defines_output[0].object_name == "ACCT.OUTPUT.FILE"

    def test_defines_output_mod(self):
        structure = _make_jcl_structure(
            dd_statements=[
                JCLDDInfo(
                    step_name="STEP01",
                    dd_name="APPEND01",
                    dataset="ACCT.LOG.FILE",
                    disposition="(MOD,CATLG,DELETE)",
                    line=10,
                ),
            ],
        )
        extractor = TripleExtractor()
        triples = extractor.extract_from_jcl(structure)

        defines_output = [
            t for t in triples if t.predicate == RelationType.DEFINES_OUTPUT
        ]
        assert len(defines_output) == 1

    def test_includes_members(self):
        structure = _make_jcl_structure(
            include_members=["MEMBER1", "MEMBER2"],
        )
        extractor = TripleExtractor()
        triples = extractor.extract_from_jcl(structure)

        includes = [t for t in triples if t.predicate == RelationType.INCLUDES]
        assert len(includes) == 2
        assert includes[0].subject_type == EntityType.JCL_JOB
        assert includes[0].subject_name == "ACCTJOB1"
        assert includes[0].object_type == EntityType.COPYBOOK
        names = {t.object_name for t in includes}
        assert names == {"MEMBER1", "MEMBER2"}

    def test_dd_without_dataset_skipped(self):
        structure = _make_jcl_structure(
            dd_statements=[
                JCLDDInfo(
                    step_name="STEP01",
                    dd_name="SYSIN",
                    dataset=None,
                    disposition=None,
                    line=10,
                ),
            ],
        )
        extractor = TripleExtractor()
        triples = extractor.extract_from_jcl(structure)

        dd_triples = [
            t
            for t in triples
            if t.predicate in (RelationType.DEFINES_INPUT, RelationType.DEFINES_OUTPUT)
        ]
        assert len(dd_triples) == 0

    def test_sysout_dd_skipped(self):
        structure = _make_jcl_structure(
            dd_statements=[
                JCLDDInfo(
                    step_name="STEP01",
                    dd_name="SYSPRINT",
                    dataset="ACCT.PRINT",
                    sysout="*",
                    line=10,
                ),
            ],
        )
        extractor = TripleExtractor()
        triples = extractor.extract_from_jcl(structure)

        dd_triples = [
            t
            for t in triples
            if t.predicate in (RelationType.DEFINES_INPUT, RelationType.DEFINES_OUTPUT)
        ]
        assert len(dd_triples) == 0

    def test_dd_with_no_disposition_skipped(self):
        structure = _make_jcl_structure(
            dd_statements=[
                JCLDDInfo(
                    step_name="STEP01",
                    dd_name="INFILE",
                    dataset="ACCT.FILE",
                    disposition=None,
                    line=10,
                ),
            ],
        )
        extractor = TripleExtractor()
        triples = extractor.extract_from_jcl(structure)

        dd_triples = [
            t
            for t in triples
            if t.predicate in (RelationType.DEFINES_INPUT, RelationType.DEFINES_OUTPUT)
        ]
        assert len(dd_triples) == 0


class TestExtractFromJclEdgeCases:
    """Edge case tests for JCL extraction."""

    def test_missing_job_name_returns_empty(self):
        structure = _make_jcl_structure(
            job_name=None,
            steps=[JCLStepInfo(name="STEP01", program="PGM1", line=5)],
        )
        extractor = TripleExtractor()
        triples = extractor.extract_from_jcl(structure)
        assert triples == []

    def test_empty_structure_returns_empty(self):
        structure = _make_jcl_structure()
        extractor = TripleExtractor()
        triples = extractor.extract_from_jcl(structure)
        assert triples == []


class TestDDDispositionClassification:
    """Tests for _classify_dd_disposition."""

    def test_shr_is_input(self):
        extractor = TripleExtractor()
        assert extractor._classify_dd_disposition("SHR") == RelationType.DEFINES_INPUT

    def test_old_is_input(self):
        extractor = TripleExtractor()
        assert extractor._classify_dd_disposition("OLD") == RelationType.DEFINES_INPUT

    def test_new_catlg_is_output(self):
        extractor = TripleExtractor()
        result = extractor._classify_dd_disposition("(NEW,CATLG,DELETE)")
        assert result == RelationType.DEFINES_OUTPUT

    def test_mod_is_output(self):
        extractor = TripleExtractor()
        result = extractor._classify_dd_disposition("(MOD,CATLG)")
        assert result == RelationType.DEFINES_OUTPUT

    def test_none_returns_none(self):
        extractor = TripleExtractor()
        assert extractor._classify_dd_disposition(None) is None

    def test_empty_string_returns_none(self):
        extractor = TripleExtractor()
        assert extractor._classify_dd_disposition("") is None

    def test_parenthesized_shr_is_input(self):
        extractor = TripleExtractor()
        result = extractor._classify_dd_disposition("(SHR)")
        assert result == RelationType.DEFINES_INPUT

    def test_parenthesized_old_is_input(self):
        extractor = TripleExtractor()
        result = extractor._classify_dd_disposition("(OLD,DELETE)")
        assert result == RelationType.DEFINES_INPUT

    def test_case_insensitive(self):
        extractor = TripleExtractor()
        assert extractor._classify_dd_disposition("shr") == RelationType.DEFINES_INPUT
        assert extractor._classify_dd_disposition("old") == RelationType.DEFINES_INPUT
        result = extractor._classify_dd_disposition("(new,catlg)")
        assert result == RelationType.DEFINES_OUTPUT


class TestExtractDispatch:
    """Tests for extract() dispatch method."""

    def test_cobol_dispatch(self):
        structure = _make_cobol_structure(
            calls=[CallInfo(program="ACCT0200", line=10)],
        )
        extractor = TripleExtractor()
        triples = extractor.extract(structure, source_pass="pass_1")

        assert len(triples) == 1
        assert triples[0].predicate == RelationType.CALLS
        assert triples[0].source_pass == "pass_1"

    def test_jcl_dispatch(self):
        structure = _make_jcl_structure(
            steps=[JCLStepInfo(name="STEP01", program="PGM1", line=5)],
        )
        extractor = TripleExtractor()
        triples = extractor.extract(structure, source_pass="preprocess")

        contains = [t for t in triples if t.predicate == RelationType.CONTAINS_STEP]
        assert len(contains) == 1

    def test_unknown_type_returns_empty(self):
        from war_rig.preprocessors.base import PreprocessorResult
        from war_rig.models.templates import FileType

        result = PreprocessorResult(
            file_name="unknown.xyz",
            file_type=FileType.OTHER,
        )
        extractor = TripleExtractor()
        triples = extractor.extract(result)
        assert triples == []
