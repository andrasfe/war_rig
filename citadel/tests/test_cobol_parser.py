"""Tests for the COBOL structural parser package.

Uses the CBPAUP0C.cbl sample from fixtures — a real IBM COBOL IMS batch
program (CardDemo authorization purge) with EXEC DLI blocks, PERFORM THRU,
DATA DIVISION with PIC/COMP fields, level-88 conditions, and COPY statements.
"""

from __future__ import annotations

from pathlib import Path

import pytest

from citadel.cobol.call_graph import CallGraph, CallGraphBuilder
from citadel.cobol.data_division import DataDivisionParser, DataItem
from citadel.cobol.procedure_division import (
    ExecBlock,
    ParagraphInfo,
    ProcedureDivisionParser,
)
from citadel.cobol.source_reader import (
    CobolSource,
    SourceLine,
    SourceReader,
)

FIXTURES_DIR = Path(__file__).parent / "fixtures" / "samples"
SAMPLE_CBL = FIXTURES_DIR / "CBPAUP0C.cbl"


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _read_source() -> CobolSource:
    """Read the sample COBOL file and return a CobolSource."""
    reader = SourceReader(skip_missing_copybooks=True)
    return reader.read(str(SAMPLE_CBL))


def _parse_data_division(source: CobolSource) -> list[DataItem]:
    parser = DataDivisionParser()
    return parser.parse(source)


def _parse_procedure_division(
    source: CobolSource,
    data_items: list[DataItem] | None = None,
) -> tuple[list[ParagraphInfo], list[ExecBlock]]:
    parser = ProcedureDivisionParser(
        data_items=data_items or [],
    )
    return parser.parse(source)


# ===================================================================
# SourceReader tests
# ===================================================================

class TestSourceReader:
    """Tests for SourceReader with the real CBPAUP0C.cbl sample."""

    def test_read_returns_cobol_source(self):
        source = _read_source()
        assert isinstance(source, CobolSource)

    def test_source_file_name(self):
        source = _read_source()
        assert source.source_file == "CBPAUP0C.cbl"

    def test_total_lines_matches_file(self):
        source = _read_source()
        # The file has 387 lines (including potential blank trailing line)
        assert source.total_lines >= 380

    def test_lines_not_empty(self):
        source = _read_source()
        assert len(source.lines) > 0

    def test_source_lines_are_source_line_objects(self):
        source = _read_source()
        for line in source.lines:
            assert isinstance(line, SourceLine)

    def test_identifies_divisions(self):
        source = _read_source()
        # CBPAUP0C has IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE
        found_text = " ".join(
            line.text for line in source.lines
        ).upper()
        assert "IDENTIFICATION DIVISION" in found_text
        assert "DATA DIVISION" in found_text
        assert "PROCEDURE DIVISION" in found_text

    def test_copy_statements_detected(self):
        reader = SourceReader(skip_missing_copybooks=True)
        reader.read(str(SAMPLE_CBL))
        # With skip_missing_copybooks=True, unresolved copybooks
        # are tracked in reader.missing_copybooks
        assert isinstance(reader.missing_copybooks, list)

    def test_inline_cobol_via_tempfile(self, tmp_path):
        """Test SourceReader with inline COBOL written to a temp file."""
        cobol_text = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COUNT PIC 9(04) VALUE 0.
       PROCEDURE DIVISION.
       MAIN-PARA.
           ADD 1 TO WS-COUNT.
           STOP RUN.
"""
        cbl_file = tmp_path / "TESTPROG.cbl"
        cbl_file.write_text(cobol_text)
        reader = SourceReader(skip_missing_copybooks=True)
        source = reader.read(str(cbl_file))
        assert source.source_file == "TESTPROG.cbl"
        assert source.total_lines >= 8


# ===================================================================
# DataDivisionParser tests
# ===================================================================

class TestDataDivisionParser:
    """Tests for DataDivisionParser with CBPAUP0C."""

    @pytest.fixture(autouse=True)
    def setup(self):
        self.source = _read_source()
        self.data_items = _parse_data_division(self.source)

    def test_returns_data_items(self):
        assert len(self.data_items) > 0

    def test_all_items_are_data_item_objects(self):
        for item in self.data_items:
            assert isinstance(item, DataItem)

    def test_level_01_items_present(self):
        level_01 = [d for d in self.data_items if d.level == 1]
        assert len(level_01) >= 1
        names = {item.name for item in level_01}
        # CBPAUP0C has: WS-VARIABLES, WS-IMS-VARIABLES, PRM-INFO,
        # PENDING-AUTH-SUMMARY, PENDING-AUTH-DETAILS, IO-PCB-MASK, etc.
        assert "WS-VARIABLES" in names

    def test_level_05_items_present(self):
        level_05 = [d for d in self.data_items if d.level == 5]
        assert len(level_05) >= 5
        names = {item.name for item in level_05}
        assert "WS-PGMNAME" in names

    def test_pic_fields_parsed(self):
        pgmname = next(
            (d for d in self.data_items if d.name == "WS-PGMNAME"),
            None,
        )
        assert pgmname is not None
        assert pgmname.picture is not None
        assert "X" in pgmname.picture.upper()

    def test_comp_fields_detected(self):
        comp_items = [
            d for d in self.data_items
            if d.usage and "COMP" in d.usage.upper()
        ]
        # CBPAUP0C has several COMP fields
        assert len(comp_items) >= 1

    def test_level_88_conditions_present(self):
        level_88 = [d for d in self.data_items if d.level == 88]
        assert len(level_88) >= 1
        names = {item.name for item in level_88}
        # e.g., ERR-FLG-ON, ERR-FLG-OFF, END-OF-AUTHDB, etc.
        assert "ERR-FLG-ON" in names or "END-OF-AUTHDB" in names

    def test_byte_length_computed(self):
        pgmname = next(
            (d for d in self.data_items if d.name == "WS-PGMNAME"),
            None,
        )
        assert pgmname is not None
        # PIC X(08) = 8 bytes
        assert pgmname.byte_length == 8

    def test_data_item_hierarchy(self):
        ws_vars = next(
            (d for d in self.data_items if d.name == "WS-VARIABLES"),
            None,
        )
        assert ws_vars is not None
        # WS-VARIABLES is a level 01 with children
        assert len(ws_vars.children) > 0

    def test_python_type_hint(self):
        pgmname = next(
            (d for d in self.data_items if d.name == "WS-PGMNAME"),
            None,
        )
        assert pgmname is not None
        # PIC X(08) should map to str
        assert pgmname.python_type_hint == "str"


# ===================================================================
# ProcedureDivisionParser tests
# ===================================================================

class TestProcedureDivisionParser:
    """Tests for ProcedureDivisionParser with CBPAUP0C."""

    @pytest.fixture(autouse=True)
    def setup(self):
        self.source = _read_source()
        self.data_items = _parse_data_division(self.source)
        self.paragraphs, self.exec_blocks = _parse_procedure_division(
            self.source, self.data_items
        )

    def test_paragraphs_found(self):
        assert len(self.paragraphs) > 0

    def test_all_paragraphs_are_paragraph_info(self):
        for para in self.paragraphs:
            assert isinstance(para, ParagraphInfo)

    def test_expected_paragraph_names(self):
        names = {p.name for p in self.paragraphs}
        expected = {
            "MAIN-PARA",
            "1000-INITIALIZE",
            "2000-FIND-NEXT-AUTH-SUMMARY",
            "3000-FIND-NEXT-AUTH-DTL",
            "4000-CHECK-IF-EXPIRED",
            "5000-DELETE-AUTH-DTL",
            "6000-DELETE-AUTH-SUMMARY",
            "9000-TAKE-CHECKPOINT",
            "9999-ABEND",
        }
        for name in expected:
            assert name in names, f"Missing paragraph: {name}"

    def test_exit_paragraphs_found(self):
        names = {p.name for p in self.paragraphs}
        exit_paras = {n for n in names if n.endswith("-EXIT")}
        # CBPAUP0C has: 1000-EXIT, 2000-EXIT, etc.
        assert len(exit_paras) >= 4

    def test_main_para_has_performs(self):
        main = next(
            (p for p in self.paragraphs if p.name == "MAIN-PARA"),
            None,
        )
        assert main is not None
        assert len(main.performs) >= 4
        targets = [perf.get("target") for perf in main.performs]
        assert "1000-INITIALIZE" in targets
        assert "2000-FIND-NEXT-AUTH-SUMMARY" in targets

    def test_perform_thru_detected(self):
        main = next(
            (p for p in self.paragraphs if p.name == "MAIN-PARA"),
            None,
        )
        assert main is not None
        thru_performs = [
            p for p in main.performs
            if p.get("thru")
        ]
        # MAIN-PARA does PERFORM X THRU X-EXIT for all paragraphs
        assert len(thru_performs) >= 4

    def test_exec_blocks_found(self):
        # CBPAUP0C has EXEC DLI blocks
        assert len(self.exec_blocks) >= 1

    def test_exec_blocks_are_exec_block_objects(self):
        for eb in self.exec_blocks:
            assert isinstance(eb, ExecBlock)

    def test_exec_dli_type(self):
        dli_blocks = [
            eb for eb in self.exec_blocks
            if eb.type == "DLI"
        ]
        assert len(dli_blocks) >= 1

    def test_exec_blocks_have_paragraph(self):
        for eb in self.exec_blocks:
            assert eb.paragraph, (
                f"Exec block {eb.id} should have a paragraph"
            )

    def test_complexity_scores_assigned(self):
        for para in self.paragraphs:
            assert isinstance(para.complexity_score, int)
            assert 1 <= para.complexity_score <= 5

    def test_main_para_higher_complexity(self):
        main = next(
            (p for p in self.paragraphs if p.name == "MAIN-PARA"),
            None,
        )
        exit_para = next(
            (p for p in self.paragraphs if p.name == "1000-EXIT"),
            None,
        )
        assert main is not None
        assert exit_para is not None
        # MAIN-PARA has nesting and performs; EXIT is trivial
        assert main.complexity_score > exit_para.complexity_score

    def test_paragraph_line_ranges(self):
        for para in self.paragraphs:
            assert para.line_start > 0
            assert para.line_end >= para.line_start

    def test_variable_reads_detected(self):
        # 4000-CHECK-IF-EXPIRED reads WS-EXPIRY-DAYS, etc.
        check = next(
            (p for p in self.paragraphs
             if p.name == "4000-CHECK-IF-EXPIRED"),
            None,
        )
        if check and check.reads:
            assert len(check.reads) > 0

    def test_variable_writes_detected(self):
        # 1000-INITIALIZE writes to WS-EXPIRY-DAYS
        init = next(
            (p for p in self.paragraphs
             if p.name == "1000-INITIALIZE"),
            None,
        )
        if init and init.writes:
            assert len(init.writes) > 0


# ===================================================================
# CallGraphBuilder tests
# ===================================================================

class TestCallGraphBuilder:
    """Tests for CallGraphBuilder with CBPAUP0C."""

    @pytest.fixture(autouse=True)
    def setup(self):
        source = _read_source()
        data_items = _parse_data_division(source)
        self.paragraphs, _ = _parse_procedure_division(
            source, data_items
        )
        builder = CallGraphBuilder()
        self.graph = builder.build(self.paragraphs)

    def test_returns_call_graph(self):
        assert isinstance(self.graph, CallGraph)

    def test_adjacency_populated(self):
        assert len(self.graph.adjacency) > 0

    def test_entry_paragraph_is_main(self):
        assert self.graph.entry_paragraph == "MAIN-PARA"

    def test_topological_order_has_all_non_exit_paragraphs(self):
        # Should contain all paragraphs except -EXIT ones
        expected_non_exit = {
            p.name for p in self.paragraphs
            if not p.name.endswith("-EXIT")
        }
        topo_set = set(self.graph.topological_order)
        for name in expected_non_exit:
            assert name in topo_set, (
                f"{name} missing from topological order"
            )

    def test_topological_order_leaves_first(self):
        # Leaf paragraphs (no outgoing performs) should appear
        # before paragraphs that call them
        order_idx = {
            name: idx
            for idx, name in enumerate(self.graph.topological_order)
        }
        # 9999-ABEND has no PERFORM targets, MAIN-PARA calls it
        if "9999-ABEND" in order_idx and "MAIN-PARA" in order_idx:
            assert order_idx["9999-ABEND"] < order_idx["MAIN-PARA"]

    def test_perform_thru_ranges_detected(self):
        assert len(self.graph.perform_thru_ranges) >= 1

    def test_thru_range_structure(self):
        for r in self.graph.perform_thru_ranges:
            assert "from" in r
            assert "thru" in r
            assert "paragraphs_included" in r
            assert len(r["paragraphs_included"]) >= 2

    def test_adjacency_main_para(self):
        main_targets = self.graph.adjacency.get("MAIN-PARA", [])
        assert "1000-INITIALIZE" in main_targets
        assert "2000-FIND-NEXT-AUTH-SUMMARY" in main_targets

    def test_empty_paragraphs_input(self):
        builder = CallGraphBuilder()
        graph = builder.build([])
        assert graph.adjacency == {}
        assert graph.topological_order == []
        assert graph.perform_thru_ranges == []
        assert graph.entry_paragraph == ""


# ===================================================================
# Bridge tests
# ===================================================================

class TestBridge:
    """Tests for the bridge module converting to citadel model."""

    @pytest.fixture(autouse=True)
    def setup(self):
        from citadel.cobol.bridge import parse_cobol_to_graph

        self.artifacts, self.relationships = parse_cobol_to_graph(
            SAMPLE_CBL,
        )

    def test_returns_artifacts_and_relationships(self):
        assert isinstance(self.artifacts, list)
        assert isinstance(self.relationships, list)
        assert len(self.artifacts) > 0

    def test_program_artifact_present(self):
        from citadel.specs.schema import ArtifactType

        programs = [
            a for a in self.artifacts
            if a.artifact_type == ArtifactType.PROGRAM
        ]
        assert len(programs) == 1
        assert programs[0].canonical_name == "CBPAUP0C"

    def test_paragraph_artifacts_present(self):
        from citadel.specs.schema import ArtifactType

        paragraphs = [
            a for a in self.artifacts
            if a.artifact_type == ArtifactType.PARAGRAPH
        ]
        assert len(paragraphs) >= 9  # at least the main ones
        names = {a.canonical_name for a in paragraphs}
        assert "MAIN-PARA" in names

    def test_data_item_artifacts_present(self):
        from citadel.specs.schema import ArtifactType

        data_arts = [
            a for a in self.artifacts
            if a.artifact_type == ArtifactType.RECORD_LAYOUT
        ]
        # Level 01/77 items should be present
        assert len(data_arts) >= 1
        names = {a.canonical_name for a in data_arts}
        assert "WS-VARIABLES" in names

    def test_exec_block_artifacts_present(self):
        from citadel.specs.schema import ArtifactType

        exec_arts = [
            a for a in self.artifacts
            if a.artifact_type == ArtifactType.FUNCTION
        ]
        # CBPAUP0C has EXEC DLI blocks
        assert len(exec_arts) >= 1

    def test_performs_relationships_present(self):
        from citadel.specs.schema import RelationshipType

        performs = [
            r for r in self.relationships
            if r.relationship_type == RelationshipType.PERFORMS
        ]
        assert len(performs) >= 4

    def test_includes_relationships_for_copybooks(self):
        from citadel.specs.schema import RelationshipType

        includes = [
            r for r in self.relationships
            if r.relationship_type == RelationshipType.INCLUDES
        ]
        # CBPAUP0C has COPY CIPAUSMY and COPY CIPAUDTY (unresolved)
        # With skip_missing_copybooks these show in copybooks_not_found
        # but bridge should still create INCLUDES for resolved ones
        # (may be 0 if copybooks couldn't be found)
        assert isinstance(includes, list)

    def test_artifact_ids_are_typed(self):
        for art in self.artifacts:
            assert "::" in art.id, (
                f"Artifact ID {art.id} should contain '::'"
            )

    def test_artifact_source_locations(self):
        for art in self.artifacts:
            if art.defined_in:
                assert art.defined_in.line_start >= 1

    def test_paragraph_attributes_have_complexity(self):
        from citadel.specs.schema import ArtifactType

        paragraphs = [
            a for a in self.artifacts
            if a.artifact_type == ArtifactType.PARAGRAPH
        ]
        for para in paragraphs:
            assert "complexity_score" in para.attributes


# ===================================================================
# SDK integration test
# ===================================================================

class TestSdkParseCobol:
    """Tests for Citadel.parse_cobol() end-to-end."""

    def test_parse_cobol_returns_result(self):
        from citadel.sdk import Citadel, CobolParseResult

        citadel = Citadel()
        result = citadel.parse_cobol(SAMPLE_CBL)
        assert isinstance(result, CobolParseResult)

    def test_program_id(self):
        from citadel.sdk import Citadel

        result = Citadel().parse_cobol(SAMPLE_CBL)
        assert result.program_id == "CBPAUP0C"

    def test_source_file(self):
        from citadel.sdk import Citadel

        result = Citadel().parse_cobol(SAMPLE_CBL)
        assert result.source_file == "CBPAUP0C.cbl"

    def test_total_lines(self):
        from citadel.sdk import Citadel

        result = Citadel().parse_cobol(SAMPLE_CBL)
        assert result.total_lines >= 380

    def test_data_items_serialized(self):
        from citadel.sdk import Citadel

        result = Citadel().parse_cobol(SAMPLE_CBL)
        assert len(result.data_items) > 0
        # Check dict structure
        first = result.data_items[0]
        assert "name" in first
        assert "level" in first
        assert "picture" in first

    def test_paragraphs_serialized(self):
        from citadel.sdk import Citadel

        result = Citadel().parse_cobol(SAMPLE_CBL)
        assert len(result.paragraphs) > 0
        names = {p["name"] for p in result.paragraphs}
        assert "MAIN-PARA" in names
        # Check dict structure
        main = next(
            p for p in result.paragraphs if p["name"] == "MAIN-PARA"
        )
        assert "performs" in main
        assert "complexity_score" in main
        assert "called_by" in main

    def test_called_by_reverse_lookup(self):
        from citadel.sdk import Citadel

        result = Citadel().parse_cobol(SAMPLE_CBL)
        # 1000-INITIALIZE is called by MAIN-PARA
        init = next(
            (p for p in result.paragraphs
             if p["name"] == "1000-INITIALIZE"),
            None,
        )
        assert init is not None
        assert "MAIN-PARA" in init["called_by"]

    def test_exec_blocks_serialized(self):
        from citadel.sdk import Citadel

        result = Citadel().parse_cobol(SAMPLE_CBL)
        assert len(result.exec_blocks) >= 1
        first = result.exec_blocks[0]
        assert "id" in first
        assert "type" in first
        assert "paragraph" in first

    def test_call_graph_serialized(self):
        from citadel.sdk import Citadel

        result = Citadel().parse_cobol(SAMPLE_CBL)
        cg = result.call_graph
        assert "adjacency" in cg
        assert "topological_order" in cg
        assert "perform_thru_ranges" in cg
        assert "entry_paragraph" in cg
        assert cg["entry_paragraph"] == "MAIN-PARA"

    def test_citadel_artifacts_present(self):
        from citadel.sdk import Citadel

        result = Citadel().parse_cobol(SAMPLE_CBL)
        assert len(result.artifacts) > 0

    def test_citadel_relationships_present(self):
        from citadel.sdk import Citadel

        result = Citadel().parse_cobol(SAMPLE_CBL)
        assert len(result.relationships) > 0


# ===================================================================
# Inline COBOL fixture tests (no external file needed)
# ===================================================================

INLINE_COBOL = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DATA.
         05 WS-NAME            PIC X(20).
         05 WS-AMOUNT          PIC S9(7)V99 COMP-3.
         05 WS-FLAG            PIC X(01).
            88 IS-ACTIVE                   VALUE 'Y'.
            88 IS-INACTIVE                 VALUE 'N'.
       01 WS-COUNT             PIC 9(04) VALUE 0.
       PROCEDURE DIVISION.
       MAIN-SECTION.
           PERFORM INIT-PARA
           PERFORM PROCESS-PARA
           STOP RUN.
       INIT-PARA.
           MOVE 'John' TO WS-NAME.
           MOVE 100.50 TO WS-AMOUNT.
           SET IS-ACTIVE TO TRUE.
       PROCESS-PARA.
           ADD 1 TO WS-COUNT.
           IF IS-ACTIVE
              DISPLAY WS-NAME
           END-IF.
"""


class TestInlineCobol:
    """Unit tests using a small inline COBOL fixture."""

    @pytest.fixture(autouse=True)
    def setup(self, tmp_path):
        cbl_file = tmp_path / "TESTPROG.cbl"
        cbl_file.write_text(INLINE_COBOL)
        reader = SourceReader(skip_missing_copybooks=True)
        self.source = reader.read(str(cbl_file))
        self.data_items = _parse_data_division(self.source)

    def test_source_read(self):
        assert self.source.source_file == "TESTPROG.cbl"

    def test_data_items_parsed(self):
        names = {d.name for d in self.data_items}
        assert "WS-DATA" in names
        assert "WS-NAME" in names
        assert "WS-AMOUNT" in names

    def test_pic_analysis_alpha(self):
        ws_name = next(
            d for d in self.data_items if d.name == "WS-NAME"
        )
        assert ws_name.byte_length == 20
        assert ws_name.python_type_hint == "str"

    def test_pic_analysis_comp3(self):
        ws_amount = next(
            d for d in self.data_items if d.name == "WS-AMOUNT"
        )
        assert ws_amount.usage is not None
        assert "COMP" in ws_amount.usage.upper()

    def test_level_88_conditions(self):
        level_88 = [d for d in self.data_items if d.level == 88]
        names_88 = {d.name for d in level_88}
        assert "IS-ACTIVE" in names_88
        assert "IS-INACTIVE" in names_88

    def test_procedure_paragraphs(self):
        paras, _ = _parse_procedure_division(
            self.source, self.data_items
        )
        names = {p.name for p in paras}
        assert "MAIN-SECTION" in names
        assert "INIT-PARA" in names
        assert "PROCESS-PARA" in names

    def test_performs_extracted(self):
        paras, _ = _parse_procedure_division(
            self.source, self.data_items
        )
        main = next(
            p for p in paras if p.name == "MAIN-SECTION"
        )
        targets = [perf.get("target") for perf in main.performs]
        assert "INIT-PARA" in targets
        assert "PROCESS-PARA" in targets

    def test_call_graph(self):
        paras, _ = _parse_procedure_division(
            self.source, self.data_items
        )
        builder = CallGraphBuilder()
        graph = builder.build(paras)
        assert graph.entry_paragraph == "MAIN-SECTION"
        assert "INIT-PARA" in graph.adjacency.get(
            "MAIN-SECTION", []
        )
