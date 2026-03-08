"""Unit tests for cobalt.generator."""

from __future__ import annotations

import json
from pathlib import Path

import pytest

from citadel.cobol.source_reader import SourceLine
from citadel.cobol.syntax_tree import (
    ParagraphSyntaxTree,
    StatementType,
    SyntaxNode,
    build_file_ast,
)
from cobalt.generator import (
    _deserialize_paragraphs,
    _serialize_node,
    _serialize_to_json,
    build_ast_from_parsed,
    parse_cobol_ast,
)

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

FIXTURE_DIR = Path(__file__).resolve().parent.parent.parent / "citadel" / "tests" / "fixtures" / "samples"
CBPAUP0C = FIXTURE_DIR / "CBPAUP0C.cbl"


def _make_source_line(
    line_number: int,
    text: str,
    *,
    is_comment: bool = False,
    is_blank: bool = False,
    area_a: str = "",
) -> SourceLine:
    return SourceLine(
        line_number=line_number,
        indicator=" ",
        area_a=area_a,
        area_b=text,
        text=text,
        is_comment=is_comment,
        is_continuation=False,
        is_blank=is_blank,
        raw="",
    )


# ---------------------------------------------------------------------------
# Round-trip: serialize → JSON → deserialize → compare
# ---------------------------------------------------------------------------

class TestRoundTrip:
    def test_simple_tree_roundtrip(self):
        """ParagraphSyntaxTree → JSON → deserialize → trees match."""
        root = SyntaxNode(
            node_type=StatementType.PARAGRAPH,
            source_text="",
            line_start=10,
            line_end=15,
            children=[
                SyntaxNode(
                    node_type=StatementType.MOVE,
                    source_text="MOVE A TO B",
                    line_start=11,
                    line_end=11,
                    attributes={"source": "A", "targets": "B"},
                ),
                SyntaxNode(
                    node_type=StatementType.DISPLAY,
                    source_text="DISPLAY 'HELLO'",
                    line_start=12,
                    line_end=12,
                ),
            ],
            attributes={"name": "TEST-PARA"},
        )
        tree = ParagraphSyntaxTree(
            paragraph_name="TEST-PARA",
            root=root,
            statement_count=2,
            max_nesting_depth=0,
        )
        trees = {"TEST-PARA": tree}

        raw_json = _serialize_to_json(trees, "TESTPROG")
        data = json.loads(raw_json)

        assert data["program_id"] == "TESTPROG"
        assert len(data["paragraphs"]) == 1

        # Deserialize back
        deserialized = _deserialize_paragraphs(data)
        assert set(deserialized.keys()) == {"TEST-PARA"}

        dt = deserialized["TEST-PARA"]
        assert dt.paragraph_name == "TEST-PARA"
        assert dt.statement_count == 2
        assert len(dt.root.children) == 2
        assert dt.root.children[0].node_type == StatementType.MOVE
        assert dt.root.children[0].source_text == "MOVE A TO B"
        assert dt.root.children[1].node_type == StatementType.DISPLAY

    def test_nested_if_roundtrip(self):
        """Nested IF/ELSE structure survives round-trip."""
        inner_move = SyntaxNode(
            node_type=StatementType.MOVE,
            source_text="MOVE 1 TO X",
            line_start=22,
            line_end=22,
        )
        else_node = SyntaxNode(
            node_type=StatementType.ELSE,
            source_text="ELSE",
            line_start=23,
            line_end=24,
            children=[
                SyntaxNode(
                    node_type=StatementType.MOVE,
                    source_text="MOVE 2 TO X",
                    line_start=24,
                    line_end=24,
                ),
            ],
        )
        if_node = SyntaxNode(
            node_type=StatementType.IF,
            source_text="IF A = B",
            line_start=21,
            line_end=25,
            children=[inner_move, else_node],
            attributes={"condition": "A = B"},
        )
        root = SyntaxNode(
            node_type=StatementType.PARAGRAPH,
            source_text="",
            line_start=20,
            line_end=25,
            children=[if_node],
            attributes={"name": "PARA-1"},
        )
        tree = ParagraphSyntaxTree(
            paragraph_name="PARA-1",
            root=root,
            statement_count=4,
            max_nesting_depth=2,
        )
        trees = {"PARA-1": tree}

        raw_json = _serialize_to_json(trees, "PROG")
        data = json.loads(raw_json)
        deserialized = _deserialize_paragraphs(data)

        dt = deserialized["PARA-1"]
        assert len(dt.root.children) == 1
        if_child = dt.root.children[0]
        assert if_child.node_type == StatementType.IF
        assert len(if_child.children) == 2  # MOVE + ELSE
        assert if_child.children[1].node_type == StatementType.ELSE
        assert len(if_child.children[1].children) == 1


# ---------------------------------------------------------------------------
# JSON schema tests
# ---------------------------------------------------------------------------

class TestJsonSchema:
    def test_schema_fields(self):
        """Output JSON has the required top-level and paragraph fields."""
        root = SyntaxNode(
            node_type=StatementType.PARAGRAPH,
            source_text="",
            line_start=1,
            line_end=2,
            children=[],
            attributes={"name": "MAIN"},
        )
        tree = ParagraphSyntaxTree(
            paragraph_name="MAIN",
            root=root,
            statement_count=0,
            max_nesting_depth=0,
        )

        raw_json = _serialize_to_json({"MAIN": tree}, "MYPROG")
        data = json.loads(raw_json)

        assert "program_id" in data
        assert "paragraphs" in data
        para = data["paragraphs"][0]
        assert para["name"] == "MAIN"
        assert "line_start" in para
        assert "line_end" in para
        assert "statements" in para


# ---------------------------------------------------------------------------
# Serialization details
# ---------------------------------------------------------------------------

class TestSerializeNode:
    def test_leaf_node(self):
        node = SyntaxNode(
            node_type=StatementType.GOBACK,
            source_text="GOBACK",
            line_start=99,
            line_end=99,
        )
        d = _serialize_node(node)
        assert d["type"] == "GOBACK"
        assert d["text"] == "GOBACK"
        assert d["children"] == []

    def test_container_with_children(self):
        child = SyntaxNode(
            node_type=StatementType.MOVE,
            source_text="MOVE 1 TO X",
            line_start=51,
            line_end=51,
        )
        parent = SyntaxNode(
            node_type=StatementType.IF,
            source_text="IF X > 0",
            line_start=50,
            line_end=52,
            children=[child],
            attributes={"condition": "X > 0"},
        )
        d = _serialize_node(parent)
        assert d["type"] == "IF"
        assert len(d["children"]) == 1
        assert d["children"][0]["type"] == "MOVE"
        assert d["attributes"]["condition"] == "X > 0"


# ---------------------------------------------------------------------------
# preserve_newlines
# ---------------------------------------------------------------------------

class TestPreserveNewlines:
    def test_multiline_statement_has_newlines(self):
        """Multi-line non-EXEC statements should have \\n in text."""
        lines = [
            _make_source_line(10, "IF A = B"),
            _make_source_line(11, "AND C = D"),
            _make_source_line(12, "MOVE 1 TO X"),
            _make_source_line(13, "END-IF."),
        ]
        para_source = {"TEST-PARA": lines}
        trees = build_file_ast(para_source, None, preserve_newlines=True)
        tree = trees["TEST-PARA"]
        if_node = tree.root.children[0]
        assert "\n" in if_node.source_text

    def test_exec_block_stays_single_line(self):
        """EXEC blocks always join with spaces even with preserve_newlines."""
        lines = [
            _make_source_line(20, "EXEC SQL"),
            _make_source_line(21, "SELECT * FROM T"),
            _make_source_line(22, "END-EXEC."),
        ]
        para_source = {"SQL-PARA": lines}
        trees = build_file_ast(para_source, None, preserve_newlines=True)
        tree = trees["SQL-PARA"]
        exec_node = tree.root.children[0]
        assert "\n" not in exec_node.source_text
        assert "EXEC SQL" in exec_node.source_text

    def test_default_joins_with_spaces(self):
        """Without preserve_newlines, continuation uses spaces."""
        lines = [
            _make_source_line(10, "IF A = B"),
            _make_source_line(11, "AND C = D"),
            _make_source_line(12, "MOVE 1 TO X"),
            _make_source_line(13, "END-IF."),
        ]
        para_source = {"TEST-PARA": lines}
        trees = build_file_ast(para_source, None, preserve_newlines=False)
        tree = trees["TEST-PARA"]
        if_node = tree.root.children[0]
        assert "\n" not in if_node.source_text


# ---------------------------------------------------------------------------
# build_ast_from_parsed
# ---------------------------------------------------------------------------

class TestBuildAstFromParsed:
    def test_empty_paragraphs(self):
        """Empty paragraph dict produces empty results."""
        trees, text, raw_json = build_ast_from_parsed({}, None, "EMPTY")
        assert trees == {}
        assert text == ""
        data = json.loads(raw_json)
        assert data["program_id"] == "EMPTY"
        assert data["paragraphs"] == []

    def test_single_paragraph(self):
        lines = [
            _make_source_line(10, "MOVE 1 TO X."),
        ]
        trees, text, raw_json = build_ast_from_parsed(
            {"INIT": lines}, None, "PROG1",
        )
        assert "INIT" in trees
        data = json.loads(raw_json)
        assert data["program_id"] == "PROG1"
        assert len(data["paragraphs"]) == 1
        assert data["paragraphs"][0]["name"] == "INIT"


# ---------------------------------------------------------------------------
# End-to-end with fixture file
# ---------------------------------------------------------------------------

class TestEndToEnd:
    @pytest.mark.skipif(
        not CBPAUP0C.exists(),
        reason="Fixture file not available",
    )
    def test_parse_cbpaup0c(self):
        """Parse the CBPAUP0C fixture and verify basic structure."""
        trees, text, raw_json = parse_cobol_ast(CBPAUP0C)

        assert len(trees) > 0
        assert len(text) > 0

        data = json.loads(raw_json)
        assert data["program_id"] == "CBPAUP0C"
        assert len(data["paragraphs"]) > 0

        # Verify each paragraph has required fields
        for para in data["paragraphs"]:
            assert "name" in para
            assert "line_start" in para
            assert "line_end" in para
            assert "statements" in para

    @pytest.mark.skipif(
        not CBPAUP0C.exists(),
        reason="Fixture file not available",
    )
    def test_program_id_valid(self):
        """Program ID should be a valid COBOL identifier."""
        _, _, raw_json = parse_cobol_ast(CBPAUP0C)
        data = json.loads(raw_json)
        assert data["program_id"], "Empty program_id"
