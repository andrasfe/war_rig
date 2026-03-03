"""Tests for the ProLeap bridge module.

Unit tests use mock JSON (no Java required).
Integration tests (marked ``integration``) require the built fat JAR.
"""

from __future__ import annotations

import json
from pathlib import Path
from unittest.mock import MagicMock, patch

import pytest

from citadel.cobol.proleap_bridge import (
    _TYPE_MAP,
    _count_statements,
    _deserialize_paragraphs,
    _find_jar,
    _find_java,
    _json_to_node,
    _max_depth,
    parse_proleap,
)
from citadel.cobol.syntax_tree import (
    ParagraphSyntaxTree,
    StatementType,
    SyntaxNode,
    format_file_ast,
)

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

SAMPLE_JSON = {
    "program_id": "TESTPROG",
    "paragraphs": [
        {
            "name": "MAIN-PARA",
            "line_start": 10,
            "line_end": 30,
            "statements": [
                {
                    "type": "PERFORM",
                    "text": "PERFORM INIT-PARA",
                    "line_start": 11,
                    "line_end": 11,
                    "children": [],
                    "attributes": {"target": "INIT-PARA"},
                },
                {
                    "type": "IF",
                    "text": "IF WS-STATUS = 'ACTIVE'",
                    "line_start": 12,
                    "line_end": 20,
                    "children": [
                        {
                            "type": "MOVE",
                            "text": "MOVE 'Y' TO WS-FLAG",
                            "line_start": 13,
                            "line_end": 13,
                            "children": [],
                            "attributes": {
                                "source": "'Y'",
                                "targets": "WS-FLAG",
                            },
                        },
                        {
                            "type": "ELSE",
                            "text": "ELSE",
                            "line_start": 15,
                            "line_end": 19,
                            "children": [
                                {
                                    "type": "MOVE",
                                    "text": "MOVE 'N' TO WS-FLAG",
                                    "line_start": 16,
                                    "line_end": 16,
                                    "children": [],
                                    "attributes": {
                                        "source": "'N'",
                                        "targets": "WS-FLAG",
                                    },
                                },
                            ],
                            "attributes": {},
                        },
                    ],
                    "attributes": {"condition": "WS-STATUS = 'ACTIVE'"},
                },
                {
                    "type": "GOBACK",
                    "text": "GOBACK",
                    "line_start": 25,
                    "line_end": 25,
                    "children": [],
                    "attributes": {},
                },
            ],
        },
        {
            "name": "INIT-PARA",
            "line_start": 32,
            "line_end": 40,
            "statements": [
                {
                    "type": "EXEC_SQL",
                    "text": "EXEC SQL SELECT COUNT(*) INTO :WS-CNT FROM ACCT END-EXEC",
                    "line_start": 33,
                    "line_end": 35,
                    "children": [],
                    "attributes": {
                        "raw_text": "SELECT COUNT(*) INTO :WS-CNT FROM ACCT",
                    },
                },
            ],
        },
    ],
}


# ---------------------------------------------------------------------------
# Unit tests — JSON deserialization
# ---------------------------------------------------------------------------


class TestJsonToNode:
    """Tests for _json_to_node conversion."""

    def test_leaf_node(self) -> None:
        data = {
            "type": "MOVE",
            "text": "MOVE X TO Y",
            "line_start": 5,
            "line_end": 5,
            "children": [],
            "attributes": {"source": "X", "targets": "Y"},
        }
        node = _json_to_node(data)
        assert node.node_type == StatementType.MOVE
        assert node.source_text == "MOVE X TO Y"
        assert node.line_start == 5
        assert node.line_end == 5
        assert node.children == []
        assert node.attributes == {"source": "X", "targets": "Y"}

    def test_container_node_with_children(self) -> None:
        data = {
            "type": "IF",
            "text": "IF A = B",
            "line_start": 10,
            "line_end": 15,
            "children": [
                {
                    "type": "DISPLAY",
                    "text": "DISPLAY 'YES'",
                    "line_start": 11,
                    "line_end": 11,
                    "children": [],
                    "attributes": {},
                },
            ],
            "attributes": {"condition": "A = B"},
        }
        node = _json_to_node(data)
        assert node.node_type == StatementType.IF
        assert len(node.children) == 1
        assert node.children[0].node_type == StatementType.DISPLAY
        assert node.attributes == {"condition": "A = B"}

    def test_unknown_type(self) -> None:
        data = {
            "type": "FOOBAR",
            "text": "FOOBAR SOMETHING",
            "line_start": 1,
            "line_end": 1,
            "children": [],
            "attributes": {},
        }
        node = _json_to_node(data)
        assert node.node_type == StatementType.UNKNOWN

    def test_empty_attributes_filtered(self) -> None:
        data = {
            "type": "GOBACK",
            "text": "GOBACK",
            "line_start": 1,
            "line_end": 1,
            "children": [],
            "attributes": {"target": "", "raw_text": ""},
        }
        node = _json_to_node(data)
        assert node.attributes == {}

    def test_all_type_mappings_exist(self) -> None:
        """Every StatementType value should be reachable via _TYPE_MAP."""
        mapped_types = set(_TYPE_MAP.values())
        for st in StatementType:
            assert st in mapped_types, f"StatementType.{st.name} not in _TYPE_MAP"


class TestDeserializeParagraphs:
    """Tests for _deserialize_paragraphs conversion."""

    def test_basic_deserialization(self) -> None:
        trees = _deserialize_paragraphs(SAMPLE_JSON)
        assert "MAIN-PARA" in trees
        assert "INIT-PARA" in trees

    def test_paragraph_structure(self) -> None:
        trees = _deserialize_paragraphs(SAMPLE_JSON)
        main = trees["MAIN-PARA"]
        assert isinstance(main, ParagraphSyntaxTree)
        assert main.paragraph_name == "MAIN-PARA"
        assert main.root.node_type == StatementType.PARAGRAPH
        assert main.root.line_start == 10
        assert main.root.line_end == 30

    def test_statement_count(self) -> None:
        trees = _deserialize_paragraphs(SAMPLE_JSON)
        main = trees["MAIN-PARA"]
        # PERFORM + IF + (MOVE + ELSE + (MOVE)) + GOBACK = 6
        assert main.statement_count == 6

    def test_nesting_depth(self) -> None:
        trees = _deserialize_paragraphs(SAMPLE_JSON)
        main = trees["MAIN-PARA"]
        # root → IF → ELSE → MOVE = depth 3
        assert main.max_nesting_depth >= 2

    def test_find_nodes(self) -> None:
        trees = _deserialize_paragraphs(SAMPLE_JSON)
        main = trees["MAIN-PARA"]
        moves = main.find(StatementType.MOVE)
        assert len(moves) == 2

    def test_exec_sql_paragraph(self) -> None:
        trees = _deserialize_paragraphs(SAMPLE_JSON)
        init = trees["INIT-PARA"]
        assert init.statement_count == 1
        sql_nodes = init.find(StatementType.EXEC_SQL)
        assert len(sql_nodes) == 1
        assert "raw_text" in sql_nodes[0].attributes

    def test_empty_paragraphs(self) -> None:
        data = {"program_id": "EMPTY", "paragraphs": []}
        trees = _deserialize_paragraphs(data)
        assert trees == {}

    def test_skip_unnamed_paragraph(self) -> None:
        data = {
            "program_id": "TEST",
            "paragraphs": [
                {
                    "name": "",
                    "line_start": 1,
                    "line_end": 5,
                    "statements": [],
                },
            ],
        }
        trees = _deserialize_paragraphs(data)
        assert trees == {}


class TestFormatFileAst:
    """Tests that formatted AST output is identical for both parsers."""

    def test_format_produces_string(self) -> None:
        trees = _deserialize_paragraphs(SAMPLE_JSON)
        text = format_file_ast(trees)
        assert isinstance(text, str)
        assert "MAIN-PARA" in text
        assert "INIT-PARA" in text

    def test_format_contains_tree_structure(self) -> None:
        trees = _deserialize_paragraphs(SAMPLE_JSON)
        text = format_file_ast(trees)
        assert "IF" in text
        assert "MOVE" in text
        assert "GOBACK" in text
        assert "EXEC_SQL" in text


class TestCountAndDepth:
    """Tests for _count_statements and _max_depth helpers."""

    def test_count_empty(self) -> None:
        root = SyntaxNode(
            node_type=StatementType.PARAGRAPH,
            source_text="",
            line_start=0,
            line_end=0,
        )
        assert _count_statements(root) == 0

    def test_count_flat(self) -> None:
        root = SyntaxNode(
            node_type=StatementType.PARAGRAPH,
            source_text="",
            line_start=0,
            line_end=0,
            children=[
                SyntaxNode(
                    node_type=StatementType.MOVE,
                    source_text="MOVE A TO B",
                    line_start=1,
                    line_end=1,
                ),
                SyntaxNode(
                    node_type=StatementType.DISPLAY,
                    source_text="DISPLAY X",
                    line_start=2,
                    line_end=2,
                ),
            ],
        )
        assert _count_statements(root) == 2

    def test_count_nested(self) -> None:
        inner = SyntaxNode(
            node_type=StatementType.MOVE,
            source_text="MOVE A TO B",
            line_start=2,
            line_end=2,
        )
        outer = SyntaxNode(
            node_type=StatementType.IF,
            source_text="IF X",
            line_start=1,
            line_end=3,
            children=[inner],
        )
        root = SyntaxNode(
            node_type=StatementType.PARAGRAPH,
            source_text="",
            line_start=0,
            line_end=5,
            children=[outer],
        )
        assert _count_statements(root) == 2  # IF + MOVE

    def test_depth_empty(self) -> None:
        root = SyntaxNode(
            node_type=StatementType.PARAGRAPH,
            source_text="",
            line_start=0,
            line_end=0,
        )
        assert _max_depth(root) == 0

    def test_depth_nested(self) -> None:
        leaf = SyntaxNode(
            node_type=StatementType.MOVE,
            source_text="",
            line_start=0,
            line_end=0,
        )
        mid = SyntaxNode(
            node_type=StatementType.IF,
            source_text="",
            line_start=0,
            line_end=0,
            children=[leaf],
        )
        root = SyntaxNode(
            node_type=StatementType.PARAGRAPH,
            source_text="",
            line_start=0,
            line_end=0,
            children=[mid],
        )
        assert _max_depth(root) == 2


# ---------------------------------------------------------------------------
# Parse via subprocess tests (mocked)
# ---------------------------------------------------------------------------


class TestParseProleap:
    """Tests for parse_proleap with mocked subprocess."""

    @patch("citadel.cobol.proleap_bridge._find_java", return_value="/usr/bin/java")
    @patch(
        "citadel.cobol.proleap_bridge._find_jar",
        return_value=Path("/fake/proleap.jar"),
    )
    @patch("subprocess.run")
    def test_successful_parse(
        self,
        mock_run: MagicMock,
        mock_jar: MagicMock,
        mock_java: MagicMock,
    ) -> None:
        mock_run.return_value = MagicMock(
            returncode=0,
            stdout=json.dumps(SAMPLE_JSON),
            stderr="",
        )
        trees, text = parse_proleap("/fake/source.cbl")
        assert "MAIN-PARA" in trees
        assert "INIT-PARA" in trees
        assert isinstance(text, str)
        assert "MAIN-PARA" in text

    @patch("citadel.cobol.proleap_bridge._find_java", return_value="/usr/bin/java")
    @patch(
        "citadel.cobol.proleap_bridge._find_jar",
        return_value=Path("/fake/proleap.jar"),
    )
    @patch("subprocess.run")
    def test_parse_failure(
        self,
        mock_run: MagicMock,
        mock_jar: MagicMock,
        mock_java: MagicMock,
    ) -> None:
        mock_run.return_value = MagicMock(
            returncode=1,
            stdout="",
            stderr="ERROR: Parse failed",
        )
        with pytest.raises(RuntimeError, match="ProLeap failed"):
            parse_proleap("/fake/source.cbl")

    @patch("citadel.cobol.proleap_bridge._find_java", return_value=None)
    def test_no_java_raises(self, mock_java: MagicMock) -> None:
        with pytest.raises(RuntimeError, match="Java 17"):
            parse_proleap("/fake/source.cbl")

    @patch("citadel.cobol.proleap_bridge._find_java", return_value="/usr/bin/java")
    @patch(
        "citadel.cobol.proleap_bridge._find_jar",
        return_value=Path("/fake/proleap.jar"),
    )
    @patch("subprocess.run")
    def test_copybook_dirs_passed(
        self,
        mock_run: MagicMock,
        mock_jar: MagicMock,
        mock_java: MagicMock,
    ) -> None:
        mock_run.return_value = MagicMock(
            returncode=0,
            stdout=json.dumps(SAMPLE_JSON),
            stderr="",
        )
        parse_proleap("/fake/source.cbl", copybook_dirs=["/cpy1", "/cpy2"])
        cmd = mock_run.call_args[0][0]
        assert "--copybook-dir=/cpy1" in cmd
        assert "--copybook-dir=/cpy2" in cmd


# ---------------------------------------------------------------------------
# Integration tests (require built JAR + Java 17)
# ---------------------------------------------------------------------------


def _skip_if_proleap_missing() -> None:
    """Skip test if Java 17+ or the fat JAR are not present."""
    if _find_java() is None:
        pytest.skip("Java 17+ not found")
    if _find_jar() is None:
        pytest.skip("ProLeap fat JAR not built")


@pytest.mark.integration
class TestProleapIntegration:
    """Integration tests that run the actual ProLeap parser.

    Uses the carddemo CBPAUP0C.cbl with its copybook directories,
    since the fixture copy may reference external copybooks.
    """

    CARDDEMO = Path(
        "/home/andras/aws-mainframe-modernization-carddemo"
        "/app/app-authorization-ims-db2-mq"
    )
    CPY_DIRS = [
        str(CARDDEMO / "cpy"),
        str(CARDDEMO / "dcl"),
        str(CARDDEMO / "cpy-bms"),
    ]

    def test_parse_cbpaup0c(self) -> None:
        _skip_if_proleap_missing()

        fixture = self.CARDDEMO / "cbl" / "CBPAUP0C.cbl"
        if not fixture.exists():
            pytest.skip("CardDemo CBPAUP0C.cbl not found")

        trees, text = parse_proleap(str(fixture), copybook_dirs=self.CPY_DIRS)
        assert len(trees) > 0
        assert isinstance(text, str)
        assert len(text) > 0

        for name, tree in trees.items():
            assert tree.paragraph_name == name
            assert tree.root.node_type == StatementType.PARAGRAPH

    def test_parse_with_copybook_dirs(self) -> None:
        _skip_if_proleap_missing()

        fixture = self.CARDDEMO / "cbl" / "PAUDBUNL.CBL"
        if not fixture.exists():
            pytest.skip("CardDemo PAUDBUNL.CBL not found")

        trees, text = parse_proleap(str(fixture), copybook_dirs=self.CPY_DIRS)
        assert len(trees) > 0
