"""Tests for AST-based dead code detection.

These tests verify the find_dead_code_ast() function using synthetic
ParagraphSyntaxTree structures, without any file I/O or parser invocations.
"""

from __future__ import annotations

from citadel.analysis.dead_code import find_dead_code_ast
from citadel.cobol.syntax_tree import (
    ParagraphSyntaxTree,
    StatementType,
    SyntaxNode,
)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _make_node(
    node_type: StatementType,
    text: str = "",
    line: int = 1,
    children: list[SyntaxNode] | None = None,
    **attrs: str,
) -> SyntaxNode:
    return SyntaxNode(
        node_type=node_type,
        source_text=text,
        line_start=line,
        line_end=line,
        children=children or [],
        attributes=dict(attrs),
    )


def _make_tree(
    name: str,
    children: list[SyntaxNode] | None = None,
    line_start: int = 1,
) -> ParagraphSyntaxTree:
    root = _make_node(
        StatementType.PARAGRAPH,
        text="",
        line=line_start,
        children=children or [],
        name=name,
    )
    return ParagraphSyntaxTree(
        paragraph_name=name,
        root=root,
        statement_count=len(children or []),
        max_nesting_depth=0,
    )


# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------


class TestEmptyInput:
    def test_empty_dict(self):
        assert find_dead_code_ast({}) == []

    def test_single_paragraph(self):
        """A single paragraph is the entry point and never dead."""
        asts = {"MAIN-PARA": _make_tree("MAIN-PARA")}
        assert find_dead_code_ast(asts) == []


class TestSimplePerform:
    def test_perform_referenced_not_dead(self):
        """Paragraphs referenced by PERFORM are not dead."""
        main = _make_tree("MAIN-PARA", [
            _make_node(StatementType.PERFORM, "PERFORM HELPER", target="HELPER"),
        ])
        helper = _make_tree("HELPER", line_start=50)

        asts = {"MAIN-PARA": main, "HELPER": helper}
        dead = find_dead_code_ast(asts, ["MAIN-PARA", "HELPER"])

        assert len(dead) == 0

    def test_unreferenced_is_dead(self):
        """A paragraph never referenced is dead code."""
        main = _make_tree("MAIN-PARA", [
            _make_node(StatementType.PERFORM, "PERFORM USED", target="USED"),
        ])
        used = _make_tree("USED", line_start=50)
        unused = _make_tree("UNUSED", line_start=100)

        asts = {"MAIN-PARA": main, "USED": used, "UNUSED": unused}
        dead = find_dead_code_ast(asts, ["MAIN-PARA", "USED", "UNUSED"])

        assert len(dead) == 1
        assert dead[0].name == "UNUSED"
        assert dead[0].artifact_type == "paragraph"


class TestPerformUntil:
    def test_perform_until_referenced(self):
        """PERFORM_UNTIL references are recognized."""
        main = _make_tree("MAIN-PARA", [
            _make_node(
                StatementType.PERFORM_UNTIL,
                "PERFORM READ-LOOP UNTIL EOF",
                target="READ-LOOP",
                condition="EOF",
            ),
        ])
        read_loop = _make_tree("READ-LOOP", line_start=50)

        asts = {"MAIN-PARA": main, "READ-LOOP": read_loop}
        dead = find_dead_code_ast(asts, ["MAIN-PARA", "READ-LOOP"])

        assert len(dead) == 0


class TestGoTo:
    def test_go_to_referenced(self):
        """GO_TO references are recognized."""
        main = _make_tree("MAIN-PARA", [
            _make_node(StatementType.GO_TO, "GO TO ERROR-EXIT", target="ERROR-EXIT"),
        ])
        error_exit = _make_tree("ERROR-EXIT", line_start=50)

        asts = {"MAIN-PARA": main, "ERROR-EXIT": error_exit}
        dead = find_dead_code_ast(asts, ["MAIN-PARA", "ERROR-EXIT"])

        assert len(dead) == 0


class TestPerformThru:
    def test_thru_endpoints_not_dead(self):
        """Both start and end of PERFORM THRU are referenced."""
        main = _make_tree("MAIN-PARA", [
            _make_node(
                StatementType.PERFORM_THRU,
                "PERFORM 1000-INIT THRU 1000-EXIT",
                target="1000-INIT",
                thru="1000-EXIT",
            ),
        ])
        init = _make_tree("1000-INIT", line_start=50)
        exit_para = _make_tree("1000-EXIT", line_start=60)

        asts = {"MAIN-PARA": main, "1000-INIT": init, "1000-EXIT": exit_para}
        order = ["MAIN-PARA", "1000-INIT", "1000-EXIT"]
        dead = find_dead_code_ast(asts, order)

        assert len(dead) == 0

    def test_thru_range_intermediate_not_dead(self):
        """Paragraphs between PERFORM THRU range endpoints are also referenced."""
        main = _make_tree("MAIN-PARA", [
            _make_node(
                StatementType.PERFORM_THRU,
                "PERFORM 1000-INIT THRU 1000-EXIT",
                target="1000-INIT",
                thru="1000-EXIT",
            ),
        ])
        init = _make_tree("1000-INIT", line_start=50)
        process = _make_tree("1000-PROCESS", line_start=55)
        exit_para = _make_tree("1000-EXIT", line_start=60)
        unused = _make_tree("9999-UNUSED", line_start=100)

        asts = {
            "MAIN-PARA": main,
            "1000-INIT": init,
            "1000-PROCESS": process,
            "1000-EXIT": exit_para,
            "9999-UNUSED": unused,
        }
        order = ["MAIN-PARA", "1000-INIT", "1000-PROCESS", "1000-EXIT", "9999-UNUSED"]
        dead = find_dead_code_ast(asts, order)

        dead_names = {d.name for d in dead}
        assert "1000-PROCESS" not in dead_names  # Inside THRU range
        assert "9999-UNUSED" in dead_names  # Outside all references


class TestFirstParagraphEntryPoint:
    def test_first_paragraph_excluded(self):
        """The first paragraph (index 0) is always excluded."""
        first = _make_tree("ENTRY-POINT", line_start=10)
        second = _make_tree("UNREACHABLE", line_start=50)

        asts = {"ENTRY-POINT": first, "UNREACHABLE": second}
        dead = find_dead_code_ast(asts, ["ENTRY-POINT", "UNREACHABLE"])

        dead_names = {d.name for d in dead}
        assert "ENTRY-POINT" not in dead_names
        assert "UNREACHABLE" in dead_names


class TestMultipleReferences:
    def test_paragraph_referenced_from_multiple(self):
        """A paragraph referenced from multiple sources is not dead."""
        main = _make_tree("MAIN-PARA", [
            _make_node(StatementType.PERFORM, target="HELPER"),
        ])
        other = _make_tree("OTHER", [
            _make_node(StatementType.PERFORM, target="HELPER"),
        ], line_start=50)
        helper = _make_tree("HELPER", line_start=100)

        asts = {"MAIN-PARA": main, "OTHER": other, "HELPER": helper}
        order = ["MAIN-PARA", "OTHER", "HELPER"]
        dead = find_dead_code_ast(asts, order)

        # OTHER is not referenced, so it's dead. HELPER is referenced.
        dead_names = {d.name for d in dead}
        assert "HELPER" not in dead_names
        assert "OTHER" in dead_names


class TestNestedReferences:
    def test_reference_inside_if_block(self):
        """References inside control flow containers are recognized."""
        if_node = _make_node(
            StatementType.IF,
            "IF WS-FLAG = 'Y'",
            children=[
                _make_node(StatementType.PERFORM, target="INNER-PARA"),
            ],
            condition="WS-FLAG = 'Y'",
        )
        main = _make_tree("MAIN-PARA", [if_node])
        inner = _make_tree("INNER-PARA", line_start=50)

        asts = {"MAIN-PARA": main, "INNER-PARA": inner}
        dead = find_dead_code_ast(asts, ["MAIN-PARA", "INNER-PARA"])

        assert len(dead) == 0


class TestNoOrder:
    def test_uses_dict_key_order_when_no_explicit_order(self):
        """When paragraph_order is None, dict key order is used."""
        main = _make_tree("MAIN-PARA", [
            _make_node(StatementType.PERFORM, target="USED"),
        ])
        used = _make_tree("USED", line_start=50)
        unused = _make_tree("UNUSED", line_start=100)

        asts = {"MAIN-PARA": main, "USED": used, "UNUSED": unused}
        dead = find_dead_code_ast(asts)

        assert len(dead) == 1
        assert dead[0].name == "UNUSED"


class TestDeadCodeItemFields:
    def test_item_has_correct_fields(self):
        """Dead code items have the expected field values."""
        main = _make_tree("MAIN-PARA")
        dead_para = _make_tree("DEAD-PARA", line_start=42)

        asts = {"MAIN-PARA": main, "DEAD-PARA": dead_para}
        dead = find_dead_code_ast(asts, ["MAIN-PARA", "DEAD-PARA"])

        assert len(dead) == 1
        item = dead[0]
        assert item.name == "DEAD-PARA"
        assert item.artifact_type == "paragraph"
        assert item.line == 42
        assert "PERFORM" in item.reason
        assert "GO TO" in item.reason
