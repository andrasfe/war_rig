"""Integration tests: ProLeap parser against CardDemo COBOL files.

Validates all .cbl files under the aws-mainframe-modernization-carddemo
app-authorization-ims-db2-mq/cbl/ directory.  Files referencing external
copybooks (IBM MQ, CICS) are handled by the wrapper's auto-stub mechanism.
"""

from __future__ import annotations

from pathlib import Path

import pytest

from citadel.cobol.proleap_bridge import (
    _find_jar,
    _find_java,
    parse_proleap,
)
from citadel.cobol.syntax_tree import StatementType

CARDDEMO_DIR = Path(
    "/home/andras/aws-mainframe-modernization-carddemo"
    "/app/app-authorization-ims-db2-mq"
)
CBL_DIR = CARDDEMO_DIR / "cbl"
COPYBOOK_DIRS = [
    str(CARDDEMO_DIR / "cpy"),
    str(CARDDEMO_DIR / "dcl"),
    str(CARDDEMO_DIR / "cpy-bms"),
    # Shared copybooks from the main app (COCOM01Y, COTTL01Y, etc.)
    str(CARDDEMO_DIR.parent / "cpy"),
]

ALL_FILES = [
    "CBPAUP0C.cbl",
    "COPAUA0C.cbl",
    "COPAUS0C.cbl",
    "COPAUS1C.cbl",
    "COPAUS2C.cbl",
    "DBUNLDGS.CBL",
    "PAUDBLOD.CBL",
    "PAUDBUNL.CBL",
]

# Files that need auto-stub for external copybooks (MQ, CICS)
STUB_FILES = [
    "COPAUA0C.cbl",
    "COPAUS0C.cbl",
    "COPAUS1C.cbl",
]


def _skip_if_not_available() -> None:
    if _find_java() is None:
        pytest.skip("Java 17+ not found")
    if _find_jar() is None:
        pytest.skip("ProLeap fat JAR not built")
    if not CBL_DIR.is_dir():
        pytest.skip(f"CardDemo directory not found: {CBL_DIR}")


@pytest.mark.integration
class TestCardDemoParseAll:
    """All 8 CardDemo COBOL files should parse via ProLeap."""

    @pytest.mark.parametrize("filename", ALL_FILES)
    def test_parse_succeeds(self, filename: str) -> None:
        _skip_if_not_available()
        source = CBL_DIR / filename
        if not source.exists():
            pytest.skip(f"File not found: {source}")

        trees, text = parse_proleap(str(source), copybook_dirs=COPYBOOK_DIRS)

        assert len(trees) > 0, f"No paragraphs found in {filename}"
        assert len(text) > 0, f"Empty AST text for {filename}"

        for name, tree in trees.items():
            assert tree.paragraph_name == name
            assert tree.root.node_type == StatementType.PARAGRAPH
            assert tree.root.line_start > 0

    def test_cbpaup0c_paragraphs(self) -> None:
        _skip_if_not_available()
        source = CBL_DIR / "CBPAUP0C.cbl"
        if not source.exists():
            pytest.skip("CBPAUP0C.cbl not found")

        trees, _ = parse_proleap(str(source), copybook_dirs=COPYBOOK_DIRS)
        names = set(trees.keys())

        # Should contain MAIN-PARA and the numbered exit paragraphs
        assert "MAIN-PARA" in names
        assert any("1000" in n for n in names)

    def test_cbpaup0c_has_perform_thru(self) -> None:
        _skip_if_not_available()
        source = CBL_DIR / "CBPAUP0C.cbl"
        if not source.exists():
            pytest.skip("CBPAUP0C.cbl not found")

        trees, _ = parse_proleap(str(source), copybook_dirs=COPYBOOK_DIRS)
        main = trees.get("MAIN-PARA")
        assert main is not None

        thru_nodes = main.find(StatementType.PERFORM_THRU)
        assert len(thru_nodes) > 0, "Expected PERFORM THRU in MAIN-PARA"
        assert any(
            "target" in n.attributes for n in thru_nodes
        ), "PERFORM_THRU should have target attribute"

    def test_dbunldgs_has_many_paragraphs(self) -> None:
        _skip_if_not_available()
        source = CBL_DIR / "DBUNLDGS.CBL"
        if not source.exists():
            pytest.skip("DBUNLDGS.CBL not found")

        trees, _ = parse_proleap(str(source), copybook_dirs=COPYBOOK_DIRS)
        assert len(trees) >= 10, f"Expected 10+ paragraphs, got {len(trees)}"

    def test_paudblod_has_call_statements(self) -> None:
        _skip_if_not_available()
        source = CBL_DIR / "PAUDBLOD.CBL"
        if not source.exists():
            pytest.skip("PAUDBLOD.CBL not found")

        trees, _ = parse_proleap(str(source), copybook_dirs=COPYBOOK_DIRS)
        all_nodes = []
        for tree in trees.values():
            all_nodes.extend(tree.walk())
        call_nodes = [n for n in all_nodes if n.node_type == StatementType.CALL]
        assert len(call_nodes) > 0, "Expected CALL statements in PAUDBLOD"


@pytest.mark.integration
class TestCardDemoAutoStub:
    """Files with external copybooks parse via auto-stub mechanism."""

    @pytest.mark.parametrize("filename", STUB_FILES)
    def test_stub_files_parse(self, filename: str) -> None:
        """Files needing MQ/CICS stubs should still parse successfully."""
        _skip_if_not_available()
        source = CBL_DIR / filename
        if not source.exists():
            pytest.skip(f"File not found: {source}")

        trees, text = parse_proleap(str(source), copybook_dirs=COPYBOOK_DIRS)
        assert len(trees) > 0, f"No paragraphs found in {filename}"

    def test_copaua0c_has_mq_related_paragraphs(self) -> None:
        """COPAUA0C is the MQ authorization program — should have paragraphs."""
        _skip_if_not_available()
        source = CBL_DIR / "COPAUA0C.cbl"
        if not source.exists():
            pytest.skip("COPAUA0C.cbl not found")

        trees, _ = parse_proleap(str(source), copybook_dirs=COPYBOOK_DIRS)
        assert "MAIN-PARA" in trees
        assert len(trees) >= 4

    def test_copaus0c_has_cics_paragraphs(self) -> None:
        """COPAUS0C is the CICS screen program — should have many paragraphs."""
        _skip_if_not_available()
        source = CBL_DIR / "COPAUS0C.cbl"
        if not source.exists():
            pytest.skip("COPAUS0C.cbl not found")

        trees, _ = parse_proleap(str(source), copybook_dirs=COPYBOOK_DIRS)
        assert "MAIN-PARA" in trees
        assert len(trees) >= 5
