"""Regression tests: compare Cobalt output against existing ProLeap .ast files.

Note: ProLeap preprocessing expands copybooks (shifting line numbers) and
uses temp files (wrong program_id).  Cobalt parses the original source
directly, so it sees the real line numbers and finds additional paragraphs
in copybook-expanded code.  The tests below account for these known
differences with appropriate tolerances.
"""

from __future__ import annotations

import json
from pathlib import Path

import pytest

from cobalt.generator import parse_cobol_ast

EXAMPLE_DIR = Path(__file__).resolve().parent.parent.parent / "examples" / "app-authorization-ims-db2-mq"
CBL_DIR = EXAMPLE_DIR / "cbl"
COPYBOOK_DIRS = [
    str(EXAMPLE_DIR / "cpy"),
    str(EXAMPLE_DIR / "dcl"),
    str(EXAMPLE_DIR / "cpy-bms"),
]

# Collect all .ast files
AST_FILES = sorted(CBL_DIR.glob("*.ast")) if CBL_DIR.exists() else []


def _load_proleap_ast(ast_path: Path) -> dict:
    return json.loads(ast_path.read_text(encoding="utf-8"))


def _corresponding_cbl(ast_path: Path) -> Path:
    return ast_path.with_suffix("")


@pytest.mark.skipif(
    not AST_FILES,
    reason="No .ast files found in examples/app-authorization-ims-db2-mq/cbl/",
)
class TestRegression:

    @pytest.fixture(params=AST_FILES, ids=[p.name for p in AST_FILES])
    def ast_pair(self, request):
        """Return (proleap_data, cobalt_data, cbl_path) for each .ast file."""
        ast_path: Path = request.param
        cbl_path = _corresponding_cbl(ast_path)
        if not cbl_path.exists():
            pytest.skip(f"Source file not found: {cbl_path}")

        proleap = _load_proleap_ast(ast_path)
        _, _, raw_json = parse_cobol_ast(cbl_path, copybook_dirs=COPYBOOK_DIRS)
        cobalt = json.loads(raw_json)
        return proleap, cobalt, cbl_path

    def test_paragraph_names_superset(self, ast_pair):
        """Cobalt should find at least all paragraphs that ProLeap found.

        Cobalt may find additional paragraphs because it resolves copybooks
        inline while ProLeap preprocessing sometimes loses paragraph
        boundaries.
        """
        proleap, cobalt, _ = ast_pair
        proleap_names = {p["name"] for p in proleap.get("paragraphs", [])}
        cobalt_names = {p["name"] for p in cobalt.get("paragraphs", [])}
        missing = proleap_names - cobalt_names
        assert not missing, (
            f"Cobalt missing paragraphs found by ProLeap: {missing}"
        )

    def test_line_ranges_ordered(self, ast_pair):
        """Cobalt paragraph line ranges should be in ascending order.

        We cannot compare line numbers directly with ProLeap because
        ProLeap preprocessing (copybook expansion) changes line numbers
        significantly.  Instead, verify that Cobalt's own ranges are
        internally consistent.
        """
        _, cobalt, _ = ast_pair
        paras = cobalt.get("paragraphs", [])
        for p in paras:
            assert p["line_start"] <= p["line_end"], (
                f"{p['name']}: line_start > line_end"
            )
        starts = [p["line_start"] for p in paras]
        assert starts == sorted(starts), "Paragraphs not in line order"

    def test_shared_paragraph_statement_types(self, ast_pair):
        """For paragraphs in both parsers, top-level statement types from
        ProLeap should be a subset of those from Cobalt (Cobalt may
        find additional statements that ProLeap's preprocessing lost).
        """
        proleap, cobalt, _ = ast_pair
        proleap_paras = {p["name"]: p for p in proleap.get("paragraphs", [])}
        cobalt_paras = {p["name"]: p for p in cobalt.get("paragraphs", [])}

        for name in proleap_paras:
            if name not in cobalt_paras:
                continue
            pp_types = [s["type"] for s in proleap_paras[name].get("statements", [])]
            cp_types = [s["type"] for s in cobalt_paras[name].get("statements", [])]
            # All ProLeap types should appear in Cobalt (in order, as a subsequence)
            pp_idx = 0
            for cp_type in cp_types:
                if pp_idx < len(pp_types) and cp_type == pp_types[pp_idx]:
                    pp_idx += 1
            assert pp_idx == len(pp_types), (
                f"{name}: ProLeap types not a subsequence of Cobalt types.\n"
                f"  ProLeap: {pp_types}\n"
                f"  Cobalt:  {cp_types}"
            )

    def test_nesting_depth_at_least_proleap(self, ast_pair):
        """Cobalt nesting depth should be >= ProLeap's per shared paragraph.

        Cobalt finds deeper nesting because it correctly parses nested
        control flow that ProLeap's preprocessing sometimes collapses.
        """
        proleap, cobalt, _ = ast_pair
        proleap_paras = {p["name"]: p for p in proleap.get("paragraphs", [])}
        cobalt_paras = {p["name"]: p for p in cobalt.get("paragraphs", [])}

        def _max_depth(stmts, depth=0):
            if not stmts:
                return depth
            return max(
                _max_depth(s.get("children", []), depth + 1) for s in stmts
            )

        for name in proleap_paras:
            if name not in cobalt_paras:
                continue
            pp_depth = _max_depth(proleap_paras[name].get("statements", []))
            cp_depth = _max_depth(cobalt_paras[name].get("statements", []))
            assert cp_depth >= pp_depth, (
                f"{name}: Cobalt depth {cp_depth} < ProLeap depth {pp_depth}"
            )

    def test_program_id_not_proleap_temp(self, ast_pair):
        """Program ID should NOT be a PROLEAP_PP_* temp name."""
        _, cobalt, _ = ast_pair
        assert not cobalt["program_id"].startswith("PROLEAP_PP_")

    def test_cobalt_finds_statements(self, ast_pair):
        """Cobalt should find at least as many total statements as ProLeap."""
        proleap, cobalt, _ = ast_pair
        proleap_paras = {p["name"]: p for p in proleap.get("paragraphs", [])}
        cobalt_paras = {p["name"]: p for p in cobalt.get("paragraphs", [])}

        def _count_stmts(stmts):
            count = len(stmts)
            for s in stmts:
                count += _count_stmts(s.get("children", []))
            return count

        for name in proleap_paras:
            if name not in cobalt_paras:
                continue
            pp_count = _count_stmts(proleap_paras[name].get("statements", []))
            cp_count = _count_stmts(cobalt_paras[name].get("statements", []))
            assert cp_count >= pp_count, (
                f"{name}: Cobalt found fewer statements ({cp_count}) "
                f"than ProLeap ({pp_count})"
            )
