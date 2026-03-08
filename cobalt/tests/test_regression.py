"""Regression tests: compare Cobalt output against existing .ast reference files.

The .ast files in the examples directory serve as reference baselines.
These tests verify that Cobalt produces consistent, well-formed output
and that it matches or exceeds the reference data.
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

# Collect all .ast reference files
AST_FILES = sorted(CBL_DIR.glob("*.ast")) if CBL_DIR.exists() else []


def _load_reference_ast(ast_path: Path) -> dict:
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
        """Return (reference_data, cobalt_data, cbl_path) for each .ast file."""
        ast_path: Path = request.param
        cbl_path = _corresponding_cbl(ast_path)
        if not cbl_path.exists():
            pytest.skip(f"Source file not found: {cbl_path}")

        reference = _load_reference_ast(ast_path)
        _, _, raw_json = parse_cobol_ast(cbl_path, copybook_dirs=COPYBOOK_DIRS)
        cobalt = json.loads(raw_json)
        return reference, cobalt, cbl_path

    def test_paragraph_names_superset(self, ast_pair):
        """Cobalt should find at least all paragraphs in the reference."""
        reference, cobalt, _ = ast_pair
        ref_names = {p["name"] for p in reference.get("paragraphs", [])}
        cobalt_names = {p["name"] for p in cobalt.get("paragraphs", [])}
        missing = ref_names - cobalt_names
        assert not missing, (
            f"Cobalt missing paragraphs from reference: {missing}"
        )

    def test_line_ranges_ordered(self, ast_pair):
        """Cobalt paragraph line ranges should be in ascending order."""
        _, cobalt, _ = ast_pair
        paras = cobalt.get("paragraphs", [])
        for p in paras:
            assert p["line_start"] <= p["line_end"], (
                f"{p['name']}: line_start > line_end"
            )
        starts = [p["line_start"] for p in paras]
        assert starts == sorted(starts), "Paragraphs not in line order"

    def test_shared_paragraph_statement_types(self, ast_pair):
        """For paragraphs in both datasets, reference statement types
        should be a subsequence of Cobalt's (Cobalt may find more).
        """
        reference, cobalt, _ = ast_pair
        ref_paras = {p["name"]: p for p in reference.get("paragraphs", [])}
        cobalt_paras = {p["name"]: p for p in cobalt.get("paragraphs", [])}

        for name in ref_paras:
            if name not in cobalt_paras:
                continue
            ref_types = [s["type"] for s in ref_paras[name].get("statements", [])]
            cb_types = [s["type"] for s in cobalt_paras[name].get("statements", [])]
            ref_idx = 0
            for cb_type in cb_types:
                if ref_idx < len(ref_types) and cb_type == ref_types[ref_idx]:
                    ref_idx += 1
            assert ref_idx == len(ref_types), (
                f"{name}: Reference types not a subsequence of Cobalt.\n"
                f"  Reference: {ref_types}\n"
                f"  Cobalt:    {cb_types}"
            )

    def test_nesting_depth_at_least_reference(self, ast_pair):
        """Cobalt nesting depth should be >= reference per shared paragraph."""
        reference, cobalt, _ = ast_pair
        ref_paras = {p["name"]: p for p in reference.get("paragraphs", [])}
        cobalt_paras = {p["name"]: p for p in cobalt.get("paragraphs", [])}

        def _max_depth(stmts, depth=0):
            if not stmts:
                return depth
            return max(
                _max_depth(s.get("children", []), depth + 1) for s in stmts
            )

        for name in ref_paras:
            if name not in cobalt_paras:
                continue
            ref_depth = _max_depth(ref_paras[name].get("statements", []))
            cb_depth = _max_depth(cobalt_paras[name].get("statements", []))
            assert cb_depth >= ref_depth, (
                f"{name}: Cobalt depth {cb_depth} < reference depth {ref_depth}"
            )

    def test_program_id_valid(self, ast_pair):
        """Program ID should be a valid COBOL identifier."""
        _, cobalt, _ = ast_pair
        pid = cobalt["program_id"]
        assert pid, "Empty program_id"
        assert not pid.startswith(("PROLEAP_PP_", "TEMP_")), "Stale temp program ID"

    def test_cobalt_finds_statements(self, ast_pair):
        """Cobalt should find at least as many total statements as reference."""
        reference, cobalt, _ = ast_pair
        ref_paras = {p["name"]: p for p in reference.get("paragraphs", [])}
        cobalt_paras = {p["name"]: p for p in cobalt.get("paragraphs", [])}

        def _count_stmts(stmts):
            count = len(stmts)
            for s in stmts:
                count += _count_stmts(s.get("children", []))
            return count

        for name in ref_paras:
            if name not in cobalt_paras:
                continue
            ref_count = _count_stmts(ref_paras[name].get("statements", []))
            cb_count = _count_stmts(cobalt_paras[name].get("statements", []))
            assert cb_count >= ref_count, (
                f"{name}: Cobalt found fewer statements ({cb_count}) "
                f"than reference ({ref_count})"
            )
