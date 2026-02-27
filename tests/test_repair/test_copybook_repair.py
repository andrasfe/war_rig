"""Tests for war_rig.repair.copybook_repair."""

import json
from pathlib import Path

import pytest

from war_rig.repair.copybook_repair import (
    RepairCandidate,
    scan_for_missing_copybooks,
    verify_resolvability,
)


@pytest.fixture
def output_dir(tmp_path: Path) -> Path:
    d = tmp_path / "output"
    d.mkdir()
    return d


def _write_doc(output_dir: Path, name: str, data: dict) -> Path:
    p = output_dir / name
    p.write_text(json.dumps(data), encoding="utf-8")
    return p


class TestScanForMissingCopybooks:
    def test_finds_copybooks_not_found_field(self, output_dir: Path) -> None:
        _write_doc(output_dir, "PROG.doc.json", {
            "header": {"file_name": "PROG.cbl"},
            "copybooks_not_found": ["CPYBK1", "CPYBK2"],
        })
        result = scan_for_missing_copybooks(output_dir)
        assert len(result) == 1
        assert set(result[0].missing_copybooks) == {"CPYBK1", "CPYBK2"}

    def test_finds_inline_markers(self, output_dir: Path) -> None:
        _write_doc(output_dir, "PROG.doc.json", {
            "header": {"file_name": "PROG.cbl"},
            "some_field": "COPY WSCPYBBK -- NOT FOUND (skipped)",
        })
        result = scan_for_missing_copybooks(output_dir)
        assert len(result) == 1
        assert "WSCPYBBK" in result[0].missing_copybooks

    def test_no_missing_returns_empty(self, output_dir: Path) -> None:
        _write_doc(output_dir, "PROG.doc.json", {
            "header": {"file_name": "PROG.cbl"},
            "copybooks_not_found": [],
        })
        assert scan_for_missing_copybooks(output_dir) == []

    def test_deduplicates_across_sources(self, output_dir: Path) -> None:
        _write_doc(output_dir, "PROG.doc.json", {
            "header": {"file_name": "PROG.cbl"},
            "copybooks_not_found": ["CPYBK1"],
            "note": "COPY CPYBK1 -- NOT FOUND (skipped)",
        })
        result = scan_for_missing_copybooks(output_dir)
        assert result[0].missing_copybooks == ["CPYBK1"]

    def test_skips_invalid_json(self, output_dir: Path) -> None:
        (output_dir / "BAD.doc.json").write_text("not json")
        assert scan_for_missing_copybooks(output_dir) == []


class TestVerifyResolvability:
    def test_resolvable_when_file_exists(self, tmp_path: Path) -> None:
        cb_dir = tmp_path / "cpys"
        cb_dir.mkdir()
        (cb_dir / "CPYBK1.cpy").write_text("01 FIELD PIC X.")

        cand = RepairCandidate(
            doc_json_path=tmp_path / "PROG.doc.json",
            file_name="PROG.cbl",
            missing_copybooks=["CPYBK1"],
        )
        result = verify_resolvability([cand], [cb_dir])
        assert len(result) == 1
        assert result[0].now_resolvable == ["CPYBK1"]
        assert result[0].still_missing == []

    def test_not_resolvable_returns_empty(self, tmp_path: Path) -> None:
        cand = RepairCandidate(
            doc_json_path=tmp_path / "PROG.doc.json",
            file_name="PROG.cbl",
            missing_copybooks=["NONEXIST"],
        )
        result = verify_resolvability([cand], [tmp_path])
        assert result == []

    def test_case_insensitive_resolution(self, tmp_path: Path) -> None:
        cb_dir = tmp_path / "cpys"
        cb_dir.mkdir()
        (cb_dir / "wscpybbk.cpy").write_text("01 FIELD PIC X.")

        cand = RepairCandidate(
            doc_json_path=tmp_path / "PROG.doc.json",
            file_name="PROG.cbl",
            missing_copybooks=["WSCPYBBK"],
        )
        result = verify_resolvability([cand], [cb_dir])
        assert len(result) == 1
        assert result[0].now_resolvable == ["WSCPYBBK"]
