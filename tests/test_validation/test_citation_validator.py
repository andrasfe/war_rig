"""Tests for citation validation pass.

Tests the CitationValidator which cross-checks LLM-generated paragraph
citations against COBOLPreprocessor ground truth.
"""

from __future__ import annotations

import json
from pathlib import Path

import pytest

from war_rig.validation.citation_validator import (
    CitationIssueType,
    CitationValidator,
)

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

COBOL_SOURCE = """\
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-FLAG PIC X.
       01 WS-AMOUNT PIC 9(7)V99.
       PROCEDURE DIVISION.
       MAIN-PARA.
           PERFORM INIT-PARA
           PERFORM PROCESS-PARA
           STOP RUN.
       INIT-PARA.
           MOVE 'Y' TO WS-FLAG.
       PROCESS-PARA.
           ADD 1 TO WS-AMOUNT.
"""

# Line numbers (1-indexed):
# 1  - IDENTIFICATION DIVISION.
# 2  - PROGRAM-ID. TESTPROG.
# 3  - DATA DIVISION.
# 4  - WORKING-STORAGE SECTION.
# 5  - 01 WS-FLAG PIC X.
# 6  - 01 WS-AMOUNT PIC 9(7)V99.
# 7  - PROCEDURE DIVISION.
# 8  - MAIN-PARA.        -> start=8
# 9  -     PERFORM INIT-PARA
# 10 -     PERFORM PROCESS-PARA
# 11 -     STOP RUN.
# 12 - INIT-PARA.        -> start=12
# 13 -     MOVE 'Y' TO WS-FLAG.
# 14 - PROCESS-PARA.     -> start=14
# 15 -     ADD 1 TO WS-AMOUNT.


def _make_doc_json(
    paragraphs: list[dict],
    file_name: str = "TESTPROG.cbl",
) -> dict:
    """Build a minimal .doc.json structure."""
    return {
        "header": {
            "program_id": "TESTPROG",
            "file_name": file_name,
            "file_type": "COBOL",
        },
        "paragraphs": paragraphs,
    }


@pytest.fixture()
def setup_dirs(tmp_path: Path) -> tuple[Path, Path]:
    """Create input/output directories with test COBOL source."""
    input_dir = tmp_path / "input"
    output_dir = tmp_path / "output"
    input_dir.mkdir()
    output_dir.mkdir()

    # Write source
    (input_dir / "TESTPROG.cbl").write_text(COBOL_SOURCE)

    return input_dir, output_dir


# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------


def test_correct_citation_no_finding(setup_dirs: tuple[Path, Path]) -> None:
    """Citation matches ground truth → no findings."""
    input_dir, output_dir = setup_dirs

    doc = _make_doc_json([
        {"paragraph_name": "MAIN-PARA", "citation": [8, 11]},
        {"paragraph_name": "INIT-PARA", "citation": [12, 13]},
    ])
    (output_dir / "TESTPROG.cbl.doc.json").write_text(json.dumps(doc))

    validator = CitationValidator(input_dir, output_dir)
    result = validator.validate_file("TESTPROG.cbl")

    assert result.total_paragraphs == 2
    assert result.corrected_count == 0
    assert result.unresolvable_count == 0
    assert result.findings == []
    assert result.corrected is False


def test_wrong_range_detected(setup_dirs: tuple[Path, Path]) -> None:
    """Citation points to WORKING-STORAGE instead of PROCEDURE DIVISION → WRONG_RANGE."""
    input_dir, output_dir = setup_dirs

    # MAIN-PARA is at lines 8-11 but citation points to WORKING-STORAGE (4-6)
    doc = _make_doc_json([
        {"paragraph_name": "MAIN-PARA", "citation": [4, 6]},
    ])
    (output_dir / "TESTPROG.cbl.doc.json").write_text(json.dumps(doc))

    validator = CitationValidator(input_dir, output_dir)
    result = validator.validate_file("TESTPROG.cbl")

    assert result.corrected_count == 1
    assert result.corrected is True
    assert len(result.findings) == 1

    finding = result.findings[0]
    assert finding.issue_type == CitationIssueType.WRONG_RANGE
    assert finding.original_citation == (4, 6)
    assert finding.corrected_citation is not None
    assert finding.corrected_citation[0] == 8  # MAIN-PARA starts at line 8

    # Verify .doc.json was rewritten with correct citation
    reloaded = json.loads(
        (output_dir / "TESTPROG.cbl.doc.json").read_text()
    )
    assert reloaded["paragraphs"][0]["citation"][0] == 8


def test_missing_citation_detected(setup_dirs: tuple[Path, Path]) -> None:
    """No citation in doc → MISSING finding with corrected value."""
    input_dir, output_dir = setup_dirs

    doc = _make_doc_json([
        {"paragraph_name": "INIT-PARA", "citation": None},
    ])
    (output_dir / "TESTPROG.cbl.doc.json").write_text(json.dumps(doc))

    validator = CitationValidator(input_dir, output_dir)
    result = validator.validate_file("TESTPROG.cbl")

    assert result.corrected_count == 1
    assert result.corrected is True
    assert len(result.findings) == 1

    finding = result.findings[0]
    assert finding.issue_type == CitationIssueType.MISSING
    assert finding.original_citation is None
    assert finding.corrected_citation is not None
    assert finding.corrected_citation[0] == 12  # INIT-PARA starts at line 12


def test_unresolvable_paragraph(setup_dirs: tuple[Path, Path]) -> None:
    """Paragraph name not in source → UNRESOLVABLE."""
    input_dir, output_dir = setup_dirs

    doc = _make_doc_json([
        {"paragraph_name": "NONEXISTENT-PARA", "citation": [1, 5]},
    ])
    (output_dir / "TESTPROG.cbl.doc.json").write_text(json.dumps(doc))

    validator = CitationValidator(input_dir, output_dir)
    result = validator.validate_file("TESTPROG.cbl")

    assert result.unresolvable_count == 1
    assert result.corrected is False
    assert len(result.findings) == 1

    finding = result.findings[0]
    assert finding.issue_type == CitationIssueType.UNRESOLVABLE
    assert finding.corrected_citation is None


def test_case_insensitive_matching(setup_dirs: tuple[Path, Path]) -> None:
    """Uppercase vs mixed case paragraph names should match."""
    input_dir, output_dir = setup_dirs

    # Use lowercase paragraph name in the doc — should match MAIN-PARA
    doc = _make_doc_json([
        {"paragraph_name": "main-para", "citation": [8, 11]},
    ])
    (output_dir / "TESTPROG.cbl.doc.json").write_text(json.dumps(doc))

    validator = CitationValidator(input_dir, output_dir)
    result = validator.validate_file("TESTPROG.cbl")

    # Should find the paragraph and consider citation correct
    assert result.corrected_count == 0
    assert result.unresolvable_count == 0


def test_validate_all_skips_non_cobol(setup_dirs: tuple[Path, Path]) -> None:
    """Only processes .cbl files, skips .jcl etc."""
    input_dir, output_dir = setup_dirs

    # Create a JCL doc.json — should be skipped
    jcl_doc = {
        "header": {"file_name": "JOBCARD.jcl", "file_type": "JCL"},
        "paragraphs": [{"paragraph_name": "STEP1", "citation": [1, 5]}],
    }
    (output_dir / "JOBCARD.jcl.doc.json").write_text(json.dumps(jcl_doc))

    # Create a COBOL doc.json with no issues
    cobol_doc = _make_doc_json([
        {"paragraph_name": "MAIN-PARA", "citation": [8, 11]},
    ])
    (output_dir / "TESTPROG.cbl.doc.json").write_text(json.dumps(cobol_doc))

    validator = CitationValidator(input_dir, output_dir)
    results = validator.validate_all()

    # validate_all only returns results with findings
    assert all(r.file_name.lower().endswith((".cbl", ".cob")) for r in results)


def test_correct_and_resplit_updates_json(setup_dirs: tuple[Path, Path]) -> None:
    """Verify .doc.json is rewritten with correct citations when corrected."""
    input_dir, output_dir = setup_dirs

    # Wrong citation for PROCESS-PARA (should be line 14+)
    doc = _make_doc_json([
        {"paragraph_name": "PROCESS-PARA", "citation": [3, 6]},
    ])
    (output_dir / "TESTPROG.cbl.doc.json").write_text(json.dumps(doc))

    validator = CitationValidator(input_dir, output_dir)
    result = validator.validate_file("TESTPROG.cbl")

    assert result.corrected is True

    # Reload and check
    reloaded = json.loads(
        (output_dir / "TESTPROG.cbl.doc.json").read_text()
    )
    citation = reloaded["paragraphs"][0]["citation"]
    assert citation[0] == 14  # PROCESS-PARA starts at line 14


def test_empty_template_no_crash(setup_dirs: tuple[Path, Path]) -> None:
    """Template with no paragraphs → empty results, no crash."""
    input_dir, output_dir = setup_dirs

    doc = _make_doc_json([])
    (output_dir / "TESTPROG.cbl.doc.json").write_text(json.dumps(doc))

    validator = CitationValidator(input_dir, output_dir)
    result = validator.validate_file("TESTPROG.cbl")

    assert result.total_paragraphs == 0
    assert result.corrected_count == 0
    assert result.findings == []
    assert result.corrected is False


def test_zero_zero_citation_treated_as_missing(
    setup_dirs: tuple[Path, Path],
) -> None:
    """Citation [0, 0] should be treated as missing."""
    input_dir, output_dir = setup_dirs

    doc = _make_doc_json([
        {"paragraph_name": "MAIN-PARA", "citation": [0, 0]},
    ])
    (output_dir / "TESTPROG.cbl.doc.json").write_text(json.dumps(doc))

    validator = CitationValidator(input_dir, output_dir)
    result = validator.validate_file("TESTPROG.cbl")

    assert result.corrected_count == 1
    assert len(result.findings) == 1
    assert result.findings[0].issue_type == CitationIssueType.MISSING


def test_overlapping_citation_accepted(
    setup_dirs: tuple[Path, Path],
) -> None:
    """Citation that partially overlaps ground truth should be accepted."""
    input_dir, output_dir = setup_dirs

    # MAIN-PARA is at lines 8-11. Citation [7, 10] overlaps → acceptable.
    doc = _make_doc_json([
        {"paragraph_name": "MAIN-PARA", "citation": [7, 10]},
    ])
    (output_dir / "TESTPROG.cbl.doc.json").write_text(json.dumps(doc))

    validator = CitationValidator(input_dir, output_dir)
    result = validator.validate_file("TESTPROG.cbl")

    assert result.corrected_count == 0
    assert result.findings == []


def test_no_doc_json_returns_empty(setup_dirs: tuple[Path, Path]) -> None:
    """Missing .doc.json → empty result, no crash."""
    input_dir, output_dir = setup_dirs

    validator = CitationValidator(input_dir, output_dir)
    result = validator.validate_file("NONEXISTENT.cbl")

    assert result.total_paragraphs == 0
    assert result.findings == []
