"""Static citation validation for COBOL paragraph line-range citations.

Cross-checks LLM-generated paragraph citations against deterministic
preprocessor ground truth. Corrects wrong citations and regenerates
paragraph split files when needed.

The Scribe sometimes hallucinates citation line numbers — for example,
pointing to WORKING-STORAGE data definitions instead of the actual
paragraph code in PROCEDURE DIVISION. The ``COBOLPreprocessor``
extracts correct paragraph boundaries via regex, giving us ground truth
to validate against.

Example:
    validator = CitationValidator(
        input_directory=Path("input/cbl"),
        output_directory=Path("output/cbl"),
    )
    results = validator.validate_all()
    for r in results:
        if r.corrected:
            print(f"{r.file_name}: corrected {r.corrected_count} citations")
"""

from __future__ import annotations

import json
import logging
from dataclasses import dataclass, field
from enum import Enum
from pathlib import Path

from war_rig.preprocessors.base import ParagraphInfo
from war_rig.preprocessors.cobol import COBOLPreprocessor

logger = logging.getLogger(__name__)


class CitationIssueType(str, Enum):
    """Classification of citation validation issues."""

    WRONG_RANGE = "WRONG_RANGE"  # Citation points to wrong code section
    MISSING = "MISSING"  # No citation provided
    UNRESOLVABLE = "UNRESOLVABLE"  # Paragraph name not found in source


@dataclass
class CitationFinding:
    """A single citation validation finding for one paragraph."""

    file_name: str
    paragraph_name: str
    issue_type: CitationIssueType
    original_citation: tuple[int, int] | None
    corrected_citation: tuple[int, int] | None
    description: str


@dataclass
class CitationValidationResult:
    """Validation result for a single file."""

    file_name: str
    total_paragraphs: int
    corrected_count: int
    unresolvable_count: int
    findings: list[CitationFinding] = field(default_factory=list)
    corrected: bool = False  # Whether .doc.json was modified


def _ranges_overlap(
    a_start: int,
    a_end: int,
    b_start: int,
    b_end: int,
) -> bool:
    """Check if two 1-indexed inclusive line ranges overlap."""
    return a_start <= b_end and b_start <= a_end


class CitationValidator:
    """Validates LLM paragraph citations against preprocessor ground truth.

    Compares the ``citation`` field on each paragraph in a ``.doc.json``
    template against the line ranges extracted by ``COBOLPreprocessor``.
    When a mismatch is detected the citation is corrected in place, the
    template is rewritten, and paragraph split files are regenerated.

    Args:
        input_directory: Directory containing COBOL source files.
        output_directory: Directory containing ``.doc.json`` output files.
    """

    COBOL_EXTENSIONS = {".cbl", ".cob", ".cobol"}

    def __init__(
        self,
        input_directory: Path,
        output_directory: Path,
    ) -> None:
        self.input_directory = input_directory
        self.output_directory = output_directory
        self._preprocessor = COBOLPreprocessor()

    def _is_cobol_file(self, file_name: str) -> bool:
        """Check if a file name has a COBOL extension."""
        return Path(file_name).suffix.lower() in self.COBOL_EXTENSIONS

    def _build_ground_truth(
        self,
        source_text: str,
        file_name: str,
    ) -> dict[str, ParagraphInfo]:
        """Run preprocessor and return paragraph info keyed by uppercase name."""
        structure = self._preprocessor.process(source_text, file_name)
        return {p.name.upper(): p for p in structure.paragraphs}

    def validate_file(self, file_name: str) -> CitationValidationResult:
        """Validate citations in a single file's documentation template.

        Args:
            file_name: Source file name (e.g. ``COPAUS1C.cbl``).

        Returns:
            CitationValidationResult with findings and correction status.
        """
        result = CitationValidationResult(
            file_name=file_name,
            total_paragraphs=0,
            corrected_count=0,
            unresolvable_count=0,
        )

        # Load .doc.json
        doc_json_path = self.output_directory / f"{file_name}.doc.json"
        if not doc_json_path.exists():
            logger.debug("No doc.json for %s, skipping", file_name)
            return result

        try:
            with open(doc_json_path, encoding="utf-8") as f:
                doc_data = json.load(f)
        except (json.JSONDecodeError, OSError) as exc:
            logger.warning("Failed to read %s: %s", doc_json_path, exc)
            return result

        paragraphs = doc_data.get("paragraphs", [])
        if not paragraphs:
            return result

        result.total_paragraphs = len(paragraphs)

        # Read source
        source_path = self._find_source(file_name)
        if source_path is None:
            logger.warning(
                "Source file %s not found in %s, skipping citation validation",
                file_name,
                self.input_directory,
            )
            return result

        source_text = source_path.read_text(encoding="utf-8", errors="replace")
        ground_truth = self._build_ground_truth(source_text, file_name)

        if not ground_truth:
            logger.debug("No paragraphs extracted by preprocessor for %s", file_name)
            return result

        # Cross-check each template paragraph
        needs_rewrite = False
        for para in paragraphs:
            para_name = para.get("paragraph_name")
            if not para_name:
                continue

            lookup_key = para_name.upper()
            gt_info = ground_truth.get(lookup_key)

            if gt_info is None:
                # Paragraph name not found in source
                result.unresolvable_count += 1
                result.findings.append(CitationFinding(
                    file_name=file_name,
                    paragraph_name=para_name,
                    issue_type=CitationIssueType.UNRESOLVABLE,
                    original_citation=self._extract_citation(para),
                    corrected_citation=None,
                    description=(
                        f"Paragraph '{para_name}' not found in preprocessor output"
                    ),
                ))
                continue

            gt_start = gt_info.location.start_line
            gt_end = gt_info.location.end_line or gt_start
            corrected = (gt_start, gt_end)

            citation = para.get("citation")

            if not citation or (
                isinstance(citation, (list, tuple))
                and len(citation) >= 2
                and citation[0] == 0
                and citation[1] == 0
            ):
                # Missing citation
                result.corrected_count += 1
                result.findings.append(CitationFinding(
                    file_name=file_name,
                    paragraph_name=para_name,
                    issue_type=CitationIssueType.MISSING,
                    original_citation=None,
                    corrected_citation=corrected,
                    description=(
                        f"Missing citation for '{para_name}', "
                        f"corrected to [{gt_start}, {gt_end}]"
                    ),
                ))
                para["citation"] = [gt_start, gt_end]
                needs_rewrite = True
                continue

            # Has citation — check if it overlaps ground truth
            if isinstance(citation, (list, tuple)) and len(citation) >= 2:
                try:
                    cite_start = int(citation[0])
                    cite_end = int(citation[1])
                except (TypeError, ValueError):
                    cite_start, cite_end = 0, 0

                if cite_start > 0 and cite_end > 0:
                    if _ranges_overlap(cite_start, cite_end, gt_start, gt_end):
                        # Citation overlaps ground truth — acceptable
                        continue

                    # Wrong range
                    result.corrected_count += 1
                    result.findings.append(CitationFinding(
                        file_name=file_name,
                        paragraph_name=para_name,
                        issue_type=CitationIssueType.WRONG_RANGE,
                        original_citation=(cite_start, cite_end),
                        corrected_citation=corrected,
                        description=(
                            f"Citation [{cite_start}, {cite_end}] for '{para_name}' "
                            f"doesn't overlap ground truth [{gt_start}, {gt_end}]"
                        ),
                    ))
                    para["citation"] = [gt_start, gt_end]
                    needs_rewrite = True
                else:
                    # Invalid values — treat as missing
                    result.corrected_count += 1
                    result.findings.append(CitationFinding(
                        file_name=file_name,
                        paragraph_name=para_name,
                        issue_type=CitationIssueType.MISSING,
                        original_citation=None,
                        corrected_citation=corrected,
                        description=(
                            f"Invalid citation for '{para_name}', "
                            f"corrected to [{gt_start}, {gt_end}]"
                        ),
                    ))
                    para["citation"] = [gt_start, gt_end]
                    needs_rewrite = True

        # Rewrite .doc.json and regenerate splits if corrections were made
        if needs_rewrite:
            result.corrected = True
            self._rewrite_doc_json(doc_json_path, doc_data)
            self._regenerate_splits(source_path, doc_json_path, file_name)

        return result

    def validate_all(self) -> list[CitationValidationResult]:
        """Validate citations for all COBOL files in the output directory.

        Returns:
            List of CitationValidationResult for each file processed.
        """
        results: list[CitationValidationResult] = []

        # Discover .doc.json files
        doc_files = sorted(self.output_directory.glob("*.doc.json"))

        for doc_path in doc_files:
            # Extract file_name: remove .doc.json suffix
            # e.g. "PROG.cbl.doc.json" -> "PROG.cbl"
            stem = doc_path.name
            if not stem.endswith(".doc.json"):
                continue
            file_name = stem[: -len(".doc.json")]

            if not self._is_cobol_file(file_name):
                continue

            result = self.validate_file(file_name)
            if result.findings:
                results.append(result)

        return results

    def _find_source(self, file_name: str) -> Path | None:
        """Locate source file in input directory, case-insensitively."""
        candidate = self.input_directory / file_name
        if candidate.exists():
            return candidate

        # Case-insensitive search
        target_lower = file_name.lower()
        for child in self.input_directory.iterdir():
            if child.is_file() and child.name.lower() == target_lower:
                return child

        # Recursive search
        for child in self.input_directory.rglob("*"):
            if child.is_file() and child.name.lower() == target_lower:
                return child

        return None

    @staticmethod
    def _extract_citation(para: dict) -> tuple[int, int] | None:
        """Extract citation tuple from paragraph dict."""
        citation = para.get("citation")
        if isinstance(citation, (list, tuple)) and len(citation) >= 2:
            try:
                return (int(citation[0]), int(citation[1]))
            except (TypeError, ValueError):
                return None
        return None

    @staticmethod
    def _rewrite_doc_json(doc_json_path: Path, doc_data: dict) -> None:
        """Atomically rewrite a .doc.json file."""
        tmp_path = doc_json_path.with_suffix(".tmp")
        with open(tmp_path, "w", encoding="utf-8") as f:
            json.dump(doc_data, f, indent=2, default=str)
        tmp_path.replace(doc_json_path)
        logger.info("Rewrote %s with corrected citations", doc_json_path)

    def _regenerate_splits(
        self,
        source_path: Path,
        doc_json_path: Path,
        file_name: str,
    ) -> None:
        """Regenerate paragraph split files after citation correction."""
        from war_rig.io.paragraph_splitter import split_and_link

        md_path = doc_json_path.parent / f"{file_name}.md"
        try:
            split_and_link(source_path, doc_json_path, md_path)
            logger.info("Regenerated paragraph splits for %s", file_name)
        except Exception as exc:
            logger.warning(
                "Failed to regenerate splits for %s: %s", file_name, exc
            )
