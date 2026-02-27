"""Repair documentation files affected by missing copybooks.

Scans ``*.doc.json`` files for ``copybooks_not_found`` entries and
for inline ``COPY XYZ -- NOT FOUND (skipped)`` markers, then
optionally re-runs the parse + Scribe pipeline for those files only.
"""

from __future__ import annotations

import json
import logging
import re
from dataclasses import dataclass, field
from pathlib import Path

logger = logging.getLogger(__name__)

# Pattern left in source by SourceReader when a copybook is missing
_INLINE_MARKER_RE = re.compile(r"COPY\s+(\S+)\s+--\s+NOT FOUND\s+\(skipped\)")


@dataclass
class RepairCandidate:
    """A documentation file that references missing copybooks."""

    doc_json_path: Path
    file_name: str
    missing_copybooks: list[str]
    now_resolvable: list[str] = field(default_factory=list)
    still_missing: list[str] = field(default_factory=list)


def scan_for_missing_copybooks(output_dir: Path) -> list[RepairCandidate]:
    """Walk ``*.doc.json`` files and collect those with missing copybooks.

    Checks the ``copybooks_not_found`` field first, then falls back to
    grepping for inline ``COPY XYZ -- NOT FOUND (skipped)`` markers in
    the JSON text (for older runs that predate the field).

    Args:
        output_dir: Root output directory to scan.

    Returns:
        List of RepairCandidates with ``missing_copybooks`` populated.
    """
    candidates: list[RepairCandidate] = []
    for doc_path in sorted(output_dir.rglob("*.doc.json")):
        try:
            text = doc_path.read_text(encoding="utf-8")
            data = json.loads(text)
        except (OSError, json.JSONDecodeError) as exc:
            logger.debug("Skipping %s: %s", doc_path, exc)
            continue

        missing: set[str] = set()

        # Primary: structured field
        for name in data.get("copybooks_not_found", []):
            missing.add(name)

        # Fallback: inline markers in any string value
        for m in _INLINE_MARKER_RE.finditer(text):
            missing.add(m.group(1))

        if missing:
            file_name = data.get("header", {}).get("file_name", doc_path.stem)
            candidates.append(
                RepairCandidate(
                    doc_json_path=doc_path,
                    file_name=file_name,
                    missing_copybooks=sorted(missing),
                )
            )

    return candidates


def verify_resolvability(
    candidates: list[RepairCandidate],
    copybook_dirs: list[Path],
) -> list[RepairCandidate]:
    """Check which missing copybooks can now be found.

    Splits each candidate's missing list into ``now_resolvable`` and
    ``still_missing``.

    Args:
        candidates: Output of :func:`scan_for_missing_copybooks`.
        copybook_dirs: Directories to search for copybooks.

    Returns:
        The same list with ``now_resolvable`` and ``still_missing`` populated.
        Only candidates with at least one resolvable copybook are included.
    """
    from citadel.cobol.source_reader import SourceReader

    reader = SourceReader(
        copybook_dirs=[str(d) for d in copybook_dirs],
        skip_missing_copybooks=True,
    )

    resolvable: list[RepairCandidate] = []
    for cand in candidates:
        for name in cand.missing_copybooks:
            try:
                reader._find_copybook(name)
                cand.now_resolvable.append(name)
            except FileNotFoundError:
                cand.still_missing.append(name)
        if cand.now_resolvable:
            resolvable.append(cand)

    return resolvable


def repair_file(
    candidate: RepairCandidate,
    input_dir: Path,
    output_dir: Path,
    copybook_dirs: list[Path],
) -> bool:
    """Re-parse a single file with copybook dirs and update its doc.json.

    Only re-runs the Citadel parse (no LLM call).  Updates the
    ``copybooks_not_found`` field and ``copybooks_resolved`` in-place.

    Args:
        candidate: The repair candidate to fix.
        input_dir: Root input directory.
        output_dir: Root output directory.
        copybook_dirs: Directories to search for copybooks.

    Returns:
        True if the file was successfully repaired.
    """
    from citadel.sdk import Citadel

    # Locate the source file
    source_path = _find_source(input_dir, candidate.file_name)
    if source_path is None:
        logger.warning(
            "Source file not found for %s, skipping repair",
            candidate.file_name,
        )
        return False

    try:
        citadel = Citadel()
        parse_result = citadel.parse_cobol(
            str(source_path), copybook_dirs=copybook_dirs,
        )
    except Exception as exc:
        logger.error("Re-parse failed for %s: %s", candidate.file_name, exc)
        return False

    # Update the doc.json
    try:
        data = json.loads(
            candidate.doc_json_path.read_text(encoding="utf-8")
        )
    except (OSError, json.JSONDecodeError) as exc:
        logger.error(
            "Cannot read %s: %s", candidate.doc_json_path, exc
        )
        return False

    data["copybooks_not_found"] = parse_result.copybooks_not_found
    data["copybooks_resolved"] = parse_result.copybooks_resolved

    try:
        candidate.doc_json_path.write_text(
            json.dumps(data, indent=2, default=str),
            encoding="utf-8",
        )
    except OSError as exc:
        logger.error(
            "Cannot write %s: %s", candidate.doc_json_path, exc
        )
        return False

    logger.info(
        "Repaired %s — resolved: %s, still missing: %s",
        candidate.file_name,
        parse_result.copybooks_resolved,
        parse_result.copybooks_not_found,
    )
    return True


def _find_source(input_dir: Path, file_name: str) -> Path | None:
    """Locate a source file by name, searching recursively."""
    # Try exact relative path first
    candidate = input_dir / file_name
    if candidate.is_file():
        return candidate

    # Recursive case-insensitive search
    name_upper = Path(file_name).name.upper()
    for entry in input_dir.rglob("*"):
        if entry.is_file() and entry.name.upper() == name_upper:
            return entry

    return None
