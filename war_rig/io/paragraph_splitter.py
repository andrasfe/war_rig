"""COBOL paragraph splitter and markdown linker.

Extracts individual COBOL paragraphs from source files using line-range
citations stored in ``.doc.json`` documentation templates, writing each
paragraph to its own file under a ``{PROGRAM}.cbl.d/`` directory.

Also provides markdown patching utilities that insert ``[Source: ...]``
links into generated ``.cbl.md`` documentation so readers can navigate
directly from paragraph documentation to the extracted source.

Typical standalone usage (batch mode on existing output)::

    from pathlib import Path
    from war_rig.io.paragraph_splitter import split_all_in_directory, patch_all_markdown_in_directory

    results = split_all_in_directory(
        source_dir=Path("input/cbl"),
        doc_dir=Path("output/cbl"),
    )
    patched = patch_all_markdown_in_directory(doc_dir=Path("output/cbl"))

Pipeline integration is handled by :func:`split_and_link` which
combines both steps for a single file and is called from
``ScribePool._save_template`` and ``DocumentationWriter.write_result``.
"""

from __future__ import annotations

import json
import logging
import os
import re
from pathlib import Path

logger = logging.getLogger(__name__)

# ---------------------------------------------------------------------------
# Filename sanitisation
# ---------------------------------------------------------------------------

# Characters that are unsafe in filenames on Windows/Linux/macOS.
_UNSAFE_FILENAME_RE = re.compile(r'[<>:"/\\|?*\x00-\x1f]')


def sanitize_filename(paragraph_name: str) -> str:
    """Convert a COBOL paragraph name into a filesystem-safe filename.

    COBOL paragraph names typically contain uppercase letters, digits, and
    hyphens (e.g. ``1000-INITIALIZE``, ``MAIN-PARA``).  This function
    replaces any characters that are invalid in filenames with underscores
    while preserving the original case.

    Args:
        paragraph_name: Raw paragraph name from the documentation template.

    Returns:
        A sanitised string safe for use as a filename component (without
        extension).
    """
    name = paragraph_name.strip()
    name = _UNSAFE_FILENAME_RE.sub("_", name)
    name = re.sub(r"_+", "_", name)
    return name


# ---------------------------------------------------------------------------
# COBOL paragraph discovery (fallback when citations are missing)
# ---------------------------------------------------------------------------

def _build_paragraph_index(
    source_lines: list[str],
    known_names: set[str],
) -> dict[str, int]:
    """Find known paragraph names in source lines.

    For each *known_names* entry, scans for a line containing that name
    followed by a period (the COBOL paragraph definition pattern).
    This is format-agnostic — works with any indentation, sequence
    numbering, or column layout.

    Returns a mapping of paragraph name (uppercase) -> 1-indexed line number.
    """
    # Build a case-insensitive lookup: search term -> canonical uppercase name
    remaining: dict[str, str] = {n.upper(): n.upper() for n in known_names}
    index: dict[str, int] = {}

    logger.debug(
        "Building paragraph index: searching %d lines for %d known names",
        len(source_lines), len(remaining),
    )

    for i, line in enumerate(source_lines, 1):
        upper_line = line.upper()
        for search_name, canonical in list(remaining.items()):
            # Check: NAME. appears in the line (paragraph definition)
            if search_name + "." in upper_line:
                index[canonical] = i
                logger.debug(
                    "  Found %r at line %d: %s",
                    canonical, i, line.rstrip()[:80],
                )
                del remaining[search_name]
                break

        if not remaining:
            break

    if remaining:
        logger.debug(
            "  Not found in source: %s", ", ".join(sorted(remaining)),
        )

    return index


def _resolve_citation(
    name: str,
    citation: object,
    source_lines: list[str],
    para_index: dict[str, int],
) -> tuple[int | None, int | None]:
    """Return (start, end) 1-indexed inclusive line range for a paragraph.

    Tries the explicit citation first; falls back to searching the source
    for the paragraph name when the citation is missing or invalid.
    """
    # --- Try explicit citation first ---
    if isinstance(citation, (list, tuple)) and len(citation) >= 2:
        try:
            start = int(citation[0])
            end = int(citation[1])
        except (TypeError, ValueError):
            logger.debug(
                "Paragraph %r: non-integer citation %r, falling back to source scan",
                name, citation,
            )
        else:
            # Skip obvious "not set" sentinel values
            if start == 0 and end == 0:
                logger.debug(
                    "Paragraph %r: citation [0, 0] (not set), falling back to source scan",
                    name,
                )
            # Accept 0-indexed citations: if start=0 with a real range, shift up
            elif start == 0 and end > 0:
                start = 1
                if end <= len(source_lines):
                    logger.debug(
                        "Paragraph %r: using 0-indexed citation [0, %d] -> [1, %d]",
                        name, end, end,
                    )
                    return start, end
            # 1-indexed citations (normal case)
            elif 1 <= start <= len(source_lines) and 1 <= end <= len(source_lines):
                logger.debug(
                    "Paragraph %r: using citation [%d, %d] (%d lines)",
                    name, start, end, end - start + 1,
                )
                return start, end
            else:
                logger.debug(
                    "Paragraph %r: citation [%d, %d] out of range "
                    "(file has %d lines), falling back to source scan",
                    name, start, end, len(source_lines),
                )
    else:
        logger.debug(
            "Paragraph %r: no valid citation (got %r), falling back to source scan",
            name, citation,
        )

    # --- Fallback: find paragraph by name in source ---
    start_line = para_index.get(name.upper())
    if start_line is None:
        logger.warning("Skipping paragraph %r: no citation and not found in source", name)
        return None, None

    # Find end: next paragraph definition or end of PROCEDURE DIVISION
    sorted_starts = sorted(para_index.values())
    idx = sorted_starts.index(start_line)
    if idx + 1 < len(sorted_starts):
        end_line = sorted_starts[idx + 1] - 1
    else:
        end_line = len(source_lines)

    # Trim trailing blank/comment lines
    while end_line > start_line:
        trailing = source_lines[end_line - 1].rstrip()
        if trailing and not trailing.startswith("      *"):
            break
        end_line -= 1

    logger.debug(
        "Paragraph %r: resolved from source scan -> [%d, %d]",
        name, start_line, end_line,
    )
    return start_line, end_line


# ---------------------------------------------------------------------------
# Single-file paragraph splitting
# ---------------------------------------------------------------------------


def split_paragraphs(
    source_path: Path,
    doc_json_path: Path,
    output_dir: Path | None = None,
) -> list[Path]:
    """Extract individual paragraph source files from a COBOL program.

    Reads the ``.doc.json`` template to obtain paragraph names and their
    ``citation`` line ranges, then slices the corresponding lines from the
    source ``.cbl`` file and writes each paragraph to its own file under
    *output_dir*.

    Args:
        source_path: Path to the COBOL source file (e.g. ``input/cbl/PROG.cbl``).
        doc_json_path: Path to the documentation JSON (e.g. ``output/cbl/PROG.cbl.doc.json``).
        output_dir: Directory to write split files into.  Defaults to
            ``{source_path}.d/`` (e.g. ``PROG.cbl.d/``) when *None*.

    Returns:
        List of :class:`~pathlib.Path` objects for each file that was
        successfully written.

    Notes:
        - Citations are ``[start_line, end_line]``, 1-indexed and inclusive.
        - Paragraphs with missing or invalid citations are skipped with a
          warning.
        - Dead-code paragraphs are still split (``is_dead_code`` is ignored).

    Raises:
        FileNotFoundError: If *source_path* does not exist.
    """
    if not source_path.exists():
        raise FileNotFoundError(f"Source file not found: {source_path}")

    logger.debug("Reading doc template: %s", doc_json_path)
    with open(doc_json_path, encoding="utf-8") as f:
        doc = json.load(f)

    paragraphs = doc.get("paragraphs", [])
    if not paragraphs:
        logger.info("No paragraphs found in %s", doc_json_path)
        return []

    source_lines = source_path.read_text(encoding="utf-8", errors="replace").splitlines(
        keepends=True,
    )

    logger.debug(
        "Processing %s: %d paragraphs in doc, %d source lines, source=%s",
        doc_json_path.name, len(paragraphs), len(source_lines), source_path,
    )

    if output_dir is None:
        output_dir = doc_json_path.parent / f"{source_path.name}.d"
    output_dir.mkdir(parents=True, exist_ok=True)

    # Build index of paragraph start lines for fallback when citations
    # are missing.  Only look for names we know about from the doc.json.
    known_names = {
        p["paragraph_name"].upper()
        for p in paragraphs
        if p.get("paragraph_name")
    }
    para_line_index = _build_paragraph_index(source_lines, known_names)

    created: list[Path] = []
    for para in paragraphs:
        name = para.get("paragraph_name")
        if not name:
            continue

        start, end = _resolve_citation(
            name, para.get("citation"), source_lines, para_line_index,
        )
        if start is None:
            continue

        extracted = source_lines[start - 1 : end]
        if not extracted:
            logger.warning(
                "Skipping paragraph %r: no lines extracted for range [%d, %d]",
                name, start, end,
            )
            continue

        out_file = output_dir / f"{sanitize_filename(name)}.cbl.md"
        body = "".join(extracted)
        if not body.endswith("\n"):
            body += "\n"
        out_file.write_text(
            f"```cobol\n{body}```\n",
            encoding="utf-8",
        )
        created.append(out_file)
        logger.debug("  Wrote %s (%d lines)", out_file.name, len(extracted))

    skipped = len(paragraphs) - len(created)
    logger.info(
        "Split %d/%d paragraphs from %s into %s%s",
        len(created), len(paragraphs), source_path.name, output_dir,
        f" ({skipped} skipped)" if skipped else "",
    )
    return created


# ---------------------------------------------------------------------------
# Markdown paragraph linking
# ---------------------------------------------------------------------------

# Matches paragraph headings in both writer.py and scribe_pool.py formats:
#   writer.py:      ### PARAGRAPH-NAME
#   scribe_pool.py: ### PARAGRAPH-NAME           (normal)
#                   ### ~~PARAGRAPH-NAME~~ (Dead Code)
_PARAGRAPH_HEADING_RE = re.compile(
    r"^### (?:~~)?([A-Za-z0-9_-]+?)(?:~~)?\s*(?:\(Dead Code\))?\s*$"
)

# Matches an existing source link line (for idempotency checking).
_SOURCE_LINK_RE = re.compile(r"^> \[Source:")


def patch_markdown_links(
    md_path: Path,
    split_dir: Path,
    relative_link: bool = True,
) -> bool:
    """Insert ``[Source: ...]`` links into a ``.cbl.md`` file.

    For each ``### PARAGRAPH-NAME`` heading found in the markdown, checks
    whether a corresponding ``.cbl.md`` file exists in *split_dir*.  If it
    does, a blockquote link is inserted on the line immediately following
    the heading::

        ### MAIN-PARA
        > [Source: MAIN-PARA.cbl.md](PROG.cbl.d/MAIN-PARA.cbl.md)

    The function is idempotent -- lines that already contain a source link
    are not duplicated.

    Args:
        md_path: Path to the markdown documentation file.
        split_dir: Directory containing the split paragraph ``.cbl.md`` files.
        relative_link: If *True* (default), the link href is a relative
            path from *md_path*'s parent to the split file.  If *False*,
            uses just the filename.

    Returns:
        *True* if the markdown file was modified (at least one link was
        added), *False* otherwise.
    """
    if not md_path.exists():
        logger.warning("Markdown file not found: %s", md_path)
        return False

    lines = md_path.read_text(encoding="utf-8").splitlines(keepends=True)

    modified = False
    result: list[str] = []
    in_code_fence = False
    i = 0

    while i < len(lines):
        line = lines[i]
        stripped = line.rstrip("\n\r")

        # Track fenced code blocks (``` ... ```) — skip everything inside.
        if stripped.startswith("```"):
            in_code_fence = not in_code_fence
            result.append(line)
            i += 1
            continue

        if in_code_fence:
            result.append(line)
            i += 1
            continue

        m = _PARAGRAPH_HEADING_RE.match(stripped)
        if m:
            para_name = m.group(1)
            filename = f"{sanitize_filename(para_name)}.cbl.md"
            split_file = split_dir / filename

            result.append(line)
            i += 1

            # Check if next line is already a source link (idempotency).
            if i < len(lines) and _SOURCE_LINK_RE.match(
                lines[i].rstrip("\n\r"),
            ):
                # Already linked, skip insertion.
                continue

            if split_file.exists():
                if relative_link:
                    rel = os.path.relpath(split_file, md_path.parent)
                else:
                    rel = filename
                link_line = f"> [Source: {filename}]({rel})\n"
                result.append(link_line)
                modified = True
            continue

        result.append(line)
        i += 1

    if modified:
        md_path.write_text("".join(result), encoding="utf-8")
        logger.info("Patched source links in %s", md_path)

    return modified


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------


def _find_source_file(source_dir: Path, file_name: str) -> Path | None:
    """Locate a source file in *source_dir*, case-insensitively.

    Checks flat layout first (direct child), then searches recursively.
    Returns the first match or *None*.
    """
    # Exact match (fast path).
    candidate = source_dir / file_name
    if candidate.exists():
        return candidate

    # Case-insensitive flat search.
    target_lower = file_name.lower()
    for child in source_dir.iterdir():
        if child.is_file() and child.name.lower() == target_lower:
            return child

    # Recursive case-insensitive search.
    for child in source_dir.rglob("*"):
        if child.is_file() and child.name.lower() == target_lower:
            return child

    return None


# ---------------------------------------------------------------------------
# Batch operations
# ---------------------------------------------------------------------------


def split_all_in_directory(
    source_dir: Path,
    doc_dir: Path,
    output_base: Path | None = None,
) -> dict[str, list[Path]]:
    """Split paragraphs for all COBOL ``.doc.json`` files in a directory.

    Scans *doc_dir* for ``*.cbl.doc.json`` files, reads each one's
    ``header.file_name`` to locate the corresponding source ``.cbl`` file
    in *source_dir*, and calls :func:`split_paragraphs` for each pair.

    Args:
        source_dir: Directory containing original COBOL source files.
        doc_dir: Directory containing ``.doc.json`` documentation files.
        output_base: If provided, paragraph ``.cbl.d/`` directories are
            created under this path.  Otherwise they are created alongside
            the *doc_dir* files.

    Returns:
        Mapping of source filename -> list of created paragraph file paths.
    """
    results: dict[str, list[Path]] = {}

    doc_files = sorted(
        set(doc_dir.rglob("*.cbl.doc.json")) | set(doc_dir.rglob("*.CBL.doc.json")),
    )

    for doc_json_path in doc_files:
        try:
            with open(doc_json_path, encoding="utf-8") as f:
                doc = json.load(f)
        except (json.JSONDecodeError, OSError) as exc:
            logger.warning("Failed to read %s: %s", doc_json_path, exc)
            continue

        header = doc.get("header", {})
        file_name = header.get("file_name")
        if not file_name:
            logger.warning("No header.file_name in %s", doc_json_path)
            continue

        # Search for source file (case-insensitive).
        source_path = _find_source_file(source_dir, file_name)
        if source_path is None:
            raise FileNotFoundError(
                f"Source file {file_name} not found in {source_dir}"
            )

        base_name = Path(file_name).name
        if output_base is not None:
            out_dir = output_base / f"{base_name}.d"
        else:
            out_dir = doc_json_path.parent / f"{base_name}.d"

        created = split_paragraphs(source_path, doc_json_path, out_dir)
        results[file_name] = created

    return results


def patch_all_markdown_in_directory(
    doc_dir: Path,
    source_dir: Path | None = None,
) -> int:
    """Patch source links into all ``.cbl.md`` files in a directory.

    For each markdown file, determines the sibling ``.cbl.d/`` directory
    and calls :func:`patch_markdown_links`.

    Args:
        doc_dir: Directory containing ``.cbl.md`` documentation files.
        source_dir: Optional alternative directory to look for ``.cbl.d/``
            split directories.

    Returns:
        Count of markdown files that were modified.
    """
    md_files = sorted(
        p
        for p in set(doc_dir.rglob("*.cbl.md")) | set(doc_dir.rglob("*.CBL.md"))
        if ".cbl.d" not in p.parent.name and ".CBL.d" not in p.parent.name
    )

    patched_count = 0
    for md_path in md_files:
        # PROG.cbl.md -> stem="PROG.cbl" -> split_dir_name="PROG.cbl.d"
        split_dir_name = f"{md_path.stem}.d"

        # Look as sibling of the md file first.
        split_dir = md_path.parent / split_dir_name
        if not split_dir.is_dir() and source_dir is not None:
            split_dir = source_dir / split_dir_name

        if not split_dir.is_dir():
            logger.debug("No split dir for %s (looked for %s)", md_path, split_dir_name)
            continue

        if patch_markdown_links(md_path, split_dir):
            patched_count += 1

    return patched_count


# ---------------------------------------------------------------------------
# Pipeline convenience wrapper
# ---------------------------------------------------------------------------


def split_and_link(
    source_path: Path,
    doc_json_path: Path,
    md_path: Path,
    output_dir: Path | None = None,
) -> list[Path]:
    """Split paragraphs and patch the corresponding markdown in one call.

    This is the main entry point for pipeline integration.  It is called
    from ``ScribePool._save_template()`` and
    ``DocumentationWriter.write_result()`` after writing the ``.doc.json``
    and ``.md`` files.

    Args:
        source_path: Path to the COBOL source file.
        doc_json_path: Path to the documentation JSON file.
        md_path: Path to the markdown documentation file.
        output_dir: Directory for split files.  Defaults to
            ``{doc_json_path.parent / source_path.name}.d/``.

    Returns:
        List of created paragraph file paths (empty if source file is
        missing or not a COBOL file).
    """
    cobol_extensions = {".cbl", ".cob", ".cobol"}
    if source_path.suffix.lower() not in cobol_extensions:
        return []

    if output_dir is None:
        output_dir = doc_json_path.parent / f"{source_path.name}.d"

    created = split_paragraphs(source_path, doc_json_path, output_dir)

    if created and md_path.exists():
        patch_markdown_links(md_path, output_dir)

    return created


# ---------------------------------------------------------------------------
# Standalone batch runner (for retroactive processing)
# ---------------------------------------------------------------------------


def run_batch_split(input_dir: Path, output_dir: Path) -> None:
    """Split all COBOL paragraphs and patch markdown links in batch.

    Intended for retroactive processing of existing output directories
    (e.g. ``example_output/``).  Can also be invoked as a CLI utility.

    Args:
        input_dir: Directory containing original COBOL source files.
        output_dir: Directory containing ``.doc.json`` and ``.md`` output.
    """
    results = split_all_in_directory(source_dir=input_dir, doc_dir=output_dir)

    total_files = len(results)
    total_paragraphs = sum(len(paths) for paths in results.values())
    logger.info(
        "Batch split complete: %d files processed, %d paragraphs extracted",
        total_files, total_paragraphs,
    )

    patched = patch_all_markdown_in_directory(doc_dir=output_dir)
    logger.info("Batch patch complete: %d markdown files updated", patched)
