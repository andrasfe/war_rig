"""Derive copybook search directories from an input directory tree.

Walks the input directory once (per process) and collects every unique
parent directory that contains at least one file.  This gives
``SourceReader`` a comprehensive search path for COPY resolution.
"""

from __future__ import annotations

import logging
from pathlib import Path

logger = logging.getLogger(__name__)

# Module-level cache keyed by resolved path string.
_cache: dict[str, list[Path]] = {}


def derive_copybook_dirs(
    input_directory: Path,
    extra_dirs: list[Path] | None = None,
) -> list[Path]:
    """Build a list of directories to search for COBOL copybooks.

    Args:
        input_directory: Root directory containing source files.
        extra_dirs: Optional additional directories (e.g. from config).

    Returns:
        De-duplicated list of directories that contain at least one file.
    """
    key = str(input_directory.resolve())
    if key not in _cache:
        dirs: dict[str, Path] = {}
        try:
            for entry in input_directory.rglob("*"):
                if entry.is_file():
                    parent = entry.parent
                    pkey = str(parent)
                    if pkey not in dirs:
                        dirs[pkey] = parent
        except OSError as exc:
            logger.warning(
                "Failed to walk %s for copybook dirs: %s",
                input_directory,
                exc,
            )
        _cache[key] = list(dirs.values())
        logger.debug(
            "Derived %d copybook directories from %s",
            len(_cache[key]),
            input_directory,
        )

    result = list(_cache[key])
    if extra_dirs:
        seen = {str(d) for d in result}
        for d in extra_dirs:
            dk = str(d)
            if dk not in seen:
                result.append(d)
                seen.add(dk)
    return result


def clear_cache() -> None:
    """Clear the module-level directory cache (for tests)."""
    _cache.clear()
