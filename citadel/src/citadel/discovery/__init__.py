"""
File discovery module for Citadel.

Handles directory traversal, file type detection, and grouping files
by artifact specification.
"""

from citadel.discovery.file_walker import (
    DEFAULT_EXCLUDES,
    EXTENSION_TO_SPEC,
    DiscoveryResult,
    FileDiscovery,
    discover_files,
)

__all__ = [
    "DEFAULT_EXCLUDES",
    "EXTENSION_TO_SPEC",
    "DiscoveryResult",
    "FileDiscovery",
    "discover_files",
]
