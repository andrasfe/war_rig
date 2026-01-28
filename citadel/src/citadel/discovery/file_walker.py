"""
File discovery module for Citadel.

This module provides the FileDiscovery class for traversing source directories,
identifying file types by extension, and grouping files by their corresponding
artifact specifications.
"""

import fnmatch
import logging
from pathlib import Path

from pydantic import BaseModel, Field

logger = logging.getLogger(__name__)


# Default patterns to exclude from discovery
DEFAULT_EXCLUDES: list[str] = [
    "**/.git/**",
    "**/.git",
    "**/.svn/**",
    "**/.svn",
    "**/node_modules/**",
    "**/node_modules",
    "**/__pycache__/**",
    "**/__pycache__",
    "**/target/**",
    "**/target",
    "**/.cache/**",
    "**/.cache",
    "**/dist/**",
    "**/dist",
    "**/build/**",
    "**/build",
    "**/*.pyc",
    "**/*.class",
    "**/*.o",
    "**/*.obj",
    "**/*.exe",
    "**/*.dll",
    "**/*.so",
    "**/*.dylib",
    "**/.venv/**",
    "**/.venv",
    "**/venv/**",
    "**/venv",
    "**/.env/**",
    "**/.idea/**",
    "**/.vscode/**",
    "**/.DS_Store",
    "**/Thumbs.db",
    # Office documents
    "**/*.docx",
    "**/*.doc",
    "**/*.xlsx",
    "**/*.xls",
    "**/*.pptx",
    "**/*.ppt",
    "**/*.pdf",
]


# Mapping of file extensions to spec IDs
# Extensions are normalized to lowercase for comparison
EXTENSION_TO_SPEC: dict[str, str] = {
    # COBOL
    ".cbl": "cobol",
    ".cob": "cobol",
    ".cobol": "cobol",
    # Copybook
    ".cpy": "copybook",
    ".copy": "copybook",
    # JCL
    ".jcl": "jcl",
    ".proc": "jcl",
    ".prc": "jcl",
    # DB2 DDL
    ".sql": "db2_ddl",
    ".ddl": "db2_ddl",
    ".db2": "db2_ddl",
    # BMS (CICS maps)
    ".bms": "bms",
    # Python
    ".py": "python",
    ".pyw": "python",
    # Datacards (utility control statements)
    ".dc": "datacard",
}


class DiscoveryResult(BaseModel):
    """
    Result of file discovery.

    Contains categorized files grouped by their spec ID, along with
    files that couldn't be identified or were excluded.
    """

    files_by_spec: dict[str, list[Path]] = Field(default_factory=dict)
    """Mapping of spec_id to list of file paths that match that spec."""

    unknown_files: list[Path] = Field(default_factory=list)
    """Files that could not be matched to any known spec."""

    excluded_files: list[Path] = Field(default_factory=list)
    """Files that matched exclude patterns and were skipped."""

    total_files: int = 0
    """Total number of files discovered (including excluded)."""

    class Config:
        """Pydantic configuration."""

        arbitrary_types_allowed = True

    def add_file(self, spec_id: str, path: Path) -> None:
        """Add a file to the appropriate spec group."""
        if spec_id not in self.files_by_spec:
            self.files_by_spec[spec_id] = []
        self.files_by_spec[spec_id].append(path)

    def summary(self) -> dict[str, int]:
        """Return a summary of discovered files by category."""
        return {
            "total": self.total_files,
            "identified": sum(len(files) for files in self.files_by_spec.values()),
            "unknown": len(self.unknown_files),
            "excluded": len(self.excluded_files),
            **{spec_id: len(files) for spec_id, files in self.files_by_spec.items()},
        }


class FileDiscovery:
    """
    Traverses source directory and groups files by artifact spec.

    This class walks the directory tree, applies exclusion patterns,
    and categorizes files based on their extensions.
    """

    def __init__(
        self,
        root: Path,
        exclude_patterns: list[str] | None = None,
    ) -> None:
        """
        Initialize the file discovery component.

        Args:
            root: Source directory root to traverse.
            exclude_patterns: Glob patterns to exclude. If None, uses DEFAULT_EXCLUDES.
        """
        self.root = root.resolve()
        self.exclude_patterns = exclude_patterns if exclude_patterns is not None else DEFAULT_EXCLUDES.copy()

        if not self.root.exists():
            raise ValueError(f"Root directory does not exist: {self.root}")
        if not self.root.is_dir():
            raise ValueError(f"Root path is not a directory: {self.root}")

        logger.debug("FileDiscovery initialized with root: %s", self.root)
        logger.debug("Exclude patterns: %s", self.exclude_patterns)

    def discover(self) -> DiscoveryResult:
        """
        Walk directory tree and categorize files.

        Returns:
            DiscoveryResult with files grouped by spec, plus unknown and excluded lists.
        """
        result = DiscoveryResult()

        logger.info("Starting file discovery in: %s", self.root)

        for path in self._walk_directory():
            result.total_files += 1

            # Check if file should be excluded
            if self._is_excluded(path):
                result.excluded_files.append(path)
                logger.debug("Excluded: %s", path)
                continue

            # Try to identify the file's spec
            spec_id = self.identify_file(path)
            if spec_id is not None:
                result.add_file(spec_id, path)
                logger.debug("Identified %s as %s", path, spec_id)
            else:
                result.unknown_files.append(path)
                logger.debug("Unknown file type: %s", path)

        logger.info(
            "Discovery complete: %d total, %d identified, %d unknown, %d excluded",
            result.total_files,
            sum(len(files) for files in result.files_by_spec.values()),
            len(result.unknown_files),
            len(result.excluded_files),
        )

        return result

    def identify_file(self, path: Path) -> str | None:
        """
        Identify which spec applies to a file based on its extension.

        Args:
            path: Path to the file to identify.

        Returns:
            Spec ID if the file matches a known extension, None otherwise.
        """
        # Get the file extension and normalize to lowercase
        extension = path.suffix.lower()

        # Check against known extensions
        if extension in EXTENSION_TO_SPEC:
            return EXTENSION_TO_SPEC[extension]

        return None

    def _walk_directory(self) -> list[Path]:
        """
        Walk the directory tree and yield all files.

        Returns:
            Generator of Path objects for all files in the tree.
        """
        files: list[Path] = []
        try:
            for item in self.root.rglob("*"):
                if item.is_file():
                    files.append(item)
        except PermissionError as e:
            logger.warning("Permission denied accessing: %s", e)
        except OSError as e:
            logger.warning("OS error during directory walk: %s", e)

        return files

    def _is_excluded(self, path: Path) -> bool:
        """
        Check if a path matches any exclude pattern.

        Args:
            path: Path to check against exclude patterns.

        Returns:
            True if the path should be excluded, False otherwise.
        """
        # Get path relative to root for pattern matching
        try:
            relative_path = path.relative_to(self.root)
        except ValueError:
            # Path is not relative to root, use absolute path
            relative_path = path

        # Convert to string for fnmatch (using forward slashes for consistency)
        path_str = str(relative_path).replace("\\", "/")

        # Also check the absolute path
        abs_path_str = str(path).replace("\\", "/")

        for pattern in self.exclude_patterns:
            # Normalize pattern to use forward slashes
            normalized_pattern = pattern.replace("\\", "/")

            # Check if pattern matches relative path
            if fnmatch.fnmatch(path_str, normalized_pattern):
                return True

            # Check if pattern matches absolute path
            if fnmatch.fnmatch(abs_path_str, normalized_pattern):
                return True

            # For patterns like "**/.git/**", also check each path component
            if "**" in normalized_pattern:
                # Check if any part of the path matches the non-** portion
                pattern_parts = normalized_pattern.replace("**", "").strip("/").split("/")
                path_parts = path_str.split("/")

                for pattern_part in pattern_parts:
                    if pattern_part and any(fnmatch.fnmatch(p, pattern_part) for p in path_parts):
                        # This is a heuristic - if the pattern has ** and a component matches,
                        # we likely want to exclude it
                        if pattern_part in [".git", ".svn", "node_modules", "__pycache__",
                                          ".cache", ".venv", "venv"]:
                            return True

        return False

    def add_exclude_pattern(self, pattern: str) -> None:
        """
        Add an additional exclude pattern.

        Args:
            pattern: Glob pattern to add to exclusions.
        """
        if pattern not in self.exclude_patterns:
            self.exclude_patterns.append(pattern)
            logger.debug("Added exclude pattern: %s", pattern)

    def remove_exclude_pattern(self, pattern: str) -> bool:
        """
        Remove an exclude pattern.

        Args:
            pattern: Glob pattern to remove from exclusions.

        Returns:
            True if pattern was removed, False if it wasn't in the list.
        """
        if pattern in self.exclude_patterns:
            self.exclude_patterns.remove(pattern)
            logger.debug("Removed exclude pattern: %s", pattern)
            return True
        return False


def discover_files(
    root: Path,
    exclude_patterns: list[str] | None = None,
) -> DiscoveryResult:
    """
    Convenience function to discover files in a directory.

    Args:
        root: Source directory root to traverse.
        exclude_patterns: Glob patterns to exclude. If None, uses DEFAULT_EXCLUDES.

    Returns:
        DiscoveryResult with categorized files.
    """
    discovery = FileDiscovery(root=root, exclude_patterns=exclude_patterns)
    return discovery.discover()
