"""Source file reading and discovery for War Rig.

This module provides functionality for discovering and reading source files
from a directory structure, with support for:

- File type detection based on extension
- Copybook resolution
- Batch file discovery
- File metadata tracking
"""

import logging
from pathlib import Path
from typing import Iterator

from pydantic import BaseModel, Field

from war_rig.config import FileExtensionsConfig, SystemConfig
from war_rig.models.templates import FileType

logger = logging.getLogger(__name__)


class SourceFile(BaseModel):
    """Metadata about a source file.

    Tracks information about a discovered source file before processing.
    """

    path: Path = Field(..., description="Full path to the file")
    name: str = Field(..., description="File name (basename) without path")
    relative_path: str = Field(
        default="",
        description="Relative path from input directory (e.g., 'app/cobol/PROG.cbl')",
    )
    file_type: FileType = Field(..., description="Detected file type")
    size_bytes: int = Field(default=0, ge=0, description="File size in bytes")
    line_count: int | None = Field(default=None, description="Number of lines (if read)")

    class Config:
        arbitrary_types_allowed = True

    @property
    def stem(self) -> str:
        """Get the file name without extension.

        Returns:
            File stem (e.g., "PROGRAM" from "PROGRAM.cbl").
        """
        return self.path.stem


class SourceReader:
    """Reader for source files and directories.

    Provides functionality to discover files by type and read their contents.
    Supports copybook resolution for COBOL programs.

    Attributes:
        config: System configuration with paths and extensions
        extensions: File extension mappings

    Example:
        reader = SourceReader(config.system)
        files = list(reader.discover_files())
        for f in files:
            content = reader.read_file(f.path)
            print(f"Read {f.name}: {len(content)} chars")
    """

    def __init__(self, config: SystemConfig):
        """Initialize the reader with configuration.

        Args:
            config: System configuration with paths and extensions.
        """
        self.config = config
        self.extensions = config.file_extensions
        self._extension_map = self._build_extension_map()

    def _build_extension_map(self) -> dict[str, FileType]:
        """Build a mapping from extension to file type.

        Returns:
            Dictionary mapping lowercase extensions to FileType.
        """
        mapping: dict[str, FileType] = {}

        for ext in self.extensions.cobol:
            mapping[ext.lower()] = FileType.COBOL

        for ext in self.extensions.copybook:
            mapping[ext.lower()] = FileType.COPYBOOK

        for ext in self.extensions.jcl:
            mapping[ext.lower()] = FileType.JCL

        for ext in self.extensions.bms:
            mapping[ext.lower()] = FileType.BMS

        for ext in self.extensions.pli:
            mapping[ext.lower()] = FileType.PLI

        # Add listing files if configured
        if hasattr(self.extensions, 'listing'):
            for ext in self.extensions.listing:
                mapping[ext.lower()] = FileType.LISTING

        return mapping

    def detect_file_type(self, path: Path) -> FileType:
        """Detect the file type based on extension.

        Args:
            path: Path to the file.

        Returns:
            Detected FileType.
        """
        ext = path.suffix.lower()
        return self._extension_map.get(ext, FileType.OTHER)

    def discover_files(
        self,
        directory: Path | None = None,
        file_types: list[FileType] | None = None,
        recursive: bool = True,
    ) -> Iterator[SourceFile]:
        """Discover source files in a directory.

        Args:
            directory: Directory to search (uses config default if None).
            file_types: File types to include (all if None).
            recursive: Whether to search subdirectories.

        Yields:
            SourceFile for each discovered file.
        """
        directory = directory or self.config.input_directory
        directory = Path(directory)

        if not directory.exists():
            logger.warning(f"Directory does not exist: {directory}")
            return

        if not directory.is_dir():
            logger.warning(f"Path is not a directory: {directory}")
            return

        # Build glob pattern
        pattern = "**/*" if recursive else "*"

        for path in directory.glob(pattern):
            if not path.is_file():
                continue

            file_type = self.detect_file_type(path)

            # Filter by requested types
            if file_types and file_type not in file_types:
                continue

            # Skip OTHER unless explicitly requested
            if file_type == FileType.OTHER and file_types is None:
                continue

            try:
                size = path.stat().st_size
            except OSError:
                size = 0

            # Compute relative path from input directory
            try:
                rel_path = path.relative_to(directory)
            except ValueError:
                # If path is not relative to directory, use basename
                rel_path = Path(path.name)

            yield SourceFile(
                path=path,
                name=path.name,
                relative_path=str(rel_path),
                file_type=file_type,
                size_bytes=size,
            )

    def read_file(self, path: Path | str) -> str:
        """Read the contents of a source file.

        Args:
            path: Path to the file.

        Returns:
            File contents as a string.

        Raises:
            FileNotFoundError: If the file doesn't exist.
            IOError: If the file can't be read.
        """
        path = Path(path)

        if not path.exists():
            raise FileNotFoundError(f"File not found: {path}")

        # Try multiple encodings common for mainframe files
        encodings = ["utf-8", "latin-1", "cp1252", "ebcdic-cp-us"]

        for encoding in encodings:
            try:
                with path.open("r", encoding=encoding) as f:
                    return f.read()
            except UnicodeDecodeError:
                continue
            except LookupError:
                # EBCDIC codec might not be available
                continue

        # Last resort: read as binary and decode with replacement
        logger.warning(f"Using binary read with replacement for {path}")
        with path.open("rb") as f:
            return f.read().decode("utf-8", errors="replace")

    def read_source_file(self, source_file: SourceFile) -> str:
        """Read the contents of a SourceFile.

        This is a convenience method that also updates the line count.

        Args:
            source_file: SourceFile to read.

        Returns:
            File contents as a string.
        """
        content = self.read_file(source_file.path)
        source_file.line_count = content.count("\n") + 1
        return content

    def discover_copybooks(
        self,
        directory: Path | None = None,
        recursive: bool = True,
    ) -> dict[str, SourceFile]:
        """Discover copybooks and build a name-to-file mapping.

        Args:
            directory: Directory to search.
            recursive: Whether to search subdirectories.

        Returns:
            Dictionary mapping copybook names to SourceFile objects.
        """
        copybooks: dict[str, SourceFile] = {}

        for source_file in self.discover_files(
            directory,
            file_types=[FileType.COPYBOOK],
            recursive=recursive,
        ):
            # Use stem as the copybook name
            name = source_file.stem.upper()
            copybooks[name] = source_file

        logger.info(f"Discovered {len(copybooks)} copybooks")
        return copybooks

    def resolve_copybooks(
        self,
        copybook_names: list[str],
        copybook_directory: Path | None = None,
    ) -> dict[str, str]:
        """Resolve copybook names to their contents.

        Args:
            copybook_names: Names of copybooks to resolve.
            copybook_directory: Directory to search for copybooks.

        Returns:
            Dictionary mapping copybook names to their contents.
        """
        # First discover all copybooks
        available = self.discover_copybooks(copybook_directory)

        # Resolve requested ones
        resolved: dict[str, str] = {}

        for name in copybook_names:
            name_upper = name.upper()
            if name_upper in available:
                try:
                    content = self.read_source_file(available[name_upper])
                    resolved[name_upper] = content
                    logger.debug(f"Resolved copybook: {name_upper}")
                except Exception as e:
                    logger.warning(f"Failed to read copybook {name_upper}: {e}")
            else:
                logger.warning(f"Copybook not found: {name_upper}")

        return resolved

    def create_inventory(
        self,
        directory: Path | None = None,
        recursive: bool = True,
    ) -> dict[str, list[SourceFile]]:
        """Create an inventory of all source files by type.

        Args:
            directory: Directory to search.
            recursive: Whether to search subdirectories.

        Returns:
            Dictionary mapping FileType names to lists of SourceFiles.
        """
        inventory: dict[str, list[SourceFile]] = {
            ft.value: [] for ft in FileType
        }

        for source_file in self.discover_files(
            directory,
            file_types=None,  # Include all
            recursive=recursive,
        ):
            inventory[source_file.file_type.value].append(source_file)

        # Log summary
        for file_type, files in inventory.items():
            if files:
                logger.info(f"Found {len(files)} {file_type} files")

        return inventory
