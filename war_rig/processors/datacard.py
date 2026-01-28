"""Datacard processor for utility control statements.

This module provides batch processing of datacard files (.dc), automatically
detecting utility types (UNLOAD, REPRO, SORT, etc.) and generating a single
consolidated DATACARDS.md catalog document.
"""

import logging
import re
from dataclasses import dataclass, field
from pathlib import Path

logger = logging.getLogger(__name__)


@dataclass
class UtilityPattern:
    """Pattern for detecting utility type from content."""

    name: str
    pattern: re.Pattern[str]
    description: str


# Ordered list of utility detection patterns (first match wins)
UTILITY_PATTERNS: list[UtilityPattern] = [
    UtilityPattern(
        name="DB2 UNLOAD",
        pattern=re.compile(r"UNLOAD\s+(DIRECT|TABLESPACE|TABLE|DATA)", re.IGNORECASE),
        description="DB2 database unload operation",
    ),
    UtilityPattern(
        name="DB2 LOAD",
        pattern=re.compile(r"LOAD\s+(DATA|INTO)", re.IGNORECASE),
        description="DB2 database load operation",
    ),
    UtilityPattern(
        name="IDCAMS REPRO",
        pattern=re.compile(r"REPRO\s*-?\s*(INFILE|OUTFILE|INDATASET|OUTDATASET)", re.IGNORECASE),
        description="IDCAMS REPRO - copy sequential/VSAM datasets",
    ),
    UtilityPattern(
        name="IDCAMS DEFINE",
        pattern=re.compile(r"DEFINE\s+(CLUSTER|AIX|PATH|GDG|ALIAS)", re.IGNORECASE),
        description="IDCAMS DEFINE - create VSAM cluster/GDG",
    ),
    UtilityPattern(
        name="IDCAMS DELETE",
        pattern=re.compile(r"DELETE\s+", re.IGNORECASE),
        description="IDCAMS DELETE - delete dataset/cluster",
    ),
    UtilityPattern(
        name="IDCAMS LISTCAT",
        pattern=re.compile(r"LISTCAT\s+", re.IGNORECASE),
        description="IDCAMS LISTCAT - list catalog entries",
    ),
    UtilityPattern(
        name="IDCAMS ALTER",
        pattern=re.compile(r"ALTER\s+", re.IGNORECASE),
        description="IDCAMS ALTER - modify catalog attributes",
    ),
    UtilityPattern(
        name="IDCAMS PRINT",
        pattern=re.compile(r"PRINT\s+", re.IGNORECASE),
        description="IDCAMS PRINT - print dataset contents",
    ),
    UtilityPattern(
        name="DFSORT",
        pattern=re.compile(r"SORT\s+(FIELDS|FORMAT)", re.IGNORECASE),
        description="DFSORT - sort/merge records",
    ),
    UtilityPattern(
        name="ICETOOL",
        pattern=re.compile(r"ICETOOL", re.IGNORECASE),
        description="ICETOOL - advanced data transformation",
    ),
    UtilityPattern(
        name="IEBCOPY",
        pattern=re.compile(r"COPY\s+(INDD|OUTDD)", re.IGNORECASE),
        description="IEBCOPY - copy PDS members",
    ),
    UtilityPattern(
        name="IEBGENER",
        pattern=re.compile(r"GENERATE\s*", re.IGNORECASE),
        description="IEBGENER - copy sequential datasets",
    ),
    UtilityPattern(
        name="IEFBR14",
        pattern=re.compile(r"IEFBR14|//\s*DD\s+.*DISP=\(NEW", re.IGNORECASE),
        description="IEFBR14 - allocate/delete datasets",
    ),
    UtilityPattern(
        name="FTP",
        pattern=re.compile(r"(GET|PUT|MGET|MPUT)\s+", re.IGNORECASE),
        description="FTP control cards",
    ),
]


@dataclass
class DatacardInfo:
    """Information about a single datacard file."""

    file_path: Path
    file_name: str
    utility_type: str
    description: str
    content_preview: str
    datasets: list[str] = field(default_factory=list)
    tables: list[str] = field(default_factory=list)


@dataclass
class DatacardCatalog:
    """Catalog of all datacards grouped by utility type."""

    datacards: list[DatacardInfo] = field(default_factory=list)
    by_utility: dict[str, list[DatacardInfo]] = field(default_factory=dict)
    total_count: int = 0
    unknown_count: int = 0

    def add(self, info: DatacardInfo) -> None:
        """Add a datacard to the catalog."""
        self.datacards.append(info)
        self.total_count += 1

        if info.utility_type == "UNKNOWN":
            self.unknown_count += 1

        if info.utility_type not in self.by_utility:
            self.by_utility[info.utility_type] = []
        self.by_utility[info.utility_type].append(info)


class DatacardProcessor:
    """Processes datacard files and generates a consolidated catalog."""

    # Patterns for extracting referenced datasets
    DATASET_PATTERN = re.compile(r"DSN=([A-Z0-9@#$.]+)", re.IGNORECASE)
    DATASET_QUOTED_PATTERN = re.compile(r"DSN='([A-Z0-9@#$.()+-]+)'", re.IGNORECASE)
    INFILE_PATTERN = re.compile(r"INFILE\(([A-Z0-9@#$]+)\)", re.IGNORECASE)
    OUTFILE_PATTERN = re.compile(r"OUTFILE\(([A-Z0-9@#$]+)\)", re.IGNORECASE)
    TABLE_PATTERN = re.compile(r"FROM\s+([A-Z0-9_]+\.[A-Z0-9_]+)", re.IGNORECASE)
    INTO_TABLE_PATTERN = re.compile(r"INTO\s+([A-Z0-9_]+\.[A-Z0-9_]+)", re.IGNORECASE)

    def __init__(self, input_directory: Path, output_directory: Path) -> None:
        """Initialize the datacard processor.

        Args:
            input_directory: Root directory to scan for .dc files.
            output_directory: Directory where DATACARDS.md will be written.
        """
        self.input_directory = input_directory
        self.output_directory = output_directory
        self._catalog = DatacardCatalog()

    def discover_datacards(self) -> list[Path]:
        """Discover all .dc files in the input directory.

        Returns:
            List of paths to datacard files.
        """
        datacards: list[Path] = []

        for path in self.input_directory.rglob("*.dc"):
            if path.is_file():
                datacards.append(path)

        for path in self.input_directory.rglob("*.DC"):
            if path.is_file() and path not in datacards:
                datacards.append(path)

        logger.info("Discovered %d datacard files", len(datacards))
        return sorted(datacards)

    def detect_utility_type(self, content: str) -> tuple[str, str]:
        """Detect the utility type from file content.

        Args:
            content: File content to analyze.

        Returns:
            Tuple of (utility_type, description).
        """
        for pattern in UTILITY_PATTERNS:
            if pattern.pattern.search(content):
                return pattern.name, pattern.description

        return "UNKNOWN", "Unrecognized utility control card"

    def extract_references(self, content: str) -> tuple[list[str], list[str]]:
        """Extract dataset and table references from content.

        Args:
            content: File content to analyze.

        Returns:
            Tuple of (datasets, tables).
        """
        datasets: set[str] = set()
        tables: set[str] = set()

        for match in self.DATASET_PATTERN.findall(content):
            datasets.add(match)

        for match in self.DATASET_QUOTED_PATTERN.findall(content):
            datasets.add(match)

        for match in self.INFILE_PATTERN.findall(content):
            datasets.add(f"DD:{match}")

        for match in self.OUTFILE_PATTERN.findall(content):
            datasets.add(f"DD:{match}")

        for match in self.TABLE_PATTERN.findall(content):
            tables.add(match)

        for match in self.INTO_TABLE_PATTERN.findall(content):
            tables.add(match)

        return sorted(datasets), sorted(tables)

    def process_file(self, file_path: Path) -> DatacardInfo:
        """Process a single datacard file.

        Args:
            file_path: Path to the datacard file.

        Returns:
            DatacardInfo with extracted information.
        """
        try:
            content = file_path.read_text(encoding="utf-8", errors="replace")
        except Exception as e:
            logger.warning("Could not read %s: %s", file_path, e)
            content = ""

        utility_type, description = self.detect_utility_type(content)
        datasets, tables = self.extract_references(content)

        # Create a content preview (first non-empty lines, max 200 chars)
        lines = [line.strip() for line in content.split("\n") if line.strip()]
        preview = " ".join(lines[:3])[:200]
        if len(preview) == 200:
            preview = preview[:197] + "..."

        return DatacardInfo(
            file_path=file_path,
            file_name=file_path.name,
            utility_type=utility_type,
            description=description,
            content_preview=preview,
            datasets=datasets,
            tables=tables,
        )

    def process_all(self) -> DatacardCatalog:
        """Process all datacards and build the catalog.

        Returns:
            Complete catalog of all datacards.
        """
        self._catalog = DatacardCatalog()
        datacards = self.discover_datacards()

        for file_path in datacards:
            info = self.process_file(file_path)
            self._catalog.add(info)
            logger.debug("Processed %s -> %s", file_path.name, info.utility_type)

        logger.info(
            "Processed %d datacards: %d identified, %d unknown",
            self._catalog.total_count,
            self._catalog.total_count - self._catalog.unknown_count,
            self._catalog.unknown_count,
        )

        return self._catalog

    def generate_catalog_markdown(self) -> str:
        """Generate the DATACARDS.md markdown catalog.

        Returns:
            Markdown content for the catalog.
        """
        lines: list[str] = [
            "# Datacard Catalog",
            "",
            "This document catalogs all utility control cards (datacards) in the system.",
            "Datacards are control statements for mainframe utilities like DB2, IDCAMS, DFSORT, etc.",
            "",
            "## Summary",
            "",
            f"- **Total datacards:** {self._catalog.total_count}",
            f"- **Identified:** {self._catalog.total_count - self._catalog.unknown_count}",
            f"- **Unrecognized:** {self._catalog.unknown_count}",
            "",
            "### By Utility Type",
            "",
            "| Utility | Count |",
            "|---------|-------|",
        ]

        # Sort by count descending
        sorted_utilities = sorted(
            self._catalog.by_utility.items(),
            key=lambda x: len(x[1]),
            reverse=True,
        )

        for utility_type, datacards in sorted_utilities:
            lines.append(f"| {utility_type} | {len(datacards)} |")

        lines.append("")
        lines.append("---")
        lines.append("")

        # Detail sections by utility type
        for utility_type, datacards in sorted_utilities:
            if not datacards:
                continue

            description = datacards[0].description
            lines.append(f"## {utility_type}")
            lines.append("")
            lines.append(f"*{description}*")
            lines.append("")
            lines.append("| File | Referenced Datasets/Tables | Preview |")
            lines.append("|------|---------------------------|---------|")

            for dc in sorted(datacards, key=lambda x: x.file_name):
                refs = ", ".join(dc.datasets[:3] + dc.tables[:2])
                if len(dc.datasets) + len(dc.tables) > 5:
                    refs += ", ..."
                if not refs:
                    refs = "-"

                # Escape pipe characters in preview
                preview = dc.content_preview.replace("|", "\\|")
                if len(preview) > 80:
                    preview = preview[:77] + "..."

                lines.append(f"| `{dc.file_name}` | {refs} | {preview} |")

            lines.append("")

        return "\n".join(lines)

    def write_catalog(self) -> Path:
        """Generate and write the DATACARDS.md file.

        Returns:
            Path to the written file.
        """
        if not self._catalog.datacards:
            self.process_all()

        markdown = self.generate_catalog_markdown()

        output_path = self.output_directory / "DATACARDS.md"
        output_path.parent.mkdir(parents=True, exist_ok=True)
        output_path.write_text(markdown, encoding="utf-8")

        logger.info("Wrote datacard catalog to %s", output_path)
        return output_path


def process_datacards(input_directory: Path, output_directory: Path) -> Path | None:
    """Convenience function to process all datacards and generate catalog.

    Args:
        input_directory: Root directory to scan for .dc files.
        output_directory: Directory where DATACARDS.md will be written.

    Returns:
        Path to DATACARDS.md if any datacards were found, None otherwise.
    """
    processor = DatacardProcessor(input_directory, output_directory)
    catalog = processor.process_all()

    if catalog.total_count == 0:
        logger.info("No datacard files found")
        return None

    return processor.write_catalog()
