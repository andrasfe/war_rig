"""Skills generator for Agent Skills format conversion.

This module provides the SkillsGenerator class which converts War Rig
documentation output into Agent Skills format for progressive discovery.

The generator takes a War Rig output directory (containing .doc.json files,
SYSTEM_OVERVIEW.md, CALL_GRAPH.md, etc.) and produces a skills directory
structure suitable for Agent Skills consumption.

Example:
    >>> generator = SkillsGenerator(Path("./output"))
    >>> skills_dir = generator.generate()
    >>> print(f"Generated skills at: {skills_dir}")
"""

import json
import logging
from pathlib import Path
from typing import Any

from war_rig.skills.call_graph_skill import CallGraphSkillGenerator
from war_rig.skills.datacard_skill import DatacardSkillGenerator
from war_rig.skills.overview_skill import OverviewSkillGenerator
from war_rig.skills.program_skill import ProgramSkillGenerator

logger = logging.getLogger(__name__)


class SkillsGeneratorError(Exception):
    """Base exception for skills generator errors."""

    pass


class InputDirectoryNotFoundError(SkillsGeneratorError):
    """Raised when the input directory does not exist."""

    pass


class InvalidInputDirectoryError(SkillsGeneratorError):
    """Raised when the input directory has invalid structure."""

    pass


class SkillsGenerator:
    """Generates Agent Skills format from War Rig documentation output.

    This class handles the conversion of War Rig documentation output
    into the Agent Skills directory format. It creates a structured
    skills directory with subdirectories for:

    - system-overview/ - System-level documentation
    - call-graph/ - Program call relationships
    - datacards/ - Data structure documentation (if available)
    - programs/<program-name>/ - Individual program documentation
    - programs/<program-name>/references/ - Cross-references

    Attributes:
        input_dir: Path to the War Rig output directory.
        output_dir: Path to the skills output directory.

    Example:
        >>> generator = SkillsGenerator(Path("./output"))
        >>> skills_dir = generator.generate()
        >>> print(f"Skills generated at: {skills_dir}")

        >>> # Custom output directory
        >>> generator = SkillsGenerator(
        ...     Path("./output"),
        ...     output_dir=Path("./my-skills")
        ... )
    """

    def __init__(self, input_dir: Path, output_dir: Path | None = None) -> None:
        """Initialize the SkillsGenerator.

        Args:
            input_dir: War Rig output directory (e.g., ./output).
                      Must contain final/programs/*.doc.json files.
            output_dir: Skills output directory. Defaults to skills-{input_dir.name}
                       at same level as input_dir (e.g., ./skills-output).

        Raises:
            InputDirectoryNotFoundError: If input_dir does not exist.
            InvalidInputDirectoryError: If input_dir lacks expected structure.
        """
        self.input_dir = input_dir.resolve()
        self._validate_input_directory()

        if output_dir is None:
            # Default: skills-{input_name} at same level as input_dir
            parent = self.input_dir.parent
            self.output_dir = parent / f"skills-{self.input_dir.name}"
        else:
            self.output_dir = output_dir.resolve()

        logger.info(
            f"SkillsGenerator initialized: input={self.input_dir}, output={self.output_dir}"
        )

    def _validate_input_directory(self) -> None:
        """Validate that the input directory exists and has expected structure.

        Raises:
            InputDirectoryNotFoundError: If input_dir does not exist.
            InvalidInputDirectoryError: If input_dir lacks expected structure.
        """
        if not self.input_dir.exists():
            raise InputDirectoryNotFoundError(
                f"Input directory does not exist: {self.input_dir}"
            )

        if not self.input_dir.is_dir():
            raise InvalidInputDirectoryError(
                f"Input path is not a directory: {self.input_dir}"
            )

        # Check for expected War Rig output structure
        # We expect at least one of: final/programs dir, SYSTEM_OVERVIEW.md, or CALL_GRAPH.md
        final_programs = self.input_dir / "final" / "programs"
        has_programs = final_programs.exists() and any(
            final_programs.glob("*.doc.json")
        )
        has_system_overview = self._has_system_overview()
        has_call_graph = self._has_call_graph()

        if not any([has_programs, has_system_overview, has_call_graph]):
            raise InvalidInputDirectoryError(
                f"Input directory lacks War Rig output structure. Expected at least one of: "
                f"final/programs/*.doc.json, SYSTEM_OVERVIEW.md, or CALL_GRAPH.md in {self.input_dir}"
            )

        logger.debug(
            f"Input validation passed: programs={has_programs}, "
            f"system_overview={has_system_overview}, call_graph={has_call_graph}"
        )

    def generate(self) -> Path:
        """Generate all skills from the documentation output.

        Creates the complete skills directory structure and converts
        all available documentation into Agent Skills format.

        This method orchestrates the generation of:
        - Individual program skills from .doc.json files
        - System overview skill (if SYSTEM_OVERVIEW.md exists)
        - Call graph skill (if CALL_GRAPH.md exists)
        - Datacards skill (if DATACARDS.md exists)

        Returns:
            Path to the generated skills directory.

        Raises:
            SkillsGeneratorError: If generation fails.
        """
        logger.info(f"Starting skills generation from {self.input_dir}")

        # Create directory structure
        self._create_directory_structure()

        # Generate program skills and collect summaries for overview
        program_gen = ProgramSkillGenerator(self.output_dir)
        program_summaries: list[dict[str, str]] = []

        for doc_path in self._find_program_docs():
            try:
                skill_path = program_gen.generate_from_doc_json(doc_path)
                logger.debug(f"Generated program skill: {skill_path}")

                # Collect summary for overview
                doc_data = self._load_doc_json(doc_path)
                summary = program_gen.get_program_summary(doc_data)
                program_summaries.append(summary)
            except Exception as e:
                logger.error(f"Failed to generate skill from {doc_path}: {e}")
                # Continue with other programs

        logger.info(f"Generated {len(program_summaries)} program skills")

        # Generate system overview (uses program summaries for index)
        if self._has_system_overview():
            overview_path = self.get_system_overview_path()
            if overview_path:
                try:
                    overview_gen = OverviewSkillGenerator(self.output_dir)
                    overview_gen.generate(overview_path, program_summaries)
                    logger.info("Generated system overview skill")
                except Exception as e:
                    logger.error(f"Failed to generate system overview skill: {e}")

        # Generate call graph skill
        if self._has_call_graph():
            call_graph_path = self.get_call_graph_path()
            if call_graph_path:
                try:
                    call_graph_gen = CallGraphSkillGenerator(self.output_dir)
                    call_graph_gen.generate(call_graph_path)
                    logger.info("Generated call graph skill")
                except Exception as e:
                    logger.error(f"Failed to generate call graph skill: {e}")

        # Generate datacards skill
        if self._has_datacards():
            datacards_path = self.get_datacards_path()
            if datacards_path:
                try:
                    datacard_gen = DatacardSkillGenerator(self.output_dir)
                    datacard_gen.generate(datacards_path)
                    logger.info("Generated datacards skill")
                except Exception as e:
                    logger.error(f"Failed to generate datacards skill: {e}")

        logger.info(f"Skills directory created at: {self.output_dir}")
        return self.output_dir

    def _load_doc_json(self, doc_json_path: Path) -> dict[str, Any]:
        """Load and parse a .doc.json file.

        Args:
            doc_json_path: Path to the .doc.json file.

        Returns:
            Parsed JSON data as a dictionary.
        """
        with open(doc_json_path, encoding="utf-8") as f:
            data: dict[str, Any] = json.load(f)
            return data

    def _create_directory_structure(self) -> None:
        """Create the skills directory structure.

        Creates the following directories:
        - output_dir/system-overview/
        - output_dir/call-graph/
        - output_dir/datacards/ (if DATACARDS.md exists)
        - output_dir/programs/
        """
        # Create base output directory
        self.output_dir.mkdir(parents=True, exist_ok=True)
        logger.debug(f"Created base output directory: {self.output_dir}")

        # Create standard subdirectories
        subdirs = ["system-overview", "call-graph", "programs"]

        # Conditionally add datacards directory
        if self._has_datacards():
            subdirs.append("datacards")

        for subdir in subdirs:
            path = self.output_dir / subdir
            path.mkdir(parents=True, exist_ok=True)
            logger.debug(f"Created subdirectory: {path}")

    def _find_program_docs(self) -> list[Path]:
        """Find all .doc.json files in the input directory.

        Searches the final/programs directory for documentation files
        generated by War Rig.

        Returns:
            List of paths to .doc.json files, sorted alphabetically.
        """
        programs_dir = self.input_dir / "final" / "programs"

        if not programs_dir.exists():
            logger.warning(f"Programs directory not found: {programs_dir}")
            return []

        doc_files = sorted(programs_dir.glob("*.doc.json"))
        logger.debug(f"Found {len(doc_files)} .doc.json files in {programs_dir}")

        return doc_files

    def _has_system_overview(self) -> bool:
        """Check if SYSTEM_OVERVIEW.md exists.

        Returns:
            True if SYSTEM_OVERVIEW.md exists in the input directory.
        """
        overview_path = self.input_dir / "SYSTEM_OVERVIEW.md"
        exists = overview_path.is_file()
        logger.debug(f"SYSTEM_OVERVIEW.md exists: {exists}")
        return exists

    def _has_call_graph(self) -> bool:
        """Check if CALL_GRAPH.md exists.

        Returns:
            True if CALL_GRAPH.md exists in the input directory.
        """
        call_graph_path = self.input_dir / "CALL_GRAPH.md"
        exists = call_graph_path.is_file()
        logger.debug(f"CALL_GRAPH.md exists: {exists}")
        return exists

    def _has_datacards(self) -> bool:
        """Check if DATACARDS.md exists.

        Returns:
            True if DATACARDS.md exists in the input directory.
        """
        datacards_path = self.input_dir / "DATACARDS.md"
        exists = datacards_path.is_file()
        logger.debug(f"DATACARDS.md exists: {exists}")
        return exists

    def get_system_overview_path(self) -> Path | None:
        """Get the path to SYSTEM_OVERVIEW.md if it exists.

        Returns:
            Path to SYSTEM_OVERVIEW.md, or None if not found.
        """
        path = self.input_dir / "SYSTEM_OVERVIEW.md"
        return path if path.is_file() else None

    def get_call_graph_path(self) -> Path | None:
        """Get the path to CALL_GRAPH.md if it exists.

        Returns:
            Path to CALL_GRAPH.md, or None if not found.
        """
        path = self.input_dir / "CALL_GRAPH.md"
        return path if path.is_file() else None

    def get_datacards_path(self) -> Path | None:
        """Get the path to DATACARDS.md if it exists.

        Returns:
            Path to DATACARDS.md, or None if not found.
        """
        path = self.input_dir / "DATACARDS.md"
        return path if path.is_file() else None
