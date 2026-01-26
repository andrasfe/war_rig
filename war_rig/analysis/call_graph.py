"""Call graph analysis module for War Rig.

Analyzes documentation files to build a comprehensive call graph showing
program relationships, entry points, external dependencies, and gaps.

This module is used by the ticket engine to identify undocumented programs
that should be added to the documentation workflow.
"""

import json
import logging
import re
from collections import defaultdict
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import Any

logger = logging.getLogger(__name__)


# Known system utilities that don't need documentation
SYSTEM_UTILITIES = frozenset({
    # Standard mainframe utilities
    "IEFBR14",      # Do-nothing program
    "IDCAMS",       # Access Method Services (VSAM)
    "IEBGENER",     # Sequential copy utility
    "IEBCOPY",      # PDS copy utility
    "IEBUPDTE",     # Update utility
    "IEBCOMPR",     # Compare utility
    "IEBEDIT",      # JCL edit utility
    "IEBPTPCH",     # Print/punch utility
    "IEHLIST",      # Catalog list utility
    "IEHPROGM",     # Catalog maintenance
    "IEHMOVE",      # Move/copy utility
    "SORT",         # DFSORT utility
    "ICEMAN",       # DFSORT
    "SYNCSORT",     # Syncsort
    "ADRDSSU",      # DFDSS dump/restore
    "IKJEFT01",     # TSO batch terminal monitor
    "IKJEFT1A",     # TSO batch (alternate)
    "IKJEFT1B",     # TSO batch (alternate)
    "IRXJCL",       # REXX in batch
    # COBOL/compiler related
    "IGYCRCTL",     # COBOL compiler
    "IGYWCL",       # COBOL compile/link
    "IGYWCLG",      # COBOL compile/link/go
    "IGYWCG",       # COBOL compile/go
    "IGYWC",        # COBOL compile
    "IGYWCPL",      # COBOL compile/prelink
    "IEWL",         # Linkage editor
    "IEWBLINK",     # Binder
    "HEWL",         # Linkage editor (alias)
    # CICS related
    "DFHECP1$",     # CICS command translator
    "DFHECP1",      # CICS command translator
    "DFHYITVL",     # CICS translator
    "DFH$MOLS",     # CICS utility
    # DB2 related
    "DSNHPC",       # DB2 precompiler
    "DSNH",         # DB2 precompiler
    "DSNUPROC",     # DB2 utility
    "DSNTEP2",      # DB2 SPUFI
    "DSNTEP4",      # DB2 SPUFI
    "DSNTIAD",      # DB2 interactive
    "IKJEFT01",     # TSO (DB2 access)
    # Other system facilities
    "SDSF",         # System Display Search Facility
    "ISPF",         # Interactive System Productivity Facility
    "PDF",          # Program Development Facility
    "ISPSTART",     # ISPF start
    "ISRSUPC",      # SuperC compare
    "ASMA90",       # Assembler
    "ASMA",         # Assembler
    "IEV90",        # Assembler (older)
    "HEWLDRGO",     # Load and go
    "LOADER",       # Loader
    "ICKDSF",       # DASD initialization
    "AMATERSE",     # Terse utility
    "TRSMAIN",      # Terse/unterse
    "IEBIMAGE",     # FCB/UCS images
})

# Pattern to detect dynamic program names (contains parentheses with variables)
DYNAMIC_PROGRAM_PATTERN = re.compile(r"\([A-Z0-9-]+\)")

# Prefixes that indicate IBM system utilities (auto-classification)
SYSTEM_UTILITY_PREFIXES = (
    # IBM system utilities
    "IEF", "IEB", "IEH", "IEV",
    # CICS utilities
    "DFH",
    # DB2 utilities
    "DSN",
    # COBOL compiler and related
    "IGY", "IEW", "HEW",
    # TSO and REXX
    "IKJ", "IRX",
    # DFSORT
    "ICE",
    # DASD/Dataset utilities
    "ADR", "ICK",
    # Assembler
    "ASM",
)


@dataclass
class CallRelationship:
    """Represents a call from one program to another."""
    caller: str
    callee: str
    call_type: str
    is_dynamic: bool = False
    citation: int | None = None


class ResolutionStatus(Enum):
    """Status of a program's documentation resolution."""
    DOCUMENTED = "documented"          # Fully documented from source file
    INTERNAL_ROUTINE = "internal"      # Found as internal routine in parent
    EXTERNAL_MISSING = "external"      # External/missing program
    SYSTEM_UTILITY = "system"          # Known system utility


@dataclass
class ProgramInfo:
    """Information about a documented program."""
    program_id: str
    file_name: str
    file_type: str | None = None
    summary: str | None = None
    calls: list[CallRelationship] = field(default_factory=list)
    called_by: list[str] = field(default_factory=list)
    resolution_status: ResolutionStatus = ResolutionStatus.DOCUMENTED
    parent_program: str | None = None  # For internal routines


@dataclass
class ResolvedProgram:
    """A program that was resolved through discovery workflow."""
    program_id: str
    status: ResolutionStatus
    parent_program: str | None = None  # For internal routines
    called_by: list[str] = field(default_factory=list)
    explanation: str | None = None


@dataclass
class CallGraphAnalysis:
    """Results of call graph analysis."""
    documented_programs: dict[str, ProgramInfo]
    external_dependencies: set[str]
    system_utilities: set[str]
    custom_missing: set[str]  # External deps that are NOT system utilities
    entry_points: set[str]
    leaf_nodes: set[str]
    call_chains: list[list[str]]
    dynamic_calls: list[CallRelationship]
    total_calls: int
    auto_classified_utilities: set[str] = field(default_factory=set)  # Auto-detected by prefix
    resolved_programs: dict[str, ResolvedProgram] = field(default_factory=dict)  # Discovery results

    def has_gaps(self) -> bool:
        """Check if there are undocumented custom programs."""
        return len(self.custom_missing) > 0

    def get_missing_for_documentation(self) -> list[str]:
        """Get list of custom programs that need documentation."""
        return sorted(self.custom_missing)


class CallGraphAnalyzer:
    """Analyzes documentation files to build call graphs.

    The analyzer supports two data sources:
    1. Citadel dependency_graph.json (preferred) - Accurate static analysis
    2. .doc.json files (fallback) - Documentation-based extraction

    Usage:
        # With Citadel graph (recommended)
        analyzer = CallGraphAnalyzer(output_directory)
        analysis = analyzer.analyze(dependency_graph_path=Path("dependency_graph.json"))

        # Fallback to doc.json parsing
        analyzer = CallGraphAnalyzer(output_directory)
        analysis = analyzer.analyze()

        if analysis.has_gaps():
            for program in analysis.get_missing_for_documentation():
                # Create documentation ticket for program
                pass
    """

    def __init__(
        self,
        doc_directory: Path,
        additional_system_utilities: set[str] | None = None,
    ):
        """Initialize the analyzer.

        Args:
            doc_directory: Directory containing .doc.json files
            additional_system_utilities: Extra utilities to skip (beyond defaults)
        """
        self.doc_directory = Path(doc_directory)
        self.system_utilities = SYSTEM_UTILITIES.copy()
        if additional_system_utilities:
            self.system_utilities = self.system_utilities | additional_system_utilities
        # Internal graph structures populated by load_from_citadel_graph
        self._citadel_programs: dict[str, ProgramInfo] = {}
        self._citadel_calls: list[CallRelationship] = []
        self._citadel_loaded: bool = False

    def load_from_citadel_graph(self, graph_path: Path) -> None:
        """Load call relationships from Citadel dependency graph.

        Parses the Citadel-generated dependency_graph.json file which contains
        accurate static analysis of program artifacts and their relationships.

        The Citadel graph structure is:
        {
            "artifacts": {
                "artifact_id": {
                    "id": "procedure::PROGRAM1",
                    "artifact_type": "procedure|paragraph|program",
                    "canonical_name": "PROGRAM1",
                    "defined_in": {
                        "file_path": "/path/to/file.cbl",
                        "line_start": 1
                    },
                    "language": "COBOL|JCL"
                }
            },
            "relationships": [
                {
                    "from_artifact": "artifact_id",
                    "to_artifact": "artifact_id",
                    "relationship_type": "calls",
                    "location": {"line_start": 123}
                }
            ]
        }

        Args:
            graph_path: Path to the Citadel dependency_graph.json file.

        Raises:
            FileNotFoundError: If the graph file does not exist.
            json.JSONDecodeError: If the file is not valid JSON.
        """
        logger.info(f"Loading Citadel dependency graph from {graph_path}")

        data = json.loads(graph_path.read_text(encoding="utf-8"))
        artifacts = data.get("artifacts", {})
        relationships = data.get("relationships", [])

        logger.debug(f"Citadel graph: {len(artifacts)} artifacts, {len(relationships)} relationships")

        # Build mapping from file path to parent procedure/job
        # This is used to resolve paragraphs (JCL steps) to their parent job
        self._file_to_parent: dict[str, str] = {}

        # Build program info from artifacts
        self._citadel_programs = {}
        for artifact_id, artifact in artifacts.items():
            # Extract the canonical name (program/procedure name)
            canonical_name = artifact.get("canonical_name", "")
            if not canonical_name:
                continue

            # Get artifact type and file info
            artifact_type = artifact.get("artifact_type", "unknown")
            defined_in = artifact.get("defined_in", {})
            file_path = defined_in.get("file_path", "")
            file_name = Path(file_path).name if file_path else ""
            language = artifact.get("language", "")

            # Map language to file type
            file_type = self._map_language_to_file_type(language)

            # Create ProgramInfo - use canonical_name as program_id
            # Skip paragraphs for now as they are internal, but include procedures, programs, and copybooks
            if artifact_type in ("procedure", "program", "job", "copybook"):
                # For JCL jobs, use file name for more intuitive identification
                program_id = canonical_name
                if artifact_type == "job" and file_path:
                    program_id = Path(file_path).stem.upper()

                program_info = ProgramInfo(
                    program_id=program_id,
                    file_name=file_name,
                    file_type=file_type,
                    summary=None,  # Citadel doesn't provide summaries
                    resolution_status=ResolutionStatus.DOCUMENTED,
                )
                self._citadel_programs[program_id] = program_info

                # Map file path to this procedure/job for paragraph resolution
                if file_path:
                    self._file_to_parent[file_path] = program_id

                # Also store the artifact_id -> program_id mapping for relationship resolution
                if artifact_id != program_id:
                    # Store with artifact_id prefix for relationship lookup
                    self._citadel_programs[artifact_id] = program_info

        # Build call relationships from Citadel relationships
        self._citadel_calls = []
        for rel in relationships:
            rel_type = rel.get("relationship_type", "")

            # Only process call-like and include relationships
            if rel_type not in ("calls", "performs", "invokes", "executes", "includes"):
                continue

            from_artifact = rel.get("from_artifact", "")
            to_artifact = rel.get("to_artifact", "")
            location = rel.get("location", {})
            line = location.get("line_start")

            # Resolve artifact IDs to canonical names
            caller = self._resolve_artifact_name(from_artifact, artifacts)
            callee = self._resolve_artifact_name(to_artifact, artifacts)

            if not caller or not callee:
                continue

            # Normalize callee name
            callee = callee.strip().rstrip("$").upper()

            # Check for dynamic calls
            is_dynamic = bool(DYNAMIC_PROGRAM_PATTERN.search(callee))

            # Map relationship type to call type
            call_type = self._map_relationship_to_call_type(rel_type)

            relationship = CallRelationship(
                caller=caller,
                callee=callee,
                call_type=call_type,
                is_dynamic=is_dynamic,
                citation=line,
            )
            self._citadel_calls.append(relationship)

        # Also process unresolved references to capture external dependencies
        # These are calls to programs/procedures that weren't found in the codebase
        unresolved = data.get("unresolved", [])
        for unres in unresolved:
            # Only process procedure/program references (skip data references)
            expected_type = unres.get("expected_type", "")
            if expected_type not in ("procedure", "program"):
                continue

            reference_text = unres.get("reference_text", "")
            if not reference_text:
                continue

            containing_artifact = unres.get("containing_artifact", "")
            location = unres.get("location", {})
            line = location.get("line_start")

            # Resolve the containing artifact to get the caller
            caller = self._resolve_artifact_name(containing_artifact, artifacts)
            if not caller:
                continue

            # Normalize callee name
            callee = reference_text.strip().rstrip("$").upper()

            # Check for dynamic calls
            is_dynamic = bool(DYNAMIC_PROGRAM_PATTERN.search(callee))

            relationship = CallRelationship(
                caller=caller,
                callee=callee,
                call_type="EXEC",  # Most unresolved are EXEC calls
                is_dynamic=is_dynamic,
                citation=line,
            )
            self._citadel_calls.append(relationship)

        self._citadel_loaded = True
        logger.info(
            f"Loaded {len(self._citadel_programs)} programs and "
            f"{len(self._citadel_calls)} call relationships from Citadel graph"
        )

    def _resolve_artifact_name(self, artifact_id: str, artifacts: dict) -> str | None:
        """Resolve an artifact ID to its canonical name.

        For paragraphs (JCL steps), resolves to the parent file name
        since steps are internal to their parent and we want to track
        program-to-program relationships using intuitive file names.

        Args:
            artifact_id: The artifact ID (e.g., "procedure::BUILDBAT")
            artifacts: The artifacts dictionary from the Citadel graph

        Returns:
            The canonical name or None if not found
        """
        if artifact_id in artifacts:
            artifact = artifacts[artifact_id]
            artifact_type = artifact.get("artifact_type", "")
            defined_in = artifact.get("defined_in", {})
            file_path = defined_in.get("file_path", "")

            # For paragraphs (JCL steps), resolve to file name
            # This gives more intuitive names like BATCMP instead of CNJBATMP
            if artifact_type == "paragraph" and file_path:
                file_name = Path(file_path).stem
                return file_name.upper()

            # For jobs in JCL, also use file name for consistency
            if artifact_type == "job" and file_path:
                file_name = Path(file_path).stem
                return file_name.upper()

            return artifact.get("canonical_name", "")

        # Try to extract name from the ID format (e.g., "procedure::NAME")
        if "::" in artifact_id:
            return artifact_id.split("::", 1)[1]

        return artifact_id if artifact_id else None

    def _map_language_to_file_type(self, language: str) -> str | None:
        """Map Citadel language to file type.

        Args:
            language: The language from Citadel (e.g., "COBOL", "JCL")

        Returns:
            The file type string
        """
        mapping = {
            "COBOL": "COBOL",
            "JCL": "JCL",
            "COPYBOOK": "COPYBOOK",
            "BMS": "BMS",
            "SQL": "SQL",
        }
        return mapping.get(language.upper() if language else "", None)

    def _map_relationship_to_call_type(self, rel_type: str) -> str:
        """Map Citadel relationship type to call type.

        Args:
            rel_type: The relationship type from Citadel

        Returns:
            The call type string
        """
        mapping = {
            "calls": "CALL",
            "performs": "PERFORM",
            "invokes": "INVOKE",
            "executes": "EXEC",
            "includes": "COPY",
        }
        return mapping.get(rel_type.lower(), "CALL")

    def analyze(
        self,
        dependency_graph_path: Path | None = None,
    ) -> CallGraphAnalysis:
        """Perform call graph analysis.

        This method supports two data sources:
        1. Citadel dependency_graph.json (preferred) - Uses accurate static analysis
        2. .doc.json files (fallback) - Parses documentation for call relationships

        When dependency_graph_path is provided and exists, the Citadel graph is used
        to supplement or replace the doc.json-based analysis. The Citadel graph
        provides more accurate call relationships from static analysis.

        Args:
            dependency_graph_path: Optional path to Citadel dependency_graph.json.
                If provided and exists, loads call relationships from static analysis.
                Falls back to doc.json parsing if not available.

        Returns:
            CallGraphAnalysis with all findings
        """
        # Try to load Citadel graph if path provided
        use_citadel = False
        if dependency_graph_path and dependency_graph_path.exists():
            try:
                self.load_from_citadel_graph(dependency_graph_path)
                use_citadel = self._citadel_loaded
                if use_citadel:
                    logger.info("Using Citadel dependency graph for call analysis")
            except (json.JSONDecodeError, IOError, KeyError) as e:
                logger.warning(f"Failed to load Citadel graph, falling back to doc.json: {e}")
                use_citadel = False

        # Load documentation files (needed for program summaries even with Citadel)
        docs = self._load_documentation()

        if use_citadel and self._citadel_loaded:
            # Use Citadel graph for call relationships, docs for metadata
            return self._analyze_with_citadel(docs)

        # Fallback: analyze from doc.json files only
        if not docs:
            logger.warning(f"No documentation files found in {self.doc_directory}")
            return self._empty_analysis()

        logger.info(f"Analyzing {len(docs)} documentation files (doc.json fallback)")
        return self._analyze_from_docs(docs)

    def _analyze_with_citadel(self, docs: list[dict[str, Any]]) -> CallGraphAnalysis:
        """Analyze using Citadel graph with doc.json for supplementary metadata.

        Args:
            docs: Documentation files for extracting summaries and metadata

        Returns:
            CallGraphAnalysis from Citadel static analysis
        """
        # Build program info from docs for summaries/metadata
        doc_programs, resolved_programs = self._build_program_info(docs) if docs else ({}, {})

        # Merge Citadel programs with doc metadata
        programs = {}
        for program_id, citadel_info in self._citadel_programs.items():
            # Skip artifact IDs (those with ::)
            if "::" in program_id:
                continue

            # Merge with doc info if available
            if program_id in doc_programs:
                doc_info = doc_programs[program_id]
                programs[program_id] = ProgramInfo(
                    program_id=program_id,
                    file_name=citadel_info.file_name or doc_info.file_name,
                    file_type=citadel_info.file_type or doc_info.file_type,
                    summary=doc_info.summary,  # Get summary from docs
                    resolution_status=doc_info.resolution_status,
                    parent_program=doc_info.parent_program,
                )
            else:
                programs[program_id] = citadel_info

        # Use Citadel call relationships
        all_calls = [c for c in self._citadel_calls if not c.is_dynamic]
        dynamic_calls = [c for c in self._citadel_calls if c.is_dynamic]

        # Update program info with calls
        for call in all_calls:
            if call.caller in programs:
                programs[call.caller].calls.append(call)
            if call.callee in programs:
                programs[call.callee].called_by.append(call.caller)

        # Continue with standard analysis flow
        documented_ids = set(programs.keys())
        all_callees = {call.callee for call in all_calls}

        # Identify external dependencies
        documented_ids = set(programs.keys())
        all_callees = {call.callee for call in all_calls if not call.is_dynamic}
        external_deps = all_callees - documented_ids

        # Classify external dependencies (with auto-detection)
        system_utils = set()
        auto_classified = set()
        custom_missing = set()

        for dep in external_deps:
            if dep in self.system_utilities:
                system_utils.add(dep)
            elif self._detect_system_utility(dep):
                # Auto-classified based on prefix pattern
                system_utils.add(dep)
                auto_classified.add(dep)
            else:
                custom_missing.add(dep)

        if auto_classified:
            logger.info(f"Auto-classified {len(auto_classified)} utilities: {sorted(auto_classified)}")

        # Find entry points and leaf nodes
        entry_points = self._find_entry_points(programs, documented_ids)
        leaf_nodes = self._find_leaf_nodes(programs, documented_ids)

        # Build call chains
        call_chains = self._build_call_chains(programs, documented_ids)

        logger.info(f"Found {len(external_deps)} external dependencies")
        logger.info(f"  - {len(system_utils)} system utilities (skipped)")
        logger.info(f"  - {len(custom_missing)} custom programs (need documentation)")

        return CallGraphAnalysis(
            documented_programs=programs,
            external_dependencies=external_deps,
            system_utilities=system_utils,
            custom_missing=custom_missing,
            entry_points=entry_points,
            leaf_nodes=leaf_nodes,
            call_chains=call_chains,
            dynamic_calls=dynamic_calls,
            total_calls=len(all_calls),
            auto_classified_utilities=auto_classified,
            resolved_programs=resolved_programs,
        )

    def _analyze_from_docs(self, docs: list[dict[str, Any]]) -> CallGraphAnalysis:
        """Analyze using only doc.json files (fallback when Citadel unavailable).

        Args:
            docs: Documentation files to analyze

        Returns:
            CallGraphAnalysis from documentation parsing
        """
        # Build program info
        programs, resolved_programs = self._build_program_info(docs)

        # Extract call relationships
        all_calls, dynamic_calls = self._extract_calls(docs, programs)

        # Identify external dependencies
        documented_ids = set(programs.keys())
        all_callees = {call.callee for call in all_calls if not call.is_dynamic}
        external_deps = all_callees - documented_ids

        # Classify external dependencies (with auto-detection)
        system_utils = set()
        auto_classified = set()
        custom_missing = set()

        for dep in external_deps:
            if dep in self.system_utilities:
                system_utils.add(dep)
            elif self._detect_system_utility(dep):
                # Auto-classified based on prefix pattern
                system_utils.add(dep)
                auto_classified.add(dep)
            else:
                custom_missing.add(dep)

        if auto_classified:
            logger.info(f"Auto-classified {len(auto_classified)} utilities: {sorted(auto_classified)}")

        # Find entry points and leaf nodes
        entry_points = self._find_entry_points(programs, documented_ids)
        leaf_nodes = self._find_leaf_nodes(programs, documented_ids)

        # Build call chains
        call_chains = self._build_call_chains(programs, documented_ids)

        logger.info(f"Found {len(external_deps)} external dependencies")
        logger.info(f"  - {len(system_utils)} system utilities (skipped)")
        logger.info(f"  - {len(custom_missing)} custom programs (need documentation)")

        return CallGraphAnalysis(
            documented_programs=programs,
            external_dependencies=external_deps,
            system_utilities=system_utils,
            custom_missing=custom_missing,
            entry_points=entry_points,
            leaf_nodes=leaf_nodes,
            call_chains=call_chains,
            dynamic_calls=dynamic_calls,
            total_calls=len(all_calls),
            auto_classified_utilities=auto_classified,
            resolved_programs=resolved_programs,
        )

    def _empty_analysis(self) -> CallGraphAnalysis:
        """Return empty analysis when no docs found."""
        return CallGraphAnalysis(
            documented_programs={},
            external_dependencies=set(),
            system_utilities=set(),
            custom_missing=set(),
            entry_points=set(),
            leaf_nodes=set(),
            call_chains=[],
            dynamic_calls=[],
            total_calls=0,
            auto_classified_utilities=set(),
            resolved_programs={},
        )

    def _load_documentation(self) -> list[dict[str, Any]]:
        """Load all .doc.json files from directory (recursively)."""
        docs = []
        for doc_file in sorted(self.doc_directory.glob("**/*.doc.json")):
            try:
                data = json.loads(doc_file.read_text())
                data["_source_file"] = doc_file.name
                data["_source_path"] = str(doc_file)
                docs.append(data)
            except (json.JSONDecodeError, IOError) as e:
                logger.warning(f"Could not load {doc_file}: {e}")
        return docs

    def _get_program_id(self, doc: dict) -> str:
        """Extract program ID from documentation."""
        if "header" in doc and doc["header"]:
            if isinstance(doc["header"], dict):
                return doc["header"].get("program_id") or doc["header"].get("job_name") or ""
        if "program_id" in doc:
            return doc["program_id"]
        return doc.get("_source_file", "Unknown").replace(".doc.json", "")

    def _get_summary(self, doc: dict) -> str | None:
        """Extract summary from documentation."""
        if "purpose" in doc and isinstance(doc["purpose"], dict):
            return doc["purpose"].get("summary")
        return doc.get("summary")

    def _build_program_info(
        self,
        docs: list[dict],
    ) -> tuple[dict[str, ProgramInfo], dict[str, ResolvedProgram]]:
        """Build program info from documentation.

        Returns:
            Tuple of (programs dict, resolved_programs dict)
        """
        programs = {}
        resolved_programs = {}

        for doc in docs:
            program_id = self._get_program_id(doc)
            if not program_id:
                continue

            file_type = None
            resolution_status = ResolutionStatus.DOCUMENTED
            parent_program = None

            if "header" in doc and isinstance(doc["header"], dict):
                file_type = doc["header"].get("file_type")

            # Check for discovery metadata
            discovery_result = doc.get("discovery_result")
            if discovery_result:
                status = discovery_result.get("status")
                if status == "internal_routine":
                    resolution_status = ResolutionStatus.INTERNAL_ROUTINE
                    parent_program = discovery_result.get("parent_file")
                    resolved_programs[program_id] = ResolvedProgram(
                        program_id=program_id,
                        status=ResolutionStatus.INTERNAL_ROUTINE,
                        parent_program=parent_program,
                        explanation=discovery_result.get("likely_explanation"),
                    )
                elif status == "external":
                    resolution_status = ResolutionStatus.EXTERNAL_MISSING
                    resolved_programs[program_id] = ResolvedProgram(
                        program_id=program_id,
                        status=ResolutionStatus.EXTERNAL_MISSING,
                        explanation=discovery_result.get("likely_explanation"),
                    )
                elif status == "system_utility":
                    resolution_status = ResolutionStatus.SYSTEM_UTILITY
                    resolved_programs[program_id] = ResolvedProgram(
                        program_id=program_id,
                        status=ResolutionStatus.SYSTEM_UTILITY,
                    )

            programs[program_id] = ProgramInfo(
                program_id=program_id,
                file_name=doc.get("_source_file", ""),
                file_type=file_type,
                summary=self._get_summary(doc),
                resolution_status=resolution_status,
                parent_program=parent_program,
            )

        return programs, resolved_programs

    def _extract_calls(
        self,
        docs: list[dict],
        programs: dict[str, ProgramInfo]
    ) -> tuple[list[CallRelationship], list[CallRelationship]]:
        """Extract call relationships from documentation."""
        all_calls = []
        dynamic_calls = []

        for doc in docs:
            caller = self._get_program_id(doc)
            if not caller:
                continue

            called_programs = doc.get("called_programs", [])
            if not called_programs:
                continue

            for call_info in called_programs:
                if isinstance(call_info, dict):
                    callee = call_info.get("program_name", "")
                    call_type = call_info.get("call_type", "UNKNOWN")
                    citation = call_info.get("citation")
                elif isinstance(call_info, str):
                    callee = call_info
                    call_type = "UNKNOWN"
                    citation = None
                else:
                    continue

                if not callee:
                    continue

                # Normalize callee name (remove trailing special chars)
                callee = callee.strip().rstrip("$").upper()

                is_dynamic = bool(DYNAMIC_PROGRAM_PATTERN.search(callee))

                relationship = CallRelationship(
                    caller=caller,
                    callee=callee,
                    call_type=call_type,
                    is_dynamic=is_dynamic,
                    citation=citation,
                )

                if is_dynamic:
                    dynamic_calls.append(relationship)
                else:
                    all_calls.append(relationship)

                    # Update program info
                    if caller in programs:
                        programs[caller].calls.append(relationship)
                    if callee in programs:
                        programs[callee].called_by.append(caller)

        return all_calls, dynamic_calls

    def _find_entry_points(
        self,
        programs: dict[str, ProgramInfo],
        documented_ids: set[str]
    ) -> set[str]:
        """Find programs with no documented callers."""
        entry_points = set()
        for program_id, info in programs.items():
            documented_callers = [c for c in info.called_by if c in documented_ids]
            if not documented_callers:
                entry_points.add(program_id)
        return entry_points

    def _find_leaf_nodes(
        self,
        programs: dict[str, ProgramInfo],
        documented_ids: set[str]
    ) -> set[str]:
        """Find programs that don't call other documented programs."""
        leaf_nodes = set()
        for program_id, info in programs.items():
            documented_calls = [c.callee for c in info.calls if c.callee in documented_ids]
            if not documented_calls:
                leaf_nodes.add(program_id)
        return leaf_nodes

    def _build_call_chains(
        self,
        programs: dict[str, ProgramInfo],
        documented_ids: set[str],
        max_depth: int = 10
    ) -> list[list[str]]:
        """Build representative call chains from entry points."""
        entry_points = self._find_entry_points(programs, documented_ids)
        chains = []
        visited_chains: set[tuple[str, ...]] = set()

        def dfs_chain(current: str, path: list[str], depth: int) -> None:
            if depth > max_depth:
                return

            if current not in programs:
                return

            documented_calls = [
                c.callee for c in programs[current].calls
                if c.callee in documented_ids
            ]

            if not documented_calls:
                if len(path) > 1:
                    chain_tuple = tuple(path)
                    if chain_tuple not in visited_chains:
                        visited_chains.add(chain_tuple)
                        chains.append(path.copy())
                return

            for callee in documented_calls:
                if callee not in path:  # Avoid cycles
                    path.append(callee)
                    dfs_chain(callee, path, depth + 1)
                    path.pop()

        for entry in sorted(entry_points):
            dfs_chain(entry, [entry], 0)

        return chains

    def is_system_utility(self, program_name: str) -> bool:
        """Check if a program is a known system utility."""
        return program_name.upper() in self.system_utilities

    def _detect_system_utility(self, program_name: str) -> bool:
        """Auto-detect if a program is a system utility based on naming patterns.

        Returns True if the program matches known IBM system utility prefixes.
        """
        name_upper = program_name.upper()
        # Check explicit list first
        if name_upper in self.system_utilities:
            return True
        # Check prefixes for auto-classification
        return name_upper.startswith(SYSTEM_UTILITY_PREFIXES)

    def generate_markdown_report(self, analysis: CallGraphAnalysis) -> str:
        """Generate a markdown report of the call graph analysis."""
        lines = []

        # Header
        lines.append("# Call Graph Analysis")
        lines.append("")
        lines.append(f"*Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}*")
        lines.append("")
        lines.append(f"**Programs Analyzed:** {len(analysis.documented_programs)}")
        lines.append("")

        # Mermaid diagram
        lines.append("## Visual Call Graph")
        lines.append("")
        lines.append(self._generate_mermaid_diagram(analysis))
        lines.append("")

        # Entry points
        lines.append("## Entry Points")
        lines.append("")
        if analysis.entry_points:
            for prog in sorted(analysis.entry_points):
                info = analysis.documented_programs.get(prog)
                summary = info.summary[:80] + "..." if info and info.summary and len(info.summary) > 80 else (info.summary if info else "")
                # Get status icon
                if info and info.resolution_status == ResolutionStatus.INTERNAL_ROUTINE:
                    icon = "~"
                elif info and info.resolution_status == ResolutionStatus.EXTERNAL_MISSING:
                    icon = "✗"
                elif info and info.resolution_status == ResolutionStatus.SYSTEM_UTILITY:
                    icon = "⚙"
                else:
                    icon = "✓"
                lines.append(f"- {icon} **{prog}**: {summary or 'No summary'}")
        else:
            lines.append("*No entry points found*")
        lines.append("")

        # External dependencies
        lines.append("## External Dependencies")
        lines.append("")
        lines.append("### System Utilities (Skipped)")
        lines.append("")
        if analysis.system_utilities:
            known_utils = analysis.system_utilities - analysis.auto_classified_utilities
            if known_utils:
                lines.append("**Known utilities:**")
                for prog in sorted(known_utils):
                    lines.append(f"- {prog}")
                lines.append("")
            if analysis.auto_classified_utilities:
                lines.append("**Auto-classified by prefix pattern:**")
                for prog in sorted(analysis.auto_classified_utilities):
                    lines.append(f"- {prog}")
                lines.append("")
        else:
            lines.append("*None*")
            lines.append("")

        # Resolved programs (from discovery workflow)
        if analysis.resolved_programs:
            lines.append("### Resolved Through Discovery")
            lines.append("")
            lines.append("| Status | Program | Resolution |")
            lines.append("|--------|---------|------------|")
            for prog_id in sorted(analysis.resolved_programs.keys()):
                resolved = analysis.resolved_programs[prog_id]
                if resolved.status == ResolutionStatus.INTERNAL_ROUTINE:
                    status_icon = "~"
                    resolution = f"Internal routine in {resolved.parent_program}"
                elif resolved.status == ResolutionStatus.EXTERNAL_MISSING:
                    status_icon = "✗"
                    resolution = resolved.explanation or "External/missing"
                elif resolved.status == ResolutionStatus.SYSTEM_UTILITY:
                    status_icon = "⚙"
                    resolution = "System utility"
                else:
                    status_icon = "✓"
                    resolution = "Documented"
                lines.append(f"| {status_icon} | {prog_id} | {resolution} |")
            lines.append("")

        lines.append("### Custom Programs (Need Documentation)")
        lines.append("")
        # Exclude programs already in resolved_programs
        truly_missing = analysis.custom_missing - set(analysis.resolved_programs.keys())
        if truly_missing:
            for prog in sorted(truly_missing):
                # Find who calls this
                callers = []
                for pid, info in analysis.documented_programs.items():
                    for call in info.calls:
                        if call.callee == prog:
                            callers.append(f"{pid} ({call.call_type})")
                callers_str = ", ".join(callers) if callers else "Unknown"
                lines.append(f"- **{prog}**: Called by {callers_str}")
        else:
            lines.append("*None - call graph is complete!*")
        lines.append("")

        # Statistics
        lines.append("## Statistics")
        lines.append("")
        lines.append("| Metric | Count |")
        lines.append("|--------|-------|")
        lines.append(f"| Documented Programs | {len(analysis.documented_programs)} |")
        lines.append(f"| Entry Points | {len(analysis.entry_points)} |")
        lines.append(f"| Leaf Nodes | {len(analysis.leaf_nodes)} |")
        lines.append(f"| External Dependencies | {len(analysis.external_dependencies)} |")
        lines.append(f"| System Utilities | {len(analysis.system_utilities)} |")
        lines.append(f"| Auto-classified | {len(analysis.auto_classified_utilities)} |")

        # Count resolved by status
        internal_routines = sum(
            1 for r in analysis.resolved_programs.values()
            if r.status == ResolutionStatus.INTERNAL_ROUTINE
        )
        external_resolved = sum(
            1 for r in analysis.resolved_programs.values()
            if r.status == ResolutionStatus.EXTERNAL_MISSING
        )
        truly_missing = len(analysis.custom_missing) - len(analysis.resolved_programs)

        if analysis.resolved_programs:
            lines.append(f"| Resolved (Internal) | {internal_routines} |")
            lines.append(f"| Resolved (External) | {external_resolved} |")

        lines.append(f"| Custom Missing | {truly_missing} |")
        lines.append(f"| Total Calls | {analysis.total_calls} |")
        lines.append("")

        # Legend
        lines.append("### Status Legend")
        lines.append("")
        lines.append("- ✓ **Documented**: Fully documented from source file")
        lines.append("- ~ **Internal**: Found as routine/section in parent program")
        lines.append("- ✗ **External**: External or missing program")
        lines.append("- ⚙ **System**: Known system utility")
        lines.append("")

        return "\n".join(lines)

    def _generate_mermaid_diagram(self, analysis: CallGraphAnalysis) -> str:
        """Generate Mermaid flowchart diagram."""
        lines = ["```mermaid", "flowchart TD"]

        def sanitize_id(name: str) -> str:
            sanitized = name.replace("-", "_").replace("$", "")
            sanitized = re.sub(r"\([^)]*\)", "", sanitized)
            sanitized = re.sub(r"[^A-Za-z0-9_]", "", sanitized)
            return sanitized

        # Identify copybooks (file_type == "COPYBOOK") for separate rendering
        copybook_ids = {
            prog_id for prog_id, info in analysis.documented_programs.items()
            if info.file_type == "COPYBOOK"
        }

        # Identify programs that make calls (callers) vs those that are only called
        callers = set()
        callees = set()
        for prog_id, info in analysis.documented_programs.items():
            for call in info.calls:
                if call.callee not in analysis.system_utilities:
                    callers.add(prog_id)
                    callees.add(call.callee)

        # Programs that are called but also documented (excluding copybooks)
        called_documented = (callees & set(analysis.documented_programs.keys())) - copybook_ids
        # Copybooks that are included
        called_copybooks = callees & copybook_ids
        # Programs that only call others (entry points that make calls)
        only_callers = callers - callees
        # Programs with no connections
        isolated = set(analysis.documented_programs.keys()) - callers - callees - copybook_ids

        # Subgraph: Jobs/Entry Points (callers)
        if only_callers or (callers - called_documented - copybook_ids):
            lines.append("")
            lines.append("    subgraph jobs[\" \"]")
            for prog in sorted(only_callers | (callers - called_documented - copybook_ids)):
                node_id = sanitize_id(prog)
                lines.append(f"        {node_id}([{prog}])")
            lines.append("    end")

        # Subgraph: Procedures (documented callees, excluding copybooks)
        if called_documented:
            lines.append("")
            lines.append("    subgraph procs[\" \"]")
            for prog in sorted(called_documented):
                node_id = sanitize_id(prog)
                if prog in analysis.leaf_nodes:
                    lines.append(f"        {node_id}[/{prog}/]")
                else:
                    lines.append(f"        {node_id}[{prog}]")
            lines.append("    end")

        # Single copybook node listing all included copybooks
        if called_copybooks:
            lines.append("")
            copybook_label = "<br>".join(sorted(called_copybooks))
            lines.append(f"    COPYBOOKS[/{copybook_label}/]")

        # Subgraph: Missing/External programs
        if analysis.custom_missing:
            lines.append("")
            lines.append("    subgraph external[\" \"]")
            for prog in sorted(analysis.custom_missing):
                node_id = sanitize_id(prog)
                lines.append(f"        {node_id}>{prog}]")
            lines.append("    end")

        # Edges
        lines.append("")
        lines.append("    %% Call relationships")
        copy_edges_emitted: set[str] = set()
        for prog_id, info in sorted(analysis.documented_programs.items()):
            caller_id = sanitize_id(prog_id)
            for call in info.calls:
                # Skip system utilities in diagram
                if call.callee in analysis.system_utilities:
                    continue
                callee_id = sanitize_id(call.callee)
                if call.call_type == "COPY":
                    # One edge per caller to the shared COPYBOOKS node
                    if caller_id not in copy_edges_emitted:
                        copy_edges_emitted.add(caller_id)
                        lines.append(f"    {caller_id} -.->|COPY| COPYBOOKS")
                elif "XCTL" in call.call_type:
                    lines.append(f"    {caller_id} -.->|XCTL| {callee_id}")
                elif "LINK" in call.call_type:
                    lines.append(f"    {caller_id} -->|LINK| {callee_id}")
                else:
                    lines.append(f"    {caller_id} --> {callee_id}")

        # Styling
        lines.append("")
        lines.append("    %% Styling")
        if analysis.entry_points:
            ids = [sanitize_id(p) for p in sorted(analysis.entry_points)]
            lines.append("    classDef entryPoint fill:#90EE90,stroke:#228B22")
            lines.append(f"    class {','.join(ids)} entryPoint")

        # Copybooks
        if called_copybooks:
            lines.append("    classDef copybook fill:#E8F5E9,stroke:#4CAF50")
            lines.append("    class COPYBOOKS copybook")

        # Truly missing (not resolved)
        truly_missing = analysis.custom_missing - set(analysis.resolved_programs.keys())
        if truly_missing:
            ids = [sanitize_id(p) for p in sorted(truly_missing)]
            lines.append("    classDef missing fill:#1E3A5F,stroke:#2E5A8F,color:#FFFFFF")
            lines.append(f"    class {','.join(ids)} missing")

        # Internal routines (resolved)
        internal_routines = [
            p for p, r in analysis.resolved_programs.items()
            if r.status == ResolutionStatus.INTERNAL_ROUTINE
        ]
        if internal_routines:
            ids = [sanitize_id(p) for p in sorted(internal_routines)]
            lines.append("    classDef internal fill:#FFF8DC,stroke:#DAA520")
            lines.append(f"    class {','.join(ids)} internal")

        # External resolved (documented as missing)
        external_resolved = [
            p for p, r in analysis.resolved_programs.items()
            if r.status == ResolutionStatus.EXTERNAL_MISSING
        ]
        if external_resolved:
            ids = [sanitize_id(p) for p in sorted(external_resolved)]
            lines.append("    classDef external fill:#E6E6FA,stroke:#9370DB")
            lines.append(f"    class {','.join(ids)} external")

        lines.append("```")
        return "\n".join(lines)

    def generate_system_design_md(
        self,
        analysis: CallGraphAnalysis,
        sequence_diagrams: list[str] | None = None,
    ) -> str:
        """Generate SYSTEM_DESIGN.md content documenting the architecture.

        This method generates a basic system design document with the call graph
        mermaid diagram. The Imperator's holistic review may later enhance this
        document with LLM-generated content, but it will use the same mermaid
        diagram from CALL_GRAPH.md for consistency.

        Args:
            analysis: The call graph analysis results.
            sequence_diagrams: Optional list of Mermaid sequence diagram strings
                from citadel.get_sequence_diagrams(). If provided and non-empty,
                a "## Flows" section will be added showing key call sequences.

        Returns:
            The generated SYSTEM_DESIGN.md content as a string.
        """
        lines = []

        lines.append("# System Design")
        lines.append("")
        lines.append(f"*Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}*")
        lines.append("")

        lines.append("## Overview")
        lines.append("")
        lines.append(f"This document describes the architecture of the analyzed codebase,")
        lines.append(f"including {len(analysis.documented_programs)} documented programs ")
        lines.append(f"and their dependencies.")
        lines.append("")

        # Architecture diagram (same as in CALL_GRAPH.md for consistency)
        lines.append("## Architecture Overview")
        lines.append("")
        lines.append(self._generate_mermaid_diagram(analysis))
        lines.append("")

        # Flows section with sequence diagrams (if provided)
        if sequence_diagrams:
            lines.append("## Flows")
            lines.append("")
            lines.append("The following sequence diagrams illustrate key call sequences")
            lines.append("identified in the codebase, showing how programs interact")
            lines.append("during execution.")
            lines.append("")
            for i, diagram in enumerate(sequence_diagrams, start=1):
                lines.append(f"### Flow {i}")
                lines.append("")
                lines.append(diagram)
                lines.append("")

        # Program inventory
        lines.append("## Program Inventory")
        lines.append("")
        lines.append("| Status | Program | Type | Summary |")
        lines.append("|--------|---------|------|---------|")
        for prog_id in sorted(analysis.documented_programs.keys()):
            info = analysis.documented_programs[prog_id]
            file_type = info.file_type or "Unknown"
            summary = info.summary or ""
            # Get status icon
            if info.resolution_status == ResolutionStatus.INTERNAL_ROUTINE:
                icon = "~"
            elif info.resolution_status == ResolutionStatus.EXTERNAL_MISSING:
                icon = "✗"
            elif info.resolution_status == ResolutionStatus.SYSTEM_UTILITY:
                icon = "⚙"
            else:
                icon = "✓"
            lines.append(f"| {icon} | {prog_id} | {file_type} | {summary} |")
        lines.append("")

        # External dependencies
        lines.append("## External Dependencies")
        lines.append("")
        lines.append("### System Utilities")
        lines.append("")
        lines.append("The following system utilities are called but not documented:")
        lines.append("")
        known_utils = analysis.system_utilities - analysis.auto_classified_utilities
        if known_utils:
            for prog in sorted(known_utils):
                lines.append(f"- `{prog}`")
        if analysis.auto_classified_utilities:
            lines.append("")
            lines.append("**Auto-classified (by prefix pattern):**")
            lines.append("")
            for prog in sorted(analysis.auto_classified_utilities):
                lines.append(f"- `{prog}`")
        lines.append("")

        if analysis.custom_missing:
            lines.append("### Missing Custom Programs")
            lines.append("")
            lines.append("The following custom programs are referenced but not documented:")
            lines.append("")
            lines.append("| Program | Called By | Call Type |")
            lines.append("|---------|-----------|-----------|")
            for prog in sorted(analysis.custom_missing):
                callers = []
                for pid, info in analysis.documented_programs.items():
                    for call in info.calls:
                        if call.callee == prog:
                            callers.append((pid, call.call_type))
                for caller, call_type in callers:
                    lines.append(f"| {prog} | {caller} | {call_type} |")
            lines.append("")
            lines.append("> **Note**: These programs should be located and documented to complete the system design.")
            lines.append("")

        # Call graph summary
        lines.append("## Call Graph Summary")
        lines.append("")
        lines.append(f"- **Entry Points**: {len(analysis.entry_points)}")
        lines.append(f"- **Leaf Nodes**: {len(analysis.leaf_nodes)}")
        lines.append(f"- **Total Calls**: {analysis.total_calls}")
        lines.append(f"- **Documentation Complete**: {'Yes' if not analysis.has_gaps() else 'No'}")
        lines.append("")

        return "\n".join(lines)
