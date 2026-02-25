"""
Main orchestrator for Citadel dependency graph extraction.

Coordinates all phases of analysis:
1. Discovery - find and categorize files
2. Spec Resolution - get specs for all file types
3. Parsing - extract definitions and references (parallel)
4. Registration - build artifact registry
5. Resolution - bind references to artifacts
6. LLM Disambiguation - resolve ambiguous references (optional)
7. Assembly - build final graph
8. Export - write output
"""

from __future__ import annotations

import asyncio
import logging
from pathlib import Path
from typing import TYPE_CHECKING

from rich.progress import (
    BarColumn,
    MofNCompleteColumn,
    Progress,
    SpinnerColumn,
    TaskProgressColumn,
    TextColumn,
    TimeElapsedColumn,
)

from citadel.config import CitadelConfig, get_specs_cache_dir, load_config
from citadel.discovery import DiscoveryResult, FileDiscovery
from citadel.graph import (
    DependencyGraph,
    GraphBuilder,
    GraphExporter,
    Relationship,
    SourceLocation,
    UnresolvedReference,
)
from citadel.parser import FileParseResult, ParserEngine, RawReference
from citadel.resolver import AliasResolver, ArtifactRegistry, CrossReferenceResolver
from citadel.specs import (
    AliasRule,
    ArtifactSpec,
    ArtifactType,
    RelationshipType,
    SpecManager,
)

if TYPE_CHECKING:
    pass

logger = logging.getLogger(__name__)


# Known IBM system utilities and external programs that should be marked as external
# These are typically provided by the operating system or middleware and don't need resolution
KNOWN_SYSTEM_UTILITIES: frozenset[str] = frozenset({
    # TSO/ISPF utilities
    "IKJEFT01",  # TSO Terminal Monitor Program (TMP) - runs TSO commands in batch
    "IKJEFT1A",  # TSO TMP variant
    "IKJEFT1B",  # TSO TMP variant
    # Data set utilities
    "IEFBR14",   # Dummy program - does nothing, used for allocating datasets
    "IEBGENER",  # Copy sequential datasets
    "IEBCOPY",   # Copy partitioned datasets (PDS)
    "IEBUPDTE",  # Update PDS members
    "IEBPTPCH",  # Print/punch utility
    "IEBCOMPR",  # Compare datasets
    "IEHPROGM", # Scratch/rename datasets
    "IEHLIST",   # List VTOC
    "IEHINITT",  # Initialize tape
    # Access Method Services (IDCAMS)
    "IDCAMS",    # VSAM Access Method Services
    # Sort/Merge utilities
    "SORT",      # Sort utility
    "DFSORT",    # Data Facility Sort
    "ICEMAN",    # Sort/merge utility
    "SYNCSORT",  # Syncsort utility (3rd party)
    # DB2 utilities
    "DSNTEP2",   # DB2 SQL processor
    "DSNTEP4",   # DB2 SQL processor variant
    "DSNTIAUL",  # DB2 unload utility
    "DSNUPROC",  # DB2 utility processor
    "DSNUTILB",  # DB2 online utilities
    # Language Environment
    "CEE3ABD",   # LE abnormal termination
    "CEEPIPI",   # LE preinitialization services
    "CEEUOPT",   # LE user options
    # CICS utilities
    "DFHCSDUP",  # CICS CSD utility
    "DFHSTUP",   # CICS statistics utility
    # JES utilities
    "IEFSSN",    # Subsystem name table
    # SDSF
    "SDSF",      # System Display and Search Facility
    # File-AID and common tools
    "FILEAID",   # File-AID utility
    "COMPAREX",  # Compare utility
    # Common system services
    "ADRDSSU",   # DFDSS data set services
    "AMASPZAP",  # Superzap - patch programs/datasets
    # Catalog utilities
    # Already listed but critical
    # Common batch utilities
    "FTP",       # File transfer
    "FTPBATCH",  # Batch FTP
    # Compilers (usually external)
    "IGYCRCTL",  # COBOL compiler
    "ASMA90",    # Assembler
    "IEWL",      # Linkage editor
    "IEWBLINK",  # Binder
    # Report utilities
    "EASYTRIEVE", # Easytrieve report writer
    "EZTRIEVE",   # Easytrieve variant
})

# JCL step names that should not be resolved as procedures
# These are common step naming patterns that are local to the JCL
KNOWN_JCL_STEP_PATTERNS: frozenset[str] = frozenset({
    "PG",        # Common step name prefix
    "STEP",      # Common step name
    "S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "S10",  # Numbered steps
    "INIT", "MAIN", "END", "FINAL", "CLEANUP",  # Common step names
})


# Default alias rules for cross-artifact-type resolution
DEFAULT_ALIAS_RULES: list[AliasRule] = [
    # COBOL SQL to DDL tables
    AliasRule(
        source_type=None,
        target_type=ArtifactType.TABLE,
        transformations=["uppercase", "remove_hyphens"],
        confidence_base=0.85,
    ),
    # Copybook to table
    AliasRule(
        source_type=ArtifactType.RECORD_LAYOUT,
        target_type=ArtifactType.TABLE,
        transformations=["uppercase", "remove_hyphens", "remove_suffixes"],
        confidence_base=0.75,
    ),
    # JCL dataset to file
    AliasRule(
        source_type=ArtifactType.DATASET,
        target_type=ArtifactType.FILE,
        transformations=["extract_last_qualifier", "uppercase"],
        confidence_base=0.8,
    ),
    # CICS file to VSAM
    AliasRule(
        source_type=None,
        target_type=ArtifactType.FILE,
        transformations=["uppercase", "truncate_8"],
        confidence_base=0.8,
    ),
    # Program calls (case normalization)
    AliasRule(
        source_type=None,
        target_type=ArtifactType.PROGRAM,
        transformations=["uppercase"],
        confidence_base=0.9,
    ),
    # COBOL COPY statement to copybook (handles case normalization)
    AliasRule(
        source_type=None,
        target_type=ArtifactType.COPYBOOK,
        transformations=["uppercase"],
        confidence_base=0.9,
    ),
    # COBOL COPY to record_layout (copybooks often define record layouts)
    AliasRule(
        source_type=None,
        target_type=ArtifactType.RECORD_LAYOUT,
        transformations=["uppercase", "remove_hyphens"],
        confidence_base=0.85,
    ),
    # BMS map references (SEND MAP/RECEIVE MAP)
    AliasRule(
        source_type=None,
        target_type=ArtifactType.MAP,
        transformations=["uppercase", "truncate_8"],
        confidence_base=0.9,
    ),
    # BMS screen references
    AliasRule(
        source_type=None,
        target_type=ArtifactType.SCREEN,
        transformations=["uppercase", "truncate_8"],
        confidence_base=0.9,
    ),
    # JCL procedure calls (case normalization)
    AliasRule(
        source_type=None,
        target_type=ArtifactType.PROCEDURE,
        transformations=["uppercase"],
        confidence_base=0.9,
    ),
]


class Orchestrator:
    """
    Main coordinator for dependency graph extraction.

    Orchestrates the complete analysis pipeline from source discovery through
    graph assembly and export. Coordinates all component modules and manages
    parallel file processing.

    Usage:
        config = load_config()
        orchestrator = Orchestrator(config)
        graph = await orchestrator.analyze(Path("./src"))

    Attributes:
        config: The Citadel configuration instance.
        spec_manager: Manager for loading and caching artifact specs.
        parser: The parser engine for extracting artifacts and references.
        registry: The artifact registry for storing discovered artifacts.
    """

    def __init__(
        self,
        config: CitadelConfig | None = None,
        *,
        use_structural_parser: bool = True,
    ) -> None:
        """
        Initialize orchestrator with configuration.

        Creates all component instances needed for analysis.

        Args:
            config: Configuration instance. If not provided, loads from environment.
            use_structural_parser: If True, run the COBOL structural parser
                alongside the regex parser for richer artifact extraction.
                Default is True since it's strictly additive.
        """
        self.config = config or load_config()
        self.use_structural_parser = use_structural_parser

        # Initialize spec manager with builtin and cache directories
        builtin_dir = self.config.builtin_specs_dir
        cache_dir = get_specs_cache_dir(self.config)

        self.spec_manager = SpecManager(
            builtin_dir=builtin_dir,
            cache_dir=cache_dir,
        )

        # Initialize parser engine
        self.parser = ParserEngine()

        # Initialize registry (populated during analysis)
        self.registry = ArtifactRegistry()

        # Track analysis state
        self._alias_rules: list[AliasRule] = DEFAULT_ALIAS_RULES.copy()

    async def analyze(
        self,
        source_root: Path,
        output_path: Path | None = None,
        output_format: str = "json",
    ) -> DependencyGraph:
        """
        Main entry point: analyze source and produce graph.

        Executes the complete analysis pipeline:
        1. Discovery - find and categorize files
        2. Spec Resolution - get specs for all file types
        3. Parsing - extract definitions and references
        4. Registration - build artifact registry
        5. Resolution - bind references to artifacts
        6. Assembly - build final graph
        7. Export - write output (if path provided)

        Args:
            source_root: Directory to analyze.
            output_path: Where to write output (optional).
            output_format: Output format - json, dot, cypher, csv.

        Returns:
            Complete dependency graph.

        Raises:
            FileNotFoundError: If source_root does not exist.
            ValueError: If output_format is not supported.
        """
        source_root = source_root.resolve()

        if not source_root.exists():
            raise FileNotFoundError(f"Source directory not found: {source_root}")

        if output_format not in ("json", "dot", "cypher", "csv", "markdown"):
            raise ValueError(f"Unsupported output format: {output_format}")

        logger.info("Starting analysis of %s", source_root)

        # Phase 1: Discovery
        discovery = await self._run_discovery(source_root)
        logger.info(
            "Discovery complete: %d files identified, %d unknown, %d excluded",
            sum(len(files) for files in discovery.files_by_spec.values()),
            len(discovery.unknown_files),
            len(discovery.excluded_files),
        )

        # Phase 2: Spec Resolution
        specs = await self._resolve_specs(discovery)
        logger.info("Specs resolved: %s", list(specs.keys()))

        # Phase 3: Parsing
        parse_results = await self._parse_files(discovery.files_by_spec, specs)
        logger.info(
            "Parsing complete: %d files, %d artifacts, %d references",
            len(parse_results),
            sum(len(r.artifacts_defined) for r in parse_results),
            sum(len(r.references_found) for r in parse_results),
        )

        # Phase 3b: Enhanced COBOL structural parsing (optional)
        structural_artifacts, structural_relationships = (
            self._run_structural_cobol_parse(
                discovery.files_by_spec, specs, source_root
            )
        )
        if structural_artifacts:
            logger.info(
                "Structural COBOL parse: %d artifacts, %d relationships",
                len(structural_artifacts),
                len(structural_relationships),
            )

        # Phase 4: Registration
        self.registry = self._register_artifacts(parse_results)
        # Merge structural parser artifacts into registry
        for artifact in structural_artifacts:
            try:
                self.registry.register(artifact)
            except ValueError:
                # Already registered by regex parser -- skip
                pass
        logger.info("Registry populated: %d artifacts", self.registry.size())

        # Phase 5: Resolution
        relationships, unresolved = self._resolve_references(parse_results, self.registry)
        # Merge structural parser relationships
        relationships.extend(structural_relationships)
        logger.info(
            "Resolution complete: %d relationships, %d unresolved",
            len(relationships),
            len(unresolved),
        )

        # Phase 7: Assembly
        graph = self._build_graph(
            registry=self.registry,
            relationships=relationships,
            unresolved=unresolved,
            source_root=source_root,
            specs=specs,
            discovery=discovery,
            parse_results=parse_results,
        )
        logger.info(
            "Graph built: %d artifacts, %d relationships",
            len(graph.artifacts),
            len(graph.relationships),
        )

        # Phase 8: Export (if output path provided)
        if output_path is not None:
            self._export_graph(graph, output_path, output_format)
            logger.info("Graph exported to %s (%s)", output_path, output_format)

        return graph

    async def analyze_incremental(
        self,
        source_root: Path,
        changed_files: list[Path],
        existing_graph: DependencyGraph,
    ) -> DependencyGraph:
        """
        Incremental analysis of changed files.

        Reuses existing artifacts for unchanged files, only re-parsing
        files that have been modified.

        Args:
            source_root: Directory containing source.
            changed_files: List of files that have changed.
            existing_graph: Previously generated graph.

        Returns:
            Updated dependency graph.
        """
        source_root = source_root.resolve()

        if not source_root.exists():
            raise FileNotFoundError(f"Source directory not found: {source_root}")

        logger.info(
            "Starting incremental analysis: %d changed files",
            len(changed_files),
        )

        # Identify specs needed for changed files
        specs_needed: dict[str, list[Path]] = {}
        for file_path in changed_files:
            spec = self.spec_manager.get_spec_for_file(file_path)
            if spec is not None:
                if spec.spec_id not in specs_needed:
                    specs_needed[spec.spec_id] = []
                specs_needed[spec.spec_id].append(file_path)

        # Get specs
        specs: dict[str, ArtifactSpec] = {}
        for spec_id in specs_needed:
            spec = self.spec_manager.get_spec(spec_id)
            if spec is not None:
                specs[spec_id] = spec

        # Parse changed files
        parse_results = await self._parse_files(specs_needed, specs)

        # Build new registry from existing graph artifacts
        self.registry = ArtifactRegistry()

        # Register existing artifacts not in changed files
        changed_file_paths = {str(f.resolve()) for f in changed_files}
        for artifact in existing_graph.artifacts.values():
            if artifact.defined_in is None or artifact.defined_in.file_path not in changed_file_paths:
                self.registry.register(artifact)

        # Register newly parsed artifacts
        for result in parse_results:
            for artifact in result.artifacts_defined:
                self.registry.register(artifact)

        # Resolve references from new parse results
        relationships, unresolved = self._resolve_references(parse_results, self.registry)

        # Keep existing relationships not from changed files
        for rel in existing_graph.relationships:
            if rel.location.file_path not in changed_file_paths:
                relationships.append(rel)

        # Build updated graph
        graph = self._build_graph(
            registry=self.registry,
            relationships=relationships,
            unresolved=unresolved,
            source_root=source_root,
            specs=specs,
            discovery=None,
            parse_results=parse_results,
        )

        return graph

    async def _run_discovery(self, source_root: Path) -> DiscoveryResult:
        """
        Phase 1: Discover files.

        Traverses the source directory tree and categorizes files by
        their artifact specification.

        Args:
            source_root: Root directory to scan.

        Returns:
            DiscoveryResult with files grouped by spec ID.
        """
        logger.debug("Starting file discovery in %s", source_root)

        discovery = FileDiscovery(root=source_root)
        result = discovery.discover()

        logger.debug("Discovery summary: %s", result.summary())

        return result

    async def _resolve_specs(
        self, discovery: DiscoveryResult
    ) -> dict[str, ArtifactSpec]:
        """
        Phase 2: Get specs for all file types.

        Loads the appropriate spec for each discovered file type.
        For unknown file types, could trigger LLM-based spec generation
        (not yet implemented).

        Args:
            discovery: Discovery result with files grouped by spec ID.

        Returns:
            Dictionary mapping spec ID to ArtifactSpec.
        """
        specs: dict[str, ArtifactSpec] = {}

        for spec_id in discovery.files_by_spec:
            spec = self.spec_manager.get_spec(spec_id)
            if spec is not None:
                specs[spec_id] = spec
                logger.debug("Loaded spec '%s' for %s", spec_id, spec.language)
            else:
                logger.warning(
                    "No spec found for spec_id '%s' - %d files will be skipped",
                    spec_id,
                    len(discovery.files_by_spec[spec_id]),
                )

        # Log unknown files that need spec generation
        if discovery.unknown_files:
            logger.info(
                "%d files have unknown types and will be skipped",
                len(discovery.unknown_files),
            )
            # TODO: Implement LLM-based spec generation for unknown files

        return specs

    async def _parse_files(
        self,
        files_by_spec: dict[str, list[Path]],
        specs: dict[str, ArtifactSpec],
    ) -> list[FileParseResult]:
        """
        Phase 3: Parse all files (parallel).

        Parses each file using its corresponding spec to extract
        artifact definitions and references. Uses parallel processing
        for improved performance.

        Args:
            files_by_spec: Files grouped by spec ID.
            specs: Available specs keyed by spec ID.

        Returns:
            List of parse results for all successfully parsed files.
        """
        all_results: list[FileParseResult] = []

        # Count total files
        total_files = sum(len(files) for files in files_by_spec.values())

        if total_files == 0:
            logger.warning("No files to parse")
            return all_results

        # Build list of (file_path, spec) tuples for processing
        parse_tasks: list[tuple[Path, ArtifactSpec]] = []
        for spec_id, files in files_by_spec.items():
            spec = specs.get(spec_id)
            if spec is None:
                logger.warning("Skipping %d files for missing spec '%s'", len(files), spec_id)
                continue
            for file_path in files:
                parse_tasks.append((file_path, spec))

        # Process files with progress bar
        with Progress(
            SpinnerColumn(),
            TextColumn("[progress.description]{task.description}"),
            BarColumn(),
            MofNCompleteColumn(),
            TaskProgressColumn(),
            TimeElapsedColumn(),
        ) as progress:
            task = progress.add_task("Parsing files...", total=len(parse_tasks))

            # Process in batches for controlled parallelism
            batch_size = self.config.parallel_files
            for i in range(0, len(parse_tasks), batch_size):
                batch = parse_tasks[i : i + batch_size]

                # Process batch concurrently
                batch_coros = [
                    self._parse_single_file(file_path, spec)
                    for file_path, spec in batch
                ]
                batch_results = await asyncio.gather(*batch_coros, return_exceptions=True)

                # Collect results, handling errors
                for j, result in enumerate(batch_results):
                    if isinstance(result, Exception):
                        file_path, _ = batch[j]
                        logger.warning("Failed to parse %s: %s", file_path, result)
                    elif result is not None:
                        all_results.append(result)

                    progress.advance(task)

        return all_results

    async def _parse_single_file(
        self, file_path: Path, spec: ArtifactSpec
    ) -> FileParseResult | None:
        """
        Parse a single file with error handling.

        Reads the file content and passes it to the parser engine.
        Handles encoding errors gracefully by trying fallback encodings.

        Args:
            file_path: Path to the file to parse.
            spec: The artifact specification for this file type.

        Returns:
            FileParseResult if successful, None if the file couldn't be read.
        """
        # Try to read the file with different encodings
        content: str | None = None
        encodings_to_try = ["utf-8", "latin-1", "cp1252"]

        for encoding in encodings_to_try:
            try:
                content = file_path.read_text(encoding=encoding)
                break
            except UnicodeDecodeError:
                continue
            except OSError as e:
                logger.warning("Cannot read file %s: %s", file_path, e)
                return None

        if content is None:
            logger.warning(
                "Could not decode file %s with any supported encoding", file_path
            )
            return None

        # Parse the file
        try:
            result = self.parser.parse_file(file_path, content, spec)
            return result
        except Exception as e:
            logger.warning("Error parsing %s: %s", file_path, e)
            return None

    def _register_artifacts(
        self, parse_results: list[FileParseResult]
    ) -> ArtifactRegistry:
        """
        Phase 4: Build artifact registry.

        Registers all discovered artifacts in a central registry for
        fast lookup during reference resolution.

        Args:
            parse_results: Parse results containing artifact definitions.

        Returns:
            Populated artifact registry.
        """
        registry = ArtifactRegistry()

        for result in parse_results:
            for artifact in result.artifacts_defined:
                try:
                    registry.register(artifact)
                except ValueError as e:
                    logger.warning(
                        "Failed to register artifact %s: %s",
                        artifact.id,
                        e,
                    )

        logger.debug(
            "Registered %d artifacts: %s",
            registry.size(),
            registry.get_statistics(),
        )

        return registry

    def _resolve_references(
        self,
        parse_results: list[FileParseResult],
        registry: ArtifactRegistry,
    ) -> tuple[list[Relationship], list[UnresolvedReference]]:
        """
        Phase 5: Resolve references.

        Binds raw references extracted from source files to known artifacts
        in the registry. Uses exact matching, alias resolution, transformation
        rules, and fuzzy matching.

        Also filters out known system utilities and JCL step patterns that
        should not appear in the unresolved list (they are external dependencies).

        Args:
            parse_results: Parse results containing raw references.
            registry: Artifact registry to resolve against.

        Returns:
            Tuple of (resolved_relationships, unresolved_references).
        """
        # Create alias resolver with transformation rules
        alias_resolver = AliasResolver(registry, self._alias_rules)

        # Create cross-reference resolver
        cross_ref_resolver = CrossReferenceResolver(registry, alias_resolver)

        # Collect all raw references, filtering out known external utilities
        all_references = []
        filtered_external_count = 0
        for result in parse_results:
            for ref in result.references_found:
                ref_text_upper = ref.raw_text.upper()
                # Filter out known system utilities (programs)
                if ref.expected_type == ArtifactType.PROGRAM:
                    if ref_text_upper in KNOWN_SYSTEM_UTILITIES:
                        logger.debug(
                            "Skipping known system utility: %s", ref.raw_text
                        )
                        filtered_external_count += 1
                        continue
                # Filter out known JCL step patterns (procedures)
                if ref.expected_type == ArtifactType.PROCEDURE:
                    if ref_text_upper in KNOWN_JCL_STEP_PATTERNS:
                        logger.debug(
                            "Skipping known JCL step pattern: %s", ref.raw_text
                        )
                        filtered_external_count += 1
                        continue
                all_references.append(ref)

            # Convert preprocessor directives (COPY statements) to RawReferences
            # These are extracted separately by the parser but need resolution too
            if result.preprocessor_directives:
                # Determine containing artifact (first artifact defined in the file)
                containing_artifact = (
                    result.artifacts_defined[0].id
                    if result.artifacts_defined
                    else None
                )

                for directive in result.preprocessor_directives:
                    # Skip directives without a relationship type
                    if not directive.get("relationship_type"):
                        logger.debug(
                            "Skipping preprocessor directive without relationship_type: %s",
                            directive.get("target"),
                        )
                        continue

                    # Build SourceLocation from directive location dict
                    loc_dict = directive.get("location", {})
                    location = SourceLocation(
                        file_path=loc_dict.get("file_path", result.file_path),
                        line_start=loc_dict.get("line_start", 1),
                        line_end=loc_dict.get("line_end"),
                        column_start=loc_dict.get("column_start"),
                    )

                    # Convert target_type string to ArtifactType enum
                    expected_type = None
                    if directive.get("target_type"):
                        try:
                            expected_type = ArtifactType(directive["target_type"])
                        except ValueError:
                            logger.warning(
                                "Unknown target_type '%s' for preprocessor directive",
                                directive["target_type"],
                            )

                    # Create RawReference from preprocessor directive
                    raw_ref = RawReference(
                        raw_text=directive["target"],
                        pattern_name=directive.get("type", "preprocessor"),
                        expected_type=expected_type,
                        relationship_type=RelationshipType(directive["relationship_type"]),
                        location=location,
                        containing_artifact=containing_artifact,
                    )

                    all_references.append(raw_ref)
                    logger.debug(
                        "Converted preprocessor directive to reference: %s (%s)",
                        directive["target"],
                        directive.get("relationship_type"),
                    )

        if filtered_external_count > 0:
            logger.info(
                "Filtered %d references to known external utilities/patterns",
                filtered_external_count,
            )

        # Resolve all references
        relationships, unresolved = cross_ref_resolver.resolve_all(all_references)

        # Post-filter unresolved: remove JCL dataset references (external by nature)
        # Datasets like "AWS.M2.CARDDEMO.LOADLIB" are typically external
        filtered_unresolved = []
        external_dataset_count = 0
        for unref in unresolved:
            # Filter out dataset references - they are external by definition
            if unref.expected_type == ArtifactType.DATASET:
                # Check if it looks like an external dataset (contains dots = qualifiers)
                if "." in unref.reference_text:
                    logger.debug(
                        "Filtering external dataset from unresolved: %s",
                        unref.reference_text,
                    )
                    external_dataset_count += 1
                    continue
            filtered_unresolved.append(unref)

        if external_dataset_count > 0:
            logger.info(
                "Filtered %d external dataset references from unresolved",
                external_dataset_count,
            )

        # Log resolution statistics
        stats = cross_ref_resolver.get_resolution_stats()
        logger.debug("Resolution stats: %s", stats)

        return relationships, filtered_unresolved

    def _build_graph(
        self,
        registry: ArtifactRegistry,
        relationships: list[Relationship],
        unresolved: list[UnresolvedReference],
        source_root: Path,
        specs: dict[str, ArtifactSpec],
        discovery: DiscoveryResult | None,
        parse_results: list[FileParseResult],
    ) -> DependencyGraph:
        """
        Phase 7: Assemble final graph.

        Combines all artifacts, relationships, and unresolved references
        into a complete dependency graph with computed statistics.

        Args:
            registry: Artifact registry with all discovered artifacts.
            relationships: Resolved relationships.
            unresolved: Unresolved references.
            source_root: Root directory that was analyzed.
            specs: Specs used during analysis.
            discovery: Discovery result (for file counts).
            parse_results: Parse results (for counting).

        Returns:
            Complete DependencyGraph.
        """
        builder = GraphBuilder(source_root, self.config)

        # Add all artifacts from registry
        builder.add_artifacts(list(registry))

        # Add relationships
        builder.add_relationships(relationships)

        # Add unresolved references
        builder.add_unresolved(unresolved)

        # Set specs used (mapping language to spec_id)
        specs_used = {spec.language: spec.spec_id for spec in specs.values()}
        builder.set_specs_used(specs_used)

        # Set spec hashes for reproducibility
        specs_hashes = {}
        for spec in specs.values():
            try:
                specs_hashes[spec.language] = self.spec_manager.get_spec_hash(spec.spec_id)
            except KeyError:
                pass
        builder.set_specs_hashes(specs_hashes)

        # Set file counts
        files_analyzed = len(parse_results)
        files_failed = sum(1 for r in parse_results if r.errors)

        if discovery is not None:
            files_skipped = len(discovery.unknown_files) + len(discovery.excluded_files)
        else:
            files_skipped = 0

        builder.set_file_counts(
            analyzed=files_analyzed,
            skipped=files_skipped,
            failed=files_failed,
        )

        # Build the final graph
        graph = builder.build()

        # Validate and log any issues
        warnings = builder.validate()
        for warning in warnings:
            logger.warning("Graph validation: %s", warning)

        return graph

    def _run_structural_cobol_parse(
        self,
        files_by_spec: dict[str, list[Path]],
        specs: dict[str, ArtifactSpec],
        source_root: Path,
    ) -> tuple[list, list]:
        """Run the COBOL structural parser on COBOL files.

        Produces richer artifacts with complexity scores, variable
        reads/writes, and detailed call graph relationships that the
        regex-based parser cannot provide.

        Args:
            files_by_spec: Files grouped by spec ID.
            specs: Available specs keyed by spec ID.
            source_root: Root directory for deriving copybook dirs.

        Returns:
            Tuple of (artifacts, relationships) from the structural parser.
        """
        from citadel.graph.model import Artifact  # noqa: F811

        if not self.use_structural_parser:
            return [], []

        # Find COBOL files
        cobol_files: list[Path] = []
        for spec_id, files in files_by_spec.items():
            spec = specs.get(spec_id)
            if spec and spec.spec_id == "cobol":
                cobol_files.extend(files)

        if not cobol_files:
            return [], []

        # Derive copybook dirs from the discovery file inventory
        copybook_dirs: list[Path] = []
        seen_dirs: set[str] = set()
        for _spec_id, files in files_by_spec.items():
            for f in files:
                parent = f.parent
                parent_str = str(parent)
                if parent_str not in seen_dirs:
                    seen_dirs.add(parent_str)
                    copybook_dirs.append(parent)
        # Also include source_root itself
        if str(source_root) not in seen_dirs:
            copybook_dirs.insert(0, source_root)

        all_artifacts: list[Artifact] = []
        all_relationships: list[Relationship] = []

        try:
            from citadel.cobol.bridge import parse_cobol_to_graph

            for cobol_file in cobol_files:
                try:
                    artifacts, relationships = parse_cobol_to_graph(
                        cobol_file, copybook_dirs
                    )
                    all_artifacts.extend(artifacts)
                    all_relationships.extend(relationships)
                except Exception as e:
                    logger.warning(
                        "Structural COBOL parse failed for %s: %s",
                        cobol_file,
                        e,
                    )
        except ImportError:
            logger.debug(
                "COBOL structural parser not available; skipping"
            )

        return all_artifacts, all_relationships

    def _export_graph(
        self,
        graph: DependencyGraph,
        output_path: Path,
        output_format: str,
    ) -> None:
        """
        Phase 8: Export the graph to file.

        Writes the dependency graph to the specified output path in
        the requested format.

        Args:
            graph: The dependency graph to export.
            output_path: Path to write to.
            output_format: Format to use (json, dot, cypher, csv, markdown).
        """
        exporter = GraphExporter()

        if output_format == "json":
            exporter.export_json(graph, output_path)
        elif output_format == "dot":
            exporter.export_dot(graph, output_path)
        elif output_format == "cypher":
            exporter.export_cypher(graph, output_path)
        elif output_format == "csv":
            # CSV export needs a directory
            exporter.export_csv(graph, output_path)
        elif output_format == "markdown":
            exporter.export_markdown(graph, output_path)
        else:
            raise ValueError(f"Unsupported output format: {output_format}")

    def add_alias_rule(self, rule: AliasRule) -> None:
        """
        Add a custom alias transformation rule.

        Rules are used during reference resolution to transform
        reference names to match artifact names.

        Args:
            rule: The alias rule to add.
        """
        self._alias_rules.append(rule)
        logger.debug(
            "Added alias rule: %s -> %s with transformations %s",
            rule.source_type,
            rule.target_type,
            rule.transformations,
        )

    def get_alias_rules(self) -> list[AliasRule]:
        """
        Get the current list of alias transformation rules.

        Returns:
            List of alias rules.
        """
        return list(self._alias_rules)

    def clear_alias_rules(self) -> None:
        """Reset alias rules to defaults."""
        self._alias_rules = DEFAULT_ALIAS_RULES.copy()

    def get_available_specs(self) -> list[str]:
        """
        Get list of available spec IDs.

        Returns:
            Sorted list of spec IDs.
        """
        return self.spec_manager.list_available_specs()

    def get_spec(self, spec_id: str) -> ArtifactSpec | None:
        """
        Get a spec by ID.

        Args:
            spec_id: The spec ID to look up.

        Returns:
            The ArtifactSpec if found, None otherwise.
        """
        return self.spec_manager.get_spec(spec_id)

    def print_summary(self, graph: DependencyGraph) -> None:
        """
        Print a human-readable summary of the graph.

        Args:
            graph: The dependency graph to summarize.
        """
        exporter = GraphExporter()
        summary = exporter.export_summary(graph)
        print(summary)
