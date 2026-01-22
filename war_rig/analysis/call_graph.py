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


@dataclass
class CallRelationship:
    """Represents a call from one program to another."""
    caller: str
    callee: str
    call_type: str
    is_dynamic: bool = False
    citation: int | None = None


@dataclass
class ProgramInfo:
    """Information about a documented program."""
    program_id: str
    file_name: str
    file_type: str | None = None
    summary: str | None = None
    calls: list[CallRelationship] = field(default_factory=list)
    called_by: list[str] = field(default_factory=list)


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

    def has_gaps(self) -> bool:
        """Check if there are undocumented custom programs."""
        return len(self.custom_missing) > 0

    def get_missing_for_documentation(self) -> list[str]:
        """Get list of custom programs that need documentation."""
        return sorted(self.custom_missing)


class CallGraphAnalyzer:
    """Analyzes documentation files to build call graphs.

    Usage:
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

    def analyze(self) -> CallGraphAnalysis:
        """Perform call graph analysis on all documentation files.

        Returns:
            CallGraphAnalysis with all findings
        """
        docs = self._load_documentation()
        if not docs:
            logger.warning(f"No documentation files found in {self.doc_directory}")
            return self._empty_analysis()

        logger.info(f"Analyzing {len(docs)} documentation files")

        # Build program info
        programs = self._build_program_info(docs)

        # Extract call relationships
        all_calls, dynamic_calls = self._extract_calls(docs, programs)

        # Identify external dependencies
        documented_ids = set(programs.keys())
        all_callees = {call.callee for call in all_calls if not call.is_dynamic}
        external_deps = all_callees - documented_ids

        # Classify external dependencies
        system_utils = external_deps & self.system_utilities
        custom_missing = external_deps - system_utils

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

    def _build_program_info(self, docs: list[dict]) -> dict[str, ProgramInfo]:
        """Build program info from documentation."""
        programs = {}
        for doc in docs:
            program_id = self._get_program_id(doc)
            if not program_id:
                continue

            file_type = None
            if "header" in doc and isinstance(doc["header"], dict):
                file_type = doc["header"].get("file_type")

            programs[program_id] = ProgramInfo(
                program_id=program_id,
                file_name=doc.get("_source_file", ""),
                file_type=file_type,
                summary=self._get_summary(doc),
            )
        return programs

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
                lines.append(f"- **{prog}**: {summary or 'No summary'}")
        else:
            lines.append("*No entry points found*")
        lines.append("")

        # External dependencies
        lines.append("## External Dependencies")
        lines.append("")
        lines.append("### System Utilities (Skipped)")
        lines.append("")
        if analysis.system_utilities:
            for prog in sorted(analysis.system_utilities):
                lines.append(f"- {prog}")
        else:
            lines.append("*None*")
        lines.append("")

        lines.append("### Custom Programs (Need Documentation)")
        lines.append("")
        if analysis.custom_missing:
            for prog in sorted(analysis.custom_missing):
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
        lines.append(f"| Custom Missing | {len(analysis.custom_missing)} |")
        lines.append(f"| Total Calls | {analysis.total_calls} |")
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

        # Node definitions
        lines.append("")
        lines.append("    %% Documented programs")
        for prog in sorted(analysis.documented_programs.keys()):
            node_id = sanitize_id(prog)
            if prog in analysis.entry_points:
                lines.append(f"    {node_id}([{prog}])")
            elif prog in analysis.leaf_nodes:
                lines.append(f"    {node_id}[/{prog}/]")
            else:
                lines.append(f"    {node_id}[{prog}]")

        # External nodes (only custom missing, not system utils)
        if analysis.custom_missing:
            lines.append("")
            lines.append("    %% Missing custom programs")
            for prog in sorted(analysis.custom_missing):
                node_id = sanitize_id(prog)
                lines.append(f"    {node_id}>{prog}]")

        # Edges
        lines.append("")
        lines.append("    %% Call relationships")
        for prog_id, info in sorted(analysis.documented_programs.items()):
            caller_id = sanitize_id(prog_id)
            for call in info.calls:
                # Skip system utilities in diagram
                if call.callee in analysis.system_utilities:
                    continue
                callee_id = sanitize_id(call.callee)
                if "XCTL" in call.call_type:
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

        if analysis.custom_missing:
            ids = [sanitize_id(p) for p in sorted(analysis.custom_missing)]
            lines.append("    classDef missing fill:#FFB6C1,stroke:#DC143C")
            lines.append(f"    class {','.join(ids)} missing")

        lines.append("```")
        return "\n".join(lines)

    def generate_system_design_md(self, analysis: CallGraphAnalysis) -> str:
        """Generate SYSTEM_DESIGN.md content documenting the architecture."""
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

        # Program inventory
        lines.append("## Program Inventory")
        lines.append("")
        lines.append("| Program | Type | Summary |")
        lines.append("|---------|------|---------|")
        for prog_id in sorted(analysis.documented_programs.keys()):
            info = analysis.documented_programs[prog_id]
            file_type = info.file_type or "Unknown"
            summary = (info.summary[:60] + "...") if info.summary and len(info.summary) > 60 else (info.summary or "")
            lines.append(f"| {prog_id} | {file_type} | {summary} |")
        lines.append("")

        # External dependencies
        lines.append("## External Dependencies")
        lines.append("")
        lines.append("### System Utilities")
        lines.append("")
        lines.append("The following system utilities are called but not documented:")
        lines.append("")
        for prog in sorted(analysis.system_utilities):
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
