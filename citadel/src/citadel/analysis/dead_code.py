"""
Dead code detection for dependency graphs.

This module identifies artifacts (paragraphs, programs, copybooks, etc.)
that are never referenced by any other artifact in the dependency graph.
These unreferenced artifacts are candidates for dead code removal.

Entry Point Exclusion
---------------------
Not all unreferenced artifacts are dead code. Some are legitimate entry
points that are expected to have no callers:

- JCL procedures: Top-level job definitions that initiate processing.
- Programs executed by JCL: Programs referenced via EXEC PGM= are
  entry points into the COBOL layer.
- First paragraph in a COBOL program: COBOL execution starts at the
  first paragraph after PROCEDURE DIVISION. This paragraph is the
  program's entry point and is implicitly "called" by the runtime.

Algorithm
---------
1. Build a reverse adjacency map (target -> set of sources) from all
   resolved relationships in the dependency graph.
2. For each artifact, check if it has any incoming edges.
3. If it has zero incoming edges, check whether it qualifies as an
   entry point (and should be excluded).
4. If it is neither referenced nor an entry point, classify it as
   dead code with a human-readable reason.

Time Complexity: O(V + E) where V = artifacts, E = relationships.
Space Complexity: O(V + E) for the reverse adjacency map.
"""

from __future__ import annotations

import logging
from collections import defaultdict
from dataclasses import dataclass, field
from typing import Any

from citadel.graph.model import Artifact, DependencyGraph, Relationship

logger = logging.getLogger(__name__)

# Artifact types that are always considered entry points (top-level).
_ENTRY_POINT_TYPES = {"procedure"}

# Relationship types that indicate a program is an entry point
# (i.e., it is invoked from JCL or a transaction).
_ENTRY_RELATIONSHIP_TYPES = {"executes", "triggers"}

# Relationship types that count as "incoming references" for dead code
# analysis. We consider ALL relationship types, not just call-type ones,
# because even an "includes" edge means the artifact is used.
_ALL_RELATIONSHIP_TYPES = None  # None means "all types count"


@dataclass
class DeadCodeItem:
    """A single dead code finding."""

    name: str
    """Canonical name of the unreferenced artifact."""

    artifact_type: str
    """Type of artifact (program, paragraph, copybook, etc.)."""

    file: str | None
    """Source file where the artifact is defined."""

    line: int | None
    """Line number where the artifact is defined."""

    reason: str
    """Human-readable explanation of why this is considered dead code."""

    def to_dict(self) -> dict[str, Any]:
        """Convert to dictionary for JSON serialization."""
        return {
            "name": self.name,
            "type": self.artifact_type,
            "file": self.file,
            "line": self.line,
            "reason": self.reason,
        }


def find_dead_code(
    graph: DependencyGraph,
    exclude_types: set[str] | None = None,
    include_only_types: set[str] | None = None,
) -> list[DeadCodeItem]:
    """
    Find artifacts in the dependency graph that are never referenced.

    This function identifies dead code by finding artifacts with zero
    incoming edges (no other artifact references them), while excluding
    legitimate entry points.

    Args:
        graph: The complete dependency graph to analyze.
        exclude_types: Optional set of artifact types to skip entirely.
            For example, {"table", "dataset"} to ignore data artifacts.
        include_only_types: Optional set of artifact types to analyze.
            If provided, only these types are checked. Mutually exclusive
            with exclude_types.

    Returns:
        List of DeadCodeItem instances, sorted by type then name.

    Raises:
        ValueError: If both exclude_types and include_only_types are set.

    Example:
        >>> from citadel.analysis.dead_code import find_dead_code
        >>> dead = find_dead_code(graph)
        >>> for item in dead:
        ...     print(f"{item.name} ({item.artifact_type}): {item.reason}")
    """
    if exclude_types and include_only_types:
        raise ValueError(
            "Cannot specify both exclude_types and include_only_types"
        )

    # Step 1: Build reverse adjacency map (who references whom).
    incoming: dict[str, set[str]] = defaultdict(set)
    for rel in graph.relationships:
        incoming[rel.to_artifact].add(rel.from_artifact)

    # Step 2: Identify which programs are JCL-executed entry points.
    jcl_executed_programs = _find_jcl_executed_programs(graph)

    # Step 3: Identify first paragraphs per program (COBOL entry points).
    first_paragraphs = _find_first_paragraphs(graph)

    # Step 4: Scan all artifacts for dead code.
    dead_items: list[DeadCodeItem] = []

    for artifact_id, artifact in graph.artifacts.items():
        art_type = artifact.artifact_type.value

        # Apply type filters.
        if include_only_types and art_type not in include_only_types:
            continue
        if exclude_types and art_type in exclude_types:
            continue

        # Check if this artifact has any incoming edges.
        has_incoming = bool(incoming.get(artifact_id))

        if has_incoming:
            continue  # Referenced by something, not dead.

        # Check if this is a legitimate entry point.
        if _is_entry_point(
            artifact_id, artifact, jcl_executed_programs, first_paragraphs
        ):
            continue  # Entry point, expected to have no callers.

        # This artifact is dead code.
        reason = _build_reason(artifact, incoming)
        dead_items.append(
            DeadCodeItem(
                name=artifact.canonical_name,
                artifact_type=art_type,
                file=artifact.defined_in.file_path if artifact.defined_in else None,
                line=artifact.defined_in.line_start if artifact.defined_in else None,
                reason=reason,
            )
        )

    # Sort by type, then by file, then by name for deterministic output.
    dead_items.sort(
        key=lambda item: (item.artifact_type, item.file or "", item.name)
    )

    logger.info(
        "Found %d dead code artifacts out of %d total artifacts",
        len(dead_items),
        len(graph.artifacts),
    )

    return dead_items


def _find_jcl_executed_programs(graph: DependencyGraph) -> set[str]:
    """
    Find programs that are executed by JCL (EXEC PGM=).

    These programs are entry points into the COBOL layer and should
    not be flagged as dead code even if nothing else calls them.

    Args:
        graph: The dependency graph to analyze.

    Returns:
        Set of artifact IDs for programs executed by JCL.
    """
    executed: set[str] = set()

    for rel in graph.relationships:
        rel_type = rel.relationship_type.value
        if rel_type in _ENTRY_RELATIONSHIP_TYPES:
            executed.add(rel.to_artifact)

    return executed


def _find_first_paragraphs(graph: DependencyGraph) -> set[str]:
    """
    Find the first paragraph defined in each COBOL program.

    In COBOL, execution starts at the first paragraph after
    PROCEDURE DIVISION. This paragraph is implicitly called by the
    runtime and should not be flagged as dead code.

    Heuristic: Group paragraphs by their containing file, then
    identify the paragraph with the lowest line_start in each file.

    Args:
        graph: The dependency graph to analyze.

    Returns:
        Set of artifact IDs for first paragraphs.
    """
    # Group paragraphs by file.
    paragraphs_by_file: dict[str, list[tuple[int, str]]] = defaultdict(list)

    for artifact_id, artifact in graph.artifacts.items():
        if artifact.artifact_type.value != "paragraph":
            continue
        if artifact.defined_in is None:
            continue

        file_path = artifact.defined_in.file_path
        line_start = artifact.defined_in.line_start
        paragraphs_by_file[file_path].append((line_start, artifact_id))

    # For each file, the paragraph with the lowest line number is the
    # entry point.
    first_paras: set[str] = set()
    for file_path, paragraphs in paragraphs_by_file.items():
        paragraphs.sort(key=lambda x: x[0])
        first_paras.add(paragraphs[0][1])

    return first_paras


def _is_entry_point(
    artifact_id: str,
    artifact: Artifact,
    jcl_executed_programs: set[str],
    first_paragraphs: set[str],
) -> bool:
    """
    Determine whether an artifact is a legitimate entry point.

    Entry points are artifacts that are expected to have no callers
    because they are the starting points of execution.

    Args:
        artifact_id: The artifact's graph ID.
        artifact: The artifact instance.
        jcl_executed_programs: Set of artifact IDs for JCL-executed programs.
        first_paragraphs: Set of artifact IDs for first paragraphs.

    Returns:
        True if the artifact is an entry point.
    """
    art_type = artifact.artifact_type.value

    # JCL procedures are always entry points (top-level job definitions).
    if art_type in _ENTRY_POINT_TYPES:
        return True

    # Programs are entry points if they are executed by JCL, or if they
    # are top-level programs (the simpler heuristic: all programs are
    # entry points since they can be invoked externally).
    if art_type == "program":
        return True

    # The first paragraph in a COBOL program is an implicit entry point.
    if art_type == "paragraph" and artifact_id in first_paragraphs:
        return True

    return False


def _build_reason(
    artifact: Artifact,
    incoming: dict[str, set[str]],
) -> str:
    """
    Build a human-readable reason for why an artifact is dead code.

    Args:
        artifact: The dead code artifact.
        incoming: The reverse adjacency map.

    Returns:
        A descriptive reason string.
    """
    art_type = artifact.artifact_type.value

    reasons = {
        "paragraph": (
            f"Paragraph '{artifact.canonical_name}' is never PERFORMed "
            f"or referenced by any other paragraph or program"
        ),
        "copybook": (
            f"Copybook '{artifact.canonical_name}' is never included "
            f"(COPY) by any program"
        ),
        "function": (
            f"Function '{artifact.canonical_name}' is never called "
            f"by any other artifact"
        ),
        "method": (
            f"Method '{artifact.canonical_name}' is never called "
            f"by any other artifact"
        ),
        "macro": (
            f"Macro '{artifact.canonical_name}' is never referenced "
            f"by any program or copybook"
        ),
        "table": (
            f"Table '{artifact.canonical_name}' is never read, written, "
            f"or referenced by any program"
        ),
        "screen": (
            f"Screen/Map '{artifact.canonical_name}' is never sent to "
            f"or received from by any program"
        ),
        "transaction": (
            f"Transaction '{artifact.canonical_name}' is never triggered "
            f"by any artifact"
        ),
        "record_layout": (
            f"Record layout '{artifact.canonical_name}' is never used "
            f"by any program"
        ),
    }

    return reasons.get(
        art_type,
        f"Artifact '{artifact.canonical_name}' ({art_type}) is never "
        f"referenced by any other artifact in the dependency graph",
    )


def dead_code_to_dicts(items: list[DeadCodeItem]) -> list[dict[str, Any]]:
    """
    Convert a list of DeadCodeItem instances to dictionaries.

    Convenience function for JSON serialization.

    Args:
        items: List of DeadCodeItem instances.

    Returns:
        List of dictionaries suitable for JSON serialization.
    """
    return [item.to_dict() for item in items]


def dead_code_summary(items: list[DeadCodeItem]) -> dict[str, Any]:
    """
    Generate a summary of dead code findings.

    Args:
        items: List of DeadCodeItem instances.

    Returns:
        Dictionary with summary statistics:
        - total: Total number of dead code artifacts
        - by_type: Count of dead code items per artifact type
        - files_affected: Number of unique files with dead code
    """
    by_type: dict[str, int] = defaultdict(int)
    files: set[str] = set()

    for item in items:
        by_type[item.artifact_type] += 1
        if item.file:
            files.add(item.file)

    return {
        "total": len(items),
        "by_type": dict(sorted(by_type.items())),
        "files_affected": len(files),
    }
