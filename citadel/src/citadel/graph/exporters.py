"""
Graph exporters for Citadel dependency graphs.

This module provides export functionality for dependency graphs in various
formats including JSON, GraphViz DOT, Cypher (Neo4j), CSV, and Markdown.
"""

import csv
import json
import logging
from collections import defaultdict
from datetime import datetime
from pathlib import Path
from typing import Any

from citadel.graph.model import (
    Artifact,
    DependencyGraph,
    Relationship,
    UnresolvedReference,
)
from citadel.specs.schema import ArtifactCategory, RelationshipType

logger = logging.getLogger(__name__)


class DateTimeEncoder(json.JSONEncoder):
    """JSON encoder with datetime support."""

    def default(self, obj: Any) -> Any:
        if isinstance(obj, datetime):
            return obj.isoformat()
        return super().default(obj)


class GraphExporter:
    """
    Exports dependency graph in various formats.

    Supports JSON, DOT (GraphViz), Cypher (Neo4j), CSV, and Markdown exports.
    """

    def export_json(
        self,
        graph: DependencyGraph,
        path: Path,
        pretty: bool = True,
    ) -> None:
        """
        Export graph as JSON with proper datetime handling.

        Args:
            graph: The dependency graph to export.
            path: Output file path.
            pretty: If True, format with indentation for readability.
        """
        logger.info("Exporting graph to JSON: %s", path)

        data = graph.model_dump(mode="json")

        indent = 2 if pretty else None
        with open(path, "w", encoding="utf-8") as f:
            json.dump(data, f, indent=indent, cls=DateTimeEncoder)

        logger.info(
            "JSON export complete: %d artifacts, %d relationships",
            len(graph.artifacts),
            len(graph.relationships),
        )

    def export_dot(
        self,
        graph: DependencyGraph,
        path: Path,
        cluster_by: str = "category",
    ) -> None:
        """
        Export graph as GraphViz DOT format for visualization.

        Args:
            graph: The dependency graph to export.
            path: Output file path.
            cluster_by: How to cluster nodes - "category", "language", or "file".
        """
        logger.info("Exporting graph to DOT format: %s (cluster_by=%s)", path, cluster_by)

        lines = [
            "digraph DependencyGraph {",
            '    rankdir=LR;',
            '    node [shape=box, style=filled];',
            '    edge [fontsize=10];',
            "",
        ]

        # Group artifacts by cluster key
        clusters = self._group_artifacts_by_cluster(graph, cluster_by)

        # Define color schemes
        category_colors = {
            ArtifactCategory.CODE: "#a8d5e5",
            ArtifactCategory.DATA: "#f5d5a8",
            ArtifactCategory.INTERFACE: "#d5a8f5",
        }

        # Generate clusters and nodes
        cluster_idx = 0
        for cluster_name, artifacts in sorted(clusters.items()):
            lines.append(f"    subgraph cluster_{cluster_idx} {{")
            lines.append(f'        label="{self._escape_dot_string(cluster_name)}";')
            lines.append('        style=filled;')
            lines.append('        color=lightgrey;')
            lines.append("")

            for artifact in artifacts:
                node_id = self._make_dot_node_id(artifact.id)
                label = self._escape_dot_string(artifact.canonical_name)
                color = category_colors.get(artifact.category, "#ffffff")
                tooltip = f"{artifact.artifact_type.value}: {artifact.canonical_name}"
                lines.append(
                    f'        {node_id} [label="{label}", '
                    f'fillcolor="{color}", '
                    f'tooltip="{self._escape_dot_string(tooltip)}"];'
                )

            lines.append("    }")
            lines.append("")
            cluster_idx += 1

        # Generate edges
        lines.append("    // Relationships")
        for rel in graph.relationships:
            from_id = self._make_dot_node_id(rel.from_artifact)
            to_id = self._make_dot_node_id(rel.to_artifact)
            label = rel.relationship_type.value
            edge_color = self._get_relationship_color(rel.relationship_type)
            lines.append(
                f'    {from_id} -> {to_id} [label="{label}", color="{edge_color}"];'
            )

        lines.append("}")

        with open(path, "w", encoding="utf-8") as f:
            f.write("\n".join(lines))

        logger.info("DOT export complete: %d nodes, %d edges", len(graph.artifacts), len(graph.relationships))

    def export_cypher(
        self,
        graph: DependencyGraph,
        path: Path,
    ) -> None:
        """
        Export graph as Cypher statements for Neo4j import.

        Args:
            graph: The dependency graph to export.
            path: Output file path.
        """
        logger.info("Exporting graph to Cypher: %s", path)

        lines = [
            "// Cypher import script for dependency graph",
            f"// Generated at: {graph.generated_at.isoformat()}",
            f"// Source root: {graph.source_root}",
            "",
            "// Clear existing data (optional - uncomment if needed)",
            "// MATCH (n) DETACH DELETE n;",
            "",
            "// Create constraints for better performance",
            "CREATE CONSTRAINT IF NOT EXISTS FOR (a:Artifact) REQUIRE a.id IS UNIQUE;",
            "",
            "// Create artifacts",
        ]

        # Generate CREATE statements for artifacts
        for artifact_id, artifact in graph.artifacts.items():
            props = self._artifact_to_cypher_props(artifact)
            label = artifact.artifact_type.value.upper()
            lines.append(f"CREATE (:{label}:Artifact {props});")

        lines.append("")
        lines.append("// Create relationships")

        # Generate relationship statements
        for rel in graph.relationships:
            rel_type = rel.relationship_type.value.upper()
            from_id = self._escape_cypher_string(rel.from_artifact)
            to_id = self._escape_cypher_string(rel.to_artifact)
            props = self._relationship_to_cypher_props(rel)

            lines.append(
                f"MATCH (from:Artifact {{id: '{from_id}'}}), "
                f"(to:Artifact {{id: '{to_id}'}}) "
                f"CREATE (from)-[:{rel_type} {props}]->(to);"
            )

        # Add unresolved references as special nodes
        if graph.unresolved:
            lines.append("")
            lines.append("// Create unresolved reference nodes")
            for idx, unresolved in enumerate(graph.unresolved):
                props = self._unresolved_to_cypher_props(unresolved, idx)
                lines.append(f"CREATE (:UnresolvedReference {props});")

        with open(path, "w", encoding="utf-8") as f:
            f.write("\n".join(lines))

        logger.info(
            "Cypher export complete: %d artifacts, %d relationships, %d unresolved",
            len(graph.artifacts),
            len(graph.relationships),
            len(graph.unresolved),
        )

    def export_csv(
        self,
        graph: DependencyGraph,
        directory: Path,
    ) -> None:
        """
        Export graph as CSV files.

        Creates three files:
        - artifacts.csv: All artifact nodes
        - relationships.csv: All relationships
        - unresolved.csv: All unresolved references

        Args:
            graph: The dependency graph to export.
            directory: Output directory path.
        """
        logger.info("Exporting graph to CSV files in: %s", directory)

        directory.mkdir(parents=True, exist_ok=True)

        # Export artifacts
        artifacts_path = directory / "artifacts.csv"
        self._export_artifacts_csv(graph, artifacts_path)

        # Export relationships
        relationships_path = directory / "relationships.csv"
        self._export_relationships_csv(graph, relationships_path)

        # Export unresolved references
        unresolved_path = directory / "unresolved.csv"
        self._export_unresolved_csv(graph, unresolved_path)

        logger.info(
            "CSV export complete: artifacts=%d, relationships=%d, unresolved=%d",
            len(graph.artifacts),
            len(graph.relationships),
            len(graph.unresolved),
        )

    def export_markdown(
        self,
        graph: DependencyGraph,
        path: Path,
    ) -> None:
        """
        Export graph as a human-readable Markdown summary with Mermaid diagram.

        Args:
            graph: The dependency graph to export.
            path: Output file path.
        """
        logger.info("Exporting graph to Markdown: %s", path)

        lines = [
            "# Dependency Graph Summary",
            "",
            f"**Generated:** {graph.generated_at.isoformat()}",
            f"**Source Root:** `{graph.source_root}`",
            f"**Version:** {graph.version}",
            "",
            "---",
            "",
            "## Statistics",
            "",
            f"- **Files Analyzed:** {graph.statistics.files_analyzed}",
            f"- **Files Skipped:** {graph.statistics.files_skipped}",
            f"- **Files Failed:** {graph.statistics.files_failed}",
            f"- **Total Artifacts:** {graph.statistics.artifacts_total}",
            f"- **Total Relationships:** {graph.statistics.relationships_total}",
            f"- **Unresolved References:** {graph.statistics.unresolved_count}",
            f"- **Resolution Rate:** {graph.statistics.resolution_rate:.1%}",
            "",
        ]

        # Artifacts by type
        if graph.statistics.artifacts_by_type:
            lines.append("### Artifacts by Type")
            lines.append("")
            lines.append("| Type | Count |")
            lines.append("|------|-------|")
            for artifact_type, count in sorted(graph.statistics.artifacts_by_type.items()):
                lines.append(f"| {artifact_type} | {count} |")
            lines.append("")

        # Relationships by type
        if graph.statistics.relationships_by_type:
            lines.append("### Relationships by Type")
            lines.append("")
            lines.append("| Type | Count |")
            lines.append("|------|-------|")
            for rel_type, count in sorted(graph.statistics.relationships_by_type.items()):
                lines.append(f"| {rel_type} | {count} |")
            lines.append("")

        # Languages detected
        if graph.statistics.languages_detected:
            lines.append("### Languages Detected")
            lines.append("")
            for lang in graph.statistics.languages_detected:
                spec_id = graph.statistics.specs_used.get(lang, "unknown")
                lines.append(f"- **{lang}** (spec: `{spec_id}`)")
            lines.append("")

        # Mermaid diagram
        lines.append("---")
        lines.append("")
        lines.append("## Dependency Diagram")
        lines.append("")
        lines.append("```mermaid")
        lines.extend(self._generate_mermaid_flowchart(graph))
        lines.append("```")
        lines.append("")

        # Artifacts table
        lines.append("---")
        lines.append("")
        lines.append("## Artifacts")
        lines.append("")
        lines.append("| ID | Name | Type | Category | Language | Defined In |")
        lines.append("|----|------|------|----------|----------|------------|")

        for artifact_id, artifact in sorted(graph.artifacts.items()):
            defined_in = str(artifact.defined_in) if artifact.defined_in else "N/A"
            lines.append(
                f"| `{artifact_id}` | {artifact.canonical_name} | "
                f"{artifact.artifact_type.value} | {artifact.category.value} | "
                f"{artifact.language} | {defined_in} |"
            )
        lines.append("")

        # Unresolved references
        if graph.unresolved:
            lines.append("---")
            lines.append("")
            lines.append("## Unresolved References")
            lines.append("")
            lines.append("| Reference | Expected Type | Location | Reason |")
            lines.append("|-----------|---------------|----------|--------|")

            for unresolved in graph.unresolved:
                expected = unresolved.expected_type.value if unresolved.expected_type else "N/A"
                lines.append(
                    f"| `{unresolved.reference_text}` | {expected} | "
                    f"{unresolved.location} | {unresolved.reason} |"
                )
            lines.append("")

        with open(path, "w", encoding="utf-8") as f:
            f.write("\n".join(lines))

        logger.info("Markdown export complete")

    def export_summary(self, graph: DependencyGraph) -> str:
        """
        Generate a human-readable summary string.

        Args:
            graph: The dependency graph to summarize.

        Returns:
            A formatted summary string.
        """
        lines = [
            "=" * 60,
            "DEPENDENCY GRAPH SUMMARY",
            "=" * 60,
            "",
            f"Source Root: {graph.source_root}",
            f"Generated:   {graph.generated_at.isoformat()}",
            "",
            "-" * 60,
            "FILE ANALYSIS",
            "-" * 60,
            f"  Analyzed: {graph.statistics.files_analyzed}",
            f"  Skipped:  {graph.statistics.files_skipped}",
            f"  Failed:   {graph.statistics.files_failed}",
            "",
            "-" * 60,
            "ARTIFACTS",
            "-" * 60,
            f"  Total: {graph.statistics.artifacts_total}",
        ]

        # Artifacts by type
        if graph.statistics.artifacts_by_type:
            for artifact_type, count in sorted(graph.statistics.artifacts_by_type.items()):
                lines.append(f"    {artifact_type}: {count}")

        lines.extend([
            "",
            "-" * 60,
            "RELATIONSHIPS",
            "-" * 60,
            f"  Total: {graph.statistics.relationships_total}",
        ])

        # Relationships by type
        if graph.statistics.relationships_by_type:
            for rel_type, count in sorted(graph.statistics.relationships_by_type.items()):
                lines.append(f"    {rel_type}: {count}")

        lines.extend([
            "",
            "-" * 60,
            "RESOLUTION",
            "-" * 60,
            f"  Resolved:   {graph.statistics.relationships_total}",
            f"  Unresolved: {graph.statistics.unresolved_count}",
            f"  Rate:       {graph.statistics.resolution_rate:.1%}",
            "",
            "-" * 60,
            "LANGUAGES",
            "-" * 60,
        ])

        if graph.statistics.languages_detected:
            for lang in graph.statistics.languages_detected:
                spec_id = graph.statistics.specs_used.get(lang, "unknown")
                lines.append(f"  {lang} (spec: {spec_id})")
        else:
            lines.append("  None detected")

        lines.extend([
            "",
            "=" * 60,
        ])

        return "\n".join(lines)

    # -------------------------------------------------------------------------
    # Private helper methods
    # -------------------------------------------------------------------------

    def _group_artifacts_by_cluster(
        self,
        graph: DependencyGraph,
        cluster_by: str,
    ) -> dict[str, list[Artifact]]:
        """Group artifacts by the specified clustering key."""
        clusters: dict[str, list[Artifact]] = defaultdict(list)

        for artifact in graph.artifacts.values():
            if cluster_by == "category":
                key = artifact.category.value
            elif cluster_by == "language":
                key = artifact.language
            elif cluster_by == "file":
                if artifact.defined_in:
                    key = artifact.defined_in.file_path
                else:
                    key = "unknown"
            else:
                logger.warning("Unknown cluster_by value: %s, defaulting to category", cluster_by)
                key = artifact.category.value

            clusters[key].append(artifact)

        return clusters

    def _make_dot_node_id(self, artifact_id: str) -> str:
        """Convert artifact ID to a valid DOT node identifier."""
        # Replace special characters with underscores
        return "n_" + artifact_id.replace("::", "_").replace("-", "_").replace(".", "_").replace("/", "_")

    def _escape_dot_string(self, s: str) -> str:
        """Escape a string for use in DOT format."""
        return s.replace("\\", "\\\\").replace('"', '\\"').replace("\n", "\\n")

    def _get_relationship_color(self, rel_type: RelationshipType) -> str:
        """Get a color for a relationship type."""
        colors = {
            # Code-to-code (blue shades)
            RelationshipType.CALLS: "#2196F3",
            RelationshipType.INCLUDES: "#1976D2",
            RelationshipType.EXECUTES: "#0D47A1",
            RelationshipType.INHERITS: "#42A5F5",
            RelationshipType.IMPORTS: "#64B5F6",
            RelationshipType.PERFORMS: "#1E88E5",
            # Code-to-data (green shades)
            RelationshipType.READS: "#4CAF50",
            RelationshipType.WRITES: "#F44336",
            RelationshipType.UPDATES: "#FF9800",
            RelationshipType.DELETES: "#E91E63",
            RelationshipType.DEFINES: "#8BC34A",
            RelationshipType.USES_LAYOUT: "#CDDC39",
            RelationshipType.REFERENCES: "#009688",
            # Interface (purple shades)
            RelationshipType.RECEIVES_FROM: "#9C27B0",
            RelationshipType.SENDS_TO: "#673AB7",
            RelationshipType.TRIGGERS: "#3F51B5",
            RelationshipType.ENQUEUES: "#7C4DFF",
            RelationshipType.DEQUEUES: "#B388FF",
        }
        return colors.get(rel_type, "#757575")

    def _escape_cypher_string(self, s: str) -> str:
        """Escape a string for use in Cypher queries."""
        return s.replace("\\", "\\\\").replace("'", "\\'")

    def _artifact_to_cypher_props(self, artifact: Artifact) -> str:
        """Convert artifact to Cypher property map string."""
        props = {
            "id": artifact.id,
            "canonical_name": artifact.canonical_name,
            "artifact_type": artifact.artifact_type.value,
            "category": artifact.category.value,
            "language": artifact.language,
        }

        if artifact.display_name:
            props["display_name"] = artifact.display_name

        if artifact.aliases:
            props["aliases"] = artifact.aliases

        if artifact.defined_in:
            props["file_path"] = artifact.defined_in.file_path
            props["line_start"] = artifact.defined_in.line_start

        return self._dict_to_cypher_props(props)

    def _relationship_to_cypher_props(self, rel: Relationship) -> str:
        """Convert relationship to Cypher property map string."""
        props = {
            "id": rel.id,
            "evidence_text": rel.evidence_text,
            "confidence": rel.confidence,
            "resolution_method": rel.resolution_method,
            "file_path": rel.location.file_path,
            "line_start": rel.location.line_start,
        }

        if rel.columns_accessed:
            props["columns_accessed"] = rel.columns_accessed

        if rel.access_mode:
            props["access_mode"] = rel.access_mode

        return self._dict_to_cypher_props(props)

    def _unresolved_to_cypher_props(self, unresolved: UnresolvedReference, idx: int) -> str:
        """Convert unresolved reference to Cypher property map string."""
        props = {
            "id": f"unresolved_{idx}",
            "reference_text": unresolved.reference_text,
            "reason": unresolved.reason,
            "file_path": unresolved.location.file_path,
            "line_start": unresolved.location.line_start,
            "best_score": unresolved.best_score,
        }

        if unresolved.expected_type:
            props["expected_type"] = unresolved.expected_type.value

        if unresolved.containing_artifact:
            props["containing_artifact"] = unresolved.containing_artifact

        if unresolved.candidates:
            props["candidates"] = unresolved.candidates

        return self._dict_to_cypher_props(props)

    def _dict_to_cypher_props(self, props: dict[str, Any]) -> str:
        """Convert a dictionary to a Cypher property map string."""
        parts = []
        for key, value in props.items():
            if isinstance(value, str):
                escaped = self._escape_cypher_string(value)
                parts.append(f"{key}: '{escaped}'")
            elif isinstance(value, bool):
                parts.append(f"{key}: {str(value).lower()}")
            elif isinstance(value, (int, float)):
                parts.append(f"{key}: {value}")
            elif isinstance(value, list):
                items = ", ".join(f"'{self._escape_cypher_string(str(v))}'" for v in value)
                parts.append(f"{key}: [{items}]")

        return "{" + ", ".join(parts) + "}"

    def _export_artifacts_csv(self, graph: DependencyGraph, path: Path) -> None:
        """Export artifacts to CSV file."""
        fieldnames = [
            "id",
            "canonical_name",
            "artifact_type",
            "category",
            "language",
            "display_name",
            "aliases",
            "file_path",
            "line_start",
            "line_end",
            "maps_to",
        ]

        with open(path, "w", newline="", encoding="utf-8") as f:
            writer = csv.DictWriter(f, fieldnames=fieldnames, extrasaction="ignore")
            writer.writeheader()

            for artifact in graph.artifacts.values():
                row = {
                    "id": artifact.id,
                    "canonical_name": artifact.canonical_name,
                    "artifact_type": artifact.artifact_type.value,
                    "category": artifact.category.value,
                    "language": artifact.language,
                    "display_name": artifact.display_name or "",
                    "aliases": ";".join(artifact.aliases),
                    "file_path": artifact.defined_in.file_path if artifact.defined_in else "",
                    "line_start": artifact.defined_in.line_start if artifact.defined_in else "",
                    "line_end": artifact.defined_in.line_end if artifact.defined_in else "",
                    "maps_to": artifact.maps_to or "",
                }
                writer.writerow(row)

    def _export_relationships_csv(self, graph: DependencyGraph, path: Path) -> None:
        """Export relationships to CSV file."""
        fieldnames = [
            "id",
            "from_artifact",
            "to_artifact",
            "relationship_type",
            "file_path",
            "line_start",
            "evidence_text",
            "columns_accessed",
            "access_mode",
            "confidence",
            "resolution_method",
        ]

        with open(path, "w", newline="", encoding="utf-8") as f:
            writer = csv.DictWriter(f, fieldnames=fieldnames, extrasaction="ignore")
            writer.writeheader()

            for rel in graph.relationships:
                row = {
                    "id": rel.id,
                    "from_artifact": rel.from_artifact,
                    "to_artifact": rel.to_artifact,
                    "relationship_type": rel.relationship_type.value,
                    "file_path": rel.location.file_path,
                    "line_start": rel.location.line_start,
                    "evidence_text": rel.evidence_text,
                    "columns_accessed": ";".join(rel.columns_accessed),
                    "access_mode": rel.access_mode or "",
                    "confidence": rel.confidence,
                    "resolution_method": rel.resolution_method,
                }
                writer.writerow(row)

    def _export_unresolved_csv(self, graph: DependencyGraph, path: Path) -> None:
        """Export unresolved references to CSV file."""
        fieldnames = [
            "reference_text",
            "expected_type",
            "file_path",
            "line_start",
            "containing_artifact",
            "candidates",
            "best_score",
            "reason",
        ]

        with open(path, "w", newline="", encoding="utf-8") as f:
            writer = csv.DictWriter(f, fieldnames=fieldnames, extrasaction="ignore")
            writer.writeheader()

            for unresolved in graph.unresolved:
                row = {
                    "reference_text": unresolved.reference_text,
                    "expected_type": unresolved.expected_type.value if unresolved.expected_type else "",
                    "file_path": unresolved.location.file_path,
                    "line_start": unresolved.location.line_start,
                    "containing_artifact": unresolved.containing_artifact or "",
                    "candidates": ";".join(unresolved.candidates),
                    "best_score": unresolved.best_score,
                    "reason": unresolved.reason,
                }
                writer.writerow(row)

    def _generate_mermaid_flowchart(self, graph: DependencyGraph) -> list[str]:
        """Generate Mermaid flowchart lines for the graph."""
        lines = ["flowchart LR"]

        # Define subgraphs by category
        categories: dict[str, list[str]] = defaultdict(list)

        for artifact_id, artifact in graph.artifacts.items():
            node_id = self._make_mermaid_node_id(artifact_id)
            label = artifact.canonical_name
            shape_start, shape_end = self._get_mermaid_shape(artifact.artifact_type.value)
            categories[artifact.category.value].append(f"        {node_id}{shape_start}{label}{shape_end}")

        # Output subgraphs
        for category, nodes in categories.items():
            lines.append(f"    subgraph {category}")
            lines.extend(nodes)
            lines.append("    end")

        # Output relationships (limit to prevent diagram from being too large)
        lines.append("")
        call_relationships = [
            r for r in graph.relationships
            if r.relationship_type in (
                RelationshipType.CALLS,
                RelationshipType.EXECUTES,
                RelationshipType.IMPORTS,
                RelationshipType.INCLUDES,
                RelationshipType.READS,
                RelationshipType.WRITES,
            )
        ]

        # Limit to first 50 relationships to keep diagram readable
        max_edges = 50
        if len(call_relationships) > max_edges:
            lines.append(f"    %% Showing {max_edges} of {len(call_relationships)} relationships")
            call_relationships = call_relationships[:max_edges]

        for rel in call_relationships:
            from_id = self._make_mermaid_node_id(rel.from_artifact)
            to_id = self._make_mermaid_node_id(rel.to_artifact)
            label = rel.relationship_type.value
            lines.append(f"    {from_id} -->|{label}| {to_id}")

        return lines

    def _make_mermaid_node_id(self, artifact_id: str) -> str:
        """Convert artifact ID to a valid Mermaid node identifier."""
        # Mermaid IDs must start with a letter and contain only alphanumeric chars
        clean_id = artifact_id.replace("::", "_").replace("-", "_").replace(".", "_").replace("/", "_")
        return f"n_{clean_id}"

    def _get_mermaid_shape(self, artifact_type: str) -> tuple[str, str]:
        """Get Mermaid shape delimiters based on artifact type."""
        shapes = {
            # Code artifacts - rectangles
            "program": ("[", "]"),
            "function": ("[", "]"),
            "class": ("[", "]"),
            "method": ("[", "]"),
            "module": ("[", "]"),
            "procedure": ("[", "]"),
            "paragraph": ("[", "]"),
            "copybook": ("[/", "/]"),
            "macro": ("[/", "/]"),
            # Data artifacts - cylinders / database shapes
            "table": ("[(", ")]"),
            "view": ("[(", ")]"),
            "file": ("[(", ")]"),
            "dataset": ("[(", ")]"),
            "record_layout": ("[(", ")]"),
            "column": ("(", ")"),
            "index": ("(", ")"),
            "segment": ("(", ")"),
            # Interface artifacts - special shapes
            "transaction": ("{{", "}}"),
            "screen": ("{{", "}}"),
            "queue": ("{{", "}}"),
            "service": ("{{", "}}"),
            "map": ("{{", "}}"),
        }
        return shapes.get(artifact_type, ("[", "]"))
