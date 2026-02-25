"""Bridge between COBOL structural parser and citadel's graph model.

Converts parsed COBOL output (DataItem, ParagraphInfo, ExecBlock,
CallGraph) into citadel's native Artifact/Relationship model.
"""

from __future__ import annotations

import logging
import re
import uuid
from pathlib import Path

from citadel.cobol.call_graph import CallGraph, CallGraphBuilder
from citadel.cobol.data_division import DataDivisionParser, DataItem
from citadel.cobol.procedure_division import (
    ExecBlock,
    ParagraphInfo,
    ProcedureDivisionParser,
)
from citadel.cobol.source_reader import CobolSource, SourceReader
from citadel.graph.model import Artifact, Relationship, SourceLocation
from citadel.specs.schema import (
    ArtifactCategory,
    ArtifactType,
    RelationshipType,
)

logger = logging.getLogger(__name__)


def _make_id(artifact_type: str, name: str) -> str:
    """Create a citadel artifact ID."""
    return f"{artifact_type}::{name}"


def _rel_id() -> str:
    """Generate a unique relationship ID."""
    return str(uuid.uuid4())


def build_graph_from_parsed(
    source: CobolSource,
    data_items: list[DataItem],
    paragraphs: list[ParagraphInfo],
    exec_blocks: list[ExecBlock],
    call_graph: CallGraph,
    source_path: Path,
) -> tuple[list[Artifact], list[Relationship]]:
    """Convert pre-parsed COBOL objects into citadel artifacts + relationships.

    Accepts already-parsed objects so callers that have run the pipeline
    don't need to re-parse from disk.

    Args:
        source: Parsed COBOL source.
        data_items: Parsed data items from DATA DIVISION.
        paragraphs: Parsed paragraphs from PROCEDURE DIVISION.
        exec_blocks: Parsed EXEC blocks.
        call_graph: Built call graph.
        source_path: Path to the original COBOL source file.

    Returns:
        Tuple of (artifacts, relationships) in citadel's model.
    """
    artifacts: list[Artifact] = []
    relationships: list[Relationship] = []

    file_path_str = str(source_path)
    program_name = source.source_file.rsplit(".", 1)[0].upper()

    # --- Program-level artifact ---
    program_artifact = Artifact(
        id=_make_id("program", program_name),
        artifact_type=ArtifactType.PROGRAM,
        category=ArtifactCategory.CODE,
        canonical_name=program_name,
        language="COBOL",
        defined_in=SourceLocation(
            file_path=file_path_str,
            line_start=1,
            line_end=source.total_lines,
        ),
        attributes={
            "total_lines": source.total_lines,
            "copybooks_resolved": source.copybooks_resolved,
            "entry_paragraph": call_graph.entry_paragraph,
            "paragraph_count": len(paragraphs),
            "data_item_count": len([
                d for d in data_items if d.level != 88
            ]),
            "exec_block_count": len(exec_blocks),
        },
    )
    artifacts.append(program_artifact)

    # --- Paragraph artifacts ---
    for para in paragraphs:
        para_id = _make_id("paragraph", para.name)
        artifacts.append(Artifact(
            id=para_id,
            artifact_type=ArtifactType.PARAGRAPH,
            category=ArtifactCategory.CODE,
            canonical_name=para.name,
            language="COBOL",
            defined_in=SourceLocation(
                file_path=file_path_str,
                line_start=para.line_start,
                line_end=para.line_end,
            ),
            attributes={
                "complexity_score": para.complexity_score,
                "reads": para.reads,
                "writes": para.writes,
                "notes": para.notes,
                "exec_block_ids": para.exec_blocks,
            },
        ))

    # --- Data item artifacts (level 01/77 top-level only) ---
    for item in data_items:
        if item.level not in (1, 77):
            continue
        item_id = _make_id("data_item", item.name)
        artifacts.append(Artifact(
            id=item_id,
            artifact_type=ArtifactType.RECORD_LAYOUT,
            category=ArtifactCategory.DATA,
            canonical_name=item.name,
            language="COBOL",
            defined_in=SourceLocation(
                file_path=file_path_str,
                line_start=item.line_number,
            ),
            attributes={
                "level": item.level,
                "picture": item.picture,
                "usage": item.usage,
                "byte_length": item.byte_length,
                "python_type_hint": item.python_type_hint,
                "children_count": len(item.children),
                "section": item.section,
                "copybook_source": item.copybook_source,
            },
        ))

    # --- Exec block artifacts ---
    for eb in exec_blocks:
        eb_id = _make_id("exec_block", eb.id)
        artifacts.append(Artifact(
            id=eb_id,
            artifact_type=ArtifactType.FUNCTION,
            category=ArtifactCategory.CODE,
            canonical_name=eb.id,
            display_name=f"EXEC {eb.type} {eb.subtype}".strip(),
            language="COBOL",
            defined_in=SourceLocation(
                file_path=file_path_str,
                line_start=eb.line_number,
            ),
            attributes={
                "exec_type": eb.type,
                "subtype": eb.subtype,
                "host_variables_in": eb.host_variables_in,
                "host_variables_out": eb.host_variables_out,
                "paragraph": eb.paragraph,
            },
        ))

    # --- Relationships ---

    # PERFORM targets
    for para in paragraphs:
        from_id = _make_id("paragraph", para.name)
        for perform in para.performs:
            target = perform.get("target", "")
            if not target:
                continue
            mechanism = perform.get("mechanism", "PERFORM")
            thru = perform.get("thru")

            to_id = _make_id("paragraph", target)

            if thru:
                rel_type = RelationshipType.PERFORMS
                relationships.append(Relationship(
                    id=_rel_id(),
                    from_artifact=from_id,
                    to_artifact=to_id,
                    relationship_type=rel_type,
                    location=SourceLocation(
                        file_path=file_path_str,
                        line_start=para.line_start,
                    ),
                    evidence_text=(
                        f"PERFORM {target} THRU {thru}"
                    ),
                ))
            else:
                relationships.append(Relationship(
                    id=_rel_id(),
                    from_artifact=from_id,
                    to_artifact=to_id,
                    relationship_type=RelationshipType.PERFORMS,
                    location=SourceLocation(
                        file_path=file_path_str,
                        line_start=para.line_start,
                    ),
                    evidence_text=f"{mechanism} {target}",
                ))

    # External CALL relationships from exec blocks
    for eb in exec_blocks:
        if eb.type == "CICS" and eb.subtype in (
            "LINK",
            "XCTL",
        ):
            pgm_match = re.search(
                r"\bPROGRAM\s*\(\s*'?([A-Za-z0-9_-]+)'?\s*\)",
                eb.raw_text,
                re.IGNORECASE,
            )
            if pgm_match:
                target_pgm = pgm_match.group(1).upper()
                from_id = _make_id(
                    "paragraph", eb.paragraph
                )
                to_id = _make_id("program", target_pgm)
                relationships.append(Relationship(
                    id=_rel_id(),
                    from_artifact=from_id,
                    to_artifact=to_id,
                    relationship_type=RelationshipType.CALLS,
                    location=SourceLocation(
                        file_path=file_path_str,
                        line_start=eb.line_number,
                    ),
                    evidence_text=eb.raw_text[:120],
                ))

        if eb.type == "SQL":
            for table_match in re.finditer(
                r"\b(?:FROM|INTO|UPDATE|JOIN)\s+"
                r"([A-Za-z][A-Za-z0-9_]*)",
                eb.raw_text,
                re.IGNORECASE,
            ):
                table_name = table_match.group(1).upper()
                if table_name in (
                    "INTO",
                    "FROM",
                    "WHERE",
                    "SET",
                    "VALUES",
                ):
                    continue
                from_id = _make_id(
                    "paragraph", eb.paragraph
                )
                to_id = _make_id("table", table_name)

                if eb.subtype in ("SELECT", "FETCH"):
                    rel_type = RelationshipType.READS
                elif eb.subtype in ("INSERT",):
                    rel_type = RelationshipType.WRITES
                elif eb.subtype in ("UPDATE",):
                    rel_type = RelationshipType.UPDATES
                elif eb.subtype in ("DELETE",):
                    rel_type = RelationshipType.DELETES
                else:
                    rel_type = RelationshipType.REFERENCES

                relationships.append(Relationship(
                    id=_rel_id(),
                    from_artifact=from_id,
                    to_artifact=to_id,
                    relationship_type=rel_type,
                    location=SourceLocation(
                        file_path=file_path_str,
                        line_start=eb.line_number,
                    ),
                    evidence_text=eb.raw_text[:120],
                ))

    # COPY relationships
    for copybook_name in source.copybooks_resolved:
        from_id = program_artifact.id
        to_id = _make_id("copybook", copybook_name.upper())
        relationships.append(Relationship(
            id=_rel_id(),
            from_artifact=from_id,
            to_artifact=to_id,
            relationship_type=RelationshipType.INCLUDES,
            location=SourceLocation(
                file_path=file_path_str,
                line_start=1,
            ),
            evidence_text=f"COPY {copybook_name}",
            is_resolved=False,
        ))

    return artifacts, relationships


def parse_cobol_to_graph(
    source_path: Path,
    copybook_dirs: list[Path] | None = None,
) -> tuple[list[Artifact], list[Relationship]]:
    """Parse a COBOL source file and return citadel artifacts + relationships.

    Convenience wrapper that runs the full pipeline then converts to the
    citadel model.  Prefer ``build_graph_from_parsed`` when you already
    have parsed objects to avoid a redundant parse.

    Args:
        source_path: Path to the COBOL source file.
        copybook_dirs: Directories to search for copybooks.

    Returns:
        Tuple of (artifacts, relationships) in citadel's model.
    """
    reader = SourceReader(
        copybook_dirs=[str(d) for d in (copybook_dirs or [])],
        skip_missing_copybooks=True,
    )
    source = reader.read(str(source_path))

    data_parser = DataDivisionParser()
    data_items = data_parser.parse(source)

    proc_parser = ProcedureDivisionParser(data_items=data_items)
    paragraphs, exec_blocks = proc_parser.parse(source)

    graph_builder = CallGraphBuilder()
    call_graph = graph_builder.build(paragraphs)

    return build_graph_from_parsed(
        source, data_items, paragraphs, exec_blocks,
        call_graph, source_path,
    )
