"""Chunk documentation merger.

This module combines multiple partial documentation templates (from
chunk processing) into a single unified template.

The merger handles:
- Deduplication of entries (by name/key)
- Citation line number preservation
- Confidence aggregation
- Open question collection
"""

from __future__ import annotations

import logging
from typing import Any

from war_rig.agents.scribe import ScribeOutput
from war_rig.chunking.models import CodeChunk
from war_rig.models.templates import (
    DocumentationTemplate,
    FileType,
    HeaderSection,
    ProgramType,
    PurposeSection,
)

logger = logging.getLogger(__name__)


class ChunkMerger:
    """Merges multiple chunk documentation into unified template.

    The merger combines partial templates from chunk processing into
    a single coherent documentation template. It handles:

    - **Header/Purpose**: Uses first chunk, augments with context
    - **Inputs/Outputs**: Deduplicates by name, merges citations
    - **Paragraphs**: Combines all, preserves citation ranges
    - **Business Rules**: Combines all, deduplicates by description
    - **Copybooks**: Deduplicates by name
    - **Error Handling**: Combines all
    - **SQL/CICS Operations**: Combines all, deduplicates

    Example:
        >>> merger = ChunkMerger()
        >>> chunks = chunker.chunk(source_code, max_tokens=8000)
        >>> chunk_outputs = [await scribe.ainvoke(chunk) for chunk in chunks]
        >>> merged = merger.merge(chunks, chunk_outputs, "PROGRAM.cbl")
    """

    def merge(
        self,
        chunks: list[CodeChunk],
        chunk_outputs: list[ScribeOutput],
        file_name: str,
        program_id: str | None = None,
    ) -> DocumentationTemplate:
        """Merge chunk outputs into single unified template.

        Args:
            chunks: The code chunks that were processed.
            chunk_outputs: ScribeOutput for each chunk (same order as chunks).
            file_name: Name of the original source file.
            program_id: Program ID (extracted from first chunk if not provided).

        Returns:
            Unified DocumentationTemplate.

        Raises:
            ValueError: If chunks and outputs have different lengths.

        TODO: Implement comprehensive merging logic.
        """
        if len(chunks) != len(chunk_outputs):
            raise ValueError(
                f"Chunk count ({len(chunks)}) != output count ({len(chunk_outputs)})"
            )

        if not chunks:
            raise ValueError("No chunks to merge")

        # Collect all successful templates
        templates: list[tuple[CodeChunk, DocumentationTemplate]] = []
        for chunk, output in zip(chunks, chunk_outputs):
            if output.success and output.template:
                templates.append((chunk, output.template))
            else:
                logger.warning(
                    f"Chunk {chunk.chunk_id} failed or has no template: {output.error}"
                )

        if not templates:
            raise ValueError("No successful chunk outputs to merge")

        # Use first template as base
        base_chunk, base_template = templates[0]

        # Merge header
        merged_header = self._merge_headers(templates, file_name)

        # Merge purpose
        merged_purpose = self._merge_purposes(templates)

        # Merge list sections with deduplication
        merged_inputs = self._merge_io_list(templates, "inputs")
        merged_outputs = self._merge_io_list(templates, "outputs")
        merged_called_programs = self._merge_called_programs(templates)
        merged_copybooks = self._merge_copybooks(templates)
        merged_paragraphs = self._merge_paragraphs(templates, chunks)
        merged_business_rules = self._merge_business_rules(templates)
        merged_error_handling = self._merge_error_handling(templates)
        merged_sql = self._merge_sql_operations(templates)
        merged_cics = self._merge_cics_operations(templates)

        # Merge data flow
        merged_data_flow = self._merge_data_flow(templates)

        # Merge calling context
        merged_calling_context = self._merge_calling_context(templates)

        # Collect open questions from all chunks
        merged_open_questions = self._merge_open_questions(templates)

        # Build merged template
        merged = DocumentationTemplate(
            header=merged_header,
            purpose=merged_purpose,
            inputs=merged_inputs,
            outputs=merged_outputs,
            called_programs=merged_called_programs,
            calling_context=merged_calling_context,
            business_rules=merged_business_rules,
            data_flow=merged_data_flow,
            copybooks_used=merged_copybooks,
            paragraphs=merged_paragraphs,
            error_handling=merged_error_handling,
            sql_operations=merged_sql,
            cics_operations=merged_cics,
            open_questions=merged_open_questions,
        )

        logger.info(
            f"Merged {len(templates)} chunk templates into unified documentation "
            f"for {file_name}"
        )

        return merged

    def _merge_headers(
        self,
        templates: list[tuple[CodeChunk, DocumentationTemplate]],
        file_name: str,
    ) -> HeaderSection:
        """Merge header sections from chunk templates.

        Uses the first chunk's header as base, updates iteration count.

        TODO: Implement header merging.
        """
        base_chunk, base_template = templates[0]

        # Use base header, update iteration to reflect chunked processing
        if base_template.header:
            return HeaderSection(
                program_id=base_template.header.program_id,
                file_name=file_name,
                file_type=base_template.header.file_type,
                analyzed_by=base_template.header.analyzed_by + "_MERGED",
                analyzed_at=base_template.header.analyzed_at,
                iteration_count=base_template.header.iteration_count,
            )
        else:
            # Create minimal header if base has none
            return HeaderSection(
                program_id=file_name.split(".")[0].upper(),
                file_name=file_name,
                file_type=FileType.COBOL,
                analyzed_by="WAR_RIG_MERGED",
            )

    def _merge_purposes(
        self,
        templates: list[tuple[CodeChunk, DocumentationTemplate]],
    ) -> PurposeSection:
        """Merge purpose sections.

        Uses header chunk's purpose as primary, augments with procedure insights.

        TODO: Implement purpose merging.
        """
        # Find the chunk with full header/data (usually first)
        primary_purpose = None
        for chunk, template in templates:
            if chunk.includes_header and template.purpose:
                primary_purpose = template.purpose
                break

        # If no header chunk, use first available
        if not primary_purpose:
            for _, template in templates:
                if template.purpose:
                    primary_purpose = template.purpose
                    break

        if primary_purpose:
            # Collect all citations from all chunks
            all_citations = set(primary_purpose.citations or [])
            for _, template in templates:
                if template.purpose and template.purpose.citations:
                    all_citations.update(template.purpose.citations)

            return PurposeSection(
                summary=primary_purpose.summary,
                business_context=primary_purpose.business_context,
                program_type=primary_purpose.program_type,
                citations=sorted(list(all_citations)),
            )
        else:
            return PurposeSection(
                summary="[MERGED] Purpose could not be determined from chunks",
                program_type=ProgramType.BATCH,
                citations=[],
            )

    def _merge_io_list(
        self,
        templates: list[tuple[CodeChunk, DocumentationTemplate]],
        attr_name: str,  # "inputs" or "outputs"
    ) -> list[Any]:
        """Merge input or output lists with deduplication.

        TODO: Implement I/O list merging with citation aggregation.
        """
        seen: dict[str, Any] = {}  # name -> merged entry

        for _, template in templates:
            entries = getattr(template, attr_name, []) or []
            for entry in entries:
                if hasattr(entry, "name"):
                    name = entry.name
                else:
                    # Dict-style entry
                    name = entry.get("name", str(entry))

                if name not in seen:
                    seen[name] = entry
                else:
                    # Merge citations if both have them
                    existing = seen[name]
                    if hasattr(entry, "citation") and hasattr(existing, "citation"):
                        # Combine citation lists
                        existing_cites = existing.citation or []
                        new_cites = entry.citation or []
                        if isinstance(existing_cites, list) and isinstance(new_cites, list):
                            combined = list(set(existing_cites + new_cites))
                            existing.citation = sorted(combined)

        return list(seen.values())

    def _merge_called_programs(
        self,
        templates: list[tuple[CodeChunk, DocumentationTemplate]],
    ) -> list[Any]:
        """Merge called_programs lists with deduplication.

        TODO: Implement called programs merging.
        """
        seen: dict[str, Any] = {}

        for _, template in templates:
            for entry in template.called_programs or []:
                if hasattr(entry, "program_name"):
                    name = entry.program_name
                else:
                    name = entry.get("program_name", str(entry))

                if name not in seen:
                    seen[name] = entry

        return list(seen.values())

    def _merge_copybooks(
        self,
        templates: list[tuple[CodeChunk, DocumentationTemplate]],
    ) -> list[Any]:
        """Merge copybooks_used lists with deduplication.

        TODO: Implement copybook merging.
        """
        seen: dict[str, Any] = {}

        for _, template in templates:
            for entry in template.copybooks_used or []:
                if hasattr(entry, "copybook_name"):
                    name = entry.copybook_name
                else:
                    name = entry.get("copybook_name", str(entry))

                if name not in seen:
                    seen[name] = entry

        return list(seen.values())

    def _merge_paragraphs(
        self,
        templates: list[tuple[CodeChunk, DocumentationTemplate]],
        chunks: list[CodeChunk],
    ) -> list[Any]:
        """Merge paragraph documentation from all chunks.

        Preserves citation line numbers (already absolute in chunks).

        TODO: Implement paragraph merging with proper citation handling.
        """
        seen: dict[str, Any] = {}

        for _, template in templates:
            for entry in template.paragraphs or []:
                if hasattr(entry, "paragraph_name"):
                    name = entry.paragraph_name
                else:
                    name = entry.get("paragraph_name", str(entry))

                if name not in seen:
                    seen[name] = entry
                # If already seen, keep first (may have more context)

        return list(seen.values())

    def _merge_business_rules(
        self,
        templates: list[tuple[CodeChunk, DocumentationTemplate]],
    ) -> list[Any]:
        """Merge business rules, deduplicating by description similarity.

        TODO: Implement business rule merging with deduplication.
        """
        rules = []
        seen_descriptions: set[str] = set()

        rule_counter = 1
        for _, template in templates:
            for entry in template.business_rules or []:
                if hasattr(entry, "description"):
                    desc = entry.description
                else:
                    desc = entry.get("description", str(entry))

                # Simple dedup by normalized description
                normalized = desc.lower().strip()
                if normalized not in seen_descriptions:
                    seen_descriptions.add(normalized)
                    # Renumber rule IDs to avoid conflicts
                    if hasattr(entry, "rule_id"):
                        entry.rule_id = f"BR{rule_counter:03d}"
                    elif isinstance(entry, dict):
                        entry["rule_id"] = f"BR{rule_counter:03d}"
                    rules.append(entry)
                    rule_counter += 1

        return rules

    def _merge_error_handling(
        self,
        templates: list[tuple[CodeChunk, DocumentationTemplate]],
    ) -> list[Any]:
        """Merge error handling entries.

        TODO: Implement error handling merging.
        """
        entries = []
        seen: set[str] = set()

        for _, template in templates:
            for entry in template.error_handling or []:
                if hasattr(entry, "condition"):
                    key = entry.condition
                else:
                    key = entry.get("condition", str(entry))

                if key not in seen:
                    seen.add(key)
                    entries.append(entry)

        return entries

    def _merge_sql_operations(
        self,
        templates: list[tuple[CodeChunk, DocumentationTemplate]],
    ) -> list[Any]:
        """Merge SQL operations with deduplication.

        TODO: Implement SQL operations merging.
        """
        entries = []
        seen: set[str] = set()

        for _, template in templates:
            for entry in template.sql_operations or []:
                # Create unique key from operation + table
                if hasattr(entry, "operation"):
                    key = f"{entry.operation}:{getattr(entry, 'table', '')}"
                else:
                    key = f"{entry.get('operation', '')}:{entry.get('table', '')}"

                if key not in seen:
                    seen.add(key)
                    entries.append(entry)

        return entries

    def _merge_cics_operations(
        self,
        templates: list[tuple[CodeChunk, DocumentationTemplate]],
    ) -> list[Any]:
        """Merge CICS operations with deduplication.

        TODO: Implement CICS operations merging.
        """
        entries = []
        seen: set[str] = set()

        for _, template in templates:
            for entry in template.cics_operations or []:
                # Create unique key from command + resource
                if hasattr(entry, "command"):
                    key = f"{entry.command}:{getattr(entry, 'resource', '')}"
                else:
                    key = f"{entry.get('command', '')}:{entry.get('resource', '')}"

                if key not in seen:
                    seen.add(key)
                    entries.append(entry)

        return entries

    def _merge_data_flow(
        self,
        templates: list[tuple[CodeChunk, DocumentationTemplate]],
    ) -> dict[str, Any]:
        """Merge data flow information.

        TODO: Implement data flow merging.
        """
        merged = {
            "reads_from": [],
            "writes_to": [],
            "transforms": [],
        }

        seen_reads: set[str] = set()
        seen_writes: set[str] = set()
        seen_transforms: set[str] = set()

        for _, template in templates:
            if not template.data_flow:
                continue

            data_flow = template.data_flow
            if hasattr(data_flow, "reads_from"):
                reads = data_flow.reads_from or []
            elif isinstance(data_flow, dict):
                reads = data_flow.get("reads_from", [])
            else:
                reads = []

            for entry in reads:
                if hasattr(entry, "source"):
                    key = entry.source
                else:
                    key = entry.get("source", str(entry))
                if key not in seen_reads:
                    seen_reads.add(key)
                    merged["reads_from"].append(entry)

            if hasattr(data_flow, "writes_to"):
                writes = data_flow.writes_to or []
            elif isinstance(data_flow, dict):
                writes = data_flow.get("writes_to", [])
            else:
                writes = []

            for entry in writes:
                if hasattr(entry, "destination"):
                    key = entry.destination
                else:
                    key = entry.get("destination", str(entry))
                if key not in seen_writes:
                    seen_writes.add(key)
                    merged["writes_to"].append(entry)

            if hasattr(data_flow, "transforms"):
                transforms = data_flow.transforms or []
            elif isinstance(data_flow, dict):
                transforms = data_flow.get("transforms", [])
            else:
                transforms = []

            for entry in transforms:
                if hasattr(entry, "input_field"):
                    key = f"{entry.input_field}->{getattr(entry, 'output_field', '')}"
                else:
                    key = f"{entry.get('input_field', '')}->{entry.get('output_field', '')}"
                if key not in seen_transforms:
                    seen_transforms.add(key)
                    merged["transforms"].append(entry)

        return merged

    def _merge_calling_context(
        self,
        templates: list[tuple[CodeChunk, DocumentationTemplate]],
    ) -> dict[str, Any]:
        """Merge calling context information.

        TODO: Implement calling context merging.
        """
        merged = {
            "called_by": [],
            "entry_points": [],
            "linkage_section": [],
        }

        seen_called_by: set[str] = set()
        seen_entry_points: set[str] = set()
        seen_linkage: set[str] = set()

        for _, template in templates:
            if not template.calling_context:
                continue

            ctx = template.calling_context
            if hasattr(ctx, "called_by"):
                for entry in ctx.called_by or []:
                    entry_str = str(entry)
                    if entry_str not in seen_called_by:
                        seen_called_by.add(entry_str)
                        merged["called_by"].append(entry)

            if hasattr(ctx, "entry_points"):
                for entry in ctx.entry_points or []:
                    entry_str = str(entry)
                    if entry_str not in seen_entry_points:
                        seen_entry_points.add(entry_str)
                        merged["entry_points"].append(entry)

            if hasattr(ctx, "linkage_section"):
                for entry in ctx.linkage_section or []:
                    entry_str = str(entry)
                    if entry_str not in seen_linkage:
                        seen_linkage.add(entry_str)
                        merged["linkage_section"].append(entry)

        return merged

    def _merge_open_questions(
        self,
        templates: list[tuple[CodeChunk, DocumentationTemplate]],
    ) -> list[Any]:
        """Collect open questions from all chunks.

        TODO: Implement open questions merging with chunk context.
        """
        questions = []

        for chunk, template in templates:
            for q in template.open_questions or []:
                # Add chunk context to question if not already there
                if hasattr(q, "context"):
                    q_text = q.question if hasattr(q, "question") else str(q)
                else:
                    q_text = q.get("question", str(q)) if isinstance(q, dict) else str(q)

                # Add source chunk info
                augmented = {
                    "question": q_text,
                    "context": f"From chunk {chunk.chunk_id} (lines {chunk.start_line}-{chunk.end_line})",
                    "suggestion": q.get("suggestion", "") if isinstance(q, dict) else "",
                }
                questions.append(augmented)

        return questions
