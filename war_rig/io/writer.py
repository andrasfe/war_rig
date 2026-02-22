"""Output generation for War Rig documentation.

This module provides functionality for writing documentation output in
various formats:

- JSON: Machine-readable structured output
- Markdown: Human-readable documentation
- Metrics: Processing statistics

Output is organized following the War Rig specification structure.
"""

import json
import logging
from datetime import datetime
from pathlib import Path
from typing import Any

from war_rig.config import SystemConfig
from war_rig.models.assessments import QualityMetrics
from war_rig.models.templates import (
    DocumentationTemplate,
    FileType,
)
from war_rig.orchestration.state import WarRigState

logger = logging.getLogger(__name__)


class DocumentationWriter:
    """Writer for documentation output.

    Handles writing documentation to various formats and organizing
    output in the directory structure defined by the spec.

    Attributes:
        config: System configuration with output paths

    Example:
        writer = DocumentationWriter(config.system)
        writer.write_result(war_rig_state)
        writer.write_summary(all_results)
    """

    def __init__(self, config: SystemConfig):
        """Initialize the writer with configuration.

        Args:
            config: System configuration with output paths.
        """
        self.config = config
        self._ensure_directories()

    def _get_header_attr(self, template, attr: str, default=None):
        """Get an attribute from template header, handling both object and dict.

        When templates are created via lenient model_construct(), the header
        may be a dict instead of a HeaderSection object.

        Args:
            template: The DocumentationTemplate.
            attr: The attribute name to get from header.
            default: Default value if attribute not found.

        Returns:
            The attribute value or default.
        """
        header = template.header
        if isinstance(header, dict):
            return header.get(attr, default)
        return getattr(header, attr, default)

    def _ensure_directories(self) -> None:
        """Ensure output directory structure exists."""
        base = self.config.output_directory

        # Create all required directories
        directories = [
            base / "analysis",
            base / "war_rig" / "iterations",
            base / "war_rig" / "chrome",
            base / "war_rig" / "dialogues",
            base / "valhalla",
            base / "final" / "programs",
            base / "final" / "copybooks",
            base / "final" / "jcl",
            base / "metrics",
        ]

        for directory in directories:
            directory.mkdir(parents=True, exist_ok=True)

    def _get_output_subdir(self, file_type: FileType) -> str:
        """Get the subdirectory for a file type.

        Args:
            file_type: Type of file.

        Returns:
            Subdirectory name.
        """
        if file_type == FileType.COPYBOOK:
            return "copybooks"
        elif file_type == FileType.JCL:
            return "jcl"
        else:
            return "programs"

    def write_json(
        self,
        data: Any,
        path: Path,
        indent: int = 2,
    ) -> None:
        """Write data as JSON to a file.

        Args:
            data: Data to write (must be JSON-serializable or Pydantic model).
            path: Output path.
            indent: JSON indentation level.
        """
        path.parent.mkdir(parents=True, exist_ok=True)

        # Handle Pydantic models
        if hasattr(data, "model_dump_json"):
            json_str = data.model_dump_json(indent=indent)
            with path.open("w") as f:
                f.write(json_str)
        else:
            with path.open("w") as f:
                json.dump(data, f, indent=indent, default=str)

        logger.debug(f"Wrote JSON: {path}")

    def write_markdown(
        self,
        content: str,
        path: Path,
    ) -> None:
        """Write markdown content to a file.

        Args:
            content: Markdown content.
            path: Output path.
        """
        path.parent.mkdir(parents=True, exist_ok=True)

        with path.open("w") as f:
            f.write(content)

        logger.debug(f"Wrote Markdown: {path}")

    def write_result(self, state: WarRigState) -> dict[str, Path]:
        """Write all outputs from a War Rig run.

        This is the main method for writing results. It creates:
        - Iteration drafts
        - Chrome tickets (if any)
        - Dialogue history
        - Final documentation (JSON and Markdown)

        Args:
            state: Final state from the War Rig graph.

        Returns:
            Dictionary of output paths by type.
        """
        base = self.config.output_directory
        program_id = "UNKNOWN"
        file_type = state.get("file_type", FileType.OTHER)

        if state.get("final_template"):
            program_id = self._get_header_attr(state["final_template"], "program_id", "UNKNOWN")
        elif state.get("preprocessor_result"):
            program_id = state["preprocessor_result"].program_id or program_id

        outputs: dict[str, Path] = {}

        # Write final template
        if state.get("final_template"):
            subdir = self._get_output_subdir(file_type)
            final_json = base / "final" / subdir / f"{program_id}.json"
            final_md = base / "final" / subdir / f"{program_id}.md"

            self.write_json(state["final_template"], final_json)
            self.write_markdown(
                self._template_to_markdown(state["final_template"]),
                final_md,
            )

            outputs["final_json"] = final_json
            outputs["final_md"] = final_md

            # Write to valhalla if exceptional
            if state.get("decision") == "VALHALLA":
                valhalla_path = base / "valhalla" / f"{program_id}.json"
                self.write_json(state["final_template"], valhalla_path)
                outputs["valhalla"] = valhalla_path

        # Write iteration history
        if state.get("current_template"):
            iteration = state.get("iteration", 1)
            iter_path = base / "war_rig" / "iterations" / f"{program_id}_iter{iteration}.json"
            self.write_json(state["current_template"], iter_path)
            outputs["iteration"] = iter_path

        # Write Chrome tickets
        if state.get("chrome_tickets"):
            chrome_path = base / "war_rig" / "chrome" / f"{program_id}_chrome.json"
            self.write_json(state["chrome_tickets"], chrome_path)
            outputs["chrome"] = chrome_path

        # Write dialogue
        if state.get("challenger_questions") or state.get("scribe_responses"):
            dialogue_data = {
                "program_id": program_id,
                "questions": [q.model_dump() for q in state.get("challenger_questions", [])],
                "responses": [r.model_dump() for r in state.get("scribe_responses", [])],
            }
            dialogue_path = base / "war_rig" / "dialogues" / f"{program_id}_dialogue.json"
            self.write_json(dialogue_data, dialogue_path)
            outputs["dialogue"] = dialogue_path

        logger.info(f"Wrote {len(outputs)} output files for {program_id}")
        return outputs

    def _program_link(self, program_name: str) -> str:
        """Generate a markdown link to a program's documentation.

        Args:
            program_name: Name of the program.

        Returns:
            Markdown link string.
        """
        # Clean the program name (remove any extension if present)
        clean_name = program_name.upper().replace(".CBL", "").replace(".COB", "")
        # Use .cbl.md extension (most common for COBOL programs)
        return f"[{program_name}](./{clean_name}.cbl.md)"

    def _copybook_link(self, copybook_name: str) -> str:
        """Generate a markdown link to a copybook's documentation.

        Args:
            copybook_name: Name of the copybook.

        Returns:
            Markdown link string (relative path from programs folder).
        """
        # Clean the copybook name (remove any extension if present)
        clean_name = copybook_name.upper().replace(".CPY", "").replace(".COPY", "")
        # Use .cpy.md extension for copybooks
        return f"[{copybook_name}](../copybooks/{clean_name}.cpy.md)"

    def _template_to_markdown(self, template: DocumentationTemplate) -> str:
        """Convert a documentation template to Markdown.

        Args:
            template: The documentation template.

        Returns:
            Markdown string representation.
        """
        lines: list[str] = []

        # Header - handle both object and dict (from lenient model_construct)
        program_id = self._get_header_attr(template, "program_id", "UNKNOWN")
        file_name = self._get_header_attr(template, "file_name", "")
        file_type = self._get_header_attr(template, "file_type", "OTHER")
        # file_type could be enum or string
        file_type_str = file_type.value if hasattr(file_type, "value") else str(file_type)
        final_status = self._get_header_attr(template, "final_status", None)
        iteration_count = self._get_header_attr(template, "iteration_count", 0)
        analyzed_at = self._get_header_attr(template, "analyzed_at", "")

        lines.append(f"# {program_id}")
        lines.append("")
        lines.append(f"**File:** {file_name}")
        lines.append(f"**Type:** {file_type_str}")
        lines.append(f"**Status:** {final_status or 'In Progress'}")
        lines.append(f"**Iterations:** {iteration_count}")
        lines.append(f"**Analyzed:** {analyzed_at}")
        lines.append("")

        # Purpose
        lines.append("## Purpose")
        lines.append("")
        lines.append(template.purpose.summary)
        if template.purpose.business_context:
            lines.append("")
            lines.append(f"**Business Context:** {template.purpose.business_context}")
        lines.append(f"**Program Type:** {template.purpose.program_type.value}")
        if template.purpose.citations:
            lines.append(f"**Citations:** Lines {', '.join(map(str, template.purpose.citations))}")
        lines.append("")

        # Calling Context (who calls this program)
        if template.calling_context and (
            template.calling_context.called_by
            or template.calling_context.entry_points
            or template.calling_context.linkage_section
        ):
            lines.append("## Calling Context")
            lines.append("")
            if template.calling_context.called_by:
                callers = ", ".join(
                    self._program_link(p) for p in template.calling_context.called_by
                )
                lines.append(f"**Called By:** {callers}")
            if template.calling_context.entry_points:
                lines.append(f"**Entry Points:** {', '.join(template.calling_context.entry_points)}")
            if template.calling_context.linkage_section:
                lines.append(f"**Linkage Section:** {', '.join(template.calling_context.linkage_section)}")
            lines.append("")

        # Inputs
        if template.inputs:
            lines.append("## Inputs")
            lines.append("")
            for inp in template.inputs:
                lines.append(f"### {inp.name}")
                lines.append(f"- **Type:** {inp.io_type.value}")
                lines.append(f"- **Description:** {inp.description}")
                if inp.copybook:
                    lines.append(f"- **Copybook:** {self._copybook_link(inp.copybook)}")
                if inp.citation:
                    lines.append(f"- **Lines:** {', '.join(map(str, inp.citation))}")
                lines.append("")

        # Outputs
        if template.outputs:
            lines.append("## Outputs")
            lines.append("")
            for out in template.outputs:
                lines.append(f"### {out.name}")
                lines.append(f"- **Type:** {out.io_type.value}")
                lines.append(f"- **Description:** {out.description}")
                if out.copybook:
                    lines.append(f"- **Copybook:** {self._copybook_link(out.copybook)}")
                if out.citation:
                    lines.append(f"- **Lines:** {', '.join(map(str, out.citation))}")
                lines.append("")

        # Called Programs
        if template.called_programs:
            lines.append("## Called Programs")
            lines.append("")
            lines.append("| Program | Call Type | Purpose | Line |")
            lines.append("|---------|-----------|---------|------|")
            for prog in template.called_programs:
                prog_link = self._program_link(prog.program_name)
                purpose = prog.purpose or ""
                line = str(prog.citation) if prog.citation else ""
                lines.append(f"| {prog_link} | {prog.call_type.value} | {purpose} | {line} |")
            lines.append("")

        # Business Rules
        if template.business_rules:
            lines.append("## Business Rules")
            lines.append("")
            for rule in template.business_rules:
                lines.append(f"### {rule.rule_id}: {rule.description}")
                if rule.logic_summary:
                    lines.append(f"**Logic:** {rule.logic_summary}")
                if rule.conditions:
                    lines.append(f"**Conditions:** {', '.join(rule.conditions)}")
                if rule.citation:
                    lines.append(f"**Lines:** {', '.join(map(str, rule.citation))}")
                lines.append("")

        # Copybooks
        if template.copybooks_used:
            lines.append("## Copybooks Used")
            lines.append("")
            lines.append("| Copybook | Location | Purpose | Line |")
            lines.append("|----------|----------|---------|------|")
            for cb in template.copybooks_used:
                cb_link = self._copybook_link(cb.copybook_name)
                purpose = cb.purpose or ""
                line = str(cb.citation) if cb.citation else ""
                lines.append(f"| {cb_link} | {cb.location.value} | {purpose} | {line} |")
            lines.append("")

        # Data Flow
        if template.data_flow and (
            template.data_flow.reads_from
            or template.data_flow.writes_to
            or template.data_flow.transforms
        ):
            lines.append("## Data Flow")
            lines.append("")
            if template.data_flow.reads_from:
                lines.append("### Reads From")
                for read in template.data_flow.reads_from:
                    fields = ", ".join(read.fields_used) if read.fields_used else "all fields"
                    lines.append(f"- **{read.source}**: {fields}")
                    if read.citation:
                        lines.append(f"  (Lines: {', '.join(map(str, read.citation))})")
                lines.append("")
            if template.data_flow.writes_to:
                lines.append("### Writes To")
                for write in template.data_flow.writes_to:
                    fields = ", ".join(write.fields_written) if write.fields_written else "all fields"
                    lines.append(f"- **{write.destination}**: {fields}")
                    if write.citation:
                        lines.append(f"  (Lines: {', '.join(map(str, write.citation))})")
                lines.append("")
            if template.data_flow.transforms:
                lines.append("### Transformations")
                for transform in template.data_flow.transforms:
                    lines.append(f"- **{transform.input_field}** → **{transform.output_field}**: {transform.transformation_description}")
                    if transform.citation:
                        lines.append(f"  (Lines: {', '.join(map(str, transform.citation))})")
                lines.append("")

        # Key Paragraphs
        if template.paragraphs:
            lines.append("## Key Paragraphs")
            lines.append("")
            for para in template.paragraphs:
                lines.append(f"### {para.paragraph_name}")
                lines.append(f"**Purpose:** {para.purpose}")
                if para.called_by:
                    lines.append(f"- Called by: {', '.join(para.called_by)}")
                if para.calls:
                    lines.append(f"- Calls: {', '.join(para.calls)}")
                if para.citation:
                    lines.append(f"- Lines: {para.citation[0]}-{para.citation[1]}")
                lines.append("")

        # Inter-Paragraph Data Flow (from call_semantics)
        if template.call_semantics:
            lines.append("## Inter-Paragraph Data Flow")
            lines.append("")
            lines.append("| Caller | Callee | Inputs | Outputs | Purpose |")
            lines.append("|--------|--------|--------|---------|---------|")
            for cs in template.call_semantics:
                # Handle both dict and object (from lenient model_construct)
                if isinstance(cs, dict):
                    caller = cs.get("caller", "")
                    callee = cs.get("callee", "")
                    inputs = cs.get("inputs", [])
                    outputs = cs.get("outputs", [])
                    purpose = cs.get("purpose", "")
                else:
                    caller = cs.caller or ""
                    callee = cs.callee or ""
                    inputs = cs.inputs or []
                    outputs = cs.outputs or []
                    purpose = cs.purpose or ""
                inputs_str = ", ".join(inputs) if inputs else "-"
                outputs_str = ", ".join(outputs) if outputs else "-"
                purpose_str = purpose or "-"
                lines.append(f"| {caller} | {callee} | {inputs_str} | {outputs_str} | {purpose_str} |")
            lines.append("")

        # Error Handling
        if template.error_handling:
            lines.append("## Error Handling")
            lines.append("")
            for err in template.error_handling:
                lines.append(f"- **{err.condition}:** {err.action}")
                if err.citation:
                    lines.append(f"  (Lines: {', '.join(map(str, err.citation))})")
            lines.append("")

        # SQL Operations
        if template.sql_operations:
            lines.append("## SQL Operations")
            lines.append("")
            lines.append("| Operation | Table | Purpose | Line |")
            lines.append("|-----------|-------|---------|------|")
            for sql in template.sql_operations:
                purpose = sql.purpose or ""
                line = str(sql.citation) if sql.citation else ""
                lines.append(f"| {sql.operation} | {sql.table} | {purpose} | {line} |")
            lines.append("")

        # CICS Operations
        if template.cics_operations:
            lines.append("## CICS Operations")
            lines.append("")
            lines.append("| Command | Resource | Purpose | Line |")
            lines.append("|---------|----------|---------|------|")
            for cics in template.cics_operations:
                resource = cics.resource or ""
                purpose = cics.purpose or ""
                line = str(cics.citation) if cics.citation else ""
                lines.append(f"| {cics.command} | {resource} | {purpose} | {line} |")
            lines.append("")

        # Open Questions
        if template.open_questions:
            lines.append("## Open Questions")
            lines.append("")
            for q in template.open_questions:
                lines.append(f"- **{q.question}**")
                if q.context:
                    lines.append(f"  - Context: {q.context}")
                if q.suggestion:
                    lines.append(f"  - Suggestion: {q.suggestion}")
            lines.append("")

        # Resolved Questions
        if template.resolved_questions:
            lines.append("## Resolved Questions")
            lines.append("")
            for rq in template.resolved_questions:
                lines.append(f"- **Q:** {rq.original_question}")
                lines.append(f"  **A:** {rq.answer}")
                if rq.confidence != "HIGH":
                    lines.append(f"  *Confidence: {rq.confidence}*")
            lines.append("")

        # Sequence Diagram (if paragraphs with calls exist)
        sequence_diagram = self._render_sequence_diagram(template)
        if sequence_diagram:
            lines.append("## Sequence Diagram")
            lines.append("")
            lines.append(sequence_diagram)
            lines.append("")

        # Footer
        lines.append("---")
        lines.append(f"*Generated by War Rig {template.header.analyzed_by}*")

        return "\n".join(lines)

    @staticmethod
    def _mermaid_safe(name: str) -> str:
        """Sanitize a name for use as a Mermaid sequence diagram participant.

        Mermaid interprets hyphens as arrow syntax (e.g. ``->>``, ``-->>``),
        so participant IDs must not contain them.

        Args:
            name: Raw participant name (e.g. a COBOL paragraph name).

        Returns:
            Alphanumeric+underscore identifier safe for Mermaid syntax.
        """
        return "".join(c if c.isalnum() or c == "_" else "_" for c in name)

    @staticmethod
    def _mermaid_safe_label(label: str) -> str:
        """Sanitize a label for Mermaid sequence diagram arrows.

        Replaces characters/sequences that Mermaid interprets as syntax:
        commas (delimiters), ``--`` (dotted arrows), ``->`` (solid arrows).

        Args:
            label: Raw label text.

        Returns:
            Label safe for Mermaid arrow syntax.
        """
        return (
            label
            .replace("--", "\u2011\u2011")  # non-breaking hyphens
            .replace("->", "\u2011>")
            .replace(",", ";")
        )

    def _render_sequence_diagram(self, template: DocumentationTemplate) -> str:
        """Generate Mermaid sequence diagram with data flow annotations.

        Uses call_semantics for enhanced labels when available, falling back to
        basic "performs" labels. Supports two modes:
        1. If paragraphs have outgoing_calls, iterate through them and look up semantics
        2. If paragraphs don't have outgoing_calls but call_semantics exist, use them directly

        Args:
            template: The documentation template containing paragraphs and call semantics.

        Returns:
            Mermaid sequence diagram as a string, or empty string if no calls exist.
        """
        # Check if there are any paragraphs with outgoing calls
        has_outgoing_calls = any(
            para.outgoing_calls for para in template.paragraphs if para.outgoing_calls
        )

        # If no outgoing_calls and no call_semantics, no diagram to render
        if not has_outgoing_calls and not template.call_semantics:
            return ""

        lines = ["```mermaid", "sequenceDiagram"]

        # Collect all participants for declaration (preserves readable labels)
        participants: dict[str, str] = {}  # safe_name -> original_name

        def register(name: str) -> str:
            safe = self._mermaid_safe(name)
            if safe not in participants:
                participants[safe] = name
            return safe

        # Build lookup from call_semantics (handle both dict and object)
        semantics_map: dict[tuple[str, str], Any] = {}
        for cs in template.call_semantics:
            if isinstance(cs, dict):
                caller = cs.get("caller")
                callee = cs.get("callee")
            else:
                caller = cs.caller
                callee = cs.callee
            if caller and callee:
                semantics_map[(caller, callee)] = cs

        # Collect arrows first so we can emit participant declarations
        arrows: list[str] = []

        if has_outgoing_calls:
            # Mode 1: Iterate through paragraphs.outgoing_calls, look up semantics
            for para in template.paragraphs:
                if not para.outgoing_calls:
                    continue
                caller = para.paragraph_name
                if not caller:
                    continue

                safe_caller = register(caller)

                for call in para.outgoing_calls:
                    # FunctionCall has a 'target' attribute, but may be dict from JSON
                    if hasattr(call, "target"):
                        target = call.target
                    elif isinstance(call, dict):
                        target = call.get("target")
                    else:
                        target = str(call)
                    if not target:
                        continue

                    safe_target = register(target)

                    sem = semantics_map.get((caller, target))
                    if sem:
                        # Get inputs/outputs from semantics
                        if isinstance(sem, dict):
                            inputs = sem.get("inputs", [])
                            outputs = sem.get("outputs", [])
                        else:
                            inputs = sem.inputs or []
                            outputs = sem.outputs or []

                        if inputs:
                            in_label = self._mermaid_safe_label(
                                self._truncate_label(inputs)
                            )
                            arrows.append(
                                f"    {safe_caller}->>{safe_target}: {in_label}"
                            )
                        else:
                            arrows.append(
                                f"    {safe_caller}->>{safe_target}: performs"
                            )

                        if outputs:
                            out_label = self._mermaid_safe_label(
                                self._truncate_label(outputs)
                            )
                            arrows.append(
                                f"    {safe_target}-->>{safe_caller}: {out_label}"
                            )
                    else:
                        # Fallback: basic label
                        arrows.append(
                            f"    {safe_caller}->>{safe_target}: performs"
                        )
        else:
            # Mode 2: No outgoing_calls but call_semantics exist, use them directly
            for cs in template.call_semantics:
                if isinstance(cs, dict):
                    caller = cs.get("caller")
                    callee = cs.get("callee")
                    inputs = cs.get("inputs", [])
                    outputs = cs.get("outputs", [])
                else:
                    caller = cs.caller
                    callee = cs.callee
                    inputs = cs.inputs or []
                    outputs = cs.outputs or []

                if not caller or not callee:
                    continue

                safe_caller = register(caller)
                safe_callee = register(callee)

                # Show forward call with inputs or "performs"
                if inputs:
                    in_label = self._mermaid_safe_label(
                        self._truncate_label(inputs)
                    )
                    arrows.append(
                        f"    {safe_caller}->>{safe_callee}: {in_label}"
                    )
                else:
                    arrows.append(
                        f"    {safe_caller}->>{safe_callee}: performs"
                    )

                # Show return with outputs if available
                if outputs:
                    out_label = self._mermaid_safe_label(
                        self._truncate_label(outputs)
                    )
                    arrows.append(
                        f"    {safe_callee}-->>{safe_caller}: {out_label}"
                    )

        # Emit participant declarations (safe_id as readable label)
        for safe_name, original_name in participants.items():
            lines.append(f"    participant {safe_name} as {original_name}")

        # Emit arrows
        lines.extend(arrows)

        lines.append("```")

        # Only return diagram if we have actual content beyond the header/footer
        # (header + participants + footer = at least 3, need arrows too)
        if not arrows:
            return ""

        return "\n".join(lines)

    def _truncate_label(self, items: list[str], max_items: int = 3) -> str:
        """Truncate a list of items for use as a diagram label.

        Args:
            items: List of variable/field names.
            max_items: Maximum number of items to show before truncation.

        Returns:
            Comma-separated string with truncation indicator if needed.
        """
        if not items:
            return ""

        # Truncate individual item names if too long
        truncated_items = []
        for item in items[:max_items]:
            if len(item) > 20:
                truncated_items.append(item[:17] + "...")
            else:
                truncated_items.append(item)

        result = ", ".join(truncated_items)
        if len(items) > max_items:
            result += "..."

        return result

    def write_metrics(
        self,
        metrics: list[QualityMetrics],
        summary: dict[str, Any] | None = None,
    ) -> Path:
        """Write processing metrics.

        Args:
            metrics: List of per-program metrics.
            summary: Optional summary statistics.

        Returns:
            Path to metrics file.
        """
        base = self.config.output_directory
        metrics_path = base / "metrics" / "processing_metrics.json"

        data = {
            "generated_at": datetime.utcnow().isoformat(),
            "program_count": len(metrics),
            "programs": [m.model_dump() for m in metrics],
        }

        if summary:
            data["summary"] = summary

        self.write_json(data, metrics_path)
        return metrics_path

    def write_index(self, results: list[WarRigState]) -> Path:
        """Write an index of all documented programs.

        Args:
            results: List of War Rig results.

        Returns:
            Path to index file.
        """
        base = self.config.output_directory
        index_path = base / "final" / "index.md"

        lines: list[str] = []
        lines.append("# War Rig Documentation Index")
        lines.append("")
        lines.append(f"Generated: {datetime.utcnow().isoformat()}")
        lines.append("")

        # Group by decision
        witnessed = []
        valhalla = []
        forced = []

        for result in results:
            program_id = "UNKNOWN"
            if result.get("final_template"):
                program_id = self._get_header_attr(result["final_template"], "program_id", "UNKNOWN")

            decision = result.get("decision", "UNKNOWN")

            entry = {
                "program_id": program_id,
                "file_name": result.get("file_name", ""),
                "file_type": str(result.get("file_type", "")),
                "iterations": result.get("iteration", 0),
            }

            if decision == "VALHALLA":
                valhalla.append(entry)
            elif decision == "FORCED":
                forced.append(entry)
            else:
                witnessed.append(entry)

        # Write sections
        if valhalla:
            lines.append("## Valhalla (Exceptional Quality)")
            lines.append("")
            lines.append("| Program | File | Type | Iterations |")
            lines.append("|---------|------|------|------------|")
            for e in valhalla:
                # Link uses file_name to get correct extension (e.g., PROG.cbl.md)
                lines.append(f"| [{e['program_id']}](programs/{e['file_name']}.md) | {e['file_name']} | {e['file_type']} | {e['iterations']} |")
            lines.append("")

        if witnessed:
            lines.append("## Witnessed (Approved)")
            lines.append("")
            lines.append("| Program | File | Type | Iterations |")
            lines.append("|---------|------|------|------------|")
            for e in witnessed:
                lines.append(f"| [{e['program_id']}](programs/{e['file_name']}.md) | {e['file_name']} | {e['file_type']} | {e['iterations']} |")
            lines.append("")

        if forced:
            lines.append("## Forced (Max Iterations)")
            lines.append("")
            lines.append("| Program | File | Type | Iterations |")
            lines.append("|---------|------|------|------------|")
            for e in forced:
                lines.append(f"| [{e['program_id']}](programs/{e['file_name']}.md) | {e['file_name']} | {e['file_type']} | {e['iterations']} |")
            lines.append("")

        # Summary
        lines.append("## Summary")
        lines.append("")
        lines.append(f"- Total Programs: {len(results)}")
        lines.append(f"- Valhalla: {len(valhalla)}")
        lines.append(f"- Witnessed: {len(witnessed)}")
        lines.append(f"- Forced: {len(forced)}")

        self.write_markdown("\n".join(lines), index_path)
        return index_path
