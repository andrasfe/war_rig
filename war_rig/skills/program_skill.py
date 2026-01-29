"""Program skill generator for Agent Skills format conversion.

This module provides the ProgramSkillGenerator class which converts War Rig
program documentation files (.doc.json) into individual Agent Skills for
progressive discovery by AI agents.

Example:
    >>> generator = ProgramSkillGenerator(Path("./skills"))
    >>> skill_dir = generator.generate_from_doc_json(Path("./output/CBACT01C.cbl.doc.json"))
    >>> print(f"Generated skill at: {skill_dir}")
"""

import json
import logging
from pathlib import Path
from typing import Any

from war_rig.skills.naming import (
    create_skill_frontmatter,
    normalize_skill_name,
    truncate_description,
)

logger = logging.getLogger(__name__)


class ProgramSkillGenerationError(Exception):
    """Raised when program skill generation fails."""

    pass


class ProgramSkillGenerator:
    """Generates skills from program documentation files.

    This class converts War Rig .doc.json files into Agent Skills format,
    creating structured skill directories with SKILL.md and reference files.

    The generated skill structure:
        programs/{skill-name}/SKILL.md - Main skill file with summary
        programs/{skill-name}/references/REFERENCE.md - Detailed JSON data

    Attributes:
        output_dir: Root skills output directory.

    Example:
        >>> generator = ProgramSkillGenerator(Path("./skills"))
        >>> skill_dir = generator.generate_from_doc_json(
        ...     Path("./output/final/programs/CBACT01C.cbl.doc.json")
        ... )
        >>> print(f"Generated: {skill_dir}")
    """

    def __init__(self, output_dir: Path) -> None:
        """Initialize the ProgramSkillGenerator.

        Args:
            output_dir: Root skills output directory (e.g., ./skills).
                       Program skills will be created under output_dir/programs/.
        """
        self.output_dir = output_dir.resolve()
        logger.debug(f"ProgramSkillGenerator initialized: output_dir={self.output_dir}")

    def generate_from_doc_json(self, doc_json_path: Path) -> Path:
        """Generate a skill from a .doc.json file.

        Creates:
            programs/{skill-name}/SKILL.md - Main skill file with summary
            programs/{skill-name}/references/REFERENCE.md - Detailed JSON data

        Args:
            doc_json_path: Path to the .doc.json documentation file.

        Returns:
            Path to the created skill directory.

        Raises:
            ProgramSkillGenerationError: If generation fails.
        """
        logger.info(f"Generating skill from: {doc_json_path}")

        # Load and parse the documentation JSON
        try:
            doc_data = self._load_doc_json(doc_json_path)
        except Exception as e:
            raise ProgramSkillGenerationError(
                f"Failed to load {doc_json_path}: {e}"
            ) from e

        # Extract skill name from filename
        skill_name = normalize_skill_name(doc_json_path.name)
        if not skill_name:
            raise ProgramSkillGenerationError(
                f"Could not normalize skill name from: {doc_json_path.name}"
            )

        # Create skill directory structure
        skill_dir = self.output_dir / "programs" / skill_name
        refs_dir = skill_dir / "references"
        skill_dir.mkdir(parents=True, exist_ok=True)
        refs_dir.mkdir(parents=True, exist_ok=True)

        # Generate SKILL.md
        skill_md_path = skill_dir / "SKILL.md"
        skill_content = self._generate_skill_md(skill_name, doc_data)
        skill_md_path.write_text(skill_content, encoding="utf-8")
        logger.debug(f"Created SKILL.md at: {skill_md_path}")

        # Generate REFERENCE.md with detailed JSON data
        ref_md_path = refs_dir / "REFERENCE.md"
        ref_content = self._format_reference(doc_data)
        ref_md_path.write_text(ref_content, encoding="utf-8")
        logger.debug(f"Created REFERENCE.md at: {ref_md_path}")

        logger.info(f"Generated skill '{skill_name}' at: {skill_dir}")
        return skill_dir

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

    def _generate_skill_md(self, skill_name: str, doc_data: dict[str, Any]) -> str:
        """Generate the complete SKILL.md content.

        Args:
            skill_name: Normalized skill name.
            doc_data: Parsed documentation data.

        Returns:
            Complete SKILL.md content with frontmatter and body.
        """
        description = self._extract_description(doc_data)
        frontmatter = create_skill_frontmatter(skill_name, description)
        body = self._format_skill_body(doc_data)
        return f"{frontmatter}\n\n{body}"

    def _extract_summary(self, doc_data: dict[str, Any]) -> str:
        """Extract a concise summary from documentation for SKILL.md.

        Args:
            doc_data: Parsed documentation data.

        Returns:
            Concise summary string.
        """
        purpose = doc_data.get("purpose", {})
        summary = purpose.get("summary", "")

        if not summary:
            # Fallback to program ID
            header = doc_data.get("header", {})
            program_id = header.get("program_id", "Unknown program")
            return f"Documentation for {program_id}"

        return str(summary)

    def _extract_description(self, doc_data: dict[str, Any]) -> str:
        """Extract description for frontmatter (max 1024 chars).

        Args:
            doc_data: Parsed documentation data.

        Returns:
            Description string truncated to 1024 characters.
        """
        summary = self._extract_summary(doc_data)
        return truncate_description(summary)

    def _format_skill_body(self, doc_data: dict[str, Any]) -> str:
        """Format the main SKILL.md body content.

        The body includes:
        - Program purpose/summary
        - Key business rules
        - Called programs list
        - Inputs/outputs summary
        - When to use this skill (for agent discovery)

        Args:
            doc_data: Parsed documentation data.

        Returns:
            Formatted markdown body content.
        """
        sections: list[str] = []

        # Header section with program info
        header = doc_data.get("header", {})
        program_id = header.get("program_id", "Unknown")
        file_type = header.get("file_type", "Unknown")
        purpose = doc_data.get("purpose", {})
        program_type = purpose.get("program_type", "Unknown")
        business_context = purpose.get("business_context", "")

        sections.append(f"# {program_id}")
        sections.append("")
        sections.append(f"**Type:** {file_type} ({program_type})")
        if business_context:
            sections.append(f"**Context:** {business_context}")
        sections.append("")

        # Purpose/Summary section
        summary = purpose.get("summary", "")
        if summary:
            sections.append("## Purpose")
            sections.append("")
            sections.append(summary)
            sections.append("")

        # Business Rules section
        business_rules = doc_data.get("business_rules", [])
        if business_rules:
            sections.append("## Business Rules")
            sections.append("")
            for rule in business_rules[:5]:  # Limit to first 5 for summary
                rule_id = rule.get("rule_id", "")
                description = rule.get("description", "")
                if description:
                    sections.append(f"- **{rule_id}**: {description}")
            if len(business_rules) > 5:
                sections.append(f"- *(+{len(business_rules) - 5} more rules)*")
            sections.append("")

        # Called Programs section
        called_programs = doc_data.get("called_programs", [])
        if called_programs:
            sections.append("## Called Programs")
            sections.append("")
            for prog in called_programs:
                if isinstance(prog, dict):
                    prog_name = prog.get("program_name", str(prog))
                    call_type = prog.get("call_type", "")
                    if call_type:
                        sections.append(f"- {prog_name} ({call_type})")
                    else:
                        sections.append(f"- {prog_name}")
                else:
                    sections.append(f"- {prog}")
            sections.append("")

        # Inputs section
        inputs = doc_data.get("inputs", [])
        if inputs:
            sections.append("## Inputs")
            sections.append("")
            for inp in inputs[:5]:  # Limit to first 5
                name = inp.get("name", "Unknown")
                io_type = inp.get("io_type", "")
                description = inp.get("description", "")
                type_str = f" ({io_type})" if io_type else ""
                desc_str = f": {description}" if description else ""
                sections.append(f"- **{name}**{type_str}{desc_str}")
            if len(inputs) > 5:
                sections.append(f"- *(+{len(inputs) - 5} more inputs)*")
            sections.append("")

        # Outputs section
        outputs = doc_data.get("outputs", [])
        if outputs:
            sections.append("## Outputs")
            sections.append("")
            for out in outputs[:5]:  # Limit to first 5
                name = out.get("name", "Unknown")
                io_type = out.get("io_type", "")
                description = out.get("description", "")
                type_str = f" ({io_type})" if io_type else ""
                desc_str = f": {description}" if description else ""
                sections.append(f"- **{name}**{type_str}{desc_str}")
            if len(outputs) > 5:
                sections.append(f"- *(+{len(outputs) - 5} more outputs)*")
            sections.append("")

        # Copybooks section
        copybooks = doc_data.get("copybooks_used", [])
        if copybooks:
            sections.append("## Copybooks Used")
            sections.append("")
            for cpy in copybooks:
                name = cpy.get("copybook_name", "Unknown")
                purpose_text = cpy.get("purpose", "")
                if purpose_text:
                    sections.append(f"- **{name}**: {purpose_text}")
                else:
                    sections.append(f"- {name}")
            sections.append("")

        # When to use section (for agent discovery)
        sections.append("## When to Use This Skill")
        sections.append("")
        sections.append("Use this skill when you need to:")
        sections.append(f"- Understand the purpose and functionality of {program_id}")
        if business_rules:
            sections.append(f"- Understand business rules implemented in {program_id}")
        if called_programs:
            sections.append(f"- Trace program calls from {program_id}")
        if inputs or outputs:
            sections.append(f"- Identify inputs/outputs for {program_id}")
        sections.append(f"- Maintain or modify {program_id}")
        sections.append("")

        # Reference pointer
        sections.append("## Additional Details")
        sections.append("")
        sections.append(
            "See [REFERENCE.md](references/REFERENCE.md) for complete technical details "
            "including paragraphs, data flow, error handling, and SQL/CICS operations."
        )

        return "\n".join(sections)

    def _format_reference(self, doc_data: dict[str, Any]) -> str:
        """Format the detailed REFERENCE.md content.

        This file contains the complete documentation data in a readable
        markdown format, including all technical details.

        Args:
            doc_data: Parsed documentation data.

        Returns:
            Formatted markdown reference content.
        """
        sections: list[str] = []

        header = doc_data.get("header", {})
        program_id = header.get("program_id", "Unknown")

        sections.append(f"# {program_id} - Complete Reference")
        sections.append("")
        sections.append(
            "*This file contains detailed technical documentation. "
            "See SKILL.md for a summary.*"
        )
        sections.append("")

        # Header section
        sections.append("## Program Header")
        sections.append("")
        sections.append(f"- **Program ID:** {header.get('program_id', 'N/A')}")
        sections.append(f"- **File Name:** {header.get('file_name', 'N/A')}")
        sections.append(f"- **File Type:** {header.get('file_type', 'N/A')}")
        sections.append(f"- **Analyzed By:** {header.get('analyzed_by', 'N/A')}")
        sections.append(f"- **Analyzed At:** {header.get('analyzed_at', 'N/A')}")
        sections.append("")

        # Purpose section
        purpose = doc_data.get("purpose", {})
        if purpose:
            sections.append("## Purpose")
            sections.append("")
            sections.append(f"**Summary:** {purpose.get('summary', 'N/A')}")
            sections.append("")
            sections.append(
                f"**Business Context:** {purpose.get('business_context', 'N/A')}"
            )
            sections.append(f"**Program Type:** {purpose.get('program_type', 'N/A')}")
            sections.append("")

        # All inputs
        inputs = doc_data.get("inputs", [])
        if inputs:
            sections.append("## Inputs")
            sections.append("")
            for inp in inputs:
                name = inp.get("name", "Unknown")
                io_type = inp.get("io_type", "")
                description = inp.get("description", "")
                copybook = inp.get("copybook")
                sections.append(f"### {name}")
                sections.append("")
                sections.append(f"- **Type:** {io_type}")
                sections.append(f"- **Description:** {description}")
                if copybook:
                    sections.append(f"- **Copybook:** {copybook}")
                sections.append("")

        # All outputs
        outputs = doc_data.get("outputs", [])
        if outputs:
            sections.append("## Outputs")
            sections.append("")
            for out in outputs:
                name = out.get("name", "Unknown")
                io_type = out.get("io_type", "")
                description = out.get("description", "")
                copybook = out.get("copybook")
                sections.append(f"### {name}")
                sections.append("")
                sections.append(f"- **Type:** {io_type}")
                sections.append(f"- **Description:** {description}")
                if copybook:
                    sections.append(f"- **Copybook:** {copybook}")
                sections.append("")

        # All business rules
        business_rules = doc_data.get("business_rules", [])
        if business_rules:
            sections.append("## Business Rules")
            sections.append("")
            for rule in business_rules:
                rule_id = rule.get("rule_id", "Unknown")
                description = rule.get("description", "")
                logic_summary = rule.get("logic_summary", "")
                conditions = rule.get("conditions", [])
                sections.append(f"### {rule_id}")
                sections.append("")
                sections.append(f"**Description:** {description}")
                sections.append("")
                if logic_summary:
                    sections.append(f"**Logic:** {logic_summary}")
                    sections.append("")
                if conditions:
                    sections.append("**Conditions:**")
                    for cond in conditions:
                        sections.append(f"- `{cond}`")
                    sections.append("")

        # Paragraphs section
        paragraphs = doc_data.get("paragraphs", [])
        if paragraphs:
            sections.append("## Paragraphs")
            sections.append("")
            for para in paragraphs:
                para_name = para.get("paragraph_name", "Unknown")
                purpose_text = para.get("purpose", "")
                calls = para.get("calls", [])
                is_dead = para.get("is_dead_code", False)

                sections.append(f"### {para_name}")
                sections.append("")
                if is_dead:
                    sections.append("**(Dead Code)**")
                    sections.append("")
                if purpose_text:
                    sections.append(purpose_text)
                    sections.append("")
                if calls:
                    sections.append(f"**Calls:** {', '.join(calls)}")
                    sections.append("")

        # Data Flow section
        data_flow = doc_data.get("data_flow", {})
        if data_flow:
            sections.append("## Data Flow")
            sections.append("")

            reads_from = data_flow.get("reads_from", [])
            if reads_from:
                sections.append("### Reads From")
                sections.append("")
                for read in reads_from:
                    source = read.get("source", "Unknown")
                    fields = read.get("fields_used", [])
                    sections.append(f"- **{source}:** {', '.join(fields)}")
                sections.append("")

            writes_to = data_flow.get("writes_to", [])
            if writes_to:
                sections.append("### Writes To")
                sections.append("")
                for write in writes_to:
                    dest = write.get("destination", "Unknown")
                    fields = write.get("fields_written", [])
                    sections.append(f"- **{dest}:** {', '.join(fields)}")
                sections.append("")

            transforms = data_flow.get("transforms", [])
            if transforms:
                sections.append("### Transforms")
                sections.append("")
                for transform in transforms:
                    inp_field = transform.get("input_field", "")
                    out_field = transform.get("output_field", "")
                    desc = transform.get("transformation_description", "")
                    sections.append(f"- `{inp_field}` -> `{out_field}`: {desc}")
                sections.append("")

        # Error handling section
        error_handling = doc_data.get("error_handling", [])
        if error_handling:
            sections.append("## Error Handling")
            sections.append("")
            for err in error_handling:
                condition = err.get("condition", "Unknown")
                action = err.get("action", "")
                sections.append(f"- **{condition}:** {action}")
            sections.append("")

        # SQL Operations section
        sql_ops = doc_data.get("sql_operations", [])
        if sql_ops:
            sections.append("## SQL Operations")
            sections.append("")
            for op in sql_ops:
                if isinstance(op, dict):
                    op_type = op.get("operation_type", "Unknown")
                    table = op.get("table", "")
                    sections.append(f"- **{op_type}** on {table}")
                else:
                    sections.append(f"- {op}")
            sections.append("")

        # CICS Operations section
        cics_ops = doc_data.get("cics_operations", [])
        if cics_ops:
            sections.append("## CICS Operations")
            sections.append("")
            for op in cics_ops:
                if isinstance(op, dict):
                    command = op.get("command", "Unknown")
                    sections.append(f"- {command}")
                else:
                    sections.append(f"- {op}")
            sections.append("")

        # Dead Code section
        dead_code = doc_data.get("dead_code", [])
        if dead_code:
            sections.append("## Dead Code")
            sections.append("")
            for dc in dead_code:
                name = dc.get("name", "Unknown")
                artifact_type = dc.get("artifact_type", "")
                reason = dc.get("reason", "")
                sections.append(f"- **{name}** ({artifact_type}): {reason}")
            sections.append("")

        # Flow diagram section
        flow_diagram = doc_data.get("flow_diagram", "")
        if flow_diagram:
            sections.append("## Flow Diagram")
            sections.append("")
            sections.append("```mermaid")
            sections.append(flow_diagram)
            sections.append("```")

        return "\n".join(sections)

    def get_program_summary(self, doc_data: dict[str, Any]) -> dict[str, str]:
        """Extract a summary dict for the system overview.

        Args:
            doc_data: Parsed documentation data.

        Returns:
            Dict with name, description, and type for the program.
        """
        header = doc_data.get("header", {})
        purpose = doc_data.get("purpose", {})

        program_id = header.get("program_id", "Unknown")
        skill_name = normalize_skill_name(program_id)
        summary = self._extract_summary(doc_data)
        program_type = purpose.get("program_type", header.get("file_type", "Unknown"))

        return {
            "name": skill_name,
            "program_id": program_id,
            "description": truncate_description(summary, max_length=200),
            "type": program_type,
        }
