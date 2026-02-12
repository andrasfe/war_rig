"""Knowledge graph tools for CodeWhisper SDK.

This module provides KG-backed tools that allow the CodeWhisper agent
to query structural relationships on-demand during agentic README
generation. Tools follow the ``ToolDefinition`` pattern used by
``codewhisper.tools.knowledge``.

Tools:
    - kg_get_hub_entities: Most-connected programs/datasets
    - kg_get_program_relationships: Calls, called-by, datasets, copybooks
    - kg_get_programs_for_dataset: Programs that read/write a dataset
    - kg_get_jcl_context: JCL steps, jobs, co-programs for a program
    - kg_get_copybook_users: Programs sharing a copybook

Example:
    from war_rig.agents.kg_tools import create_kg_tools

    tools = create_kg_tools(kg_manager)
    sdk.tool_registry.register_all(tools)
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING

from codewhisper.core.tool_protocol import ToolDefinition

if TYPE_CHECKING:
    from war_rig.knowledge_graph.manager import KnowledgeGraphManager

logger = logging.getLogger(__name__)


def create_kg_tools(kg_manager: KnowledgeGraphManager) -> list[ToolDefinition]:
    """Create knowledge graph tools with injected manager.

    Each tool is an async closure over the injected ``kg_manager``.
    Safe when KG is disabled — tools simply return "KG not available".

    Args:
        kg_manager: Initialized KnowledgeGraphManager instance.

    Returns:
        List of ToolDefinition objects for KG query tools.
    """

    async def kg_get_hub_entities(limit: int = 5) -> str:
        """Get the most-connected programs and datasets in the system.

        Args:
            limit: Maximum number of hub entities to return.

        Returns:
            Formatted markdown listing hub entities with connection counts.
        """
        if not kg_manager.enabled:
            return "Knowledge graph is not available."

        try:
            summary = await kg_manager.get_system_summary(max_tokens=600)
            if not summary:
                return "Knowledge graph is empty — no entities found."
            return summary
        except Exception as e:
            logger.warning("kg_get_hub_entities failed: %s", e)
            return f"Error querying knowledge graph: {e}"

    async def kg_get_program_relationships(program_name: str) -> str:
        """Get all relationships for a program: calls, called-by, datasets, copybooks.

        Args:
            program_name: Name of the program (e.g., "CBACT01C").

        Returns:
            Formatted markdown with the program's relationships.
        """
        if not kg_manager.enabled or kg_manager._query_helper is None:
            return "Knowledge graph is not available."

        try:
            qh = kg_manager._query_helper
            called = await qh.get_called_programs(program_name)
            callers = await qh.get_calling_programs(program_name)
            datasets = await qh.get_datasets_for_program(program_name)
            copybooks = await qh.get_copybooks_for_program(program_name)

            lines = [f"## Relationships for {program_name}", ""]

            if called:
                lines.append("**Calls:**")
                for e in called:
                    lines.append(f"- {e.name}")
                lines.append("")

            if callers:
                lines.append("**Called by:**")
                for e in callers:
                    lines.append(f"- {e.name}")
                lines.append("")

            reads = datasets.get("reads", [])
            writes = datasets.get("writes", [])
            if reads:
                lines.append("**Reads:**")
                for e in reads:
                    lines.append(f"- {e.name}")
                lines.append("")
            if writes:
                lines.append("**Writes:**")
                for e in writes:
                    lines.append(f"- {e.name}")
                lines.append("")

            if copybooks:
                lines.append("**Copybooks:**")
                for e in copybooks:
                    lines.append(f"- {e.name}")
                lines.append("")

            if len(lines) == 2:
                return f"No relationships found for program '{program_name}'."

            return "\n".join(lines)
        except Exception as e:
            logger.warning("kg_get_program_relationships failed: %s", e)
            return f"Error querying relationships for '{program_name}': {e}"

    async def kg_get_programs_for_dataset(dataset_name: str) -> str:
        """Get programs that read or write a dataset.

        Args:
            dataset_name: Name of the dataset.

        Returns:
            Formatted markdown listing readers and writers.
        """
        if not kg_manager.enabled or kg_manager._store is None:
            return "Knowledge graph is not available."

        try:
            from war_rig.knowledge_graph.models import EntityType, RelationType

            store = kg_manager._store
            entity = await store.get_entity(EntityType.DATASET, dataset_name)
            if entity is None:
                return f"Dataset '{dataset_name}' not found in knowledge graph."

            triples = await store.get_triples_for_entity(
                entity.id, as_subject=False, as_object=True  # type: ignore[arg-type]
            )

            readers: list[str] = []
            writers: list[str] = []
            for t in triples:
                subj = await store.get_entity_by_id(t.subject_id)
                if subj is None:
                    continue
                if t.predicate == RelationType.READS:
                    readers.append(subj.name)
                elif t.predicate == RelationType.WRITES:
                    writers.append(subj.name)

            lines = [f"## Programs for dataset {dataset_name}", ""]
            if readers:
                lines.append(f"**Readers:** {', '.join(readers)}")
            if writers:
                lines.append(f"**Writers:** {', '.join(writers)}")
            if not readers and not writers:
                lines.append("No programs found for this dataset.")
            return "\n".join(lines)
        except Exception as e:
            logger.warning("kg_get_programs_for_dataset failed: %s", e)
            return f"Error querying dataset '{dataset_name}': {e}"

    async def kg_get_jcl_context(program_name: str) -> str:
        """Get JCL context for a program: steps, jobs, co-programs.

        Args:
            program_name: Name of the program.

        Returns:
            Formatted markdown with JCL execution context.
        """
        if not kg_manager.enabled or kg_manager._query_helper is None:
            return "Knowledge graph is not available."

        try:
            ctx = await kg_manager._query_helper.get_jcl_context_for_program(
                program_name
            )

            lines = [f"## JCL Context for {program_name}", ""]

            steps = ctx.get("steps", [])
            jobs = ctx.get("jobs", [])
            co_programs = ctx.get("co_programs", [])

            if jobs:
                lines.append("**JCL Jobs:**")
                for e in jobs:
                    lines.append(f"- {e.name}")
                lines.append("")

            if steps:
                lines.append("**JCL Steps:**")
                for e in steps:
                    lines.append(f"- {e.name}")
                lines.append("")

            if co_programs:
                lines.append("**Co-programs (in same job stream):**")
                for e in co_programs:
                    lines.append(f"- {e.name}")
                lines.append("")

            if not steps and not jobs and not co_programs:
                return f"No JCL context found for program '{program_name}'."

            return "\n".join(lines)
        except Exception as e:
            logger.warning("kg_get_jcl_context failed: %s", e)
            return f"Error querying JCL context for '{program_name}': {e}"

    async def kg_get_copybook_users(copybook_name: str) -> str:
        """Get all programs that include a specific copybook.

        Args:
            copybook_name: Name of the copybook.

        Returns:
            Formatted markdown listing programs that use this copybook.
        """
        if not kg_manager.enabled or kg_manager._query_helper is None:
            return "Knowledge graph is not available."

        try:
            programs = await kg_manager._query_helper.get_programs_sharing_copybook(
                copybook_name
            )

            if not programs:
                return f"No programs found using copybook '{copybook_name}'."

            lines = [f"## Programs using copybook {copybook_name}", ""]
            for p in programs:
                lines.append(f"- {p.name}")
            return "\n".join(lines)
        except Exception as e:
            logger.warning("kg_get_copybook_users failed: %s", e)
            return f"Error querying copybook '{copybook_name}': {e}"

    return [
        ToolDefinition(
            name="kg_get_hub_entities",
            description=(
                "Get the most-connected programs and datasets in the system "
                "from the knowledge graph. Shows hub programs with their "
                "connection counts, data flow hotspots, and shared copybooks. "
                "Use this to understand the system's architectural backbone."
            ),
            parameters={
                "type": "object",
                "properties": {
                    "limit": {
                        "type": "integer",
                        "description": "Maximum number of hub entities to return",
                        "default": 5,
                        "minimum": 1,
                        "maximum": 20,
                    },
                },
                "required": [],
            },
            handler=kg_get_hub_entities,
        ),
        ToolDefinition(
            name="kg_get_program_relationships",
            description=(
                "Get all structural relationships for a specific program: "
                "which programs it calls, which call it, datasets it reads/writes, "
                "and copybooks it includes. Essential for understanding a program's "
                "role in the system architecture."
            ),
            parameters={
                "type": "object",
                "properties": {
                    "program_name": {
                        "type": "string",
                        "description": "Name of the program (e.g., 'CBACT01C')",
                    },
                },
                "required": ["program_name"],
            },
            handler=kg_get_program_relationships,
        ),
        ToolDefinition(
            name="kg_get_programs_for_dataset",
            description=(
                "Get all programs that read from or write to a specific dataset. "
                "Use this to trace data flow through the system and understand "
                "which programs share data via files or databases."
            ),
            parameters={
                "type": "object",
                "properties": {
                    "dataset_name": {
                        "type": "string",
                        "description": "Name of the dataset (e.g., 'ACCTFILE')",
                    },
                },
                "required": ["dataset_name"],
            },
            handler=kg_get_programs_for_dataset,
        ),
        ToolDefinition(
            name="kg_get_jcl_context",
            description=(
                "Get JCL execution context for a program: which JCL steps "
                "execute it, which JCL jobs contain those steps, and which "
                "other programs run in the same job stream. Use this to "
                "understand batch scheduling and job flow."
            ),
            parameters={
                "type": "object",
                "properties": {
                    "program_name": {
                        "type": "string",
                        "description": "Name of the program to look up JCL context for",
                    },
                },
                "required": ["program_name"],
            },
            handler=kg_get_jcl_context,
        ),
        ToolDefinition(
            name="kg_get_copybook_users",
            description=(
                "Get all programs that include a specific copybook. "
                "Use this to identify shared data structures and understand "
                "which programs share common field definitions."
            ),
            parameters={
                "type": "object",
                "properties": {
                    "copybook_name": {
                        "type": "string",
                        "description": "Name of the copybook (e.g., 'CBACTM01')",
                    },
                },
                "required": ["copybook_name"],
            },
            handler=kg_get_copybook_users,
        ),
    ]
