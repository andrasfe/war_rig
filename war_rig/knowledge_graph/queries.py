"""Graph query helpers for the knowledge graph.

This module provides higher-level query operations built on top of the
KnowledgeGraphStore interface. These helpers support:

- 2-hop neighborhood retrieval with filtering
- Triple delta calculation for convergence tracking
- Entity search and discovery
- Relationship path finding between entities

These queries are used by the Context Injector, Challenger cross-check,
and convergence monitoring components.

Example:
    helper = GraphQueryHelper(store)
    programs_called = await helper.get_called_programs("ACCT0100")
    is_converged = await helper.check_convergence("pass_1", "pass_2", threshold=0.05)
"""

import logging

from war_rig.knowledge_graph.models import (
    Entity,
    EntityType,  # noqa: F401 (used in implementation)
    Neighborhood,  # noqa: F401 (used in implementation)
    RelationType,  # noqa: F401 (used in implementation)
    Triple,
    TripleDelta,  # noqa: F401 (used in implementation)
)
from war_rig.knowledge_graph.store import KnowledgeGraphStore

logger = logging.getLogger(__name__)


class GraphQueryHelper:
    """High-level query operations on the knowledge graph.

    Wraps the store interface with domain-specific query patterns
    needed by the War Rig pipeline.

    Args:
        store: The knowledge graph store to query.
    """

    def __init__(self, store: KnowledgeGraphStore) -> None:
        """Initialize the query helper.

        Args:
            store: Knowledge graph store.
        """
        self._store = store

    async def get_called_programs(self, program_name: str) -> list[Entity]:
        """Get all programs called by a given program.

        Args:
            program_name: Name of the calling program.

        Returns:
            List of PROGRAM entities called by the given program.
        """
        entity = await self._store.get_entity(EntityType.PROGRAM, program_name)
        if entity is None:
            return []
        triples = await self._store.get_triples_for_entity(
            entity.id, as_subject=True, as_object=False  # type: ignore[arg-type]
        )
        results: list[Entity] = []
        for t in triples:
            if t.predicate == RelationType.CALLS:
                obj = await self._store.get_entity_by_id(t.object_id)
                if obj is not None:
                    results.append(obj)
        return results

    async def get_calling_programs(self, program_name: str) -> list[Entity]:
        """Get all programs that call a given program.

        Args:
            program_name: Name of the called program.

        Returns:
            List of PROGRAM entities that call the given program.
        """
        entity = await self._store.get_entity(EntityType.PROGRAM, program_name)
        if entity is None:
            return []
        triples = await self._store.get_triples_for_entity(
            entity.id, as_subject=False, as_object=True  # type: ignore[arg-type]
        )
        results: list[Entity] = []
        for t in triples:
            if t.predicate == RelationType.CALLS:
                subj = await self._store.get_entity_by_id(t.subject_id)
                if subj is not None:
                    results.append(subj)
        return results

    async def get_datasets_for_program(
        self,
        program_name: str,
    ) -> dict[str, list[Entity]]:
        """Get datasets read and written by a program.

        Args:
            program_name: Name of the program.

        Returns:
            Dict with keys "reads" and "writes", each containing
            a list of DATASET entities.
        """
        entity = await self._store.get_entity(EntityType.PROGRAM, program_name)
        if entity is None:
            return {"reads": [], "writes": []}
        triples = await self._store.get_triples_for_entity(
            entity.id, as_subject=True, as_object=False  # type: ignore[arg-type]
        )
        reads: list[Entity] = []
        writes: list[Entity] = []
        for t in triples:
            if t.predicate == RelationType.READS:
                obj = await self._store.get_entity_by_id(t.object_id)
                if obj is not None:
                    reads.append(obj)
            elif t.predicate == RelationType.WRITES:
                obj = await self._store.get_entity_by_id(t.object_id)
                if obj is not None:
                    writes.append(obj)
        return {"reads": reads, "writes": writes}

    async def get_copybooks_for_program(self, program_name: str) -> list[Entity]:
        """Get all copybooks included by a program.

        Args:
            program_name: Name of the program.

        Returns:
            List of COPYBOOK entities.
        """
        entity = await self._store.get_entity(EntityType.PROGRAM, program_name)
        if entity is None:
            return []
        triples = await self._store.get_triples_for_entity(
            entity.id, as_subject=True, as_object=False  # type: ignore[arg-type]
        )
        results: list[Entity] = []
        for t in triples:
            if t.predicate == RelationType.INCLUDES:
                obj = await self._store.get_entity_by_id(t.object_id)
                if obj is not None:
                    results.append(obj)
        return results

    async def get_programs_sharing_copybook(
        self,
        copybook_name: str,
    ) -> list[Entity]:
        """Get all programs that include a given copybook.

        Useful for terminology consistency checking in the Challenger.

        Args:
            copybook_name: Name of the copybook.

        Returns:
            List of PROGRAM entities that include this copybook.
        """
        entity = await self._store.get_entity(EntityType.COPYBOOK, copybook_name)
        if entity is None:
            return []
        triples = await self._store.get_triples_for_entity(
            entity.id, as_subject=False, as_object=True  # type: ignore[arg-type]
        )
        results: list[Entity] = []
        for t in triples:
            if t.predicate == RelationType.INCLUDES:
                subj = await self._store.get_entity_by_id(t.subject_id)
                if subj is not None:
                    results.append(subj)
        return results

    async def get_jcl_context_for_program(
        self,
        program_name: str,
    ) -> dict[str, list[Entity]]:
        """Get JCL context for a program (2-hop via JCL_STEP).

        Returns the JCL steps that execute this program, the parent
        JCL jobs, and other programs in the same job stream.

        Args:
            program_name: Name of the program.

        Returns:
            Dict with keys "steps", "jobs", "co_programs" containing
            the related entities.
        """
        entity = await self._store.get_entity(EntityType.PROGRAM, program_name)
        if entity is None:
            return {"steps": [], "jobs": [], "co_programs": []}

        # 1. Find JCL_STEP entities that EXECUTE this program
        program_triples = await self._store.get_triples_for_entity(
            entity.id, as_subject=False, as_object=True  # type: ignore[arg-type]
        )
        steps: list[Entity] = []
        for t in program_triples:
            if t.predicate == RelationType.EXECUTES:
                step = await self._store.get_entity_by_id(t.subject_id)
                if step is not None:
                    steps.append(step)

        # 2. Find JCL_JOB entities that CONTAIN_STEP those steps
        jobs: list[Entity] = []
        seen_job_ids: set[int] = set()
        for step in steps:
            step_triples = await self._store.get_triples_for_entity(
                step.id, as_subject=False, as_object=True  # type: ignore[arg-type]
            )
            for t in step_triples:
                if t.predicate == RelationType.CONTAINS_STEP:
                    job = await self._store.get_entity_by_id(t.subject_id)
                    if job is not None and job.id not in seen_job_ids:
                        jobs.append(job)
                        seen_job_ids.add(job.id)  # type: ignore[arg-type]

        # 3. Find all steps in those jobs, then find co-programs
        co_programs: list[Entity] = []
        seen_program_ids: set[int] = set()
        for job in jobs:
            job_triples = await self._store.get_triples_for_entity(
                job.id, as_subject=True, as_object=False  # type: ignore[arg-type]
            )
            for t in job_triples:
                if t.predicate == RelationType.CONTAINS_STEP:
                    all_step = await self._store.get_entity_by_id(t.object_id)
                    if all_step is not None:
                        # 4. Find programs executed by each step
                        all_step_triples = await self._store.get_triples_for_entity(
                            all_step.id,  # type: ignore[arg-type]
                            as_subject=True,
                            as_object=False,
                        )
                        for st in all_step_triples:
                            if st.predicate == RelationType.EXECUTES:
                                prog = await self._store.get_entity_by_id(
                                    st.object_id
                                )
                                if (
                                    prog is not None
                                    and prog.id != entity.id
                                    and prog.id not in seen_program_ids
                                ):
                                    co_programs.append(prog)
                                    seen_program_ids.add(prog.id)  # type: ignore[arg-type]

        return {"steps": steps, "jobs": jobs, "co_programs": co_programs}

    async def check_convergence(
        self,
        pass_from: str,
        pass_to: str,
        threshold: float = 0.05,
    ) -> bool:
        """Check if the graph has converged between two passes.

        Per spec Section 8.1, convergence is reached when the triple
        delta drops below the configured threshold (default 5%).

        Args:
            pass_from: Earlier pass identifier.
            pass_to: Later pass identifier.
            threshold: Maximum change rate for convergence (default 0.05).

        Returns:
            True if change rate is below threshold.
        """
        delta = await self._store.compute_delta(pass_from, pass_to)
        logger.info(
            "Delta %s -> %s: %d changes out of %d triples (%.2f%% change rate)",
            pass_from,
            pass_to,
            delta.change_count,
            delta.total_triples,
            delta.change_rate * 100,
        )
        return delta.change_rate < threshold

    async def get_unconfirmed_triples(self) -> list[Triple]:
        """Get all unconfirmed triples for manual review.

        Per spec Section 13, unconfirmed triples after two passes
        should be flagged for manual review.

        Returns:
            List of Triple objects where confirmed is False.
        """
        all_triples = await self._store.get_all_triples()
        return [t for t in all_triples if not t.confirmed]
