"""Structural cross-validation between documentation and knowledge graph.

Compares documentation claims against KG triples and call semantics
to identify mismatches, conflicts, and orphans. Produces a compact
findings list for Imperator holistic review prompt injection.

The entire findings section targets ~300 tokens (~1200 chars) with
a maximum of 10 findings. Each finding is approximately 30 tokens.
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from enum import Enum

from war_rig.knowledge_graph.manager import KnowledgeGraphManager
from war_rig.knowledge_graph.models import EntityType, RelationType

logger = logging.getLogger(__name__)


class FindingType(str, Enum):
    """Classification of structural validation findings.

    Attributes:
        MISMATCH: Documentation says X, KG says Y.
        CONFLICT: KG has unresolved conflicting triples.
        ORPHAN: Documented but no KG triples, or vice versa.
        SEMANTICS: Call semantics inconsistency (outputs/inputs mismatch).
    """

    MISMATCH = "MISMATCH"
    CONFLICT = "CONFLICT"
    ORPHAN = "ORPHAN"
    SEMANTICS = "SEMANTICS"


@dataclass
class StructuralFinding:
    """A single structural validation finding.

    Attributes:
        finding_type: Category of the finding.
        description: One-line description (~30 tokens).
        affected_files: Source files affected by this finding.
    """

    finding_type: FindingType
    description: str
    affected_files: list[str] = field(default_factory=list)


class StructuralValidator:
    """Cross-validates documentation against KG triples and call semantics.

    Runs four check categories and returns a compact findings list
    suitable for injection into the Imperator's holistic review prompt.

    Args:
        kg_manager: Initialized KnowledgeGraphManager facade.
    """

    def __init__(self, kg_manager: KnowledgeGraphManager) -> None:
        self._kg = kg_manager

    async def validate(
        self,
        documented_programs: list[str],
        call_graph: dict[str, list[str]],
        cross_file_call_semantics: dict[str, dict] | None = None,
        max_findings: int = 10,
    ) -> list[StructuralFinding]:
        """Run all structural checks and return up to max_findings.

        Executes call mismatch, conflict, orphan, and semantics checks
        in sequence. Results are capped at max_findings total.

        Args:
            documented_programs: Program names from completed documentation.
            call_graph: Mapping of program -> list of called programs.
            cross_file_call_semantics: Optional call semantics dict with
                format ``{'CALLER->CALLEE': {'inputs': [...], 'outputs': [...]}}``.
            max_findings: Maximum number of findings to return.

        Returns:
            List of StructuralFinding, at most max_findings items.
        """
        findings: list[StructuralFinding] = []

        # Run each check category, collecting up to max_findings total
        for check_fn in [
            lambda: self._check_call_mismatches(
                documented_programs, call_graph
            ),
            lambda: self._check_conflicts(),
            lambda: self._check_orphans(documented_programs),
            lambda: self._check_call_semantics(cross_file_call_semantics),
        ]:
            if len(findings) >= max_findings:
                break
            try:
                batch = await check_fn()
                remaining = max_findings - len(findings)
                findings.extend(batch[:remaining])
            except Exception:
                logger.warning(
                    "Structural check failed, skipping",
                    exc_info=True,
                )

        return findings

    async def _check_call_mismatches(
        self,
        documented_programs: list[str],
        call_graph: dict[str, list[str]],
    ) -> list[StructuralFinding]:
        """Compare call_graph against KG CALLS triples.

        For each caller in the call graph, checks whether the KG has
        corresponding CALLS triples. Flags when documentation claims
        a CALLS relationship that has no KG evidence, or when the KG
        has a CALLS triple not reflected in the documentation.

        Args:
            documented_programs: Program names from completed docs.
            call_graph: Mapping of program -> list of called programs.

        Returns:
            List of mismatch findings.
        """
        findings: list[StructuralFinding] = []

        if not self._kg._store:
            return findings

        store = self._kg._store

        for caller, doc_callees in call_graph.items():
            try:
                caller_entity = await store.get_entity(
                    EntityType.PROGRAM, caller
                )
                if caller_entity is None or caller_entity.id is None:
                    # Caller not in KG at all - will be caught by orphan check
                    continue

                # Get all CALLS triples where this program is the subject
                triples = await store.get_triples_for_entity(
                    caller_entity.id, as_subject=True, as_object=False
                )
                kg_callee_ids = {
                    t.object_id
                    for t in triples
                    if t.predicate == RelationType.CALLS
                }

                # Resolve KG callee names
                kg_callee_names: set[str] = set()
                for obj_id in kg_callee_ids:
                    entity = await store.get_entity_by_id(obj_id)
                    if entity is not None:
                        kg_callee_names.add(entity.name)

                # Doc says CALLS X but KG has no evidence
                doc_set = set(doc_callees)
                for callee in doc_set - kg_callee_names:
                    findings.append(
                        StructuralFinding(
                            finding_type=FindingType.MISMATCH,
                            description=(
                                f"{caller} docs claim CALLS {callee}"
                                " -- no KG evidence"
                            ),
                            affected_files=[caller],
                        )
                    )

                # KG says CALLS Y but doc doesn't mention it
                for callee in kg_callee_names - doc_set:
                    findings.append(
                        StructuralFinding(
                            finding_type=FindingType.MISMATCH,
                            description=(
                                f"KG has {caller} CALLS {callee}"
                                " -- not in docs"
                            ),
                            affected_files=[caller],
                        )
                    )
            except Exception:
                logger.debug(
                    "Call mismatch check failed for %s", caller, exc_info=True
                )

        return findings

    async def _check_conflicts(self) -> list[StructuralFinding]:
        """Surface unresolved KG conflicts as findings.

        Queries the KG store for unresolved conflicts and formats
        each as a CONFLICT finding with the involved triple details.

        Returns:
            List of conflict findings.
        """
        findings: list[StructuralFinding] = []

        if not self._kg._store:
            return findings

        store = self._kg._store

        try:
            conflicts = await store.get_unresolved_conflicts()
            for conflict in conflicts:
                # Resolve the conflicting triples for description
                triple_a = None
                triple_b = None
                affected: list[str] = []

                try:
                    # Fetch triple details for a readable description
                    triples_all = await store.get_all_triples()
                    triple_map = {t.id: t for t in triples_all}
                    triple_a = triple_map.get(conflict.triple_a_id)
                    triple_b = triple_map.get(conflict.triple_b_id)
                except Exception:
                    pass

                if triple_a and triple_b:
                    # Try to get entity names for the subject
                    subj_a = await store.get_entity_by_id(
                        triple_a.subject_id
                    )
                    obj_a = await store.get_entity_by_id(triple_a.object_id)
                    subj_name = subj_a.name if subj_a else "?"
                    obj_name = obj_a.name if obj_a else "?"

                    desc = (
                        f"{subj_name} {triple_a.predicate.value}"
                        f" vs {triple_b.predicate.value}"
                        f" {obj_name} (unresolved)"
                    )
                    if subj_a:
                        affected.append(subj_name)
                else:
                    desc = (
                        f"Unresolved conflict #{conflict.id or '?'}"
                        " between triples"
                    )

                findings.append(
                    StructuralFinding(
                        finding_type=FindingType.CONFLICT,
                        description=desc,
                        affected_files=affected,
                    )
                )
        except Exception:
            logger.debug("Conflict check failed", exc_info=True)

        return findings

    async def _check_orphans(
        self,
        documented_programs: list[str],
    ) -> list[StructuralFinding]:
        """Find programs with docs but no KG triples, and vice versa.

        Compares the set of documented program names against KG entities
        of type PROGRAM. Flags orphans in both directions.

        Args:
            documented_programs: Program names from completed docs.

        Returns:
            List of orphan findings.
        """
        findings: list[StructuralFinding] = []

        if not self._kg._store:
            return findings

        store = self._kg._store

        try:
            all_entities = await store.get_all_entities()
            kg_programs = {
                e.name
                for e in all_entities
                if e.entity_type == EntityType.PROGRAM
            }
            doc_set = set(documented_programs)

            # Documented but not in KG
            for prog in sorted(doc_set - kg_programs):
                findings.append(
                    StructuralFinding(
                        finding_type=FindingType.ORPHAN,
                        description=f"{prog} documented but 0 KG triples",
                        affected_files=[prog],
                    )
                )

            # In KG but not documented
            for prog in sorted(kg_programs - doc_set):
                findings.append(
                    StructuralFinding(
                        finding_type=FindingType.ORPHAN,
                        description=f"{prog} in KG but not documented",
                        affected_files=[prog],
                    )
                )
        except Exception:
            logger.debug("Orphan check failed", exc_info=True)

        return findings

    async def _check_call_semantics(
        self,
        cross_file_call_semantics: dict[str, dict] | None,
    ) -> list[StructuralFinding]:
        """Check if caller outputs match callee expected inputs.

        For each ``CALLER->CALLEE`` entry in the semantics dict, compares
        the outputs documented for the caller against the inputs documented
        for the callee using simple name overlap. Flags mismatches where
        the caller produces outputs that do not appear in the callee's
        inputs (or vice versa).

        Args:
            cross_file_call_semantics: Optional semantics dict with format
                ``{'CALLER->CALLEE': {'inputs': [...], 'outputs': [...]}}``.

        Returns:
            List of semantics findings.
        """
        findings: list[StructuralFinding] = []

        if not cross_file_call_semantics:
            return findings

        # Build per-program input/output maps from the semantics data.
        # Each entry has format: "CALLER->CALLEE": {inputs: [...], outputs: [...]}
        # The "outputs" represent what the caller passes and "inputs" represent
        # what the callee expects.
        # Group by callee to check if callers' outputs match callee inputs.
        callee_expected_inputs: dict[str, set[str]] = {}
        caller_provided_outputs: dict[str, dict[str, set[str]]] = {}

        for pair_key, semantics in cross_file_call_semantics.items():
            if "->" not in pair_key:
                continue

            parts = pair_key.split("->", 1)
            if len(parts) != 2:
                continue

            caller = parts[0].strip()
            callee = parts[1].strip()

            outputs = {
                str(o).upper()
                for o in semantics.get("outputs", [])
                if o
            }
            inputs = {
                str(i).upper()
                for i in semantics.get("inputs", [])
                if i
            }

            if callee not in callee_expected_inputs:
                callee_expected_inputs[callee] = set()
            callee_expected_inputs[callee].update(inputs)

            if caller not in caller_provided_outputs:
                caller_provided_outputs[caller] = {}
            caller_provided_outputs[caller][callee] = outputs

        # Check for mismatches: caller outputs that don't match callee inputs
        try:
            for caller, callee_map in caller_provided_outputs.items():
                for callee, outputs in callee_map.items():
                    expected = callee_expected_inputs.get(callee, set())
                    if not outputs or not expected:
                        continue

                    # Check if any outputs are NOT in the callee's expected inputs
                    unmatched = outputs - expected
                    if unmatched and len(unmatched) == len(outputs):
                        # No overlap at all - flag it
                        findings.append(
                            StructuralFinding(
                                finding_type=FindingType.SEMANTICS,
                                description=(
                                    f"{caller}->{callee}: outputs do not"
                                    " match expected inputs"
                                ),
                                affected_files=[caller, callee],
                            )
                        )
        except Exception:
            logger.debug("Call semantics check failed", exc_info=True)

        return findings

    def format_findings(
        self,
        findings: list[StructuralFinding],
        max_tokens: int = 300,
    ) -> str:
        """Format findings as compact markdown for prompt injection.

        Produces a section suitable for injection into the Imperator's
        holistic review prompt. Budget is ~30 tokens per finding plus
        ~10 tokens for the header. If the formatted output exceeds
        max_tokens (estimated as chars / 4), truncates and appends
        ``... and N more``.

        Args:
            findings: List of structural findings to format.
            max_tokens: Approximate token budget for the output.

        Returns:
            Formatted markdown string, or empty string if no findings.
        """
        if not findings:
            return ""

        # Estimate: 1 token ~= 4 characters
        max_chars = max_tokens * 4

        header = f"## Structural Cross-Check ({len(findings)} findings)\n"
        lines: list[str] = []
        included_count = 0

        for finding in findings:
            line = f"- {finding.finding_type.value}: {finding.description}"
            lines.append(line)
            included_count += 1

            # Check if we are approaching the char budget
            current_text = header + "\n".join(lines)
            if len(current_text) > max_chars:
                # Remove the last line that pushed us over
                lines.pop()
                included_count -= 1
                remaining = len(findings) - included_count
                if remaining > 0:
                    lines.append(f"... and {remaining} more")
                break

        if not lines:
            return ""

        return header + "\n".join(lines) + "\n"
