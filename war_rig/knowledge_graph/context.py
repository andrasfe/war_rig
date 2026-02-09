"""Context formatting for knowledge graph injection into prompts.

This module formats a Neighborhood subgraph into a structured text block
suitable for injection into Scribe and Challenger prompts. The formatted
context gives agents awareness of a module's position in the broader system
without overwhelming them with full source code of related modules.

The context block has a strict 500-token budget (per spec Section 6.3).
When the neighborhood exceeds this budget, relationships are summarized
rather than enumerated individually.

Output format (per spec Section 6.2):
    ## System Context (from knowledge graph)

    The program you are documenting exists in this context:

    CALLS OUT TO:
      - PROGRAM:ACCT0200 (Account validation subroutine)
      ...

Example:
    formatter = ContextFormatter(max_tokens=500)
    context_block = formatter.format(neighborhood)
"""

import logging

from war_rig.knowledge_graph.models import (
    Entity,
    EntityType,  # noqa: F401 (used in implementation)
    Neighborhood,
    RelationType,  # noqa: F401 (used in implementation)
    Triple,
)

logger = logging.getLogger(__name__)

# Default max tokens for context block
DEFAULT_MAX_CONTEXT_TOKENS = 500

# Approximate chars per token for budget estimation
_CHARS_PER_TOKEN = 4


class ContextFormatter:
    """Formats graph neighborhood into structured prompt context.

    Respects a configurable token budget and summarizes when the
    neighborhood is too large. Groups relationships by type for
    readability.

    Args:
        max_tokens: Maximum token budget for the context block.
    """

    def __init__(self, max_tokens: int = DEFAULT_MAX_CONTEXT_TOKENS) -> None:
        """Initialize the context formatter.

        Args:
            max_tokens: Maximum token budget for the context block.
        """
        self._max_tokens = max_tokens

    def format(self, neighborhood: Neighborhood | None) -> str:
        """Format a neighborhood into a structured context block.

        If neighborhood is None (entity not in graph yet), returns an
        empty string — this is normal for Pass 1 cold start.

        The output is a Markdown block suitable for prompt injection,
        with relationships grouped by type.

        Args:
            neighborhood: The subgraph to format, or None.

        Returns:
            Formatted context string, or empty string if no context.
        """
        if neighborhood is None:
            return ""

        groups = self._group_triples_by_relation(neighborhood)
        if not groups:
            return ""

        # Build the body by enumerating each group
        body_lines: list[str] = []
        for category, entries in groups.items():
            body_lines.append(f"{category}:")
            for _triple, entity in entries:
                body_lines.append(f"  - {entity.qualified_name}")

        body = "\n".join(body_lines)

        # Check token budget; summarize if over
        max_body_chars = self._max_tokens * _CHARS_PER_TOKEN
        if self._estimate_tokens(body) > self._max_tokens:
            logger.debug(
                "Context body exceeds %d token budget, summarizing",
                self._max_tokens,
            )
            body = self._summarize(groups, max_body_chars)

        header = (
            "## System Context (from knowledge graph)\n\n"
            "The program you are documenting exists in this context:\n\n"
        )
        footer = (
            "\n\n_This context is auto-generated from the knowledge graph. "
            "Use it to inform your documentation but verify against "
            "the source code._"
        )

        return header + body + footer

    def format_for_challenger(self, neighborhood: Neighborhood | None) -> str:
        """Format neighborhood context specifically for the Challenger.

        Similar to format() but includes additional structural hints
        for cross-checking (per spec Section 7.1):
        - Expected datasets for this program
        - Expected calls
        - Canonical names for shared copybooks

        Args:
            neighborhood: The subgraph to format, or None.

        Returns:
            Formatted context string for Challenger prompts.
        """
        if neighborhood is None:
            return ""

        base = self.format(neighborhood)
        if not base:
            return ""

        groups = self._group_triples_by_relation(neighborhood)

        # Collect cross-check hints
        datasets: list[str] = []
        calls: list[str] = []
        copybooks: list[str] = []

        for category, entries in groups.items():
            for _triple, entity in entries:
                if category in ("READS FROM", "WRITES TO"):
                    datasets.append(entity.name)
                elif category in ("CALLS OUT TO",):
                    calls.append(entity.name)
                elif category in ("INCLUDES",):
                    copybooks.append(entity.name)

        hints_lines: list[str] = ["\n\n### Cross-Check Hints"]
        if datasets:
            hints_lines.append(
                f"- Expected datasets: {', '.join(sorted(set(datasets)))}"
            )
        if calls:
            hints_lines.append(
                f"- Expected calls: {', '.join(sorted(set(calls)))}"
            )
        if copybooks:
            hints_lines.append(
                f"- Shared copybooks: {', '.join(sorted(set(copybooks)))}"
            )

        # Only append hints if there is at least one category populated
        if len(hints_lines) > 1:
            return base + "\n".join(hints_lines)

        return base

    def _group_triples_by_relation(
        self,
        neighborhood: Neighborhood,
    ) -> dict[str, list[tuple[Triple, Entity]]]:
        """Group triples by relationship category relative to target.

        Categories (for PROGRAM target):
        - "CALLS OUT TO": target is subject, predicate is CALLS
        - "CALLED BY": target is object, predicate is CALLS
        - "READS FROM": target is subject, predicate is READS
        - "WRITES TO": target is subject, predicate is WRITES
        - "INCLUDES": target is subject, predicate is INCLUDES
        - "EXECUTED BY": target is object, predicate is EXECUTES

        Args:
            neighborhood: The subgraph to categorize.

        Returns:
            Dict mapping category labels to (triple, related_entity) pairs.
        """
        entity_lookup: dict[int | None, Entity] = {
            e.id: e for e in neighborhood.entities
        }
        target_id = neighborhood.target.id

        groups: dict[str, list[tuple[Triple, Entity]]] = {}

        for triple in neighborhood.triples:
            is_subject = triple.subject_id == target_id
            is_object = triple.object_id == target_id
            predicate = triple.predicate

            category: str | None = None
            related_entity: Entity | None = None

            if is_subject and predicate == RelationType.CALLS:
                category = "CALLS OUT TO"
                related_entity = entity_lookup.get(triple.object_id)
            elif is_object and predicate == RelationType.CALLS:
                category = "CALLED BY"
                related_entity = entity_lookup.get(triple.subject_id)
            elif is_subject and predicate == RelationType.READS:
                category = "READS FROM"
                related_entity = entity_lookup.get(triple.object_id)
            elif is_subject and predicate == RelationType.WRITES:
                category = "WRITES TO"
                related_entity = entity_lookup.get(triple.object_id)
            elif is_subject and predicate == RelationType.INCLUDES:
                category = "INCLUDES"
                related_entity = entity_lookup.get(triple.object_id)
            elif is_object and predicate == RelationType.EXECUTES:
                category = "EXECUTED BY"
                related_entity = entity_lookup.get(triple.subject_id)
            elif is_subject and predicate == RelationType.QUERIES:
                category = "QUERIES"
                related_entity = entity_lookup.get(triple.object_id)
            elif is_subject and predicate == RelationType.MODIFIES:
                category = "MODIFIES"
                related_entity = entity_lookup.get(triple.object_id)
            elif is_subject and predicate == RelationType.PERFORMS:
                category = "PERFORMS"
                related_entity = entity_lookup.get(triple.object_id)
            else:
                # Generic fallback: use predicate name as category
                if is_subject:
                    category = predicate.value
                    related_entity = entity_lookup.get(triple.object_id)
                elif is_object:
                    category = f"{predicate.value} (inverse)"
                    related_entity = entity_lookup.get(triple.subject_id)

            if category is None or related_entity is None:
                logger.debug(
                    "Skipping triple %s: category=%s, related_entity=%s",
                    triple.id,
                    category,
                    related_entity,
                )
                continue

            groups.setdefault(category, []).append((triple, related_entity))

        return groups

    def _estimate_tokens(self, text: str) -> int:
        """Estimate token count for a text string.

        Uses a simple chars/4 heuristic — sufficient for budget enforcement.

        Args:
            text: Text to estimate.

        Returns:
            Estimated token count.
        """
        return len(text) // _CHARS_PER_TOKEN

    def _summarize(
        self,
        groups: dict[str, list[tuple[Triple, Entity]]],
        max_chars: int,
    ) -> str:
        """Summarize relationship groups to fit within budget.

        When the full enumeration exceeds the token budget, this method
        produces a compressed version that lists counts instead of
        individual entities.

        Args:
            groups: Categorized relationship groups.
            max_chars: Maximum characters for the summary.

        Returns:
            Summarized context string.
        """
        _MAX_PREVIEW_NAMES = 3
        lines: list[str] = []

        for category, entries in groups.items():
            count = len(entries)
            names = [entity.name for _triple, entity in entries]
            preview = ", ".join(names[:_MAX_PREVIEW_NAMES])
            if count > _MAX_PREVIEW_NAMES:
                preview += ", ..."
            # Determine entity type label from the first entry
            entity_type_label = entries[0][1].entity_type.value.lower() + "s"
            lines.append(f"{category}: {count} {entity_type_label} ({preview})")

        return "\n".join(lines)
