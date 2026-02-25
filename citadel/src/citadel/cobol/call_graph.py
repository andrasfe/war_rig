"""Call graph builder for COBOL PROCEDURE DIVISION paragraphs.

Builds a directed call graph from parsed paragraphs, computes topological
ordering (leaves first) for translation dependency order, and records
PERFORM THRU ranges for structural analysis.

Adapted from cbexplore's rosetta.parse.call_graph.
"""

from __future__ import annotations

import logging
from dataclasses import dataclass

from citadel.cobol.procedure_division import ParagraphInfo

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Data classes
# ---------------------------------------------------------------------------

@dataclass
class CallGraph:
    """Directed call graph of COBOL paragraphs."""

    adjacency: dict[str, list[str]]
    """Paragraph name -> list of called paragraph names."""

    topological_order: list[str]
    """Paragraphs sorted leaves-first (reverse post-order DFS)."""

    perform_thru_ranges: list[dict]
    """List of PERFORM THRU ranges.

    Each entry has keys: from, thru, paragraphs_included.
    """

    entry_paragraph: str
    """The root/entry point paragraph of the program."""


# ---------------------------------------------------------------------------
# Builder
# ---------------------------------------------------------------------------

class CallGraphBuilder:
    """Builds a CallGraph from parsed ParagraphInfo objects."""

    def build(
        self, paragraphs: list[ParagraphInfo]
    ) -> CallGraph:
        """Build a call graph from parsed paragraphs.

        Args:
            paragraphs: List of ParagraphInfo objects in source order.

        Returns:
            A CallGraph with adjacency list, topological order,
            PERFORM THRU ranges, and identified entry paragraph.
        """
        if not paragraphs:
            return CallGraph(
                adjacency={},
                topological_order=[],
                perform_thru_ranges=[],
                entry_paragraph="",
            )

        para_names = [p.name for p in paragraphs]
        para_index = {
            name: idx for idx, name in enumerate(para_names)
        }

        adjacency = self._build_adjacency(
            paragraphs, para_index
        )
        perform_thru_ranges = self._build_thru_ranges(
            paragraphs, para_names
        )

        exit_paragraphs = {
            name
            for name in para_names
            if name.endswith("-EXIT")
            and not adjacency.get(name, [])
        }

        topological_order = self._topological_sort(
            adjacency, para_names
        )
        topological_order = [
            name
            for name in topological_order
            if name not in exit_paragraphs
        ]

        entry_paragraph = self._find_entry_paragraph(
            adjacency, para_names
        )

        graph = CallGraph(
            adjacency=adjacency,
            topological_order=topological_order,
            perform_thru_ranges=perform_thru_ranges,
            entry_paragraph=entry_paragraph,
        )

        logger.info(
            "Call graph built: %d paragraphs, entry=%s, "
            "%d THRU ranges",
            len(adjacency),
            entry_paragraph,
            len(perform_thru_ranges),
        )
        return graph

    # ----- adjacency list -----

    def _build_adjacency(
        self,
        paragraphs: list[ParagraphInfo],
        para_index: dict[str, int],
    ) -> dict[str, list[str]]:
        """Build the adjacency list from paragraph PERFORM targets."""
        adjacency: dict[str, list[str]] = {}

        for para in paragraphs:
            targets: list[str] = []
            seen: set[str] = set()

            for perform in para.performs:
                target = perform.get("target", "")
                if not target:
                    continue

                if target not in para_index:
                    logger.debug(
                        "Paragraph '%s' performs '%s' which "
                        "is not a known paragraph; skipping",
                        para.name,
                        target,
                    )
                    continue

                if target not in seen:
                    seen.add(target)
                    targets.append(target)

            adjacency[para.name] = targets

        return adjacency

    # ----- PERFORM THRU ranges -----

    def _build_thru_ranges(
        self,
        paragraphs: list[ParagraphInfo],
        para_names: list[str],
    ) -> list[dict]:
        """Build the list of PERFORM THRU ranges."""
        name_to_idx = {
            name: idx
            for idx, name in enumerate(para_names)
        }
        ranges: list[dict] = []
        seen_ranges: set[tuple[str, str]] = set()

        for para in paragraphs:
            for perform in para.performs:
                if (
                    perform.get("mechanism")
                    != "PERFORM THRU"
                ):
                    continue

                from_para = perform.get("target", "")
                thru_para = perform.get("thru", "")

                if not from_para or not thru_para:
                    continue

                key = (from_para, thru_para)
                if key in seen_ranges:
                    continue
                seen_ranges.add(key)

                from_idx = name_to_idx.get(from_para)
                thru_idx = name_to_idx.get(thru_para)

                if from_idx is None or thru_idx is None:
                    logger.warning(
                        "PERFORM THRU range %s -> %s: "
                        "one or both paragraphs not found",
                        from_para,
                        thru_para,
                    )
                    continue

                if from_idx > thru_idx:
                    logger.warning(
                        "PERFORM THRU range %s -> %s: "
                        "'from' comes after 'thru'",
                        from_para,
                        thru_para,
                    )
                    continue

                included = para_names[
                    from_idx : thru_idx + 1
                ]

                ranges.append({
                    "from": from_para,
                    "thru": thru_para,
                    "paragraphs_included": included,
                })

        return ranges

    # ----- topological sort -----

    def _topological_sort(
        self,
        adjacency: dict[str, list[str]],
        para_names: list[str],
    ) -> list[str]:
        """Compute topological ordering with leaves first."""
        all_nodes = set(para_names)
        for targets in adjacency.values():
            all_nodes.update(targets)

        visited: set[str] = set()
        in_stack: set[str] = set()
        post_order: list[str] = []

        def _dfs(node: str) -> None:
            """Iterative DFS that records nodes in post-order."""
            stack: list[tuple[str, int]] = []
            stack.append((node, 0))
            visited.add(node)
            in_stack.add(node)

            while stack:
                current, neighbor_idx = stack[-1]
                neighbors = adjacency.get(current, [])

                if neighbor_idx < len(neighbors):
                    stack[-1] = (
                        current,
                        neighbor_idx + 1,
                    )
                    neighbor = neighbors[neighbor_idx]

                    if neighbor in in_stack:
                        logger.debug(
                            "Cycle detected: %s -> %s",
                            current,
                            neighbor,
                        )
                        continue

                    if neighbor not in visited:
                        visited.add(neighbor)
                        in_stack.add(neighbor)
                        stack.append((neighbor, 0))
                else:
                    stack.pop()
                    in_stack.discard(current)
                    post_order.append(current)

        for name in para_names:
            if name not in visited:
                _dfs(name)

        for name in sorted(all_nodes - visited):
            _dfs(name)

        return post_order

    # ----- entry paragraph -----

    def _find_entry_paragraph(
        self,
        adjacency: dict[str, list[str]],
        para_names: list[str],
    ) -> str:
        """Identify the entry paragraph of the program."""
        if not para_names:
            return ""

        called: set[str] = set()
        for targets in adjacency.values():
            called.update(targets)

        for name in para_names:
            if name not in called:
                return name

        return para_names[0]
