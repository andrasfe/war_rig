"""Bundle grouping strategies for Pass 2 coherence merging.

Pass 2 of the summarization pipeline groups bundle summaries into
segments before running the coherence merge LLM pass.  The grouping
strategy controls which bundles are merged together and therefore
affects the quality of coreference resolution and argument threading.

This module defines a :class:`BundleGroupingStrategy` protocol and a
default :class:`AdjacentSequentialGrouping` implementation that
preserves source-file adjacency.
"""

from __future__ import annotations

from typing import Protocol, runtime_checkable

from war_rig.models.summaries import BundleSummary


@runtime_checkable
class BundleGroupingStrategy(Protocol):
    """Protocol for grouping bundle summaries into segments.

    Implementations partition a flat list of
    :class:`~war_rig.models.summaries.BundleSummary` objects into
    groups, each of which becomes the input for one coherence merge
    call (Pass 2).

    The ``group_size`` parameter is a target — implementations may
    produce groups smaller than ``group_size`` (e.g. the final group)
    but should never exceed it.
    """

    def group(
        self,
        bundles: list[BundleSummary],
        group_size: int,
    ) -> list[list[BundleSummary]]:
        """Partition *bundles* into groups of at most *group_size*.

        Args:
            bundles: Flat, ordered list of bundle summaries from Pass 1.
            group_size: Maximum number of bundles per group.

        Returns:
            A list of groups, where each group is a list of
            :class:`BundleSummary` objects.
        """
        ...  # pragma: no cover


class AdjacentSequentialGrouping:
    """Groups bundles in sequential order, respecting adjacency.

    The simplest strategy: walks through the bundle list in order and
    slices it into consecutive chunks of ``group_size``.  This
    preserves the physical proximity of paragraphs in the source file,
    which tends to correlate with functional cohesion in COBOL
    programs.

    Example::

        strategy = AdjacentSequentialGrouping()
        groups = strategy.group(bundles, group_size=4)
        # bundles[0:4], bundles[4:8], bundles[8:10], ...
    """

    def group(
        self,
        bundles: list[BundleSummary],
        group_size: int,
    ) -> list[list[BundleSummary]]:
        """Partition *bundles* into adjacent groups of at most *group_size*.

        Args:
            bundles: Flat, ordered list of bundle summaries from Pass 1.
            group_size: Maximum number of bundles per group.  Must be
                at least 1.

        Returns:
            A list of groups.  Returns an empty list when *bundles* is
            empty.

        Raises:
            ValueError: If *group_size* is less than 1.
        """
        if group_size < 1:
            raise ValueError(f"group_size must be >= 1, got {group_size}")
        if not bundles:
            return []
        groups: list[list[BundleSummary]] = []
        for i in range(0, len(bundles), group_size):
            groups.append(bundles[i : i + group_size])
        return groups
