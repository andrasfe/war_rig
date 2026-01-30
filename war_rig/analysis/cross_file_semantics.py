"""Cross-file call semantics aggregator for README.md sequence diagrams.

This module provides the CrossFileCallSemanticsAggregator class which aggregates
call semantics from multiple files to enable cross-file data flow in README.md
sequence diagrams.

The aggregator:
1. Collects CallSemantics from Scribe outputs for each documented file
2. Provides lookup for semantics between caller/callee pairs
3. Converts to a format suitable for Citadel's generate_flow_diagram()

Example:
    from war_rig.analysis.cross_file_semantics import CrossFileCallSemanticsAggregator

    aggregator = CrossFileCallSemanticsAggregator()

    # Add semantics from each documented file
    aggregator.add_file_semantics("PROG1.cbl", template1.call_semantics)
    aggregator.add_file_semantics("PROG2.cbl", template2.call_semantics)

    # Get semantics for a specific call pair
    sem = aggregator.get_semantics("1000-INIT", "2000-PROCESS")
    if sem:
        print(f"Inputs: {sem.inputs}, Outputs: {sem.outputs}")

    # Convert to format for flow diagram generation
    diagram_semantics = aggregator.to_flow_diagram_format()
"""

import logging
from dataclasses import dataclass, field
from typing import Any

from war_rig.models.templates import CallSemantics

logger = logging.getLogger(__name__)

# Maximum number of variables to include in diagram labels
MAX_VARIABLES_IN_DIAGRAM = 5


@dataclass
class AggregatedSemantics:
    """Aggregated call semantics with source file tracking.

    Attributes:
        caller: Name of the calling paragraph/function.
        callee: Name of the called paragraph/function.
        inputs: Variables/data passed into the call.
        outputs: Variables/data returned or modified.
        purpose: Brief description of what the call accomplishes.
        source_file: The file where this call relationship was found.
    """

    caller: str
    callee: str
    inputs: list[str] = field(default_factory=list)
    outputs: list[str] = field(default_factory=list)
    purpose: str | None = None
    source_file: str = ""


class CrossFileCallSemanticsAggregator:
    """Aggregates call semantics from multiple files for cross-file flows.

    This class collects call semantics from all documented files and provides
    methods to look up semantics for specific caller/callee pairs, as well as
    convert the aggregated data to a format suitable for Citadel's flow diagram
    generation.

    The aggregator handles:
    - Multiple files with their own call semantics
    - Deduplication of caller/callee pairs
    - Truncation of long variable lists for diagram readability

    Attributes:
        _file_semantics: Mapping of file path to list of CallSemantics.
        _call_lookup: Fast lookup table for caller->callee semantics.

    Example:
        aggregator = CrossFileCallSemanticsAggregator()

        # Add semantics from completed Scribe outputs
        for file_name, template in completed_docs.items():
            if template.call_semantics:
                aggregator.add_file_semantics(file_name, template.call_semantics)

        # Use for flow diagram generation
        diagram_format = aggregator.to_flow_diagram_format()
    """

    def __init__(self) -> None:
        """Initialize the aggregator with empty storage."""
        # file_path -> list of CallSemantics
        self._file_semantics: dict[str, list[CallSemantics]] = {}
        # "CALLER->CALLEE" -> AggregatedSemantics (for fast lookup)
        self._call_lookup: dict[str, AggregatedSemantics] = {}

    def add_file_semantics(
        self,
        file_path: str,
        semantics: list[CallSemantics] | None,
    ) -> None:
        """Add call semantics from a single file.

        Stores the semantics for the file and updates the lookup table for
        fast caller/callee queries.

        Args:
            file_path: Path to the source file.
            semantics: List of CallSemantics from the file's documentation.
                If None or empty, the file is recorded but no semantics added.
        """
        if not semantics:
            logger.debug(f"No call semantics for {file_path}")
            return

        self._file_semantics[file_path] = semantics
        logger.debug(f"Added {len(semantics)} call semantics from {file_path}")

        # Update lookup table
        for cs in semantics:
            if not cs.caller or not cs.callee:
                continue

            # Normalize to uppercase for consistent matching
            key = self._make_key(cs.caller, cs.callee)

            # Create aggregated semantics entry
            aggregated = AggregatedSemantics(
                caller=cs.caller.upper(),
                callee=cs.callee.upper(),
                inputs=cs.inputs[:],  # Copy to avoid mutation
                outputs=cs.outputs[:],
                purpose=cs.purpose,
                source_file=file_path,
            )

            # Store in lookup (later files can override earlier ones)
            self._call_lookup[key] = aggregated

    def get_semantics(
        self,
        caller: str,
        callee: str,
    ) -> AggregatedSemantics | None:
        """Get semantics for a specific caller/callee pair.

        Args:
            caller: Name of the calling paragraph/function.
            callee: Name of the called paragraph/function.

        Returns:
            AggregatedSemantics for the call pair, or None if not found.
        """
        key = self._make_key(caller, callee)
        return self._call_lookup.get(key)

    def get_cross_file_semantics(
        self,
        caller_file: str,
        caller_para: str,
        callee_file: str,
        callee_para: str,
    ) -> AggregatedSemantics | None:
        """Get semantics for a cross-file call.

        This method is for cases where you need to consider file context.
        For most use cases, get_semantics() is sufficient since paragraph
        names are typically unique within a system.

        Args:
            caller_file: File containing the caller.
            caller_para: Name of the calling paragraph.
            callee_file: File containing the callee.
            callee_para: Name of the called paragraph.

        Returns:
            AggregatedSemantics if found, None otherwise.
        """
        # For now, we use simple name-based lookup
        # Future enhancement: could consider file context for disambiguation
        return self.get_semantics(caller_para, callee_para)

    def to_flow_diagram_format(
        self,
        max_variables: int = MAX_VARIABLES_IN_DIAGRAM,
    ) -> dict[str, dict[str, Any]]:
        """Convert to format expected by generate_flow_diagram().

        Produces a dictionary keyed by "CALLER->CALLEE" with input/output
        variable lists suitable for rendering in Mermaid sequence diagrams.

        Args:
            max_variables: Maximum number of variables to include per list.
                Longer lists are truncated to avoid cluttering the diagram.

        Returns:
            Dictionary in the format:
            {
                "CALLER->CALLEE": {
                    "inputs": ["VAR1", "VAR2", ...],
                    "outputs": ["RESULT1", ...],
                    "purpose": "Brief description"
                }
            }
        """
        result: dict[str, dict[str, Any]] = {}

        for key, sem in self._call_lookup.items():
            # Truncate variable lists if needed
            inputs = sem.inputs[:max_variables]
            outputs = sem.outputs[:max_variables]

            result[key] = {
                "inputs": inputs,
                "outputs": outputs,
                "purpose": sem.purpose,
            }

        logger.info(
            f"Converted {len(result)} call semantics entries to flow diagram format"
        )

        return result

    def get_all_semantics(self) -> list[AggregatedSemantics]:
        """Get all aggregated semantics entries.

        Returns:
            List of all AggregatedSemantics in the aggregator.
        """
        return list(self._call_lookup.values())

    def get_files_with_semantics(self) -> list[str]:
        """Get list of files that have call semantics.

        Returns:
            List of file paths that contributed call semantics.
        """
        return list(self._file_semantics.keys())

    def __len__(self) -> int:
        """Return the number of unique caller/callee pairs."""
        return len(self._call_lookup)

    @staticmethod
    def _make_key(caller: str, callee: str) -> str:
        """Create a lookup key from caller and callee names.

        Args:
            caller: Caller paragraph name.
            callee: Callee paragraph name.

        Returns:
            Normalized key in format "CALLER->CALLEE".
        """
        return f"{caller.upper()}->{callee.upper()}"
