"""
Tests for the sequence finder algorithm.

These tests verify the correctness of the longest call sequence finder
for various graph topologies including linear chains, trees, DAGs,
cycles, and disconnected components.
"""

import pytest

from citadel.analysis.sequence_finder import (
    find_longest_sequences,
    find_sequences_containing,
    sequences_to_mermaid,
)


class TestLinearSequences:
    """Tests for simple linear call chains."""

    def test_single_edge(self):
        """A single call edge should return one sequence with one edge."""
        artifacts = {
            "prog::A": {"name": "A", "type": "program"},
            "prog::B": {"name": "B", "type": "program"},
        }
        edges = [
            {"source": "prog::A", "target": "prog::B", "relationship_type": "calls"},
        ]

        sequences = find_longest_sequences(artifacts, edges)

        assert len(sequences) == 1
        assert sequences[0] == [("prog::A", "prog::B", "calls")]

    def test_linear_chain(self):
        """A -> B -> C should return one sequence of length 2."""
        artifacts = {
            "prog::A": {"name": "A", "type": "program"},
            "prog::B": {"name": "B", "type": "program"},
            "prog::C": {"name": "C", "type": "program"},
        }
        edges = [
            {"source": "prog::A", "target": "prog::B", "relationship_type": "calls"},
            {"source": "prog::B", "target": "prog::C", "relationship_type": "calls"},
        ]

        sequences = find_longest_sequences(artifacts, edges)

        assert len(sequences) == 1
        assert len(sequences[0]) == 2
        assert sequences[0] == [
            ("prog::A", "prog::B", "calls"),
            ("prog::B", "prog::C", "calls"),
        ]

    def test_long_chain(self):
        """Test a longer chain A -> B -> C -> D -> E."""
        artifacts = {f"prog::{c}": {"name": c} for c in "ABCDE"}
        edges = [
            {"source": f"prog::{c1}", "target": f"prog::{c2}", "relationship_type": "calls"}
            for c1, c2 in [("A", "B"), ("B", "C"), ("C", "D"), ("D", "E")]
        ]

        sequences = find_longest_sequences(artifacts, edges)

        assert len(sequences) == 1
        assert len(sequences[0]) == 4


class TestBranchingSequences:
    """Tests for tree and DAG structures."""

    def test_single_branch(self):
        """A calls both B and C (separately), should find the longest branch."""
        artifacts = {
            "prog::A": {"name": "A"},
            "prog::B": {"name": "B"},
            "prog::C": {"name": "C"},
        }
        edges = [
            {"source": "prog::A", "target": "prog::B", "relationship_type": "calls"},
            {"source": "prog::A", "target": "prog::C", "relationship_type": "calls"},
        ]

        sequences = find_longest_sequences(artifacts, edges)

        # Should return one sequence of length 1 (either A->B or A->C)
        assert len(sequences) == 1
        assert len(sequences[0]) == 1

    def test_unequal_branches(self):
        """
        A -> B -> C (length 2)
        A -> D (length 1)
        Should prefer the longer branch A -> B -> C.
        """
        artifacts = {f"prog::{c}": {"name": c} for c in "ABCD"}
        edges = [
            {"source": "prog::A", "target": "prog::B", "relationship_type": "calls"},
            {"source": "prog::B", "target": "prog::C", "relationship_type": "calls"},
            {"source": "prog::A", "target": "prog::D", "relationship_type": "calls"},
        ]

        sequences = find_longest_sequences(artifacts, edges)

        assert len(sequences) == 1
        assert len(sequences[0]) == 2
        assert sequences[0] == [
            ("prog::A", "prog::B", "calls"),
            ("prog::B", "prog::C", "calls"),
        ]

    def test_diamond_dag(self):
        """
        Diamond pattern:
            A
           / \\
          B   C
           \\ /
            D

        The algorithm finds the longest path from the entry point A,
        which covers A -> B -> D (or A -> C -> D). The remaining node
        (C or B) that wasn't in the main path forms a second sequence.
        """
        artifacts = {f"prog::{c}": {"name": c} for c in "ABCD"}
        edges = [
            {"source": "prog::A", "target": "prog::B", "relationship_type": "calls"},
            {"source": "prog::A", "target": "prog::C", "relationship_type": "calls"},
            {"source": "prog::B", "target": "prog::D", "relationship_type": "calls"},
            {"source": "prog::C", "target": "prog::D", "relationship_type": "calls"},
        ]

        sequences = find_longest_sequences(artifacts, edges)

        # First sequence is the longest: A -> B -> D or A -> C -> D (length 2)
        assert len(sequences[0]) == 2
        assert sequences[0][0][0] == "prog::A"
        assert sequences[0][1][1] == "prog::D"

        # The algorithm also covers remaining edges from uncovered nodes
        # This ensures good coverage of the graph for sequence diagrams
        assert len(sequences) >= 1


class TestCycleHandling:
    """Tests for cycle detection and handling."""

    def test_simple_cycle(self):
        """A -> B -> A (cycle) should not infinite loop."""
        artifacts = {
            "prog::A": {"name": "A"},
            "prog::B": {"name": "B"},
        }
        edges = [
            {"source": "prog::A", "target": "prog::B", "relationship_type": "calls"},
            {"source": "prog::B", "target": "prog::A", "relationship_type": "calls"},
        ]

        # Should complete without hanging
        sequences = find_longest_sequences(artifacts, edges)

        # Should find A -> B (length 1) - starts from one node in cycle
        assert len(sequences) >= 1
        assert len(sequences[0]) == 1

    def test_cycle_with_entry(self):
        """
        X -> A -> B -> A (cycle)
        Entry point X should give X -> A -> B.
        """
        artifacts = {f"prog::{c}": {"name": c} for c in "XAB"}
        edges = [
            {"source": "prog::X", "target": "prog::A", "relationship_type": "calls"},
            {"source": "prog::A", "target": "prog::B", "relationship_type": "calls"},
            {"source": "prog::B", "target": "prog::A", "relationship_type": "calls"},
        ]

        sequences = find_longest_sequences(artifacts, edges)

        assert len(sequences) >= 1
        # Longest should be X -> A -> B (length 2)
        assert len(sequences[0]) == 2
        assert sequences[0][0] == ("prog::X", "prog::A", "calls")
        assert sequences[0][1] == ("prog::A", "prog::B", "calls")

    def test_self_loop(self):
        """A -> A (self-loop) should be handled gracefully."""
        artifacts = {"prog::A": {"name": "A"}}
        edges = [
            {"source": "prog::A", "target": "prog::A", "relationship_type": "calls"},
        ]

        sequences = find_longest_sequences(artifacts, edges)

        # Self-loops are skipped in simple path finding
        assert sequences == []


class TestDisconnectedComponents:
    """Tests for graphs with multiple connected components."""

    def test_two_components(self):
        """Two separate chains should both be found."""
        artifacts = {f"prog::{c}": {"name": c} for c in "ABXY"}
        edges = [
            {"source": "prog::A", "target": "prog::B", "relationship_type": "calls"},
            {"source": "prog::X", "target": "prog::Y", "relationship_type": "calls"},
        ]

        sequences = find_longest_sequences(artifacts, edges)

        assert len(sequences) == 2
        # Both sequences should have length 1
        assert all(len(seq) == 1 for seq in sequences)

    def test_unequal_components(self):
        """Components of different sizes should be sorted by length."""
        artifacts = {f"prog::{c}": {"name": c} for c in "ABCXY"}
        edges = [
            # Component 1: A -> B -> C (length 2)
            {"source": "prog::A", "target": "prog::B", "relationship_type": "calls"},
            {"source": "prog::B", "target": "prog::C", "relationship_type": "calls"},
            # Component 2: X -> Y (length 1)
            {"source": "prog::X", "target": "prog::Y", "relationship_type": "calls"},
        ]

        sequences = find_longest_sequences(artifacts, edges)

        assert len(sequences) == 2
        # First sequence should be longer
        assert len(sequences[0]) == 2
        assert len(sequences[1]) == 1


class TestRelationshipTypeFiltering:
    """Tests for filtering by relationship type."""

    def test_default_relationship_types(self):
        """Default types should include calls, executes, performs."""
        artifacts = {f"prog::{c}": {"name": c} for c in "ABCD"}
        edges = [
            {"source": "prog::A", "target": "prog::B", "relationship_type": "calls"},
            {"source": "prog::B", "target": "prog::C", "relationship_type": "executes"},
            {"source": "prog::C", "target": "prog::D", "relationship_type": "performs"},
        ]

        sequences = find_longest_sequences(artifacts, edges)

        assert len(sequences) == 1
        assert len(sequences[0]) == 3

    def test_filter_relationship_type(self):
        """Should only follow specified relationship types."""
        artifacts = {f"prog::{c}": {"name": c} for c in "ABCD"}
        edges = [
            {"source": "prog::A", "target": "prog::B", "relationship_type": "calls"},
            {"source": "prog::B", "target": "prog::C", "relationship_type": "reads"},  # Not a call
            {"source": "prog::C", "target": "prog::D", "relationship_type": "calls"},
        ]

        sequences = find_longest_sequences(
            artifacts, edges, relationship_types=["calls"]
        )

        # Should find two separate sequences: A->B and C->D
        assert len(sequences) == 2
        assert all(len(seq) == 1 for seq in sequences)

    def test_custom_relationship_types(self):
        """Should work with custom relationship types."""
        artifacts = {f"prog::{c}": {"name": c} for c in "ABC"}
        edges = [
            {"source": "prog::A", "target": "prog::B", "relationship_type": "invokes"},
            {"source": "prog::B", "target": "prog::C", "relationship_type": "invokes"},
        ]

        sequences = find_longest_sequences(
            artifacts, edges, relationship_types=["invokes"]
        )

        assert len(sequences) == 1
        assert len(sequences[0]) == 2


class TestMinLengthFiltering:
    """Tests for minimum sequence length filtering."""

    def test_min_length_filters_short(self):
        """min_length=2 should exclude single-edge sequences."""
        artifacts = {f"prog::{c}": {"name": c} for c in "ABCD"}
        edges = [
            {"source": "prog::A", "target": "prog::B", "relationship_type": "calls"},
            {"source": "prog::C", "target": "prog::D", "relationship_type": "calls"},
        ]

        sequences = find_longest_sequences(artifacts, edges, min_length=2)

        assert len(sequences) == 0

    def test_min_length_keeps_long(self):
        """min_length=2 should keep sequences of length 2 or more."""
        artifacts = {f"prog::{c}": {"name": c} for c in "ABCXY"}
        edges = [
            {"source": "prog::A", "target": "prog::B", "relationship_type": "calls"},
            {"source": "prog::B", "target": "prog::C", "relationship_type": "calls"},
            {"source": "prog::X", "target": "prog::Y", "relationship_type": "calls"},
        ]

        sequences = find_longest_sequences(artifacts, edges, min_length=2)

        assert len(sequences) == 1
        assert len(sequences[0]) == 2


class TestMaxSequences:
    """Tests for maximum sequence count limiting."""

    def test_max_sequences_limits_output(self):
        """max_sequences should limit the number of returned sequences."""
        artifacts = {f"prog::{c}": {"name": c} for c in "ABCDEF"}
        edges = [
            {"source": "prog::A", "target": "prog::B", "relationship_type": "calls"},
            {"source": "prog::C", "target": "prog::D", "relationship_type": "calls"},
            {"source": "prog::E", "target": "prog::F", "relationship_type": "calls"},
        ]

        sequences = find_longest_sequences(artifacts, edges, max_sequences=2)

        assert len(sequences) == 2


class TestEdgeFormats:
    """Tests for different edge data formats."""

    def test_from_artifact_format(self):
        """Should accept 'from_artifact'/'to_artifact' keys."""
        artifacts = {
            "prog::A": {"name": "A"},
            "prog::B": {"name": "B"},
        }
        edges = [
            {
                "from_artifact": "prog::A",
                "to_artifact": "prog::B",
                "relationship_type": "calls",
            },
        ]

        sequences = find_longest_sequences(artifacts, edges)

        assert len(sequences) == 1
        assert sequences[0] == [("prog::A", "prog::B", "calls")]


class TestEmptyInputs:
    """Tests for edge cases with empty inputs."""

    def test_empty_artifacts(self):
        """Empty artifacts should return empty sequences."""
        sequences = find_longest_sequences({}, [])
        assert sequences == []

    def test_empty_edges(self):
        """Artifacts with no edges should return empty sequences."""
        artifacts = {"prog::A": {"name": "A"}}
        sequences = find_longest_sequences(artifacts, [])
        assert sequences == []

    def test_no_matching_relationship_types(self):
        """No edges matching relationship types should return empty."""
        artifacts = {
            "prog::A": {"name": "A"},
            "prog::B": {"name": "B"},
        }
        edges = [
            {"source": "prog::A", "target": "prog::B", "relationship_type": "reads"},
        ]

        sequences = find_longest_sequences(
            artifacts, edges, relationship_types=["calls"]
        )
        assert sequences == []


class TestSequencesToMermaid:
    """Tests for Mermaid diagram generation."""

    def test_simple_sequence(self):
        """Generate Mermaid for a simple sequence."""
        sequences = [[("prog::A", "prog::B", "calls")]]

        mermaid = sequences_to_mermaid(sequences)

        assert "sequenceDiagram" in mermaid
        assert "A->>B: calls" in mermaid

    def test_with_artifacts_names(self):
        """Should use display names from artifacts."""
        sequences = [[("prog::A", "prog::B", "calls")]]
        artifacts = {
            "prog::A": {"name": "Program A", "display_name": "ProgramA"},
            "prog::B": {"name": "Program B"},
        }

        mermaid = sequences_to_mermaid(sequences, artifacts=artifacts)

        assert "ProgramA" in mermaid
        assert "Program B" in mermaid

    def test_with_title(self):
        """Should include title when specified."""
        sequences = [[("A", "B", "calls")]]

        mermaid = sequences_to_mermaid(sequences, title="Test Flow")

        assert "title Test Flow" in mermaid

    def test_empty_sequences(self):
        """Should handle empty sequences gracefully."""
        mermaid = sequences_to_mermaid([])

        assert "sequenceDiagram" in mermaid
        assert "No sequences found" in mermaid


class TestFindSequencesContaining:
    """Tests for finding sequences containing a specific artifact."""

    def test_find_containing_middle(self):
        """Find sequences containing a node in the middle."""
        artifacts = {f"prog::{c}": {"name": c} for c in "ABCDE"}
        edges = [
            {"source": "prog::A", "target": "prog::B", "relationship_type": "calls"},
            {"source": "prog::B", "target": "prog::C", "relationship_type": "calls"},
            {"source": "prog::C", "target": "prog::D", "relationship_type": "calls"},
            {"source": "prog::D", "target": "prog::E", "relationship_type": "calls"},
        ]

        sequences = find_sequences_containing(
            "prog::C", artifacts, edges, context_depth=2
        )

        assert len(sequences) >= 1
        # Should include nodes around C within depth 2
        all_nodes = set()
        for seq in sequences:
            for caller, callee, _ in seq:
                all_nodes.add(caller)
                all_nodes.add(callee)

        assert "prog::C" in all_nodes or any(
            "prog::C" in (caller, callee) for seq in sequences for caller, callee, _ in seq
        )

    def test_find_containing_not_found(self):
        """Return empty when artifact not in graph."""
        artifacts = {"prog::A": {"name": "A"}, "prog::B": {"name": "B"}}
        edges = [
            {"source": "prog::A", "target": "prog::B", "relationship_type": "calls"}
        ]

        sequences = find_sequences_containing("prog::X", artifacts, edges)

        assert sequences == []


class TestEntryPointIdentification:
    """Tests for correct entry point identification."""

    def test_identifies_true_entry_point(self):
        """Should identify node with no incoming edges as entry point."""
        artifacts = {f"prog::{c}": {"name": c} for c in "ABC"}
        edges = [
            {"source": "prog::A", "target": "prog::B", "relationship_type": "calls"},
            {"source": "prog::B", "target": "prog::C", "relationship_type": "calls"},
        ]

        sequences = find_longest_sequences(artifacts, edges)

        # A is the entry point, sequence should start from A
        assert sequences[0][0][0] == "prog::A"

    def test_multiple_entry_points(self):
        """Multiple entry points should yield multiple sequences."""
        artifacts = {f"prog::{c}": {"name": c} for c in "ABXY"}
        edges = [
            {"source": "prog::A", "target": "prog::B", "relationship_type": "calls"},
            {"source": "prog::X", "target": "prog::Y", "relationship_type": "calls"},
        ]

        sequences = find_longest_sequences(artifacts, edges)

        # Should find sequences starting from both A and X
        starts = {seq[0][0] for seq in sequences}
        assert "prog::A" in starts
        assert "prog::X" in starts


class TestMermaidWithCallSemantics:
    """Tests for Mermaid generation with call semantics (data flow)."""

    def test_call_semantics_with_inputs_and_outputs(self):
        """Should show inputs on forward arrow and outputs on return arrow."""
        sequences = [[("prog::MAIN", "prog::PROCESS", "calls")]]
        artifacts = {
            "prog::MAIN": {"name": "MAIN"},
            "prog::PROCESS": {"name": "PROCESS"},
        }
        call_semantics = {
            "MAIN->PROCESS": {
                "inputs": ["WS-INPUT-1", "WS-INPUT-2"],
                "outputs": ["WS-RESULT"],
                "purpose": "Process data",
            }
        }

        mermaid = sequences_to_mermaid(
            sequences, artifacts=artifacts, call_semantics=call_semantics
        )

        assert "sequenceDiagram" in mermaid
        # Forward arrow with inputs
        assert "MAIN->>PROCESS: WS-INPUT-1, WS-INPUT-2" in mermaid
        # Return arrow with outputs (dashed)
        assert "PROCESS-->>MAIN: WS-RESULT" in mermaid

    def test_call_semantics_inputs_only(self):
        """Should show inputs on forward arrow when no outputs."""
        sequences = [[("prog::A", "prog::B", "calls")]]
        artifacts = {"prog::A": {"name": "A"}, "prog::B": {"name": "B"}}
        call_semantics = {
            "A->B": {
                "inputs": ["VAR1"],
                "outputs": [],
            }
        }

        mermaid = sequences_to_mermaid(
            sequences, artifacts=artifacts, call_semantics=call_semantics
        )

        assert "A->>B: VAR1" in mermaid
        # Should NOT have return arrow
        assert "B-->>A" not in mermaid

    def test_call_semantics_outputs_only(self):
        """Should fall back to rel_type when no inputs, show outputs on return."""
        sequences = [[("prog::A", "prog::B", "calls")]]
        artifacts = {"prog::A": {"name": "A"}, "prog::B": {"name": "B"}}
        call_semantics = {
            "A->B": {
                "inputs": [],
                "outputs": ["RESULT"],
            }
        }

        mermaid = sequences_to_mermaid(
            sequences, artifacts=artifacts, call_semantics=call_semantics
        )

        # Forward arrow with relationship type (no inputs)
        assert "A->>B: calls" in mermaid
        # Return arrow with outputs
        assert "B-->>A: RESULT" in mermaid

    def test_call_semantics_none_defaults_to_rel_type(self):
        """Without call_semantics, should use relationship type."""
        sequences = [[("prog::A", "prog::B", "calls")]]
        artifacts = {"prog::A": {"name": "A"}, "prog::B": {"name": "B"}}

        mermaid = sequences_to_mermaid(sequences, artifacts=artifacts)

        assert "A->>B: calls" in mermaid
        # No return arrow
        assert "-->>A" not in mermaid

    def test_call_semantics_truncates_long_lists(self):
        """Should truncate variable lists when exceeding max_variables."""
        sequences = [[("prog::A", "prog::B", "calls")]]
        artifacts = {"prog::A": {"name": "A"}, "prog::B": {"name": "B"}}
        call_semantics = {
            "A->B": {
                "inputs": [f"VAR{i}" for i in range(10)],
                "outputs": [f"OUT{i}" for i in range(8)],
            }
        }

        mermaid = sequences_to_mermaid(
            sequences, artifacts=artifacts, call_semantics=call_semantics, max_variables=3
        )

        # Should show first 3 inputs plus count
        assert "VAR0, VAR1, VAR2, +7" in mermaid
        # Should show first 3 outputs plus count
        assert "OUT0, OUT1, OUT2, +5" in mermaid

    def test_call_semantics_case_insensitive_lookup(self):
        """Call semantics lookup should be case-insensitive."""
        sequences = [[("prog::main", "prog::process", "calls")]]
        artifacts = {"prog::main": {"name": "MAIN"}, "prog::process": {"name": "PROCESS"}}
        # Key uses uppercase names (from display_name)
        call_semantics = {
            "MAIN->PROCESS": {
                "inputs": ["X"],
                "outputs": ["Y"],
            }
        }

        mermaid = sequences_to_mermaid(
            sequences, artifacts=artifacts, call_semantics=call_semantics
        )

        # Should find semantics even with different cases
        assert "MAIN->>PROCESS: X" in mermaid
        assert "PROCESS-->>MAIN: Y" in mermaid

    def test_call_semantics_multi_sequence(self):
        """Should apply call semantics across multiple sequences."""
        sequences = [
            [("prog::A", "prog::B", "calls"), ("prog::B", "prog::C", "calls")],
        ]
        artifacts = {
            "prog::A": {"name": "A"},
            "prog::B": {"name": "B"},
            "prog::C": {"name": "C"},
        }
        call_semantics = {
            "A->B": {"inputs": ["IN1"], "outputs": ["MID"]},
            "B->C": {"inputs": ["MID"], "outputs": ["OUT"]},
        }

        mermaid = sequences_to_mermaid(
            sequences, artifacts=artifacts, call_semantics=call_semantics
        )

        # First call
        assert "A->>B: IN1" in mermaid
        assert "B-->>A: MID" in mermaid
        # Second call
        assert "B->>C: MID" in mermaid
        assert "C-->>B: OUT" in mermaid

    def test_call_semantics_partial_match(self):
        """Should use rel_type for edges without matching semantics."""
        sequences = [
            [("prog::A", "prog::B", "calls"), ("prog::B", "prog::C", "executes")],
        ]
        artifacts = {
            "prog::A": {"name": "A"},
            "prog::B": {"name": "B"},
            "prog::C": {"name": "C"},
        }
        # Only have semantics for A->B, not B->C
        call_semantics = {
            "A->B": {"inputs": ["X"], "outputs": ["Y"]},
        }

        mermaid = sequences_to_mermaid(
            sequences, artifacts=artifacts, call_semantics=call_semantics
        )

        # A->B has semantics
        assert "A->>B: X" in mermaid
        assert "B-->>A: Y" in mermaid
        # B->C uses relationship type
        assert "B->>C: executes" in mermaid
        assert "C-->>B" not in mermaid
