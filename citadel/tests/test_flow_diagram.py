"""
Tests for the flow diagram generator.

These tests verify correctness of the Mermaid flowchart generation for
COBOL file control flow analysis. They use synthetic ``FileAnalysisResult``
objects (no file I/O) and cover node shapes, edge styles, cycles,
reachability filtering, de-duplication, and external call rendering.
"""

from __future__ import annotations

import pytest

from citadel.analysis.flow_diagram import (
    _make_node_id,
    generate_flow_diagram,
)
from citadel.sdk import Callout, FileAnalysisResult, FileArtifact

# ---------------------------------------------------------------------------
# Helper factories
# ---------------------------------------------------------------------------


def _make_callout(
    target: str,
    relationship: str,
    target_type: str | None = None,
    line: int | None = None,
) -> Callout:
    """Create a Callout for testing."""
    return Callout(
        target=target,
        relationship=relationship,
        target_type=target_type,
        line=line,
    )


def _make_artifact(
    name: str,
    art_type: str = "paragraph",
    callouts: list[Callout] | None = None,
    line_start: int | None = None,
    line_end: int | None = None,
) -> FileArtifact:
    """Create a FileArtifact for testing."""
    return FileArtifact(
        name=name,
        type=art_type,
        category="code",
        line_start=line_start,
        line_end=line_end,
        callouts=callouts or [],
    )


def _make_result(
    artifacts: list[FileArtifact],
    file_path: str = "TEST.cbl",
    language: str = "cobol",
) -> FileAnalysisResult:
    """Create a FileAnalysisResult for testing."""
    return FileAnalysisResult(
        file_path=file_path,
        language=language,
        artifacts=artifacts,
    )


# ---------------------------------------------------------------------------
# Tests for _make_node_id
# ---------------------------------------------------------------------------


class TestMakeNodeId:
    """Tests for the node ID sanitization function."""

    def test_simple_name(self):
        """Alphanumeric names pass through unchanged."""
        assert _make_node_id("INIT") == "INIT"

    def test_hyphenated_name(self):
        """Hyphens are replaced with underscores."""
        assert _make_node_id("MAIN-PARA") == "MAIN_PARA"

    def test_name_with_spaces(self):
        """Spaces are replaced with underscores."""
        assert _make_node_id("MY PARAGRAPH") == "MY_PARAGRAPH"

    def test_empty_name(self):
        """Empty names produce a fallback ID."""
        assert _make_node_id("") == "node"

    def test_special_characters(self):
        """Special characters are replaced with underscores."""
        result = _make_node_id("FOO.BAR/BAZ")
        assert result == "FOO_BAR_BAZ"


# ---------------------------------------------------------------------------
# Tests for single paragraph (no calls)
# ---------------------------------------------------------------------------


class TestSingleParagraph:
    """A file with one paragraph and no calls produces just one node."""

    def test_single_node_output(self):
        """One paragraph, no callouts -> one rectangle node."""
        result = _make_result([
            _make_artifact("INIT-PARA"),
        ])

        diagram = generate_flow_diagram(result)

        assert "flowchart TD" in diagram
        assert 'INIT_PARA["INIT-PARA"]' in diagram
        # No arrows
        assert "-->" not in diagram
        assert "-.->" not in diagram


# ---------------------------------------------------------------------------
# Tests for linear chain
# ---------------------------------------------------------------------------


class TestLinearChain:
    """A performs B, B performs C -> linear chain with solid arrows."""

    def test_linear_a_b_c(self):
        """Linear chain: A -> B -> C."""
        result = _make_result([
            _make_artifact("A", callouts=[
                _make_callout("B", "performs"),
            ]),
            _make_artifact("B", callouts=[
                _make_callout("C", "performs"),
            ]),
            _make_artifact("C"),
        ])

        diagram = generate_flow_diagram(result)

        assert "A --> B" in diagram
        assert "B --> C" in diagram
        # All nodes defined as rectangles
        assert 'A["A"]' in diagram
        assert 'B["B"]' in diagram
        assert 'C["C"]' in diagram


# ---------------------------------------------------------------------------
# Tests for branching
# ---------------------------------------------------------------------------


class TestBranching:
    """A performs B and C -> branching with two solid arrows from A."""

    def test_branch_a_to_b_and_c(self):
        """Branching: A -> B and A -> C."""
        result = _make_result([
            _make_artifact("A", callouts=[
                _make_callout("B", "performs"),
                _make_callout("C", "performs"),
            ]),
            _make_artifact("B"),
            _make_artifact("C"),
        ])

        diagram = generate_flow_diagram(result)

        assert "A --> B" in diagram
        assert "A --> C" in diagram


# ---------------------------------------------------------------------------
# Tests for cycles
# ---------------------------------------------------------------------------


class TestCycles:
    """A performs B, B performs A -> cycle handled gracefully."""

    def test_cycle_does_not_crash(self):
        """Cycle: A -> B -> A should not crash or infinite loop."""
        result = _make_result([
            _make_artifact("A", callouts=[
                _make_callout("B", "performs"),
            ]),
            _make_artifact("B", callouts=[
                _make_callout("A", "performs"),
            ]),
        ])

        diagram = generate_flow_diagram(result)

        assert "A --> B" in diagram
        assert "B --> A" in diagram

    def test_cycle_with_start_paragraph(self):
        """Starting from A in a cycle A->B->A includes both nodes."""
        result = _make_result([
            _make_artifact("A", callouts=[
                _make_callout("B", "performs"),
            ]),
            _make_artifact("B", callouts=[
                _make_callout("A", "performs"),
            ]),
        ])

        diagram = generate_flow_diagram(result, start_paragraph="A")

        assert 'A["A"]' in diagram
        assert 'B["B"]' in diagram


# ---------------------------------------------------------------------------
# Tests for external calls
# ---------------------------------------------------------------------------


class TestExternalCalls:
    """External calls shown as leaf nodes with dashed arrows."""

    def test_call_external_program(self):
        """CALL to external program -> stadium shape, dashed arrow."""
        result = _make_result([
            _make_artifact("MAIN-PARA", callouts=[
                _make_callout("SUBPROG", "calls", target_type="program"),
            ]),
        ])

        diagram = generate_flow_diagram(result)

        # Dashed arrow with label
        assert '-.->' in diagram
        assert '|calls|' in diagram
        # Stadium shape for program
        assert 'SUBPROG__ext(["SUBPROG"])' in diagram

    def test_reads_table(self):
        """EXEC SQL SELECT -> cylinder shape, dashed arrow."""
        result = _make_result([
            _make_artifact("DB-READ-PARA", callouts=[
                _make_callout("CUSTOMER", "reads", target_type="table"),
            ]),
        ])

        diagram = generate_flow_diagram(result)

        assert '-.->' in diagram
        assert '|reads|' in diagram
        # Cylinder shape for table
        assert 'CUSTOMER__ext[("CUSTOMER")]' in diagram

    def test_writes_table(self):
        """EXEC SQL INSERT -> cylinder shape, dashed arrow with writes."""
        result = _make_result([
            _make_artifact("DB-WRITE-PARA", callouts=[
                _make_callout("ORDERS", "writes", target_type="table"),
            ]),
        ])

        diagram = generate_flow_diagram(result)

        assert '|writes|' in diagram
        assert 'ORDERS__ext[("ORDERS")]' in diagram

    def test_sends_to_screen(self):
        """EXEC CICS SEND MAP -> hexagon shape."""
        result = _make_result([
            _make_artifact("DISPLAY-PARA", callouts=[
                _make_callout("MENUMAP", "sends_to", target_type="screen"),
            ]),
        ])

        diagram = generate_flow_diagram(result)

        assert '-.->' in diagram
        assert '|sends_to|' in diagram
        # Hexagon shape for screen
        assert 'MENUMAP__ext{{"MENUMAP"}}' in diagram

    def test_receives_from_screen(self):
        """EXEC CICS RECEIVE MAP -> hexagon shape."""
        result = _make_result([
            _make_artifact("INPUT-PARA", callouts=[
                _make_callout("INPUTMAP", "receives_from", target_type="map"),
            ]),
        ])

        diagram = generate_flow_diagram(result)

        assert 'INPUTMAP__ext{{"INPUTMAP"}}' in diagram

    def test_includes_copybook(self):
        """COPY statement -> dashed arrow."""
        result = _make_result([
            _make_artifact("INIT-PARA", callouts=[
                _make_callout("COMMON-CPY", "includes", target_type="copybook"),
            ]),
        ])

        diagram = generate_flow_diagram(result)

        assert '-.->' in diagram
        assert '|includes|' in diagram

    def test_call_without_target_type(self):
        """External call without target_type uses relationship heuristic."""
        result = _make_result([
            _make_artifact("MAIN-PARA", callouts=[
                _make_callout("UNKNOWN-TARGET", "calls"),
            ]),
        ])

        diagram = generate_flow_diagram(result)

        # Should default to program (stadium) shape based on "calls" rel.
        assert 'UNKNOWN_TARGET__ext(["UNKNOWN-TARGET"])' in diagram


# ---------------------------------------------------------------------------
# Tests for include_external=False
# ---------------------------------------------------------------------------


class TestExcludeExternal:
    """When include_external=False, external calls are not shown."""

    def test_no_external_calls_shown(self):
        """External calls should be omitted entirely."""
        result = _make_result([
            _make_artifact("A", callouts=[
                _make_callout("B", "performs"),
                _make_callout("EXTPROG", "calls", target_type="program"),
                _make_callout("MYTABLE", "reads", target_type="table"),
            ]),
            _make_artifact("B"),
        ])

        diagram = generate_flow_diagram(result, include_external=False)

        assert "A --> B" in diagram
        assert "EXTPROG" not in diagram
        assert "MYTABLE" not in diagram
        assert "-.->" not in diagram


# ---------------------------------------------------------------------------
# Tests for start_paragraph filtering
# ---------------------------------------------------------------------------


class TestStartParagraph:
    """Starting from a specific paragraph only shows reachable subgraph."""

    def test_start_from_middle(self):
        """Starting from B in A->B->C should show B and C only."""
        result = _make_result([
            _make_artifact("A", callouts=[
                _make_callout("B", "performs"),
            ]),
            _make_artifact("B", callouts=[
                _make_callout("C", "performs"),
            ]),
            _make_artifact("C"),
        ])

        diagram = generate_flow_diagram(result, start_paragraph="B")

        # B and C should be present.
        assert 'B["B"]' in diagram
        assert 'C["C"]' in diagram
        assert "B --> C" in diagram
        # A should NOT be present (not reachable from B).
        assert 'A["A"]' not in diagram

    def test_start_from_leaf(self):
        """Starting from a leaf paragraph shows just that one node."""
        result = _make_result([
            _make_artifact("A", callouts=[
                _make_callout("B", "performs"),
            ]),
            _make_artifact("B"),
        ])

        diagram = generate_flow_diagram(result, start_paragraph="B")

        assert 'B["B"]' in diagram
        assert "A" not in diagram
        assert "-->" not in diagram

    def test_start_from_none_shows_all(self):
        """When start_paragraph is None, all paragraphs are shown."""
        result = _make_result([
            _make_artifact("A", callouts=[
                _make_callout("B", "performs"),
            ]),
            _make_artifact("B", callouts=[
                _make_callout("C", "performs"),
            ]),
            _make_artifact("C"),
            _make_artifact("D"),  # Disconnected
        ])

        diagram = generate_flow_diagram(result)

        assert 'A["A"]' in diagram
        assert 'B["B"]' in diagram
        assert 'C["C"]' in diagram
        assert 'D["D"]' in diagram


# ---------------------------------------------------------------------------
# Tests for paragraph not found
# ---------------------------------------------------------------------------


class TestParagraphNotFound:
    """When start_paragraph does not exist, raise ValueError."""

    def test_raises_value_error(self):
        """Non-existent paragraph should raise ValueError."""
        result = _make_result([
            _make_artifact("A"),
            _make_artifact("B"),
        ])

        with pytest.raises(ValueError, match="not found"):
            generate_flow_diagram(result, start_paragraph="NONEXISTENT")

    def test_error_includes_available(self):
        """ValueError message should list available paragraphs."""
        result = _make_result([
            _make_artifact("INIT-PARA"),
            _make_artifact("PROCESS-PARA"),
        ])

        with pytest.raises(ValueError, match="INIT-PARA"):
            generate_flow_diagram(result, start_paragraph="MISSING")


# ---------------------------------------------------------------------------
# Tests for node shapes
# ---------------------------------------------------------------------------


class TestNodeShapes:
    """Verify correct Mermaid shapes for different node types."""

    def test_internal_paragraph_rectangle(self):
        """Internal paragraphs use rectangle shape [...]."""
        result = _make_result([_make_artifact("MY-PARA")])
        diagram = generate_flow_diagram(result)
        assert 'MY_PARA["MY-PARA"]' in diagram

    def test_external_program_stadium(self):
        """External programs use stadium shape ([...])."""
        result = _make_result([
            _make_artifact("A", callouts=[
                _make_callout("PROG", "calls", target_type="program"),
            ]),
        ])
        diagram = generate_flow_diagram(result)
        assert 'PROG__ext(["PROG"])' in diagram

    def test_external_table_cylinder(self):
        """External tables use cylinder shape [(...)]."""
        result = _make_result([
            _make_artifact("A", callouts=[
                _make_callout("TBL", "reads", target_type="table"),
            ]),
        ])
        diagram = generate_flow_diagram(result)
        assert 'TBL__ext[("TBL")]' in diagram

    def test_external_screen_hexagon(self):
        """External screens use hexagon shape {{...}}."""
        result = _make_result([
            _make_artifact("A", callouts=[
                _make_callout("MYMAP", "sends_to", target_type="screen"),
            ]),
        ])
        diagram = generate_flow_diagram(result)
        assert 'MYMAP__ext{{"MYMAP"}}' in diagram

    def test_external_dataset_cylinder(self):
        """External datasets use cylinder shape."""
        result = _make_result([
            _make_artifact("A", callouts=[
                _make_callout("MYFILE", "reads", target_type="dataset"),
            ]),
        ])
        diagram = generate_flow_diagram(result)
        assert 'MYFILE__ext[("MYFILE")]' in diagram


# ---------------------------------------------------------------------------
# Tests for edge styles
# ---------------------------------------------------------------------------


class TestEdgeStyles:
    """Verify solid vs dashed arrow styles."""

    def test_performs_solid_arrow(self):
        """PERFORM uses solid arrow -->."""
        result = _make_result([
            _make_artifact("A", callouts=[
                _make_callout("B", "performs"),
            ]),
            _make_artifact("B"),
        ])

        diagram = generate_flow_diagram(result)

        assert "A --> B" in diagram
        assert "-.->" not in diagram

    def test_external_dashed_arrow(self):
        """External calls use dashed arrow -.->."""
        result = _make_result([
            _make_artifact("A", callouts=[
                _make_callout("EXT", "calls", target_type="program"),
            ]),
        ])

        diagram = generate_flow_diagram(result)

        assert "-.->" in diagram
        assert "A --> " not in diagram  # No solid arrow to ext


# ---------------------------------------------------------------------------
# Tests for de-duplication
# ---------------------------------------------------------------------------


class TestDeDuplication:
    """A paragraph may PERFORM the same target multiple times."""

    def test_internal_dedup(self):
        """Multiple PERFORMs to same target -> single arrow."""
        result = _make_result([
            _make_artifact("A", callouts=[
                _make_callout("B", "performs"),
                _make_callout("B", "performs"),
                _make_callout("B", "performs"),
            ]),
            _make_artifact("B"),
        ])

        diagram = generate_flow_diagram(result)

        # Count occurrences of the edge
        assert diagram.count("A --> B") == 1

    def test_external_dedup(self):
        """Multiple calls to same external target -> single dashed arrow."""
        result = _make_result([
            _make_artifact("A", callouts=[
                _make_callout("EXTPROG", "calls", target_type="program"),
                _make_callout("EXTPROG", "calls", target_type="program"),
            ]),
        ])

        diagram = generate_flow_diagram(result)

        # Only one dashed arrow to EXTPROG
        assert diagram.count("EXTPROG__ext") == 2  # One definition + one edge reference


# ---------------------------------------------------------------------------
# Tests for program-level artifact exclusion
# ---------------------------------------------------------------------------


class TestProgramExclusion:
    """The program-level artifact should NOT appear as a node."""

    def test_program_not_a_node(self):
        """Program artifacts are containers, not flow participants."""
        result = _make_result([
            _make_artifact("MYPROGRAM", art_type="program", callouts=[
                _make_callout("INIT-PARA", "performs"),
            ]),
            _make_artifact("INIT-PARA", callouts=[
                _make_callout("PROCESS-PARA", "performs"),
            ]),
            _make_artifact("PROCESS-PARA"),
        ])

        diagram = generate_flow_diagram(result)

        # The program name should not appear as a standalone node.
        assert 'MYPROGRAM["MYPROGRAM"]' not in diagram
        # But the paragraphs should appear.
        assert 'INIT_PARA["INIT-PARA"]' in diagram
        assert 'PROCESS_PARA["PROCESS-PARA"]' in diagram

    def test_program_performs_not_shown(self):
        """PERFORM from program artifact to paragraph is not shown
        (because the program is filtered out)."""
        result = _make_result([
            _make_artifact("MYPROGRAM", art_type="program", callouts=[
                _make_callout("INIT-PARA", "performs"),
            ]),
            _make_artifact("INIT-PARA"),
        ])

        diagram = generate_flow_diagram(result)

        # No edge from program to paragraph.
        assert "MYPROGRAM" not in diagram


# ---------------------------------------------------------------------------
# Tests for case insensitivity
# ---------------------------------------------------------------------------


class TestCaseInsensitivity:
    """PERFORM targets should match paragraphs case-insensitively."""

    def test_case_insensitive_perform_match(self):
        """Lowercase PERFORM target matches uppercase paragraph name."""
        result = _make_result([
            _make_artifact("MAIN-PARA", callouts=[
                _make_callout("init-para", "performs"),
            ]),
            _make_artifact("INIT-PARA"),
        ])

        diagram = generate_flow_diagram(result)

        assert "MAIN_PARA --> INIT_PARA" in diagram

    def test_case_insensitive_start_paragraph(self):
        """Start paragraph matching is case-insensitive."""
        result = _make_result([
            _make_artifact("MAIN-PARA", callouts=[
                _make_callout("HELPER", "performs"),
            ]),
            _make_artifact("HELPER"),
        ])

        # Should not raise even though case differs.
        diagram = generate_flow_diagram(result, start_paragraph="main-para")

        assert 'MAIN_PARA["MAIN-PARA"]' in diagram


# ---------------------------------------------------------------------------
# Tests for empty file
# ---------------------------------------------------------------------------


class TestEmptyFile:
    """Edge cases with empty or error results."""

    def test_no_artifacts(self):
        """File with no artifacts produces minimal diagram."""
        result = _make_result([])

        diagram = generate_flow_diagram(result)

        assert "flowchart TD" in diagram
        assert "No paragraphs found" in diagram

    def test_only_program_artifact(self):
        """File with only a program artifact (no paragraphs)."""
        result = _make_result([
            _make_artifact("MYPROGRAM", art_type="program"),
        ])

        diagram = generate_flow_diagram(result)

        assert "No paragraphs found" in diagram


# ---------------------------------------------------------------------------
# Tests for title
# ---------------------------------------------------------------------------


class TestTitle:
    """Title should appear in the diagram output."""

    def test_title_included(self):
        """Title appears as a comment in the diagram."""
        result = _make_result([_make_artifact("A")])

        diagram = generate_flow_diagram(result, title="My Diagram")

        assert "My Diagram" in diagram

    def test_no_title(self):
        """Without title, no title line appears."""
        result = _make_result([_make_artifact("A")])

        diagram = generate_flow_diagram(result)

        assert "Title:" not in diagram


# ---------------------------------------------------------------------------
# Tests for mixed internal + external
# ---------------------------------------------------------------------------


class TestMixedFlow:
    """Tests combining internal performs and external calls."""

    def test_full_flow(self):
        """A complete flow with performs, calls, reads, and sends_to."""
        result = _make_result([
            _make_artifact("MAIN-PARA", callouts=[
                _make_callout("INIT-PARA", "performs"),
                _make_callout("PROCESS-PARA", "performs"),
            ]),
            _make_artifact("INIT-PARA", callouts=[
                _make_callout("COMMON-CPY", "includes", target_type="copybook"),
            ]),
            _make_artifact("PROCESS-PARA", callouts=[
                _make_callout("DB-READ", "performs"),
                _make_callout("DISPLAY-PARA", "performs"),
            ]),
            _make_artifact("DB-READ", callouts=[
                _make_callout("CUSTOMER", "reads", target_type="table"),
            ]),
            _make_artifact("DISPLAY-PARA", callouts=[
                _make_callout("MENUMAP", "sends_to", target_type="screen"),
                _make_callout("SUBPROG", "calls", target_type="program"),
            ]),
        ])

        diagram = generate_flow_diagram(result)

        # Internal edges (solid).
        assert "MAIN_PARA --> INIT_PARA" in diagram
        assert "MAIN_PARA --> PROCESS_PARA" in diagram
        assert "PROCESS_PARA --> DB_READ" in diagram
        assert "PROCESS_PARA --> DISPLAY_PARA" in diagram

        # External edges (dashed).
        assert "|reads|" in diagram
        assert "|sends_to|" in diagram
        assert "|calls|" in diagram
        assert "|includes|" in diagram

        # Node shapes.
        assert 'CUSTOMER__ext[("CUSTOMER")]' in diagram  # cylinder
        assert 'MENUMAP__ext{{"MENUMAP"}}' in diagram  # hexagon
        assert 'SUBPROG__ext(["SUBPROG"])' in diagram  # stadium


# ---------------------------------------------------------------------------
# Tests for external classification heuristics
# ---------------------------------------------------------------------------


class TestExternalClassification:
    """Test the fallback classification when target_type is missing."""

    def test_reads_without_type_gets_cylinder(self):
        """reads relationship without target_type -> data (cylinder)."""
        result = _make_result([
            _make_artifact("A", callouts=[
                _make_callout("SOMETBL", "reads"),
            ]),
        ])

        diagram = generate_flow_diagram(result)

        assert 'SOMETBL__ext[("SOMETBL")]' in diagram

    def test_writes_without_type_gets_cylinder(self):
        """writes relationship without target_type -> data (cylinder)."""
        result = _make_result([
            _make_artifact("A", callouts=[
                _make_callout("OUTTBL", "writes"),
            ]),
        ])

        diagram = generate_flow_diagram(result)

        assert 'OUTTBL__ext[("OUTTBL")]' in diagram

    def test_sends_to_without_type_gets_hexagon(self):
        """sends_to without target_type -> screen (hexagon)."""
        result = _make_result([
            _make_artifact("A", callouts=[
                _make_callout("AMAP", "sends_to"),
            ]),
        ])

        diagram = generate_flow_diagram(result)

        assert 'AMAP__ext{{"AMAP"}}' in diagram

    def test_includes_without_type_gets_rectangle(self):
        """includes without target_type -> other (rectangle)."""
        result = _make_result([
            _make_artifact("A", callouts=[
                _make_callout("MYCPY", "includes"),
            ]),
        ])

        diagram = generate_flow_diagram(result)

        assert 'MYCPY__ext["MYCPY"]' in diagram


# ---------------------------------------------------------------------------
# Test that performs to non-existent paragraph becomes external
# ---------------------------------------------------------------------------


class TestPerformsToMissing:
    """PERFORM to a target not defined as a paragraph in the file."""

    def test_performs_missing_treated_as_internal_only_if_exists(self):
        """PERFORM to non-existent paragraph is NOT shown as internal edge
        and NOT shown as external (performs is internal-only)."""
        result = _make_result([
            _make_artifact("A", callouts=[
                _make_callout("NONEXISTENT", "performs"),
            ]),
        ])

        diagram = generate_flow_diagram(result)

        # NONEXISTENT should not appear since performs is internal-only
        # and the target does not exist as a paragraph.
        assert "NONEXISTENT" not in diagram


# ---------------------------------------------------------------------------
# Test SDK convenience function
# ---------------------------------------------------------------------------


class TestSDKIntegration:
    """Test the get_flow_diagram function via the SDK."""

    def test_import_from_citadel(self):
        """get_flow_diagram should be importable from citadel."""
        from citadel import get_flow_diagram
        assert callable(get_flow_diagram)

    def test_import_from_analysis(self):
        """generate_flow_diagram should be importable from analysis."""
        from citadel.analysis import generate_flow_diagram as gfd
        assert callable(gfd)


# ---------------------------------------------------------------------------
# Tests for deterministic output
# ---------------------------------------------------------------------------


class TestDeterministicOutput:
    """Output should be deterministic (sorted) for the same input."""

    def test_output_is_deterministic(self):
        """Running generate_flow_diagram twice gives the same output."""
        result = _make_result([
            _make_artifact("C", callouts=[
                _make_callout("A", "performs"),
            ]),
            _make_artifact("B", callouts=[
                _make_callout("C", "performs"),
            ]),
            _make_artifact("A", callouts=[
                _make_callout("B", "performs"),
            ]),
        ])

        diagram1 = generate_flow_diagram(result)
        diagram2 = generate_flow_diagram(result)

        assert diagram1 == diagram2
