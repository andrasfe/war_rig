"""Integration tests for citadel tools with real mainframe code.

This module tests:
- Citadel SDK integration with real COBOL files
- Analysis of CardDemo authorization system
- End-to-end tool behavior with actual mainframe code

These tests use real source files from the CardDemo application
to verify the citadel integration works correctly with actual
mainframe codebases.

Tests are skipped if the CardDemo source is not available.
"""

from pathlib import Path
from typing import Any

import pytest

# Paths to test data - support both Linux and macOS environments
_LINUX_CARDDEMO_DIR = Path(
    "/home/andras/aws-mainframe-modernization-carddemo/app/app-authorization-ims-db2-mq"
)
_MACOS_CARDDEMO_DIR = Path(
    "/Users/andraslferenczi/war_rig/aws-mainframe-modernization-carddemo/"
    "app/app-authorization-ims-db2-mq"
)

# Use whichever path exists
CARDDEMO_DIR = _MACOS_CARDDEMO_DIR if _MACOS_CARDDEMO_DIR.exists() else _LINUX_CARDDEMO_DIR
COBOL_DIR = CARDDEMO_DIR / "cbl"
CPY_DIR = CARDDEMO_DIR / "cpy"
JCL_DIR = CARDDEMO_DIR / "jcl"


@pytest.fixture
def carddemo_available() -> bool:
    """Check if CardDemo source is available."""
    return CARDDEMO_DIR.exists() and COBOL_DIR.exists()


@pytest.fixture
def citadel_available() -> bool:
    """Check if citadel SDK is available."""
    try:
        import citadel.sdk  # noqa: F401

        return True
    except ImportError:
        return False


@pytest.fixture
def citadel():
    """Create a Citadel instance for testing."""
    try:
        from citadel.sdk import Citadel

        return Citadel()
    except ImportError:
        pytest.skip("Citadel SDK not available")


def get_cobol_files() -> list[Path]:
    """Get list of COBOL files in CardDemo."""
    if not COBOL_DIR.exists():
        return []
    return sorted(COBOL_DIR.glob("*.cbl"))


def get_copybook_files() -> list[Path]:
    """Get list of copybook files in CardDemo."""
    if not CPY_DIR.exists():
        return []
    return list(CPY_DIR.glob("*.cpy")) + list(CPY_DIR.glob("*.CPY"))


class TestCitadelAnalyzeFile:
    """Integration tests for citadel.analyze_file with real COBOL."""

    @pytest.mark.skipif(
        not CARDDEMO_DIR.exists(),
        reason="CardDemo source not available",
    )
    def test_analyze_cobol_program(self, citadel) -> None:
        """Test analyzing a real COBOL program."""
        cobol_files = get_cobol_files()
        if not cobol_files:
            pytest.skip("No COBOL files found")

        # Analyze the first COBOL file
        cobol_file = cobol_files[0]
        result = citadel.analyze_file(cobol_file)

        assert result.error is None, f"Error analyzing {cobol_file}: {result.error}"
        assert result.language.lower() == "cobol"
        assert len(result.artifacts) > 0, "Should find at least one artifact"

        # Verify artifact structure
        for artifact in result.artifacts:
            assert artifact.name, "Artifact should have a name"
            assert artifact.type, "Artifact should have a type"

    @pytest.mark.skipif(
        not CARDDEMO_DIR.exists(),
        reason="CardDemo source not available",
    )
    def test_analyze_cbpaup0c(self, citadel) -> None:
        """Test analyzing CBPAUP0C.cbl specifically."""
        cbpaup0c = COBOL_DIR / "CBPAUP0C.cbl"
        if not cbpaup0c.exists():
            pytest.skip("CBPAUP0C.cbl not found")

        result = citadel.analyze_file(cbpaup0c)

        assert result.error is None, f"Error: {result.error}"
        assert result.language.lower() == "cobol"

        # CBPAUP0C is a batch program, should have paragraphs
        paragraph_types = ["paragraph", "section", "program"]
        has_code_artifacts = any(
            a.type.lower() in paragraph_types for a in result.artifacts
        )
        assert has_code_artifacts, "Should find code artifacts (paragraphs/sections)"

    @pytest.mark.skipif(
        not CARDDEMO_DIR.exists(),
        reason="CardDemo source not available",
    )
    def test_analyze_multiple_programs(self, citadel) -> None:
        """Test analyzing multiple COBOL programs."""
        cobol_files = get_cobol_files()[:5]  # Limit to 5 files
        if not cobol_files:
            pytest.skip("No COBOL files found")

        results: dict[str, Any] = {}
        for cobol_file in cobol_files:
            result = citadel.analyze_file(cobol_file)
            results[cobol_file.name] = {
                "error": result.error,
                "artifact_count": len(result.artifacts),
                "language": result.language,
            }

        # All should analyze without error
        for filename, info in results.items():
            assert info["error"] is None, f"{filename} had error: {info['error']}"
            assert info["language"].lower() == "cobol"


class TestCitadelGetFunctions:
    """Integration tests for citadel.get_functions with real COBOL."""

    @pytest.mark.skipif(
        not CARDDEMO_DIR.exists(),
        reason="CardDemo source not available",
    )
    def test_get_functions_from_cobol(self, citadel) -> None:
        """Test getting functions from a COBOL program."""
        cobol_files = get_cobol_files()
        if not cobol_files:
            pytest.skip("No COBOL files found")

        cobol_file = cobol_files[0]
        functions = citadel.get_functions(cobol_file)

        # Should not have error
        if functions and "error" in functions[0]:
            pytest.fail(f"Error getting functions: {functions[0]['error']}")

        # Should find some functions/paragraphs
        assert len(functions) > 0, f"No functions found in {cobol_file.name}"

        # Verify function structure
        for func in functions:
            assert "name" in func, "Function should have name"
            assert "type" in func, "Function should have type"

    @pytest.mark.skipif(
        not CARDDEMO_DIR.exists(),
        reason="CardDemo source not available",
    )
    def test_functions_have_callouts(self, citadel) -> None:
        """Test that functions have callouts (PERFORM, CALL, etc.)."""
        cobol_files = get_cobol_files()
        if not cobol_files:
            pytest.skip("No COBOL files found")

        # Find a file with callouts
        found_callouts = False
        for cobol_file in cobol_files[:5]:
            functions = citadel.get_functions(cobol_file)
            if functions and "error" not in functions[0]:
                for func in functions:
                    if func.get("calls"):
                        found_callouts = True
                        # Verify callout structure
                        for call in func["calls"]:
                            assert "target" in call, "Callout should have target"
                            assert "type" in call, "Callout should have type"
                        break
            if found_callouts:
                break

        # At least one file should have functions with callouts
        assert found_callouts, "No functions with callouts found in any file"


class TestCitadelGetCallouts:
    """Integration tests for citadel.get_callouts with real COBOL."""

    @pytest.mark.skipif(
        not CARDDEMO_DIR.exists(),
        reason="CardDemo source not available",
    )
    def test_get_callouts_from_file(self, citadel) -> None:
        """Test getting callouts from a single file."""
        cobol_files = get_cobol_files()
        if not cobol_files:
            pytest.skip("No COBOL files found")

        cobol_file = cobol_files[0]
        callouts = citadel.get_callouts(cobol_file)

        # Should not have error
        if callouts and "error" in callouts[0]:
            pytest.fail(f"Error getting callouts: {callouts[0]['error']}")

        # Verify callout structure (if any found)
        for callout in callouts:
            assert "from" in callout, "Callout should have 'from'"
            assert "to" in callout, "Callout should have 'to'"
            assert "type" in callout, "Callout should have 'type'"

    @pytest.mark.skipif(
        not CARDDEMO_DIR.exists(),
        reason="CardDemo source not available",
    )
    def test_get_callouts_from_directory(self, citadel) -> None:
        """Test getting callouts from directory with resolution status."""
        callouts = citadel.get_callouts(COBOL_DIR)

        if callouts and "error" in callouts[0]:
            pytest.fail(f"Error getting callouts: {callouts[0]['error']}")

        # Directory analysis should have resolved field
        has_resolved = False
        for callout in callouts:
            if "resolved" in callout:
                has_resolved = True
                break

        # Should have some callouts with resolution status
        assert len(callouts) > 0, "Should find callouts in directory"
        assert has_resolved, "Directory callouts should have resolved field"


class TestCitadelGetIncludes:
    """Integration tests for citadel.get_includes with real COBOL."""

    @pytest.mark.skipif(
        not CARDDEMO_DIR.exists(),
        reason="CardDemo source not available",
    )
    def test_get_includes_from_cobol(self, citadel) -> None:
        """Test getting COPY includes from COBOL programs."""
        cobol_files = get_cobol_files()
        if not cobol_files:
            pytest.skip("No COBOL files found")

        # Look for files with COPY statements
        found_includes = False
        for cobol_file in cobol_files:
            includes = citadel.get_includes(cobol_file)
            if includes:
                found_includes = True
                # Verify include names are strings
                for inc in includes:
                    assert isinstance(inc, str), f"Include should be string: {inc}"
                break

        # At least some files should have COPY statements
        assert found_includes, "No COPY includes found in any COBOL file"


class TestCitadelGetFunctionBody:
    """Integration tests for citadel.get_function_body with real COBOL."""

    @pytest.mark.skipif(
        not CARDDEMO_DIR.exists(),
        reason="CardDemo source not available",
    )
    def test_get_function_body_success(self, citadel) -> None:
        """Test extracting a function body from COBOL."""
        cobol_files = get_cobol_files()
        if not cobol_files:
            pytest.skip("No COBOL files found")

        # First, find a file with functions
        for cobol_file in cobol_files:
            functions = citadel.get_functions(cobol_file)
            if functions and "error" not in functions[0] and len(functions) > 0:
                func_name = functions[0]["name"]

                # Extract the function body
                body = citadel.get_function_body(cobol_file, func_name)

                if body is not None:
                    # Verify body content
                    assert isinstance(body, str), "Body should be string"
                    assert len(body) > 0, "Body should not be empty"
                    return

        pytest.skip("No extractable function bodies found")

    @pytest.mark.skipif(
        not CARDDEMO_DIR.exists(),
        reason="CardDemo source not available",
    )
    def test_get_function_body_not_found(self, citadel) -> None:
        """Test that non-existent function returns None."""
        cobol_files = get_cobol_files()
        if not cobol_files:
            pytest.skip("No COBOL files found")

        cobol_file = cobol_files[0]
        body = citadel.get_function_body(cobol_file, "NONEXISTENT-PARAGRAPH-12345")

        assert body is None, "Non-existent function should return None"


class TestCitadelGetFunctionBodies:
    """Integration tests for citadel.get_function_bodies with real COBOL."""

    @pytest.mark.skipif(
        not CARDDEMO_DIR.exists(),
        reason="CardDemo source not available",
    )
    def test_get_function_bodies_batch(self, citadel) -> None:
        """Test batch extraction of multiple function bodies."""
        cobol_files = get_cobol_files()
        if not cobol_files:
            pytest.skip("No COBOL files found")

        # Find a file with multiple functions
        for cobol_file in cobol_files:
            functions = citadel.get_functions(cobol_file)
            if functions and "error" not in functions[0] and len(functions) >= 2:
                func_names = [f["name"] for f in functions[:3]]

                # Batch extract
                bodies = citadel.get_function_bodies(cobol_file, func_names)

                assert isinstance(bodies, dict), "Should return dict"
                assert len(bodies) == len(func_names), "Should have all requested keys"

                # At least some should be found
                found_bodies = [v for v in bodies.values() if v is not None]
                if found_bodies:
                    return

        pytest.skip("No files with multiple extractable functions found")


class TestCitadelGetFileStats:
    """Integration tests for citadel.get_file_stats with real COBOL."""

    @pytest.mark.skipif(
        not CARDDEMO_DIR.exists(),
        reason="CardDemo source not available",
    )
    def test_get_file_stats(self, citadel) -> None:
        """Test getting file statistics."""
        cobol_files = get_cobol_files()
        if not cobol_files:
            pytest.skip("No COBOL files found")

        cobol_file = cobol_files[0]
        stats = citadel.get_file_stats(cobol_file)

        # Verify structure
        assert "total_lines" in stats, "Should have total_lines"
        assert "paragraph_count" in stats, "Should have paragraph_count"
        assert "language" in stats, "Should have language"
        assert "paragraphs" in stats, "Should have paragraphs list"

        # Verify values make sense
        assert stats["total_lines"] > 0, "Should have positive line count"
        assert stats["language"].lower() == "cobol"


class TestCitadelGetCallers:
    """Integration tests for citadel.get_callers with real COBOL."""

    @pytest.mark.skipif(
        not CARDDEMO_DIR.exists(),
        reason="CardDemo source not available",
    )
    def test_get_callers_internal(self, citadel) -> None:
        """Test finding callers of an internal paragraph."""
        cobol_files = get_cobol_files()
        if not cobol_files:
            pytest.skip("No COBOL files found")

        # Find a paragraph that is likely called internally
        for cobol_file in cobol_files:
            functions = citadel.get_functions(cobol_file)
            if functions and "error" not in functions[0]:
                # Look for a function with calls
                for func in functions:
                    calls = func.get("calls", [])
                    for call in calls:
                        if call.get("type") == "performs":
                            target = call.get("target")
                            # Try to find callers of this target
                            callers = citadel.get_callers(
                                cobol_file, target, search_paths=[COBOL_DIR]
                            )

                            if callers:
                                # Verify caller structure
                                for caller in callers:
                                    assert "file" in caller
                                    assert "type" in caller
                                return

        pytest.skip("No internal paragraph calls found")


class TestCitadelGetFlowDiagram:
    """Integration tests for citadel.get_flow_diagram with real COBOL."""

    @pytest.mark.skipif(
        not CARDDEMO_DIR.exists(),
        reason="CardDemo source not available",
    )
    def test_get_flow_diagram(self, citadel) -> None:
        """Test generating a flow diagram for COBOL."""
        cobol_files = get_cobol_files()
        if not cobol_files:
            pytest.skip("No COBOL files found")

        cobol_file = cobol_files[0]
        diagram = citadel.get_flow_diagram(cobol_file)

        assert isinstance(diagram, str), "Diagram should be string"
        assert "flowchart" in diagram.lower() or "graph" in diagram.lower(), (
            "Should be Mermaid flowchart"
        )

    @pytest.mark.skipif(
        not CARDDEMO_DIR.exists(),
        reason="CardDemo source not available",
    )
    def test_get_flow_diagram_from_paragraph(self, citadel) -> None:
        """Test generating a flow diagram starting from a specific paragraph."""
        cobol_files = get_cobol_files()
        if not cobol_files:
            pytest.skip("No COBOL files found")

        for cobol_file in cobol_files:
            functions = citadel.get_functions(cobol_file)
            if functions and "error" not in functions[0] and len(functions) > 0:
                # Find an actual paragraph (not program) for the flow diagram
                for func in functions:
                    func_type = func.get("type", "").lower()
                    # Only use paragraph types, not program types
                    if func_type in ("paragraph", "section"):
                        para_name = func["name"]
                        try:
                            diagram = citadel.get_flow_diagram(
                                cobol_file, paragraph=para_name
                            )
                            assert isinstance(diagram, str), "Diagram should be string"
                            return
                        except ValueError:
                            # Paragraph not suitable for flow diagram, try next
                            continue

        pytest.skip("No suitable paragraphs found for flow diagram")


class TestCitadelGetFileSummary:
    """Integration tests for citadel.get_file_summary with real COBOL."""

    @pytest.mark.skipif(
        not CARDDEMO_DIR.exists(),
        reason="CardDemo source not available",
    )
    def test_get_file_summary(self, citadel) -> None:
        """Test getting compact file summary."""
        cobol_files = get_cobol_files()
        if not cobol_files:
            pytest.skip("No COBOL files found")

        cobol_file = cobol_files[0]
        summary = citadel.get_file_summary(cobol_file)

        # Verify structure
        assert "file_name" in summary, "Should have file_name"
        assert "language" in summary, "Should have language"
        assert "total_lines" in summary, "Should have total_lines"
        assert "paragraph_count" in summary, "Should have paragraph_count"
        assert "entry_points" in summary, "Should have entry_points"
        assert "main_calls" in summary, "Should have main_calls"

        # Verify values
        assert summary["file_name"] == cobol_file.name
        assert summary["language"].lower() == "cobol"


class TestCitadelGetAnalysisPatterns:
    """Integration tests for citadel.get_analysis_patterns with real COBOL."""

    @pytest.mark.skipif(
        not CARDDEMO_DIR.exists(),
        reason="CardDemo source not available",
    )
    def test_get_analysis_patterns(self, citadel) -> None:
        """Test extracting analysis patterns from COBOL."""
        cobol_files = get_cobol_files()
        if not cobol_files:
            pytest.skip("No COBOL files found")

        cobol_file = cobol_files[0]
        result = citadel.get_analysis_patterns(cobol_file)

        # Should not have error
        assert result.error is None, f"Error: {result.error}"
        assert result.language.lower() == "cobol"

        # May or may not have matches depending on file content
        assert hasattr(result, "total_matches")
        assert hasattr(result, "categories")

    @pytest.mark.skipif(
        not CARDDEMO_DIR.exists(),
        reason="CardDemo source not available",
    )
    def test_get_analysis_patterns_summary_only(self, citadel) -> None:
        """Test analysis patterns with summary_only flag."""
        cobol_files = get_cobol_files()
        if not cobol_files:
            pytest.skip("No COBOL files found")

        cobol_file = cobol_files[0]
        result = citadel.get_analysis_patterns(cobol_file, summary_only=True)

        assert result.error is None, f"Error: {result.error}"

        # In summary mode, matches list should be empty but counts preserved
        for category_result in result.categories.values():
            # Summary mode clears individual matches
            assert isinstance(category_result.matches, list)


class TestCitadelGetSequenceDiagrams:
    """Integration tests for citadel.get_sequence_diagrams with real COBOL."""

    @pytest.mark.skipif(
        not CARDDEMO_DIR.exists(),
        reason="CardDemo source not available",
    )
    def test_get_sequence_diagrams(self, citadel) -> None:
        """Test generating sequence diagrams from directory."""
        diagrams = citadel.get_sequence_diagrams(
            COBOL_DIR, max_diagrams=2, min_sequence_length=1
        )

        # May or may not find sequences depending on codebase complexity
        assert isinstance(diagrams, list), "Should return list"

        for diagram in diagrams:
            assert isinstance(diagram, str), "Each diagram should be string"
            assert "sequenceDiagram" in diagram or "sequence" in diagram.lower(), (
                "Should be Mermaid sequence diagram"
            )


class TestCitadelGetDeadCode:
    """Integration tests for citadel.get_dead_code with real COBOL."""

    @pytest.mark.skipif(
        not CARDDEMO_DIR.exists(),
        reason="CardDemo source not available",
    )
    def test_get_dead_code(self, citadel) -> None:
        """Test dead code detection."""
        dead_code = citadel.get_dead_code(COBOL_DIR)

        assert isinstance(dead_code, list), "Should return list"

        # Verify structure if any dead code found
        for item in dead_code:
            assert "name" in item, "Dead code item should have name"
            assert "type" in item, "Dead code item should have type"
            assert "reason" in item, "Dead code item should have reason"


class TestEndToEndToolBehavior:
    """End-to-end tests simulating how tools would be used by an agent."""

    @pytest.mark.skipif(
        not CARDDEMO_DIR.exists(),
        reason="CardDemo source not available",
    )
    def test_analyze_then_get_body_workflow(self, citadel) -> None:
        """Test workflow: analyze file -> get function body."""
        cobol_files = get_cobol_files()
        if not cobol_files:
            pytest.skip("No COBOL files found")

        cobol_file = cobol_files[0]

        # Step 1: Analyze the file
        analysis = citadel.analyze_file(cobol_file)
        assert analysis.error is None

        # Step 2: Get a function's body
        if analysis.artifacts:
            func_name = analysis.artifacts[0].name
            body = citadel.get_function_body(cobol_file, func_name)

            # Body may be None if extraction fails, but no exception
            assert body is None or isinstance(body, str)

    @pytest.mark.skipif(
        not CARDDEMO_DIR.exists(),
        reason="CardDemo source not available",
    )
    def test_find_callers_workflow(self, citadel) -> None:
        """Test workflow: find a function -> find its callers."""
        cobol_files = get_cobol_files()
        if not cobol_files:
            pytest.skip("No COBOL files found")

        # Find a function that might have callers
        for cobol_file in cobol_files:
            functions = citadel.get_functions(cobol_file)
            if functions and "error" not in functions[0]:
                for func in functions:
                    if func.get("calls"):
                        # Try to find callers of one of its callees
                        target = func["calls"][0]["target"]
                        callers = citadel.get_callers(
                            cobol_file, target, search_paths=[COBOL_DIR]
                        )

                        # Result is a list (may be empty)
                        assert isinstance(callers, list)
                        return

        pytest.skip("No suitable call relationships found")

    @pytest.mark.skipif(
        not CARDDEMO_DIR.exists(),
        reason="CardDemo source not available",
    )
    def test_comprehensive_analysis_workflow(self, citadel) -> None:
        """Test comprehensive analysis workflow."""
        cobol_files = get_cobol_files()
        if not cobol_files:
            pytest.skip("No COBOL files found")

        cobol_file = cobol_files[0]

        # Gather all analysis data
        analysis = citadel.analyze_file(cobol_file)
        functions = citadel.get_functions(cobol_file)
        callouts = citadel.get_callouts(cobol_file)
        includes = citadel.get_includes(cobol_file)
        stats = citadel.get_file_stats(cobol_file)
        summary = citadel.get_file_summary(cobol_file)
        flow = citadel.get_flow_diagram(cobol_file)

        # All should complete without exceptions
        assert analysis is not None
        assert isinstance(functions, list)
        assert isinstance(callouts, list)
        assert isinstance(includes, list)
        assert isinstance(stats, dict)
        assert isinstance(summary, dict)
        assert isinstance(flow, str)

        # Cross-check some data consistency
        if analysis.error is None:
            # Stats and analysis should agree on paragraph count
            assert stats["paragraph_count"] == len(analysis.artifacts)

            # Summary should match stats
            assert summary["paragraph_count"] == stats["paragraph_count"]


class TestCopybooks:
    """Integration tests for copybook analysis."""

    @pytest.mark.skipif(
        not CARDDEMO_DIR.exists(),
        reason="CardDemo source not available",
    )
    def test_analyze_copybook(self, citadel) -> None:
        """Test analyzing copybook files."""
        copybooks = get_copybook_files()
        if not copybooks:
            pytest.skip("No copybook files found")

        copybook = copybooks[0]
        result = citadel.analyze_file(copybook)

        # Copybooks may or may not analyze depending on spec support
        # The important thing is no exception
        assert result is not None

    @pytest.mark.skipif(
        not CARDDEMO_DIR.exists(),
        reason="CardDemo source not available",
    )
    def test_find_copybook_usage(self, citadel) -> None:
        """Test finding where copybooks are used."""
        copybooks = get_copybook_files()
        if not copybooks:
            pytest.skip("No copybook files found")

        # Get a copybook name
        copybook = copybooks[0]
        copybook_name = copybook.stem  # Name without extension

        # Search for COPY statements referencing this copybook
        callers = citadel.get_callers(
            copybook, copybook_name, search_paths=[COBOL_DIR]
        )

        # Result should be a list (may be empty if not used)
        assert isinstance(callers, list)


class TestJCL:
    """Integration tests for JCL analysis."""

    @pytest.mark.skipif(
        not CARDDEMO_DIR.exists() or not JCL_DIR.exists(),
        reason="CardDemo JCL not available",
    )
    def test_analyze_jcl(self, citadel) -> None:
        """Test analyzing JCL files."""
        jcl_files = list(JCL_DIR.glob("*.jcl"))
        if not jcl_files:
            pytest.skip("No JCL files found")

        jcl_file = jcl_files[0]
        result = citadel.analyze_file(jcl_file)

        # JCL may or may not be supported - important thing is no exception
        assert result is not None


class TestErrorHandling:
    """Integration tests for error handling with real files."""

    @pytest.mark.skipif(
        not CARDDEMO_DIR.exists(),
        reason="CardDemo source not available",
    )
    def test_analyze_nonexistent_file(self, citadel) -> None:
        """Test analyzing a file that doesn't exist."""
        fake_path = COBOL_DIR / "NONEXISTENT123456.cbl"
        result = citadel.analyze_file(fake_path)

        # Should return result with error, not raise exception
        assert result.error is not None

    @pytest.mark.skipif(
        not CARDDEMO_DIR.exists(),
        reason="CardDemo source not available",
    )
    def test_get_callers_nonexistent_function(self, citadel) -> None:
        """Test finding callers of a non-existent function."""
        cobol_files = get_cobol_files()
        if not cobol_files:
            pytest.skip("No COBOL files found")

        callers = citadel.get_callers(
            cobol_files[0],
            "NONEXISTENT-FUNCTION-123456",
            search_paths=[COBOL_DIR],
        )

        # Should return empty list, not raise exception
        assert isinstance(callers, list)
        assert len(callers) == 0
