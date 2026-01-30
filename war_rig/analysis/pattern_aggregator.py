"""Pattern Aggregator for transforming Citadel analysis patterns into agent insights.

This module provides the PatternAggregator class which transforms raw Citadel
pattern matches (data_flow, control_flow, error_handling) into summarized,
actionable insights for the Scribe and Challenger agents.

Key Design Principles:
- Summarize, don't dump: Transform raw matches into aggregated insights
- Paragraph-level focus: Align pattern insights with paragraph documentation
- Actionable guidance: Tell agents what to document, not just what exists
- Bounded context: Stay within token budgets via intelligent filtering
- Differentiated use: Scribe uses for writing, Challenger uses for validation
"""

from __future__ import annotations

import logging
from collections import Counter
from dataclasses import dataclass, field
from typing import Any

logger = logging.getLogger(__name__)

# Hint generation templates for different pattern combinations
HINT_TEMPLATES = {
    "data_flow_heavy": (
        "Modifies {write_count} variables ({top_writes}). "
        "Document the transformations."
    ),
    "control_flow_complex": (
        "Contains {decision_count} decision points. "
        "Explain the business logic for each branch."
    ),
    "error_handling": (
        "Handles {error_types} errors. "
        "Document recovery/abend behavior."
    ),
    "loop_processing": (
        "Loops using PERFORM {loop_type}. "
        "Describe termination condition and iteration logic."
    ),
    "simple_delegation": (
        "Delegates to {called_paragraphs}. "
        "Explain orchestration purpose."
    ),
}

# Token budget constants
MAX_FILE_SUMMARY_TOKENS = 200
MAX_PER_PARAGRAPH_TOKENS = 100
MAX_TOTAL_TOKENS = 4000


def estimate_tokens(text: str) -> int:
    """Rough token estimate: ~4 chars per token.

    Args:
        text: Text to estimate tokens for.

    Returns:
        Estimated token count.
    """
    return len(text) // 4


@dataclass
class ParagraphPatternStats:
    """Statistics for patterns within a single paragraph."""

    paragraph_name: str
    line_start: int | None = None
    line_end: int | None = None

    # Data flow stats
    variables_read: list[str] = field(default_factory=list)
    variables_written: list[str] = field(default_factory=list)
    key_transformations: list[str] = field(default_factory=list)

    # Control flow stats
    decision_points: int = 0
    performs_paragraphs: list[str] = field(default_factory=list)
    has_loop: bool = False
    loop_type: str | None = None

    # Error handling stats
    handles_errors: bool = False
    error_types: list[str] = field(default_factory=list)

    # Pattern counts
    data_flow_count: int = 0
    control_flow_count: int = 0
    error_handling_count: int = 0

    # Key operations (for challenger validation)
    key_operations: list[str] = field(default_factory=list)


class PatternAggregator:
    """Transforms raw Citadel patterns into agent-consumable insights.

    The aggregator takes raw pattern matches from Citadel's get_analysis_patterns()
    and transforms them into summarized insights suitable for LLM consumption.
    Output is bounded to stay within token budgets.

    Example:
        aggregator = PatternAggregator()
        pattern_result = citadel.get_analysis_patterns(file_path)
        citadel_outline = [{"name": "MAIN", "line_start": 100, "line_end": 200, ...}]

        # For Scribe documentation
        insights = aggregator.aggregate_for_scribe(pattern_result, citadel_outline)

        # For Challenger validation
        facts = aggregator.aggregate_for_challenger(pattern_result, citadel_outline)
    """

    def __init__(
        self,
        max_variables: int = 10,
        max_paragraphs: int = 20,
        max_hints_per_paragraph: int = 4,
    ):
        """Initialize the PatternAggregator.

        Args:
            max_variables: Maximum number of key variables to include.
            max_paragraphs: Maximum number of paragraphs to include hints for.
            max_hints_per_paragraph: Maximum hints per paragraph.
        """
        self.max_variables = max_variables
        self.max_paragraphs = max_paragraphs
        self.max_hints_per_paragraph = max_hints_per_paragraph

    def aggregate_for_scribe(
        self,
        pattern_result: Any,  # FileAnalysisPatternResult from Citadel SDK
        citadel_outline: list[dict] | None,
    ) -> dict[str, Any]:
        """Transform patterns into Scribe-friendly insights.

        Creates a structured dictionary containing:
        - file_summary: Overview of key variables and complexity indicators
        - paragraph_hints: Per-paragraph documentation guidance
        - critical_patterns: Specific patterns that must be documented

        Args:
            pattern_result: Result from Citadel.get_analysis_patterns().
            citadel_outline: List of paragraph dicts with name, line_start, line_end.

        Returns:
            Dictionary with file_summary, paragraph_hints, critical_patterns.
        """
        if pattern_result is None or pattern_result.error:
            return {}

        citadel_outline = citadel_outline or []

        # Build paragraph list from outline
        paragraphs = [
            {
                "name": p.get("name", ""),
                "line_start": p.get("line_start"),
                "line_end": p.get("line_end"),
            }
            for p in citadel_outline
            if p.get("name")
        ]

        # Collect all matches
        all_matches = self._collect_all_matches(pattern_result)

        # Associate patterns with paragraphs
        para_matches = self._associate_patterns_with_paragraphs(all_matches, paragraphs)

        # Extract file-level summary
        file_summary = self._build_file_summary(pattern_result, all_matches)

        # Generate per-paragraph hints (limited to max_paragraphs)
        paragraph_hints = self._build_paragraph_hints(para_matches, paragraphs)

        # Identify critical patterns
        critical_patterns = self._identify_critical_patterns(para_matches, all_matches)

        return {
            "file_summary": file_summary,
            "paragraph_hints": paragraph_hints[:self.max_paragraphs],
            "critical_patterns": critical_patterns[:10],
        }

    def aggregate_for_challenger(
        self,
        pattern_result: Any,  # FileAnalysisPatternResult from Citadel SDK
        citadel_outline: list[dict] | None,
    ) -> dict[str, Any]:
        """Transform patterns into Challenger validation facts.

        Creates a structured dictionary containing:
        - paragraph_facts: Ground-truth counts per paragraph for validation
        - validation_cues: Specific checks the Challenger should perform
        - expected_coverage: Minimum expected mentions per paragraph

        Args:
            pattern_result: Result from Citadel.get_analysis_patterns().
            citadel_outline: List of paragraph dicts with name, line_start, line_end.

        Returns:
            Dictionary with paragraph_facts, validation_cues, expected_coverage.
        """
        if pattern_result is None or pattern_result.error:
            return {}

        citadel_outline = citadel_outline or []

        # Build paragraph list from outline
        paragraphs = [
            {
                "name": p.get("name", ""),
                "line_start": p.get("line_start"),
                "line_end": p.get("line_end"),
            }
            for p in citadel_outline
            if p.get("name")
        ]

        # Collect all matches
        all_matches = self._collect_all_matches(pattern_result)

        # Associate patterns with paragraphs
        para_matches = self._associate_patterns_with_paragraphs(all_matches, paragraphs)

        # Build paragraph facts
        paragraph_facts = self._build_paragraph_facts(para_matches, paragraphs)

        # Generate validation cues
        validation_cues = self._generate_validation_cues(para_matches, paragraphs)

        # Calculate expected coverage
        expected_coverage = self._calculate_expected_coverage(para_matches)

        return {
            "paragraph_facts": paragraph_facts[:self.max_paragraphs],
            "validation_cues": validation_cues[:10],
            "expected_coverage": expected_coverage,
        }

    def _collect_all_matches(self, pattern_result: Any) -> list[dict[str, Any]]:
        """Collect all matches from pattern result into a flat list.

        Args:
            pattern_result: Result from Citadel.get_analysis_patterns().

        Returns:
            List of match dicts with pattern_name, category, captured, line.
        """
        all_matches: list[dict[str, Any]] = []

        if not hasattr(pattern_result, 'categories'):
            return all_matches

        for category, cat_result in pattern_result.categories.items():
            if not hasattr(cat_result, 'matches'):
                continue
            for match in cat_result.matches:
                all_matches.append({
                    "pattern_name": match.pattern_name,
                    "category": match.category,
                    "captured": match.captured,
                    "line": match.line,
                    "context": getattr(match, 'context', []),
                })

        return all_matches

    def _associate_patterns_with_paragraphs(
        self,
        matches: list[dict[str, Any]],
        paragraphs: list[dict[str, Any]],
    ) -> dict[str, list[dict[str, Any]]]:
        """Group pattern matches by containing paragraph using line ranges.

        Args:
            matches: List of pattern match dicts with line numbers.
            paragraphs: List of paragraph dicts with name, line_start, line_end.

        Returns:
            Dictionary mapping paragraph name to list of matches in that paragraph.
        """
        result: dict[str, list[dict[str, Any]]] = {}

        # Initialize empty lists for each paragraph
        for para in paragraphs:
            para_name = para.get("name", "")
            if para_name:
                result[para_name] = []

        # Sort paragraphs by line_start for efficient lookup
        sorted_paragraphs = sorted(
            [p for p in paragraphs if p.get("line_start") is not None],
            key=lambda p: p.get("line_start", 0),
        )

        # Associate each match with its containing paragraph
        for match in matches:
            match_line = match.get("line")
            if match_line is None:
                continue

            # Find the paragraph containing this line
            for para in sorted_paragraphs:
                line_start = para.get("line_start")
                line_end = para.get("line_end")
                para_name = para.get("name", "")

                if line_start is None or line_end is None:
                    continue

                if line_start <= match_line <= line_end:
                    if para_name in result:
                        result[para_name].append(match)
                    break

        return result

    def _extract_variable_frequency(
        self,
        data_flow_matches: list[dict[str, Any]],
    ) -> dict[str, int]:
        """Count variable occurrences across data flow patterns.

        Args:
            data_flow_matches: List of data_flow pattern matches.

        Returns:
            Dictionary mapping variable name to occurrence count.
        """
        var_counts: Counter[str] = Counter()

        for match in data_flow_matches:
            captured = match.get("captured", [])
            pattern_name = match.get("pattern_name", "")

            # Extract variables based on pattern type
            if pattern_name in ("move_simple", "move_corresponding"):
                # captured: [source, target]
                if len(captured) >= 2:
                    var_counts[captured[0]] += 1  # source (read)
                    var_counts[captured[1]] += 1  # target (write)
            elif pattern_name in ("compute_expression", "compute"):
                # captured: [target, expression] or [target]
                if captured:
                    var_counts[captured[0]] += 1
            elif pattern_name in (
                "add_statement", "subtract_statement",
                "multiply_statement", "divide_statement",
                "add", "subtract", "multiply", "divide",
            ):
                # captured: [var1, var2, result] or variations
                for var in captured:
                    if var:
                        var_counts[var] += 1
            elif pattern_name in ("initialize_statement", "initialize"):
                # captured: [target]
                if captured:
                    var_counts[captured[0]] += 1
            elif pattern_name in ("set_statement", "set"):
                # captured: [target, value]
                if captured:
                    var_counts[captured[0]] += 1
            else:
                # Generic: count all captured values
                for var in captured:
                    if var and isinstance(var, str):
                        var_counts[var] += 1

        return dict(var_counts)

    def _compute_paragraph_complexity(
        self,
        para_matches: list[dict[str, Any]],
    ) -> str:
        """Classify paragraph as simple/moderate/complex.

        Classification based on:
        - Total pattern matches
        - Decision points (IF/EVALUATE)
        - Error handling presence
        - Loop presence

        Args:
            para_matches: List of pattern matches for a single paragraph.

        Returns:
            "simple", "moderate", or "complex"
        """
        if not para_matches:
            return "simple"

        total_matches = len(para_matches)
        decision_points = sum(
            1 for m in para_matches
            if m.get("pattern_name", "") in (
                "if_perform", "if_call", "evaluate_when", "if_then",
                "if_else", "evaluate", "if_condition",
            )
        )
        has_loops = any(
            m.get("pattern_name", "") in (
                "perform_until", "perform_varying", "perform_times",
                "perform_thru",
            )
            for m in para_matches
        )
        has_error_handling = any(
            m.get("category") == "error_handling"
            for m in para_matches
        )

        # Scoring
        complexity_score = 0
        complexity_score += min(total_matches // 5, 3)  # 0-3 points for match count
        complexity_score += min(decision_points, 3)     # 0-3 points for decisions
        complexity_score += 2 if has_loops else 0       # 2 points for loops
        complexity_score += 1 if has_error_handling else 0  # 1 point for error handling

        if complexity_score <= 2:
            return "simple"
        elif complexity_score <= 5:
            return "moderate"
        else:
            return "complex"

    def _generate_documentation_hints(
        self,
        paragraph_name: str,
        para_matches: list[dict[str, Any]],
    ) -> list[str]:
        """Generate natural language documentation hints.

        Args:
            paragraph_name: Name of the paragraph.
            para_matches: List of pattern matches for this paragraph.

        Returns:
            List of hint strings (up to max_hints_per_paragraph).
        """
        hints: list[str] = []

        # Categorize matches
        data_flow_matches = [m for m in para_matches if m.get("category") == "data_flow"]
        control_flow_matches = [m for m in para_matches if m.get("category") == "control_flow"]
        error_matches = [m for m in para_matches if m.get("category") == "error_handling"]

        # Extract variable frequency for data flow hints
        var_freq = self._extract_variable_frequency(data_flow_matches)
        top_vars = sorted(var_freq.items(), key=lambda x: x[1], reverse=True)[:5]

        # Data flow hint
        if len(data_flow_matches) >= 3:
            write_count = len([
                m for m in data_flow_matches
                if m.get("pattern_name", "") in (
                    "move_simple", "move_corresponding", "compute_expression",
                    "compute", "initialize_statement", "initialize",
                    "set_statement", "set",
                )
            ])
            top_writes = ", ".join(v[0] for v in top_vars[:3]) if top_vars else "variables"
            hints.append(HINT_TEMPLATES["data_flow_heavy"].format(
                write_count=write_count,
                top_writes=top_writes,
            ))

        # Control flow hint
        decision_count = sum(
            1 for m in control_flow_matches
            if m.get("pattern_name", "") in (
                "if_perform", "if_call", "evaluate_when", "if_then",
                "if_else", "evaluate", "if_condition",
            )
        )
        if decision_count >= 2:
            hints.append(HINT_TEMPLATES["control_flow_complex"].format(
                decision_count=decision_count,
            ))

        # Loop hint
        loop_matches = [
            m for m in control_flow_matches
            if m.get("pattern_name", "") in (
                "perform_until", "perform_varying", "perform_times",
            )
        ]
        if loop_matches:
            loop_type = loop_matches[0].get("pattern_name", "").replace("perform_", "").upper()
            hints.append(HINT_TEMPLATES["loop_processing"].format(
                loop_type=loop_type,
            ))

        # Error handling hint
        if error_matches:
            error_types_set: set[str] = set()
            for m in error_matches:
                pname = m.get("pattern_name", "")
                if "file_status" in pname:
                    error_types_set.add("file status")
                elif "sqlcode" in pname:
                    error_types_set.add("DB2 SQLCODE")
                elif "exception" in pname:
                    error_types_set.add("exception")
                elif "invalid_key" in pname:
                    error_types_set.add("invalid key")
                elif "at_end" in pname:
                    error_types_set.add("at-end")
                else:
                    error_types_set.add(pname.replace("_", " "))

            error_types_str = ", ".join(sorted(error_types_set)[:3])
            hints.append(HINT_TEMPLATES["error_handling"].format(
                error_types=error_types_str,
            ))

        # Simple delegation hint (when mostly PERFORMs with few other operations)
        perform_matches = [
            m for m in control_flow_matches
            if "perform" in m.get("pattern_name", "").lower()
        ]
        if perform_matches and len(data_flow_matches) < 3:
            targets = []
            for m in perform_matches:
                captured = m.get("captured", [])
                if captured:
                    targets.append(captured[0])
            if targets:
                called_paras = ", ".join(targets[:3])
                hints.append(HINT_TEMPLATES["simple_delegation"].format(
                    called_paragraphs=called_paras,
                ))

        return hints[:self.max_hints_per_paragraph]

    def _build_file_summary(
        self,
        pattern_result: Any,
        all_matches: list[dict[str, Any]],
    ) -> dict[str, Any]:
        """Build file-level summary of patterns.

        Args:
            pattern_result: Result from Citadel.get_analysis_patterns().
            all_matches: List of all matches.

        Returns:
            Dictionary with key_variables, complexity_indicators.
        """
        # Count by category
        data_flow_count = sum(
            1 for m in all_matches if m.get("category") == "data_flow"
        )
        control_flow_count = sum(
            1 for m in all_matches if m.get("category") == "control_flow"
        )
        error_handling_count = sum(
            1 for m in all_matches if m.get("category") == "error_handling"
        )

        # Extract key variables
        data_flow_matches = [m for m in all_matches if m.get("category") == "data_flow"]
        var_freq = self._extract_variable_frequency(data_flow_matches)
        top_vars = sorted(var_freq.items(), key=lambda x: x[1], reverse=True)
        key_variables = [v[0] for v in top_vars[:self.max_variables]]

        # Detect complexity indicators
        has_loops = any(
            "loop" in m.get("pattern_name", "").lower() or
            "until" in m.get("pattern_name", "").lower() or
            "varying" in m.get("pattern_name", "").lower()
            for m in all_matches
        )
        has_nested = control_flow_count > 10
        has_go_to = any(
            "go_to" in m.get("pattern_name", "").lower()
            for m in all_matches
        )

        # Error coverage rating
        if error_handling_count >= 5:
            error_coverage = "good"
        elif error_handling_count >= 2:
            error_coverage = "moderate"
        elif error_handling_count >= 1:
            error_coverage = "minimal"
        else:
            error_coverage = "none"

        return {
            "key_variables": key_variables,
            "complexity_indicators": {
                "total_data_ops": data_flow_count,
                "decision_points": control_flow_count,
                "has_loops": has_loops,
                "has_nested_conditionals": has_nested,
                "has_go_to": has_go_to,
                "error_coverage": error_coverage,
            },
        }

    def _build_paragraph_hints(
        self,
        para_matches: dict[str, list[dict[str, Any]]],
        paragraphs: list[dict[str, Any]],
    ) -> list[dict[str, Any]]:
        """Build per-paragraph documentation hints.

        Args:
            para_matches: Dictionary mapping paragraph name to matches.
            paragraphs: List of paragraph dicts with name, line_start, line_end.

        Returns:
            List of hint dicts with paragraph_name, hints, variables, complexity.
        """
        results: list[dict[str, Any]] = []

        # Build paragraph info lookup
        para_info = {p.get("name", ""): p for p in paragraphs if p.get("name")}

        # Calculate complexity for sorting
        para_complexity: list[tuple[str, int, str]] = []
        for para_name, matches in para_matches.items():
            complexity = self._compute_paragraph_complexity(matches)
            # Score for sorting (complex=2, moderate=1, simple=0)
            score = {"complex": 2, "moderate": 1, "simple": 0}.get(complexity, 0)
            para_complexity.append((para_name, score, complexity))

        # Sort by complexity (most complex first)
        para_complexity.sort(key=lambda x: (-x[1], x[0]))

        for para_name, score, complexity in para_complexity[:self.max_paragraphs]:
            matches = para_matches.get(para_name, [])

            # Generate hints
            hints = self._generate_documentation_hints(para_name, matches)

            # Extract variables
            data_flow_matches = [m for m in matches if m.get("category") == "data_flow"]
            var_freq = self._extract_variable_frequency(data_flow_matches)

            # Separate read vs write (simplified heuristic)
            variables_read: list[str] = []
            variables_write: list[str] = []
            for m in data_flow_matches:
                captured = m.get("captured", [])
                pattern = m.get("pattern_name", "")
                if pattern in ("move_simple", "move_corresponding") and len(captured) >= 2:
                    if captured[0] not in variables_read:
                        variables_read.append(captured[0])
                    if captured[1] not in variables_write:
                        variables_write.append(captured[1])
                elif captured:
                    # First captured is usually the target
                    if captured[0] not in variables_write:
                        variables_write.append(captured[0])

            results.append({
                "paragraph_name": para_name,
                "hints": hints,
                "variables": {
                    "read": variables_read[:5],
                    "write": variables_write[:5],
                },
                "complexity": complexity,
            })

        return results

    def _identify_critical_patterns(
        self,
        para_matches: dict[str, list[dict[str, Any]]],
        all_matches: list[dict[str, Any]],
    ) -> list[str]:
        """Identify critical patterns that must be documented.

        Args:
            para_matches: Dictionary mapping paragraph name to matches.
            all_matches: List of all matches.

        Returns:
            List of critical pattern description strings.
        """
        critical: list[str] = []

        # Find COMPUTE operations (calculations are usually important)
        for match in all_matches:
            if match.get("pattern_name", "") in ("compute_expression", "compute"):
                captured = match.get("captured", [])
                line = match.get("line")
                if captured and line:
                    target = captured[0]
                    critical.append(
                        f"Document COMPUTE {target} at line {line}"
                    )

        # Find error handling patterns
        for match in all_matches:
            if match.get("category") == "error_handling":
                line = match.get("line")
                pname = match.get("pattern_name", "")
                if line:
                    critical.append(
                        f"Document error handling ({pname}) at line {line}"
                    )

        # Find loop termination conditions
        for match in all_matches:
            pname = match.get("pattern_name", "")
            if pname in ("perform_until", "perform_varying"):
                captured = match.get("captured", [])
                line = match.get("line")
                if captured and line:
                    condition = captured[1] if len(captured) > 1 else captured[0]
                    critical.append(
                        f"Document loop termination: {condition} at line {line}"
                    )

        return critical[:10]

    def _build_paragraph_facts(
        self,
        para_matches: dict[str, list[dict[str, Any]]],
        paragraphs: list[dict[str, Any]],
    ) -> list[dict[str, Any]]:
        """Build per-paragraph ground-truth facts for Challenger validation.

        Args:
            para_matches: Dictionary mapping paragraph name to matches.
            paragraphs: List of paragraph dicts.

        Returns:
            List of fact dicts per paragraph.
        """
        results: list[dict[str, Any]] = []

        for para_name, matches in para_matches.items():
            data_flow_count = sum(
                1 for m in matches if m.get("category") == "data_flow"
            )
            control_flow_count = sum(
                1 for m in matches if m.get("category") == "control_flow"
            )
            error_handling_count = sum(
                1 for m in matches if m.get("category") == "error_handling"
            )

            # Extract key operations
            key_ops: list[str] = []
            for m in matches[:5]:  # Limit to first 5
                pname = m.get("pattern_name", "")
                captured = m.get("captured", [])
                if captured:
                    key_ops.append(f"{pname.upper()} {captured[0]}")

            results.append({
                "paragraph_name": para_name,
                "data_flow_count": data_flow_count,
                "control_flow_count": control_flow_count,
                "error_handling_count": error_handling_count,
                "key_operations": key_ops[:3],
            })

        # Sort by total pattern count descending
        results.sort(key=lambda x: (
            -(x.get("data_flow_count", 0) +
              x.get("control_flow_count", 0) +
              x.get("error_handling_count", 0))
        ))

        return results

    def _generate_validation_cues(
        self,
        para_matches: dict[str, list[dict[str, Any]]],
        paragraphs: list[dict[str, Any]],
    ) -> list[str]:
        """Generate validation checkpoints for Challenger.

        Args:
            para_matches: Dictionary mapping paragraph name to matches.
            paragraphs: List of paragraph dicts.

        Returns:
            List of validation cue strings.
        """
        cues: list[str] = []

        for para_name, matches in para_matches.items():
            data_flow_count = sum(
                1 for m in matches if m.get("category") == "data_flow"
            )
            error_handling_count = sum(
                1 for m in matches if m.get("category") == "error_handling"
            )

            # High data flow operations
            if data_flow_count >= 5:
                cues.append(
                    f"{para_name} has {data_flow_count} data flow ops - "
                    f"verify paragraph describes data transformations"
                )

            # Error handling
            if error_handling_count >= 1:
                cues.append(
                    f"{para_name} handles {error_handling_count} error condition(s) - "
                    f"verify error handling is documented"
                )

            # Complex paragraph
            complexity = self._compute_paragraph_complexity(matches)
            if complexity == "complex":
                cues.append(
                    f"{para_name} is complex - verify documentation is detailed"
                )

        return cues

    def _calculate_expected_coverage(
        self,
        para_matches: dict[str, list[dict[str, Any]]],
    ) -> dict[str, int]:
        """Calculate expected documentation coverage per paragraph.

        Args:
            para_matches: Dictionary mapping paragraph name to matches.

        Returns:
            Dictionary mapping paragraph name to expected mention count.
        """
        expected: dict[str, int] = {}

        for para_name, matches in para_matches.items():
            data_flow_count = sum(
                1 for m in matches if m.get("category") == "data_flow"
            )
            # Expect at least 1 mention per 10 data flow operations
            expected[para_name] = max(1, data_flow_count // 10)

        return expected
