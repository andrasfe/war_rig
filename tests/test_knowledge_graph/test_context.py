"""Unit tests for ContextFormatter.

Tests:
- format() with None neighborhood (cold start)
- format() with empty triples
- format() with typical neighborhood
- format() token budget and summarization
- format_for_challenger() cross-check hints
- _group_triples_by_relation() categorization logic
- _summarize() compressed output
- _estimate_tokens() heuristic
"""

from war_rig.knowledge_graph.context import ContextFormatter
from war_rig.knowledge_graph.models import (
    Entity,
    EntityType,
    Neighborhood,
    RelationType,
    Triple,
)


def _make_entity(
    id: int,
    entity_type: EntityType,
    name: str,
) -> Entity:
    """Helper to create an Entity with minimal boilerplate."""
    return Entity(id=id, entity_type=entity_type, name=name)


def _make_triple(
    subject_id: int,
    predicate: RelationType,
    object_id: int,
    **kwargs,
) -> Triple:
    """Helper to create a Triple with minimal boilerplate."""
    return Triple(
        subject_id=subject_id,
        predicate=predicate,
        object_id=object_id,
        **kwargs,
    )


# Reusable test fixtures
_TARGET = _make_entity(1, EntityType.PROGRAM, "ACCT0100")
_CALLED_PROG = _make_entity(2, EntityType.PROGRAM, "ACCT0200")
_CALLER_PROG = _make_entity(3, EntityType.PROGRAM, "ACCT0300")
_DATASET_IN = _make_entity(4, EntityType.DATASET, "ACCT.MASTER.FILE")
_DATASET_OUT = _make_entity(5, EntityType.DATASET, "ACCT.REPORT.FILE")
_COPYBOOK = _make_entity(6, EntityType.COPYBOOK, "ACCTCPY1")
_JCL_JOB = _make_entity(7, EntityType.JCL_JOB, "ACCTJOB1")
_DB_TABLE = _make_entity(8, EntityType.DB_TABLE, "ACCOUNT_TBL")


def _make_standard_neighborhood() -> Neighborhood:
    """Build a typical neighborhood for testing."""
    return Neighborhood(
        target=_TARGET,
        entities=[
            _TARGET,
            _CALLED_PROG,
            _CALLER_PROG,
            _DATASET_IN,
            _DATASET_OUT,
            _COPYBOOK,
            _JCL_JOB,
        ],
        triples=[
            _make_triple(1, RelationType.CALLS, 2),       # target CALLS ACCT0200
            _make_triple(3, RelationType.CALLS, 1),        # ACCT0300 CALLS target
            _make_triple(1, RelationType.READS, 4),        # target READS dataset
            _make_triple(1, RelationType.WRITES, 5),       # target WRITES dataset
            _make_triple(1, RelationType.INCLUDES, 6),     # target INCLUDES copybook
            _make_triple(7, RelationType.EXECUTES, 1),     # JCL EXECUTES target
        ],
    )


class TestContextFormatterFormat:
    """Tests for ContextFormatter.format()."""

    def test_none_returns_empty(self):
        """Cold start: None neighborhood returns empty string."""
        formatter = ContextFormatter()
        assert formatter.format(None) == ""

    def test_empty_triples_returns_empty(self):
        """Neighborhood with no triples returns empty string."""
        neighborhood = Neighborhood(
            target=_TARGET,
            entities=[_TARGET],
            triples=[],
        )
        formatter = ContextFormatter()
        assert formatter.format(neighborhood) == ""

    def test_standard_neighborhood_has_header(self):
        """Output starts with the required header."""
        formatter = ContextFormatter()
        result = formatter.format(_make_standard_neighborhood())
        assert result.startswith("## System Context (from knowledge graph)")

    def test_standard_neighborhood_has_context_intro(self):
        """Output includes the introductory sentence."""
        formatter = ContextFormatter()
        result = formatter.format(_make_standard_neighborhood())
        assert "The program you are documenting exists in this context:" in result

    def test_standard_neighborhood_has_footer(self):
        """Output ends with the advisory footer."""
        formatter = ContextFormatter()
        result = formatter.format(_make_standard_neighborhood())
        assert result.endswith(
            "_This context is auto-generated from the knowledge graph. "
            "Use it to inform your documentation but verify against "
            "the source code._"
        )

    def test_standard_neighborhood_contains_categories(self):
        """Output contains all expected relationship categories."""
        formatter = ContextFormatter()
        result = formatter.format(_make_standard_neighborhood())
        assert "CALLS OUT TO:" in result
        assert "CALLED BY:" in result
        assert "READS FROM:" in result
        assert "WRITES TO:" in result
        assert "INCLUDES:" in result
        assert "EXECUTED BY:" in result

    def test_standard_neighborhood_contains_qualified_names(self):
        """Output lists entities with their qualified names."""
        formatter = ContextFormatter()
        result = formatter.format(_make_standard_neighborhood())
        assert "PROGRAM:ACCT0200" in result
        assert "PROGRAM:ACCT0300" in result
        assert "DATASET:ACCT.MASTER.FILE" in result
        assert "DATASET:ACCT.REPORT.FILE" in result
        assert "COPYBOOK:ACCTCPY1" in result
        assert "JCL_JOB:ACCTJOB1" in result

    def test_entity_lines_indented(self):
        """Entity lines use two-space indent with dash."""
        formatter = ContextFormatter()
        result = formatter.format(_make_standard_neighborhood())
        assert "  - PROGRAM:ACCT0200" in result

    def test_summarization_triggered_by_small_budget(self):
        """When token budget is very small, output is summarized."""
        formatter = ContextFormatter(max_tokens=10)
        result = formatter.format(_make_standard_neighborhood())
        # Summarized format shows counts
        assert "1 programs" in result or "1 datasets" in result
        # Should still have header and footer
        assert "## System Context" in result
        assert "auto-generated" in result


class TestContextFormatterForChallenger:
    """Tests for ContextFormatter.format_for_challenger()."""

    def test_none_returns_empty(self):
        """Challenger format with None returns empty string."""
        formatter = ContextFormatter()
        assert formatter.format_for_challenger(None) == ""

    def test_empty_triples_returns_empty(self):
        """Challenger format with no triples returns empty string."""
        neighborhood = Neighborhood(
            target=_TARGET,
            entities=[_TARGET],
            triples=[],
        )
        formatter = ContextFormatter()
        assert formatter.format_for_challenger(neighborhood) == ""

    def test_includes_cross_check_hints_header(self):
        """Challenger output includes Cross-Check Hints section."""
        formatter = ContextFormatter()
        result = formatter.format_for_challenger(_make_standard_neighborhood())
        assert "### Cross-Check Hints" in result

    def test_includes_expected_datasets(self):
        """Challenger output lists expected datasets."""
        formatter = ContextFormatter()
        result = formatter.format_for_challenger(_make_standard_neighborhood())
        assert "Expected datasets:" in result
        assert "ACCT.MASTER.FILE" in result
        assert "ACCT.REPORT.FILE" in result

    def test_includes_expected_calls(self):
        """Challenger output lists expected calls."""
        formatter = ContextFormatter()
        result = formatter.format_for_challenger(_make_standard_neighborhood())
        assert "Expected calls:" in result
        assert "ACCT0200" in result

    def test_includes_shared_copybooks(self):
        """Challenger output lists shared copybooks."""
        formatter = ContextFormatter()
        result = formatter.format_for_challenger(_make_standard_neighborhood())
        assert "Shared copybooks:" in result
        assert "ACCTCPY1" in result

    def test_base_context_included(self):
        """Challenger output includes the base format() output."""
        formatter = ContextFormatter()
        result = formatter.format_for_challenger(_make_standard_neighborhood())
        assert "## System Context (from knowledge graph)" in result
        assert "CALLS OUT TO:" in result

    def test_no_hints_when_no_matching_categories(self):
        """When only CALLS triples exist, no datasets or copybooks in hints."""
        neighborhood = Neighborhood(
            target=_TARGET,
            entities=[_TARGET, _CALLED_PROG],
            triples=[_make_triple(1, RelationType.CALLS, 2)],
        )
        formatter = ContextFormatter()
        result = formatter.format_for_challenger(neighborhood)
        assert "Expected calls:" in result
        assert "Expected datasets:" not in result
        assert "Shared copybooks:" not in result


class TestGroupTriplesByRelation:
    """Tests for ContextFormatter._group_triples_by_relation()."""

    def test_calls_outbound(self):
        """Target as subject of CALLS -> 'CALLS OUT TO'."""
        neighborhood = Neighborhood(
            target=_TARGET,
            entities=[_TARGET, _CALLED_PROG],
            triples=[_make_triple(1, RelationType.CALLS, 2)],
        )
        formatter = ContextFormatter()
        groups = formatter._group_triples_by_relation(neighborhood)
        assert "CALLS OUT TO" in groups
        assert len(groups["CALLS OUT TO"]) == 1
        assert groups["CALLS OUT TO"][0][1].name == "ACCT0200"

    def test_calls_inbound(self):
        """Target as object of CALLS -> 'CALLED BY'."""
        neighborhood = Neighborhood(
            target=_TARGET,
            entities=[_TARGET, _CALLER_PROG],
            triples=[_make_triple(3, RelationType.CALLS, 1)],
        )
        formatter = ContextFormatter()
        groups = formatter._group_triples_by_relation(neighborhood)
        assert "CALLED BY" in groups
        assert groups["CALLED BY"][0][1].name == "ACCT0300"

    def test_reads(self):
        """Target READS -> 'READS FROM'."""
        neighborhood = Neighborhood(
            target=_TARGET,
            entities=[_TARGET, _DATASET_IN],
            triples=[_make_triple(1, RelationType.READS, 4)],
        )
        formatter = ContextFormatter()
        groups = formatter._group_triples_by_relation(neighborhood)
        assert "READS FROM" in groups

    def test_writes(self):
        """Target WRITES -> 'WRITES TO'."""
        neighborhood = Neighborhood(
            target=_TARGET,
            entities=[_TARGET, _DATASET_OUT],
            triples=[_make_triple(1, RelationType.WRITES, 5)],
        )
        formatter = ContextFormatter()
        groups = formatter._group_triples_by_relation(neighborhood)
        assert "WRITES TO" in groups

    def test_includes(self):
        """Target INCLUDES -> 'INCLUDES'."""
        neighborhood = Neighborhood(
            target=_TARGET,
            entities=[_TARGET, _COPYBOOK],
            triples=[_make_triple(1, RelationType.INCLUDES, 6)],
        )
        formatter = ContextFormatter()
        groups = formatter._group_triples_by_relation(neighborhood)
        assert "INCLUDES" in groups

    def test_executed_by(self):
        """Target as object of EXECUTES -> 'EXECUTED BY'."""
        neighborhood = Neighborhood(
            target=_TARGET,
            entities=[_TARGET, _JCL_JOB],
            triples=[_make_triple(7, RelationType.EXECUTES, 1)],
        )
        formatter = ContextFormatter()
        groups = formatter._group_triples_by_relation(neighborhood)
        assert "EXECUTED BY" in groups

    def test_queries(self):
        """Target QUERIES -> 'QUERIES'."""
        neighborhood = Neighborhood(
            target=_TARGET,
            entities=[_TARGET, _DB_TABLE],
            triples=[_make_triple(1, RelationType.QUERIES, 8)],
        )
        formatter = ContextFormatter()
        groups = formatter._group_triples_by_relation(neighborhood)
        assert "QUERIES" in groups

    def test_modifies(self):
        """Target MODIFIES -> 'MODIFIES'."""
        neighborhood = Neighborhood(
            target=_TARGET,
            entities=[_TARGET, _DB_TABLE],
            triples=[_make_triple(1, RelationType.MODIFIES, 8)],
        )
        formatter = ContextFormatter()
        groups = formatter._group_triples_by_relation(neighborhood)
        assert "MODIFIES" in groups

    def test_performs(self):
        """Target PERFORMS -> 'PERFORMS'."""
        paragraph = _make_entity(9, EntityType.PARAGRAPH, "1000-INIT")
        neighborhood = Neighborhood(
            target=_TARGET,
            entities=[_TARGET, paragraph],
            triples=[_make_triple(1, RelationType.PERFORMS, 9)],
        )
        formatter = ContextFormatter()
        groups = formatter._group_triples_by_relation(neighborhood)
        assert "PERFORMS" in groups

    def test_generic_fallback_subject(self):
        """Unmapped predicate with target as subject uses predicate name."""
        neighborhood = Neighborhood(
            target=_TARGET,
            entities=[_TARGET, _DATASET_IN],
            triples=[_make_triple(1, RelationType.DEFINES_INPUT, 4)],
        )
        formatter = ContextFormatter()
        groups = formatter._group_triples_by_relation(neighborhood)
        assert "DEFINES_INPUT" in groups

    def test_generic_fallback_object(self):
        """Unmapped predicate with target as object uses inverse label."""
        neighborhood = Neighborhood(
            target=_TARGET,
            entities=[_TARGET, _CALLED_PROG],
            triples=[_make_triple(2, RelationType.DEFINES_OUTPUT, 1)],
        )
        formatter = ContextFormatter()
        groups = formatter._group_triples_by_relation(neighborhood)
        assert "DEFINES_OUTPUT (inverse)" in groups

    def test_skips_triple_with_missing_entity(self):
        """Triples referencing entities not in the neighborhood are skipped."""
        neighborhood = Neighborhood(
            target=_TARGET,
            entities=[_TARGET],  # no entity with id=99
            triples=[_make_triple(1, RelationType.CALLS, 99)],
        )
        formatter = ContextFormatter()
        groups = formatter._group_triples_by_relation(neighborhood)
        assert len(groups) == 0

    def test_skips_triple_not_involving_target(self):
        """Triples not involving the target entity are skipped."""
        neighborhood = Neighborhood(
            target=_TARGET,
            entities=[_TARGET, _CALLED_PROG, _CALLER_PROG],
            triples=[_make_triple(2, RelationType.CALLS, 3)],
        )
        formatter = ContextFormatter()
        groups = formatter._group_triples_by_relation(neighborhood)
        assert len(groups) == 0


class TestSummarize:
    """Tests for ContextFormatter._summarize()."""

    def test_single_group(self):
        """Summarize a single group with one entry."""
        formatter = ContextFormatter()
        groups = {
            "CALLS OUT TO": [
                (_make_triple(1, RelationType.CALLS, 2), _CALLED_PROG),
            ],
        }
        result = formatter._summarize(groups, max_chars=500)
        assert "CALLS OUT TO: 1 programs (ACCT0200)" in result

    def test_truncation_with_ellipsis(self):
        """When group has more than 3 entries, preview ends with '...'."""
        progs = [
            _make_entity(i, EntityType.PROGRAM, f"PROG{i:04d}")
            for i in range(10, 16)
        ]
        entries = [
            (_make_triple(1, RelationType.CALLS, p.id), p) for p in progs
        ]
        formatter = ContextFormatter()
        groups = {"CALLS OUT TO": entries}
        result = formatter._summarize(groups, max_chars=500)
        assert "6 programs" in result
        assert "PROG0010, PROG0011, PROG0012, ..." in result

    def test_multiple_groups(self):
        """Summarize multiple groups."""
        formatter = ContextFormatter()
        groups = {
            "CALLS OUT TO": [
                (_make_triple(1, RelationType.CALLS, 2), _CALLED_PROG),
            ],
            "READS FROM": [
                (_make_triple(1, RelationType.READS, 4), _DATASET_IN),
            ],
        }
        result = formatter._summarize(groups, max_chars=500)
        assert "CALLS OUT TO:" in result
        assert "READS FROM:" in result


class TestEstimateTokens:
    """Tests for ContextFormatter._estimate_tokens()."""

    def test_empty_string(self):
        """Empty string has 0 tokens."""
        formatter = ContextFormatter()
        assert formatter._estimate_tokens("") == 0

    def test_known_length(self):
        """A string of 20 chars has 5 estimated tokens (20 / 4)."""
        formatter = ContextFormatter()
        assert formatter._estimate_tokens("a" * 20) == 5

    def test_integer_division(self):
        """Token count uses integer division."""
        formatter = ContextFormatter()
        assert formatter._estimate_tokens("abc") == 0  # 3 // 4 == 0
        assert formatter._estimate_tokens("abcd") == 1  # 4 // 4 == 1
