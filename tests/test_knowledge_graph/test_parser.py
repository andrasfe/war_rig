"""Unit tests for TripleOutputParser.

Tests:
- Parse valid triples block from Scribe output
- No triples block in output returns []
- Invalid line format logged as warning, skipped
- Unknown entity type logged as warning, returns None
- Unknown relation type logged as warning, returns None
- Case-insensitive type matching
- Empty block returns []
- Multiple valid triples parsed correctly
- source_pass and source_artifact forwarded to RawTriple
"""

import logging

from war_rig.knowledge_graph.models import EntityType, RelationType
from war_rig.knowledge_graph.parser import TripleOutputParser


class TestParseValidTriples:
    """Tests for parsing valid triples blocks."""

    def test_single_valid_triple(self):
        output = (
            "Some documentation text.\n"
            "```triples\n"
            "PROGRAM:ACCT0100 | CALLS | PROGRAM:ACCT0200\n"
            "```\n"
        )
        parser = TripleOutputParser()
        triples = parser.parse(output)

        assert len(triples) == 1
        assert triples[0].subject_type == EntityType.PROGRAM
        assert triples[0].subject_name == "ACCT0100"
        assert triples[0].predicate == RelationType.CALLS
        assert triples[0].object_type == EntityType.PROGRAM
        assert triples[0].object_name == "ACCT0200"

    def test_multiple_valid_triples(self):
        output = (
            "```triples\n"
            "PROGRAM:ACCT0100 | CALLS | PROGRAM:ACCT0200\n"
            "PROGRAM:ACCT0100 | READS | DATASET:INPUT.FILE\n"
            "PROGRAM:ACCT0100 | INCLUDES | COPYBOOK:ACCTCPY1\n"
            "JCL_JOB:ACCTJOB1 | CONTAINS_STEP | JCL_STEP:STEP01\n"
            "JCL_STEP:STEP01 | EXECUTES | PROGRAM:ACCT0100\n"
            "```\n"
        )
        parser = TripleOutputParser()
        triples = parser.parse(output)

        assert len(triples) == 5
        predicates = [t.predicate for t in triples]
        assert RelationType.CALLS in predicates
        assert RelationType.READS in predicates
        assert RelationType.INCLUDES in predicates
        assert RelationType.CONTAINS_STEP in predicates
        assert RelationType.EXECUTES in predicates

    def test_all_entity_types(self):
        output = (
            "```triples\n"
            "PROGRAM:PGM1 | CALLS | PROGRAM:PGM2\n"
            "JCL_JOB:JOB1 | CONTAINS_STEP | JCL_STEP:STEP1\n"
            "PROGRAM:PGM1 | READS | DATASET:DS1\n"
            "PROGRAM:PGM1 | INCLUDES | COPYBOOK:CPY1\n"
            "COPYBOOK:CPY1 | DEFINES_FIELD | FIELD:FLD1\n"
            "PARAGRAPH:PARA1 | PERFORMS | PARAGRAPH:PARA2\n"
            "PROGRAM:PGM1 | QUERIES | DB_TABLE:TBL1\n"
            "CUSTOM:THING1 | RELATED_TO | CUSTOM:THING2\n"
            "```\n"
        )
        parser = TripleOutputParser()
        triples = parser.parse(output)
        assert len(triples) == 8

    def test_all_relation_types(self):
        output = (
            "```triples\n"
            "PROGRAM:A | CALLS | PROGRAM:B\n"
            "PROGRAM:A | READS | DATASET:D\n"
            "PROGRAM:A | WRITES | DATASET:D\n"
            "PROGRAM:A | INCLUDES | COPYBOOK:C\n"
            "JCL_STEP:S | EXECUTES | PROGRAM:A\n"
            "JCL_STEP:S | DEFINES_INPUT | DATASET:D\n"
            "JCL_STEP:S | DEFINES_OUTPUT | DATASET:D\n"
            "JCL_JOB:J | CONTAINS_STEP | JCL_STEP:S\n"
            "COPYBOOK:C | DEFINES_FIELD | FIELD:F\n"
            "PROGRAM:A | QUERIES | DB_TABLE:T\n"
            "PROGRAM:A | MODIFIES | DB_TABLE:T\n"
            "PARAGRAPH:P1 | PERFORMS | PARAGRAPH:P2\n"
            "CUSTOM:X | RELATED_TO | CUSTOM:Y\n"
            "```\n"
        )
        parser = TripleOutputParser()
        triples = parser.parse(output)
        assert len(triples) == 13
        predicates = {t.predicate for t in triples}
        assert predicates == set(RelationType)


class TestParseNoTriplesBlock:
    """Tests for output without triples block."""

    def test_no_block_returns_empty(self):
        output = "This is just regular documentation output with no triples."
        parser = TripleOutputParser()
        triples = parser.parse(output)
        assert triples == []

    def test_other_code_block_not_matched(self):
        output = (
            "```python\n"
            "print('hello')\n"
            "```\n"
        )
        parser = TripleOutputParser()
        triples = parser.parse(output)
        assert triples == []

    def test_empty_output(self):
        parser = TripleOutputParser()
        triples = parser.parse("")
        assert triples == []


class TestParseEmptyBlock:
    """Tests for empty triples blocks."""

    def test_empty_block_returns_empty(self):
        output = (
            "```triples\n"
            "```\n"
        )
        parser = TripleOutputParser()
        triples = parser.parse(output)
        assert triples == []

    def test_whitespace_only_block_returns_empty(self):
        output = (
            "```triples\n"
            "  \n"
            "\n"
            "  \n"
            "```\n"
        )
        parser = TripleOutputParser()
        triples = parser.parse(output)
        assert triples == []


class TestParseInvalidLines:
    """Tests for invalid line formats."""

    def test_invalid_format_skipped_with_warning(self, caplog):
        output = (
            "```triples\n"
            "PROGRAM:ACCT0100 | CALLS | PROGRAM:ACCT0200\n"
            "this is not a valid triple line\n"
            "PROGRAM:ACCT0100 | READS | DATASET:INPUT.FILE\n"
            "```\n"
        )
        parser = TripleOutputParser()
        with caplog.at_level(logging.WARNING):
            triples = parser.parse(output)

        assert len(triples) == 2
        assert any("does not match expected format" in r.message for r in caplog.records)

    def test_missing_pipe_separator(self, caplog):
        output = (
            "```triples\n"
            "PROGRAM:ACCT0100 CALLS PROGRAM:ACCT0200\n"
            "```\n"
        )
        parser = TripleOutputParser()
        with caplog.at_level(logging.WARNING):
            triples = parser.parse(output)

        assert len(triples) == 0
        assert any("does not match expected format" in r.message for r in caplog.records)

    def test_missing_type_prefix(self, caplog):
        output = (
            "```triples\n"
            "ACCT0100 | CALLS | ACCT0200\n"
            "```\n"
        )
        parser = TripleOutputParser()
        with caplog.at_level(logging.WARNING):
            triples = parser.parse(output)

        assert len(triples) == 0


class TestParseUnknownTypes:
    """Tests for unknown entity and relation types."""

    def test_unknown_subject_entity_type(self, caplog):
        output = (
            "```triples\n"
            "FOOBAR:ACCT0100 | CALLS | PROGRAM:ACCT0200\n"
            "```\n"
        )
        parser = TripleOutputParser()
        with caplog.at_level(logging.WARNING):
            triples = parser.parse(output)

        assert len(triples) == 0
        assert any("Unknown subject entity type" in r.message for r in caplog.records)

    def test_unknown_object_entity_type(self, caplog):
        output = (
            "```triples\n"
            "PROGRAM:ACCT0100 | CALLS | FOOBAR:ACCT0200\n"
            "```\n"
        )
        parser = TripleOutputParser()
        with caplog.at_level(logging.WARNING):
            triples = parser.parse(output)

        assert len(triples) == 0
        assert any("Unknown object entity type" in r.message for r in caplog.records)

    def test_unknown_relation_type(self, caplog):
        output = (
            "```triples\n"
            "PROGRAM:ACCT0100 | FOOBAR | PROGRAM:ACCT0200\n"
            "```\n"
        )
        parser = TripleOutputParser()
        with caplog.at_level(logging.WARNING):
            triples = parser.parse(output)

        assert len(triples) == 0
        assert any("Unknown relation type" in r.message for r in caplog.records)


class TestCaseInsensitiveMatching:
    """Tests for case-insensitive type matching."""

    def test_lowercase_entity_types(self):
        output = (
            "```triples\n"
            "program:ACCT0100 | CALLS | program:ACCT0200\n"
            "```\n"
        )
        parser = TripleOutputParser()
        triples = parser.parse(output)

        assert len(triples) == 1
        assert triples[0].subject_type == EntityType.PROGRAM
        assert triples[0].object_type == EntityType.PROGRAM

    def test_lowercase_relation_type(self):
        output = (
            "```triples\n"
            "PROGRAM:ACCT0100 | calls | PROGRAM:ACCT0200\n"
            "```\n"
        )
        parser = TripleOutputParser()
        triples = parser.parse(output)

        assert len(triples) == 1
        assert triples[0].predicate == RelationType.CALLS

    def test_mixed_case(self):
        output = (
            "```triples\n"
            "Program:ACCT0100 | Calls | Dataset:INPUT.FILE\n"
            "```\n"
        )
        parser = TripleOutputParser()
        triples = parser.parse(output)

        assert len(triples) == 1
        assert triples[0].subject_type == EntityType.PROGRAM
        assert triples[0].predicate == RelationType.CALLS
        assert triples[0].object_type == EntityType.DATASET


class TestProvenance:
    """Tests for source_pass and source_artifact forwarding."""

    def test_source_pass_forwarded(self):
        output = (
            "```triples\n"
            "PROGRAM:ACCT0100 | CALLS | PROGRAM:ACCT0200\n"
            "```\n"
        )
        parser = TripleOutputParser()
        triples = parser.parse(output, source_pass="pass_2")

        assert triples[0].source_pass == "pass_2"

    def test_source_artifact_forwarded(self):
        output = (
            "```triples\n"
            "PROGRAM:ACCT0100 | CALLS | PROGRAM:ACCT0200\n"
            "```\n"
        )
        parser = TripleOutputParser()
        triples = parser.parse(output, source_artifact="ACCT0100.cbl")

        assert triples[0].source_artifact == "ACCT0100.cbl"

    def test_both_provenance_fields(self):
        output = (
            "```triples\n"
            "PROGRAM:ACCT0100 | CALLS | PROGRAM:ACCT0200\n"
            "PROGRAM:ACCT0100 | READS | DATASET:FILE1\n"
            "```\n"
        )
        parser = TripleOutputParser()
        triples = parser.parse(
            output, source_pass="pass_1", source_artifact="ACCT0100.cbl"
        )

        assert len(triples) == 2
        for t in triples:
            assert t.source_pass == "pass_1"
            assert t.source_artifact == "ACCT0100.cbl"

    def test_none_provenance_defaults(self):
        output = (
            "```triples\n"
            "PROGRAM:ACCT0100 | CALLS | PROGRAM:ACCT0200\n"
            "```\n"
        )
        parser = TripleOutputParser()
        triples = parser.parse(output)

        assert triples[0].source_pass is None
        assert triples[0].source_artifact is None


class TestParseLineParsing:
    """Tests for _parse_line internal method."""

    def test_valid_line(self):
        parser = TripleOutputParser()
        result = parser._parse_line("PROGRAM:ACCT0100 | CALLS | PROGRAM:ACCT0200")
        assert result is not None
        assert result.subject_name == "ACCT0100"
        assert result.object_name == "ACCT0200"

    def test_invalid_line_returns_none(self):
        parser = TripleOutputParser()
        result = parser._parse_line("not a triple")
        assert result is None

    def test_line_with_leading_trailing_whitespace(self):
        parser = TripleOutputParser()
        result = parser._parse_line("  PROGRAM:ACCT0100 | CALLS | PROGRAM:ACCT0200  ")
        assert result is not None

    def test_provenance_forwarded_in_parse_line(self):
        parser = TripleOutputParser()
        result = parser._parse_line(
            "PROGRAM:ACCT0100 | CALLS | PROGRAM:ACCT0200",
            source_pass="pass_3",
            source_artifact="test.cbl",
        )
        assert result is not None
        assert result.source_pass == "pass_3"
        assert result.source_artifact == "test.cbl"
