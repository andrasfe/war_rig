"""LLM triple output parser for Scribe-emitted triples.

This module parses the structured ```triples``` block from Scribe output,
validates each triple against the known schema, and returns RawTriple objects
ready for ingestion.

The expected format (per spec Section 5.1):
    ```triples
    SUBJECT_TYPE:subject_name | PREDICATE | OBJECT_TYPE:object_name
    ```

Triples referencing unknown entity or relationship types are logged as
warnings rather than silently dropped, since they may indicate schema gaps.

Example:
    parser = TripleOutputParser()
    raw_triples = parser.parse(scribe_output, source_pass="pass_1", source_artifact="ACCT0100.cbl")
"""

import logging
import re

from war_rig.knowledge_graph.models import EntityType, RawTriple, RelationType

logger = logging.getLogger(__name__)

# Regex to extract the ```triples``` fenced block from Scribe output
_TRIPLES_BLOCK_PATTERN = re.compile(
    r"```triples\s*\n(.*?)```",
    re.DOTALL,
)

# Regex to parse a single triple line: TYPE:name | PREDICATE | TYPE:name
_TRIPLE_LINE_PATTERN = re.compile(
    r"^\s*(\w+):(\S+)\s*\|\s*(\w+)\s*\|\s*(\w+):(\S+)\s*$",
)


class TripleOutputParser:
    """Parses Scribe LLM output to extract structured triples.

    Validates each triple against the known EntityType and RelationType
    enums. Invalid triples are logged as warnings, not silently dropped.
    """

    def parse(
        self,
        scribe_output: str,
        source_pass: str | None = None,
        source_artifact: str | None = None,
    ) -> list[RawTriple]:
        """Parse triples from Scribe output text.

        Extracts the ```triples``` fenced block and parses each line
        into a RawTriple. Lines that don't match the expected format
        or reference unknown types are logged and skipped.

        Args:
            scribe_output: Full text output from the Scribe agent.
            source_pass: Which War Rig pass produced this output.
            source_artifact: Source file the Scribe was analyzing.

        Returns:
            List of valid RawTriple objects parsed from the output.
        """
        match = _TRIPLES_BLOCK_PATTERN.search(scribe_output)
        if match is None:
            return []

        block_content = match.group(1)
        triples: list[RawTriple] = []

        for line in block_content.split("\n"):
            stripped = line.strip()
            if not stripped:
                continue
            parsed = self._parse_line(stripped, source_pass, source_artifact)
            if parsed is not None:
                triples.append(parsed)

        logger.info("Parsed %d triples from Scribe output", len(triples))
        return triples

    def _parse_line(
        self,
        line: str,
        source_pass: str | None = None,
        source_artifact: str | None = None,
    ) -> RawTriple | None:
        """Parse a single triple line.

        Expected format: SUBJECT_TYPE:subject_name | PREDICATE | OBJECT_TYPE:object_name

        Args:
            line: A single line from the triples block.
            source_pass: Pass identifier for provenance.
            source_artifact: Source artifact for provenance.

        Returns:
            RawTriple if valid, None if the line is malformed or uses
            unknown types (warning logged).
        """
        match = _TRIPLE_LINE_PATTERN.match(line)
        if match is None:
            logger.warning("Triple line does not match expected format: %s", line)
            return None

        subject_type_str, subject_name, predicate_str, object_type_str, object_name = (
            match.groups()
        )

        subject_type = self._validate_entity_type(subject_type_str)
        if subject_type is None:
            logger.warning("Unknown subject entity type '%s' in line: %s", subject_type_str, line)
            return None

        predicate = self._validate_relation_type(predicate_str)
        if predicate is None:
            logger.warning("Unknown relation type '%s' in line: %s", predicate_str, line)
            return None

        object_type = self._validate_entity_type(object_type_str)
        if object_type is None:
            logger.warning("Unknown object entity type '%s' in line: %s", object_type_str, line)
            return None

        return RawTriple(
            subject_type=subject_type,
            subject_name=subject_name,
            predicate=predicate,
            object_type=object_type,
            object_name=object_name,
            source_pass=source_pass,
            source_artifact=source_artifact,
        )

    def _validate_entity_type(self, type_str: str) -> EntityType | None:
        """Validate and convert a string to EntityType.

        Args:
            type_str: String to validate (e.g., "PROGRAM", "DATASET").

        Returns:
            EntityType if valid, None otherwise.
        """
        try:
            return EntityType(type_str.upper())
        except ValueError:
            return None

    def _validate_relation_type(self, type_str: str) -> RelationType | None:
        """Validate and convert a string to RelationType.

        Args:
            type_str: String to validate (e.g., "CALLS", "READS").

        Returns:
            RelationType if valid, None otherwise.
        """
        try:
            return RelationType(type_str.upper())
        except ValueError:
            return None
