"""Relevance analyzer for extracting sampling hints from questions.

This module provides the RelevanceAnalyzer class which extracts hints
from Challenger questions and Chrome tickets to guide intelligent
source code sampling.
"""

from __future__ import annotations

import logging
import re
from dataclasses import dataclass, field
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from war_rig.sampling.context import SamplingContext

logger = logging.getLogger(__name__)


@dataclass
class RelevanceHints:
    """Hints extracted from questions/tickets to guide sampling.

    Contains various types of hints that can be used by sampling strategies
    to identify relevant portions of source code.

    Attributes:
        line_numbers: Explicit line numbers from question evidence fields.
        line_ranges: Ranges of lines (start, end) from evidence.
        section_names: Template section names referenced in questions.
        section_citations: Map of section names to line numbers from template.
        identifiers: COBOL-style identifiers found in question text.

    Example:
        >>> hints = RelevanceHints(
        ...     line_numbers=[234, 567],
        ...     section_names=["inputs", "data_flow"],
        ...     identifiers=["CUSTOMER-RECORD", "PROCESS-ACCOUNT"],
        ... )
        >>> hints.has_explicit_hints
        True
    """

    line_numbers: list[int] = field(default_factory=list)
    line_ranges: list[tuple[int, int]] = field(default_factory=list)
    section_names: list[str] = field(default_factory=list)
    section_citations: dict[str, list[int]] = field(default_factory=dict)
    identifiers: list[str] = field(default_factory=list)

    @property
    def has_explicit_hints(self) -> bool:
        """Check if any explicit hints were extracted.

        Returns True if there are line numbers, line ranges, section names
        with citations, or identifiers found. This determines whether
        intelligent sampling can be applied or if random fallback is needed.

        Returns:
            True if any hints are present, False otherwise.
        """
        return bool(
            self.line_numbers
            or self.line_ranges
            or self.section_citations
            or self.identifiers
        )

    @property
    def all_cited_lines(self) -> list[int]:
        """Get all line numbers from all hint sources.

        Combines line_numbers and section_citations into a single
        deduplicated list of line numbers.

        Returns:
            Sorted list of unique line numbers from all sources.
        """
        lines: set[int] = set(self.line_numbers)

        # Add lines from section citations
        for section_lines in self.section_citations.values():
            lines.update(section_lines)

        return sorted(lines)


class RelevanceAnalyzer:
    """Analyzes questions and tickets to extract relevance hints.

    The analyzer examines ChallengerQuestion and ChromeTicket objects to
    extract hints that can guide intelligent source code sampling:

    1. Line numbers from evidence fields
    2. Line number mentions in question text
    3. Template section references
    4. COBOL-style identifiers in question text
    5. Citations from the previous template

    These hints are used by sampling strategies to select the most
    relevant portions of source code.

    Example:
        >>> analyzer = RelevanceAnalyzer()
        >>> hints = analyzer.analyze(context)
        >>> print(f"Found {len(hints.line_numbers)} line citations")
    """

    # Pattern to match line number references in text
    # Matches: "line 234", "lines 234-567", "line 234, 567"
    LINE_NUMBER_PATTERN = re.compile(
        r"lines?\s*(\d+)(?:\s*[-,]\s*(\d+))?",
        re.IGNORECASE,
    )

    # Pattern to match COBOL-style identifiers
    # Matches: WS-CUSTOMER-ID, PROCESS-ACCOUNT, CBACT04C
    # Must be at least 3 characters and start with a letter
    IDENTIFIER_PATTERN = re.compile(
        r"\b([A-Z][A-Z0-9-]{2,})\b",
        re.IGNORECASE,
    )

    # Common English words to exclude from identifier matching
    COMMON_WORDS = frozenset({
        "THE", "AND", "FOR", "NOT", "ARE", "BUT", "FROM", "HAVE", "HAS",
        "THIS", "THAT", "WITH", "WHAT", "WHEN", "WHERE", "WHY", "HOW",
        "WHICH", "DOES", "SHOULD", "WOULD", "COULD", "WILL", "CAN",
        "MAY", "MUST", "SHALL", "LINE", "LINES", "CODE", "FILE",
        "SECTION", "PROGRAM", "DATA", "INPUT", "OUTPUT", "ERROR",
        "MISSING", "VAGUE", "WRONG", "INCOMPLETE", "UNCITED",
        "CLARIFICATION", "VERIFICATION", "COMPLETENESS", "CHALLENGE",
        "YES", "NO", "TRUE", "FALSE", "NULL", "NONE",
    })

    def analyze(self, context: "SamplingContext") -> RelevanceHints:
        """Analyze context to extract relevance hints.

        Examines challenger questions, chrome tickets, and previous template
        to extract all available hints for guiding source sampling.

        Args:
            context: The sampling context containing questions/tickets.

        Returns:
            RelevanceHints with all extracted hint information.
        """
        hints = RelevanceHints()

        # Extract from challenger questions
        if context.challenger_questions:
            for question in context.challenger_questions:
                self._extract_from_question(question, hints, context)

        # Extract from chrome tickets
        if context.chrome_tickets:
            for ticket in context.chrome_tickets:
                self._extract_from_chrome_ticket(ticket, hints, context)

        # Deduplicate and sort
        hints.line_numbers = sorted(set(hints.line_numbers))
        hints.identifiers = sorted(set(hints.identifiers))
        hints.section_names = sorted(set(hints.section_names))

        # Log what we found
        if hints.has_explicit_hints:
            logger.debug(
                f"Extracted hints: {len(hints.line_numbers)} lines, "
                f"{len(hints.identifiers)} identifiers, "
                f"{len(hints.section_names)} sections"
            )
        else:
            logger.debug("No explicit hints found, will use random fallback")

        return hints

    def _extract_from_question(
        self,
        question: "ChallengerQuestion",
        hints: RelevanceHints,
        context: "SamplingContext",
    ) -> None:
        """Extract hints from a ChallengerQuestion.

        Args:
            question: The question to analyze.
            hints: Hints object to populate.
            context: The sampling context for template lookup.
        """
        from war_rig.models.tickets import ChallengerQuestion

        # Extract line numbers from evidence field
        if hasattr(question, "evidence") and question.evidence:
            for line_num in question.evidence:
                if isinstance(line_num, int) and 1 <= line_num <= context.total_lines:
                    hints.line_numbers.append(line_num)

        # Extract section name
        if hasattr(question, "section") and question.section:
            hints.section_names.append(question.section)
            # Look up citations for this section in the template
            self._add_section_citations(question.section, hints, context)

        # Extract line numbers from question text
        if hasattr(question, "question") and question.question:
            self._extract_line_numbers_from_text(question.question, hints, context)
            self._extract_identifiers_from_text(question.question, hints)

        # Extract from context field if present
        if hasattr(question, "context") and question.context:
            self._extract_line_numbers_from_text(question.context, hints, context)
            self._extract_identifiers_from_text(question.context, hints)

    def _extract_from_chrome_ticket(
        self,
        ticket: "ChromeTicket",
        hints: RelevanceHints,
        context: "SamplingContext",
    ) -> None:
        """Extract hints from a ChromeTicket.

        Args:
            ticket: The chrome ticket to analyze.
            hints: Hints object to populate.
            context: The sampling context for template lookup.
        """
        from war_rig.models.tickets import ChromeTicket

        # Extract section name
        if hasattr(ticket, "section") and ticket.section:
            hints.section_names.append(ticket.section)
            self._add_section_citations(ticket.section, hints, context)

        # Extract from description
        if hasattr(ticket, "description") and ticket.description:
            self._extract_line_numbers_from_text(ticket.description, hints, context)
            self._extract_identifiers_from_text(ticket.description, hints)

        # Extract from guidance
        if hasattr(ticket, "guidance") and ticket.guidance:
            self._extract_line_numbers_from_text(ticket.guidance, hints, context)
            self._extract_identifiers_from_text(ticket.guidance, hints)

    def _extract_line_numbers_from_text(
        self,
        text: str,
        hints: RelevanceHints,
        context: "SamplingContext",
    ) -> None:
        """Extract line number references from text.

        Args:
            text: Text to search for line number references.
            hints: Hints object to populate.
            context: Context for validating line numbers.
        """
        for match in self.LINE_NUMBER_PATTERN.finditer(text):
            start_line = int(match.group(1))
            end_line = match.group(2)

            # Validate line number is within source bounds
            if 1 <= start_line <= context.total_lines:
                hints.line_numbers.append(start_line)

            if end_line:
                end_line_int = int(end_line)
                if 1 <= end_line_int <= context.total_lines:
                    # Add as a range
                    hints.line_ranges.append(
                        (min(start_line, end_line_int), max(start_line, end_line_int))
                    )

    def _extract_identifiers_from_text(
        self,
        text: str,
        hints: RelevanceHints,
    ) -> None:
        """Extract COBOL-style identifiers from text.

        Args:
            text: Text to search for identifiers.
            hints: Hints object to populate.
        """
        for match in self.IDENTIFIER_PATTERN.finditer(text):
            identifier = match.group(1).upper()

            # Filter out common English words
            if identifier not in self.COMMON_WORDS:
                hints.identifiers.append(identifier)

    def _add_section_citations(
        self,
        section_name: str,
        hints: RelevanceHints,
        context: "SamplingContext",
    ) -> None:
        """Add line citations from template section.

        Looks up the specified section in the previous template and
        extracts any citation line numbers.

        Args:
            section_name: Name of the template section.
            hints: Hints object to populate.
            context: Context containing the previous template.
        """
        if context.previous_template is None:
            return

        template = context.previous_template
        citations: list[int] = []

        # Map section names to template fields with citations
        section_lower = section_name.lower()

        # Handle purpose section
        if section_lower == "purpose" and hasattr(template, "purpose"):
            purpose = template.purpose
            if purpose and hasattr(purpose, "citations"):
                citations.extend(purpose.citations or [])

        # Handle inputs section
        elif section_lower == "inputs" and hasattr(template, "inputs"):
            for inp in template.inputs or []:
                if hasattr(inp, "citation"):
                    citations.extend(inp.citation or [])

        # Handle outputs section
        elif section_lower == "outputs" and hasattr(template, "outputs"):
            for out in template.outputs or []:
                if hasattr(out, "citation"):
                    citations.extend(out.citation or [])

        # Handle business_rules section
        elif section_lower == "business_rules" and hasattr(template, "business_rules"):
            for rule in template.business_rules or []:
                if hasattr(rule, "citation"):
                    citations.extend(rule.citation or [])

        # Handle data_flow section
        elif section_lower == "data_flow" and hasattr(template, "data_flow"):
            data_flow = template.data_flow
            if data_flow:
                for read in getattr(data_flow, "reads_from", []) or []:
                    if hasattr(read, "citation"):
                        citations.extend(read.citation or [])
                for write in getattr(data_flow, "writes_to", []) or []:
                    if hasattr(write, "citation"):
                        citations.extend(write.citation or [])
                for transform in getattr(data_flow, "transforms", []) or []:
                    if hasattr(transform, "citation"):
                        citations.extend(transform.citation or [])

        # Handle called_programs section
        elif section_lower == "called_programs" and hasattr(template, "called_programs"):
            for called in template.called_programs or []:
                if hasattr(called, "citation") and called.citation:
                    citations.append(called.citation)

        # Handle copybooks_used section
        elif section_lower == "copybooks_used" and hasattr(template, "copybooks_used"):
            for copybook in template.copybooks_used or []:
                if hasattr(copybook, "citation") and copybook.citation:
                    citations.append(copybook.citation)

        # Handle error_handling section
        elif section_lower == "error_handling" and hasattr(template, "error_handling"):
            for handler in template.error_handling or []:
                if hasattr(handler, "citation"):
                    citations.extend(handler.citation or [])

        # Handle sql_operations section
        elif section_lower == "sql_operations" and hasattr(template, "sql_operations"):
            for sql_op in template.sql_operations or []:
                if hasattr(sql_op, "citation") and sql_op.citation:
                    citations.append(sql_op.citation)

        # Handle cics_operations section
        elif section_lower == "cics_operations" and hasattr(template, "cics_operations"):
            for cics_op in template.cics_operations or []:
                if hasattr(cics_op, "citation") and cics_op.citation:
                    citations.append(cics_op.citation)

        # Handle paragraphs section
        elif section_lower == "paragraphs" and hasattr(template, "paragraphs"):
            for para in template.paragraphs or []:
                if hasattr(para, "citation") and para.citation:
                    # Citation is tuple (start, end)
                    if isinstance(para.citation, tuple) and len(para.citation) == 2:
                        citations.extend(range(para.citation[0], para.citation[1] + 1))

        # Filter valid citations and store
        valid_citations = [
            c for c in citations
            if isinstance(c, int) and 1 <= c <= context.total_lines
        ]

        if valid_citations:
            hints.section_citations[section_name] = sorted(set(valid_citations))
