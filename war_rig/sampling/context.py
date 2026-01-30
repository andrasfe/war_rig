"""Sampling context for intelligent source code sampling.

This module defines the SamplingContext dataclass which holds all the
information needed to make intelligent sampling decisions for
CLARIFICATION and CHROME tickets.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from war_rig.beads import TicketType
    from war_rig.models.templates import DocumentationTemplate, FileType
    from war_rig.models.tickets import ChallengerQuestion, ChromeTicket


@dataclass
class SamplingContext:
    """Context for intelligent source code sampling decisions.

    Contains all the information needed by sampling strategies to
    extract relevant portions of source code for CLARIFICATION and
    CHROME tickets.

    Attributes:
        source_code: The complete source code to sample from.
        source_lines: Source code split into lines (cached for efficiency).
        file_name: Name of the source file being processed.
        file_type: Type of the source file (COBOL, JCL, etc.).
        total_lines: Total number of lines in the source.
        max_tokens: Maximum tokens available for the sample.
        estimated_source_tokens: Estimated tokens in the full source.
        ticket_type: Type of ticket being processed (CLARIFICATION, CHROME).
        challenger_questions: Questions from Challenger, if processing CLARIFICATION.
        chrome_tickets: Chrome tickets from Imperator, if processing CHROME.
        previous_template: Previous documentation template with citations.

    Example:
        >>> context = SamplingContext(
        ...     source_code=source,
        ...     source_lines=source.split("\\n"),
        ...     file_name="CBACT04C.cbl",
        ...     file_type=FileType.COBOL,
        ...     total_lines=2500,
        ...     max_tokens=11000,
        ...     estimated_source_tokens=50000,
        ...     ticket_type=TicketType.CLARIFICATION,
        ...     challenger_questions=[question],
        ...     chrome_tickets=None,
        ...     previous_template=template,
        ... )
    """

    source_code: str
    source_lines: list[str]
    file_name: str
    file_type: FileType
    total_lines: int
    max_tokens: int
    estimated_source_tokens: int
    ticket_type: TicketType
    challenger_questions: list[ChallengerQuestion] | None = None
    chrome_tickets: list[ChromeTicket] | None = None
    previous_template: DocumentationTemplate | None = None

    @classmethod
    def create(
        cls,
        source_code: str,
        file_name: str,
        file_type: FileType,
        max_tokens: int,
        estimated_source_tokens: int,
        ticket_type: TicketType,
        challenger_questions: list[ChallengerQuestion] | None = None,
        chrome_tickets: list[ChromeTicket] | None = None,
        previous_template: DocumentationTemplate | None = None,
    ) -> SamplingContext:
        """Factory method to create a SamplingContext.

        Handles splitting source into lines and computing total_lines.

        Args:
            source_code: The complete source code.
            file_name: Name of the source file.
            file_type: Type of source file.
            max_tokens: Maximum tokens for the sample.
            estimated_source_tokens: Estimated tokens in full source.
            ticket_type: Type of ticket being processed.
            challenger_questions: Optional Challenger questions.
            chrome_tickets: Optional Chrome tickets.
            previous_template: Optional previous template.

        Returns:
            Configured SamplingContext instance.
        """
        source_lines = source_code.split("\n")
        return cls(
            source_code=source_code,
            source_lines=source_lines,
            file_name=file_name,
            file_type=file_type,
            total_lines=len(source_lines),
            max_tokens=max_tokens,
            estimated_source_tokens=estimated_source_tokens,
            ticket_type=ticket_type,
            challenger_questions=challenger_questions,
            chrome_tickets=chrome_tickets,
            previous_template=previous_template,
        )
