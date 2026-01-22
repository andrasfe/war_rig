"""Ticket models for agent communication in War Rig.

This module defines the data structures used for communication between
agents during the documentation process:

- ChromeTicket: Issued by Imperator for major issues requiring rework
- ChallengerQuestion: Questions from Challenger to probe Scribe's documentation
- ScribeResponse: Scribe's answers to Challenger's questions

These models facilitate the iterative refinement loop between agents.
"""

from datetime import datetime
from enum import Enum
from uuid import uuid4

from pydantic import BaseModel, ConfigDict, Field

from war_rig.models.templates import LenientIntList


class IssueType(str, Enum):
    """Types of issues that can be identified in documentation.

    Used by Imperator when creating Chrome tickets.
    """

    VAGUE = "VAGUE"  # Description is too generic or unclear
    MISSING = "MISSING"  # Required information is absent
    CONTRADICTORY = "CONTRADICTORY"  # Conflicts with other sections
    UNCITED = "UNCITED"  # Claims without line number citations
    INCOMPLETE = "INCOMPLETE"  # Section started but not finished
    WRONG = "WRONG"  # Factually incorrect based on source code


class IssuePriority(str, Enum):
    """Priority levels for Chrome tickets.

    Determines the urgency of addressing the issue.
    """

    CRITICAL = "CRITICAL"  # Must be fixed before approval
    HIGH = "HIGH"  # Should be fixed, blocks WITNESSED
    MEDIUM = "MEDIUM"  # Should be addressed but not blocking


class QuestionType(str, Enum):
    """Types of questions Challenger can ask.

    Each type serves a different validation purpose.
    """

    CLARIFICATION = "CLARIFICATION"  # "What do you mean by X?"
    VERIFICATION = "VERIFICATION"  # "Line 234 says Y, but you documented Z"
    COMPLETENESS = "COMPLETENESS"  # "What about the error handling at line 456?"
    CHALLENGE = "CHALLENGE"  # "Are you sure this is an input, not an output?"


class QuestionSeverity(str, Enum):
    """Severity levels for Challenger questions.

    Indicates how important the question is for documentation quality.
    """

    BLOCKING = "BLOCKING"  # Must be resolved before approval
    IMPORTANT = "IMPORTANT"  # Should be addressed
    MINOR = "MINOR"  # Nice to have but not critical


class ActionTaken(str, Enum):
    """Actions Scribe can take in response to a question.

    Indicates how the Scribe handled the Challenger's feedback.
    """

    UPDATED = "UPDATED"  # Documentation was modified
    DEFENDED = "DEFENDED"  # Original documentation justified with evidence
    ACKNOWLEDGED = "ACKNOWLEDGED"  # Issue noted but cannot be resolved


def generate_ticket_id() -> str:
    """Generate a unique ticket identifier.

    Returns:
        A unique string identifier for a ticket.
    """
    return f"CHR-{uuid4().hex[:8].upper()}"


def generate_question_id() -> str:
    """Generate a unique question identifier.

    Returns:
        A unique string identifier for a question.
    """
    return f"Q-{uuid4().hex[:8].upper()}"


class ChromeTicket(BaseModel):
    """A ticket issued by Imperator for documentation issues.

    Chrome tickets represent significant problems that the Scribe must
    address before the documentation can be approved. They are more
    formal than Challenger questions and indicate reviewer-level concerns.

    Attributes:
        ticket_id: Unique identifier (auto-generated if not provided)
        program_id: The program being documented
        section: Which template section needs work
        issue_type: Classification of the problem
        description: Specific description of the problem
        guidance: Hint for how to resolve the issue
        priority: Urgency level
        created_at: When the ticket was created
        resolved: Whether the ticket has been addressed
        resolution_notes: Notes on how it was resolved
    """

    ticket_id: str = Field(
        default_factory=generate_ticket_id,
        description="Unique identifier for this ticket",
    )
    program_id: str = Field(
        default="UNKNOWN",
        description="Target program being documented",
    )
    section: str = Field(
        default="general",
        description="Which template section needs work",
    )
    issue_type: IssueType = Field(
        default=IssueType.INCOMPLETE,
        description="Classification of the problem",
    )
    description: str = Field(
        default="Issue identified during review",
        description="Specific description of the problem",
    )
    guidance: str | None = Field(
        default=None,
        description="Hint for resolution",
    )
    priority: IssuePriority = Field(
        default=IssuePriority.HIGH,
        description="Urgency level",
    )
    created_at: datetime = Field(
        default_factory=datetime.utcnow,
        description="When the ticket was created",
    )
    resolved: bool = Field(
        default=False,
        description="Whether the ticket has been addressed",
    )
    resolution_notes: str | None = Field(
        default=None,
        description="Notes on how it was resolved",
    )

    def resolve(self, notes: str) -> None:
        """Mark this ticket as resolved.

        Args:
            notes: Description of how the issue was resolved.
        """
        self.resolved = True
        self.resolution_notes = notes


class ChallengerQuestion(BaseModel):
    """A question from Challenger to probe Scribe's documentation.

    Challenger questions are part of the validation dialogue between
    agents. They help identify gaps, errors, and areas needing clarification
    in the documentation.

    Attributes:
        question_id: Unique identifier (auto-generated if not provided)
        section: Which template section this relates to
        question_type: Classification of the question
        question: The actual question text
        evidence: Line numbers or citations supporting the question
        severity: How important this question is
        iteration: Which iteration this question was asked in
        answered: Whether Scribe has responded
    """

    question_id: str = Field(
        default_factory=generate_question_id,
        description="Unique identifier for this question",
    )
    section: str | None = Field(
        default=None,
        description="Which template section this relates to",
    )
    question_type: QuestionType = Field(
        default=QuestionType.CLARIFICATION,
        description="Classification of the question",
    )
    question: str = Field(
        default="",
        description="The actual question",
    )
    context: str | None = Field(
        default=None,
        description="Additional context for the question",
    )
    evidence: LenientIntList = Field(
        default_factory=list,
        description="Line numbers or citations supporting the question",
    )
    severity: QuestionSeverity = Field(
        default=QuestionSeverity.IMPORTANT,
        description="How important this question is",
    )
    iteration: int = Field(
        default=1,
        ge=1,
        description="Which iteration this question was asked in",
    )
    answered: bool = Field(
        default=False,
        description="Whether Scribe has responded",
    )

    @property
    def is_blocking(self) -> bool:
        """Check if this question blocks documentation approval.

        Returns:
            True if the question must be resolved before approval.
        """
        return self.severity == QuestionSeverity.BLOCKING and not self.answered


class ScribeResponse(BaseModel):
    """Scribe's answer to a Challenger question.

    This model captures how the Scribe addresses feedback from the
    Challenger, including what action was taken and supporting evidence.

    Attributes:
        question_id: Reference to the question being answered (also accepts 'question' alias)
        response: The answer text
        action_taken: What the Scribe did (UPDATED, DEFENDED, ACKNOWLEDGED)
        updated_section: If UPDATED, which section changed
        citation: Supporting evidence (line numbers)
        iteration: Which iteration this response was provided in
        parsing_incomplete: Flag indicating lenient parsing was used due to missing fields
    """

    # Enable alias support for lenient LLM response parsing
    model_config = ConfigDict(populate_by_name=True)

    question_id: str = Field(
        default="UNKNOWN",
        validation_alias="question",
        description="Reference to the question being answered",
    )
    response: str = Field(
        default="",
        description="The answer",
    )
    action_taken: ActionTaken = Field(
        default=ActionTaken.ACKNOWLEDGED,
        description="What the Scribe did in response",
    )
    updated_section: str | None = Field(
        default=None,
        description="If UPDATED, which section changed",
    )
    citation: LenientIntList = Field(
        default_factory=list,
        description="Supporting evidence (line numbers)",
    )
    iteration: int = Field(
        default=1,
        ge=1,
        description="Which iteration this response was provided in",
    )
    parsing_incomplete: bool = Field(
        default=False,
        description="Flag indicating lenient parsing was used due to missing/invalid fields",
    )


class DialogueExchange(BaseModel):
    """A complete question-response exchange between Challenger and Scribe.

    This model pairs a question with its response for easier tracking
    of the validation dialogue.
    """

    question: ChallengerQuestion = Field(
        ...,
        description="The Challenger's question",
    )
    response: ScribeResponse | None = Field(
        default=None,
        description="The Scribe's response (None if not yet answered)",
    )

    @property
    def is_complete(self) -> bool:
        """Check if this exchange has been completed.

        Returns:
            True if the Scribe has responded to the question.
        """
        return self.response is not None


class DialogueRound(BaseModel):
    """A complete round of Challenger-Scribe dialogue.

    Groups all exchanges that occurred in a single iteration.
    """

    iteration: int = Field(
        ...,
        ge=1,
        description="Which iteration this round occurred in",
    )
    exchanges: list[DialogueExchange] = Field(
        default_factory=list,
        description="Question-response pairs from this round",
    )
    timestamp: datetime = Field(
        default_factory=datetime.utcnow,
        description="When this round occurred",
    )

    @property
    def all_questions_answered(self) -> bool:
        """Check if all questions in this round have been answered.

        Returns:
            True if every question has a response.
        """
        return all(exchange.is_complete for exchange in self.exchanges)

    @property
    def blocking_questions_resolved(self) -> bool:
        """Check if all blocking questions have been resolved.

        Returns:
            True if no blocking questions remain unanswered.
        """
        return not any(
            exchange.question.is_blocking and not exchange.is_complete
            for exchange in self.exchanges
        )


class Dialogue(BaseModel):
    """Complete dialogue history between Challenger and Scribe.

    Tracks all rounds of validation dialogue across iterations.
    """

    program_id: str = Field(
        ...,
        description="The program being documented",
    )
    rounds: list[DialogueRound] = Field(
        default_factory=list,
        description="All dialogue rounds",
    )

    @property
    def total_questions(self) -> int:
        """Get the total number of questions asked.

        Returns:
            Count of all questions across all rounds.
        """
        return sum(len(round.exchanges) for round in self.rounds)

    @property
    def total_answers(self) -> int:
        """Get the total number of answers provided.

        Returns:
            Count of all answered questions across all rounds.
        """
        return sum(
            1
            for round in self.rounds
            for exchange in round.exchanges
            if exchange.is_complete
        )

    def add_round(self, questions: list[ChallengerQuestion]) -> DialogueRound:
        """Add a new round of questions.

        Args:
            questions: The Challenger's questions for this round.

        Returns:
            The newly created DialogueRound.
        """
        iteration = len(self.rounds) + 1
        round = DialogueRound(
            iteration=iteration,
            exchanges=[
                DialogueExchange(question=q)
                for q in questions
            ],
        )
        self.rounds.append(round)
        return round

    def add_responses(
        self,
        responses: list[ScribeResponse],
        iteration: int,
    ) -> None:
        """Add Scribe's responses to a dialogue round.

        Args:
            responses: The Scribe's responses.
            iteration: Which iteration to add responses to.

        Raises:
            ValueError: If the iteration doesn't exist or response
                doesn't match a question.
        """
        if iteration < 1 or iteration > len(self.rounds):
            raise ValueError(f"Invalid iteration: {iteration}")

        round = self.rounds[iteration - 1]
        response_map = {r.question_id: r for r in responses}

        for exchange in round.exchanges:
            if exchange.question.question_id in response_map:
                exchange.response = response_map[exchange.question.question_id]
                exchange.question.answered = True


# =============================================================================
# Imperator Feedback Models
# =============================================================================


class QualityNoteCategory(str, Enum):
    """Categories of quality issues identified by Imperator."""

    EMPTY_SECTION = "empty_section"  # Section is empty or has only placeholders
    MISSING_CITATION = "missing_citation"  # Claims without line number citations
    VAGUE_CONTENT = "vague_content"  # Content is too generic or unclear
    NO_CROSS_REFERENCE = "no_cross_reference"  # Missing program cross-references
    CONFIDENCE_MISMATCH = "confidence_mismatch"  # Confidence doesn't match quality
    REDUNDANT_DOC = "redundant_doc"  # Duplicate documentation detected
    OTHER = "other"  # Other quality issues


class QualityNoteSeverity(str, Enum):
    """Severity levels for quality notes."""

    CRITICAL = "critical"  # Must be fixed - blocks approval
    HIGH = "high"  # Should be fixed before next review
    MEDIUM = "medium"  # Should be addressed
    LOW = "low"  # Nice to have


class QualityNote(BaseModel):
    """A quality observation from Imperator holistic review.

    Quality notes identify systemic issues across documentation that
    should be addressed in subsequent cycles.
    """

    model_config = ConfigDict(extra="ignore")

    note_id: str = Field(
        default_factory=lambda: f"QN-{uuid4().hex[:8].upper()}",
        description="Unique identifier for the note",
    )
    category: str = Field(
        description="Category: empty_section, missing_citation, vague_content, "
        "no_cross_reference, confidence_mismatch, redundant_doc, other",
    )
    severity: str = Field(
        default="medium",
        description="Severity: critical, high, medium, low",
    )
    description: str = Field(
        description="Human-readable description of the quality issue",
    )
    affected_sections: list[str] = Field(
        default_factory=list,
        description="Template sections affected (e.g., 'inputs', 'outputs', 'data_flow')",
    )
    affected_files: list[str] = Field(
        default_factory=list,
        description="Files affected (empty = all files)",
    )
    guidance: str | None = Field(
        default=None,
        description="Specific guidance for addressing the issue",
    )
    cycle_identified: int = Field(
        default=1,
        description="Cycle when this note was first identified",
    )


class FeedbackContext(BaseModel):
    """Aggregated feedback context for a documentation cycle.

    Contains all Imperator feedback that should be applied globally
    to documentation and validation tickets.
    """

    model_config = ConfigDict(extra="ignore")

    quality_notes: list[QualityNote] = Field(
        default_factory=list,
        description="Quality observations from Imperator",
    )
    critical_sections: list[str] = Field(
        default_factory=list,
        description="Sections that MUST be populated (not empty)",
    )
    required_citations: bool = Field(
        default=True,
        description="Whether citations are required for all claims",
    )
    cross_reference_required: bool = Field(
        default=False,
        description="Whether cross-program references must be documented",
    )
    previous_cycle_issues: dict[str, list[str]] = Field(
        default_factory=dict,
        description="File-specific issues from previous cycle {file_name: [issues]}",
    )
    augment_existing: bool = Field(
        default=True,
        description="Always augment existing docs instead of creating new",
    )

    def get_notes_for_file(self, file_name: str) -> list[QualityNote]:
        """Get quality notes applicable to a specific file.

        Args:
            file_name: The file to get notes for.

        Returns:
            List of notes that apply to this file (global + file-specific).
        """
        applicable = []
        for note in self.quality_notes:
            # Global notes (no specific files) apply to all
            if not note.affected_files:
                applicable.append(note)
            # File-specific notes
            elif file_name in note.affected_files:
                applicable.append(note)
        return applicable

    def get_critical_notes(self) -> list[QualityNote]:
        """Get all critical severity notes.

        Returns:
            List of notes with critical severity.
        """
        return [n for n in self.quality_notes if n.severity == "critical"]
