"""Token estimation for prompt construction.

This module provides heuristic-based token estimation for determining
whether files need chunking and how to allocate token budgets.

The estimator uses character-based heuristics rather than actual
tokenization for performance. This is sufficient for chunking decisions
since we use conservative estimates.
"""

from __future__ import annotations

import logging
from dataclasses import dataclass
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from war_rig.agents.scribe import ScribeInput

logger = logging.getLogger(__name__)


@dataclass
class PromptTokenEstimate:
    """Breakdown of estimated tokens for a Scribe prompt.

    Attributes:
        system_prompt: Tokens for the system prompt.
        template_schema: Tokens for the JSON schema example.
        preprocessor_hints: Tokens for structural hints.
        copybook_contents: Tokens for referenced copybooks.
        source_code: Tokens for the source code itself.
        previous_template: Tokens for previous iteration's template.
        questions_and_tickets: Tokens for Challenger questions/Chrome tickets.
        instructions: Tokens for task instructions.
        total: Total estimated tokens.
    """

    system_prompt: int = 0
    template_schema: int = 0
    preprocessor_hints: int = 0
    copybook_contents: int = 0
    source_code: int = 0
    previous_template: int = 0
    questions_and_tickets: int = 0
    instructions: int = 0

    @property
    def total(self) -> int:
        """Total estimated tokens."""
        return (
            self.system_prompt
            + self.template_schema
            + self.preprocessor_hints
            + self.copybook_contents
            + self.source_code
            + self.previous_template
            + self.questions_and_tickets
            + self.instructions
        )

    @property
    def source_code_percentage(self) -> float:
        """Percentage of total tokens used by source code."""
        if self.total == 0:
            return 0.0
        return (self.source_code / self.total) * 100


class TokenEstimator:
    """Estimates token counts for prompt construction.

    Uses character-based heuristics for fast estimation. The estimates
    are intentionally conservative to ensure we don't exceed limits.

    For COBOL code specifically, we use a higher chars-per-token ratio
    because COBOL has verbose keywords and fixed-width formatting.

    Attributes:
        chars_per_token: Default characters per token ratio.
        cobol_chars_per_token: Chars per token for COBOL code.

    Example:
        >>> estimator = TokenEstimator()
        >>> tokens = estimator.estimate_tokens("MOVE FIELD-A TO FIELD-B.")
        >>> print(tokens)
        8
    """

    # Default characters per token (conservative for English text)
    DEFAULT_CHARS_PER_TOKEN: float = 4.0

    # COBOL-specific ratio (more verbose than typical code)
    COBOL_CHARS_PER_TOKEN: float = 3.5

    # Fixed token estimates for known prompt components
    SYSTEM_PROMPT_TOKENS: int = 2000  # Scribe's detailed system prompt
    TEMPLATE_SCHEMA_TOKENS: int = 1500  # JSON schema in system prompt
    INSTRUCTIONS_TOKENS: int = 500  # Task instructions

    def __init__(
        self,
        chars_per_token: float | None = None,
        cobol_chars_per_token: float | None = None,
    ):
        """Initialize the token estimator.

        Args:
            chars_per_token: Override default chars per token ratio.
            cobol_chars_per_token: Override COBOL-specific ratio.
        """
        self.chars_per_token = chars_per_token or self.DEFAULT_CHARS_PER_TOKEN
        self.cobol_chars_per_token = cobol_chars_per_token or self.COBOL_CHARS_PER_TOKEN

    def estimate_tokens(self, text: str, is_cobol: bool = False) -> int:
        """Estimate token count for text.

        Args:
            text: Text to estimate tokens for.
            is_cobol: If True, use COBOL-specific ratio.

        Returns:
            Estimated token count (conservative).
        """
        if not text:
            return 0

        ratio = self.cobol_chars_per_token if is_cobol else self.chars_per_token
        return int(len(text) / ratio)

    def estimate_source_tokens(self, source_code: str) -> int:
        """Estimate tokens for source code.

        Assumes COBOL unless the code clearly indicates otherwise.

        Args:
            source_code: Source code to estimate.

        Returns:
            Estimated token count.
        """
        # Simple heuristic: if it has COBOL-style line numbers or divisions, it's COBOL
        is_cobol = (
            "IDENTIFICATION DIVISION" in source_code.upper()
            or "PROCEDURE DIVISION" in source_code.upper()
            or source_code[:6].strip().isdigit()  # Line numbers in columns 1-6
        )
        return self.estimate_tokens(source_code, is_cobol=is_cobol)

    def estimate_prompt_tokens(self, scribe_input: ScribeInput) -> PromptTokenEstimate:
        """Estimate total tokens for a complete Scribe prompt.

        Breaks down the estimate by component to help identify what's
        consuming the token budget.

        Args:
            scribe_input: The ScribeInput that will be used to build the prompt.

        Returns:
            PromptTokenEstimate with detailed breakdown.

        TODO: Implement this method.
            - Calculate tokens for each prompt component
            - Use the ratios defined in this class
            - Consider preprocessor_result, copybook_contents, previous_template
            - Consider challenger_questions and chrome_tickets
        """
        estimate = PromptTokenEstimate()

        # Fixed components
        estimate.system_prompt = self.SYSTEM_PROMPT_TOKENS
        estimate.template_schema = self.TEMPLATE_SCHEMA_TOKENS
        estimate.instructions = self.INSTRUCTIONS_TOKENS

        # Source code
        estimate.source_code = self.estimate_source_tokens(scribe_input.source_code)

        # Preprocessor hints (JSON serialized)
        if scribe_input.preprocessor_result:
            preprocessor_json = scribe_input.preprocessor_result.model_dump_json()
            estimate.preprocessor_hints = self.estimate_tokens(preprocessor_json)

        # Copybook contents
        if scribe_input.copybook_contents:
            total_copybook_text = "\n".join(scribe_input.copybook_contents.values())
            estimate.copybook_contents = self.estimate_source_tokens(total_copybook_text)

        # Previous template (for updates)
        if scribe_input.previous_template:
            template_json = scribe_input.previous_template.model_dump_json()
            estimate.previous_template = self.estimate_tokens(template_json)

        # Challenger questions and Chrome tickets
        questions_text = ""
        if scribe_input.challenger_questions:
            for q in scribe_input.challenger_questions:
                questions_text += f"{q.question_id}: {q.question}\n"
        if scribe_input.chrome_tickets:
            for t in scribe_input.chrome_tickets:
                questions_text += f"{t.ticket_id}: {t.description}\n"
        estimate.questions_and_tickets = self.estimate_tokens(questions_text)

        return estimate

    def calculate_available_source_tokens(
        self,
        max_total_tokens: int,
        scribe_input: ScribeInput,
    ) -> int:
        """Calculate how many tokens are available for source code.

        Subtracts fixed overhead from the budget to determine how much
        source code can fit.

        Args:
            max_total_tokens: Maximum total tokens allowed.
            scribe_input: The input (used for non-source components).

        Returns:
            Tokens available for source code.

        TODO: Implement this method.
            - Estimate overhead from all non-source components
            - Subtract from max_total_tokens
            - Return available tokens for source code
            - Return 0 if budget is already exceeded by overhead
        """
        estimate = self.estimate_prompt_tokens(scribe_input)

        # Calculate overhead (everything except source code)
        overhead = (
            estimate.system_prompt
            + estimate.template_schema
            + estimate.preprocessor_hints
            + estimate.copybook_contents
            + estimate.previous_template
            + estimate.questions_and_tickets
            + estimate.instructions
        )

        available = max_total_tokens - overhead

        if available < 0:
            logger.warning(
                f"Token budget ({max_total_tokens}) exceeded by overhead alone ({overhead}). "
                f"Source code cannot fit."
            )
            return 0

        return available
