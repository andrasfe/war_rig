"""Challenger agent for documentation validation.

The Challenger is the validator War Boy. It reviews the Scribe's documentation,
asks probing questions, and identifies gaps or errors.

Responsibilities:
- Review each template section for accuracy
- Cross-check claims against source code
- Ask clarifying questions about vague entries
- Identify contradictions between sections
- Verify that citations are accurate
"""

import json
import logging
import re
from typing import Any

from pydantic import Field

from war_rig.agents.base import AgentInput, AgentOutput, BaseAgent
from war_rig.config import APIConfig, ChallengerConfig
from war_rig.models.assessments import (
    ChallengerAssessment,
    SectionAssessment,
    ValidationLevel,
)
from war_rig.models.templates import DocumentationTemplate, FileType
from war_rig.models.tickets import (
    ChallengerQuestion,
    QuestionSeverity,
    QuestionType,
    ScribeResponse,
)
from war_rig.preprocessors.base import PreprocessorResult

logger = logging.getLogger(__name__)


class ChallengerInput(AgentInput):
    """Input data for the Challenger agent.

    Contains the Scribe's documentation and source code for validation.
    """

    template: DocumentationTemplate = Field(
        ...,
        description="Scribe's documentation to validate",
    )
    source_code: str = Field(
        ...,
        description="Original source code",
    )
    file_name: str = Field(
        ...,
        description="Name of the source file",
    )
    file_type: FileType = Field(
        ...,
        description="Type of source file",
    )
    preprocessor_result: PreprocessorResult | None = Field(
        default=None,
        description="Structural hints from preprocessor",
    )
    previous_questions: list[ChallengerQuestion] = Field(
        default_factory=list,
        description="Questions from previous rounds",
    )
    scribe_responses: list[ScribeResponse] = Field(
        default_factory=list,
        description="Scribe's responses to previous questions",
    )
    max_questions: int = Field(
        default=5,
        ge=1,
        le=20,
        description="Maximum questions to ask this round",
    )


class ChallengerOutput(AgentOutput):
    """Output from the Challenger agent.

    Contains questions for the Scribe and validation assessment.
    """

    questions: list[ChallengerQuestion] = Field(
        default_factory=list,
        description="Questions for the Scribe",
    )
    assessment: ChallengerAssessment | None = Field(
        default=None,
        description="Validation assessment",
    )
    issues_found: list[str] = Field(
        default_factory=list,
        description="Potential issues identified",
    )
    suggested_corrections: list[dict[str, Any]] = Field(
        default_factory=list,
        description="Suggested corrections with citations",
    )


class ChallengerAgent(BaseAgent[ChallengerInput, ChallengerOutput]):
    """The Challenger agent validates documentation quality.

    The Challenger's role is to:
    1. Review each section of the Scribe's documentation
    2. Cross-check claims against the source code
    3. Ask probing questions about vague or suspicious entries
    4. Identify contradictions and missing information
    5. Provide an overall validation assessment

    Example:
        config = ChallengerConfig(model="claude-sonnet-4-20250514", temperature=0.5)
        challenger = ChallengerAgent(config)

        output = await challenger.ainvoke(ChallengerInput(
            template=scribe_output.template,
            source_code=cobol_source,
            file_name="PROGRAM.cbl",
            file_type=FileType.COBOL,
        ))
        for q in output.questions:
            print(f"{q.question_type}: {q.question}")
    """

    def __init__(
        self,
        config: ChallengerConfig,
        api_config: APIConfig | None = None,
    ):
        """Initialize the Challenger agent.

        Args:
            config: Challenger-specific configuration.
            api_config: API configuration. If None, loads from environment.
        """
        super().__init__(config, api_config, name="Challenger")

    def _build_system_prompt(self) -> str:
        """Build the Challenger's system prompt.

        Returns:
            The system prompt defining Challenger's role and behavior.
        """
        return """You are the Challenger, a validation specialist for mainframe documentation.

Your role is to review documentation produced by the Scribe and identify issues, gaps, and errors by cross-checking against the source code.

## Core Principles

1. **Be thorough but fair**: Look for real issues, not nitpicks. Focus on accuracy and completeness.

2. **Provide evidence**: Every challenge must include line number citations from the source code.

3. **Ask meaningful questions**: Questions should help improve the documentation, not just create work.

4. **Assess per-section**: Evaluate each section independently as SOLID, SHAKY, or WRONG.

## Question Types

Use these question types appropriately:

- **CLARIFICATION**: "What do you mean by X?" - for vague or unclear statements
- **VERIFICATION**: "Line 234 says Y, but you documented Z" - for potential errors
- **COMPLETENESS**: "What about the error handling at line 456?" - for missing information
- **CHALLENGE**: "Are you sure this is an input, not an output?" - for questionable claims

## Severity Levels

- **BLOCKING**: Must be resolved before approval (factual errors, missing critical info)
- **IMPORTANT**: Should be addressed but not strictly required
- **MINOR**: Nice to have improvements

## Assessment Levels

For each section, assess as:

- **SOLID**: Documentation is accurate, complete, and well-cited
- **SHAKY**: Some concerns but mostly acceptable
- **WRONG**: Factual errors or significant problems

## Critical Sections

These sections MUST be SOLID for approval:
- Purpose (must be specific, not generic)
- Inputs
- Outputs
- Business Rules (at least one with citation)

## Output Format

Respond with valid JSON containing:
- "questions": array of ChallengerQuestion objects
- "assessment": ChallengerAssessment object with per-section assessments
- "issues_found": array of issue descriptions
- "suggested_corrections": array of {section, issue, correction, citations}

Respond ONLY with valid JSON. Do not include markdown code fences or explanatory text."""

    def _build_user_prompt(self, input_data: ChallengerInput) -> str:
        """Build the user prompt from input data.

        Args:
            input_data: The Challenger's input.

        Returns:
            The user message with documentation and code.
        """
        parts = []

        # Basic context
        parts.append(f"## Validating: {input_data.file_name}")
        parts.append(f"File Type: {input_data.file_type.value}")
        parts.append(f"Iteration: {input_data.iteration}")
        parts.append(f"Max Questions: {input_data.max_questions}")
        parts.append("")

        # Scribe's documentation
        parts.append("## Scribe's Documentation")
        parts.append("```json")
        parts.append(input_data.template.model_dump_json(indent=2))
        parts.append("```")
        parts.append("")

        # Preprocessor hints if available
        if input_data.preprocessor_result:
            parts.append("## Preprocessor Analysis")
            parts.append("```json")
            parts.append(input_data.preprocessor_result.model_dump_json(indent=2))
            parts.append("```")
            parts.append("")

        # Source code for verification
        parts.append("## Source Code (for verification)")
        parts.append("```")
        lines = input_data.source_code.split("\n")
        for i, line in enumerate(lines, start=1):
            parts.append(f"{i:5d} | {line}")
        parts.append("```")
        parts.append("")

        # Previous questions and responses
        if input_data.previous_questions:
            parts.append("## Previous Questions and Responses")
            for q in input_data.previous_questions:
                parts.append(f"Q [{q.question_id}]: {q.question}")
                # Find matching response
                response = next(
                    (r for r in input_data.scribe_responses if r.question_id == q.question_id),
                    None,
                )
                if response:
                    parts.append(f"A: {response.response} (Action: {response.action_taken.value})")
                else:
                    parts.append("A: [No response]")
                parts.append("")

        # Instructions
        parts.append("## Task")
        parts.append("1. Review the documentation against the source code")
        parts.append("2. Verify all citations are accurate")
        parts.append("3. Identify any gaps, errors, or vague statements")
        parts.append(f"4. Ask up to {input_data.max_questions} questions")
        parts.append("5. Provide a per-section assessment")
        parts.append("")
        parts.append("Focus on critical sections: Purpose, Inputs, Outputs, Business Rules")

        return "\n".join(parts)

    def _parse_response(self, response: str, input_data: ChallengerInput) -> ChallengerOutput:
        """Parse the LLM response into ChallengerOutput.

        Args:
            response: Raw text response from the LLM.
            input_data: Original input.

        Returns:
            Parsed ChallengerOutput.
        """
        try:
            # Try to extract JSON from response
            json_match = re.search(r"\{[\s\S]*\}", response)
            if not json_match:
                raise ValueError("No JSON object found in response")

            data = json.loads(json_match.group())

            # Parse questions
            questions = []
            if "questions" in data:
                for q in data["questions"]:
                    # Ensure iteration is set
                    q["iteration"] = input_data.iteration
                    questions.append(ChallengerQuestion.model_validate(q))

            # Parse assessment
            assessment = None
            if "assessment" in data:
                assessment = ChallengerAssessment.model_validate(data["assessment"])

            # Parse issues
            issues_found = data.get("issues_found", [])

            # Parse suggested corrections
            suggested_corrections = data.get("suggested_corrections", [])

            return ChallengerOutput(
                success=True,
                questions=questions,
                assessment=assessment,
                issues_found=issues_found,
                suggested_corrections=suggested_corrections,
            )

        except Exception as e:
            logger.error(f"Failed to parse Challenger response: {e}")
            logger.debug(f"Response was: {response[:500]}...")
            return ChallengerOutput(
                success=False,
                error=f"Failed to parse response: {e}",
            )

    def _create_error_output(self, error: str, input_data: ChallengerInput) -> ChallengerOutput:
        """Create an error output.

        Args:
            error: Error message.
            input_data: Original input.

        Returns:
            ChallengerOutput indicating failure.
        """
        return ChallengerOutput(
            success=False,
            error=error,
        )

    def create_mock_output(self, input_data: ChallengerInput) -> ChallengerOutput:
        """Create mock output for testing without LLM calls.

        This method produces valid output structure with placeholder data,
        useful for testing the orchestration without making API calls.

        Args:
            input_data: The Challenger's input.

        Returns:
            Mock ChallengerOutput with valid structure.
        """
        # Create a mock question
        mock_question = ChallengerQuestion(
            section="purpose",
            question_type=QuestionType.CLARIFICATION,
            question="[MOCK] Can you provide more specific details about the business process?",
            evidence=[1, 2, 3],
            severity=QuestionSeverity.IMPORTANT,
            iteration=input_data.iteration,
        )

        # Create mock assessment
        mock_assessment = ChallengerAssessment(
            program_id=input_data.template.header.program_id,
            iteration=input_data.iteration,
            section_assessments=[
                SectionAssessment(
                    section_name="purpose",
                    validation_level=ValidationLevel.SHAKY,
                    issues=["[MOCK] Purpose could be more specific"],
                    suggestions=["Add more business context"],
                ),
                SectionAssessment(
                    section_name="inputs",
                    validation_level=ValidationLevel.SOLID,
                    issues=[],
                    suggestions=[],
                ),
                SectionAssessment(
                    section_name="outputs",
                    validation_level=ValidationLevel.SOLID,
                    issues=[],
                    suggestions=[],
                ),
            ],
            overall_assessment="[MOCK] Documentation needs minor improvements",
            key_concerns=["Purpose section needs more detail"],
        )

        return ChallengerOutput(
            success=True,
            questions=[mock_question],
            assessment=mock_assessment,
            issues_found=["[MOCK] Purpose section is too generic"],
            suggested_corrections=[],
        )
