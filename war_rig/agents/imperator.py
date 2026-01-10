"""Imperator agent for documentation review and approval.

The Imperator is the reviewer who oversees the War Boy pair (Scribe and Challenger).
It decides when documentation is complete and issues Chrome tickets for major issues.

Responsibilities:
- Evaluate if Scribe and Challenger have converged
- Check that critical sections are complete
- Identify issues the pair missed
- Decide when further iteration is unlikely to improve quality
- Issue final approval (WITNESSED) or exceptional quality (VALHALLA)
"""

import json
import logging
import re
from enum import Enum
from typing import Any

from pydantic import Field

from war_rig.agents.base import AgentInput, AgentOutput, BaseAgent
from war_rig.config import ImperatorConfig
from war_rig.models.assessments import ChallengerAssessment, ConfidenceAssessment
from war_rig.models.templates import DocumentationTemplate, FileType, FinalStatus
from war_rig.models.tickets import ChromeTicket, IssuePriority, IssueType, ScribeResponse
from war_rig.preprocessors.base import PreprocessorResult

logger = logging.getLogger(__name__)


class ImperatorDecision(str, Enum):
    """Possible decisions from the Imperator.

    - WITNESSED: Documentation approved, meets quality standards
    - CHROME: Issues found, needs more work (Chrome tickets issued)
    - VALHALLA: Exceptional quality documentation
    - FORCED: Approved after max iterations despite issues
    """

    WITNESSED = "WITNESSED"
    CHROME = "CHROME"
    VALHALLA = "VALHALLA"
    FORCED = "FORCED"


class ImperatorInput(AgentInput):
    """Input data for the Imperator agent.

    Contains all information needed to make an approval decision.
    """

    template: DocumentationTemplate = Field(
        ...,
        description="Current documentation draft",
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
    challenger_assessment: ChallengerAssessment | None = Field(
        default=None,
        description="Challenger's validation assessment",
    )
    scribe_confidence: ConfidenceAssessment | None = Field(
        default=None,
        description="Scribe's confidence assessment",
    )
    scribe_responses: list[ScribeResponse] = Field(
        default_factory=list,
        description="Scribe's responses to questions",
    )
    preprocessor_result: PreprocessorResult | None = Field(
        default=None,
        description="Structural hints from preprocessor",
    )
    max_iterations: int = Field(
        default=3,
        ge=1,
        description="Maximum allowed iterations",
    )
    max_chrome_tickets: int = Field(
        default=5,
        ge=1,
        description="Maximum Chrome tickets per review",
    )


class ImperatorOutput(AgentOutput):
    """Output from the Imperator agent.

    Contains the approval decision and any Chrome tickets.
    """

    decision: ImperatorDecision = Field(
        ...,
        description="The Imperator's decision",
    )
    chrome_tickets: list[ChromeTicket] = Field(
        default_factory=list,
        description="Chrome tickets if decision is CHROME",
    )
    final_template: DocumentationTemplate | None = Field(
        default=None,
        description="Approved template if WITNESSED or VALHALLA",
    )
    reasoning: str = Field(
        default="",
        description="Explanation of the decision",
    )
    quality_notes: list[str] = Field(
        default_factory=list,
        description="Notes on documentation quality",
    )


class ImperatorAgent(BaseAgent[ImperatorInput, ImperatorOutput]):
    """The Imperator agent reviews and approves documentation.

    The Imperator's role is to:
    1. Evaluate if Scribe and Challenger have converged
    2. Check that critical sections are complete
    3. Issue Chrome tickets for major issues
    4. Decide when documentation is ready for approval

    Acceptance Criteria:
    - Purpose section: Specific, not generic boilerplate
    - All inputs and outputs identified
    - At least one business rule documented with citation
    - Copybooks listed match structural hints
    - Challenger assessment is SOLID on critical sections
    - No unresolved WRONG assessments

    Example:
        config = ImperatorConfig(model="claude-sonnet-4-20250514", temperature=0.2)
        imperator = ImperatorAgent(config)

        output = await imperator.ainvoke(ImperatorInput(
            template=scribe_output.template,
            source_code=cobol_source,
            challenger_assessment=challenger_output.assessment,
            ...
        ))
        if output.decision == ImperatorDecision.WITNESSED:
            print("Documentation approved!")
    """

    def __init__(
        self,
        config: ImperatorConfig,
        api_key: str | None = None,
    ):
        """Initialize the Imperator agent.

        Args:
            config: Imperator-specific configuration.
            api_key: Anthropic API key.
        """
        super().__init__(config, api_key, name="Imperator")

    def _build_system_prompt(self) -> str:
        """Build the Imperator's system prompt.

        Returns:
            The system prompt defining Imperator's role and behavior.
        """
        return """You are the Imperator, the final reviewer for mainframe documentation.

Your role is to evaluate documentation quality and decide whether to approve (WITNESSED), request improvements (CHROME), or recognize exceptional work (VALHALLA).

## Core Principles

1. **Quality over speed**: Do not approve incomplete or inaccurate documentation.

2. **Be decisive**: Make a clear decision with reasoning.

3. **Provide actionable feedback**: If issuing Chrome tickets, be specific about what needs improvement.

4. **Recognize excellence**: Award VALHALLA for truly exceptional documentation.

## Acceptance Criteria

For WITNESSED approval, ALL of these must be true:
- Purpose section is specific (not generic boilerplate)
- All inputs and outputs are identified
- At least one business rule is documented with citation
- Copybooks listed match the preprocessor's findings
- Challenger assessment is SOLID on critical sections (Purpose, Inputs, Outputs)
- No unresolved WRONG assessments

For VALHALLA (exceptional), additionally:
- Every section is thoroughly documented
- All business rules are captured
- Error handling is complete
- Data flow is well-documented

## Chrome Tickets

If issuing CHROME decision, create tickets specifying:
- section: Which template section needs work
- issue_type: VAGUE, MISSING, CONTRADICTORY, UNCITED, INCOMPLETE, or WRONG
- description: Specific description of the problem
- guidance: Hint for how to resolve
- priority: CRITICAL, HIGH, or MEDIUM

## Decision Logic

1. If max iterations reached -> FORCED (approve with quality flag)
2. If critical issues exist -> CHROME (issue tickets)
3. If all criteria met with excellence -> VALHALLA
4. If all criteria met -> WITNESSED
5. Otherwise -> CHROME

## Output Format

Respond with valid JSON containing:
- "decision": one of WITNESSED, CHROME, VALHALLA
- "chrome_tickets": array of ChromeTicket objects (if CHROME)
- "reasoning": explanation of your decision
- "quality_notes": array of observations about quality

Respond ONLY with valid JSON. Do not include markdown code fences or explanatory text."""

    def _build_user_prompt(self, input_data: ImperatorInput) -> str:
        """Build the user prompt from input data.

        Args:
            input_data: The Imperator's input.

        Returns:
            The user message with documentation and context.
        """
        parts = []

        # Basic context
        parts.append(f"## Reviewing: {input_data.file_name}")
        parts.append(f"File Type: {input_data.file_type.value}")
        parts.append(f"Iteration: {input_data.iteration} of {input_data.max_iterations}")
        is_final = input_data.iteration >= input_data.max_iterations
        if is_final:
            parts.append("**THIS IS THE FINAL ITERATION - MUST APPROVE OR FORCE**")
        parts.append("")

        # Documentation to review
        parts.append("## Documentation")
        parts.append("```json")
        parts.append(input_data.template.model_dump_json(indent=2))
        parts.append("```")
        parts.append("")

        # Challenger assessment
        if input_data.challenger_assessment:
            parts.append("## Challenger Assessment")
            parts.append("```json")
            parts.append(input_data.challenger_assessment.model_dump_json(indent=2))
            parts.append("```")
            parts.append("")

        # Scribe confidence
        if input_data.scribe_confidence:
            parts.append("## Scribe Confidence")
            parts.append("```json")
            parts.append(input_data.scribe_confidence.model_dump_json(indent=2))
            parts.append("```")
            parts.append("")

        # Preprocessor for verification
        if input_data.preprocessor_result:
            parts.append("## Preprocessor Analysis (ground truth)")
            parts.append("```json")
            parts.append(input_data.preprocessor_result.model_dump_json(indent=2))
            parts.append("```")
            parts.append("")

        # Scribe responses
        if input_data.scribe_responses:
            parts.append("## Scribe's Responses to Challenger")
            for r in input_data.scribe_responses:
                parts.append(f"- Q[{r.question_id}]: {r.action_taken.value} - {r.response[:100]}...")
            parts.append("")

        # Key sections of source for spot-checking
        parts.append("## Source Code (first 100 lines for reference)")
        parts.append("```")
        lines = input_data.source_code.split("\n")[:100]
        for i, line in enumerate(lines, start=1):
            parts.append(f"{i:5d} | {line}")
        parts.append("```")
        parts.append("")

        # Instructions
        parts.append("## Task")
        parts.append("1. Review the documentation against acceptance criteria")
        parts.append("2. Consider the Challenger's assessment")
        parts.append("3. Verify critical sections are complete")
        parts.append("4. Make your decision: WITNESSED, CHROME, or VALHALLA")
        if is_final:
            parts.append("5. This is the final iteration - you MUST approve (WITNESSED/FORCED)")
        else:
            parts.append(f"5. If CHROME, issue up to {input_data.max_chrome_tickets} tickets")

        return "\n".join(parts)

    def _parse_response(self, response: str, input_data: ImperatorInput) -> ImperatorOutput:
        """Parse the LLM response into ImperatorOutput.

        Args:
            response: Raw text response from the LLM.
            input_data: Original input.

        Returns:
            Parsed ImperatorOutput.
        """
        try:
            # Try to extract JSON from response
            json_match = re.search(r"\{[\s\S]*\}", response)
            if not json_match:
                raise ValueError("No JSON object found in response")

            data = json.loads(json_match.group())

            # Parse decision
            decision_str = data.get("decision", "CHROME")
            try:
                decision = ImperatorDecision(decision_str)
            except ValueError:
                decision = ImperatorDecision.CHROME

            # Handle forced approval at max iterations
            if input_data.iteration >= input_data.max_iterations:
                if decision == ImperatorDecision.CHROME:
                    decision = ImperatorDecision.FORCED

            # Parse Chrome tickets
            chrome_tickets = []
            if "chrome_tickets" in data:
                for t in data["chrome_tickets"]:
                    t["program_id"] = input_data.template.header.program_id
                    chrome_tickets.append(ChromeTicket.model_validate(t))

            # Get reasoning
            reasoning = data.get("reasoning", "")

            # Get quality notes
            quality_notes = data.get("quality_notes", [])

            # Prepare final template if approved
            final_template = None
            if decision in (ImperatorDecision.WITNESSED, ImperatorDecision.VALHALLA, ImperatorDecision.FORCED):
                final_template = input_data.template.model_copy(deep=True)
                final_template.header.iteration_count = input_data.iteration
                final_template.header.final_status = FinalStatus(decision.value)

            return ImperatorOutput(
                success=True,
                decision=decision,
                chrome_tickets=chrome_tickets,
                final_template=final_template,
                reasoning=reasoning,
                quality_notes=quality_notes,
            )

        except Exception as e:
            logger.error(f"Failed to parse Imperator response: {e}")
            logger.debug(f"Response was: {response[:500]}...")

            # On error, default to CHROME if not at max iterations
            if input_data.iteration >= input_data.max_iterations:
                return ImperatorOutput(
                    success=False,
                    error=f"Failed to parse response: {e}",
                    decision=ImperatorDecision.FORCED,
                    reasoning="Forced approval due to parse error at max iterations",
                )
            return ImperatorOutput(
                success=False,
                error=f"Failed to parse response: {e}",
                decision=ImperatorDecision.CHROME,
            )

    def _create_error_output(self, error: str, input_data: ImperatorInput) -> ImperatorOutput:
        """Create an error output.

        Args:
            error: Error message.
            input_data: Original input.

        Returns:
            ImperatorOutput indicating failure.
        """
        # Default to FORCED at max iterations, CHROME otherwise
        if input_data.iteration >= input_data.max_iterations:
            return ImperatorOutput(
                success=False,
                error=error,
                decision=ImperatorDecision.FORCED,
                reasoning=f"Forced approval due to error: {error}",
            )
        return ImperatorOutput(
            success=False,
            error=error,
            decision=ImperatorDecision.CHROME,
        )

    def create_mock_output(self, input_data: ImperatorInput) -> ImperatorOutput:
        """Create mock output for testing without LLM calls.

        This method produces valid output structure with placeholder data,
        useful for testing the orchestration without making API calls.

        Args:
            input_data: The Imperator's input.

        Returns:
            Mock ImperatorOutput with valid structure.
        """
        # Decide based on iteration
        if input_data.iteration >= input_data.max_iterations:
            decision = ImperatorDecision.WITNESSED
        elif input_data.iteration == 1:
            decision = ImperatorDecision.CHROME
        else:
            decision = ImperatorDecision.WITNESSED

        chrome_tickets = []
        if decision == ImperatorDecision.CHROME:
            chrome_tickets.append(ChromeTicket(
                program_id=input_data.template.header.program_id,
                section="purpose",
                issue_type=IssueType.VAGUE,
                description="[MOCK] Purpose section needs more specific business context",
                guidance="Add details about what business process this program supports",
                priority=IssuePriority.HIGH,
            ))

        final_template = None
        if decision in (ImperatorDecision.WITNESSED, ImperatorDecision.VALHALLA):
            final_template = input_data.template.model_copy(deep=True)
            final_template.header.iteration_count = input_data.iteration
            final_template.header.final_status = FinalStatus(decision.value)

        return ImperatorOutput(
            success=True,
            decision=decision,
            chrome_tickets=chrome_tickets,
            final_template=final_template,
            reasoning=f"[MOCK] Decision after iteration {input_data.iteration}",
            quality_notes=["[MOCK] This is mock output for testing"],
        )
