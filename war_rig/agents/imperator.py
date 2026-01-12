"""Imperator agent for documentation review and approval.

The Imperator is the reviewer who oversees the War Boy pair (Scribe and Challenger).
It decides when documentation is complete and issues Chrome tickets for major issues.

Responsibilities:
- Evaluate if Scribe and Challenger have converged
- Check that critical sections are complete
- Identify issues the pair missed
- Decide when further iteration is unlikely to improve quality
- Issue final approval (WITNESSED) or exceptional quality (VALHALLA)

Holistic Review Mode:
- Review all documentation together for batch processing
- Identify cross-program inconsistencies
- Create assumptions about system behavior
- Track clarification requests across cycles
"""

import json
import logging
import re
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field

from war_rig.agents.base import AgentInput, AgentOutput, BaseAgent
from war_rig.config import APIConfig, ImperatorConfig
from war_rig.models.assessments import ChallengerAssessment, ConfidenceAssessment, ConfidenceLevel
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
    beads_ticket_ids: list[str] = Field(
        default_factory=list,
        description="Beads ticket IDs created for Chrome tickets",
    )


# =============================================================================
# Holistic Review Models
# =============================================================================


class ImperatorHolisticDecision(str, Enum):
    """Possible decisions from holistic review of batch documentation.

    - SATISFIED: All documentation meets quality standards across all files
    - NEEDS_CLARIFICATION: Questions need to be answered before approval
    - FORCED_COMPLETE: Max cycles reached, accept documentation as-is
    """

    SATISFIED = "SATISFIED"
    NEEDS_CLARIFICATION = "NEEDS_CLARIFICATION"
    FORCED_COMPLETE = "FORCED_COMPLETE"


class FileDocumentation(BaseModel):
    """Documentation for a single file in a batch.

    Contains the template and associated assessments for cross-file review.
    """

    file_name: str = Field(
        ...,
        description="Name of the source file",
    )
    program_id: str = Field(
        ...,
        description="Program identifier",
    )
    template: DocumentationTemplate = Field(
        ...,
        description="The documentation template",
    )
    scribe_confidence: ConfidenceAssessment | None = Field(
        default=None,
        description="Scribe's confidence assessment",
    )
    challenger_assessment: ChallengerAssessment | None = Field(
        default=None,
        description="Challenger's validation assessment",
    )
    iteration_count: int = Field(
        default=1,
        ge=1,
        description="Number of iterations completed for this file",
    )


class ClarificationRequest(BaseModel):
    """A clarification request from the Imperator during holistic review.

    Represents a question that needs to be answered to proceed with approval.
    """

    request_id: str = Field(
        default="",
        description="Unique identifier for this request",
    )
    file_name: str = Field(
        default="CROSS_FILE",
        description="File this request relates to (or 'CROSS_FILE' for system-level)",
    )
    question: str = Field(
        default="",
        description="The clarification question",
    )
    context: str = Field(
        default="",
        description="Additional context for the question",
    )
    related_files: list[str] = Field(
        default_factory=list,
        description="Other files related to this question",
    )
    cycle_asked: int = Field(
        default=1,
        ge=1,
        description="Which cycle this question was first asked",
    )


class ConsistencyIssue(BaseModel):
    """Cross-file consistency issue identified by Imperator.

    Represents a discrepancy or conflict between documentation of different files.
    """

    issue_type: str = Field(
        default="UNKNOWN",
        description="Type of issue: NAMING, DATA_FLOW, INTERFACE, BUSINESS_RULE, CALL_CHAIN",
    )
    # Also accept issue_id as alias (LLM sometimes uses this)
    issue_id: str | None = Field(
        default=None,
        description="Alternative identifier (mapped to issue_type if issue_type missing)",
    )
    description: str = Field(
        default="",
        description="Description of the consistency issue",
    )
    affected_files: list[str] = Field(
        default_factory=list,
        description="Files affected by this issue",
    )
    guidance: str = Field(
        default="",
        description="Guidance for resolving the issue",
    )
    severity: str = Field(
        default="MEDIUM",
        description="Severity: CRITICAL, HIGH, MEDIUM",
    )


class SystemAssumption(BaseModel):
    """An assumption made by the Imperator about system behavior.

    When documentation is incomplete, the Imperator may need to make
    assumptions about how the system works.
    """

    assumption_id: str = Field(
        default="",
        description="Unique identifier for this assumption",
    )
    description: str = Field(
        default="",
        description="The assumption being made",
    )
    basis: str = Field(
        default="",
        description="Evidence or reasoning supporting the assumption",
    )
    affected_files: list[str] = Field(
        default_factory=list,
        description="Files affected by this assumption",
    )
    confidence: ConfidenceLevel = Field(
        default=ConfidenceLevel.MEDIUM,
        description="Confidence level in this assumption",
    )
    needs_verification: bool = Field(
        default=True,
        description="Whether this assumption should be verified by SME",
    )


class HolisticReviewInput(AgentInput):
    """Input for Imperator holistic review of batch documentation.

    Contains all documentation templates, cross-program dependencies,
    and cycle history for a comprehensive review.
    """

    batch_id: str = Field(
        ...,
        description="Unique identifier for this batch",
    )
    cycle: int = Field(
        default=1,
        ge=1,
        description="Current review cycle number",
    )

    # All documentation to review
    file_documentation: list[FileDocumentation] = Field(
        default_factory=list,
        description="Documentation for all files in the batch",
    )

    # Cross-file analysis
    shared_copybooks: dict[str, list[str]] = Field(
        default_factory=dict,
        description="Mapping of copybook -> list of files using it",
    )
    call_graph: dict[str, list[str]] = Field(
        default_factory=dict,
        description="Mapping of program -> list of called programs",
    )
    data_flow: dict[str, list[str]] = Field(
        default_factory=dict,
        description="Mapping of file -> list of input/output files",
    )

    # Quality metrics
    per_file_confidence: dict[str, ConfidenceLevel] = Field(
        default_factory=dict,
        description="Mapping of file_name -> confidence level",
    )
    per_file_issues: dict[str, list[str]] = Field(
        default_factory=dict,
        description="Mapping of file_name -> list of known issues",
    )

    # Previous cycle history (for avoiding repetition)
    previous_clarification_requests: list[ClarificationRequest] = Field(
        default_factory=list,
        description="Clarification requests from previous cycles",
    )
    previous_chrome_tickets: list[ChromeTicket] = Field(
        default_factory=list,
        description="Chrome tickets from previous cycles",
    )
    resolution_status: dict[str, bool] = Field(
        default_factory=dict,
        description="Mapping of ticket_id -> whether resolved",
    )

    # Configuration
    max_cycles: int = Field(
        default=5,
        ge=1,
        description="Maximum number of review cycles",
    )


class HolisticReviewOutput(AgentOutput):
    """Output from Imperator holistic review.

    Contains per-file feedback, consistency issues, clarification requests,
    and the overall decision.
    """

    decision: ImperatorHolisticDecision = Field(
        ...,
        description="The holistic review decision",
    )

    # Per-file feedback
    file_feedback: dict[str, list[ChromeTicket]] = Field(
        default_factory=dict,
        description="Mapping of file_name -> Chrome tickets for that file",
    )

    # Cross-file issues
    consistency_issues: list[ConsistencyIssue] = Field(
        default_factory=list,
        description="Cross-file consistency issues identified",
    )

    # Clarification requests
    clarification_requests: list[ClarificationRequest] = Field(
        default_factory=list,
        description="New clarification requests (not asked in previous cycles)",
    )

    # System assumptions
    assumptions: list[SystemAssumption] = Field(
        default_factory=list,
        description="Assumptions made about system behavior",
    )

    # Quality assessment
    overall_quality: str = Field(
        default="ACCEPTABLE",
        description="Overall quality: EXCELLENT, GOOD, ACCEPTABLE, POOR",
    )
    quality_notes: list[str] = Field(
        default_factory=list,
        description="Notes on documentation quality",
    )

    # Recommendations
    priority_files: list[str] = Field(
        default_factory=list,
        description="Files to prioritize in next cycle",
    )
    missing_documentation: list[str] = Field(
        default_factory=list,
        description="Files that need more work",
    )

    # Reasoning
    reasoning: str = Field(
        default="",
        description="Explanation of the decision",
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
        api_config: APIConfig | None = None,
    ):
        """Initialize the Imperator agent.

        Args:
            config: Imperator-specific configuration.
            api_config: API configuration. If None, loads from environment.
        """
        super().__init__(config, api_config, name="Imperator")

    async def _call_llm(self, system_prompt: str, user_prompt: str) -> str:
        """Call the LLM with system and user prompts.

        Args:
            system_prompt: The system message.
            user_prompt: The user message.

        Returns:
            The LLM response content as string.
        """
        from langchain_core.messages import HumanMessage, SystemMessage

        messages = [
            SystemMessage(content=system_prompt),
            HumanMessage(content=user_prompt),
        ]

        response = await self.llm.ainvoke(messages)
        content = response.content
        if isinstance(content, list):
            content = "\n".join(str(c) for c in content)
        return str(content)

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

    def create_beads_tickets(
        self,
        output: ImperatorOutput,
        program_id: str,
        team_id: int = 1,
        enabled: bool = True,
    ) -> ImperatorOutput:
        """Create beads tickets for Chrome tickets issued by Imperator.

        Args:
            output: The Imperator output with Chrome tickets.
            program_id: ID of the program being documented.
            team_id: Team number for multi-team setups.
            enabled: Whether to actually create tickets.

        Returns:
            Updated ImperatorOutput with ticket IDs.
        """
        if not enabled or not output.success:
            return output

        # Only create beads tickets if decision is CHROME
        if output.decision != ImperatorDecision.CHROME:
            return output

        from war_rig.beads import create_imperator_ticket, get_beads_client

        client = get_beads_client(enabled=enabled)
        ticket_ids = []

        for chrome_ticket in output.chrome_tickets:
            ticket_id = create_imperator_ticket(
                program_id=program_id,
                issue_type=chrome_ticket.issue_type.value,
                description=chrome_ticket.description,
                section=chrome_ticket.section,
                priority=chrome_ticket.priority.value,
                team_id=team_id,
                client=client,
            )
            if ticket_id:
                ticket_ids.append(ticket_id)
                # Store beads ticket ID on the chrome ticket
                chrome_ticket.ticket_id = f"{chrome_ticket.ticket_id}:{ticket_id}"

        output.beads_ticket_ids = ticket_ids
        return output

    # =========================================================================
    # Holistic Review Methods
    # =========================================================================

    def _build_holistic_system_prompt(self) -> str:
        """Build the system prompt for holistic review mode.

        Returns:
            The system prompt defining Imperator's holistic review behavior.
        """
        return """You are the Imperator conducting a holistic review of documentation for multiple programs.

## Holistic Review Criteria

Beyond individual file quality, evaluate:

1. **Consistency**: Are similar patterns documented consistently across files?
2. **Data Flow**: Are input/output relationships between programs documented?
3. **Call Chains**: Are CALL relationships properly documented on both ends?
4. **Shared Resources**: Are copybooks and common data areas explained?
5. **Business Process**: Does the documentation tell a coherent story?

## Cross-File Issues to Flag

- Program A documents calling B, but B doesn't mention being called
- Inconsistent naming for the same data elements across files
- Missing documentation for programs in call chains
- Conflicting business rule documentation
- Copybook usage not aligned with data flow

## Decision Criteria

SATISFIED: All files meet quality standards, cross-file issues resolved, documentation is coherent
NEEDS_CLARIFICATION: Questions need to be answered before approval can be granted
FORCED_COMPLETE: Max cycles reached, accept current state despite issues

## Avoiding Repetition

You will be provided with previous clarification requests. Do NOT ask the same questions again.
If a previous question was not adequately answered, you may rephrase or provide more context,
but prefer to make assumptions and document them rather than repeatedly asking.

## Output Format

Respond with valid JSON containing:
- "decision": one of SATISFIED, NEEDS_CLARIFICATION, FORCED_COMPLETE
- "file_feedback": object mapping file_name to array of ChromeTicket objects
- "consistency_issues": array of ConsistencyIssue objects
- "clarification_requests": array of new ClarificationRequest objects (not repeating previous)
- "assumptions": array of SystemAssumption objects
- "overall_quality": one of EXCELLENT, GOOD, ACCEPTABLE, POOR
- "quality_notes": array of observations about quality
- "priority_files": array of file names to focus on next cycle
- "missing_documentation": array of file names needing more work
- "reasoning": explanation of your decision

Respond ONLY with valid JSON. Do not include markdown code fences or explanatory text."""

    def _build_holistic_user_prompt(self, input_data: HolisticReviewInput) -> str:
        """Build the user prompt for holistic review.

        Args:
            input_data: The holistic review input.

        Returns:
            The user message with all documentation and context.
        """
        parts = []

        # Basic context
        parts.append(f"## Batch: {input_data.batch_id}")
        parts.append(f"Cycle: {input_data.cycle} of {input_data.max_cycles}")
        is_final = input_data.cycle >= input_data.max_cycles
        if is_final:
            parts.append("**THIS IS THE FINAL CYCLE - MUST MAKE FINAL DECISION**")
        parts.append(f"Files to review: {len(input_data.file_documentation)}")
        parts.append("")

        # System Overview - Executive summaries for holistic understanding
        # This allows the Imperator to grasp the overall system before diving into details
        parts.append("## System Overview (Read This First)")
        parts.append("")
        parts.append("The following summaries describe each program's purpose. "
                     "Read all summaries to understand how the system works holistically "
                     "before reviewing detailed documentation.")
        parts.append("")

        for doc in input_data.file_documentation:
            program_type = "UNKNOWN"
            summary = "No summary available"
            business_context = ""

            if doc.template and doc.template.purpose:
                if doc.template.purpose.summary:
                    summary = doc.template.purpose.summary
                if doc.template.purpose.program_type:
                    program_type = doc.template.purpose.program_type
                if doc.template.purpose.business_context:
                    business_context = f" ({doc.template.purpose.business_context})"

            parts.append(f"### {doc.program_id} [{program_type}]{business_context}")
            parts.append(f"{summary}")
            parts.append("")

        parts.append("---")
        parts.append("")

        # All file documentation (detailed)
        parts.append("## Detailed File Documentation")
        parts.append("")
        parts.append("Full documentation for each program follows. "
                     "Use this for verifying specific details and cross-referencing.")
        parts.append("")

        for doc in input_data.file_documentation:
            parts.append(f"### {doc.file_name} ({doc.program_id})")
            parts.append(f"Iterations: {doc.iteration_count}")
            parts.append("```json")
            parts.append(doc.template.model_dump_json(indent=2))
            parts.append("```")

            if doc.challenger_assessment:
                parts.append("Challenger Assessment:")
                parts.append(f"  Solid: {doc.challenger_assessment.solid_count}")
                parts.append(f"  Shaky: {doc.challenger_assessment.shaky_count}")
                parts.append(f"  Wrong: {doc.challenger_assessment.wrong_count}")

            if doc.scribe_confidence:
                parts.append(f"Scribe Confidence: {doc.scribe_confidence.overall_confidence.value}")

            parts.append("")

        # Cross-file analysis
        if input_data.shared_copybooks:
            parts.append("## Shared Copybooks")
            for copybook, files in input_data.shared_copybooks.items():
                parts.append(f"- {copybook}: used by {', '.join(files)}")
            parts.append("")

        if input_data.call_graph:
            parts.append("## Call Graph")
            for program, calls in input_data.call_graph.items():
                parts.append(f"- {program} calls: {', '.join(calls)}")
            parts.append("")

        if input_data.data_flow:
            parts.append("## Data Flow")
            for file, flows in input_data.data_flow.items():
                parts.append(f"- {file}: {', '.join(flows)}")
            parts.append("")

        # Quality metrics
        if input_data.per_file_confidence:
            parts.append("## Per-File Confidence")
            for file, conf in input_data.per_file_confidence.items():
                parts.append(f"- {file}: {conf.value}")
            parts.append("")

        if input_data.per_file_issues:
            parts.append("## Known Issues")
            for file, issues in input_data.per_file_issues.items():
                parts.append(f"- {file}:")
                for issue in issues:
                    parts.append(f"  - {issue}")
            parts.append("")

        # Previous cycle history
        if input_data.previous_clarification_requests:
            parts.append("## Previous Clarification Requests (DO NOT REPEAT)")
            for req in input_data.previous_clarification_requests:
                resolved = input_data.resolution_status.get(req.request_id, False)
                status = "RESOLVED" if resolved else "UNRESOLVED"
                parts.append(f"- [{status}] Cycle {req.cycle_asked}: {req.question}")
                if req.context:
                    parts.append(f"  Context: {req.context}")
            parts.append("")

        if input_data.previous_chrome_tickets:
            parts.append("## Previous Chrome Tickets")
            for ticket in input_data.previous_chrome_tickets:
                resolved = input_data.resolution_status.get(ticket.ticket_id, False)
                status = "RESOLVED" if resolved else "OPEN"
                parts.append(f"- [{status}] {ticket.section}: {ticket.description}")
            parts.append("")

        # Instructions
        parts.append("## Task")
        parts.append("1. Review all documentation for cross-file consistency")
        parts.append("2. Identify any conflicting or missing information")
        parts.append("3. Note any assumptions you need to make")
        parts.append("4. Decide: SATISFIED, NEEDS_CLARIFICATION, or FORCED_COMPLETE")
        if is_final:
            parts.append("5. This is the final cycle - you MUST decide SATISFIED or FORCED_COMPLETE")
        else:
            parts.append("5. If NEEDS_CLARIFICATION, ask NEW questions (not repeating previous)")

        return "\n".join(parts)

    def _parse_holistic_response(
        self,
        response: str,
        input_data: HolisticReviewInput,
    ) -> HolisticReviewOutput:
        """Parse the LLM response into HolisticReviewOutput.

        Args:
            response: Raw text response from the LLM.
            input_data: Original input.

        Returns:
            Parsed HolisticReviewOutput.
        """
        try:
            # Try to extract JSON from response
            json_match = re.search(r"\{[\s\S]*\}", response)
            if not json_match:
                raise ValueError("No JSON object found in response")

            data = json.loads(json_match.group())

            # Parse decision
            decision_str = data.get("decision", "NEEDS_CLARIFICATION")
            try:
                decision = ImperatorHolisticDecision(decision_str)
            except ValueError:
                decision = ImperatorHolisticDecision.NEEDS_CLARIFICATION

            # Handle forced completion at max cycles
            if input_data.cycle >= input_data.max_cycles:
                if decision == ImperatorHolisticDecision.NEEDS_CLARIFICATION:
                    decision = ImperatorHolisticDecision.FORCED_COMPLETE

            # Parse file feedback (Chrome tickets per file)
            file_feedback: dict[str, list[ChromeTicket]] = {}
            if "file_feedback" in data:
                for file_name, tickets in data["file_feedback"].items():
                    file_feedback[file_name] = []
                    for t in tickets:
                        # Find program_id from file documentation
                        program_id = file_name
                        for doc in input_data.file_documentation:
                            if doc.file_name == file_name:
                                program_id = doc.program_id
                                break
                        t["program_id"] = program_id
                        file_feedback[file_name].append(ChromeTicket.model_validate(t))

            # Parse consistency issues
            consistency_issues = []
            if "consistency_issues" in data:
                for issue in data["consistency_issues"]:
                    consistency_issues.append(ConsistencyIssue.model_validate(issue))

            # Parse clarification requests
            clarification_requests = []
            if "clarification_requests" in data:
                for req in data["clarification_requests"]:
                    req["cycle_asked"] = input_data.cycle
                    clarification_requests.append(ClarificationRequest.model_validate(req))

            # Parse assumptions
            assumptions = []
            if "assumptions" in data:
                for assumption in data["assumptions"]:
                    assumptions.append(SystemAssumption.model_validate(assumption))

            return HolisticReviewOutput(
                success=True,
                decision=decision,
                file_feedback=file_feedback,
                consistency_issues=consistency_issues,
                clarification_requests=clarification_requests,
                assumptions=assumptions,
                overall_quality=data.get("overall_quality", "ACCEPTABLE"),
                quality_notes=data.get("quality_notes", []),
                priority_files=data.get("priority_files", []),
                missing_documentation=data.get("missing_documentation", []),
                reasoning=data.get("reasoning", ""),
            )

        except Exception as e:
            logger.error(f"Failed to parse holistic review response: {e}")
            logger.debug(f"Response was: {response[:500]}...")

            # On error, default to NEEDS_CLARIFICATION if not at max cycles
            if input_data.cycle >= input_data.max_cycles:
                return HolisticReviewOutput(
                    success=False,
                    error=f"Failed to parse response: {e}",
                    decision=ImperatorHolisticDecision.FORCED_COMPLETE,
                    reasoning="Forced completion due to parse error at max cycles",
                )
            return HolisticReviewOutput(
                success=False,
                error=f"Failed to parse response: {e}",
                decision=ImperatorHolisticDecision.NEEDS_CLARIFICATION,
            )

    async def holistic_review(
        self,
        input_data: HolisticReviewInput,
        use_mock: bool = False,
    ) -> HolisticReviewOutput:
        """Perform holistic review of batch documentation.

        Reviews all documentation together, identifies cross-program
        inconsistencies, creates assumptions about system behavior,
        and returns clarification requests if needed.

        This method is separate from the per-file review (ainvoke) and
        is intended for batch processing workflows.

        Args:
            input_data: The holistic review input containing all documentation.
            use_mock: If True, return mock output instead of calling LLM.

        Returns:
            HolisticReviewOutput with decision and feedback.
        """
        if use_mock:
            return self.create_mock_holistic_output(input_data)

        try:
            # Build prompts
            system_prompt = self._build_holistic_system_prompt()
            user_prompt = self._build_holistic_user_prompt(input_data)

            # Call LLM
            response = await self._call_llm(system_prompt, user_prompt)

            # Parse response
            return self._parse_holistic_response(response, input_data)

        except Exception as e:
            logger.error(f"Holistic review failed: {e}")
            if input_data.cycle >= input_data.max_cycles:
                return HolisticReviewOutput(
                    success=False,
                    error=str(e),
                    decision=ImperatorHolisticDecision.FORCED_COMPLETE,
                    reasoning=f"Forced completion due to error: {e}",
                )
            return HolisticReviewOutput(
                success=False,
                error=str(e),
                decision=ImperatorHolisticDecision.NEEDS_CLARIFICATION,
            )

    def create_mock_holistic_output(
        self,
        input_data: HolisticReviewInput,
    ) -> HolisticReviewOutput:
        """Create mock output for holistic review testing.

        This method produces valid output structure with placeholder data,
        useful for testing the batch orchestration without making API calls.

        Args:
            input_data: The holistic review input.

        Returns:
            Mock HolisticReviewOutput with valid structure.
        """
        from uuid import uuid4

        # Decide based on cycle
        if input_data.cycle >= input_data.max_cycles:
            decision = ImperatorHolisticDecision.SATISFIED
        elif input_data.cycle == 1:
            decision = ImperatorHolisticDecision.NEEDS_CLARIFICATION
        else:
            decision = ImperatorHolisticDecision.SATISFIED

        # Generate mock file feedback for first file if NEEDS_CLARIFICATION
        file_feedback: dict[str, list[ChromeTicket]] = {}
        if decision == ImperatorHolisticDecision.NEEDS_CLARIFICATION:
            if input_data.file_documentation:
                first_doc = input_data.file_documentation[0]
                file_feedback[first_doc.file_name] = [
                    ChromeTicket(
                        program_id=first_doc.program_id,
                        section="purpose",
                        issue_type=IssueType.VAGUE,
                        description="[MOCK] Purpose section needs cross-program context",
                        guidance="Explain how this program fits in the overall workflow",
                        priority=IssuePriority.MEDIUM,
                    )
                ]

        # Generate mock consistency issue if multiple files
        consistency_issues = []
        if len(input_data.file_documentation) > 1:
            consistency_issues.append(
                ConsistencyIssue(
                    issue_type="NAMING",
                    description="[MOCK] Inconsistent naming for shared data elements",
                    affected_files=[
                        doc.file_name for doc in input_data.file_documentation[:2]
                    ],
                    guidance="Align naming conventions across files",
                    severity="MEDIUM",
                )
            )

        # Generate mock clarification request if not satisfied
        clarification_requests = []
        if decision == ImperatorHolisticDecision.NEEDS_CLARIFICATION:
            # Check if this question was already asked
            existing_questions = {
                req.question for req in input_data.previous_clarification_requests
            }
            mock_question = "[MOCK] What is the expected frequency of batch execution?"
            if mock_question not in existing_questions:
                clarification_requests.append(
                    ClarificationRequest(
                        request_id=f"CLR-{uuid4().hex[:8].upper()}",
                        file_name="CROSS_FILE",
                        question=mock_question,
                        context="This affects error handling and recovery procedures",
                        related_files=[
                            doc.file_name for doc in input_data.file_documentation
                        ],
                        cycle_asked=input_data.cycle,
                    )
                )

        # Generate mock assumption
        assumptions = []
        if input_data.file_documentation:
            assumptions.append(
                SystemAssumption(
                    assumption_id=f"ASM-{uuid4().hex[:8].upper()}",
                    description="[MOCK] Programs execute in sequential order as listed",
                    basis="Based on typical batch processing patterns",
                    affected_files=[
                        doc.file_name for doc in input_data.file_documentation
                    ],
                    confidence=ConfidenceLevel.MEDIUM,
                    needs_verification=True,
                )
            )

        # Determine overall quality
        overall_quality = "ACCEPTABLE"
        if decision == ImperatorHolisticDecision.SATISFIED:
            overall_quality = "GOOD"

        return HolisticReviewOutput(
            success=True,
            decision=decision,
            file_feedback=file_feedback,
            consistency_issues=consistency_issues,
            clarification_requests=clarification_requests,
            assumptions=assumptions,
            overall_quality=overall_quality,
            quality_notes=[
                "[MOCK] This is mock output for holistic review testing",
                f"[MOCK] Reviewed {len(input_data.file_documentation)} files",
            ],
            priority_files=(
                [input_data.file_documentation[0].file_name]
                if input_data.file_documentation
                else []
            ),
            missing_documentation=[],
            reasoning=f"[MOCK] Holistic review decision after cycle {input_data.cycle}",
        )
