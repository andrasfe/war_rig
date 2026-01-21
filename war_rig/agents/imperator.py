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

from __future__ import annotations

import json
import logging
import re
from enum import Enum
from pathlib import Path

from pydantic import BaseModel, Field

from war_rig.agents.base import AgentInput, AgentOutput, BaseAgent
from war_rig.config import APIConfig, ImperatorConfig
from war_rig.models.assessments import (
    ChallengerAssessment,
    ConfidenceAssessment,
    ConfidenceLevel,
)
from war_rig.models.templates import DocumentationTemplate, FileType, FinalStatus
from war_rig.models.tickets import (
    ChromeTicket,
    IssuePriority,
    IssueType,
    ScribeResponse,
)
from war_rig.preprocessors.base import PreprocessorResult
from war_rig.providers import Message

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

    # System Design document generation results
    system_design_generated: bool = Field(
        default=False,
        description="Whether SYSTEM_DESIGN.md was generated/updated in this cycle",
    )
    system_design_questions: list[InlineQuestion] = Field(
        default_factory=list,
        description="Questions identified during system design generation",
    )
    system_design_path: str | None = Field(
        default=None,
        description="Path to the generated SYSTEM_DESIGN.md file",
    )


# =============================================================================
# System Overview Generation Models
# =============================================================================


class ProgramSummary(BaseModel):
    """Summary of a single program for system overview generation."""

    file_name: str = Field(..., description="Source file name")
    program_id: str = Field(..., description="Program identifier")
    program_type: str = Field(default="UNKNOWN", description="BATCH, CICS, UTILITY, etc.")
    summary: str = Field(..., description="One-line summary of what the program does")
    business_context: str = Field(default="", description="Business context/purpose")
    calls: list[str] = Field(default_factory=list, description="Programs this one calls")
    called_by: list[str] = Field(default_factory=list, description="Programs that call this one")
    inputs: list[str] = Field(default_factory=list, description="Key inputs (files, parameters)")
    outputs: list[str] = Field(default_factory=list, description="Key outputs (files, reports)")
    json_path: str = Field(default="", description="Relative path to the .json documentation file")


class SystemOverviewInput(BaseModel):
    """Input for system overview generation."""

    batch_id: str = Field(default="", description="Batch identifier")
    system_name: str = Field(default="CardDemo", description="Name of the system being documented")
    programs: list[ProgramSummary] = Field(
        default_factory=list,
        description="Summaries of all documented programs",
    )
    total_files: int = Field(default=0, description="Total number of files documented")


class SystemOverviewOutput(BaseModel):
    """Output from system overview generation."""

    success: bool = Field(default=True, description="Whether generation succeeded")
    error: str | None = Field(default=None, description="Error message if failed")
    markdown: str = Field(default="", description="The generated markdown content")
    executive_summary: str = Field(default="", description="High-level system description")
    subsystems: list[str] = Field(default_factory=list, description="Identified subsystems/modules")


class InlineQuestion(BaseModel):
    """A question identified during system design document generation.

    Questions are embedded inline in the document to highlight areas
    that need clarification or further investigation.
    """

    question_id: str = Field(
        description="Unique identifier for the question (e.g., 'Q001')"
    )
    question_text: str = Field(description="The actual question being asked")
    context: str = Field(
        description="Where in the document this question applies (section/subsection)"
    )
    related_files: list[str] = Field(
        default_factory=list,
        description="Files this question relates to",
    )
    cycle_asked: int = Field(
        default=1,
        description="Which review cycle this question was identified",
    )


class SystemDesignInput(BaseModel):
    """Input for system design document generation."""

    batch_id: str = Field(default="", description="Batch identifier")
    cycle: int = Field(default=1, description="Current review cycle number")
    file_documentation: list[dict] = Field(
        default_factory=list,
        description="All file documentation (summaries from documented files)",
    )
    existing_content: str | None = Field(
        default=None,
        description="Existing SYSTEM_DESIGN.md content if any (for iterative refinement)",
    )
    call_graph: dict | None = Field(
        default=None,
        description="Call graph data if available (program dependencies)",
    )


class SystemDesignOutput(BaseModel):
    """Output from system design document generation."""

    success: bool = Field(default=True, description="Whether generation succeeded")
    error: str | None = Field(default=None, description="Error message if failed")
    markdown: str = Field(default="", description="The generated markdown content")
    questions: list[InlineQuestion] = Field(
        default_factory=list,
        description="Questions identified in the document requiring clarification",
    )
    sections_updated: list[str] = Field(
        default_factory=list,
        description="Which sections were updated in this cycle",
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

    def _get_header_attr(self, template, attr: str, default=None):
        """Get an attribute from template header, handling both object and dict.

        When templates are created via lenient model_construct(), the header
        may be a dict instead of a HeaderSection object.

        Args:
            template: The DocumentationTemplate.
            attr: The attribute name to get from header.
            default: Default value if attribute not found.

        Returns:
            The attribute value or default.
        """
        header = template.header
        if isinstance(header, dict):
            return header.get(attr, default)
        return getattr(header, attr, default)

    def _set_header_attr(self, template, attr: str, value):
        """Set an attribute on template header, handling both object and dict.

        Args:
            template: The DocumentationTemplate.
            attr: The attribute name to set on header.
            value: The value to set.
        """
        header = template.header
        if isinstance(header, dict):
            header[attr] = value
        else:
            setattr(header, attr, value)

    async def _call_llm(self, system_prompt: str, user_prompt: str) -> str:
        """Call the LLM with system and user prompts.

        Uses the provider interface which supports all configured providers
        (OpenRouter, Anthropic, Google, OpenAI, etc.).

        Args:
            system_prompt: The system message.
            user_prompt: The user message.

        Returns:
            The LLM response content as string.
        """
        messages = [
            Message(role="system", content=system_prompt),
            Message(role="user", content=user_prompt),
        ]

        response = await self._provider.complete(
            messages=messages,
            model=self.config.model,
            temperature=self.config.temperature,
        )
        return response.content

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
                    t["program_id"] = self._get_header_attr(input_data.template, "program_id", "UNKNOWN")
                    chrome_tickets.append(ChromeTicket.model_validate(t))

            # Get reasoning
            reasoning = data.get("reasoning", "")

            # Get quality notes
            quality_notes = data.get("quality_notes", [])

            # Prepare final template if approved
            final_template = None
            if decision in (ImperatorDecision.WITNESSED, ImperatorDecision.VALHALLA, ImperatorDecision.FORCED):
                final_template = input_data.template.model_copy(deep=True)
                self._set_header_attr(final_template, "iteration_count", input_data.iteration)
                self._set_header_attr(final_template, "final_status", FinalStatus(decision.value))

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
                program_id=self._get_header_attr(input_data.template, "program_id", "UNKNOWN"),
                section="purpose",
                issue_type=IssueType.VAGUE,
                description="[MOCK] Purpose section needs more specific business context",
                guidance="Add details about what business process this program supports",
                priority=IssuePriority.HIGH,
            ))

        final_template = None
        if decision in (ImperatorDecision.WITNESSED, ImperatorDecision.VALHALLA):
            final_template = input_data.template.model_copy(deep=True)
            self._set_header_attr(final_template, "iteration_count", input_data.iteration)
            self._set_header_attr(final_template, "final_status", FinalStatus(decision.value))

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
        output_directory: Path | None = None,
    ) -> HolisticReviewOutput:
        """Perform holistic review of batch documentation.

        Reviews all documentation together, identifies cross-program
        inconsistencies, creates assumptions about system behavior,
        and returns clarification requests if needed.

        This method is separate from the per-file review (ainvoke) and
        is intended for batch processing workflows.

        If output_directory is provided, also generates/updates SYSTEM_DESIGN.md
        in that directory.

        Args:
            input_data: The holistic review input containing all documentation.
            use_mock: If True, return mock output instead of calling LLM.
            output_directory: If provided, generates SYSTEM_DESIGN.md in this
                directory. If None, skips system design generation.

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
            review_output = self._parse_holistic_response(response, input_data)

            # Generate SYSTEM_DESIGN.md if output_directory is provided
            if output_directory is not None:
                try:
                    # Read existing SYSTEM_DESIGN.md content if available
                    existing_content = self._read_existing_system_design(output_directory)

                    # Generate system design document
                    design_result = await self.generate_system_design(
                        input_data,
                        existing_content=existing_content,
                        use_mock=use_mock,
                    )

                    if design_result.success:
                        # Write the markdown to SYSTEM_DESIGN.md
                        system_design_path = output_directory / "SYSTEM_DESIGN.md"
                        system_design_path.write_text(
                            design_result.markdown, encoding="utf-8"
                        )

                        # Populate the new fields in the output
                        review_output.system_design_generated = True
                        review_output.system_design_questions = design_result.questions
                        review_output.system_design_path = str(system_design_path)

                        logger.info(
                            f"Generated SYSTEM_DESIGN.md at {system_design_path} "
                            f"with {len(design_result.questions)} inline questions"
                        )
                    else:
                        logger.warning(
                            f"System design generation failed: {design_result.error}"
                        )
                        review_output.system_design_generated = False

                except Exception as design_error:
                    # Log the error but don't fail the holistic review
                    logger.warning(
                        f"Failed to generate SYSTEM_DESIGN.md: {design_error}"
                    )
                    review_output.system_design_generated = False

            return review_output

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

    # =========================================================================
    # SYSTEM OVERVIEW GENERATION
    # =========================================================================

    def _build_system_overview_prompt(self, input_data: SystemOverviewInput) -> str:
        """Build the prompt for system overview generation.

        Args:
            input_data: The system overview input with all program summaries.

        Returns:
            The user prompt for the LLM.
        """
        parts = []

        parts.append(f"# System Overview Generation: {input_data.system_name}")
        parts.append("")
        parts.append(f"Total programs documented: {input_data.total_files}")
        parts.append("")
        parts.append("## Program Summaries")
        parts.append("")
        parts.append("Below are summaries of each program in the system. Your task is to:")
        parts.append("1. Understand how these programs work together as a system")
        parts.append("2. Identify logical subsystems or functional areas")
        parts.append("3. Describe the overall business purpose")
        parts.append("4. Write a coherent narrative that explains the system")
        parts.append("")

        # Group by program type
        by_type: dict[str, list[ProgramSummary]] = {}
        for prog in input_data.programs:
            ptype = prog.program_type or "UNKNOWN"
            if ptype not in by_type:
                by_type[ptype] = []
            by_type[ptype].append(prog)

        for ptype, programs in sorted(by_type.items()):
            parts.append(f"### {ptype} Programs ({len(programs)})")
            parts.append("")
            for prog in programs:
                parts.append(f"**{prog.program_id}** (`{prog.file_name}`)")
                parts.append(f"- Summary: {prog.summary}")
                if prog.business_context:
                    parts.append(f"- Business Context: {prog.business_context}")
                if prog.calls:
                    parts.append(f"- Calls: {', '.join(prog.calls)}")
                if prog.called_by:
                    parts.append(f"- Called by: {', '.join(prog.called_by)}")
                if prog.inputs:
                    parts.append(f"- Inputs: {', '.join(prog.inputs[:5])}")
                if prog.outputs:
                    parts.append(f"- Outputs: {', '.join(prog.outputs[:5])}")
                parts.append(f"- Documentation: [{prog.program_id}.json]({prog.json_path})")
                parts.append("")

        parts.append("## Output Format")
        parts.append("")
        parts.append("Generate a markdown document with the following structure:")
        parts.append("1. Executive Summary (2-3 paragraphs describing the system)")
        parts.append("2. Subsystems/Modules (group related programs)")
        parts.append("3. Data Flow (how data moves through the system)")
        parts.append("4. Program Index (table with links to each program's documentation)")
        parts.append("")
        parts.append("Use relative links to the .json files like: [PROGRAM](./programs/PROGRAM.json)")
        parts.append("")
        parts.append("Output ONLY the markdown content, no JSON wrapper.")

        return "\n".join(parts)

    async def generate_system_overview(
        self,
        input_data: SystemOverviewInput,
        use_mock: bool = False,
    ) -> SystemOverviewOutput:
        """Generate a system overview from all program documentation.

        This method synthesizes individual program summaries into a coherent
        narrative describing the overall system. It should be called after
        all documentation is complete.

        Args:
            input_data: System overview input with all program summaries.
            use_mock: If True, return mock output instead of calling LLM.

        Returns:
            SystemOverviewOutput with the generated markdown.
        """
        if use_mock:
            return self._create_mock_system_overview(input_data)

        try:
            system_prompt = """You are a technical writer creating a system overview document.

Your task is to synthesize individual program documentation summaries into a coherent
narrative that explains what the system does as a whole.

Guidelines:
- Write for a technical audience (developers, architects)
- Focus on business functionality, not implementation details
- Identify logical groupings of programs (subsystems)
- Describe data flow between components
- Use markdown formatting with headers, lists, and tables
- Include links to individual program documentation files

Be concise but comprehensive. The reader should understand the system's purpose
and architecture after reading your overview."""

            user_prompt = self._build_system_overview_prompt(input_data)

            # Call LLM
            response = await self._call_llm(system_prompt, user_prompt)

            # Extract executive summary (first few paragraphs after # header)
            lines = response.split("\n")
            exec_summary_lines = []
            in_summary = False
            for line in lines:
                if line.startswith("# "):
                    in_summary = True
                    continue
                if in_summary and line.startswith("## "):
                    break
                if in_summary and line.strip():
                    exec_summary_lines.append(line)
                if len(exec_summary_lines) >= 5:
                    break

            return SystemOverviewOutput(
                success=True,
                markdown=response,
                executive_summary=" ".join(exec_summary_lines),
            )

        except Exception as e:
            logger.error(f"System overview generation failed: {e}")
            return SystemOverviewOutput(
                success=False,
                error=str(e),
                markdown="",
            )

    def _create_mock_system_overview(
        self,
        input_data: SystemOverviewInput,
    ) -> SystemOverviewOutput:
        """Create mock system overview for testing.

        Args:
            input_data: The system overview input.

        Returns:
            Mock SystemOverviewOutput.
        """
        # Group programs by type
        by_type: dict[str, list[ProgramSummary]] = {}
        for prog in input_data.programs:
            ptype = prog.program_type or "UNKNOWN"
            if ptype not in by_type:
                by_type[ptype] = []
            by_type[ptype].append(prog)

        lines = []
        lines.append(f"# {input_data.system_name} System Overview")
        lines.append("")
        lines.append(f"*Generated automatically from {input_data.total_files} documented programs*")
        lines.append("")
        lines.append("## Executive Summary")
        lines.append("")
        lines.append(f"[MOCK] This is a mock system overview for {input_data.system_name}. ")
        lines.append(f"The system consists of {len(input_data.programs)} programs ")
        lines.append(f"organized into {len(by_type)} categories: {', '.join(sorted(by_type.keys()))}.")
        lines.append("")

        lines.append("## Subsystems")
        lines.append("")
        for ptype, programs in sorted(by_type.items()):
            lines.append(f"### {ptype}")
            lines.append("")
            for prog in programs:
                lines.append(f"- **{prog.program_id}**: {prog.summary}")
            lines.append("")

        lines.append("## Program Index")
        lines.append("")
        lines.append("| Program | Type | Description | Documentation |")
        lines.append("|---------|------|-------------|---------------|")
        for prog in sorted(input_data.programs, key=lambda p: p.program_id):
            lines.append(
                f"| {prog.program_id} | {prog.program_type} | "
                f"{prog.summary[:50]}... | [{prog.program_id}.json]({prog.json_path}) |"
            )

        return SystemOverviewOutput(
            success=True,
            markdown="\n".join(lines),
            executive_summary=f"[MOCK] {input_data.system_name} system with {len(input_data.programs)} programs",
            subsystems=list(by_type.keys()),
        )

    # =========================================================================
    # SYSTEM DESIGN DOCUMENT GENERATION
    # =========================================================================

    def _read_existing_system_design(self, output_dir: Path) -> str | None:
        """Read existing SYSTEM_DESIGN.md content if it exists.

        This utility method checks for an existing system design document
        and returns its content for iterative refinement.

        Args:
            output_dir: Directory where SYSTEM_DESIGN.md would be located.

        Returns:
            The content of SYSTEM_DESIGN.md as a string, or None if:
            - The file does not exist
            - A read error occurred (logged as warning)
        """
        system_design_path = output_dir / "SYSTEM_DESIGN.md"

        if not system_design_path.exists():
            return None

        try:
            return system_design_path.read_text(encoding="utf-8")
        except OSError as e:
            logger.warning(
                f"Failed to read existing SYSTEM_DESIGN.md at {system_design_path}: {e}"
            )
            return None

    def _build_system_design_prompt(
        self,
        input_data: HolisticReviewInput,
        existing_content: str | None = None,
    ) -> str:
        """Build the prompt for system design document generation.

        Creates a prompt instructing the LLM to generate a comprehensive
        SYSTEM_DESIGN.md document based on the file documentation and
        call graph information.

        Args:
            input_data: The holistic review input containing file documentation,
                call graph, and other system information.
            existing_content: If provided, the LLM will enhance/update this
                existing content rather than creating from scratch.

        Returns:
            The user prompt for the LLM to generate SYSTEM_DESIGN.md content.
        """
        parts = []

        parts.append("# System Design Document Generation")
        parts.append("")
        parts.append(f"**Batch ID**: {input_data.batch_id}")
        parts.append(f"**Review Cycle**: {input_data.cycle}")
        parts.append(f"**Total Files Documented**: {len(input_data.file_documentation)}")
        parts.append("")

        # Instructions for the LLM
        parts.append("## Your Task")
        parts.append("")
        if existing_content:
            parts.append(
                "An existing SYSTEM_DESIGN.md document is provided below. Your task is to "
                "**enhance and update** this document based on the latest documentation. "
                "Preserve valuable existing content while improving clarity, adding missing "
                "details, and resolving any previously marked questions if the answer is now clear."
            )
        else:
            parts.append(
                "Create a comprehensive SYSTEM_DESIGN.md document that describes the "
                "overall system architecture based on the program documentation provided below."
            )
        parts.append("")

        parts.append("## Required Document Structure")
        parts.append("")
        parts.append("Generate a markdown document with the following sections:")
        parts.append("")
        parts.append("### 1. Executive Summary")
        parts.append("- 2-3 paragraphs describing the system's purpose and scope")
        parts.append("- Key business functions served")
        parts.append("- Technology stack summary (COBOL, PL/I, DB2, CICS, etc.)")
        parts.append("")
        parts.append("### 2. Architecture Overview")
        parts.append("- High-level system architecture description")
        parts.append("- Entry points and interfaces")
        parts.append("- Integration patterns (batch, online, database)")
        parts.append("")
        parts.append("### 3. Key Components and Relationships")
        parts.append("- Core programs and their responsibilities")
        parts.append("- Shared components (copybooks, utilities)")
        parts.append("- Dependency relationships between programs")
        parts.append("")
        parts.append("### 4. Data Flows")
        parts.append("- How data moves between programs")
        parts.append("- Input sources and output destinations")
        parts.append("- File/database interactions")
        parts.append("")
        parts.append("### 5. Subsystem Breakdown")
        parts.append("- Logical groupings of programs by function")
        parts.append("- Batch vs. online processing areas")
        parts.append("- Shared services and utilities")
        parts.append("")

        parts.append("## Important Guidelines")
        parts.append("")
        parts.append(
            "- **Insert inline questions** wherever your understanding is unclear or "
            "information is missing. Mark these with: `\u2753 QUESTION: [your question here]`"
        )
        parts.append(
            "- Focus on architectural understanding, not implementation details"
        )
        parts.append("- Identify patterns and relationships across the system")
        parts.append("- Be explicit about assumptions and uncertainties")
        parts.append("")

        # Include file documentation summaries
        parts.append("## Program Documentation Summaries")
        parts.append("")

        if input_data.file_documentation:
            # Group by program type for better organization
            by_type: dict[str, list[FileDocumentation]] = {}
            for doc in input_data.file_documentation:
                ptype = (
                    doc.template.purpose.program_type.value
                    if doc.template.purpose.program_type
                    else "UNKNOWN"
                )
                if ptype not in by_type:
                    by_type[ptype] = []
                by_type[ptype].append(doc)

            for ptype, docs in sorted(by_type.items()):
                parts.append(f"### {ptype} Programs ({len(docs)})")
                parts.append("")

                for doc in docs:
                    parts.append(f"**{doc.program_id}** (`{doc.file_name}`)")
                    parts.append(f"- **Summary**: {doc.template.purpose.summary}")

                    if doc.template.purpose.business_context:
                        parts.append(
                            f"- **Business Context**: {doc.template.purpose.business_context}"
                        )

                    # Called programs
                    if doc.template.called_programs:
                        called_names = [
                            cp.program_name for cp in doc.template.called_programs
                        ]
                        parts.append(f"- **Calls**: {', '.join(called_names)}")

                    # Calling context
                    if doc.template.calling_context.called_by:
                        parts.append(
                            f"- **Called By**: {', '.join(doc.template.calling_context.called_by)}"
                        )

                    # Inputs
                    if doc.template.inputs:
                        input_names = [inp.name for inp in doc.template.inputs[:5]]
                        suffix = (
                            f" (+{len(doc.template.inputs) - 5} more)"
                            if len(doc.template.inputs) > 5
                            else ""
                        )
                        parts.append(f"- **Inputs**: {', '.join(input_names)}{suffix}")

                    # Outputs
                    if doc.template.outputs:
                        output_names = [out.name for out in doc.template.outputs[:5]]
                        suffix = (
                            f" (+{len(doc.template.outputs) - 5} more)"
                            if len(doc.template.outputs) > 5
                            else ""
                        )
                        parts.append(f"- **Outputs**: {', '.join(output_names)}{suffix}")

                    # Copybooks
                    if doc.template.copybooks_used:
                        copybook_names = [
                            cb.copybook_name for cb in doc.template.copybooks_used[:5]
                        ]
                        suffix = (
                            f" (+{len(doc.template.copybooks_used) - 5} more)"
                            if len(doc.template.copybooks_used) > 5
                            else ""
                        )
                        parts.append(
                            f"- **Copybooks**: {', '.join(copybook_names)}{suffix}"
                        )

                    parts.append("")
        else:
            parts.append("*No file documentation available.*")
            parts.append("")

        # Include call graph if available
        if input_data.call_graph:
            parts.append("## Call Graph")
            parts.append("")
            parts.append(
                "The following shows which programs call other programs:"
            )
            parts.append("")
            for caller, callees in sorted(input_data.call_graph.items()):
                if callees:
                    parts.append(f"- **{caller}** calls: {', '.join(callees)}")
            parts.append("")

        # Include shared copybooks if available
        if input_data.shared_copybooks:
            parts.append("## Shared Copybooks")
            parts.append("")
            parts.append(
                "The following copybooks are used by multiple programs:"
            )
            parts.append("")
            for copybook, users in sorted(input_data.shared_copybooks.items()):
                if len(users) > 1:
                    parts.append(f"- **{copybook}**: used by {', '.join(users)}")
            parts.append("")

        # Include data flow if available
        if input_data.data_flow:
            parts.append("## Data Flow Information")
            parts.append("")
            for program, flows in sorted(input_data.data_flow.items()):
                if flows:
                    parts.append(f"- **{program}**: {', '.join(flows)}")
            parts.append("")

        # Include existing content if provided
        if existing_content:
            parts.append("## Existing SYSTEM_DESIGN.md Content")
            parts.append("")
            parts.append(
                "Below is the current content of SYSTEM_DESIGN.md. Enhance and update "
                "this document, preserving valuable content while improving it:"
            )
            parts.append("")
            parts.append("```markdown")
            parts.append(existing_content)
            parts.append("```")
            parts.append("")

        # Final output instructions
        parts.append("## Output Format")
        parts.append("")
        parts.append(
            "Output ONLY the markdown content for SYSTEM_DESIGN.md. "
            "Do not wrap in JSON or add any preamble."
        )

        return "\n".join(parts)

    async def generate_system_design(
        self,
        input_data: HolisticReviewInput,
        existing_content: str | None = None,
        use_mock: bool = False,
    ) -> SystemDesignOutput:
        """Generate a SYSTEM_DESIGN.md document from batch documentation.

        This method synthesizes individual file documentation into a comprehensive
        system design document. It can either create a new document or enhance
        an existing one based on the provided content.

        Args:
            input_data: Holistic review input containing file documentation,
                call graph, and other system information.
            existing_content: If provided, the document will be enhanced/updated
                rather than created from scratch.
            use_mock: If True, return mock output without calling the LLM.

        Returns:
            SystemDesignOutput with the generated markdown and any identified
            questions requiring clarification.
        """
        if use_mock:
            return self._create_mock_system_design(input_data, existing_content)

        try:
            system_prompt = """You are a technical architect creating a system design document.

Your task is to synthesize individual program documentation into a comprehensive
SYSTEM_DESIGN.md document that describes the overall system architecture.

Guidelines:
- Write for a technical audience (developers, architects, maintainers)
- Focus on system-level understanding, not individual program details
- Identify patterns, relationships, and architectural boundaries
- Mark uncertainties with inline questions using the format: ❓ QUESTION: [your question]
- Use clear markdown formatting with headers, lists, and tables
- Be explicit about assumptions and information gaps"""

            user_prompt = self._build_system_design_prompt(input_data, existing_content)

            # Call LLM
            response = await self._call_llm(system_prompt, user_prompt)

            # Parse inline questions from the markdown
            questions = self._parse_inline_questions(response, input_data.cycle)

            # Identify which sections were updated (look for main headers)
            sections_updated = self._extract_sections(response)

            return SystemDesignOutput(
                success=True,
                markdown=response,
                questions=questions,
                sections_updated=sections_updated,
            )

        except Exception as e:
            logger.error(f"System design generation failed: {e}")
            return SystemDesignOutput(
                success=False,
                error=str(e),
                markdown="",
            )

    def _parse_inline_questions(
        self,
        markdown: str,
        cycle: int,
    ) -> list[InlineQuestion]:
        """Parse inline questions from the generated markdown.

        Looks for the pattern: ❓ QUESTION: [question text]

        Args:
            markdown: The generated markdown content.
            cycle: The current review cycle number.

        Returns:
            List of InlineQuestion objects parsed from the markdown.
        """
        import re

        questions = []
        # Pattern: ❓ QUESTION: followed by text until newline or end
        pattern = r"❓\s*QUESTION:\s*(.+?)(?:\n|$)"
        matches = re.finditer(pattern, markdown)

        for idx, match in enumerate(matches, start=1):
            question_text = match.group(1).strip()

            # Try to determine context from surrounding content
            # Look for the nearest preceding header
            start_pos = match.start()
            context = self._find_section_context(markdown, start_pos)

            questions.append(
                InlineQuestion(
                    question_id=f"Q{idx:03d}",
                    question_text=question_text,
                    context=context,
                    related_files=[],  # Could be enhanced to detect file references
                    cycle_asked=cycle,
                )
            )

        return questions

    def _find_section_context(self, markdown: str, position: int) -> str:
        """Find the section context for a given position in markdown.

        Looks backwards from the position to find the nearest header.

        Args:
            markdown: The markdown content.
            position: Character position to find context for.

        Returns:
            The section header text, or "General" if no header found.
        """
        import re

        # Get the text before the position
        text_before = markdown[:position]

        # Find all headers before this position
        header_pattern = r"^(#{1,6})\s+(.+?)$"
        headers = list(re.finditer(header_pattern, text_before, re.MULTILINE))

        if headers:
            # Return the last (nearest) header
            last_header = headers[-1]
            return last_header.group(2).strip()

        return "General"

    def _extract_sections(self, markdown: str) -> list[str]:
        """Extract top-level section names from markdown.

        Args:
            markdown: The markdown content.

        Returns:
            List of section names (## level headers).
        """
        import re

        sections = []
        # Match ## headers (level 2)
        pattern = r"^##\s+(.+?)$"
        matches = re.finditer(pattern, markdown, re.MULTILINE)

        for match in matches:
            sections.append(match.group(1).strip())

        return sections

    def _create_mock_system_design(
        self,
        input_data: HolisticReviewInput,
        existing_content: str | None = None,
    ) -> SystemDesignOutput:
        """Create mock system design output for testing.

        Args:
            input_data: The holistic review input.
            existing_content: Existing content if any.

        Returns:
            Mock SystemDesignOutput with sample content.
        """
        # Count programs by type
        program_count = len(input_data.file_documentation)

        lines = []
        lines.append("# System Design Document")
        lines.append("")
        lines.append(f"*Batch: {input_data.batch_id} | Cycle: {input_data.cycle}*")
        lines.append("")
        lines.append("## Executive Summary")
        lines.append("")
        lines.append(
            f"This system consists of {program_count} documented programs that work "
            "together to provide business functionality. The system processes data "
            "through a combination of batch and online components."
        )
        lines.append("")
        lines.append(
            "❓ QUESTION: What is the primary business domain this system serves?"
        )
        lines.append("")
        lines.append("## Architecture Overview")
        lines.append("")
        lines.append(
            "The system follows a layered architecture with clear separation between "
            "presentation, business logic, and data access layers."
        )
        lines.append("")
        lines.append(
            "❓ QUESTION: Are there external system integrations that need to be documented?"
        )
        lines.append("")
        lines.append("## Key Components and Relationships")
        lines.append("")

        if input_data.file_documentation:
            lines.append("### Documented Programs")
            lines.append("")
            for doc in input_data.file_documentation[:5]:  # Limit for mock
                lines.append(f"- **{doc.program_id}**: {doc.template.purpose.summary}")
            if len(input_data.file_documentation) > 5:
                lines.append(
                    f"- *...and {len(input_data.file_documentation) - 5} more programs*"
                )
            lines.append("")

        lines.append("## Data Flows")
        lines.append("")
        lines.append(
            "Data flows through the system via file inputs, database operations, "
            "and inter-program communication."
        )
        lines.append("")
        lines.append("## Subsystem Breakdown")
        lines.append("")
        lines.append(
            "Programs are organized into logical subsystems based on their functionality."
        )
        lines.append("")

        if existing_content:
            lines.append("---")
            lines.append("*Note: This mock output was generated with existing content provided.*")

        markdown = "\n".join(lines)

        # Create mock questions
        questions = [
            InlineQuestion(
                question_id="Q001",
                question_text="What is the primary business domain this system serves?",
                context="Executive Summary",
                related_files=[],
                cycle_asked=input_data.cycle,
            ),
            InlineQuestion(
                question_id="Q002",
                question_text="Are there external system integrations that need to be documented?",
                context="Architecture Overview",
                related_files=[],
                cycle_asked=input_data.cycle,
            ),
        ]

        return SystemDesignOutput(
            success=True,
            markdown=markdown,
            questions=questions,
            sections_updated=[
                "Executive Summary",
                "Architecture Overview",
                "Key Components and Relationships",
                "Data Flows",
                "Subsystem Breakdown",
            ],
        )
