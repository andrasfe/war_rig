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

import asyncio
import json
import logging
import re
from enum import Enum
from pathlib import Path
from typing import Any

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
from war_rig.validation.document_validator import DocumentValidator

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


# =============================================================================
# Tier 1 Compact Models (for reduced token usage in holistic review)
# =============================================================================


class FileDocumentationSummary(BaseModel):
    """Compact documentation summary for Tier 1 holistic review.

    Contains only essential information needed for cross-file consistency
    checking, reducing token usage by ~70-85% compared to full FileDocumentation.

    Use this instead of FileDocumentation when the Imperator only needs
    to assess overall quality and consistency, not detailed paragraph content.
    """

    file_name: str = Field(
        ...,
        description="Name of the source file",
    )
    program_id: str = Field(
        ...,
        description="Program identifier",
    )
    purpose_summary: str = Field(
        default="",
        description="Brief summary of what the program does",
    )
    program_type: str = Field(
        default="UNKNOWN",
        description="Type: BATCH, ONLINE_CICS, UTILITY, etc.",
    )
    paragraph_count: int = Field(
        default=0,
        ge=0,
        description="Number of paragraphs/functions in the file",
    )
    main_calls: list[str] = Field(
        default_factory=list,
        description="Primary programs/procedures this file calls",
    )
    main_inputs: list[str] = Field(
        default_factory=list,
        description="Key input files/tables (names only)",
    )
    main_outputs: list[str] = Field(
        default_factory=list,
        description="Key output files/tables (names only)",
    )
    confidence: ConfidenceLevel = Field(
        default=ConfidenceLevel.MEDIUM,
        description="Overall documentation confidence level",
    )
    has_open_questions: bool = Field(
        default=False,
        description="Whether there are unresolved questions",
    )
    iteration_count: int = Field(
        default=1,
        ge=1,
        description="Number of iterations completed for this file",
    )


class HolisticReviewInputCompact(BaseModel):
    """Compact input for Imperator holistic review (Tier 1).

    Uses FileDocumentationSummary instead of full FileDocumentation,
    and excludes verbose fields like call_graph_markdown and
    cross_file_call_semantics detail.

    Designed to reduce token usage from 15K-50K to 3K-8K tokens
    while still providing sufficient information for quality assessment.
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

    # Compact documentation summaries (Tier 1)
    file_summaries: list[FileDocumentationSummary] = Field(
        default_factory=list,
        description="Compact summaries for all files in the batch",
    )

    # Call graph topology (compact: just program -> callees mapping)
    call_graph: dict[str, list[str]] = Field(
        default_factory=dict,
        description="Mapping of program -> list of called programs",
    )

    # Shared resources (compact)
    shared_copybooks: dict[str, list[str]] = Field(
        default_factory=dict,
        description="Mapping of copybook -> list of files using it",
    )

    # Per-file quality indicators
    per_file_confidence: dict[str, ConfidenceLevel] = Field(
        default_factory=dict,
        description="Mapping of file_name -> confidence level",
    )
    files_with_issues: list[str] = Field(
        default_factory=list,
        description="File names that have known issues",
    )

    # Previous cycle history (compact: just counts, not full details)
    previous_clarification_count: int = Field(
        default=0,
        ge=0,
        description="Number of previous clarification requests",
    )
    unresolved_issues_count: int = Field(
        default=0,
        ge=0,
        description="Number of unresolved issues from previous cycles",
    )

    # Configuration
    max_cycles: int = Field(
        default=5,
        ge=1,
        description="Maximum number of review cycles",
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
    call_graph_markdown: str | None = Field(
        default=None,
        description="Full CALL_GRAPH.md content from Citadel static analysis (contains Mermaid diagram)",
    )
    cross_file_call_semantics: dict[str, dict] | None = Field(
        default=None,
        description="Call semantics for cross-file flows (inputs/outputs per call pair). "
        "Format: {'CALLER->CALLEE': {'inputs': [...], 'outputs': [...], 'purpose': ...}}",
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
        description="Notes on documentation quality (legacy string format)",
    )
    quality_notes_structured: list[dict[str, Any]] = Field(
        default_factory=list,
        description="Structured quality notes with category, severity, affected_sections, guidance",
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
        description="Whether README.md was generated/updated in this cycle",
    )
    system_design_questions: list[InlineQuestion] = Field(
        default_factory=list,
        description="Questions identified during system design generation",
    )
    system_design_path: str | None = Field(
        default=None,
        description="Path to the generated README.md file",
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


# Rebuild HolisticReviewOutput to resolve forward references (Any, InlineQuestion)
HolisticReviewOutput.model_rebuild()


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
        description="Existing README.md content if any (for iterative refinement)",
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
        self._document_validator = DocumentValidator()

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

        # Log token estimation for monitoring
        total_chars = len(system_prompt) + len(user_prompt)
        estimated_tokens = total_chars // 4  # Rough: 1 token ≈ 4 chars
        if estimated_tokens > 30000:
            logger.warning(
                f"Imperator: Large prompt detected: ~{estimated_tokens:,} tokens "
                f"({total_chars:,} chars) for {self.config.model}"
            )
        else:
            logger.info(
                f"Imperator: Sending ~{estimated_tokens:,} tokens "
                f"({total_chars:,} chars) to {self.config.model}"
            )

        # Some models require specific temperature settings
        # o3 and other reasoning models require temperature=1.0
        temperature = self.config.temperature
        model_lower = self.config.model.lower()
        if any(m in model_lower for m in ["o3", "o1-", "o1/"]):
            temperature = 1.0

        response = await self._provider.complete(
            messages=messages,
            model=self.config.model,
            temperature=temperature,
        )
        return response.content

    def _summarize_template(
        self,
        template: DocumentationTemplate,
        max_items: int = 5,
    ) -> str:
        """Create a compact text summary of a DocumentationTemplate.

        This produces a human-readable summary that is much smaller than
        the full JSON dump, reducing token usage in prompts significantly.

        Args:
            template: The DocumentationTemplate to summarize.
            max_items: Maximum items to show for lists (e.g., inputs, outputs).
                Beyond this, shows a count of remaining items.

        Returns:
            A compact text summary of the template.
        """
        parts = []

        # Purpose section (most important)
        if template.purpose:
            parts.append(f"**Purpose**: {template.purpose.summary or 'Not specified'}")
            if template.purpose.program_type:
                parts.append(f"**Type**: {template.purpose.program_type}")
            if template.purpose.business_context:
                parts.append(f"**Business Context**: {template.purpose.business_context}")

        # Inputs - compact format
        if template.inputs:
            input_names = [inp.name for inp in template.inputs[:max_items]]
            remaining = len(template.inputs) - max_items
            inputs_str = ", ".join(input_names)
            if remaining > 0:
                inputs_str += f", +{remaining} more"
            parts.append(f"**Inputs** ({len(template.inputs)}): {inputs_str}")

        # Outputs - compact format
        if template.outputs:
            output_names = [out.name for out in template.outputs[:max_items]]
            remaining = len(template.outputs) - max_items
            outputs_str = ", ".join(output_names)
            if remaining > 0:
                outputs_str += f", +{remaining} more"
            parts.append(f"**Outputs** ({len(template.outputs)}): {outputs_str}")

        # Called programs - compact format
        if template.called_programs:
            call_names = [cp.program_name for cp in template.called_programs[:max_items]]
            remaining = len(template.called_programs) - max_items
            calls_str = ", ".join(call_names)
            if remaining > 0:
                calls_str += f", +{remaining} more"
            parts.append(f"**Calls** ({len(template.called_programs)}): {calls_str}")

        # Called by
        if template.calling_context and template.calling_context.called_by:
            callers = template.calling_context.called_by[:max_items]
            remaining = len(template.calling_context.called_by) - max_items
            callers_str = ", ".join(callers)
            if remaining > 0:
                callers_str += f", +{remaining} more"
            parts.append(f"**Called By**: {callers_str}")

        # Copybooks - compact format
        if template.copybooks_used:
            cb_names = [cb.copybook_name for cb in template.copybooks_used[:max_items]]
            remaining = len(template.copybooks_used) - max_items
            cb_str = ", ".join(cb_names)
            if remaining > 0:
                cb_str += f", +{remaining} more"
            parts.append(f"**Copybooks** ({len(template.copybooks_used)}): {cb_str}")

        # Business rules - just count and first few descriptions
        if template.business_rules:
            parts.append(f"**Business Rules** ({len(template.business_rules)}):")
            for rule in template.business_rules[:3]:
                desc = rule.description or "No description"
                # Truncate long descriptions
                if len(desc) > 100:
                    desc = desc[:100] + "..."
                parts.append(f"  - {desc}")
            if len(template.business_rules) > 3:
                parts.append(f"  - (+{len(template.business_rules) - 3} more rules)")

        # Error handling - count only
        if template.error_handling:
            parts.append(f"**Error Handlers**: {len(template.error_handling)} defined")

        # SQL operations - count only
        if template.sql_operations:
            parts.append(f"**SQL Operations**: {len(template.sql_operations)}")

        # CICS operations - count only
        if template.cics_operations:
            parts.append(f"**CICS Operations**: {len(template.cics_operations)}")

        # Open questions
        if template.open_questions:
            parts.append(f"**Open Questions**: {len(template.open_questions)}")
            for q in template.open_questions[:2]:
                parts.append(f"  - {q.question}")
            if len(template.open_questions) > 2:
                parts.append(f"  - (+{len(template.open_questions) - 2} more)")

        return "\n".join(parts)

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

## Document Type Validation

Each document type has specific structural requirements that must be verified:

### JCL Jobs
- EXEC statements: Each step must document PGM=/PROC= and purpose
- DD statements: Key DDs must document DSN, DISP, and purpose
- Job flow: Overall sequence and dependencies must be clear

### COBOL Programs
- Procedure division: Key paragraphs must be documented
- Called programs: All CALLs with type (STATIC/DYNAMIC) and purpose
- Data structures: Copybooks and data flow must be explained

### Copybooks
- Field descriptions: Business meaning, not just PIC clauses
- Usage context: Where included (WS/LINKAGE/FILE) and by which programs
- Key fields: Important fields identified with significance

### PLI Programs
- Procedure structure: Key procedures must be documented
- Called procedures: All calls with purpose
- Data structures: Declarations and their usage

### BMS Maps
- Screen purpose: What screen this defines and which transaction uses it
- Screen fields: Input/output fields with their purposes

When reviewing, flag documents missing these type-specific elements.

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
- "quality_notes": array of observations about quality (legacy string format)
- "quality_notes_structured": array of structured quality observations (REQUIRED), each with:
  - "category": one of empty_section, missing_citation, vague_content, no_cross_reference, confidence_mismatch, redundant_doc, other
  - "severity": one of critical, high, medium, low
  - "description": human-readable description of the quality issue
  - "affected_sections": array of section names affected (e.g., ["inputs", "outputs", "data_flow"])
  - "affected_files": array of file names affected (empty = applies to all files)
  - "guidance": specific guidance for addressing the issue (optional)
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
            parts.append("")
            # Use compact summary instead of full JSON to reduce token usage
            parts.append(self._summarize_template(doc.template))
            parts.append("")

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
            # Build set of valid file names from input documentation
            valid_file_names = {doc.file_name for doc in input_data.file_documentation}
            # Build basename→full path lookup for normalizing LLM output
            # The LLM may output bare filenames (e.g., "PROG.cbl") instead of
            # relative paths (e.g., "cbl/PROG.cbl"). This maps basenames to
            # their full relative paths so we can correct them.
            basename_to_full: dict[str, str] = {}
            for vfn in valid_file_names:
                basename = Path(vfn).name
                # Only use unambiguous mappings (if multiple files share a
                # basename, we can't resolve and must skip)
                if basename in basename_to_full:
                    basename_to_full[basename] = ""  # Mark ambiguous
                else:
                    basename_to_full[basename] = vfn

            def _normalize_file_name(name: str) -> str | None:
                """Normalize an LLM-provided file name to a valid relative path.

                Returns the normalized name, or None if unresolvable.
                """
                if name in valid_file_names:
                    return name
                resolved = basename_to_full.get(name, "")
                if resolved:
                    logger.debug(f"Normalized LLM file name '{name}' -> '{resolved}'")
                    return resolved
                return None

            file_feedback: dict[str, list[ChromeTicket]] = {}
            if "file_feedback" in data:
                if not isinstance(data["file_feedback"], dict):
                    logger.warning(
                        f"LLM returned file_feedback as {type(data['file_feedback']).__name__} "
                        f"instead of dict, skipping"
                    )
                else:
                    for file_name, tickets in data["file_feedback"].items():
                        # Normalize LLM file name to valid relative path
                        resolved_name = _normalize_file_name(file_name)
                        if not resolved_name:
                            logger.warning(
                                f"Skipping Chrome tickets for {file_name}: "
                                f"not in documented files list"
                            )
                            continue
                        file_name = resolved_name

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
                    # Normalize file_name from LLM output
                    if "file_name" in req:
                        resolved = _normalize_file_name(req["file_name"])
                        if resolved:
                            req["file_name"] = resolved
                        else:
                            logger.warning(
                                f"Skipping clarification request for "
                                f"{req['file_name']}: not in documented files list"
                            )
                            continue
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
                quality_notes_structured=data.get("quality_notes_structured", []),
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

    def _validate_documents_by_type(
        self,
        file_documentation: list[FileDocumentation],
    ) -> dict[str, list[ChromeTicket]]:
        """Validate each document against type-specific criteria.

        This runs BEFORE the LLM holistic review to catch structural
        issues that can be detected programmatically.
        """
        validation_feedback: dict[str, list[ChromeTicket]] = {}

        for doc in file_documentation:
            result = self._document_validator.validate_document(
                file_name=doc.file_name,
                program_id=doc.program_id,
                template=doc.template,
            )

            if not result.is_valid:
                validation_feedback[doc.file_name] = result.tickets
                logger.info(
                    f"Document validation failed for {doc.file_name}: "
                    f"{len(result.failed_criteria)} criteria failed"
                )

        return validation_feedback

    async def holistic_review(
        self,
        input_data: HolisticReviewInput,
        use_mock: bool = False,
        output_directory: Path | None = None,
        sequence_diagrams: list[str] | None = None,
    ) -> HolisticReviewOutput:
        """Perform holistic review of batch documentation.

        Reviews all documentation together, identifies cross-program
        inconsistencies, creates assumptions about system behavior,
        and returns clarification requests if needed.

        This method is separate from the per-file review (ainvoke) and
        is intended for batch processing workflows.

        If output_directory is provided, also generates/updates README.md
        in that directory.

        Args:
            input_data: The holistic review input containing all documentation.
            use_mock: If True, return mock output instead of calling LLM.
            output_directory: If provided, generates README.md in this
                directory. If None, skips system design generation.
            sequence_diagrams: Optional list of mermaid sequence diagram strings
                from citadel to include in README.md Flows section.

        Returns:
            HolisticReviewOutput with decision and feedback.
        """
        if use_mock:
            return self.create_mock_holistic_output(input_data)

        # Run type-specific validation before LLM review
        type_validation_feedback = self._validate_documents_by_type(
            input_data.file_documentation
        )

        try:
            # Build prompts for holistic review
            system_prompt = self._build_holistic_system_prompt()
            user_prompt = self._build_holistic_user_prompt(input_data)

            # Read existing README.md content before parallel execution
            # (file I/O is fast and needed for system design generation)
            existing_content: str | None = None
            if output_directory is not None:
                existing_content = self._read_existing_system_design(output_directory)

            # Run holistic review and system design generation sequentially
            # to avoid HTTP 429 rate limit errors from too many concurrent requests
            if output_directory is not None:
                # Run LLM calls SEQUENTIALLY
                logger.info("Starting sequential holistic review and system design LLM calls...")

                # First: holistic review
                try:
                    response = await asyncio.wait_for(
                        self._call_llm(system_prompt, user_prompt),
                        timeout=600.0,  # 10 minutes for holistic review
                    )
                    response_or_error = response
                except TimeoutError:
                    response_or_error = TimeoutError("Holistic review timed out")
                except Exception as e:
                    response_or_error = e

                # Second: system design (README generation)
                try:
                    design_result = await asyncio.wait_for(
                        self.generate_system_design(
                            input_data,
                            existing_content=existing_content,
                            use_mock=use_mock,
                            sequence_diagrams=sequence_diagrams,
                        ),
                        timeout=300.0,  # 5 minutes for README
                    )
                    design_result_or_error = design_result
                except TimeoutError:
                    design_result_or_error = TimeoutError("System design timed out")
                except Exception as e:
                    design_result_or_error = e

                results = [response_or_error, design_result_or_error]
                logger.info("Sequential LLM calls completed")

                response_or_error = results[0]
                design_result_or_error = results[1]

                # Handle holistic review result - this is the primary operation
                if isinstance(response_or_error, Exception):
                    # Re-raise to be caught by outer exception handler
                    raise response_or_error

                response = response_or_error
                review_output = self._parse_holistic_response(response, input_data)

                # Handle system design result - failure should not fail holistic review
                if isinstance(design_result_or_error, Exception):
                    logger.warning(
                        f"Failed to generate README.md: {design_result_or_error}"
                    )
                    review_output.system_design_generated = False
                else:
                    design_result = design_result_or_error
                    if design_result.success:
                        # Write the markdown to README.md
                        system_design_path = output_directory / "README.md"
                        system_design_path.write_text(
                            design_result.markdown, encoding="utf-8"
                        )

                        # Populate the new fields in the output
                        review_output.system_design_generated = True
                        review_output.system_design_questions = design_result.questions
                        review_output.system_design_path = str(system_design_path)

                        logger.info(
                            f"Generated README.md at {system_design_path} "
                            f"with {len(design_result.questions)} inline questions"
                        )
                    else:
                        logger.warning(
                            f"System design generation failed: {design_result.error}"
                        )
                        review_output.system_design_generated = False
            else:
                # No output_directory, just run holistic review
                response = await self._call_llm(system_prompt, user_prompt)
                review_output = self._parse_holistic_response(response, input_data)

            # Merge type-specific validation feedback into review output
            for file_name, tickets in type_validation_feedback.items():
                if file_name in review_output.file_feedback:
                    review_output.file_feedback[file_name].extend(tickets)
                else:
                    review_output.file_feedback[file_name] = tickets

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
    # TIER 1 COMPACT HOLISTIC REVIEW
    # =========================================================================

    def _build_compact_holistic_system_prompt(self) -> str:
        """Build the system prompt for compact holistic review (Tier 1).

        This prompt is optimized for reviewing file summaries rather than
        full documentation templates.

        Returns:
            The system prompt for compact holistic review.
        """
        return """You are the Imperator conducting a compact holistic review of documentation summaries.

## Your Task

Review the documentation summaries for cross-file consistency and overall quality.
You are receiving SUMMARIES only (not full documentation) to enable efficient review.

## Evaluation Criteria

1. **Cross-Program Consistency**: Do programs that call each other have aligned I/O?
2. **Coverage**: Are all key programs documented? Are there gaps in the call chain?
3. **Quality Indicators**: Are confidence levels appropriate? Are there unresolved questions?
4. **Shared Resources**: Are copybooks used consistently?

## Decision Criteria

SATISFIED: Documentation summaries show good coverage, consistent cross-references, acceptable confidence
NEEDS_CLARIFICATION: Significant gaps, inconsistencies, or low confidence areas need attention
FORCED_COMPLETE: Max cycles reached, accept current state despite issues

## Output Format

Respond with valid JSON containing:
- "decision": one of SATISFIED, NEEDS_CLARIFICATION, FORCED_COMPLETE
- "file_feedback": object mapping file_name to array of issue objects (section, description, priority)
- "consistency_issues": array of cross-file issues (issue_type, description, affected_files, severity)
- "overall_quality": one of EXCELLENT, GOOD, ACCEPTABLE, POOR
- "quality_notes": array of observations about quality
- "priority_files": array of file names to focus on next cycle
- "reasoning": explanation of your decision

Respond ONLY with valid JSON. Do not include markdown code fences."""

    def _build_compact_holistic_user_prompt(
        self, input_data: HolisticReviewInputCompact
    ) -> str:
        """Build the user prompt for compact holistic review.

        Args:
            input_data: The compact holistic review input.

        Returns:
            The user message with file summaries and call graph.
        """
        parts = []

        # Basic context
        parts.append(f"## Batch: {input_data.batch_id}")
        parts.append(f"Cycle: {input_data.cycle} of {input_data.max_cycles}")
        is_final = input_data.cycle >= input_data.max_cycles
        if is_final:
            parts.append("**THIS IS THE FINAL CYCLE - MUST MAKE FINAL DECISION**")
        parts.append(f"Files to review: {len(input_data.file_summaries)}")
        parts.append("")

        # File summaries (compact format)
        parts.append("## File Summaries")
        parts.append("")

        for summary in input_data.file_summaries:
            parts.append(f"### {summary.program_id} ({summary.file_name})")
            parts.append(f"- Type: {summary.program_type}")
            parts.append(f"- Purpose: {summary.purpose_summary or 'Not specified'}")
            parts.append(f"- Paragraphs: {summary.paragraph_count}")
            if summary.main_calls:
                parts.append(f"- Calls: {', '.join(summary.main_calls[:5])}")
            if summary.main_inputs:
                parts.append(f"- Inputs: {', '.join(summary.main_inputs[:5])}")
            if summary.main_outputs:
                parts.append(f"- Outputs: {', '.join(summary.main_outputs[:5])}")
            parts.append(f"- Confidence: {summary.confidence.value}")
            if summary.has_open_questions:
                parts.append("- ⚠️ Has open questions")
            parts.append("")

        # Call graph (topology only, no Mermaid)
        if input_data.call_graph:
            parts.append("## Call Graph")
            for program, calls in sorted(input_data.call_graph.items()):
                parts.append(f"- {program} → {', '.join(calls)}")
            parts.append("")

        # Shared copybooks
        if input_data.shared_copybooks:
            parts.append("## Shared Copybooks")
            for copybook, users in sorted(input_data.shared_copybooks.items()):
                parts.append(f"- {copybook}: {', '.join(users)}")
            parts.append("")

        # Quality indicators
        if input_data.files_with_issues:
            parts.append(f"## Files with Issues ({len(input_data.files_with_issues)})")
            for file_name in input_data.files_with_issues:
                parts.append(f"- {file_name}")
            parts.append("")

        # Previous cycle summary (compact)
        if input_data.previous_clarification_count > 0:
            parts.append(
                f"Previous cycles: {input_data.previous_clarification_count} "
                f"clarifications, {input_data.unresolved_issues_count} unresolved"
            )
            parts.append("")

        # Task instructions
        parts.append("## Task")
        parts.append("1. Review file summaries for cross-program consistency")
        parts.append("2. Check call graph for missing documentation")
        parts.append("3. Assess overall quality from confidence levels")
        parts.append("4. Decide: SATISFIED, NEEDS_CLARIFICATION, or FORCED_COMPLETE")

        return "\n".join(parts)

    def _parse_compact_holistic_response(
        self,
        response: str,
        input_data: HolisticReviewInputCompact,
    ) -> HolisticReviewOutput:
        """Parse the LLM response from compact holistic review.

        Args:
            response: Raw text response from the LLM.
            input_data: Original compact input.

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

            # Build set of valid file names from input summaries
            valid_file_names = {s.file_name for s in input_data.file_summaries}

            # Parse file feedback
            file_feedback: dict[str, list[ChromeTicket]] = {}
            if "file_feedback" in data and isinstance(data["file_feedback"], dict):
                for file_name, issues in data["file_feedback"].items():
                    # Validate file name
                    if file_name not in valid_file_names:
                        logger.warning(
                            f"Skipping feedback for unknown file: {file_name}"
                        )
                        continue

                    # Find program_id from summaries
                    program_id = file_name
                    for summary in input_data.file_summaries:
                        if summary.file_name == file_name:
                            program_id = summary.program_id
                            break

                    file_feedback[file_name] = []
                    for issue in issues:
                        issue["program_id"] = program_id
                        try:
                            file_feedback[file_name].append(
                                ChromeTicket.model_validate(issue)
                            )
                        except Exception as e:
                            logger.warning(f"Failed to parse Chrome ticket: {e}")

            # Parse consistency issues
            consistency_issues = []
            if "consistency_issues" in data:
                for issue in data["consistency_issues"]:
                    try:
                        consistency_issues.append(
                            ConsistencyIssue.model_validate(issue)
                        )
                    except Exception as e:
                        logger.warning(f"Failed to parse consistency issue: {e}")

            return HolisticReviewOutput(
                success=True,
                decision=decision,
                file_feedback=file_feedback,
                consistency_issues=consistency_issues,
                clarification_requests=[],  # Compact review doesn't generate these
                assumptions=[],  # Compact review doesn't generate these
                overall_quality=data.get("overall_quality", "ACCEPTABLE"),
                quality_notes=data.get("quality_notes", []),
                quality_notes_structured=[],
                priority_files=data.get("priority_files", []),
                missing_documentation=[],
                reasoning=data.get("reasoning", ""),
            )

        except Exception as e:
            logger.error(f"Failed to parse compact holistic response: {e}")
            logger.debug(f"Response was: {response[:500]}...")

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

    async def holistic_review_compact(
        self,
        input_data: HolisticReviewInputCompact,
        use_mock: bool = False,
    ) -> HolisticReviewOutput:
        """Perform compact holistic review using Tier 1 summaries.

        This is an optimized version of holistic_review() that uses
        FileDocumentationSummary instead of full templates, reducing
        token usage by ~70-85%.

        Does NOT generate README.md (that requires full documentation).
        For README generation, use the standard holistic_review() method.

        Args:
            input_data: The compact holistic review input.
            use_mock: If True, return mock output instead of calling LLM.

        Returns:
            HolisticReviewOutput with decision and feedback.
        """
        if use_mock:
            # Create mock output based on compact input
            return self._create_mock_compact_holistic_output(input_data)

        try:
            # Build prompts for compact holistic review
            system_prompt = self._build_compact_holistic_system_prompt()
            user_prompt = self._build_compact_holistic_user_prompt(input_data)

            # Log token estimation
            total_chars = len(system_prompt) + len(user_prompt)
            estimated_tokens = total_chars // 4
            logger.info(
                f"Imperator compact review: ~{estimated_tokens:,} tokens "
                f"({total_chars:,} chars) for {self.config.model}"
            )

            # Call LLM
            response = await self._call_llm(system_prompt, user_prompt)

            # Parse response
            return self._parse_compact_holistic_response(response, input_data)

        except Exception as e:
            logger.error(f"Compact holistic review failed: {e}")
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

    def _create_mock_compact_holistic_output(
        self,
        input_data: HolisticReviewInputCompact,
    ) -> HolisticReviewOutput:
        """Create mock output for compact holistic review testing.

        Args:
            input_data: The compact holistic review input.

        Returns:
            Mock HolisticReviewOutput.
        """
        # Decide based on cycle
        if input_data.cycle >= input_data.max_cycles:
            decision = ImperatorHolisticDecision.SATISFIED
        elif input_data.cycle == 1:
            decision = ImperatorHolisticDecision.NEEDS_CLARIFICATION
        else:
            decision = ImperatorHolisticDecision.SATISFIED

        # Generate mock file feedback if NEEDS_CLARIFICATION
        file_feedback: dict[str, list[ChromeTicket]] = {}
        if decision == ImperatorHolisticDecision.NEEDS_CLARIFICATION:
            if input_data.file_summaries:
                first = input_data.file_summaries[0]
                file_feedback[first.file_name] = [
                    ChromeTicket(
                        program_id=first.program_id,
                        section="purpose",
                        issue_type=IssueType.VAGUE,
                        description="[MOCK] Purpose needs more business context",
                        guidance="Add specific business process details",
                        priority=IssuePriority.MEDIUM,
                    )
                ]

        return HolisticReviewOutput(
            success=True,
            decision=decision,
            file_feedback=file_feedback,
            consistency_issues=[],
            clarification_requests=[],
            assumptions=[],
            overall_quality="ACCEPTABLE" if decision != ImperatorHolisticDecision.SATISFIED else "GOOD",
            quality_notes=[
                "[MOCK] Compact holistic review",
                f"[MOCK] Reviewed {len(input_data.file_summaries)} file summaries",
            ],
            priority_files=[],
            missing_documentation=[],
            reasoning=f"[MOCK] Compact review decision after cycle {input_data.cycle}",
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
        """Read existing README.md content if it exists.

        This utility method checks for an existing system design document
        and returns its content for iterative refinement.

        Args:
            output_dir: Directory where README.md would be located.

        Returns:
            The content of README.md as a string, or None if:
            - The file does not exist
            - A read error occurred (logged as warning)
        """
        system_design_path = output_dir / "README.md"

        if not system_design_path.exists():
            return None

        try:
            return system_design_path.read_text(encoding="utf-8")
        except OSError as e:
            logger.warning(
                f"Failed to read existing README.md at {system_design_path}: {e}"
            )
            return None

    def _extract_mermaid_from_markdown(self, markdown: str) -> str | None:
        """Extract the Mermaid diagram block from markdown content.

        Searches for a ```mermaid ... ``` code block and returns the complete
        block including the fence markers.

        Args:
            markdown: The markdown content to search.

        Returns:
            The complete Mermaid code block with fences, or None if not found.
        """
        import re

        # Find ```mermaid ... ``` block (handles both \n and any whitespace)
        match = re.search(r"```mermaid\n(.*?)\n```", markdown, re.DOTALL)
        if match:
            return f"```mermaid\n{match.group(1)}\n```"
        return None

    def _build_system_design_prompt(
        self,
        input_data: HolisticReviewInput,
        existing_content: str | None = None,
    ) -> str:
        """Build the prompt for system design document generation.

        Creates a prompt instructing the LLM to generate a comprehensive
        README.md document based on the file documentation and
        call graph information.

        Args:
            input_data: The holistic review input containing file documentation,
                call graph, and other system information.
            existing_content: If provided, the LLM will enhance/update this
                existing content rather than creating from scratch.

        Returns:
            The user prompt for the LLM to generate README.md content.
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
                "**CRITICAL: An existing README.md document is provided below.**"
            )
            parts.append("")
            parts.append(
                "You MUST **preserve ALL existing content** and **incrementally enhance** it. "
                "Do NOT rewrite from scratch. Do NOT remove sections or details. Instead:"
            )
            parts.append("- **Add** new information discovered in the latest documentation")
            parts.append("- **Expand** sections with more detail and context")
            parts.append("- **Clarify** ambiguous areas based on new understanding")
            parts.append("- **Resolve** previously marked questions if answers are now available")
            parts.append("- **Cross-reference** by adding links to component documentation")
            parts.append("")
            parts.append(
                "The resulting document should be **strictly longer and more detailed** "
                "than the input document. Information loss is unacceptable."
            )
        else:
            parts.append(
                "Create a comprehensive, detailed README.md document that describes the"
                "overall system architecture based on the program documentation provided below."
            )
            parts.append("")
            parts.append(
                "Be **verbose and thorough** - this document should serve as the definitive "
                "architectural reference. Include all relevant details, not just summaries."
            )
        parts.append("")

        parts.append("## Required Document Structure (Comprehensive)")
        parts.append("")
        parts.append("Generate a detailed markdown document with the following sections. ")
        parts.append("Each section should be **thorough and verbose** - this is the definitive reference.")
        parts.append("")
        parts.append("### 1. Executive Summary (MANDATORY - NEVER SKIP)")
        parts.append("")
        parts.append("**CRITICAL REQUIREMENT**: This section MUST contain a comprehensive summary of ")
        parts.append("AT LEAST 20 SENTENCES demonstrating deep understanding of the system.")
        parts.append("")
        parts.append("The Executive Summary must include:")
        parts.append("- **System Purpose** (4-5 sentences): What business problem does this system solve? ")
        parts.append("  What is its primary mission? Who are the users and stakeholders?")
        parts.append("- **Functional Overview** (5-6 sentences): What are the major capabilities? ")
        parts.append("  What transactions or processes does it handle? What are the key workflows?")
        parts.append("- **Technical Foundation** (4-5 sentences): What technologies power this system? ")
        parts.append("  (COBOL, PL/I, DB2, CICS, IMS, JCL, etc.) How do they work together?")
        parts.append("- **System Boundaries** (3-4 sentences): What are the inputs and outputs? ")
        parts.append("  What external systems does it integrate with? What are the data sources and sinks?")
        parts.append("- **Business Value** (3-4 sentences): Why is this system important to the organization? ")
        parts.append("  What would happen if it stopped working? What business metrics does it support?")
        parts.append("")
        parts.append("**DO NOT write a superficial summary. You must demonstrate that you understand ")
        parts.append("HOW the system works, not just WHAT files exist. Connect the components into a ")
        parts.append("coherent narrative that a new developer could use to understand the system.**")
        parts.append("")
        parts.append("### 2. Architecture Overview")
        parts.append("- Detailed high-level system architecture description")
        parts.append("- All entry points and interfaces with their purposes")
        parts.append("- Integration patterns (batch, online, database, messaging)")
        parts.append("- Architectural patterns identified (layers, tiers, pipelines)")
        if input_data.call_graph_markdown:
            parts.append("- **IMPORTANT**: Use the exact Mermaid diagram provided in the 'Actual System Call Graph' section below")
            parts.append("- Do NOT create your own diagram - use the provided one which is based on actual static analysis")
        else:
            parts.append("- ASCII or Mermaid diagrams showing component relationships")
        parts.append("")
        parts.append("### 3. Component Catalog")
        parts.append("**Create a comprehensive table of ALL components with markdown links:**")
        parts.append("")
        parts.append("| Component | Type | Purpose | Dependencies | Doc Link |")
        parts.append("|-----------|------|---------|--------------|----------|")
        parts.append("| PROGNAME  | COBOL| Brief   | PROG2, PROG3 | [Link](PROGNAME.cbl.md) |")
        parts.append("| JOBNAME   | JCL  | Brief   | PROG1        | [Link](JOBNAME.jcl.md) |")
        parts.append("| CPYNAME   | COPY | Brief   | -            | [Link](CPYNAME.cpy.md) |")
        parts.append("")
        parts.append("Include EVERY program, JCL, copybook, and procedure documented.")
        parts.append("Links must include the source file extension: PROG.cbl.md, JOB.jcl.md, CPY.cpy.md")
        parts.append("")
        parts.append("### 4. Subsystem Breakdown")
        parts.append("- Logical groupings of programs by business function")
        parts.append("- Batch processing subsystems with job flows")
        parts.append("- Online/CICS transaction processing areas")
        parts.append("- Shared services, utilities, and common routines")
        parts.append("- For each subsystem: detailed description with component links")
        parts.append("")
        parts.append("### 5. Data Architecture")
        parts.append("- All data stores (files, databases, queues)")
        parts.append("- Data flow diagrams showing how data moves between components")
        parts.append("- Input sources with formats and frequencies")
        parts.append("- Output destinations with formats and consumers")
        parts.append("- Key data structures and their usage patterns")
        parts.append("")
        parts.append("### 6. Integration Points")
        parts.append("- External system interfaces")
        parts.append("- Batch job dependencies and scheduling")
        parts.append("- Database connections and access patterns")
        parts.append("- File transfers and data exchanges")
        parts.append("")
        parts.append("### 7. Business Rules Summary")
        parts.append("- Compile ALL business rules from component documentation")
        parts.append("- Group by business domain or function")
        parts.append("- Link each rule to its source component")
        parts.append("")
        parts.append("### 8. Error Handling Patterns")
        parts.append("- Common error handling approaches in the system")
        parts.append("- Recovery procedures and restart logic")
        parts.append("- Logging and monitoring patterns")
        parts.append("")
        parts.append("### 9. Open Questions and Uncertainties")
        parts.append("- Consolidate all ❓ QUESTION markers")
        parts.append("- Areas needing further investigation")
        parts.append("- Assumptions made and their implications")
        parts.append("")

        parts.append("## Critical Guidelines")
        parts.append("")
        parts.append("**EXECUTIVE SUMMARY IS MANDATORY**: The Executive Summary section MUST be at least ")
        parts.append("20 sentences long and demonstrate genuine understanding of the system. A placeholder ")
        parts.append("or superficial summary is NOT acceptable. This is the most important section.")
        parts.append("")
        parts.append("**VERBOSITY**: Write detailed, comprehensive content. Every section should be ")
        parts.append("thoroughly explained. A longer document is better than a sparse one.")
        parts.append("")
        parts.append("**LINKING**: Every time you mention a component (program, JCL, copybook), ")
        parts.append("include a markdown link to its documentation. Links MUST include the source extension:")
        parts.append("  - COBOL: `[PROGNAME](path/PROGNAME.cbl.md)`")
        parts.append("  - JCL: `[JOBNAME](path/JOBNAME.jcl.md)`")
        parts.append("  - Copybook: `[CPYNAME](path/CPYNAME.cpy.md)`")
        parts.append("")
        parts.append("**QUESTIONS**: Insert inline questions wherever your understanding is unclear ")
        parts.append("or information is missing. Mark these with: `❓ QUESTION: [your question here]`")
        parts.append("")
        parts.append("**PRESERVATION**: If enhancing an existing document, preserve ALL existing content. ")
        parts.append("Add to it, don't replace it. The document should only grow richer.")
        parts.append("")
        parts.append("**COMPLETENESS**: Include information about ALL documented components, not just ")
        parts.append("the most important ones. Every component deserves mention and a link.")
        parts.append("")

        # Build documentation path reference table for linking
        parts.append("## Documentation File Paths (for linking)")
        parts.append("")
        parts.append(
            "When referencing these components in the README.md, use markdown links."
            "Links MUST include the source file extension. Examples: "
            "`[PROG](PROG.cbl.md)`, `[JOB](JOB.jcl.md)`, `[CPY](CPY.cpy.md)`"
        )
        parts.append("")
        parts.append("| Component | Doc Path |")
        parts.append("|-----------|----------|")

        if input_data.file_documentation:
            for doc in input_data.file_documentation:
                # Compute documentation path from file_name
                from pathlib import Path as PathLib
                rel_path = PathLib(doc.file_name)
                if rel_path.parent != PathLib("."):
                    doc_path = f"{rel_path.parent}/{rel_path.name}.md"
                else:
                    doc_path = f"{rel_path.name}.md"
                parts.append(f"| {doc.program_id} | `{doc_path}` |")
        parts.append("")

        # Include detailed file documentation
        parts.append("## Program Documentation (Full Details)")
        parts.append("")
        parts.append(
            "Below is comprehensive documentation for each component. "
            "Include ALL relevant details in the README.md and link to each component."
        )
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
                parts.append(f"### {ptype} Components ({len(docs)})")
                parts.append("")

                for doc in docs:
                    # Compute doc path for this file
                    from pathlib import Path as PathLib
                    rel_path = PathLib(doc.file_name)
                    if rel_path.parent != PathLib("."):
                        doc_path = f"{rel_path.parent}/{rel_path.name}.md"
                    else:
                        doc_path = f"{rel_path.name}.md"

                    parts.append(f"#### {doc.program_id}")
                    parts.append(f"- **Source File**: `{doc.file_name}`")
                    parts.append(f"- **Documentation**: `{doc_path}`")
                    parts.append(f"- **Link Format**: `[{doc.program_id}]({doc_path})`")
                    parts.append("")
                    parts.append(f"**Summary**: {doc.template.purpose.summary}")
                    parts.append("")

                    if doc.template.purpose.business_context:
                        parts.append(f"**Business Context**: {doc.template.purpose.business_context}")
                        parts.append("")

                    # Called programs - show ALL
                    if doc.template.called_programs:
                        parts.append("**Programs Called**:")
                        for cp in doc.template.called_programs:
                            purpose = f" - {cp.purpose}" if cp.purpose else ""
                            parts.append(f"  - `{cp.program_name}`{purpose}")
                        parts.append("")

                    # Calling context - show ALL
                    if doc.template.calling_context.called_by:
                        parts.append(f"**Called By**: {', '.join(doc.template.calling_context.called_by)}")
                        parts.append("")

                    # Inputs - show ALL with descriptions
                    if doc.template.inputs:
                        parts.append("**Inputs**:")
                        for inp in doc.template.inputs:
                            io_type = inp.io_type.value if inp.io_type else "unknown"
                            desc = f" - {inp.description}" if inp.description else ""
                            parts.append(f"  - `{inp.name}` ({io_type}){desc}")
                        parts.append("")

                    # Outputs - show ALL with descriptions
                    if doc.template.outputs:
                        parts.append("**Outputs**:")
                        for out in doc.template.outputs:
                            io_type = out.io_type.value if out.io_type else "unknown"
                            desc = f" - {out.description}" if out.description else ""
                            parts.append(f"  - `{out.name}` ({io_type}){desc}")
                        parts.append("")

                    # Business rules - show ALL
                    if doc.template.business_rules:
                        parts.append("**Business Rules**:")
                        for rule in doc.template.business_rules:
                            rule_desc = rule.description or "No description"
                            parts.append(f"  - {rule_desc}")
                            if rule.logic_summary:
                                parts.append(f"    *Logic: {rule.logic_summary}*")
                        parts.append("")

                    # Copybooks - show ALL
                    if doc.template.copybooks_used:
                        parts.append("**Copybooks Used**:")
                        for cb in doc.template.copybooks_used:
                            purpose = f" - {cb.purpose}" if cb.purpose else ""
                            parts.append(f"  - `{cb.copybook_name}`{purpose}")
                        parts.append("")

                    # Error handling
                    if doc.template.error_handling:
                        parts.append("**Error Handling**:")
                        for eh in doc.template.error_handling:
                            condition = eh.condition or "Unknown condition"
                            action = eh.action or "Unknown action"
                            parts.append(f"  - **{condition}**: {action}")
                        parts.append("")

                    parts.append("---")
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

        # Add actual call graph diagram from Citadel analysis
        if input_data.call_graph_markdown:
            parts.append("## Actual System Call Graph (from static analysis)")
            parts.append("")
            parts.append(
                "**IMPORTANT**: Use this exact Mermaid diagram in the Architecture Overview section."
            )
            parts.append(
                "Do NOT create your own diagram - use this one which is based on actual code analysis:"
            )
            parts.append("")
            # Extract just the mermaid block from the call graph markdown
            mermaid_diagram = self._extract_mermaid_from_markdown(
                input_data.call_graph_markdown
            )
            if mermaid_diagram:
                parts.append(mermaid_diagram)
            else:
                # If no mermaid block found, log a warning but include the full content
                logger.warning(
                    "No Mermaid diagram found in call_graph_markdown, "
                    "including full content for reference"
                )
                parts.append("```")
                parts.append(input_data.call_graph_markdown)
                parts.append("```")
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
            parts.append("## Existing README.md Content")
            parts.append("")
            parts.append(
                "Below is the current content of README.md. Enhance and update"
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
            "Output ONLY the markdown content for README.md."
            "Do not wrap in JSON or add any preamble."
        )

        return "\n".join(parts)

    async def generate_system_design(
        self,
        input_data: HolisticReviewInput,
        existing_content: str | None = None,
        use_mock: bool = False,
        sequence_diagrams: list[str] | None = None,
    ) -> SystemDesignOutput:
        """Generate a README.md document from batch documentation.

        This method synthesizes individual file documentation into a comprehensive
        system design document. It can either create a new document or enhance
        an existing one based on the provided content.

        Args:
            input_data: Holistic review input containing file documentation,
                call graph, and other system information.
            existing_content: If provided, the document will be enhanced/updated
                rather than created from scratch.
            use_mock: If True, return mock output without calling the LLM.
            sequence_diagrams: Optional list of mermaid sequence diagram strings
                from citadel to append as a Flows section.

        Returns:
            SystemDesignOutput with the generated markdown and any identified
            questions requiring clarification.
        """
        if use_mock:
            return self._create_mock_system_design(input_data, existing_content)

        try:
            system_prompt = """You are a technical architect creating a comprehensive, detailed system design document.

Your task is to synthesize individual program documentation into a README.md
document that serves as the authoritative architectural reference for this system.

## CRITICAL REQUIREMENT: Executive Summary

The Executive Summary is the MOST IMPORTANT section and MUST NOT be skipped or abbreviated.
It MUST contain AT LEAST 20 SENTENCES demonstrating deep, genuine understanding of the system.

A proper Executive Summary covers:
- System Purpose (4-5 sentences): What business problem it solves, its mission, users
- Functional Overview (5-6 sentences): Major capabilities, transactions, key workflows
- Technical Foundation (4-5 sentences): Technologies used and how they work together
- System Boundaries (3-4 sentences): Inputs, outputs, external integrations
- Business Value (3-4 sentences): Why it matters, impact if unavailable

DO NOT write placeholder text like "This system processes data." You must show that you
understand HOW the system works by connecting components into a coherent narrative.

## Core Principles

**INCREMENTAL ENHANCEMENT**: If existing content is provided, you MUST preserve and enhance it.
Never discard valuable information from previous versions. Add to it, clarify it, and
integrate new information seamlessly. The document should grow richer with each iteration.

**MAXIMUM VERBOSITY**: Be thorough and detailed. Include:
- Full explanations of architectural decisions and patterns
- Detailed descriptions of each subsystem and component
- Comprehensive data flow narratives
- Complete interface specifications
- All business rules and constraints discovered

**CROSS-REFERENCING WITH LINKS**: For every component (program, JCL, copybook) you mention,
include a markdown link to its documentation file. Links MUST include the source extension:
- COBOL: [PROGRAM_NAME](PROGRAM.cbl.md)
- JCL: [JOB_NAME](JOB.jcl.md)
- Copybook: [COPY_NAME](COPY.cpy.md)
This creates a navigable documentation web.

## Writing Style
- Write for a technical audience (developers, architects, maintainers)
- Be comprehensive - include all relevant details, not just summaries
- Use tables for structured comparisons and listings
- Use diagrams (in ASCII or mermaid syntax) where helpful
- Mark uncertainties with inline questions using the format: ❓ QUESTION: [your question]
- Be explicit about assumptions and information gaps"""

            user_prompt = self._build_system_design_prompt(input_data, existing_content)

            # Call LLM
            response = await self._call_llm(system_prompt, user_prompt)

            # Strip outer code fence wrapper if the LLM wrapped its response
            # in ```markdown ... ``` (which breaks inner mermaid blocks)
            response = self._strip_outer_code_fence(response)

            # Append Flows section with sequence diagrams if provided
            final_markdown = response
            if sequence_diagrams:
                flows_section = self._format_flows_section(sequence_diagrams)
                final_markdown = response.rstrip() + "\n\n" + flows_section

            # Parse inline questions from the markdown
            questions = self._parse_inline_questions(final_markdown, input_data.cycle)

            # Identify which sections were updated (look for main headers)
            sections_updated = self._extract_sections(final_markdown)

            return SystemDesignOutput(
                success=True,
                markdown=final_markdown,
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

    @staticmethod
    def _strip_outer_code_fence(text: str) -> str:
        """Strip an outer markdown/text code fence wrapping the LLM response.

        LLMs sometimes wrap their entire response in a code fence like:

            ```markdown
            # Actual Content
            ...inner ```mermaid blocks...
            ```

        This causes inner fenced blocks (e.g. mermaid diagrams) to be treated
        as literal text instead of being rendered. This method strips only the
        outermost fence, preserving all inner content intact.

        Recognized fence tags: ``markdown``, ``md``, ``text``, or bare ``````.

        Args:
            text: The raw LLM response string.

        Returns:
            The content with the outer code fence removed, or the original
            text unchanged if no outer fence was detected.
        """
        stripped = text.strip()
        # Match opening fence: ``` optionally followed by markdown/md/text
        # The opening fence must be at the very start of the (stripped) text.
        match = re.match(
            r"^```(?:markdown|md|text)?[ \t]*\r?\n", stripped, re.IGNORECASE
        )
        if not match:
            return text

        # The closing fence must be at the very end (possibly with trailing whitespace)
        if not re.search(r"\n```[ \t]*$", stripped):
            return text

        # Remove the first line (opening fence) and the last closing fence line
        inner = stripped[match.end():]
        # Remove trailing ``` (last occurrence at end of string)
        inner = re.sub(r"\n```[ \t]*$", "", inner)
        return inner

    def _format_flows_section(self, sequence_diagrams: list[str]) -> str:
        """Format sequence diagrams as a Flows section for README.md.

        Args:
            sequence_diagrams: List of mermaid sequence diagram strings.

        Returns:
            Formatted markdown Flows section with all diagrams.
        """
        if not sequence_diagrams:
            return ""

        lines = []
        lines.append("## Flows")
        lines.append("")
        lines.append(
            "The following sequence diagrams illustrate key call sequences "
            "identified in the codebase, showing how programs interact "
            "during execution."
        )
        lines.append("")

        for idx, diagram in enumerate(sequence_diagrams, start=1):
            lines.append(f"### Flow {idx}")
            lines.append("")
            lines.append("```mermaid")
            # The diagram may already have mermaid fence, strip if so
            diagram_content = diagram.strip()
            if diagram_content.startswith("```mermaid"):
                diagram_content = diagram_content[len("```mermaid"):].strip()
            if diagram_content.endswith("```"):
                diagram_content = diagram_content[:-3].strip()
            lines.append(diagram_content)
            lines.append("```")
            lines.append("")

        return "\n".join(lines)

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
