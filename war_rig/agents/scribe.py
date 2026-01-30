"""Scribe agent for documentation generation.

The Scribe is the documenter War Boy. It analyzes mainframe source code
and fills out the documentation template, citing line numbers for every
factual claim.

Responsibilities:
- Analyze code structure and purpose
- Fill out all template sections
- Cite line numbers for claims
- Mark unknowns as UNKNOWN with explanation
- Respond to Challenger questions
- Address Chrome tickets from Imperator
"""

import json
import logging
import re

from pydantic import Field, ValidationError

from war_rig.agents.base import AgentInput, AgentOutput, BaseAgent
from war_rig.config import APIConfig, ScribeConfig
from war_rig.models.assessments import ConfidenceAssessment, ConfidenceLevel
from war_rig.models.templates import DocumentationTemplate, FileType
from war_rig.models.tickets import ChallengerQuestion, ChromeTicket, ScribeResponse
from war_rig.preprocessors.base import PreprocessorResult

logger = logging.getLogger(__name__)


class ScribeInput(AgentInput):
    """Input data for the Scribe agent.

    Contains all information needed to generate or update documentation.
    """

    source_code: str = Field(
        ...,
        description="The source code to analyze",
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
    copybook_contents: dict[str, str] = Field(
        default_factory=dict,
        description="Resolved copybook contents (name -> content)",
    )
    previous_template: DocumentationTemplate | None = Field(
        default=None,
        description="Previous iteration's template (for updates)",
    )
    challenger_questions: list[ChallengerQuestion] = Field(
        default_factory=list,
        description="Questions from Challenger to address",
    )
    chrome_tickets: list[ChromeTicket] = Field(
        default_factory=list,
        description="Chrome tickets from Imperator to address",
    )
    formatting_strict: bool = Field(
        default=False,
        description="If True, add extra instructions about JSON formatting (used on retry)",
    )
    feedback_context: dict | None = Field(
        default=None,
        description="FeedbackContext from Imperator review with quality notes to address",
    )
    citadel_outline: list[dict] | None = Field(
        default=None,
        description="Citadel paragraph outline with names, line ranges, and calls for guided documentation",
    )
    pattern_insights: dict | None = Field(
        default=None,
        description="Aggregated analysis pattern insights to guide documentation (from PatternAggregator)",
    )


class ScribeOutput(AgentOutput):
    """Output from the Scribe agent.

    Contains the generated documentation and metadata.
    """

    template: DocumentationTemplate | None = Field(
        default=None,
        description="The completed documentation template",
    )
    confidence: ConfidenceAssessment | None = Field(
        default=None,
        description="Scribe's confidence assessment",
    )
    responses: list[ScribeResponse] = Field(
        default_factory=list,
        description="Responses to Challenger questions",
    )
    open_questions: list[str] = Field(
        default_factory=list,
        description="Questions Scribe could not resolve",
    )
    responses_incomplete: bool = Field(
        default=False,
        description="True if any ScribeResponse had parsing errors and used lenient fallback. "
        "When True, the ticket should NOT be marked as complete to allow retry.",
    )
    needs_revalidation: bool = Field(
        default=False,
        description="True if the ticket (CLARIFICATION/CHROME) had no questions/issues to process. "
        "When True, the ticket should be marked complete and a new VALIDATION ticket created "
        "so a Challenger can re-review the documentation from scratch.",
    )
    discovery_result: dict | None = Field(
        default=None,
        description="Result of discovery ticket processing. Contains status (internal_routine, "
        "external), program_id, and resolution details. Only set for discovery tickets.",
    )


class ScribeAgent(BaseAgent[ScribeInput, ScribeOutput]):
    """The Scribe agent analyzes code and produces documentation.

    The Scribe's role is to:
    1. First pass: Analyze code and fill template completely
    2. Subsequent passes: Address Chrome tickets and Challenger questions
    3. Always cite line numbers for factual claims
    4. Mark unknowns explicitly rather than guessing

    Example:
        config = ScribeConfig(model="claude-sonnet-4-20250514", temperature=0.3)
        scribe = ScribeAgent(config)

        output = await scribe.ainvoke(ScribeInput(
            source_code=cobol_source,
            file_name="PROGRAM.cbl",
            file_type=FileType.COBOL,
        ))
        print(output.template.purpose.summary)
    """

    def __init__(
        self,
        config: ScribeConfig,
        api_config: APIConfig | None = None,
    ):
        """Initialize the Scribe agent.

        Args:
            config: Scribe-specific configuration.
            api_config: API configuration. If None, loads from environment.
        """
        super().__init__(config, api_config, name="Scribe")

    def _build_system_prompt(self) -> str:
        """Build the Scribe's system prompt.

        Returns:
            The system prompt defining Scribe's role and behavior.
        """
        return """You are the Scribe, a documentation specialist for mainframe code analysis.

Your role is to analyze COBOL, PL/I, JCL, and other mainframe source files and produce comprehensive documentation following a structured template.

## Special File Types

### LISTING Files (.lst, .list)
LISTING files are NOT executable programs. They contain **prose documentation** such as:
- Compiler output and diagnostics
- Change history and modification notes
- System documentation and comments
- Version control notes
- Developer notes and explanations

When analyzing a LISTING file:
1. The summary should describe what the file CONTAINS (change history, compiler output, notes), not what code it "executes"
2. Do NOT try to document called_programs, data_flow, sql_operations, cics_operations - these are not applicable
3. Focus on purpose (what documentation is in the file), and any inputs/outputs mentioned as references
4. Treat the content as prose to be understood and summarized, not as code to be reverse-engineered

## Core Principles

1. **Accuracy over completeness**: Never invent information. If something cannot be determined from the code, mark it as UNKNOWN with an explanation.

2. **Citation required**: Every factual claim must include line number citations from the source code.

3. **Be specific**: Avoid generic descriptions. "Processes customer data" is bad. "Reads CUSTOMER-MASTER file, validates account status, and updates TRANSACTION-LOG" is good.

4. **Address all sections**: Even if a section is not applicable, explicitly note this rather than leaving it empty.

## Required JSON Schema

You MUST respond with valid JSON matching this EXACT structure:

```json
{
  "template": {
    "header": {
      "program_id": "PROGRAM_NAME",
      "file_name": "filename.cbl",
      "file_type": "COBOL",  // One of: COBOL, PLI, JCL, COPYBOOK, PROC, BMS, LISTING, OTHER
      "analyzed_by": "WAR_RIG",
      "iteration_count": 1
    },
    "purpose": {
      "summary": "2-3 sentence description of what this program does",
      "business_context": "What business process this serves (or null)",
      "program_type": "BATCH",  // One of: BATCH, ONLINE_CICS, SUBROUTINE, UTILITY
      "citations": [1, 5, 10]  // Line numbers supporting the summary
    },
    "inputs": [
      {
        "name": "FILE-NAME or TABLE-NAME",
        "io_type": "FILE_VSAM",  // One of: FILE_SEQUENTIAL, FILE_VSAM, DB2_TABLE, IMS_SEGMENT, PARAMETER, CICS_COMMAREA, CICS_MAP, CICS_QUEUE, REPORT, RETURN_CODE, OTHER
        "description": "What this input contains",
        "copybook": "COPYBOOK-NAME or null",
        "citation": [100, 200]  // List of integers (line numbers where read)
      }
    ],
    "outputs": [
      {
        "name": "OUTPUT-FILE",
        "io_type": "FILE_SEQUENTIAL",
        "description": "What this output contains",
        "copybook": null,
        "citation": [300]  // List of integers (line numbers where written)
      }
    ],
    "called_programs": [
      {
        "program_name": "CALLED-PGM",
        "call_type": "STATIC_CALL",  // One of: STATIC_CALL, DYNAMIC_CALL, CICS_LINK, CICS_XCTL
        "purpose": "Why this program is called",
        "parameters": ["PARAM1", "PARAM2"],
        "citation": 150  // Single integer, NOT a list
      }
    ],
    "calling_context": {
      "called_by": [],  // Programs that call this (may be empty)
      "entry_points": [],  // Transaction IDs for CICS
      "linkage_section": []  // Key LINKAGE fields
    },
    "business_rules": [
      {
        "rule_id": "BR001",
        "description": "Plain English description of the rule",
        "logic_summary": "Brief explanation of implementation",
        "conditions": ["IF CONDITION-1", "IF CONDITION-2"],
        "citation": [200, 250]  // List of integers
      }
    ],
    "data_flow": {
      "reads_from": [{"source": "SOURCE-NAME", "fields_used": ["FIELD1"], "citation": [100]}],
      "writes_to": [{"destination": "DEST-NAME", "fields_written": ["FIELD1"], "citation": [200]}],
      "transforms": [
        {
          "input_field": "INPUT-FIELD",
          "output_field": "OUTPUT-FIELD",
          "transformation_description": "Description of transformation",
          "citation": [150]
        }
      ]
    },
    "copybooks_used": [
      {
        "copybook_name": "COPY-NAME",
        "purpose": "What data structure it defines",
        "location": "WORKING_STORAGE",  // One of: WORKING_STORAGE, LINKAGE, FILE_SECTION, LOCAL_STORAGE, OTHER
        "citation": 50  // Single integer, NOT a list
      }
    ],
    "paragraphs": [
      {
        "paragraph_name": "0000-MAIN",
        "purpose": "DETAILED DESCRIPTION REQUIRED (5-10 sentences minimum). Must include: (1) The paragraph's primary purpose and role in the program flow. (2) What inputs/data it consumes and from where. (3) What outputs/data it produces and where they go. (4) The business logic implemented - what decisions are made and why. (5) Any error handling or validation performed. (6) What other paragraphs/programs it calls and why. Example: 'This is the main orchestration paragraph that controls the overall program flow. It begins by performing 1000-INIT to open files and initialize working storage variables. After initialization, it enters a processing loop controlled by WS-EOF-FLAG, repeatedly calling 2000-PROCESS for each input record. The 2000-PROCESS paragraph handles individual customer records, validating the account status before updating. Once EOF is reached, control passes to 9000-TERMINATE for file closure and return code setting. Error conditions from any subordinate paragraph trigger an immediate branch to 9999-ABEND.'",
        "called_by": [],
        "calls": ["1000-INIT", "2000-PROCESS"],
        "citation": [100, 150]  // EXACTLY 2 integers: [start_line, end_line]
      }
    ],
    "error_handling": [
      {
        "condition": "FILE STATUS NOT = '00'",
        "action": "ABEND with code 100",
        "citation": [500]
      }
    ],
    "sql_operations": [
      {
        "operation": "SELECT",
        "table": "TABLE_NAME",
        "purpose": "Why this operation is performed",
        "citation": 300  // Single integer, NOT a list
      }
    ],
    "cics_operations": [
      {
        "command": "SEND MAP",
        "resource": "MAP-NAME",
        "purpose": "Display screen to user",
        "citation": 400  // Single integer, NOT a list
      }
    ],
    "open_questions": [
      {
        "question": "What remains unclear",
        "context": "Why it could not be determined",
        "suggestion": "How it might be resolved"
      }
    ]
  },
  "confidence": {
    "program_id": "PROGRAM_NAME",
    "iteration": 1,
    "overall_confidence": "MEDIUM",  // One of: HIGH, MEDIUM, LOW
    "reasoning": "Explanation of confidence level"
  },
  "responses": [],
  "open_questions": []
}
```

IMPORTANT: All nested objects MUST use the exact field names shown above. For example:
- inputs/outputs MUST have: "name", "io_type", "description" (NOT "type")
- business_rules MUST have: "rule_id", "description"
- copybooks_used MUST have: "copybook_name", "purpose", "location"
- paragraphs MUST be a LIST of objects, NOT a dictionary
- data_flow.transforms MUST be a LIST of objects with: "input_field", "output_field", "transformation_description", "citation"
- error_handling MUST be a LIST of objects
- sql_operations MUST be a LIST of objects (or empty list)

CITATION TYPES (follow exactly):
- List of integers [100, 200]: inputs.citation, outputs.citation, business_rules.citation, error_handling.citation, data_flow.*.citation
- Single integer 100: called_programs.citation, copybooks_used.citation, sql_operations.citation, cics_operations.citation
- Tuple of exactly 2 [start, end]: paragraphs.citation (line range)

## CRITICAL: Paragraph Description Requirements

Each paragraph's "purpose" field MUST contain a DETAILED description of 5-10 sentences minimum. One-liner descriptions like "Orchestrates initialization" are UNACCEPTABLE.

For EVERY paragraph, include:
1. **Primary Purpose**: What is this paragraph's role in the overall program?
2. **Inputs Consumed**: What data/files/variables does it read? From where?
3. **Outputs Produced**: What data/files/variables does it write or modify?
4. **Business Logic**: What decisions are made? What conditions are checked?
5. **Error Handling**: How are errors or exceptional conditions handled?
6. **Calls Made**: What paragraphs or programs does it call, and why?

Bad example: "Handles customer processing"
Good example: "This paragraph processes individual customer records from the INPUT-FILE. It first validates the CUST-ACCT-STATUS field against the valid status codes table. If the status is 'A' (Active), it calculates the new balance by adding TRANS-AMOUNT to CURRENT-BALANCE. For status 'S' (Suspended), it logs an error message to the AUDIT-LOG and sets WS-ERROR-FLAG to 'Y'. The paragraph performs 3100-WRITE-OUTPUT to write the updated record. If any file I/O errors occur, it branches to 9000-ERROR-HANDLER with the appropriate error code in WS-ERR-CODE."

## When Responding to Challenger Questions

- Address each question directly
- Provide evidence (line numbers) for your answer
- If you were wrong, update the template and note the change
- If you stand by your original answer, explain why with citations

## When Addressing Chrome Tickets

- Read the issue description and guidance carefully
- Make the requested improvements
- Note what changes you made

Respond ONLY with valid JSON. Do not include markdown code fences or explanatory text outside the JSON."""

    def _build_user_prompt(self, input_data: ScribeInput) -> str:
        """Build the user prompt from input data.

        Args:
            input_data: The Scribe's input.

        Returns:
            The user message with code and context.
        """
        parts = []

        # Basic context
        parts.append(f"## Source File: {input_data.file_name}")
        parts.append(f"File Type: {input_data.file_type.value}")
        parts.append(f"Iteration: {input_data.iteration}")
        parts.append("")

        # Preprocessor hints if available
        if input_data.preprocessor_result:
            parts.append("## Preprocessor Analysis")
            parts.append("```json")
            parts.append(input_data.preprocessor_result.model_dump_json(indent=2))
            parts.append("```")
            parts.append("")

        # Citadel paragraph outline (if provided)
        if input_data.citadel_outline:
            parts.append("## Paragraph Outline (from static analysis)")
            parts.append("")
            parts.append("The following paragraphs were identified by static analysis.")
            parts.append("You MUST document ALL listed paragraphs. Do not skip any.")
            parts.append("")
            for para in input_data.citadel_outline:
                name = para.get("name", "UNKNOWN")
                line_start = para.get("line_start", "?")
                line_end = para.get("line_end", "?")
                calls = para.get("calls", [])
                call_targets = ", ".join(
                    c.get("target", "") for c in calls if c.get("target")
                )
                line_range = f"lines {line_start}-{line_end}" if line_start and line_end else ""
                calls_str = f" → calls: {call_targets}" if call_targets else ""
                parts.append(f"- **{name}** ({line_range}){calls_str}")
            parts.append("")

        # Pattern insights for documentation guidance
        if input_data.pattern_insights:
            parts.append("## Analysis Pattern Insights")
            parts.append("")

            # File-level summary
            summary = input_data.pattern_insights.get("file_summary", {})
            if summary.get("key_variables"):
                vars_list = ", ".join(summary["key_variables"][:10])
                parts.append(f"**Key Variables**: {vars_list}")
            if summary.get("complexity_indicators"):
                indicators = summary["complexity_indicators"]
                parts.append(f"**Complexity**: {indicators}")
            parts.append("")

            # Per-paragraph hints
            para_hints = input_data.pattern_insights.get("paragraph_hints", [])
            if para_hints:
                parts.append("### Paragraph Documentation Hints")
                parts.append("")
                for hint_item in para_hints[:20]:  # Limit to 20 paragraphs
                    para_name = hint_item.get("paragraph_name", "?")
                    hints = hint_item.get("hints", [])
                    complexity = hint_item.get("complexity", "")
                    parts.append(f"**{para_name}** ({complexity}):")
                    for hint in hints[:4]:  # Max 4 hints per paragraph
                        parts.append(f"  - {hint}")
                parts.append("")

            # Critical patterns
            critical = input_data.pattern_insights.get("critical_patterns", [])
            if critical:
                parts.append("### Critical Patterns (MUST Document)")
                for pattern in critical[:10]:
                    parts.append(f"- {pattern}")
                parts.append("")

        # Source code
        parts.append("## Source Code")
        parts.append("```")
        # Add line numbers
        lines = input_data.source_code.split("\n")
        for i, line in enumerate(lines, start=1):
            parts.append(f"{i:5d} | {line}")
        parts.append("```")
        parts.append("")

        # Copybook contents if any
        if input_data.copybook_contents:
            parts.append("## Referenced Copybooks")
            for name, content in input_data.copybook_contents.items():
                parts.append(f"### {name}")
                parts.append("```")
                parts.append(content)
                parts.append("```")
            parts.append("")

        # Previous template for subsequent iterations
        if input_data.previous_template and input_data.iteration > 1:
            parts.append("## Previous Documentation (to update)")
            parts.append("```json")
            parts.append(input_data.previous_template.model_dump_json(indent=2))
            parts.append("```")
            parts.append("")

        # Challenger questions to address
        if input_data.challenger_questions:
            parts.append("## Challenger Questions to Address")
            for q in input_data.challenger_questions:
                parts.append(f"- [{q.question_id}] ({q.question_type.value}) {q.question}")
                if q.evidence:
                    parts.append(f"  Evidence: lines {q.evidence}")
            parts.append("")

        # Chrome tickets to address
        if input_data.chrome_tickets:
            parts.append("## Chrome Tickets to Address")
            for t in input_data.chrome_tickets:
                parts.append(f"- [{t.ticket_id}] {t.section}: {t.description}")
                if t.guidance:
                    parts.append(f"  Guidance: {t.guidance}")
            parts.append("")

        # Feedback context from Imperator review (IMPFB-004)
        if input_data.feedback_context:
            parts.append("## Imperator Feedback Context (MUST Address)")
            parts.append("")
            parts.append("The Imperator has identified the following quality issues from previous cycles.")
            parts.append("You MUST explicitly address each note in your documentation:")
            parts.append("")

            # Quality notes
            quality_notes = input_data.feedback_context.get("quality_notes", [])
            if quality_notes:
                for note in quality_notes:
                    severity = note.get("severity", "medium").upper()
                    category = note.get("category", "other")
                    description = note.get("description", "")
                    guidance = note.get("guidance", "")
                    affected_sections = note.get("affected_sections", [])

                    parts.append(f"- [{severity}] {category}: {description}")
                    if affected_sections:
                        parts.append(f"  Affected sections: {', '.join(affected_sections)}")
                    if guidance:
                        parts.append(f"  Guidance: {guidance}")
                parts.append("")

            # Critical sections that must be populated
            critical_sections = input_data.feedback_context.get("critical_sections", [])
            if critical_sections:
                parts.append(f"**CRITICAL SECTIONS**: The following sections MUST NOT be empty: {', '.join(critical_sections)}")
                parts.append("")

            # Augment existing documentation
            if input_data.feedback_context.get("augment_existing", True):
                parts.append("**IMPORTANT**: You must AUGMENT existing documentation, not replace it.")
                parts.append("Preserve all valid existing content and ADD missing information.")
                parts.append("")

            # Citations requirement
            if input_data.feedback_context.get("required_citations", True):
                parts.append("**CITATIONS REQUIRED**: Every factual claim must cite line numbers.")
                parts.append("")

        # Instructions
        if input_data.iteration == 1:
            parts.append("## Task")
            parts.append("Analyze this source code and produce complete documentation.")
            parts.append("Fill out ALL sections of the template.")
            parts.append("Cite line numbers for every factual claim.")
        else:
            parts.append("## Task")
            parts.append("Update the documentation based on feedback.")
            parts.append("Address all Challenger questions and Chrome tickets.")
            parts.append("Improve any sections that were marked as issues.")

        # Strict formatting instructions (added on retry after parse failure)
        if input_data.formatting_strict:
            parts.append("")
            parts.append("## ⚠️ CRITICAL: YOUR PREVIOUS RESPONSE HAD JSON ERRORS ⚠️")
            parts.append("")
            parts.append("Your last response failed to parse. You MUST fix your JSON formatting:")
            parts.append("")
            parts.append("REQUIREMENTS:")
            parts.append("1. Start your response with { and end with } - NO other text")
            parts.append("2. NO markdown code blocks (no ```json)")
            parts.append("3. NO trailing commas: WRONG: [1, 2, 3,]  CORRECT: [1, 2, 3]")
            parts.append("4. Escape special characters in strings: use \\n \\t \\\" \\\\")
            parts.append("5. All property names in double quotes: {\"name\": \"value\"}")
            parts.append("6. NO comments in JSON")
            parts.append("")
            parts.append("BEFORE RESPONDING: Mentally validate your JSON is parseable.")

        parts.append("")
        parts.append("Respond with a JSON object containing:")
        parts.append('- "template": the DocumentationTemplate')
        parts.append('- "confidence": your confidence assessment')
        parts.append('- "responses": your responses to Challenger questions (if any)')
        parts.append('- "open_questions": questions you cannot resolve')

        return "\n".join(parts)

    def _repair_json(self, json_str: str) -> str:
        """Attempt to repair common JSON errors.

        Fixes:
        - Trailing commas before ] or }
        - Single quotes instead of double quotes (in some cases)
        - Unescaped newlines in strings

        Args:
            json_str: Potentially malformed JSON string.

        Returns:
            Repaired JSON string.
        """
        import re as repair_re

        # Remove trailing commas before ] or }
        # Match: comma followed by optional whitespace then ] or }
        repaired = repair_re.sub(r',(\s*[}\]])', r'\1', json_str)

        # Try to fix unescaped newlines within strings (common LLM error)
        # This is tricky - we need to find strings and escape newlines inside them
        # For now, just replace literal newlines that aren't \n
        # repaired = repaired.replace('\n', '\\n')  # Too aggressive, breaks valid JSON

        return repaired

    def _parse_template_lenient(self, template_data: dict) -> DocumentationTemplate | None:
        """Parse template with lenient validation.

        If the JSON is valid, we accept it. Schema mismatches are logged as
        warnings but don't cause failures. Uses model_construct() to bypass
        Pydantic validation when strict validation fails.

        Args:
            template_data: Raw template dict from LLM response.

        Returns:
            Parsed DocumentationTemplate, or None if data is completely empty.
        """
        if not template_data:
            return None

        # First try strict validation
        try:
            return DocumentationTemplate.load_lenient(template_data)
        except ValidationError as e:
            # Log warning but don't fail - the JSON was valid, just schema mismatch
            error_fields = [".".join(str(x) for x in err["loc"]) for err in e.errors()]
            logger.warning(
                f"Template schema mismatch ({e.error_count()} fields): {error_fields}. "
                "Accepting as-is."
            )

        # Use model_construct to bypass validation entirely
        # This creates the object without running validators
        try:
            # Build nested models where possible, fall back to raw dicts
            header = template_data.get("header", {})
            purpose = template_data.get("purpose", {})

            return DocumentationTemplate.model_construct(
                header=header if isinstance(header, dict) else {},
                purpose=purpose if isinstance(purpose, dict) else {},
                inputs=template_data.get("inputs", []),
                outputs=template_data.get("outputs", []),
                called_programs=template_data.get("called_programs", []),
                calling_context=template_data.get("calling_context", {}),
                business_rules=template_data.get("business_rules", []),
                data_flow=template_data.get("data_flow", {}),
                copybooks_used=template_data.get("copybooks_used", []),
                paragraphs=template_data.get("paragraphs", []),
                error_handling=template_data.get("error_handling", []),
                sql_operations=template_data.get("sql_operations", []),
                cics_operations=template_data.get("cics_operations", []),
                open_questions=template_data.get("open_questions", []),
            )
        except Exception as e:
            logger.error(f"Failed to construct template: {e}")
            return None

    def _parse_response(self, response: str, input_data: ScribeInput) -> ScribeOutput:
        """Parse the LLM response into ScribeOutput.

        If validation fails, captures the raw response and validation errors
        for potential recovery via FORMATTING_FIX ticket.

        Args:
            response: Raw text response from the LLM.
            input_data: Original input.

        Returns:
            Parsed ScribeOutput.
        """
        try:
            # Try to extract JSON from response
            json_match = re.search(r"\{[\s\S]*\}", response)
            if not json_match:
                # No JSON found - not recoverable without structure
                return ScribeOutput(
                    success=False,
                    error="No JSON object found in response",
                    raw_response=response,
                    recoverable=False,
                )

            json_str = json_match.group()

            # First try parsing as-is
            try:
                data = json.loads(json_str)
            except json.JSONDecodeError as e:
                # Try to repair common JSON errors
                logger.debug("JSON parse failed, attempting repair...")
                repaired = self._repair_json(json_str)
                try:
                    data = json.loads(repaired)
                    logger.info("JSON repair succeeded")
                except json.JSONDecodeError:
                    # JSON repair failed - recoverable with stronger model
                    return ScribeOutput(
                        success=False,
                        error=f"JSON parse error: {e}",
                        raw_response=response,
                        recoverable=True,
                    )

            # Parse template - use lenient validation
            template = None
            if "template" in data:
                template = self._parse_template_lenient(data["template"])

            # Parse confidence
            confidence = None
            if "confidence" in data:
                try:
                    confidence = ConfidenceAssessment.model_validate(data["confidence"])
                except ValidationError as e:
                    # Confidence validation errors are recoverable
                    validation_errors = [
                        {
                            "field": f"confidence.{'.'.join(str(x) for x in err['loc'])}",
                            "type": err["type"],
                            "message": err["msg"],
                            "input": err.get("input"),
                        }
                        for err in e.errors()
                    ]
                    logger.warning(f"Confidence validation failed: {e.error_count()} errors")
                    return ScribeOutput(
                        success=False,
                        error=f"Confidence validation failed: {e.error_count()} errors",
                        raw_response=response,
                        validation_errors=validation_errors,
                        recoverable=True,
                    )

            # Parse responses with lenient fallback
            responses = []
            responses_had_errors = False
            if "responses" in data:
                for r in data["responses"]:
                    try:
                        responses.append(ScribeResponse.model_validate(r))
                    except ValidationError as e:
                        # Log warning but try lenient parsing
                        error_fields = [".".join(str(x) for x in err["loc"]) for err in e.errors()]
                        logger.warning(
                            f"ScribeResponse validation failed ({e.error_count()} fields): {error_fields}. "
                            "Using lenient parsing."
                        )
                        responses_had_errors = True
                        # Use model_construct to bypass validation, mark as incomplete
                        response_obj = ScribeResponse.model_construct(
                            question_id=r.get("question_id", r.get("question", "UNKNOWN")),
                            response=r.get("response", ""),
                            action_taken=r.get("action_taken", "ACKNOWLEDGED"),
                            updated_section=r.get("updated_section"),
                            citation=r.get("citation", []),
                            iteration=r.get("iteration", 1),
                            parsing_incomplete=True,
                        )
                        responses.append(response_obj)

            # Parse open questions - handle both string and dict formats
            raw_questions = data.get("open_questions", [])
            open_questions = []
            for q in raw_questions:
                if isinstance(q, str):
                    open_questions.append(q)
                elif isinstance(q, dict):
                    # LLM sometimes returns OpenQuestion-style dicts
                    open_questions.append(q.get("question", str(q)))

            return ScribeOutput(
                success=True,
                template=template,
                confidence=confidence,
                responses=responses,
                open_questions=open_questions,
                responses_incomplete=responses_had_errors,
            )

        except Exception as e:
            logger.error(f"Failed to parse Scribe response: {e}")
            logger.debug(f"Response was: {response[:500]}...")
            return ScribeOutput(
                success=False,
                error=f"Failed to parse response: {e}",
                raw_response=response,
                recoverable=False,  # Unknown errors not recoverable
            )

    def _create_error_output(self, error: str, input_data: ScribeInput) -> ScribeOutput:
        """Create an error output.

        Args:
            error: Error message.
            input_data: Original input.

        Returns:
            ScribeOutput indicating failure.
        """
        return ScribeOutput(
            success=False,
            error=error,
        )

    def create_mock_output(self, input_data: ScribeInput) -> ScribeOutput:
        """Create mock output for testing without LLM calls.

        This method produces valid output structure with placeholder data,
        useful for testing the orchestration without making API calls.

        Args:
            input_data: The Scribe's input.

        Returns:
            Mock ScribeOutput with valid structure.
        """
        from datetime import datetime

        from war_rig.models.templates import (
            HeaderSection,
            ProgramType,
            PurposeSection,
        )

        program_id = "UNKNOWN"
        if input_data.preprocessor_result:
            program_id = input_data.preprocessor_result.program_id or "UNKNOWN"

        template = DocumentationTemplate(
            header=HeaderSection(
                program_id=program_id,
                file_name=input_data.file_name,
                file_type=input_data.file_type,
                analyzed_by="WAR_RIG_MOCK",
                analyzed_at=datetime.utcnow(),
                iteration_count=input_data.iteration,
            ),
            purpose=PurposeSection(
                summary=f"[MOCK] This is a mock documentation for {input_data.file_name}",
                program_type=ProgramType.BATCH,
                citations=[1],
            ),
        )

        confidence = ConfidenceAssessment(
            program_id=program_id,
            iteration=input_data.iteration,
            overall_confidence=ConfidenceLevel.LOW,
            reasoning="Mock output - no actual analysis performed",
        )

        return ScribeOutput(
            success=True,
            template=template,
            confidence=confidence,
            responses=[],
            open_questions=["This is mock output - no actual analysis was performed"],
        )
