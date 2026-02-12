"""Agentic README generation via CodeWhisper SDK.

Replaces the monolithic generate_system_design() prompt with an
investigative flow where the Imperator uses CodeWhisper tools to build
each README section through focused queries against skills (documentation),
the knowledge graph (structural relationships), and citadel (code analysis).

Architecture:
    AgenticReadmeGenerator creates a CodeWhisper SDK instance with skills,
    KG tools, and citadel tools registered. It then generates 9 README
    sections sequentially in a single conversation, allowing later sections
    to benefit from earlier context. An optional merge pass fixes
    cross-references and deduplication.

Example:
    generator = AgenticReadmeGenerator(
        code_dir=Path("./input"),
        skills_dir=Path("./output/skills"),
        kg_manager=kg_manager,
    )
    result = await generator.generate(structural_context, sequence_diagrams)
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from pathlib import Path
from typing import TYPE_CHECKING

from war_rig.agents.imperator import InlineQuestion, SystemDesignOutput

if TYPE_CHECKING:
    from war_rig.knowledge_graph.manager import KnowledgeGraphManager

logger = logging.getLogger(__name__)


# ============================================================================
# Section definitions
# ============================================================================


@dataclass
class ReadmeSection:
    """Definition of a single README section to generate.

    Attributes:
        number: Section ordering number (1-9).
        name: Section header (used as ## header in output).
        prompt_template: Prompt text instructing the agent on how to
            generate this section. May reference tools to use.
        min_sentences: Minimum expected sentence count for quality check.
        requires_kg: Whether this section benefits from KG tools.
    """

    number: int
    name: str
    prompt_template: str
    min_sentences: int = 5
    requires_kg: bool = False


# The 9 canonical README sections
SECTIONS: list[ReadmeSection] = [
    ReadmeSection(
        number=1,
        name="Executive Summary",
        prompt_template="""\
Write section "## 1. Executive Summary" for the system README.

This is the MOST IMPORTANT section and MUST contain AT LEAST 20 sentences
demonstrating deep understanding of the system.

Cover these aspects:
- **System Purpose** (4-5 sentences): What business problem it solves, its mission, users
- **Functional Overview** (5-6 sentences): Major capabilities, transactions, key workflows
- **Technical Foundation** (4-5 sentences): Technologies used (COBOL, JCL, CICS, IMS, etc.)
- **System Boundaries** (3-4 sentences): Inputs, outputs, external integrations
- **Business Value** (3-4 sentences): Why it matters, impact if unavailable

Instructions:
1. Use `search_skills("system overview")` and `search_skills("purpose")` to find overview skills
2. Use `load_skill` on the most relevant results to understand the system deeply
3. Use `kg_get_hub_entities` to identify the most important programs
4. Synthesize findings into a rich, detailed narrative — NOT placeholder text
5. For every program or component you mention, include a markdown link: [NAME](NAME.ext.md)

Output ONLY the markdown for this section (starting with ## 1. Executive Summary).""",
        min_sentences=20,
        requires_kg=True,
    ),
    ReadmeSection(
        number=2,
        name="Architecture Overview",
        prompt_template="""\
Write section "## 2. Architecture Overview" for the system README.

Instructions:
1. If a Mermaid call graph diagram was provided in the structural context, include it VERBATIM
   in a ```mermaid code block. Do NOT create your own diagram — use the provided one.
2. Use `search_skills` to find entry point programs and understand system layers
3. Use `kg_get_program_relationships` for the top 3-5 hub programs to understand their roles
4. Describe the architectural layers: batch vs online, data access patterns, integration points
5. Explain how components interact (call chains, data flow, messaging)

Link every program mentioned: [NAME](NAME.ext.md)

Output ONLY the markdown for this section (starting with ## 2. Architecture Overview).""",
        min_sentences=10,
        requires_kg=True,
    ),
    ReadmeSection(
        number=3,
        name="Component Catalog",
        prompt_template="""\
Write section "## 3. Component Catalog" for the system README.

Build a comprehensive table of ALL documented components using the doc path table
from the structural context.

Instructions:
1. For each component in the documentation file paths table, use `search_skills` with
   the program name to get its purpose
2. Build a markdown table with columns: Component | Type | Purpose | Doc Link
3. Group components by type (COBOL programs, JCL jobs, copybooks, etc.)
4. Every component MUST have a documentation link: [NAME](path/NAME.ext.md)

Output ONLY the markdown for this section (starting with ## 3. Component Catalog).""",
        min_sentences=3,
    ),
    ReadmeSection(
        number=4,
        name="Subsystem Breakdown",
        prompt_template="""\
Write section "## 4. Subsystem Breakdown" for the system README.

Group programs into logical subsystems based on shared functionality.

Instructions:
1. Use `kg_get_copybook_users` for shared copybooks to find groups of programs
   that share data structures
2. Use `kg_get_jcl_context` for key programs to understand batch job groupings
3. Use `search_skills("subsystem")` and `search_skills("module")` for named groupings
4. For each subsystem: name it, list its programs, describe its responsibility,
   and explain how it interacts with other subsystems
5. Include links to all programs mentioned

Output ONLY the markdown for this section (starting with ## 4. Subsystem Breakdown).""",
        min_sentences=10,
        requires_kg=True,
    ),
    ReadmeSection(
        number=5,
        name="Data Architecture",
        prompt_template="""\
Write section "## 5. Data Architecture" for the system README.

Document the system's data landscape: files, databases, data flows.

Instructions:
1. Use `kg_get_programs_for_dataset` for key datasets to trace data flow
2. Use `search_skills("dataset")`, `search_skills("file")`, `search_skills("database")`
3. Document:
   - Key datasets and their purposes
   - Which programs read/write each dataset
   - Data flow patterns (sequential, random, VSAM, DB2, IMS)
   - Shared data structures via copybooks
4. Include a data flow narrative or table showing producer → consumer relationships

Output ONLY the markdown for this section (starting with ## 5. Data Architecture).""",
        min_sentences=8,
        requires_kg=True,
    ),
    ReadmeSection(
        number=6,
        name="Integration Points",
        prompt_template="""\
Write section "## 6. Integration Points" for the system README.

Document external interfaces and integration boundaries.

Instructions:
1. Use `kg_get_jcl_context` for entry point programs to find job-level integrations
2. Use `search_skills("interface")`, `search_skills("external")`, `search_skills("MQ")`
3. Document:
   - External system interfaces (files, MQ queues, APIs)
   - Batch job entry points and scheduling
   - CICS transaction entry points
   - Cross-system data exchanges
4. Mark unclear integrations with ❓ QUESTION: markers

Output ONLY the markdown for this section (starting with ## 6. Integration Points).""",
        min_sentences=5,
        requires_kg=True,
    ),
    ReadmeSection(
        number=7,
        name="Business Rules",
        prompt_template="""\
Write section "## 7. Business Rules" for the system README.

Compile ALL business rules from the documented components.

Instructions:
1. Use `search_skills("business rule")` to find skills with business rules
2. Use `load_skill` on programs known to have important business logic
3. Group rules by business domain (e.g., account processing, authorization,
   validation, reporting)
4. For each rule: describe it, cite the source program, and link to documentation
5. Include validation rules, calculation formulas, and processing constraints

Output ONLY the markdown for this section (starting with ## 7. Business Rules).""",
        min_sentences=8,
    ),
    ReadmeSection(
        number=8,
        name="Error Handling Patterns",
        prompt_template="""\
Write section "## 8. Error Handling Patterns" for the system README.

Document common error handling approaches across the system.

Instructions:
1. Use `search_skills("error")`, `search_skills("abend")`, `search_skills("recovery")`
2. Use `load_skill` on programs with error handling documentation
3. Document:
   - Common error handling patterns (abend codes, return codes)
   - Recovery procedures and restart logic
   - Logging and monitoring patterns
   - Error escalation chains
4. Reference specific programs implementing each pattern

Output ONLY the markdown for this section (starting with ## 8. Error Handling Patterns).""",
        min_sentences=5,
    ),
    ReadmeSection(
        number=9,
        name="Open Questions and Uncertainties",
        prompt_template="""\
Write section "## 9. Open Questions and Uncertainties" for the system README.

Consolidate all questions and uncertainties from the documentation.

Instructions:
1. Collect ALL ❓ QUESTION: markers from previous sections in this conversation
2. Use `search_skills("question")` to find documented open questions
3. Group questions by category (architecture, data flow, business rules, etc.)
4. For each question: state it clearly, explain why it matters, and suggest
   how it might be resolved
5. Document assumptions made during documentation and their implications

Output ONLY the markdown for this section (starting with ## 9. Open Questions and Uncertainties).""",
        min_sentences=3,
    ),
]


# ============================================================================
# Structural context (pre-computed data injected into system prompt)
# ============================================================================


@dataclass
class StructuralContext:
    """Pre-computed structural data for README generation.

    Built from call graph analysis, KG summary, and file documentation
    metadata. Injected as a compact system message so the agent has
    architectural awareness before starting section generation.
    """

    call_graph_mermaid: str = ""
    entry_points: list[str] = field(default_factory=list)
    call_chains: list[list[str]] = field(default_factory=list)
    shared_copybooks: dict[str, list[str]] = field(default_factory=dict)
    kg_summary: str = ""
    doc_path_table: str = ""

    def to_context_string(self) -> str:
        """Format as a compact markdown context block (~1500 tokens).

        Returns:
            Formatted markdown for system prompt injection.
        """
        parts = ["## Structural Context (Reference Data)", ""]

        if self.call_graph_mermaid:
            parts.append("### Call Graph Diagram")
            parts.append("Use this diagram VERBATIM in the Architecture Overview:")
            parts.append("")
            parts.append(self.call_graph_mermaid)
            parts.append("")

        if self.entry_points:
            parts.append("### Entry Points (no callers)")
            for ep in self.entry_points[:10]:
                parts.append(f"- {ep}")
            parts.append("")

        if self.call_chains:
            parts.append("### Typical Execution Flows")
            for idx, chain in enumerate(self.call_chains[:5], start=1):
                parts.append(f"{idx}. {' → '.join(chain)}")
            parts.append("")

        if self.shared_copybooks:
            parts.append("### Shared Copybooks")
            for cb, users in sorted(self.shared_copybooks.items()):
                if len(users) > 1:
                    parts.append(f"- **{cb}**: {', '.join(users)}")
            parts.append("")

        if self.kg_summary:
            parts.append(self.kg_summary)
            parts.append("")

        if self.doc_path_table:
            parts.append("### Documentation File Paths")
            parts.append(self.doc_path_table)
            parts.append("")

        return "\n".join(parts)


# ============================================================================
# Configuration
# ============================================================================


@dataclass
class AgenticReadmeConfig:
    """Configuration for the agentic README generator.

    Attributes:
        max_iterations_per_section: Max tool-calling iterations per section.
        temperature: LLM sampling temperature.
        max_tokens: Max tokens per LLM response.
        use_minion: Use minion model for large tool output summarization.
        merge_pass_enabled: Run a final merge pass to fix cross-refs.
    """

    max_iterations_per_section: int = 10
    temperature: float = 0.3
    max_tokens: int = 4096
    use_minion: bool = True
    merge_pass_enabled: bool = True


# ============================================================================
# Main generator
# ============================================================================


ARCHITECT_PROMPT = """\
You are a technical architect creating a comprehensive system design document
(README.md) for a mainframe system. You have access to tools that let you
search documentation (skills), query the knowledge graph for structural
relationships, analyze source code (citadel), and read raw files.

## Core Principles

**INVESTIGATE BEFORE WRITING**: For each section, use the tools to gather
real information. Do NOT write placeholder or generic text. Every claim
should be backed by evidence from skills or code analysis.

**CROSS-REFERENCING WITH LINKS**: For every component you mention, include
a markdown link to its documentation file using the paths from the
documentation file paths table. Links MUST include the source extension:
- COBOL: [PROGRAM_NAME](PROGRAM.cbl.md)
- JCL: [JOB_NAME](JOB.jcl.md)
- Copybook: [COPY_NAME](COPY.cpy.md)

**MARK UNCERTAINTIES**: Where information is unclear or missing, insert:
❓ QUESTION: [your question here]

**BE THOROUGH**: Write detailed, comprehensive sections. A longer document
with evidence-backed content is better than a sparse one.

## Tool Usage Strategy

1. **search_skills** → discover relevant documentation by keyword
2. **load_skill** → read full program/concept documentation
3. **kg_get_hub_entities** → find architecturally important programs
4. **kg_get_program_relationships** → trace calls, datasets, copybooks
5. **kg_get_programs_for_dataset** → find data producers/consumers
6. **kg_get_jcl_context** → understand batch job context
7. **kg_get_copybook_users** → find programs sharing data structures
8. **citadel_analyze_file** → deep-dive into specific source files
9. **read_file** → examine raw source when needed

Keep making tool calls until you have enough information for a thorough section.
Only respond when you have gathered sufficient evidence.
"""


def _parse_inline_questions(markdown: str, cycle: int = 1) -> list[InlineQuestion]:
    """Parse inline questions from generated markdown.

    Looks for the pattern: ❓ QUESTION: [question text]

    Args:
        markdown: The generated markdown content.
        cycle: The current review cycle number.

    Returns:
        List of InlineQuestion objects parsed from the markdown.
    """
    import re as _re

    questions: list[InlineQuestion] = []
    pattern = r"❓\s*QUESTION:\s*(.+?)(?:\n|$)"
    matches = _re.finditer(pattern, markdown)

    for idx, match in enumerate(matches, start=1):
        question_text = match.group(1).strip()

        # Find nearest preceding header for context
        text_before = markdown[: match.start()]
        header_pattern = r"^#{1,6}\s+(.+?)$"
        headers = list(_re.finditer(header_pattern, text_before, _re.MULTILINE))
        context = headers[-1].group(1).strip() if headers else "General"

        questions.append(
            InlineQuestion(
                question_id=f"Q{idx:03d}",
                question_text=question_text,
                context=context,
                related_files=[],
                cycle_asked=cycle,
            )
        )

    return questions


class AgenticReadmeGenerator:
    """Generate README.md through agentic investigation via CodeWhisper SDK.

    Creates a CodeWhisper SDK instance with skills, KG tools, and citadel
    tools registered, then generates each README section sequentially through
    focused tool-based investigation.

    Args:
        code_dir: Path to source code directory.
        skills_dir: Path to skills directory (documentation).
        kg_manager: Optional knowledge graph manager for KG tools.
        config: Generator configuration.
    """

    def __init__(
        self,
        code_dir: Path,
        skills_dir: Path | None = None,
        kg_manager: KnowledgeGraphManager | None = None,
        config: AgenticReadmeConfig | None = None,
    ) -> None:
        self._code_dir = code_dir
        self._skills_dir = skills_dir
        self._kg_manager = kg_manager
        self._config = config or AgenticReadmeConfig()

    async def generate(
        self,
        structural_context: StructuralContext,
        sequence_diagrams: list[str] | None = None,
    ) -> SystemDesignOutput:
        """Generate a complete README.md through agentic investigation.

        Generates 9 sections sequentially in one conversation, then
        optionally runs a merge pass. Appends sequence diagrams at the end.

        Args:
            structural_context: Pre-computed structural data.
            sequence_diagrams: Optional mermaid sequence diagrams to append.

        Returns:
            SystemDesignOutput with the assembled markdown.
        """
        sdk = self._create_sdk()

        # Inject architect persona and structural context as system messages
        sdk.add_system_message(ARCHITECT_PROMPT)
        context_str = structural_context.to_context_string()
        if context_str.strip():
            sdk.add_system_message(context_str)

        # Generate sections sequentially
        generated_sections: dict[str, str] = {}

        for section in SECTIONS:
            # Skip KG-dependent sections if KG is not available
            if section.requires_kg and (
                self._kg_manager is None or not self._kg_manager.enabled
            ):
                # Still generate but note KG unavailability in prompt
                pass

            logger.info(
                "Generating README section %d: %s", section.number, section.name
            )

            try:
                prompt = self._build_section_prompt(
                    section, structural_context, generated_sections
                )
                section_content = await self._generate_section(
                    sdk, section, prompt
                )
                generated_sections[section.name] = section_content
                logger.info(
                    "Section %d (%s): %d chars generated",
                    section.number,
                    section.name,
                    len(section_content),
                )
            except Exception as e:
                logger.warning(
                    "Failed to generate section %d (%s): %s",
                    section.number,
                    section.name,
                    e,
                )
                generated_sections[section.name] = (
                    f"## {section.number}. {section.name}\n\n"
                    f"*Section generation failed: {e}*\n"
                )

        # Optional merge pass
        final_markdown = self._assemble_document(
            generated_sections, sequence_diagrams
        )

        if self._config.merge_pass_enabled and len(generated_sections) > 3:
            try:
                final_markdown = await self._merge_sections(
                    sdk, final_markdown, structural_context
                )
            except Exception as e:
                logger.warning("Merge pass failed, using unmerged output: %s", e)

        # Sanitize any invalid mermaid blocks before output
        from war_rig.validation.mermaid_validator import sanitize_mermaid_blocks

        final_markdown = sanitize_mermaid_blocks(final_markdown)

        # Parse inline questions
        questions = _parse_inline_questions(final_markdown, cycle=1)
        sections_updated = [s.name for s in SECTIONS if s.name in generated_sections]

        return SystemDesignOutput(
            success=True,
            markdown=final_markdown,
            questions=questions,
            sections_updated=sections_updated,
        )

    def _create_sdk(self):
        """Create and configure a CodeWhisper SDK instance.

        Registers skills, KG tools, and citadel tools. Enables minions
        for large tool output summarization.

        Returns:
            Configured CodeWhisper instance.
        """
        from codewhisper.sdk import CodeWhisper, CodeWhisperConfig

        from war_rig.providers import get_provider_from_env

        provider = get_provider_from_env()

        config = CodeWhisperConfig(
            max_iterations=self._config.max_iterations_per_section,
            temperature=self._config.temperature,
            max_tokens=self._config.max_tokens,
            use_minion=self._config.use_minion,
        )

        sdk = CodeWhisper(
            llm_provider=provider,
            code_dir=self._code_dir,
            documents_dir=self._skills_dir,
            config=config,
        )

        # Register KG tools if manager is available and enabled
        if self._kg_manager is not None and self._kg_manager.enabled:
            from war_rig.agents.kg_tools import create_kg_tools

            kg_tools = create_kg_tools(self._kg_manager)
            sdk.tool_registry.register_all(kg_tools)
            logger.info("Registered %d KG tools in CodeWhisper SDK", len(kg_tools))

        return sdk

    def _build_section_prompt(
        self,
        section: ReadmeSection,
        structural_context: StructuralContext,
        previous_sections: dict[str, str],
    ) -> str:
        """Build the focused prompt for generating one section.

        Args:
            section: The section definition.
            structural_context: Pre-computed structural data.
            previous_sections: Already-generated sections (for reference).

        Returns:
            The prompt string.
        """
        parts = [section.prompt_template]

        # Add note about KG unavailability if needed
        if section.requires_kg and (
            self._kg_manager is None or not self._kg_manager.enabled
        ):
            parts.append(
                "\n\nNote: Knowledge graph tools are not available. "
                "Use search_skills and citadel tools instead for structural information."
            )

        return "\n".join(parts)

    async def _generate_section(
        self,
        sdk,
        section: ReadmeSection,
        prompt: str,
    ) -> str:
        """Generate one section via SDK.complete().

        Args:
            sdk: The CodeWhisper SDK instance.
            section: The section definition.
            prompt: The section prompt.

        Returns:
            The generated section markdown.
        """
        result = await sdk.complete(prompt)
        content = result.content.strip()

        logger.debug(
            "Section %d used %d tool calls in %d iterations",
            section.number,
            result.tool_calls_made,
            result.iterations,
        )

        return content

    async def _merge_sections(
        self,
        sdk,
        assembled_markdown: str,
        structural_context: StructuralContext,
    ) -> str:
        """Run a merge pass to fix cross-references and deduplication.

        Args:
            sdk: The CodeWhisper SDK instance (continuing conversation).
            assembled_markdown: The assembled document from all sections.
            structural_context: Pre-computed structural data.

        Returns:
            The merged/cleaned markdown.
        """
        merge_prompt = """\
Review and clean up the complete README document assembled from all sections above.

Fix the following issues:
1. **Cross-references**: Ensure all section cross-references (e.g., "see Section 5")
   point to correct section numbers and names
2. **Deduplication**: Remove redundant content that appears in multiple sections
3. **Link consistency**: Ensure all component links use consistent paths from the
   documentation file paths table
4. **Flow**: Ensure smooth transitions between sections
5. **Missing links**: Add documentation links for any component mentioned without one

Output the COMPLETE cleaned-up README.md document.
Do NOT summarize or truncate — output the full document."""

        result = await sdk.complete(merge_prompt)
        merged = result.content.strip()

        # Only use merged version if it's substantial (not a summary)
        if len(merged) > len(assembled_markdown) * 0.5:
            return merged

        logger.warning(
            "Merge pass output too short (%d vs %d chars), keeping original",
            len(merged),
            len(assembled_markdown),
        )
        return assembled_markdown

    def _assemble_document(
        self,
        sections: dict[str, str],
        sequence_diagrams: list[str] | None = None,
    ) -> str:
        """Concatenate sections into a complete document.

        Args:
            sections: Mapping of section name to generated markdown.
            sequence_diagrams: Optional sequence diagrams to append.

        Returns:
            The assembled README.md content.
        """
        parts = ["# System Design Document", ""]

        # Add sections in order
        for section_def in SECTIONS:
            if section_def.name in sections:
                parts.append(sections[section_def.name])
                parts.append("")

        # Append sequence diagrams
        if sequence_diagrams:
            parts.append(self._format_flows_section(sequence_diagrams))

        return "\n".join(parts)

    @staticmethod
    def _format_flows_section(sequence_diagrams: list[str]) -> str:
        """Format sequence diagrams as a Flows section.

        Args:
            sequence_diagrams: List of mermaid sequence diagram strings.

        Returns:
            Formatted markdown Flows section.
        """
        if not sequence_diagrams:
            return ""

        lines = ["## Flows", ""]
        lines.append(
            "The following sequence diagrams illustrate key call sequences "
            "identified in the codebase."
        )
        lines.append("")

        from war_rig.validation.mermaid_validator import is_valid_mermaid

        for idx, diagram in enumerate(sequence_diagrams, start=1):
            diagram_content = diagram.strip()
            if diagram_content.startswith("```mermaid"):
                diagram_content = diagram_content[len("```mermaid"):].strip()
            if diagram_content.endswith("```"):
                diagram_content = diagram_content[:-3].strip()
            if not is_valid_mermaid(diagram_content):
                logger.warning(
                    "Skipping invalid mermaid diagram in flows section: %.80s",
                    diagram_content.replace("\n", " "),
                )
                continue
            lines.append(f"### Flow {idx}")
            lines.append("")
            lines.append("```mermaid")
            lines.append(diagram_content)
            lines.append("```")
            lines.append("")

        return "\n".join(lines)
