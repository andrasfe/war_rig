"""Automatic open question resolution using CodeWhisper SDK.

Resolves open questions in .doc.json templates and inline ❓ QUESTION:
markers in README.md files by investigating the source code.
"""

import asyncio
import json
import logging
import re
import time
from dataclasses import dataclass, field
from pathlib import Path
from typing import TYPE_CHECKING

from war_rig.config import QuestionResolutionConfig
from war_rig.models.templates import (
    DocumentationTemplate,
    ResolvedQuestion,
)

if TYPE_CHECKING:
    from codewhisper.sdk import CodeWhisper  # type: ignore[import-not-found]

    from war_rig.agents.imperator import HolisticReviewOutput

logger = logging.getLogger(__name__)


@dataclass
class ComponentQuestionContext:
    """Context for a question from a .doc.json component template."""

    doc_path: Path
    file_name: str
    program_id: str
    question: str
    question_index: int
    template: DocumentationTemplate


@dataclass
class ReadmeQuestionContext:
    """Context for an inline question from README.md."""

    question: str
    readme_path: Path
    surrounding_text: str


@dataclass
class QuestionResolutionResult:
    """Summary of question resolution for a cycle."""

    component_questions_found: int = 0
    component_questions_resolved: int = 0
    readme_questions_found: int = 0
    readme_questions_resolved: int = 0
    errors: list[str] = field(default_factory=list)
    duration_seconds: float = 0.0


class QuestionResolver:
    """Resolves open questions using CodeWhisper SDK investigations."""

    def __init__(
        self,
        config: QuestionResolutionConfig,
        output_directory: Path,
        input_directory: Path,
        cycle: int,
    ) -> None:
        self._config = config
        self._output_directory = output_directory
        self._input_directory = input_directory
        self._cycle = cycle
        self._skills_dir: Path | None = None

    _MAX_CONCURRENCY = 5

    async def resolve_all(
        self, review_result: "HolisticReviewOutput",
    ) -> QuestionResolutionResult:
        """Resolve open questions from component templates and README.

        Questions are resolved concurrently using asyncio.gather() with a
        semaphore to bound parallelism. Each concurrent task creates its own
        SDK instance to avoid shared mutable state. Template updates are
        applied sequentially after all resolutions complete (in reverse index
        order to avoid index shifting).

        Args:
            review_result: The holistic review output from Imperator.

        Returns:
            Summary of resolution results.
        """
        start = time.monotonic()
        result = QuestionResolutionResult()

        if not self._config.enabled:
            return result

        # Collect questions
        component_questions = self._collect_component_questions()
        result.component_questions_found = len(component_questions)

        readme_questions: list[ReadmeQuestionContext] = []
        if self._config.resolve_readme_questions:
            readme_questions = self._collect_readme_questions(review_result)
            result.readme_questions_found = len(readme_questions)

        total = len(component_questions) + len(readme_questions)
        if total == 0:
            logger.info("No open questions to resolve")
            result.duration_seconds = time.monotonic() - start
            return result

        # Cap questions per cycle (split evenly, component gets remainder)
        cap = self._config.max_questions_per_cycle
        if total > cap:
            readme_cap = min(len(readme_questions), cap // 3)
            component_cap = cap - readme_cap
            component_questions = component_questions[:component_cap]
            readme_questions = readme_questions[:readme_cap]

        # Sort by (file, descending index) so removals don't shift subsequent indices
        component_questions.sort(
            key=lambda q: (str(q.doc_path), -q.question_index)
        )

        logger.info(
            f"Resolving {len(component_questions)} component + "
            f"{len(readme_questions)} README questions (cap={cap})"
        )

        # Verify SDK can be created before spawning concurrent tasks
        try:
            test_sdk = self._create_sdk()
            del test_sdk
        except Exception as e:
            logger.warning(f"Failed to create CodeWhisper SDK: {e}")
            result.errors.append(f"SDK creation failed: {e}")
            result.duration_seconds = time.monotonic() - start
            return result

        semaphore = asyncio.Semaphore(self._MAX_CONCURRENCY)

        # --- Resolve component questions concurrently ---
        async def _resolve_component(
            ctx: ComponentQuestionContext,
        ) -> tuple[ComponentQuestionContext, ResolvedQuestion | None, str | None]:
            """Resolve a single component question with its own SDK."""
            async with semaphore:
                try:
                    sdk = self._create_sdk()
                    resolved = await self._resolve_component_question(sdk, ctx)
                    return (ctx, resolved, None)
                except Exception as e:
                    msg = f"Error resolving question for {ctx.file_name}: {e}"
                    logger.warning(msg)
                    return (ctx, None, msg)

        component_results = await asyncio.gather(
            *[_resolve_component(ctx) for ctx in component_questions]
        )

        # Apply template updates sequentially in reverse index order.
        # The component_questions list is already sorted by (file, -index),
        # and gather preserves input order, so updates apply safely.
        for ctx, resolved, error in component_results:
            if error is not None:
                result.errors.append(error)
            elif resolved is not None:
                self._update_template(
                    ctx.doc_path, ctx.question_index, resolved
                )
                result.component_questions_resolved += 1

        # --- Resolve README questions concurrently ---
        async def _resolve_readme(
            rq_ctx: ReadmeQuestionContext,
        ) -> tuple[ReadmeQuestionContext, str | None, str | None]:
            """Resolve a single README question with its own SDK."""
            async with semaphore:
                try:
                    sdk = self._create_sdk()
                    answer = await self._resolve_readme_question(sdk, rq_ctx)
                    return (rq_ctx, answer, None)
                except Exception as e:
                    msg = f"Error resolving README question: {e}"
                    logger.warning(msg)
                    return (rq_ctx, None, msg)

        readme_results = await asyncio.gather(
            *[_resolve_readme(rq_ctx) for rq_ctx in readme_questions]
        )

        readme_resolved: dict[str, str] = {}
        for rq_ctx, answer, error in readme_results:
            if error is not None:
                result.errors.append(error)
            elif answer is not None:
                readme_resolved[rq_ctx.question] = answer
                result.readme_questions_resolved += 1

        if readme_resolved:
            try:
                self._update_readme(readme_resolved)
            except Exception as e:
                msg = f"Error updating README: {e}"
                logger.warning(msg)
                result.errors.append(msg)

        result.duration_seconds = time.monotonic() - start
        return result

    def _collect_component_questions(self) -> list[ComponentQuestionContext]:
        """Scan output directory for .doc.json files with open questions."""
        questions: list[ComponentQuestionContext] = []

        for doc_path in self._output_directory.rglob("*.doc.json"):
            try:
                data = json.loads(doc_path.read_text(encoding="utf-8"))
                template = DocumentationTemplate.load_lenient(data)

                if not template.open_questions:
                    continue

                file_name = doc_path.stem.replace(".doc", "")
                program_id = ""
                if template.header and template.header.program_id:
                    program_id = template.header.program_id

                for idx, oq in enumerate(template.open_questions):
                    if oq.question:
                        questions.append(
                            ComponentQuestionContext(
                                doc_path=doc_path,
                                file_name=file_name,
                                program_id=program_id,
                                question=oq.question,
                                question_index=idx,
                                template=template,
                            )
                        )
            except Exception as e:
                logger.debug(f"Skipping {doc_path}: {e}")

        return questions

    def _collect_readme_questions(
        self, review_result: "HolisticReviewOutput",
    ) -> list[ReadmeQuestionContext]:
        """Collect inline questions from README and review result."""
        questions: list[ReadmeQuestionContext] = []

        # From review_result.system_design_questions (InlineQuestion objects)
        readme_path = self._output_directory / "README.md"
        for iq in review_result.system_design_questions:
            questions.append(
                ReadmeQuestionContext(
                    question=iq.question_text,
                    readme_path=readme_path,
                    surrounding_text=iq.context,
                )
            )

        # Also parse README.md for ❓ QUESTION: markers
        if readme_path.exists():
            try:
                content = readme_path.read_text(encoding="utf-8")
                lines = content.splitlines()
                pattern = re.compile(r"❓\s*\*{0,2}QUESTION\*{0,2}:\s*(.+?)(?:\n|$)")

                for match in pattern.finditer(content):
                    q_text = match.group(1).strip()

                    # Avoid duplicates from system_design_questions
                    if any(q.question == q_text for q in questions):
                        continue

                    # Extract surrounding context (5 lines)
                    line_num = content[:match.start()].count("\n")
                    start = max(0, line_num - 2)
                    end = min(len(lines), line_num + 3)
                    surrounding = "\n".join(lines[start:end])

                    questions.append(
                        ReadmeQuestionContext(
                            question=q_text,
                            readme_path=readme_path,
                            surrounding_text=surrounding,
                        )
                    )
            except Exception as e:
                logger.debug(f"Error parsing README for questions: {e}")

        return questions

    def _generate_skills(self) -> Path | None:
        """Generate skills index from documentation for CodeWhisper.

        Returns the skills directory path, or None if generation fails.
        """
        try:
            from war_rig.skills import SkillsGenerator

            generator = SkillsGenerator(
                input_dir=self._output_directory,
                system_name="System",
            )
            skills_dir = generator.generate()
            logger.info(f"Generated skills index at {skills_dir}")
            return skills_dir
        except Exception as e:
            logger.warning(f"Skills generation failed, SDK will lack docs context: {e}")
            return None

    def _ensure_skills(self) -> Path | None:
        """Generate skills once and cache the path."""
        if self._skills_dir is None:
            self._skills_dir = self._generate_skills()
        return self._skills_dir

    def _create_sdk(self) -> "CodeWhisper":
        """Create a fresh CodeWhisper SDK instance."""
        from codewhisper.sdk import CodeWhisper, CodeWhisperConfig

        from war_rig.providers.factory import get_provider_from_env

        provider = get_provider_from_env()
        config = CodeWhisperConfig(
            max_iterations=self._config.codewhisper_max_iterations,
            temperature=self._config.codewhisper_temperature,
            max_tokens=self._config.codewhisper_max_tokens,
            use_minion=False,
        )
        return CodeWhisper(
            llm_provider=provider,
            code_dir=self._input_directory,
            documents_dir=self._ensure_skills(),
            config=config,
        )

    async def _resolve_component_question(
        self,
        sdk: "CodeWhisper",
        ctx: ComponentQuestionContext,
    ) -> ResolvedQuestion | None:
        """Resolve a single component question using CodeWhisper."""
        prompt = (
            f"Investigate this open question about the program '{ctx.program_id}' "
            f"in file '{ctx.file_name}':\n\n"
            f"Question: {ctx.question}\n\n"
            "Search the source code to find a concrete answer. "
            "If you cannot find a definitive answer, prefix your response with "
            "'INCONCLUSIVE:' and explain what you checked."
        )

        template_summary = ""
        if ctx.template.purpose and ctx.template.purpose.summary:
            template_summary = (
                f"Program purpose: {ctx.template.purpose.summary}"
            )

        sdk.reset()
        try:
            completion = await asyncio.wait_for(
                sdk.complete(prompt, context=template_summary),
                timeout=self._config.timeout_per_question,
            )
        except TimeoutError:
            logger.warning(
                f"Timeout resolving question for {ctx.file_name}: {ctx.question}"
            )
            return None

        answer: str = str(completion.content).strip()
        is_good, confidence = self._assess_answer_quality(answer)

        if not is_good:
            logger.debug(
                f"Low quality answer for {ctx.file_name}, skipping: {answer[:100]}"
            )
            return None

        # Check minimum confidence
        confidence_rank = {"HIGH": 3, "MEDIUM": 2, "LOW": 1}
        min_rank = confidence_rank.get(self._config.min_confidence, 2)
        if confidence_rank.get(confidence, 0) < min_rank:
            return None

        return ResolvedQuestion(
            original_question=ctx.question,
            original_context=(
                ctx.template.open_questions[ctx.question_index].context
                if ctx.question_index < len(ctx.template.open_questions)
                else None
            ),
            answer=answer,
            confidence=confidence,
            resolved_by="CODEWHISPER",
            cycle_resolved=self._cycle,
            tool_calls_used=completion.tool_calls_made,
        )

    async def _resolve_readme_question(
        self,
        sdk: "CodeWhisper",
        ctx: ReadmeQuestionContext,
    ) -> str | None:
        """Resolve a single README inline question."""
        prompt = (
            f"Investigate this question from the system design document:\n\n"
            f"Question: {ctx.question}\n\n"
            f"Context in document:\n{ctx.surrounding_text}\n\n"
            "Search the source code to find a concrete answer suitable for "
            "inclusion in a README. If you cannot find a definitive answer, "
            "prefix your response with 'INCONCLUSIVE:'."
        )

        sdk.reset()
        try:
            completion = await asyncio.wait_for(
                sdk.complete(prompt),
                timeout=self._config.timeout_per_question,
            )
        except TimeoutError:
            logger.warning(f"Timeout resolving README question: {ctx.question}")
            return None

        answer: str = str(completion.content).strip()
        is_good, _ = self._assess_answer_quality(answer)

        if not is_good:
            return None

        return answer

    def _assess_answer_quality(self, answer: str) -> tuple[bool, str]:
        """Assess the quality of a resolved answer.

        Returns:
            Tuple of (is_acceptable, confidence_level).
        """
        if not answer:
            return (False, "LOW")

        lower = answer.lower()
        # Strip markdown bold/italic for checks
        stripped = re.sub(r"\*{1,2}([^*]+)\*{1,2}", r"\1", lower)

        # Explicit inconclusive (LLM may wrap in markdown bold)
        first_line = stripped.split("\n", 1)[0].strip()
        if first_line.startswith("inconclusive:") or first_line.startswith("inconclusive -"):
            return (False, "LOW")

        # Hedging language
        hedging = [
            "could not find",
            "unable to determine",
            "unclear from the code",
            "cannot determine",
            "no evidence found",
            "insufficient information",
            "not defined in the accessible",
            "not found in the",
        ]
        if any(phrase in stripped for phrase in hedging):
            return (False, "LOW")

        # High confidence indicators: specific code references
        high_indicators = [
            re.compile(r"line\s+\d+", re.IGNORECASE),
            re.compile(r"\b[A-Z][A-Z0-9-]{2,}\b"),  # COBOL-style identifiers
            re.compile(r"PERFORM\s+", re.IGNORECASE),
            re.compile(r"CALL\s+'", re.IGNORECASE),
            re.compile(r"COPY\s+", re.IGNORECASE),
        ]
        has_evidence = any(p.search(answer) for p in high_indicators)

        if has_evidence:
            return (True, "HIGH")

        return (True, "MEDIUM")

    def _update_template(
        self,
        doc_path: Path,
        question_index: int,
        resolved: ResolvedQuestion,
    ) -> None:
        """Move a question from open_questions to resolved_questions in template."""
        try:
            data = json.loads(doc_path.read_text(encoding="utf-8"))

            # Remove from open_questions
            oq_list = data.get("open_questions", [])
            if question_index < len(oq_list):
                oq_list.pop(question_index)
            data["open_questions"] = oq_list

            # Append to resolved_questions
            rq_list = data.get("resolved_questions", [])
            rq_list.append(resolved.model_dump())
            data["resolved_questions"] = rq_list

            doc_path.write_text(
                json.dumps(data, indent=2, ensure_ascii=False),
                encoding="utf-8",
            )

            # Regenerate .md from updated template
            self._regenerate_markdown(doc_path, data)

            logger.debug(
                f"Updated {doc_path.name}: moved question to resolved"
            )
        except Exception as e:
            logger.warning(f"Failed to update template {doc_path}: {e}")

    def _regenerate_markdown(self, doc_path: Path, data: dict) -> None:
        """Regenerate the .md file from the updated template data."""
        try:
            from war_rig.config import SystemConfig
            from war_rig.io.writer import DocumentationWriter

            template = DocumentationTemplate.load_lenient(data)
            sys_config = SystemConfig(output_directory=self._output_directory)
            writer = DocumentationWriter(sys_config)
            md_content = writer._template_to_markdown(template)

            md_path = doc_path.with_suffix("").with_suffix(".md")
            md_path.write_text(md_content, encoding="utf-8")
        except Exception as e:
            logger.debug(f"Failed to regenerate markdown for {doc_path}: {e}")

    def _update_readme(self, resolved_questions: dict[str, str]) -> None:
        """Replace ❓ QUESTION: markers in README.md with answers."""
        readme_path = self._output_directory / "README.md"
        if not readme_path.exists():
            return

        content = readme_path.read_text(encoding="utf-8")
        for question_text, answer in resolved_questions.items():
            # Escape for regex
            escaped = re.escape(question_text)
            pattern = rf"❓\s*\*{{0,2}}QUESTION\*{{0,2}}:\s*{escaped}"
            content = re.sub(pattern, answer, content)

        readme_path.write_text(content, encoding="utf-8")
        logger.debug(
            f"Updated README.md: replaced {len(resolved_questions)} questions"
        )
