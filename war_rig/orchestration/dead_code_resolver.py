"""Automatic dead code resolution using CodeWhisper SDK.

Resolves dead code candidates in .doc.json templates by investigating
the source code for cross-file references, ALTER usage, and dynamic
dispatch patterns.
"""

import asyncio
import json
import logging
import time
from dataclasses import dataclass, field
from pathlib import Path
from typing import TYPE_CHECKING

from war_rig.config import DeadCodeResolutionConfig
from war_rig.models.templates import (
    DocumentationTemplate,
    ResolvedDeadCode,
)

if TYPE_CHECKING:
    from codewhisper.sdk import CodeWhisper  # type: ignore[import-not-found]

logger = logging.getLogger(__name__)


@dataclass
class DeadCodeCandidateContext:
    """Context for a dead code candidate from a .doc.json template."""

    doc_path: Path
    file_name: str
    program_id: str
    candidate_name: str
    candidate_reason: str
    candidate_index: int
    template: DocumentationTemplate


@dataclass
class DeadCodeResolutionResult:
    """Summary of dead code resolution for a cycle."""

    candidates_found: int = 0
    candidates_resolved: int = 0
    confirmed_dead: int = 0
    false_positives: int = 0
    errors: list[str] = field(default_factory=list)
    duration_seconds: float = 0.0


class DeadCodeResolver:
    """Resolves dead code candidates using CodeWhisper SDK investigations."""

    def __init__(
        self,
        config: DeadCodeResolutionConfig,
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

    async def resolve_all(self) -> DeadCodeResolutionResult:
        """Resolve dead code candidates from component templates.

        Candidates are resolved concurrently with a semaphore to bound
        parallelism. Each task creates its own SDK instance. Template
        updates are applied sequentially after all resolutions complete
        (in reverse index order to avoid index shifting).

        Returns:
            Summary of resolution results.
        """
        start = time.monotonic()
        result = DeadCodeResolutionResult()

        if not self._config.enabled:
            return result

        # Collect candidates
        candidates = self._collect_candidates()
        result.candidates_found = len(candidates)

        if not candidates:
            logger.info("No dead code candidates to resolve")
            result.duration_seconds = time.monotonic() - start
            return result

        # Cap per cycle
        cap = self._config.max_candidates_per_cycle
        if len(candidates) > cap:
            candidates = candidates[:cap]

        # Sort by (file, descending index) for safe reverse-order removal
        candidates.sort(
            key=lambda c: (str(c.doc_path), -c.candidate_index)
        )

        logger.info(
            f"Resolving {len(candidates)} dead code candidates (cap={cap})"
        )

        # Verify SDK creation
        try:
            test_sdk = self._create_sdk()
            del test_sdk
        except Exception as e:
            logger.warning(f"Failed to create CodeWhisper SDK: {e}")
            result.errors.append(f"SDK creation failed: {e}")
            result.duration_seconds = time.monotonic() - start
            return result

        semaphore = asyncio.Semaphore(self._MAX_CONCURRENCY)

        async def _resolve_one(
            ctx: DeadCodeCandidateContext,
        ) -> tuple[DeadCodeCandidateContext, ResolvedDeadCode | None, str | None]:
            async with semaphore:
                try:
                    sdk = self._create_sdk()
                    resolved = await self._resolve_candidate(sdk, ctx)
                    return (ctx, resolved, None)
                except Exception as e:
                    msg = f"Error resolving dead code '{ctx.candidate_name}' in {ctx.file_name}: {e}"
                    logger.warning(msg)
                    return (ctx, None, msg)

        results = await asyncio.gather(
            *[_resolve_one(ctx) for ctx in candidates]
        )

        # Apply template updates sequentially
        for ctx, resolved, error in results:
            if error is not None:
                result.errors.append(error)
            elif resolved is not None:
                self._update_template(ctx.doc_path, ctx.candidate_index, resolved)
                result.candidates_resolved += 1
                if resolved.is_dead:
                    result.confirmed_dead += 1
                else:
                    result.false_positives += 1

        result.duration_seconds = time.monotonic() - start
        return result

    def _collect_candidates(self) -> list[DeadCodeCandidateContext]:
        """Scan output directory for .doc.json files with dead_code items."""
        candidates: list[DeadCodeCandidateContext] = []

        for doc_path in self._output_directory.rglob("*.doc.json"):
            try:
                data = json.loads(doc_path.read_text(encoding="utf-8"))
                template = DocumentationTemplate.load_lenient(data)

                if not template.dead_code:
                    continue

                file_name = doc_path.stem.replace(".doc", "")
                program_id = ""
                if template.header and template.header.program_id:
                    program_id = template.header.program_id

                for idx, item in enumerate(template.dead_code):
                    candidates.append(
                        DeadCodeCandidateContext(
                            doc_path=doc_path,
                            file_name=file_name,
                            program_id=program_id,
                            candidate_name=item.name,
                            candidate_reason=item.reason,
                            candidate_index=idx,
                            template=template,
                        )
                    )
            except Exception as e:
                logger.debug(f"Skipping {doc_path}: {e}")

        return candidates

    def _create_sdk(self) -> "CodeWhisper":
        """Create a fresh CodeWhisper SDK instance."""
        from codewhisper.sdk import (  # type: ignore[import-not-found]
            CodeWhisper,
            CodeWhisperConfig,
        )

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
            documents_dir=None,
            config=config,
        )

    async def _resolve_candidate(
        self,
        sdk: "CodeWhisper",
        ctx: DeadCodeCandidateContext,
    ) -> ResolvedDeadCode | None:
        """Resolve a single dead code candidate using CodeWhisper."""
        prompt = (
            f"Investigate whether paragraph '{ctx.candidate_name}' in program "
            f"'{ctx.program_id}' (file '{ctx.file_name}') is truly dead code.\n\n"
            f"Static AST analysis found no PERFORM, GO TO, or PERFORM THRU "
            f"references to this paragraph within the same file.\n\n"
            f"Check for:\n"
            f"1. Is it PERFORMed or GO TO'd from another paragraph?\n"
            f"2. Is it part of a PERFORM THRU range (between start and end)?\n"
            f"3. Is it called via ALTER (dynamic GO TO)?\n"
            f"4. Is it an EXIT paragraph used as a PERFORM THRU endpoint?\n"
            f"5. Could it be called from another program (cross-file CALL)?\n\n"
            f"Answer with either:\n"
            f"- 'CONFIRMED DEAD: <explanation>' if truly unreachable\n"
            f"- 'FALSE POSITIVE: <explanation>' if it IS reachable"
        )

        sdk.reset()
        try:
            completion = await asyncio.wait_for(
                sdk.complete(prompt),
                timeout=self._config.timeout_per_candidate,
            )
        except TimeoutError:
            logger.warning(
                f"Timeout resolving dead code '{ctx.candidate_name}' "
                f"in {ctx.file_name}"
            )
            return None

        answer: str = str(completion.content).strip()
        if not answer:
            return None

        # Parse the verdict
        lower = answer.lower()
        is_dead = lower.startswith("confirmed dead")
        is_false_positive = lower.startswith("false positive")

        if not is_dead and not is_false_positive:
            # Try to infer from content
            if "confirmed dead" in lower:
                is_dead = True
            elif "false positive" in lower:
                is_false_positive = True
            else:
                # Default to confirmed dead if ambiguous
                is_dead = True

        return ResolvedDeadCode(
            name=ctx.candidate_name,
            original_reason=ctx.candidate_reason,
            is_dead=is_dead,
            explanation=answer,
            resolved_by="CODEWHISPER",
            cycle_resolved=self._cycle,
            tool_calls_used=completion.tool_calls_made,
        )

    def _update_template(
        self,
        doc_path: Path,
        candidate_index: int,
        resolved: ResolvedDeadCode,
    ) -> None:
        """Update template: confirmed dead stays, false positive moves to resolved."""
        try:
            data = json.loads(doc_path.read_text(encoding="utf-8"))

            dc_list = data.get("dead_code", [])
            if candidate_index < len(dc_list):
                if not resolved.is_dead:
                    # False positive: remove from dead_code
                    dc_list.pop(candidate_index)
                    data["dead_code"] = dc_list

                    # Also unmark paragraph as dead code
                    for para in data.get("paragraphs", []):
                        if isinstance(para, dict):
                            name = para.get("paragraph_name", "")
                            if name and name.upper() == resolved.name.upper():
                                para["is_dead_code"] = False
                                para["dead_code_reason"] = None

            # Append to dead_code_resolved
            resolved_list = data.get("dead_code_resolved", [])
            resolved_list.append(resolved.model_dump())
            data["dead_code_resolved"] = resolved_list

            doc_path.write_text(
                json.dumps(data, indent=2, ensure_ascii=False),
                encoding="utf-8",
            )

            # Regenerate .md from updated template
            self._regenerate_markdown(doc_path, data)

            verdict = "CONFIRMED" if resolved.is_dead else "FALSE POSITIVE"
            logger.debug(
                f"Updated {doc_path.name}: {resolved.name} → {verdict}"
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
