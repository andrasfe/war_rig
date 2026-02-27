"""Call semantics analyzer for inferring data flow between paragraphs.

This module provides the CallSemanticsAnalyzer class which uses Citadel's
static analysis combined with LLM inference to determine what data flows
between caller and callee paragraphs in COBOL programs.

The analyzer:
1. Uses Citadel to identify call edges between paragraphs
2. Extracts function bodies for context
3. Batches multiple calls per LLM request for efficiency
4. Parses structured JSON responses into CallSemantics models

Example:
    from war_rig.analysis.call_semantics import CallSemanticsAnalyzer
    from war_rig.config import load_config

    config = load_config()
    analyzer = CallSemanticsAnalyzer(config.api)

    semantics = await analyzer.analyze_file(
        source_path=Path("PROGRAM.cbl"),
        citadel_context=citadel.get_functions("PROGRAM.cbl"),
        working_storage=working_storage_text,
    )

    for cs in semantics:
        print(f"{cs.caller} -> {cs.callee}: {cs.purpose}")
        print(f"  Inputs: {cs.inputs}")
        print(f"  Outputs: {cs.outputs}")
"""

import json
import logging
import re
from pathlib import Path
from typing import Any

from war_rig.config import APIConfig
from war_rig.models.templates import CallSemantics
from war_rig.providers import LLMProvider, Message, get_provider_from_env

logger = logging.getLogger(__name__)

# Default batch size for LLM requests
DEFAULT_BATCH_SIZE = 5

# Maximum tokens for working storage context (to avoid overwhelming the prompt)
MAX_WORKING_STORAGE_CHARS = 4000


class CallSemanticsAnalyzer:
    """Analyzes COBOL paragraph calls to infer data flow semantics.

    Uses Citadel for call graph and function bodies, then asks the LLM
    to infer what data flows in/out of each PERFORM call.

    Attributes:
        api_config: API configuration for the LLM provider.
        provider: The LLM provider instance for making inference calls.
        batch_size: Number of call edges to analyze per LLM request.
    """

    def __init__(
        self,
        api_config: APIConfig,
        model: str | None = None,
        provider: LLMProvider | None = None,
        batch_size: int = DEFAULT_BATCH_SIZE,
    ):
        """Initialize with LLM configuration.

        Args:
            api_config: API configuration for the LLM provider.
            model: Model identifier to use for LLM calls. If not provided,
                defaults to Claude Sonnet.
            provider: Optional LLM provider instance. If not provided,
                creates one from environment variables.
            batch_size: Number of call edges to process per LLM request.
                Defaults to 5 for a good balance of efficiency and context.
        """
        self.api_config = api_config
        self.model = model or "anthropic/claude-sonnet-4-20250514"
        self._provider = provider or get_provider_from_env()
        self.batch_size = batch_size

    @property
    def provider(self) -> LLMProvider:
        """Get the LLM provider instance."""
        return self._provider

    @provider.setter
    def provider(self, value: LLMProvider) -> None:
        """Set the LLM provider instance (useful for testing)."""
        self._provider = value

    async def analyze_file(
        self,
        source_path: Path,
        citadel_context: list[dict[str, Any]],
        working_storage: str | None = None,
    ) -> list[CallSemantics]:
        """Analyze all calls in a file and infer semantics.

        Args:
            source_path: Path to the COBOL source file.
            citadel_context: Context from Citadel with functions list.
                Each function dict should have: name, type, line, line_end, calls.
            working_storage: Optional DATA DIVISION text for context.

        Returns:
            List of CallSemantics for each call edge.
        """
        if not citadel_context:
            logger.debug("No citadel context provided, returning empty semantics")
            return []

        # Extract call edges from citadel context
        call_edges = self._extract_call_edges(citadel_context)

        if not call_edges:
            logger.debug("No call edges found in citadel context")
            return []

        logger.info(
            f"CallSemanticsAnalyzer: Found {len(call_edges)} call edges in {source_path.name}"
        )

        # Get function bodies for all relevant paragraphs
        function_names = set()
        for caller, callee in call_edges:
            function_names.add(caller)
            function_names.add(callee)

        bodies = self._get_function_bodies(source_path, list(function_names))

        # Truncate working storage if too long
        ws_context = self._truncate_working_storage(working_storage)

        # Process call edges in batches
        all_semantics: list[CallSemantics] = []

        for i in range(0, len(call_edges), self.batch_size):
            batch = call_edges[i : i + self.batch_size]
            batch_num = i // self.batch_size + 1
            total_batches = (len(call_edges) + self.batch_size - 1) // self.batch_size

            logger.debug(
                f"Processing batch {batch_num}/{total_batches} "
                f"({len(batch)} call edges)"
            )

            try:
                batch_semantics = await self._analyze_batch(
                    batch, bodies, ws_context, source_path.name
                )
                all_semantics.extend(batch_semantics)
            except Exception as e:
                # Log warning but don't fail the pipeline
                logger.warning(
                    f"Failed to analyze batch {batch_num}: {e}. "
                    "Returning empty semantics for this batch."
                )
                # Return empty semantics for failed batch
                for caller, callee in batch:
                    all_semantics.append(
                        CallSemantics(
                            caller=caller,
                            callee=callee,
                            inputs=[],
                            outputs=[],
                            purpose=None,
                        )
                    )

        logger.info(
            f"CallSemanticsAnalyzer: Completed analysis with "
            f"{len(all_semantics)} call semantics"
        )

        return all_semantics

    def _extract_call_edges(
        self, citadel_context: list[dict[str, Any]]
    ) -> list[tuple[str, str]]:
        """Extract caller-callee pairs from Citadel function data.

        Args:
            citadel_context: List of function dicts from Citadel's get_functions().

        Returns:
            List of (caller_name, callee_name) tuples.
        """
        edges: list[tuple[str, str]] = []

        for func in citadel_context:
            caller_name = func.get("name", "")
            if not caller_name:
                continue

            calls = func.get("calls", [])
            for call in calls:
                # call is a dict with target, type, line
                callee_name = call.get("target", "")
                call_type = call.get("type", "").lower()

                # Only consider PERFORM calls for paragraph-to-paragraph semantics
                # Exclude CALL (external programs), includes, etc.
                if callee_name and call_type in ("performs", "perform", ""):
                    edges.append((caller_name, callee_name))

        return edges

    def _get_function_bodies(
        self, source_path: Path, function_names: list[str]
    ) -> dict[str, str | None]:
        """Get function bodies using Citadel SDK.

        Args:
            source_path: Path to the source file.
            function_names: List of function names to extract.

        Returns:
            Dict mapping function name to body text (or None if not found).
        """
        try:
            # Import Citadel SDK dynamically - it's an optional dependency
            from citadel.sdk import (
                get_function_bodies,  # type: ignore[import-not-found]
            )

            result: dict[str, str | None] = get_function_bodies(
                source_path, function_names
            )
        except ImportError:
            logger.warning("Citadel SDK not available, cannot extract function bodies")
            return dict.fromkeys(function_names)
        except Exception as e:
            logger.warning(f"Failed to get function bodies: {e}")
            return dict.fromkeys(function_names)

        # For COBOL files, prefer AST over raw function bodies
        if str(source_path).lower().endswith((".cbl", ".cob")):
            try:
                from citadel.sdk import Citadel  # type: ignore[import-not-found]

                from war_rig.utils.copybook_dirs import derive_copybook_dirs

                cb_dirs = derive_copybook_dirs(source_path.parent)
                parse_result = Citadel().parse_cobol(
                    str(source_path), copybook_dirs=cb_dirs,
                )
                ast_bodies = parse_result.paragraph_asts
                if ast_bodies:
                    for name in function_names:
                        upper = name.upper()
                        if upper in ast_bodies:
                            result[name] = ast_bodies[upper]
            except Exception:
                pass  # Fall back to raw bodies

        return result

    def _truncate_working_storage(self, working_storage: str | None) -> str | None:
        """Truncate working storage to fit within token limits.

        Args:
            working_storage: Full working storage text.

        Returns:
            Truncated working storage or None.
        """
        if not working_storage:
            return None

        if len(working_storage) <= MAX_WORKING_STORAGE_CHARS:
            return working_storage

        # Truncate with indicator
        truncated = working_storage[:MAX_WORKING_STORAGE_CHARS]
        return truncated + "\n... [TRUNCATED]"

    async def _analyze_batch(
        self,
        batch: list[tuple[str, str]],
        bodies: dict[str, str | None],
        working_storage: str | None,
        file_name: str,
    ) -> list[CallSemantics]:
        """Analyze a batch of call edges using the LLM.

        Args:
            batch: List of (caller, callee) tuples to analyze.
            bodies: Dict mapping paragraph name to body text.
            working_storage: Optional working storage context.
            file_name: Name of the source file (for logging).

        Returns:
            List of CallSemantics for this batch.
        """
        # Build the prompt
        prompt = self._build_batch_prompt(batch, bodies, working_storage)

        # Call the LLM
        messages = [
            Message(role="system", content=self._build_system_prompt()),
            Message(role="user", content=prompt),
        ]

        # Some models require specific temperature settings
        # o3 and other reasoning models require temperature=1.0
        temperature = 0.3
        model_lower = self.model.lower()
        if any(m in model_lower for m in ["o3", "o1-", "o1/"]):
            temperature = 1.0

        response = await self._provider.complete(
            messages=messages,
            model=self.model,
            temperature=temperature,
        )

        # Parse the response
        return self._parse_batch_response(response.content, batch)

    def _build_system_prompt(self) -> str:
        """Build the system prompt for call semantics inference.

        Returns:
            System prompt string.
        """
        return """You are a COBOL code analyst specializing in data flow analysis.

Your task is to analyze PERFORM calls between paragraphs and determine:
1. What WORKING-STORAGE variables the caller sets up before the PERFORM
2. What variables the callee reads/uses as inputs
3. What variables the callee modifies/outputs
4. The purpose of the call in one sentence

Focus on CONCRETE variable names from the code. Do not invent variables.
If you cannot determine inputs/outputs, return empty lists.

Respond ONLY with valid JSON. No markdown, no explanations outside JSON."""

    def _build_batch_prompt(
        self,
        batch: list[tuple[str, str]],
        bodies: dict[str, str | None],
        working_storage: str | None,
    ) -> str:
        """Build prompt for analyzing a batch of calls.

        Args:
            batch: List of (caller, callee) tuples.
            bodies: Dict mapping paragraph name to body text.
            working_storage: Optional working storage context.

        Returns:
            Prompt string.
        """
        parts = []

        # Working storage context (if available)
        if working_storage:
            parts.append("## WORKING-STORAGE Section")
            parts.append("```cobol")
            parts.append(working_storage)
            parts.append("```")
            parts.append("")

        # Call edges to analyze
        parts.append("## Calls to Analyze")
        parts.append("")

        for i, (caller, callee) in enumerate(batch, start=1):
            parts.append(f"### Call {i}: {caller} -> {callee}")
            parts.append("")

            # Caller body
            caller_body = bodies.get(caller)
            if caller_body:
                parts.append(f"**{caller} (Caller):**")
                parts.append("```cobol")
                parts.append(caller_body)
                parts.append("```")
            else:
                parts.append(f"**{caller} (Caller):** [Body not available]")
            parts.append("")

            # Callee body
            callee_body = bodies.get(callee)
            if callee_body:
                parts.append(f"**{callee} (Callee):**")
                parts.append("```cobol")
                parts.append(callee_body)
                parts.append("```")
            else:
                parts.append(f"**{callee} (Callee):** [Body not available]")
            parts.append("")

        # Response format
        parts.append("## Required Response Format")
        parts.append("")
        parts.append("Respond with a JSON object containing a `calls` array:")
        parts.append("```json")
        parts.append("{")
        parts.append('  "calls": [')
        parts.append("    {")
        parts.append('      "caller": "PARAGRAPH-NAME",')
        parts.append('      "callee": "PARAGRAPH-NAME",')
        parts.append(
            '      "inputs": ["WS-VAR-1", "WS-VAR-2"],  // Variables callee reads'
        )
        parts.append('      "outputs": ["WS-VAR-3"],  // Variables callee modifies')
        parts.append('      "purpose": "One sentence describing what this call does"')
        parts.append("    }")
        parts.append("  ]")
        parts.append("}")
        parts.append("```")
        parts.append("")
        parts.append("Rules:")
        parts.append("- Include ALL calls from the Calls to Analyze section")
        parts.append("- Use exact variable names from the code")
        parts.append("- Empty lists are acceptable if data flow is unclear")
        parts.append("- Purpose should be a single sentence")

        return "\n".join(parts)

    def _build_prompt(
        self,
        caller_name: str,
        caller_body: str,
        callee_name: str,
        callee_body: str,
        working_storage: str | None = None,
    ) -> str:
        """Build prompt for inferring call semantics (single call).

        This is a simpler version for single-call analysis.

        Args:
            caller_name: Name of the calling paragraph.
            caller_body: Source code of the caller.
            callee_name: Name of the called paragraph.
            callee_body: Source code of the callee.
            working_storage: Optional working storage context.

        Returns:
            Prompt string.
        """
        return self._build_batch_prompt(
            batch=[(caller_name, callee_name)],
            bodies={caller_name: caller_body, callee_name: callee_body},
            working_storage=working_storage,
        )

    def _parse_batch_response(
        self, response: str, batch: list[tuple[str, str]]
    ) -> list[CallSemantics]:
        """Parse LLM response into CallSemantics models.

        Args:
            response: Raw LLM response text.
            batch: Original batch of (caller, callee) tuples for fallback.

        Returns:
            List of CallSemantics objects.
        """
        # Try to extract JSON from response
        json_match = re.search(r"\{[\s\S]*\}", response)
        if not json_match:
            logger.warning("No JSON found in LLM response, returning empty semantics")
            return self._empty_semantics_for_batch(batch)

        try:
            data = json.loads(json_match.group())
        except json.JSONDecodeError as e:
            logger.warning(f"Failed to parse JSON response: {e}")
            return self._empty_semantics_for_batch(batch)

        # Extract calls array
        calls_data = data.get("calls", [])
        if not calls_data:
            logger.warning("No 'calls' array in response, checking for flat response")
            # Maybe the response is a single call object
            if "caller" in data:
                calls_data = [data]
            else:
                return self._empty_semantics_for_batch(batch)

        # Parse each call into CallSemantics
        results: list[CallSemantics] = []
        parsed_pairs: set[tuple[str, str]] = set()

        for call_data in calls_data:
            try:
                semantics = self._parse_response(
                    call_data,
                    call_data.get("caller", ""),
                    call_data.get("callee", ""),
                )
                results.append(semantics)
                parsed_pairs.add((semantics.caller, semantics.callee))
            except Exception as e:
                logger.warning(f"Failed to parse call semantics: {e}")

        # Add empty semantics for any missing pairs
        for caller, callee in batch:
            if (caller, callee) not in parsed_pairs:
                results.append(
                    CallSemantics(
                        caller=caller,
                        callee=callee,
                        inputs=[],
                        outputs=[],
                        purpose=None,
                    )
                )

        return results

    def _parse_response(
        self, data: dict[str, Any], caller: str, callee: str
    ) -> CallSemantics:
        """Parse a single call response into CallSemantics model.

        Args:
            data: Dict with call semantics data.
            caller: Fallback caller name.
            callee: Fallback callee name.

        Returns:
            CallSemantics object.
        """
        return CallSemantics(
            caller=data.get("caller", caller),
            callee=data.get("callee", callee),
            inputs=self._ensure_list(data.get("inputs", [])),
            outputs=self._ensure_list(data.get("outputs", [])),
            purpose=data.get("purpose"),
        )

    def _ensure_list(self, value: Any) -> list[str]:
        """Ensure a value is a list of strings.

        Args:
            value: Any value to convert.

        Returns:
            List of strings.
        """
        if value is None:
            return []
        if isinstance(value, str):
            return [value] if value.strip() else []
        if isinstance(value, list):
            return [str(v) for v in value if v]
        return []

    def _empty_semantics_for_batch(
        self, batch: list[tuple[str, str]]
    ) -> list[CallSemantics]:
        """Create empty CallSemantics for a failed batch.

        Args:
            batch: List of (caller, callee) tuples.

        Returns:
            List of CallSemantics with empty inputs/outputs.
        """
        return [
            CallSemantics(
                caller=caller,
                callee=callee,
                inputs=[],
                outputs=[],
                purpose=None,
            )
            for caller, callee in batch
        ]
