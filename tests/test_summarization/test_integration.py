"""Integration tests for the War Rig recursive document summarization pipeline.

Tests the full multi-pass pipeline end-to-end:
  Pass 1 (BundleSummarizer) -> Pass 2 (CoherenceMerger) ->
  Pass 3 (RootSummarizer) -> Validation (ValidationLoop)

Uses a realistic mock provider with plausible JSON responses for each pass.
Exercises grouping with 10 chunks, artifact linkage, computed fields,
save/load round-trip, context injection, source_lines support, and
validation loop convergence.
"""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any

import pytest

from war_rig.models.summaries import (
    BundleSummary,
    ClaimCategory,
    ClaimVerdict,
    FileSummary,
    SegmentSummary,
    VerifiableClaim,
)
from war_rig.models.templates import DocumentationTemplate, HeaderSection, Paragraph
from war_rig.providers.protocol import CompletionResponse, Message
from war_rig.summarization.pipeline import SummarizationPipeline
from war_rig.summarization.prompts import CONTEXT_INJECTION_TEMPLATE

# ============================================================================
# Realistic mock JSON responses
# ============================================================================

# Pass 1: one bundle JSON per call -- the provider cycles through these.
# The bundle_id will be overridden by the BundleSummarizer from the input,
# but having it present exercises the happy-path parse branch.
_BUNDLE_RESPONSE_TEMPLATE = """{{\
"bundle_id": "{bundle_id}",\
"paragraph_names": {para_list},\
"functional_summary": "Handles {area} logic for CBACT04C",\
"data_items_read": ["WS-ACCT-ID", "WS-AMOUNT-{idx}"],\
"data_items_written": ["WS-OUT-RECORD-{idx}"],\
"data_items_transformed": [],\
"perform_calls": ["9000-ERROR-HANDLER"],\
"perform_callers": ["0000-MAIN"],\
"conditional_branches": [\
{{"condition": "IF WS-RETURN-CODE = 0",\
"business_meaning": "Successful {area} step",\
"outcome_true": "Continue",\
"outcome_false": "Branch to error"}}],\
"anomalies": []\
}}"""

# Build 10 distinct bundle JSON strings -- the mock provider will match on
# "Bundle ID:" which appears in every Pass 1 prompt, so we return a
# per-call response using a stateful provider.
_FUNCTIONAL_AREAS = [
    "initialisation",
    "account-read",
    "balance-validation",
    "fee-calculation",
    "interest-accrual",
    "overdraft-check",
    "transaction-post",
    "audit-logging",
    "report-generation",
    "finalisation",
]


def _bundle_json(chunk_id: str, para_names: list[str], idx: int) -> str:
    para_list_json = json.dumps(para_names)
    return _BUNDLE_RESPONSE_TEMPLATE.format(
        bundle_id=chunk_id,
        para_list=para_list_json,
        area=_FUNCTIONAL_AREAS[idx % len(_FUNCTIONAL_AREAS)],
        idx=idx,
    )


# Pass 2: two segment responses (10 bundles / group_size=5 = 2 segments).
_SEG_001_JSON = json.dumps(
    {
        "segment_id": "SEG-001",
        "bundle_ids": [
            "CHUNK-000",
            "CHUNK-001",
            "CHUNK-002",
            "CHUNK-003",
            "CHUNK-004",
        ],
        "functional_area": "Account Setup and Validation",
        "summary": (
            "Handles initialisation, account-read, balance-validation, "
            "fee-calculation, and interest-accrual for CBACT04C"
        ),
        "data_flows": [
            {
                "source": "0100-INIT",
                "target": "0200-READ-ACCT",
                "data_item": "WS-ACCT-ID",
                "description": "Account ID set during init, consumed during read",
            }
        ],
        "perform_graph": [
            {
                "caller": "0000-MAIN",
                "callee": "0100-INIT",
                "purpose": "Initialise file handles",
            },
            {
                "caller": "0000-MAIN",
                "callee": "0200-READ-ACCT",
                "purpose": "Read account record",
            },
        ],
        "resolved_coreferences": [
            "WS-RETURN-CODE in CHUNK-000 is the same as WS-STATUS in CHUNK-001"
        ],
        "flagged_contradictions": [],
        "flagged_redundancies": [],
    }
)

_SEG_002_JSON = json.dumps(
    {
        "segment_id": "SEG-002",
        "bundle_ids": [
            "CHUNK-005",
            "CHUNK-006",
            "CHUNK-007",
            "CHUNK-008",
            "CHUNK-009",
        ],
        "functional_area": "Transaction Processing and Reporting",
        "summary": (
            "Handles overdraft-check, transaction-post, audit-logging, "
            "report-generation, and finalisation for CBACT04C"
        ),
        "data_flows": [
            {
                "source": "0600-OVERDRAFT",
                "target": "0700-POST-TXN",
                "data_item": "WS-BALANCE",
                "description": "Balance checked before posting transaction",
            }
        ],
        "perform_graph": [
            {
                "caller": "0000-MAIN",
                "callee": "0700-POST-TXN",
                "purpose": "Post transaction to account",
            }
        ],
        "resolved_coreferences": [
            "WS-AMOUNT-5 in CHUNK-005 feeds directly into WS-OUT-RECORD-6"
        ],
        "flagged_contradictions": [],
        "flagged_redundancies": [
            "CHUNK-007 and CHUNK-008 both describe error-code initialisation"
        ],
    }
)

# Pass 3: root summary with 4 verifiable claims covering all evidence bundles.
_ROOT_SUMMARY_JSON = json.dumps(
    {
        "business_function": (
            "Batch account transaction processor: reads account master file, "
            "validates balances, applies fees and interest, posts transactions, "
            "and produces an audit report"
        ),
        "primary_data_flows": [
            "Input account file -> balance validation -> transaction posting",
            "Fee and interest calculations -> audit report",
            "Return-code propagation from sub-routines to main control loop",
        ],
        "call_graph_summary": [
            "0000-MAIN -> 0100-INIT -> 9000-ERROR-HANDLER",
            "0000-MAIN -> 0200-READ-ACCT -> 0300-VALIDATE-BAL",
            "0000-MAIN -> 0400-CALC-FEES -> 0500-CALC-INT",
            "0000-MAIN -> 0600-OVERDRAFT -> 0700-POST-TXN",
            "0000-MAIN -> 0800-AUDIT -> 0900-REPORT -> 9900-FINALIZE",
        ],
        "risk_areas": [
            "No error handling for file I/O failures in CHUNK-000",
            "Hard-coded interest rate in CHUNK-004",
        ],
        "technical_debt": [
            "Hard-coded file paths in CHUNK-000",
            "Duplicated error-code initialisation in CHUNK-007 and CHUNK-008",
        ],
        "migration_considerations": [
            "Sequential file access pattern requires redesign for cloud storage",
            "Batch-only execution model -- no online equivalent",
        ],
        "claims": [
            {
                "claim_id": "C-001",
                "claim": "0000-MAIN performs 0100-INIT before entering the main loop",
                "category": "PERFORM_RELATIONSHIP",
                "evidence_bundle_ids": ["CHUNK-000", "CHUNK-001"],
            },
            {
                "claim_id": "C-002",
                "claim": "WS-ACCT-ID is read in CHUNK-000 and consumed in CHUNK-001",
                "category": "DATA_FLOW",
                "evidence_bundle_ids": ["CHUNK-000", "CHUNK-001"],
            },
            {
                "claim_id": "C-003",
                "claim": "9000-ERROR-HANDLER is called from all ten processing bundles",
                "category": "PERFORM_RELATIONSHIP",
                "evidence_bundle_ids": [
                    "CHUNK-000",
                    "CHUNK-001",
                    "CHUNK-002",
                    "CHUNK-003",
                    "CHUNK-004",
                    "CHUNK-005",
                    "CHUNK-006",
                    "CHUNK-007",
                    "CHUNK-008",
                    "CHUNK-009",
                ],
            },
            {
                "claim_id": "C-004",
                "claim": "Report generation occurs after all transactions are posted",
                "category": "BUSINESS_RULE",
                "evidence_bundle_ids": ["CHUNK-007", "CHUNK-008", "CHUNK-009"],
            },
        ],
    }
)

# Validation: all four claims confirmed on the first iteration.
_VERIFICATION_ALL_CONFIRMED_JSON = json.dumps(
    [
        {
            "claim_id": "C-001",
            "verdict": "CONFIRMED",
            "notes": "PERFORM 0100-INIT appears before the main processing loop.",
        },
        {
            "claim_id": "C-002",
            "verdict": "CONFIRMED",
            "notes": "WS-ACCT-ID data flow confirmed across CHUNK-000 and CHUNK-001.",
        },
        {
            "claim_id": "C-003",
            "verdict": "CONFIRMED",
            "notes": "9000-ERROR-HANDLER is listed in perform_calls for all bundles.",
        },
        {
            "claim_id": "C-004",
            "verdict": "CONFIRMED",
            "notes": "Report paragraph follows transaction posting paragraphs.",
        },
    ]
)


# ============================================================================
# Stateful mock provider
# ============================================================================


class RealisticPipelineProvider:
    """Mock LLM provider that returns plausible JSON for each pipeline pass.

    Detects which pass is being executed from distinctive phrases in the
    prompt and returns appropriate JSON.  Pass 1 uses a per-call counter
    so each bundle gets a distinct response tied to its index.
    """

    def __init__(self) -> None:
        self._pass1_call_count = 0

    @property
    def default_model(self) -> str:
        return "test-model"

    async def complete(
        self,
        messages: list[Message],
        model: str | None = None,
        temperature: float = 1.0,
        **kwargs: Any,
    ) -> CompletionResponse:
        user_content = messages[-1].content if messages else ""

        # Pass 1: "Bundle ID:" appears in every BundleSummarizer prompt.
        if "Bundle ID:" in user_content:
            idx = self._pass1_call_count
            self._pass1_call_count += 1
            chunk_id = f"CHUNK-{idx:03d}"
            para_names = [
                f"{_FUNCTIONAL_AREAS[idx % len(_FUNCTIONAL_AREAS)].upper()}-A",
                f"{_FUNCTIONAL_AREAS[idx % len(_FUNCTIONAL_AREAS)].upper()}-B",
            ]
            content = _bundle_json(chunk_id, para_names, idx)
            return CompletionResponse(
                content=content,
                model=model or "test-model",
                tokens_used=120,
            )

        # Pass 2: "Merge the following" appears in CoherenceMerger prompts.
        if "Merge the following" in user_content:
            # Return SEG-001 for the first group, SEG-002 for subsequent.
            if "SEG-001" in user_content:
                content = _SEG_001_JSON
            else:
                content = _SEG_002_JSON
            return CompletionResponse(
                content=content,
                model=model or "test-model",
                tokens_used=200,
            )

        # Pass 3: "Synthesize the following" appears in RootSummarizer prompts.
        if "Synthesize the following" in user_content:
            return CompletionResponse(
                content=_ROOT_SUMMARY_JSON,
                model=model or "test-model",
                tokens_used=350,
            )

        # Validation: "Verify the following" appears in ClaimVerifier prompts.
        if "Verify the following" in user_content:
            return CompletionResponse(
                content=_VERIFICATION_ALL_CONFIRMED_JSON,
                model=model or "test-model",
                tokens_used=150,
            )

        # Fallback -- should not be reached in a correctly exercised pipeline.
        return CompletionResponse(
            content="{}",
            model=model or "test-model",
            tokens_used=10,
        )


# ============================================================================
# Fixtures
# ============================================================================


@pytest.fixture
def chunk_templates() -> list[tuple[str, DocumentationTemplate]]:
    """Ten DocumentationTemplate chunks that simulate a large COBOL file.

    Each chunk contains two paragraphs (matching the two-paragraph-per-bundle
    pattern common in CBACT04C-style batch programs).  Ten chunks give enough
    volume to exercise the grouping logic (group_size=5 -> 2 segments).
    """
    templates: list[tuple[str, DocumentationTemplate]] = []
    for i in range(10):
        area = _FUNCTIONAL_AREAS[i]
        para_a_name = f"{area.upper().replace('-', '_')}_A"
        para_b_name = f"{area.upper().replace('-', '_')}_B"
        template = DocumentationTemplate(
            header=HeaderSection(
                program_id="CBACT04C",
                file_name="CBACT04C.cbl",
            ),
            paragraphs=[
                Paragraph(
                    paragraph_name=para_a_name,
                    summary=f"First paragraph handling {area} (step A)",
                ),
                Paragraph(
                    paragraph_name=para_b_name,
                    summary=f"Second paragraph handling {area} (step B)",
                ),
            ],
        )
        templates.append((f"CHUNK-{i:03d}", template))
    return templates


@pytest.fixture
def realistic_provider() -> RealisticPipelineProvider:
    """Fresh provider instance for each test."""
    return RealisticPipelineProvider()


@pytest.fixture
def source_lines() -> list[str]:
    """Minimal COBOL source lines used to exercise the source_lines path."""
    lines = [
        "       IDENTIFICATION DIVISION.\n",
        "       PROGRAM-ID. CBACT04C.\n",
        "       DATA DIVISION.\n",
        "       WORKING-STORAGE SECTION.\n",
        "       01 WS-ACCT-ID        PIC X(10).\n",
        "       01 WS-AMOUNT-0       PIC S9(9)V99.\n",
        "       01 WS-RETURN-CODE    PIC S9(4) COMP.\n",
        "       01 WS-BALANCE        PIC S9(11)V99.\n",
        "       PROCEDURE DIVISION.\n",
        "       0000-MAIN.\n",
        "           PERFORM 0100-INIT\n",
        "           PERFORM UNTIL WS-RETURN-CODE = 8\n",
        "               PERFORM 0200-READ-ACCT\n",
        "               PERFORM 0300-VALIDATE-BAL\n",
        "           END-PERFORM.\n",
        "           PERFORM 0900-REPORT\n",
        "           PERFORM 9900-FINALIZE\n",
        "           STOP RUN.\n",
        "       0100-INIT.\n",
        "           MOVE 0 TO WS-RETURN-CODE\n",
        "           OPEN INPUT ACCT-FILE\n",
        "           IF WS-RETURN-CODE NOT = 0\n",
        "               PERFORM 9000-ERROR-HANDLER\n",
        "           END-IF.\n",
    ]
    # Pad to 200 lines so source_line_start/end references are valid.
    lines += [f"      * line {i}\n" for i in range(len(lines), 200)]
    return lines


# ============================================================================
# Integration tests
# ============================================================================


@pytest.mark.integration
class TestSummarizationPipelineIntegration:
    """End-to-end integration tests for the full summarization pipeline."""

    async def test_full_pipeline_produces_three_artifact_levels(
        self,
        chunk_templates: list[tuple[str, DocumentationTemplate]],
        realistic_provider: RealisticPipelineProvider,
    ) -> None:
        """All three artifact levels are produced with correct counts."""
        pipeline = SummarizationPipeline(
            provider=realistic_provider,
            model="test-model",
            merge_group_size=5,
            max_validation_iterations=3,
        )

        result = await pipeline.run(
            program_id="CBACT04C",
            file_name="CBACT04C.cbl",
            chunk_templates=chunk_templates,
        )

        assert isinstance(result, FileSummary)

        # Pass 1: one bundle per chunk.
        assert len(result.bundle_summaries) == 10

        # Pass 2: ceil(10 / 5) = 2 segments.
        assert len(result.segment_summaries) == 2

        # Pass 3: root summary populated.
        assert result.passes_completed == 3
        assert result.business_function != ""
        assert len(result.primary_data_flows) >= 1
        assert len(result.call_graph_summary) >= 1

        # Validation ran.
        assert result.validation_iterations >= 1

    @pytest.mark.integration
    async def test_bundle_ids_referenced_in_segment_summaries(
        self,
        chunk_templates: list[tuple[str, DocumentationTemplate]],
        realistic_provider: RealisticPipelineProvider,
    ) -> None:
        """Every bundle_id in a SegmentSummary.bundle_ids exists in bundle_summaries."""
        pipeline = SummarizationPipeline(
            provider=realistic_provider,
            model="test-model",
            merge_group_size=5,
            max_validation_iterations=1,
        )

        result = await pipeline.run(
            program_id="CBACT04C",
            file_name="CBACT04C.cbl",
            chunk_templates=chunk_templates,
        )

        all_bundle_ids = {b.bundle_id for b in result.bundle_summaries}

        for seg in result.segment_summaries:
            assert len(seg.bundle_ids) > 0, (
                f"Segment {seg.segment_id} has no bundle_ids"
            )
            for bid in seg.bundle_ids:
                assert bid in all_bundle_ids, (
                    f"Segment {seg.segment_id} references unknown bundle_id {bid!r}"
                )

    @pytest.mark.integration
    async def test_claim_evidence_bundle_ids_exist(
        self,
        chunk_templates: list[tuple[str, DocumentationTemplate]],
        realistic_provider: RealisticPipelineProvider,
    ) -> None:
        """Every claim's evidence_bundle_ids reference existing bundles."""
        pipeline = SummarizationPipeline(
            provider=realistic_provider,
            model="test-model",
            merge_group_size=5,
            max_validation_iterations=1,
        )

        result = await pipeline.run(
            program_id="CBACT04C",
            file_name="CBACT04C.cbl",
            chunk_templates=chunk_templates,
        )

        assert len(result.claims) > 0, "Pipeline should produce at least one claim"

        all_bundle_ids = {b.bundle_id for b in result.bundle_summaries}

        for claim in result.claims:
            for bid in claim.evidence_bundle_ids:
                assert bid in all_bundle_ids, (
                    f"Claim {claim.claim_id} references unknown bundle_id {bid!r}"
                )

    @pytest.mark.integration
    async def test_computed_fields_total_paragraphs(
        self,
        chunk_templates: list[tuple[str, DocumentationTemplate]],
        realistic_provider: RealisticPipelineProvider,
    ) -> None:
        """total_paragraphs is the sum of paragraph_names across all bundles."""
        pipeline = SummarizationPipeline(
            provider=realistic_provider,
            model="test-model",
            merge_group_size=5,
            max_validation_iterations=1,
        )

        result = await pipeline.run(
            program_id="CBACT04C",
            file_name="CBACT04C.cbl",
            chunk_templates=chunk_templates,
        )

        expected = sum(len(b.paragraph_names) for b in result.bundle_summaries)
        assert result.total_paragraphs == expected
        # Each mock bundle response contains 2 paragraph names, 10 bundles.
        assert result.total_paragraphs == 20

    @pytest.mark.integration
    async def test_computed_fields_claims_verified_and_refuted(
        self,
        chunk_templates: list[tuple[str, DocumentationTemplate]],
        realistic_provider: RealisticPipelineProvider,
    ) -> None:
        """claims_verified counts CONFIRMED, claims_refuted counts REFUTED."""
        pipeline = SummarizationPipeline(
            provider=realistic_provider,
            model="test-model",
            merge_group_size=5,
            max_validation_iterations=3,
        )

        result = await pipeline.run(
            program_id="CBACT04C",
            file_name="CBACT04C.cbl",
            chunk_templates=chunk_templates,
        )

        confirmed = sum(
            1 for c in result.claims if c.verdict == ClaimVerdict.CONFIRMED
        )
        refuted = sum(1 for c in result.claims if c.verdict == ClaimVerdict.REFUTED)

        assert result.claims_verified == confirmed
        assert result.claims_refuted == refuted

        # Our mock returns all CONFIRMED, so all claims should be confirmed.
        assert result.claims_verified == len(result.claims)
        assert result.claims_refuted == 0

    @pytest.mark.integration
    async def test_validation_loop_converges_with_all_confirmed(
        self,
        chunk_templates: list[tuple[str, DocumentationTemplate]],
        realistic_provider: RealisticPipelineProvider,
    ) -> None:
        """Validation loop stops after one iteration when all claims are CONFIRMED."""
        pipeline = SummarizationPipeline(
            provider=realistic_provider,
            model="test-model",
            merge_group_size=5,
            max_validation_iterations=5,
        )

        result = await pipeline.run(
            program_id="CBACT04C",
            file_name="CBACT04C.cbl",
            chunk_templates=chunk_templates,
        )

        # Loop must have run at least once.
        assert result.validation_iterations >= 1
        # No refuted claims -- loop converged without exhausting the budget.
        assert result.claims_refuted == 0
        # All claim verdicts are populated (not None).
        assert all(c.verdict is not None for c in result.claims)

    @pytest.mark.integration
    async def test_save_load_round_trip_preserves_all_data(
        self,
        chunk_templates: list[tuple[str, DocumentationTemplate]],
        realistic_provider: RealisticPipelineProvider,
        tmp_path: Path,
    ) -> None:
        """save_summary / load_summary preserves the full FileSummary faithfully."""
        pipeline = SummarizationPipeline(
            provider=realistic_provider,
            model="test-model",
            merge_group_size=5,
            max_validation_iterations=1,
        )

        original = await pipeline.run(
            program_id="CBACT04C",
            file_name="CBACT04C.cbl",
            chunk_templates=chunk_templates,
        )

        output_path = tmp_path / "CBACT04C.summary.json"
        SummarizationPipeline.save_summary(original, output_path)
        assert output_path.exists()

        loaded = SummarizationPipeline.load_summary(output_path)

        # Top-level fields.
        assert loaded.program_id == original.program_id
        assert loaded.file_name == original.file_name
        assert loaded.business_function == original.business_function
        assert loaded.passes_completed == original.passes_completed
        assert loaded.validation_iterations == original.validation_iterations
        assert loaded.primary_data_flows == original.primary_data_flows
        assert loaded.call_graph_summary == original.call_graph_summary
        assert loaded.risk_areas == original.risk_areas
        assert loaded.technical_debt == original.technical_debt
        assert loaded.migration_considerations == original.migration_considerations

        # Bundle summaries preserved.
        assert len(loaded.bundle_summaries) == len(original.bundle_summaries)
        for orig_b, load_b in zip(original.bundle_summaries, loaded.bundle_summaries, strict=True):
            assert load_b.bundle_id == orig_b.bundle_id
            assert load_b.paragraph_names == orig_b.paragraph_names
            assert load_b.functional_summary == orig_b.functional_summary
            assert load_b.data_items_read == orig_b.data_items_read
            assert load_b.data_items_written == orig_b.data_items_written
            assert load_b.perform_calls == orig_b.perform_calls

        # Segment summaries preserved.
        assert len(loaded.segment_summaries) == len(original.segment_summaries)
        for orig_s, load_s in zip(
            original.segment_summaries, loaded.segment_summaries, strict=True
        ):
            assert load_s.segment_id == orig_s.segment_id
            assert load_s.functional_area == orig_s.functional_area
            assert load_s.bundle_ids == orig_s.bundle_ids
            assert load_s.summary == orig_s.summary

        # Claims preserved including verdicts and notes.
        assert len(loaded.claims) == len(original.claims)
        for orig_c, load_c in zip(original.claims, loaded.claims, strict=True):
            assert load_c.claim_id == orig_c.claim_id
            assert load_c.claim == orig_c.claim
            assert load_c.category == orig_c.category
            assert load_c.evidence_bundle_ids == orig_c.evidence_bundle_ids
            assert load_c.verdict == orig_c.verdict
            assert load_c.verification_notes == orig_c.verification_notes

        # Computed fields are re-derived on the loaded instance.
        assert loaded.total_paragraphs == original.total_paragraphs
        assert loaded.claims_verified == original.claims_verified
        assert loaded.claims_refuted == original.claims_refuted

    @pytest.mark.integration
    async def test_context_injection_to_context_string(
        self,
        chunk_templates: list[tuple[str, DocumentationTemplate]],
        realistic_provider: RealisticPipelineProvider,
    ) -> None:
        """to_context_string() renders the expected compact Markdown summary."""
        pipeline = SummarizationPipeline(
            provider=realistic_provider,
            model="test-model",
            merge_group_size=5,
            max_validation_iterations=1,
        )

        result = await pipeline.run(
            program_id="CBACT04C",
            file_name="CBACT04C.cbl",
            chunk_templates=chunk_templates,
        )

        ctx = result.to_context_string()

        assert "## File-Level Summary: CBACT04C.cbl" in ctx
        assert "**Business Function**:" in ctx
        assert result.business_function in ctx
        assert "**Primary Data Flows**:" in ctx
        for flow in result.primary_data_flows:
            assert f"- {flow}" in ctx
        assert "**Call Graph**:" in ctx
        for entry in result.call_graph_summary:
            assert f"- {entry}" in ctx
        assert "**Risk Areas**:" in ctx
        for risk in result.risk_areas:
            assert f"- {risk}" in ctx
        assert "**Functional Areas**:" in ctx
        for seg in result.segment_summaries:
            assert seg.functional_area in ctx
            assert seg.summary in ctx

    @pytest.mark.integration
    async def test_context_injection_template_wraps_context_string(
        self,
        chunk_templates: list[tuple[str, DocumentationTemplate]],
        realistic_provider: RealisticPipelineProvider,
    ) -> None:
        """CONTEXT_INJECTION_TEMPLATE wraps to_context_string() output correctly."""
        pipeline = SummarizationPipeline(
            provider=realistic_provider,
            model="test-model",
            merge_group_size=5,
            max_validation_iterations=1,
        )

        result = await pipeline.run(
            program_id="CBACT04C",
            file_name="CBACT04C.cbl",
            chunk_templates=chunk_templates,
        )

        ctx_string = result.to_context_string()
        injected = CONTEXT_INJECTION_TEMPLATE.format(summary_text=ctx_string)

        assert "## Program-Level Context" in injected
        assert "## File-Level Summary: CBACT04C.cbl" in injected
        assert result.business_function in injected
        # The template header must precede the summary body.
        header_pos = injected.index("## Program-Level Context")
        summary_pos = injected.index("## File-Level Summary:")
        assert header_pos < summary_pos

    @pytest.mark.integration
    async def test_pipeline_with_source_lines(
        self,
        chunk_templates: list[tuple[str, DocumentationTemplate]],
        source_lines: list[str],
    ) -> None:
        """Pipeline succeeds and produces valid output when source_lines are provided."""
        provider = RealisticPipelineProvider()
        pipeline = SummarizationPipeline(
            provider=provider,
            model="test-model",
            merge_group_size=5,
            max_validation_iterations=2,
        )

        result = await pipeline.run(
            program_id="CBACT04C",
            file_name="CBACT04C.cbl",
            chunk_templates=chunk_templates,
            source_lines=source_lines,
        )

        assert isinstance(result, FileSummary)
        assert result.program_id == "CBACT04C"
        assert result.passes_completed == 3
        assert len(result.bundle_summaries) == 10
        assert len(result.segment_summaries) == 2
        assert len(result.claims) >= 1
        assert result.validation_iterations >= 1

    @pytest.mark.integration
    async def test_pipeline_without_source_lines(
        self,
        chunk_templates: list[tuple[str, DocumentationTemplate]],
    ) -> None:
        """Pipeline succeeds when source_lines are omitted (None)."""
        provider = RealisticPipelineProvider()
        pipeline = SummarizationPipeline(
            provider=provider,
            model="test-model",
            merge_group_size=5,
            max_validation_iterations=1,
        )

        result = await pipeline.run(
            program_id="CBACT04C",
            file_name="CBACT04C.cbl",
            chunk_templates=chunk_templates,
            source_lines=None,
        )

        assert isinstance(result, FileSummary)
        assert result.program_id == "CBACT04C"
        assert result.passes_completed == 3
        assert len(result.claims) >= 1

    @pytest.mark.integration
    async def test_segment_summaries_cover_all_bundles(
        self,
        chunk_templates: list[tuple[str, DocumentationTemplate]],
        realistic_provider: RealisticPipelineProvider,
    ) -> None:
        """The union of all segment bundle_ids covers every bundle produced in Pass 1."""
        pipeline = SummarizationPipeline(
            provider=realistic_provider,
            model="test-model",
            merge_group_size=5,
            max_validation_iterations=1,
        )

        result = await pipeline.run(
            program_id="CBACT04C",
            file_name="CBACT04C.cbl",
            chunk_templates=chunk_templates,
        )

        all_bundle_ids = {b.bundle_id for b in result.bundle_summaries}
        covered_by_segments: set[str] = set()
        for seg in result.segment_summaries:
            covered_by_segments.update(seg.bundle_ids)

        # Every bundle that exists in a segment must be from the actual bundle set.
        assert covered_by_segments.issubset(all_bundle_ids)
        # With group_size=5 and 10 bundles, both segments should cover 5 each.
        assert len(result.segment_summaries) == 2
        seg1_ids = set(result.segment_summaries[0].bundle_ids)
        seg2_ids = set(result.segment_summaries[1].bundle_ids)
        # The two segment sets must be disjoint.
        assert seg1_ids.isdisjoint(seg2_ids)

    @pytest.mark.integration
    async def test_claim_categories_are_valid_enum_values(
        self,
        chunk_templates: list[tuple[str, DocumentationTemplate]],
        realistic_provider: RealisticPipelineProvider,
    ) -> None:
        """All claims have valid ClaimCategory and populated claim_id fields."""
        pipeline = SummarizationPipeline(
            provider=realistic_provider,
            model="test-model",
            merge_group_size=5,
            max_validation_iterations=1,
        )

        result = await pipeline.run(
            program_id="CBACT04C",
            file_name="CBACT04C.cbl",
            chunk_templates=chunk_templates,
        )

        for claim in result.claims:
            assert claim.claim_id != "", "Claim must have a non-empty claim_id"
            assert isinstance(claim.category, ClaimCategory)
            assert claim.claim != "", "Claim text must not be empty"

    @pytest.mark.integration
    async def test_risk_areas_and_technical_debt_populated(
        self,
        chunk_templates: list[tuple[str, DocumentationTemplate]],
        realistic_provider: RealisticPipelineProvider,
    ) -> None:
        """Root summary includes risk_areas and technical_debt from the LLM response."""
        pipeline = SummarizationPipeline(
            provider=realistic_provider,
            model="test-model",
            merge_group_size=5,
            max_validation_iterations=1,
        )

        result = await pipeline.run(
            program_id="CBACT04C",
            file_name="CBACT04C.cbl",
            chunk_templates=chunk_templates,
        )

        assert len(result.risk_areas) >= 1
        assert len(result.technical_debt) >= 1
        assert len(result.migration_considerations) >= 1

    @pytest.mark.integration
    async def test_bundle_summaries_have_data_items(
        self,
        chunk_templates: list[tuple[str, DocumentationTemplate]],
        realistic_provider: RealisticPipelineProvider,
    ) -> None:
        """Each bundle summary from Pass 1 contains at least one data item."""
        pipeline = SummarizationPipeline(
            provider=realistic_provider,
            model="test-model",
            merge_group_size=5,
            max_validation_iterations=1,
        )

        result = await pipeline.run(
            program_id="CBACT04C",
            file_name="CBACT04C.cbl",
            chunk_templates=chunk_templates,
        )

        for bundle in result.bundle_summaries:
            total_data_items = (
                len(bundle.data_items_read)
                + len(bundle.data_items_written)
                + len(bundle.data_items_transformed)
            )
            assert total_data_items >= 1, (
                f"Bundle {bundle.bundle_id} has no data items"
            )

    @pytest.mark.integration
    async def test_segment_data_flows_and_perform_graph(
        self,
        chunk_templates: list[tuple[str, DocumentationTemplate]],
        realistic_provider: RealisticPipelineProvider,
    ) -> None:
        """Segment summaries contain data_flows and perform_graph entries."""
        pipeline = SummarizationPipeline(
            provider=realistic_provider,
            model="test-model",
            merge_group_size=5,
            max_validation_iterations=1,
        )

        result = await pipeline.run(
            program_id="CBACT04C",
            file_name="CBACT04C.cbl",
            chunk_templates=chunk_templates,
        )

        total_data_flows = sum(len(s.data_flows) for s in result.segment_summaries)
        total_perform_links = sum(
            len(s.perform_graph) for s in result.segment_summaries
        )
        assert total_data_flows >= 1, "At least one data flow expected across segments"
        assert total_perform_links >= 1, (
            "At least one perform link expected across segments"
        )

    @pytest.mark.integration
    async def test_save_load_computed_fields_survive_json_round_trip(
        self,
        tmp_path: Path,
    ) -> None:
        """Computed fields are correctly re-derived after JSON deserialization.

        Builds a FileSummary manually so the test is deterministic regardless
        of any provider response variation.
        """
        bundles = [
            BundleSummary(
                bundle_id=f"CHUNK-{i:03d}",
                paragraph_names=[f"PARA-{i}-A", f"PARA-{i}-B"],
                functional_summary=f"Bundle {i} functional summary",
                data_items_read=["WS-ACCT-ID"],
                perform_calls=["9000-ERROR-HANDLER"],
            )
            for i in range(10)
        ]
        segments = [
            SegmentSummary(
                segment_id="SEG-001",
                bundle_ids=[f"CHUNK-{i:03d}" for i in range(5)],
                functional_area="Account Setup",
                summary="Setup and validation segment",
            ),
            SegmentSummary(
                segment_id="SEG-002",
                bundle_ids=[f"CHUNK-{i:03d}" for i in range(5, 10)],
                functional_area="Transaction Processing",
                summary="Processing and reporting segment",
            ),
        ]
        claims = [
            VerifiableClaim(
                claim_id="C-001",
                claim="PERFORM 0100-INIT happens before main loop",
                category=ClaimCategory.PERFORM_RELATIONSHIP,
                evidence_bundle_ids=["CHUNK-000"],
                verdict=ClaimVerdict.CONFIRMED,
                verification_notes="Confirmed by bundle evidence.",
            ),
            VerifiableClaim(
                claim_id="C-002",
                claim="WS-ACCT-ID flows from read to process",
                category=ClaimCategory.DATA_FLOW,
                evidence_bundle_ids=["CHUNK-000", "CHUNK-001"],
                verdict=ClaimVerdict.CONFIRMED,
                verification_notes="Data flow confirmed.",
            ),
            VerifiableClaim(
                claim_id="C-003",
                claim="Hard-coded interest rate is a risk",
                category=ClaimCategory.BUSINESS_RULE,
                evidence_bundle_ids=["CHUNK-004"],
                verdict=ClaimVerdict.REFUTED,
                verification_notes="Rate is parameterised via PARM.",
            ),
            VerifiableClaim(
                claim_id="C-004",
                claim="Report generation is unreachable on error path",
                category=ClaimCategory.DEAD_CODE,
                evidence_bundle_ids=["CHUNK-008", "CHUNK-009"],
                verdict=ClaimVerdict.UNVERIFIABLE,
                verification_notes="Insufficient evidence in bundles.",
            ),
        ]

        summary = FileSummary(
            program_id="CBACT04C",
            file_name="CBACT04C.cbl",
            business_function="Batch account transaction processor",
            primary_data_flows=["Input -> validation -> output"],
            call_graph_summary=["0000-MAIN -> 0100-INIT"],
            risk_areas=["No I/O error handling"],
            technical_debt=["Hard-coded paths"],
            migration_considerations=["Sequential file access"],
            claims=claims,
            bundle_summaries=bundles,
            segment_summaries=segments,
            passes_completed=3,
            validation_iterations=1,
        )

        # Verify computed fields before serialization.
        assert summary.total_paragraphs == 20  # 10 bundles x 2 paragraphs
        assert summary.claims_verified == 2     # C-001 and C-002
        assert summary.claims_refuted == 1      # C-003

        output_path = tmp_path / "CBACT04C.summary.json"
        SummarizationPipeline.save_summary(summary, output_path)
        loaded = SummarizationPipeline.load_summary(output_path)

        # Computed fields survive the round-trip.
        assert loaded.total_paragraphs == 20
        assert loaded.claims_verified == 2
        assert loaded.claims_refuted == 1

        # Enum values round-trip correctly.
        assert loaded.claims[0].verdict == ClaimVerdict.CONFIRMED
        assert loaded.claims[2].verdict == ClaimVerdict.REFUTED
        assert loaded.claims[3].verdict == ClaimVerdict.UNVERIFIABLE
        assert loaded.claims[0].category == ClaimCategory.PERFORM_RELATIONSHIP
        assert loaded.claims[1].category == ClaimCategory.DATA_FLOW
        assert loaded.claims[2].category == ClaimCategory.BUSINESS_RULE
        assert loaded.claims[3].category == ClaimCategory.DEAD_CODE

    @pytest.mark.integration
    async def test_grouping_with_ten_chunks_produces_two_segments(
        self,
        chunk_templates: list[tuple[str, DocumentationTemplate]],
    ) -> None:
        """With group_size=5 and 10 chunks, exactly 2 segments are produced."""
        provider = RealisticPipelineProvider()
        pipeline = SummarizationPipeline(
            provider=provider,
            model="test-model",
            merge_group_size=5,
            max_validation_iterations=1,
        )

        result = await pipeline.run(
            program_id="CBACT04C",
            file_name="CBACT04C.cbl",
            chunk_templates=chunk_templates,
        )

        assert len(result.segment_summaries) == 2
        # Each segment must have a non-empty functional_area.
        for seg in result.segment_summaries:
            assert seg.functional_area != ""
            assert seg.summary != ""

    @pytest.mark.integration
    async def test_pipeline_pass1_model_override_is_accepted(
        self,
        chunk_templates: list[tuple[str, DocumentationTemplate]],
    ) -> None:
        """Pipeline accepts a separate pass1_model without raising."""
        call_log: list[tuple[str, str]] = []  # (pass, model)

        class LoggingProvider:
            @property
            def default_model(self) -> str:
                return "primary-model"

            async def complete(
                self,
                messages: list[Message],
                model: str | None = None,
                temperature: float = 1.0,
                **kwargs: Any,
            ) -> CompletionResponse:
                user_content = messages[-1].content if messages else ""
                if "Bundle ID:" in user_content:
                    call_log.append(("pass1", model or "primary-model"))
                    # Return one of the realistic bundle responses.
                    idx = sum(1 for p, _ in call_log if p == "pass1") - 1
                    cid = f"CHUNK-{idx:03d}"
                    area = _FUNCTIONAL_AREAS[idx % len(_FUNCTIONAL_AREAS)]
                    para_names = [f"{area.upper()}_A", f"{area.upper()}_B"]
                    content = _bundle_json(cid, para_names, idx)
                else:
                    call_log.append(("other", model or "primary-model"))
                    # Cycle through segment / root / verify responses.
                    user_lower = user_content.lower()
                    if "merge the following" in user_lower:
                        seg_num = sum(1 for p, _ in call_log if p == "other")
                        content = _SEG_001_JSON if seg_num <= 1 else _SEG_002_JSON
                    elif "synthesize the following" in user_lower:
                        content = _ROOT_SUMMARY_JSON
                    elif "verify the following" in user_lower:
                        content = _VERIFICATION_ALL_CONFIRMED_JSON
                    else:
                        content = "{}"
                return CompletionResponse(
                    content=content,
                    model=model or "primary-model",
                    tokens_used=50,
                )

        provider = LoggingProvider()
        pipeline = SummarizationPipeline(
            provider=provider,
            model="primary-model",
            pass1_model="haiku-model",
            merge_group_size=5,
            max_validation_iterations=1,
        )

        result = await pipeline.run(
            program_id="CBACT04C",
            file_name="CBACT04C.cbl",
            chunk_templates=chunk_templates,
        )

        assert isinstance(result, FileSummary)
        assert result.passes_completed == 3

        # All Pass 1 calls must have used the haiku-model.
        pass1_models = [m for p, m in call_log if p == "pass1"]
        assert len(pass1_models) == 10
        assert all(m == "haiku-model" for m in pass1_models)

        # Passes 2, 3, and validation must have used the primary-model.
        other_models = [m for p, m in call_log if p == "other"]
        assert len(other_models) >= 3  # at least 2 merge + 1 root + 1 verify
        assert all(m == "primary-model" for m in other_models)
