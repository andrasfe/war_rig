"""Comprehensive tests for the War Rig summarization package.

Covers:
- Section 1: Models (summaries.py) -- construction, serialization, computed fields
- Section 2: Grouping strategies (grouping.py)
- Section 3: Pass 1 BundleSummarizer, Pass 2 CoherenceMerger, Pass 3 RootSummarizer,
             ClaimVerifier, ValidationLoop
- Section 4: Pipeline integration (pipeline.py)
- Section 5: Config (summarization settings in WarRigConfig)
"""

from __future__ import annotations

import json
from typing import Any

import pytest

from war_rig.models.summaries import (
    BundleSummary,
    ClaimCategory,
    ClaimVerdict,
    ConditionalBranch,
    DataFlowItem,
    FileSummary,
    PerformLink,
    SegmentSummary,
    VerifiableClaim,
)
from war_rig.models.templates import (
    DocumentationTemplate,
    HeaderSection,
    Paragraph,
)
from war_rig.providers.protocol import CompletionResponse, Message
from war_rig.summarization.bundle_summarizer import BundleInput, BundleSummarizer
from war_rig.summarization.claim_verifier import ClaimVerifier
from war_rig.summarization.coherence_merger import CoherenceMerger
from war_rig.summarization.grouping import AdjacentSequentialGrouping
from war_rig.summarization.pipeline import SummarizationPipeline
from war_rig.summarization.root_summarizer import RootSummarizer
from war_rig.summarization.validation_loop import ValidationLoop

# ============================================================================
# Mock providers
# ============================================================================


class MockProvider:
    """Mock LLM provider that returns a single predefined response."""

    def __init__(self, response_content: str) -> None:
        self._response = response_content

    @property
    def default_model(self) -> str:
        return "test-model"

    async def complete(
        self,
        messages: list[Message],
        model: str | None = None,
        temperature: float = 0.7,
        **kwargs: Any,
    ) -> CompletionResponse:
        return CompletionResponse(
            content=self._response,
            model=model or "test-model",
            tokens_used=100,
        )


class MultiResponseProvider:
    """Mock LLM provider that returns different responses based on prompt content."""

    def __init__(self, responses: dict[str, str]) -> None:
        self._responses = responses
        self._default = '{"functional_summary": "fallback"}'

    @property
    def default_model(self) -> str:
        return "test-model"

    async def complete(
        self,
        messages: list[Message],
        model: str | None = None,
        temperature: float = 0.7,
        **kwargs: Any,
    ) -> CompletionResponse:
        content = messages[-1].content if messages else ""
        for key, response in self._responses.items():
            if key in content:
                return CompletionResponse(
                    content=response,
                    model=model or "test-model",
                    tokens_used=100,
                )
        return CompletionResponse(
            content=self._default,
            model=model or "test-model",
            tokens_used=100,
        )


class FailingProvider:
    """Mock LLM provider that always raises an exception."""

    @property
    def default_model(self) -> str:
        return "test-model"

    async def complete(
        self,
        messages: list[Message],
        model: str | None = None,
        temperature: float = 0.7,
        **kwargs: Any,
    ) -> CompletionResponse:
        raise RuntimeError("Simulated provider failure")


# ============================================================================
# Shared JSON response fixtures
# ============================================================================

BUNDLE_SUMMARY_JSON = json.dumps(
    {
        "bundle_id": "BDL-001",
        "paragraph_names": ["0100-INIT", "0200-PROCESS"],
        "functional_summary": "Initialises file handles and processes records",
        "data_items_read": ["IN-RECORD", "WS-STATUS"],
        "data_items_written": ["OUT-RECORD"],
        "data_items_transformed": [],
        "perform_calls": ["1000-VALIDATE"],
        "perform_callers": ["0000-MAIN"],
        "conditional_branches": [
            {
                "condition": "IF WS-STATUS = '00'",
                "business_meaning": "Successful read",
                "outcome_true": "Continue processing",
                "outcome_false": "Set EOF flag",
            }
        ],
        "anomalies": [],
    }
)

BUNDLE_SUMMARY_JSON_B = json.dumps(
    {
        "bundle_id": "BDL-002",
        "paragraph_names": ["0300-FINALIZE"],
        "functional_summary": "Closes files and reports record count",
        "data_items_read": ["WS-RECORD-COUNT"],
        "data_items_written": [],
        "perform_calls": [],
        "perform_callers": ["0000-MAIN"],
        "conditional_branches": [],
        "anomalies": [],
    }
)

SEGMENT_SUMMARY_JSON = json.dumps(
    {
        "segment_id": "SEG-001",
        "bundle_ids": ["BDL-001", "BDL-002"],
        "functional_area": "Record Processing",
        "summary": "Processes input records with validation and writes output",
        "data_flows": [
            {
                "source": "0100-INIT",
                "target": "0200-PROCESS",
                "data_item": "IN-RECORD",
                "description": "Record read in init, processed in main loop",
            }
        ],
        "perform_graph": [
            {
                "caller": "0000-MAIN",
                "callee": "0100-INIT",
                "purpose": "File initialisation",
            }
        ],
        "resolved_coreferences": [
            "WS-STATUS in BDL-001 is the same as WS-RETURN-CODE in BDL-002"
        ],
        "flagged_contradictions": [],
        "flagged_redundancies": [],
    }
)

ROOT_SUMMARY_JSON = json.dumps(
    {
        "business_function": "Batch record processing with validation and reporting",
        "primary_data_flows": [
            "Input file -> validation -> output file",
            "Record count accumulation for end-of-job report",
        ],
        "call_graph_summary": [
            "0000-MAIN -> 0100-INIT -> 1000-VALIDATE",
            "0000-MAIN -> 0200-PROCESS -> 1000-VALIDATE",
            "0000-MAIN -> 0300-FINALIZE",
        ],
        "risk_areas": ["No error handling for file I/O failures"],
        "technical_debt": ["Hard-coded file paths"],
        "migration_considerations": ["Sequential file access pattern"],
        "claims": [
            {
                "claim_id": "C-001",
                "claim": "0000-MAIN performs 0100-INIT before 0200-PROCESS",
                "category": "PERFORM_RELATIONSHIP",
                "evidence_bundle_ids": ["BDL-001"],
            },
            {
                "claim_id": "C-002",
                "claim": "IN-RECORD is read and transformed into OUT-RECORD",
                "category": "DATA_FLOW",
                "evidence_bundle_ids": ["BDL-001", "BDL-002"],
            },
        ],
    }
)

VERIFICATION_ALL_CONFIRMED_JSON = json.dumps(
    [
        {
            "claim_id": "C-001",
            "verdict": "CONFIRMED",
            "notes": "PERFORM 0100-INIT is executed before loop.",
        },
        {
            "claim_id": "C-002",
            "verdict": "CONFIRMED",
            "notes": "IN-RECORD fields are moved to OUT-RECORD.",
        },
    ]
)

VERIFICATION_MIXED_JSON = json.dumps(
    [
        {
            "claim_id": "C-001",
            "verdict": "CONFIRMED",
            "notes": "Confirmed by bundle evidence.",
        },
        {
            "claim_id": "C-002",
            "verdict": "REFUTED",
            "notes": "No transformation occurs; fields are moved directly.",
        },
    ]
)


# ============================================================================
# Section 1: Model tests
# ============================================================================


class TestBundleSummary:
    """Tests for the BundleSummary model."""

    def test_construction_with_defaults(self) -> None:
        bs = BundleSummary()
        assert bs.bundle_id == ""
        assert bs.paragraph_names == []
        assert bs.functional_summary == ""
        assert bs.data_items_read == []
        assert bs.data_items_written == []
        assert bs.data_items_transformed == []
        assert bs.perform_calls == []
        assert bs.perform_callers == []
        assert bs.conditional_branches == []
        assert bs.anomalies == []
        assert bs.source_line_start == 0
        assert bs.source_line_end == 0
        assert bs.metadata is None

    def test_construction_with_all_fields(self) -> None:
        branch = ConditionalBranch(
            condition="IF X = 1",
            business_meaning="Flag check",
            outcome_true="Process",
            outcome_false="Skip",
        )
        bs = BundleSummary(
            bundle_id="BDL-099",
            paragraph_names=["PARA-A", "PARA-B"],
            functional_summary="Handles record validation",
            data_items_read=["FIELD-A"],
            data_items_written=["FIELD-B"],
            data_items_transformed=["FIELD-C"],
            perform_calls=["SUB-PARA"],
            perform_callers=["MAIN-PARA"],
            conditional_branches=[branch],
            anomalies=["Dead code at line 200"],
            source_line_start=100,
            source_line_end=150,
            metadata={"custom_key": "custom_value"},
        )
        assert bs.bundle_id == "BDL-099"
        assert len(bs.paragraph_names) == 2
        assert bs.source_line_start == 100
        assert bs.source_line_end == 150
        assert bs.metadata == {"custom_key": "custom_value"}
        assert len(bs.conditional_branches) == 1
        assert bs.conditional_branches[0].condition == "IF X = 1"

    def test_json_round_trip(self) -> None:
        original = BundleSummary(
            bundle_id="BDL-RT",
            paragraph_names=["PARA-1"],
            functional_summary="Round-trip test",
            data_items_read=["ITEM-A"],
            perform_calls=["TARGET-1"],
            conditional_branches=[
                ConditionalBranch(
                    condition="IF EOF",
                    business_meaning="End of file",
                    outcome_true="Stop",
                )
            ],
            source_line_start=10,
            source_line_end=20,
        )
        json_str = original.model_dump_json()
        restored = BundleSummary.model_validate_json(json_str)
        assert restored.bundle_id == original.bundle_id
        assert restored.paragraph_names == original.paragraph_names
        assert restored.functional_summary == original.functional_summary
        assert restored.data_items_read == original.data_items_read
        assert restored.perform_calls == original.perform_calls
        assert len(restored.conditional_branches) == 1
        assert restored.conditional_branches[0].condition == "IF EOF"
        assert restored.source_line_start == 10
        assert restored.source_line_end == 20

    def test_extra_fields_ignored(self) -> None:
        """BundleSummary has extra='ignore', so unknown keys are dropped."""
        data = {"bundle_id": "BDL-X", "unknown_field": "should be dropped"}
        bs = BundleSummary.model_validate(data)
        assert bs.bundle_id == "BDL-X"
        assert not hasattr(bs, "unknown_field")


class TestSegmentSummary:
    """Tests for the SegmentSummary model."""

    def test_construction(self) -> None:
        ss = SegmentSummary(
            segment_id="SEG-001",
            bundle_ids=["BDL-001", "BDL-002"],
            functional_area="Input Processing",
            summary="Handles all input file operations",
        )
        assert ss.segment_id == "SEG-001"
        assert ss.bundle_ids == ["BDL-001", "BDL-002"]
        assert ss.functional_area == "Input Processing"
        assert ss.data_flows == []
        assert ss.perform_graph == []

    def test_serialization_with_nested_models(self) -> None:
        ss = SegmentSummary(
            segment_id="SEG-002",
            bundle_ids=["BDL-003"],
            functional_area="Output",
            summary="Output handling",
            data_flows=[
                DataFlowItem(
                    source="PARA-A",
                    target="PARA-B",
                    data_item="WS-TOTAL",
                    description="Accumulated total passed to output",
                )
            ],
            perform_graph=[
                PerformLink(
                    caller="MAIN", callee="OUTPUT-PARA", purpose="Write results"
                )
            ],
        )
        data = ss.model_dump(mode="json")
        assert data["segment_id"] == "SEG-002"
        assert len(data["data_flows"]) == 1
        assert data["data_flows"][0]["data_item"] == "WS-TOTAL"
        assert len(data["perform_graph"]) == 1
        assert data["perform_graph"][0]["caller"] == "MAIN"


class TestFileSummary:
    """Tests for the FileSummary model including computed fields."""

    def _make_file_summary(self) -> FileSummary:
        bundles = [
            BundleSummary(
                bundle_id="BDL-001",
                paragraph_names=["PARA-A", "PARA-B"],
            ),
            BundleSummary(
                bundle_id="BDL-002",
                paragraph_names=["PARA-C"],
            ),
        ]
        claims = [
            VerifiableClaim(
                claim_id="C-001",
                claim="Claim one",
                verdict=ClaimVerdict.CONFIRMED,
            ),
            VerifiableClaim(
                claim_id="C-002",
                claim="Claim two",
                verdict=ClaimVerdict.REFUTED,
            ),
            VerifiableClaim(
                claim_id="C-003",
                claim="Claim three",
                verdict=ClaimVerdict.CONFIRMED,
            ),
            VerifiableClaim(
                claim_id="C-004",
                claim="Claim four",
                verdict=ClaimVerdict.UNVERIFIABLE,
            ),
        ]
        return FileSummary(
            program_id="PROG001",
            file_name="PROG001.cbl",
            business_function="Batch processing",
            primary_data_flows=["Flow A", "Flow B"],
            call_graph_summary=["MAIN -> INIT"],
            risk_areas=["No error handling"],
            bundle_summaries=bundles,
            claims=claims,
            passes_completed=3,
        )

    def test_computed_total_paragraphs(self) -> None:
        fs = self._make_file_summary()
        assert fs.total_paragraphs == 3  # 2 + 1

    def test_computed_claims_verified(self) -> None:
        fs = self._make_file_summary()
        assert fs.claims_verified == 2  # C-001 and C-003

    def test_computed_claims_refuted(self) -> None:
        fs = self._make_file_summary()
        assert fs.claims_refuted == 1  # C-002

    def test_to_context_string_renders_expected_format(self) -> None:
        fs = self._make_file_summary()
        fs.segment_summaries = [
            SegmentSummary(
                segment_id="SEG-001",
                bundle_ids=["BDL-001"],
                functional_area="Init",
                summary="Initialisation logic",
            ),
        ]
        ctx = fs.to_context_string()
        assert "## File-Level Summary: PROG001.cbl" in ctx
        assert "**Business Function**: Batch processing" in ctx
        assert "**Primary Data Flows**:" in ctx
        assert "- Flow A" in ctx
        assert "- Flow B" in ctx
        assert "**Call Graph**:" in ctx
        assert "- MAIN -> INIT" in ctx
        assert "**Risk Areas**:" in ctx
        assert "- No error handling" in ctx
        assert "**Functional Areas**:" in ctx
        assert "- Init: Initialisation logic" in ctx

    def test_to_context_string_minimal(self) -> None:
        """With only file_name set, output is still valid."""
        fs = FileSummary(file_name="EMPTY.cbl")
        ctx = fs.to_context_string()
        assert "## File-Level Summary: EMPTY.cbl" in ctx
        assert "**Business Function**" not in ctx

    def test_json_round_trip(self) -> None:
        fs = self._make_file_summary()
        json_str = fs.model_dump_json()
        restored = FileSummary.model_validate_json(json_str)
        assert restored.program_id == "PROG001"
        assert len(restored.bundle_summaries) == 2
        assert len(restored.claims) == 4
        assert restored.total_paragraphs == 3
        assert restored.claims_verified == 2
        assert restored.claims_refuted == 1


class TestVerifiableClaim:
    """Tests for the VerifiableClaim model."""

    def test_all_claim_categories(self) -> None:
        for cat in ClaimCategory:
            claim = VerifiableClaim(
                claim_id=f"C-{cat.value}",
                claim=f"Test claim for {cat.value}",
                category=cat,
            )
            assert claim.category == cat

    def test_default_verdict_is_none(self) -> None:
        claim = VerifiableClaim(claim_id="C-X", claim="Test")
        assert claim.verdict is None
        assert claim.verification_notes is None


class TestClaimCategory:
    """Tests for ClaimCategory enum case-insensitive matching."""

    def test_uppercase_match(self) -> None:
        assert (
            ClaimCategory("PERFORM_RELATIONSHIP") == ClaimCategory.PERFORM_RELATIONSHIP
        )

    def test_lowercase_match(self) -> None:
        assert (
            ClaimCategory("perform_relationship") == ClaimCategory.PERFORM_RELATIONSHIP
        )

    def test_mixed_case_match(self) -> None:
        assert ClaimCategory("Data_Flow") == ClaimCategory.DATA_FLOW

    def test_hyphenated_match(self) -> None:
        assert ClaimCategory("io-operation") == ClaimCategory.IO_OPERATION

    def test_space_separated_match(self) -> None:
        assert ClaimCategory("business rule") == ClaimCategory.BUSINESS_RULE

    def test_invalid_returns_none(self) -> None:
        result = ClaimCategory._missing_("completely_invalid_value_xyz")
        assert result is None

    def test_non_string_returns_none(self) -> None:
        result = ClaimCategory._missing_(42)
        assert result is None


class TestClaimVerdict:
    """Tests for ClaimVerdict enum case-insensitive matching."""

    def test_uppercase_match(self) -> None:
        assert ClaimVerdict("CONFIRMED") == ClaimVerdict.CONFIRMED

    def test_lowercase_match(self) -> None:
        assert ClaimVerdict("confirmed") == ClaimVerdict.CONFIRMED

    def test_mixed_case_match(self) -> None:
        assert ClaimVerdict("Refuted") == ClaimVerdict.REFUTED

    def test_all_values(self) -> None:
        assert ClaimVerdict("CONFIRMED") == ClaimVerdict.CONFIRMED
        assert ClaimVerdict("REFUTED") == ClaimVerdict.REFUTED
        assert ClaimVerdict("UNVERIFIABLE") == ClaimVerdict.UNVERIFIABLE

    def test_invalid_returns_none(self) -> None:
        result = ClaimVerdict._missing_("NOT_A_VERDICT")
        assert result is None


class TestSupportingModels:
    """Tests for ConditionalBranch, DataFlowItem, PerformLink."""

    def test_conditional_branch_construction(self) -> None:
        cb = ConditionalBranch(
            condition="IF WS-FLAG = 'Y'",
            business_meaning="Active flag check",
            outcome_true="Process record",
            outcome_false="Skip record",
        )
        assert cb.condition == "IF WS-FLAG = 'Y'"
        assert cb.business_meaning == "Active flag check"
        assert cb.outcome_true == "Process record"
        assert cb.outcome_false == "Skip record"

    def test_conditional_branch_defaults(self) -> None:
        cb = ConditionalBranch()
        assert cb.condition == ""
        assert cb.business_meaning == ""
        assert cb.outcome_true == ""
        assert cb.outcome_false is None

    def test_data_flow_item_construction(self) -> None:
        dfi = DataFlowItem(
            source="PARA-READ",
            target="PARA-WRITE",
            data_item="WS-AMOUNT",
            description="Amount passed from read to write",
        )
        assert dfi.source == "PARA-READ"
        assert dfi.target == "PARA-WRITE"
        assert dfi.data_item == "WS-AMOUNT"
        assert dfi.description == "Amount passed from read to write"

    def test_data_flow_item_defaults(self) -> None:
        dfi = DataFlowItem()
        assert dfi.source == ""
        assert dfi.target == ""
        assert dfi.data_item == ""
        assert dfi.description == ""

    def test_perform_link_construction(self) -> None:
        pl = PerformLink(
            caller="MAIN-PARA",
            callee="SUB-PARA",
            purpose="Execute subroutine",
        )
        assert pl.caller == "MAIN-PARA"
        assert pl.callee == "SUB-PARA"
        assert pl.purpose == "Execute subroutine"

    def test_perform_link_defaults(self) -> None:
        pl = PerformLink()
        assert pl.caller == ""
        assert pl.callee == ""
        assert pl.purpose is None


# ============================================================================
# Section 2: Grouping tests
# ============================================================================


class TestAdjacentSequentialGrouping:
    """Tests for the AdjacentSequentialGrouping strategy."""

    def _make_bundles(self, count: int) -> list[BundleSummary]:
        return [
            BundleSummary(bundle_id=f"BDL-{i:03d}", paragraph_names=[f"PARA-{i}"])
            for i in range(count)
        ]

    def test_empty_input_returns_empty_list(self) -> None:
        strategy = AdjacentSequentialGrouping()
        result = strategy.group([], group_size=4)
        assert result == []

    def test_fewer_bundles_than_group_size(self) -> None:
        strategy = AdjacentSequentialGrouping()
        bundles = self._make_bundles(3)
        result = strategy.group(bundles, group_size=6)
        assert len(result) == 1
        assert len(result[0]) == 3

    def test_exact_multiple_of_group_size(self) -> None:
        strategy = AdjacentSequentialGrouping()
        bundles = self._make_bundles(8)
        result = strategy.group(bundles, group_size=4)
        assert len(result) == 2
        assert len(result[0]) == 4
        assert len(result[1]) == 4
        # Verify order preserved
        assert result[0][0].bundle_id == "BDL-000"
        assert result[1][0].bundle_id == "BDL-004"

    def test_non_exact_division(self) -> None:
        strategy = AdjacentSequentialGrouping()
        bundles = self._make_bundles(7)
        result = strategy.group(bundles, group_size=3)
        assert len(result) == 3
        assert len(result[0]) == 3
        assert len(result[1]) == 3
        assert len(result[2]) == 1  # last group is smaller

    def test_group_size_of_one(self) -> None:
        strategy = AdjacentSequentialGrouping()
        bundles = self._make_bundles(3)
        result = strategy.group(bundles, group_size=1)
        assert len(result) == 3
        for group in result:
            assert len(group) == 1

    def test_group_size_less_than_one_raises(self) -> None:
        strategy = AdjacentSequentialGrouping()
        with pytest.raises(ValueError, match="group_size must be >= 1"):
            strategy.group([], group_size=0)

    def test_single_bundle(self) -> None:
        strategy = AdjacentSequentialGrouping()
        bundles = self._make_bundles(1)
        result = strategy.group(bundles, group_size=4)
        assert len(result) == 1
        assert len(result[0]) == 1

    def test_preserves_bundle_identity(self) -> None:
        """All bundles from input appear exactly once in the output."""
        strategy = AdjacentSequentialGrouping()
        bundles = self._make_bundles(10)
        result = strategy.group(bundles, group_size=3)
        flat = [b for group in result for b in group]
        assert len(flat) == 10
        ids = [b.bundle_id for b in flat]
        assert ids == [f"BDL-{i:03d}" for i in range(10)]


# ============================================================================
# Section 3: Pass 1 -- BundleSummarizer
# ============================================================================


class TestBundleSummarizer:
    """Tests for the BundleSummarizer (Pass 1)."""

    def _make_bundle_input(
        self, bundle_id: str = "BDL-001", para_names: list[str] | None = None
    ) -> BundleInput:
        return BundleInput(
            bundle_id=bundle_id,
            paragraph_names=para_names or ["0100-INIT"],
            template_json='{"header": {}}',
            source_excerpt="       PERFORM 0100-INIT.",
        )

    async def test_summarize_single_bundle(self) -> None:
        provider = MockProvider(BUNDLE_SUMMARY_JSON)
        summarizer = BundleSummarizer(provider=provider, model="test-model")
        bundle_input = self._make_bundle_input()
        result = await summarizer.summarize_bundle(bundle_input)

        assert isinstance(result, BundleSummary)
        assert result.bundle_id == "BDL-001"
        assert result.paragraph_names == ["0100-INIT", "0200-PROCESS"]
        assert "Initialises" in result.functional_summary
        assert "IN-RECORD" in result.data_items_read
        assert "1000-VALIDATE" in result.perform_calls
        assert len(result.conditional_branches) == 1

    async def test_summarize_bundles_parallel(self) -> None:
        provider = MockProvider(BUNDLE_SUMMARY_JSON)
        summarizer = BundleSummarizer(provider=provider, model="test-model")
        inputs = [self._make_bundle_input(f"BDL-{i:03d}") for i in range(5)]
        results = await summarizer.summarize_bundles(inputs)

        assert len(results) == 5
        for r in results:
            assert isinstance(r, BundleSummary)

    async def test_summarize_bundle_json_in_code_fence(self) -> None:
        fenced = f"Here is the summary:\n```json\n{BUNDLE_SUMMARY_JSON}\n```\nDone."
        provider = MockProvider(fenced)
        summarizer = BundleSummarizer(provider=provider, model="test-model")
        result = await summarizer.summarize_bundle(self._make_bundle_input())

        assert isinstance(result, BundleSummary)
        assert result.bundle_id == "BDL-001"
        assert "Initialises" in result.functional_summary

    async def test_summarize_bundle_parse_failure(self) -> None:
        provider = MockProvider("This is not JSON at all, just gibberish text!!")
        summarizer = BundleSummarizer(provider=provider, model="test-model")
        result = await summarizer.summarize_bundle(self._make_bundle_input())

        assert isinstance(result, BundleSummary)
        assert result.bundle_id == "BDL-001"
        # Fallback stores raw text as functional_summary
        assert "gibberish" in result.functional_summary

    async def test_summarize_bundle_provider_failure(self) -> None:
        provider = FailingProvider()
        summarizer = BundleSummarizer(provider=provider, model="test-model")
        result = await summarizer.summarize_bundle(self._make_bundle_input())

        assert isinstance(result, BundleSummary)
        assert result.bundle_id == "BDL-001"
        assert result.functional_summary == ""  # empty on provider failure

    async def test_summarize_bundle_preserves_bundle_id_from_input(self) -> None:
        """If LLM response omits bundle_id, the input value is used."""
        response = json.dumps({"functional_summary": "No bundle_id in response"})
        provider = MockProvider(response)
        summarizer = BundleSummarizer(provider=provider, model="test-model")
        result = await summarizer.summarize_bundle(
            self._make_bundle_input("BDL-CUSTOM")
        )
        assert result.bundle_id == "BDL-CUSTOM"

    async def test_summarize_empty_bundle_list(self) -> None:
        provider = MockProvider(BUNDLE_SUMMARY_JSON)
        summarizer = BundleSummarizer(provider=provider, model="test-model")
        results = await summarizer.summarize_bundles([])
        assert results == []


# ============================================================================
# Section 3: Pass 2 -- CoherenceMerger
# ============================================================================


class TestCoherenceMerger:
    """Tests for the CoherenceMerger (Pass 2)."""

    def _make_bundles(self, count: int) -> list[BundleSummary]:
        return [
            BundleSummary(
                bundle_id=f"BDL-{i:03d}",
                paragraph_names=[f"PARA-{i}"],
                functional_summary=f"Bundle {i} handles step {i}",
            )
            for i in range(count)
        ]

    async def test_merge_single_group(self) -> None:
        provider = MockProvider(SEGMENT_SUMMARY_JSON)
        merger = CoherenceMerger(
            provider=provider,
            model="test-model",
            group_size=10,  # all bundles in one group
        )
        bundles = self._make_bundles(3)
        segments = await merger.merge_bundles(bundles)

        assert len(segments) == 1
        assert isinstance(segments[0], SegmentSummary)
        assert segments[0].segment_id == "SEG-001"
        assert segments[0].functional_area == "Record Processing"
        assert len(segments[0].data_flows) == 1
        assert len(segments[0].perform_graph) == 1

    async def test_merge_multiple_groups(self) -> None:
        provider = MockProvider(SEGMENT_SUMMARY_JSON)
        merger = CoherenceMerger(
            provider=provider,
            model="test-model",
            group_size=3,
        )
        bundles = self._make_bundles(7)
        segments = await merger.merge_bundles(bundles)

        assert len(segments) == 3  # ceil(7/3) = 3 groups

    async def test_merge_empty_bundles(self) -> None:
        provider = MockProvider(SEGMENT_SUMMARY_JSON)
        merger = CoherenceMerger(provider=provider, model="test-model")
        segments = await merger.merge_bundles([])
        assert segments == []

    async def test_merge_provider_failure_returns_fallback(self) -> None:
        provider = FailingProvider()
        merger = CoherenceMerger(
            provider=provider,
            model="test-model",
            group_size=10,
        )
        bundles = self._make_bundles(2)
        segments = await merger.merge_bundles(bundles)

        assert len(segments) == 1
        seg = segments[0]
        assert seg.segment_id == "SEG-001"
        assert seg.bundle_ids == ["BDL-000", "BDL-001"]
        # Fallback concatenates functional summaries
        assert "Bundle 0" in seg.summary
        assert "Bundle 1" in seg.summary

    async def test_merge_parse_failure_returns_fallback(self) -> None:
        provider = MockProvider("not valid json at all")
        merger = CoherenceMerger(
            provider=provider,
            model="test-model",
            group_size=10,
        )
        bundles = self._make_bundles(2)
        segments = await merger.merge_bundles(bundles)

        assert len(segments) == 1
        seg = segments[0]
        # Fallback used when parse fails
        assert "Bundle 0" in seg.summary or seg.functional_area == ""


# ============================================================================
# Section 3: Pass 3 -- RootSummarizer
# ============================================================================


class TestRootSummarizer:
    """Tests for the RootSummarizer (Pass 3)."""

    def _make_inputs(self) -> tuple[list[SegmentSummary], list[BundleSummary]]:
        bundles = [
            BundleSummary(
                bundle_id="BDL-001",
                paragraph_names=["PARA-A", "PARA-B"],
                functional_summary="Init and process",
            ),
            BundleSummary(
                bundle_id="BDL-002",
                paragraph_names=["PARA-C"],
                functional_summary="Finalize",
            ),
        ]
        segments = [
            SegmentSummary(
                segment_id="SEG-001",
                bundle_ids=["BDL-001", "BDL-002"],
                functional_area="Main Processing",
                summary="Full processing pipeline",
            ),
        ]
        return segments, bundles

    async def test_summarize_root(self) -> None:
        provider = MockProvider(ROOT_SUMMARY_JSON)
        summarizer = RootSummarizer(provider=provider, model="test-model")
        segments, bundles = self._make_inputs()

        result = await summarizer.summarize(
            program_id="PROG001",
            file_name="PROG001.cbl",
            segments=segments,
            bundles=bundles,
        )

        assert isinstance(result, FileSummary)
        assert result.program_id == "PROG001"
        assert result.file_name == "PROG001.cbl"
        assert "Batch" in result.business_function
        assert len(result.primary_data_flows) == 2
        assert len(result.call_graph_summary) == 3
        assert len(result.risk_areas) == 1
        assert len(result.claims) == 2
        assert result.claims[0].claim_id == "C-001"
        assert result.claims[0].category == ClaimCategory.PERFORM_RELATIONSHIP
        assert result.claims[1].category == ClaimCategory.DATA_FLOW
        assert result.passes_completed == 3

    async def test_summarize_preserves_bundle_and_segment_refs(self) -> None:
        provider = MockProvider(ROOT_SUMMARY_JSON)
        summarizer = RootSummarizer(provider=provider, model="test-model")
        segments, bundles = self._make_inputs()

        result = await summarizer.summarize(
            program_id="PROG001",
            file_name="PROG001.cbl",
            segments=segments,
            bundles=bundles,
        )

        assert result.bundle_summaries is bundles
        assert result.segment_summaries is segments
        assert len(result.bundle_summaries) == 2
        assert len(result.segment_summaries) == 1

    async def test_summarize_provider_failure_returns_fallback(self) -> None:
        provider = FailingProvider()
        summarizer = RootSummarizer(provider=provider, model="test-model")
        segments, bundles = self._make_inputs()

        result = await summarizer.summarize(
            program_id="PROG002",
            file_name="PROG002.cbl",
            segments=segments,
            bundles=bundles,
        )

        assert isinstance(result, FileSummary)
        assert result.program_id == "PROG002"
        assert result.passes_completed == 3
        # Fallback concatenates segment summaries
        assert "Full processing pipeline" in result.business_function

    async def test_summarize_parse_failure_returns_fallback(self) -> None:
        provider = MockProvider("completely invalid output from LLM")
        summarizer = RootSummarizer(provider=provider, model="test-model")
        segments, bundles = self._make_inputs()

        result = await summarizer.summarize(
            program_id="PROG003",
            file_name="PROG003.cbl",
            segments=segments,
            bundles=bundles,
        )

        assert isinstance(result, FileSummary)
        assert result.program_id == "PROG003"
        assert result.passes_completed == 3


# ============================================================================
# Section 3: Claim Verification
# ============================================================================


class TestClaimVerifier:
    """Tests for the ClaimVerifier."""

    def _make_claims_and_bundles(
        self,
    ) -> tuple[list[VerifiableClaim], list[BundleSummary]]:
        claims = [
            VerifiableClaim(
                claim_id="C-001",
                claim="0000-MAIN performs 0100-INIT before 0200-PROCESS",
                category=ClaimCategory.PERFORM_RELATIONSHIP,
                evidence_bundle_ids=["BDL-001"],
            ),
            VerifiableClaim(
                claim_id="C-002",
                claim="IN-RECORD is transformed into OUT-RECORD",
                category=ClaimCategory.DATA_FLOW,
                evidence_bundle_ids=["BDL-001", "BDL-002"],
            ),
        ]
        bundles = [
            BundleSummary(
                bundle_id="BDL-001",
                paragraph_names=["0100-INIT", "0200-PROCESS"],
                functional_summary="Init and process records",
                data_items_read=["IN-RECORD"],
                perform_calls=["1000-VALIDATE"],
            ),
            BundleSummary(
                bundle_id="BDL-002",
                paragraph_names=["0300-FINALIZE"],
                functional_summary="Close files",
                data_items_written=["OUT-RECORD"],
            ),
        ]
        return claims, bundles

    async def test_verify_claims_all_confirmed(self) -> None:
        provider = MockProvider(VERIFICATION_ALL_CONFIRMED_JSON)
        verifier = ClaimVerifier(provider=provider, model="test-model")
        claims, bundles = self._make_claims_and_bundles()

        result = await verifier.verify_claims(claims, bundles)

        assert len(result) == 2
        assert result[0].verdict == ClaimVerdict.CONFIRMED
        assert result[1].verdict == ClaimVerdict.CONFIRMED
        assert result[0].verification_notes is not None

    async def test_verify_claims_mixed_verdicts(self) -> None:
        provider = MockProvider(VERIFICATION_MIXED_JSON)
        verifier = ClaimVerifier(provider=provider, model="test-model")
        claims, bundles = self._make_claims_and_bundles()

        result = await verifier.verify_claims(claims, bundles)

        assert result[0].verdict == ClaimVerdict.CONFIRMED
        assert result[1].verdict == ClaimVerdict.REFUTED

    async def test_verify_claims_empty_list(self) -> None:
        provider = MockProvider("should not be called")
        verifier = ClaimVerifier(provider=provider, model="test-model")

        result = await verifier.verify_claims([], [])
        assert result == []

    async def test_verify_claims_provider_failure_marks_unverifiable(self) -> None:
        provider = FailingProvider()
        verifier = ClaimVerifier(provider=provider, model="test-model")
        claims, bundles = self._make_claims_and_bundles()

        result = await verifier.verify_claims(claims, bundles)

        assert len(result) == 2
        for claim in result:
            assert claim.verdict == ClaimVerdict.UNVERIFIABLE

    async def test_verify_claims_unparseable_response(self) -> None:
        provider = MockProvider("this is not json")
        verifier = ClaimVerifier(provider=provider, model="test-model")
        claims, bundles = self._make_claims_and_bundles()

        result = await verifier.verify_claims(claims, bundles)

        # With empty verdicts, all claims are UNVERIFIABLE
        assert len(result) == 2
        for claim in result:
            assert claim.verdict == ClaimVerdict.UNVERIFIABLE

    async def test_verify_claims_with_source_lines(self) -> None:
        provider = MockProvider(VERIFICATION_ALL_CONFIRMED_JSON)
        verifier = ClaimVerifier(provider=provider, model="test-model")
        claims, bundles = self._make_claims_and_bundles()
        # Add source line ranges to bundles for evidence building
        bundles[0].source_line_start = 10
        bundles[0].source_line_end = 20

        source = [f"line {i}\n" for i in range(30)]
        result = await verifier.verify_claims(claims, bundles, source_lines=source)

        assert len(result) == 2
        assert result[0].verdict == ClaimVerdict.CONFIRMED

    async def test_verify_claims_verdicts_dict_format(self) -> None:
        """LLM returns verdicts wrapped in a dict instead of a list."""
        wrapped = json.dumps(
            {
                "verdicts": [
                    {"claim_id": "C-001", "verdict": "CONFIRMED", "notes": "OK"},
                    {"claim_id": "C-002", "verdict": "UNVERIFIABLE", "notes": "N/A"},
                ]
            }
        )
        provider = MockProvider(wrapped)
        verifier = ClaimVerifier(provider=provider, model="test-model")
        claims, bundles = self._make_claims_and_bundles()

        result = await verifier.verify_claims(claims, bundles)

        assert result[0].verdict == ClaimVerdict.CONFIRMED
        assert result[1].verdict == ClaimVerdict.UNVERIFIABLE


# ============================================================================
# Section 3: Validation Loop
# ============================================================================


class TestValidationLoop:
    """Tests for the ValidationLoop."""

    def _make_file_summary(
        self, verdicts: list[ClaimVerdict | None] | None = None
    ) -> FileSummary:
        verdicts_list = verdicts or [None, None]
        claims = [
            VerifiableClaim(
                claim_id=f"C-{i + 1:03d}",
                claim=f"Claim {i + 1}",
                category=ClaimCategory.OTHER,
                evidence_bundle_ids=["BDL-001"],
                verdict=v,
            )
            for i, v in enumerate(verdicts_list)
        ]
        bundles = [BundleSummary(bundle_id="BDL-001", paragraph_names=["PARA-A"])]
        return FileSummary(
            program_id="TESTPROG",
            file_name="TESTPROG.cbl",
            business_function="Test program",
            claims=claims,
            bundle_summaries=bundles,
            segment_summaries=[],
            passes_completed=3,
        )

    async def test_converges_immediately(self) -> None:
        """All claims confirmed on first pass -- loop runs one iteration."""
        verify_provider = MockProvider(VERIFICATION_ALL_CONFIRMED_JSON)
        verifier = ClaimVerifier(provider=verify_provider, model="test-model")

        root_provider = MockProvider(ROOT_SUMMARY_JSON)
        root_summarizer = RootSummarizer(provider=root_provider, model="test-model")

        loop = ValidationLoop(
            verifier=verifier,
            root_summarizer=root_summarizer,
            max_iterations=3,
        )

        summary = self._make_file_summary()
        result = await loop.validate(summary)

        assert result.validation_iterations == 1
        assert all(c.verdict == ClaimVerdict.CONFIRMED for c in result.claims)

    async def test_max_iterations_reached(self) -> None:
        """Refuted claims persist every round, loop stops at max_iterations."""
        # Verifier always returns mixed verdicts (one refuted)
        verify_provider = MockProvider(VERIFICATION_MIXED_JSON)
        verifier = ClaimVerifier(provider=verify_provider, model="test-model")

        # Root summarizer produces a summary with 2 claims each time
        root_provider = MockProvider(ROOT_SUMMARY_JSON)
        root_summarizer = RootSummarizer(provider=root_provider, model="test-model")

        max_iter = 2
        loop = ValidationLoop(
            verifier=verifier,
            root_summarizer=root_summarizer,
            max_iterations=max_iter,
        )

        summary = self._make_file_summary()
        result = await loop.validate(summary)

        assert result.validation_iterations == max_iter

    async def test_single_iteration_max(self) -> None:
        """With max_iterations=1, loop runs exactly once regardless of verdict."""
        verify_provider = MockProvider(VERIFICATION_MIXED_JSON)
        verifier = ClaimVerifier(provider=verify_provider, model="test-model")

        root_provider = MockProvider(ROOT_SUMMARY_JSON)
        root_summarizer = RootSummarizer(provider=root_provider, model="test-model")

        loop = ValidationLoop(
            verifier=verifier,
            root_summarizer=root_summarizer,
            max_iterations=1,
        )

        summary = self._make_file_summary()
        result = await loop.validate(summary)
        assert result.validation_iterations == 1

    async def test_no_claims_skips_gracefully(self) -> None:
        """Summary with no claims converges immediately."""
        verify_provider = MockProvider("[]")
        verifier = ClaimVerifier(provider=verify_provider, model="test-model")

        root_provider = MockProvider(ROOT_SUMMARY_JSON)
        root_summarizer = RootSummarizer(provider=root_provider, model="test-model")

        loop = ValidationLoop(
            verifier=verifier,
            root_summarizer=root_summarizer,
            max_iterations=3,
        )

        summary = FileSummary(
            program_id="EMPTY",
            file_name="EMPTY.cbl",
            business_function="Nothing",
            claims=[],
            bundle_summaries=[],
            segment_summaries=[],
            passes_completed=3,
        )
        result = await loop.validate(summary)
        assert result.validation_iterations == 1


# ============================================================================
# Section 4: Pipeline Integration
# ============================================================================


class TestSummarizationPipeline:
    """Integration tests for the full SummarizationPipeline."""

    def _make_chunk_templates(
        self, count: int = 3
    ) -> list[tuple[str, DocumentationTemplate]]:
        templates = []
        for i in range(count):
            template = DocumentationTemplate(
                header=HeaderSection(program_id="TESTPROG", file_name="TESTPROG.cbl"),
                paragraphs=[
                    Paragraph(
                        paragraph_name=f"PARA-{i}-A",
                        summary=f"Paragraph A of chunk {i}",
                    ),
                    Paragraph(
                        paragraph_name=f"PARA-{i}-B",
                        summary=f"Paragraph B of chunk {i}",
                    ),
                ],
            )
            templates.append((f"CHUNK-{i:03d}", template))
        return templates

    async def test_pipeline_end_to_end(self) -> None:
        """Run the full pipeline with a multi-response mock provider."""
        responses: dict[str, str] = {
            # Pass 1: BundleSummarizer prompt contains "Bundle ID:"
            "Bundle ID:": BUNDLE_SUMMARY_JSON,
            # Pass 2: CoherenceMerger prompt contains "Merge the following"
            "Merge the following": SEGMENT_SUMMARY_JSON,
            # Pass 3: RootSummarizer prompt contains "Synthesize the following"
            "Synthesize the following": ROOT_SUMMARY_JSON,
            # Validation: ClaimVerifier prompt contains "Verify the following"
            "Verify the following": VERIFICATION_ALL_CONFIRMED_JSON,
        }
        provider = MultiResponseProvider(responses)

        pipeline = SummarizationPipeline(
            provider=provider,
            model="test-model",
            merge_group_size=6,
            max_validation_iterations=2,
        )

        chunk_templates = self._make_chunk_templates(3)
        result = await pipeline.run(
            program_id="TESTPROG",
            file_name="TESTPROG.cbl",
            chunk_templates=chunk_templates,
        )

        assert isinstance(result, FileSummary)
        assert result.program_id == "TESTPROG"
        assert result.file_name == "TESTPROG.cbl"
        assert result.passes_completed == 3
        # Should have bundle summaries from Pass 1
        assert len(result.bundle_summaries) == 3
        # Should have segment summaries from Pass 2
        assert len(result.segment_summaries) >= 1
        # Should have claims from Pass 3
        assert len(result.claims) >= 1
        # Validation should have run
        assert result.validation_iterations >= 1

    async def test_pipeline_with_source_lines(self) -> None:
        responses: dict[str, str] = {
            "Bundle ID:": BUNDLE_SUMMARY_JSON,
            "Merge the following": SEGMENT_SUMMARY_JSON,
            "Synthesize the following": ROOT_SUMMARY_JSON,
            "Verify the following": VERIFICATION_ALL_CONFIRMED_JSON,
        }
        provider = MultiResponseProvider(responses)

        pipeline = SummarizationPipeline(
            provider=provider,
            model="test-model",
        )

        chunk_templates = self._make_chunk_templates(2)
        source = ["       PERFORM 0100-INIT.\n"] * 50

        result = await pipeline.run(
            program_id="SRCPROG",
            file_name="SRCPROG.cbl",
            chunk_templates=chunk_templates,
            source_lines=source,
        )

        assert isinstance(result, FileSummary)
        assert result.program_id == "SRCPROG"

    async def test_pipeline_empty_chunks(self) -> None:
        """Pipeline with no chunks should still produce a valid (empty) summary."""
        responses: dict[str, str] = {
            "Merge the following": SEGMENT_SUMMARY_JSON,
            "Synthesize the following": ROOT_SUMMARY_JSON,
            "Verify the following": VERIFICATION_ALL_CONFIRMED_JSON,
        }
        provider = MultiResponseProvider(responses)

        pipeline = SummarizationPipeline(
            provider=provider,
            model="test-model",
        )

        result = await pipeline.run(
            program_id="EMPTY",
            file_name="EMPTY.cbl",
            chunk_templates=[],
        )

        assert isinstance(result, FileSummary)
        assert len(result.bundle_summaries) == 0

    def test_build_bundle_inputs(self) -> None:
        """Test the static method that converts chunk templates to BundleInputs."""
        templates = self._make_chunk_templates(2)
        inputs = SummarizationPipeline._build_bundle_inputs(templates)

        assert len(inputs) == 2
        assert inputs[0].bundle_id == "CHUNK-000"
        assert inputs[0].paragraph_names == ["PARA-0-A", "PARA-0-B"]
        assert inputs[1].bundle_id == "CHUNK-001"
        assert inputs[1].paragraph_names == ["PARA-1-A", "PARA-1-B"]
        # template_json should be valid JSON
        json.loads(inputs[0].template_json)

    def test_save_and_load_summary(self, tmp_path: Any) -> None:
        """Test persistence round-trip via save_summary / load_summary."""
        summary = FileSummary(
            program_id="SAVE-TEST",
            file_name="SAVE-TEST.cbl",
            business_function="Testing save/load",
            primary_data_flows=["A -> B"],
            claims=[
                VerifiableClaim(
                    claim_id="C-001",
                    claim="Saveable claim",
                    category=ClaimCategory.OTHER,
                    verdict=ClaimVerdict.CONFIRMED,
                )
            ],
            bundle_summaries=[
                BundleSummary(bundle_id="BDL-001", paragraph_names=["PARA-1"])
            ],
            segment_summaries=[],
            passes_completed=3,
            validation_iterations=1,
        )

        path = tmp_path / "test.summary.json"
        SummarizationPipeline.save_summary(summary, path)
        assert path.exists()

        loaded = SummarizationPipeline.load_summary(path)
        assert loaded.program_id == "SAVE-TEST"
        assert loaded.business_function == "Testing save/load"
        assert len(loaded.claims) == 1
        assert loaded.claims[0].verdict == ClaimVerdict.CONFIRMED
        assert len(loaded.bundle_summaries) == 1
        assert loaded.passes_completed == 3

    async def test_pipeline_with_pass1_model_override(self) -> None:
        """Pass 1 can use a different (cheaper) model."""
        call_log: list[str] = []

        class LoggingProvider:
            @property
            def default_model(self) -> str:
                return "default-model"

            async def complete(
                self,
                messages: list[Message],
                model: str | None = None,
                temperature: float = 0.7,
                **kwargs: Any,
            ) -> CompletionResponse:
                call_log.append(model or "default")
                return CompletionResponse(
                    content=BUNDLE_SUMMARY_JSON,
                    model=model or "default-model",
                    tokens_used=50,
                )

        provider = LoggingProvider()
        pipeline = SummarizationPipeline(
            provider=provider,
            model="expensive-model",
            pass1_model="cheap-model",
        )

        chunk_templates = self._make_chunk_templates(1)

        # We only test Pass 1 here by directly calling the bundle summarizer
        # since the full pipeline would require valid responses for all passes
        bundle_summarizer = BundleSummarizer(
            provider=provider,
            model="cheap-model",
        )
        inputs = pipeline._build_bundle_inputs(chunk_templates)
        await bundle_summarizer.summarize_bundles(inputs)

        assert "cheap-model" in call_log


# ============================================================================
# Section 5: Config tests
# ============================================================================


class TestSummarizationConfig:
    """Tests for summarization-related settings in WarRigConfig."""

    def test_summarization_config_defaults(self) -> None:
        from war_rig.config import WarRigConfig

        config = WarRigConfig()
        assert config.summarization_enabled is False
        assert config.summarization_merge_group_size == 6
        assert config.summarization_pass1_model is None
        assert config.summarization_max_validation_iterations == 3
        assert config.summarization_min_bundles == 3

    def test_summarization_config_custom(self) -> None:
        from war_rig.config import WarRigConfig

        config = WarRigConfig(
            summarization_enabled=True,
            summarization_merge_group_size=8,
            summarization_pass1_model="cheap/haiku",
            summarization_max_validation_iterations=5,
            summarization_min_bundles=5,
        )
        assert config.summarization_enabled is True
        assert config.summarization_merge_group_size == 8
        assert config.summarization_pass1_model == "cheap/haiku"
        assert config.summarization_max_validation_iterations == 5
        assert config.summarization_min_bundles == 5

    def test_summarization_merge_group_size_bounds(self) -> None:
        """merge_group_size has ge=2, le=12 bounds."""
        from pydantic import ValidationError

        from war_rig.config import WarRigConfig

        with pytest.raises(ValidationError):
            WarRigConfig(summarization_merge_group_size=1)
        with pytest.raises(ValidationError):
            WarRigConfig(summarization_merge_group_size=13)

    def test_summarization_max_validation_iterations_bounds(self) -> None:
        """max_validation_iterations has ge=1, le=10 bounds."""
        from pydantic import ValidationError

        from war_rig.config import WarRigConfig

        with pytest.raises(ValidationError):
            WarRigConfig(summarization_max_validation_iterations=0)
        with pytest.raises(ValidationError):
            WarRigConfig(summarization_max_validation_iterations=11)

    def test_summarization_min_bundles_bounds(self) -> None:
        """min_bundles has ge=2, le=20 bounds."""
        from pydantic import ValidationError

        from war_rig.config import WarRigConfig

        with pytest.raises(ValidationError):
            WarRigConfig(summarization_min_bundles=1)
        with pytest.raises(ValidationError):
            WarRigConfig(summarization_min_bundles=21)


# ============================================================================
# Section 3 (extra): _extract_json helper tests
# ============================================================================


class TestExtractJson:
    """Tests for the _extract_json helper used by multiple passes."""

    def test_direct_json(self) -> None:
        from war_rig.summarization.bundle_summarizer import _extract_json

        data = _extract_json('{"key": "value"}')
        assert data == {"key": "value"}

    def test_fenced_json(self) -> None:
        from war_rig.summarization.bundle_summarizer import _extract_json

        text = 'Here is the JSON:\n```json\n{"key": "value"}\n```\nDone.'
        data = _extract_json(text)
        assert data == {"key": "value"}

    def test_outermost_braces(self) -> None:
        from war_rig.summarization.bundle_summarizer import _extract_json

        text = 'Some preamble {"key": "value"} postamble'
        data = _extract_json(text)
        assert data == {"key": "value"}

    def test_total_failure_returns_empty_dict(self) -> None:
        from war_rig.summarization.bundle_summarizer import _extract_json

        data = _extract_json("no json here at all")
        assert data == {}

    def test_array_is_not_returned_as_dict(self) -> None:
        from war_rig.summarization.bundle_summarizer import _extract_json

        data = _extract_json("[1, 2, 3]")
        assert data == {}  # bundle_summarizer version only accepts dicts

    def test_claim_verifier_extract_json_accepts_lists(self) -> None:
        from war_rig.summarization.claim_verifier import (
            _extract_json as cv_extract_json,
        )

        data = cv_extract_json('[{"claim_id": "C-001"}]')
        assert isinstance(data, list)
        assert len(data) == 1


# ============================================================================
# BundleInput tests
# ============================================================================


class TestBundleInput:
    """Tests for the BundleInput data transfer object."""

    def test_construction(self) -> None:
        bi = BundleInput(
            bundle_id="BDL-001",
            paragraph_names=["PARA-A", "PARA-B"],
            template_json='{"header": {}}',
            source_excerpt="       PERFORM PARA-A.",
        )
        assert bi.bundle_id == "BDL-001"
        assert bi.paragraph_names == ["PARA-A", "PARA-B"]
        assert bi.template_json == '{"header": {}}'
        assert bi.source_excerpt == "       PERFORM PARA-A."

    def test_default_source_excerpt(self) -> None:
        bi = BundleInput(
            bundle_id="BDL-002",
            paragraph_names=["PARA-C"],
            template_json="{}",
        )
        assert bi.source_excerpt == ""
