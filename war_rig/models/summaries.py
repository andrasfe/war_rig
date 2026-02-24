"""Summarization models for recursive document compression.

Defines a three-level hierarchy used when a COBOL file exceeds the
practical context window for a single Scribe pass:

- :class:`BundleSummary` — per-bundle structured extraction (Pass 1)
- :class:`SegmentSummary` — coherence-merged mid-level summary (Pass 2)
- :class:`FileSummary` — canonical root summary with verifiable claims (Pass 3)

Plus :class:`VerifiableClaim` for Challenger-driven top-down validation.
"""

from __future__ import annotations

from datetime import UTC, datetime
from enum import Enum
from typing import Any

from pydantic import BaseModel, ConfigDict, Field, computed_field

from war_rig.models.templates import LenientStrList

# ---------------------------------------------------------------------------
# Enums
# ---------------------------------------------------------------------------


class ClaimCategory(str, Enum):
    """Category of a verifiable claim about a documented file."""

    PERFORM_RELATIONSHIP = "PERFORM_RELATIONSHIP"
    DATA_FLOW = "DATA_FLOW"
    IO_OPERATION = "IO_OPERATION"
    EXTERNAL_CALL = "EXTERNAL_CALL"
    BUSINESS_RULE = "BUSINESS_RULE"
    ERROR_HANDLING = "ERROR_HANDLING"
    DEAD_CODE = "DEAD_CODE"
    OTHER = "OTHER"

    @classmethod
    def _missing_(cls, value: object) -> ClaimCategory | None:
        if isinstance(value, str):
            upper = value.upper().replace(" ", "_").replace("-", "_")
            for member in cls:
                if member.value == upper:
                    return member
        return None


class ClaimVerdict(str, Enum):
    """Result of verifying a claim against evidence."""

    CONFIRMED = "CONFIRMED"
    REFUTED = "REFUTED"
    UNVERIFIABLE = "UNVERIFIABLE"

    @classmethod
    def _missing_(cls, value: object) -> ClaimVerdict | None:
        if isinstance(value, str):
            upper = value.upper()
            for member in cls:
                if member.value == upper:
                    return member
        return None


# ---------------------------------------------------------------------------
# Supporting models
# ---------------------------------------------------------------------------


class ConditionalBranch(BaseModel):
    """A conditional logic branch within a bundle."""

    model_config = ConfigDict(extra="ignore")

    condition: str = ""
    business_meaning: str = ""
    outcome_true: str = ""
    outcome_false: str | None = None


class DataFlowItem(BaseModel):
    """A resolved data flow between program elements."""

    model_config = ConfigDict(extra="ignore")

    source: str = ""
    target: str = ""
    data_item: str = ""
    description: str = ""


class PerformLink(BaseModel):
    """A resolved PERFORM relationship."""

    model_config = ConfigDict(extra="ignore")

    caller: str = ""
    callee: str = ""
    purpose: str | None = None


# ---------------------------------------------------------------------------
# Pass 1: Bundle summaries
# ---------------------------------------------------------------------------


class BundleSummary(BaseModel):
    """Structured extraction from a single paragraph bundle.

    Produced by Pass 1 of the summarization pipeline.  Each bundle
    corresponds to a chunk of the original file (one or more paragraphs
    processed together by the Scribe).
    """

    model_config = ConfigDict(extra="ignore")

    bundle_id: str = ""
    paragraph_names: LenientStrList = Field(default_factory=list)
    functional_summary: str = ""
    data_items_read: LenientStrList = Field(default_factory=list)
    data_items_written: LenientStrList = Field(default_factory=list)
    data_items_transformed: LenientStrList = Field(default_factory=list)
    perform_calls: LenientStrList = Field(default_factory=list)
    perform_callers: LenientStrList = Field(default_factory=list)
    conditional_branches: list[ConditionalBranch] = Field(default_factory=list)
    anomalies: LenientStrList = Field(default_factory=list)
    source_line_start: int = 0
    source_line_end: int = 0
    metadata: dict[str, Any] | None = None


# ---------------------------------------------------------------------------
# Pass 2: Segment summaries
# ---------------------------------------------------------------------------


class SegmentSummary(BaseModel):
    """Coherence-resolved mid-level summary of a functional area.

    Produced by Pass 2 of the summarization pipeline.  Groups 4–8
    adjacent bundle summaries and actively resolves coreferences,
    argument threading, and contradictions.
    """

    model_config = ConfigDict(extra="ignore")

    segment_id: str = ""
    bundle_ids: LenientStrList = Field(default_factory=list)
    functional_area: str = ""
    summary: str = ""
    data_flows: list[DataFlowItem] = Field(default_factory=list)
    perform_graph: list[PerformLink] = Field(default_factory=list)
    resolved_coreferences: LenientStrList = Field(default_factory=list)
    flagged_contradictions: LenientStrList = Field(default_factory=list)
    flagged_redundancies: LenientStrList = Field(default_factory=list)
    metadata: dict[str, Any] | None = None


# ---------------------------------------------------------------------------
# Verifiable claims
# ---------------------------------------------------------------------------


class VerifiableClaim(BaseModel):
    """A concrete assertion about the file that can be checked.

    Generated during Pass 3 and verified by the Challenger in the
    validation loop.
    """

    model_config = ConfigDict(extra="ignore")

    claim_id: str = ""
    claim: str = ""
    category: ClaimCategory = ClaimCategory.OTHER
    evidence_bundle_ids: LenientStrList = Field(default_factory=list)
    verdict: ClaimVerdict | None = None
    verification_notes: str | None = None


# ---------------------------------------------------------------------------
# Pass 3: File-level root summary
# ---------------------------------------------------------------------------


class FileSummary(BaseModel):
    """Canonical root-level summary of an entire file.

    The top of the three-level hierarchy.  Includes the full bundle
    and segment summaries for drill-down, plus verifiable claims for
    Challenger validation.

    Use :meth:`to_context_string` to render a compact version suitable
    for injection into Scribe paragraph documentation prompts.
    """

    model_config = ConfigDict(extra="ignore")

    program_id: str = ""
    file_name: str = ""
    business_function: str = ""
    primary_data_flows: LenientStrList = Field(default_factory=list)
    call_graph_summary: LenientStrList = Field(default_factory=list)
    risk_areas: LenientStrList = Field(default_factory=list)
    technical_debt: LenientStrList = Field(default_factory=list)
    migration_considerations: LenientStrList = Field(default_factory=list)
    claims: list[VerifiableClaim] = Field(default_factory=list)
    bundle_summaries: list[BundleSummary] = Field(default_factory=list)
    segment_summaries: list[SegmentSummary] = Field(default_factory=list)
    timestamp: datetime = Field(
        default_factory=lambda: datetime.now(UTC),
    )
    passes_completed: int = 0
    validation_iterations: int = 0
    metadata: dict[str, Any] | None = None

    @computed_field  # type: ignore[prop-decorator]
    @property
    def total_paragraphs(self) -> int:
        return sum(len(b.paragraph_names) for b in self.bundle_summaries)

    @computed_field  # type: ignore[prop-decorator]
    @property
    def claims_verified(self) -> int:
        return sum(1 for c in self.claims if c.verdict == ClaimVerdict.CONFIRMED)

    @computed_field  # type: ignore[prop-decorator]
    @property
    def claims_refuted(self) -> int:
        return sum(1 for c in self.claims if c.verdict == ClaimVerdict.REFUTED)

    def to_context_string(self) -> str:
        """Render a compact summary for injection into Scribe prompts.

        The output is designed to give Scribe agents awareness of the
        overall program when documenting individual paragraphs.
        """
        parts: list[str] = []
        parts.append(f"## File-Level Summary: {self.file_name}")
        if self.business_function:
            parts.append(f"\n**Business Function**: {self.business_function}")

        if self.primary_data_flows:
            parts.append("\n**Primary Data Flows**:")
            for flow in self.primary_data_flows:
                parts.append(f"- {flow}")

        if self.call_graph_summary:
            parts.append("\n**Call Graph**:")
            for entry in self.call_graph_summary:
                parts.append(f"- {entry}")

        if self.risk_areas:
            parts.append("\n**Risk Areas**:")
            for risk in self.risk_areas:
                parts.append(f"- {risk}")

        if self.segment_summaries:
            parts.append("\n**Functional Areas**:")
            for seg in self.segment_summaries:
                parts.append(f"- {seg.functional_area}: {seg.summary}")

        return "\n".join(parts)
