"""Tests for structural cross-validation between docs and knowledge graph.

Covers StructuralValidator, StructuralFinding, FindingType, and the
format_findings compact output for Imperator prompt injection.
"""

from __future__ import annotations

from unittest.mock import AsyncMock, MagicMock

import pytest

from war_rig.knowledge_graph.models import (
    Conflict,
    Entity,
    EntityType,
    RelationType,
    Triple,
)
from war_rig.validation.structural_validator import (
    FindingType,
    StructuralFinding,
    StructuralValidator,
)

# =============================================================================
# Fixtures
# =============================================================================


@pytest.fixture
def mock_store() -> MagicMock:
    """Create a mock SQLiteGraphStore with async methods."""
    store = MagicMock()
    store.get_entity = AsyncMock(return_value=None)
    store.get_entity_by_id = AsyncMock(return_value=None)
    store.get_triples_for_entity = AsyncMock(return_value=[])
    store.get_unresolved_conflicts = AsyncMock(return_value=[])
    store.get_all_entities = AsyncMock(return_value=[])
    store.get_all_triples = AsyncMock(return_value=[])
    return store


@pytest.fixture
def mock_kg_manager(mock_store: MagicMock) -> MagicMock:
    """Create a mock KnowledgeGraphManager with a mock store."""
    manager = MagicMock()
    manager.enabled = True
    manager._store = mock_store
    return manager


@pytest.fixture
def validator(mock_kg_manager: MagicMock) -> StructuralValidator:
    """Create a StructuralValidator with mocked KG manager."""
    return StructuralValidator(mock_kg_manager)


def _make_entity(
    entity_id: int,
    name: str,
    entity_type: EntityType = EntityType.PROGRAM,
) -> Entity:
    """Helper to create an Entity with given id and name."""
    return Entity(id=entity_id, entity_type=entity_type, name=name)


def _make_triple(
    triple_id: int,
    subject_id: int,
    predicate: RelationType,
    object_id: int,
    source_pass: str = "pass_1",
) -> Triple:
    """Helper to create a Triple."""
    return Triple(
        id=triple_id,
        subject_id=subject_id,
        predicate=predicate,
        object_id=object_id,
        source_pass=source_pass,
    )


# =============================================================================
# FindingType / StructuralFinding unit tests
# =============================================================================


class TestFindingType:
    """Tests for FindingType enum."""

    def test_values(self) -> None:
        assert FindingType.MISMATCH == "MISMATCH"
        assert FindingType.CONFLICT == "CONFLICT"
        assert FindingType.ORPHAN == "ORPHAN"
        assert FindingType.SEMANTICS == "SEMANTICS"

    def test_is_str_enum(self) -> None:
        assert isinstance(FindingType.MISMATCH, str)


class TestStructuralFinding:
    """Tests for StructuralFinding dataclass."""

    def test_basic_creation(self) -> None:
        finding = StructuralFinding(
            finding_type=FindingType.MISMATCH,
            description="PROG_A docs claim CALLS PROG_C -- no KG evidence",
        )
        assert finding.finding_type == FindingType.MISMATCH
        assert "PROG_A" in finding.description
        assert finding.affected_files == []

    def test_with_affected_files(self) -> None:
        finding = StructuralFinding(
            finding_type=FindingType.ORPHAN,
            description="PROG_Z documented but 0 KG triples",
            affected_files=["PROG_Z"],
        )
        assert finding.affected_files == ["PROG_Z"]


# =============================================================================
# StructuralValidator._check_call_mismatches
# =============================================================================


class TestCheckCallMismatches:
    """Tests for call graph vs KG CALLS triple mismatch detection."""

    async def test_no_mismatches_when_doc_matches_kg(
        self,
        validator: StructuralValidator,
        mock_store: MagicMock,
    ) -> None:
        """When doc call graph matches KG triples, no findings."""
        caller = _make_entity(1, "PROG_A")
        callee = _make_entity(2, "PROG_B")
        mock_store.get_entity = AsyncMock(return_value=caller)
        mock_store.get_triples_for_entity = AsyncMock(
            return_value=[
                _make_triple(10, 1, RelationType.CALLS, 2),
            ]
        )
        mock_store.get_entity_by_id = AsyncMock(return_value=callee)

        findings = await validator._check_call_mismatches(
            documented_programs=["PROG_A", "PROG_B"],
            call_graph={"PROG_A": ["PROG_B"]},
        )
        assert len(findings) == 0

    async def test_doc_claims_call_not_in_kg(
        self,
        validator: StructuralValidator,
        mock_store: MagicMock,
    ) -> None:
        """Doc says PROG_A CALLS PROG_C but KG has no such triple."""
        caller = _make_entity(1, "PROG_A")
        mock_store.get_entity = AsyncMock(return_value=caller)
        # KG has no CALLS triples for this entity
        mock_store.get_triples_for_entity = AsyncMock(return_value=[])

        findings = await validator._check_call_mismatches(
            documented_programs=["PROG_A", "PROG_C"],
            call_graph={"PROG_A": ["PROG_C"]},
        )
        assert len(findings) == 1
        assert findings[0].finding_type == FindingType.MISMATCH
        assert "PROG_C" in findings[0].description
        assert "no KG evidence" in findings[0].description

    async def test_kg_has_call_not_in_doc(
        self,
        validator: StructuralValidator,
        mock_store: MagicMock,
    ) -> None:
        """KG has PROG_A CALLS PROG_D but doc doesn't mention it."""
        caller = _make_entity(1, "PROG_A")
        callee_d = _make_entity(4, "PROG_D")
        mock_store.get_entity = AsyncMock(return_value=caller)
        mock_store.get_triples_for_entity = AsyncMock(
            return_value=[
                _make_triple(10, 1, RelationType.CALLS, 4),
            ]
        )
        mock_store.get_entity_by_id = AsyncMock(return_value=callee_d)

        findings = await validator._check_call_mismatches(
            documented_programs=["PROG_A"],
            call_graph={"PROG_A": []},  # doc says no calls
        )
        assert len(findings) == 1
        assert findings[0].finding_type == FindingType.MISMATCH
        assert "PROG_D" in findings[0].description
        assert "not in docs" in findings[0].description

    async def test_caller_not_in_kg_skipped(
        self,
        validator: StructuralValidator,
        mock_store: MagicMock,
    ) -> None:
        """When caller entity is not in KG, skip (orphan check handles it)."""
        mock_store.get_entity = AsyncMock(return_value=None)

        findings = await validator._check_call_mismatches(
            documented_programs=["PROG_X"],
            call_graph={"PROG_X": ["PROG_Y"]},
        )
        assert len(findings) == 0

    async def test_non_calls_triples_ignored(
        self,
        validator: StructuralValidator,
        mock_store: MagicMock,
    ) -> None:
        """READS/WRITES triples should not be treated as CALLS mismatches."""
        caller = _make_entity(1, "PROG_A")
        mock_store.get_entity = AsyncMock(return_value=caller)
        # KG has READS triple, not CALLS
        mock_store.get_triples_for_entity = AsyncMock(
            return_value=[
                _make_triple(10, 1, RelationType.READS, 5),
            ]
        )

        findings = await validator._check_call_mismatches(
            documented_programs=["PROG_A"],
            call_graph={"PROG_A": []},
        )
        assert len(findings) == 0


# =============================================================================
# StructuralValidator._check_conflicts
# =============================================================================


class TestCheckConflicts:
    """Tests for unresolved KG conflict surfacing."""

    async def test_no_conflicts(
        self,
        validator: StructuralValidator,
        mock_store: MagicMock,
    ) -> None:
        mock_store.get_unresolved_conflicts = AsyncMock(return_value=[])
        findings = await validator._check_conflicts()
        assert len(findings) == 0

    async def test_single_conflict(
        self,
        validator: StructuralValidator,
        mock_store: MagicMock,
    ) -> None:
        conflict = Conflict(
            id=1, triple_a_id=10, triple_b_id=11, resolved=False
        )
        triple_a = _make_triple(10, 1, RelationType.READS, 5)
        triple_b = _make_triple(11, 1, RelationType.WRITES, 5)
        subj = _make_entity(1, "PROG_A")
        obj = _make_entity(5, "DATASET_X", EntityType.DATASET)

        mock_store.get_unresolved_conflicts = AsyncMock(
            return_value=[conflict]
        )
        mock_store.get_all_triples = AsyncMock(
            return_value=[triple_a, triple_b]
        )
        mock_store.get_entity_by_id = AsyncMock(
            side_effect=lambda eid: {1: subj, 5: obj}.get(eid)
        )

        findings = await validator._check_conflicts()
        assert len(findings) == 1
        assert findings[0].finding_type == FindingType.CONFLICT
        assert "READS" in findings[0].description
        assert "WRITES" in findings[0].description
        assert "PROG_A" in findings[0].affected_files

    async def test_conflict_with_missing_triples(
        self,
        validator: StructuralValidator,
        mock_store: MagicMock,
    ) -> None:
        """When triple details can't be resolved, fallback description."""
        conflict = Conflict(
            id=99, triple_a_id=100, triple_b_id=101, resolved=False
        )
        mock_store.get_unresolved_conflicts = AsyncMock(
            return_value=[conflict]
        )
        # No triples in store
        mock_store.get_all_triples = AsyncMock(return_value=[])

        findings = await validator._check_conflicts()
        assert len(findings) == 1
        assert findings[0].finding_type == FindingType.CONFLICT
        assert "Unresolved conflict" in findings[0].description


# =============================================================================
# StructuralValidator._check_orphans
# =============================================================================


class TestCheckOrphans:
    """Tests for documented-but-no-KG and KG-but-not-documented orphans."""

    async def test_no_orphans(
        self,
        validator: StructuralValidator,
        mock_store: MagicMock,
    ) -> None:
        mock_store.get_all_entities = AsyncMock(
            return_value=[
                _make_entity(1, "PROG_A"),
                _make_entity(2, "PROG_B"),
            ]
        )
        findings = await validator._check_orphans(["PROG_A", "PROG_B"])
        assert len(findings) == 0

    async def test_documented_but_no_kg(
        self,
        validator: StructuralValidator,
        mock_store: MagicMock,
    ) -> None:
        mock_store.get_all_entities = AsyncMock(
            return_value=[_make_entity(1, "PROG_A")]
        )
        findings = await validator._check_orphans(["PROG_A", "PROG_Z"])
        assert len(findings) == 1
        assert findings[0].finding_type == FindingType.ORPHAN
        assert "PROG_Z" in findings[0].description
        assert "0 KG triples" in findings[0].description

    async def test_kg_but_not_documented(
        self,
        validator: StructuralValidator,
        mock_store: MagicMock,
    ) -> None:
        mock_store.get_all_entities = AsyncMock(
            return_value=[
                _make_entity(1, "PROG_A"),
                _make_entity(2, "PROG_B"),
                _make_entity(3, "PROG_EXTRA"),
            ]
        )
        findings = await validator._check_orphans(["PROG_A", "PROG_B"])
        assert len(findings) == 1
        assert findings[0].finding_type == FindingType.ORPHAN
        assert "PROG_EXTRA" in findings[0].description
        assert "not documented" in findings[0].description

    async def test_non_program_entities_ignored(
        self,
        validator: StructuralValidator,
        mock_store: MagicMock,
    ) -> None:
        """Dataset entities should not appear as orphans."""
        mock_store.get_all_entities = AsyncMock(
            return_value=[
                _make_entity(1, "PROG_A"),
                _make_entity(5, "DATASET_X", EntityType.DATASET),
            ]
        )
        findings = await validator._check_orphans(["PROG_A"])
        assert len(findings) == 0


# =============================================================================
# StructuralValidator._check_call_semantics
# =============================================================================


class TestCheckCallSemantics:
    """Tests for call semantics output/input mismatch detection."""

    async def test_no_semantics(
        self,
        validator: StructuralValidator,
    ) -> None:
        findings = await validator._check_call_semantics(None)
        assert len(findings) == 0

    async def test_empty_semantics(
        self,
        validator: StructuralValidator,
    ) -> None:
        findings = await validator._check_call_semantics({})
        assert len(findings) == 0

    async def test_matching_semantics_no_findings(
        self,
        validator: StructuralValidator,
    ) -> None:
        """When caller outputs overlap with callee inputs, no finding."""
        semantics = {
            "PROG_A->PROG_B": {
                "inputs": ["ACCOUNT_NUM", "BALANCE"],
                "outputs": ["ACCOUNT_NUM", "BALANCE"],
            },
        }
        findings = await validator._check_call_semantics(semantics)
        assert len(findings) == 0

    async def test_no_overlap_flags_semantics_issue(
        self,
        validator: StructuralValidator,
    ) -> None:
        """When caller outputs have zero overlap with callee inputs."""
        semantics = {
            "PROG_A->PROG_B": {
                "inputs": ["CUSTOMER_ID", "NAME"],
                "outputs": ["TRANSACTION_AMT", "DATE"],
            },
        }
        findings = await validator._check_call_semantics(semantics)
        assert len(findings) == 1
        assert findings[0].finding_type == FindingType.SEMANTICS
        assert "PROG_A->PROG_B" in findings[0].description
        assert "PROG_A" in findings[0].affected_files
        assert "PROG_B" in findings[0].affected_files

    async def test_partial_overlap_no_finding(
        self,
        validator: StructuralValidator,
    ) -> None:
        """When there is partial overlap, no finding (only flags zero overlap)."""
        semantics = {
            "PROG_A->PROG_B": {
                "inputs": ["ACCOUNT_NUM", "NAME"],
                "outputs": ["ACCOUNT_NUM", "AMOUNT"],
            },
        }
        findings = await validator._check_call_semantics(semantics)
        assert len(findings) == 0

    async def test_missing_arrow_in_key_skipped(
        self,
        validator: StructuralValidator,
    ) -> None:
        """Keys without '->' separator should be skipped."""
        semantics = {
            "PROG_A_PROG_B": {
                "inputs": ["X"],
                "outputs": ["Y"],
            },
        }
        findings = await validator._check_call_semantics(semantics)
        assert len(findings) == 0

    async def test_empty_inputs_or_outputs_skipped(
        self,
        validator: StructuralValidator,
    ) -> None:
        """When inputs or outputs are empty, skip the check."""
        semantics = {
            "PROG_A->PROG_B": {
                "inputs": [],
                "outputs": ["SOMETHING"],
            },
        }
        findings = await validator._check_call_semantics(semantics)
        assert len(findings) == 0


# =============================================================================
# StructuralValidator.validate (integration)
# =============================================================================


class TestValidate:
    """Tests for the top-level validate method."""

    async def test_empty_inputs(
        self,
        validator: StructuralValidator,
        mock_store: MagicMock,
    ) -> None:
        mock_store.get_all_entities = AsyncMock(return_value=[])
        findings = await validator.validate(
            documented_programs=[],
            call_graph={},
        )
        assert isinstance(findings, list)

    async def test_max_findings_cap(
        self,
        validator: StructuralValidator,
        mock_store: MagicMock,
    ) -> None:
        """Findings should be capped at max_findings."""
        # Create many orphans to exceed the cap
        mock_store.get_all_entities = AsyncMock(return_value=[])
        documented = [f"PROG_{i}" for i in range(20)]

        findings = await validator.validate(
            documented_programs=documented,
            call_graph={},
            max_findings=5,
        )
        assert len(findings) <= 5

    async def test_collects_from_all_checks(
        self,
        validator: StructuralValidator,
        mock_store: MagicMock,
    ) -> None:
        """validate() should collect findings from all check categories."""
        # Set up orphan (doc'd but not in KG)
        mock_store.get_all_entities = AsyncMock(return_value=[])
        # Set up conflict
        conflict = Conflict(
            id=1, triple_a_id=10, triple_b_id=11, resolved=False
        )
        mock_store.get_unresolved_conflicts = AsyncMock(
            return_value=[conflict]
        )
        mock_store.get_all_triples = AsyncMock(return_value=[])

        findings = await validator.validate(
            documented_programs=["PROG_ORPHAN"],
            call_graph={},
        )
        types = {f.finding_type for f in findings}
        assert FindingType.ORPHAN in types
        assert FindingType.CONFLICT in types

    async def test_resilient_to_check_failures(
        self,
        validator: StructuralValidator,
        mock_store: MagicMock,
    ) -> None:
        """Individual check failures should not crash validate()."""
        mock_store.get_unresolved_conflicts = AsyncMock(
            side_effect=RuntimeError("DB error")
        )
        mock_store.get_all_entities = AsyncMock(return_value=[])

        # Should not raise, should still return orphan findings
        findings = await validator.validate(
            documented_programs=["PROG_X"],
            call_graph={},
        )
        # Conflict check failed but orphan check should still work
        assert any(
            f.finding_type == FindingType.ORPHAN for f in findings
        )

    async def test_store_none_returns_empty(
        self,
        mock_kg_manager: MagicMock,
    ) -> None:
        """When store is None, all checks return empty lists."""
        mock_kg_manager._store = None
        val = StructuralValidator(mock_kg_manager)

        findings = await val.validate(
            documented_programs=["PROG_A"],
            call_graph={"PROG_A": ["PROG_B"]},
        )
        assert findings == []


# =============================================================================
# StructuralValidator.format_findings
# =============================================================================


class TestFormatFindings:
    """Tests for compact markdown formatting of findings."""

    def test_empty_findings(
        self,
        validator: StructuralValidator,
    ) -> None:
        assert validator.format_findings([]) == ""

    def test_single_finding(
        self,
        validator: StructuralValidator,
    ) -> None:
        findings = [
            StructuralFinding(
                finding_type=FindingType.MISMATCH,
                description="PROG_A docs claim CALLS PROG_C -- no KG evidence",
            ),
        ]
        result = validator.format_findings(findings)
        assert "## Structural Cross-Check (1 findings)" in result
        assert "- MISMATCH:" in result
        assert "PROG_A" in result

    def test_multiple_findings(
        self,
        validator: StructuralValidator,
    ) -> None:
        findings = [
            StructuralFinding(
                finding_type=FindingType.MISMATCH,
                description="PROG_A docs claim CALLS PROG_C -- no KG evidence",
            ),
            StructuralFinding(
                finding_type=FindingType.CONFLICT,
                description="PROG_B READS vs WRITES DATASET_X (unresolved)",
            ),
            StructuralFinding(
                finding_type=FindingType.ORPHAN,
                description="PROG_Z documented but 0 KG triples",
            ),
        ]
        result = validator.format_findings(findings)
        assert "(3 findings)" in result
        assert "- MISMATCH:" in result
        assert "- CONFLICT:" in result
        assert "- ORPHAN:" in result

    def test_truncation_with_low_budget(
        self,
        validator: StructuralValidator,
    ) -> None:
        """When token budget is very low, should truncate with '... and N more'."""
        findings = [
            StructuralFinding(
                finding_type=FindingType.ORPHAN,
                description=f"PROG_{i} documented but 0 KG triples",
            )
            for i in range(10)
        ]
        # Very low budget: ~50 tokens = ~200 chars
        result = validator.format_findings(findings, max_tokens=50)
        assert "... and" in result
        assert "more" in result

    def test_within_default_budget(
        self,
        validator: StructuralValidator,
    ) -> None:
        """Normal findings should fit within the 300 token default budget."""
        findings = [
            StructuralFinding(
                finding_type=FindingType.MISMATCH,
                description="PROG_A CALLS PROG_C -- no KG evidence",
            ),
            StructuralFinding(
                finding_type=FindingType.CONFLICT,
                description="PROG_B READS vs WRITES DSN (unresolved)",
            ),
            StructuralFinding(
                finding_type=FindingType.ORPHAN,
                description="PROG_Z documented but 0 KG triples",
            ),
        ]
        result = validator.format_findings(findings, max_tokens=300)
        # Should include all 3 findings without truncation
        assert "... and" not in result
        # Count lines starting with "- " (finding lines)
        finding_lines = [
            line for line in result.splitlines() if line.startswith("- ")
        ]
        assert len(finding_lines) == 3

    def test_output_ends_with_newline(
        self,
        validator: StructuralValidator,
    ) -> None:
        findings = [
            StructuralFinding(
                finding_type=FindingType.ORPHAN,
                description="PROG_X orphan",
            ),
        ]
        result = validator.format_findings(findings)
        assert result.endswith("\n")
