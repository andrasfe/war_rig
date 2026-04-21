"""Tests for war_rig.doof_wagon.bridge.

Exercises the integration surface without running the real War Rig graph:
- priors loading with schema validation
- open_questions emission
- signal wait loop (short timeouts)
- ticket creation falls back cleanly when beads is unavailable
"""

from __future__ import annotations

import json
import threading
import time
from pathlib import Path

import pytest
from jsonschema import ValidationError

from war_rig.doof_wagon.bridge import (
    create_residue_ticket,
    emit_open_questions,
    load_priors,
    wait_for_signal,
)


def test_load_priors_accepts_war_rig_consumer(tmp_path: Path) -> None:
    priors_path = tmp_path / "priors.json"
    priors_path.write_text(json.dumps({
        "schema_version": "1.0",
        "module": "PROCESS-PAYMENT-ELIGIBILITY",
        "round": 2,
        "consumer": "war_rig",
        "challenger_inputs": [
            {
                "finding_id": "G1",
                "finding_type": "UNDOCUMENTED_PATH",
                "location": "LINE-75",
                "evidence": {"constraint": "x > 0"},
                "prompt": "Document this path.",
            }
        ],
    }))
    priors = load_priors(priors_path)
    assert priors["consumer"] == "war_rig"
    assert len(priors["challenger_inputs"]) == 1


def test_load_priors_rejects_specter_consumer(tmp_path: Path) -> None:
    priors_path = tmp_path / "priors.json"
    priors_path.write_text(json.dumps({
        "schema_version": "1.0",
        "module": "M",
        "round": 2,
        "consumer": "specter",
    }))
    with pytest.raises(ValueError, match="war_rig"):
        load_priors(priors_path)


def test_load_priors_rejects_missing_required_fields(tmp_path: Path) -> None:
    priors_path = tmp_path / "priors.json"
    priors_path.write_text(json.dumps({"schema_version": "1.0"}))
    with pytest.raises(ValidationError):
        load_priors(priors_path)


def test_load_priors_not_found(tmp_path: Path) -> None:
    with pytest.raises(FileNotFoundError):
        load_priors(tmp_path / "nope.json")


def test_emit_open_questions_minimal_shape(tmp_path: Path) -> None:
    out = tmp_path / "war_rig_out.json"
    emit_open_questions(
        out,
        module="PROCESS-PAYMENT-ELIGIBILITY",
        round_num=1,
        war_rig_result={},
        started_at="2026-04-21T10:00:00Z",
        completed_at="2026-04-21T10:05:00Z",
    )
    assert out.exists()
    data = json.loads(out.read_text())
    assert data["module"] == "PROCESS-PAYMENT-ELIGIBILITY"
    assert data["producer"] == "war_rig"
    assert data["round"] == 1
    assert data["business_rules"] == []
    assert data["open_questions"] == []
    assert data["branch_priority_hints"] == {"high": [], "low": []}
    assert data["imperator_signoff"] is False


def test_emit_open_questions_maps_native_business_rules(tmp_path: Path) -> None:
    """War Rig's BusinessRule shape (description, logic_summary, conditions) gets
    translated into SPEC's preconditions/postconditions."""
    out = tmp_path / "war_rig_out.json"
    emit_open_questions(
        out,
        module="M",
        round_num=2,
        war_rig_result={
            "final_template": {
                "business_rules": [
                    {
                        "rule_id": "BR001",
                        "description": "Monthly reports set date range to current month.",
                        "logic_summary": "Uses CURRENT-DATE to populate start/end.",
                        "conditions": ["MONTHLYI NOT = SPACES", "CURRENT-DATE available"],
                    }
                ],
                "open_questions": [],
                "dead_code": [],
            },
            "decision": "WITNESSED",
        },
        started_at="2026-04-21T10:00:00Z",
        completed_at="2026-04-21T10:05:00Z",
    )
    data = json.loads(out.read_text())
    rule = data["business_rules"][0]
    assert rule["rule_id"] == "BR001"
    assert "Monthly reports" in rule["description"]
    assert "MONTHLYI NOT = SPACES AND CURRENT-DATE" in rule["preconditions"]
    assert "CURRENT-DATE" in rule["postconditions"]
    assert data["imperator_signoff"] is True


def test_emit_open_questions_classifies_question_hints(tmp_path: Path) -> None:
    """Questions get classified into INTENT / DATA_SOURCE / BRANCH_COVERAGE based
    on their text, and COBOL field names get extracted into target_fields."""
    out = tmp_path / "war_rig_out.json"
    emit_open_questions(
        out,
        module="M",
        round_num=1,
        war_rig_result={
            "final_template": {
                "business_rules": [],
                "open_questions": [
                    {
                        "question": "Where is PROBATION-LIMIT sourced?",
                        "context": "The copybook is not present.",
                        "suggestion": "Check COPY statements in calling programs.",
                    },
                    {
                        "question": "What is the intent of paragraph 2000-VALIDATE?",
                        "context": "The logic is unclear.",
                        "suggestion": "Interview domain experts.",
                    },
                    {
                        "question": "Is the branch at LINE-42 reachable?",
                        "context": "Static analysis suggests unreachable.",
                        "suggestion": "Add coverage probe.",
                    },
                ],
                "dead_code": [],
            },
        },
        started_at="2026-04-21T10:00:00Z",
        completed_at="2026-04-21T10:05:00Z",
    )
    data = json.loads(out.read_text())
    qs = {q["question_id"]: q for q in data["open_questions"]}
    assert qs["Q1"]["hint"] == "DATA_SOURCE"
    assert qs["Q1"]["target_fields"] == ["PROBATION-LIMIT"]
    assert qs["Q2"]["hint"] == "INTENT"
    assert qs["Q3"]["hint"] == "BRANCH_COVERAGE"


def test_emit_open_questions_derives_branch_priority_hints(tmp_path: Path) -> None:
    """DATA_SOURCE questions with target fields surface as high-priority branches;
    dead_code items surface as low-priority."""
    out = tmp_path / "war_rig_out.json"
    emit_open_questions(
        out,
        module="M",
        round_num=1,
        war_rig_result={
            "final_template": {
                "business_rules": [],
                "open_questions": [
                    {
                        "question": "Where is PROBATION-LIMIT sourced?",
                        "context": "missing copybook",
                        "suggestion": "",
                    },
                ],
                "dead_code": [
                    {"name": "9000-LEGACY-BONUS", "artifact_type": "paragraph", "line": 88, "reason": "never called"},
                ],
            },
        },
        started_at="2026-04-21T10:00:00Z",
        completed_at="2026-04-21T10:05:00Z",
    )
    data = json.loads(out.read_text())
    assert data["branch_priority_hints"]["high"][0]["location"] == "PROBATION-LIMIT"
    assert data["branch_priority_hints"]["low"][0]["location"] == "9000-LEGACY-BONUS:88"


def test_emit_open_questions_handles_empty_result(tmp_path: Path) -> None:
    """Unexpected result shapes produce valid-but-empty documents, not crashes."""
    out = tmp_path / "war_rig_out.json"
    emit_open_questions(
        out,
        module="M",
        round_num=1,
        war_rig_result={"decision": "CHROME"},
        started_at="2026-04-21T10:00:00Z",
        completed_at="2026-04-21T10:05:00Z",
    )
    data = json.loads(out.read_text())
    assert data["business_rules"] == []
    assert data["open_questions"] == []
    assert data["branch_priority_hints"] == {"high": [], "low": []}


def test_emit_open_questions_uses_atomic_write(tmp_path: Path) -> None:
    """After emit, there should be no .tmp file left over in the parent dir."""
    out = tmp_path / "war_rig_out.json"
    emit_open_questions(
        out,
        module="M",
        round_num=1,
        war_rig_result=None,
        started_at="2026-04-21T10:00:00Z",
        completed_at="2026-04-21T10:05:00Z",
    )
    leftover = list(tmp_path.glob("*.tmp"))
    assert leftover == []


def test_wait_for_signal_times_out_quickly(tmp_path: Path) -> None:
    signal = tmp_path / "signal"
    start = time.time()
    result = wait_for_signal(signal, timeout_seconds=1)
    elapsed = time.time() - start
    assert result is False
    assert elapsed < 3.0


def test_wait_for_signal_returns_true_when_file_appears(tmp_path: Path) -> None:
    signal = tmp_path / "signal"

    def _drop() -> None:
        time.sleep(0.3)
        signal.write_text("ok")

    t = threading.Thread(target=_drop)
    t.start()
    try:
        result = wait_for_signal(signal, timeout_seconds=5)
    finally:
        t.join()
    assert result is True


def test_seed_questions_flow_into_initial_state() -> None:
    """Externally-authored ChallengerQuestions seeded via create_initial_state appear
    in both the cumulative and current-round lists so Scribe picks them up."""
    from war_rig.models.tickets import ChallengerQuestion, QuestionSeverity, QuestionType
    from war_rig.orchestration.state import create_initial_state

    seed = [
        ChallengerQuestion(
            question_id="DW-1",
            question="What does EIBCALEN=0 branch mean?",
            question_type=QuestionType.COMPLETENESS,
            severity=QuestionSeverity.IMPORTANT,
        ),
        ChallengerQuestion(
            question_id="DW-2",
            question="Is DFHENTER handling documented?",
            question_type=QuestionType.VERIFICATION,
            severity=QuestionSeverity.IMPORTANT,
        ),
    ]
    state = create_initial_state(
        source_code="IDENTIFICATION DIVISION.",
        file_name="M.cbl",
        seed_questions=seed,
    )
    assert len(state["challenger_questions"]) == 2
    assert len(state["current_round_questions"]) == 2
    assert state["current_round_questions"][0].question_id == "DW-1"


def test_create_initial_state_without_seed_questions_is_empty() -> None:
    """No seed_questions → empty dialogue lists, existing behavior preserved."""
    from war_rig.orchestration.state import create_initial_state

    state = create_initial_state(source_code="X.", file_name="M.cbl")
    assert state["challenger_questions"] == []
    assert state["current_round_questions"] == []


def test_create_residue_ticket_returns_string(tmp_path: Path) -> None:
    residue_path = tmp_path / "residue.json"
    residue_path.write_text(json.dumps({
        "gap_id": "G-42",
        "type": "ALIASED_INTENT",
        "severity": "HIGH",
        "description": "Aliased intent detected at LINE-99.",
        "recommended_action": "HUMAN_REVIEW",
    }))
    ticket_id = create_residue_ticket(residue_path)
    assert isinstance(ticket_id, str)
    assert ticket_id  # non-empty; either a real beads id or a synthetic fallback
