"""War Rig ↔ Doof Wagon IPC bridge.

This module implements the SPEC-defined integration points:

- `load_priors(path)`  — load + schema-validate a priors document with consumer="war_rig".
- `emit_open_questions(path, result)` — write a schema-conformant open_questions.json
  derived from War Rig's internal analysis result, atomically (tmp-and-rename).
- `wait_for_signal(path, timeout)` — block until a signal file appears, with timeout.

This bridge is deliberately minimal: it does NOT change War Rig's existing analysis
behavior. When these functions are not invoked (i.e., when the new CLI flags are
absent), War Rig runs exactly as before.

Schema files live next to this module (copied from doof-wagon/schemas/). We do not
import from the doof-wagon package — each repo owns its own copy for packaging
independence.
"""

from __future__ import annotations

import json
import logging
import os
import tempfile
import time
from dataclasses import dataclass
from importlib.resources import files
from pathlib import Path
from typing import Any

from jsonschema import Draft202012Validator

logger = logging.getLogger(__name__)

_SCHEMAS_PACKAGE = "war_rig.doof_wagon.schemas"


def _load_schema(name: str) -> dict[str, Any]:
    """Load a schema from the packaged schema files."""
    resource = files(_SCHEMAS_PACKAGE) / f"{name}.schema.v1.json"
    return json.loads(resource.read_text(encoding="utf-8"))


def _validator(name: str) -> Draft202012Validator:
    schema = _load_schema(name)
    Draft202012Validator.check_schema(schema)
    return Draft202012Validator(schema)


def _atomic_write_json(path: Path, data: Any) -> None:
    """Write JSON atomically (tmp-and-rename) so readers never see partial writes."""
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    fd, tmp_name = tempfile.mkstemp(
        dir=str(path.parent),
        prefix=f".{path.name}.",
        suffix=".tmp",
    )
    try:
        with os.fdopen(fd, "w") as f:
            json.dump(data, f, indent=2, sort_keys=False)
            f.flush()
            os.fsync(f.fileno())
        os.replace(tmp_name, path)
    except Exception:
        try:
            os.unlink(tmp_name)
        except OSError:
            pass
        raise


@dataclass
class DoofWagonOptions:
    """CLI options that activate Doof Wagon integration.

    When `output_path` is None, War Rig behaves normally and the other options
    are ignored.
    """

    output_path: Path | None = None
    priors_path: Path | None = None
    await_signal: Path | None = None
    signal_timeout_seconds: int = 3600
    round_num: int = 1

    @property
    def active(self) -> bool:
        return self.output_path is not None


def load_priors(path: Path) -> dict[str, Any]:
    """Load and schema-validate a priors document. The `consumer` field must be
    `"war_rig"` — priors intended for Specter are rejected here to prevent
    accidental cross-tool misrouting.
    """
    path = Path(path)
    if not path.exists():
        raise FileNotFoundError(f"priors not found: {path}")
    with path.open() as f:
        data = json.load(f)
    _validator("priors").validate(data)
    consumer = data.get("consumer")
    if consumer != "war_rig":
        raise ValueError(
            f"priors at {path} target consumer={consumer!r}; "
            "expected 'war_rig'. Did the orchestrator cross wires?"
        )
    return data


_DATA_SOURCE_KEYWORDS = (
    "source", "copybook", "copy book", "where is", "sourced", "origin",
    "defined", "declaration", "declared", "data source", "file", "dataset",
    "DB2", "VSAM", "table",
)
_BRANCH_COVERAGE_KEYWORDS = (
    "branch", "path", "if ", "evaluate", "coverage", "condition", "flag",
    "status code", "dead code", "unreachable",
)


def _classify_question_hint(question: str | None, context: str | None, suggestion: str | None) -> str:
    """Best-effort classification of a War Rig open_question into the SPEC's hint enum."""
    blob = " ".join(x.lower() for x in (question or "", context or "", suggestion or ""))
    if any(k in blob for k in _DATA_SOURCE_KEYWORDS):
        return "DATA_SOURCE"
    if any(k in blob for k in _BRANCH_COVERAGE_KEYWORDS):
        return "BRANCH_COVERAGE"
    return "INTENT"


def _extract_target_fields(question: str | None, context: str | None, suggestion: str | None) -> list[str]:
    """Harvest plausible COBOL field names from a War Rig question's prose.

    COBOL convention: dashed uppercase identifiers of 3+ chars (PROBATION-LIMIT,
    WS-MESSAGE). We pull those out of the question + context + suggestion text
    so Specter's fields_of_interest can carry concrete referents.
    """
    import re

    blob = " ".join(x for x in (question, context, suggestion) if x)
    # COBOL identifier: at least one dash, uppercase letters/digits, 4+ chars total.
    candidates = re.findall(r"\b[A-Z][A-Z0-9]*(?:-[A-Z0-9]+)+\b", blob)
    seen: list[str] = []
    for c in candidates:
        if c not in seen and len(c) >= 4:
            seen.append(c)
    return seen[:8]  # cap to keep fields_of_interest manageable


def _as_dict(value: Any) -> dict[str, Any]:
    """Coerce a Pydantic model or dict-like into a plain dict."""
    if value is None:
        return {}
    if hasattr(value, "model_dump"):
        return value.model_dump()
    if isinstance(value, dict):
        return value
    return {}


def _iter_dicts(items: Any) -> list[dict[str, Any]]:
    if not items:
        return []
    return [_as_dict(x) for x in items]


def _map_business_rule(br: dict[str, Any], index: int) -> dict[str, Any]:
    """Map War Rig's BusinessRule shape → SPEC business_rules entry.

    War Rig carries `conditions` (list[str]) + `logic_summary`. The SPEC asks
    for `preconditions` + `postconditions`. We use the conditions list as the
    precondition text (joined with AND) and the logic_summary as the postcondition
    approximation. Both are free-form strings in the SPEC, so this is a best-effort
    mapping — a reviewer reading the doc will still get meaningful text.
    """
    rule_id = br.get("rule_id") or f"R{index + 1}"
    description = br.get("description") or ""
    conditions = br.get("conditions") or []
    if isinstance(conditions, str):
        conditions = [conditions]
    preconditions = " AND ".join(c for c in conditions if c) if conditions else ""
    postconditions = br.get("logic_summary") or ""
    return {
        "rule_id": rule_id,
        "description": description,
        "preconditions": preconditions,
        "postconditions": postconditions,
    }


def _map_open_question(oq: dict[str, Any], index: int) -> dict[str, Any]:
    question = oq.get("question") or ""
    context = oq.get("context") or ""
    suggestion = oq.get("suggestion") or ""
    entry = {
        "question_id": oq.get("question_id") or f"Q{index + 1}",
        "question": question,
        "hint": _classify_question_hint(question, context, suggestion),
    }
    fields = _extract_target_fields(question, context, suggestion)
    if fields:
        entry["target_fields"] = fields
    return entry


def _map_dead_code(item: dict[str, Any]) -> dict[str, Any]:
    name = item.get("name") or ""
    line = item.get("line")
    location = f"{name}:{line}" if name and line else name or f"line-{line}" if line else "unknown"
    # War Rig doesn't currently emit a confidence; default to MEDIUM.
    return {"location": location, "confidence": "MEDIUM"}


_QUOTED_LITERAL_RE = __import__("re").compile(r"'([^']{1,16})'")
_COBOL_FIELD_RE = __import__("re").compile(r"\b[A-Z][A-Z0-9]*(?:-[A-Z0-9]+){1,}\b")

# Field-name substrings that signal "this is a status / response / code field
# whose literal domain we want Specter to know about". Matches WS-XFILE-STATUS,
# WS-STATUS-CODE, DFHRESP, SQLCODE-style names.
_STATUS_FIELD_SUFFIXES = ("STATUS", "CODE", "RESP", "-RESP", "SQLCODE")


def _extract_paragraph_literals(paragraphs: list[dict[str, Any]]) -> list[dict[str, Any]]:
    """Harvest quoted literals + associated status-field names per paragraph.

    This is the cross-paragraph signal Specter can't recover from the AST:
    War Rig's prose describes "file status '23' means record not found" and
    names the field holding that status ("WS-XREFFILE-STATUS"). Surfacing the
    (field, literal) pair lets Specter seed a test case that forces that
    specific status into the branch guard.

    Returns a list of `{paragraph, literals, associated_fields}` dicts, one per
    paragraph that mentioned at least one literal. Fields are kept only when
    they look like status/response/code fields (see `_STATUS_FIELD_SUFFIXES`).
    """
    out: list[dict[str, Any]] = []
    for para in paragraphs or []:
        para = _as_dict(para)
        name = para.get("paragraph_name") or para.get("name") or ""
        prose = " ".join(
            str(para.get(k) or "") for k in ("summary", "purpose", "logic", "description")
        )
        if not prose:
            continue

        literals = sorted({
            m for m in _QUOTED_LITERAL_RE.findall(prose)
            if m and not m.isspace()
        })
        if not literals:
            continue

        candidate_fields = _COBOL_FIELD_RE.findall(prose)
        # Keep only status/response/code-style field names.
        status_fields = sorted({
            f for f in candidate_fields
            if any(suf in f.upper() for suf in _STATUS_FIELD_SUFFIXES)
        })

        entry: dict[str, Any] = {
            "paragraph": name or "UNKNOWN",
            "literals": literals,
        }
        if status_fields:
            entry["associated_fields"] = status_fields
        out.append(entry)
    return out


def _extract_final_template(result: Any) -> dict[str, Any]:
    """Pull the DocumentationTemplate payload out of War Rig's graph state.

    The graph result may be a plain dict (e.g. from `--mock`), a TypedDict-like
    state, or a Pydantic model. We accept all three and return an empty dict
    on shape mismatch so the bridge never crashes on unexpected state shapes —
    at worst we emit a structurally-valid but empty open_questions document.
    """
    if result is None:
        return {}
    # Pydantic model with .final_template attr.
    tmpl = getattr(result, "final_template", None)
    if tmpl is None and isinstance(result, dict):
        tmpl = result.get("final_template")
    if tmpl is None and isinstance(result, dict):
        # Some graph states may inline rule/question fields at the top level.
        if any(k in result for k in ("business_rules", "open_questions", "dead_code")):
            return result
    return _as_dict(tmpl)


def emit_open_questions(
    output_path: Path,
    *,
    module: str,
    round_num: int,
    war_rig_result: Any,
    war_rig_version: str = "0.1.0",
    started_at: str,
    completed_at: str,
) -> None:
    """Map a War Rig graph result into a SPEC-conformant open_questions.v1 document.

    Reads `final_template` (a DocumentationTemplate) from the graph state and
    translates its `business_rules`, `open_questions`, and `dead_code` into the
    SPEC's schema shapes. Best-effort mapping — see helper functions for details.

    When `final_template` is missing or empty, produces a structurally-valid
    document with empty collections so downstream skills (witness, translate-*)
    don't fail on schema validation. Nothing in War Rig's core pipeline depends
    on this function — it runs only when `--doof-wagon-output` is set.
    """
    template = _extract_final_template(war_rig_result)

    native_rules = _iter_dicts(template.get("business_rules"))
    native_qs = _iter_dicts(template.get("open_questions"))
    native_dead = _iter_dicts(template.get("dead_code"))
    native_paragraphs = _iter_dicts(template.get("paragraphs"))

    business_rules = [_map_business_rule(br, i) for i, br in enumerate(native_rules)]
    open_questions = [_map_open_question(oq, i) for i, oq in enumerate(native_qs)]
    dead_code_candidates = [_map_dead_code(d) for d in native_dead]
    paragraph_literals = _extract_paragraph_literals(native_paragraphs)

    # Derive branch_priority_hints from open_questions + dead_code.
    # - Questions hinting at DATA_SOURCE with target fields → high-priority
    #   locations (Specter should preserve their concrete values).
    # - Dead code candidates → low-priority (Specter should deprioritize them).
    high = []
    for q in open_questions:
        if q.get("hint") == "DATA_SOURCE" and q.get("target_fields"):
            for f in q["target_fields"][:3]:
                high.append({"location": f, "reason": q["question"][:120]})
    low = []
    for d in dead_code_candidates:
        low.append({"location": d["location"], "reason": "Flagged as dead code by War Rig"})
    branch_priority_hints = {"high": high, "low": low}

    imperator_signoff = False
    if isinstance(war_rig_result, dict):
        imperator_signoff = bool(war_rig_result.get("decision") == "WITNESSED")

    document = {
        "schema_version": "1.0",
        "module": module,
        "round": round_num,
        "producer": "war_rig",
        "business_rules": business_rules,
        "open_questions": open_questions,
        "branch_priority_hints": branch_priority_hints,
        "dead_code_candidates": dead_code_candidates,
        "imperator_signoff": imperator_signoff,
        "round_metadata": {
            "started_at": started_at,
            "completed_at": completed_at,
            "war_rig_version": war_rig_version,
        },
    }

    if paragraph_literals:
        document["paragraph_literals"] = paragraph_literals

    _validator("open_questions").validate(document)
    _atomic_write_json(Path(output_path), document)
    logger.info(
        "doof_wagon: emitted open_questions at %s (rules=%d, questions=%d, dead=%d, para_literals=%d)",
        output_path, len(business_rules), len(open_questions), len(dead_code_candidates),
        len(paragraph_literals),
    )


def wait_for_signal(signal_path: Path, timeout_seconds: int) -> bool:
    """Block until `signal_path` exists or `timeout_seconds` elapses.

    Returns True if the signal arrived, False on timeout. Callers should treat
    timeout as a normal condition — the SPEC instructs orchestrators to mark
    the round `WAITING_TIMEOUT` and exit cleanly rather than killing this process.
    """
    signal_path = Path(signal_path)
    deadline = time.time() + max(0, int(timeout_seconds))
    while time.time() < deadline:
        if signal_path.exists():
            return True
        time.sleep(1.0)
    return False


def create_residue_ticket(residue_path: Path) -> str:
    """Create a war_rig ticket for a synthesis residue and return the ticket_id.

    `residue_path` points to a JSON file matching the synthesis residue shape:
    `{gap_id, type, severity, description, recommended_action}`.

    The hook wraps War Rig's existing beads ticketing system. When the beads
    backend is unavailable or disabled, falls back to a synthesized ticket_id
    of the form `doof-wagon-residue-<gap_id>` so the caller always has something
    to record in synthesis.json.
    """
    from war_rig.beads import BeadsPriority, BeadsTicketType, get_beads_client

    residue_path = Path(residue_path)
    residue = json.loads(residue_path.read_text())
    gap_id = residue.get("gap_id", "unknown")
    severity = residue.get("severity", "MEDIUM")
    description = residue.get("description", "")
    title = f"[doof-wagon] Residue {gap_id} ({severity}): {description[:80]}"

    priority_map = {
        "HIGH": BeadsPriority.HIGH,
        "MEDIUM": BeadsPriority.MEDIUM,
        "LOW": BeadsPriority.LOW,
    }
    priority = priority_map.get(severity, BeadsPriority.MEDIUM)

    try:
        client = get_beads_client(enabled=True)
        ticket_id = client.create_ticket(
            title=title,
            ticket_type=BeadsTicketType.TASK,
            priority=priority,
            labels=["doof-wagon", "residue", gap_id],
        )
        if ticket_id:
            return ticket_id
    except Exception as e:  # noqa: BLE001
        logger.warning("doof_wagon: beads ticket creation failed (%s); using synthetic id", e)

    return f"doof-wagon-residue-{gap_id}"
