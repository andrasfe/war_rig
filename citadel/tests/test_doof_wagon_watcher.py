"""Tests for citadel.doof_wagon.watcher.

Exercises:
- Schema validation of incoming queries.
- Stub resolver writes a structurally-valid PENDING callback.
- Processed queries are moved to `_processed/` and not reprocessed.
- Malformed queries get quarantined in `_failed/`.
- The `once` vs `watch_until` behaviors.
"""

from __future__ import annotations

import json
from datetime import datetime, timezone
from pathlib import Path

import pytest

from citadel.doof_wagon.watcher import Watcher, pending_resolver, process_query_file


def _valid_query(tmp_path: Path, query_id: str = "q-1", target: str = "PROBATION-LIMIT") -> dict:
    return {
        "schema_version": "1.0",
        "query_id": query_id,
        "originating_module": "PROCESS-PAYMENT-ELIGIBILITY",
        "query_type": "FIELD_SOURCE",
        "target": target,
        "callback_path": str(tmp_path / "callbacks" / f"{query_id}.json"),
        "created_at": datetime.now(timezone.utc).isoformat(),
    }


def test_pending_resolver_preserves_core_fields(tmp_path: Path) -> None:
    query = _valid_query(tmp_path)
    result = pending_resolver(query)
    assert result["query_id"] == query["query_id"]
    assert result["target"] == query["target"]
    assert result["status"] == "PENDING"
    assert "notes" in result


def test_process_query_file_writes_callback(tmp_path: Path) -> None:
    input_dir = tmp_path / "in"
    input_dir.mkdir()
    processed_dir = tmp_path / "processed"
    query = _valid_query(tmp_path)
    query_path = input_dir / "q-1.json"
    query_path.write_text(json.dumps(query))

    callback = process_query_file(query_path, pending_resolver, processed_dir)
    assert callback.exists()
    result = json.loads(callback.read_text())
    assert result["query_id"] == "q-1"
    assert result["status"] == "PENDING"


def test_process_query_file_moves_to_processed(tmp_path: Path) -> None:
    input_dir = tmp_path / "in"
    input_dir.mkdir()
    processed_dir = tmp_path / "processed"
    query = _valid_query(tmp_path, query_id="q-abc")
    query_path = input_dir / "q-abc.json"
    query_path.write_text(json.dumps(query))

    process_query_file(query_path, pending_resolver, processed_dir)
    assert not query_path.exists()
    assert (processed_dir / "q-abc.json").exists()


def test_watcher_once_processes_all_pending(tmp_path: Path) -> None:
    input_dir = tmp_path / "in"
    input_dir.mkdir()

    for i in range(3):
        query = _valid_query(tmp_path, query_id=f"q-{i}")
        (input_dir / f"q-{i}.json").write_text(json.dumps(query))

    watcher = Watcher(input_dir=input_dir)
    processed = watcher.process_once()
    assert processed == 3
    # Pending dir should now be empty (aside from _processed and callbacks).
    remaining = [
        p for p in input_dir.iterdir()
        if p.is_file() and p.suffix == ".json"
    ]
    assert remaining == []


def test_watcher_quarantines_malformed_queries(tmp_path: Path) -> None:
    input_dir = tmp_path / "in"
    input_dir.mkdir()

    # Missing required fields.
    (input_dir / "bad.json").write_text(json.dumps({"schema_version": "1.0"}))

    watcher = Watcher(input_dir=input_dir)
    processed = watcher.process_once()
    assert processed == 0
    assert (input_dir / "_failed" / "bad.json").exists()


def test_watcher_validates_callback_path_is_set(tmp_path: Path) -> None:
    input_dir = tmp_path / "in"
    input_dir.mkdir()
    query = _valid_query(tmp_path)
    (input_dir / "q.json").write_text(json.dumps(query))

    watcher = Watcher(input_dir=input_dir)
    watcher.process_once()
    callback = Path(query["callback_path"])
    assert callback.exists()


def test_watcher_handles_empty_dir(tmp_path: Path) -> None:
    input_dir = tmp_path / "in"
    input_dir.mkdir()
    watcher = Watcher(input_dir=input_dir)
    assert watcher.process_once() == 0


def test_watcher_watch_until_exits_on_deadline(tmp_path: Path) -> None:
    import time
    input_dir = tmp_path / "in"
    input_dir.mkdir()
    watcher = Watcher(input_dir=input_dir, poll_interval_seconds=0.1)
    start = time.time()
    watcher.watch_until(deadline_seconds=0.3)
    elapsed = time.time() - start
    assert elapsed < 2.0  # should exit promptly after deadline


def test_custom_resolver_wins_over_stub(tmp_path: Path) -> None:
    input_dir = tmp_path / "in"
    input_dir.mkdir()
    processed_dir = tmp_path / "processed"

    def _resolver(query: dict) -> dict:
        return {
            "query_id": query["query_id"],
            "originating_module": query["originating_module"],
            "query_type": query["query_type"],
            "target": query["target"],
            "status": "RESOLVED",
            "source_module": "LIMITS-CPY",
            "source_location": "LIMITS-CPY:LINE-5",
        }

    query = _valid_query(tmp_path)
    query_path = input_dir / "q.json"
    query_path.write_text(json.dumps(query))

    callback = process_query_file(query_path, _resolver, processed_dir)
    result = json.loads(callback.read_text())
    assert result["status"] == "RESOLVED"
    assert result["source_module"] == "LIMITS-CPY"
