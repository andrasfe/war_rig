"""Citadel watcher for Doof Wagon async queries.

The watcher polls a directory for `citadel_query.schema.v1.json` files, validates
each one, dispatches to a resolver, and writes the result to the callback path
specified in the query. Queries are moved to an `_processed/` subdirectory after
handling so the watcher never reprocesses them.

Real resolution (call-graph traversal, copybook propagation) is deferred. The
stub resolver writes a result with `status: "PENDING"` and a short note so
downstream `citadel_resolutions` consumers don't block on Citadel indefinitely.
"""

from __future__ import annotations

import json
import logging
import os
import tempfile
import time
from dataclasses import dataclass, field
from datetime import datetime, timezone
from importlib.resources import files
from pathlib import Path
from typing import Any, Callable

from jsonschema import Draft202012Validator

logger = logging.getLogger(__name__)

_SCHEMAS_PACKAGE = "citadel.doof_wagon.schemas"


def _load_schema(name: str) -> dict[str, Any]:
    resource = files(_SCHEMAS_PACKAGE) / f"{name}.schema.v1.json"
    return json.loads(resource.read_text(encoding="utf-8"))


def _validator(name: str) -> Draft202012Validator:
    schema = _load_schema(name)
    Draft202012Validator.check_schema(schema)
    return Draft202012Validator(schema)


def _atomic_write_json(path: Path, data: Any) -> None:
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


Resolver = Callable[[dict[str, Any]], dict[str, Any]]


def pending_resolver(query: dict[str, Any]) -> dict[str, Any]:
    """Stub resolver. Returns a structurally-valid PENDING result.

    The callback result shape mirrors the fields a real resolver would produce,
    but with `status: "PENDING"` so that orchestrators can continue without
    blocking. When the real resolver lands, its signature is identical.
    """
    return {
        "query_id": query["query_id"],
        "originating_module": query["originating_module"],
        "query_type": query["query_type"],
        "target": query["target"],
        "status": "PENDING",
        "notes": (
            "Citadel's real resolver is not yet wired in this version. "
            "The query was received, validated, and acknowledged; no graph "
            "traversal has occurred. Re-dispatch later, or switch to a "
            "real resolver."
        ),
        "resolved_at": datetime.now(timezone.utc).isoformat(),
    }


@dataclass
class Watcher:
    """Polling-based directory watcher for citadel_query files."""

    input_dir: Path
    resolver: Resolver = field(default=pending_resolver)
    processed_subdir: str = "_processed"
    poll_interval_seconds: float = 1.0
    _validator_cache: Draft202012Validator | None = None

    def __post_init__(self) -> None:
        self.input_dir = Path(self.input_dir)
        self._validator_cache = _validator("citadel_query")

    @property
    def processed_dir(self) -> Path:
        return self.input_dir / self.processed_subdir

    def _pending_files(self) -> list[Path]:
        if not self.input_dir.exists():
            return []
        return sorted(
            p for p in self.input_dir.iterdir()
            if p.is_file() and p.suffix == ".json" and not p.name.startswith(".")
        )

    def process_once(self) -> int:
        """Process every pending query in the input directory. Returns count processed."""
        processed = 0
        for path in self._pending_files():
            try:
                process_query_file(path, self.resolver, self.processed_dir)
                processed += 1
            except Exception as e:  # noqa: BLE001
                logger.error("doof_wagon[citadel]: failed to process %s: %s", path, e)
                # Move failed queries out of the input dir so we don't spin on them.
                failed_dir = self.input_dir / "_failed"
                failed_dir.mkdir(exist_ok=True)
                try:
                    path.rename(failed_dir / path.name)
                except OSError:
                    pass
        return processed

    def watch_until(self, deadline_seconds: float) -> int:
        """Poll-process until `deadline_seconds` from now. Returns count processed."""
        deadline = time.time() + max(0.0, float(deadline_seconds))
        total = 0
        while time.time() < deadline:
            total += self.process_once()
            time.sleep(self.poll_interval_seconds)
        return total


def process_query_file(
    query_path: Path,
    resolver: Resolver,
    processed_dir: Path,
) -> Path:
    """Validate one query file, invoke the resolver, write the callback, move the query.

    Returns the path to the written callback file.
    """
    query_path = Path(query_path)
    processed_dir = Path(processed_dir)
    processed_dir.mkdir(parents=True, exist_ok=True)

    with query_path.open() as f:
        query = json.load(f)

    _validator("citadel_query").validate(query)
    result = resolver(query)

    callback_path = Path(query["callback_path"])
    _atomic_write_json(callback_path, result)
    logger.info(
        "doof_wagon[citadel]: resolved query %s → %s (status=%s)",
        query["query_id"],
        callback_path,
        result.get("status"),
    )

    try:
        query_path.rename(processed_dir / query_path.name)
    except OSError as e:
        logger.warning("doof_wagon[citadel]: could not move query to processed dir: %s", e)

    return callback_path
