"""Error file logging utility for War Rig.

Provides centralized error logging with full stack traces, request/response data,
and ticket information. Logs to file specified by ERR_FILE environment variable.

Usage:
    from war_rig.utils import log_error

    try:
        # some operation
    except Exception as e:
        log_error(
            e,
            ticket_id="mem-000001",
            request={"prompt": "..."},
            response={"content": "..."},
            context={"worker_id": "scribe-1"},
        )
"""

from __future__ import annotations

import json
import logging
import os
import traceback
from datetime import datetime
from pathlib import Path
from typing import Any

# Module-level error logger
_error_logger: logging.Logger | None = None
_error_file_path: str | None = None


def get_error_logger() -> logging.Logger | None:
    """Get or create the error file logger.

    Reads ERR_FILE from environment. If not set, returns None (no file logging).

    Returns:
        Logger configured for error file output, or None if ERR_FILE not set.
    """
    global _error_logger, _error_file_path

    err_file = os.environ.get("ERR_FILE")
    if not err_file:
        return None

    # Check if we need to create/update the logger
    if _error_logger is None or _error_file_path != err_file:
        _error_file_path = err_file

        # Ensure directory exists
        err_path = Path(err_file)
        err_path.parent.mkdir(parents=True, exist_ok=True)

        # Create logger
        _error_logger = logging.getLogger("war_rig.errors")
        _error_logger.setLevel(logging.ERROR)

        # Remove existing handlers to avoid duplicates
        _error_logger.handlers.clear()

        # Create file handler
        file_handler = logging.FileHandler(err_file, mode="a", encoding="utf-8")
        file_handler.setLevel(logging.ERROR)

        # Create formatter - simple format, we'll add structured data in log_error
        formatter = logging.Formatter("%(message)s")
        file_handler.setFormatter(formatter)

        _error_logger.addHandler(file_handler)

        # Don't propagate to root logger
        _error_logger.propagate = False

    return _error_logger


def log_error(
    exception: Exception | str,
    *,
    ticket_id: str | None = None,
    request: dict[str, Any] | str | None = None,
    response: dict[str, Any] | str | None = None,
    context: dict[str, Any] | None = None,
    include_traceback: bool = True,
) -> None:
    """Log an error to the error file with full context.

    Args:
        exception: The exception that occurred, or an error message string.
        ticket_id: Associated ticket ID if applicable.
        request: Request data (prompt, messages, etc.).
        response: Response data from LLM or API.
        context: Additional context (worker_id, file_name, etc.).
        include_traceback: Whether to include full stack trace (default True).
    """
    logger = get_error_logger()
    if logger is None:
        return  # ERR_FILE not configured, skip file logging

    # Build structured error entry
    timestamp = datetime.now().isoformat()
    error_entry: dict[str, Any] = {
        "timestamp": timestamp,
        "error_type": type(exception).__name__ if isinstance(exception, Exception) else "Error",
        "error_message": str(exception),
    }

    if ticket_id:
        error_entry["ticket_id"] = ticket_id

    if context:
        error_entry["context"] = context

    if include_traceback and isinstance(exception, Exception):
        error_entry["traceback"] = traceback.format_exc()

    if request is not None:
        # Truncate very long requests
        if isinstance(request, dict):
            error_entry["request"] = _truncate_data(request)
        else:
            error_entry["request"] = _truncate_string(str(request))

    if response is not None:
        # Truncate very long responses
        if isinstance(response, dict):
            error_entry["response"] = _truncate_data(response)
        else:
            error_entry["response"] = _truncate_string(str(response))

    # Format as JSON for easy parsing
    separator = "=" * 80
    log_message = f"\n{separator}\n{json.dumps(error_entry, indent=2, default=str)}\n{separator}"

    logger.error(log_message)


def _truncate_string(s: str, max_length: int = 5000) -> str:
    """Truncate a string if it exceeds max_length."""
    if len(s) <= max_length:
        return s
    return s[:max_length] + f"... [truncated, total length: {len(s)}]"


def _truncate_data(data: dict[str, Any], max_length: int = 5000) -> dict[str, Any]:
    """Truncate string values in a dict that exceed max_length."""
    result = {}
    for key, value in data.items():
        if isinstance(value, str) and len(value) > max_length:
            result[key] = _truncate_string(value, max_length)
        elif isinstance(value, dict):
            result[key] = _truncate_data(value, max_length)
        elif isinstance(value, list):
            result[key] = [
                _truncate_data(v, max_length) if isinstance(v, dict)
                else _truncate_string(v, max_length) if isinstance(v, str) and len(v) > max_length
                else v
                for v in value
            ]
        else:
            result[key] = value
    return result
