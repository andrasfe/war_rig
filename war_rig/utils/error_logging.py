"""Error file logging utility for War Rig.

Provides centralized error logging to capture ALL ERROR level logs to a file.
Logs to file specified by ERR_FILE environment variable.

Usage:
    # At application startup (e.g., in cli.py):
    from war_rig.utils import setup_error_file_handler
    setup_error_file_handler()

    # Then any logger.error() call anywhere will be captured:
    logger.error("Something went wrong")  # -> written to ERR_FILE

    # For extra context (optional), use log_error():
    from war_rig.utils import log_error
    log_error(
        exception,
        ticket_id="mem-000001",
        request={"prompt": "..."},
        response={"content": "..."},
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

from dotenv import load_dotenv

# Ensure .env is loaded for ERR_FILE
load_dotenv()

# Track if handler is already set up
_handler_installed = False


def setup_error_file_handler() -> bool:
    """Set up a file handler to capture ALL ERROR level logs.

    Reads ERR_FILE from environment. If set, adds a file handler to the
    root 'war_rig' logger that captures all ERROR and above messages.

    Call this once at application startup.

    Returns:
        True if handler was set up, False if ERR_FILE not configured.
    """
    global _handler_installed

    if _handler_installed:
        return True

    err_file = os.environ.get("ERR_FILE")
    if not err_file:
        return False

    # Ensure directory exists
    err_path = Path(err_file)
    err_path.parent.mkdir(parents=True, exist_ok=True)

    # Create file handler for ERROR level
    file_handler = logging.FileHandler(err_file, mode="a", encoding="utf-8")
    file_handler.setLevel(logging.ERROR)

    # Detailed formatter with timestamp, logger name, and full message
    formatter = logging.Formatter(
        "%(asctime)s | %(levelname)s | %(name)s | %(message)s\n"
        "%(pathname)s:%(lineno)d\n"
    )
    file_handler.setFormatter(formatter)

    # Add to root logger to capture ALL errors from all war_rig modules
    root_logger = logging.getLogger()
    root_logger.addHandler(file_handler)

    _handler_installed = True

    # Log that error logging is enabled
    logging.getLogger("war_rig.utils").info(f"Error file logging enabled: {err_file}")

    return True


def get_error_logger() -> logging.Logger | None:
    """Get the error file logger (for backwards compatibility).

    Returns:
        Logger configured for error file output, or None if ERR_FILE not set.
    """
    err_file = os.environ.get("ERR_FILE")
    if not err_file:
        return None
    return logging.getLogger("war_rig.errors")


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

    This provides richer context than a simple logger.error() call.
    Use this when you have request/response data or ticket info to include.

    Args:
        exception: The exception that occurred, or an error message string.
        ticket_id: Associated ticket ID if applicable.
        request: Request data (prompt, messages, etc.).
        response: Response data from LLM or API.
        context: Additional context (worker_id, file_name, etc.).
        include_traceback: Whether to include full stack trace (default True).
    """
    # Ensure handler is set up
    if not setup_error_file_handler():
        return  # ERR_FILE not configured

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
    log_message = f"\n{separator}\nSTRUCTURED ERROR:\n{json.dumps(error_entry, indent=2, default=str)}\n{separator}"

    # Use a dedicated logger for structured errors
    logging.getLogger("war_rig.errors").error(log_message)


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
