"""War Rig utility modules."""

from war_rig.utils.error_logging import (
    get_error_logger,
    log_error,
    setup_error_file_handler,
)

__all__ = ["log_error", "get_error_logger", "setup_error_file_handler"]
