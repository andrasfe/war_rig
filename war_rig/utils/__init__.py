"""War Rig utility modules."""

from war_rig.utils.error_logging import (
    get_error_logger,
    log_error,
    setup_error_file_handler,
)
from war_rig.utils.exceptions import FatalWorkerError
from war_rig.utils.file_lock import FileLockManager, LockInfo

__all__ = [
    "log_error",
    "get_error_logger",
    "setup_error_file_handler",
    "FatalWorkerError",
    "FileLockManager",
    "LockInfo",
]
