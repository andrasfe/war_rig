"""File lock manager for concurrent Scribe and Challenger workers.

This module provides a centralized file locking mechanism to prevent race
conditions when multiple workers attempt to process the same output file
simultaneously.

Ticket: war_rig-qxw9

Features:
    - Coroutine-level synchronization using asyncio.Lock (NOT thread-safe)
    - Automatic lock expiration for crash recovery
    - Workers can check if a file is locked before claiming a ticket
    - Locks are identified by file path (normalized to absolute paths)

Important:
    This manager provides synchronization for coroutines running in the same
    asyncio event loop. It is NOT thread-safe. All methods must be called
    from an async context within the same event loop.

Example:
    from war_rig.utils.file_lock import FileLockManager

    lock_manager = FileLockManager(lock_timeout=300.0)

    # Worker attempts to acquire lock
    if await lock_manager.acquire("output/PROG.doc.json", "scribe-1"):
        try:
            # Process the file
            ...
        finally:
            await lock_manager.release("output/PROG.doc.json", "scribe-1")

See Also:
    - war_rig.workers.scribe_pool: ScribeWorkerPool integration
    - war_rig.workers.challenger_pool: ChallengerWorkerPool integration
    - war_rig.orchestration.ticket_engine: FileLockManager instantiation
"""

from __future__ import annotations

import asyncio
import logging
import time
from dataclasses import dataclass
from pathlib import Path

logger = logging.getLogger(__name__)


@dataclass
class LockInfo:
    """Information about a held lock.

    Attributes:
        worker_id: ID of the worker holding the lock.
        acquired_at: Unix timestamp when the lock was acquired.
        file_path: Normalized path to the locked file.
    """

    worker_id: str
    acquired_at: float
    file_path: str


class FileLockManager:
    """File lock manager for concurrent asyncio workers.

    Manages locks on output files to prevent multiple workers from
    simultaneously processing tickets that write to the same file.
    Locks are stored in memory and automatically expire after a
    configurable timeout (for crash recovery).

    The lock manager uses asyncio.Lock for coroutine-level synchronization,
    ensuring atomic acquire/release operations even with concurrent coroutines.
    This is NOT thread-safe - all operations must occur in the same event loop.

    Attributes:
        lock_timeout: Seconds after which a lock automatically expires.

    Example:
        lock_manager = FileLockManager(lock_timeout=300.0)

        # Acquire lock before processing
        acquired = await lock_manager.acquire("/path/to/output.json", "worker-1")
        if not acquired:
            # Another worker has the lock, skip this file
            return

        try:
            # Do processing...
            pass
        finally:
            await lock_manager.release("/path/to/output.json", "worker-1")
    """

    def __init__(self, lock_timeout: float = 300.0):
        """Initialize the FileLockManager.

        Args:
            lock_timeout: Seconds after which locks automatically expire.
                Defaults to 300.0 (5 minutes). This handles crash recovery
                by allowing other workers to acquire locks on files that
                were being processed by a crashed worker.
        """
        # Maps normalized file path -> LockInfo
        self._locks: dict[str, LockInfo] = {}
        # Mutex for thread-safe access to _locks
        self._lock = asyncio.Lock()
        self._timeout = lock_timeout

    async def acquire(self, file_path: str, worker_id: str) -> bool:
        """Try to acquire a lock on a file.

        Atomically checks if the file is available and acquires the lock
        if so. If the file is already locked by another worker, returns
        False immediately (no blocking).

        If the same worker already holds the lock, the lock is refreshed
        (timestamp updated) and True is returned.

        Args:
            file_path: Path to the file to lock. Will be normalized to
                an absolute path for consistent comparison.
            worker_id: Unique identifier for the worker requesting the lock.

        Returns:
            True if the lock was acquired (or refreshed), False if another
            worker holds the lock.

        Example:
            if await lock_manager.acquire("output/PROG.doc.json", "scribe-1"):
                print("Lock acquired!")
            else:
                print("File is locked by another worker")
        """
        async with self._lock:
            # Clean up expired locks first
            self._cleanup_expired()

            # Normalize path for consistent comparison
            normalized = str(Path(file_path).resolve())

            if normalized in self._locks:
                existing = self._locks[normalized]
                if existing.worker_id != worker_id:
                    # Another worker holds the lock
                    logger.debug(
                        f"Lock denied for {worker_id} on {normalized}: "
                        f"held by {existing.worker_id}"
                    )
                    return False
                # Same worker - refresh the lock
                logger.debug(f"Lock refreshed for {worker_id} on {normalized}")

            # Acquire or refresh the lock
            self._locks[normalized] = LockInfo(
                worker_id=worker_id,
                acquired_at=time.time(),
                file_path=normalized,
            )
            logger.debug(f"Lock acquired by {worker_id} on {normalized}")
            return True

    async def release(self, file_path: str, worker_id: str) -> bool:
        """Release a lock on a file.

        Only releases the lock if it's held by the specified worker.
        If the lock is held by a different worker or doesn't exist,
        the operation is a no-op.

        Args:
            file_path: Path to the file to unlock.
            worker_id: ID of the worker releasing the lock.

        Returns:
            True if the lock was released, False if the lock was not
            held by this worker (or didn't exist).

        Example:
            released = await lock_manager.release("output/PROG.doc.json", "scribe-1")
            if released:
                print("Lock released successfully")
        """
        async with self._lock:
            normalized = str(Path(file_path).resolve())

            if normalized not in self._locks:
                logger.debug(
                    f"Release no-op for {worker_id} on {normalized}: not locked"
                )
                return False

            existing = self._locks[normalized]
            if existing.worker_id != worker_id:
                logger.warning(
                    f"Release denied for {worker_id} on {normalized}: "
                    f"held by {existing.worker_id}"
                )
                return False

            del self._locks[normalized]
            logger.debug(f"Lock released by {worker_id} on {normalized}")
            return True

    async def is_locked(self, file_path: str) -> bool:
        """Check if a file is currently locked.

        This is a non-blocking check that returns the current lock state.
        Note: The state may change immediately after this call returns.

        Args:
            file_path: Path to check.

        Returns:
            True if the file is locked (by any worker), False otherwise.
        """
        async with self._lock:
            self._cleanup_expired()
            normalized = str(Path(file_path).resolve())
            return normalized in self._locks

    async def is_locked_by(self, file_path: str, worker_id: str) -> bool:
        """Check if a file is locked by a specific worker.

        Args:
            file_path: Path to check.
            worker_id: Worker ID to check.

        Returns:
            True if the file is locked by the specified worker.
        """
        async with self._lock:
            self._cleanup_expired()
            normalized = str(Path(file_path).resolve())
            if normalized not in self._locks:
                return False
            return self._locks[normalized].worker_id == worker_id

    async def is_locked_by_other(self, file_path: str, worker_id: str) -> bool:
        """Check if a file is locked by a different worker.

        This method allows a worker to check if a file is locked by someone
        else BEFORE attempting to claim a ticket. This implements the
        "lock-before-claim" pattern to reduce wasteful ticket churn.

        Note: The state may change immediately after this call returns.
        This is an optimization check, not an atomic lock acquisition.
        The actual lock must still be acquired in _process_ticket().

        Must be called from an async context within the same event loop
        as all other FileLockManager operations.

        Args:
            file_path: Path to the file to check.
            worker_id: The worker asking (returns False if this worker holds the lock).

        Returns:
            True if the file is locked by another worker, False if unlocked
            or if locked by the calling worker.
        """
        async with self._lock:
            self._cleanup_expired()
            normalized = str(Path(file_path).resolve())

            if normalized not in self._locks:
                # File is not locked
                return False

            # File is locked - check if it's by someone else
            return self._locks[normalized].worker_id != worker_id

    async def get_lock_holder(self, file_path: str) -> str | None:
        """Get the worker ID holding a lock on a file.

        Args:
            file_path: Path to check.

        Returns:
            Worker ID if the file is locked, None otherwise.
        """
        async with self._lock:
            self._cleanup_expired()
            normalized = str(Path(file_path).resolve())
            if normalized in self._locks:
                return self._locks[normalized].worker_id
            return None

    async def get_all_locks(self) -> dict[str, str]:
        """Get all current locks.

        Returns:
            Dictionary mapping file paths to worker IDs for all
            currently held locks.
        """
        async with self._lock:
            self._cleanup_expired()
            return {
                path: info.worker_id for path, info in self._locks.items()
            }

    async def release_all_by_worker(self, worker_id: str) -> int:
        """Release all locks held by a specific worker.

        Useful for cleanup when a worker is stopping.

        Args:
            worker_id: Worker whose locks should be released.

        Returns:
            Number of locks released.
        """
        async with self._lock:
            paths_to_release = [
                path
                for path, info in self._locks.items()
                if info.worker_id == worker_id
            ]
            for path in paths_to_release:
                del self._locks[path]
                logger.debug(f"Lock released by worker cleanup: {path}")
            return len(paths_to_release)

    def _cleanup_expired(self) -> None:
        """Remove expired locks (for crash recovery).

        Called internally during acquire/release operations to
        clean up locks that have exceeded the timeout. This handles
        cases where a worker crashed without releasing its locks.

        Note: Must be called while holding self._lock.
        """
        now = time.time()
        expired = [
            path
            for path, info in self._locks.items()
            if now - info.acquired_at > self._timeout
        ]
        for path in expired:
            old_info = self._locks[path]
            del self._locks[path]
            logger.warning(
                f"Expired lock released: {path} "
                f"(held by {old_info.worker_id} for "
                f"{now - old_info.acquired_at:.1f}s)"
            )

    @property
    def lock_count(self) -> int:
        """Get the number of currently held locks.

        Note: This is an approximate count as it doesn't clean up
        expired locks first. Use get_all_locks() for an accurate count.
        """
        return len(self._locks)
