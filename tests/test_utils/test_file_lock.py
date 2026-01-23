"""Tests for FileLockManager.

Tests the centralized file locking mechanism that prevents race conditions
when multiple workers attempt to process the same output file.

Ticket: war_rig-qxw9
"""

from __future__ import annotations

import asyncio
import time

import pytest

from war_rig.utils.file_lock import FileLockManager, LockInfo


class TestFileLockManager:
    """Tests for FileLockManager class."""

    @pytest.mark.asyncio
    async def test_acquire_lock_success(self, tmp_path):
        """Test successfully acquiring a lock on a file."""
        manager = FileLockManager(lock_timeout=60.0)
        file_path = str(tmp_path / "output.doc.json")

        result = await manager.acquire(file_path, "worker-1")

        assert result is True
        assert await manager.is_locked(file_path) is True
        assert await manager.get_lock_holder(file_path) == "worker-1"

    @pytest.mark.asyncio
    async def test_acquire_lock_denied_different_worker(self, tmp_path):
        """Test that a different worker cannot acquire an already-held lock."""
        manager = FileLockManager(lock_timeout=60.0)
        file_path = str(tmp_path / "output.doc.json")

        # First worker acquires lock
        assert await manager.acquire(file_path, "worker-1") is True

        # Second worker tries to acquire same lock
        result = await manager.acquire(file_path, "worker-2")

        assert result is False
        assert await manager.get_lock_holder(file_path) == "worker-1"

    @pytest.mark.asyncio
    async def test_acquire_lock_refresh_same_worker(self, tmp_path):
        """Test that the same worker can refresh its lock."""
        manager = FileLockManager(lock_timeout=60.0)
        file_path = str(tmp_path / "output.doc.json")

        # First acquisition
        assert await manager.acquire(file_path, "worker-1") is True

        # Same worker can re-acquire (refresh)
        result = await manager.acquire(file_path, "worker-1")

        assert result is True
        assert await manager.get_lock_holder(file_path) == "worker-1"

    @pytest.mark.asyncio
    async def test_release_lock_success(self, tmp_path):
        """Test successfully releasing a lock."""
        manager = FileLockManager(lock_timeout=60.0)
        file_path = str(tmp_path / "output.doc.json")

        await manager.acquire(file_path, "worker-1")
        result = await manager.release(file_path, "worker-1")

        assert result is True
        assert await manager.is_locked(file_path) is False

    @pytest.mark.asyncio
    async def test_release_lock_wrong_worker(self, tmp_path):
        """Test that a different worker cannot release another worker's lock."""
        manager = FileLockManager(lock_timeout=60.0)
        file_path = str(tmp_path / "output.doc.json")

        await manager.acquire(file_path, "worker-1")
        result = await manager.release(file_path, "worker-2")

        assert result is False
        assert await manager.is_locked(file_path) is True
        assert await manager.get_lock_holder(file_path) == "worker-1"

    @pytest.mark.asyncio
    async def test_release_nonexistent_lock(self, tmp_path):
        """Test releasing a lock that doesn't exist."""
        manager = FileLockManager(lock_timeout=60.0)
        file_path = str(tmp_path / "output.doc.json")

        result = await manager.release(file_path, "worker-1")

        assert result is False

    @pytest.mark.asyncio
    async def test_is_locked_by(self, tmp_path):
        """Test checking if a file is locked by a specific worker."""
        manager = FileLockManager(lock_timeout=60.0)
        file_path = str(tmp_path / "output.doc.json")

        await manager.acquire(file_path, "worker-1")

        assert await manager.is_locked_by(file_path, "worker-1") is True
        assert await manager.is_locked_by(file_path, "worker-2") is False

    @pytest.mark.asyncio
    async def test_is_locked_by_other_not_locked(self, tmp_path):
        """Test is_locked_by_other returns False when file is not locked."""
        manager = FileLockManager(lock_timeout=60.0)
        file_path = str(tmp_path / "output.doc.json")

        # File is not locked - should return False for any worker
        assert await manager.is_locked_by_other(file_path, "worker-1") is False
        assert await manager.is_locked_by_other(file_path, "worker-2") is False

    @pytest.mark.asyncio
    async def test_is_locked_by_other_locked_by_same_worker(self, tmp_path):
        """Test is_locked_by_other returns False when locked by the same worker."""
        manager = FileLockManager(lock_timeout=60.0)
        file_path = str(tmp_path / "output.doc.json")

        # Worker-1 acquires the lock
        await manager.acquire(file_path, "worker-1")

        # Worker-1 asks if it's locked by someone else - should be False
        assert await manager.is_locked_by_other(file_path, "worker-1") is False

    @pytest.mark.asyncio
    async def test_is_locked_by_other_locked_by_different_worker(self, tmp_path):
        """Test is_locked_by_other returns True when locked by a different worker."""
        manager = FileLockManager(lock_timeout=60.0)
        file_path = str(tmp_path / "output.doc.json")

        # Worker-1 acquires the lock
        await manager.acquire(file_path, "worker-1")

        # Worker-2 asks if it's locked by someone else - should be True
        assert await manager.is_locked_by_other(file_path, "worker-2") is True
        # Worker-3 also sees it as locked by someone else
        assert await manager.is_locked_by_other(file_path, "worker-3") is True

    @pytest.mark.asyncio
    async def test_is_locked_by_other_handles_expired_locks(self, tmp_path):
        """Test is_locked_by_other correctly handles expired locks."""
        # Very short timeout for testing
        manager = FileLockManager(lock_timeout=0.1)
        file_path = str(tmp_path / "output.doc.json")

        # Worker-1 acquires the lock
        await manager.acquire(file_path, "worker-1")

        # Worker-2 sees it as locked
        assert await manager.is_locked_by_other(file_path, "worker-2") is True

        # Wait for lock to expire
        import asyncio
        await asyncio.sleep(0.15)

        # After expiration, worker-2 should not see it as locked by other
        assert await manager.is_locked_by_other(file_path, "worker-2") is False

    @pytest.mark.asyncio
    async def test_get_all_locks(self, tmp_path):
        """Test getting all current locks."""
        manager = FileLockManager(lock_timeout=60.0)
        file1 = str(tmp_path / "output1.doc.json")
        file2 = str(tmp_path / "output2.doc.json")

        await manager.acquire(file1, "worker-1")
        await manager.acquire(file2, "worker-2")

        locks = await manager.get_all_locks()

        assert len(locks) == 2
        # Note: paths are normalized to absolute, so we check by suffix
        found_worker1 = any(
            k.endswith("output1.doc.json") and v == "worker-1" for k, v in locks.items()
        )
        found_worker2 = any(
            k.endswith("output2.doc.json") and v == "worker-2" for k, v in locks.items()
        )
        assert found_worker1 is True
        assert found_worker2 is True

    @pytest.mark.asyncio
    async def test_release_all_by_worker(self, tmp_path):
        """Test releasing all locks held by a specific worker."""
        manager = FileLockManager(lock_timeout=60.0)
        file1 = str(tmp_path / "output1.doc.json")
        file2 = str(tmp_path / "output2.doc.json")
        file3 = str(tmp_path / "output3.doc.json")

        await manager.acquire(file1, "worker-1")
        await manager.acquire(file2, "worker-1")
        await manager.acquire(file3, "worker-2")

        released = await manager.release_all_by_worker("worker-1")

        assert released == 2
        assert await manager.is_locked(file1) is False
        assert await manager.is_locked(file2) is False
        assert await manager.is_locked(file3) is True  # worker-2's lock preserved

    @pytest.mark.asyncio
    async def test_lock_count_property(self, tmp_path):
        """Test the lock_count property."""
        manager = FileLockManager(lock_timeout=60.0)
        file1 = str(tmp_path / "output1.doc.json")
        file2 = str(tmp_path / "output2.doc.json")

        assert manager.lock_count == 0

        await manager.acquire(file1, "worker-1")
        assert manager.lock_count == 1

        await manager.acquire(file2, "worker-2")
        assert manager.lock_count == 2

        await manager.release(file1, "worker-1")
        assert manager.lock_count == 1

    @pytest.mark.asyncio
    async def test_path_normalization(self, tmp_path):
        """Test that paths are normalized for consistent comparison."""
        manager = FileLockManager(lock_timeout=60.0)

        # These should all be treated as the same file
        path1 = str(tmp_path / "output.doc.json")
        path2 = str(tmp_path / "./output.doc.json")
        path3 = str(tmp_path / "subdir" / ".." / "output.doc.json")

        await manager.acquire(path1, "worker-1")

        # Should fail - all paths normalize to the same file
        assert await manager.acquire(path2, "worker-2") is False
        assert await manager.acquire(path3, "worker-3") is False

        # Should all show as locked
        assert await manager.is_locked(path1) is True
        assert await manager.is_locked(path2) is True
        assert await manager.is_locked(path3) is True


class TestFileLockManagerExpiration:
    """Tests for lock expiration/timeout behavior."""

    @pytest.mark.asyncio
    async def test_expired_lock_cleaned_on_acquire(self, tmp_path):
        """Test that expired locks are cleaned up during acquire."""
        # Very short timeout for testing
        manager = FileLockManager(lock_timeout=0.1)
        file_path = str(tmp_path / "output.doc.json")

        # Worker 1 acquires lock
        await manager.acquire(file_path, "worker-1")
        assert await manager.is_locked(file_path) is True

        # Wait for lock to expire
        await asyncio.sleep(0.15)

        # Worker 2 can now acquire - expired lock should be cleaned up
        result = await manager.acquire(file_path, "worker-2")

        assert result is True
        assert await manager.get_lock_holder(file_path) == "worker-2"

    @pytest.mark.asyncio
    async def test_expired_lock_cleaned_on_is_locked(self, tmp_path):
        """Test that expired locks are cleaned up during is_locked check."""
        manager = FileLockManager(lock_timeout=0.1)
        file_path = str(tmp_path / "output.doc.json")

        await manager.acquire(file_path, "worker-1")

        # Wait for lock to expire
        await asyncio.sleep(0.15)

        # is_locked should return False after expiration
        assert await manager.is_locked(file_path) is False

    @pytest.mark.asyncio
    async def test_non_expired_lock_preserved(self, tmp_path):
        """Test that non-expired locks are not cleaned up."""
        manager = FileLockManager(lock_timeout=60.0)  # Long timeout
        file_path = str(tmp_path / "output.doc.json")

        await manager.acquire(file_path, "worker-1")

        # Brief wait - lock should still be valid
        await asyncio.sleep(0.1)

        # Worker 2 should not be able to acquire
        assert await manager.acquire(file_path, "worker-2") is False


class TestFileLockManagerConcurrency:
    """Tests for concurrent access behavior."""

    @pytest.mark.asyncio
    async def test_concurrent_acquire_attempts(self, tmp_path):
        """Test that only one worker can acquire a lock concurrently."""
        manager = FileLockManager(lock_timeout=60.0)
        file_path = str(tmp_path / "output.doc.json")
        results = []

        async def try_acquire(worker_id: str):
            result = await manager.acquire(file_path, worker_id)
            results.append((worker_id, result))

        # Multiple workers try to acquire simultaneously
        await asyncio.gather(
            try_acquire("worker-1"),
            try_acquire("worker-2"),
            try_acquire("worker-3"),
        )

        # Exactly one should succeed
        successful = [r for r in results if r[1] is True]
        failed = [r for r in results if r[1] is False]

        assert len(successful) == 1
        assert len(failed) == 2

    @pytest.mark.asyncio
    async def test_acquire_after_release(self, tmp_path):
        """Test that a new worker can acquire after release."""
        manager = FileLockManager(lock_timeout=60.0)
        file_path = str(tmp_path / "output.doc.json")

        # Worker 1 acquires and releases
        await manager.acquire(file_path, "worker-1")
        await manager.release(file_path, "worker-1")

        # Worker 2 can now acquire
        result = await manager.acquire(file_path, "worker-2")

        assert result is True
        assert await manager.get_lock_holder(file_path) == "worker-2"

    @pytest.mark.asyncio
    async def test_multiple_files_different_workers(self, tmp_path):
        """Test that different files can be locked by different workers."""
        manager = FileLockManager(lock_timeout=60.0)
        file1 = str(tmp_path / "output1.doc.json")
        file2 = str(tmp_path / "output2.doc.json")

        # Different workers lock different files
        assert await manager.acquire(file1, "worker-1") is True
        assert await manager.acquire(file2, "worker-2") is True

        # Each worker holds their own lock
        assert await manager.get_lock_holder(file1) == "worker-1"
        assert await manager.get_lock_holder(file2) == "worker-2"

        # Cross-access attempts fail
        assert await manager.acquire(file1, "worker-2") is False
        assert await manager.acquire(file2, "worker-1") is False


class TestLockInfo:
    """Tests for LockInfo dataclass."""

    def test_lock_info_creation(self):
        """Test creating a LockInfo instance."""
        now = time.time()
        info = LockInfo(
            worker_id="worker-1",
            acquired_at=now,
            file_path="/path/to/file.json",
        )

        assert info.worker_id == "worker-1"
        assert info.acquired_at == now
        assert info.file_path == "/path/to/file.json"
