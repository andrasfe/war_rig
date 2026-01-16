"""Tests for Atlas adapter implementations.

These tests verify that War Rig's Atlas adapters correctly implement
the Atlas interfaces and properly bridge between systems.
"""

import json
import pytest
import tempfile
from pathlib import Path
from unittest.mock import AsyncMock, MagicMock, patch

from war_rig.adapters import (
    AnalysisRoute,
    AnalysisRouter,
    BeadsTicketAdapter,
    FileArtifactAdapter,
    RouteDecision,
)


class TestAnalysisRouter:
    """Tests for the AnalysisRouter."""

    @pytest.fixture
    def config(self):
        """Create mock config for testing."""
        config = MagicMock()
        config.atlas_enabled = True
        config.atlas_context_budget = 4000
        config.atlas_max_chunk_tokens = 3500
        config.atlas_semantic_chunking = True
        return config

    @pytest.fixture
    def router(self, config):
        """Create an AnalysisRouter for testing."""
        return AnalysisRouter(config)

    def test_route_small_file_direct(self, router, tmp_path):
        """Small files should be routed for direct processing."""
        # Create small test file
        test_file = tmp_path / "small.cbl"
        test_file.write_text("       IDENTIFICATION DIVISION.\n" * 100)  # ~3500 chars

        decision = router.route(test_file)

        assert decision.route == AnalysisRoute.DIRECT
        assert decision.estimated_tokens < router.context_budget

    def test_route_large_file_chunked(self, router, tmp_path):
        """Large files should be routed for chunked processing."""
        # Create large test file (~80,000 chars = ~20,000 tokens)
        test_file = tmp_path / "large.cbl"
        test_file.write_text("       IDENTIFICATION DIVISION.\n" * 2000)

        decision = router.route(test_file)

        assert decision.route == AnalysisRoute.CHUNKED
        assert decision.estimated_tokens > router.context_budget

    def test_route_unsupported_extension_skip(self, router, tmp_path):
        """Unsupported file types should be skipped."""
        test_file = tmp_path / "readme.txt"
        test_file.write_text("This is a text file")

        decision = router.route(test_file)

        assert decision.route == AnalysisRoute.SKIP

    def test_route_binary_file_skip(self, router, tmp_path):
        """Binary files should be skipped."""
        test_file = tmp_path / "program.exe"
        test_file.write_bytes(b"\x00\x01\x02\x03")

        decision = router.route(test_file)

        assert decision.route == AnalysisRoute.SKIP

    def test_route_batch(self, router, tmp_path):
        """Test batch routing of multiple files."""
        # Create various test files
        small_file = tmp_path / "small.cbl"
        small_file.write_text("X" * 1000)

        large_file = tmp_path / "large.cbl"
        large_file.write_text("X" * 50000)

        unsupported_file = tmp_path / "readme.txt"
        unsupported_file.write_text("text")

        result = router.route_batch([small_file, large_file, unsupported_file])

        assert small_file in result[AnalysisRoute.DIRECT]
        assert large_file in result[AnalysisRoute.CHUNKED]
        assert unsupported_file in result[AnalysisRoute.SKIP]

    def test_disabled_atlas_routes_direct(self, tmp_path):
        """When Atlas is disabled, all files should route direct."""
        config = MagicMock()
        config.atlas_enabled = False
        config.atlas_context_budget = 4000
        router = AnalysisRouter(config)

        large_file = tmp_path / "large.cbl"
        large_file.write_text("X" * 50000)

        decision = router.route(large_file)

        assert decision.route == AnalysisRoute.DIRECT
        assert "disabled" in decision.reason.lower()


class TestFileArtifactAdapter:
    """Tests for the FileArtifactAdapter."""

    @pytest.fixture
    def adapter(self, tmp_path):
        """Create a FileArtifactAdapter for testing."""
        return FileArtifactAdapter(tmp_path)

    @pytest.mark.asyncio
    async def test_write_and_read(self, adapter):
        """Test writing and reading content."""
        content = b'{"test": "data"}'
        uri = "test/artifact.json"

        await adapter.write(uri, content)
        result = await adapter.read(uri)

        assert result == content

    @pytest.mark.asyncio
    async def test_exists(self, adapter):
        """Test existence check."""
        uri = "test/exists.json"

        assert not await adapter.exists(uri)

        await adapter.write(uri, b"content")

        assert await adapter.exists(uri)

    @pytest.mark.asyncio
    async def test_delete(self, adapter):
        """Test artifact deletion."""
        uri = "test/delete.json"
        await adapter.write(uri, b"content")

        assert await adapter.exists(uri)
        result = await adapter.delete(uri)
        assert result is True
        assert not await adapter.exists(uri)

    @pytest.mark.asyncio
    async def test_delete_nonexistent(self, adapter):
        """Deleting nonexistent artifact returns False."""
        result = await adapter.delete("nonexistent.json")
        assert result is False

    @pytest.mark.asyncio
    async def test_write_json(self, adapter):
        """Test JSON writing convenience method."""
        data = {"key": "value", "number": 42}
        uri = "test/data.json"

        await adapter.write_json(uri, data)
        result = await adapter.read_json(uri)

        assert result == data

    @pytest.mark.asyncio
    async def test_get_metadata(self, adapter):
        """Test metadata retrieval."""
        content = b"test content"
        uri = "test/meta.txt"

        await adapter.write(uri, content, content_type="text/plain")
        metadata = await adapter.get_metadata(uri)

        assert metadata is not None
        assert metadata.artifact_id == "meta.txt"
        assert metadata.metadata.get("size_bytes") == len(content)

    @pytest.mark.asyncio
    async def test_list_artifacts(self, adapter):
        """Test artifact listing."""
        # Create some artifacts
        await adapter.write("sources/file1.cbl", b"content1")
        await adapter.write("sources/file2.cbl", b"content2")
        await adapter.write("results/result.json", b"{}")

        sources = await adapter.list_artifacts("sources/")

        assert len(sources) == 2
        assert any("file1.cbl" in s for s in sources)
        assert any("file2.cbl" in s for s in sources)

    @pytest.mark.asyncio
    async def test_compute_hash(self, adapter):
        """Test content hashing."""
        content = b"test content"
        hash1 = await adapter.compute_hash(content)
        hash2 = await adapter.compute_hash(content)
        hash3 = await adapter.compute_hash(b"different")

        assert hash1 == hash2  # Same content, same hash
        assert hash1 != hash3  # Different content, different hash

    @pytest.mark.asyncio
    async def test_store_source(self, adapter, tmp_path):
        """Test source file storage convenience method."""
        # Create a source file
        source_file = tmp_path / "test.cbl"
        source_file.write_text("       IDENTIFICATION DIVISION.")

        artifact = await adapter.store_source(source_file)

        assert artifact.artifact_id == "test.cbl"
        assert artifact.artifact_type == "cobol"
        assert artifact.artifact_uri == "sources/test.cbl"


class TestBeadsTicketAdapter:
    """Tests for the BeadsTicketAdapter."""

    @pytest.fixture
    def beads_client(self):
        """Create mock BeadsClient for testing."""
        client = MagicMock()
        client._pm_ticket_cache = {}
        return client

    @pytest.fixture
    def adapter(self, beads_client):
        """Create a BeadsTicketAdapter for testing."""
        return BeadsTicketAdapter(beads_client)

    @pytest.mark.asyncio
    async def test_create_work_item(self, adapter, beads_client):
        """Test work item creation."""
        from atlas.models.work_item import WorkItem, WorkItemPayload
        from atlas.models.enums import WorkItemStatus, WorkItemType

        # Mock create_pm_ticket to return a ticket
        mock_ticket = MagicMock()
        mock_ticket.ticket_id = "test-ticket-123"
        beads_client.create_pm_ticket.return_value = mock_ticket

        work_item = WorkItem(
            work_id="temp",
            work_type=WorkItemType.DOC_CHUNK,
            status=WorkItemStatus.NEW,
            payload=WorkItemPayload(job_id="job-1"),
        )

        result = await adapter.create_work_item(work_item)

        assert result == "test-ticket-123"
        beads_client.create_pm_ticket.assert_called_once()

    @pytest.mark.asyncio
    async def test_get_work_item_from_cache(self, adapter):
        """Test work item retrieval from cache."""
        from atlas.models.work_item import WorkItem, WorkItemPayload
        from atlas.models.enums import WorkItemStatus, WorkItemType

        # Add item to cache
        work_item = WorkItem(
            work_id="cached-item",
            work_type=WorkItemType.DOC_CHUNK,
            status=WorkItemStatus.READY,
            payload=WorkItemPayload(job_id="job-1"),
        )
        adapter._work_item_cache["cached-item"] = work_item

        result = await adapter.get_work_item("cached-item")

        assert result is not None
        assert result.work_id == "cached-item"

    @pytest.mark.asyncio
    async def test_update_status(self, adapter, beads_client):
        """Test status update."""
        from atlas.models.enums import WorkItemStatus

        beads_client.update_ticket_state.return_value = True

        result = await adapter.update_status(
            "ticket-123",
            WorkItemStatus.DONE,
        )

        assert result is True
        beads_client.update_ticket_state.assert_called_once()

    @pytest.mark.asyncio
    async def test_claim_work_item(self, adapter, beads_client):
        """Test work item claiming."""
        beads_client.claim_ticket.return_value = True

        result = await adapter.claim_work_item(
            "ticket-123",
            "worker-1",
            lease_duration_seconds=300,
        )

        assert result is True
        beads_client.claim_ticket.assert_called_once_with(
            "ticket-123", "worker-1"
        )
