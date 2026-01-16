"""Filesystem-based artifact storage adapter for Atlas.

This adapter implements Atlas's ArtifactStoreAdapter interface using
the local filesystem, suitable for single-machine War Rig deployments.

URIs use the format:
- file:///absolute/path/to/artifact
- Or relative paths resolved against a base directory
"""

from __future__ import annotations

import hashlib
import json
from pathlib import Path
from typing import Any

from atlas.adapters.artifact_store import ArtifactStoreAdapter
from atlas.models.artifact import Artifact


class FileArtifactAdapter(ArtifactStoreAdapter):
    """Filesystem implementation of Atlas ArtifactStoreAdapter.

    Stores artifacts as files on the local filesystem with JSON
    metadata sidecar files.

    Attributes:
        base_path: Root directory for artifact storage.

    Example:
        >>> adapter = FileArtifactAdapter(Path("./output/artifacts"))
        >>> await adapter.write("chunks/chunk-001.json", b'{"content": "..."}')
        >>> content = await adapter.read("chunks/chunk-001.json")
    """

    def __init__(self, base_path: Path) -> None:
        """Initialize the adapter.

        Args:
            base_path: Root directory for artifact storage.
                       Will be created if it doesn't exist.
        """
        self.base_path = base_path
        self.base_path.mkdir(parents=True, exist_ok=True)

    def _resolve_path(self, uri: str) -> Path:
        """Resolve a URI to a filesystem path.

        Args:
            uri: URI (file:// or relative path).

        Returns:
            Resolved absolute Path.
        """
        # Strip file:// prefix if present
        if uri.startswith("file://"):
            path_str = uri[7:]
            return Path(path_str)

        # Relative path - resolve against base
        return self.base_path / uri

    def _metadata_path(self, artifact_path: Path) -> Path:
        """Get the metadata sidecar path for an artifact.

        Args:
            artifact_path: Path to the artifact file.

        Returns:
            Path to the .meta.json sidecar file.
        """
        return artifact_path.with_suffix(artifact_path.suffix + ".meta.json")

    async def write(
        self,
        uri: str,
        content: bytes,
        *,
        content_type: str = "application/json",
        metadata: dict[str, str] | None = None,
    ) -> str:
        """Write content to the filesystem.

        Creates parent directories as needed. Writes are idempotent.

        Args:
            uri: Target URI or relative path.
            content: Content bytes to write.
            content_type: MIME type for metadata.
            metadata: Optional metadata dict.

        Returns:
            The URI where content was written.
        """
        path = self._resolve_path(uri)
        path.parent.mkdir(parents=True, exist_ok=True)

        # Write content
        path.write_bytes(content)

        # Write metadata sidecar
        content_hash = await self.compute_hash(content)
        meta = {
            "content_type": content_type,
            "content_hash": content_hash,
            "size_bytes": len(content),
            **(metadata or {}),
        }
        meta_path = self._metadata_path(path)
        meta_path.write_text(json.dumps(meta, indent=2))

        return uri

    async def read(self, uri: str) -> bytes:
        """Read content from the filesystem.

        Args:
            uri: Artifact URI or relative path.

        Returns:
            Content bytes.

        Raises:
            FileNotFoundError: If artifact doesn't exist.
        """
        path = self._resolve_path(uri)
        return path.read_bytes()

    async def exists(self, uri: str) -> bool:
        """Check if an artifact exists.

        Args:
            uri: Artifact URI or relative path.

        Returns:
            True if artifact exists.
        """
        path = self._resolve_path(uri)
        return path.exists()

    async def delete(self, uri: str) -> bool:
        """Delete an artifact and its metadata.

        Args:
            uri: Artifact URI or relative path.

        Returns:
            True if deleted, False if didn't exist.
        """
        path = self._resolve_path(uri)

        if not path.exists():
            return False

        path.unlink()

        # Also delete metadata sidecar if present
        meta_path = self._metadata_path(path)
        if meta_path.exists():
            meta_path.unlink()

        return True

    async def get_metadata(self, uri: str) -> Artifact | None:
        """Get artifact metadata.

        Args:
            uri: Artifact URI or relative path.

        Returns:
            Artifact with metadata if exists, None otherwise.
        """
        path = self._resolve_path(uri)

        if not path.exists():
            return None

        # Read metadata sidecar
        meta_path = self._metadata_path(path)
        metadata: dict[str, Any] = {}

        if meta_path.exists():
            metadata = json.loads(meta_path.read_text())

        # Determine artifact type from extension
        suffix = path.suffix.lower()
        artifact_type = self._infer_artifact_type(suffix)

        return Artifact(
            artifact_id=path.name,
            artifact_type=artifact_type,
            artifact_version=metadata.get("content_hash", "unknown"),
            artifact_uri=uri,
            metadata=metadata,
        )

    def _infer_artifact_type(self, suffix: str) -> str:
        """Infer artifact type from file extension.

        Args:
            suffix: File extension (e.g., ".cbl").

        Returns:
            Artifact type string.
        """
        cobol_exts = {".cbl", ".cob"}
        copybook_exts = {".cpy", ".copy"}
        jcl_exts = {".jcl"}

        if suffix in cobol_exts:
            return "cobol"
        elif suffix in copybook_exts:
            return "copybook"
        elif suffix in jcl_exts:
            return "jcl"
        elif suffix == ".json":
            return "json"
        elif suffix == ".md":
            return "markdown"
        else:
            return "other"

    async def list_artifacts(
        self,
        prefix: str,
        limit: int = 1000,
    ) -> list[str]:
        """List artifacts by URI prefix.

        Args:
            prefix: URI prefix to filter by (relative to base_path).
            limit: Maximum number of results.

        Returns:
            List of artifact URIs matching the prefix.
        """
        prefix_path = self._resolve_path(prefix)

        # If prefix is a directory, list its contents
        if prefix_path.is_dir():
            search_dir = prefix_path
            pattern = "**/*"
        else:
            # Prefix is partial path - search parent with glob
            search_dir = prefix_path.parent
            pattern = f"{prefix_path.name}*"

        if not search_dir.exists():
            return []

        results: list[str] = []
        for path in search_dir.glob(pattern):
            if path.is_file() and not path.name.endswith(".meta.json"):
                # Convert back to relative URI
                try:
                    rel_path = path.relative_to(self.base_path)
                    results.append(str(rel_path))
                except ValueError:
                    # Path is outside base_path
                    results.append(str(path))

                if len(results) >= limit:
                    break

        return results

    async def compute_hash(self, content: bytes) -> str:
        """Compute SHA-256 hash of content.

        Args:
            content: Content bytes to hash.

        Returns:
            Hex-encoded SHA-256 hash.
        """
        return hashlib.sha256(content).hexdigest()

    async def store_source(
        self,
        file_path: Path,
        artifact_id: str | None = None,
    ) -> Artifact:
        """Store a source file as an artifact.

        Convenience method for ingesting source files.

        Args:
            file_path: Path to the source file.
            artifact_id: Optional artifact ID (defaults to filename).

        Returns:
            Artifact metadata for the stored file.
        """
        content = file_path.read_bytes()
        aid = artifact_id or file_path.name

        # Store in sources/ subdirectory
        uri = f"sources/{aid}"
        await self.write(
            uri,
            content,
            content_type="text/plain",
            metadata={
                "original_path": str(file_path),
            },
        )

        content_hash = await self.compute_hash(content)
        artifact_type = self._infer_artifact_type(file_path.suffix.lower())

        return Artifact(
            artifact_id=aid,
            artifact_type=artifact_type,
            artifact_version=content_hash,
            artifact_uri=uri,
            metadata={
                "original_path": str(file_path),
                "size_bytes": len(content),
            },
        )
