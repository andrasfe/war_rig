"""
Spec manager for loading, caching, and validating artifact specifications.

This module provides the SpecManager class that handles:
- Loading specs from builtin and cache directories
- Matching specs to files by extension or pattern
- Validating specs against the schema
- Caching generated specs
- Computing content hashes for reproducibility
"""

from __future__ import annotations

import fnmatch
import hashlib
import logging
import re
from pathlib import Path
from typing import TYPE_CHECKING

import yaml

from citadel.specs.schema import ArtifactSpec

if TYPE_CHECKING:
    pass

logger = logging.getLogger(__name__)


class SpecManager:
    """
    Manages artifact specifications: loading, caching, validation.

    Specs are YAML files that define how to extract artifacts and relationships
    from source files. The manager maintains indices for fast lookup by ID,
    file extension, and file pattern.
    """

    def __init__(self, builtin_dir: Path, cache_dir: Path) -> None:
        """
        Initialize the spec manager with directories for builtin and cached specs.

        Args:
            builtin_dir: Directory containing built-in specs (version controlled).
            cache_dir: Directory for cached generated specs (gitignored).
        """
        self._builtin_dir = builtin_dir
        self._cache_dir = cache_dir

        # Indices for fast lookup
        self._specs_by_id: dict[str, ArtifactSpec] = {}
        self._specs_by_extension: dict[str, ArtifactSpec] = {}
        self._specs_with_patterns: list[ArtifactSpec] = []

        # Track which specs came from where for cache management
        self._spec_sources: dict[str, Path] = {}

        # Load specs on initialization
        self._load_all_specs()

    def _load_all_specs(self) -> None:
        """Load all specs from builtin and cache directories."""
        # Load builtin specs first (they take precedence for same IDs)
        if self._builtin_dir.exists():
            self._load_specs_from_directory(self._builtin_dir, is_builtin=True)
        else:
            logger.warning("Builtin spec directory does not exist: %s", self._builtin_dir)

        # Load cached specs (only if not already loaded from builtin)
        if self._cache_dir.exists():
            self._load_specs_from_directory(self._cache_dir, is_builtin=False)
        else:
            logger.debug("Cache spec directory does not exist: %s", self._cache_dir)

    def _load_specs_from_directory(self, directory: Path, *, is_builtin: bool) -> None:
        """
        Load all YAML spec files from a directory.

        Args:
            directory: Directory to scan for .yaml files.
            is_builtin: Whether these are builtin specs (affects precedence).
        """
        for yaml_path in directory.glob("*.yaml"):
            try:
                spec = self._load_spec_file(yaml_path)
                self._register_spec(spec, yaml_path, is_builtin=is_builtin)
                logger.debug("Loaded spec '%s' from %s", spec.spec_id, yaml_path)
            except Exception as e:
                logger.error("Failed to load spec from %s: %s", yaml_path, e)

    def _load_spec_file(self, path: Path) -> ArtifactSpec:
        """
        Load and parse a single spec file.

        Args:
            path: Path to the YAML spec file.

        Returns:
            Parsed ArtifactSpec model.

        Raises:
            ValueError: If the YAML is invalid or doesn't match the schema.
        """
        with open(path, encoding="utf-8") as f:
            data = yaml.safe_load(f)

        if data is None:
            raise ValueError(f"Empty spec file: {path}")

        return ArtifactSpec.model_validate(data)

    def _register_spec(
        self, spec: ArtifactSpec, source_path: Path, *, is_builtin: bool
    ) -> None:
        """
        Register a spec in the internal indices.

        Args:
            spec: The spec to register.
            source_path: Path where the spec was loaded from.
            is_builtin: Whether this is a builtin spec.
        """
        # Skip if already registered from builtin (builtin takes precedence)
        if spec.spec_id in self._specs_by_id and not is_builtin:
            logger.debug(
                "Skipping cached spec '%s' - builtin version already loaded",
                spec.spec_id,
            )
            return

        # Register by ID
        self._specs_by_id[spec.spec_id] = spec
        self._spec_sources[spec.spec_id] = source_path

        # Register by extension
        for ext in spec.file_extensions:
            # Normalize extension to lowercase with leading dot
            normalized_ext = ext.lower() if ext.startswith(".") else f".{ext.lower()}"
            self._specs_by_extension[normalized_ext] = spec

        # Register specs that have file patterns
        if spec.file_patterns:
            self._specs_with_patterns.append(spec)

    def get_spec(self, spec_id: str) -> ArtifactSpec | None:
        """
        Get a spec by its unique identifier.

        Checks builtin specs first, then cache.

        Args:
            spec_id: The unique identifier of the spec (e.g., "cobol", "python").

        Returns:
            The ArtifactSpec if found, None otherwise.
        """
        return self._specs_by_id.get(spec_id)

    def get_spec_for_extension(self, ext: str) -> ArtifactSpec | None:
        """
        Get the spec that handles a file extension.

        Args:
            ext: File extension including the dot (e.g., ".py", ".cbl").

        Returns:
            The ArtifactSpec that handles this extension, or None if not found.
        """
        # Normalize extension to lowercase
        normalized_ext = ext.lower() if ext.startswith(".") else f".{ext.lower()}"
        return self._specs_by_extension.get(normalized_ext)

    def get_spec_for_file(self, path: Path) -> ArtifactSpec | None:
        """
        Get the spec for a specific file.

        Checks file patterns first (more specific), then falls back to extension.

        Args:
            path: Path to the file.

        Returns:
            The ArtifactSpec for this file, or None if not found.
        """
        # First try pattern matching (more specific)
        for spec in self._specs_with_patterns:
            for pattern in spec.file_patterns:
                if self._matches_pattern(path, pattern):
                    return spec

        # Fall back to extension matching
        ext = path.suffix
        if ext:
            return self.get_spec_for_extension(ext)

        return None

    def _matches_pattern(self, path: Path, pattern: str) -> bool:
        """
        Check if a file path matches a glob pattern.

        Args:
            path: Path to check.
            pattern: Glob pattern to match against.

        Returns:
            True if the path matches the pattern.
        """
        # Convert path to string for matching
        path_str = str(path)

        # Handle both absolute and relative patterns
        if fnmatch.fnmatch(path_str, pattern):
            return True

        # Also try matching just the filename
        if fnmatch.fnmatch(path.name, pattern):
            return True

        # Try matching the path parts
        path_parts = "/".join(path.parts)
        if fnmatch.fnmatch(path_parts, pattern):
            return True

        return False

    def validate_spec(self, spec: ArtifactSpec) -> list[str]:
        """
        Validate a spec against the schema and check pattern validity.

        Performs checks beyond Pydantic validation:
        - Regex patterns are valid
        - Required fields are meaningful
        - No conflicting configurations

        Args:
            spec: The spec to validate.

        Returns:
            List of validation error messages. Empty if valid.
        """
        errors: list[str] = []

        # Validate spec_id format
        if not spec.spec_id:
            errors.append("spec_id cannot be empty")
        elif not re.match(r"^[a-z0-9_-]+$", spec.spec_id):
            errors.append(
                f"spec_id '{spec.spec_id}' should only contain lowercase letters, "
                "numbers, underscores, and hyphens"
            )

        # Validate file extensions
        if not spec.file_extensions:
            errors.append("file_extensions cannot be empty - spec must match some files")

        # Validate definition patterns
        for pattern in spec.definition_patterns:
            pattern_errors = self._validate_extraction_pattern(pattern, "definition")
            errors.extend(pattern_errors)

        # Validate reference patterns
        for pattern in spec.reference_patterns:
            pattern_errors = self._validate_extraction_pattern(pattern, "reference")
            errors.extend(pattern_errors)

        # Validate preprocessor patterns
        for pattern in spec.preprocessor_patterns:
            pattern_errors = self._validate_extraction_pattern(pattern, "preprocessor")
            errors.extend(pattern_errors)

        # Validate scope syntax if present
        if spec.scope:
            scope_errors = self._validate_scope_syntax(spec.scope)
            errors.extend(scope_errors)

        # Validate comment syntax
        if spec.comments.fixed_column is not None:
            if spec.comments.fixed_column < 0:
                errors.append("comments.fixed_column cannot be negative")
            if spec.comments.fixed_indicator is None:
                errors.append(
                    "comments.fixed_indicator is required when fixed_column is set"
                )

        return errors

    def _validate_extraction_pattern(
        self, pattern: ExtractionPattern, pattern_type: str
    ) -> list[str]:
        """
        Validate a single extraction pattern.

        Args:
            pattern: The pattern to validate.
            pattern_type: Description for error messages (definition/reference/preprocessor).

        Returns:
            List of error messages.
        """

        errors: list[str] = []
        prefix = f"{pattern_type} pattern '{pattern.name}'"

        # Validate regex pattern
        try:
            flags = 0
            if pattern.ignore_case:
                flags |= re.IGNORECASE
            if pattern.multiline:
                flags |= re.MULTILINE
            if pattern.dotall:
                flags |= re.DOTALL
            re.compile(pattern.pattern, flags)
        except re.error as e:
            errors.append(f"{prefix}: invalid regex pattern - {e}")

        # Validate capture groups reference valid groups in the pattern
        try:
            compiled = re.compile(pattern.pattern)
            num_groups = compiled.groups
            for group_num in pattern.capture_groups:
                if group_num < 0 or group_num > num_groups:
                    errors.append(
                        f"{prefix}: capture group {group_num} is out of range "
                        f"(pattern has {num_groups} groups)"
                    )
        except re.error:
            pass  # Already reported above

        # Validate must_not_follow pattern if present
        if pattern.must_not_follow:
            try:
                re.compile(pattern.must_not_follow)
            except re.error as e:
                errors.append(f"{prefix}: invalid must_not_follow pattern - {e}")

        # Validate must_not_precede pattern if present
        if pattern.must_not_precede:
            try:
                re.compile(pattern.must_not_precede)
            except re.error as e:
                errors.append(f"{prefix}: invalid must_not_precede pattern - {e}")

        # Validate column_pattern if present
        if pattern.column_pattern:
            try:
                re.compile(pattern.column_pattern)
            except re.error as e:
                errors.append(f"{prefix}: invalid column_pattern - {e}")

        return errors

    def _validate_scope_syntax(self, scope: ScopeSyntax) -> list[str]:
        """
        Validate scope syntax patterns.

        Args:
            scope: The scope syntax to validate.

        Returns:
            List of error messages.
        """

        errors: list[str] = []

        try:
            re.compile(scope.start_pattern)
        except re.error as e:
            errors.append(f"scope.start_pattern: invalid regex - {e}")

        try:
            re.compile(scope.end_pattern)
        except re.error as e:
            errors.append(f"scope.end_pattern: invalid regex - {e}")

        if scope.name_pattern:
            try:
                re.compile(scope.name_pattern)
            except re.error as e:
                errors.append(f"scope.name_pattern: invalid regex - {e}")

        return errors

    def cache_spec(self, spec: ArtifactSpec) -> None:
        """
        Save a spec to the cache directory.

        Creates the cache directory if it doesn't exist. The spec is saved
        as a YAML file named {spec_id}.yaml.

        Args:
            spec: The spec to cache.

        Raises:
            ValueError: If the spec fails validation.
        """
        # Validate before caching
        errors = self.validate_spec(spec)
        if errors:
            raise ValueError(f"Cannot cache invalid spec: {'; '.join(errors)}")

        # Ensure cache directory exists
        self._cache_dir.mkdir(parents=True, exist_ok=True)

        # Write spec to YAML file
        cache_path = self._cache_dir / f"{spec.spec_id}.yaml"

        # Convert to dict for YAML serialization
        spec_dict = spec.model_dump(mode="json", exclude_none=True)

        with open(cache_path, "w", encoding="utf-8") as f:
            yaml.safe_dump(spec_dict, f, default_flow_style=False, sort_keys=False)

        # Register the spec in our indices
        self._register_spec(spec, cache_path, is_builtin=False)

        logger.info("Cached spec '%s' to %s", spec.spec_id, cache_path)

    def list_available_specs(self) -> list[str]:
        """
        List all available spec IDs.

        Returns:
            Sorted list of all spec IDs from both builtin and cache.
        """
        return sorted(self._specs_by_id.keys())

    def get_spec_hash(self, spec_id: str) -> str:
        """
        Get a content hash for a spec for reproducibility tracking.

        The hash is computed from the YAML representation of the spec,
        ensuring consistent hashing regardless of how the spec was loaded.

        Args:
            spec_id: The spec identifier.

        Returns:
            SHA-256 hash of the spec content (hex digest).

        Raises:
            KeyError: If the spec_id is not found.
        """
        spec = self._specs_by_id.get(spec_id)
        if spec is None:
            raise KeyError(f"Spec not found: {spec_id}")

        # Convert to dict and then to consistent YAML for hashing
        spec_dict = spec.model_dump(mode="json", exclude_none=True)

        # Sort keys for consistent serialization
        yaml_content = yaml.safe_dump(spec_dict, default_flow_style=False, sort_keys=True)

        # Compute SHA-256 hash
        hasher = hashlib.sha256()
        hasher.update(yaml_content.encode("utf-8"))

        return hasher.hexdigest()

    def reload(self) -> None:
        """
        Reload all specs from disk.

        Useful when specs have been modified externally or new specs
        have been added to the cache directory.
        """
        # Clear existing indices
        self._specs_by_id.clear()
        self._specs_by_extension.clear()
        self._specs_with_patterns.clear()
        self._spec_sources.clear()

        # Reload
        self._load_all_specs()

        logger.info("Reloaded specs - found %d specs", len(self._specs_by_id))

    def get_spec_source(self, spec_id: str) -> Path | None:
        """
        Get the file path where a spec was loaded from.

        Args:
            spec_id: The spec identifier.

        Returns:
            Path to the spec file, or None if not found.
        """
        return self._spec_sources.get(spec_id)

    def is_builtin(self, spec_id: str) -> bool:
        """
        Check if a spec is a builtin spec.

        Args:
            spec_id: The spec identifier.

        Returns:
            True if the spec was loaded from the builtin directory.
        """
        source = self._spec_sources.get(spec_id)
        if source is None:
            return False
        return source.parent == self._builtin_dir

    def get_extensions_handled(self) -> list[str]:
        """
        Get all file extensions that have specs.

        Returns:
            Sorted list of file extensions (including the dot).
        """
        return sorted(self._specs_by_extension.keys())
