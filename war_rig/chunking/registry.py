"""Splitter registry for routing artifacts to appropriate splitters.

The SplitterRegistry provides a plugin architecture for splitters:
- Register splitters by artifact type
- Look up appropriate splitter for a given artifact type
- Fall back to LineBasedSplitter for unknown types

Usage:
    >>> registry = SplitterRegistry()
    >>> registry.register(COBOLSplitter)
    >>> splitter = registry.get_splitter("cobol")
    >>> result = splitter.split(source, profile, artifact_id)

The registry supports both class-based and instance-based registration,
and allows custom configuration per artifact type.
"""

import logging
from typing import Type, TypeVar

from war_rig.chunking.base import Splitter, SplitResult
from war_rig.chunking.models import SplitterProfile

logger = logging.getLogger(__name__)


class SplitterNotFoundError(Exception):
    """Raised when no splitter is found for an artifact type."""

    pass


T = TypeVar("T", bound=Splitter)


class SplitterRegistry:
    """Registry for mapping artifact types to splitter implementations.

    The registry maintains a mapping from artifact types (strings) to
    splitter classes. When a splitter is requested for an artifact type,
    the registry instantiates and returns the appropriate splitter.

    Features:
    - Register splitter classes that declare their handled types
    - Look up splitters by artifact type string
    - Automatic fallback to LineBasedSplitter for unknown types
    - Support for custom configuration per artifact type
    - Thread-safe registration and lookup

    Design Principle:
        The registry enables a plugin architecture where new splitter
        implementations can be added without modifying existing code.
        Each splitter declares which artifact types it handles via
        the get_artifact_types() class method.

    Example:
        >>> from war_rig.chunking.registry import SplitterRegistry
        >>> from war_rig.chunking.cobol import COBOLSplitter
        >>>
        >>> registry = SplitterRegistry()
        >>> registry.register(COBOLSplitter)
        >>>
        >>> # Get splitter for COBOL files
        >>> splitter = registry.get_splitter("cobol")
        >>> result = splitter.split(source, profile, "program.cbl")
        >>>
        >>> # Unknown types fall back to line-based splitting
        >>> splitter = registry.get_splitter("unknown_type")
        >>> # Returns LineBasedSplitter instance

    Attributes:
        _splitter_classes: Mapping from artifact type to splitter class.
        _splitter_instances: Cache of instantiated splitters.
        _configs: Per-artifact-type configuration overrides.
        _fallback_class: Splitter class to use for unknown types.
    """

    def __init__(self, fallback_class: Type[Splitter] | None = None) -> None:
        """Initialize the splitter registry.

        Args:
            fallback_class: Optional splitter class to use as fallback.
                           If None, LineBasedSplitter is used.
        """
        self._splitter_classes: dict[str, Type[Splitter]] = {}
        self._splitter_instances: dict[str, Splitter] = {}
        self._configs: dict[str, dict] = {}

        # Import here to avoid circular import
        from war_rig.chunking.line_based import LineBasedSplitter

        self._fallback_class: Type[Splitter] = fallback_class or LineBasedSplitter

    def register(
        self,
        splitter_class: Type[T],
        artifact_types: list[str] | None = None,
    ) -> None:
        """Register a splitter class for its declared artifact types.

        The splitter class must implement the get_artifact_types() class
        method, which returns the list of artifact types it handles.
        Alternatively, you can override the types by passing artifact_types.

        Args:
            splitter_class: The splitter class to register.
            artifact_types: Optional override for artifact types.
                           If None, uses splitter_class.get_artifact_types().

        Raises:
            ValueError: If splitter_class doesn't implement get_artifact_types()
                       and no artifact_types override is provided.

        Example:
            >>> registry.register(COBOLSplitter)
            >>> # Or with explicit types
            >>> registry.register(COBOLSplitter, ["cobol", "cob"])
        """
        types_to_register = artifact_types or splitter_class.get_artifact_types()

        if not types_to_register:
            raise ValueError(
                f"Splitter class {splitter_class.__name__} declares no artifact types "
                "and no override was provided"
            )

        for artifact_type in types_to_register:
            normalized_type = artifact_type.lower().strip()
            if normalized_type in self._splitter_classes:
                existing = self._splitter_classes[normalized_type]
                logger.warning(
                    "Overwriting splitter for artifact type '%s': %s -> %s",
                    normalized_type,
                    existing.__name__,
                    splitter_class.__name__,
                )
            self._splitter_classes[normalized_type] = splitter_class
            # Clear cached instance if type was previously instantiated
            self._splitter_instances.pop(normalized_type, None)
            logger.debug(
                "Registered splitter %s for artifact type '%s'",
                splitter_class.__name__,
                normalized_type,
            )

    def unregister(self, artifact_type: str) -> bool:
        """Unregister a splitter for an artifact type.

        Args:
            artifact_type: The artifact type to unregister.

        Returns:
            True if a splitter was unregistered, False if not found.
        """
        normalized_type = artifact_type.lower().strip()
        if normalized_type in self._splitter_classes:
            del self._splitter_classes[normalized_type]
            self._splitter_instances.pop(normalized_type, None)
            self._configs.pop(normalized_type, None)
            logger.debug("Unregistered splitter for artifact type '%s'", normalized_type)
            return True
        return False

    def set_config(self, artifact_type: str, config: dict) -> None:
        """Set configuration for a specific artifact type.

        Configuration is passed to the splitter during instantiation
        or can be used to customize splitting behavior.

        Args:
            artifact_type: The artifact type to configure.
            config: Configuration dictionary.
        """
        normalized_type = artifact_type.lower().strip()
        self._configs[normalized_type] = config
        # Clear cached instance to pick up new config
        self._splitter_instances.pop(normalized_type, None)

    def get_config(self, artifact_type: str) -> dict:
        """Get configuration for a specific artifact type.

        Args:
            artifact_type: The artifact type.

        Returns:
            Configuration dictionary, or empty dict if none set.
        """
        normalized_type = artifact_type.lower().strip()
        return self._configs.get(normalized_type, {})

    def get_splitter(
        self,
        artifact_type: str,
        use_fallback: bool = True,
    ) -> Splitter:
        """Get a splitter instance for the given artifact type.

        If no splitter is registered for the artifact type and use_fallback
        is True, returns an instance of the fallback splitter (LineBasedSplitter
        by default). Splitter instances are cached for reuse.

        Args:
            artifact_type: The artifact type to get a splitter for.
            use_fallback: If True, return fallback for unknown types.
                         If False, raise SplitterNotFoundError.

        Returns:
            Splitter instance for the artifact type.

        Raises:
            SplitterNotFoundError: If no splitter found and use_fallback is False.

        Example:
            >>> splitter = registry.get_splitter("cobol")
            >>> result = splitter.split(source, profile, "program.cbl")
        """
        normalized_type = artifact_type.lower().strip()

        # Check cache first
        if normalized_type in self._splitter_instances:
            return self._splitter_instances[normalized_type]

        # Look up splitter class
        splitter_class = self._splitter_classes.get(normalized_type)

        if splitter_class is None:
            if use_fallback:
                logger.debug(
                    "No splitter registered for artifact type '%s', using fallback %s",
                    artifact_type,
                    self._fallback_class.__name__,
                )
                splitter_class = self._fallback_class
            else:
                raise SplitterNotFoundError(
                    f"No splitter registered for artifact type '{artifact_type}'"
                )

        # Instantiate and cache
        splitter = splitter_class()
        self._splitter_instances[normalized_type] = splitter

        return splitter

    def get_splitter_class(self, artifact_type: str) -> Type[Splitter] | None:
        """Get the splitter class for an artifact type without instantiating.

        Args:
            artifact_type: The artifact type.

        Returns:
            Splitter class, or None if not registered.
        """
        normalized_type = artifact_type.lower().strip()
        return self._splitter_classes.get(normalized_type)

    def has_splitter(self, artifact_type: str) -> bool:
        """Check if a splitter is registered for an artifact type.

        Args:
            artifact_type: The artifact type to check.

        Returns:
            True if a splitter is registered, False otherwise.
        """
        normalized_type = artifact_type.lower().strip()
        return normalized_type in self._splitter_classes

    def list_registered_types(self) -> list[str]:
        """List all registered artifact types.

        Returns:
            List of registered artifact type strings.
        """
        return list(self._splitter_classes.keys())

    def clear(self) -> None:
        """Clear all registered splitters and cached instances."""
        self._splitter_classes.clear()
        self._splitter_instances.clear()
        self._configs.clear()

    def __contains__(self, artifact_type: str) -> bool:
        """Check if artifact type is registered (supports 'in' operator).

        Note: This normalizes the artifact type to lowercase and strips
        whitespace, so '  COBOL  ' in registry will check for 'cobol'.
        """
        return self.has_splitter(artifact_type)

    def __len__(self) -> int:
        """Return number of registered artifact types."""
        return len(self._splitter_classes)


# Global default registry instance
_default_registry: SplitterRegistry | None = None


def get_default_registry() -> SplitterRegistry:
    """Get the default global splitter registry.

    The default registry is lazily initialized and pre-populated with
    built-in splitters (COBOLSplitter, LineBasedSplitter).

    Returns:
        The default SplitterRegistry instance.

    Example:
        >>> from war_rig.chunking.registry import get_default_registry
        >>> registry = get_default_registry()
        >>> splitter = registry.get_splitter("cobol")
    """
    global _default_registry
    if _default_registry is None:
        _default_registry = SplitterRegistry()
        _register_builtin_splitters(_default_registry)
    return _default_registry


def _register_builtin_splitters(registry: SplitterRegistry) -> None:
    """Register all built-in splitters with the registry.

    Args:
        registry: The registry to populate.
    """
    # Import here to avoid circular imports
    from war_rig.chunking.cobol import COBOLSplitter
    from war_rig.chunking.line_based import LineBasedSplitter

    # Register COBOL splitter
    registry.register(COBOLSplitter)

    # LineBasedSplitter is the fallback, but we can also register it
    # explicitly for certain types if needed
    logger.debug("Registered built-in splitters with default registry")


def reset_default_registry() -> None:
    """Reset the default registry to None (for testing purposes).

    This clears the global default registry so it will be re-initialized
    on the next call to get_default_registry().
    """
    global _default_registry
    _default_registry = None
