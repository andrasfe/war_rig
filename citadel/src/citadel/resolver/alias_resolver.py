"""
Alias resolver for Citadel.

This module provides the AliasResolver class for resolving references to artifacts
using transformation rules, alias mappings, and fuzzy matching. It implements the
resolution cascade: exact match -> alias match -> transformation match -> fuzzy match.
"""

import logging
import re
from collections.abc import Callable

from citadel.parser.engine import RawReference
from citadel.resolver.registry import ArtifactRegistry
from citadel.specs.schema import AliasRule, ArtifactType

logger = logging.getLogger(__name__)


# Built-in transformations for alias resolution
TRANSFORMATIONS: dict[str, Callable[[str], str]] = {
    "uppercase": lambda s: s.upper(),
    "lowercase": lambda s: s.lower(),
    "remove_hyphens": lambda s: s.replace("-", ""),
    "remove_underscores": lambda s: s.replace("_", ""),
    "hyphens_to_underscores": lambda s: s.replace("-", "_"),
    "underscores_to_hyphens": lambda s: s.replace("_", "-"),
    "truncate_8": lambda s: s[:8],
    "truncate_6": lambda s: s[:6],
}


def _parse_transformation(transform_spec: str) -> Callable[[str], str] | None:
    """
    Parse a transformation specification string into a callable.

    Handles built-in transformations and parameterized ones like:
    - truncate_N: Truncate to N characters
    - add_prefix_X: Add prefix X
    - add_suffix_X: Add suffix X
    - remove_suffix_X: Remove suffix X if present
    - remove_prefix_X: Remove prefix X if present

    Args:
        transform_spec: The transformation specification string.

    Returns:
        A callable that performs the transformation, or None if unknown.
    """
    # Check built-in transformations first
    if transform_spec in TRANSFORMATIONS:
        return TRANSFORMATIONS[transform_spec]

    # Handle parameterized transformations
    truncate_match = re.match(r"truncate_(\d+)", transform_spec)
    if truncate_match:
        length = int(truncate_match.group(1))
        return lambda s, n=length: s[:n]

    prefix_match = re.match(r"add_prefix_(.+)", transform_spec)
    if prefix_match:
        prefix = prefix_match.group(1)
        return lambda s, p=prefix: p + s

    suffix_match = re.match(r"add_suffix_(.+)", transform_spec)
    if suffix_match:
        suffix = suffix_match.group(1)
        return lambda s, x=suffix: s + x

    remove_suffix_match = re.match(r"remove_suffix_(.+)", transform_spec)
    if remove_suffix_match:
        suffix = remove_suffix_match.group(1)
        return lambda s, x=suffix: s[: -len(x)] if s.endswith(x) else s

    remove_prefix_match = re.match(r"remove_prefix_(.+)", transform_spec)
    if remove_prefix_match:
        prefix = remove_prefix_match.group(1)
        return lambda s, p=prefix: s[len(p) :] if s.startswith(p) else s

    # Handle extract_last_qualifier for dataset names (PROD.DATA.NAME -> NAME)
    if transform_spec == "extract_last_qualifier":
        return lambda s: s.split(".")[-1] if "." in s else s

    # Handle remove_suffixes for common record suffixes
    if transform_spec == "remove_suffixes":

        def remove_common_suffixes(s: str) -> str:
            """Remove common copybook/record suffixes."""
            suffixes = ["-REC", "-RECORD", "-LAYOUT", "-CPY", "-COPY", "_REC", "_RECORD"]
            upper_s = s.upper()
            for suffix in suffixes:
                if upper_s.endswith(suffix):
                    return s[: -len(suffix)]
            return s

        return remove_common_suffixes

    logger.warning("Unknown transformation: %s", transform_spec)
    return None


class AliasResolver:
    """
    Resolves references to artifacts using transformation rules.

    Resolution is attempted in the following order:
    1. Exact match - Direct lookup in registry by ID or canonical name
    2. Alias match - Lookup via registered aliases
    3. Transformation match - Apply transformation rules to find matches
    4. Fuzzy match - Use fuzzy string matching as a last resort

    The resolver maintains transformation rules that define how reference names
    from one artifact type can be transformed to match artifact names of another
    type (e.g., COBOL SQL references to DDL table names).
    """

    def __init__(self, registry: ArtifactRegistry, rules: list[AliasRule]) -> None:
        """
        Initialize the alias resolver.

        Args:
            registry: The artifact registry to search for matches.
            rules: List of alias transformation rules to apply.
        """
        self._registry = registry
        self._rules = rules
        self._transformation_cache: dict[str, Callable[[str], str] | None] = {}

    def resolve(
        self, reference: RawReference
    ) -> tuple[str | None, float, str]:
        """
        Attempt to resolve a reference to an artifact.

        Tries resolution methods in order: exact, alias, transformation, fuzzy.
        Returns as soon as a match is found.

        Args:
            reference: The raw reference to resolve.

        Returns:
            A tuple of (artifact_id, confidence, method) where:
            - artifact_id: The resolved artifact ID, or None if unresolved
            - confidence: Confidence score (0.0 to 1.0)
            - method: The resolution method used ("exact", "alias",
              "transformation", "fuzzy") or a reason for failure
        """
        name = reference.raw_text
        expected_type = reference.expected_type

        # Step 1: Try exact match
        artifact_id = self._try_exact(name, expected_type)
        if artifact_id is not None:
            logger.debug("Resolved %s via exact match: %s", name, artifact_id)
            return (artifact_id, 1.0, "exact")

        # Step 2: Try alias match
        result = self._try_alias(name, expected_type)
        if result is not None:
            artifact_id, confidence = result
            logger.debug(
                "Resolved %s via alias match: %s (confidence=%.2f)",
                name,
                artifact_id,
                confidence,
            )
            return (artifact_id, confidence, "alias")

        # Step 3: Try transformation rules
        result = self._try_transformations(
            name,
            source_type=None,  # Could use containing_artifact type if available
            target_type=expected_type,
        )
        if result is not None:
            artifact_id, confidence = result
            logger.debug(
                "Resolved %s via transformation: %s (confidence=%.2f)",
                name,
                artifact_id,
                confidence,
            )
            return (artifact_id, confidence, "transformation")

        # Step 4: Try fuzzy match
        result = self._try_fuzzy(name, expected_type)
        if result is not None:
            artifact_id, confidence = result
            logger.debug(
                "Resolved %s via fuzzy match: %s (confidence=%.2f)",
                name,
                artifact_id,
                confidence,
            )
            return (artifact_id, confidence, "fuzzy")

        # Resolution failed
        logger.debug("Failed to resolve reference: %s (type=%s)", name, expected_type)
        return (None, 0.0, f"no match found for '{name}'")

    def _try_exact(
        self, name: str, expected_type: ArtifactType | None
    ) -> str | None:
        """
        Try exact match by ID or canonical name.

        Args:
            name: The name to look up.
            expected_type: Optional type filter for the lookup.

        Returns:
            Artifact ID if found with confidence 1.0, None otherwise.
        """
        # Use registry lookup which checks ID, canonical name, and type
        results = self._registry.lookup(name, expected_type)

        # Only return exact matches (confidence 1.0)
        for artifact, confidence in results:
            if confidence == 1.0:
                return artifact.id

        return None

    def _try_alias(
        self, name: str, expected_type: ArtifactType | None
    ) -> tuple[str, float] | None:
        """
        Try registered aliases in the registry.

        The registry's lookup method already checks aliases, but we need to
        return results with confidence < 1.0 (alias matches).

        Args:
            name: The name to look up as an alias.
            expected_type: Optional type filter for the lookup.

        Returns:
            Tuple of (artifact_id, confidence) if found via alias, None otherwise.
        """
        results = self._registry.lookup(name, expected_type)

        # Return alias matches (confidence < 1.0 but still a valid alias)
        for artifact, confidence in results:
            if confidence < 1.0:
                return (artifact.id, confidence)

        return None

    def _try_transformations(
        self,
        name: str,
        source_type: ArtifactType | None,
        target_type: ArtifactType | None,
    ) -> tuple[str, float] | None:
        """
        Apply transformation rules to find matches.

        Filters applicable rules based on source and target types, then
        applies each transformation in sequence to generate candidate names.
        Looks up each transformed name in the registry.

        Args:
            name: The original reference name.
            source_type: The type of the artifact containing the reference.
            target_type: The expected type of the referenced artifact.

        Returns:
            Tuple of (artifact_id, confidence) if a transformed match is found,
            None otherwise.
        """
        for rule in self._rules:
            # Check if rule applies to this resolution
            if not self._rule_applies(rule, source_type, target_type):
                continue

            # Generate transformed name
            transformed = self._apply_transformations(name, rule.transformations)
            if transformed is None:
                continue

            # Look up the transformed name
            results = self._registry.lookup(transformed, rule.target_type)
            if results:
                best_artifact, base_confidence = results[0]
                # Combine registry confidence with rule's base confidence
                final_confidence = rule.confidence_base * base_confidence
                return (best_artifact.id, final_confidence)

            # Also try the transformed name as an artifact ID pattern
            # e.g., "table::CUSTOMER_MASTER"
            if target_type:
                potential_id = f"{target_type.value}::{transformed}"
                artifact = self._registry.lookup_exact(potential_id)
                if artifact is not None:
                    return (artifact.id, rule.confidence_base)

        return None

    def _rule_applies(
        self,
        rule: AliasRule,
        source_type: ArtifactType | None,
        target_type: ArtifactType | None,
    ) -> bool:
        """
        Check if a rule applies to the given source and target types.

        Args:
            rule: The alias rule to check.
            source_type: The source artifact type.
            target_type: The target artifact type.

        Returns:
            True if the rule applies, False otherwise.
        """
        # Check source type (None in rule means any source)
        if rule.source_type is not None and source_type is not None:
            if rule.source_type != source_type:
                return False

        # Check target type (must match if specified in both)
        if target_type is not None and rule.target_type != target_type:
            return False

        return True

    def _apply_transformations(
        self, name: str, transformations: list[str]
    ) -> str | None:
        """
        Apply a sequence of transformations to a name.

        Args:
            name: The original name.
            transformations: List of transformation specifications to apply in order.

        Returns:
            The transformed name, or None if any transformation failed.
        """
        result = name
        for transform_spec in transformations:
            transform_fn = self._get_transformation(transform_spec)
            if transform_fn is None:
                logger.warning(
                    "Skipping unknown transformation '%s' in sequence",
                    transform_spec,
                )
                continue
            try:
                result = transform_fn(result)
            except Exception as e:
                logger.warning(
                    "Transformation '%s' failed on '%s': %s",
                    transform_spec,
                    result,
                    e,
                )
                return None
        return result

    def _get_transformation(
        self, transform_spec: str
    ) -> Callable[[str], str] | None:
        """
        Get a transformation function by specification, with caching.

        Args:
            transform_spec: The transformation specification string.

        Returns:
            The transformation callable, or None if unknown.
        """
        if transform_spec not in self._transformation_cache:
            self._transformation_cache[transform_spec] = _parse_transformation(
                transform_spec
            )
        return self._transformation_cache[transform_spec]

    def _try_fuzzy(
        self,
        name: str,
        expected_type: ArtifactType | None,
        threshold: float = 0.8,
    ) -> tuple[str, float] | None:
        """
        Try fuzzy matching as a last resort.

        Uses the registry's fuzzy matching capability to find similar names.
        Only returns matches above the specified threshold.

        Args:
            name: The name to fuzzy match.
            expected_type: Optional type filter for the lookup.
            threshold: Minimum similarity threshold (default 0.8).

        Returns:
            Tuple of (artifact_id, confidence) if a fuzzy match is found
            above threshold, None otherwise.
        """
        results = self._registry.lookup_fuzzy(name, expected_type, threshold)

        if results:
            best_artifact, confidence = results[0]
            return (best_artifact.id, confidence)

        return None

    def add_rule(self, rule: AliasRule) -> None:
        """
        Add a new transformation rule.

        Args:
            rule: The alias rule to add.
        """
        self._rules.append(rule)
        logger.debug(
            "Added alias rule: %s -> %s with transformations %s",
            rule.source_type,
            rule.target_type,
            rule.transformations,
        )

    def get_rules(self) -> list[AliasRule]:
        """
        Get all registered transformation rules.

        Returns:
            List of alias rules.
        """
        return list(self._rules)

    def clear_cache(self) -> None:
        """Clear the transformation function cache."""
        self._transformation_cache.clear()
