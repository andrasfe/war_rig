"""
Reference resolution module for Citadel.

Contains:
- Artifact registry with indices (registry.py)
- Alias transformation rules (alias_resolver.py)
- Cross-reference binding (cross_reference.py)
"""

from citadel.resolver.alias_resolver import TRANSFORMATIONS, AliasResolver
from citadel.resolver.cross_reference import CrossReferenceResolver
from citadel.resolver.registry import ArtifactRegistry

__all__ = [
    "ArtifactRegistry",
    "AliasResolver",
    "CrossReferenceResolver",
    "TRANSFORMATIONS",
]
