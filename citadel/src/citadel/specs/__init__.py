"""
Artifact specifications module for Citadel.

Contains:
- Pydantic models for spec schema (schema.py)
- Spec manager for loading and caching specs (manager.py)
- LLM-based spec generator (generator.py)
"""

from citadel.specs.manager import SpecManager
from citadel.specs.schema import (
    AliasRule,
    AnalysisPattern,
    ArtifactCategory,
    ArtifactSpec,
    ArtifactType,
    CommentSyntax,
    ContinuationSyntax,
    ExtractionPattern,
    NamingConvention,
    RelationshipType,
    ScopeSyntax,
    StringSyntax,
)

__all__ = [
    "AliasRule",
    "AnalysisPattern",
    "ArtifactCategory",
    "ArtifactSpec",
    "ArtifactType",
    "CommentSyntax",
    "ContinuationSyntax",
    "ExtractionPattern",
    "NamingConvention",
    "RelationshipType",
    "ScopeSyntax",
    "SpecManager",
    "StringSyntax",
]
