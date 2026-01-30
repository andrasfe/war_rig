"""Sampling strategies for intelligent source code sampling.

This package provides various strategies for sampling source code based
on relevance hints extracted from Challenger questions and Chrome tickets.

Strategies are applied in priority order:
1. LineCitationStrategy - Windows around explicitly cited line numbers
2. SectionReferenceStrategy - Windows from template section citations
3. IdentifierMentionStrategy - Search source for mentioned identifiers
4. SemanticRegionStrategy - COBOL division/section based on topic
5. RandomFallbackStrategy - Random sampling when no hints available

Each strategy implements the SamplingStrategy protocol and produces
SourceWindow objects that the WindowComposer merges into a final sample.
"""

from war_rig.sampling.strategies.identifier_mention import IdentifierMentionStrategy
from war_rig.sampling.strategies.line_citation import LineCitationStrategy
from war_rig.sampling.strategies.protocol import (
    SamplingStrategy,
    SourceWindow,
)
from war_rig.sampling.strategies.random_fallback import RandomFallbackStrategy
from war_rig.sampling.strategies.section_reference import SectionReferenceStrategy
from war_rig.sampling.strategies.semantic_region import SemanticRegionStrategy

__all__ = [
    "SamplingStrategy",
    "SourceWindow",
    "LineCitationStrategy",
    "SectionReferenceStrategy",
    "IdentifierMentionStrategy",
    "SemanticRegionStrategy",
    "RandomFallbackStrategy",
]
