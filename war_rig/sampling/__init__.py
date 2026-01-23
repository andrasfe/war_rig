"""Intelligent source code sampling for War Rig.

This module provides intelligent source code sampling for CLARIFICATION
and CHROME tickets. Instead of random sampling, it extracts relevance
hints from Challenger questions and Chrome tickets to sample the most
relevant portions of source code.

Architecture:
    The sampling system has these main components:

    1. SamplingContext - Holds all information needed for sampling decisions
    2. RelevanceAnalyzer - Extracts hints from questions/tickets
    3. Sampling Strategies - Generate source windows based on hints:
       - LineCitationStrategy (priority 1): Windows around cited line numbers
       - SectionReferenceStrategy (priority 2): Windows from template citations
       - IdentifierMentionStrategy (priority 3): Search for mentioned identifiers
       - SemanticRegionStrategy (priority 4): COBOL division-based sampling
       - RandomFallbackStrategy (priority 99): Fallback when no hints
    4. WindowComposer - Merges windows and fits them to token budget
    5. SamplingOrchestrator - Coordinates the entire process

Usage:
    from war_rig.sampling import SamplingOrchestrator
    from war_rig.chunking import TokenEstimator

    estimator = TokenEstimator()
    orchestrator = SamplingOrchestrator(estimator)

    result = orchestrator.prepare_sample(
        source_code=source,
        file_name="CBACT04C.cbl",
        file_type=FileType.COBOL,
        max_tokens=11000,
        ticket_type=TicketType.CLARIFICATION,
        challenger_questions=questions,
        previous_template=template,
    )

    # result.source_code contains the intelligently sampled source
    # result.strategies_used shows which strategies contributed

Integration:
    The SamplingOrchestrator is used by SourceCodePreparer when processing
    CLARIFICATION and CHROME tickets. The _apply_sampling method checks
    the ticket type and delegates to intelligent sampling when appropriate.

See Also:
    - war_rig.workers.source_preparer: Integration point
    - war_rig.models.tickets: ChallengerQuestion, ChromeTicket models
    - war_rig.models.templates: DocumentationTemplate with citations
"""

from war_rig.sampling.analyzer import RelevanceAnalyzer, RelevanceHints
from war_rig.sampling.composer import ComposedSample, WindowComposer
from war_rig.sampling.context import SamplingContext
from war_rig.sampling.orchestrator import SamplingOrchestrator, SamplingResult
from war_rig.sampling.strategies import (
    IdentifierMentionStrategy,
    LineCitationStrategy,
    RandomFallbackStrategy,
    SamplingStrategy,
    SectionReferenceStrategy,
    SemanticRegionStrategy,
    SourceWindow,
)

__all__ = [
    # Main orchestrator
    "SamplingOrchestrator",
    "SamplingResult",
    # Context and hints
    "SamplingContext",
    "RelevanceAnalyzer",
    "RelevanceHints",
    # Composer
    "WindowComposer",
    "ComposedSample",
    # Strategies
    "SamplingStrategy",
    "SourceWindow",
    "LineCitationStrategy",
    "SectionReferenceStrategy",
    "IdentifierMentionStrategy",
    "SemanticRegionStrategy",
    "RandomFallbackStrategy",
]
