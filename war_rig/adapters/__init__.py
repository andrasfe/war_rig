"""Atlas adapters for War Rig integration.

This package provides concrete implementations of Atlas's abstract
adapter interfaces, bridging Atlas orchestration to War Rig's
existing infrastructure.

Adapters:
    - BeadsTicketAdapter: Maps Atlas WorkItems to War Rig tickets
    - FileArtifactAdapter: Filesystem-based artifact storage
    - ScribeWorkerAdapter: Wraps ScribeAgent for chunk processing
    - AnalysisRouter: Routes files to direct or chunked processing

Factory Functions:
    - create_scribe_worker: Creates ScribeWorkerAdapter with agent
"""

from war_rig.adapters.analysis_router import (
    AnalysisRoute,
    AnalysisRouter,
    RouteDecision,
)
from war_rig.adapters.beads_ticket_adapter import BeadsTicketAdapter
from war_rig.adapters.file_artifact_adapter import FileArtifactAdapter
from war_rig.adapters.scribe_worker_adapter import (
    ScribeWorkerAdapter,
    create_scribe_worker,
)

__all__ = [
    "AnalysisRoute",
    "AnalysisRouter",
    "BeadsTicketAdapter",
    "FileArtifactAdapter",
    "RouteDecision",
    "ScribeWorkerAdapter",
    "create_scribe_worker",
]
