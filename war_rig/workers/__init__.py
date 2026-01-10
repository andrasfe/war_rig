"""Worker pools for parallel processing in War Rig.

This module provides worker pool implementations for the Program Manager
workflow, enabling parallel processing of documentation tickets.

Worker Types:
- ScribeWorker: Processes DOCUMENTATION, CLARIFICATION, and CHROME tickets
- ScribeWorkerPool: Manages multiple ScribeWorker instances
- ChallengerWorker: Validates completed documentation via VALIDATION tickets
- ChallengerWorkerPool: Manages multiple ChallengerWorker instances

Usage:
    from war_rig.workers import ScribeWorkerPool, ChallengerWorkerPool

    scribe_pool = ScribeWorkerPool(config=config, beads_client=client)
    challenger_pool = ChallengerWorkerPool(
        num_workers=config.num_challengers,
        config=config,
        beads_client=client,
    )
    await scribe_pool.start()
    await challenger_pool.start()
    # Workers process tickets until none remain
    await challenger_pool.stop()
    await scribe_pool.stop()

See Also:
    - war_rig.beads: Ticket types and BeadsClient
    - war_rig.agents.scribe: ScribeAgent for documentation generation
    - war_rig.agents.challenger: ChallengerAgent for validation
    - docs/program_manager_architecture.md: Architecture overview
"""

from war_rig.workers.challenger_pool import (
    ChallengerWorker,
    ChallengerWorkerPool,
    ValidationResult,
)
from war_rig.workers.scribe_pool import (
    ScribeWorker,
    ScribeWorkerPool,
    WorkerState,
    WorkerStatus,
)

__all__ = [
    # Scribe workers
    "ScribeWorker",
    "ScribeWorkerPool",
    # Challenger workers
    "ChallengerWorker",
    "ChallengerWorkerPool",
    "ValidationResult",
    # Shared types
    "WorkerState",
    "WorkerStatus",
]
