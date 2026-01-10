"""Agent implementations for War Rig.

This package provides the AI agent implementations that power the
documentation process:

- ScribeAgent: Analyzes code and produces documentation
- ChallengerAgent: Validates documentation and asks probing questions
- ImperatorAgent: Reviews and approves final documentation
- ProgramManagerAgent: Orchestrates parallel batch documentation workflows

All agents share a common base class and interface.

Example:
    from war_rig.agents import ScribeAgent, ChallengerAgent, ImperatorAgent
    from war_rig.config import WarRigConfig

    config = WarRigConfig()
    scribe = ScribeAgent(config.scribe)
    challenger = ChallengerAgent(config.challenger)
    imperator = ImperatorAgent(config.imperator)

    # For batch processing:
    from war_rig.agents import ProgramManagerAgent
    pm = ProgramManagerAgent(config)
    tickets = pm.initialize_batch(Path("./input"))
"""

from war_rig.agents.base import BaseAgent, AgentInput, AgentOutput
from war_rig.agents.scribe import ScribeAgent, ScribeInput, ScribeOutput
from war_rig.agents.challenger import ChallengerAgent, ChallengerInput, ChallengerOutput
from war_rig.agents.imperator import (
    ImperatorAgent,
    ImperatorInput,
    ImperatorOutput,
    ImperatorDecision,
)
from war_rig.agents.program_manager import (
    ProgramManagerAgent,
    ProgramManagerInput,
    ProgramManagerOutput,
    ClarificationRequest,
    CycleSummary,
    BatchTicketSummary,
)

__all__ = [
    "AgentInput",
    "AgentOutput",
    "BaseAgent",
    "BatchTicketSummary",
    "ChallengerAgent",
    "ChallengerInput",
    "ChallengerOutput",
    "ClarificationRequest",
    "CycleSummary",
    "ImperatorAgent",
    "ImperatorDecision",
    "ImperatorInput",
    "ImperatorOutput",
    "ProgramManagerAgent",
    "ProgramManagerInput",
    "ProgramManagerOutput",
    "ScribeAgent",
    "ScribeInput",
    "ScribeOutput",
]
