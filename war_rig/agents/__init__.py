"""Agent implementations for War Rig.

This package provides the AI agent implementations that power the
documentation process:

- ScribeAgent: Analyzes code and produces documentation
- ChallengerAgent: Validates documentation and asks probing questions
- ImperatorAgent: Reviews and approves final documentation

All agents share a common base class and interface.

Example:
    from war_rig.agents import ScribeAgent, ChallengerAgent, ImperatorAgent
    from war_rig.config import WarRigConfig

    config = WarRigConfig()
    scribe = ScribeAgent(config.scribe)
    challenger = ChallengerAgent(config.challenger)
    imperator = ImperatorAgent(config.imperator)
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

__all__ = [
    "AgentInput",
    "AgentOutput",
    "BaseAgent",
    "ChallengerAgent",
    "ChallengerInput",
    "ChallengerOutput",
    "ImperatorAgent",
    "ImperatorDecision",
    "ImperatorInput",
    "ImperatorOutput",
    "ScribeAgent",
    "ScribeInput",
    "ScribeOutput",
]
