"""Human feedback injection module for War Rig.

This module provides tools for injecting human feedback into War Rig tickets
before agents process them. Human feedback can supplement or override
Imperator-generated feedback.

Main components:
- HumanFeedbackNote: A quality observation from a human reviewer
- HumanFeedbackContext: Collection of human feedback for injection
- FeedbackInjector: Service class for injecting feedback into tickets
"""

from war_rig.feedback.models import (
    HumanFeedbackCategory,
    HumanFeedbackNote,
    HumanFeedbackContext,
)
from war_rig.feedback.injector import FeedbackInjector

__all__ = [
    "HumanFeedbackCategory",
    "HumanFeedbackNote",
    "HumanFeedbackContext",
    "FeedbackInjector",
]
