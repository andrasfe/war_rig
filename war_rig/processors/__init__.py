"""Processors for special file types.

This module provides batch processors for file types that require
specialized handling rather than the standard Scribe→Challenger→Imperator flow.
"""

from war_rig.processors.datacard import (
    DatacardCatalog,
    DatacardInfo,
    DatacardProcessor,
    process_datacards,
)

__all__ = [
    "DatacardCatalog",
    "DatacardInfo",
    "DatacardProcessor",
    "process_datacards",
]
