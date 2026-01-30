"""Analysis modules for War Rig.

This package contains analysis tools for understanding codebase structure
and relationships between programs.
"""

from war_rig.analysis.call_graph import (
    SYSTEM_UTILITIES,
    CallGraphAnalysis,
    CallGraphAnalyzer,
    CallRelationship,
    ProgramInfo,
)
from war_rig.analysis.call_semantics import CallSemanticsAnalyzer
from war_rig.analysis.pattern_aggregator import PatternAggregator

__all__ = [
    "CallGraphAnalyzer",
    "CallGraphAnalysis",
    "CallRelationship",
    "CallSemanticsAnalyzer",
    "PatternAggregator",
    "ProgramInfo",
    "SYSTEM_UTILITIES",
]
