"""
Analysis module for Citadel dependency graph.

This module provides algorithms for analyzing dependency graphs,
including finding call sequences for generating sequence diagrams,
detecting dead (unreferenced) code, and generating flow diagrams.
"""

from citadel.analysis.dead_code import find_dead_code
from citadel.analysis.flow_diagram import generate_flow_diagram
from citadel.analysis.sequence_finder import find_longest_sequences

__all__ = ["find_dead_code", "find_longest_sequences", "generate_flow_diagram"]
