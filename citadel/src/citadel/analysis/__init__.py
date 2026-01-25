"""
Analysis module for Citadel dependency graph.

This module provides algorithms for analyzing dependency graphs,
including finding call sequences for generating sequence diagrams.
"""

from citadel.analysis.sequence_finder import find_longest_sequences

__all__ = ["find_longest_sequences"]
