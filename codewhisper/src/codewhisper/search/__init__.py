"""Search module for CodeWhisper.

This module provides code search capabilities for the agent,
allowing it to find patterns, references, and implementations
in the source codebase.

Components:
    - code_search: Search for patterns in source files
"""

from codewhisper.search.code_search import CodeSearcher, search_in_directory

__all__ = [
    "CodeSearcher",
    "search_in_directory",
]
