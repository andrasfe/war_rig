"""Doof Wagon integration for Citadel.

Citadel's role in Doof Wagon is to answer asynchronous queries about field
sources, PERFORM targets, and copybook propagation. Queries arrive as JSON
files in a watched directory; responses are written to the callback path
each query declares.

This package implements the watcher + stub resolver. Real resolution against
Citadel's existing graph engine is a separate delivery; the stub responds
with a structurally-valid PENDING result so dispatchers don't block.
"""

from .watcher import Watcher, process_query_file

__all__ = ["Watcher", "process_query_file"]
