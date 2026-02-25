"""COBOL structural parser for citadel.

Provides deep structural parsing of IBM Enterprise COBOL fixed-format
source files, including:

- Fixed-format column parsing and COPY/copybook resolution
- DATA DIVISION parsing with DataItem hierarchy and byte lengths
- PROCEDURE DIVISION parsing with paragraph boundaries, EXEC blocks,
  variable read/write analysis, and complexity scoring
- Call graph construction with topological ordering

Adapted from cbexplore's rosetta.parse package.
"""

from citadel.cobol.call_graph import CallGraph, CallGraphBuilder
from citadel.cobol.data_division import DataDivisionParser, DataItem
from citadel.cobol.procedure_division import (
    ExecBlock,
    ParagraphInfo,
    ProcedureDivisionParser,
)
from citadel.cobol.source_reader import (
    CobolSource,
    CopybookNotFoundError,
    CopybookRecursionError,
    SourceLine,
    SourceReader,
)

__all__ = [
    "CallGraph",
    "CallGraphBuilder",
    "CobolSource",
    "CopybookNotFoundError",
    "CopybookRecursionError",
    "DataDivisionParser",
    "DataItem",
    "ExecBlock",
    "ParagraphInfo",
    "ProcedureDivisionParser",
    "SourceLine",
    "SourceReader",
]
