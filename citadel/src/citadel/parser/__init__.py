"""
Parser engine module for Citadel.

Contains:
- Preprocessor for comment/string stripping (preprocessor.py)
- Pattern-based extraction engine (engine.py)
- Scope tracking for nested artifacts (scope_tracker.py)
"""

from citadel.parser.engine import (
    AnalysisMatch,
    AnalysisPatternResult,
    AnalysisPatternStats,
    FileParseResult,
    ParserEngine,
    RawReference,
)
from citadel.parser.preprocessor import PreprocessedSource, Preprocessor

__all__ = [
    "AnalysisMatch",
    "AnalysisPatternResult",
    "AnalysisPatternStats",
    "FileParseResult",
    "ParserEngine",
    "PreprocessedSource",
    "Preprocessor",
    "RawReference",
]
