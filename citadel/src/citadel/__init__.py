"""
Citadel - Universal Dependency Graph Builder.

A tool for analyzing source code repositories containing mixed languages
to produce a comprehensive graph of code-to-code, code-to-data, and
interface relationships.

Uses LLM-generated language/artifact specifications combined with
deterministic parsing for reliability and performance.

SDK Usage:
    from citadel import Citadel, analyze_file, get_functions

    # Quick one-off analysis
    result = analyze_file("program.cbl")
    for artifact in result.artifacts:
        print(f"{artifact.name}: {len(artifact.callouts)} callouts")

    # Or use the class for multiple files
    citadel = Citadel()
    funcs = citadel.get_functions("program.cbl")
"""

__version__ = "0.1.0"

# SDK exports
from citadel.sdk import (
    Citadel,
    FileAnalysisResult,
    FileArtifact,
    Callout,
    analyze_file,
    get_functions,
    get_function_body,
    get_callers,
)

__all__ = [
    "Citadel",
    "FileAnalysisResult",
    "FileArtifact",
    "Callout",
    "analyze_file",
    "get_functions",
    "get_function_body",
    "get_callers",
    "__version__",
]
