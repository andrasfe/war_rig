"""Preprocessors for extracting structural information from source code.

This package provides deterministic (non-LLM) extraction of structural
information from mainframe source files. The extracted information serves
as hints for the Scribe agent.

Available preprocessors:
- COBOLPreprocessor: Extract COBOL program structure
- JCLPreprocessor: Extract JCL job structure
- CopybookPreprocessor: Extract copybook record layouts

Example:
    from war_rig.preprocessors import COBOLPreprocessor

    preprocessor = COBOLPreprocessor()
    result = preprocessor.process(source_code)
    print(result.paragraphs)
"""

from war_rig.preprocessors.base import (
    BasePreprocessor,
    PreprocessorResult,
)
from war_rig.preprocessors.cobol import (
    COBOLPreprocessor,
    COBOLStructure,
)
from war_rig.preprocessors.copybook import (
    CopybookPreprocessor,
    CopybookStructure,
)
from war_rig.preprocessors.jcl import (
    JCLPreprocessor,
    JCLStructure,
)

__all__ = [
    "BasePreprocessor",
    "COBOLPreprocessor",
    "COBOLStructure",
    "CopybookPreprocessor",
    "CopybookStructure",
    "JCLPreprocessor",
    "JCLStructure",
    "PreprocessorResult",
]
