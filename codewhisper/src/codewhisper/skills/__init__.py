"""Skills module for CodeWhisper.

This module handles loading, indexing, and generating skill files -
structured documentation about programs, subsystems, and concepts
that the agent can access to answer questions.

Skills are markdown files with YAML frontmatter containing metadata.

Components:
    - loader: Load skill files from directories
    - index: Index and search skills by keyword
    - generator: Generate skill files from War Rig documentation
"""

from codewhisper.skills.generator import (
    GenerationResult,
    SkillGenerator,
    SkillGeneratorConfig,
)
from codewhisper.skills.index import SkillsIndex
from codewhisper.skills.loader import Skill, SkillsLoader

__all__ = [
    "GenerationResult",
    "Skill",
    "SkillGenerator",
    "SkillGeneratorConfig",
    "SkillsIndex",
    "SkillsLoader",
]
