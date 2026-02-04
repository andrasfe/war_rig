"""Skills module for CodeWhisper.

This module handles loading and indexing skill files -
structured documentation about programs, subsystems, and concepts
that the agent can access to answer questions.

Skills are markdown files with YAML frontmatter containing metadata.

For skill generation, use war_rig.skills.SkillsGenerator.

Components:
    - loader: Load skill files from directories
    - index: Index and search skills by keyword
"""

from codewhisper.skills.index import SkillsIndex
from codewhisper.skills.loader import Skill, SkillsLoader

__all__ = [
    "Skill",
    "SkillsIndex",
    "SkillsLoader",
]
