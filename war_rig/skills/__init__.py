"""Agent Skills converter for War Rig documentation.

This module provides utilities for converting War Rig documentation output
into Agent Skills format for progressive discovery.

The main component is:

- SkillsGenerator: Converts War Rig documentation to hierarchical skills

The generator creates category-based skills (cobol/, jcl/, ims/, etc.)
with program summaries and links to full documentation.

Example:
    >>> from war_rig.skills import SkillsGenerator
    >>> from pathlib import Path
    >>>
    >>> # Convert War Rig documentation to skills
    >>> generator = SkillsGenerator(Path("./output/documentation"))
    >>> skills_dir = generator.generate()
    >>> print(f"Skills generated at: {skills_dir}")
"""

from war_rig.skills.generator import (
    CATEGORY_DESCRIPTIONS,
    CATEGORY_MAPPING,
    GenerationResult,
    InputDirectoryNotFoundError,
    InvalidInputDirectoryError,
    SkillsGenerator,
    SkillsGeneratorError,
    get_markdown_summary,
)
from war_rig.skills.naming import (
    MAX_DESCRIPTION_LENGTH,
    MAX_SKILL_NAME_LENGTH,
    create_skill_frontmatter,
    normalize_skill_name,
    truncate_description,
    validate_skill_name,
)

__all__ = [
    # Main Generator
    "SkillsGenerator",
    "SkillsGeneratorError",
    "InputDirectoryNotFoundError",
    "InvalidInputDirectoryError",
    "GenerationResult",
    # Category mappings
    "CATEGORY_MAPPING",
    "CATEGORY_DESCRIPTIONS",
    # Utility functions
    "get_markdown_summary",
    # Naming utilities
    "normalize_skill_name",
    "validate_skill_name",
    "create_skill_frontmatter",
    "truncate_description",
    # Constants
    "MAX_SKILL_NAME_LENGTH",
    "MAX_DESCRIPTION_LENGTH",
]
