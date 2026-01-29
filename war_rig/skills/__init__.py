"""Agent Skills converter for War Rig documentation.

This module provides utilities for converting War Rig documentation output
into Agent Skills format for progressive discovery.

The main components are:

- SkillsGenerator: Converts War Rig output to Agent Skills directory structure
- ProgramSkillGenerator: Generates individual program skills from .doc.json
- OverviewSkillGenerator: Generates system overview skill
- CallGraphSkillGenerator: Generates call graph skill
- DatacardSkillGenerator: Generates datacards catalog skill
- normalize_skill_name: Converts filenames to valid skill names
- validate_skill_name: Validates skill names against the spec
- create_skill_frontmatter: Generates SKILL.md frontmatter
- truncate_description: Truncates descriptions to spec limits

Example:
    >>> from war_rig.skills import SkillsGenerator, normalize_skill_name
    >>> from pathlib import Path
    >>>
    >>> # Convert War Rig output to skills
    >>> generator = SkillsGenerator(Path("./output"))
    >>> skills_dir = generator.generate()
    >>>
    >>> # Normalize a program name to a skill name
    >>> skill_name = normalize_skill_name("CBACT01C.cbl.doc.json")
    >>> print(skill_name)  # 'cbact01c'
"""

from war_rig.skills.call_graph_skill import (
    CALL_GRAPH_SKILL_NAME,
    CallGraphSkillGenerationError,
    CallGraphSkillGenerator,
)
from war_rig.skills.datacard_skill import (
    DATACARDS_SKILL_NAME,
    DatacardSkillGenerationError,
    DatacardSkillGenerator,
)
from war_rig.skills.generator import (
    InputDirectoryNotFoundError,
    InvalidInputDirectoryError,
    SkillsGenerator,
    SkillsGeneratorError,
)
from war_rig.skills.naming import (
    MAX_DESCRIPTION_LENGTH,
    MAX_SKILL_NAME_LENGTH,
    create_skill_frontmatter,
    normalize_skill_name,
    truncate_description,
    validate_skill_name,
)
from war_rig.skills.overview_skill import (
    SYSTEM_OVERVIEW_SKILL_NAME,
    OverviewSkillGenerationError,
    OverviewSkillGenerator,
)
from war_rig.skills.program_skill import (
    ProgramSkillGenerationError,
    ProgramSkillGenerator,
)

__all__ = [
    # Main Generator
    "SkillsGenerator",
    "SkillsGeneratorError",
    "InputDirectoryNotFoundError",
    "InvalidInputDirectoryError",
    # Program Skill Generator
    "ProgramSkillGenerator",
    "ProgramSkillGenerationError",
    # Overview Skill Generator
    "OverviewSkillGenerator",
    "OverviewSkillGenerationError",
    "SYSTEM_OVERVIEW_SKILL_NAME",
    # Call Graph Skill Generator
    "CallGraphSkillGenerator",
    "CallGraphSkillGenerationError",
    "CALL_GRAPH_SKILL_NAME",
    # Datacard Skill Generator
    "DatacardSkillGenerator",
    "DatacardSkillGenerationError",
    "DATACARDS_SKILL_NAME",
    # Naming utilities
    "normalize_skill_name",
    "validate_skill_name",
    "create_skill_frontmatter",
    "truncate_description",
    # Constants
    "MAX_SKILL_NAME_LENGTH",
    "MAX_DESCRIPTION_LENGTH",
]
