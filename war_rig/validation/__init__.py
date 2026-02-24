"""Validation module for War Rig documentation quality assurance.

This module provides validation criteria and utilities for assessing
the quality and completeness of generated documentation.
"""

from war_rig.validation.citation_validator import (
    CitationFinding,
    CitationIssueType,
    CitationValidationResult,
    CitationValidator,
)
from war_rig.validation.document_criteria import (
    BMS_CRITERIA,
    COBOL_CRITERIA,
    COPYBOOK_CRITERIA,
    CRITERIA_REGISTRY,
    JCL_CRITERIA,
    PLI_CRITERIA,
    PROC_CRITERIA,
    DocumentTypeCriteria,
    Priority,
    ValidationCriterion,
    get_criteria_for_type,
)
from war_rig.validation.document_validator import DocumentValidator, ValidationResult
from war_rig.validation.mermaid_validator import (
    is_valid_mermaid,
    sanitize_mermaid_blocks,
)

__all__ = [
    # Criteria classes
    "ValidationCriterion",
    "DocumentTypeCriteria",
    "Priority",
    # File type criteria
    "JCL_CRITERIA",
    "COBOL_CRITERIA",
    "COPYBOOK_CRITERIA",
    "PLI_CRITERIA",
    "BMS_CRITERIA",
    "PROC_CRITERIA",
    # Registry and lookup
    "CRITERIA_REGISTRY",
    "get_criteria_for_type",
    # Validator
    "DocumentValidator",
    "ValidationResult",
    # Citation validator
    "CitationValidator",
    "CitationValidationResult",
    "CitationFinding",
    "CitationIssueType",
    # Mermaid
    "is_valid_mermaid",
    "sanitize_mermaid_blocks",
]
