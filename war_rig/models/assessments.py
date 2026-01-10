"""Assessment models for War Rig documentation quality.

This module defines models for tracking confidence levels and validation
assessments throughout the documentation process:

- ConfidenceAssessment: Scribe's self-assessment of documentation quality
- ValidationAssessment: Challenger's validation of documentation accuracy
- SectionAssessment: Per-section validation details

These assessments guide the Imperator's approval decisions.
"""

from datetime import datetime
from enum import Enum

from pydantic import BaseModel, Field, computed_field


class ConfidenceLevel(str, Enum):
    """Confidence levels for documentation quality.

    Used by Scribe to indicate certainty about documented information.
    """

    HIGH = "HIGH"  # Very confident, well-supported by code
    MEDIUM = "MEDIUM"  # Reasonably confident, some uncertainty
    LOW = "LOW"  # Uncertain, may need SME verification


class ValidationLevel(str, Enum):
    """Validation levels for Challenger assessments.

    Indicates the Challenger's evaluation of documentation accuracy.
    """

    SOLID = "SOLID"  # Documentation is accurate and complete
    SHAKY = "SHAKY"  # Some concerns but mostly acceptable
    WRONG = "WRONG"  # Factual errors or significant problems


class SectionAssessment(BaseModel):
    """Challenger's assessment of a specific documentation section.

    Provides detailed validation feedback for individual sections
    of the documentation template.
    """

    section_name: str = Field(
        ...,
        description="Name of the template section assessed",
    )
    validation_level: ValidationLevel = Field(
        ...,
        description="Overall validation of this section",
    )
    issues: list[str] = Field(
        default_factory=list,
        description="Specific issues found in this section",
    )
    suggestions: list[str] = Field(
        default_factory=list,
        description="Suggestions for improvement",
    )
    evidence: list[int] = Field(
        default_factory=list,
        description="Line numbers supporting the assessment",
    )

    @property
    def has_issues(self) -> bool:
        """Check if this section has any issues.

        Returns:
            True if issues were identified.
        """
        return len(self.issues) > 0 or self.validation_level == ValidationLevel.WRONG

    @property
    def is_acceptable(self) -> bool:
        """Check if this section is acceptable for approval.

        Returns:
            True if the section is SOLID or SHAKY without blocking issues.
        """
        return self.validation_level != ValidationLevel.WRONG


class ChallengerAssessment(BaseModel):
    """Complete validation assessment from Challenger.

    Aggregates per-section assessments into an overall evaluation
    of the documentation quality.
    """

    program_id: str = Field(
        ...,
        description="The program being assessed",
    )
    iteration: int = Field(
        ...,
        ge=1,
        description="Which iteration this assessment is for",
    )
    section_assessments: list[SectionAssessment] = Field(
        default_factory=list,
        description="Per-section validation results",
    )
    overall_assessment: str = Field(
        default="",
        description="Summary of the overall assessment",
    )
    key_concerns: list[str] = Field(
        default_factory=list,
        description="Most important issues to address",
    )
    timestamp: datetime = Field(
        default_factory=datetime.utcnow,
        description="When this assessment was made",
    )

    @computed_field
    @property
    def solid_count(self) -> int:
        """Count of sections assessed as SOLID.

        Returns:
            Number of SOLID sections.
        """
        return sum(
            1 for sa in self.section_assessments
            if sa.validation_level == ValidationLevel.SOLID
        )

    @computed_field
    @property
    def shaky_count(self) -> int:
        """Count of sections assessed as SHAKY.

        Returns:
            Number of SHAKY sections.
        """
        return sum(
            1 for sa in self.section_assessments
            if sa.validation_level == ValidationLevel.SHAKY
        )

    @computed_field
    @property
    def wrong_count(self) -> int:
        """Count of sections assessed as WRONG.

        Returns:
            Number of WRONG sections.
        """
        return sum(
            1 for sa in self.section_assessments
            if sa.validation_level == ValidationLevel.WRONG
        )

    @property
    def has_blocking_issues(self) -> bool:
        """Check if there are any blocking issues.

        Returns:
            True if any section is assessed as WRONG.
        """
        return self.wrong_count > 0

    @property
    def is_acceptable(self) -> bool:
        """Check if the overall assessment is acceptable for approval.

        Returns:
            True if no sections are WRONG and critical sections are SOLID.
        """
        if self.has_blocking_issues:
            return False

        # Check critical sections (purpose, inputs, outputs) are solid
        critical_sections = {"purpose", "inputs", "outputs"}
        for sa in self.section_assessments:
            if sa.section_name.lower() in critical_sections:
                if sa.validation_level != ValidationLevel.SOLID:
                    return False

        return True

    def get_section_assessment(self, section_name: str) -> SectionAssessment | None:
        """Get the assessment for a specific section.

        Args:
            section_name: Name of the section to look up.

        Returns:
            The SectionAssessment if found, None otherwise.
        """
        for sa in self.section_assessments:
            if sa.section_name.lower() == section_name.lower():
                return sa
        return None


class ConfidenceAssessment(BaseModel):
    """Scribe's confidence assessment of the documentation.

    Captures the Scribe's self-evaluation of how confident they are
    in the accuracy and completeness of the documentation.
    """

    program_id: str = Field(
        ...,
        description="The program being documented",
    )
    iteration: int = Field(
        ...,
        ge=1,
        description="Which iteration this assessment is for",
    )
    overall_confidence: ConfidenceLevel = Field(
        ...,
        description="Overall confidence level",
    )
    section_confidence: dict[str, ConfidenceLevel] = Field(
        default_factory=dict,
        description="Per-section confidence levels",
    )
    low_confidence_sections: list[str] = Field(
        default_factory=list,
        description="Sections with LOW confidence",
    )
    reasoning: str = Field(
        default="",
        description="Brief explanation of the confidence assessment",
    )
    unknown_items: list[str] = Field(
        default_factory=list,
        description="Items that could not be determined",
    )
    timestamp: datetime = Field(
        default_factory=datetime.utcnow,
        description="When this assessment was made",
    )

    @computed_field
    @property
    def high_confidence_count(self) -> int:
        """Count of sections with HIGH confidence.

        Returns:
            Number of HIGH confidence sections.
        """
        return sum(
            1 for level in self.section_confidence.values()
            if level == ConfidenceLevel.HIGH
        )

    @computed_field
    @property
    def medium_confidence_count(self) -> int:
        """Count of sections with MEDIUM confidence.

        Returns:
            Number of MEDIUM confidence sections.
        """
        return sum(
            1 for level in self.section_confidence.values()
            if level == ConfidenceLevel.MEDIUM
        )

    @computed_field
    @property
    def low_confidence_count(self) -> int:
        """Count of sections with LOW confidence.

        Returns:
            Number of LOW confidence sections.
        """
        return sum(
            1 for level in self.section_confidence.values()
            if level == ConfidenceLevel.LOW
        )

    def get_section_confidence(self, section_name: str) -> ConfidenceLevel | None:
        """Get the confidence level for a specific section.

        Args:
            section_name: Name of the section to look up.

        Returns:
            The ConfidenceLevel if found, None otherwise.
        """
        return self.section_confidence.get(section_name.lower())


class QualityMetrics(BaseModel):
    """Aggregated quality metrics for a documentation effort.

    Combines confidence and validation assessments into overall
    quality indicators.
    """

    program_id: str = Field(
        ...,
        description="The program being documented",
    )
    iterations_completed: int = Field(
        default=0,
        ge=0,
        description="Number of iterations completed",
    )
    final_confidence: ConfidenceAssessment | None = Field(
        default=None,
        description="Final confidence assessment from Scribe",
    )
    final_validation: ChallengerAssessment | None = Field(
        default=None,
        description="Final validation assessment from Challenger",
    )
    questions_asked: int = Field(
        default=0,
        ge=0,
        description="Total questions asked by Challenger",
    )
    questions_resolved: int = Field(
        default=0,
        ge=0,
        description="Questions that led to documentation updates",
    )
    chrome_tickets_issued: int = Field(
        default=0,
        ge=0,
        description="Number of Chrome tickets from Imperator",
    )
    chrome_tickets_resolved: int = Field(
        default=0,
        ge=0,
        description="Number of Chrome tickets resolved",
    )

    @computed_field
    @property
    def resolution_rate(self) -> float:
        """Calculate the question resolution rate.

        Returns:
            Percentage of questions that were resolved (0.0 to 1.0).
        """
        if self.questions_asked == 0:
            return 1.0
        return self.questions_resolved / self.questions_asked

    @computed_field
    @property
    def ticket_resolution_rate(self) -> float:
        """Calculate the Chrome ticket resolution rate.

        Returns:
            Percentage of tickets that were resolved (0.0 to 1.0).
        """
        if self.chrome_tickets_issued == 0:
            return 1.0
        return self.chrome_tickets_resolved / self.chrome_tickets_issued

    @property
    def is_quality_acceptable(self) -> bool:
        """Check if quality metrics indicate acceptable documentation.

        Returns:
            True if quality metrics are within acceptable bounds.
        """
        if self.final_validation and self.final_validation.has_blocking_issues:
            return False

        if self.final_confidence:
            if self.final_confidence.overall_confidence == ConfidenceLevel.LOW:
                return False

        return True
