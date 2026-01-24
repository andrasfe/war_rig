"""Document validation for War Rig documentation templates.

This module provides validation logic to check if documentation templates
meet completeness criteria based on file type. It creates ChromeTickets
for any failed validation criteria.
"""

from dataclasses import dataclass, field
from typing import Any, Union

from war_rig.models.templates import (
    CopybookTemplate,
    DataFlow,
    DocumentationTemplate,
    FileType,
    JCLDependencies,
    JCLTemplate,
)
from war_rig.models.tickets import ChromeTicket, IssuePriority, IssueType
from war_rig.validation.document_criteria import (
    Priority,
    ValidationCriterion,
    get_criteria_for_type,
)


# Type alias for template types
TemplateType = Union[DocumentationTemplate, JCLTemplate, CopybookTemplate]


@dataclass
class ValidationResult:
    """Result of validating a documentation template.

    Attributes:
        file_name: Name of the file being validated.
        file_type: Detected file type (COBOL, JCL, COPYBOOK, etc.).
        is_valid: True if all criteria passed.
        failed_criteria: List of criteria that failed validation.
        tickets: ChromeTickets created for failed criteria.
    """

    file_name: str
    file_type: FileType
    is_valid: bool
    failed_criteria: list[ValidationCriterion] = field(default_factory=list)
    tickets: list[ChromeTicket] = field(default_factory=list)


class DocumentValidator:
    """Validates documentation templates against completeness criteria.

    This validator checks that documentation templates contain meaningful
    content in required sections based on the file type. It creates
    ChromeTickets for any sections that fail validation.

    Example:
        validator = DocumentValidator()
        result = validator.validate_document(
            file_name="PROG001.cbl",
            program_id="PROG001",
            template=my_template,
        )
        if not result.is_valid:
            for ticket in result.tickets:
                print(f"Issue: {ticket.description}")
    """

    # File extension to FileType mapping
    EXTENSION_MAP: dict[str, FileType] = {
        ".cbl": FileType.COBOL,
        ".cob": FileType.COBOL,
        ".cpy": FileType.COPYBOOK,
        ".copy": FileType.COPYBOOK,
        ".jcl": FileType.JCL,
        ".bms": FileType.BMS,
        ".pli": FileType.PLI,
        ".pl1": FileType.PLI,
        ".proc": FileType.PROC,
        ".asm": FileType.ASM,
        ".hlasm": FileType.ASM,
        ".rexx": FileType.REXX,
        ".rex": FileType.REXX,
        ".clist": FileType.CLIST,
    }

    # Minimum length for meaningful text content
    MIN_SUMMARY_LENGTH = 20

    def validate_document(
        self,
        file_name: str,
        program_id: str,
        template: TemplateType,
        file_type: FileType | None = None,
    ) -> ValidationResult:
        """Validate a documentation template against completeness criteria.

        Args:
            file_name: Name of the source file being documented.
            program_id: Program identifier for ticket creation.
            template: The documentation template to validate.
            file_type: Optional explicit file type. If None, will be detected
                from filename extension or template header.

        Returns:
            ValidationResult containing validation outcome and any tickets.
        """
        # Detect file type if not provided
        detected_type = file_type or self._detect_file_type(file_name, template)

        # Get criteria for this file type
        criteria_container = get_criteria_for_type(detected_type)

        failed_criteria: list[ValidationCriterion] = []
        tickets: list[ChromeTicket] = []

        # If no criteria defined for this type, return valid with no failures
        if criteria_container is None:
            return ValidationResult(
                file_name=file_name,
                file_type=detected_type,
                is_valid=True,
                failed_criteria=[],
                tickets=[],
            )

        for criterion in criteria_container.criteria:
            if not self._check_criterion(template, criterion, detected_type):
                failed_criteria.append(criterion)
                ticket = self._create_ticket(program_id, criterion)
                tickets.append(ticket)

        return ValidationResult(
            file_name=file_name,
            file_type=detected_type,
            is_valid=len(failed_criteria) == 0,
            failed_criteria=failed_criteria,
            tickets=tickets,
        )

    def _detect_file_type(
        self,
        file_name: str,
        template: TemplateType,
    ) -> FileType:
        """Detect file type from filename extension or template header.

        Detection priority:
        1. File extension mapping
        2. Template header file_type field
        3. Default to OTHER

        Args:
            file_name: Source file name.
            template: Template that may contain header with file_type.

        Returns:
            Detected FileType.
        """
        # Try extension-based detection first
        lower_name = file_name.lower()
        for ext, ftype in self.EXTENSION_MAP.items():
            if lower_name.endswith(ext):
                return ftype

        # Try template header
        if hasattr(template, "header") and template.header is not None:
            header = template.header
            if hasattr(header, "file_type") and header.file_type is not None:
                return header.file_type

        # Default fallback
        return FileType.OTHER

    def _check_criterion(
        self,
        template: TemplateType,
        criterion: ValidationCriterion,
        file_type: FileType,
    ) -> bool:
        """Check if a single validation criterion is satisfied.

        The validation logic depends on the criterion's section and
        the type of content expected:
        - List sections: Check len > 0 and has_meaningful_content
        - Purpose-like sections with summary: Check summary exists and len > 20
        - Dependencies/data_flow: Check has_meaningful_nested_content

        Args:
            template: The template to validate.
            criterion: The criterion to check.
            file_type: The file type for context.

        Returns:
            True if the criterion is satisfied, False otherwise.
        """
        # Navigate to the section using the path (section can be dot-separated)
        section_value = self._get_section_value(template, criterion.section)

        if section_value is None:
            return False

        # Handle different section types based on the section name
        section_name = criterion.section.split(".")[-1]

        # Purpose-like sections with summary field
        if section_name in ("purpose",):
            return self._check_purpose_section(section_value)

        # Nested content sections (data_flow, dependencies)
        if section_name in ("data_flow", "dependencies"):
            return self._has_meaningful_nested_content(section_value)

        # List sections (steps, paragraphs, inputs, outputs, etc.)
        if isinstance(section_value, list):
            return len(section_value) > 0 and self._has_meaningful_content(section_value)

        # String sections - check for meaningful content
        if isinstance(section_value, str):
            return len(section_value.strip()) >= self.MIN_SUMMARY_LENGTH

        # For other object types, check if it has any non-None attributes
        if hasattr(section_value, "__dict__"):
            return self._has_meaningful_nested_content(section_value)

        return section_value is not None

    def _check_purpose_section(self, section: Any) -> bool:
        """Check if a purpose section has meaningful content.

        Args:
            section: The purpose section object.

        Returns:
            True if summary exists and has sufficient length.
        """
        if section is None:
            return False

        # Check for summary attribute
        summary = getattr(section, "summary", None)
        if summary is None:
            return False

        return len(summary.strip()) >= self.MIN_SUMMARY_LENGTH

    def _has_meaningful_content(self, items: list[Any]) -> bool:
        """Check if list items have meaningful content.

        An item is considered meaningful if it has at least one non-None,
        non-empty attribute (excluding metadata like citations).

        Args:
            items: List of items to check.

        Returns:
            True if at least one item has meaningful content.
        """
        if not items:
            return False

        # Metadata fields to ignore when checking for content
        metadata_fields = {"citation", "citations", "line_number", "line_numbers"}

        for item in items:
            if item is None:
                continue

            # For dict items
            if isinstance(item, dict):
                for key, value in item.items():
                    if key in metadata_fields:
                        continue
                    if self._is_meaningful_value(value):
                        return True

            # For object items (Pydantic models, dataclasses, etc.)
            elif hasattr(item, "__dict__"):
                for key, value in vars(item).items():
                    if key.startswith("_") or key in metadata_fields:
                        continue
                    if self._is_meaningful_value(value):
                        return True

            # For primitive types
            elif self._is_meaningful_value(item):
                return True

        return False

    def _has_meaningful_nested_content(self, section: Any) -> bool:
        """Check if a nested section (DataFlow/JCLDependencies) has content.

        These sections contain multiple sub-lists that should be checked
        for meaningful content.

        Args:
            section: The nested section object.

        Returns:
            True if any sub-section has meaningful content.
        """
        if section is None:
            return False

        # Handle DataFlow specifically
        if isinstance(section, DataFlow):
            if section.reads_from and self._has_meaningful_content(section.reads_from):
                return True
            if section.writes_to and self._has_meaningful_content(section.writes_to):
                return True
            if section.transforms and self._has_meaningful_content(section.transforms):
                return True
            return False

        # Handle JCLDependencies specifically
        if isinstance(section, JCLDependencies):
            if section.input_datasets:
                return True
            if section.output_datasets:
                return True
            if section.predecessor_jobs:
                return True
            if section.successor_jobs:
                return True
            return False

        # Generic handling for other nested objects
        if hasattr(section, "__dict__"):
            for key, value in vars(section).items():
                if key.startswith("_"):
                    continue
                if isinstance(value, list) and value:
                    if self._has_meaningful_content(value):
                        return True
                elif self._is_meaningful_value(value):
                    return True

        return False

    def _is_meaningful_value(self, value: Any) -> bool:
        """Check if a value is meaningful (non-None, non-empty).

        Args:
            value: The value to check.

        Returns:
            True if the value is meaningful.
        """
        if value is None:
            return False
        if isinstance(value, str):
            return len(value.strip()) > 0
        if isinstance(value, (list, dict)):
            return len(value) > 0
        if isinstance(value, bool):
            return True  # Booleans are meaningful
        if isinstance(value, (int, float)):
            return True  # Numbers are meaningful
        return True  # Other types are assumed meaningful

    def _get_section_value(self, template: TemplateType, path: str) -> Any:
        """Navigate to a section value using a dot-separated path.

        Args:
            template: The template to navigate.
            path: Dot-separated path (e.g., "purpose.summary").

        Returns:
            The value at the path, or None if not found.
        """
        parts = path.split(".")
        current: Any = template

        for part in parts:
            if current is None:
                return None

            # Try attribute access
            if hasattr(current, part):
                current = getattr(current, part)
            # Try dict access
            elif isinstance(current, dict) and part in current:
                current = current[part]
            else:
                return None

        return current

    def _create_ticket(
        self,
        program_id: str,
        criterion: ValidationCriterion,
    ) -> ChromeTicket:
        """Create a ChromeTicket for a failed validation criterion.

        Args:
            program_id: The program being documented.
            criterion: The criterion that failed.

        Returns:
            A ChromeTicket describing the issue.
        """
        return ChromeTicket(
            program_id=program_id,
            section=criterion.section,
            issue_type=IssueType.INCOMPLETE,
            description=f"Validation failed: {criterion.description}",
            guidance=criterion.guidance,
            priority=self._criterion_to_priority(criterion),
        )

    def _criterion_to_priority(self, criterion: ValidationCriterion) -> IssuePriority:
        """Map criterion priority to ticket priority.

        Args:
            criterion: The validation criterion.

        Returns:
            Corresponding IssuePriority.
        """
        # Map criterion Priority to IssuePriority
        if criterion.priority == Priority.CRITICAL:
            return IssuePriority.CRITICAL
        elif criterion.priority == Priority.HIGH:
            return IssuePriority.HIGH
        return IssuePriority.MEDIUM
