"""Skill name normalization utilities for Agent Skills format.

This module provides utilities for converting War Rig program names and
identifiers into valid Agent Skills names that follow the spec requirements:

- Max 64 characters
- Lowercase letters, numbers, and hyphens only
- Must not start or end with hyphen
- No consecutive hyphens

Example:
    >>> normalize_skill_name("CBACT01C.cbl.doc.json")
    'cbact01c'
    >>> validate_skill_name("my-valid-skill")
    (True, None)
"""

import logging
import re

logger = logging.getLogger(__name__)

# Agent Skills spec constraints
MAX_SKILL_NAME_LENGTH = 64
MAX_DESCRIPTION_LENGTH = 1024

# Regex pattern for valid skill names: lowercase alphanumeric + hyphens
# Must not start/end with hyphen, no consecutive hyphens
_VALID_SKILL_NAME_PATTERN = re.compile(r"^[a-z0-9]([a-z0-9-]*[a-z0-9])?$|^[a-z0-9]$")


def normalize_skill_name(name: str) -> str:
    """Convert a filename/identifier to a valid Agent Skills name.

    Applies the following transformations in order:
    1. Remove file extensions (.cbl, .doc.json, etc.)
    2. Convert to lowercase
    3. Replace underscores with hyphens
    4. Remove all characters except alphanumeric and hyphens
    5. Collapse consecutive hyphens to single hyphen
    6. Strip leading/trailing hyphens
    7. Truncate to 64 characters (preserving valid endings)

    Args:
        name: The filename or identifier to normalize.

    Returns:
        A valid Agent Skills name. Returns empty string if input is empty
        or cannot be normalized to a valid name.

    Examples:
        >>> normalize_skill_name("CBACT01C")
        'cbact01c'
        >>> normalize_skill_name("PROGRAM_NAME")
        'program-name'
        >>> normalize_skill_name("my--file")
        'my-file'
        >>> normalize_skill_name("123_test")
        '123-test'
        >>> normalize_skill_name("CBACT01C.cbl.doc.json")
        'cbact01c'
    """
    if not name:
        return ""

    # Step 1: Remove common file extensions
    result = name
    extensions_to_remove = [
        ".doc.json",
        ".cbl",
        ".cob",
        ".cpy",
        ".copy",
        ".jcl",
        ".asm",
        ".pli",
        ".pl1",
        ".rexx",
        ".rex",
        ".json",
        ".md",
    ]
    lower_result = result.lower()
    for ext in extensions_to_remove:
        if lower_result.endswith(ext):
            result = result[: -len(ext)]
            lower_result = result.lower()

    # Step 2: Convert to lowercase
    result = result.lower()

    # Step 3: Replace underscores with hyphens
    result = result.replace("_", "-")

    # Step 4: Remove all characters except alphanumeric and hyphens
    result = re.sub(r"[^a-z0-9-]", "", result)

    # Step 5: Collapse consecutive hyphens
    result = re.sub(r"-+", "-", result)

    # Step 6: Strip leading/trailing hyphens
    result = result.strip("-")

    # Step 7: Truncate to max length, avoiding ending with hyphen
    if len(result) > MAX_SKILL_NAME_LENGTH:
        result = result[:MAX_SKILL_NAME_LENGTH].rstrip("-")

    logger.debug(f"Normalized skill name: {name!r} -> {result!r}")
    return result


def validate_skill_name(name: str) -> tuple[bool, str | None]:
    """Validate a skill name against Agent Skills spec.

    A valid skill name must:
    - Be 1-64 characters long
    - Contain only lowercase letters, numbers, and hyphens
    - Not start or end with a hyphen
    - Not contain consecutive hyphens

    Args:
        name: The skill name to validate.

    Returns:
        Tuple of (is_valid, error_message).
        error_message is None if valid, otherwise describes the validation failure.

    Examples:
        >>> validate_skill_name("my-valid-skill")
        (True, None)
        >>> validate_skill_name("")
        (False, 'Skill name cannot be empty')
        >>> validate_skill_name("-invalid")
        (False, 'Skill name cannot start with a hyphen')
    """
    if not name:
        return False, "Skill name cannot be empty"

    if len(name) > MAX_SKILL_NAME_LENGTH:
        return (
            False,
            f"Skill name exceeds maximum length of {MAX_SKILL_NAME_LENGTH} characters",
        )

    if name.startswith("-"):
        return False, "Skill name cannot start with a hyphen"

    if name.endswith("-"):
        return False, "Skill name cannot end with a hyphen"

    if "--" in name:
        return False, "Skill name cannot contain consecutive hyphens"

    # Check for invalid characters
    if not re.match(r"^[a-z0-9-]+$", name):
        invalid_chars = set(re.findall(r"[^a-z0-9-]", name))
        return False, f"Skill name contains invalid characters: {invalid_chars}"

    # Final validation with complete pattern
    if not _VALID_SKILL_NAME_PATTERN.match(name):
        return False, "Skill name does not match the required pattern"

    return True, None


def create_skill_frontmatter(
    name: str,
    description: str,
    metadata: dict[str, str] | None = None,
) -> str:
    """Create YAML frontmatter for a SKILL.md file.

    Generates properly formatted YAML frontmatter that conforms to the
    Agent Skills specification. The name is validated and description
    is truncated if necessary.

    Args:
        name: Skill name (must be valid per validate_skill_name).
        description: Skill description (will be truncated to 1024 chars if needed).
        metadata: Optional additional metadata key-value pairs to include.

    Returns:
        YAML frontmatter string including --- delimiters.

    Raises:
        ValueError: If name is not a valid skill name.

    Example:
        >>> print(create_skill_frontmatter("my-skill", "A helpful skill"))
        ---
        name: my-skill
        description: A helpful skill
        ---
    """
    is_valid, error = validate_skill_name(name)
    if not is_valid:
        raise ValueError(f"Invalid skill name: {error}")

    truncated_desc = truncate_description(description)

    # Build YAML content
    lines = ["---", f"name: {name}"]

    # Handle multiline descriptions by using YAML literal block scalar
    if "\n" in truncated_desc:
        lines.append("description: |")
        for line in truncated_desc.split("\n"):
            lines.append(f"  {line}")
    else:
        # Escape special characters for single-line YAML
        escaped_desc = _escape_yaml_string(truncated_desc)
        lines.append(f"description: {escaped_desc}")

    # Add optional metadata
    if metadata:
        for key, value in sorted(metadata.items()):
            escaped_value = _escape_yaml_string(str(value))
            lines.append(f"{key}: {escaped_value}")

    lines.append("---")
    return "\n".join(lines)


def truncate_description(text: str, max_length: int = MAX_DESCRIPTION_LENGTH) -> str:
    """Truncate description to max length, ending at word boundary.

    If the text exceeds max_length, it is truncated at the last word
    boundary before max_length and an ellipsis (...) is appended.

    Args:
        text: The text to truncate.
        max_length: Maximum length of the result (default 1024).

    Returns:
        The truncated text, or original text if within limit.

    Examples:
        >>> truncate_description("Hello world", 100)
        'Hello world'
        >>> truncate_description("Hello world", 8)
        'Hello...'
    """
    if not text:
        return ""

    if len(text) <= max_length:
        return text

    # Reserve space for ellipsis
    truncate_at = max_length - 3

    if truncate_at <= 0:
        return "..."

    # Find last space before truncation point
    truncated = text[:truncate_at]
    last_space = truncated.rfind(" ")

    if last_space > 0:
        # Truncate at word boundary
        truncated = truncated[:last_space]
    # else: no space found, truncate at character limit

    return truncated.rstrip() + "..."


def _escape_yaml_string(text: str) -> str:
    """Escape a string for safe YAML single-line output.

    Wraps strings in quotes if they contain special characters that
    could be misinterpreted by YAML parsers.

    Args:
        text: The string to escape.

    Returns:
        The escaped string, quoted if necessary.
    """
    if not text:
        return '""'

    # Characters that require quoting
    needs_quotes = any(
        c in text
        for c in [
            ":",
            "#",
            "[",
            "]",
            "{",
            "}",
            ",",
            "&",
            "*",
            "!",
            "|",
            ">",
            "'",
            '"',
            "%",
            "@",
            "`",
        ]
    )

    # Also quote if starts/ends with whitespace or special YAML values
    yaml_special_values = {"true", "false", "yes", "no", "null", "~"}
    if (
        text.lower() in yaml_special_values
        or text.startswith(" ")
        or text.endswith(" ")
        or text.startswith("-")
        or text.startswith("?")
    ):
        needs_quotes = True

    if needs_quotes:
        # Use double quotes and escape internal double quotes
        escaped = text.replace("\\", "\\\\").replace('"', '\\"')
        return f'"{escaped}"'

    return text
