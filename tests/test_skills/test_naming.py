"""Unit tests for skill name normalization utilities.

Tests:
- normalize_skill_name with various inputs
- validate_skill_name edge cases
- create_skill_frontmatter output format
- truncate_description behavior
"""

import pytest

from war_rig.skills.naming import (
    MAX_DESCRIPTION_LENGTH,
    MAX_SKILL_NAME_LENGTH,
    create_skill_frontmatter,
    normalize_skill_name,
    truncate_description,
    validate_skill_name,
)


class TestNormalizeSkillName:
    """Tests for normalize_skill_name function."""

    def test_uppercase_conversion(self):
        """Test that uppercase is converted to lowercase."""
        assert normalize_skill_name("CBACT01C") == "cbact01c"
        assert normalize_skill_name("MyProgram") == "myprogram"
        assert normalize_skill_name("TEST") == "test"

    def test_underscore_to_hyphen(self):
        """Test that underscores are converted to hyphens."""
        assert normalize_skill_name("PROGRAM_NAME") == "program-name"
        assert normalize_skill_name("my_cool_program") == "my-cool-program"
        assert normalize_skill_name("a_b_c") == "a-b-c"

    def test_consecutive_hyphens_collapsed(self):
        """Test that consecutive hyphens are collapsed to single hyphen."""
        assert normalize_skill_name("my--file") == "my-file"
        assert normalize_skill_name("test---name") == "test-name"
        assert (
            normalize_skill_name("a____b") == "a-b"
        )  # Underscores become hyphens first

    def test_leading_trailing_hyphens_stripped(self):
        """Test that leading and trailing hyphens are stripped."""
        assert normalize_skill_name("-test-") == "test"
        assert normalize_skill_name("--leading") == "leading"
        assert normalize_skill_name("trailing--") == "trailing"
        assert normalize_skill_name("---both---") == "both"

    def test_numeric_prefix(self):
        """Test that numeric prefixes are preserved."""
        assert normalize_skill_name("123_test") == "123-test"
        assert normalize_skill_name("456ABC") == "456abc"
        assert normalize_skill_name("1a2b3c") == "1a2b3c"

    def test_file_extension_removal(self):
        """Test that common file extensions are removed."""
        assert normalize_skill_name("CBACT01C.cbl.doc.json") == "cbact01c"
        assert normalize_skill_name("PROGRAM.cbl") == "program"
        assert normalize_skill_name("COPYBOOK.cpy") == "copybook"
        assert normalize_skill_name("MYJOB.jcl") == "myjob"
        assert normalize_skill_name("SKILL.md") == "skill"

    def test_special_characters_removed(self):
        """Test that special characters are removed."""
        assert normalize_skill_name("program!@#$%name") == "programname"
        assert normalize_skill_name("test.file.name") == "testfilename"
        assert normalize_skill_name("path/to/file") == "pathtofile"

    def test_empty_string(self):
        """Test handling of empty strings."""
        assert normalize_skill_name("") == ""
        assert normalize_skill_name("   ") == ""  # Whitespace only

    def test_special_only_input(self):
        """Test input with only special characters."""
        assert normalize_skill_name("---") == ""
        assert normalize_skill_name("!@#$%") == ""

    def test_length_truncation(self):
        """Test that names are truncated to 64 characters."""
        long_name = "a" * 100
        result = normalize_skill_name(long_name)
        assert len(result) <= MAX_SKILL_NAME_LENGTH
        assert result == "a" * 64

    def test_truncation_avoids_trailing_hyphen(self):
        """Test that truncation doesn't leave a trailing hyphen."""
        # Create a name that would end with hyphen after truncation
        name = "a" * 63 + "-b"  # 65 chars, truncating would leave "aaa...a-"
        result = normalize_skill_name(name)
        assert not result.endswith("-")
        assert len(result) <= MAX_SKILL_NAME_LENGTH

    def test_real_world_examples(self):
        """Test with real War Rig program names."""
        assert normalize_skill_name("CBACT01C.cbl.doc.json") == "cbact01c"
        assert normalize_skill_name("CBPAUP0C.cbl") == "cbpaup0c"
        assert normalize_skill_name("CUSTOMER-RECORD.cpy") == "customer-record"
        assert normalize_skill_name("BATCH_JOB_01.jcl") == "batch-job-01"


class TestValidateSkillName:
    """Tests for validate_skill_name function."""

    def test_valid_simple_names(self):
        """Test valid simple skill names."""
        assert validate_skill_name("test") == (True, None)
        assert validate_skill_name("my-skill") == (True, None)
        assert validate_skill_name("cbact01c") == (True, None)

    def test_valid_with_numbers(self):
        """Test valid names with numbers."""
        assert validate_skill_name("123") == (True, None)
        assert validate_skill_name("test123") == (True, None)
        assert validate_skill_name("1a2b3c") == (True, None)

    def test_single_character(self):
        """Test single character names."""
        assert validate_skill_name("a") == (True, None)
        assert validate_skill_name("1") == (True, None)

    def test_empty_string_invalid(self):
        """Test that empty string is invalid."""
        is_valid, error = validate_skill_name("")
        assert not is_valid
        assert "empty" in error.lower()

    def test_too_long_invalid(self):
        """Test that names exceeding 64 chars are invalid."""
        long_name = "a" * 65
        is_valid, error = validate_skill_name(long_name)
        assert not is_valid
        assert "64" in error or "length" in error.lower()

    def test_leading_hyphen_invalid(self):
        """Test that leading hyphen is invalid."""
        is_valid, error = validate_skill_name("-test")
        assert not is_valid
        assert "start" in error.lower() and "hyphen" in error.lower()

    def test_trailing_hyphen_invalid(self):
        """Test that trailing hyphen is invalid."""
        is_valid, error = validate_skill_name("test-")
        assert not is_valid
        assert "end" in error.lower() and "hyphen" in error.lower()

    def test_consecutive_hyphens_invalid(self):
        """Test that consecutive hyphens are invalid."""
        is_valid, error = validate_skill_name("test--name")
        assert not is_valid
        assert "consecutive" in error.lower()

    def test_uppercase_invalid(self):
        """Test that uppercase characters are invalid."""
        is_valid, error = validate_skill_name("Test")
        assert not is_valid
        assert "invalid" in error.lower()

    def test_underscore_invalid(self):
        """Test that underscores are invalid."""
        is_valid, error = validate_skill_name("test_name")
        assert not is_valid
        assert "invalid" in error.lower()

    def test_special_characters_invalid(self):
        """Test that special characters are invalid."""
        is_valid, error = validate_skill_name("test@name")
        assert not is_valid
        assert "invalid" in error.lower()

        is_valid, error = validate_skill_name("test.name")
        assert not is_valid

    def test_max_length_valid(self):
        """Test that exactly 64 characters is valid."""
        name = "a" * 64
        assert validate_skill_name(name) == (True, None)


class TestCreateSkillFrontmatter:
    """Tests for create_skill_frontmatter function."""

    def test_simple_frontmatter(self):
        """Test creating simple frontmatter."""
        result = create_skill_frontmatter("my-skill", "A helpful skill")
        assert "---" in result
        assert "name: my-skill" in result
        assert "description: A helpful skill" in result
        # Should start and end with ---
        lines = result.strip().split("\n")
        assert lines[0] == "---"
        assert lines[-1] == "---"

    def test_frontmatter_with_metadata(self):
        """Test frontmatter with additional metadata."""
        result = create_skill_frontmatter(
            "my-skill",
            "A skill",
            metadata={"category": "programs", "version": "1.0"},
        )
        assert "category: programs" in result
        assert 'version: "1.0"' in result or "version: 1.0" in result

    def test_frontmatter_description_truncation(self):
        """Test that long descriptions are truncated."""
        long_desc = "x" * 2000
        result = create_skill_frontmatter("my-skill", long_desc)
        # Description should be truncated
        assert len(result) < 2200  # Some overhead for YAML structure

    def test_invalid_name_raises(self):
        """Test that invalid skill name raises ValueError."""
        with pytest.raises(ValueError, match="Invalid skill name"):
            create_skill_frontmatter("Invalid Name", "Description")

        with pytest.raises(ValueError):
            create_skill_frontmatter("", "Description")

        with pytest.raises(ValueError):
            create_skill_frontmatter("-invalid", "Description")

    def test_special_characters_in_description(self):
        """Test that special YAML characters in description are escaped."""
        result = create_skill_frontmatter("my-skill", "Contains: colon and #hash")
        assert "my-skill" in result
        # Should be escaped/quoted
        assert "Contains" in result

    def test_multiline_description(self):
        """Test handling of multiline descriptions."""
        result = create_skill_frontmatter("my-skill", "Line 1\nLine 2\nLine 3")
        assert "name: my-skill" in result
        # Should use YAML literal block scalar
        assert "description: |" in result or "\\n" in result or "Line 1" in result

    def test_metadata_sorted_alphabetically(self):
        """Test that metadata keys are sorted."""
        result = create_skill_frontmatter(
            "my-skill",
            "Desc",
            metadata={"zebra": "last", "alpha": "first"},
        )
        # alpha should appear before zebra
        alpha_pos = result.find("alpha")
        zebra_pos = result.find("zebra")
        assert alpha_pos < zebra_pos


class TestTruncateDescription:
    """Tests for truncate_description function."""

    def test_no_truncation_needed(self):
        """Test that short text is not truncated."""
        text = "Hello world"
        assert truncate_description(text) == text
        assert truncate_description(text, 100) == text

    def test_truncation_at_word_boundary(self):
        """Test that truncation happens at word boundary."""
        text = "Hello world test"
        result = truncate_description(text, 10)
        assert result == "Hello..."
        assert not result.endswith(" ...")  # No trailing space before ellipsis

    def test_truncation_adds_ellipsis(self):
        """Test that truncated text ends with ellipsis."""
        text = "This is a very long description that needs truncation"
        result = truncate_description(text, 20)
        assert result.endswith("...")

    def test_truncation_respects_max_length(self):
        """Test that result respects max_length."""
        text = "x" * 2000
        result = truncate_description(text, 100)
        assert len(result) <= 100

    def test_empty_string(self):
        """Test handling of empty string."""
        assert truncate_description("") == ""
        assert truncate_description("", 100) == ""

    def test_default_max_length(self):
        """Test default max length is 1024."""
        text = "x" * 2000
        result = truncate_description(text)
        assert len(result) <= MAX_DESCRIPTION_LENGTH

    def test_very_short_max_length(self):
        """Test handling of very short max length."""
        text = "Hello world"
        result = truncate_description(text, 3)
        assert result == "..."

    def test_single_long_word(self):
        """Test truncation of single long word."""
        text = "supercalifragilisticexpialidocious"
        result = truncate_description(text, 15)
        assert len(result) <= 15
        assert result.endswith("...")

    def test_exact_length_no_truncation(self):
        """Test that text at exact max length is not truncated."""
        text = "x" * 1024
        result = truncate_description(text, 1024)
        assert result == text
        assert not result.endswith("...")

    def test_preserves_content_semantics(self):
        """Test that truncation preserves meaningful content."""
        text = "The quick brown fox jumps over the lazy dog"
        result = truncate_description(text, 25)
        # Should truncate at a word boundary
        assert result in ["The quick brown fox...", "The quick brown..."]


class TestIntegration:
    """Integration tests for naming utilities."""

    def test_normalize_then_validate(self):
        """Test that normalized names are always valid."""
        test_inputs = [
            "CBACT01C.cbl.doc.json",
            "MY_PROGRAM_NAME",
            "test--file--name",
            "-leading-trailing-",
            "123_numeric",
            "CamelCase",
            "a" * 100,  # Long name
        ]

        for input_name in test_inputs:
            normalized = normalize_skill_name(input_name)
            if normalized:  # Skip empty results
                is_valid, error = validate_skill_name(normalized)
                assert is_valid, (
                    f"Normalized {input_name!r} -> {normalized!r} is invalid: {error}"
                )

    def test_frontmatter_round_trip(self):
        """Test that frontmatter can be parsed as YAML."""
        import yaml

        frontmatter = create_skill_frontmatter(
            "test-skill",
            "A test skill for validation",
            metadata={"category": "test", "version": "1.0"},
        )

        # Remove --- delimiters and parse
        yaml_content = frontmatter.strip().strip("-").strip()
        parsed = yaml.safe_load(yaml_content)

        assert parsed["name"] == "test-skill"
        assert "test skill" in parsed["description"]
        assert parsed["category"] == "test"
