"""Tests for the get_markdown_summary() function in the Citadel SDK."""

from __future__ import annotations

import tempfile
from pathlib import Path
from textwrap import dedent


class TestBasicMarkdownExtraction:
    """Tests for basic markdown summary extraction."""

    def test_simple_markdown(self, citadel_instance):
        """Test extraction from a simple markdown file with H1 and paragraph."""
        content = dedent("""
            # My Documentation

            This is the first paragraph of the documentation. It contains
            a description of what this file does and should be extracted
            as the summary.

            ## Another Section

            This should not be included in the summary.
        """).strip()

        with tempfile.NamedTemporaryFile(
            suffix=".md", delete=False, mode="w"
        ) as f:
            f.write(content)
            f.flush()
            path = Path(f.name)

        try:
            result = citadel_instance.get_markdown_summary(path)

            assert result["title"] == "My Documentation"
            assert "first paragraph" in result["summary"]
            assert result["frontmatter"] is None
            assert result["file_path"] == str(path)
        finally:
            path.unlink()

    def test_setext_style_heading(self, citadel_instance):
        """Test extraction with Setext-style H1 heading (underlined with =)."""
        content = dedent("""
            My Title
            ========

            This is the description paragraph that should be extracted.
        """).strip()

        with tempfile.NamedTemporaryFile(
            suffix=".md", delete=False, mode="w"
        ) as f:
            f.write(content)
            f.flush()
            path = Path(f.name)

        try:
            result = citadel_instance.get_markdown_summary(path)

            assert result["title"] == "My Title"
            assert "description paragraph" in result["summary"]
        finally:
            path.unlink()


class TestYamlFrontmatter:
    """Tests for YAML frontmatter parsing."""

    def test_frontmatter_extraction(self, citadel_instance):
        """Test that YAML frontmatter is correctly parsed."""
        content = dedent("""
            ---
            title: Frontmatter Title
            author: John Doe
            tags:
              - python
              - documentation
            ---

            # Document Title

            This is the main content paragraph.
        """).strip()

        with tempfile.NamedTemporaryFile(
            suffix=".md", delete=False, mode="w"
        ) as f:
            f.write(content)
            f.flush()
            path = Path(f.name)

        try:
            result = citadel_instance.get_markdown_summary(path)

            assert result["frontmatter"] is not None
            assert result["frontmatter"]["title"] == "Frontmatter Title"
            assert result["frontmatter"]["author"] == "John Doe"
            assert "python" in result["frontmatter"]["tags"]
            # Title from H1 should take precedence
            assert result["title"] == "Document Title"
            assert "main content" in result["summary"]
        finally:
            path.unlink()

    def test_frontmatter_only_no_h1(self, citadel_instance):
        """Test when frontmatter exists but no H1 heading."""
        content = dedent("""
            ---
            title: Only Frontmatter Title
            ---

            This is a paragraph without any H1 heading in the document.
        """).strip()

        with tempfile.NamedTemporaryFile(
            suffix=".md", delete=False, mode="w"
        ) as f:
            f.write(content)
            f.flush()
            path = Path(f.name)

        try:
            result = citadel_instance.get_markdown_summary(path)

            assert result["frontmatter"]["title"] == "Only Frontmatter Title"
            # Title falls back to filename when no H1
            assert result["title"] == path.stem
        finally:
            path.unlink()

    def test_invalid_frontmatter(self, citadel_instance):
        """Test handling of invalid YAML frontmatter."""
        content = dedent("""
            ---
            invalid: [yaml: content
            ---

            # Title

            Paragraph content.
        """).strip()

        with tempfile.NamedTemporaryFile(
            suffix=".md", delete=False, mode="w"
        ) as f:
            f.write(content)
            f.flush()
            path = Path(f.name)

        try:
            result = citadel_instance.get_markdown_summary(path)

            # Should handle gracefully - frontmatter should be None
            assert result["frontmatter"] is None
            assert result["title"] == "Title"
        finally:
            path.unlink()


class TestMissingTitle:
    """Tests for handling missing titles."""

    def test_no_h1_heading(self, citadel_instance):
        """Test that filename is used when no H1 heading exists."""
        content = dedent("""
            ## Only H2 Heading

            This is a paragraph without any H1 heading.
        """).strip()

        with tempfile.NamedTemporaryFile(
            suffix=".md", delete=False, mode="w"
        ) as f:
            f.write(content)
            f.flush()
            path = Path(f.name)

        try:
            result = citadel_instance.get_markdown_summary(path)

            # Should fall back to filename stem
            assert result["title"] == path.stem
            assert "paragraph without" in result["summary"]
        finally:
            path.unlink()

    def test_empty_file(self, citadel_instance):
        """Test handling of empty file."""
        with tempfile.NamedTemporaryFile(
            suffix=".md", delete=False, mode="w"
        ) as f:
            f.write("")
            f.flush()
            path = Path(f.name)

        try:
            result = citadel_instance.get_markdown_summary(path)

            assert result["title"] == path.stem
            assert result["summary"] == ""
            assert result["frontmatter"] is None
        finally:
            path.unlink()


class TestTruncation:
    """Tests for summary truncation at word boundary."""

    def test_truncation_at_word_boundary(self, citadel_instance):
        """Test that long summaries are truncated at word boundaries."""
        # Create a paragraph longer than default max_chars
        long_paragraph = (
            "This is a very long paragraph that should be truncated. " * 20
        )
        content = f"# Title\n\n{long_paragraph}"

        with tempfile.NamedTemporaryFile(
            suffix=".md", delete=False, mode="w"
        ) as f:
            f.write(content)
            f.flush()
            path = Path(f.name)

        try:
            result = citadel_instance.get_markdown_summary(path, max_chars=100)

            assert len(result["summary"]) <= 100
            assert result["summary"].endswith("...")
            # Should end at a word boundary - the character before "..." should be
            # either a space or the end of a complete word (followed by space in original)
            text_before_ellipsis = result["summary"][:-3]
            # The last word should be complete (not cut off mid-word)
            # This means either it ends with a space, punctuation, or the last word
            # appears complete in the original text
            last_word = text_before_ellipsis.split()[-1] if text_before_ellipsis.split() else ""
            assert last_word in long_paragraph
        finally:
            path.unlink()

    def test_short_paragraph_not_truncated(self, citadel_instance):
        """Test that short paragraphs are not truncated."""
        content = dedent("""
            # Title

            This is a short paragraph.
        """).strip()

        with tempfile.NamedTemporaryFile(
            suffix=".md", delete=False, mode="w"
        ) as f:
            f.write(content)
            f.flush()
            path = Path(f.name)

        try:
            result = citadel_instance.get_markdown_summary(path, max_chars=500)

            assert result["summary"] == "This is a short paragraph."
            assert not result["summary"].endswith("...")
        finally:
            path.unlink()

    def test_custom_max_chars(self, citadel_instance):
        """Test custom max_chars parameter."""
        content = dedent("""
            # Title

            This paragraph has exactly enough content to test truncation behavior.
        """).strip()

        with tempfile.NamedTemporaryFile(
            suffix=".md", delete=False, mode="w"
        ) as f:
            f.write(content)
            f.flush()
            path = Path(f.name)

        try:
            # Very short max_chars
            result = citadel_instance.get_markdown_summary(path, max_chars=30)

            assert len(result["summary"]) <= 30
        finally:
            path.unlink()


class TestCodeBlocksAndImages:
    """Tests for handling code blocks and images."""

    def test_skip_code_blocks(self, citadel_instance):
        """Test that code blocks are skipped when finding the summary."""
        content = dedent("""
            # My Project

            ```python
            def hello():
                print("Hello, World!")
            ```

            This is the actual description paragraph after the code block.
        """).strip()

        with tempfile.NamedTemporaryFile(
            suffix=".md", delete=False, mode="w"
        ) as f:
            f.write(content)
            f.flush()
            path = Path(f.name)

        try:
            result = citadel_instance.get_markdown_summary(path)

            assert "def hello" not in result["summary"]
            assert "actual description" in result["summary"]
        finally:
            path.unlink()

    def test_skip_badges(self, citadel_instance):
        """Test that badge lines are skipped."""
        content = dedent("""
            # My Project

            [![Build Status](https://img.shields.io/travis/user/repo.svg)](https://travis-ci.org/user/repo)
            [![Coverage](https://coveralls.io/repos/user/repo/badge.svg)](https://coveralls.io/r/user/repo)

            This is the real description of the project, not the badges.
        """).strip()

        with tempfile.NamedTemporaryFile(
            suffix=".md", delete=False, mode="w"
        ) as f:
            f.write(content)
            f.flush()
            path = Path(f.name)

        try:
            result = citadel_instance.get_markdown_summary(path)

            assert "shields.io" not in result["summary"]
            assert "coveralls.io" not in result["summary"]
            assert "real description" in result["summary"]
        finally:
            path.unlink()

    def test_skip_images(self, citadel_instance):
        """Test that image lines are skipped."""
        content = dedent("""
            # Documentation

            ![Screenshot](./images/screenshot.png)

            This is the description that should be extracted as the summary.
        """).strip()

        with tempfile.NamedTemporaryFile(
            suffix=".md", delete=False, mode="w"
        ) as f:
            f.write(content)
            f.flush()
            path = Path(f.name)

        try:
            result = citadel_instance.get_markdown_summary(path)

            assert "Screenshot" not in result["summary"]
            assert "description" in result["summary"]
        finally:
            path.unlink()

    def test_skip_indented_code_blocks(self, citadel_instance):
        """Test that indented code blocks are skipped."""
        content = dedent("""
            # Project

            Example code:

                def example():
                    return 42

            This is the actual project description.
        """).strip()

        with tempfile.NamedTemporaryFile(
            suffix=".md", delete=False, mode="w"
        ) as f:
            f.write(content)
            f.flush()
            path = Path(f.name)

        try:
            result = citadel_instance.get_markdown_summary(path)

            # "Example code:" is short (<50 chars), so should skip to real description
            # or collect "Example code:" then stop at indented block
            assert "def example" not in result["summary"]
        finally:
            path.unlink()


class TestNonExistentFile:
    """Tests for handling non-existent files."""

    def test_nonexistent_file(self, citadel_instance):
        """Test handling of non-existent file."""
        result = citadel_instance.get_markdown_summary("/nonexistent/path.md")

        # Should return gracefully with defaults
        assert result["title"] == "path"
        assert result["summary"] == ""
        assert result["frontmatter"] is None
        assert result["file_path"] == "/nonexistent/path.md"


class TestConvenienceFunction:
    """Tests for the module-level convenience function."""

    def test_convenience_function(self):
        """Test that the module-level convenience function works."""
        from citadel.sdk import get_markdown_summary

        content = dedent("""
            # Test Title

            This is the summary paragraph for testing the convenience function.
        """).strip()

        with tempfile.NamedTemporaryFile(
            suffix=".md", delete=False, mode="w"
        ) as f:
            f.write(content)
            f.flush()
            path = Path(f.name)

        try:
            result = get_markdown_summary(path)

            assert result["title"] == "Test Title"
            assert "summary paragraph" in result["summary"]
        finally:
            path.unlink()

    def test_convenience_function_with_max_chars(self):
        """Test convenience function with custom max_chars."""
        from citadel.sdk import get_markdown_summary

        content = dedent("""
            # Title

            This is a paragraph that will be truncated to test the max_chars parameter.
        """).strip()

        with tempfile.NamedTemporaryFile(
            suffix=".md", delete=False, mode="w"
        ) as f:
            f.write(content)
            f.flush()
            path = Path(f.name)

        try:
            result = get_markdown_summary(path, max_chars=50)

            assert len(result["summary"]) <= 50
        finally:
            path.unlink()


class TestEdgeCases:
    """Tests for edge cases in markdown summary extraction."""

    def test_only_heading(self, citadel_instance):
        """Test file with only a heading, no paragraphs."""
        content = "# Just a Title"

        with tempfile.NamedTemporaryFile(
            suffix=".md", delete=False, mode="w"
        ) as f:
            f.write(content)
            f.flush()
            path = Path(f.name)

        try:
            result = citadel_instance.get_markdown_summary(path)

            assert result["title"] == "Just a Title"
            assert result["summary"] == ""
        finally:
            path.unlink()

    def test_multiple_h1_headings(self, citadel_instance):
        """Test that only the first H1 is used as title."""
        content = dedent("""
            # First Title

            First paragraph.

            # Second Title

            Second paragraph.
        """).strip()

        with tempfile.NamedTemporaryFile(
            suffix=".md", delete=False, mode="w"
        ) as f:
            f.write(content)
            f.flush()
            path = Path(f.name)

        try:
            result = citadel_instance.get_markdown_summary(path)

            assert result["title"] == "First Title"
            assert "First paragraph" in result["summary"]
        finally:
            path.unlink()

    def test_h1_with_trailing_hashes(self, citadel_instance):
        """Test H1 heading with trailing hashes (optional in ATX style)."""
        content = dedent("""
            # Title with Trailing Hashes ###

            Description paragraph.
        """).strip()

        with tempfile.NamedTemporaryFile(
            suffix=".md", delete=False, mode="w"
        ) as f:
            f.write(content)
            f.flush()
            path = Path(f.name)

        try:
            result = citadel_instance.get_markdown_summary(path)

            assert result["title"] == "Title with Trailing Hashes"
        finally:
            path.unlink()

    def test_skip_lists(self, citadel_instance):
        """Test that list items are skipped when finding paragraph."""
        content = dedent("""
            # Project

            - Item 1
            - Item 2
            * Item 3

            1. Numbered item
            2. Another numbered item

            This is the actual description paragraph.
        """).strip()

        with tempfile.NamedTemporaryFile(
            suffix=".md", delete=False, mode="w"
        ) as f:
            f.write(content)
            f.flush()
            path = Path(f.name)

        try:
            result = citadel_instance.get_markdown_summary(path)

            assert "actual description" in result["summary"]
            assert "Item 1" not in result["summary"]
        finally:
            path.unlink()

    def test_skip_horizontal_rules(self, citadel_instance):
        """Test that horizontal rules are skipped."""
        content = dedent("""
            # Title

            ---

            ***

            ___

            This is the description after horizontal rules.
        """).strip()

        with tempfile.NamedTemporaryFile(
            suffix=".md", delete=False, mode="w"
        ) as f:
            f.write(content)
            f.flush()
            path = Path(f.name)

        try:
            result = citadel_instance.get_markdown_summary(path)

            assert "description after" in result["summary"]
        finally:
            path.unlink()

    def test_multiline_paragraph(self, citadel_instance):
        """Test that multi-line paragraphs are joined correctly."""
        content = dedent("""
            # Title

            This is the first line of a paragraph
            that spans multiple lines in the source
            but should be joined as a single paragraph.
        """).strip()

        with tempfile.NamedTemporaryFile(
            suffix=".md", delete=False, mode="w"
        ) as f:
            f.write(content)
            f.flush()
            path = Path(f.name)

        try:
            result = citadel_instance.get_markdown_summary(path)

            assert "first line" in result["summary"]
            assert "multiple lines" in result["summary"]
            assert "single paragraph" in result["summary"]
            # Should be joined with spaces
            assert "\n" not in result["summary"]
        finally:
            path.unlink()

    def test_skip_metadata_lines(self, citadel_instance):
        """Test that bold key-value metadata lines are skipped."""
        content = dedent("""
            # TESTPROG

            **File**: `cbl/TESTPROG.cbl`
            **Type**: COBOL
            **Analyzed**: 2026-01-01

            This is the actual program description.
        """).strip()

        with tempfile.NamedTemporaryFile(
            suffix=".md", delete=False, mode="w"
        ) as f:
            f.write(content)
            f.flush()
            path = Path(f.name)

        try:
            result = citadel_instance.get_markdown_summary(path)

            assert "actual program description" in result["summary"]
            assert "**File**" not in result["summary"]
            assert "COBOL" not in result["summary"]
        finally:
            path.unlink()
