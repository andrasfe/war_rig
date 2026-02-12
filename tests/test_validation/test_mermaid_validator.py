"""Tests for mermaid diagram validation utilities."""

from __future__ import annotations

import pytest

from war_rig.validation.mermaid_validator import (
    is_valid_mermaid,
    sanitize_mermaid_blocks,
)

# ---------------------------------------------------------------------------
# is_valid_mermaid
# ---------------------------------------------------------------------------

class TestIsValidMermaid:
    """Tests for is_valid_mermaid()."""

    @pytest.mark.parametrize(
        "content",
        [
            "flowchart TD\n  A --> B",
            "flowchart LR\n  X --> Y",
            "sequenceDiagram\n  Alice->>Bob: Hello",
            "graph LR\n  A --> B",
            "graph TD\n  A --> B",
            "classDiagram\n  class Animal",
            "stateDiagram\n  [*] --> Still",
            "stateDiagram-v2\n  [*] --> Still",
            "erDiagram\n  CUSTOMER ||--o{ ORDER : places",
            "journey\n  title My working day",
            "gantt\n  title A Gantt Diagram",
            "pie\n  title Pets",
            "gitGraph\n  commit",
            "mindmap\n  root((mindmap))",
            "timeline\n  title Timeline",
            "sankey\n  A,B,10",
            "block-beta\n  columns 1",
            "xychart-beta\n  title Chart",
            "quadrantChart\n  title Quadrant",
            "requirementDiagram\n  requirement test_req",
            "C4Context\n  title System Context",
            "C4Container\n  title Container",
            "C4Component\n  title Component",
            "C4Deployment\n  title Deployment",
            "packet-beta\n  0-15: Source",
            "kanban\n  Todo",
            "architecture-beta\n  group api",
        ],
        ids=lambda c: c.split("\n")[0].split()[0],
    )
    def test_valid_types(self, content: str) -> None:
        assert is_valid_mermaid(content) is True

    def test_empty_string(self) -> None:
        assert is_valid_mermaid("") is False

    def test_whitespace_only(self) -> None:
        assert is_valid_mermaid("   \n\n  \t  ") is False

    def test_no_type_keyword(self) -> None:
        assert is_valid_mermaid("A --> B\nB --> C") is False

    def test_random_text(self) -> None:
        assert is_valid_mermaid("Hello world this is not a diagram") is False

    def test_leading_comments_before_type(self) -> None:
        content = "%% This is a comment\n%% Another comment\nflowchart TD\n  A --> B"
        assert is_valid_mermaid(content) is True

    def test_leading_blank_lines_before_type(self) -> None:
        content = "\n\n  \nsequenceDiagram\n  Alice->>Bob: Hi"
        assert is_valid_mermaid(content) is True

    def test_leading_comments_and_blanks_before_type(self) -> None:
        content = "\n%% comment\n\n%% another\ngraph LR\n  A --> B"
        assert is_valid_mermaid(content) is True

    def test_only_comments(self) -> None:
        assert is_valid_mermaid("%% just comments\n%% nothing else") is False

    def test_none_like(self) -> None:
        # Empty after strip
        assert is_valid_mermaid("   ") is False


# ---------------------------------------------------------------------------
# sanitize_mermaid_blocks
# ---------------------------------------------------------------------------

class TestSanitizeMermaidBlocks:
    """Tests for sanitize_mermaid_blocks()."""

    def test_no_diagrams_passthrough(self) -> None:
        md = "# Title\n\nSome text.\n\n## Section\n\nMore text."
        assert sanitize_mermaid_blocks(md) == md

    def test_valid_diagram_unchanged(self) -> None:
        md = (
            "# Title\n\n"
            "```mermaid\n"
            "flowchart TD\n"
            "  A --> B\n"
            "```\n\n"
            "Done."
        )
        assert sanitize_mermaid_blocks(md) == md

    def test_invalid_diagram_replaced(self) -> None:
        md = (
            "# Title\n\n"
            "```mermaid\n"
            "A --> B\n"
            "```\n\n"
            "Done."
        )
        result = sanitize_mermaid_blocks(md)
        assert "```mermaid" not in result
        assert "<!-- Invalid mermaid diagram removed -->" in result
        assert "Done." in result

    def test_empty_fenced_block_replaced(self) -> None:
        md = "Before\n\n```mermaid\n\n```\n\nAfter"
        result = sanitize_mermaid_blocks(md)
        assert "<!-- Invalid mermaid diagram removed -->" in result
        assert "Before" in result
        assert "After" in result

    def test_mixed_valid_and_invalid(self) -> None:
        md = (
            "# Doc\n\n"
            "```mermaid\n"
            "sequenceDiagram\n"
            "  Alice->>Bob: Hello\n"
            "```\n\n"
            "```mermaid\n"
            "not a real diagram\n"
            "```\n\n"
            "```mermaid\n"
            "erDiagram\n"
            "  CUSTOMER ||--o{ ORDER : places\n"
            "```\n"
        )
        result = sanitize_mermaid_blocks(md)
        # Valid diagrams kept
        assert "sequenceDiagram" in result
        assert "erDiagram" in result
        # Invalid replaced
        assert "not a real diagram" not in result
        assert result.count("<!-- Invalid mermaid diagram removed -->") == 1

    def test_whitespace_only_content_replaced(self) -> None:
        md = "```mermaid\n   \n  \n```"
        result = sanitize_mermaid_blocks(md)
        assert "<!-- Invalid mermaid diagram removed -->" in result

    def test_logs_warning_for_removed_block(self, caplog: pytest.LogCaptureFixture) -> None:
        md = "```mermaid\nbogus content here\n```"
        with caplog.at_level("WARNING", logger="war_rig.validation.mermaid_validator"):
            sanitize_mermaid_blocks(md)
        assert "Removing invalid mermaid block" in caplog.text
        assert "bogus content here" in caplog.text
