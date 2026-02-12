"""Tests for mermaid diagram validation utilities."""

from __future__ import annotations

import json
from pathlib import Path
from unittest.mock import MagicMock, patch

import pytest

import war_rig.validation.mermaid_validator as mod
from war_rig.validation.mermaid_validator import (
    _check_mermaid_node,
    _is_valid_mermaid_regex,
    _validate_mermaid_batch,
    is_valid_mermaid,
    sanitize_mermaid_blocks,
)


# ---------------------------------------------------------------------------
# _is_valid_mermaid_regex  (formerly is_valid_mermaid)
# ---------------------------------------------------------------------------


class TestIsValidMermaidRegex:
    """Tests for the regex-only fallback."""

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
        assert _is_valid_mermaid_regex(content) is True

    def test_empty_string(self) -> None:
        assert _is_valid_mermaid_regex("") is False

    def test_whitespace_only(self) -> None:
        assert _is_valid_mermaid_regex("   \n\n  \t  ") is False

    def test_no_type_keyword(self) -> None:
        assert _is_valid_mermaid_regex("A --> B\nB --> C") is False

    def test_random_text(self) -> None:
        assert _is_valid_mermaid_regex("Hello world this is not a diagram") is False

    def test_leading_comments_before_type(self) -> None:
        content = "%% This is a comment\n%% Another comment\nflowchart TD\n  A --> B"
        assert _is_valid_mermaid_regex(content) is True

    def test_leading_blank_lines_before_type(self) -> None:
        content = "\n\n  \nsequenceDiagram\n  Alice->>Bob: Hi"
        assert _is_valid_mermaid_regex(content) is True

    def test_leading_comments_and_blanks_before_type(self) -> None:
        content = "\n%% comment\n\n%% another\ngraph LR\n  A --> B"
        assert _is_valid_mermaid_regex(content) is True

    def test_only_comments(self) -> None:
        assert _is_valid_mermaid_regex("%% just comments\n%% nothing else") is False

    def test_none_like(self) -> None:
        assert _is_valid_mermaid_regex("   ") is False


# ---------------------------------------------------------------------------
# is_valid_mermaid  (public API)
# ---------------------------------------------------------------------------


class TestIsValidMermaid:
    """Tests for is_valid_mermaid() — dispatches to node or regex."""

    @pytest.mark.parametrize(
        "content",
        [
            "flowchart TD\n  A --> B",
            "sequenceDiagram\n  Alice->>Bob: Hello",
            "graph LR\n  A --> B",
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
        assert is_valid_mermaid("   ") is False


# ---------------------------------------------------------------------------
# _check_mermaid_node  (availability probing)
# ---------------------------------------------------------------------------


class TestCheckMermaidNode:
    """Tests for Node.js availability detection."""

    def setup_method(self) -> None:
        # Reset cached availability before each test
        mod._mermaid_node_available = None

    def test_no_node_binary(self) -> None:
        with patch("war_rig.validation.mermaid_validator.shutil.which", return_value=None):
            assert _check_mermaid_node() is False

    def test_no_script_file(self) -> None:
        with (
            patch("war_rig.validation.mermaid_validator.shutil.which", return_value="/usr/bin/node"),
            patch.object(mod, "_SCRIPT_PATH", Path("/nonexistent/_mermaid_parse.mjs")),
        ):
            assert _check_mermaid_node() is False

    def test_no_node_modules(self, tmp_path: Path) -> None:
        script = tmp_path / "_mermaid_parse.mjs"
        script.write_text("// stub")
        with (
            patch("war_rig.validation.mermaid_validator.shutil.which", return_value="/usr/bin/node"),
            patch.object(mod, "_SCRIPT_PATH", script),
        ):
            assert _check_mermaid_node() is False

    def test_smoke_test_failure(self, tmp_path: Path) -> None:
        script = tmp_path / "_mermaid_parse.mjs"
        script.write_text("// stub")
        (tmp_path / "node_modules").mkdir()
        with (
            patch("war_rig.validation.mermaid_validator.shutil.which", return_value="/usr/bin/node"),
            patch.object(mod, "_SCRIPT_PATH", script),
            patch(
                "war_rig.validation.mermaid_validator._validate_mermaid_batch",
                side_effect=RuntimeError("boom"),
            ),
        ):
            assert _check_mermaid_node() is False

    def test_caching_returns_previous_result(self) -> None:
        mod._mermaid_node_available = True
        assert _check_mermaid_node() is True
        mod._mermaid_node_available = False
        assert _check_mermaid_node() is False

    def test_successful_probe(self, tmp_path: Path) -> None:
        script = tmp_path / "_mermaid_parse.mjs"
        script.write_text("// stub")
        (tmp_path / "node_modules").mkdir()
        with (
            patch("war_rig.validation.mermaid_validator.shutil.which", return_value="/usr/bin/node"),
            patch.object(mod, "_SCRIPT_PATH", script),
            patch(
                "war_rig.validation.mermaid_validator._validate_mermaid_batch",
                return_value=[True],
            ),
        ):
            assert _check_mermaid_node() is True
            assert mod._mermaid_node_available is True


# ---------------------------------------------------------------------------
# _validate_mermaid_batch  (mocked subprocess)
# ---------------------------------------------------------------------------


class TestValidateMermaidBatch:
    """Tests for the Node subprocess batch validation."""

    def _mock_run(self, results: list[dict], returncode: int = 0, stderr: str = "") -> MagicMock:
        mock = MagicMock()
        mock.returncode = returncode
        mock.stdout = json.dumps({"results": results})
        mock.stderr = stderr
        return mock

    def test_batch_valid_results(self) -> None:
        results_data = [{"valid": True}, {"valid": False, "error": "bad"}]
        with patch("war_rig.validation.mermaid_validator.subprocess.run") as mock_run:
            mock_run.return_value = self._mock_run(results_data)
            result = _validate_mermaid_batch(["graph LR\n  A-->B", "not valid"])
            assert result == [True, False]

    def test_subprocess_nonzero_exit(self) -> None:
        with patch("war_rig.validation.mermaid_validator.subprocess.run") as mock_run:
            mock_run.return_value = self._mock_run([], returncode=1, stderr="Error occurred")
            with pytest.raises(RuntimeError, match="mermaid parse script failed"):
                _validate_mermaid_batch(["graph TD\n  A-->B"])

    def test_subprocess_timeout(self) -> None:
        import subprocess

        with patch(
            "war_rig.validation.mermaid_validator.subprocess.run",
            side_effect=subprocess.TimeoutExpired(cmd="node", timeout=5),
        ):
            with pytest.raises(subprocess.TimeoutExpired):
                _validate_mermaid_batch(["graph TD\n  A-->B"])

    def test_timeout_scales_with_batch_size(self) -> None:
        results_data = [{"valid": True}] * 10
        with patch("war_rig.validation.mermaid_validator.subprocess.run") as mock_run:
            mock_run.return_value = self._mock_run(results_data)
            _validate_mermaid_batch(["graph LR\n  A-->B"] * 10)
            call_kwargs = mock_run.call_args[1]
            assert call_kwargs["timeout"] == 20  # max(5, 2*10)


# ---------------------------------------------------------------------------
# Node fallback behaviour in is_valid_mermaid
# ---------------------------------------------------------------------------


class TestNodeFallback:
    """Verify is_valid_mermaid falls back to regex when node is unavailable."""

    def setup_method(self) -> None:
        mod._mermaid_node_available = None

    def test_uses_regex_when_node_unavailable(self) -> None:
        with patch("war_rig.validation.mermaid_validator._check_mermaid_node", return_value=False):
            assert is_valid_mermaid("flowchart TD\n  A --> B") is True
            assert is_valid_mermaid("not a diagram") is False

    def test_uses_node_when_available(self) -> None:
        with (
            patch("war_rig.validation.mermaid_validator._check_mermaid_node", return_value=True),
            patch(
                "war_rig.validation.mermaid_validator._validate_mermaid_node",
                return_value=False,
            ) as mock_node,
        ):
            result = is_valid_mermaid("flowchart TD\n  A --> B")
            assert result is False
            mock_node.assert_called_once()


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
        assert "sequenceDiagram" in result
        assert "erDiagram" in result
        assert "not a real diagram" not in result
        assert result.count("<!-- Invalid mermaid diagram removed -->") == 1

    def test_whitespace_only_content_replaced(self) -> None:
        md = "```mermaid\n   \n  \n```"
        result = sanitize_mermaid_blocks(md)
        assert "<!-- Invalid mermaid diagram removed -->" in result

    def test_logs_warning_for_removed_block(
        self, caplog: pytest.LogCaptureFixture
    ) -> None:
        md = "```mermaid\nbogus content here\n```"
        with caplog.at_level("WARNING", logger="war_rig.validation.mermaid_validator"):
            sanitize_mermaid_blocks(md)
        assert "Removing invalid mermaid block" in caplog.text
        assert "bogus content here" in caplog.text

    def test_batch_validation_used_when_node_available(self) -> None:
        """When node is available, sanitize uses batch validation."""
        md = (
            "```mermaid\n"
            "flowchart TD\n"
            "  A --> B\n"
            "```\n\n"
            "```mermaid\n"
            "junk\n"
            "```\n"
        )
        with (
            patch("war_rig.validation.mermaid_validator._check_mermaid_node", return_value=True),
            patch(
                "war_rig.validation.mermaid_validator._validate_mermaid_batch",
                return_value=[True, False],
            ) as mock_batch,
        ):
            result = sanitize_mermaid_blocks(md)
            mock_batch.assert_called_once()
            assert "flowchart TD" in result
            assert "junk" not in result
            assert "<!-- Invalid mermaid diagram removed -->" in result

    def test_batch_failure_falls_back_to_regex(self) -> None:
        """When batch node validation raises, fall back to regex."""
        md = (
            "```mermaid\n"
            "flowchart TD\n"
            "  A --> B\n"
            "```\n"
        )
        with (
            patch("war_rig.validation.mermaid_validator._check_mermaid_node", return_value=True),
            patch(
                "war_rig.validation.mermaid_validator._validate_mermaid_batch",
                side_effect=RuntimeError("node crashed"),
            ),
        ):
            result = sanitize_mermaid_blocks(md)
            # Regex fallback sees valid type keyword → keeps it
            assert "flowchart TD" in result


# ---------------------------------------------------------------------------
# Integration tests — real Node.js + mermaid (skipped when not installed)
# ---------------------------------------------------------------------------

_node_modules_exist = (Path(__file__).resolve().parents[2] / "war_rig" / "validation" / "node_modules").is_dir()


@pytest.mark.integration
@pytest.mark.skipif(not _node_modules_exist, reason="node_modules not installed")
class TestMermaidNodeIntegration:
    """Integration tests that exercise the real mermaid parser."""

    def setup_method(self) -> None:
        mod._mermaid_node_available = None

    def test_valid_flowchart(self) -> None:
        assert _validate_mermaid_batch(["flowchart LR\n  X --> Y"])[0] is True

    def test_valid_graph(self) -> None:
        assert _validate_mermaid_batch(["graph TD\n  Start --> End"])[0] is True

    def test_valid_pie(self) -> None:
        assert _validate_mermaid_batch(['pie\n  "Cats" : 40\n  "Dogs" : 60'])[0] is True

    def test_invalid_broken_arrows(self) -> None:
        result = _validate_mermaid_batch(["flowchart TD\n  A --> --> B"])
        assert result[0] is False

    def test_invalid_no_type(self) -> None:
        result = _validate_mermaid_batch(["just some random text"])
        assert result[0] is False

    def test_empty_diagram(self) -> None:
        result = _validate_mermaid_batch([""])
        assert result[0] is False

    def test_batch_mixed(self) -> None:
        diagrams = [
            "flowchart LR\n  A --> B",
            "not a diagram at all",
            "graph TD\n  X --> Y",
        ]
        results = _validate_mermaid_batch(diagrams)
        assert results == [True, False, True]

    def test_sanitize_catches_syntax_errors(self) -> None:
        """A diagram with valid type but broken syntax gets removed."""
        md = (
            "# Heading\n\n"
            "```mermaid\n"
            "flowchart TD\n"
            "  A --> --> B\n"
            "```\n\n"
            "Done."
        )
        result = sanitize_mermaid_blocks(md)
        assert "<!-- Invalid mermaid diagram removed -->" in result
        assert "Done." in result

    def test_is_valid_mermaid_rejects_broken_syntax(self) -> None:
        assert is_valid_mermaid("flowchart TD\n  A --> --> B") is False

    def test_is_valid_mermaid_accepts_valid(self) -> None:
        assert is_valid_mermaid("flowchart LR\n  A --> B") is True
