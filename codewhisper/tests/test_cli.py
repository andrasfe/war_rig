"""Tests for codewhisper CLI.

This module tests:
- CLI argument parsing
- Configuration building from CLI args
- Path validation
- Interactive and single-query modes
"""

from pathlib import Path
from unittest.mock import AsyncMock, patch

from typer.testing import CliRunner

from codewhisper.cli import app, setup_logging

runner = CliRunner()


class TestSetupLogging:
    """Tests for setup_logging function."""

    def test_setup_logging_verbose(self) -> None:
        """Test verbose logging sets DEBUG level."""
        import logging

        # Reset root logger first
        root = logging.getLogger()
        for handler in root.handlers[:]:
            root.removeHandler(handler)

        setup_logging(verbose=True)
        # The root logger level should be DEBUG when verbose
        assert logging.getLogger().level == logging.DEBUG

    def test_setup_logging_non_verbose(self) -> None:
        """Test non-verbose logging sets INFO level."""
        import logging

        # Reset root logger first
        root = logging.getLogger()
        for handler in root.handlers[:]:
            root.removeHandler(handler)

        setup_logging(verbose=False)
        assert logging.getLogger().level == logging.INFO


class TestCLIHelp:
    """Tests for CLI help and basic invocation."""

    def test_help_message(self) -> None:
        """Test that --help works."""
        result = runner.invoke(app, ["--help"])
        assert result.exit_code == 0
        assert "CodeWhisper" in result.stdout or "codewhisper" in result.stdout.lower()
        assert "--skills-dir" in result.stdout
        assert "--code-dir" in result.stdout


class TestCLIPathValidation:
    """Tests for CLI path validation."""

    def test_skills_dir_not_found(self, tmp_path: Path) -> None:
        """Test error when skills directory doesn't exist."""
        code_dir = tmp_path / "code"
        code_dir.mkdir()

        result = runner.invoke(
            app,
            [
                "--skills-dir",
                str(tmp_path / "nonexistent"),
                "--code-dir",
                str(code_dir),
            ],
        )

        assert result.exit_code == 1
        assert "not found" in result.stdout.lower() or "does not exist" in result.stdout.lower()

    def test_code_dir_not_found(self, tmp_path: Path) -> None:
        """Test error when code directory doesn't exist."""
        skills_dir = tmp_path / "skills"
        skills_dir.mkdir()

        result = runner.invoke(
            app,
            [
                "--skills-dir",
                str(skills_dir),
                "--code-dir",
                str(tmp_path / "nonexistent"),
            ],
        )

        assert result.exit_code == 1
        assert "not found" in result.stdout.lower() or "does not exist" in result.stdout.lower()


class TestCLIOptions:
    """Tests for CLI option handling."""

    def test_verbose_flag(self, tmp_skills_dir: Path, tmp_code_dir: Path) -> None:
        """Test verbose flag is accepted."""
        with patch("codewhisper.cli.run_interactive", new_callable=AsyncMock):
            # Mock the asyncio.run to prevent actual execution
            with patch("asyncio.run", side_effect=KeyboardInterrupt):
                result = runner.invoke(
                    app,
                    [
                        "--skills-dir",
                        str(tmp_skills_dir),
                        "--code-dir",
                        str(tmp_code_dir),
                        "--verbose",
                    ],
                )
            # KeyboardInterrupt exits with 130 in some environments
            # Just verify the command was parsed correctly
            assert result.exit_code in (0, 1, -1, 130)

    def test_model_option(self, tmp_skills_dir: Path, tmp_code_dir: Path) -> None:
        """Test model option is accepted."""
        with patch("codewhisper.cli.run_interactive", new_callable=AsyncMock):
            with patch("asyncio.run", side_effect=KeyboardInterrupt):
                result = runner.invoke(
                    app,
                    [
                        "--skills-dir",
                        str(tmp_skills_dir),
                        "--code-dir",
                        str(tmp_code_dir),
                        "--model",
                        "openai/gpt-4o",
                    ],
                )
            assert result.exit_code in (0, 1, -1, 130)

    def test_provider_option(self, tmp_skills_dir: Path, tmp_code_dir: Path) -> None:
        """Test provider option is accepted."""
        with patch("codewhisper.cli.run_interactive", new_callable=AsyncMock):
            with patch("asyncio.run", side_effect=KeyboardInterrupt):
                result = runner.invoke(
                    app,
                    [
                        "--skills-dir",
                        str(tmp_skills_dir),
                        "--code-dir",
                        str(tmp_code_dir),
                        "--provider",
                        "anthropic",
                    ],
                )
            assert result.exit_code in (0, 1, -1, 130)

    def test_query_option_invokes_single_query(
        self, tmp_skills_dir: Path, tmp_code_dir: Path
    ) -> None:
        """Test --query option runs single query mode."""
        with patch("codewhisper.cli.run_single_query", new_callable=AsyncMock):
            with patch("asyncio.run") as mock_asyncio:
                runner.invoke(
                    app,
                    [
                        "--skills-dir",
                        str(tmp_skills_dir),
                        "--code-dir",
                        str(tmp_code_dir),
                        "--query",
                        "What does TESTPROG do?",
                    ],
                )

            # asyncio.run should be called with run_single_query
            assert mock_asyncio.called


class TestCLIShortOptions:
    """Tests for CLI short option aliases."""

    def test_short_options(self, tmp_skills_dir: Path, tmp_code_dir: Path) -> None:
        """Test short option aliases work."""
        with patch("codewhisper.cli.run_interactive", new_callable=AsyncMock):
            with patch("asyncio.run", side_effect=KeyboardInterrupt):
                result = runner.invoke(
                    app,
                    [
                        "-s",
                        str(tmp_skills_dir),
                        "-c",
                        str(tmp_code_dir),
                        "-m",
                        "test-model",
                        "-p",
                        "openrouter",
                        "-v",
                    ],
                )
            assert result.exit_code in (0, 1, -1, 130)

    def test_short_query_option(
        self, tmp_skills_dir: Path, tmp_code_dir: Path
    ) -> None:
        """Test -q short option for query."""
        with patch("codewhisper.cli.run_single_query", new_callable=AsyncMock):
            with patch("asyncio.run"):
                result = runner.invoke(
                    app,
                    [
                        "-s",
                        str(tmp_skills_dir),
                        "-c",
                        str(tmp_code_dir),
                        "-q",
                        "Test query",
                    ],
                )
            # Should not error on parsing
            assert result.exit_code in (0, 1)
