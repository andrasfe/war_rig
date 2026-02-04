"""Configuration models for CodeWhisper.

This module defines Pydantic models for configuring the CodeWhisper agent,
including paths to skills and code directories, model selection, and
behavioral settings.

Configuration is compatible with war_rig's .env file - it reads LLM_PROVIDER,
IMPERATOR_MODEL, and OPENROUTER_API_KEY directly.

Example:
    from codewhisper.config import AgentConfig

    config = AgentConfig(
        skills_dir="./example_output/skills",
        code_dir="./src",
        model="anthropic/claude-sonnet-4-20250514",
    )
"""

import os
from pathlib import Path
from typing import Literal

from pydantic import BaseModel, Field, field_validator
from pydantic_settings import BaseSettings, SettingsConfigDict

ProviderType = str  # Any provider supported by the system


def _get_default_provider() -> str:
    """Get default provider from war_rig's LLM_PROVIDER env var."""
    return os.environ.get("LLM_PROVIDER", "openrouter")


def _get_default_model() -> str:
    """Get default model from war_rig's IMPERATOR_MODEL env var.

    Uses IMPERATOR (judgment/decision model) for the chat CLI since it
    needs reasoning and decision-making capabilities.
    """
    return os.environ.get(
        "IMPERATOR_MODEL",
        os.environ.get("LLM_DEFAULT_MODEL", "anthropic/claude-sonnet-4-20250514"),
    )


def _get_use_minions_default() -> bool:
    """Default to True if minion model is available."""
    return bool(os.environ.get("MINION_SCRIBE_MODEL"))


class AgentConfig(BaseSettings):
    """Configuration for the CodeWhisper agent.

    This configuration is compatible with war_rig's .env file:
    - Reads LLM_PROVIDER for the provider setting
    - Reads IMPERATOR_MODEL for the default model (judgment/decision model)
    - Reads OPENROUTER_API_KEY, ANTHROPIC_API_KEY, OPENAI_API_KEY

    Can also be configured with CODEWHISPER_ prefixed env vars which
    take precedence.

    Attributes:
        skills_dir: Path to the directory containing skill files.
        code_dir: Path to the source code directory to explore.
        model: LLM model identifier to use for the agent.
        provider: LLM provider name (openrouter, anthropic, openai).
        temperature: Sampling temperature for the LLM.
        max_history: Maximum number of conversation turns to keep in context.
        max_tokens: Maximum tokens in LLM response.
        verbose: Enable verbose logging.

    Example:
        # Works with war_rig's .env (LLM_PROVIDER, SCRIBE_MODEL, etc.)
        config = AgentConfig(
            skills_dir="./skills",
            code_dir="./src",
        )

        # Or with CODEWHISPER_ prefix
        export CODEWHISPER_MODEL=openai/gpt-4o
        config = AgentConfig()
    """

    model_config = SettingsConfigDict(
        env_prefix="CODEWHISPER_",
        env_file=".env",
        env_file_encoding="utf-8",
        extra="ignore",
    )

    skills_dir: Path = Field(
        default=Path("./skills"),
        description="Path to the skills directory containing SKILL.md files",
    )
    code_dir: Path = Field(
        default=Path("."),
        description="Path to the source code directory to explore",
    )
    model: str = Field(
        default_factory=_get_default_model,
        description="LLM model identifier",
    )
    provider: ProviderType = Field(
        default_factory=_get_default_provider,
        description="LLM provider (openrouter, anthropic, openai)",
    )
    temperature: float = Field(
        default=0.3,
        ge=0.0,
        le=2.0,
        description="Sampling temperature for the LLM",
    )
    max_history: int = Field(
        default=20,
        gt=0,
        description="Maximum conversation turns to keep in context",
    )
    max_tokens: int = Field(
        default=4096,
        gt=0,
        le=32000,
        description="Maximum tokens in LLM response",
    )
    verbose: bool = Field(
        default=False,
        description="Enable verbose logging",
    )

    use_minions: bool = Field(
        default_factory=_get_use_minions_default,
        description="Use minions for processing large tool results",
    )

    @field_validator("skills_dir", "code_dir", mode="before")
    @classmethod
    def resolve_path(cls, v: str | Path) -> Path:
        """Resolve paths to absolute form."""
        return Path(v).expanduser().resolve()


class SkillMetadata(BaseModel):
    """Metadata for a loaded skill.

    Parsed from the YAML frontmatter of SKILL.md files.

    Attributes:
        name: Unique identifier for the skill.
        description: Brief description of what the skill provides.
        file_path: Path to the skill file.
        content: Full content of the skill file (without frontmatter).
    """

    name: str = Field(
        ...,
        description="Unique skill identifier",
    )
    description: str = Field(
        default="",
        description="Brief description of the skill",
    )
    file_path: Path = Field(
        ...,
        description="Path to the skill file",
    )
    content: str = Field(
        default="",
        description="Full skill content (markdown)",
    )


class SearchResult(BaseModel):
    """Result from a code search operation.

    Attributes:
        file_path: Path to the file containing the match.
        line_number: Line number of the match (1-indexed).
        line_content: Content of the matching line.
        context_before: Lines before the match for context.
        context_after: Lines after the match for context.
    """

    file_path: Path = Field(
        ...,
        description="Path to the file containing the match",
    )
    line_number: int = Field(
        ...,
        gt=0,
        description="Line number of the match (1-indexed)",
    )
    line_content: str = Field(
        ...,
        description="Content of the matching line",
    )
    context_before: list[str] = Field(
        default_factory=list,
        description="Lines before the match",
    )
    context_after: list[str] = Field(
        default_factory=list,
        description="Lines after the match",
    )
