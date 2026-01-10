"""Configuration management for War Rig.

This module provides configuration with support for .env files via python-dotenv,
supporting OpenRouter as the API provider with per-agent model configuration.

Typical usage:
    # Load from .env file automatically
    config = load_config()

    # Access agent configuration
    print(config.scribe.model)
    print(config.max_iterations)
"""

import os
from pathlib import Path
from typing import Literal

from dotenv import load_dotenv
from pydantic import BaseModel, Field
from pydantic_settings import BaseSettings, SettingsConfigDict

# Load .env file from current directory or parent directories
load_dotenv()


class APIConfig(BaseModel):
    """Configuration for the API provider."""

    provider: Literal["openrouter", "anthropic"] = Field(
        default="openrouter",
        description="API provider to use",
    )
    api_key: str | None = Field(
        default=None,
        description="API key for the provider",
    )
    base_url: str = Field(
        default="https://openrouter.ai/api/v1",
        description="Base URL for the API",
    )
    site_url: str | None = Field(
        default=None,
        description="Site URL for OpenRouter rankings",
    )
    site_name: str | None = Field(
        default=None,
        description="Site name for OpenRouter rankings",
    )


class ModelConfig(BaseModel):
    """Configuration for an LLM model."""

    model: str = Field(
        default="anthropic/claude-sonnet-4-20250514",
        description="Model identifier for the LLM",
    )
    temperature: float = Field(
        default=0.3,
        ge=0.0,
        le=2.0,
        description="Sampling temperature for generation",
    )
    max_tokens: int = Field(
        default=4000,
        gt=0,
        description="Maximum tokens in response",
    )


class ScribeConfig(ModelConfig):
    """Configuration specific to the Scribe agent.

    The Scribe analyzes source code and fills out documentation templates.
    Lower temperature (0.3) encourages more deterministic, factual output.
    """

    model: str = Field(default="anthropic/claude-sonnet-4-20250514")
    temperature: float = Field(default=0.3)
    max_tokens: int = Field(default=4000)


class ChallengerConfig(ModelConfig):
    """Configuration specific to the Challenger agent.

    The Challenger validates documentation and asks probing questions.
    Slightly higher temperature (0.5) allows for more creative questioning.
    """

    model: str = Field(default="openai/gpt-4o-2024-11-20")
    temperature: float = Field(default=0.5)
    max_tokens: int = Field(default=2000)


class ImperatorConfig(ModelConfig):
    """Configuration specific to the Imperator agent.

    The Imperator reviews documentation and makes approval decisions.
    Lower temperature (0.2) ensures consistent, decisive output.
    """

    model: str = Field(default="anthropic/claude-sonnet-4-20250514")
    temperature: float = Field(default=0.2)
    max_tokens: int = Field(default=2000)


class PreprocessingConfig(BaseModel):
    """Configuration for the preprocessing phase.

    Controls which structural elements are extracted from source code
    before agent analysis begins.
    """

    extract_paragraphs: bool = Field(default=True)
    extract_performs: bool = Field(default=True)
    extract_calls: bool = Field(default=True)
    extract_copybooks: bool = Field(default=True)
    extract_sql: bool = Field(default=True)
    extract_cics: bool = Field(default=True)


class FileExtensionsConfig(BaseModel):
    """Configuration for recognized file extensions by type."""

    cobol: list[str] = Field(default=[".cbl", ".cob", ".CBL", ".COB"])
    copybook: list[str] = Field(default=[".cpy", ".CPY", ".copy", ".COPY"])
    jcl: list[str] = Field(default=[".jcl", ".JCL"])
    bms: list[str] = Field(default=[".bms", ".BMS"])
    pli: list[str] = Field(default=[".pli", ".PLI", ".pl1", ".PL1"])


class LoggingConfig(BaseModel):
    """Configuration for logging."""

    level: str = Field(default="INFO")
    log_file: Path | None = Field(default=None)
    save_dialogues: bool = Field(default=True)


class CheckpointConfig(BaseModel):
    """Configuration for checkpointing."""

    enabled: bool = Field(default=True)
    frequency: str = Field(default="per_program")
    directory: Path = Field(default=Path("./output/checkpoints"))


class SystemConfig(BaseModel):
    """System-wide configuration settings."""

    input_directory: Path = Field(
        default=Path("./input"),
        description="Directory containing source files to analyze",
    )
    output_directory: Path = Field(
        default=Path("./output"),
        description="Directory for generated documentation",
    )
    checkpoint_frequency: str = Field(
        default="per_program",
        description="How often to save checkpoints",
    )
    preprocessing: PreprocessingConfig = Field(default_factory=PreprocessingConfig)
    file_extensions: FileExtensionsConfig = Field(default_factory=FileExtensionsConfig)


class WarRigConfig(BaseSettings):
    """Main configuration for the War Rig system.

    Configuration is loaded from environment variables (with .env support).
    See .env.example for all available options.
    """

    model_config = SettingsConfigDict(
        env_prefix="",
        extra="ignore",
    )

    # War Rig identification
    rig_id: str = Field(default="ALPHA")

    # API configuration (loaded from env)
    api_provider: str = Field(default="openrouter")
    openrouter_api_key: str | None = Field(default=None)
    openrouter_base_url: str = Field(default="https://openrouter.ai/api/v1")
    openrouter_site_url: str | None = Field(default=None)
    openrouter_site_name: str | None = Field(default=None)

    # Legacy Anthropic support
    anthropic_api_key: str | None = Field(default=None)

    # Agent model configurations (loaded from env)
    scribe_model: str = Field(default="anthropic/claude-sonnet-4-20250514")
    scribe_temperature: float = Field(default=0.3)
    scribe_max_tokens: int = Field(default=4000)

    challenger_model: str = Field(default="openai/gpt-4o-2024-11-20")
    challenger_temperature: float = Field(default=0.5)
    challenger_max_tokens: int = Field(default=2000)

    imperator_model: str = Field(default="anthropic/claude-sonnet-4-20250514")
    imperator_temperature: float = Field(default=0.2)
    imperator_max_tokens: int = Field(default=2000)

    # Workflow limits
    max_iterations: int = Field(default=3, ge=1, le=10)
    max_questions_per_round: int = Field(default=5, ge=1, le=20)
    max_chrome_tickets: int = Field(default=5, ge=1, le=20)

    # Paths
    input_directory: Path = Field(default=Path("./input"))
    output_directory: Path = Field(default=Path("./output"))

    # Preprocessing flags
    extract_paragraphs: bool = Field(default=True)
    extract_performs: bool = Field(default=True)
    extract_calls: bool = Field(default=True)
    extract_copybooks: bool = Field(default=True)
    extract_sql: bool = Field(default=True)
    extract_cics: bool = Field(default=True)

    # Logging
    log_level: str = Field(default="INFO")
    log_file: str | None = Field(default=None)
    save_dialogues: bool = Field(default=True)

    # Checkpointing
    checkpoint_enabled: bool = Field(default=True)
    checkpoint_frequency: str = Field(default="per_program")
    checkpoint_directory: Path = Field(default=Path("./output/checkpoints"))

    @property
    def api(self) -> APIConfig:
        """Get API configuration."""
        return APIConfig(
            provider=self.api_provider,
            api_key=self.openrouter_api_key or self.anthropic_api_key,
            base_url=self.openrouter_base_url,
            site_url=self.openrouter_site_url,
            site_name=self.openrouter_site_name,
        )

    @property
    def scribe(self) -> ScribeConfig:
        """Get Scribe agent configuration."""
        return ScribeConfig(
            model=self.scribe_model,
            temperature=self.scribe_temperature,
            max_tokens=self.scribe_max_tokens,
        )

    @property
    def challenger(self) -> ChallengerConfig:
        """Get Challenger agent configuration."""
        return ChallengerConfig(
            model=self.challenger_model,
            temperature=self.challenger_temperature,
            max_tokens=self.challenger_max_tokens,
        )

    @property
    def imperator(self) -> ImperatorConfig:
        """Get Imperator agent configuration."""
        return ImperatorConfig(
            model=self.imperator_model,
            temperature=self.imperator_temperature,
            max_tokens=self.imperator_max_tokens,
        )

    @property
    def system(self) -> SystemConfig:
        """Get system configuration."""
        return SystemConfig(
            input_directory=self.input_directory,
            output_directory=self.output_directory,
            checkpoint_frequency=self.checkpoint_frequency,
            preprocessing=PreprocessingConfig(
                extract_paragraphs=self.extract_paragraphs,
                extract_performs=self.extract_performs,
                extract_calls=self.extract_calls,
                extract_copybooks=self.extract_copybooks,
                extract_sql=self.extract_sql,
                extract_cics=self.extract_cics,
            ),
        )

    @property
    def logging(self) -> LoggingConfig:
        """Get logging configuration."""
        return LoggingConfig(
            level=self.log_level,
            log_file=Path(self.log_file) if self.log_file else None,
            save_dialogues=self.save_dialogues,
        )

    @property
    def checkpoint(self) -> CheckpointConfig:
        """Get checkpoint configuration."""
        return CheckpointConfig(
            enabled=self.checkpoint_enabled,
            frequency=self.checkpoint_frequency,
            directory=self.checkpoint_directory,
        )

    def get_agent_config(self, agent_name: str) -> ModelConfig:
        """Get configuration for a specific agent by name.

        Args:
            agent_name: One of 'scribe', 'challenger', or 'imperator'.

        Returns:
            The agent's ModelConfig.

        Raises:
            ValueError: If agent_name is not recognized.
        """
        agent_configs: dict[str, ModelConfig] = {
            "scribe": self.scribe,
            "challenger": self.challenger,
            "imperator": self.imperator,
        }

        if agent_name not in agent_configs:
            raise ValueError(
                f"Unknown agent: {agent_name}. "
                f"Expected one of: {list(agent_configs.keys())}"
            )

        return agent_configs[agent_name]


def load_config() -> WarRigConfig:
    """Load configuration from environment variables.

    This function loads .env file automatically and creates
    the configuration from environment variables.

    Returns:
        Configured WarRigConfig instance.
    """
    # Ensure .env is loaded
    load_dotenv()
    return WarRigConfig()
