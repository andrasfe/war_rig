"""Configuration management for War Rig.

This module provides Pydantic Settings-based configuration with support for
YAML file loading and environment variable overrides.

Typical usage:
    # Load from default locations
    config = WarRigConfig()

    # Load from specific file
    config = WarRigConfig.from_yaml("path/to/config.yaml")

    # Access nested configuration
    print(config.scribe.model)
    print(config.max_iterations)
"""

from pathlib import Path
from typing import Any

import yaml
from pydantic import BaseModel, Field
from pydantic_settings import BaseSettings, SettingsConfigDict


class ModelConfig(BaseModel):
    """Configuration for an LLM model."""

    model: str = Field(
        default="claude-sonnet-4-20250514",
        description="Model identifier for the LLM",
    )
    temperature: float = Field(
        default=0.3,
        ge=0.0,
        le=1.0,
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

    temperature: float = Field(default=0.3)
    max_tokens: int = Field(default=4000)


class ChallengerConfig(ModelConfig):
    """Configuration specific to the Challenger agent.

    The Challenger validates documentation and asks probing questions.
    Slightly higher temperature (0.5) allows for more creative questioning.
    """

    temperature: float = Field(default=0.5)
    max_tokens: int = Field(default=2000)


class ImperatorConfig(ModelConfig):
    """Configuration specific to the Imperator agent.

    The Imperator reviews documentation and makes approval decisions.
    Lower temperature (0.2) ensures consistent, decisive output.
    """

    temperature: float = Field(default=0.2)
    max_tokens: int = Field(default=2000)


class PreprocessingConfig(BaseModel):
    """Configuration for the preprocessing phase.

    Controls which structural elements are extracted from source code
    before agent analysis begins.
    """

    extract_paragraphs: bool = Field(
        default=True,
        description="Extract COBOL paragraph definitions",
    )
    extract_performs: bool = Field(
        default=True,
        description="Extract PERFORM statements and targets",
    )
    extract_calls: bool = Field(
        default=True,
        description="Extract CALL statements and targets",
    )
    extract_copybooks: bool = Field(
        default=True,
        description="Extract COPY statements",
    )
    extract_sql: bool = Field(
        default=True,
        description="Extract embedded SQL statements",
    )
    extract_cics: bool = Field(
        default=True,
        description="Extract CICS commands",
    )


class FileExtensionsConfig(BaseModel):
    """Configuration for recognized file extensions by type."""

    cobol: list[str] = Field(
        default=[".cbl", ".cob", ".CBL", ".COB"],
        description="COBOL source file extensions",
    )
    copybook: list[str] = Field(
        default=[".cpy", ".CPY", ".copy", ".COPY"],
        description="Copybook file extensions",
    )
    jcl: list[str] = Field(
        default=[".jcl", ".JCL"],
        description="JCL file extensions",
    )
    bms: list[str] = Field(
        default=[".bms", ".BMS"],
        description="BMS map file extensions",
    )
    pli: list[str] = Field(
        default=[".pli", ".PLI", ".pl1", ".PL1"],
        description="PL/I source file extensions",
    )


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
    preprocessing: PreprocessingConfig = Field(
        default_factory=PreprocessingConfig,
        description="Preprocessing configuration",
    )
    file_extensions: FileExtensionsConfig = Field(
        default_factory=FileExtensionsConfig,
        description="File extension mappings",
    )


class WarRigConfig(BaseSettings):
    """Main configuration for the War Rig system.

    Configuration is loaded from (in order of precedence):
    1. Environment variables (prefixed with WAR_RIG_)
    2. YAML configuration file
    3. Default values

    Example YAML configuration:
        war_rig:
          rig_id: "ALPHA"
          max_iterations: 3
          scribe:
            model: "claude-sonnet-4-20250514"
            temperature: 0.3
    """

    model_config = SettingsConfigDict(
        env_prefix="WAR_RIG_",
        env_nested_delimiter="__",
        extra="ignore",
    )

    # War Rig identification
    rig_id: str = Field(
        default="ALPHA",
        description="Unique identifier for this War Rig instance",
    )

    # Agent configurations
    scribe: ScribeConfig = Field(
        default_factory=ScribeConfig,
        description="Scribe agent configuration",
    )
    challenger: ChallengerConfig = Field(
        default_factory=ChallengerConfig,
        description="Challenger agent configuration",
    )
    imperator: ImperatorConfig = Field(
        default_factory=ImperatorConfig,
        description="Imperator agent configuration",
    )

    # Workflow limits
    max_iterations: int = Field(
        default=3,
        ge=1,
        le=10,
        description="Maximum Scribe-Challenger iterations before forced approval",
    )
    max_questions_per_round: int = Field(
        default=5,
        ge=1,
        le=20,
        description="Maximum questions Challenger can ask per round",
    )
    max_chrome_tickets: int = Field(
        default=5,
        ge=1,
        le=20,
        description="Maximum Chrome tickets Imperator can issue per review",
    )

    # System configuration
    system: SystemConfig = Field(
        default_factory=SystemConfig,
        description="System-wide settings",
    )

    # API configuration
    anthropic_api_key: str | None = Field(
        default=None,
        description="Anthropic API key (can also use ANTHROPIC_API_KEY env var)",
    )

    @classmethod
    def from_yaml(cls, yaml_path: str | Path) -> "WarRigConfig":
        """Load configuration from a YAML file.

        Args:
            yaml_path: Path to the YAML configuration file.

        Returns:
            Configured WarRigConfig instance.

        Raises:
            FileNotFoundError: If the YAML file doesn't exist.
            yaml.YAMLError: If the YAML file is malformed.
        """
        yaml_path = Path(yaml_path)

        if not yaml_path.exists():
            raise FileNotFoundError(f"Configuration file not found: {yaml_path}")

        with yaml_path.open() as f:
            data = yaml.safe_load(f)

        # Handle nested 'war_rig' key in YAML
        if data and "war_rig" in data:
            config_data = data["war_rig"]
            # Merge system config if present at root level
            if "system" in data:
                config_data["system"] = data["system"]
        else:
            config_data = data or {}

        return cls(**config_data)

    @classmethod
    def from_yaml_or_default(
        cls,
        yaml_path: str | Path | None = None,
    ) -> "WarRigConfig":
        """Load configuration from YAML if it exists, otherwise use defaults.

        Searches for configuration in order:
        1. Provided yaml_path
        2. war_rig.yaml in current directory
        3. ~/.war_rig/config.yaml
        4. Default values

        Args:
            yaml_path: Optional explicit path to YAML configuration.

        Returns:
            Configured WarRigConfig instance.
        """
        search_paths: list[Path] = []

        if yaml_path:
            search_paths.append(Path(yaml_path))

        search_paths.extend([
            Path("war_rig.yaml"),
            Path("war_rig.yml"),
            Path.home() / ".war_rig" / "config.yaml",
        ])

        for path in search_paths:
            if path.exists():
                return cls.from_yaml(path)

        return cls()

    def to_yaml(self, yaml_path: str | Path) -> None:
        """Save current configuration to a YAML file.

        Args:
            yaml_path: Path where the YAML file should be written.
        """
        yaml_path = Path(yaml_path)
        yaml_path.parent.mkdir(parents=True, exist_ok=True)

        data = {
            "war_rig": self.model_dump(
                exclude={"anthropic_api_key"},
                exclude_none=True,
            ),
        }

        with yaml_path.open("w") as f:
            yaml.dump(data, f, default_flow_style=False, sort_keys=False)

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


def load_config(config_path: str | Path | None = None) -> WarRigConfig:
    """Convenience function to load configuration.

    This is the recommended entry point for loading configuration.

    Args:
        config_path: Optional path to a YAML configuration file.

    Returns:
        Configured WarRigConfig instance.
    """
    return WarRigConfig.from_yaml_or_default(config_path)
