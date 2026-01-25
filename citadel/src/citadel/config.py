"""
Configuration loading for Citadel.

Loads settings from environment variables using pydantic-settings.
Supports shared .env file in monorepo root.
"""

from pathlib import Path
from typing import Literal

from pydantic import Field
from pydantic_settings import BaseSettings, SettingsConfigDict


class CitadelConfig(BaseSettings):
    """
    Configuration loaded from environment variables.

    All settings can be overridden via environment variables.
    Citadel-specific settings are prefixed with CITADEL_.

    Example .env:
        CITADEL_LLM_MODEL=claude-sonnet-4-20250514
        CITADEL_CACHE_DIR=.cache/citadel
        CITADEL_PARALLEL_FILES=4
        CITADEL_LLM_DISAMBIGUATION=true
        CITADEL_MAX_LLM_CALLS=100
    """

    model_config = SettingsConfigDict(
        env_file=".env",
        env_file_encoding="utf-8",
        env_prefix="CITADEL_",
        extra="ignore",
    )

    # LLM settings
    llm_model: str = Field(
        default="claude-sonnet-4-20250514",
        description="LLM model to use for spec generation and disambiguation"
    )
    llm_disambiguation: bool = Field(
        default=True,
        description="Whether to use LLM for disambiguating unresolved references"
    )
    max_llm_calls: int = Field(
        default=100,
        ge=0,
        description="Maximum number of LLM calls per analysis run"
    )

    # Processing settings
    parallel_files: int = Field(
        default=4,
        ge=1,
        le=32,
        description="Number of files to process in parallel"
    )

    # Cache settings
    cache_dir: Path = Field(
        default=Path(".cache/citadel"),
        description="Directory for caching specs and parse results"
    )

    # Specs settings
    builtin_specs_dir: Path = Field(
        default_factory=lambda: Path(__file__).parent.parent.parent / "specs" / "builtin",
        description="Directory containing built-in specs"
    )

    # Output settings
    default_output_format: Literal["json", "dot", "cypher", "csv"] = Field(
        default="json",
        description="Default output format for dependency graphs"
    )

    # Logging
    log_level: Literal["DEBUG", "INFO", "WARNING", "ERROR"] = Field(
        default="INFO",
        description="Logging level"
    )


class LLMProviderConfig(BaseSettings):
    """
    Configuration for the LLM provider.

    These settings are shared with other tools in the monorepo.
    """

    model_config = SettingsConfigDict(
        env_file=".env",
        env_file_encoding="utf-8",
        extra="ignore",
    )

    llm_provider: str = Field(
        default="anthropic",
        description="LLM provider to use (anthropic, openai, etc.)"
    )
    anthropic_api_key: str | None = Field(
        default=None,
        description="Anthropic API key"
    )
    openai_api_key: str | None = Field(
        default=None,
        description="OpenAI API key (if using OpenAI)"
    )


def load_config() -> CitadelConfig:
    """
    Load Citadel configuration from environment.

    Searches for .env file in current directory and parent directories.

    Returns:
        CitadelConfig instance with loaded settings.
    """
    return CitadelConfig()


def load_llm_config() -> LLMProviderConfig:
    """
    Load LLM provider configuration from environment.

    Returns:
        LLMProviderConfig instance with loaded settings.
    """
    return LLMProviderConfig()


def get_cache_dir(config: CitadelConfig | None = None) -> Path:
    """
    Get the cache directory, creating it if necessary.

    Args:
        config: Optional config instance. If not provided, loads from environment.

    Returns:
        Path to the cache directory.
    """
    if config is None:
        config = load_config()

    cache_dir = config.cache_dir
    cache_dir.mkdir(parents=True, exist_ok=True)
    return cache_dir


def get_specs_cache_dir(config: CitadelConfig | None = None) -> Path:
    """
    Get the specs cache directory, creating it if necessary.

    Args:
        config: Optional config instance. If not provided, loads from environment.

    Returns:
        Path to the specs cache directory.
    """
    cache_dir = get_cache_dir(config)
    specs_dir = cache_dir / "specs"
    specs_dir.mkdir(parents=True, exist_ok=True)
    return specs_dir


def get_parse_cache_dir(config: CitadelConfig | None = None) -> Path:
    """
    Get the parse results cache directory, creating it if necessary.

    Args:
        config: Optional config instance. If not provided, loads from environment.

    Returns:
        Path to the parse results cache directory.
    """
    cache_dir = get_cache_dir(config)
    parse_dir = cache_dir / "parse_results"
    parse_dir.mkdir(parents=True, exist_ok=True)
    return parse_dir
