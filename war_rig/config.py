"""Configuration management for War Rig.

This module provides configuration with support for .env files via python-dotenv,
with per-agent model configuration.

**Provider-Agnostic Design:**
    War Rig supports any LLM provider registered via its plugin system.
    The provider is determined by the LLM_PROVIDER environment variable.
    Each provider handles its own API key and configuration.

Typical usage:
    # Load from .env file automatically
    config = load_config()

    # Access agent configuration
    print(config.scribe.model)
    print(config.max_iterations)
"""

from pathlib import Path

from dotenv import load_dotenv
from pydantic import BaseModel, Field
from pydantic_settings import BaseSettings, SettingsConfigDict

# Load .env file from current directory or parent directories
load_dotenv()


class APIConfig(BaseModel):
    """Configuration for the API provider.

    **Provider-Agnostic:** This configuration works with any LLM provider.
    The provider field determines which backend is used, and each provider
    handles its own API key lookup from environment variables.
    """

    provider: str = Field(
        default="openrouter",
        description="API provider to use (any registered provider name)",
    )
    api_key: str | None = Field(
        default=None,
        description="API key for the provider (optional, provider reads from env)",
    )
    base_url: str | None = Field(
        default=None,
        description="Base URL for the API (provider-specific)",
    )
    site_url: str | None = Field(
        default=None,
        description="Site URL for provider analytics (optional)",
    )
    site_name: str | None = Field(
        default=None,
        description="Site name for provider analytics (optional)",
    )


class ModelConfig(BaseModel):
    """Configuration for an LLM model."""

    model: str = Field(
        ...,
        description="Model identifier for the LLM (required)",
    )
    temperature: float = Field(
        default=1.0,
        ge=0.0,
        le=2.0,
        description="Sampling temperature for generation (1.0 required for o3/o1 models)",
    )


class ScribeConfig(ModelConfig):
    """Configuration specific to the Scribe agent.

    The Scribe analyzes source code and fills out documentation templates.
    Temperature 1.0 is required for o3/o1 reasoning models.
    """

    temperature: float = Field(default=1.0)
    max_prompt_tokens: int = Field(
        default=15000,
        description="Maximum approximate tokens for the prompt (will truncate content to fit)",
    )
    max_completion_tokens: int = Field(
        default=8192,
        description="Maximum tokens for response (limits output generation time)",
    )
    citadel_max_paragraphs_per_batch: int = Field(
        default=10,
        ge=1,
        le=100,
        description="Maximum paragraphs per batch in Citadel-guided processing (LLM output limit)",
    )


class ChallengerConfig(ModelConfig):
    """Configuration specific to the Challenger agent.

    The Challenger validates documentation and asks probing questions.
    Temperature 1.0 is required for o3/o1 reasoning models.
    """

    temperature: float = Field(default=1.0)
    max_prompt_tokens: int = Field(
        default=15000,
        description="Maximum approximate tokens for the prompt",
    )
    max_completion_tokens: int = Field(
        default=4096,
        description="Maximum tokens for response",
    )


class ImperatorConfig(ModelConfig):
    """Configuration specific to the Imperator agent.

    The Imperator reviews documentation and makes approval decisions.
    Temperature 1.0 is required for o3/o1 reasoning models.
    """

    temperature: float = Field(default=1.0)
    max_prompt_tokens: int = Field(
        default=15000,
        description="Maximum approximate tokens for the prompt",
    )
    max_completion_tokens: int = Field(
        default=8192,
        description="Maximum tokens for response (holistic review can be large)",
    )
    fallback_model: str | None = Field(
        default=None,
        description="Fallback model to use when the primary model fails (e.g. stream stall)",
    )


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

    cobol: list[str] = Field(default=[".cbl", ".cob", ".CBL", ".COB", ".cobol", ".COBOL"])
    copybook: list[str] = Field(default=[".cpy", ".CPY", ".copy", ".COPY", ".copybook"])
    jcl: list[str] = Field(default=[".jcl", ".JCL", ".proc", ".PROC", ".prc", ".PRC"])
    bms: list[str] = Field(default=[".bms", ".BMS"])
    pli: list[str] = Field(default=[".pli", ".PLI", ".pl1", ".PL1"])
    listing: list[str] = Field(default=[".lst", ".LST", ".list", ".LIST"])
    asm: list[str] = Field(default=[".asm", ".ASM", ".hlasm", ".HLASM", ".s", ".S"])
    rexx: list[str] = Field(default=[".rexx", ".REXX", ".rex", ".REX", ".exec", ".EXEC"])
    clist: list[str] = Field(default=[".clist", ".CLIST"])
    natural: list[str] = Field(default=[".nsp", ".NSP", ".nsn", ".NSN"])
    easytrieve: list[str] = Field(default=[".ezt", ".EZT", ".ezy", ".EZY"])
    sort: list[str] = Field(default=[".srt", ".SRT", ".sort", ".SORT"])
    ddl: list[str] = Field(default=[".ddl", ".DDL", ".sql", ".SQL"])
    ims: list[str] = Field(default=[".dbd", ".DBD", ".psb", ".PSB"])


class QuestionResolutionConfig(BaseModel):
    """Configuration for automatic open question resolution via CodeWhisper."""

    enabled: bool = Field(default=False)
    max_questions_per_cycle: int = Field(default=10)
    timeout_per_question: int = Field(default=120)
    min_confidence: str = Field(default="MEDIUM")
    codewhisper_max_iterations: int = Field(default=8)
    codewhisper_temperature: float = Field(default=0.2)
    codewhisper_max_tokens: int = Field(default=2048)
    resolve_readme_questions: bool = Field(default=True)


class KnowledgeGraphConfig(BaseModel):
    """Configuration for the knowledge graph subsystem.

    Controls graph construction, context injection, and convergence tracking.
    The knowledge graph captures structural relationships between mainframe
    artifacts and feeds them back as context into subsequent documentation passes.
    """

    enabled: bool = Field(
        default=False,
        description="Enable knowledge graph construction during documentation passes",
    )
    db_path: str = Field(
        default="./output/knowledge_graph.db",
        description="Path to the SQLite database file for the graph store",
    )
    max_context_tokens: int = Field(
        default=500,
        ge=100,
        le=2000,
        description="Maximum tokens for graph context injection into prompts",
    )
    neighborhood_hops: int = Field(
        default=2,
        ge=1,
        le=5,
        description="Number of hops for neighborhood queries",
    )
    convergence_threshold: float = Field(
        default=0.05,
        ge=0.0,
        le=1.0,
        description="Triple delta threshold for convergence (0.05 = 5%)",
    )
    extract_from_preprocessors: bool = Field(
        default=True,
        description="Extract ground-truth triples from preprocessor output (JCL, COBOL)",
    )
    emit_triples_from_scribe: bool = Field(
        default=True,
        description="Instruct Scribe to emit triples in output",
    )
    challenger_cross_check: bool = Field(
        default=True,
        description="Enable Challenger structural cross-check against graph",
    )


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

    **Provider-Agnostic:** Configuration supports any LLM provider registered
    in war_rig.providers. The provider is determined by LLM_PROVIDER env var.

    Configuration is loaded from environment variables (with .env support).
    See .env.example for all available options.
    """

    model_config = SettingsConfigDict(
        env_prefix="",
        extra="ignore",
    )

    # War Rig identification
    rig_id: str = Field(default="ALPHA")

    # API configuration
    # LLM_PROVIDER determines which provider to use (any registered name)
    llm_provider: str = Field(default="openrouter", description="LLM provider name (any registered provider)")
    api_provider: str = Field(default="openrouter", description="Deprecated: use llm_provider")

    # Provider-specific configuration (for backward compatibility)
    # New providers should read their own env vars directly
    openrouter_api_key: str | None = Field(default=None, description="OpenRouter API key (if using openrouter)")
    openrouter_base_url: str = Field(default="https://openrouter.ai/api/v1")
    openrouter_site_url: str | None = Field(default=None)
    openrouter_site_name: str | None = Field(default=None)

    # Anthropic configuration (if using anthropic provider directly)
    anthropic_api_key: str | None = Field(default=None, description="Anthropic API key (if using anthropic)")

    # Agent model configurations (defaults provided, can override in .env)
    scribe_model: str = Field(
        default="anthropic/claude-sonnet-4-20250514",
        description="Model for Scribe agent",
    )
    scribe_temperature: float = Field(default=1.0)
    scribe_max_prompt_tokens: int = Field(
        default=15000,
        description="Maximum tokens for Scribe prompt (will truncate to fit)",
    )
    scribe_max_completion_tokens: int = Field(
        default=8192,
        description="Maximum tokens for Scribe response (limits output generation time)",
    )
    citadel_max_paragraphs_per_batch: int = Field(
        default=10,
        ge=1,
        le=100,
        description="Maximum paragraphs per batch in Citadel-guided processing",
    )

    challenger_model: str = Field(
        default="anthropic/claude-sonnet-4-20250514",
        description="Model for Challenger agent",
    )
    challenger_temperature: float = Field(default=1.0)
    challenger_max_prompt_tokens: int = Field(
        default=15000,
        description="Maximum tokens for Challenger prompt",
    )
    challenger_max_completion_tokens: int = Field(
        default=4096,
        description="Maximum tokens for Challenger response",
    )

    imperator_model: str = Field(
        default="anthropic/claude-sonnet-4-20250514",
        description="Model for Imperator agent",
    )
    imperator_temperature: float = Field(default=1.0)
    imperator_max_prompt_tokens: int = Field(
        default=15000,
        description="Maximum tokens for Imperator prompt",
    )
    imperator_max_completion_tokens: int = Field(
        default=8192,
        description="Maximum tokens for Imperator response (holistic review can be large)",
    )
    imperator_fallback_model: str | None = Field(
        default=None,
        description="Fallback model when Imperator primary model fails (stream stall, etc.)",
    )

    # Workflow limits
    num_teams: int = Field(default=1, ge=1, le=10)
    max_iterations: int = Field(default=3, ge=1, le=10)
    max_questions_per_round: int = Field(default=5, ge=1, le=20)
    max_chrome_tickets: int = Field(default=5, ge=1, le=20)

    # Challenger sampling (reduces context by validating only a sample of paragraphs)
    challenger_paragraph_sample_size: int = Field(
        default=5,
        ge=1,
        le=20,
        description="Number of paragraphs to sample for Challenger validation (0 = all)",
    )

    # Parallel worker pools
    num_scribes: int = Field(default=3, ge=1, le=10, description="Number of parallel Scribe workers")
    num_challengers: int = Field(default=2, ge=1, le=10, description="Number of parallel Challenger workers")
    pm_max_cycles: int = Field(default=5, ge=1, le=20, description="Max Program Manager cycles before forced completion")

    # Super-Scribe configuration (stronger model for blocked tickets)
    super_scribe_model: str = Field(
        default="anthropic/claude-opus-4-20250514",
        description="Model for Super-Scribe (stronger model to handle blocked tickets)",
    )
    super_scribe_max_prompt_tokens: int = Field(
        default=30000,
        description="Maximum tokens for Super-Scribe prompt (higher for Opus)",
    )
    num_super_scribes: int = Field(
        default=1,
        ge=1,
        le=5,
        description="Number of parallel Super-Scribe workers (typically 1 due to cost)",
    )

    # Call semantics enrichment (makes additional LLM calls per file)
    enable_call_semantics: bool = Field(
        default=True,
        description="Enable call semantics analysis (adds LLM calls to infer data flow between paragraphs)",
    )

    # Minion Scribe pool configuration (for parallel call semantics processing)
    minion_scribe_model: str = Field(
        default="anthropic/claude-3-haiku-20240307",
        description="Model for Minion Scribe workers (fast, cheap model for call semantics)",
    )
    num_minion_scribes: int = Field(
        default=4,
        ge=1,
        le=10,
        description="Number of parallel Minion Scribe workers for call semantics analysis",
    )
    minion_scribe_batch_size: int = Field(
        default=5,
        ge=1,
        le=20,
        description="Number of call edges per Minion Scribe LLM request",
    )

    # Cycle limits for agents
    max_challenger_cycles: int = Field(
        default=2,
        ge=1,
        le=10,
        description="Maximum Challenger review cycles before forcing completion",
    )
    max_imperator_cycles: int = Field(
        default=1,
        ge=1,
        le=5,
        description="Maximum Imperator review cycles before forcing approval",
    )

    # Compact review mode (Tier 1 - reduced token usage)
    use_compact_holistic_review: bool = Field(
        default=True,
        description="Use compact holistic review (Tier 1) for reduced token usage. "
        "Falls back to full review for README.md generation.",
    )

    # Formatting error recovery configuration
    enable_formatting_recovery: bool = Field(
        default=True,
        description="Enable FORMATTING_FIX ticket creation for recoverable errors",
    )
    max_formatting_fix_attempts: int = Field(
        default=2,
        ge=1,
        le=5,
        description="Maximum attempts to fix formatting before giving up",
    )
    formatting_fix_timeout: int = Field(
        default=120,
        ge=30,
        le=600,
        description="Timeout in seconds for formatting fix attempts",
    )

    # Error handling
    exit_on_error: bool = Field(
        default=True,
        description="Exit immediately when any error occurs during processing",
    )
    max_ticket_retries: int = Field(
        default=5,
        ge=1,
        le=20,
        description="Maximum retry attempts per ticket (including Super-Scribe) before fatal exit",
    )

    # Citadel-guided documentation thresholds
    citadel_guided_threshold_lines: int = Field(
        default=2000,
        ge=100,
        description="Files with more lines than this use batched per-paragraph processing",
    )
    citadel_guided_threshold_paragraphs: int = Field(
        default=15,
        ge=1,
        description="Files with more paragraphs than this use batched processing",
    )

    # Knowledge graph
    knowledge_graph_enabled: bool = Field(
        default=False,
        description="Enable knowledge graph construction during documentation passes",
    )
    knowledge_graph_db_path: str = Field(
        default="./output/knowledge_graph.db",
        description="Path to the SQLite database for the graph store",
    )
    knowledge_graph_max_context_tokens: int = Field(
        default=500,
        ge=100,
        le=2000,
        description="Maximum tokens for graph context injection into prompts",
    )
    knowledge_graph_neighborhood_hops: int = Field(
        default=2,
        ge=1,
        le=5,
        description="Number of hops for neighborhood queries",
    )
    knowledge_graph_convergence_threshold: float = Field(
        default=0.05,
        ge=0.0,
        le=1.0,
        description="Triple delta threshold for convergence (0.05 = 5%)",
    )
    knowledge_graph_extract_from_preprocessors: bool = Field(
        default=True,
        description="Extract ground-truth triples from preprocessor output",
    )
    knowledge_graph_emit_triples_from_scribe: bool = Field(
        default=True,
        description="Instruct Scribe to emit triples in output",
    )
    knowledge_graph_challenger_cross_check: bool = Field(
        default=True,
        description="Enable Challenger structural cross-check against graph",
    )

    # Beads integration
    # Disabled by default - War Rig uses in-memory ticket tracking
    # Enable only if you have a separate beads instance configured
    beads_enabled: bool = Field(default=False)
    beads_dry_run: bool = Field(default=False)

    # Question resolution (automatic via CodeWhisper)
    question_resolution_enabled: bool = Field(default=False)
    question_resolution_max_per_cycle: int = Field(default=10)
    question_resolution_timeout: int = Field(default=120)
    question_resolution_min_confidence: str = Field(default="MEDIUM")
    question_resolution_cw_max_iterations: int = Field(default=8)
    question_resolution_cw_temperature: float = Field(default=0.2)
    question_resolution_cw_max_tokens: int = Field(default=2048)
    question_resolution_resolve_readme: bool = Field(default=True)

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
        """Get API configuration.

        Uses llm_provider (or api_provider for backward compatibility).
        API keys are looked up based on the provider.
        """
        # Use llm_provider, fall back to api_provider for backward compat
        provider = self.llm_provider or self.api_provider

        # Look up API key based on provider
        api_key = None
        base_url = None
        if provider == "openrouter":
            api_key = self.openrouter_api_key
            base_url = self.openrouter_base_url
        elif provider == "anthropic":
            api_key = self.anthropic_api_key

        return APIConfig(
            provider=provider,
            api_key=api_key,
            base_url=base_url,
            site_url=self.openrouter_site_url,
            site_name=self.openrouter_site_name,
        )

    @property
    def scribe(self) -> ScribeConfig:
        """Get Scribe agent configuration."""
        return ScribeConfig(
            model=self.scribe_model,
            temperature=self.scribe_temperature,
            max_prompt_tokens=self.scribe_max_prompt_tokens,
            citadel_max_paragraphs_per_batch=self.citadel_max_paragraphs_per_batch,
        )

    @property
    def challenger(self) -> ChallengerConfig:
        """Get Challenger agent configuration."""
        return ChallengerConfig(
            model=self.challenger_model,
            temperature=self.challenger_temperature,
            max_prompt_tokens=self.challenger_max_prompt_tokens,
        )

    @property
    def imperator(self) -> ImperatorConfig:
        """Get Imperator agent configuration."""
        return ImperatorConfig(
            model=self.imperator_model,
            temperature=self.imperator_temperature,
            max_prompt_tokens=self.imperator_max_prompt_tokens,
            fallback_model=self.imperator_fallback_model,
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

    @property
    def question_resolution(self) -> QuestionResolutionConfig:
        """Get question resolution configuration."""
        return QuestionResolutionConfig(
            enabled=self.question_resolution_enabled,
            max_questions_per_cycle=self.question_resolution_max_per_cycle,
            timeout_per_question=self.question_resolution_timeout,
            min_confidence=self.question_resolution_min_confidence,
            codewhisper_max_iterations=self.question_resolution_cw_max_iterations,
            codewhisper_temperature=self.question_resolution_cw_temperature,
            codewhisper_max_tokens=self.question_resolution_cw_max_tokens,
            resolve_readme_questions=self.question_resolution_resolve_readme,
        )

    @property
    def knowledge_graph(self) -> KnowledgeGraphConfig:
        """Get knowledge graph configuration."""
        return KnowledgeGraphConfig(
            enabled=self.knowledge_graph_enabled,
            db_path=self.knowledge_graph_db_path,
            max_context_tokens=self.knowledge_graph_max_context_tokens,
            neighborhood_hops=self.knowledge_graph_neighborhood_hops,
            convergence_threshold=self.knowledge_graph_convergence_threshold,
            extract_from_preprocessors=self.knowledge_graph_extract_from_preprocessors,
            emit_triples_from_scribe=self.knowledge_graph_emit_triples_from_scribe,
            challenger_cross_check=self.knowledge_graph_challenger_cross_check,
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
