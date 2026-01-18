"""Base agent class for War Rig agents.

This module defines the abstract base class and common interfaces for all
War Rig agents (Scribe, Challenger, Imperator). It provides:

- Common configuration handling
- LLM integration abstraction (supporting OpenRouter and Anthropic)
- Async invocation interface
- Logging and error handling

Each agent implementation extends this base and provides specific
input/output types and prompt templates.
"""

import logging
from abc import ABC, abstractmethod
from typing import TYPE_CHECKING, Any, Generic, TypeVar

from langchain_core.language_models.chat_models import BaseChatModel
from langchain_core.messages import HumanMessage, SystemMessage
from pydantic import BaseModel, Field

from war_rig.config import APIConfig, ModelConfig, load_config

logger = logging.getLogger(__name__)


class AgentInput(BaseModel):
    """Base class for agent input data.

    All agent-specific inputs should extend this class to ensure
    consistent handling across the system.
    """

    iteration: int = Field(
        default=1,
        ge=1,
        description="Current iteration number",
    )


class AgentOutput(BaseModel):
    """Base class for agent output data.

    All agent-specific outputs should extend this class to ensure
    consistent handling across the system.
    """

    success: bool = Field(
        default=True,
        description="Whether the agent completed successfully",
    )
    error: str | None = Field(
        default=None,
        description="Error message if success is False",
    )
    tokens_used: int = Field(
        default=0,
        ge=0,
        description="Total tokens used in this invocation",
    )

    # Error recovery fields (for formatting error capture)
    raw_response: str | None = Field(
        default=None,
        description="Raw LLM response when validation failed (for recovery)",
    )
    validation_errors: list[dict[str, Any]] = Field(
        default_factory=list,
        description="Pydantic validation error details for recovery",
    )
    recoverable: bool = Field(
        default=False,
        description="Whether this failure can be recovered via FORMATTING_FIX",
    )


# Type variables for generic agent typing
InputT = TypeVar("InputT", bound=AgentInput)
OutputT = TypeVar("OutputT", bound=AgentOutput)


class BaseAgent(ABC, Generic[InputT, OutputT]):
    """Abstract base class for War Rig agents.

    This class provides the foundation for all agents in the War Rig system.
    It handles LLM configuration (including OpenRouter support), message
    construction, and the basic invoke/ainvoke interface.

    Subclasses must implement:
    - _build_system_prompt(): Return the system prompt for the agent
    - _build_user_prompt(): Convert input to a user message
    - _parse_response(): Parse LLM response into the output type

    Attributes:
        config: Model configuration for this agent
        api_config: API configuration (provider, keys, etc.)
        llm: The LangChain LLM instance
        name: Human-readable agent name

    Example:
        class MyAgent(BaseAgent[MyInput, MyOutput]):
            def _build_system_prompt(self) -> str:
                return "You are a helpful assistant."

            def _build_user_prompt(self, input_data: MyInput) -> str:
                return f"Process this: {input_data.content}"

            def _parse_response(self, response: str, input_data: MyInput) -> MyOutput:
                return MyOutput(result=response)
    """

    def __init__(
        self,
        config: ModelConfig,
        api_config: APIConfig | None = None,
        name: str = "Agent",
    ):
        """Initialize the agent.

        Args:
            config: Model configuration specifying model, temperature, etc.
            api_config: API configuration. If None, loads from environment.
            name: Human-readable name for logging.
        """
        self.config = config
        self.name = name

        # Load API config from environment if not provided
        if api_config is None:
            full_config = load_config()
            self._api_config = full_config.api
        else:
            self._api_config = api_config

        # Initialize LLM lazily to allow for mock injection in tests
        self._llm: BaseChatModel | None = None

    @property
    def api_config(self) -> APIConfig:
        """Get the API configuration."""
        return self._api_config

    @property
    def llm(self) -> BaseChatModel:
        """Get the LLM instance, initializing if needed.

        Returns:
            The configured LLM instance.
        """
        if self._llm is None:
            self._llm = self._create_llm()
        return self._llm

    @llm.setter
    def llm(self, value: BaseChatModel) -> None:
        """Set the LLM instance (useful for testing).

        Args:
            value: The LLM instance to use.
        """
        self._llm = value

    def _create_llm(self) -> BaseChatModel:
        """Create the LLM instance based on configuration.

        Supports both OpenRouter (via OpenAI-compatible API) and Anthropic.

        Returns:
            Configured LLM instance.
        """
        provider = self._api_config.provider.lower()

        if provider == "openrouter":
            return self._create_openrouter_llm()
        elif provider == "anthropic":
            return self._create_anthropic_llm()
        else:
            raise ValueError(f"Unknown API provider: {provider}")

    def _create_openrouter_llm(self) -> BaseChatModel:
        """Create an OpenRouter LLM via OpenAI-compatible API.

        Returns:
            Configured ChatOpenAI instance for OpenRouter.
        """
        from langchain_openai import ChatOpenAI

        # Build default headers for OpenRouter
        default_headers: dict[str, str] = {}
        if self._api_config.site_url:
            default_headers["HTTP-Referer"] = self._api_config.site_url
        if self._api_config.site_name:
            default_headers["X-Title"] = self._api_config.site_name

        kwargs: dict[str, Any] = {
            "model": self.config.model,
            "temperature": self.config.temperature,
            "max_tokens": self.config.max_tokens,
            "base_url": self._api_config.base_url,
        }

        if self._api_config.api_key:
            kwargs["api_key"] = self._api_config.api_key

        if default_headers:
            kwargs["default_headers"] = default_headers

        logger.debug(
            f"{self.name}: Creating OpenRouter LLM with model={self.config.model}, "
            f"temp={self.config.temperature}"
        )

        return ChatOpenAI(**kwargs)

    def _create_anthropic_llm(self) -> BaseChatModel:
        """Create an Anthropic LLM.

        Returns:
            Configured ChatAnthropic instance.
        """
        from langchain_anthropic import ChatAnthropic

        kwargs: dict[str, Any] = {
            "model": self.config.model,
            "temperature": self.config.temperature,
            "max_tokens": self.config.max_tokens,
        }

        if self._api_config.api_key:
            kwargs["api_key"] = self._api_config.api_key

        logger.debug(
            f"{self.name}: Creating Anthropic LLM with model={self.config.model}, "
            f"temp={self.config.temperature}"
        )

        return ChatAnthropic(**kwargs)

    @abstractmethod
    def _build_system_prompt(self) -> str:
        """Build the system prompt for this agent.

        Returns:
            The system prompt string.
        """
        pass

    @abstractmethod
    def _build_user_prompt(self, input_data: InputT) -> str:
        """Build the user prompt from input data.

        Args:
            input_data: The agent's input.

        Returns:
            The user message string.
        """
        pass

    @abstractmethod
    def _parse_response(self, response: str, input_data: InputT) -> OutputT:
        """Parse the LLM response into the output type.

        Args:
            response: Raw text response from the LLM.
            input_data: Original input (may be needed for context).

        Returns:
            Parsed output of the appropriate type.
        """
        pass

    def _build_messages(self, input_data: InputT) -> list[SystemMessage | HumanMessage]:
        """Build the message list for the LLM.

        Args:
            input_data: The agent's input.

        Returns:
            List of messages to send to the LLM.
        """
        return [
            SystemMessage(content=self._build_system_prompt()),
            HumanMessage(content=self._build_user_prompt(input_data)),
        ]

    async def ainvoke(self, input_data: InputT) -> OutputT:
        """Asynchronously invoke the agent.

        This is the primary method for running an agent. It:
        1. Builds the message list
        2. Calls the LLM
        3. Parses the response

        Args:
            input_data: The agent's input.

        Returns:
            The agent's output.

        Raises:
            AgentError: If the agent fails to produce valid output.
        """
        logger.info(f"{self.name}: Starting invocation (iteration {input_data.iteration})")

        try:
            messages = self._build_messages(input_data)

            logger.debug(f"{self.name}: Calling LLM with {len(messages)} messages")
            response = await self.llm.ainvoke(messages)

            # Extract content from response
            content = response.content
            if isinstance(content, list):
                # Handle multi-part responses
                content = "".join(
                    part.get("text", "") if isinstance(part, dict) else str(part)
                    for part in content
                )

            logger.debug(f"{self.name}: Received response ({len(content)} chars)")

            # Parse and return
            output = self._parse_response(content, input_data)

            # Track token usage if available
            if hasattr(response, "usage_metadata") and response.usage_metadata:
                output.tokens_used = (
                    response.usage_metadata.get("input_tokens", 0) +
                    response.usage_metadata.get("output_tokens", 0)
                )

            logger.info(f"{self.name}: Completed successfully")
            return output

        except Exception as e:
            logger.error(f"{self.name}: Error during invocation: {e}")
            return self._create_error_output(str(e), input_data)

    def invoke(self, input_data: InputT) -> OutputT:
        """Synchronously invoke the agent.

        This is a convenience wrapper around ainvoke for non-async contexts.

        Args:
            input_data: The agent's input.

        Returns:
            The agent's output.
        """
        import asyncio

        # Check if we're already in an async context
        try:
            loop = asyncio.get_running_loop()
            # We're in an async context, need to use run_coroutine_threadsafe
            import concurrent.futures
            future = asyncio.run_coroutine_threadsafe(
                self.ainvoke(input_data),
                loop,
            )
            return future.result()
        except RuntimeError:
            # No running loop, we can use asyncio.run
            return asyncio.run(self.ainvoke(input_data))

    @abstractmethod
    def _create_error_output(self, error: str, input_data: InputT) -> OutputT:
        """Create an error output when the agent fails.

        Args:
            error: Error message.
            input_data: Original input.

        Returns:
            Output indicating failure.
        """
        pass


class AgentError(Exception):
    """Exception raised when an agent fails.

    Attributes:
        agent_name: Name of the agent that failed.
        message: Description of the error.
        iteration: Iteration number when failure occurred.
    """

    def __init__(
        self,
        agent_name: str,
        message: str,
        iteration: int = 0,
    ):
        """Initialize an AgentError.

        Args:
            agent_name: Name of the agent that failed.
            message: Description of the error.
            iteration: Iteration number when failure occurred.
        """
        self.agent_name = agent_name
        self.message = message
        self.iteration = iteration

        full_message = f"{agent_name}: {message}"
        if iteration:
            full_message = f"{full_message} (iteration {iteration})"

        super().__init__(full_message)
