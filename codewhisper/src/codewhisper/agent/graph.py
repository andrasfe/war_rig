"""LangGraph agent definition for CodeWhisper.

This module defines the StateGraph-based agent that powers CodeWhisper's
conversational code exploration. It uses a ReAct-style pattern where
the agent reasons about the query, decides which tools to use, and
synthesizes a response.

The agent follows this general flow:
1. Receive user query
2. Decide if skills/search are needed
3. Execute tools to gather information
4. Generate a helpful response

Example:
    from codewhisper import create_agent, AgentConfig

    config = AgentConfig(
        skills_dir="./skills",
        code_dir="./src",
    )
    agent = create_agent(config)
    response = await agent.chat("What does CBPAUP0C do?")
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING, Any, Literal

from langchain_core.messages import AIMessage, HumanMessage, SystemMessage
from langgraph.graph import END, START, StateGraph
from langgraph.prebuilt import ToolNode

from codewhisper.agent.state import AgentState
from codewhisper.agent.tools import configure_tools, get_all_tools

if TYPE_CHECKING:
    from langchain_core.language_models import BaseChatModel
    from langgraph.graph.state import CompiledStateGraph

    from codewhisper.config import AgentConfig
    from codewhisper.skills.index import SkillsIndex

logger = logging.getLogger(__name__)

# System prompt for the agent
SYSTEM_PROMPT = """You are CodeWhisper, an expert assistant for exploring and understanding mainframe codebases. You have access to a skills-based knowledge system containing documentation about programs, subsystems, and architectural concepts.

Your capabilities:
- Search for relevant skills/documentation by keyword
- Load specific skills to get detailed information about programs
- Search the source code for patterns and references
- Read source files to examine implementation details

When answering questions:
1. First, search for relevant skills if you need program-specific or system knowledge
2. Load skills that seem relevant to get full documentation
3. Search code or read files if you need to verify details or see implementation
4. Synthesize information from multiple sources into a clear, helpful response

Always cite your sources (which skills or files you consulted) and be honest about limitations or uncertainties. If you cannot find information about something, say so clearly.

The codebase contains mainframe programs (COBOL, JCL, PL/I, Assembler, REXX) for a financial authorization system. Key concepts include:
- IMS databases for hierarchical data storage
- CICS for online transaction processing
- IBM MQ for messaging
- Batch jobs for scheduled processing
"""


class CodeWhisperAgent:
    """LangGraph-based agent for code exploration.

    This agent uses a StateGraph to process user queries, gather
    information from skills and code, and generate helpful responses.

    Attributes:
        config: Agent configuration.
        graph: Compiled StateGraph.
        skills_index: Index of available skills.

    Example:
        agent = CodeWhisperAgent(config, skills_index)
        response = await agent.chat("What does CBPAUP0C do?")
    """

    def __init__(
        self,
        config: AgentConfig,
        skills_index: SkillsIndex,
        llm: BaseChatModel | None = None,
    ):
        """Initialize the agent.

        Args:
            config: Agent configuration.
            skills_index: Index of available skills.
            llm: Optional language model. If not provided, one will be
                created based on the config.
        """
        self.config = config
        self.skills_index = skills_index
        self._llm = llm
        self._graph: CompiledStateGraph | None = None
        self._conversation_state: AgentState | None = None

        # Configure tools with dependencies
        configure_tools(skills_index, config)

        # Initialize LLM if not provided
        if self._llm is None:
            self._llm = self._create_llm()

        logger.info(
            f"CodeWhisperAgent initialized: model={config.model}, "
            f"provider={config.provider}"
        )

    def _create_llm(self) -> BaseChatModel:
        """Create LLM from configuration using llm-providers.

        Returns:
            Configured chat model.
        """
        import os

        # Use langchain's chat model wrappers based on provider
        if self.config.provider == "anthropic":
            from langchain_anthropic import ChatAnthropic

            api_key = os.environ.get("ANTHROPIC_API_KEY", "")
            if not api_key:
                raise ValueError(
                    "ANTHROPIC_API_KEY environment variable not set. "
                    "Please set it to use the anthropic provider."
                )

            return ChatAnthropic(
                model=self.config.model,
                temperature=self.config.temperature,
                max_tokens=self.config.max_tokens,
                api_key=api_key,
            )
        elif self.config.provider == "openai":
            from langchain_openai import ChatOpenAI

            api_key = os.environ.get("OPENAI_API_KEY", "")
            if not api_key:
                raise ValueError(
                    "OPENAI_API_KEY environment variable not set. "
                    "Please set it to use the openai provider."
                )

            return ChatOpenAI(
                model=self.config.model,
                temperature=self.config.temperature,
                max_tokens=self.config.max_tokens,
                api_key=api_key,
            )
        else:  # openrouter (default)
            from langchain_openai import ChatOpenAI

            api_key = os.environ.get("OPENROUTER_API_KEY", "")
            if not api_key:
                raise ValueError(
                    "OPENROUTER_API_KEY environment variable not set. "
                    "Please set it to use the openrouter provider."
                )

            return ChatOpenAI(
                model=self.config.model,
                temperature=self.config.temperature,
                max_tokens=self.config.max_tokens,
                base_url="https://openrouter.ai/api/v1",
                api_key=api_key,
            )

    @property
    def llm(self) -> BaseChatModel:
        """Get the language model."""
        if self._llm is None:
            self._llm = self._create_llm()
        return self._llm

    @property
    def graph(self) -> CompiledStateGraph:
        """Get or build the compiled graph.

        Returns:
            Compiled StateGraph.
        """
        if self._graph is None:
            self._graph = self._build_graph()
        return self._graph

    def _build_graph(self) -> CompiledStateGraph:
        """Build the StateGraph for the agent.

        The graph has the following structure:
        - START -> agent (LLM decision node)
        - agent -> tools (if tool calls needed)
        - agent -> END (if response ready)
        - tools -> agent (loop back with results)

        Returns:
            Compiled StateGraph.
        """
        # Create the graph with our state schema
        graph_builder = StateGraph(AgentState)

        # Get tools and bind to LLM
        tools = get_all_tools()
        llm_with_tools = self.llm.bind_tools(tools)

        # Agent node: calls LLM to decide action
        async def agent_node(state: AgentState) -> dict[str, Any]:
            """Call the LLM to decide on tools or response."""
            # Build message list with system prompt
            messages = [SystemMessage(content=SYSTEM_PROMPT)]

            # Add conversation history
            for msg in state.messages:
                if hasattr(msg, "content"):
                    if hasattr(msg, "type"):
                        # LangChain message type
                        messages.append(msg)
                    elif hasattr(msg, "role"):
                        # Our message type
                        if msg.role == "user":
                            messages.append(HumanMessage(content=msg.content))
                        elif msg.role == "assistant":
                            messages.append(AIMessage(content=msg.content))
                        elif msg.role == "system":
                            messages.append(SystemMessage(content=msg.content))
                elif isinstance(msg, dict):
                    role = msg.get("role", "user")
                    content = msg.get("content", "")
                    if role == "user":
                        messages.append(HumanMessage(content=content))
                    elif role == "assistant":
                        messages.append(AIMessage(content=content))

            logger.debug(f"Calling LLM with {len(messages)} messages")
            response = await llm_with_tools.ainvoke(messages)

            return {"messages": [response]}

        # Add nodes
        graph_builder.add_node("agent", agent_node)
        graph_builder.add_node("tools", ToolNode(tools))

        # Add edges
        graph_builder.add_edge(START, "agent")
        graph_builder.add_conditional_edges(
            "agent",
            self._should_continue,
            {"tools": "tools", "end": END},
        )
        graph_builder.add_edge("tools", "agent")

        return graph_builder.compile()

    def _should_continue(
        self, state: AgentState
    ) -> Literal["tools", "end"]:
        """Decide whether to continue to tools or end.

        Args:
            state: Current agent state.

        Returns:
            "tools" if tool calls pending, "end" otherwise.
        """
        if not state.messages:
            return "end"

        last_message = state.messages[-1]

        # Check for tool calls on the last message
        if hasattr(last_message, "tool_calls") and last_message.tool_calls:
            return "tools"

        return "end"

    async def chat(self, user_message: str) -> str:
        """Send a message and get a response.

        Args:
            user_message: The user's query.

        Returns:
            The assistant's response.
        """
        logger.info(f"Chat: {user_message[:100]}...")

        # Build initial state, preserving conversation history
        if self._conversation_state is None:
            initial_messages = [HumanMessage(content=user_message)]
        else:
            # Append to existing conversation
            initial_messages = list(self._conversation_state.messages)
            initial_messages.append(HumanMessage(content=user_message))

            # Trim to max history
            if len(initial_messages) > self.config.max_history * 2:
                # Keep system prompt area + recent messages
                initial_messages = initial_messages[-(self.config.max_history * 2) :]

        initial_state = AgentState(
            messages=initial_messages,
            current_query=user_message,
        )

        # Run the graph
        result = await self.graph.ainvoke(initial_state)

        # Update conversation state
        self._conversation_state = AgentState(
            messages=result.get("messages", []),
            loaded_skills=result.get("loaded_skills", []),
            skill_contexts=result.get("skill_contexts", []),
        )

        # Extract the last assistant message
        for msg in reversed(result.get("messages", [])):
            if hasattr(msg, "content"):
                # LangChain AIMessage
                if hasattr(msg, "type") and msg.type == "ai":
                    return msg.content
                # Our message type
                if hasattr(msg, "role") and msg.role == "assistant":
                    return msg.content

        return "No response generated"

    async def reset(self) -> None:
        """Reset the conversation state."""
        logger.info("Resetting conversation")
        self._conversation_state = None


def create_agent(config: AgentConfig) -> CodeWhisperAgent:
    """Create a CodeWhisper agent from configuration.

    This is the main entry point for creating an agent instance.

    Args:
        config: Agent configuration.

    Returns:
        Initialized CodeWhisperAgent.

    Raises:
        ValueError: If skills directory doesn't exist or contains no skills.

    Example:
        config = AgentConfig(
            skills_dir="./skills",
            code_dir="./src",
        )
        agent = create_agent(config)
    """
    from codewhisper.skills.index import SkillsIndex
    from codewhisper.skills.loader import SkillsLoader

    # Validate directories
    if not config.skills_dir.exists():
        raise ValueError(f"Skills directory does not exist: {config.skills_dir}")

    if not config.code_dir.exists():
        raise ValueError(f"Code directory does not exist: {config.code_dir}")

    # Load skills
    logger.info(f"Loading skills from {config.skills_dir}")
    loader = SkillsLoader(config.skills_dir)
    skills = loader.load_all()

    if not skills:
        logger.warning(f"No skills found in {config.skills_dir}")

    skills_index = SkillsIndex(skills)
    logger.info(f"Loaded {len(skills)} skills into index")

    return CodeWhisperAgent(config, skills_index)
