"""CodeWhisper - Interactive chatbot for mainframe code exploration.

CodeWhisper is a LangGraph-powered chatbot CLI that helps developers
explore and understand mainframe codebases (COBOL, PL/I, JCL, etc.)
through natural language queries and a skills-based knowledge system.

Key Components:
    - Agent: LangGraph-based conversational agent with tool support
    - Skills: Loadable knowledge units for code documentation
    - Search: Code search and file reading capabilities
    - CLI: Interactive REPL for asking questions

Example:
    # Start the chatbot
    $ codewhisper --skills-dir ./skills --code-dir ./src

    # Or programmatically
    from codewhisper import create_agent, AgentConfig

    config = AgentConfig(
        skills_dir="./skills",
        code_dir="./src",
    )
    agent = create_agent(config)
    response = await agent.chat("What does CBPAUP0C do?")
"""

from codewhisper.agent.graph import CodeWhisperAgent, create_agent
from codewhisper.agent.state import AgentState, ConversationMessage
from codewhisper.config import AgentConfig
from codewhisper.skills.index import SkillsIndex
from codewhisper.skills.loader import SkillsLoader

__all__ = [
    "AgentConfig",
    "AgentState",
    "CodeWhisperAgent",
    "ConversationMessage",
    "SkillsIndex",
    "SkillsLoader",
    "create_agent",
]

__version__ = "0.1.0"
