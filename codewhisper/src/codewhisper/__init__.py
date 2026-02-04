"""CodeWhisper - Interactive chatbot for mainframe code exploration.

CodeWhisper is a ReAct-based chatbot CLI that helps developers
explore and understand mainframe codebases (COBOL, PL/I, JCL, etc.)
through natural language queries and a skills-based knowledge system.

Key Components:
    - Agent: ReAct-based conversational agent with tool support
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

from codewhisper.agent.message import ConversationMessage
from codewhisper.agent.protocol import AgentState
from codewhisper.agent.react_loop import ReActAgent, create_agent
from codewhisper.config import AgentConfig
from codewhisper.skills.index import SkillsIndex
from codewhisper.skills.loader import SkillsLoader

__all__ = [
    "AgentConfig",
    "AgentState",
    "ConversationMessage",
    "ReActAgent",
    "SkillsIndex",
    "SkillsLoader",
    "create_agent",
]

__version__ = "0.1.0"
