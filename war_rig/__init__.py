"""War Rig: Multi-agent mainframe documentation system.

This package provides a complete system for documenting legacy mainframe
codebases (COBOL, PL/I, JCL) using AI agents orchestrated by LangGraph.

The system uses three specialized agents:
- Scribe: Analyzes code and produces documentation
- Challenger: Validates documentation and asks probing questions
- Imperator: Reviews and approves the final documentation

Example:
    from war_rig.orchestration.graph import create_war_rig_graph
    from war_rig.config import WarRigConfig

    config = WarRigConfig()
    graph = create_war_rig_graph(config)
    result = await graph.ainvoke({"source_file": "PROGRAM.cbl"})
"""

__version__ = "0.1.0"
__author__ = "War Rig Team"

from war_rig.config import WarRigConfig

__all__ = ["WarRigConfig", "__version__"]
