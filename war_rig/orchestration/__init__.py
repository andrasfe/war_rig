"""LangGraph orchestration for War Rig.

This package provides the workflow orchestration using LangGraph:

- state: Graph state management (WarRigState)
- nodes: Graph node implementations
- graph: LangGraph workflow definition and builder

The workflow implements the War Rig documentation process:
1. Preprocess source code
2. Scribe produces initial documentation
3. Challenger validates and asks questions
4. Scribe responds to questions
5. Imperator reviews and decides
6. Loop or terminate based on decision

Example:
    from war_rig.orchestration import create_war_rig_graph
    from war_rig.config import WarRigConfig

    config = WarRigConfig()
    graph = create_war_rig_graph(config)
    result = await graph.ainvoke({
        "source_code": source,
        "file_name": "PROGRAM.cbl",
    })
"""

from war_rig.orchestration.graph import WarRigGraph, create_war_rig_graph
from war_rig.orchestration.state import WarRigState

__all__ = [
    "create_war_rig_graph",
    "WarRigGraph",
    "WarRigState",
]
