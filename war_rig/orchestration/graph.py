"""LangGraph workflow definition for War Rig.

This module provides the main graph builder and workflow definition
for the War Rig documentation system.

The workflow implements the following flow:
1. preprocess -> Extract structural information
2. scribe -> Generate initial documentation
3. challenger -> Validate and ask questions
4. scribe_respond -> Answer questions (combined with scribe)
5. imperator -> Review and decide
6. If CHROME -> increment iteration and loop to scribe
7. If WITNESSED/VALHALLA/FORCED -> end

Example:
    from war_rig.orchestration import create_war_rig_graph
    from war_rig.config import WarRigConfig

    config = WarRigConfig()
    graph = create_war_rig_graph(config)

    result = await graph.ainvoke({
        "source_code": source,
        "file_name": "PROGRAM.cbl",
    })
    print(result["decision"])
"""

import logging
from typing import Any

from langgraph.graph import END, StateGraph

from war_rig.config import APIConfig, WarRigConfig
from war_rig.orchestration.nodes import WarRigNodes, should_continue
from war_rig.orchestration.state import WarRigState, create_initial_state

logger = logging.getLogger(__name__)


class WarRigGraph:
    """Wrapper for the War Rig LangGraph workflow.

    This class provides a convenient interface for creating and running
    the War Rig documentation workflow.

    Attributes:
        config: War Rig configuration
        nodes: Node implementations
        graph: Compiled LangGraph workflow

    Example:
        rig = WarRigGraph(config)
        result = await rig.run(source_code, "PROGRAM.cbl")
    """

    def __init__(
        self,
        config: WarRigConfig | None = None,
        api_config: APIConfig | None = None,
    ):
        """Initialize the War Rig graph.

        Args:
            config: War Rig configuration (uses defaults if None).
            api_config: API configuration. If None, loads from environment.
        """
        self.config = config or WarRigConfig()
        self.nodes = WarRigNodes(self.config, api_config)
        self.graph = self._build_graph()

    def _build_graph(self) -> StateGraph:
        """Build the LangGraph workflow.

        Returns:
            Compiled StateGraph ready for execution.
        """
        # Create the graph with our state type
        workflow = StateGraph(WarRigState)

        # Add nodes
        workflow.add_node("preprocess", self.nodes.preprocess)
        workflow.add_node("scribe", self.nodes.scribe_document)
        workflow.add_node("challenger", self.nodes.challenger_validate)
        workflow.add_node("imperator", self.nodes.imperator_review)
        workflow.add_node("increment", self.nodes.increment_iteration)

        # Set entry point
        workflow.set_entry_point("preprocess")

        # Add edges
        # preprocess -> scribe (always)
        workflow.add_edge("preprocess", "scribe")

        # scribe -> challenger (always)
        workflow.add_edge("scribe", "challenger")

        # challenger -> imperator (always)
        workflow.add_edge("challenger", "imperator")

        # imperator -> conditional
        workflow.add_conditional_edges(
            "imperator",
            should_continue,
            {
                "increment": "increment",
                "end": END,
            },
        )

        # increment -> scribe (loop back)
        workflow.add_edge("increment", "scribe")

        # Compile the graph
        return workflow.compile()

    async def ainvoke(
        self,
        source_code: str,
        file_name: str,
        copybook_contents: dict[str, str] | None = None,
        use_mock: bool = False,
    ) -> WarRigState:
        """Asynchronously run the War Rig workflow.

        Args:
            source_code: The source code to analyze.
            file_name: Name of the source file.
            copybook_contents: Resolved copybook contents.
            use_mock: Whether to use mock agents (for testing).

        Returns:
            Final state after workflow completion.
        """
        logger.info(f"Starting War Rig analysis: {file_name}")

        # Create initial state
        initial_state = create_initial_state(
            source_code=source_code,
            file_name=file_name,
            copybook_contents=copybook_contents,
            rig_id=self.config.rig_id,
            max_iterations=self.config.max_iterations,
            use_mock=use_mock,
        )

        # Run the graph
        result = await self.graph.ainvoke(initial_state)

        logger.info(
            f"War Rig complete: {file_name} -> {result.get('decision', 'UNKNOWN')}"
        )

        return result

    def invoke(
        self,
        source_code: str,
        file_name: str,
        copybook_contents: dict[str, str] | None = None,
        use_mock: bool = False,
    ) -> WarRigState:
        """Synchronously run the War Rig workflow.

        This is a convenience wrapper for non-async contexts.

        Args:
            source_code: The source code to analyze.
            file_name: Name of the source file.
            copybook_contents: Resolved copybook contents.
            use_mock: Whether to use mock agents (for testing).

        Returns:
            Final state after workflow completion.
        """
        import asyncio

        return asyncio.run(
            self.ainvoke(source_code, file_name, copybook_contents, use_mock)
        )

    async def run(
        self,
        source_code: str,
        file_name: str,
        copybook_contents: dict[str, str] | None = None,
        use_mock: bool = False,
    ) -> WarRigState:
        """Alias for ainvoke for more intuitive API.

        Args:
            source_code: The source code to analyze.
            file_name: Name of the source file.
            copybook_contents: Resolved copybook contents.
            use_mock: Whether to use mock agents (for testing).

        Returns:
            Final state after workflow completion.
        """
        return await self.ainvoke(source_code, file_name, copybook_contents, use_mock)


def create_war_rig_graph(
    config: WarRigConfig | None = None,
    api_config: APIConfig | None = None,
) -> WarRigGraph:
    """Create a War Rig graph instance.

    This is the main entry point for creating the War Rig workflow.

    Args:
        config: War Rig configuration (uses defaults if None).
        api_config: API configuration. If None, loads from environment.

    Returns:
        Configured WarRigGraph instance.

    Example:
        graph = create_war_rig_graph()
        result = await graph.run(source_code, "PROGRAM.cbl")
    """
    return WarRigGraph(config, api_config)


async def analyze_file(
    source_code: str,
    file_name: str,
    config: WarRigConfig | None = None,
    copybook_contents: dict[str, str] | None = None,
    use_mock: bool = False,
) -> WarRigState:
    """Convenience function to analyze a single file.

    This function creates a graph, runs it, and returns the result.
    Use WarRigGraph directly for more control or batch processing.

    Args:
        source_code: The source code to analyze.
        file_name: Name of the source file.
        config: War Rig configuration.
        copybook_contents: Resolved copybook contents.
        use_mock: Whether to use mock agents.

    Returns:
        Final state after workflow completion.

    Example:
        result = await analyze_file(source, "PROGRAM.cbl")
        print(result["final_template"])
    """
    graph = create_war_rig_graph(config)
    return await graph.ainvoke(source_code, file_name, copybook_contents, use_mock)
