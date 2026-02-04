"""Tool registry for CodeWhisper agent.

This module provides a central registry for all tools available to the agent.
Tools are registered with their definitions and handlers, and the registry
provides methods for executing tools and generating schemas for the LLM.

Example:
    from codewhisper.agent.tools.registry import ToolRegistry
    from codewhisper.agent.protocol import ToolDefinition, ToolCall

    registry = ToolRegistry()

    # Register a tool
    registry.register(ToolDefinition(
        name="my_tool",
        description="Does something",
        parameters={...},
        handler=my_handler,
    ))

    # Execute a tool call
    result = await registry.execute(
        ToolCall(id="1", name="my_tool", arguments={"arg": "value"})
    )
"""

from __future__ import annotations

import logging
from typing import Any

from codewhisper.agent.protocol import ToolCall, ToolDefinition, ToolResult

logger = logging.getLogger(__name__)


class ToolRegistry:
    """Registry of tools available to the agent.

    Provides registration, lookup, and execution of tools.

    Attributes:
        _tools: Internal dictionary mapping tool names to definitions.

    Example:
        registry = ToolRegistry()
        registry.register(my_tool)
        schema = registry.to_openai_schema()
        result = await registry.execute(tool_call)
    """

    def __init__(self) -> None:
        """Initialize an empty tool registry."""
        self._tools: dict[str, ToolDefinition] = {}

    def register(self, tool: ToolDefinition) -> None:
        """Register a tool with the registry.

        Args:
            tool: The tool definition to register.

        Raises:
            ValueError: If a tool with the same name is already registered.
        """
        if tool.name in self._tools:
            raise ValueError(f"Tool '{tool.name}' is already registered")
        self._tools[tool.name] = tool
        logger.debug(f"Registered tool: {tool.name}")

    def get(self, name: str) -> ToolDefinition | None:
        """Get a tool by name.

        Args:
            name: The tool name to look up.

        Returns:
            The tool definition, or None if not found.
        """
        return self._tools.get(name)

    def list_tools(self) -> list[ToolDefinition]:
        """Get all registered tools.

        Returns:
            List of all tool definitions.
        """
        return list(self._tools.values())

    def list_names(self) -> list[str]:
        """Get names of all registered tools.

        Returns:
            List of tool names.
        """
        return list(self._tools.keys())

    def to_openai_schema(self) -> list[dict[str, Any]]:
        """Convert all tools to OpenAI function calling schema.

        Returns:
            List of tool schemas compatible with OpenAI tools parameter.
        """
        return [tool.to_openai_schema() for tool in self._tools.values()]

    async def execute(self, tool_call: ToolCall) -> ToolResult:
        """Execute a tool call.

        Looks up the tool by name, validates arguments, and executes
        the handler. Catches exceptions and returns error results.

        Args:
            tool_call: The tool call to execute.

        Returns:
            ToolResult with the execution result or error.
        """
        tool = self.get(tool_call.name)

        if tool is None:
            logger.warning(f"Unknown tool called: {tool_call.name}")
            return ToolResult(
                tool_call_id=tool_call.id,
                name=tool_call.name,
                content=f"Error: Unknown tool '{tool_call.name}'",
                error=True,
            )

        try:
            logger.info(f"Executing tool: {tool_call.name}")
            logger.debug(f"Tool arguments: {tool_call.arguments}")

            # Execute the handler with the provided arguments
            result = await tool.handler(**tool_call.arguments)

            logger.info(f"Tool {tool_call.name} completed, result length: {len(result)}")

            return ToolResult(
                tool_call_id=tool_call.id,
                name=tool_call.name,
                content=result,
                error=False,
            )

        except TypeError as e:
            # Argument validation error
            logger.error(f"Tool {tool_call.name} argument error: {e}")
            return ToolResult(
                tool_call_id=tool_call.id,
                name=tool_call.name,
                content=f"Error: Invalid arguments for {tool_call.name}: {e}",
                error=True,
            )

        except Exception as e:
            # General execution error
            logger.exception(f"Tool {tool_call.name} execution failed: {e}")
            return ToolResult(
                tool_call_id=tool_call.id,
                name=tool_call.name,
                content=f"Error executing {tool_call.name}: {e}",
                error=True,
            )

    async def execute_batch(
        self,
        tool_calls: list[ToolCall],
        parallel: bool = True,
    ) -> list[ToolResult]:
        """Execute multiple tool calls.

        Args:
            tool_calls: List of tool calls to execute.
            parallel: If True, execute tools in parallel (default).

        Returns:
            List of tool results in the same order as calls.
        """
        import asyncio

        if parallel:
            # Execute all tools in parallel
            tasks = [self.execute(tc) for tc in tool_calls]
            results = await asyncio.gather(*tasks)
            return list(results)
        else:
            # Execute sequentially
            results = []
            for tc in tool_calls:
                result = await self.execute(tc)
                results.append(result)
            return results

    def __len__(self) -> int:
        """Get the number of registered tools."""
        return len(self._tools)

    def __contains__(self, name: str) -> bool:
        """Check if a tool is registered."""
        return name in self._tools
