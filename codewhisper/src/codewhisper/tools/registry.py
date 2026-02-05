"""Tool registry for CodeWhisper SDK.

This module provides the ToolRegistry class for managing tool definitions
and executing tool calls. The registry supports filtering, OpenAI schema
generation, and async execution.

Example:
    from codewhisper.tools import ToolRegistry
    from codewhisper.core import ToolDefinition, ToolCall

    # Create and populate registry
    registry = ToolRegistry()
    registry.register(read_file_tool)
    registry.register(search_code_tool)

    # Get OpenAI-compatible schema for LLM
    schema = registry.to_openai_schema()

    # Filter to subset of tools
    file_tools = registry.filter({"read_file", "write_file"})

    # Execute a tool call
    tool_call = ToolCall(id="1", name="read_file", arguments={"path": "main.py"})
    result = await registry.execute(tool_call)
"""

from __future__ import annotations

import asyncio
import inspect
import json
import logging
from typing import TYPE_CHECKING, Any

from codewhisper.core.message import ToolCall, ToolResult
from codewhisper.core.tool_protocol import ToolDefinition

if TYPE_CHECKING:
    from collections.abc import Iterable

logger = logging.getLogger(__name__)


class ToolRegistryError(Exception):
    """Base exception for tool registry errors."""


class ToolNotFoundError(ToolRegistryError):
    """Raised when a requested tool is not found in the registry."""

    def __init__(self, tool_name: str) -> None:
        """Initialize with the missing tool name.

        Args:
            tool_name: Name of the tool that was not found.
        """
        self.tool_name = tool_name
        super().__init__(f"Tool not found: {tool_name}")


class ToolExecutionError(ToolRegistryError):
    """Raised when tool execution fails."""

    def __init__(self, tool_name: str, cause: Exception) -> None:
        """Initialize with tool name and underlying cause.

        Args:
            tool_name: Name of the tool that failed.
            cause: The underlying exception that caused the failure.
        """
        self.tool_name = tool_name
        self.cause = cause
        super().__init__(f"Tool execution failed for '{tool_name}': {cause}")


class ToolRegistry:
    """Registry for tool definitions with execution support.

    The ToolRegistry manages a collection of ToolDefinition objects,
    providing methods to register, retrieve, filter, and execute tools.
    It also generates OpenAI-compatible schemas for LLM tool calling.

    Attributes:
        _tools: Internal dictionary mapping tool names to definitions.

    Example:
        registry = ToolRegistry()

        # Register tools
        registry.register(my_tool)
        registry.register_all([tool1, tool2, tool3])

        # Check and retrieve
        if "read_file" in registry:
            tool = registry.get("read_file")

        # Execute
        result = await registry.execute(tool_call)
    """

    def __init__(self) -> None:
        """Initialize an empty tool registry."""
        self._tools: dict[str, ToolDefinition] = {}

    def register(self, tool: ToolDefinition) -> None:
        """Register a tool definition.

        Args:
            tool: The tool definition to register.

        Raises:
            ValueError: If a tool with the same name is already registered.
        """
        if tool.name in self._tools:
            raise ValueError(f"Tool '{tool.name}' is already registered")

        logger.debug("Registering tool: %s", tool.name)
        self._tools[tool.name] = tool

    def register_all(self, tools: Iterable[ToolDefinition]) -> None:
        """Register multiple tools at once.

        Args:
            tools: Iterable of tool definitions to register.

        Raises:
            ValueError: If any tool name conflicts with existing registrations.
        """
        for tool in tools:
            self.register(tool)

    def unregister(self, name: str) -> bool:
        """Remove a tool from the registry.

        Args:
            name: Name of the tool to remove.

        Returns:
            True if the tool was removed, False if it wasn't registered.
        """
        if name in self._tools:
            del self._tools[name]
            logger.debug("Unregistered tool: %s", name)
            return True
        return False

    def get(self, name: str) -> ToolDefinition | None:
        """Get a tool definition by name.

        Args:
            name: Name of the tool to retrieve.

        Returns:
            The ToolDefinition if found, None otherwise.
        """
        return self._tools.get(name)

    def __getitem__(self, name: str) -> ToolDefinition:
        """Get a tool definition by name (dict-style access).

        Args:
            name: Name of the tool to retrieve.

        Returns:
            The ToolDefinition.

        Raises:
            ToolNotFoundError: If the tool is not registered.
        """
        tool = self._tools.get(name)
        if tool is None:
            raise ToolNotFoundError(name)
        return tool

    def __contains__(self, name: str) -> bool:
        """Check if a tool is registered.

        Args:
            name: Name of the tool to check.

        Returns:
            True if the tool is registered, False otherwise.
        """
        return name in self._tools

    def __len__(self) -> int:
        """Get the number of registered tools.

        Returns:
            Number of tools in the registry.
        """
        return len(self._tools)

    def __iter__(self) -> Any:
        """Iterate over tool definitions.

        Yields:
            ToolDefinition objects in registration order.
        """
        return iter(self._tools.values())

    def list_names(self) -> list[str]:
        """Get a list of all registered tool names.

        Returns:
            List of tool names in registration order.
        """
        return list(self._tools.keys())

    def filter(self, names: set[str] | None) -> ToolRegistry:
        """Create a new registry with only the specified tools.

        This is useful for restricting the tools available to the LLM
        in specific contexts.

        Args:
            names: Set of tool names to include. If None, returns a copy
                   of this registry with all tools.

        Returns:
            New ToolRegistry containing only the specified tools.
            Tools not found in this registry are silently ignored.

        Example:
            # Get only file-related tools
            file_registry = registry.filter({"read_file", "write_file", "list_files"})
        """
        new_registry = ToolRegistry()

        if names is None:
            # Copy all tools
            for tool in self._tools.values():
                new_registry._tools[tool.name] = tool
        else:
            # Copy only matching tools
            for name in names:
                if name in self._tools:
                    new_registry._tools[name] = self._tools[name]
                else:
                    logger.warning("Tool '%s' not found in registry for filter", name)

        return new_registry

    def to_openai_schema(self) -> list[dict[str, Any]]:
        """Generate OpenAI-compatible tool schemas for all registered tools.

        Returns:
            List of tool schemas in OpenAI's function calling format,
            suitable for passing to the API's tools parameter.

        Example:
            schema = registry.to_openai_schema()
            response = await client.chat.completions.create(
                model="gpt-4",
                messages=messages,
                tools=schema,
            )
        """
        return [tool.to_openai_schema() for tool in self._tools.values()]

    async def execute(self, tool_call: ToolCall) -> ToolResult:
        """Execute a tool call and return the result.

        This method handles both sync and async tool handlers, running
        sync handlers in a thread pool to avoid blocking.

        Args:
            tool_call: The tool call to execute.

        Returns:
            ToolResult containing the output or error message.

        Example:
            tool_call = ToolCall(
                id="call_123",
                name="read_file",
                arguments={"path": "main.py"},
            )
            result = await registry.execute(tool_call)

            if result.is_error:
                print(f"Error: {result.error}")
            else:
                print(f"Output: {result.content}")
        """
        tool = self._tools.get(tool_call.name)

        if tool is None:
            logger.warning("Tool not found: %s", tool_call.name)
            return ToolResult(
                tool_call_id=tool_call.id,
                name=tool_call.name,
                content="",
                error=f"Tool not found: {tool_call.name}",
            )

        if tool.handler is None:
            logger.warning("Tool has no handler: %s", tool_call.name)
            return ToolResult(
                tool_call_id=tool_call.id,
                name=tool_call.name,
                content="",
                error=f"Tool has no handler: {tool_call.name}",
            )

        # Validate arguments
        validation_errors = tool.validate_arguments(tool_call.arguments)
        if validation_errors:
            error_msg = "; ".join(validation_errors)
            logger.warning(
                "Invalid arguments for tool %s: %s", tool_call.name, error_msg
            )
            return ToolResult(
                tool_call_id=tool_call.id,
                name=tool_call.name,
                content="",
                error=f"Invalid arguments: {error_msg}",
            )

        try:
            logger.debug(
                "Executing tool %s with arguments: %s",
                tool_call.name,
                tool_call.arguments,
            )

            # Execute the handler
            if inspect.iscoroutinefunction(tool.handler):
                # Async handler - await directly
                result = await tool.handler(**tool_call.arguments)
            else:
                # Sync handler - run in thread pool
                loop = asyncio.get_event_loop()
                result = await loop.run_in_executor(
                    None, lambda: tool.handler(**tool_call.arguments)  # type: ignore[misc]
                )

            # Convert result to string if needed
            if result is None:
                content = ""
            elif isinstance(result, str):
                content = result
            else:
                # Try to serialize as JSON
                try:
                    content = json.dumps(result, indent=2, default=str)
                except (TypeError, ValueError):
                    content = str(result)

            logger.debug("Tool %s returned %d chars", tool_call.name, len(content))

            return ToolResult(
                tool_call_id=tool_call.id,
                name=tool_call.name,
                content=content,
            )

        except Exception as e:
            logger.exception("Tool execution failed for %s", tool_call.name)
            return ToolResult(
                tool_call_id=tool_call.id,
                name=tool_call.name,
                content="",
                error=f"Execution error: {e!s}",
            )

    async def execute_all(
        self, tool_calls: list[ToolCall], *, parallel: bool = True
    ) -> list[ToolResult]:
        """Execute multiple tool calls.

        Args:
            tool_calls: List of tool calls to execute.
            parallel: If True, execute tools concurrently. If False,
                     execute sequentially in order.

        Returns:
            List of ToolResults in the same order as the input tool_calls.
        """
        if not tool_calls:
            return []

        if parallel:
            # Execute all tools concurrently
            tasks = [self.execute(tc) for tc in tool_calls]
            return await asyncio.gather(*tasks)
        else:
            # Execute sequentially
            results = []
            for tc in tool_calls:
                result = await self.execute(tc)
                results.append(result)
            return results
