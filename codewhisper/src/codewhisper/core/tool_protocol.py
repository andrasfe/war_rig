"""Tool abstractions for CodeWhisper SDK.

This module defines the ToolDefinition dataclass for representing tools
that can be called by the LLM. The definition includes JSON Schema-based
parameters for OpenAI compatibility.

Types:
    - ToolDefinition: Complete definition of a callable tool

Example:
    from codewhisper.core import ToolDefinition

    async def read_file_handler(path: str, max_lines: int = 500) -> str:
        # Implementation here
        ...

    read_file_tool = ToolDefinition(
        name="read_file",
        description="Read the contents of a file",
        parameters={
            "type": "object",
            "properties": {
                "path": {
                    "type": "string",
                    "description": "Path to the file to read",
                },
                "max_lines": {
                    "type": "integer",
                    "description": "Maximum lines to read",
                    "default": 500,
                },
            },
            "required": ["path"],
        },
        handler=read_file_handler,
    )
"""

from collections.abc import Awaitable, Callable
from dataclasses import dataclass, field
from typing import Any

# Type alias for tool handlers - can be sync or async
ToolHandler = Callable[..., Any] | Callable[..., Awaitable[Any]]


@dataclass
class ToolDefinition:
    """Definition of a tool that can be called by the LLM.

    This dataclass encapsulates everything needed to expose a tool to an LLM:
    - Name and description for the LLM to understand when to use it
    - JSON Schema parameters for input validation and LLM guidance
    - A handler function to actually execute the tool

    The parameters use JSON Schema format, which is compatible with OpenAI's
    function calling API and most other LLM providers.

    Attributes:
        name: Unique identifier for the tool (e.g., "read_file", "search_code").
        description: Human-readable description of what the tool does.
            This is sent to the LLM to help it decide when to use the tool.
        parameters: JSON Schema object describing the tool's parameters.
            Must be an object type with properties defined.
        handler: The callable that executes the tool. Can be sync or async.
            Arguments are passed as keyword arguments matching the schema.

    Example:
        # Define a simple tool
        search_tool = ToolDefinition(
            name="search_code",
            description="Search for patterns in the codebase using regex",
            parameters={
                "type": "object",
                "properties": {
                    "pattern": {
                        "type": "string",
                        "description": "Regex pattern to search for",
                    },
                    "file_type": {
                        "type": "string",
                        "description": "File extension to filter (e.g., '.py')",
                        "default": "*",
                    },
                },
                "required": ["pattern"],
            },
            handler=search_code_impl,
        )

        # Convert to OpenAI format
        openai_tool = search_tool.to_openai_schema()
    """

    name: str
    description: str
    parameters: dict[str, Any] = field(default_factory=lambda: {"type": "object"})
    handler: ToolHandler | None = None

    def __post_init__(self) -> None:
        """Validate the tool definition after initialization."""
        if not self.name:
            raise ValueError("Tool name cannot be empty")

        if not self.name.replace("_", "").replace("-", "").isalnum():
            raise ValueError(
                f"Tool name '{self.name}' must be alphanumeric "
                "(underscores and hyphens allowed)"
            )

        # Ensure parameters is a valid JSON Schema object type
        if self.parameters.get("type") != "object":
            raise ValueError("Tool parameters must be a JSON Schema object type")

    def to_openai_schema(self) -> dict[str, Any]:
        """Convert to OpenAI function calling schema format.

        Returns:
            Dictionary in OpenAI's tool/function schema format, suitable
            for passing to the API's tools parameter.

        Example:
            schema = tool.to_openai_schema()
            # Returns:
            # {
            #     "type": "function",
            #     "function": {
            #         "name": "read_file",
            #         "description": "Read the contents of a file",
            #         "parameters": { ... }
            #     }
            # }
        """
        return {
            "type": "function",
            "function": {
                "name": self.name,
                "description": self.description,
                "parameters": self.parameters,
            },
        }

    @property
    def required_params(self) -> list[str]:
        """Get the list of required parameter names.

        Returns:
            List of parameter names that are required for this tool.
        """
        required: list[str] = self.parameters.get("required", [])
        return required

    @property
    def param_names(self) -> list[str]:
        """Get all parameter names defined for this tool.

        Returns:
            List of all parameter names (required and optional).
        """
        properties = self.parameters.get("properties", {})
        return list(properties.keys())

    def validate_arguments(self, arguments: dict[str, Any]) -> list[str]:
        """Validate that arguments satisfy the schema requirements.

        This performs basic validation - checking required fields are present.
        It does not perform full JSON Schema validation.

        Args:
            arguments: Dictionary of arguments to validate.

        Returns:
            List of validation error messages. Empty if valid.
        """
        errors: list[str] = []

        # Check required parameters
        for param in self.required_params:
            if param not in arguments:
                errors.append(f"Missing required parameter: {param}")

        # Check for unknown parameters
        known_params = set(self.param_names)
        for param in arguments:
            if param not in known_params:
                errors.append(f"Unknown parameter: {param}")

        return errors
