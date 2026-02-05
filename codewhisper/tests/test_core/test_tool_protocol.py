"""Tests for ToolDefinition in codewhisper.core.tool_protocol.

This module tests:
- ToolDefinition dataclass creation
- Validation in __post_init__
- to_openai_schema() format
- validate_arguments() method
- required_params and param_names properties
"""

from typing import Any

import pytest

from codewhisper.core.tool_protocol import ToolDefinition


class TestToolDefinitionCreation:
    """Tests for creating ToolDefinition instances."""

    def test_create_minimal(self) -> None:
        """Test creating a ToolDefinition with minimal required fields."""
        tool = ToolDefinition(
            name="test_tool",
            description="A test tool",
        )

        assert tool.name == "test_tool"
        assert tool.description == "A test tool"
        assert tool.parameters == {"type": "object"}
        assert tool.handler is None

    def test_create_with_parameters(self) -> None:
        """Test creating a ToolDefinition with full parameters."""
        params: dict[str, Any] = {
            "type": "object",
            "properties": {
                "path": {
                    "type": "string",
                    "description": "File path to read",
                },
                "max_lines": {
                    "type": "integer",
                    "description": "Maximum lines to read",
                    "default": 500,
                },
            },
            "required": ["path"],
        }

        tool = ToolDefinition(
            name="read_file",
            description="Read the contents of a file",
            parameters=params,
        )

        assert tool.name == "read_file"
        assert tool.parameters == params
        assert "properties" in tool.parameters
        assert "path" in tool.parameters["properties"]

    def test_create_with_handler(self) -> None:
        """Test creating a ToolDefinition with a handler function."""

        def my_handler(path: str) -> str:
            return f"Contents of {path}"

        tool = ToolDefinition(
            name="read_file",
            description="Read a file",
            parameters={
                "type": "object",
                "properties": {
                    "path": {"type": "string"},
                },
            },
            handler=my_handler,
        )

        assert tool.handler is my_handler

    def test_create_with_async_handler(self) -> None:
        """Test creating a ToolDefinition with an async handler."""

        async def async_handler(path: str) -> str:
            return f"Contents of {path}"

        tool = ToolDefinition(
            name="async_tool",
            description="An async tool",
            parameters={"type": "object"},
            handler=async_handler,
        )

        assert tool.handler is async_handler


class TestToolDefinitionValidation:
    """Tests for ToolDefinition validation in __post_init__."""

    def test_empty_name_raises(self) -> None:
        """Test that empty name raises ValueError."""
        with pytest.raises(ValueError) as exc_info:
            ToolDefinition(name="", description="Test")

        assert "name cannot be empty" in str(exc_info.value)

    def test_invalid_name_characters(self) -> None:
        """Test that invalid characters in name raise ValueError."""
        invalid_names = [
            "tool with space",
            "tool.with.dots",
            "tool@special",
            "tool#hash",
            "tool/slash",
        ]

        for name in invalid_names:
            with pytest.raises(ValueError) as exc_info:
                ToolDefinition(name=name, description="Test")

            assert "alphanumeric" in str(exc_info.value)

    def test_valid_name_characters(self) -> None:
        """Test that valid name characters are accepted."""
        valid_names = [
            "simple",
            "with_underscore",
            "with-hyphen",
            "Mixed_Case-123",
            "tool123",
        ]

        for name in valid_names:
            tool = ToolDefinition(name=name, description="Test")
            assert tool.name == name

    def test_parameters_must_be_object_type(self) -> None:
        """Test that parameters must have type='object'."""
        with pytest.raises(ValueError) as exc_info:
            ToolDefinition(
                name="bad_tool",
                description="Test",
                parameters={"type": "array"},
            )

        assert "object type" in str(exc_info.value)

    def test_parameters_no_type_raises(self) -> None:
        """Test that parameters without type raises ValueError."""
        with pytest.raises(ValueError) as exc_info:
            ToolDefinition(
                name="bad_tool",
                description="Test",
                parameters={"properties": {}},
            )

        assert "object type" in str(exc_info.value)


class TestToOpenAISchema:
    """Tests for to_openai_schema() method."""

    def test_basic_schema(self) -> None:
        """Test basic OpenAI schema generation."""
        tool = ToolDefinition(
            name="test_tool",
            description="A test tool for testing",
        )

        schema = tool.to_openai_schema()

        assert schema == {
            "type": "function",
            "function": {
                "name": "test_tool",
                "description": "A test tool for testing",
                "parameters": {"type": "object"},
            },
        }

    def test_schema_with_parameters(self) -> None:
        """Test schema generation with full parameters."""
        params: dict[str, Any] = {
            "type": "object",
            "properties": {
                "query": {
                    "type": "string",
                    "description": "Search query",
                },
                "limit": {
                    "type": "integer",
                    "description": "Max results",
                    "default": 10,
                },
            },
            "required": ["query"],
        }

        tool = ToolDefinition(
            name="search",
            description="Search for items",
            parameters=params,
        )

        schema = tool.to_openai_schema()

        assert schema["type"] == "function"
        assert schema["function"]["name"] == "search"
        assert schema["function"]["description"] == "Search for items"
        assert schema["function"]["parameters"] == params

    def test_schema_ignores_handler(self) -> None:
        """Test that handler is not included in schema."""

        def handler() -> str:
            return "result"

        tool = ToolDefinition(
            name="tool",
            description="Test",
            handler=handler,
        )

        schema = tool.to_openai_schema()

        assert "handler" not in schema
        assert "handler" not in schema["function"]


class TestValidateArguments:
    """Tests for validate_arguments() method."""

    def test_valid_arguments(self) -> None:
        """Test validation with valid arguments."""
        tool = ToolDefinition(
            name="tool",
            description="Test",
            parameters={
                "type": "object",
                "properties": {
                    "path": {"type": "string"},
                    "lines": {"type": "integer"},
                },
                "required": ["path"],
            },
        )

        errors = tool.validate_arguments({"path": "test.py", "lines": 100})

        assert errors == []

    def test_valid_with_only_required(self) -> None:
        """Test validation with only required arguments."""
        tool = ToolDefinition(
            name="tool",
            description="Test",
            parameters={
                "type": "object",
                "properties": {
                    "path": {"type": "string"},
                    "lines": {"type": "integer"},
                },
                "required": ["path"],
            },
        )

        errors = tool.validate_arguments({"path": "test.py"})

        assert errors == []

    def test_missing_required_parameter(self) -> None:
        """Test validation with missing required parameter."""
        tool = ToolDefinition(
            name="tool",
            description="Test",
            parameters={
                "type": "object",
                "properties": {
                    "path": {"type": "string"},
                    "pattern": {"type": "string"},
                },
                "required": ["path", "pattern"],
            },
        )

        errors = tool.validate_arguments({"path": "test.py"})

        assert len(errors) == 1
        assert "Missing required parameter: pattern" in errors[0]

    def test_multiple_missing_required(self) -> None:
        """Test validation with multiple missing required parameters."""
        tool = ToolDefinition(
            name="tool",
            description="Test",
            parameters={
                "type": "object",
                "properties": {
                    "a": {"type": "string"},
                    "b": {"type": "string"},
                    "c": {"type": "string"},
                },
                "required": ["a", "b", "c"],
            },
        )

        errors = tool.validate_arguments({})

        assert len(errors) == 3

    def test_unknown_parameter(self) -> None:
        """Test validation with unknown parameter."""
        tool = ToolDefinition(
            name="tool",
            description="Test",
            parameters={
                "type": "object",
                "properties": {
                    "known": {"type": "string"},
                },
            },
        )

        errors = tool.validate_arguments({"known": "value", "unknown": "extra"})

        assert len(errors) == 1
        assert "Unknown parameter: unknown" in errors[0]

    def test_empty_parameters_accepts_empty_args(self) -> None:
        """Test that tool with no parameters accepts empty arguments."""
        tool = ToolDefinition(
            name="tool",
            description="Test",
        )

        errors = tool.validate_arguments({})

        assert errors == []

    def test_empty_parameters_rejects_args(self) -> None:
        """Test that tool with no parameters rejects any arguments."""
        tool = ToolDefinition(
            name="tool",
            description="Test",
        )

        errors = tool.validate_arguments({"unexpected": "value"})

        assert len(errors) == 1
        assert "Unknown parameter" in errors[0]


class TestParamProperties:
    """Tests for param_names and required_params properties."""

    def test_param_names_empty(self) -> None:
        """Test param_names with no parameters defined."""
        tool = ToolDefinition(name="tool", description="Test")

        assert tool.param_names == []

    def test_param_names_with_properties(self) -> None:
        """Test param_names lists all parameter names."""
        tool = ToolDefinition(
            name="tool",
            description="Test",
            parameters={
                "type": "object",
                "properties": {
                    "alpha": {"type": "string"},
                    "beta": {"type": "integer"},
                    "gamma": {"type": "boolean"},
                },
            },
        )

        names = tool.param_names

        assert set(names) == {"alpha", "beta", "gamma"}

    def test_required_params_empty(self) -> None:
        """Test required_params when none are required."""
        tool = ToolDefinition(
            name="tool",
            description="Test",
            parameters={
                "type": "object",
                "properties": {
                    "optional": {"type": "string"},
                },
            },
        )

        assert tool.required_params == []

    def test_required_params_with_required(self) -> None:
        """Test required_params lists required parameters."""
        tool = ToolDefinition(
            name="tool",
            description="Test",
            parameters={
                "type": "object",
                "properties": {
                    "required1": {"type": "string"},
                    "required2": {"type": "string"},
                    "optional": {"type": "string"},
                },
                "required": ["required1", "required2"],
            },
        )

        required = tool.required_params

        assert set(required) == {"required1", "required2"}
        assert "optional" not in required

    def test_required_params_no_required_key(self) -> None:
        """Test required_params when 'required' key is missing."""
        tool = ToolDefinition(
            name="tool",
            description="Test",
            parameters={
                "type": "object",
                "properties": {
                    "param": {"type": "string"},
                },
            },
        )

        assert tool.required_params == []


class TestToolDefinitionEquality:
    """Tests for ToolDefinition equality and representation."""

    def test_same_tools_are_equal(self) -> None:
        """Test that tools with same attributes are equal."""
        params: dict[str, Any] = {
            "type": "object",
            "properties": {"x": {"type": "string"}},
        }

        tool1 = ToolDefinition(
            name="tool",
            description="Test",
            parameters=params,
        )
        tool2 = ToolDefinition(
            name="tool",
            description="Test",
            parameters=params,
        )

        assert tool1 == tool2

    def test_different_name_not_equal(self) -> None:
        """Test that tools with different names are not equal."""
        tool1 = ToolDefinition(name="tool1", description="Test")
        tool2 = ToolDefinition(name="tool2", description="Test")

        assert tool1 != tool2

    def test_str_representation(self) -> None:
        """Test string representation includes key info."""
        tool = ToolDefinition(
            name="my_tool",
            description="A tool for testing",
        )

        str_repr = str(tool)

        assert "my_tool" in str_repr
        assert "ToolDefinition" in str_repr
