"""Tests for agent state models.

This module tests:
- ConversationMessage creation and conversion
- SkillContext model
- CodeSearchResult and FileContent models
- AgentState with message accumulation
- create_initial_state helper
"""



from codewhisper.agent.state import (
    AgentState,
    CodeSearchResult,
    ConversationMessage,
    FileContent,
    SkillContext,
    create_initial_state,
)


class TestConversationMessage:
    """Tests for ConversationMessage dataclass."""

    def test_create_user_message(self) -> None:
        """Test creating a user message."""
        msg = ConversationMessage(role="user", content="Hello!")
        assert msg.role == "user"
        assert msg.content == "Hello!"
        assert msg.tool_calls is None
        assert msg.tool_call_id is None
        assert msg.name is None

    def test_create_system_message(self) -> None:
        """Test creating a system message."""
        msg = ConversationMessage(role="system", content="You are a helpful assistant.")
        assert msg.role == "system"
        assert msg.content == "You are a helpful assistant."

    def test_create_assistant_message(self) -> None:
        """Test creating an assistant message."""
        msg = ConversationMessage(role="assistant", content="I can help with that.")
        assert msg.role == "assistant"
        assert msg.content == "I can help with that."

    def test_create_tool_message(self) -> None:
        """Test creating a tool message."""
        msg = ConversationMessage(
            role="tool",
            content="Search results: 5 matches found",
            tool_call_id="call-123",
            name="search_skills",
        )
        assert msg.role == "tool"
        assert msg.tool_call_id == "call-123"
        assert msg.name == "search_skills"

    def test_message_with_tool_calls(self) -> None:
        """Test message with tool calls."""
        tool_calls = [
            {
                "id": "call-123",
                "type": "function",
                "function": {"name": "search_skills", "arguments": '{"query": "auth"}'},
            }
        ]
        msg = ConversationMessage(
            role="assistant",
            content="",
            tool_calls=tool_calls,
        )
        assert msg.tool_calls == tool_calls

    def test_to_dict_basic(self) -> None:
        """Test to_dict conversion for basic message."""
        msg = ConversationMessage(role="user", content="Hello!")
        result = msg.to_dict()

        assert result == {"role": "user", "content": "Hello!"}

    def test_to_dict_with_tool_calls(self) -> None:
        """Test to_dict conversion includes tool_calls."""
        tool_calls = [{"id": "call-123", "type": "function"}]
        msg = ConversationMessage(
            role="assistant",
            content="",
            tool_calls=tool_calls,
        )
        result = msg.to_dict()

        assert "tool_calls" in result
        assert result["tool_calls"] == tool_calls

    def test_to_dict_with_tool_response(self) -> None:
        """Test to_dict conversion includes tool response fields."""
        msg = ConversationMessage(
            role="tool",
            content="Result",
            tool_call_id="call-123",
            name="search_skills",
        )
        result = msg.to_dict()

        assert result["tool_call_id"] == "call-123"
        assert result["name"] == "search_skills"


class TestSkillContext:
    """Tests for SkillContext model."""

    def test_create_skill_context(self) -> None:
        """Test creating skill context."""
        ctx = SkillContext(
            name="test-skill",
            description="A test skill",
            content="# Test Skill\n\nContent here.",
        )

        assert ctx.name == "test-skill"
        assert ctx.description == "A test skill"
        assert ctx.content == "# Test Skill\n\nContent here."

    def test_skill_context_defaults(self) -> None:
        """Test skill context default values."""
        ctx = SkillContext(name="minimal")

        assert ctx.description == ""
        assert ctx.content == ""


class TestCodeSearchResult:
    """Tests for CodeSearchResult model."""

    def test_create_search_result(self) -> None:
        """Test creating code search result."""
        result = CodeSearchResult(
            file_path="cbl/TEST.cbl",
            line_number=42,
            line_content="       PERFORM PROCESS-DATA",
            context="Lines 40-44...",
        )

        assert result.file_path == "cbl/TEST.cbl"
        assert result.line_number == 42
        assert result.line_content == "       PERFORM PROCESS-DATA"
        assert result.context == "Lines 40-44..."

    def test_search_result_defaults(self) -> None:
        """Test search result default values."""
        result = CodeSearchResult(
            file_path="test.cbl",
            line_number=1,
            line_content="test",
        )

        assert result.context == ""


class TestFileContent:
    """Tests for FileContent model."""

    def test_create_file_content(self) -> None:
        """Test creating file content."""
        content = FileContent(
            file_path="cbl/TEST.cbl",
            content="IDENTIFICATION DIVISION.\nPROGRAM-ID. TEST.",
            truncated=False,
            total_lines=2,
        )

        assert content.file_path == "cbl/TEST.cbl"
        assert "IDENTIFICATION DIVISION" in content.content
        assert content.truncated is False
        assert content.total_lines == 2

    def test_file_content_defaults(self) -> None:
        """Test file content default values."""
        content = FileContent(
            file_path="test.cbl",
            content="test",
        )

        assert content.truncated is False
        assert content.total_lines == 0

    def test_file_content_truncated(self) -> None:
        """Test file content when truncated."""
        content = FileContent(
            file_path="large_file.cbl",
            content="...first 500 lines...",
            truncated=True,
            total_lines=2500,
        )

        assert content.truncated is True
        assert content.total_lines == 2500


class TestAgentState:
    """Tests for AgentState dataclass."""

    def test_create_empty_state(self) -> None:
        """Test creating empty agent state."""
        state = AgentState()

        assert state.messages == []
        assert state.loaded_skills == []
        assert state.skill_contexts == []
        assert state.search_results == []
        assert state.file_contents == []
        assert state.current_query == ""
        assert state.needs_more_info is False

    def test_create_state_with_messages(self) -> None:
        """Test creating state with initial messages."""
        messages = [
            ConversationMessage(role="user", content="Hello!"),
            ConversationMessage(role="assistant", content="Hi!"),
        ]
        state = AgentState(messages=messages)

        assert len(state.messages) == 2
        assert state.messages[0].role == "user"

    def test_create_state_with_loaded_skills(self) -> None:
        """Test creating state with loaded skills."""
        state = AgentState(
            loaded_skills=["system-overview", "cbpaup0c"],
            skill_contexts=[
                SkillContext(name="system-overview", content="Overview..."),
                SkillContext(name="cbpaup0c", content="Program docs..."),
            ],
        )

        assert len(state.loaded_skills) == 2
        assert "system-overview" in state.loaded_skills
        assert len(state.skill_contexts) == 2

    def test_create_state_with_search_results(self) -> None:
        """Test creating state with search results."""
        state = AgentState(
            search_results=[
                CodeSearchResult(
                    file_path="test.cbl",
                    line_number=10,
                    line_content="CALL 'SUBPROG'",
                )
            ],
        )

        assert len(state.search_results) == 1
        assert state.search_results[0].file_path == "test.cbl"

    def test_create_state_with_current_query(self) -> None:
        """Test creating state with current query."""
        state = AgentState(
            current_query="What does CBPAUP0C do?",
        )

        assert state.current_query == "What does CBPAUP0C do?"

    def test_state_needs_more_info_flag(self) -> None:
        """Test needs_more_info flag."""
        state = AgentState(needs_more_info=True)

        assert state.needs_more_info is True


class TestCreateInitialState:
    """Tests for create_initial_state helper function."""

    def test_create_initial_state_empty(self) -> None:
        """Test creating initial state without system prompt."""
        state = create_initial_state()

        assert state.messages == []
        assert state.loaded_skills == []
        assert state.current_query == ""

    def test_create_initial_state_with_system_prompt(self) -> None:
        """Test creating initial state with system prompt."""
        state = create_initial_state(system_prompt="You are CodeWhisper.")

        assert len(state.messages) == 1
        assert state.messages[0].role == "system"
        assert state.messages[0].content == "You are CodeWhisper."

    def test_create_initial_state_with_empty_system_prompt(self) -> None:
        """Test creating initial state with empty system prompt."""
        state = create_initial_state(system_prompt="")

        # Empty string is falsy, so no message added
        assert state.messages == []
