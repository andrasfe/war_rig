"""Graph state management for War Rig.

This module defines the state structure that flows through the LangGraph
workflow. The state is a TypedDict that carries all data needed across
nodes in the graph.

The state is designed to be:
- Serializable: Can be checkpointed and restored
- Immutable-friendly: Nodes return updates, not mutations
- Complete: Contains all data needed by any node
"""

from datetime import datetime
from typing import Annotated, Any, TypedDict

from langgraph.graph.message import add_messages

from war_rig.models.assessments import ChallengerAssessment, ConfidenceAssessment
from war_rig.models.templates import DocumentationTemplate, FileType
from war_rig.models.tickets import ChromeTicket, ChallengerQuestion, ScribeResponse
from war_rig.preprocessors.base import PreprocessorResult


def merge_lists(left: list[Any] | None, right: list[Any] | None) -> list[Any]:
    """Merge two lists, handling None values.

    This is a reducer function for LangGraph state that combines lists
    from multiple state updates.

    Args:
        left: Existing list (may be None).
        right: New list to merge (may be None).

    Returns:
        Combined list.
    """
    left = left or []
    right = right or []
    return left + right


def replace_value(left: Any, right: Any) -> Any:
    """Replace left with right if right is not None.

    This is a reducer function for LangGraph state that replaces
    values only when a new value is provided.

    Args:
        left: Existing value.
        right: New value (may be None to keep left).

    Returns:
        Right if not None, otherwise left.
    """
    return right if right is not None else left


class WarRigState(TypedDict, total=False):
    """State that flows through the War Rig graph.

    This TypedDict defines all fields that can be in the state.
    Fields are organized by category for clarity.

    Input Fields (provided at start):
        source_code: The source code to analyze
        file_name: Name of the source file
        file_type: Type of source file (auto-detected if not provided)
        copybook_contents: Resolved copybook contents

    Processing State:
        iteration: Current iteration number (1-based)
        preprocessor_result: Output from preprocessing
        current_template: Current documentation draft
        current_confidence: Scribe's current confidence assessment

    Dialogue State:
        challenger_questions: All questions from Challenger
        scribe_responses: All responses from Scribe
        challenger_assessment: Challenger's validation assessment

    Decision State:
        chrome_tickets: Tickets from Imperator
        decision: Imperator's decision (WITNESSED, CHROME, VALHALLA, FORCED)
        final_template: Approved documentation (if approved)

    Metadata:
        rig_id: War Rig identifier
        started_at: When processing started
        completed_at: When processing completed
        error: Error message if failed
        tokens_used: Total tokens consumed

    Note:
        Using total=False allows nodes to return partial updates.
        LangGraph merges updates into the full state.
    """

    # =========================================================================
    # Input Fields
    # =========================================================================

    source_code: str
    """The source code to analyze."""

    file_name: str
    """Name of the source file."""

    file_type: FileType | None
    """Type of source file (COBOL, JCL, etc.)."""

    copybook_contents: dict[str, str]
    """Resolved copybook contents (name -> content)."""

    # =========================================================================
    # Processing State
    # =========================================================================

    iteration: int
    """Current iteration number (1-based)."""

    max_iterations: int
    """Maximum allowed iterations."""

    preprocessor_result: PreprocessorResult | None
    """Output from preprocessing phase."""

    current_template: DocumentationTemplate | None
    """Current documentation draft."""

    current_confidence: ConfidenceAssessment | None
    """Scribe's current confidence assessment."""

    # =========================================================================
    # Dialogue State
    # =========================================================================

    challenger_questions: Annotated[list[ChallengerQuestion], merge_lists]
    """All questions from Challenger (accumulated across iterations)."""

    current_round_questions: list[ChallengerQuestion]
    """Questions from the current iteration only."""

    scribe_responses: Annotated[list[ScribeResponse], merge_lists]
    """All responses from Scribe (accumulated across iterations)."""

    current_round_responses: list[ScribeResponse]
    """Responses from the current iteration only."""

    challenger_assessment: ChallengerAssessment | None
    """Challenger's current validation assessment."""

    # =========================================================================
    # Decision State
    # =========================================================================

    chrome_tickets: Annotated[list[ChromeTicket], merge_lists]
    """All Chrome tickets from Imperator (accumulated)."""

    active_chrome_tickets: list[ChromeTicket]
    """Currently active (unresolved) Chrome tickets."""

    decision: str | None
    """Imperator's decision (WITNESSED, CHROME, VALHALLA, FORCED)."""

    final_template: DocumentationTemplate | None
    """Approved documentation (set when decision is approval)."""

    # =========================================================================
    # Metadata
    # =========================================================================

    rig_id: str
    """War Rig identifier."""

    started_at: datetime
    """When processing started."""

    completed_at: datetime | None
    """When processing completed."""

    error: str | None
    """Error message if processing failed."""

    tokens_used: int
    """Total tokens consumed across all agent calls."""

    # =========================================================================
    # Control Flow
    # =========================================================================

    should_continue: bool
    """Whether the graph should continue to next iteration."""

    use_mock: bool
    """Whether to use mock agents (for testing)."""


def create_initial_state(
    source_code: str,
    file_name: str,
    file_type: FileType | None = None,
    copybook_contents: dict[str, str] | None = None,
    rig_id: str = "ALPHA",
    max_iterations: int = 3,
    use_mock: bool = False,
) -> WarRigState:
    """Create initial state for a War Rig run.

    This helper function creates a properly initialized state dictionary
    with all required fields set to appropriate initial values.

    Args:
        source_code: The source code to analyze.
        file_name: Name of the source file.
        file_type: Type of source file (auto-detected if None).
        copybook_contents: Resolved copybook contents.
        rig_id: War Rig identifier.
        max_iterations: Maximum allowed iterations.
        use_mock: Whether to use mock agents.

    Returns:
        Initialized WarRigState ready for graph execution.
    """
    return WarRigState(
        # Input
        source_code=source_code,
        file_name=file_name,
        file_type=file_type,
        copybook_contents=copybook_contents or {},
        # Processing
        iteration=0,  # Will be incremented to 1 at start
        max_iterations=max_iterations,
        preprocessor_result=None,
        current_template=None,
        current_confidence=None,
        # Dialogue
        challenger_questions=[],
        current_round_questions=[],
        scribe_responses=[],
        current_round_responses=[],
        challenger_assessment=None,
        # Decision
        chrome_tickets=[],
        active_chrome_tickets=[],
        decision=None,
        final_template=None,
        # Metadata
        rig_id=rig_id,
        started_at=datetime.utcnow(),
        completed_at=None,
        error=None,
        tokens_used=0,
        # Control
        should_continue=True,
        use_mock=use_mock,
    )
