"""Graph node implementations for War Rig.

This module provides the node functions that are executed at each step
of the LangGraph workflow. Each node:

1. Receives the current state
2. Performs its operation (preprocessing, agent call, etc.)
3. Returns state updates

Nodes are designed to be:
- Stateless: All needed data comes from state
- Focused: Each node does one thing
- Resumable: Can be re-run from checkpoints
"""

import logging
from datetime import datetime
from typing import Any

from war_rig.agents.challenger import ChallengerAgent, ChallengerInput
from war_rig.agents.imperator import ImperatorAgent, ImperatorDecision, ImperatorInput
from war_rig.agents.scribe import ScribeAgent, ScribeInput
from war_rig.config import APIConfig, WarRigConfig
from war_rig.models.templates import FileType
from war_rig.orchestration.state import WarRigState
from war_rig.preprocessors.cobol import COBOLPreprocessor
from war_rig.preprocessors.copybook import CopybookPreprocessor
from war_rig.preprocessors.jcl import JCLPreprocessor

logger = logging.getLogger(__name__)


class WarRigNodes:
    """Collection of node functions for the War Rig graph.

    This class encapsulates all node implementations and provides
    shared configuration and agent instances.

    Attributes:
        config: War Rig configuration
        scribe: Scribe agent instance
        challenger: Challenger agent instance
        imperator: Imperator agent instance
    """

    def __init__(self, config: WarRigConfig, api_config: APIConfig | None = None):
        """Initialize the nodes with configuration.

        Args:
            config: War Rig configuration.
            api_config: API configuration. If None, loads from environment.
        """
        self.config = config
        self.api_config = api_config or config.api

        # Initialize agents with their respective configs
        self.scribe = ScribeAgent(config.scribe, self.api_config)
        self.challenger = ChallengerAgent(config.challenger, self.api_config)
        self.imperator = ImperatorAgent(config.imperator, self.api_config)

        # Initialize preprocessors
        self.preprocessors = [
            COBOLPreprocessor(),
            JCLPreprocessor(),
            CopybookPreprocessor(),
        ]

    async def preprocess(self, state: WarRigState) -> dict[str, Any]:
        """Preprocess node: Extract structural information from source code.

        This node runs deterministic preprocessing to extract structural
        hints that help the agents understand the code.

        Args:
            state: Current graph state.

        Returns:
            State updates with preprocessor result and file type.
        """
        logger.info(f"Preprocessing: {state['file_name']}")

        source_code = state["source_code"]
        file_name = state["file_name"]

        # Find appropriate preprocessor
        preprocessor = None
        for p in self.preprocessors:
            if p.can_process(source_code, file_name):
                preprocessor = p
                break

        if preprocessor is None:
            logger.warning(f"No preprocessor found for {file_name}")
            return {
                "file_type": FileType.OTHER,
                "preprocessor_result": None,
                "iteration": 1,
            }

        # Run preprocessing
        try:
            result = preprocessor.process(source_code, file_name)
            logger.info(f"Preprocessing complete: {result.program_id}")
            return {
                "file_type": result.file_type,
                "preprocessor_result": result,
                "iteration": 1,
            }
        except Exception as e:
            logger.error(f"Preprocessing failed: {e}")
            return {
                "file_type": state.get("file_type") or FileType.OTHER,
                "preprocessor_result": None,
                "iteration": 1,
                "error": f"Preprocessing failed: {e}",
            }

    async def scribe_document(self, state: WarRigState) -> dict[str, Any]:
        """Scribe node: Generate or update documentation.

        This node calls the Scribe agent to produce documentation.
        On first iteration, it generates initial documentation.
        On subsequent iterations, it addresses feedback.

        Args:
            state: Current graph state.

        Returns:
            State updates with documentation template and confidence.
        """
        iteration = state.get("iteration", 1)
        logger.info(f"Scribe documenting (iteration {iteration}): {state['file_name']}")

        # Build input
        scribe_input = ScribeInput(
            source_code=state["source_code"],
            file_name=state["file_name"],
            file_type=state.get("file_type") or FileType.OTHER,
            preprocessor_result=state.get("preprocessor_result"),
            copybook_contents=state.get("copybook_contents", {}),
            previous_template=state.get("current_template"),
            challenger_questions=state.get("current_round_questions", []),
            chrome_tickets=state.get("active_chrome_tickets", []),
            iteration=iteration,
        )

        # Call agent (or mock)
        if state.get("use_mock", False):
            output = self.scribe.create_mock_output(scribe_input)
        else:
            output = await self.scribe.ainvoke(scribe_input)

        if not output.success:
            logger.error(f"Scribe failed: {output.error}")
            return {"error": output.error}

        return {
            "current_template": output.template,
            "current_confidence": output.confidence,
            "current_round_responses": output.responses,
            "scribe_responses": output.responses,
            "tokens_used": state.get("tokens_used", 0) + output.tokens_used,
        }

    async def challenger_validate(self, state: WarRigState) -> dict[str, Any]:
        """Challenger node: Validate documentation and ask questions.

        This node calls the Challenger agent to review the Scribe's
        documentation and identify issues or areas for improvement.

        If the Challenger cycle limit has been reached, this node skips
        validation and returns an empty assessment to allow the workflow
        to proceed to approval.

        Args:
            state: Current graph state.

        Returns:
            State updates with questions and assessment.
        """
        iteration = state.get("iteration", 1)
        challenger_cycle = state.get("challenger_cycle_count", 0)
        max_challenger_cycles = state.get(
            "max_challenger_cycles",
            getattr(self.config, "max_challenger_cycles", 2)
        )

        # Check if we've exceeded the Challenger cycle limit
        if challenger_cycle >= max_challenger_cycles:
            logger.info(
                f"Challenger cycle limit reached ({challenger_cycle}/{max_challenger_cycles}), "
                f"skipping validation for {state['file_name']}"
            )
            return {
                "challenger_questions": [],
                "current_round_questions": [],
                "challenger_assessment": None,
                "challenger_cycle_count": challenger_cycle,
            }

        logger.info(f"Challenger validating (iteration {iteration}): {state['file_name']}")

        template = state.get("current_template")
        if template is None:
            logger.error("No template to validate")
            return {"error": "No template to validate"}

        # Build input
        challenger_input = ChallengerInput(
            template=template,
            source_code=state["source_code"],
            file_name=state["file_name"],
            file_type=state.get("file_type") or FileType.OTHER,
            preprocessor_result=state.get("preprocessor_result"),
            previous_questions=state.get("challenger_questions", []),
            scribe_responses=state.get("scribe_responses", []),
            max_questions=self.config.max_questions_per_round,
            iteration=iteration,
        )

        # Call agent (or mock)
        if state.get("use_mock", False):
            output = self.challenger.create_mock_output(challenger_input)
        else:
            output = await self.challenger.ainvoke(challenger_input)

        if not output.success:
            logger.error(f"Challenger failed: {output.error}")
            return {"error": output.error}

        # Create beads tickets for blocking questions
        program_id = state.get("preprocessor_result", {})
        if hasattr(program_id, "program_id"):
            program_id = program_id.program_id
        else:
            program_id = state["file_name"].replace(".cbl", "").replace(".CBL", "")

        output = self.challenger.create_beads_tickets(
            output=output,
            program_id=program_id,
            team_id=state.get("team_id", 1),
            enabled=self.config.beads_enabled and not state.get("use_mock", False),
        )

        # Track ticket IDs in state
        existing_tickets = state.get("beads_ticket_ids", [])
        new_tickets = existing_tickets + output.beads_ticket_ids

        return {
            "challenger_questions": output.questions,
            "current_round_questions": output.questions,
            "challenger_assessment": output.assessment,
            "tokens_used": state.get("tokens_used", 0) + output.tokens_used,
            "beads_ticket_ids": new_tickets,
            "challenger_cycle_count": challenger_cycle + 1,
        }

    async def scribe_respond(self, state: WarRigState) -> dict[str, Any]:
        """Scribe respond node: Answer Challenger's questions.

        This node calls the Scribe agent specifically to respond to
        the Challenger's questions without a full documentation pass.

        Note: In the current implementation, this is combined with
        scribe_document for simplicity. This node can be used for
        more granular control if needed.

        Args:
            state: Current graph state.

        Returns:
            State updates with responses.
        """
        # Currently, responding is handled in scribe_document
        # This node is provided for future granular control
        logger.info("Scribe responding to questions")
        return await self.scribe_document(state)

    async def imperator_review(self, state: WarRigState) -> dict[str, Any]:
        """Imperator node: Review and decide on documentation.

        This node calls the Imperator agent to make an approval decision.
        Possible decisions:
        - WITNESSED: Approved
        - CHROME: Needs work (issues tickets)
        - VALHALLA: Exceptional quality
        - FORCED: Approved despite issues (at max iterations or cycle limit)

        If the Imperator has issued CHROME decisions more than
        max_imperator_cycles times, the decision is forced to WITNESSED
        to prevent endless loops.

        Args:
            state: Current graph state.

        Returns:
            State updates with decision and any tickets.
        """
        iteration = state.get("iteration", 1)
        imperator_chrome_count = state.get("imperator_chrome_count", 0)
        max_imperator_cycles = state.get(
            "max_imperator_cycles",
            getattr(self.config, "max_imperator_cycles", 1)
        )

        logger.info(f"Imperator reviewing (iteration {iteration}): {state['file_name']}")

        template = state.get("current_template")
        if template is None:
            logger.error("No template to review")
            return {
                "decision": "FORCED",
                "error": "No template to review",
                "should_continue": False,
            }

        # Build input
        imperator_input = ImperatorInput(
            template=template,
            source_code=state["source_code"],
            file_name=state["file_name"],
            file_type=state.get("file_type") or FileType.OTHER,
            challenger_assessment=state.get("challenger_assessment"),
            scribe_confidence=state.get("current_confidence"),
            scribe_responses=state.get("current_round_responses", []),
            preprocessor_result=state.get("preprocessor_result"),
            max_iterations=state.get("max_iterations", self.config.max_iterations),
            max_chrome_tickets=self.config.max_chrome_tickets,
            iteration=iteration,
        )

        # Call agent (or mock)
        if state.get("use_mock", False):
            output = self.imperator.create_mock_output(imperator_input)
        else:
            output = await self.imperator.ainvoke(imperator_input)

        # Track CHROME decisions for cycle limiting
        new_chrome_count = imperator_chrome_count
        if output.decision == ImperatorDecision.CHROME:
            new_chrome_count = imperator_chrome_count + 1

        # Force approval if Imperator cycle limit exceeded
        if output.decision == ImperatorDecision.CHROME and new_chrome_count > max_imperator_cycles:
            logger.info(
                f"Imperator cycle limit reached ({imperator_chrome_count}/{max_imperator_cycles}), "
                f"forcing approval for {state['file_name']}"
            )
            output.decision = ImperatorDecision.FORCED
            output.reasoning = (
                f"{output.reasoning} [FORCED: Imperator cycle limit reached "
                f"({max_imperator_cycles} CHROME decisions)]"
            )
            # Set final template since we're approving
            if output.final_template is None and template is not None:
                from war_rig.models.templates import FinalStatus
                output.final_template = template.model_copy(deep=True)
                output.final_template.header.iteration_count = iteration
                output.final_template.header.final_status = FinalStatus.FORCED

        # Determine if we should continue
        should_continue = output.decision == ImperatorDecision.CHROME
        completed_at = None if should_continue else datetime.utcnow()

        # Get program ID for beads tickets
        program_id = state.get("preprocessor_result", {})
        if hasattr(program_id, "program_id"):
            program_id = program_id.program_id
        else:
            program_id = state["file_name"].replace(".cbl", "").replace(".CBL", "")

        # Create beads tickets for Chrome tickets
        output = self.imperator.create_beads_tickets(
            output=output,
            program_id=program_id,
            team_id=state.get("team_id", 1),
            enabled=self.config.beads_enabled and not state.get("use_mock", False),
        )

        updates: dict[str, Any] = {
            "decision": output.decision.value,
            "should_continue": should_continue,
            "tokens_used": state.get("tokens_used", 0) + output.tokens_used,
            "imperator_chrome_count": new_chrome_count,
        }

        # Track ticket IDs
        existing_tickets = state.get("beads_ticket_ids", [])
        new_tickets = existing_tickets + output.beads_ticket_ids
        updates["beads_ticket_ids"] = new_tickets

        if output.chrome_tickets:
            updates["chrome_tickets"] = output.chrome_tickets
            updates["active_chrome_tickets"] = output.chrome_tickets

        if output.final_template:
            updates["final_template"] = output.final_template

        if completed_at:
            updates["completed_at"] = completed_at

        if not output.success:
            updates["error"] = output.error

        # Close all beads tickets when approved (WITNESSED, VALHALLA, FORCED)
        if not should_continue and self.config.beads_enabled and not state.get("use_mock", False):
            from war_rig.beads import get_beads_client
            client = get_beads_client()
            all_tickets = state.get("beads_ticket_ids", []) + output.beads_ticket_ids
            if all_tickets:
                closed = client.close_tickets(
                    all_tickets,
                    reason=f"Documentation {output.decision.value} for {program_id}",
                )
                logger.info(f"Closed {closed} beads tickets on {output.decision.value}")

        return updates

    async def increment_iteration(self, state: WarRigState) -> dict[str, Any]:
        """Increment the iteration counter.

        This node is called when continuing to another iteration.
        It clears per-round state and increments the counter.

        Args:
            state: Current graph state.

        Returns:
            State updates with incremented iteration.
        """
        current = state.get("iteration", 1)
        logger.info(f"Incrementing iteration: {current} -> {current + 1}")

        return {
            "iteration": current + 1,
            "current_round_questions": [],
            "current_round_responses": [],
            "active_chrome_tickets": state.get("chrome_tickets", [])[-self.config.max_chrome_tickets:],
        }


def should_continue(state: WarRigState) -> str:
    """Routing function to determine next node after Imperator.

    This function implements the conditional edge logic for the graph.

    Args:
        state: Current graph state.

    Returns:
        Name of next node: "increment" to continue, "end" to finish.
    """
    if state.get("should_continue", False):
        return "increment"
    return "end"


def has_template(state: WarRigState) -> str:
    """Routing function to determine if Scribe produced a valid template.

    If Scribe failed to produce a template (validation error, API error, etc.),
    skip Challenger and go directly to Imperator for a FORCED decision.

    Args:
        state: Current graph state.

    Returns:
        "challenger" if template exists, "imperator" to skip challenger.
    """
    if state.get("current_template") is not None:
        return "challenger"
    logger.warning("No template produced, skipping Challenger")
    return "imperator"
