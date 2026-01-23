"""Custom exceptions for the War Rig system.

This module provides custom exception classes for handling specific error
conditions in the War Rig workflow, particularly for fatal errors that
should terminate processing when exit_on_error is enabled.
"""


class FatalWorkerError(Exception):
    """Raised when a worker encounters a fatal error.

    This exception is raised by workers (Scribe, Challenger) when they
    encounter an error during processing and the exit_on_error configuration
    is enabled. It propagates up to the orchestrator to trigger immediate
    system shutdown.

    Attributes:
        worker_id: Identifier of the worker that encountered the error.
        ticket_id: ID of the ticket being processed when the error occurred.
        error: Description of the error that occurred.

    Example:
        raise FatalWorkerError(
            worker_id="scribe-1",
            ticket_id="DOC-12345678",
            error="Failed to parse LLM response as JSON",
        )
    """

    def __init__(self, worker_id: str, ticket_id: str, error: str):
        """Initialize the FatalWorkerError.

        Args:
            worker_id: Identifier of the worker that encountered the error.
            ticket_id: ID of the ticket being processed when the error occurred.
            error: Description of the error that occurred.
        """
        self.worker_id = worker_id
        self.ticket_id = ticket_id
        self.error = error
        super().__init__(f"Fatal error in {worker_id} processing {ticket_id}: {error}")
