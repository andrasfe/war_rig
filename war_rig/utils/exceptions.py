"""Custom exceptions for the War Rig system.

This module provides custom exception classes for handling specific error
conditions in the War Rig workflow, particularly for fatal errors that
should terminate processing when exit_on_error is enabled.
"""


class MaxTicketRetriesExceeded(Exception):
    """Raised when a ticket exceeds the maximum retry limit.

    This exception is raised when a ticket has been retried too many times
    (including Super-Scribe escalation) and the system should exit to prevent
    endless loops. This occurs when:
    - A DOCUMENTATION ticket fails repeatedly
    - Super-Scribe escalation also fails
    - The retry count exceeds max_ticket_retries config

    Attributes:
        ticket_id: ID of the ticket that exceeded retry limit.
        file_name: Name of the file associated with the ticket.
        retry_count: Number of retries attempted.
        max_retries: Maximum retries allowed by configuration.

    Example:
        raise MaxTicketRetriesExceeded(
            ticket_id="DOC-12345678",
            file_name="TESTPROG.cbl",
            retry_count=6,
            max_retries=5,
        )
    """

    def __init__(
        self,
        ticket_id: str,
        file_name: str,
        retry_count: int,
        max_retries: int,
    ):
        """Initialize the MaxTicketRetriesExceeded exception.

        Args:
            ticket_id: ID of the ticket that exceeded retry limit.
            file_name: Name of the file associated with the ticket.
            retry_count: Number of retries attempted.
            max_retries: Maximum retries allowed by configuration.
        """
        self.ticket_id = ticket_id
        self.file_name = file_name
        self.retry_count = retry_count
        self.max_retries = max_retries
        super().__init__(
            f"Ticket {ticket_id} ({file_name}) exceeded maximum retry limit: "
            f"{retry_count} attempts > {max_retries} max. "
            f"This may indicate a persistent issue with the file or LLM configuration."
        )


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
