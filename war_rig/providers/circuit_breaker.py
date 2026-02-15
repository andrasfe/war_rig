"""Circuit breaker for LLM provider 401 errors.

When a provider returns consecutive HTTP 401 (authentication) errors,
the circuit breaker trips and pauses all LLM calls process-wide for a
cooldown period. This avoids hammering the provider with requests that
will certainly fail, and gives transient auth issues time to resolve.

The circuit breaker is a process-wide singleton shared across all worker
provider instances. It is configured once at orchestration startup via
``ProviderCircuitBreaker.configure()``.

Concurrency model:
    Multiple async workers may have in-flight requests when the circuit
    trips. Errors from those in-flight requests are silently absorbed
    (the circuit is already open). Only errors that arrive while the
    circuit is *closed* count toward the consecutive threshold. After
    cooldown, workers are released one at a time via a semaphore to
    probe the provider before letting everyone through.

Usage:
    Each provider returned by ``get_provider_from_env()`` is automatically
    wrapped in ``CircuitBreakerProvider``, which calls the singleton before
    and after every LLM request.

Example:
    from war_rig.providers.circuit_breaker import ProviderCircuitBreaker

    ProviderCircuitBreaker.configure(
        consecutive_threshold=3,
        cooldown_seconds=120.0,
        max_trips=50,
    )
"""

from __future__ import annotations

import asyncio
import logging
from typing import Any

from war_rig.providers.protocol import CompletionResponse, LLMProvider, Message

logger = logging.getLogger(__name__)

# Backoff multiplier cap: cooldown * 2^(trips) but never beyond this factor.
_MAX_BACKOFF_MULTIPLIER = 8


class CircuitBreakerExhaustedError(Exception):
    """Raised when the circuit breaker has tripped the maximum number of times."""


def is_auth_error(exc: BaseException) -> bool:
    """Check whether an exception represents an HTTP 401 auth error.

    Checks multiple patterns to handle different provider exception types:
    - ``exc.status_code == 401`` (OpenRouter, openai SDK)
    - ``exc.status == 401`` (aiohttp-style)
    - ``exc.code == 401`` (generic)
    - ``exc.response.status_code == 401`` (httpx-style)
    - Class name contains ``AuthenticationError`` (openai without import)
    - String ``"401"`` appears in the exception message as a fallback.

    Args:
        exc: The exception to inspect.

    Returns:
        True if the exception represents an authentication error.
    """
    # Direct status attributes (covers most provider SDKs).
    for attr in ("status_code", "status", "code"):
        val = getattr(exc, attr, None)
        if val == 401:
            return True

    # httpx-style: exc.response.status_code
    resp = getattr(exc, "response", None)
    if resp is not None:
        resp_status = getattr(resp, "status_code", None)
        if resp_status == 401:
            return True

    # Class name heuristic (e.g. openai.AuthenticationError).
    if "AuthenticationError" in type(exc).__name__:
        return True

    # Fallback: look for "401" in the exception string.
    # Guard against exceptions that fail to stringify.
    try:
        msg = str(exc)
    except Exception:
        return False
    if "401" in msg:
        return True

    return False


class ProviderCircuitBreaker:
    """Process-wide circuit breaker for LLM provider auth errors.

    The singleton is created via :meth:`configure` and shared by all
    ``CircuitBreakerProvider`` instances in the process.

    State machine:
        CLOSED  -> (consecutive 401s reach threshold) -> OPEN
        OPEN    -> (cooldown elapses)                 -> HALF_OPEN
        HALF_OPEN -> (probe succeeds)                 -> CLOSED
        HALF_OPEN -> (probe fails)                    -> OPEN
        *       -> (max_trips exceeded)               -> EXHAUSTED (fatal)

    Key design decisions for concurrent workers:
    - ``on_auth_error()`` is a no-op when the circuit is already open.
      This prevents in-flight requests (started before the trip) from
      cascading into extra trips.
    - After cooldown the circuit enters HALF_OPEN: a semaphore allows
      exactly one probe request through. If it succeeds, the circuit
      fully closes and all workers resume. If it fails, the circuit
      re-trips with exponential backoff.
    - ``_trip()`` cancels any pending reset timer before scheduling a
      new one, preventing stale timers from prematurely reopening.
    """

    _instance: ProviderCircuitBreaker | None = None

    def __init__(
        self,
        consecutive_threshold: int = 3,
        cooldown_seconds: float = 120.0,
        max_trips: int = 50,
        per_call_delay: float = 3.0,
    ) -> None:
        self._consecutive_threshold = consecutive_threshold
        self._base_cooldown_seconds = cooldown_seconds
        self._max_trips = max_trips
        self._per_call_delay = per_call_delay

        self._consecutive_errors: int = 0
        self._trip_count: int = 0

        # Event is *set* when the circuit is closed (calls allowed).
        self._allow = asyncio.Event()
        self._allow.set()

        # Semaphore used in half-open state to allow a single probe.
        self._probe_semaphore = asyncio.Semaphore(1)
        self._half_open = False

        self._reset_handle: asyncio.TimerHandle | None = None

    # ── class-level singleton management ──────────────────────────────

    @classmethod
    def configure(
        cls,
        consecutive_threshold: int = 3,
        cooldown_seconds: float = 120.0,
        max_trips: int = 50,
        per_call_delay: float = 3.0,
    ) -> ProviderCircuitBreaker:
        """Create or reconfigure the process-wide singleton.

        Args:
            consecutive_threshold: Number of consecutive 401s to trip.
            cooldown_seconds: Base seconds to pause when tripped
                (doubles on each successive trip, capped at 8x).
            max_trips: Maximum trips before raising fatal error.
            per_call_delay: Seconds to sleep after each auth error
                before re-raising. Slows down the caller's retry loop
                so errors don't pile up faster than the circuit can react.

        Returns:
            The singleton instance.
        """
        cls._instance = cls(
            consecutive_threshold=consecutive_threshold,
            cooldown_seconds=cooldown_seconds,
            max_trips=max_trips,
            per_call_delay=per_call_delay,
        )
        logger.info(
            "Circuit breaker configured: threshold=%d, cooldown=%.1fs, max_trips=%d",
            consecutive_threshold,
            cooldown_seconds,
            max_trips,
        )
        return cls._instance

    @classmethod
    def get_instance(cls) -> ProviderCircuitBreaker:
        """Return the singleton, creating a default one if needed."""
        if cls._instance is None:
            cls._instance = cls()
        return cls._instance

    @classmethod
    def reset_singleton(cls) -> None:
        """Destroy the singleton (for testing)."""
        if cls._instance is not None and cls._instance._reset_handle is not None:
            cls._instance._reset_handle.cancel()
        cls._instance = None

    # ── per-call hooks ────────────────────────────────────────────────

    async def before_call(self) -> None:
        """Block until the circuit is closed (or half-open), or raise if exhausted."""
        if self._trip_count >= self._max_trips:
            raise CircuitBreakerExhaustedError(
                f"Circuit breaker exhausted after {self._trip_count} trips "
                f"(max {self._max_trips}). Provider auth may be permanently invalid."
            )
        # Wait for the circuit to leave the fully-open state.
        await self._allow.wait()

        # In half-open state, only one probe request is allowed through.
        # Others block on the semaphore until the probe resolves.
        if self._half_open:
            await self._probe_semaphore.acquire()
            # Re-check: another coroutine may have completed the probe
            # (succeeded or failed) while we were waiting.
            if not self._half_open:
                self._probe_semaphore.release()

    def on_success(self) -> None:
        """Reset the consecutive error counter on a successful call."""
        if self._consecutive_errors > 0:
            logger.debug(
                "Circuit breaker: success after %d consecutive errors, resetting",
                self._consecutive_errors,
            )
        self._consecutive_errors = 0

        if self._half_open:
            # Probe succeeded — fully close the circuit.
            self._half_open = False
            self._trip_count = 0
            logger.info("Circuit breaker: probe succeeded, circuit CLOSED.")
            # Release the semaphore so blocked workers proceed normally.
            self._probe_semaphore.release()

    def on_auth_error(self) -> None:
        """Record a 401 error and trip the circuit if threshold reached.

        No-op when the circuit is already open — in-flight requests that
        started before the trip should not cascade into extra trips.
        """
        if self.is_open:
            # Circuit is already open; this is an in-flight straggler.
            return

        if self._half_open:
            # Probe failed — re-trip with backoff.
            logger.warning("Circuit breaker: probe FAILED, re-tripping.")
            self._probe_semaphore.release()
            self._trip()
            return

        self._consecutive_errors += 1
        logger.warning(
            "Circuit breaker: auth error %d/%d",
            self._consecutive_errors,
            self._consecutive_threshold,
        )
        if self._consecutive_errors >= self._consecutive_threshold:
            self._trip()

    # ── internal state transitions ────────────────────────────────────

    def _cooldown_for_trip(self) -> float:
        """Compute the cooldown with exponential backoff, capped."""
        multiplier = min(2 ** (self._trip_count - 1), _MAX_BACKOFF_MULTIPLIER)
        return self._base_cooldown_seconds * multiplier

    def _trip(self) -> None:
        """Open the circuit (block all callers) and schedule reset."""
        self._trip_count += 1
        self._consecutive_errors = 0
        self._half_open = False

        # Cancel any pending reset timer (prevents stale resets).
        if self._reset_handle is not None:
            self._reset_handle.cancel()
            self._reset_handle = None

        cooldown = self._cooldown_for_trip()
        logger.warning(
            "Circuit breaker TRIPPED (trip %d/%d). "
            "Pausing all LLM calls for %.1fs.",
            self._trip_count,
            self._max_trips,
            cooldown,
        )

        # Block all callers.
        self._allow.clear()

        # Schedule half-open after cooldown.
        loop = asyncio.get_running_loop()
        self._reset_handle = loop.call_later(cooldown, self._half_open_reset)

    def _half_open_reset(self) -> None:
        """Transition to half-open: allow one probe request through."""
        logger.info(
            "Circuit breaker entering HALF-OPEN state. "
            "Allowing one probe request (trip %d/%d).",
            self._trip_count,
            self._max_trips,
        )
        self._consecutive_errors = 0
        self._half_open = True

        # Reset the semaphore to exactly 1 permit.
        # (drain any stale permits, then set to 1)
        self._probe_semaphore = asyncio.Semaphore(1)

        # Unblock callers — they'll compete for the semaphore.
        self._allow.set()
        self._reset_handle = None

    # ── read-only properties for observability / testing ──────────────

    @property
    def is_open(self) -> bool:
        """True when the circuit is fully open (calls blocked)."""
        return not self._allow.is_set()

    @property
    def is_half_open(self) -> bool:
        """True when in half-open probe state."""
        return self._half_open

    @property
    def consecutive_errors(self) -> int:
        return self._consecutive_errors

    @property
    def trip_count(self) -> int:
        return self._trip_count


class CircuitBreakerProvider:
    """Wrapper that adds circuit-breaker protection to any LLM provider.

    Implements the ``LLMProvider`` protocol by delegating to an inner
    provider and calling the singleton circuit breaker before/after each
    request.
    """

    def __init__(self, inner: LLMProvider) -> None:
        self._inner = inner

    @property
    def default_model(self) -> str:
        return self._inner.default_model

    async def complete(
        self,
        messages: list[Message],
        model: str | None = None,
        temperature: float = 0.7,
        **kwargs: Any,
    ) -> CompletionResponse:
        """Delegate to the inner provider with circuit-breaker protection."""
        cb = ProviderCircuitBreaker.get_instance()
        await cb.before_call()

        try:
            response = await self._inner.complete(
                messages, model=model, temperature=temperature, **kwargs
            )
        except Exception as exc:
            if is_auth_error(exc):
                logger.warning(
                    "Circuit breaker: detected auth error (%s: %s), "
                    "sleeping %.1fs before re-raising",
                    type(exc).__name__,
                    exc,
                    cb._per_call_delay,
                )
                cb.on_auth_error()
                # Sleep before re-raising so the caller's retry loop
                # doesn't hammer the provider immediately.
                if cb._per_call_delay > 0:
                    await asyncio.sleep(cb._per_call_delay)
            else:
                logger.debug(
                    "Circuit breaker: non-auth error ignored (%s: %s)",
                    type(exc).__name__,
                    exc,
                )
            raise

        cb.on_success()
        return response
