"""Tests for the circuit breaker module."""

from __future__ import annotations

import asyncio
from typing import Any
from unittest.mock import AsyncMock

import pytest

from war_rig.providers.circuit_breaker import (
    CircuitBreakerExhaustedError,
    CircuitBreakerProvider,
    ProviderCircuitBreaker,
    is_auth_error,
    is_connection_error,
)
from war_rig.providers.protocol import CompletionResponse, Message


@pytest.fixture(autouse=True)
def _reset_singleton() -> Any:
    """Reset the circuit breaker singleton before and after each test."""
    ProviderCircuitBreaker.reset_singleton()
    yield
    ProviderCircuitBreaker.reset_singleton()


# ── Helper fixtures ──────────────────────────────────────────────────


def _make_provider(
    side_effect: Exception | None = None,
) -> AsyncMock:
    """Create a mock LLMProvider."""
    provider = AsyncMock()
    provider.default_model = "test-model"
    if side_effect:
        provider.complete.side_effect = side_effect
    else:
        provider.complete.return_value = CompletionResponse(
            content="ok", model="test-model", tokens_used=10
        )
    return provider


class _AuthError(Exception):
    """Fake provider error with status_code=401."""

    status_code = 401


class _ServerError(Exception):
    """Fake provider error with status_code=500."""

    status_code = 500


class _OpenAIStyleAuthenticationError(Exception):
    """Fake error matching the openai AuthenticationError class name pattern."""

    pass


class _ConnectionError(Exception):
    """Fake provider connection error."""

    pass


# ── ProviderCircuitBreaker tests ─────────────────────────────────────


class TestProviderCircuitBreaker:
    """Tests for the circuit breaker singleton."""

    async def test_no_trip_on_success(self) -> None:
        """Successful calls don't trigger the circuit breaker."""
        cb = ProviderCircuitBreaker.configure(
            consecutive_threshold=3, cooldown_seconds=1.0, max_trips=5
        )

        for _ in range(10):
            await cb.before_call()
            cb.on_success()

        assert not cb.is_open
        assert cb.trip_count == 0
        assert cb.consecutive_errors == 0

    async def test_trip_after_consecutive_401s(self) -> None:
        """N consecutive 401 errors trip the circuit."""
        cb = ProviderCircuitBreaker.configure(
            consecutive_threshold=3, cooldown_seconds=60.0, max_trips=5
        )

        cb.on_auth_error()
        cb.on_auth_error()
        assert not cb.is_open  # Not yet

        cb.on_auth_error()  # Hits threshold
        assert cb.is_open
        assert cb.trip_count == 1

    async def test_reset_after_cooldown(self) -> None:
        """Calls resume after the cooldown period elapses."""
        cb = ProviderCircuitBreaker.configure(
            consecutive_threshold=1, cooldown_seconds=0.1, max_trips=5
        )

        cb.on_auth_error()  # Trip immediately (threshold=1)
        assert cb.is_open

        # Wait for cooldown to expire
        await asyncio.sleep(0.2)

        assert not cb.is_open
        assert cb.is_half_open
        # Should be able to call again (probe)
        await cb.before_call()

    async def test_success_resets_counter(self) -> None:
        """A success between 401 errors resets the consecutive count."""
        cb = ProviderCircuitBreaker.configure(
            consecutive_threshold=3, cooldown_seconds=60.0, max_trips=5
        )

        cb.on_auth_error()
        cb.on_auth_error()
        assert cb.consecutive_errors == 2

        cb.on_success()  # Reset
        assert cb.consecutive_errors == 0

        # Two more errors shouldn't trip (need 3 consecutive)
        cb.on_auth_error()
        cb.on_auth_error()
        assert not cb.is_open

    async def test_max_trips_raises(self) -> None:
        """CircuitBreakerExhaustedError is raised after max trips."""
        cb = ProviderCircuitBreaker.configure(
            consecutive_threshold=1, cooldown_seconds=0.05, max_trips=2
        )

        # Trip 1
        cb.on_auth_error()
        assert cb.trip_count == 1
        await asyncio.sleep(0.1)  # Wait for half-open

        # Trip 2: probe fails
        cb.on_auth_error()
        assert cb.trip_count == 2
        await asyncio.sleep(0.15)  # Wait for half-open (with backoff)

        # Now max_trips is reached — before_call should raise
        with pytest.raises(CircuitBreakerExhaustedError):
            await cb.before_call()

    async def test_in_flight_errors_ignored_when_open(self) -> None:
        """Errors arriving after the circuit is already open don't re-trip."""
        cb = ProviderCircuitBreaker.configure(
            consecutive_threshold=2, cooldown_seconds=60.0, max_trips=10
        )

        # Trip the circuit
        cb.on_auth_error()
        cb.on_auth_error()
        assert cb.is_open
        assert cb.trip_count == 1

        # Simulate in-flight workers completing with 401s
        cb.on_auth_error()
        cb.on_auth_error()
        cb.on_auth_error()
        cb.on_auth_error()

        # Should still be only 1 trip — stragglers ignored
        assert cb.trip_count == 1

    async def test_cancel_pending_timer_on_retrip(self) -> None:
        """Re-tripping while a timer is still pending cancels it."""
        cb = ProviderCircuitBreaker.configure(
            consecutive_threshold=1, cooldown_seconds=5.0, max_trips=10
        )

        # Trip 1 — timer scheduled 5s from now (won't fire during test)
        cb.on_auth_error()
        assert cb.trip_count == 1
        first_handle = cb._reset_handle
        assert first_handle is not None
        assert not first_handle.cancelled()

        # Force a second trip directly (simulates defensive re-trip path)
        cb._trip()
        assert cb.trip_count == 2

        # The first timer should have been cancelled
        assert first_handle.cancelled()
        # A new timer should be in place
        assert cb._reset_handle is not None
        assert cb._reset_handle is not first_handle

    async def test_exponential_backoff(self) -> None:
        """Each successive trip increases the cooldown."""
        cb = ProviderCircuitBreaker.configure(
            consecutive_threshold=1, cooldown_seconds=10.0, max_trips=10
        )

        # trip_count=0: first trip will use 2^(1-1) * 10 = 10s
        cb._trip()
        assert cb.trip_count == 1

        # trip_count=1: next trip will use 2^(2-1) * 10 = 20s
        cb._trip()
        assert cb.trip_count == 2

        # trip_count=2: next trip will use 2^(3-1) * 10 = 40s
        cb._trip()
        assert cb.trip_count == 3

        # Verify the current cooldown value reflects trip_count=3
        # Next would be 2^(3-1) * 10 = 40s
        assert cb._cooldown_for_trip() == 40.0

    async def test_backoff_capped(self) -> None:
        """Backoff multiplier is capped at 8x."""
        cb = ProviderCircuitBreaker.configure(
            consecutive_threshold=1, cooldown_seconds=10.0, max_trips=20
        )

        # Simulate 10 trips to exceed cap
        for _ in range(10):
            cb._trip_count += 1

        # Should be capped at 8x
        assert cb._cooldown_for_trip() == 80.0

    async def test_half_open_probe_success_closes_circuit(self) -> None:
        """A successful probe in half-open state fully closes the circuit."""
        cb = ProviderCircuitBreaker.configure(
            consecutive_threshold=1, cooldown_seconds=0.05, max_trips=10
        )

        cb.on_auth_error()
        assert cb.is_open
        assert cb.trip_count == 1

        await asyncio.sleep(0.1)
        assert cb.is_half_open

        # Probe succeeds
        await cb.before_call()
        cb.on_success()

        assert not cb.is_open
        assert not cb.is_half_open
        assert cb.trip_count == 0  # Reset on success

    async def test_half_open_probe_failure_retrips(self) -> None:
        """A failed probe in half-open state re-trips the circuit."""
        cb = ProviderCircuitBreaker.configure(
            consecutive_threshold=1, cooldown_seconds=0.05, max_trips=10
        )

        cb.on_auth_error()
        assert cb.trip_count == 1
        await asyncio.sleep(0.1)
        assert cb.is_half_open

        # Probe fails
        cb.on_auth_error()
        assert cb.is_open
        assert not cb.is_half_open
        assert cb.trip_count == 2


# ── CircuitBreakerProvider tests ─────────────────────────────────────


class TestCircuitBreakerProvider:
    """Tests for the provider wrapper."""

    async def test_wrapper_delegates_normally(self) -> None:
        """CircuitBreakerProvider passes through on success."""
        ProviderCircuitBreaker.configure(
            consecutive_threshold=3, cooldown_seconds=60.0, max_trips=5,
            per_call_delay=0,
        )
        inner = _make_provider()
        wrapper = CircuitBreakerProvider(inner)

        messages = [Message(role="user", content="hello")]
        result = await wrapper.complete(messages, model="m", temperature=0.5)

        assert result.content == "ok"
        inner.complete.assert_awaited_once_with(
            messages, model="m", temperature=0.5
        )

    async def test_wrapper_exposes_default_model(self) -> None:
        """default_model property delegates to inner provider."""
        inner = _make_provider()
        wrapper = CircuitBreakerProvider(inner)
        assert wrapper.default_model == "test-model"

    async def test_wrapper_detects_auth_error(self) -> None:
        """Wrapper correctly identifies 401 errors and records them."""
        cb = ProviderCircuitBreaker.configure(
            consecutive_threshold=5, cooldown_seconds=60.0, max_trips=10,
            per_call_delay=0,
        )
        inner = _make_provider(side_effect=_AuthError("unauthorized"))
        wrapper = CircuitBreakerProvider(inner)

        messages = [Message(role="user", content="hello")]
        with pytest.raises(_AuthError):
            await wrapper.complete(messages)

        assert cb.consecutive_errors == 1

    async def test_wrapper_detects_connection_error(self) -> None:
        """Wrapper correctly identifies connection errors and records them."""
        cb = ProviderCircuitBreaker.configure(
            consecutive_threshold=5, cooldown_seconds=60.0, max_trips=10,
            per_call_delay=0,
        )
        inner = _make_provider(
            side_effect=_ConnectionError("Connection error")
        )
        wrapper = CircuitBreakerProvider(inner)

        messages = [Message(role="user", content="hello")]
        with pytest.raises(_ConnectionError):
            await wrapper.complete(messages)

        assert cb.consecutive_errors == 1

    async def test_wrapper_detects_timeout(self) -> None:
        """Wrapper treats timeouts as connection errors."""
        cb = ProviderCircuitBreaker.configure(
            consecutive_threshold=5, cooldown_seconds=60.0, max_trips=10,
            per_call_delay=0, call_timeout=0.1,
        )

        async def slow_complete(*args: Any, **kwargs: Any) -> CompletionResponse:
            await asyncio.sleep(5.0)
            return CompletionResponse(content="ok", model="m", tokens_used=1)

        inner = AsyncMock()
        inner.default_model = "test-model"
        inner.complete.side_effect = slow_complete
        wrapper = CircuitBreakerProvider(inner)

        messages = [Message(role="user", content="hello")]
        with pytest.raises(TimeoutError):
            await wrapper.complete(messages)

        assert cb.consecutive_errors == 1

    async def test_wrapper_ignores_non_circuit_errors(self) -> None:
        """Non-auth, non-connection errors are re-raised without recording."""
        cb = ProviderCircuitBreaker.configure(
            consecutive_threshold=5, cooldown_seconds=60.0, max_trips=10,
            per_call_delay=0,
        )
        inner = _make_provider(side_effect=_ServerError("server error"))
        wrapper = CircuitBreakerProvider(inner)

        messages = [Message(role="user", content="hello")]
        with pytest.raises(_ServerError):
            await wrapper.complete(messages)

        assert cb.consecutive_errors == 0

    async def test_wrapper_resets_counter_on_success(self) -> None:
        """A successful call through the wrapper resets the counter."""
        cb = ProviderCircuitBreaker.configure(
            consecutive_threshold=5, cooldown_seconds=60.0, max_trips=10,
            per_call_delay=0,
        )

        # First call fails
        inner_fail = _make_provider(side_effect=_AuthError("unauthorized"))
        wrapper_fail = CircuitBreakerProvider(inner_fail)
        with pytest.raises(_AuthError):
            await wrapper_fail.complete([Message(role="user", content="hi")])
        assert cb.consecutive_errors == 1

        # Second call succeeds
        inner_ok = _make_provider()
        wrapper_ok = CircuitBreakerProvider(inner_ok)
        await wrapper_ok.complete([Message(role="user", content="hi")])
        assert cb.consecutive_errors == 0

    async def test_reconnect_after_connection_error(self) -> None:
        """Factory is called to create a fresh provider after connection error."""
        ProviderCircuitBreaker.configure(
            consecutive_threshold=5, cooldown_seconds=60.0, max_trips=10,
            per_call_delay=0,
        )

        # First provider fails with connection error
        inner_bad = _make_provider(side_effect=_ConnectionError("Connection error"))
        inner_good = _make_provider()
        factory_calls: list[AsyncMock] = []

        def factory() -> AsyncMock:
            factory_calls.append(inner_good)
            return inner_good

        wrapper = CircuitBreakerProvider(inner_bad, factory=factory)

        # First call fails
        messages = [Message(role="user", content="hello")]
        with pytest.raises(_ConnectionError):
            await wrapper.complete(messages)

        # Factory hasn't been called yet (happens on next call)
        assert len(factory_calls) == 0
        assert wrapper._needs_reconnect

        # Second call uses the fresh provider from factory
        result = await wrapper.complete(messages)
        assert result.content == "ok"
        assert len(factory_calls) == 1
        assert not wrapper._needs_reconnect

    async def test_reconnect_after_timeout(self) -> None:
        """Factory is called to create a fresh provider after timeout."""
        ProviderCircuitBreaker.configure(
            consecutive_threshold=5, cooldown_seconds=60.0, max_trips=10,
            per_call_delay=0, call_timeout=0.1,
        )

        async def slow_complete(*args: Any, **kwargs: Any) -> CompletionResponse:
            await asyncio.sleep(5.0)
            return CompletionResponse(content="ok", model="m", tokens_used=1)

        inner_slow = AsyncMock()
        inner_slow.default_model = "test-model"
        inner_slow.complete.side_effect = slow_complete

        inner_good = _make_provider()
        reconnected = False

        def factory() -> AsyncMock:
            nonlocal reconnected
            reconnected = True
            return inner_good

        wrapper = CircuitBreakerProvider(inner_slow, factory=factory)

        # First call times out
        messages = [Message(role="user", content="hello")]
        with pytest.raises(TimeoutError):
            await wrapper.complete(messages)

        assert wrapper._needs_reconnect

        # Second call uses fresh provider (need higher timeout)
        ProviderCircuitBreaker.configure(
            consecutive_threshold=5, cooldown_seconds=60.0, max_trips=10,
            per_call_delay=0, call_timeout=10.0,
        )
        result = await wrapper.complete(messages)
        assert result.content == "ok"
        assert reconnected

    async def test_no_reconnect_after_auth_error(self) -> None:
        """Auth errors do not trigger reconnection."""
        ProviderCircuitBreaker.configure(
            consecutive_threshold=5, cooldown_seconds=60.0, max_trips=10,
            per_call_delay=0,
        )

        inner = _make_provider(side_effect=_AuthError("unauthorized"))
        factory_called = False

        def factory() -> AsyncMock:
            nonlocal factory_called
            factory_called = True
            return _make_provider()

        wrapper = CircuitBreakerProvider(inner, factory=factory)

        messages = [Message(role="user", content="hello")]
        with pytest.raises(_AuthError):
            await wrapper.complete(messages)

        # Auth errors should NOT trigger reconnection
        assert not wrapper._needs_reconnect
        assert not factory_called

    async def test_no_factory_no_crash(self) -> None:
        """Without a factory, connection errors don't crash on reconnect."""
        ProviderCircuitBreaker.configure(
            consecutive_threshold=5, cooldown_seconds=60.0, max_trips=10,
            per_call_delay=0,
        )

        call_count = 0

        async def complete_side_effect(
            *args: Any, **kwargs: Any
        ) -> CompletionResponse:
            nonlocal call_count
            call_count += 1
            if call_count == 1:
                raise _ConnectionError("Connection error")
            return CompletionResponse(content="ok", model="m", tokens_used=1)

        inner = AsyncMock()
        inner.default_model = "test-model"
        inner.complete.side_effect = complete_side_effect

        # No factory provided
        wrapper = CircuitBreakerProvider(inner)

        messages = [Message(role="user", content="hello")]
        with pytest.raises(_ConnectionError):
            await wrapper.complete(messages)

        # Still uses same inner (no factory), but doesn't crash
        result = await wrapper.complete(messages)
        assert result.content == "ok"


# ── is_auth_error tests ──────────────────────────────────────────────


class TestIsAuthError:
    """Tests for the is_auth_error helper."""

    def test_status_code_401(self) -> None:
        """Exception with status_code=401 is recognized."""
        assert is_auth_error(_AuthError("bad"))

    def test_status_code_500_not_auth(self) -> None:
        """Exception with status_code=500 is not auth error."""
        assert not is_auth_error(_ServerError("bad"))

    def test_status_attribute_401(self) -> None:
        """Exception with status=401 (aiohttp-style) is recognized."""

        class AioHttpError(Exception):
            status = 401

        assert is_auth_error(AioHttpError("bad"))

    def test_code_attribute_401(self) -> None:
        """Exception with code=401 is recognized."""

        class CodeError(Exception):
            code = 401

        assert is_auth_error(CodeError("bad"))

    def test_response_status_code_401(self) -> None:
        """Exception with response.status_code=401 (httpx-style) is recognized."""

        class FakeResponse:
            status_code = 401

        class HttpxError(Exception):
            def __init__(self) -> None:
                super().__init__("bad")
                self.response = FakeResponse()

        assert is_auth_error(HttpxError())

    def test_response_status_code_500_not_auth(self) -> None:
        """Exception with response.status_code=500 is not auth error."""

        class FakeResponse:
            status_code = 500

        class HttpxError(Exception):
            def __init__(self) -> None:
                super().__init__("bad")
                self.response = FakeResponse()

        assert not is_auth_error(HttpxError())

    def test_class_name_authentication_error(self) -> None:
        """Exception with 'AuthenticationError' in class name is recognized."""
        assert is_auth_error(_OpenAIStyleAuthenticationError("bad"))

    def test_message_contains_401(self) -> None:
        """Exception with '401' in message string is recognized."""
        assert is_auth_error(Exception("HTTP error 401: Unauthorized"))

    def test_message_without_401(self) -> None:
        """Exception without '401' in message is not recognized."""
        assert not is_auth_error(Exception("HTTP error 500: Server Error"))

    def test_plain_exception_not_auth(self) -> None:
        """Plain Exception is not recognized as auth error."""
        assert not is_auth_error(Exception("something"))

    def test_no_status_code_attribute(self) -> None:
        """Exception without status_code attribute is not auth error."""
        assert not is_auth_error(ValueError("bad"))

    def test_status_code_none(self) -> None:
        """Exception with status_code=None is not auth error."""

        class NoneStatus(Exception):
            status_code = None

        assert not is_auth_error(NoneStatus("bad"))


# ── is_connection_error tests ────────────────────────────────────────


class TestIsConnectionError:
    """Tests for the is_connection_error helper."""

    def test_connection_error_message(self) -> None:
        """Exception with 'Connection error' in message is recognized."""
        assert is_connection_error(Exception("Connection error"))

    def test_connection_refused(self) -> None:
        """Exception with 'Connection refused' in message is recognized."""
        assert is_connection_error(Exception("Connection refused"))

    def test_timeout_message(self) -> None:
        """Exception with 'timed out' in message is recognized."""
        assert is_connection_error(Exception("The request timed out"))

    def test_timeout_class_name(self) -> None:
        """Exception with 'TimeoutError' class name is recognized."""
        assert is_connection_error(TimeoutError("timed out"))

    def test_api_connection_error_class(self) -> None:
        """Exception with APIConnectionError class name is recognized."""

        class APIConnectionError(Exception):
            pass

        assert is_connection_error(APIConnectionError("failed"))

    def test_ssl_error_message(self) -> None:
        """Exception with 'ssl' in message is recognized."""
        assert is_connection_error(Exception("SSL: CERTIFICATE_VERIFY_FAILED"))

    def test_server_error_not_connection(self) -> None:
        """A plain server error is not a connection error."""
        assert not is_connection_error(_ServerError("server error"))

    def test_auth_error_not_connection(self) -> None:
        """Auth errors are not connection errors."""
        assert not is_connection_error(_AuthError("unauthorized"))

    def test_plain_exception_not_connection(self) -> None:
        """A generic exception is not a connection error."""
        assert not is_connection_error(Exception("something else entirely"))

    def test_safechain_connection_error(self) -> None:
        """SafechainProviderError with connection message is recognized."""
        assert is_connection_error(
            Exception("Safechain API error: Connection error.")
        )
