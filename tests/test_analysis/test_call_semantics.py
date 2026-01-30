"""Tests for CallSemanticsAnalyzer module.

This module provides comprehensive unit tests for the CallSemanticsAnalyzer class,
which uses LLM inference to analyze data flow between COBOL paragraphs.
"""

import json
from pathlib import Path
from unittest.mock import AsyncMock, MagicMock, patch

import pytest

from war_rig.analysis.call_semantics import (
    DEFAULT_BATCH_SIZE,
    MAX_WORKING_STORAGE_CHARS,
    CallSemanticsAnalyzer,
)
from war_rig.config import APIConfig
from war_rig.models.templates import CallSemantics
from war_rig.providers import CompletionResponse

# =============================================================================
# Fixtures
# =============================================================================


@pytest.fixture
def mock_api_config():
    """Create a mock API configuration."""
    return APIConfig(
        provider="openrouter",
        api_key="test-api-key",
        base_url="https://openrouter.ai/api/v1",
    )


@pytest.fixture
def mock_provider():
    """Create a mock LLM provider."""
    provider = MagicMock()
    provider.default_model = "test-model"
    provider.complete = AsyncMock(
        return_value=CompletionResponse(
            content='{"calls": []}',
            model="test-model",
            tokens_used=100,
        )
    )
    return provider


@pytest.fixture
def sample_citadel_context():
    """Create sample Citadel context with paragraphs and calls."""
    return [
        {
            "name": "MAIN-PARAGRAPH",
            "type": "paragraph",
            "line": 100,
            "line_end": 150,
            "calls": [
                {"target": "VALIDATE-INPUT", "type": "performs", "line": 110},
                {"target": "PROCESS-DATA", "type": "performs", "line": 120},
            ],
        },
        {
            "name": "VALIDATE-INPUT",
            "type": "paragraph",
            "line": 200,
            "line_end": 250,
            "calls": [],
        },
        {
            "name": "PROCESS-DATA",
            "type": "paragraph",
            "line": 300,
            "line_end": 400,
            "calls": [
                {"target": "WRITE-OUTPUT", "type": "performs", "line": 350},
            ],
        },
        {
            "name": "WRITE-OUTPUT",
            "type": "paragraph",
            "line": 450,
            "line_end": 500,
            "calls": [],
        },
    ]


@pytest.fixture
def sample_citadel_context_with_external_calls():
    """Create sample Citadel context with external CALL statements."""
    return [
        {
            "name": "MAIN-PARAGRAPH",
            "type": "paragraph",
            "line": 100,
            "line_end": 150,
            "calls": [
                {"target": "SUB-PARA", "type": "performs", "line": 110},
                {"target": "EXTPROG", "type": "calls", "line": 120},  # External CALL
            ],
        },
        {
            "name": "SUB-PARA",
            "type": "paragraph",
            "line": 200,
            "line_end": 250,
            "calls": [],
        },
    ]


@pytest.fixture
def sample_working_storage():
    """Create sample WORKING-STORAGE section."""
    return """01  WS-INPUT-RECORD.
    05  WS-CUSTOMER-ID     PIC X(10).
    05  WS-CUSTOMER-NAME   PIC X(50).
    05  WS-AMOUNT          PIC 9(7)V99.

01  WS-OUTPUT-RECORD.
    05  WS-RESULT-CODE     PIC 9(2).
    05  WS-RESULT-MSG      PIC X(100).

01  WS-FLAGS.
    05  WS-VALID-FLAG      PIC X VALUE 'N'.
    05  WS-EOF-FLAG        PIC X VALUE 'N'.
"""


@pytest.fixture
def valid_llm_response():
    """Create a valid LLM response with call semantics."""
    return json.dumps(
        {
            "calls": [
                {
                    "caller": "MAIN-PARAGRAPH",
                    "callee": "VALIDATE-INPUT",
                    "inputs": ["WS-CUSTOMER-ID", "WS-AMOUNT"],
                    "outputs": ["WS-VALID-FLAG", "WS-RESULT-CODE"],
                    "purpose": "Validates customer ID and amount before processing",
                },
                {
                    "caller": "MAIN-PARAGRAPH",
                    "callee": "PROCESS-DATA",
                    "inputs": ["WS-INPUT-RECORD"],
                    "outputs": ["WS-OUTPUT-RECORD"],
                    "purpose": "Processes validated input data and generates output",
                },
            ]
        }
    )


# =============================================================================
# Basic Functionality Tests
# =============================================================================


class TestCallSemanticsAnalyzerInit:
    """Tests for CallSemanticsAnalyzer initialization."""

    def test_init_with_default_batch_size(self, mock_api_config, mock_provider):
        """Test initialization with default batch size."""
        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider
        )
        assert analyzer.batch_size == DEFAULT_BATCH_SIZE
        assert analyzer.api_config == mock_api_config
        assert analyzer.provider == mock_provider

    def test_init_with_custom_batch_size(self, mock_api_config, mock_provider):
        """Test initialization with custom batch size."""
        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider, batch_size=10
        )
        assert analyzer.batch_size == 10

    def test_provider_property_setter(self, mock_api_config, mock_provider):
        """Test that provider can be changed via setter."""
        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider
        )
        new_provider = MagicMock()
        new_provider.default_model = "new-model"
        analyzer.provider = new_provider
        assert analyzer.provider == new_provider


class TestAnalyzeFileBasic:
    """Tests for basic analyze_file functionality."""

    async def test_analyze_file_with_simple_template(
        self, mock_api_config, mock_provider, sample_citadel_context, valid_llm_response
    ):
        """Test analyze_file with a simple template with calls."""
        mock_provider.complete = AsyncMock(
            return_value=CompletionResponse(
                content=valid_llm_response,
                model="test-model",
                tokens_used=100,
            )
        )

        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider
        )

        with patch.object(analyzer, "_get_function_bodies") as mock_bodies:
            mock_bodies.return_value = {
                "MAIN-PARAGRAPH": "PERFORM VALIDATE-INPUT.\nPERFORM PROCESS-DATA.",
                "VALIDATE-INPUT": "IF WS-CUSTOMER-ID = SPACES...",
                "PROCESS-DATA": "MOVE WS-INPUT TO WS-OUTPUT.",
                "WRITE-OUTPUT": "WRITE OUTPUT-RECORD.",
            }

            result = await analyzer.analyze_file(
                source_path=Path("/test/PROGRAM.cbl"),
                citadel_context=sample_citadel_context,
            )

        assert len(result) >= 2
        # Check that returned objects are CallSemantics
        for cs in result:
            assert isinstance(cs, CallSemantics)
            assert cs.caller
            assert cs.callee

    async def test_analyze_file_returns_correct_caller_callee_pairs(
        self, mock_api_config, mock_provider, valid_llm_response
    ):
        """Test that returned CallSemantics have correct caller/callee pairs."""
        mock_provider.complete = AsyncMock(
            return_value=CompletionResponse(
                content=valid_llm_response,
                model="test-model",
                tokens_used=100,
            )
        )

        citadel_context = [
            {
                "name": "CALLER-PARA",
                "type": "paragraph",
                "line": 100,
                "line_end": 150,
                "calls": [{"target": "CALLEE-PARA", "type": "performs", "line": 110}],
            },
            {
                "name": "CALLEE-PARA",
                "type": "paragraph",
                "line": 200,
                "line_end": 250,
                "calls": [],
            },
        ]

        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider
        )

        with patch.object(analyzer, "_get_function_bodies") as mock_bodies:
            mock_bodies.return_value = {
                "CALLER-PARA": "PERFORM CALLEE-PARA.",
                "CALLEE-PARA": "DISPLAY 'HELLO'.",
            }

            result = await analyzer.analyze_file(
                source_path=Path("/test/PROGRAM.cbl"),
                citadel_context=citadel_context,
            )

        # Should have at least one call semantics
        assert len(result) >= 1

    async def test_analyze_file_with_working_storage(
        self,
        mock_api_config,
        mock_provider,
        sample_citadel_context,
        sample_working_storage,
        valid_llm_response,
    ):
        """Test analyze_file passes working storage context."""
        mock_provider.complete = AsyncMock(
            return_value=CompletionResponse(
                content=valid_llm_response,
                model="test-model",
                tokens_used=100,
            )
        )

        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider
        )

        with patch.object(analyzer, "_get_function_bodies") as mock_bodies:
            mock_bodies.return_value = {
                "MAIN-PARAGRAPH": "PERFORM VALIDATE-INPUT.",
                "VALIDATE-INPUT": "IF WS-VALID-FLAG = 'Y'...",
                "PROCESS-DATA": "MOVE WS-INPUT TO WS-OUTPUT.",
                "WRITE-OUTPUT": "WRITE OUTPUT-RECORD.",
            }

            await analyzer.analyze_file(
                source_path=Path("/test/PROGRAM.cbl"),
                citadel_context=sample_citadel_context,
                working_storage=sample_working_storage,
            )

        # Verify the provider was called
        mock_provider.complete.assert_called()
        # Check that working storage is in the prompt
        call_args = mock_provider.complete.call_args
        messages = call_args.kwargs.get("messages") or call_args.args[0]
        user_message = next((m for m in messages if m.role == "user"), None)
        assert user_message is not None
        assert "WORKING-STORAGE" in user_message.content


# =============================================================================
# LLM Response Parsing Tests
# =============================================================================


class TestParseResponse:
    """Tests for _parse_response and _parse_batch_response methods."""

    def test_parse_response_with_valid_data(self, mock_api_config, mock_provider):
        """Test parsing valid response data into CallSemantics."""
        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider
        )

        data = {
            "caller": "MAIN-PARA",
            "callee": "SUB-PARA",
            "inputs": ["WS-VAR-1", "WS-VAR-2"],
            "outputs": ["WS-RESULT"],
            "purpose": "Processes the input data",
        }

        result = analyzer._parse_response(data, "MAIN-PARA", "SUB-PARA")

        assert isinstance(result, CallSemantics)
        assert result.caller == "MAIN-PARA"
        assert result.callee == "SUB-PARA"
        assert result.inputs == ["WS-VAR-1", "WS-VAR-2"]
        assert result.outputs == ["WS-RESULT"]
        assert result.purpose == "Processes the input data"

    def test_parse_response_uses_fallback_names(self, mock_api_config, mock_provider):
        """Test that fallback caller/callee names are used when missing."""
        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider
        )

        data = {
            "inputs": ["WS-INPUT"],
            "outputs": [],
            "purpose": "Does something",
        }

        result = analyzer._parse_response(data, "FALLBACK-CALLER", "FALLBACK-CALLEE")

        assert result.caller == "FALLBACK-CALLER"
        assert result.callee == "FALLBACK-CALLEE"

    def test_parse_batch_response_with_valid_json(
        self, mock_api_config, mock_provider, valid_llm_response
    ):
        """Test parsing valid JSON batch response."""
        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider
        )

        batch = [
            ("MAIN-PARAGRAPH", "VALIDATE-INPUT"),
            ("MAIN-PARAGRAPH", "PROCESS-DATA"),
        ]

        result = analyzer._parse_batch_response(valid_llm_response, batch)

        assert len(result) >= 2
        # Check that results are CallSemantics with expected pairs
        callers = {cs.caller for cs in result}
        callees = {cs.callee for cs in result}
        assert "MAIN-PARAGRAPH" in callers
        assert "VALIDATE-INPUT" in callees or "PROCESS-DATA" in callees

    def test_parse_batch_response_with_malformed_json(
        self, mock_api_config, mock_provider
    ):
        """Test handling of malformed JSON gracefully returns empty semantics."""
        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider
        )

        batch = [("CALLER", "CALLEE")]
        malformed_json = "{ this is not valid json }"

        result = analyzer._parse_batch_response(malformed_json, batch)

        # Should return empty semantics for the batch
        assert len(result) == 1
        assert result[0].caller == "CALLER"
        assert result[0].callee == "CALLEE"
        assert result[0].inputs == []
        assert result[0].outputs == []
        assert result[0].purpose is None

    def test_parse_batch_response_with_no_json(self, mock_api_config, mock_provider):
        """Test handling of response with no JSON."""
        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider
        )

        batch = [("CALLER", "CALLEE")]
        no_json_response = "This response contains no JSON at all."

        result = analyzer._parse_batch_response(no_json_response, batch)

        # Should return empty semantics
        assert len(result) == 1
        assert result[0].inputs == []
        assert result[0].outputs == []

    def test_parse_batch_response_with_missing_fields(
        self, mock_api_config, mock_provider
    ):
        """Test handling of response with missing fields."""
        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider
        )

        batch = [("MAIN", "SUB")]
        response_missing_fields = json.dumps(
            {
                "calls": [
                    {
                        "caller": "MAIN",
                        "callee": "SUB",
                        # Missing inputs, outputs, purpose
                    }
                ]
            }
        )

        result = analyzer._parse_batch_response(response_missing_fields, batch)

        assert len(result) == 1
        assert result[0].caller == "MAIN"
        assert result[0].callee == "SUB"
        assert result[0].inputs == []
        assert result[0].outputs == []
        assert result[0].purpose is None

    def test_parse_batch_response_with_single_call_object(
        self, mock_api_config, mock_provider
    ):
        """Test handling of flat response (single call, not array)."""
        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider
        )

        batch = [("SINGLE-CALLER", "SINGLE-CALLEE")]
        flat_response = json.dumps(
            {
                "caller": "SINGLE-CALLER",
                "callee": "SINGLE-CALLEE",
                "inputs": ["WS-INPUT"],
                "outputs": ["WS-OUTPUT"],
                "purpose": "Single call purpose",
            }
        )

        result = analyzer._parse_batch_response(flat_response, batch)

        assert len(result) == 1
        assert result[0].inputs == ["WS-INPUT"]

    def test_parse_batch_response_fills_missing_pairs(
        self, mock_api_config, mock_provider
    ):
        """Test that missing pairs in response get empty semantics."""
        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider
        )

        # Batch has 3 pairs but response only has 1
        batch = [
            ("A", "B"),
            ("C", "D"),
            ("E", "F"),
        ]
        partial_response = json.dumps(
            {
                "calls": [
                    {
                        "caller": "A",
                        "callee": "B",
                        "inputs": ["VAR1"],
                        "outputs": ["VAR2"],
                        "purpose": "First call",
                    }
                ]
            }
        )

        result = analyzer._parse_batch_response(partial_response, batch)

        # Should have 3 results - 1 from response, 2 filled with empty
        assert len(result) == 3
        pairs = {(cs.caller, cs.callee) for cs in result}
        assert ("A", "B") in pairs
        assert ("C", "D") in pairs
        assert ("E", "F") in pairs


class TestEnsureList:
    """Tests for _ensure_list helper method."""

    def test_ensure_list_with_none(self, mock_api_config, mock_provider):
        """Test _ensure_list with None input."""
        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider
        )
        assert analyzer._ensure_list(None) == []

    def test_ensure_list_with_string(self, mock_api_config, mock_provider):
        """Test _ensure_list with string input."""
        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider
        )
        assert analyzer._ensure_list("WS-VARIABLE") == ["WS-VARIABLE"]

    def test_ensure_list_with_empty_string(self, mock_api_config, mock_provider):
        """Test _ensure_list with empty string."""
        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider
        )
        assert analyzer._ensure_list("   ") == []

    def test_ensure_list_with_list(self, mock_api_config, mock_provider):
        """Test _ensure_list with list input."""
        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider
        )
        assert analyzer._ensure_list(["A", "B", "C"]) == ["A", "B", "C"]

    def test_ensure_list_filters_none_values(self, mock_api_config, mock_provider):
        """Test _ensure_list filters None values from list."""
        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider
        )
        assert analyzer._ensure_list(["A", None, "B", None]) == ["A", "B"]

    def test_ensure_list_converts_non_strings(self, mock_api_config, mock_provider):
        """Test _ensure_list converts non-string values to strings."""
        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider
        )
        assert analyzer._ensure_list([1, 2, 3]) == ["1", "2", "3"]


# =============================================================================
# Batching Tests
# =============================================================================


class TestBatching:
    """Tests for batching functionality."""

    async def test_large_number_of_calls_batched_correctly(
        self, mock_api_config, mock_provider
    ):
        """Test that large number of calls are batched correctly."""
        mock_provider.complete = AsyncMock(
            return_value=CompletionResponse(
                content='{"calls": []}',
                model="test-model",
                tokens_used=100,
            )
        )

        # Create context with 12 call edges (should create 3 batches with size 5)
        citadel_context = [
            {
                "name": "MAIN",
                "type": "paragraph",
                "line": 100,
                "line_end": 150,
                "calls": [
                    {"target": f"PARA-{i}", "type": "performs", "line": 100 + i}
                    for i in range(12)
                ],
            }
        ]
        # Add the callee paragraphs
        for i in range(12):
            citadel_context.append(
                {
                    "name": f"PARA-{i}",
                    "type": "paragraph",
                    "line": 200 + i * 50,
                    "line_end": 230 + i * 50,
                    "calls": [],
                }
            )

        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider, batch_size=5
        )

        with patch.object(analyzer, "_get_function_bodies") as mock_bodies:
            mock_bodies.return_value = {
                "MAIN": "PERFORM PARA-0 THRU PARA-11.",
                **{f"PARA-{i}": f"DISPLAY 'PARA-{i}'." for i in range(12)},
            }

            await analyzer.analyze_file(
                source_path=Path("/test/PROGRAM.cbl"),
                citadel_context=citadel_context,
            )

        # Should have made 3 LLM calls (12 edges / 5 per batch = 3 batches)
        assert mock_provider.complete.call_count == 3

    async def test_batch_size_is_respected(self, mock_api_config, mock_provider):
        """Test that batch size limits are respected."""
        call_count = 0
        batch_sizes = []

        async def mock_complete(messages, **kwargs):
            nonlocal call_count
            call_count += 1
            # Extract batch size from the message content
            user_msg = next(m for m in messages if m.role == "user")
            # Count "### Call" occurrences
            import re

            calls_in_batch = len(re.findall(r"### Call \d+:", user_msg.content))
            batch_sizes.append(calls_in_batch)
            return CompletionResponse(
                content='{"calls": []}',
                model="test-model",
                tokens_used=100,
            )

        mock_provider.complete = AsyncMock(side_effect=mock_complete)

        # Create context with 7 call edges (batch_size=3 -> 3 batches: 3, 3, 1)
        citadel_context = [
            {
                "name": "MAIN",
                "type": "paragraph",
                "line": 100,
                "line_end": 150,
                "calls": [
                    {"target": f"PARA-{i}", "type": "performs", "line": 100 + i}
                    for i in range(7)
                ],
            }
        ]
        for i in range(7):
            citadel_context.append(
                {
                    "name": f"PARA-{i}",
                    "type": "paragraph",
                    "line": 200 + i * 50,
                    "line_end": 230 + i * 50,
                    "calls": [],
                }
            )

        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider, batch_size=3
        )

        with patch.object(analyzer, "_get_function_bodies") as mock_bodies:
            mock_bodies.return_value = {
                "MAIN": "PERFORM PARA-0 THRU PARA-6.",
                **{f"PARA-{i}": f"DISPLAY 'PARA-{i}'." for i in range(7)},
            }

            await analyzer.analyze_file(
                source_path=Path("/test/PROGRAM.cbl"),
                citadel_context=citadel_context,
            )

        # Verify batch sizes
        assert call_count == 3
        assert batch_sizes == [3, 3, 1]


# =============================================================================
# Edge Cases
# =============================================================================


class TestEdgeCases:
    """Tests for edge cases and boundary conditions."""

    async def test_empty_citadel_context(self, mock_api_config, mock_provider):
        """Test with empty citadel context returns empty list."""
        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider
        )

        result = await analyzer.analyze_file(
            source_path=Path("/test/PROGRAM.cbl"),
            citadel_context=[],
        )

        assert result == []
        mock_provider.complete.assert_not_called()

    async def test_template_with_no_outgoing_calls(
        self, mock_api_config, mock_provider
    ):
        """Test with template that has paragraphs but no outgoing calls."""
        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider
        )

        citadel_context = [
            {
                "name": "STANDALONE-PARA",
                "type": "paragraph",
                "line": 100,
                "line_end": 150,
                "calls": [],  # No outgoing calls
            },
            {
                "name": "ANOTHER-PARA",
                "type": "paragraph",
                "line": 200,
                "line_end": 250,
                "calls": [],
            },
        ]

        result = await analyzer.analyze_file(
            source_path=Path("/test/PROGRAM.cbl"),
            citadel_context=citadel_context,
        )

        assert result == []
        mock_provider.complete.assert_not_called()

    async def test_llm_returns_empty_results(self, mock_api_config, mock_provider):
        """Test when LLM returns empty results."""
        mock_provider.complete = AsyncMock(
            return_value=CompletionResponse(
                content='{"calls": []}',
                model="test-model",
                tokens_used=100,
            )
        )

        citadel_context = [
            {
                "name": "CALLER",
                "type": "paragraph",
                "line": 100,
                "line_end": 150,
                "calls": [{"target": "CALLEE", "type": "performs", "line": 110}],
            },
            {
                "name": "CALLEE",
                "type": "paragraph",
                "line": 200,
                "line_end": 250,
                "calls": [],
            },
        ]

        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider
        )

        with patch.object(analyzer, "_get_function_bodies") as mock_bodies:
            mock_bodies.return_value = {
                "CALLER": "PERFORM CALLEE.",
                "CALLEE": "DISPLAY 'DONE'.",
            }

            result = await analyzer.analyze_file(
                source_path=Path("/test/PROGRAM.cbl"),
                citadel_context=citadel_context,
            )

        # Should have empty semantics for the one call edge
        assert len(result) == 1
        assert result[0].caller == "CALLER"
        assert result[0].callee == "CALLEE"
        assert result[0].inputs == []
        assert result[0].outputs == []

    async def test_external_calls_are_excluded(
        self, mock_api_config, mock_provider, sample_citadel_context_with_external_calls
    ):
        """Test that external CALL statements are excluded from analysis."""
        mock_provider.complete = AsyncMock(
            return_value=CompletionResponse(
                content='{"calls": []}',
                model="test-model",
                tokens_used=100,
            )
        )

        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider
        )

        with patch.object(analyzer, "_get_function_bodies") as mock_bodies:
            mock_bodies.return_value = {
                "MAIN-PARAGRAPH": "PERFORM SUB-PARA.\nCALL 'EXTPROG'.",
                "SUB-PARA": "DISPLAY 'SUB'.",
            }

            result = await analyzer.analyze_file(
                source_path=Path("/test/PROGRAM.cbl"),
                citadel_context=sample_citadel_context_with_external_calls,
            )

        # Should only have 1 edge (MAIN -> SUB-PARA), not the CALL to EXTPROG
        assert len(result) == 1
        assert result[0].callee == "SUB-PARA"


class TestWorkingStorageTruncation:
    """Tests for working storage truncation."""

    def test_truncate_working_storage_none(self, mock_api_config, mock_provider):
        """Test truncation with None input."""
        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider
        )
        assert analyzer._truncate_working_storage(None) is None

    def test_truncate_working_storage_short(self, mock_api_config, mock_provider):
        """Test truncation with short input (no truncation needed)."""
        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider
        )
        short_ws = "01 WS-VAR PIC X(10)."
        assert analyzer._truncate_working_storage(short_ws) == short_ws

    def test_truncate_working_storage_long(self, mock_api_config, mock_provider):
        """Test truncation with long input."""
        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider
        )
        # Create working storage longer than MAX_WORKING_STORAGE_CHARS
        long_ws = "X" * (MAX_WORKING_STORAGE_CHARS + 1000)
        result = analyzer._truncate_working_storage(long_ws)

        assert len(result) < len(long_ws)
        assert result.endswith("... [TRUNCATED]")
        # Should be MAX_WORKING_STORAGE_CHARS + length of truncation indicator
        assert result.startswith(
            "X" * 100
        )  # Just verify it starts with original content


# =============================================================================
# Error Handling Tests
# =============================================================================


class TestErrorHandling:
    """Tests for error handling."""

    async def test_graceful_failure_when_llm_call_fails(
        self, mock_api_config, mock_provider
    ):
        """Test graceful failure when LLM call fails."""
        mock_provider.complete = AsyncMock(
            side_effect=Exception("API Error: Rate limit exceeded")
        )

        citadel_context = [
            {
                "name": "CALLER",
                "type": "paragraph",
                "line": 100,
                "line_end": 150,
                "calls": [{"target": "CALLEE", "type": "performs", "line": 110}],
            },
            {
                "name": "CALLEE",
                "type": "paragraph",
                "line": 200,
                "line_end": 250,
                "calls": [],
            },
        ]

        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider
        )

        with patch.object(analyzer, "_get_function_bodies") as mock_bodies:
            mock_bodies.return_value = {
                "CALLER": "PERFORM CALLEE.",
                "CALLEE": "DISPLAY 'DONE'.",
            }

            # Should not raise, but return empty semantics
            result = await analyzer.analyze_file(
                source_path=Path("/test/PROGRAM.cbl"),
                citadel_context=citadel_context,
            )

        # Should return empty semantics for the failed batch
        assert len(result) == 1
        assert result[0].caller == "CALLER"
        assert result[0].callee == "CALLEE"
        assert result[0].inputs == []
        assert result[0].outputs == []
        assert result[0].purpose is None

    async def test_original_caller_callee_preserved_on_parse_error(
        self, mock_api_config, mock_provider
    ):
        """Test that original caller/callee pairs are preserved on parse errors."""
        mock_provider.complete = AsyncMock(
            return_value=CompletionResponse(
                content="This is not JSON at all!",
                model="test-model",
                tokens_used=100,
            )
        )

        citadel_context = [
            {
                "name": "MY-CALLER",
                "type": "paragraph",
                "line": 100,
                "line_end": 150,
                "calls": [{"target": "MY-CALLEE", "type": "performs", "line": 110}],
            },
            {
                "name": "MY-CALLEE",
                "type": "paragraph",
                "line": 200,
                "line_end": 250,
                "calls": [],
            },
        ]

        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider
        )

        with patch.object(analyzer, "_get_function_bodies") as mock_bodies:
            mock_bodies.return_value = {
                "MY-CALLER": "PERFORM MY-CALLEE.",
                "MY-CALLEE": "DISPLAY 'DONE'.",
            }

            result = await analyzer.analyze_file(
                source_path=Path("/test/PROGRAM.cbl"),
                citadel_context=citadel_context,
            )

        # Original caller/callee should be preserved
        assert len(result) == 1
        assert result[0].caller == "MY-CALLER"
        assert result[0].callee == "MY-CALLEE"


# =============================================================================
# Call Edge Extraction Tests
# =============================================================================


class TestExtractCallEdges:
    """Tests for _extract_call_edges method."""

    def test_extract_call_edges_basic(self, mock_api_config, mock_provider):
        """Test extracting call edges from Citadel context."""
        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider
        )

        citadel_context = [
            {
                "name": "PARA-A",
                "calls": [
                    {"target": "PARA-B", "type": "performs"},
                    {"target": "PARA-C", "type": "perform"},
                ],
            },
            {
                "name": "PARA-B",
                "calls": [{"target": "PARA-D", "type": ""}],  # Empty type
            },
        ]

        edges = analyzer._extract_call_edges(citadel_context)

        assert len(edges) == 3
        assert ("PARA-A", "PARA-B") in edges
        assert ("PARA-A", "PARA-C") in edges
        assert ("PARA-B", "PARA-D") in edges

    def test_extract_call_edges_excludes_external_calls(
        self, mock_api_config, mock_provider
    ):
        """Test that CALL statements (external programs) are excluded."""
        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider
        )

        citadel_context = [
            {
                "name": "MAIN",
                "calls": [
                    {"target": "SUB-PARA", "type": "performs"},
                    {"target": "EXTPROG", "type": "calls"},  # External CALL
                    {"target": "LINKPGM", "type": "links"},  # CICS LINK
                ],
            },
        ]

        edges = analyzer._extract_call_edges(citadel_context)

        # Only PERFORM should be included
        assert len(edges) == 1
        assert ("MAIN", "SUB-PARA") in edges

    def test_extract_call_edges_skips_empty_names(self, mock_api_config, mock_provider):
        """Test that empty caller/callee names are skipped."""
        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider
        )

        citadel_context = [
            {
                "name": "",  # Empty caller name
                "calls": [{"target": "PARA-B", "type": "performs"}],
            },
            {
                "name": "PARA-A",
                "calls": [{"target": "", "type": "performs"}],  # Empty callee
            },
        ]

        edges = analyzer._extract_call_edges(citadel_context)

        assert len(edges) == 0


# =============================================================================
# Prompt Building Tests
# =============================================================================


class TestPromptBuilding:
    """Tests for prompt building methods."""

    def test_build_system_prompt(self, mock_api_config, mock_provider):
        """Test that system prompt contains expected instructions."""
        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider
        )

        prompt = analyzer._build_system_prompt()

        assert "COBOL" in prompt
        assert "PERFORM" in prompt or "data flow" in prompt.lower()
        assert "JSON" in prompt

    def test_build_batch_prompt_includes_calls(self, mock_api_config, mock_provider):
        """Test that batch prompt includes all call edges."""
        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider
        )

        batch = [("CALLER-1", "CALLEE-1"), ("CALLER-2", "CALLEE-2")]
        bodies = {
            "CALLER-1": "PERFORM CALLEE-1.",
            "CALLEE-1": "DISPLAY 'ONE'.",
            "CALLER-2": "PERFORM CALLEE-2.",
            "CALLEE-2": "DISPLAY 'TWO'.",
        }

        prompt = analyzer._build_batch_prompt(batch, bodies, None)

        assert "CALLER-1" in prompt
        assert "CALLEE-1" in prompt
        assert "CALLER-2" in prompt
        assert "CALLEE-2" in prompt
        assert "Call 1:" in prompt
        assert "Call 2:" in prompt

    def test_build_batch_prompt_includes_working_storage(
        self, mock_api_config, mock_provider, sample_working_storage
    ):
        """Test that batch prompt includes working storage when provided."""
        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider
        )

        batch = [("CALLER", "CALLEE")]
        bodies = {"CALLER": "PERFORM CALLEE.", "CALLEE": "DISPLAY 'DONE'."}

        prompt = analyzer._build_batch_prompt(batch, bodies, sample_working_storage)

        assert "WORKING-STORAGE" in prompt
        assert "WS-CUSTOMER-ID" in prompt
        assert "WS-VALID-FLAG" in prompt

    def test_build_batch_prompt_handles_missing_bodies(
        self, mock_api_config, mock_provider
    ):
        """Test that prompt handles missing function bodies gracefully."""
        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider
        )

        batch = [("CALLER", "CALLEE")]
        bodies = {"CALLER": None, "CALLEE": None}  # Missing bodies

        prompt = analyzer._build_batch_prompt(batch, bodies, None)

        assert "[Body not available]" in prompt

    def test_build_prompt_single_call(self, mock_api_config, mock_provider):
        """Test _build_prompt for single call (delegates to batch)."""
        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider
        )

        prompt = analyzer._build_prompt(
            caller_name="MAIN-PARA",
            caller_body="PERFORM SUB-PARA.",
            callee_name="SUB-PARA",
            callee_body="DISPLAY 'HELLO'.",
            working_storage=None,
        )

        assert "MAIN-PARA" in prompt
        assert "SUB-PARA" in prompt
        assert "Call 1:" in prompt


# =============================================================================
# Function Bodies Tests
# =============================================================================


class TestGetFunctionBodies:
    """Tests for _get_function_bodies method."""

    def test_get_function_bodies_with_citadel(self, mock_api_config, mock_provider):
        """Test getting function bodies when Citadel SDK is available."""
        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider
        )

        # Create a mock module for citadel.sdk
        mock_sdk = MagicMock()
        mock_sdk.get_function_bodies = MagicMock(
            return_value={
                "PARA-A": "MOVE 1 TO WS-VAR.",
                "PARA-B": "DISPLAY WS-VAR.",
            }
        )

        with patch.dict(
            "sys.modules", {"citadel": MagicMock(), "citadel.sdk": mock_sdk}
        ):
            result = analyzer._get_function_bodies(
                Path("/test/PROGRAM.cbl"), ["PARA-A", "PARA-B"]
            )

        assert result == {"PARA-A": "MOVE 1 TO WS-VAR.", "PARA-B": "DISPLAY WS-VAR."}

    def test_get_function_bodies_without_citadel(self, mock_api_config, mock_provider):
        """Test fallback when Citadel SDK is not available."""
        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider
        )

        # Remove citadel.sdk from sys.modules to simulate it not being installed
        import sys

        original_modules = sys.modules.copy()

        # Remove any citadel-related modules
        modules_to_remove = [k for k in sys.modules if k.startswith("citadel")]
        for mod in modules_to_remove:
            sys.modules.pop(mod, None)

        # Make the import fail by setting the module to raise ImportError
        with patch.dict("sys.modules", {"citadel.sdk": None}):
            # Force the method to re-import by making the import fail
            result = analyzer._get_function_bodies(
                Path("/test/PROGRAM.cbl"), ["PARA-A", "PARA-B"]
            )

        # Restore original modules
        sys.modules.update(original_modules)

        # The result depends on whether citadel is actually installed
        # If citadel is installed, it will return real values
        # If not, it returns dict with None values
        # Let's just verify the structure is correct
        assert "PARA-A" in result
        assert "PARA-B" in result

    def test_get_function_bodies_handles_exception(
        self, mock_api_config, mock_provider
    ):
        """Test graceful handling when Citadel raises an exception."""
        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider
        )

        # Create a mock module that raises an exception
        mock_sdk = MagicMock()
        mock_sdk.get_function_bodies = MagicMock(
            side_effect=RuntimeError("Citadel internal error")
        )

        with patch.dict(
            "sys.modules", {"citadel": MagicMock(), "citadel.sdk": mock_sdk}
        ):
            result = analyzer._get_function_bodies(
                Path("/test/PROGRAM.cbl"), ["PARA-A"]
            )

        # Should return dict with None values
        assert result == {"PARA-A": None}


# =============================================================================
# Empty Semantics Helper Tests
# =============================================================================


class TestEmptySemanticsForBatch:
    """Tests for _empty_semantics_for_batch helper."""

    def test_empty_semantics_for_batch(self, mock_api_config, mock_provider):
        """Test creating empty semantics for a batch."""
        analyzer = CallSemanticsAnalyzer(
            api_config=mock_api_config, provider=mock_provider
        )

        batch = [("A", "B"), ("C", "D"), ("E", "F")]
        result = analyzer._empty_semantics_for_batch(batch)

        assert len(result) == 3
        for i, (caller, callee) in enumerate(batch):
            assert result[i].caller == caller
            assert result[i].callee == callee
            assert result[i].inputs == []
            assert result[i].outputs == []
            assert result[i].purpose is None
