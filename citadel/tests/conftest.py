"""
Pytest configuration and fixtures for Citadel tests.
"""

from __future__ import annotations

from pathlib import Path
from typing import Generator

import pytest

from citadel.config import CitadelConfig
from citadel.sdk import Citadel


@pytest.fixture
def project_root() -> Path:
    """Return the project root directory."""
    return Path(__file__).parent.parent


@pytest.fixture
def fixtures_dir(project_root: Path) -> Path:
    """Return the fixtures directory."""
    return project_root / "tests" / "fixtures"


@pytest.fixture
def sample_source_dir(fixtures_dir: Path) -> Path:
    """Return the sample source directory for testing."""
    return fixtures_dir / "samples"


@pytest.fixture
def temp_cache_dir(tmp_path: Path) -> Path:
    """Return a temporary cache directory for testing."""
    cache_dir = tmp_path / "cache"
    cache_dir.mkdir(parents=True)
    return cache_dir


@pytest.fixture
def test_config(temp_cache_dir: Path) -> CitadelConfig:
    """Return a test configuration with temporary directories."""
    return CitadelConfig(
        cache_dir=temp_cache_dir,
        parallel_files=2,
        llm_disambiguation=False,
        max_llm_calls=0,
        log_level="DEBUG",
    )


@pytest.fixture
def clean_env() -> Generator[None, None, None]:
    """
    Fixture that ensures clean environment variables for testing.

    Removes any CITADEL_ prefixed environment variables during the test.
    """
    import os

    # Store original values
    original_env = {}
    citadel_vars = [key for key in os.environ if key.startswith("CITADEL_")]
    for var in citadel_vars:
        original_env[var] = os.environ.pop(var)

    yield

    # Restore original values
    for var, value in original_env.items():
        os.environ[var] = value


@pytest.fixture
def citadel_instance(test_config: CitadelConfig) -> Citadel:
    """Return a Citadel SDK instance for testing."""
    return Citadel(config=test_config)
