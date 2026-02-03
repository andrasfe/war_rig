"""Pytest fixtures for codewhisper tests.

This module provides shared fixtures for testing the codewhisper package.
"""

from pathlib import Path
from typing import Any
from unittest.mock import AsyncMock, MagicMock

import pytest

from codewhisper.config import AgentConfig, SearchResult, SkillMetadata


@pytest.fixture
def tmp_skills_dir(tmp_path: Path) -> Path:
    """Create a temporary skills directory with sample skills."""
    skills_dir = tmp_path / "skills"
    skills_dir.mkdir()

    # Create a sample skill file
    program_skill = skills_dir / "programs"
    program_skill.mkdir()
    program_subdir = program_skill / "test-program"
    program_subdir.mkdir()

    skill_file = program_subdir / "SKILL.md"
    skill_file.write_text(
        """---
name: test-program
description: A test program skill for unit testing
tags: test, batch, cobol
---

# Test Program

This is a test skill for unit testing purposes.

## Purpose

The test program processes batch records and performs validation.

## Inputs

- INPUT-FILE: Sequential input file

## Outputs

- OUTPUT-FILE: Processed output file
"""
    )

    # Create another skill
    overview_dir = skills_dir / "system-overview"
    overview_dir.mkdir()
    overview_file = overview_dir / "SKILL.md"
    overview_file.write_text(
        """---
name: system-overview
description: High-level overview of the test system
---

# System Overview

This skill provides an overview of the system architecture.

## Components

- Batch Processing
- Online Transaction Processing
- Database Access
"""
    )

    return skills_dir


@pytest.fixture
def tmp_code_dir(tmp_path: Path) -> Path:
    """Create a temporary code directory with sample source files."""
    code_dir = tmp_path / "src"
    code_dir.mkdir()

    # Create a COBOL file
    cobol_dir = code_dir / "cbl"
    cobol_dir.mkdir()

    cobol_file = cobol_dir / "TESTPROG.cbl"
    cobol_file.write_text(
        """       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       AUTHOR. TEST.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-COUNTER          PIC 9(5) VALUE 0.
       01  WS-TOTAL            PIC 9(9)V99 VALUE 0.
      *
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS
           PERFORM 3000-FINALIZE
           STOP RUN.
      *
       1000-INITIALIZE.
           DISPLAY 'STARTING TESTPROG'.
      *
       2000-PROCESS.
           ADD 1 TO WS-COUNTER
           COMPUTE WS-TOTAL = WS-TOTAL + 100.50.
      *
       3000-FINALIZE.
           DISPLAY 'PROCESSED: ' WS-COUNTER ' RECORDS'.
"""
    )

    # Create a JCL file
    jcl_dir = code_dir / "jcl"
    jcl_dir.mkdir()

    jcl_file = jcl_dir / "TESTJOB.jcl"
    jcl_file.write_text(
        """//TESTJOB  JOB (ACCT),'TEST JOB',CLASS=A
//*
//STEP01   EXEC PGM=TESTPROG
//INFILE   DD DSN=TEST.INPUT,DISP=SHR
//OUTFILE  DD DSN=TEST.OUTPUT,DISP=(NEW,CATLG)
//
"""
    )

    return code_dir


@pytest.fixture
def agent_config(tmp_skills_dir: Path, tmp_code_dir: Path) -> AgentConfig:
    """Create an agent config for testing."""
    return AgentConfig(
        skills_dir=tmp_skills_dir,
        code_dir=tmp_code_dir,
        model="test-model",
        provider="openrouter",
        temperature=0.1,
        max_history=10,
        verbose=False,
    )


@pytest.fixture
def sample_skill_metadata() -> SkillMetadata:
    """Sample skill metadata for testing."""
    return SkillMetadata(
        name="test-skill",
        description="A test skill for unit testing",
        file_path=Path("/tmp/skills/test-skill/SKILL.md"),
        content="# Test Skill\n\nThis is a test skill.",
    )


@pytest.fixture
def sample_search_result() -> SearchResult:
    """Sample search result for testing."""
    return SearchResult(
        file_path=Path("cbl/TESTPROG.cbl"),
        line_number=15,
        line_content="           PERFORM 2000-PROCESS",
        context_before=["       0000-MAIN.", "           PERFORM 1000-INITIALIZE"],
        context_after=["           PERFORM 3000-FINALIZE", "           STOP RUN."],
    )


@pytest.fixture
def mock_llm_provider() -> MagicMock:
    """Mock LLM provider for testing."""
    from llm_providers.protocol import CompletionResponse

    mock = MagicMock()
    mock.default_model = "test-model"

    async def mock_complete(*args: Any, **kwargs: Any) -> CompletionResponse:
        return CompletionResponse(
            content="This is a mocked LLM response.",
            model="test-model",
            tokens_used=50,
        )

    mock.complete = AsyncMock(side_effect=mock_complete)
    return mock
