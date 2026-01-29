"""Unit tests for CallGraphSkillGenerator class.

Tests:
- Call graph skill generation
- SKILL.md content formatting
- Edge cases
"""

from pathlib import Path

import pytest

from war_rig.skills import (
    CALL_GRAPH_SKILL_NAME,
    CallGraphSkillGenerationError,
    CallGraphSkillGenerator,
)


@pytest.fixture
def sample_call_graph_content() -> str:
    """Create sample call graph markdown content."""
    return """# Call Graph Analysis

*Generated: 2026-01-28 07:49:48*

**Programs Analyzed:** 24

## Visual Call Graph

```mermaid
flowchart TD
    subgraph jobs[" "]
        CBPAUP0J([CBPAUP0J])
        DBPAUTP0([DBPAUTP0])
    end

    subgraph procs[" "]
        CBPAUP0C[CBPAUP0C]
        COPAUA0C[COPAUA0C]
    end

    CBPAUP0C --> COPAUA0C
```

## Entry Points

- **CBPAUP0J**: Job for purging authorizations
- **DBPAUTP0**: Database processing job

## External Dependencies

### Custom Programs (Need Documentation)

- **CBLTDLI**: Called by PAUDBLOD (EXEC)

## Statistics

| Metric | Count |
|--------|-------|
| Documented Programs | 24 |
| Entry Points | 6 |
| Total Calls | 166 |
"""


@pytest.fixture
def call_graph_md_file(tmp_path: Path, sample_call_graph_content: str) -> Path:
    """Create a sample CALL_GRAPH.md file."""
    call_graph_path = tmp_path / "CALL_GRAPH.md"
    call_graph_path.write_text(sample_call_graph_content, encoding="utf-8")
    return call_graph_path


class TestCallGraphSkillGeneratorInit:
    """Tests for CallGraphSkillGenerator initialization."""

    def test_init_creates_generator(self, tmp_path: Path):
        """Test basic initialization."""
        generator = CallGraphSkillGenerator(tmp_path)
        assert generator.output_dir == tmp_path


class TestCallGraphSkillGeneratorGenerate:
    """Tests for CallGraphSkillGenerator.generate()."""

    def test_generate_creates_skill_directory(
        self, tmp_path: Path, call_graph_md_file: Path
    ):
        """Test that generation creates the skill directory."""
        generator = CallGraphSkillGenerator(tmp_path)
        skill_dir = generator.generate(call_graph_md_file)

        assert skill_dir.exists()
        assert skill_dir.is_dir()
        assert skill_dir.name == CALL_GRAPH_SKILL_NAME

    def test_generate_creates_skill_md(self, tmp_path: Path, call_graph_md_file: Path):
        """Test that generation creates SKILL.md."""
        generator = CallGraphSkillGenerator(tmp_path)
        skill_dir = generator.generate(call_graph_md_file)

        skill_md = skill_dir / "SKILL.md"
        assert skill_md.exists()
        assert skill_md.is_file()

    def test_generate_returns_skill_path(
        self, tmp_path: Path, call_graph_md_file: Path
    ):
        """Test that generation returns the correct path."""
        generator = CallGraphSkillGenerator(tmp_path)
        skill_dir = generator.generate(call_graph_md_file)

        expected = tmp_path / CALL_GRAPH_SKILL_NAME
        assert skill_dir == expected

    def test_generate_handles_missing_file(self, tmp_path: Path):
        """Test that generation raises error for missing file."""
        generator = CallGraphSkillGenerator(tmp_path)
        nonexistent = tmp_path / "MISSING.md"

        with pytest.raises(CallGraphSkillGenerationError, match="Failed to read"):
            generator.generate(nonexistent)


class TestCallGraphSkillMdContent:
    """Tests for SKILL.md content formatting."""

    def test_skill_md_has_frontmatter(self, tmp_path: Path, call_graph_md_file: Path):
        """Test that SKILL.md has valid frontmatter."""
        generator = CallGraphSkillGenerator(tmp_path)
        skill_dir = generator.generate(call_graph_md_file)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        assert content.startswith("---\n")
        assert f"name: {CALL_GRAPH_SKILL_NAME}" in content
        assert "description:" in content

    def test_skill_md_has_title(self, tmp_path: Path, call_graph_md_file: Path):
        """Test that SKILL.md has title."""
        generator = CallGraphSkillGenerator(tmp_path)
        skill_dir = generator.generate(call_graph_md_file)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        assert "# Call Graph" in content

    def test_skill_md_has_about_section(self, tmp_path: Path, call_graph_md_file: Path):
        """Test that SKILL.md has about section."""
        generator = CallGraphSkillGenerator(tmp_path)
        skill_dir = generator.generate(call_graph_md_file)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        assert "## About This Skill" in content

    def test_skill_md_has_when_to_use(self, tmp_path: Path, call_graph_md_file: Path):
        """Test that SKILL.md has when to use section."""
        generator = CallGraphSkillGenerator(tmp_path)
        skill_dir = generator.generate(call_graph_md_file)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        assert "## When to Use This Skill" in content
        assert "call" in content.lower()

    def test_skill_md_includes_mermaid_diagram(
        self, tmp_path: Path, call_graph_md_file: Path
    ):
        """Test that SKILL.md includes the mermaid diagram."""
        generator = CallGraphSkillGenerator(tmp_path)
        skill_dir = generator.generate(call_graph_md_file)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        assert "```mermaid" in content
        assert "flowchart TD" in content

    def test_skill_md_includes_entry_points(
        self, tmp_path: Path, call_graph_md_file: Path
    ):
        """Test that SKILL.md includes entry points section."""
        generator = CallGraphSkillGenerator(tmp_path)
        skill_dir = generator.generate(call_graph_md_file)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        assert "Entry Points" in content
        assert "CBPAUP0J" in content

    def test_skill_md_includes_statistics(
        self, tmp_path: Path, call_graph_md_file: Path
    ):
        """Test that SKILL.md includes statistics."""
        generator = CallGraphSkillGenerator(tmp_path)
        skill_dir = generator.generate(call_graph_md_file)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        assert "Statistics" in content
        assert "24" in content  # Programs analyzed

    def test_skill_md_excludes_generated_timestamp(
        self, tmp_path: Path, call_graph_md_file: Path
    ):
        """Test that SKILL.md excludes generated timestamp."""
        generator = CallGraphSkillGenerator(tmp_path)
        skill_dir = generator.generate(call_graph_md_file)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        assert "*Generated:" not in content


class TestRelatedSkills:
    """Tests for related skills section."""

    def test_skill_md_has_related_skills(
        self, tmp_path: Path, call_graph_md_file: Path
    ):
        """Test that SKILL.md has related skills section."""
        generator = CallGraphSkillGenerator(tmp_path)
        skill_dir = generator.generate(call_graph_md_file)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        assert "## Related Skills" in content
        assert "system-overview" in content


class TestDescriptionExtraction:
    """Tests for description extraction."""

    def test_extracts_programs_count(self, tmp_path: Path, call_graph_md_file: Path):
        """Test that description includes programs count."""
        generator = CallGraphSkillGenerator(tmp_path)
        skill_dir = generator.generate(call_graph_md_file)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        # Description should mention 24 programs
        lines = content.split("\n")
        for line in lines:
            if line.startswith("description:"):
                assert "24" in line or "programs" in line.lower()
                break


class TestEdgeCases:
    """Tests for edge cases."""

    def test_handles_minimal_call_graph(self, tmp_path: Path):
        """Test handling of minimal call graph content."""
        call_graph_path = tmp_path / "CALL_GRAPH.md"
        call_graph_path.write_text(
            "# Call Graph\n\nNo programs analyzed yet.", encoding="utf-8"
        )

        generator = CallGraphSkillGenerator(tmp_path)
        skill_dir = generator.generate(call_graph_path)

        assert skill_dir.exists()
        skill_md = skill_dir / "SKILL.md"
        assert skill_md.exists()

    def test_handles_no_mermaid_diagram(self, tmp_path: Path):
        """Test handling of call graph without mermaid diagram."""
        call_graph_path = tmp_path / "CALL_GRAPH.md"
        call_graph_path.write_text(
            "# Call Graph\n\n## Entry Points\n\n- MAIN\n\n## Statistics\n\nNone",
            encoding="utf-8",
        )

        generator = CallGraphSkillGenerator(tmp_path)
        skill_dir = generator.generate(call_graph_path)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        assert "Entry Points" in content
        assert "MAIN" in content

    def test_generates_idempotently(self, tmp_path: Path, call_graph_md_file: Path):
        """Test that running twice produces same result."""
        generator = CallGraphSkillGenerator(tmp_path)

        skill_dir1 = generator.generate(call_graph_md_file)
        content1 = (skill_dir1 / "SKILL.md").read_text(encoding="utf-8")

        skill_dir2 = generator.generate(call_graph_md_file)
        content2 = (skill_dir2 / "SKILL.md").read_text(encoding="utf-8")

        assert content1 == content2
