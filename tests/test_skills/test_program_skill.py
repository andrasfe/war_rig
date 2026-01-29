"""Unit tests for ProgramSkillGenerator class.

Tests:
- Skill generation from .doc.json files
- SKILL.md content formatting
- REFERENCE.md content formatting
- Program summary extraction
- Error handling
"""

import json
from pathlib import Path

import pytest

from war_rig.skills import (
    ProgramSkillGenerationError,
    ProgramSkillGenerator,
)


@pytest.fixture
def sample_doc_data() -> dict:
    """Create sample documentation data for testing."""
    return {
        "header": {
            "program_id": "CBPAUP0C",
            "file_name": "CBPAUP0C.cbl",
            "file_type": "COBOL",
            "analyzed_by": "WAR_RIG",
            "analyzed_at": "2026-01-28T14:54:28.819625",
        },
        "purpose": {
            "summary": "This batch COBOL IMS program deletes expired pending authorization messages.",
            "business_context": "CardDemo - Authorization Module",
            "program_type": "BATCH",
        },
        "inputs": [
            {
                "name": "SYSIN",
                "io_type": "PARAMETER",
                "description": "Contains parameters for expiry days and checkpoint frequency.",
                "copybook": None,
            },
            {
                "name": "IMS Database - PAUTSUM0",
                "io_type": "IMS_SEGMENT",
                "description": "Contains summary information about pending authorizations.",
                "copybook": "CIPAUSMY",
            },
        ],
        "outputs": [
            {
                "name": "DISPLAY",
                "io_type": "REPORT",
                "description": "Displays program start message and summary counts.",
                "copybook": None,
            },
        ],
        "called_programs": [],
        "business_rules": [
            {
                "rule_id": "BR001",
                "description": "Determine if a pending authorization detail is expired.",
                "logic_summary": "Calculates the difference between current and auth date.",
                "conditions": ["WS-DAY-DIFF >= WS-EXPIRY-DAYS"],
            },
            {
                "rule_id": "BR002",
                "description": "Adjust approved/declined counts before deletion.",
                "logic_summary": "If response code is '00', subtract from approved.",
                "conditions": ["PA-AUTH-RESP-CODE = '00'"],
            },
        ],
        "copybooks_used": [
            {
                "copybook_name": "CIPAUSMY",
                "purpose": "Defines the layout of PENDING-AUTH-SUMMARY segment.",
                "location": "WORKING_STORAGE",
            },
        ],
        "paragraphs": [
            {
                "paragraph_name": "MAIN-PARA",
                "purpose": "Main control paragraph orchestrating the process.",
                "calls": ["1000-INITIALIZE", "2000-FIND-NEXT-AUTH-SUMMARY"],
                "is_dead_code": False,
            },
        ],
        "data_flow": {
            "reads_from": [
                {
                    "source": "SYSIN",
                    "fields_used": ["P-EXPIRY-DAYS", "P-CHKP-FREQ"],
                }
            ],
            "writes_to": [],
            "transforms": [],
        },
        "error_handling": [
            {
                "condition": "AUTH SUMMARY READ FAILED",
                "action": "DISPLAY error message and ABEND",
            }
        ],
        "sql_operations": [],
        "cics_operations": [],
        "dead_code": [],
        "flow_diagram": "flowchart TD\n    MAIN_PARA --> 1000_INITIALIZE",
    }


@pytest.fixture
def doc_json_file(tmp_path: Path, sample_doc_data: dict) -> Path:
    """Create a sample .doc.json file."""
    programs_dir = tmp_path / "final" / "programs"
    programs_dir.mkdir(parents=True)
    doc_path = programs_dir / "CBPAUP0C.cbl.doc.json"
    doc_path.write_text(json.dumps(sample_doc_data), encoding="utf-8")
    return doc_path


class TestProgramSkillGeneratorInit:
    """Tests for ProgramSkillGenerator initialization."""

    def test_init_creates_generator(self, tmp_path: Path):
        """Test basic initialization."""
        generator = ProgramSkillGenerator(tmp_path)
        assert generator.output_dir == tmp_path


class TestProgramSkillGeneratorGenerate:
    """Tests for ProgramSkillGenerator.generate_from_doc_json()."""

    def test_generate_creates_skill_directory(
        self, tmp_path: Path, doc_json_file: Path
    ):
        """Test that generation creates the skill directory."""
        generator = ProgramSkillGenerator(tmp_path)
        skill_dir = generator.generate_from_doc_json(doc_json_file)

        assert skill_dir.exists()
        assert skill_dir.is_dir()
        assert skill_dir.name == "cbpaup0c"

    def test_generate_creates_skill_md(self, tmp_path: Path, doc_json_file: Path):
        """Test that generation creates SKILL.md."""
        generator = ProgramSkillGenerator(tmp_path)
        skill_dir = generator.generate_from_doc_json(doc_json_file)

        skill_md = skill_dir / "SKILL.md"
        assert skill_md.exists()
        assert skill_md.is_file()

    def test_generate_creates_reference_md(self, tmp_path: Path, doc_json_file: Path):
        """Test that generation creates REFERENCE.md in references subdir."""
        generator = ProgramSkillGenerator(tmp_path)
        skill_dir = generator.generate_from_doc_json(doc_json_file)

        ref_md = skill_dir / "references" / "REFERENCE.md"
        assert ref_md.exists()
        assert ref_md.is_file()

    def test_generate_returns_skill_path(self, tmp_path: Path, doc_json_file: Path):
        """Test that generation returns the skill directory path."""
        generator = ProgramSkillGenerator(tmp_path)
        skill_dir = generator.generate_from_doc_json(doc_json_file)

        expected = tmp_path / "programs" / "cbpaup0c"
        assert skill_dir == expected

    def test_generate_handles_missing_file(self, tmp_path: Path):
        """Test that generation raises error for missing file."""
        generator = ProgramSkillGenerator(tmp_path)
        nonexistent = tmp_path / "missing.doc.json"

        with pytest.raises(ProgramSkillGenerationError, match="Failed to load"):
            generator.generate_from_doc_json(nonexistent)

    def test_generate_handles_invalid_json(self, tmp_path: Path):
        """Test that generation raises error for invalid JSON."""
        invalid_file = tmp_path / "invalid.doc.json"
        invalid_file.write_text("not valid json", encoding="utf-8")

        generator = ProgramSkillGenerator(tmp_path)

        with pytest.raises(ProgramSkillGenerationError, match="Failed to load"):
            generator.generate_from_doc_json(invalid_file)


class TestProgramSkillMdContent:
    """Tests for SKILL.md content formatting."""

    def test_skill_md_has_frontmatter(self, tmp_path: Path, doc_json_file: Path):
        """Test that SKILL.md has valid frontmatter."""
        generator = ProgramSkillGenerator(tmp_path)
        skill_dir = generator.generate_from_doc_json(doc_json_file)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        assert content.startswith("---\n")
        assert "name: cbpaup0c" in content
        assert "description:" in content

    def test_skill_md_has_program_header(self, tmp_path: Path, doc_json_file: Path):
        """Test that SKILL.md includes program header info."""
        generator = ProgramSkillGenerator(tmp_path)
        skill_dir = generator.generate_from_doc_json(doc_json_file)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        assert "# CBPAUP0C" in content
        assert "COBOL" in content
        assert "BATCH" in content

    def test_skill_md_has_purpose_section(self, tmp_path: Path, doc_json_file: Path):
        """Test that SKILL.md includes purpose section."""
        generator = ProgramSkillGenerator(tmp_path)
        skill_dir = generator.generate_from_doc_json(doc_json_file)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        assert "## Purpose" in content
        assert "expired pending authorization" in content

    def test_skill_md_has_business_rules(self, tmp_path: Path, doc_json_file: Path):
        """Test that SKILL.md includes business rules."""
        generator = ProgramSkillGenerator(tmp_path)
        skill_dir = generator.generate_from_doc_json(doc_json_file)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        assert "## Business Rules" in content
        assert "BR001" in content

    def test_skill_md_has_inputs(self, tmp_path: Path, doc_json_file: Path):
        """Test that SKILL.md includes inputs section."""
        generator = ProgramSkillGenerator(tmp_path)
        skill_dir = generator.generate_from_doc_json(doc_json_file)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        assert "## Inputs" in content
        assert "SYSIN" in content

    def test_skill_md_has_outputs(self, tmp_path: Path, doc_json_file: Path):
        """Test that SKILL.md includes outputs section."""
        generator = ProgramSkillGenerator(tmp_path)
        skill_dir = generator.generate_from_doc_json(doc_json_file)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        assert "## Outputs" in content
        assert "DISPLAY" in content

    def test_skill_md_has_when_to_use(self, tmp_path: Path, doc_json_file: Path):
        """Test that SKILL.md includes when to use section."""
        generator = ProgramSkillGenerator(tmp_path)
        skill_dir = generator.generate_from_doc_json(doc_json_file)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        assert "## When to Use This Skill" in content
        assert "CBPAUP0C" in content

    def test_skill_md_references_reference_md(
        self, tmp_path: Path, doc_json_file: Path
    ):
        """Test that SKILL.md references REFERENCE.md."""
        generator = ProgramSkillGenerator(tmp_path)
        skill_dir = generator.generate_from_doc_json(doc_json_file)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        assert "REFERENCE.md" in content


class TestReferenceMdContent:
    """Tests for REFERENCE.md content formatting."""

    def test_reference_md_has_header(self, tmp_path: Path, doc_json_file: Path):
        """Test that REFERENCE.md has complete header."""
        generator = ProgramSkillGenerator(tmp_path)
        skill_dir = generator.generate_from_doc_json(doc_json_file)

        ref_md = skill_dir / "references" / "REFERENCE.md"
        content = ref_md.read_text(encoding="utf-8")

        assert "# CBPAUP0C - Complete Reference" in content
        assert "Program Header" in content
        assert "CBPAUP0C.cbl" in content

    def test_reference_md_has_paragraphs(self, tmp_path: Path, doc_json_file: Path):
        """Test that REFERENCE.md includes paragraphs section."""
        generator = ProgramSkillGenerator(tmp_path)
        skill_dir = generator.generate_from_doc_json(doc_json_file)

        ref_md = skill_dir / "references" / "REFERENCE.md"
        content = ref_md.read_text(encoding="utf-8")

        assert "## Paragraphs" in content
        assert "MAIN-PARA" in content

    def test_reference_md_has_data_flow(self, tmp_path: Path, doc_json_file: Path):
        """Test that REFERENCE.md includes data flow section."""
        generator = ProgramSkillGenerator(tmp_path)
        skill_dir = generator.generate_from_doc_json(doc_json_file)

        ref_md = skill_dir / "references" / "REFERENCE.md"
        content = ref_md.read_text(encoding="utf-8")

        assert "## Data Flow" in content
        assert "Reads From" in content

    def test_reference_md_has_error_handling(self, tmp_path: Path, doc_json_file: Path):
        """Test that REFERENCE.md includes error handling section."""
        generator = ProgramSkillGenerator(tmp_path)
        skill_dir = generator.generate_from_doc_json(doc_json_file)

        ref_md = skill_dir / "references" / "REFERENCE.md"
        content = ref_md.read_text(encoding="utf-8")

        assert "## Error Handling" in content
        assert "ABEND" in content

    def test_reference_md_has_flow_diagram(self, tmp_path: Path, doc_json_file: Path):
        """Test that REFERENCE.md includes flow diagram."""
        generator = ProgramSkillGenerator(tmp_path)
        skill_dir = generator.generate_from_doc_json(doc_json_file)

        ref_md = skill_dir / "references" / "REFERENCE.md"
        content = ref_md.read_text(encoding="utf-8")

        assert "## Flow Diagram" in content
        assert "```mermaid" in content
        assert "flowchart TD" in content


class TestProgramSummaryExtraction:
    """Tests for get_program_summary()."""

    def test_get_program_summary(self, tmp_path: Path, sample_doc_data: dict):
        """Test program summary extraction."""
        generator = ProgramSkillGenerator(tmp_path)
        summary = generator.get_program_summary(sample_doc_data)

        assert summary["name"] == "cbpaup0c"
        assert summary["program_id"] == "CBPAUP0C"
        assert "expired pending authorization" in summary["description"]
        assert summary["type"] == "BATCH"

    def test_get_program_summary_handles_missing_fields(self, tmp_path: Path):
        """Test summary extraction with minimal data."""
        generator = ProgramSkillGenerator(tmp_path)
        minimal_data = {
            "header": {"program_id": "TESTPROG"},
            "purpose": {},
        }
        summary = generator.get_program_summary(minimal_data)

        assert summary["name"] == "testprog"
        assert summary["program_id"] == "TESTPROG"
        assert "TESTPROG" in summary["description"]


class TestEdgeCases:
    """Tests for edge cases and special scenarios."""

    def test_handles_empty_sections(self, tmp_path: Path):
        """Test handling of documentation with empty sections."""
        programs_dir = tmp_path / "final" / "programs"
        programs_dir.mkdir(parents=True)

        empty_doc = {
            "header": {
                "program_id": "EMPTY",
                "file_name": "EMPTY.cbl",
                "file_type": "COBOL",
            },
            "purpose": {"summary": "Test program"},
            "inputs": [],
            "outputs": [],
            "called_programs": [],
            "business_rules": [],
            "copybooks_used": [],
            "paragraphs": [],
            "data_flow": {},
            "error_handling": [],
            "sql_operations": [],
            "cics_operations": [],
            "dead_code": [],
        }

        doc_path = programs_dir / "EMPTY.cbl.doc.json"
        doc_path.write_text(json.dumps(empty_doc), encoding="utf-8")

        generator = ProgramSkillGenerator(tmp_path)
        skill_dir = generator.generate_from_doc_json(doc_path)

        assert skill_dir.exists()
        assert (skill_dir / "SKILL.md").exists()

    def test_handles_special_characters_in_descriptions(self, tmp_path: Path):
        """Test handling of special characters in content."""
        programs_dir = tmp_path / "final" / "programs"
        programs_dir.mkdir(parents=True)

        special_doc = {
            "header": {
                "program_id": "SPECIAL",
                "file_name": "SPECIAL.cbl",
                "file_type": "COBOL",
            },
            "purpose": {
                "summary": "Program with special chars: <>&\"'| and newlines\nSecond line"
            },
            "inputs": [],
            "outputs": [],
            "called_programs": [],
            "business_rules": [],
            "copybooks_used": [],
            "paragraphs": [],
        }

        doc_path = programs_dir / "SPECIAL.cbl.doc.json"
        doc_path.write_text(json.dumps(special_doc), encoding="utf-8")

        generator = ProgramSkillGenerator(tmp_path)
        skill_dir = generator.generate_from_doc_json(doc_path)

        assert skill_dir.exists()
        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")
        assert "special chars" in content

    def test_truncates_long_business_rules(self, tmp_path: Path):
        """Test that many business rules are truncated in summary."""
        programs_dir = tmp_path / "final" / "programs"
        programs_dir.mkdir(parents=True)

        many_rules_doc = {
            "header": {
                "program_id": "MANY",
                "file_name": "MANY.cbl",
                "file_type": "COBOL",
            },
            "purpose": {"summary": "Test program"},
            "inputs": [],
            "outputs": [],
            "called_programs": [],
            "business_rules": [
                {"rule_id": f"BR{i:03d}", "description": f"Rule {i}"}
                for i in range(1, 11)  # 10 rules
            ],
            "copybooks_used": [],
            "paragraphs": [],
        }

        doc_path = programs_dir / "MANY.cbl.doc.json"
        doc_path.write_text(json.dumps(many_rules_doc), encoding="utf-8")

        generator = ProgramSkillGenerator(tmp_path)
        skill_dir = generator.generate_from_doc_json(doc_path)

        skill_md = skill_dir / "SKILL.md"
        content = skill_md.read_text(encoding="utf-8")

        # Should show first 5 and indicate more
        assert "BR001" in content
        assert "BR005" in content
        assert "+5 more rules" in content
