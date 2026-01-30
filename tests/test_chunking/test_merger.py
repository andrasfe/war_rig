"""Tests for ChunkMerger."""

from datetime import datetime

import pytest

from war_rig.agents.scribe import ScribeOutput
from war_rig.chunking.merger import ChunkMerger
from war_rig.chunking.models import ChunkContextType, CodeChunk
from war_rig.models.templates import (
    DocumentationTemplate,
    FileType,
    HeaderSection,
    ProgramType,
    PurposeSection,
)


def make_chunk(
    chunk_id: str,
    start_line: int = 1,
    end_line: int = 100,
    context_type: ChunkContextType = ChunkContextType.SECTION,
    includes_header: bool = False,
) -> CodeChunk:
    """Create a test chunk."""
    return CodeChunk(
        chunk_id=chunk_id,
        content=f"* Chunk {chunk_id} content",
        start_line=start_line,
        end_line=end_line,
        context_type=context_type,
        estimated_tokens=500,
        includes_header=includes_header,
        includes_data_division=includes_header,
    )


def make_template(
    program_id: str = "TESTPROG",
    summary: str = "Test program",
    inputs: list[dict] | None = None,
    paragraphs: list[dict] | None = None,
) -> DocumentationTemplate:
    """Create a test template."""
    return DocumentationTemplate(
        header=HeaderSection(
            program_id=program_id,
            file_name=f"{program_id}.cbl",
            file_type=FileType.COBOL,
            analyzed_by="WAR_RIG_TEST",
            analyzed_at=datetime.utcnow(),
            iteration_count=1,
        ),
        purpose=PurposeSection(
            summary=summary,
            business_context="Test context",
            program_type=ProgramType.BATCH,
            citations=[1, 2],
        ),
        inputs=inputs or [],
        outputs=[],
        called_programs=[],
        business_rules=[],
        copybooks_used=[],
        paragraphs=paragraphs or [],
        error_handling=[],
    )


def make_output(template: DocumentationTemplate, success: bool = True) -> ScribeOutput:
    """Create a test ScribeOutput."""
    return ScribeOutput(
        success=success,
        template=template if success else None,
        error=None if success else "Test error",
    )


class TestChunkMergerBasic:
    """Basic tests for ChunkMerger."""

    def test_merge_single_chunk(self):
        """Single chunk should return its template unchanged (mostly)."""
        merger = ChunkMerger()

        chunk = make_chunk("chunk-1", includes_header=True)
        template = make_template()
        output = make_output(template)

        merged = merger.merge([chunk], [output], "TEST.cbl")

        assert merged.header.program_id == "TESTPROG"
        assert merged.purpose.summary == "Test program"

    def test_merge_requires_same_count(self):
        """Chunks and outputs must have same count."""
        merger = ChunkMerger()

        chunks = [make_chunk("chunk-1"), make_chunk("chunk-2")]
        outputs = [make_output(make_template())]

        with pytest.raises(ValueError, match="Chunk count"):
            merger.merge(chunks, outputs, "TEST.cbl")

    def test_merge_requires_chunks(self):
        """Must have at least one chunk."""
        merger = ChunkMerger()

        with pytest.raises(ValueError, match="No chunks to merge"):
            merger.merge([], [], "TEST.cbl")

    def test_merge_handles_failed_outputs(self):
        """Should skip failed outputs but use successful ones."""
        merger = ChunkMerger()

        chunks = [
            make_chunk("chunk-1", includes_header=True),
            make_chunk("chunk-2"),
        ]
        outputs = [
            make_output(make_template(), success=True),
            make_output(make_template(), success=False),  # Failed
        ]

        # Should not raise, should use successful output
        merged = merger.merge(chunks, outputs, "TEST.cbl")
        assert merged is not None


class TestChunkMergerDeduplication:
    """Tests for deduplication during merge."""

    def test_merge_deduplicates_inputs(self):
        """Inputs should be deduplicated by name."""
        merger = ChunkMerger()

        chunks = [
            make_chunk("chunk-1", includes_header=True),
            make_chunk("chunk-2"),
        ]

        template1 = make_template(inputs=[
            {"name": "INPUT-FILE", "io_type": "FILE_SEQUENTIAL", "description": "Input file"},
            {"name": "INPUT-PARAM", "io_type": "PARAMETER", "description": "Input parameter"},
        ])
        template2 = make_template(inputs=[
            {"name": "INPUT-FILE", "io_type": "FILE_SEQUENTIAL", "description": "Input file"},  # Duplicate
            {"name": "INPUT-OTHER", "io_type": "FILE_VSAM", "description": "Other input"},
        ])

        outputs = [make_output(template1), make_output(template2)]

        merged = merger.merge(chunks, outputs, "TEST.cbl")

        # Should have 3 unique inputs
        input_names = [i.get("name") if isinstance(i, dict) else i.name
                       for i in merged.inputs]
        assert len(input_names) == 3
        assert "INPUT-FILE" in input_names
        assert "INPUT-PARAM" in input_names
        assert "INPUT-OTHER" in input_names

    def test_merge_deduplicates_paragraphs(self):
        """Paragraphs should be deduplicated by name."""
        merger = ChunkMerger()

        chunks = [
            make_chunk("chunk-1", includes_header=True),
            make_chunk("chunk-2"),
        ]

        template1 = make_template(paragraphs=[
            {"paragraph_name": "MAIN-PARA", "purpose": "Main entry"},
        ])
        template2 = make_template(paragraphs=[
            {"paragraph_name": "MAIN-PARA", "purpose": "Main entry duplicate"},  # Dup
            {"paragraph_name": "PROCESS-PARA", "purpose": "Processing"},
        ])

        outputs = [make_output(template1), make_output(template2)]

        merged = merger.merge(chunks, outputs, "TEST.cbl")

        para_names = [p.get("paragraph_name") if isinstance(p, dict) else p.paragraph_name
                      for p in merged.paragraphs]
        # Should keep first occurrence
        assert len(para_names) == 2
        assert "MAIN-PARA" in para_names
        assert "PROCESS-PARA" in para_names


class TestChunkMergerPurpose:
    """Tests for purpose section merging."""

    def test_merge_uses_header_chunk_purpose(self):
        """Should prefer purpose from header chunk."""
        merger = ChunkMerger()

        chunks = [
            make_chunk("chunk-1", includes_header=True),
            make_chunk("chunk-2", includes_header=False),
        ]

        template1 = make_template(summary="Primary summary from header chunk")
        template2 = make_template(summary="Secondary summary")

        outputs = [make_output(template1), make_output(template2)]

        merged = merger.merge(chunks, outputs, "TEST.cbl")

        assert "Primary summary" in merged.purpose.summary

    def test_merge_combines_citations(self):
        """Should combine citations from all chunks."""
        merger = ChunkMerger()

        chunks = [
            make_chunk("chunk-1", includes_header=True),
            make_chunk("chunk-2"),
        ]

        template1 = make_template()
        template1.purpose.citations = [1, 2, 3]
        template2 = make_template()
        template2.purpose.citations = [3, 4, 5]  # Overlapping

        outputs = [make_output(template1), make_output(template2)]

        merged = merger.merge(chunks, outputs, "TEST.cbl")

        # Should have unique, sorted citations
        assert merged.purpose.citations == [1, 2, 3, 4, 5]


class TestChunkMergerOpenQuestions:
    """Tests for open questions collection."""

    def test_merge_collects_all_questions(self):
        """Should collect open questions from all chunks."""
        merger = ChunkMerger()

        chunks = [
            make_chunk("chunk-1", start_line=1, end_line=100, includes_header=True),
            make_chunk("chunk-2", start_line=101, end_line=200),
        ]

        template1 = make_template()
        template1.open_questions = [{"question": "Q1 from chunk 1"}]
        template2 = make_template()
        template2.open_questions = [{"question": "Q2 from chunk 2"}]

        outputs = [make_output(template1), make_output(template2)]

        merged = merger.merge(chunks, outputs, "TEST.cbl")

        # Should have both questions with chunk context
        assert len(merged.open_questions) == 2
        q_texts = [q.get("question") if isinstance(q, dict) else q.question
                   for q in merged.open_questions]
        assert "Q1 from chunk 1" in q_texts
        assert "Q2 from chunk 2" in q_texts
