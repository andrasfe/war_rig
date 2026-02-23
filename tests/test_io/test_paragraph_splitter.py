"""Tests for COBOL paragraph splitter and markdown linker."""

from __future__ import annotations

import json
from pathlib import Path

from war_rig.io.paragraph_splitter import (
    _PARAGRAPH_HEADING_RE,
    _SOURCE_LINK_RE,
    patch_all_markdown_in_directory,
    patch_markdown_links,
    run_batch_split,
    sanitize_filename,
    split_all_in_directory,
    split_and_link,
    split_paragraphs,
)

# ---------------------------------------------------------------------------
# Helpers to build test fixtures
# ---------------------------------------------------------------------------


def _make_source_file(path: Path, num_lines: int = 100) -> Path:
    """Create a fake COBOL source file with numbered lines."""
    lines = [f"LINE {i:03d}\n" for i in range(1, num_lines + 1)]
    path.write_text("".join(lines))
    return path


def _make_doc_json(
    path: Path,
    file_name: str = "TESTPROG.cbl",
    paragraphs: list[dict] | None = None,
) -> Path:
    """Create a minimal .doc.json file with the given paragraphs."""
    if paragraphs is None:
        paragraphs = []
    doc = {
        "header": {
            "program_id": file_name.split(".")[0],
            "file_name": file_name,
            "file_type": "COBOL",
        },
        "paragraphs": paragraphs,
    }
    path.write_text(json.dumps(doc))
    return path


def _make_markdown(path: Path, content: str) -> Path:
    """Write markdown content to a file."""
    path.write_text(content)
    return path


# ---------------------------------------------------------------------------
# TestSanitizeFilename
# ---------------------------------------------------------------------------


class TestSanitizeFilename:
    """Tests for sanitize_filename()."""

    def test_standard_cobol_name_with_hyphen(self):
        """Standard COBOL paragraph names with hyphens pass through."""
        assert sanitize_filename("MAIN-PARA") == "MAIN-PARA"

    def test_numeric_prefixed_name(self):
        """Numeric-prefixed paragraph names pass through."""
        assert sanitize_filename("1000-INITIALIZE") == "1000-INITIALIZE"

    def test_single_word_name(self):
        """Single-word program names pass through."""
        assert sanitize_filename("CBPAUP0C") == "CBPAUP0C"

    def test_empty_string(self):
        """Empty string returns empty string."""
        assert sanitize_filename("") == ""

    def test_whitespace_stripped(self):
        """Leading and trailing whitespace is stripped."""
        result = sanitize_filename("  MAIN-PARA  ")
        assert result == "MAIN-PARA"

    def test_unsafe_characters_replaced(self):
        """Filesystem-unsafe characters are replaced with underscores."""
        result = sanitize_filename('FOO<BAR>BAZ:QUX"QUUX')
        assert "<" not in result
        assert ">" not in result
        assert ":" not in result
        assert '"' not in result

    def test_backslash_replaced(self):
        """Backslash is replaced."""
        result = sanitize_filename("FOO\\BAR")
        assert "\\" not in result

    def test_consecutive_underscores_collapsed(self):
        """Multiple consecutive unsafe characters collapse to one underscore."""
        result = sanitize_filename("FOO<>BAR")
        # Should become FOO_BAR, not FOO__BAR
        assert "__" not in result

    def test_preserves_case(self):
        """Case is preserved (COBOL names are typically uppercase)."""
        assert sanitize_filename("MixedCase-Name") == "MixedCase-Name"


# ---------------------------------------------------------------------------
# TestSplitParagraphs
# ---------------------------------------------------------------------------


class TestSplitParagraphs:
    """Tests for split_paragraphs()."""

    def test_happy_path_extracts_correct_lines(self, tmp_path):
        """Paragraphs are extracted by line range with correct content."""
        source = _make_source_file(tmp_path / "TESTPROG.cbl", num_lines=50)
        doc_json = _make_doc_json(
            tmp_path / "TESTPROG.cbl.doc.json",
            paragraphs=[
                {
                    "paragraph_name": "MAIN-PARA",
                    "citation": [10, 15],
                    "purpose": "Main logic",
                },
                {
                    "paragraph_name": "1000-INIT",
                    "citation": [20, 25],
                    "purpose": "Initialization",
                },
            ],
        )
        output_dir = tmp_path / "output" / "TESTPROG.cbl.d"
        result = split_paragraphs(source, doc_json, output_dir)

        assert len(result) == 2
        assert all(p.exists() for p in result)

        # Check content of first paragraph (lines 10-15, 1-indexed inclusive)
        main_content = (output_dir / "MAIN-PARA.cbl").read_text()
        assert "LINE 010" in main_content
        assert "LINE 015" in main_content
        assert "LINE 009" not in main_content
        assert "LINE 016" not in main_content

        # Check content of second paragraph
        init_content = (output_dir / "1000-INIT.cbl").read_text()
        assert "LINE 020" in init_content
        assert "LINE 025" in init_content

    def test_single_line_paragraph(self, tmp_path):
        """A paragraph citing a single line [n, n] produces a one-line file."""
        source = _make_source_file(tmp_path / "TESTPROG.cbl", num_lines=50)
        doc_json = _make_doc_json(
            tmp_path / "TESTPROG.cbl.doc.json",
            paragraphs=[
                {
                    "paragraph_name": "SINGLE-LINE",
                    "citation": [23, 23],
                    "purpose": "One liner",
                },
            ],
        )
        output_dir = tmp_path / "out"
        result = split_paragraphs(source, doc_json, output_dir)

        assert len(result) == 1
        content = result[0].read_text()
        assert "LINE 023" in content
        lines = content.strip().splitlines()
        assert len(lines) == 1

    def test_missing_source_file_returns_empty(self, tmp_path):
        """When source file does not exist, returns empty list with warning."""
        doc_json = _make_doc_json(
            tmp_path / "TESTPROG.cbl.doc.json",
            paragraphs=[
                {
                    "paragraph_name": "MAIN-PARA",
                    "citation": [1, 5],
                    "purpose": "Main",
                },
            ],
        )
        nonexistent = tmp_path / "NOTFOUND.cbl"
        result = split_paragraphs(nonexistent, doc_json)
        assert result == []

    def test_missing_citation_skips_paragraph(self, tmp_path):
        """Paragraphs with null/missing citation are skipped."""
        source = _make_source_file(tmp_path / "TESTPROG.cbl", num_lines=50)
        doc_json = _make_doc_json(
            tmp_path / "TESTPROG.cbl.doc.json",
            paragraphs=[
                {
                    "paragraph_name": "NO-CITE",
                    "citation": None,
                    "purpose": "No citation",
                },
                {
                    "paragraph_name": "HAS-CITE",
                    "citation": [1, 5],
                    "purpose": "Has citation",
                },
            ],
        )
        output_dir = tmp_path / "out"
        result = split_paragraphs(source, doc_json, output_dir)

        assert len(result) == 1
        assert result[0].name == "HAS-CITE.cbl"

    def test_citation_beyond_file_length(self, tmp_path):
        """Citation beyond file length does not crash."""
        source = _make_source_file(tmp_path / "TESTPROG.cbl", num_lines=10)
        doc_json = _make_doc_json(
            tmp_path / "TESTPROG.cbl.doc.json",
            paragraphs=[
                {
                    "paragraph_name": "BEYOND",
                    "citation": [8, 999],
                    "purpose": "Past end of file",
                },
            ],
        )
        output_dir = tmp_path / "out"
        # Should not raise; may produce a partial file or skip
        result = split_paragraphs(source, doc_json, output_dir)
        # Either creates the file with available lines or skips gracefully
        assert isinstance(result, list)

    def test_empty_paragraphs_list(self, tmp_path):
        """Empty paragraphs list returns empty list and creates no files."""
        source = _make_source_file(tmp_path / "TESTPROG.cbl", num_lines=10)
        doc_json = _make_doc_json(
            tmp_path / "TESTPROG.cbl.doc.json",
            paragraphs=[],
        )
        output_dir = tmp_path / "out"
        result = split_paragraphs(source, doc_json, output_dir)

        assert result == []

    def test_output_directory_created(self, tmp_path):
        """Output directory is created automatically if it does not exist."""
        source = _make_source_file(tmp_path / "TESTPROG.cbl", num_lines=50)
        doc_json = _make_doc_json(
            tmp_path / "TESTPROG.cbl.doc.json",
            paragraphs=[
                {
                    "paragraph_name": "MAIN-PARA",
                    "citation": [1, 5],
                    "purpose": "Main",
                },
            ],
        )
        deep_dir = tmp_path / "a" / "b" / "c" / "TESTPROG.cbl.d"
        assert not deep_dir.exists()

        result = split_paragraphs(source, doc_json, deep_dir)

        assert deep_dir.exists()
        assert len(result) == 1

    def test_default_output_dir(self, tmp_path):
        """When output_dir is None, defaults to source_path.parent / {name}.d."""
        source = _make_source_file(tmp_path / "TESTPROG.cbl", num_lines=50)
        doc_json = _make_doc_json(
            tmp_path / "TESTPROG.cbl.doc.json",
            paragraphs=[
                {
                    "paragraph_name": "MAIN-PARA",
                    "citation": [1, 5],
                    "purpose": "Main",
                },
            ],
        )
        result = split_paragraphs(source, doc_json)  # no output_dir

        expected_dir = tmp_path / "TESTPROG.cbl.d"
        assert expected_dir.exists()
        assert len(result) == 1
        assert result[0].parent == expected_dir

    def test_dead_code_paragraphs_are_still_split(self, tmp_path):
        """Dead code paragraphs are split (is_dead_code is ignored)."""
        source = _make_source_file(tmp_path / "TESTPROG.cbl", num_lines=50)
        doc_json = _make_doc_json(
            tmp_path / "TESTPROG.cbl.doc.json",
            paragraphs=[
                {
                    "paragraph_name": "DEAD-PARA",
                    "citation": [1, 5],
                    "purpose": "Dead code paragraph",
                    "is_dead_code": True,
                    "dead_code_reason": "Never called",
                },
            ],
        )
        output_dir = tmp_path / "out"
        result = split_paragraphs(source, doc_json, output_dir)

        assert len(result) == 1
        assert result[0].name == "DEAD-PARA.cbl"


# ---------------------------------------------------------------------------
# TestPatchMarkdownLinks
# ---------------------------------------------------------------------------


class TestPatchMarkdownLinks:
    """Tests for patch_markdown_links()."""

    def _setup_split_dir(self, tmp_path: Path, names: list[str]) -> Path:
        """Create a split directory with .cbl files for the given names."""
        split_dir = tmp_path / "TESTPROG.cbl.d"
        split_dir.mkdir(parents=True, exist_ok=True)
        for name in names:
            (split_dir / f"{name}.cbl").write_text(f"SOURCE FOR {name}\n")
        return split_dir

    def test_happy_path_inserts_links(self, tmp_path):
        """Source links are inserted after paragraph headings."""
        split_dir = self._setup_split_dir(tmp_path, ["MAIN-PARA", "1000-INIT"])
        md_content = (
            "## Key Paragraphs\n"
            "\n"
            "### MAIN-PARA\n"
            "Main logic description.\n"
            "\n"
            "### 1000-INIT\n"
            "Initialization description.\n"
        )
        md_path = _make_markdown(tmp_path / "TESTPROG.cbl.md", md_content)

        result = patch_markdown_links(md_path, split_dir)

        assert result is True
        patched = md_path.read_text()
        assert "> [Source:" in patched
        assert "MAIN-PARA.cbl" in patched
        assert "1000-INIT.cbl" in patched

    def test_dead_code_heading_gets_link(self, tmp_path):
        """Dead code headings like ### ~~NAME~~ (Dead Code) get correct link."""
        split_dir = self._setup_split_dir(tmp_path, ["9999-EXIT"])
        md_content = (
            "## Key Paragraphs\n"
            "\n"
            "### ~~9999-EXIT~~ (Dead Code)\n"
            "Never called.\n"
        )
        md_path = _make_markdown(tmp_path / "TESTPROG.cbl.md", md_content)

        result = patch_markdown_links(md_path, split_dir)

        assert result is True
        patched = md_path.read_text()
        assert "9999-EXIT.cbl" in patched

    def test_idempotency(self, tmp_path):
        """Running twice does not duplicate links."""
        split_dir = self._setup_split_dir(tmp_path, ["MAIN-PARA"])
        md_content = (
            "## Key Paragraphs\n"
            "\n"
            "### MAIN-PARA\n"
            "Main logic.\n"
        )
        md_path = _make_markdown(tmp_path / "TESTPROG.cbl.md", md_content)

        patch_markdown_links(md_path, split_dir)
        first_pass = md_path.read_text()

        patch_markdown_links(md_path, split_dir)
        second_pass = md_path.read_text()

        assert first_pass == second_pass
        # Should have exactly one source link
        assert second_pass.count("> [Source:") == 1

    def test_no_matching_files_returns_false(self, tmp_path):
        """When no split files match headings, returns False and markdown is unchanged."""
        split_dir = self._setup_split_dir(tmp_path, ["NONEXISTENT-PARA"])
        md_content = (
            "## Key Paragraphs\n"
            "\n"
            "### MAIN-PARA\n"
            "Main logic.\n"
        )
        md_path = _make_markdown(tmp_path / "TESTPROG.cbl.md", md_content)
        original = md_path.read_text()

        result = patch_markdown_links(md_path, split_dir)

        assert result is False
        assert md_path.read_text() == original

    def test_mixed_some_match_some_not(self, tmp_path):
        """Only paragraphs with matching split files get links."""
        split_dir = self._setup_split_dir(tmp_path, ["MAIN-PARA"])
        md_content = (
            "## Key Paragraphs\n"
            "\n"
            "### MAIN-PARA\n"
            "Main logic.\n"
            "\n"
            "### 1000-INIT\n"
            "Init logic.\n"
        )
        md_path = _make_markdown(tmp_path / "TESTPROG.cbl.md", md_content)

        result = patch_markdown_links(md_path, split_dir)

        assert result is True
        patched = md_path.read_text()
        assert "MAIN-PARA.cbl" in patched
        assert "1000-INIT.cbl" not in patched

    def test_does_not_patch_input_output_headings(self, tmp_path):
        """### headings in Inputs/Outputs sections are NOT patched."""
        # Create split files that coincidentally match an input/output name
        split_dir = self._setup_split_dir(
            tmp_path, ["PENDING-AUTH-SUMMARY", "MAIN-PARA"]
        )
        md_content = (
            "## Inputs\n"
            "\n"
            "### PENDING-AUTH-SUMMARY\n"
            "IMS segment for auth summaries.\n"
            "\n"
            "## Paragraphs/Procedures\n"
            "\n"
            "### MAIN-PARA\n"
            "Main logic.\n"
        )
        md_path = _make_markdown(tmp_path / "TESTPROG.cbl.md", md_content)

        patch_markdown_links(md_path, split_dir)
        patched = md_path.read_text()

        # MAIN-PARA in Paragraphs section should get a link
        assert "MAIN-PARA.cbl" in patched
        # PENDING-AUTH-SUMMARY in Inputs section should NOT get a link
        # Count source links: should be 1 (only MAIN-PARA)
        assert patched.count("> [Source:") == 1


# ---------------------------------------------------------------------------
# Regex pattern tests
# ---------------------------------------------------------------------------


class TestParagraphHeadingRegex:
    """Tests for the _PARAGRAPH_HEADING_RE regex pattern."""

    def test_matches_standard_heading(self):
        """Matches ### PARAGRAPH-NAME format."""
        match = _PARAGRAPH_HEADING_RE.match("### MAIN-PARA")
        assert match is not None
        assert match.group(1) == "MAIN-PARA"

    def test_matches_numeric_prefix(self):
        """Matches ### 1000-INITIALIZE format."""
        match = _PARAGRAPH_HEADING_RE.match("### 1000-INITIALIZE")
        assert match is not None
        assert match.group(1) == "1000-INITIALIZE"

    def test_matches_dead_code_heading(self):
        """Matches ### ~~NAME~~ (Dead Code) format."""
        match = _PARAGRAPH_HEADING_RE.match("### ~~9999-EXIT~~ (Dead Code)")
        assert match is not None
        assert match.group(1) == "9999-EXIT"

    def test_no_match_for_h2(self):
        """Does not match ## level headings."""
        match = _PARAGRAPH_HEADING_RE.match("## Key Paragraphs")
        assert match is None

    def test_no_match_for_plain_text(self):
        """Does not match plain text."""
        match = _PARAGRAPH_HEADING_RE.match("MAIN-PARA")
        assert match is None


class TestSourceLinkRegex:
    """Tests for the _SOURCE_LINK_RE regex pattern."""

    def test_matches_source_link(self):
        """Matches > [Source: ...] lines."""
        match = _SOURCE_LINK_RE.match(
            "> [Source: MAIN-PARA.cbl](TESTPROG.cbl.d/MAIN-PARA.cbl)"
        )
        assert match is not None

    def test_no_match_for_heading(self):
        """Does not match heading lines."""
        match = _SOURCE_LINK_RE.match("### MAIN-PARA")
        assert match is None


# ---------------------------------------------------------------------------
# TestSplitAllInDirectory
# ---------------------------------------------------------------------------


class TestSplitAllInDirectory:
    """Tests for split_all_in_directory()."""

    def test_multiple_doc_json_processed(self, tmp_path):
        """Multiple .doc.json files are processed."""
        source_dir = tmp_path / "src"
        source_dir.mkdir()
        doc_dir = tmp_path / "docs"
        doc_dir.mkdir()

        # Create two source/doc pairs
        _make_source_file(source_dir / "PROG1.cbl", num_lines=30)
        _make_doc_json(
            doc_dir / "PROG1.cbl.doc.json",
            file_name="PROG1.cbl",
            paragraphs=[
                {"paragraph_name": "MAIN-PARA", "citation": [1, 10], "purpose": "Main"},
            ],
        )

        _make_source_file(source_dir / "PROG2.cbl", num_lines=30)
        _make_doc_json(
            doc_dir / "PROG2.cbl.doc.json",
            file_name="PROG2.cbl",
            paragraphs=[
                {"paragraph_name": "INIT", "citation": [1, 5], "purpose": "Init"},
            ],
        )

        result = split_all_in_directory(source_dir, doc_dir)

        assert len(result) == 2
        assert "PROG1.cbl" in result or any("PROG1" in k for k in result)
        assert "PROG2.cbl" in result or any("PROG2" in k for k in result)

    def test_missing_source_files_skipped(self, tmp_path):
        """Doc.json files without matching source files are handled gracefully."""
        source_dir = tmp_path / "src"
        source_dir.mkdir()
        doc_dir = tmp_path / "docs"
        doc_dir.mkdir()

        # Only create doc.json, no source file
        _make_doc_json(
            doc_dir / "MISSING.cbl.doc.json",
            file_name="MISSING.cbl",
            paragraphs=[
                {"paragraph_name": "MAIN", "citation": [1, 5], "purpose": "Main"},
            ],
        )

        result = split_all_in_directory(source_dir, doc_dir)

        # Should not crash; result for MISSING.cbl should be empty or absent
        assert isinstance(result, dict)


# ---------------------------------------------------------------------------
# TestPatchAllMarkdownInDirectory
# ---------------------------------------------------------------------------


class TestPatchAllMarkdownInDirectory:
    """Tests for patch_all_markdown_in_directory()."""

    def test_multiple_md_files_patched(self, tmp_path):
        """Multiple .cbl.md files are patched."""
        doc_dir = tmp_path / "docs"
        doc_dir.mkdir()

        # Create split dirs and markdown for two programs
        for prog in ("PROG1", "PROG2"):
            split_dir = doc_dir / f"{prog}.cbl.d"
            split_dir.mkdir()
            (split_dir / "MAIN-PARA.cbl").write_text("SOURCE\n")

            md_content = (
                "## Paragraphs/Procedures\n"
                "\n"
                "### MAIN-PARA\n"
                "Description.\n"
            )
            _make_markdown(doc_dir / f"{prog}.cbl.md", md_content)

        count = patch_all_markdown_in_directory(doc_dir)

        assert count == 2

    def test_returns_correct_count(self, tmp_path):
        """Returns count of modified files, not total files."""
        doc_dir = tmp_path / "docs"
        doc_dir.mkdir()

        # Create one with matching split files, one without
        split_dir = doc_dir / "PROG1.cbl.d"
        split_dir.mkdir()
        (split_dir / "MAIN-PARA.cbl").write_text("SOURCE\n")
        _make_markdown(
            doc_dir / "PROG1.cbl.md",
            "## Key Paragraphs\n\n### MAIN-PARA\nDesc.\n",
        )

        # PROG2 has no split dir
        _make_markdown(
            doc_dir / "PROG2.cbl.md",
            "## Key Paragraphs\n\n### MAIN-PARA\nDesc.\n",
        )

        count = patch_all_markdown_in_directory(doc_dir)

        # Only PROG1 should be patched
        assert count == 1


# ---------------------------------------------------------------------------
# TestSplitAndLink
# ---------------------------------------------------------------------------


class TestSplitAndLink:
    """Tests for split_and_link()."""

    def test_combines_split_and_patch(self, tmp_path):
        """Convenience wrapper splits and patches in one call."""
        source = _make_source_file(tmp_path / "TESTPROG.cbl", num_lines=50)
        doc_json = _make_doc_json(
            tmp_path / "TESTPROG.cbl.doc.json",
            paragraphs=[
                {
                    "paragraph_name": "MAIN-PARA",
                    "citation": [1, 10],
                    "purpose": "Main",
                },
            ],
        )
        md_content = (
            "## Paragraphs/Procedures\n"
            "\n"
            "### MAIN-PARA\n"
            "Description.\n"
        )
        md_path = _make_markdown(tmp_path / "TESTPROG.cbl.md", md_content)

        result = split_and_link(source, doc_json, md_path)

        assert len(result) == 1
        # Verify split file exists
        assert result[0].exists()
        # Verify markdown was patched
        patched = md_path.read_text()
        assert "> [Source:" in patched

    def test_non_cobol_file_returns_empty(self, tmp_path):
        """Non-COBOL source files return an empty list."""
        source = tmp_path / "TESTPROG.py"
        source.write_text("print('hello')\n")
        doc_json = _make_doc_json(
            tmp_path / "TESTPROG.py.doc.json",
            file_name="TESTPROG.py",
            paragraphs=[
                {
                    "paragraph_name": "MAIN-PARA",
                    "citation": [1, 1],
                    "purpose": "Main",
                },
            ],
        )
        md_path = _make_markdown(tmp_path / "TESTPROG.py.md", "### MAIN-PARA\n")

        result = split_and_link(source, doc_json, md_path)

        assert result == []

    def test_cobol_extensions_accepted(self, tmp_path):
        """Various COBOL extensions (.cbl, .cob, .cobol, .CBL) are accepted."""
        for ext in (".cbl", ".cob", ".cobol", ".CBL"):
            name = f"PROG{ext}"
            source = _make_source_file(tmp_path / name, num_lines=10)
            doc_json = _make_doc_json(
                tmp_path / f"{name}.doc.json",
                file_name=name,
                paragraphs=[
                    {
                        "paragraph_name": "MAIN",
                        "citation": [1, 5],
                        "purpose": "Main",
                    },
                ],
            )
            md_path = _make_markdown(
                tmp_path / f"{name}.md",
                "## Key Paragraphs\n\n### MAIN\nDesc.\n",
            )

            result = split_and_link(source, doc_json, md_path)
            assert len(result) >= 1, f"Extension {ext} should be accepted"


# ---------------------------------------------------------------------------
# TestRunBatchSplit
# ---------------------------------------------------------------------------


class TestRunBatchSplit:
    """Tests for run_batch_split()."""

    def test_batch_split_creates_output(self, tmp_path):
        """Integration: batch split creates paragraph files and patches markdown."""
        input_dir = tmp_path / "input"
        input_dir.mkdir()
        output_dir = tmp_path / "output"
        output_dir.mkdir()

        # Create realistic source file
        _make_source_file(input_dir / "MYPROG.cbl", num_lines=100)

        # Create doc.json and markdown in output dir
        _make_doc_json(
            output_dir / "MYPROG.cbl.doc.json",
            file_name="MYPROG.cbl",
            paragraphs=[
                {
                    "paragraph_name": "MAIN-PARA",
                    "citation": [10, 25],
                    "purpose": "Main logic",
                },
                {
                    "paragraph_name": "1000-INIT",
                    "citation": [26, 40],
                    "purpose": "Initialization",
                },
            ],
        )
        _make_markdown(
            output_dir / "MYPROG.cbl.md",
            (
                "## Paragraphs/Procedures\n"
                "\n"
                "### MAIN-PARA\n"
                "Main description.\n"
                "\n"
                "### 1000-INIT\n"
                "Init description.\n"
            ),
        )

        run_batch_split(input_dir, output_dir)

        # Verify split directory was created
        split_dir = output_dir / "MYPROG.cbl.d"
        assert split_dir.exists()

        # Verify split files were created
        assert (split_dir / "MAIN-PARA.cbl").exists()
        assert (split_dir / "1000-INIT.cbl").exists()

        # Verify markdown was patched with source links
        md_text = (output_dir / "MYPROG.cbl.md").read_text()
        assert "> [Source:" in md_text


# ---------------------------------------------------------------------------
# Integration: roundtrip test
# ---------------------------------------------------------------------------


class TestIntegration:
    """Integration tests combining split and patch operations."""

    def test_split_and_link_roundtrip(self, tmp_path):
        """Full roundtrip: split paragraphs then patch markdown links."""
        source = _make_source_file(tmp_path / "TESTPROG.cbl", num_lines=100)
        doc_json = _make_doc_json(
            tmp_path / "TESTPROG.cbl.doc.json",
            paragraphs=[
                {
                    "paragraph_name": "MAIN-PARA",
                    "citation": [10, 25],
                    "purpose": "Main",
                },
                {
                    "paragraph_name": "1000-INIT",
                    "citation": [30, 45],
                    "purpose": "Init",
                },
                {
                    "paragraph_name": "9999-EXIT",
                    "citation": [90, 92],
                    "purpose": "Exit",
                    "is_dead_code": True,
                    "dead_code_reason": "Never called",
                },
            ],
        )
        md_content = (
            "# TESTPROG\n"
            "\n"
            "## Inputs\n"
            "\n"
            "### SOME-INPUT\n"
            "Input description.\n"
            "\n"
            "## Key Paragraphs\n"
            "\n"
            "### MAIN-PARA\n"
            "Main logic.\n"
            "\n"
            "### 1000-INIT\n"
            "Init logic.\n"
            "\n"
            "### ~~9999-EXIT~~ (Dead Code)\n"
            "Never called.\n"
        )
        md_path = _make_markdown(tmp_path / "TESTPROG.cbl.md", md_content)

        output_dir = tmp_path / "TESTPROG.cbl.d"

        # Step 1: Split
        split_files = split_paragraphs(source, doc_json, output_dir)
        assert len(split_files) == 3

        # Step 2: Patch
        modified = patch_markdown_links(md_path, output_dir)
        assert modified is True

        # Verify all links point to files that exist
        patched = md_path.read_text()
        for split_file in split_files:
            assert split_file.name in patched

        # Verify split files contain correct lines
        main_content = (output_dir / "MAIN-PARA.cbl").read_text()
        assert "LINE 010" in main_content
        assert "LINE 025" in main_content

        init_content = (output_dir / "1000-INIT.cbl").read_text()
        assert "LINE 030" in init_content
        assert "LINE 045" in init_content

        exit_content = (output_dir / "9999-EXIT.cbl").read_text()
        assert "LINE 090" in exit_content
        assert "LINE 092" in exit_content

    def test_batch_directory_roundtrip(self, tmp_path):
        """Batch: process multiple files then patch all markdown."""
        source_dir = tmp_path / "src"
        source_dir.mkdir()
        doc_dir = tmp_path / "docs"
        doc_dir.mkdir()

        for prog in ("ALPHA", "BRAVO"):
            _make_source_file(source_dir / f"{prog}.cbl", num_lines=50)
            _make_doc_json(
                doc_dir / f"{prog}.cbl.doc.json",
                file_name=f"{prog}.cbl",
                paragraphs=[
                    {
                        "paragraph_name": "MAIN-PARA",
                        "citation": [1, 10],
                        "purpose": "Main",
                    },
                    {
                        "paragraph_name": "1000-INIT",
                        "citation": [15, 25],
                        "purpose": "Init",
                    },
                ],
            )
            _make_markdown(
                doc_dir / f"{prog}.cbl.md",
                (
                    "## Paragraphs/Procedures\n"
                    "\n"
                    "### MAIN-PARA\n"
                    "Main logic.\n"
                    "\n"
                    "### 1000-INIT\n"
                    "Init logic.\n"
                ),
            )

        # Split all
        results = split_all_in_directory(source_dir, doc_dir)
        assert len(results) == 2

        # Patch all
        count = patch_all_markdown_in_directory(doc_dir)
        assert count == 2

        # Verify both markdown files were patched
        for prog in ("ALPHA", "BRAVO"):
            md_text = (doc_dir / f"{prog}.cbl.md").read_text()
            assert "> [Source:" in md_text
