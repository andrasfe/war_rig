"""Tests for skills index.

This module tests:
- SkillsIndex initialization
- Search functionality
- Skill retrieval by name
- Tag-based filtering
"""

from pathlib import Path

import pytest

from codewhisper.skills.index import SearchResult, SkillsIndex
from codewhisper.skills.loader import Skill


@pytest.fixture
def sample_skills() -> list[Skill]:
    """Create sample skills for testing."""
    return [
        Skill(
            name="cbpaup0c",
            description="Batch cleanup program for expired authorizations",
            content="# CBPAUP0C\n\nThis program removes expired authorization records.",
            file_path=Path("/skills/cbpaup0c/SKILL.md"),
            tags=["batch", "cleanup", "authorization"],
        ),
        Skill(
            name="copaus0c",
            description="Online authorization inquiry screen",
            content="# COPAUS0C\n\nCICS program for viewing authorization status.",
            file_path=Path("/skills/copaus0c/SKILL.md"),
            tags=["online", "cics", "inquiry"],
        ),
        Skill(
            name="system-overview",
            description="High-level system architecture overview",
            content="# System Overview\n\nThe authorization system handles...",
            file_path=Path("/skills/overview/SKILL.md"),
            tags=["documentation", "architecture"],
        ),
        Skill(
            name="call-graph",
            description="Program call relationships and dependencies",
            content="# Call Graph\n\nShows which programs call other programs.",
            file_path=Path("/skills/call-graph/SKILL.md"),
            tags=["documentation", "dependencies"],
        ),
    ]


class TestSkillsIndexInit:
    """Tests for SkillsIndex initialization."""

    def test_index_creation(self, sample_skills: list[Skill]) -> None:
        """Test creating a skills index."""
        index = SkillsIndex(sample_skills)

        assert len(index) == 4

    def test_index_empty(self) -> None:
        """Test creating empty index."""
        index = SkillsIndex([])

        assert len(index) == 0

    def test_index_contains(self, sample_skills: list[Skill]) -> None:
        """Test checking if skill exists in index."""
        index = SkillsIndex(sample_skills)

        assert "cbpaup0c" in index
        assert "nonexistent" not in index

    def test_index_contains_case_insensitive(self, sample_skills: list[Skill]) -> None:
        """Test that contains check is case-insensitive."""
        index = SkillsIndex(sample_skills)

        assert "CBPAUP0C" in index
        assert "CbPaUp0c" in index


class TestSkillsIndexSearch:
    """Tests for search functionality."""

    def test_search_by_name(self, sample_skills: list[Skill]) -> None:
        """Test searching by skill name."""
        index = SkillsIndex(sample_skills)

        results = index.search("cbpaup0c")

        assert len(results) > 0
        assert results[0].skill.name == "cbpaup0c"

    def test_search_by_description_keyword(self, sample_skills: list[Skill]) -> None:
        """Test searching by description keyword."""
        index = SkillsIndex(sample_skills)

        results = index.search("cleanup")

        assert len(results) > 0
        # CBPAUP0C should be in results (has "cleanup" in description)
        skill_names = [r.skill.name for r in results]
        assert "cbpaup0c" in skill_names

    def test_search_by_content_keyword(self, sample_skills: list[Skill]) -> None:
        """Test searching by content keyword."""
        index = SkillsIndex(sample_skills)

        results = index.search("CICS")

        assert len(results) > 0
        # COPAUS0C has CICS in content
        skill_names = [r.skill.name for r in results]
        assert "copaus0c" in skill_names

    def test_search_multiple_keywords(self, sample_skills: list[Skill]) -> None:
        """Test searching with multiple keywords."""
        index = SkillsIndex(sample_skills)

        results = index.search("authorization batch")

        assert len(results) > 0
        # CBPAUP0C should rank high (matches both keywords)
        assert results[0].skill.name == "cbpaup0c"

    def test_search_returns_search_results(self, sample_skills: list[Skill]) -> None:
        """Test that search returns SearchResult objects."""
        index = SkillsIndex(sample_skills)

        results = index.search("authorization")

        assert all(isinstance(r, SearchResult) for r in results)
        for result in results:
            assert hasattr(result, "skill")
            assert hasattr(result, "score")
            assert hasattr(result, "matched_terms")

    def test_search_results_have_scores(self, sample_skills: list[Skill]) -> None:
        """Test that search results have relevance scores."""
        index = SkillsIndex(sample_skills)

        results = index.search("authorization")

        assert all(r.score > 0 for r in results)

    def test_search_results_sorted_by_score(self, sample_skills: list[Skill]) -> None:
        """Test that results are sorted by score descending."""
        index = SkillsIndex(sample_skills)

        results = index.search("program")

        scores = [r.score for r in results]
        assert scores == sorted(scores, reverse=True)

    def test_search_with_limit(self, sample_skills: list[Skill]) -> None:
        """Test limiting number of results."""
        index = SkillsIndex(sample_skills)

        results = index.search("authorization", limit=2)

        assert len(results) <= 2

    def test_search_empty_query(self, sample_skills: list[Skill]) -> None:
        """Test search with empty query returns empty."""
        index = SkillsIndex(sample_skills)

        results = index.search("")

        assert results == []

    def test_search_no_matches(self, sample_skills: list[Skill]) -> None:
        """Test search with no matches returns empty."""
        index = SkillsIndex(sample_skills)

        results = index.search("xyznomatch123")

        assert results == []

    def test_search_records_matched_terms(self, sample_skills: list[Skill]) -> None:
        """Test that search records which terms matched."""
        index = SkillsIndex(sample_skills)

        results = index.search("batch cleanup")

        # CBPAUP0C should match both terms
        cbpaup_result = next(r for r in results if r.skill.name == "cbpaup0c")
        assert "batch" in cbpaup_result.matched_terms
        assert "cleanup" in cbpaup_result.matched_terms

    def test_search_name_weighted_higher(self, sample_skills: list[Skill]) -> None:
        """Test that name matches are weighted higher than content."""
        index = SkillsIndex(sample_skills)

        # "overview" is in the name of system-overview
        results = index.search("overview")

        # system-overview should rank first
        assert results[0].skill.name == "system-overview"


class TestSkillsIndexGet:
    """Tests for getting skills by name."""

    def test_get_existing_skill(self, sample_skills: list[Skill]) -> None:
        """Test getting a skill that exists."""
        index = SkillsIndex(sample_skills)

        skill = index.get("cbpaup0c")

        assert skill is not None
        assert skill.name == "cbpaup0c"

    def test_get_case_insensitive(self, sample_skills: list[Skill]) -> None:
        """Test that get is case-insensitive."""
        index = SkillsIndex(sample_skills)

        skill = index.get("CBPAUP0C")

        assert skill is not None
        assert skill.name == "cbpaup0c"

    def test_get_nonexistent(self, sample_skills: list[Skill]) -> None:
        """Test getting a skill that doesn't exist."""
        index = SkillsIndex(sample_skills)

        skill = index.get("nonexistent")

        assert skill is None


class TestSkillsIndexGetAll:
    """Tests for getting all skills."""

    def test_get_all_returns_all_skills(self, sample_skills: list[Skill]) -> None:
        """Test get_all returns all indexed skills."""
        index = SkillsIndex(sample_skills)

        all_skills = index.get_all()

        assert len(all_skills) == 4
        assert all(isinstance(s, Skill) for s in all_skills)


class TestSkillsIndexByTag:
    """Tests for tag-based filtering."""

    def test_get_by_tag(self, sample_skills: list[Skill]) -> None:
        """Test filtering skills by tag."""
        index = SkillsIndex(sample_skills)

        batch_skills = index.get_by_tag("batch")

        assert len(batch_skills) == 1
        assert batch_skills[0].name == "cbpaup0c"

    def test_get_by_tag_multiple_matches(self, sample_skills: list[Skill]) -> None:
        """Test filtering when multiple skills have the tag."""
        index = SkillsIndex(sample_skills)

        doc_skills = index.get_by_tag("documentation")

        assert len(doc_skills) == 2
        names = [s.name for s in doc_skills]
        assert "system-overview" in names
        assert "call-graph" in names

    def test_get_by_tag_case_insensitive(self, sample_skills: list[Skill]) -> None:
        """Test that tag filtering is case-insensitive."""
        index = SkillsIndex(sample_skills)

        skills = index.get_by_tag("BATCH")

        assert len(skills) == 1
        assert skills[0].name == "cbpaup0c"

    def test_get_by_tag_no_matches(self, sample_skills: list[Skill]) -> None:
        """Test filtering with non-matching tag."""
        index = SkillsIndex(sample_skills)

        skills = index.get_by_tag("nonexistent-tag")

        assert skills == []


class TestSkillsIndexListNames:
    """Tests for listing skill names."""

    def test_list_names(self, sample_skills: list[Skill]) -> None:
        """Test listing all skill names."""
        index = SkillsIndex(sample_skills)

        names = index.list_names()

        assert len(names) == 4
        assert "cbpaup0c" in names
        assert "copaus0c" in names
        assert "system-overview" in names
        assert "call-graph" in names

    def test_list_names_sorted(self, sample_skills: list[Skill]) -> None:
        """Test that names are sorted."""
        index = SkillsIndex(sample_skills)

        names = index.list_names()

        assert names == sorted(names)
