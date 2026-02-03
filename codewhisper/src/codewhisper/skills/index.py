"""Skills index for CodeWhisper.

This module provides indexing and search capabilities for skills,
allowing the agent to quickly find relevant documentation based
on keywords and semantic similarity.

The index supports lazy loading - it starts with just the root skill
and loads category skills on-demand when requested. This enables
progressive disclosure without loading all skills upfront.

Example:
    from codewhisper.skills.loader import SkillsLoader
    from codewhisper.skills.index import SkillsIndex

    loader = SkillsLoader(Path("./skills"))
    index = SkillsIndex.from_loader(loader)  # Only loads root skill

    # Category skills are loaded on-demand
    skill = index.get("cobol")  # Loads cobol/SKILL.md if not cached

    # Search still works (searches root + cached skills)
    results = index.search("authorization")
"""

from __future__ import annotations

import logging
import re
from dataclasses import dataclass
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from codewhisper.skills.loader import Skill, SkillsLoader

logger = logging.getLogger(__name__)


@dataclass
class SearchResult:
    """Result from a skill search.

    Attributes:
        skill: The matching skill.
        score: Relevance score (higher is better).
        matched_terms: Terms that matched the query.
    """

    skill: Skill
    score: float
    matched_terms: list[str]


class SkillsIndex:
    """Index for searching skills with lazy loading support.

    Provides keyword-based search over skill names, descriptions,
    and content. Uses simple TF-IDF-like scoring.

    The index supports two modes:
    1. Eager: Pass a list of skills (loads all into memory)
    2. Lazy: Use from_loader() to start with root and load on-demand

    Attributes:
        skills: List of indexed skills.

    Example:
        # Lazy loading (preferred for hierarchical skills)
        loader = SkillsLoader(Path("./skills"))
        index = SkillsIndex.from_loader(loader)

        # Eager loading (for small skill sets)
        skills = loader.load_all(recursive=True)
        index = SkillsIndex(skills)

    TODO: Add vector-based semantic search
    TODO: Implement caching for repeated searches
    """

    def __init__(self, skills: list[Skill], loader: SkillsLoader | None = None):
        """Initialize the index with skills.

        Args:
            skills: List of skills to index initially.
            loader: Optional loader for on-demand category loading.
        """
        self._skills = {skill.name.lower(): skill for skill in skills}
        self._skill_list = list(skills)
        self._loader = loader

        # Build inverted index for keyword search
        self._inverted_index: dict[str, set[str]] = {}
        self._build_inverted_index()

        logger.info(f"SkillsIndex built with {len(skills)} skills")

    @classmethod
    def from_loader(cls, loader: SkillsLoader) -> SkillsIndex:
        """Create an index with lazy loading support.

        Only loads the root skill initially. Category skills are
        loaded on-demand when get() is called.

        Args:
            loader: The skills loader to use.

        Returns:
            SkillsIndex configured for lazy loading.
        """
        # Load only the root skill
        root_skill = loader.load_root()
        initial_skills = [root_skill] if root_skill else []

        return cls(initial_skills, loader=loader)

    def _build_inverted_index(self) -> None:
        """Build an inverted index for keyword search.

        TODO: Implement proper tokenization
        TODO: Add stemming/lemmatization
        TODO: Handle stopwords
        """
        for skill in self._skill_list:
            # Tokenize name, description, and content
            text = f"{skill.name} {skill.description} {skill.content}"
            tokens = self._tokenize(text)

            skill_key = skill.name.lower()
            for token in tokens:
                if token not in self._inverted_index:
                    self._inverted_index[token] = set()
                self._inverted_index[token].add(skill_key)

    def _tokenize(self, text: str) -> set[str]:
        """Tokenize text into searchable terms.

        Args:
            text: Text to tokenize.

        Returns:
            Set of lowercase tokens.

        TODO: Improve tokenization (handle hyphens, underscores, etc.)
        """
        # Simple word tokenization
        words = re.findall(r"\b\w+\b", text.lower())
        return set(words)

    def search(
        self,
        query: str,
        limit: int = 5,
    ) -> list[SearchResult]:
        """Search for skills matching the query.

        Only searches skills currently in the index. For progressive
        discovery, the LLM should use reasoning to navigate from
        root -> category -> specific documentation.

        Args:
            query: Search query (keywords).
            limit: Maximum results to return.

        Returns:
            List of SearchResult objects, sorted by relevance.

        TODO: Implement proper BM25 or TF-IDF scoring
        TODO: Add query expansion with synonyms
        """
        query_tokens = self._tokenize(query)

        if not query_tokens:
            return []

        # Score each skill
        scores: dict[str, tuple[float, list[str]]] = {}

        for token in query_tokens:
            if token in self._inverted_index:
                for skill_name in self._inverted_index[token]:
                    if skill_name not in scores:
                        scores[skill_name] = (0.0, [])

                    current_score, matched = scores[skill_name]
                    # Simple scoring: +1 for each matching token
                    # Weight name matches higher
                    skill = self._skills[skill_name]
                    if token in skill.name.lower():
                        current_score += 3.0  # Name match weighted higher
                    elif token in skill.description.lower():
                        current_score += 2.0  # Description match
                    else:
                        current_score += 1.0  # Content match

                    matched.append(token)
                    scores[skill_name] = (current_score, matched)

        # Sort by score and return top results
        sorted_results = sorted(
            scores.items(),
            key=lambda x: x[1][0],
            reverse=True,
        )[:limit]

        results = []
        for skill_name, (score, matched) in sorted_results:
            skill = self._skills[skill_name]
            results.append(
                SearchResult(
                    skill=skill,
                    score=score,
                    matched_terms=matched,
                )
            )

        return results

    def get(self, name: str) -> Skill | None:
        """Get a skill by exact name, with lazy loading support.

        If the skill is not in the index but a loader is configured,
        attempts to load it as a category skill.

        Args:
            name: Skill name (case-insensitive).

        Returns:
            The skill if found, None otherwise.
        """
        name_lower = name.lower()

        # Try exact match first
        result = self._skills.get(name_lower)
        if result:
            return result

        # Try partial match (name contains query or query contains name)
        for skill_name, skill in self._skills.items():
            if name_lower in skill_name or skill_name in name_lower:
                return skill

        # Try lazy loading as a category
        if self._loader is not None:
            skill = self._loader.load_category(name_lower)
            if skill:
                # Add to index for future lookups
                self._add_skill(skill)
                logger.info(f"Lazy-loaded category skill: {skill.name}")
                return skill

        return None

    def _add_skill(self, skill: Skill) -> None:
        """Add a skill to the index.

        Args:
            skill: Skill to add.
        """
        skill_key = skill.name.lower()
        if skill_key not in self._skills:
            self._skills[skill_key] = skill
            self._skill_list.append(skill)

            # Update inverted index
            text = f"{skill.name} {skill.description} {skill.content}"
            tokens = self._tokenize(text)
            for token in tokens:
                if token not in self._inverted_index:
                    self._inverted_index[token] = set()
                self._inverted_index[token].add(skill_key)

    def get_all(self) -> list[Skill]:
        """Get all indexed skills.

        Returns:
            List of all skills.
        """
        return self._skill_list

    def get_by_tag(self, tag: str) -> list[Skill]:
        """Get skills with a specific tag.

        Args:
            tag: Tag to filter by.

        Returns:
            List of skills with the given tag.

        TODO: Build tag index for efficiency
        """
        tag_lower = tag.lower()
        return [
            skill
            for skill in self._skill_list
            if skill.tags and tag_lower in [t.lower() for t in skill.tags]
        ]

    def list_names(self) -> list[str]:
        """List all skill names.

        Returns:
            Sorted list of skill names.
        """
        return sorted(self._skills.keys())

    def __len__(self) -> int:
        """Number of indexed skills."""
        return len(self._skills)

    def __contains__(self, name: str) -> bool:
        """Check if a skill exists by name."""
        return name.lower() in self._skills
