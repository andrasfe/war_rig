"""Router for deciding between direct and chunked analysis.

The AnalysisRouter examines source files and determines whether they
should be processed directly by War Rig's normal pipeline or routed
through Atlas for chunked processing.

Decision criteria:
- If estimated tokens <= context_budget: Direct processing
- If estimated tokens > context_budget: Chunked via Atlas
"""

from __future__ import annotations

import logging
from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from war_rig.config import WarRigConfig

logger = logging.getLogger(__name__)


class AnalysisRoute(Enum):
    """Routes for analysis processing."""

    DIRECT = "direct"
    """File fits in context - use normal War Rig pipeline."""

    CHUNKED = "chunked"
    """File exceeds context budget - use Atlas chunking."""

    SKIP = "skip"
    """File should be skipped (e.g., binary, unsupported type)."""


@dataclass
class RouteDecision:
    """Result of routing decision for a file.

    Attributes:
        route: The recommended processing route.
        file_path: Path to the source file.
        estimated_tokens: Estimated token count.
        context_budget: Configured context budget.
        reason: Human-readable explanation of decision.
    """

    route: AnalysisRoute
    file_path: Path
    estimated_tokens: int
    context_budget: int
    reason: str

    @property
    def needs_chunking(self) -> bool:
        """Check if this file requires chunked processing."""
        return self.route == AnalysisRoute.CHUNKED


class AnalysisRouter:
    """Routes files to appropriate processing pipelines.

    The router estimates token counts for source files and determines
    whether they should be processed:
    - DIRECT: File fits in context budget, use normal War Rig pipeline
    - CHUNKED: File exceeds budget, use Atlas for chunked processing
    - SKIP: File is binary, unsupported, or otherwise not processable

    Attributes:
        context_budget: Maximum tokens before requiring chunking.
        chars_per_token: Characters per token for estimation.
        enabled: Whether Atlas chunking is enabled.

    Example:
        >>> router = AnalysisRouter(config)
        >>> decision = router.route(Path("LARGE_PROGRAM.cbl"))
        >>> if decision.needs_chunking:
        ...     # Use Atlas orchestration
        ...     pass
        >>> else:
        ...     # Use normal War Rig pipeline
        ...     pass
    """

    # Average characters per token for estimation
    CHARS_PER_TOKEN: int = 4

    # File extensions that should be processed
    PROCESSABLE_EXTENSIONS: set[str] = {
        ".cbl", ".cob", ".CBL", ".COB",  # COBOL
        ".cpy", ".CPY", ".copy", ".COPY",  # Copybooks
        ".jcl", ".JCL",  # JCL
        ".pli", ".PLI", ".pl1", ".PL1",  # PL/I
        ".bms", ".BMS",  # BMS maps
    }

    # File extensions that should always be skipped
    BINARY_EXTENSIONS: set[str] = {
        ".exe", ".dll", ".so", ".dylib",
        ".zip", ".tar", ".gz", ".jar",
        ".png", ".jpg", ".jpeg", ".gif", ".bmp",
        ".pdf", ".doc", ".docx", ".xls", ".xlsx",
        ".class", ".o", ".obj", ".pyc",
    }

    def __init__(self, config: WarRigConfig) -> None:
        """Initialize the router.

        Args:
            config: War Rig configuration with Atlas settings.
        """
        self.context_budget = config.atlas_context_budget
        self.max_chunk_tokens = config.atlas_max_chunk_tokens
        self.enabled = config.atlas_enabled
        self.semantic_chunking = config.atlas_semantic_chunking

    def route(self, file_path: Path) -> RouteDecision:
        """Determine the processing route for a file.

        Args:
            file_path: Path to the source file.

        Returns:
            RouteDecision with recommended route and reasoning.
        """
        # Check if Atlas is enabled
        if not self.enabled:
            return RouteDecision(
                route=AnalysisRoute.DIRECT,
                file_path=file_path,
                estimated_tokens=0,
                context_budget=self.context_budget,
                reason="Atlas chunking disabled - using direct processing",
            )

        # Check file extension
        suffix = file_path.suffix.lower()

        # Skip binary files
        if suffix in self.BINARY_EXTENSIONS:
            return RouteDecision(
                route=AnalysisRoute.SKIP,
                file_path=file_path,
                estimated_tokens=0,
                context_budget=self.context_budget,
                reason=f"Binary file type: {suffix}",
            )

        # Check if file is processable
        if suffix not in {ext.lower() for ext in self.PROCESSABLE_EXTENSIONS}:
            return RouteDecision(
                route=AnalysisRoute.SKIP,
                file_path=file_path,
                estimated_tokens=0,
                context_budget=self.context_budget,
                reason=f"Unsupported file type: {suffix}",
            )

        # Read file and estimate tokens
        try:
            content = file_path.read_text(encoding="utf-8", errors="replace")
        except Exception as e:
            logger.warning(f"Failed to read {file_path}: {e}")
            return RouteDecision(
                route=AnalysisRoute.SKIP,
                file_path=file_path,
                estimated_tokens=0,
                context_budget=self.context_budget,
                reason=f"Failed to read file: {e}",
            )

        # Estimate tokens
        estimated_tokens = self.estimate_tokens(content)

        # Make routing decision
        if estimated_tokens <= self.context_budget:
            return RouteDecision(
                route=AnalysisRoute.DIRECT,
                file_path=file_path,
                estimated_tokens=estimated_tokens,
                context_budget=self.context_budget,
                reason=(
                    f"File fits in context: {estimated_tokens} tokens "
                    f"<= {self.context_budget} budget"
                ),
            )
        else:
            return RouteDecision(
                route=AnalysisRoute.CHUNKED,
                file_path=file_path,
                estimated_tokens=estimated_tokens,
                context_budget=self.context_budget,
                reason=(
                    f"File exceeds context: {estimated_tokens} tokens "
                    f"> {self.context_budget} budget - needs chunking"
                ),
            )

    def estimate_tokens(self, text: str) -> int:
        """Estimate token count for text.

        Uses a heuristic of ~4 characters per token.

        Args:
            text: Text to estimate tokens for.

        Returns:
            Estimated token count.
        """
        return max(1, len(text) // self.CHARS_PER_TOKEN)

    def route_batch(self, file_paths: list[Path]) -> dict[AnalysisRoute, list[Path]]:
        """Route multiple files and group by route.

        Args:
            file_paths: List of file paths to route.

        Returns:
            Dictionary mapping route types to lists of files.
        """
        result: dict[AnalysisRoute, list[Path]] = {
            AnalysisRoute.DIRECT: [],
            AnalysisRoute.CHUNKED: [],
            AnalysisRoute.SKIP: [],
        }

        for path in file_paths:
            decision = self.route(path)
            result[decision.route].append(path)

        # Log summary
        logger.info(
            f"Routing results: {len(result[AnalysisRoute.DIRECT])} direct, "
            f"{len(result[AnalysisRoute.CHUNKED])} chunked, "
            f"{len(result[AnalysisRoute.SKIP])} skipped"
        )

        return result

    def get_routing_summary(
        self,
        file_paths: list[Path],
    ) -> dict[str, int | list[str]]:
        """Generate a summary of routing decisions.

        Args:
            file_paths: Files to analyze.

        Returns:
            Summary dictionary with counts and file lists.
        """
        decisions = [self.route(p) for p in file_paths]

        direct_files = [d.file_path.name for d in decisions if d.route == AnalysisRoute.DIRECT]
        chunked_files = [d.file_path.name for d in decisions if d.route == AnalysisRoute.CHUNKED]
        skipped_files = [d.file_path.name for d in decisions if d.route == AnalysisRoute.SKIP]

        total_tokens = sum(d.estimated_tokens for d in decisions)

        return {
            "total_files": len(file_paths),
            "direct_count": len(direct_files),
            "chunked_count": len(chunked_files),
            "skipped_count": len(skipped_files),
            "total_estimated_tokens": total_tokens,
            "context_budget": self.context_budget,
            "direct_files": direct_files,
            "chunked_files": chunked_files,
            "skipped_files": skipped_files,
        }
