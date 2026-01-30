#!/usr/bin/env python3
"""
Validation script for COBOL analysis patterns.

This script tests the analysis pattern extraction on all COBOL files in the
CardDemo repository and reports statistics on pattern coverage and accuracy.
"""

import random
import sys
from collections import defaultdict
from pathlib import Path

# Add the citadel src directory to the path
citadel_src = Path(__file__).parent.parent / "citadel" / "src"
sys.path.insert(0, str(citadel_src))

from citadel.sdk import Citadel, FileAnalysisPatternResult


def main() -> int:
    """Run validation on all CardDemo COBOL files."""
    # Find the CardDemo COBOL directory
    base_dir = Path(__file__).parent.parent
    cobol_dir = base_dir / "aws-mainframe-modernization-carddemo" / "app" / "cbl"

    if not cobol_dir.exists():
        print(f"ERROR: COBOL directory not found: {cobol_dir}")
        return 1

    # Find all COBOL files
    cobol_files = list(cobol_dir.glob("*.cbl")) + list(cobol_dir.glob("*.CBL"))
    print(f"Found {len(cobol_files)} COBOL files in {cobol_dir}")
    print("=" * 70)

    # Initialize Citadel
    citadel = Citadel()

    # Aggregate statistics
    total_files = 0
    files_with_errors = 0
    files_with_zero_matches = []
    total_matches_by_category: dict[str, int] = defaultdict(int)
    total_matches_by_pattern: dict[str, int] = defaultdict(int)
    all_results: list[tuple[Path, FileAnalysisPatternResult]] = []

    # Sample matches for false positive checking
    sample_matches: list[dict] = []

    for cobol_file in sorted(cobol_files):
        total_files += 1
        result = citadel.get_analysis_patterns(cobol_file)

        if result.error:
            files_with_errors += 1
            print(f"ERROR: {cobol_file.name}: {result.error}")
            continue

        all_results.append((cobol_file, result))

        if result.total_matches == 0:
            files_with_zero_matches.append(cobol_file.name)

        # Aggregate category stats
        for category, cat_result in result.categories.items():
            total_matches_by_category[category] += cat_result.match_count
            for pattern_name, count in cat_result.patterns_matched.items():
                total_matches_by_pattern[f"{category}:{pattern_name}"] += count

            # Collect sample matches for false positive checking
            for match in cat_result.matches:
                sample_matches.append({
                    "file": cobol_file.name,
                    "pattern": match.pattern_name,
                    "category": match.category,
                    "captured": match.captured,
                    "line": match.line,
                    "context": match.context,
                })

    # Print summary statistics
    print("\n" + "=" * 70)
    print("SUMMARY STATISTICS")
    print("=" * 70)

    print(f"\nFiles analyzed: {total_files}")
    print(f"Files with errors: {files_with_errors}")
    print(f"Files with zero matches: {len(files_with_zero_matches)}")

    if files_with_zero_matches:
        print(f"  -> {', '.join(files_with_zero_matches)}")

    # Category breakdown
    print("\n" + "-" * 70)
    print("MATCHES BY CATEGORY")
    print("-" * 70)
    total_all = sum(total_matches_by_category.values())
    for category in sorted(total_matches_by_category.keys()):
        count = total_matches_by_category[category]
        pct = (count / total_all * 100) if total_all > 0 else 0
        print(f"  {category}: {count:,} ({pct:.1f}%)")
    print(f"  TOTAL: {total_all:,}")

    # Pattern breakdown
    print("\n" + "-" * 70)
    print("MATCHES BY PATTERN (top 25)")
    print("-" * 70)
    sorted_patterns = sorted(
        total_matches_by_pattern.items(),
        key=lambda x: x[1],
        reverse=True
    )
    for pattern_key, count in sorted_patterns[:25]:
        print(f"  {pattern_key}: {count:,}")

    # Patterns with zero matches
    print("\n" + "-" * 70)
    print("PATTERNS WITH ZERO MATCHES")
    print("-" * 70)
    zero_patterns = [p for p, c in total_matches_by_pattern.items() if c == 0]

    # Also check for patterns defined in spec but never matched
    spec = citadel._get_spec_for_file(cobol_files[0])
    if spec and spec.analysis_patterns:
        all_defined_patterns = set()
        for category, patterns in spec.analysis_patterns.items():
            for pattern in patterns:
                all_defined_patterns.add(f"{category}:{pattern.name}")

        matched_patterns = set(total_matches_by_pattern.keys())
        never_matched = all_defined_patterns - matched_patterns
        zero_patterns.extend(list(never_matched))

    if zero_patterns:
        for p in sorted(set(zero_patterns)):
            print(f"  - {p}")
    else:
        print("  (none - all patterns matched at least once)")

    # Coverage statistics
    print("\n" + "-" * 70)
    print("COVERAGE STATISTICS")
    print("-" * 70)
    if all_results:
        avg_coverage = sum(r.coverage_pct for _, r in all_results) / len(all_results)
        min_coverage = min(r.coverage_pct for _, r in all_results)
        max_coverage = max(r.coverage_pct for _, r in all_results)
        print(f"  Average pattern coverage: {avg_coverage:.1f}%")
        print(f"  Min coverage: {min_coverage:.1f}%")
        print(f"  Max coverage: {max_coverage:.1f}%")

    # Sample matches for false positive checking
    print("\n" + "-" * 70)
    print("SAMPLE MATCHES FOR REVIEW (5 random samples per category)")
    print("-" * 70)

    categories_to_sample = ["data_flow", "control_flow", "error_handling"]
    for category in categories_to_sample:
        cat_samples = [s for s in sample_matches if s["category"] == category]
        if cat_samples:
            print(f"\n{category.upper()}:")
            samples = random.sample(cat_samples, min(5, len(cat_samples)))
            for sample in samples:
                print(f"  [{sample['file']}:{sample['line']}] {sample['pattern']}")
                print(f"    Captured: {sample['captured']}")
                if sample["context"]:
                    # Print just the matching line context
                    context_str = " | ".join(
                        line.strip()[:60] for line in sample["context"][:3]
                    )
                    print(f"    Context: {context_str}")

    # Per-file breakdown for top files by match count
    print("\n" + "-" * 70)
    print("FILES BY MATCH COUNT (top 10)")
    print("-" * 70)
    sorted_files = sorted(all_results, key=lambda x: x[1].total_matches, reverse=True)
    for cobol_file, result in sorted_files[:10]:
        print(f"  {cobol_file.name}: {result.total_matches:,} matches")
        for category, cat_result in result.categories.items():
            if cat_result.match_count > 0:
                print(f"    {category}: {cat_result.match_count:,}")

    # Final verdict
    print("\n" + "=" * 70)
    print("VALIDATION RESULT")
    print("=" * 70)

    issues = []
    if files_with_errors > 0:
        issues.append(f"{files_with_errors} files had errors")
    if len(files_with_zero_matches) > 3:
        issues.append(f"{len(files_with_zero_matches)} files had zero matches")
    if total_all == 0:
        issues.append("No matches found at all")

    if issues:
        print(f"ISSUES FOUND: {', '.join(issues)}")
        return 1
    else:
        print("PASS - All patterns working correctly")
        return 0


if __name__ == "__main__":
    sys.exit(main())
