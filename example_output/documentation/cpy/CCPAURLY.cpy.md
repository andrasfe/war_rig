# CCPAURLY

**File**: `cpy/CCPAURLY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-30 19:43:13.422702

## Purpose

This COBOL copybook defines data structures at level 05 for a Pending Authorization Response record. It includes elementary fields for card number (PIC X(16)), transaction ID (PIC X(15)), authorization ID code (PIC X(06)), authorization response code (PIC X(02)), response reason (PIC X(04)), and approved amount (PIC +9(10).99). The fields share a 'PA-RL-' prefix, logically grouping them for use in payment authorization processing.

**Business Context**: Payment card authorization processing, capturing response details from authorization requests

## Paragraphs/Procedures

### CCPAURLY
[Citadel] Paragraph identified by static analysis

### ~~05:PA-RL-CARD-NUM~~ (Dead Code)
*Artifact '05:PA-RL-CARD-NUM' (column) is never referenced by any other artifact in the dependency graph*

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| 05:PA-RL-CARD-NUM | column | 1 | Artifact '05:PA-RL-CARD-NUM' (column) is never referenced by any other artifact in the dependency graph |

## Open Questions

- ? In which programs or sections (e.g., WORKING-STORAGE, LINKAGE) is this copybook included?
  - Context: Copybook usage cannot be determined from this file alone
- ? What are the exact validation rules applied to these fields in using programs?
  - Context: No logic present in copybook; only data definitions
