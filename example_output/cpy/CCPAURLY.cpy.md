# CCPAURLY

**File**: `cpy/CCPAURLY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-02-09 15:47:24.467088

## Purpose

This copybook defines the data structure for a pending authorization response, including fields for card number, transaction ID, authorization ID code, response code and reason, and approved amount. It also includes copyright and licensing information under the Apache License 2.0.

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

- ? How is the data structure defined in this copybook used in a larger program context?
  - Context: The copybook only defines the structure, but its usage is unclear without the context of a calling program.
