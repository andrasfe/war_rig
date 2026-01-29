# CCPAURLY

**File**: `cpy/CCPAURLY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-28 14:54:41.209966

## Purpose

This copybook defines the data structure for a pending authorization response related to card transactions. It includes fields for card number, transaction ID, authorization codes, response codes, reason codes, and the approved amount.

**Business Context**: This copybook is used in the context of processing card payments and handling authorization responses, likely within a financial transaction system.

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

- ? What is the specific purpose of the authorization response reason code?
  - Context: The copybook defines a field for 'PA-RL-AUTH-RESP-REASON', but its exact meaning and usage are unclear without additional context.
