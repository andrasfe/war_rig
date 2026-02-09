# CIPAUSMY

**File**: `cpy/CIPAUSMY.cpy`
**Type**: COPYBOOK
**Analyzed**: 2026-02-09 15:46:56.140059

## Purpose

This copybook defines the structure of the IMS segment for pending authorization summary data. It includes fields for account ID, customer ID, authorization status, account statuses (occurs 5 times), credit and cash limits and balances, and counts and amounts for approved and declined authorizations. The copybook is used to map the layout of the data within the IMS database.

## Paragraphs/Procedures

### CIPAUSMY
[Citadel] Paragraph identified by static analysis

### ~~05:PA-ACCT-ID~~ (Dead Code)
*Artifact '05:PA-ACCT-ID' (column) is never referenced by any other artifact in the dependency graph*

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| 05:PA-ACCT-ID | column | 1 | Artifact '05:PA-ACCT-ID' (column) is never referenced by any other artifact in the dependency graph |

## Open Questions

- ? What programs use this copybook?
  - Context: The copybook defines an IMS segment, but the programs that read or write to this segment are unknown.
