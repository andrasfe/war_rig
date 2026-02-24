# CIPAUSMY

**File**: `cpy/CIPAUSMY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-02-24 17:38:11.574180

## Purpose

This copybook defines the data structure for the IMS segment 'PENDING AUTHORIZATION SUMMARY'. It contains fields related to account ID, customer ID, authorization status, account status, credit and cash limits/balances, and authorization counts/amounts. The copybook is used to represent pending authorization information within an IMS database.

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

- ? How is the FILLER field (line 31) used? What kind of data does it contain?
  - Context: The purpose of the FILLER field is unclear from the copybook definition.
