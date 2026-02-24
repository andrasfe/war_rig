# CIPAUSMY

**File**: `cpy/CIPAUSMY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-02-24 04:00:35.645395

## Purpose

This copybook defines the layout for the IMS 'Pending Authorization Summary' segment. Key fields include account and customer identifiers, authorization and account statuses, credit/cash limits and balances, counts and amounts for approved/declined authorizations, and a filler area. It is used to structure data in an IMS database for authorization processing.

**Business Context**: Serves financial authorization workflows by summarizing pending authorizations per customer account, including limits, balances, and transaction statistics (evident from field names and IMS segment header at lines 1-3).

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

- ? In which section of calling programs is this copybook typically included?
  - Context: Copybook usage location (e.g., WORKING_STORAGE, LINKAGE) cannot be determined from the copybook itself.
