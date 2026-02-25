# CIPAUSMY

**File**: `cpy/CIPAUSMY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-02-25 15:31:01.581161

## Purpose

This copybook defines the data structure for the IMS segment named PENDING AUTHORIZATION SUMMARY. It specifies fields for account identification, customer ID, authorization and account statuses, credit and cash limits and balances, and summary counts and amounts for approved and declined authorizations. The structure is used to store and retrieve pending authorization summary data in an IMS database.

**Business Context**: Supports business processes involving authorization tracking for customer accounts, including limits, balances, and transaction approval/decline summaries in a mainframe IMS environment.

## Business Rules

- **BR001**: PA-ACCT-ID serves as the primary key for the IMS PENDING AUTHORIZATION SUMMARY segment, uniquely identifying the account.
- **BR002**: PA-ACCOUNT-STATUS is an array storing up to 5 account status values.
- **BR003**: Credit and cash limits, balances, and authorization counts/amounts are tracked separately for approved and declined transactions.

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

- ? Which programs or modules include and use this CIPAUSMY copybook?
  - Context: Copybook defines structure but does not indicate including programs.
- ? What are the exact usage contexts for fields like PA-AUTH-STATUS and PA-ACCOUNT-STATUS in parent programs?
  - Context: Field purposes inferred from names but not explicitly documented beyond structure.
