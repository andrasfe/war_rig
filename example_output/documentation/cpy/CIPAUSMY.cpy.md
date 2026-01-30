# CIPAUSMY

**File**: `cpy/CIPAUSMY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-30 19:46:36.136517

## Purpose

This copybook defines the data structure for the IMS segment 'PENDING AUTHORIZATION SUMMARY'. It includes fields for account ID (PA-ACCT-ID), customer ID (PA-CUST-ID), authorization status (PA-AUTH-STATUS), an array of account statuses (PA-ACCOUNT-STATUS OCCURS 5 TIMES), credit and cash limits and balances, counts of approved/declined authorizations, and corresponding amounts. A filler field pads the structure to the required length.

**Business Context**: Supports management of pending authorizations for customer accounts in an IMS database, tracking statuses, limits, balances, and authorization history for credit and cash transactions.

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
