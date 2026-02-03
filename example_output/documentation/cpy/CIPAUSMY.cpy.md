# CIPAUSMY

**File**: `cpy/CIPAUSMY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-02-03 21:07:10.910765

## Purpose

This copybook defines the data structure for the IMS segment related to pending authorization summary information. It includes fields for account ID, customer ID, authorization status, account status, credit and cash limits/balances, and counts/amounts for approved and declined authorizations. The copybook is used for representing pending authorization data within an IMS database.

## Paragraphs/Procedures

### N/A
This copybook does not contain any paragraphs. It is a data structure definition used to define the layout of an IMS segment. The copybook defines fields such as PA-ACCT-ID (account ID), PA-CUST-ID (customer ID), PA-AUTH-STATUS (authorization status), PA-ACCOUNT-STATUS (account status), credit and cash limits and balances, and approved and declined authorization counts and amounts. These fields are used to store and retrieve pending authorization summary data from an IMS database. The copybook itself does not contain any executable logic or error handling. It is purely a data definition.

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| 05:PA-ACCT-ID | column | 1 | Artifact '05:PA-ACCT-ID' (column) is never referenced by any other artifact in the dependency graph |
