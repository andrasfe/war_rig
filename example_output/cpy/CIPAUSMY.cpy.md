# CIPAUSMY

**File**: `cpy/CIPAUSMY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-03-04 04:43:46.272301

## Purpose

This copybook defines the structure of the IMS segment for Pending Authorization Summary. It contains fields related to account identification, customer identification, authorization status, account status, credit and cash limits, balances, and authorization counts and amounts. The segment is used to store information about pending authorizations for customer accounts. 

**Business Context**: This copybook is likely used in a financial system to manage and track pending authorizations for customer accounts, potentially related to credit card transactions or other financial transactions requiring authorization.

## Paragraphs/Procedures

### PA-ACCT-ID
This data field defines the Account ID for the Pending Authorization Summary segment. It is a COMP-3 field, meaning it is stored in packed decimal format, which is a space-efficient way to store numeric data. The field is 11 digits long, including the sign. This field is likely used to uniquely identify the account associated with the pending authorization.

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| 05:PA-ACCT-ID | column | 1 | Artifact '05:PA-ACCT-ID' (column) is never referenced by any other artifact in the dependency graph |
