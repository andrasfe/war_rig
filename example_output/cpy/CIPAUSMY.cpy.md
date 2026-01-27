# CIPAUSMY

**File**: `cpy/CIPAUSMY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-27 23:05:09.642099

## Purpose

This copybook defines the data structure for the IMS segment related to pending authorization summary information. It includes fields for account ID, customer ID, authorization status, account status, credit and cash limits/balances, and counts/amounts for approved and declined authorizations.

**Business Context**: This copybook is used in applications that manage and process pending authorizations for customer accounts, likely within a financial institution or credit card processing system.

## Paragraphs/Procedures

### DATA-DEFINITION
This section defines the data structure for the pending authorization summary IMS segment. It contains various fields related to customer accounts and their authorization activities. The PA-ACCT-ID field stores the account identifier as a COMP-3 packed decimal. PA-CUST-ID holds the customer identifier. PA-AUTH-STATUS indicates the overall authorization status. PA-ACCOUNT-STATUS is an array that stores up to 5 account statuses. PA-CREDIT-LIMIT and PA-CASH-LIMIT define the credit and cash limits for the account, respectively. PA-CREDIT-BALANCE and PA-CASH-BALANCE represent the current credit and cash balances. PA-APPROVED-AUTH-CNT and PA-DECLINED-AUTH-CNT store the counts of approved and declined authorizations. PA-APPROVED-AUTH-AMT and PA-DECLINED-AUTH-AMT store the total amounts of approved and declined authorizations. Finally, FILLER provides padding to ensure the segment has a fixed length. This data structure is likely used by IMS transactions to retrieve and update pending authorization information.

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| 05:PA-ACCT-ID | column | 1 | Artifact '05:PA-ACCT-ID' (column) is never referenced by any other artifact in the dependency graph |
