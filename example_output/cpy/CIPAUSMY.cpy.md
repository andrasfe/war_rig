# CIPAUSMY

**File**: `cpy/CIPAUSMY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-26 15:14:56.834383

## Purpose

This copybook defines the COBOL level-05 record layout for the IMS 'PENDING AUTHORIZATION SUMMARY' segment (PAUSMY). It specifies fields for account and customer identifiers, authorization and account statuses, credit/cash limits and balances, counts and amounts of approved/declined authorizations, and a filler area. The structure supports storage and retrieval of summary data for pending authorizations in an IMS database.

**Business Context**: Serves financial transaction processing by maintaining summary records of pending authorizations for customer accounts, including limits, balances, and approval/decline statistics (likely credit/cash accounts).

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PAUSMY | IOType.IMS_SEGMENT | IMS segment structure defining pending authorization summary data, including PA-ACCT-ID (account ID), PA-CUST-ID (customer ID), PA-AUTH-STATUS (auth status), PA-ACCOUNT-STATUS (5-occurrence account status array), PA-CREDIT-LIMIT/BALANCE, PA-CASH-LIMIT/BALANCE, approval/decline counts and amounts, and filler. |

## Business Rules

- **BR001**: PA-ACCT-ID is defined as a signed 11-digit packed decimal field for the account identifier.
- **BR002**: PA-ACCOUNT-STATUS is a repeating group of 5 two-character status codes for account statuses.
- **BR003**: Credit and cash limits/balances are signed 9-digit with 2 decimals in packed decimal format.

## Open Questions

- ? Specific usage context of this IMS segment (e.g., PCB name, parent/child segments in database).
  - Context: Copybook defines structure only; no calling program provided to determine DL/I calls or hierarchy.
- ? Exact business meaning of PA-ACCOUNT-STATUS codes and PA-AUTH-STATUS values.
  - Context: PIC clauses define format but not valid values or enums.
