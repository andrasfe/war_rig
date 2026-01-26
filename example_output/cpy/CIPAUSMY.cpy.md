# CIPAUSMY

**File**: `cpy/CIPAUSMY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-26 17:41:40.148363

## Purpose

This copybook defines the data structure for an IMS database segment named 'PENDING AUTHORIZATION SUMMARY'. It specifies fields for account and customer identification, authorization and account statuses, credit and cash limits and balances, and cumulative counts and amounts for approved and declined authorizations. The layout supports IMS DL/I get/insert/update operations in programs handling pending authorizations.

**Business Context**: Financial authorization processing for customer accounts, tracking pending credit and cash authorization summaries in an IMS hierarchical database.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| PENDING-AUTHORIZATION-SUMMARY | IOType.IMS_SEGMENT | IMS segment layout holding account ID, customer ID, auth status, 5 account statuses, credit/cash limits and balances, approved/declined auth counts and amounts. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| PENDING-AUTHORIZATION-SUMMARY | IOType.IMS_SEGMENT | IMS segment layout for output/update of pending authorization summary data matching input structure. |

## Business Rules

- **BR001**: PA-ACCOUNT-STATUS defines an array of exactly 5 two-character account status values.
- **BR002**: Monetary fields (limits, balances, auth amounts) use signed 9-digit with 2 decimals, packed decimal format.
- **BR003**: Authorization count fields are signed 4-digit binary integers.
- **BR004**: Account ID is a signed 11-digit packed decimal.
