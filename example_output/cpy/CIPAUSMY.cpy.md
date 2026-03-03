# CIPAUSMY

**File**: `cpy/CIPAUSMY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-02-27 14:43:55.018807

## Purpose

This copybook defines the data structure for the IMS segment related to pending authorization summary information. It includes fields for account ID, customer ID, authorization status, account status, credit and cash limits/balances, and authorization counts/amounts.

**Business Context**: This copybook is likely used in a system that manages and tracks pending authorizations for customer accounts, possibly within a financial institution or credit card processing system.

## Paragraphs/Procedures

### PA-ACCT-ID
This data field defines the pending authorization account identifier. It is a COMP-3 (packed decimal) field with a length of 11 digits and is signed. This field likely stores the unique identifier for an account that has pending authorizations. The field is defined on line 19. It does not perform any business logic or call any other paragraphs. It is an input field to the segment defined by the copybook.

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| 05:PA-ACCT-ID | column | 1 | Artifact '05:PA-ACCT-ID' (column) is never referenced by any other artifact in the dependency graph |

## Open Questions

- ? The purpose of the FILLER field is unclear.
  - Context: The code does not specify what data, if any, is stored in the FILLER field.
