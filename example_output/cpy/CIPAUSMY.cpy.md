# CIPAUSMY

**File**: `cpy/CIPAUSMY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-03-03 16:49:45.294469

## Purpose

This copybook defines the data structure for the IMS segment related to pending authorization summary information. It contains fields for account ID, customer ID, authorization status, account status, credit and cash limits/balances, and counts/amounts for approved and declined authorizations.

**Business Context**: UNKNOWN

## Paragraphs/Procedures

### PA-ACCT-ID
This data field, PA-ACCT-ID, defines the pending authorization account identifier. It is a COMP-3 (packed decimal) field with a signed 11-digit precision. This field is intended to store the unique identifier for an account that has pending authorizations. The field is part of the CIPAUSMY copybook which defines the structure for an IMS segment related to pending authorization summary information. This field is used to identify the account to which the pending authorization summary data applies. The data contained within this field is used in conjunction with other fields in the copybook to provide a comprehensive overview of pending authorizations for a given account. No specific validation or error handling is defined within the copybook itself for this field. The field does not directly call any other paragraphs or programs.

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| 05:PA-ACCT-ID | column | 1 | Artifact '05:PA-ACCT-ID' (column) is never referenced by any other artifact in the dependency graph |

## Open Questions

- ? What is the business context for this IMS segment?
  - Context: The purpose section is missing the business context.
