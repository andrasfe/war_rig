# CIPAUSMY

**File**: `cpy/CIPAUSMY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-25 18:38:20.255001

## Purpose

This COBOL copybook defines the record layout for the IMS 'PENDING AUTHORIZATION SUMMARY' segment. It specifies fields including account ID, customer ID, authorization status, multiple account statuses, credit and cash limits and balances, and cumulative counts and amounts for approved and declined authorizations. The structure supports storage and retrieval of pending authorization data in an IMS database.

**Business Context**: Tracks pending authorization summaries for customer accounts, including limits, balances, and transaction history in a financial or payment processing system.

## Open Questions

- ? Specific usage of this copybook in programs or IMS PCB/DBD definitions
  - Context: Copybook defines structure only; no including program or IMS context provided in source.
- ? Confirmation of segment key field(s)
  - Context: PA-ACCT-ID (line 19) positioned first with packed format, conventional for IMS keys, but not explicitly stated.
