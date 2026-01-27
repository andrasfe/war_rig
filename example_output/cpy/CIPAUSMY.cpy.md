# CIPAUSMY

**File**: `cpy/CIPAUSMY.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-27 02:43:07.655153

## Purpose

This copybook defines the data structure for the IMS segment named Pending Authorization Summary (PAUS). It includes fields for account and customer identifiers, authorization and account status information, credit and cash limits and current balances, counts of approved and declined authorizations, and corresponding amounts. The structure is used to store summary data related to pending authorizations in an IMS database.

**Business Context**: Serves financial authorization processing for customer accounts, tracking pending authorization summaries including limits, balances, and approval/decline statistics, within an Amazon-affiliated system (citation lines 4-17).

## Open Questions

- ? In which programs or modules is this copybook included?
  - Context: The copybook defines an IMS segment layout but does not indicate usage locations.
- ? What are the exact meanings or valid values for PA-AUTH-STATUS and PA-ACCOUNT-STATUS?
  - Context: Field descriptions are not provided beyond PIC clauses; no comments explain codes.
