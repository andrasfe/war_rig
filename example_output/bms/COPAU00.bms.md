# COPAU00

**File**: `bms/COPAU00.bms`
**Type**: FileType.BMS
**Analyzed**: 2026-01-26 14:23:14.843576

## Purpose

The COPAU00 BMS map defines the layout for the CardDemo Pending Authorization Screen. It provides fields for inputting an Account ID to search, displays customer account details including name, customer ID, address, status, phone, approval/decline counts, credit/cash limits and balances. It also shows a table of up to five pending authorization transactions with columns for selection, transaction ID, date, time, type, A/D, status, and amount, allowing user selection via 'S' for details.

**Business Context**: Serves credit card authorization review process by displaying pending authorizations for a specific account to enable selection and further viewing.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| ACCTID | IOType.CICS_MAP | Account ID search field, unprotected for user entry |
| SEL0001 | IOType.CICS_MAP | Selection field (e.g., 'S') for first pending transaction, unprotected |
| SEL0002 | IOType.CICS_MAP | Selection field (e.g., 'S') for second pending transaction, unprotected |
| SEL0003 | IOType.CICS_MAP | Selection field (e.g., 'S') for third pending transaction, unprotected |
| SEL0004 | IOType.CICS_MAP | Selection field (e.g., 'S') for fourth pending transaction, unprotected |
| SEL0005 | IOType.CICS_MAP | Selection field (e.g., 'S') for fifth pending transaction, unprotected |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU00 | IOType.CICS_MAP | Full screen map including headers, account details (CNAME, CUSTID, PHONE1, APPRCNT, DECLCNT, CREDLIM, CASHLIM, APPRAMT, CREDBAL, CASHBAL, DECLAMT), and transaction list fields (TRNID01-05, PDATE01-05, etc.) populated by calling program |
| ERRMSG | IOType.CICS_MAP | Error message display field |

## Business Rules

- **BR001**: Limits display to a maximum of five pending authorization transactions per account

## Open Questions

- ? Which CICS COBOL program(s) reference and use this BMS map?
  - Context: BMS file defines the map but does not indicate the calling transaction/program
- ? What populates the dynamic display fields like TRNID01, CNAME, etc.?
  - Context: BMS defines layout and initial values (mostly spaces) but data sources are in calling program
