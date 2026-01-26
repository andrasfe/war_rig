# COPAU00

**File**: `bms/COPAU00.bms`
**Type**: FileType.BMS
**Analyzed**: 2026-01-26 17:41:11.273119

## Purpose

Defines BMS mapset COPAU00 with map COPAU0A for the CardDemo Pending Authorization Screen. Enables searching by Account ID to view customer details, credit/cash limits and balances, approval/decline counts, and a list of up to 5 pending transactions. Users select transactions by entering 'S' in SEL fields for detail view, with error message display and function key guidance.

**Business Context**: CICS-based credit card demonstration application for inquiring on pending authorizations.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| ACCTID | IOType.CICS_MAP | Account ID field for searching pending authorizations |
| SEL0001 | IOType.CICS_MAP | Selection field (enter 'S') for first listed transaction |
| SEL0002 | IOType.CICS_MAP | Selection field (enter 'S') for second listed transaction |
| SEL0003 | IOType.CICS_MAP | Selection field (enter 'S') for third listed transaction |
| SEL0004 | IOType.CICS_MAP | Selection field (enter 'S') for fourth listed transaction |
| SEL0005 | IOType.CICS_MAP | Selection field (enter 'S') for fifth listed transaction |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| CUSTOMER_INFO | IOType.CICS_MAP | Display fields for customer name, ID, address lines, status, phone (CNAME, CUSTID, ADDR001, ADDR002, ACCSTAT, PHONE1) |
| LIMITS_COUNTS | IOType.CICS_MAP | Credit/cash limits, balances, approval/decline counts and amounts (CREDLIM, CASHLIM, APPRAMT, CREDBAL, CASHBAL, DECLAMT, APPRCNT, DECLCNT) |
| TRANSACTION_LIST | IOType.CICS_MAP | Pending transaction details including ID, date, time, type, approve/decline, status, amount (TRNIDxx, PDATExx, PTIMExx, PTYPExx, PAPRVxx, PSTATxx, PAMTxx for xx=01-05) |
| ERRMSG | IOType.CICS_MAP | Error message display field |

## Business Rules

- **BR001**: Users enter 'S' in one of the SELxxxx fields to select a listed transaction for viewing authorization details.

## Open Questions

- ? Which CICS program(s) include and use this BMS mapset COPAU00?
  - Context: BMS file does not reference calling programs or transactions.
