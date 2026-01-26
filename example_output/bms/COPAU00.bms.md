# COPAU00

**File**: `bms/COPAU00.bms`
**Type**: FileType.BMS
**Analyzed**: 2026-01-26 02:29:53.682994

## Purpose

This BMS file defines the COPAU00 mapset and COPAU0A physical map for the 'Pending Authorization Screen' in the CardDemo CICS application. The screen layout includes headers for transaction name, program name, date, and time; an input field for searching by Account ID; display areas for customer name, ID, address lines, account status, phone, approval/decline counts, credit/cash limits, balances, and amounts; a table listing up to 5 pending transactions with selectable checkboxes, transaction ID, date, time, type, A/D, status, and amount; and instructions for function keys and error messages.

**Business Context**: Credit card authorization viewing and selection in a demo banking application, allowing users to search an account and select pending authorizations for details.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| ACCTID | IOType.CICS_MAP | Unprotected input field for Account ID search |
| SEL0001 | IOType.CICS_MAP | Unprotected selection field (e.g., 'S') for first pending transaction |
| SEL0002 | IOType.CICS_MAP | Unprotected selection field for second pending transaction |
| SEL0003 | IOType.CICS_MAP | Unprotected selection field for third pending transaction |
| SEL0004 | IOType.CICS_MAP | Unprotected selection field for fourth pending transaction |
| SEL0005 | IOType.CICS_MAP | Unprotected selection field for fifth pending transaction |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU0A | IOType.CICS_MAP | Physical map defining all screen fields including headers (TRNNAME, CURDATE, PGMNAME, CURTIME), customer info (CNAME, CUSTID, ADDR001, ADDR002, ACCSTAT, PHONE1), counts (APPRCNT, DECLCNT), limits/balances/amounts (CREDLIM, CASHLIM, APPRAMT, CREDBAL, CASHBAL, DECLAMT), transaction list (TRNID01-PAMT005 etc.), instructions, and error message (ERRMSG) |

## Open Questions

- ? Which CICS COBOL program(s) include and use this BMS map?
  - Context: BMS file does not specify the calling program; only mapset/map names provided
- ? What are the exact data sources populating the display fields (e.g., customer details, transaction list)?
  - Context: BMS defines layout only, not data retrieval logic
