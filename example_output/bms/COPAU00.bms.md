# COPAU00

**File**: `bms/COPAU00.bms`
**Type**: FileType.BMS
**Analyzed**: 2026-01-26 15:14:02.371671

## Purpose

BMS mapset definition for the 'Pending Authorization Screen' in the CardDemo application. Provides a terminal screen for searching pending authorizations by account ID, displaying customer details, account limits/balances, and a selectable list of up to 5 transactions. Supports user interaction via selection fields and function keys for navigation.

**Business Context**: Credit card processing system for customer service representatives to view, search, and select pending authorization transactions by account.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| ACCTID | IOType.CICS_MAP | 11-character unprotected input field for searching account ID |
| SEL0001 | IOType.CICS_MAP | 1-character unprotected selection field for transaction 1 |
| SEL0002 | IOType.CICS_MAP | 1-character unprotected selection field for transaction 2 |
| SEL0003 | IOType.CICS_MAP | 1-character unprotected selection field for transaction 3 |
| SEL0004 | IOType.CICS_MAP | 1-character unprotected selection field for transaction 4 |
| SEL0005 | IOType.CICS_MAP | 1-character unprotected selection field for transaction 5 |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| Customer Info Fields | IOType.CICS_MAP | Protected display fields for customer name (CNAME), ID (CUSTID), address (ADDR001/ADDR002), phone (PHONE1), account status (ACCSTAT) |
| Account Summary Fields | IOType.CICS_MAP | Protected display fields for credit/cash limits (CREDLIM/CASHLIM), balances (CREDBAL/CASHBAL), approved/declined counts (APPRCNT/DECLCNT), amounts (APPRAMT/DECLAMT) |
| Transaction List Fields | IOType.CICS_MAP | Protected display fields for transaction details including TRNIDxx, PDATExx, PTIMExx, PTYPExx, PAPRVxx, PSTATxx, PAMTxx (x=01-05) |
| ERRMSG | IOType.CICS_MAP | Error message display field (78 characters, bright, red) |
| Header/Trailer Fields | IOType.CICS_MAP | Static labels, titles, current date/time (CURDATE/CURTIME), program name (PGMNAME/TRNNAME), function key help |

## Business Rules

- **BR001**: Account search input constrained to exactly 11 characters
- **BR002**: Transaction selection by entering 'S' in one of the 1-character SEL fields
- **BR003**: Screen navigation via function keys: F3=Back, F7=Backward, F8=Forward, ENTER=Continue

## Open Questions

- ? Which CICS transaction ID or program uses this BMS mapset?
  - Context: BMS file defines the map but does not reference calling program or transaction
- ? What are the exact data sources populating the output fields (e.g., TRNID01)?
  - Context: BMS defines layout only, not data population logic
