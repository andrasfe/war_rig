# COPAU00

**File**: `bms/COPAU00.bms`
**Type**: FileType.BMS
**Analyzed**: 2026-01-25 18:34:45.291848

## Purpose

BMS mapset COPAU00 defines map COPAU0A for the Pending Authorization Screen, displaying header info, customer account details, credit/cash limits and balances, approval/decline counts, and a scrollable list of up to 5 pending transactions. Users search by Account ID and select transactions by entering 'S' in selection fields. Includes error message field and navigation instructions.

**Business Context**: Credit card authorization review process in CardDemo application, allowing tellers or operators to view and select pending authorizations for further action.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| COPAU0A | IOType.CICS_MAP | Input fields include ACCTID for account search and SEL0001-SEL0005 for transaction selection (UNPROT, FSET attributes) |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU0A | IOType.CICS_MAP | Output fields display transaction ID/name (TRNNAME, PGMNAME), date/time (CURDATE, CURTIME), customer name/address/phone (CNAME, ADDR001, ADDR002, PHONE1), status/limits/balances/counts (CUSTID, ACCSTAT, CREDLIM, CASHLIM, APPRAMT, CREDBAL, CASHBAL, DECLAMT, APPRCNT, DECLCNT), transaction list (TRNIDxx, PDATExx, PTIMExx, PTYPExx, PAPRVxx, PSTATxx, PAMTxx), instructions, and ERRMSG |

## Business Rules

- **BR001**: Users type 'S' in SEL fields (SEL0001-SEL0005) to select a transaction from the list for viewing authorization details

## Open Questions

- ? Which CICS transaction ID or program uses this BMS mapset COPAU00?
  - Context: BMS source defines the map but does not specify invoking program or transaction
- ? What are the exact data sources populating the output fields like TRNID01, CNAME, CREDLIM?
  - Context: BMS defines layout only; no logic or data flow specified
