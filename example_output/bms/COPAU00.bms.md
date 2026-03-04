# COPAU00

**File**: `bms/COPAU00.bms`
**Type**: FileType.BMS
**Analyzed**: 2026-03-04 03:32:01.189040

## Purpose

This BMS map defines the COPAU00 screen, which is used to display pending authorization information for a given account ID. The screen allows users to search for an account and view transaction details, including transaction ID, date, time, type, approval status, and amount.

**Business Context**: This screen is likely used in a customer service or fraud investigation context to review and manage pending card authorizations.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| ACCTID | IOType.CICS_MAP | Account ID entered by the user to search for pending authorizations. |
| SEL0001 - SEL0004 | IOType.CICS_MAP | Selection fields for each transaction displayed on the screen. User selects a transaction for further action. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU0A | IOType.CICS_MAP | The entire screen layout, including labels, data fields, and transaction details. |
| TRNNAME | IOType.CICS_MAP | Transaction name |
| TITLE01 | IOType.CICS_MAP | Title of the screen |
| CURDATE | IOType.CICS_MAP | Current Date |
| PGMNAME | IOType.CICS_MAP | Program Name |
| TITLE02 | IOType.CICS_MAP | Title of the screen |
| CURTIME | IOType.CICS_MAP | Current Time |
| CNAME | IOType.CICS_MAP | Customer Name |
| CUSTID | IOType.CICS_MAP | Customer ID |
| ADDR001 | IOType.CICS_MAP | Customer Address Line 1 |
| ACCSTAT | IOType.CICS_MAP | Account Status |
| ADDR002 | IOType.CICS_MAP | Customer Address Line 2 |
| PHONE1 | IOType.CICS_MAP | Customer Phone Number |
| APPRCNT | IOType.CICS_MAP | Approval Count |
| DECLCNT | IOType.CICS_MAP | Decline Count |
| CREDLIM | IOType.CICS_MAP | Credit Limit |
| CASHLIM | IOType.CICS_MAP | Cash Limit |
| APPRAMT | IOType.CICS_MAP | Approved Amount |
| CREDBAL | IOType.CICS_MAP | Credit Balance |
| CASHBAL | IOType.CICS_MAP | Cash Balance |
| DECLAMT | IOType.CICS_MAP | Declined Amount |
| TRNID01 - TRNID05 | IOType.CICS_MAP | Transaction ID for each transaction displayed. |
| PDATE01 - PDATE05 | IOType.CICS_MAP | Transaction Date for each transaction displayed. |
| PTIME01 - PTIME05 | IOType.CICS_MAP | Transaction Time for each transaction displayed. |
| PTYPE01 - PTYPE05 | IOType.CICS_MAP | Transaction Type for each transaction displayed. |
| PAPRV01 - PAPRV04 | IOType.CICS_MAP | Transaction Approval Status for each transaction displayed. |
| PSTAT01 - PSTAT04 | IOType.CICS_MAP | Transaction Status for each transaction displayed. |
| PAMT001 - PAMT004 | IOType.CICS_MAP | Transaction Amount for each transaction displayed. |

## Paragraphs/Procedures

### ~~COPAU00~~ (Dead Code)
*Artifact 'COPAU00' (map) is never referenced by any other artifact in the dependency graph*

### ~~COPAU0A~~ (Dead Code)
*Screen/Map 'COPAU0A' is never sent to or received from by any program*

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| COPAU00 | map | 19 | Artifact 'COPAU00' (map) is never referenced by any other artifact in the dependency graph |
| COPAU0A | screen | 26 | Screen/Map 'COPAU0A' is never sent to or received from by any program |
