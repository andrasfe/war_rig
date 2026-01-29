# COPAU00

**File**: `bms/COPAU00.bms`
**Type**: FileType.BMS
**Analyzed**: 2026-01-28 14:54:49.526333

## Purpose

This BMS map defines the screen layout for the CardDemo Pending Authorization screen. It displays customer account information and a list of pending transactions, allowing the user to select a transaction for further details.

**Business Context**: This screen is used to view and manage pending authorizations for credit card transactions.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| ACCTID | IOType.CICS_MAP | Account ID entered by the user to search for pending authorizations. |
| SEL0001 - SEL0005 | IOType.CICS_MAP | Selection fields for each transaction displayed on the screen. User enters a value to select a specific transaction. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU0A | IOType.CICS_MAP | The entire screen layout, including labels, data fields, and messages. Displays customer information and pending transactions. |
| TRNNAME | IOType.CICS_MAP | Transaction Name |
| TITLE01 | IOType.CICS_MAP | Title 1 |
| CURDATE | IOType.CICS_MAP | Current Date |
| PGMNAME | IOType.CICS_MAP | Program Name |
| TITLE02 | IOType.CICS_MAP | Title 2 |
| CURTIME | IOType.CICS_MAP | Current Time |
| CNAME | IOType.CICS_MAP | Customer Name |
| CUSTID | IOType.CICS_MAP | Customer ID |
| ADDR001 | IOType.CICS_MAP | Address Line 1 |
| ACCSTAT | IOType.CICS_MAP | Account Status |
| ADDR002 | IOType.CICS_MAP | Address Line 2 |
| PHONE1 | IOType.CICS_MAP | Phone Number |
| APPRCNT | IOType.CICS_MAP | Approval Count |
| DECLCNT | IOType.CICS_MAP | Decline Count |
| CREDLIM | IOType.CICS_MAP | Credit Limit |
| CASHLIM | IOType.CICS_MAP | Cash Limit |
| APPRAMT | IOType.CICS_MAP | Approved Amount |
| CREDBAL | IOType.CICS_MAP | Credit Balance |
| CASHBAL | IOType.CICS_MAP | Cash Balance |
| DECLAMT | IOType.CICS_MAP | Declined Amount |
| TRNID01 - TRNID05 | IOType.CICS_MAP | Transaction ID for each of the 5 transactions displayed. |
| PDATE01 - PDATE05 | IOType.CICS_MAP | Transaction Date for each of the 5 transactions displayed. |
| PTIME01 - PTIME05 | IOType.CICS_MAP | Transaction Time for each of the 5 transactions displayed. |
| PTYPE01 - PTYPE05 | IOType.CICS_MAP | Transaction Type for each of the 5 transactions displayed. |
| PAPRV01 - PAPRV05 | IOType.CICS_MAP | Transaction Approval indicator for each of the 5 transactions displayed. |
| PSTAT01 - PSTAT05 | IOType.CICS_MAP | Transaction Status for each of the 5 transactions displayed. |
| PAMT001 - PAMT005 | IOType.CICS_MAP | Transaction Amount for each of the 5 transactions displayed. |
| ERRMSG | IOType.CICS_MAP | Error message field to display error conditions to the user. |

## Paragraphs/Procedures

### ~~COPAU0A~~ (Dead Code)
*Screen/Map 'COPAU0A' is never sent to or received from by any program*

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| COPAU00 | map | 19 | Artifact 'COPAU00' (map) is never referenced by any other artifact in the dependency graph |
| COPAU0A | screen | 26 | Screen/Map 'COPAU0A' is never sent to or received from by any program |
