# COPAU00

**File**: `bms/COPAU00.bms`
**Type**: FileType.BMS
**Analyzed**: 2026-02-09 15:46:42.326924

## Purpose

This BMS map defines the screen layout for the Pending Authorization Screen in the CardDemo application. It displays a list of pending authorizations for a given account ID, along with customer details and credit information. The screen allows the user to select a transaction to view its details.

**Business Context**: This screen is used to view and manage pending card authorizations.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| ACCTID | IOType.CICS_MAP | Account ID to search for pending authorizations. This is an unprotected field where the user enters the account ID. |
| SEL0001 - SEL0005 | IOType.CICS_MAP | Selection fields for each transaction. User enters a value to select a transaction. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU0A | IOType.CICS_MAP | The entire screen layout, including labels, data fields, and messages. Displays customer information, account details, and a list of pending transactions. |
| TRNNAME | IOType.CICS_MAP | Transaction name |
| CURDATE | IOType.CICS_MAP | Current Date |
| PGMNAME | IOType.CICS_MAP | Program Name |
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
| TRNID01 - TRNID05 | IOType.CICS_MAP | Transaction IDs for the pending authorizations. |
| PDATE01 - PDATE05 | IOType.CICS_MAP | Transaction Dates for the pending authorizations. |
| PTIME01 - PTIME05 | IOType.CICS_MAP | Transaction Times for the pending authorizations. |
| PTYPE01 - PTYPE05 | IOType.CICS_MAP | Transaction Types for the pending authorizations. |
| PAPRV01 - PAPRV05 | IOType.CICS_MAP | Approval indicators for the pending authorizations. |
| PSTAT01 - PSTAT05 | IOType.CICS_MAP | Transaction Statuses for the pending authorizations. |
| PAMT001 - PAMT005 | IOType.CICS_MAP | Transaction Amounts for the pending authorizations. |
| ERRMSG | IOType.CICS_MAP | Error message field to display errors to the user. |

## Paragraphs/Procedures

### ~~COPAU0A~~ (Dead Code)
*Screen/Map 'COPAU0A' is never sent to or received from by any program*

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| COPAU00 | map | 19 | Artifact 'COPAU00' (map) is never referenced by any other artifact in the dependency graph |
| COPAU0A | screen | 26 | Screen/Map 'COPAU0A' is never sent to or received from by any program |

## Open Questions

- ? What is the purpose of the &&SYSPARM variable?
  - Context: The code uses `TYPE=&&SYSPARM` but its value and usage are unclear without more context.
