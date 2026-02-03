# COPAU00

**File**: `bms/COPAU00.bms`
**Type**: FileType.BMS
**Analyzed**: 2026-02-03 21:04:20.405397

## Purpose

This BMS map defines the screen layout for the CardDemo Pending Authorization Screen. It displays a list of pending authorizations for a given account ID, along with customer details and credit/cash limits and balances. The screen allows the user to select a transaction to view its details.

**Business Context**: This screen is used to view and manage pending card authorizations within the CardDemo application.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| ACCTID | IOType.CICS_MAP | Account ID to search for pending authorizations (input field). |
| SEL0001 - SEL0005 | IOType.CICS_MAP | Selection fields for each transaction (input field). |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| TRNNAME | IOType.CICS_MAP | Transaction Name |
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
| TRNID01 - TRNID05 | IOType.CICS_MAP | Transaction IDs for the list of pending authorizations. |
| PDATE01 - PDATE05 | IOType.CICS_MAP | Transaction Dates for the list of pending authorizations. |
| PTIME01 - PTIME05 | IOType.CICS_MAP | Transaction Times for the list of pending authorizations. |
| PTYPE01 - PTYPE05 | IOType.CICS_MAP | Transaction Types for the list of pending authorizations. |
| PAPRV01 - PAPRV05 | IOType.CICS_MAP | Transaction Approval Status for the list of pending authorizations. |
| PSTAT01 - PSTAT05 | IOType.CICS_MAP | Transaction Status for the list of pending authorizations. |
| PAMT001 - PAMT005 | IOType.CICS_MAP | Transaction Amounts for the list of pending authorizations. |
| ERRMSG | IOType.CICS_MAP | Error Message Displayed on the Screen |

## Paragraphs/Procedures

### COPAU0A DFHMDI
This paragraph defines the map's overall structure and attributes. It specifies the screen size as 24 lines by 80 columns (line 28). It sets the column and line position to 1 (lines 26, 27). It includes control options such as ALARM and FREEKB (line 19), indicating that the screen can trigger an alarm and that the keyboard is initially unlocked. The EXTATT=YES attribute (line 20) enables extended attributes like color and highlighting. LANG=COBOL (line 21) specifies the programming language. MODE=INOUT (line 22) indicates that the map is used for both input and output. STORAGE=AUTO (line 23) means storage is automatically allocated. TIOAPFX=YES (line 24) includes the terminal I/O area prefix. TYPE=&&SYSPARM (line 25) allows specifying the map type during assembly.

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| COPAU00 | map | 19 | Artifact 'COPAU00' (map) is never referenced by any other artifact in the dependency graph |
| COPAU0A | screen | 26 | Screen/Map 'COPAU0A' is never sent to or received from by any program |

## Open Questions

- ? How are the transaction details (date, time, type, A/D, STS, Amount) populated for each of the five transactions displayed?
  - Context: The BMS map defines the fields, but the logic for populating them is not present in this file.
