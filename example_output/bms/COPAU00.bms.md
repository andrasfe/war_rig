# COPAU00

**File**: `bms/COPAU00.bms`
**Type**: FileType.BMS
**Analyzed**: 2026-01-27 23:05:09.274267

## Purpose

This BMS map defines the screen layout for the CardDemo Pending Authorization screen. It displays a list of pending authorizations for a given account ID, along with customer details and credit information. The screen allows the user to select a transaction to view its details.

**Business Context**: This screen is used in a card authorization application to display and manage pending authorizations.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| ACCTID | IOType.CICS_MAP | Account ID entered by the user to search for pending authorizations. |
| SEL0001 - SEL0005 | IOType.CICS_MAP | Selection fields for each transaction, allowing the user to select a transaction for viewing details. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU0A | IOType.CICS_MAP | The entire screen layout, including customer information, transaction details, and messages. |
| TRNNAME | IOType.CICS_MAP | Transaction name |
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
| TRNID01 - TRNID05 | IOType.CICS_MAP | Transaction IDs for the displayed transactions. |
| PDATE01 - PDATE05 | IOType.CICS_MAP | Transaction Dates for the displayed transactions. |
| PTIME01 - PTIME05 | IOType.CICS_MAP | Transaction Times for the displayed transactions. |
| PTYPE01 - PTYPE05 | IOType.CICS_MAP | Transaction Types for the displayed transactions. |
| PAPRV01 - PAPRV05 | IOType.CICS_MAP | Transaction Approval indicators for the displayed transactions. |
| PSTAT01 - PSTAT05 | IOType.CICS_MAP | Transaction Statuses for the displayed transactions. |
| PAMT001 - PAMT005 | IOType.CICS_MAP | Transaction Amounts for the displayed transactions. |
| ERRMSG | IOType.CICS_MAP | Error message displayed on the screen. |

## Paragraphs/Procedures

### MAP DEFINITION
This section defines the BMS map COPAU0A, which represents the 'CardDemo - Pending Authorization Screen' (lines 1-513). It specifies the overall screen characteristics, such as the control options (ALARM, FREEKB), extended attributes (EXTATT=YES), programming language (LANG=COBOL), input/output mode (MODE=INOUT), storage type (STORAGE=AUTO), terminal I/O area prefix (TIOAPFX=YES), and the map type (TYPE=&&SYSPARM). The map is divided into fields (DFHMDF) that define the position, size, attributes, and initial values of text and input areas on the screen. These fields are used to display labels, data, and allow user input for searching account IDs and selecting transactions. The map includes fields for displaying customer information (name, address, account status), credit limits and balances, and a list of pending transactions with details like transaction ID, date, time, type, approval status, and amount. The ERRMSG field (line 503) is used to display error messages to the user. The final line (513) indicates the end of the map definition.

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| COPAU00 | map | 19 | Artifact 'COPAU00' (map) is never referenced by any other artifact in the dependency graph |
| COPAU0A | screen | 26 | Screen/Map 'COPAU0A' is never sent to or received from by any program |

## Open Questions

- ? What is the purpose of the TYPE=&&SYSPARM parameter in the DFHMSD macro?
  - Context: The meaning of &&SYSPARM is unclear without additional context or JCL.
