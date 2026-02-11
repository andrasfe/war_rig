# COPAU00

**File**: `bms/COPAU00.bms`
**Type**: FileType.BMS
**Analyzed**: 2026-02-10 17:19:05.202114

## Purpose

This BMS map defines the screen layout for the CardDemo application's Pending Authorization screen. It displays customer account information, a list of pending transactions, and options for navigating the application. The screen allows users to view pending authorizations and select a transaction for further details.

**Business Context**: This screen is part of a card authorization system, allowing users to view and manage pending authorizations for customer accounts.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| ACCTID | IOType.CICS_MAP | Account ID entered by the user to search for pending authorizations. |
| SEL0001 | IOType.CICS_MAP | Selection field for the first transaction in the list. |
| SEL0002 | IOType.CICS_MAP | Selection field for the second transaction in the list. |
| SEL0003 | IOType.CICS_MAP | Selection field for the third transaction in the list. |
| SEL0004 | IOType.CICS_MAP | Selection field for the fourth transaction in the list. |
| SEL0005 | IOType.CICS_MAP | Selection field for the fifth transaction in the list. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU0A | IOType.CICS_MAP | The entire screen layout, including labels, data fields, and messages. |
| TRNNAME | IOType.CICS_MAP | Transaction name. |
| TITLE01 | IOType.CICS_MAP | Title of the screen. |
| CURDATE | IOType.CICS_MAP | Current date. |
| PGMNAME | IOType.CICS_MAP | Program name. |
| TITLE02 | IOType.CICS_MAP | Secondary title. |
| CURTIME | IOType.CICS_MAP | Current time. |
| CNAME | IOType.CICS_MAP | Customer name. |
| CUSTID | IOType.CICS_MAP | Customer ID. |
| ADDR001 | IOType.CICS_MAP | Customer address line 1. |
| ACCSTAT | IOType.CICS_MAP | Account status. |
| ADDR002 | IOType.CICS_MAP | Customer address line 2. |
| PHONE1 | IOType.CICS_MAP | Customer phone number. |
| APPRCNT | IOType.CICS_MAP | Approval count. |
| DECLCNT | IOType.CICS_MAP | Decline count. |
| CREDLIM | IOType.CICS_MAP | Credit limit. |
| CASHLIM | IOType.CICS_MAP | Cash limit. |
| APPRAMT | IOType.CICS_MAP | Approved amount. |
| CREDBAL | IOType.CICS_MAP | Credit balance. |
| CASHBAL | IOType.CICS_MAP | Cash balance. |
| DECLAMT | IOType.CICS_MAP | Declined amount. |
| TRNID01 | IOType.CICS_MAP | Transaction ID for the first transaction. |
| PDATE01 | IOType.CICS_MAP | Date of the first transaction. |
| PTIME01 | IOType.CICS_MAP | Time of the first transaction. |
| PTYPE01 | IOType.CICS_MAP | Type of the first transaction. |
| PAPRV01 | IOType.CICS_MAP | Approval status of the first transaction. |
| PSTAT01 | IOType.CICS_MAP | Status of the first transaction. |
| PAMT001 | IOType.CICS_MAP | Amount of the first transaction. |
| TRNID02 | IOType.CICS_MAP | Transaction ID for the second transaction. |
| PDATE02 | IOType.CICS_MAP | Date of the second transaction. |
| PTIME02 | IOType.CICS_MAP | Time of the second transaction. |
| PTYPE02 | IOType.CICS_MAP | Type of the second transaction. |
| PAPRV02 | IOType.CICS_MAP | Approval status of the second transaction. |
| PSTAT02 | IOType.CICS_MAP | Status of the second transaction. |
| PAMT002 | IOType.CICS_MAP | Amount of the second transaction. |
| TRNID03 | IOType.CICS_MAP | Transaction ID for the third transaction. |
| PDATE03 | IOType.CICS_MAP | Date of the third transaction. |
| PTIME03 | IOType.CICS_MAP | Time of the third transaction. |
| PTYPE03 | IOType.CICS_MAP | Type of the third transaction. |
| PAPRV03 | IOType.CICS_MAP | Approval status of the third transaction. |
| PSTAT03 | IOType.CICS_MAP | Status of the third transaction. |
| PAMT003 | IOType.CICS_MAP | Amount of the third transaction. |
| TRNID04 | IOType.CICS_MAP | Transaction ID for the fourth transaction. |
| PDATE04 | IOType.CICS_MAP | Date of the fourth transaction. |
| PTIME04 | IOType.CICS_MAP | Time of the fourth transaction. |
| PTYPE04 | IOType.CICS_MAP | Type of the fourth transaction. |
| PAPRV04 | IOType.CICS_MAP | Approval status of the fourth transaction. |
| PSTAT04 | IOType.CICS_MAP | Status of the fourth transaction. |
| PAMT004 | IOType.CICS_MAP | Amount of the fourth transaction. |
| TRNID05 | IOType.CICS_MAP | Transaction ID for the fifth transaction. |
| PDATE05 | IOType.CICS_MAP | Date of the fifth transaction. |
| PTIME05 | IOType.CICS_MAP | Time of the fifth transaction. |
| PTYPE05 | IOType.CICS_MAP | Type of the fifth transaction. |
| PAPRV05 | IOType.CICS_MAP | Approval status of the fifth transaction. |
| PSTAT05 | IOType.CICS_MAP | Status of the fifth transaction. |
| PAMT005 | IOType.CICS_MAP | Amount of the fifth transaction. |
| ERRMSG | IOType.CICS_MAP | Error message display field. |

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

- ? What is the purpose of the 'Sel' field and how is it used to select a transaction?
  - Context: The code defines 'Sel' fields (SEL0001-SEL0005), but the exact mechanism for using them to select a transaction is unclear from the BMS map definition alone.
