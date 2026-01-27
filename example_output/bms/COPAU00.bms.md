# COPAU00

**File**: `bms/COPAU00.bms`
**Type**: FileType.BMS
**Analyzed**: 2026-01-27 02:42:27.496687

## Purpose

This BMS mapset COPAU00 defines the COPAU0A map for the Pending Authorization Screen (CardDemo). It displays header info (transaction, date/time, program, titles), customer details (name, ID, address, status, phone, approval/decline counts, credit/cash limits/balances), and a table of up to 5 pending transactions (ID, date, time, type, A/D, status, amount). Users input search account ID and select transactions via SEL fields.

**Business Context**: Supports reviewing pending credit card authorizations by account in a demo payment processing system

## Inputs

| Name | Type | Description |
|------|------|-------------|
| ACCTID | IOType.CICS_MAP | User-entered account ID for searching pending authorizations |
| SEL0001 | IOType.CICS_MAP | Selection field (e.g., 'S') for first pending transaction |
| SEL0002 | IOType.CICS_MAP | Selection field (e.g., 'S') for second pending transaction |
| SEL0003 | IOType.CICS_MAP | Selection field (e.g., 'S') for third pending transaction |
| SEL0004 | IOType.CICS_MAP | Selection field (e.g., 'S') for fourth pending transaction |
| SEL0005 | IOType.CICS_MAP | Selection field (e.g., 'S') for fifth pending transaction |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU0A | IOType.CICS_MAP | Display-only fields for customer info (CNAME, CUSTID, ADDR001/2, ACCSTAT, PHONE1), counts (APPRCNT, DECLCNT), limits/balances (CREDLIM, CASHLIM, APPRAMT, CREDBAL, CASHBAL, DECLAMT), transaction list (TRNIDxx, PDATExx, PTIMExx, PTYPExx, PAPRVxx, PSTATxx, PAMTxx), headers, titles, dates/times, instructions, and error message (ERRMSG) |

## Business Rules

- **BR001**: User must enter 'S' in one of the SELxxxx fields to select a pending transaction for viewing authorization details

## Open Questions

- ? Specific transaction ID or CICS program using this map
  - Context: Not specified in BMS source; would be in referencing COBOL/JCL
