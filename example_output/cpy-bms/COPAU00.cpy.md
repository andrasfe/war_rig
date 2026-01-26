# COPAU00

**File**: `cpy-bms/COPAU00.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-26 15:10:56.102993

## Purpose

This COBOL copybook defines the symbolic map structure for the COPAU00 BMS map used in CICS online transactions. It includes COPAU0AI for input fields from the terminal screen and COPAU0AO as a redefines for output fields, supporting display of transaction name, titles, current date/time, program name, customer account details (ID, name, customer ID, addresses, status, phone), approval/decline counts, credit/cash limits and balances, up to five recent transactions with details (select flag, transaction ID, date, time, type, approval flag, status, amount), and error messages. The structure follows standard BMS conventions with length (L), format/flag (F with attribute A), and input/output data fields (I/O).

**Business Context**: CICS screen for customer account inquiry displaying summary information and recent transaction history in a financial or credit card processing system.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| COPAU0AI | IOType.CICS_MAP | Input structure containing terminal screen data for account details, transaction history selectors, and fields like ACCTIDI, CNAMEI, transaction details (TRNID01I etc.), populated from 3270 input |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU0AO | IOType.CICS_MAP | Output structure redefining COPAU0AI for sending data to terminal screen, including protected/highlight/visible attributes (C/P/H/V) and output data (O) for all fields like TRNNAMEO, ACCTIDO, balances, transaction details, and ERRMSGO |

## Open Questions

- ? Which specific CICS COBOL programs include and use this COPYBOOK?
  - Context: The copybook does not contain references to including programs; usage can only be inferred from BMS map name and field purposes.
- ? What are the exact meanings of transaction fields like PTYPEnnI, PAPRVnnI, PSTATnnI, and SEL000nI?
  - Context: Field names suggest PTYPE=transaction type, PAPRV=approval, PSTAT=status, SEL=selector, but no comments or context provided in copybook.
