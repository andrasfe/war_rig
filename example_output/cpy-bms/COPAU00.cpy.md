# COPAU00

**File**: `cpy-bms/COPAU00.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-27 02:39:58.053664

## Purpose

This COBOL copybook defines the symbolic input map COPAU0AI and output map COPAU0AO (via REDEFINES at line 391) for a CICS BMS screen named COPAU00. It structures fields for displaying transaction name, titles, current date/time, program name, customer account ID, name, address lines, account status, phone, approval/decline counts, credit/cash limits and balances, selection flags for up to 5 transactions, and per-transaction details (ID, date, time, type, approval, status, amount), plus an error message field. The structure follows standard BMS conventions with L (length), F/A (flags/attributes), I (input data) for AI, and C/P/H/V/O (change/protect/highlight/visible/output) for AO.

**Business Context**: Supports CICS online transaction processing for customer account summary inquiry in a financial or banking system, displaying account details and recent transactions.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| COPAU0AI | IOType.CICS_MAP | Symbolic input map structure capturing data from BMS RECEIVE MAP, including customer details and transaction selections |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU0AO | IOType.CICS_MAP | Symbolic output map structure for populating BMS SEND MAP with account summary, limits, balances, and transaction history |

## Open Questions

- ? Which CICS programs include this COPY statement?
  - Context: Copybook usage not visible in this file; required to understand integration.
- ? What are the exact BMS mapset name and transaction ID using this map?
  - Context: Inferred as COPAU00 from filename and group name, but not explicitly stated.
