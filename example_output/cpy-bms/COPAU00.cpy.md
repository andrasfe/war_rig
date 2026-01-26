# COPAU00

**File**: `cpy-bms/COPAU00.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-26 14:20:20.558487

## Purpose

This COBOL copybook defines the data structure for the COPAU00 BMS map used in CICS online programs to handle input and output for a customer account summary screen. It includes COPAU0AI with fields for lengths, formats, attributes, and input data areas for elements like transaction name, titles, dates, account ID, customer details, addresses, status, phone, counts, limits, balances, selectors for up to 5 transactions (each with ID, date, time, type, approval, status, amount), and error message. COPAU0AO redefines COPAU0AI for output with standard BMS attributes (C=constant, P=protected, H=highlight, V=visible, O=output data).

**Business Context**: Customer account inquiry screen in a financial or credit system, displaying personal info, account status, limits, balances, approval/decline summaries, and recent transaction history.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| COPAU0AI | IOType.CICS_MAP | Input structure containing length (L), format flag (F), attribute (A), and input data (I) fields for all screen elements including account details, limits, balances, transaction history, and error message |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU0AO | IOType.CICS_MAP | Output structure redefining COPAU0AI with BMS attributes C (constant), P (protected), H (highlight), V (visible), and O (output data) for all screen fields |

## Open Questions

- ? Which CICS programs include and use this copybook?
  - Context: No including program provided; cannot determine usage context
