# COPAU00

**File**: `cpy-bms/COPAU00.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-26 17:36:56.405042

## Purpose

This COBOL copybook defines the symbolic map data structures COPAU0AI for input and COPAU0AO for output used with the COPAU0A BMS map in CICS online transactions. It structures fields for screen display and data entry including headers like transaction name, titles, date, time, program name; customer account details such as account ID, name, customer ID, addresses, status, phone; financial data like approval/decline counts, credit/cash limits, balances, totals; up to five recent transactions with ID, post date, time, type, approval flag, status, amount; and an error message field. The structure supports typical CICS BMS field attributes and formats for protected, highlighted, visible, and output/input data handling.

**Business Context**: Customer account inquiry and update screen in a financial or banking CICS application, displaying summary information and recent transaction history.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| COPAU0AI | IOType.CICS_MAP | Input symbolic map structure capturing terminal data for fields like account ID, selectors, transaction details entered by user |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU0AO | IOType.CICS_MAP | Output symbolic map structure for sending data to terminal including headers, customer details, balances, recent transactions, error messages |

## Open Questions

- ? Which CICS programs include and use this copybook?
  - Context: Copybook defines structures but does not specify including programs
- ? What are the exact BMS physical map details and transaction IDs using this symbolic map?
  - Context: Symbolic map only; physical map layout and transids not defined here
