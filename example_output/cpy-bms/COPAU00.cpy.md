# COPAU00

**File**: `cpy-bms/COPAU00.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-03-16 20:00:31.733138

## Purpose

This copybook defines the data structure COPAU0AI and its redefinition COPAU0AO, which are likely used for screen input and output in a CICS environment. It contains fields for transaction name, titles, current date and time, account and customer information, addresses, phone number, credit and cash limits/balances, transaction details, and error messages.

**Business Context**: This copybook is likely used in a customer account management system to display and capture customer and transaction information on a CICS screen.

## Open Questions

- ? What program(s) use this copybook?
  - Context: From chunk source:chunk-1 (lines 1-549)
- ? What is the purpose of the 'C', 'P', 'H', and 'V' suffixes in the redefined COPAU0AO structure?
  - Context: From chunk source:chunk-1 (lines 1-549)
- ? What CICS program uses this BMS map?
  - Context: From chunk source:chunk-2 (lines 550-765)
