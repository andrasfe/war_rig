# COPAU00

**File**: `cpy-bms/COPAU00.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-04-21 13:45:02.807675

## Purpose

This copybook defines the data structure COPAU0AI and its redefinition COPAU0AO, which are likely used for screen input and output in a CICS environment. The copybook contains fields for transaction name, titles, current date and time, account and customer information, addresses, phone number, credit and cash limits/balances, amounts, selection indicators, and transaction details, along with an error message field.

**Business Context**: This copybook likely supports a customer account management system, providing the data structure for displaying and capturing customer and transaction information on a screen.

## Open Questions

- ? What specific CICS program(s) use this copybook?
  - Context: From chunk source:chunk-1 (lines 1-549)
- ? What is the purpose of the 'C', 'P', 'H', and 'V' suffixes in the redefined COPAU0AO?
  - Context: From chunk source:chunk-1 (lines 1-549)
- ? What is the specific CICS application that uses this BMS map?
  - Context: From chunk source:chunk-2 (lines 550-765)
