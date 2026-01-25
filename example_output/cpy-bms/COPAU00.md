# COPAU00

**File**: `cpy-bms/COPAU00.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-25 18:30:42.052528

## Purpose

COBOL copybook defining symbolic map structures for a CICS BMS screen. COPAU0AI provides input layout with length, format flag, attribute, and data fields for screen elements including titles, dates, account/customer details, counters, limits, balances, transaction records, and error message. COPAU0AO redefines COPAU0AI for output layout with separate C (color?), P (protected?), H (highlight?), V (ask for vector?), and O (output data) fields.

**Business Context**: Financial application screen for customer account inquiry displaying account status, recent transactions, limits, and balances (UNKNOWN: exact business process undetermined from structure alone)

## Inputs

| Name | Type | Description |
|------|------|-------------|
| COPAU0AI | IOType.CICS_MAP | Input symbolic map structure containing screen field data with prefixed lengths, flags, and attributes |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU0AO | IOType.CICS_MAP | Output symbolic map structure redefining input, separating BMS attributes (C/P/H/V) and output data (O) for each field |

## Open Questions

- ? Which CICS programs include this copybook via COPY statement?
  - Context: No usage context provided in source code
- ? Exact mapping of field attribute flags (e.g., F/A meanings)?
  - Context: Standard BMS pattern assumed but specific values undocumented
- ? Precise business role of transaction fields (TRNID01I to TRNID05I, etc.)?
  - Context: Fields suggest recent transaction history but types/statuses unknown
