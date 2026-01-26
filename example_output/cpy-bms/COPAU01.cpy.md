# COPAU01

**File**: `cpy-bms/COPAU01.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-26 15:13:42.871836

## Purpose

This copybook defines the data structures COPAU1AI (input) and COPAU1AO (output redefines input) for the COPAU1 BMS map in a CICS online application. It structures fields for screen elements including transaction name, titles, current date/time, program name, card number, authorization details (date, time, response, code, amount), POS entry mode, source, MCC, card expiration, auth type, transaction ID, match code, fraud code, merchant details (name, ID, city, state, zip), and error message. Each field follows BMS convention with length (L COMP S9(4)), format flag (F X), attribute (A X), and input data (I), redefined for output with control chars (C/P/H/V) and output data (O).

**Business Context**: CICS BMS map for payment card authorization screen, capturing/displaying transaction, card, auth response, and merchant information in a financial processing system.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1AI | IOType.CICS_MAP | Input data structure for COPAU1 BMS map, containing screen input fields for transaction details, card info, auth data, merchant info, and error msg. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1AO | IOType.CICS_MAP | Output data structure redefining COPAU1AI for COPAU1 BMS map, with BMS control characters (C=changed, P=protected, H=high, V=visible) and output field values. |

## Open Questions

- ? Which CICS programs include and use this COPAU01 copybook?
  - Context: Copybook defines map structures but does not specify including programs.
- ? What is the exact transaction ID or program that uses this map?
  - Context: Fields like TRNNAMEL/TRNNAMEI suggest transaction name, but no specific usage shown.
