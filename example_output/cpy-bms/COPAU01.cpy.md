# COPAU01

**File**: `cpy-bms/COPAU01.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-26 02:33:00.052494

## Purpose

This copybook defines the symbolic map structures COPAU1AI (input) and COPAU1AO (output, redefining input) for a CICS BMS screen displaying credit card authorization response details. Fields include transaction name, titles, current date/time, program name, card number, authorization date/time/response/reason/code/amount, POS entry mode, auth source, MCC, card expiration, auth type, transaction ID, auth method/fraud, merchant details (name/ID/city/state/zip), and error message. It follows standard BMS format with length (L: COMP S9(4)), flag (F: X), attribute (A: X), and input data (I: X(n)) for AI, redefined to C/P/H/V/O output fields.

**Business Context**: CICS online screen for displaying authorization responses in a credit card payment processing system.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1AI | IOType.CICS_MAP | Symbolic input map structure populated by CICS RECEIVE MAP, holding data from terminal screen fields like card number, auth details, merchant info. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1AO | IOType.CICS_MAP | Symbolic output map structure used by CICS SEND MAP to display data on terminal screen, including titles, dates, auth response, merchant info, error message. |

## Open Questions

- ? Which CICS program(s) include and use this copybook?
  - Context: Copybook defines structures but does not reference using programs.
