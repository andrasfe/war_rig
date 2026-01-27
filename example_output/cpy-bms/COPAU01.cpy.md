# COPAU01

**File**: `cpy-bms/COPAU01.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-27 02:42:16.634986

## Purpose

This copybook defines the symbolic map input structure COPAU1AI and output structure COPAU1AO for the CICS BMS map COPAU1. It structures fields for transaction name, titles, current date/time, program name, card number, authorization details (date, time, response, code, amount), POS entry mode, auth source, MCC, card expiration, auth type, transaction ID, merchant details (name, ID, city, state, zip), and error messages. The input area includes length (L), format (F), attribute (A), and input data (I) fields; the output redefines with attribute flags (C=Cursor, P=Protected, H=Highlight, V=Visible) and output data (O).

**Business Context**: CICS online screen for credit card payment authorization response, displaying transaction, card, auth, and merchant data with error messaging.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1AI | IOType.CICS_MAP | Input (receive) map area with field lengths, formats, attributes, field names, and input data from CICS RECEIVE MAP |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1AO | IOType.CICS_MAP | Output (send) map area redefining COPAU1AI, with per-field C/P/H/V attributes and output data for CICS SEND MAP |

## Business Rules

- **BR001**: Card number input field CARDNUMI is fixed length PIC X(16) to accommodate standard credit card numbers (e.g., 16-digit Visa/Mastercard)
- **BR002**: Error message field ERRMSGI is PIC X(78) to span full screen width for display messages
- **BR003**: Field lengths (e.g., TRNNAMEL) are COMP PIC S9(4) supporting up to 9999 characters per BMS field
