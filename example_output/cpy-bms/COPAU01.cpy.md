# COPAU01

**File**: `cpy-bms/COPAU01.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-02-24 03:58:48.947516

## Purpose

This COBOL copybook defines the symbolic map structures COPAU1AI (input) and COPAU1AO (output redefines COPAU1AI) for the CICS BMS map COPAU01. It provides data areas, length fields (e.g., TRNNAMEL COMP PIC S9(4)), format/attribute fields (e.g., TRNNAMEF/A), and input/output data fields (e.g., TRNNAMEI, TRNNAMEO) for screen elements related to payment authorization. Fields cover transaction name, titles, dates/times, program name, card number, auth details (date/time/response/reason/code/amount), POS mode, source, MCC, expiration, type, txn ID, method, fraud, merchant info, and error message.

**Business Context**: CICS online screen for credit card authorization processing, displaying merchant/transaction details and capturing card/auth response data.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1AI | IOType.CICS_MAP | Symbolic input map structure receiving user-entered data from COPAU01 BMS screen, including fields like CARDNUMI, AUTHRSPI, ERRMSGI |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1AO | IOType.CICS_MAP | Symbolic output map structure sending display data and attributes to COPAU01 BMS screen, with per-character C/P/H/V/O controls (e.g., TRNNAMEO, CARDNUMO) |

## Business Rules

- **BR001**: Transaction name input field TRNNAMEI accepts exactly 4 alphanumeric characters
- **BR002**: Card number input field CARDNUMI accepts exactly 16 alphanumeric characters for credit/debit card
- **BR003**: Authorization response input field AUTHRSPI accepts 1 character response code
- **BR004**: Error message output field ERRMSGO supports up to 78 characters for screen display

## Paragraphs/Procedures

### COPAU01
[Citadel] Paragraph identified by static analysis

### ~~COPAU1AI~~ (Dead Code)
*Record layout 'COPAU1AI' is never used by any program*

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| COPAU1AI | record_layout | 1 | Record layout 'COPAU1AI' is never used by any program |
