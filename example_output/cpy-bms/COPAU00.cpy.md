# COPAU00

**File**: `cpy-bms/COPAU00.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-26 02:33:06.435150

## Purpose

This COBOL copybook defines the symbolic BMS map structures COPAU0AI and COPAU0AO for a CICS screen displaying customer account details. COPAU0AI provides input-oriented fields including lengths, format flags, attributes, and input data areas for screen fields. COPAU0AO redefines COPAU0AI with output-oriented fields including constant, protected, highlight, visible, and output data areas.

**Business Context**: CICS online transaction screen for customer account inquiry in a financial system, showing account summary, limits, balances, counts, recent transactions, and error messages.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| COPAU0AI | IOType.CICS_MAP | Input symbolic map structure containing field lengths (L), format flags (F), attributes (A), and input data (I) for screen elements like transaction name, titles, dates, customer info, balances, transactions, and error message. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU0AO | IOType.CICS_MAP | Output symbolic map structure redefining COPAU0AI, with separate fields for constant (C), protected (P), highlight/highlighted (H), visible (V), and output data (O) for all screen fields. |

## Open Questions

- ? Which CICS programs include and use this BMS copybook?
  - Context: Copybook defines map structures but does not specify including programs.
- ? What is the exact transaction ID for this screen?
  - Context: TRNNAM* fields suggest transaction name display, but specific ID not defined here.
- ? Precise meanings of field abbreviations like APPRCNT, DECLCNT, PAPRV, PSTAT?
  - Context: Inferred from context (approved count, declined count, previous approval, status) but not explicitly documented.
