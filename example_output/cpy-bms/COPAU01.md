# COPAU01

**File**: `cpy-bms/COPAU01.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-25 18:35:18.164580

## Purpose

This COBOL copybook defines the symbolic map structures COPAU1AI and COPAU1AO for a CICS BMS screen. COPAU1AI provides input fields with length (L), flag/attribute (F/A), and data (I) areas for transaction name, card number, authorization details, merchant info, and error message. COPAU1AO redefines it for output with conditioned (C), protected (P), highlight (H), visible (V), and output data (O) fields.

**Business Context**: CICS online transaction screen for displaying credit card authorization responses in a payment processing system

## Inputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1AI | IOType.CICS_MAP | Input symbolic map area with fields for screen data entry including TRNNAMEI (transaction name), CARDNUMI (card number), AUTHRSPI (auth response), MERNAMEI (merchant name), ERRMSGI (error message) |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1AO | IOType.CICS_MAP | Output symbolic map area with attribute fields (C, P, H, V) and output data (O) for screen display of authorization details |

## Business Rules

- **BR001**: BMS symbolic map fields follow standard structure: length (COMP S9(4)), attribute flag (X redefinable to A), filler X(4), and data field (I for input, O for output)

## Open Questions

- ? What CICS programs use this copybook and how are field attributes set?
  - Context: Copybook defines structure but not usage or logic
- ? What is the corresponding physical BMS map definition?
  - Context: Only symbolic map copybook provided
