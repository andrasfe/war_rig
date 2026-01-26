# COPAU01

**File**: `cpy-bms/COPAU01.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-26 14:19:57.212282

## Purpose

This copybook defines the data structures COPAU1AI and COPAU1AO for a BMS map in a CICS online transaction screen. COPAU1AI provides the input layout from the terminal with length, flag/attribute, and input data fields for transaction details, authorization information, merchant data, and error messages. COPAU1AO redefines COPAU1AI for output to the terminal using standard BMS C/P/H/V/O attribute and output data fields.

**Business Context**: Credit card authorization response display in a merchant point-of-sale (POS) CICS application, showing transaction name, card number, auth details, merchant info, and errors.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1AI | IOType.CICS_MAP | Input structure from RECEIVE MAP containing terminal-entered data for fields like transaction name (TRNNAMEI), card number (CARDNUMI), authorization details (AUTHDTI, AUTHTMI, etc.), merchant info (MERNAMESI, etc.), and error message (ERRMSGI) |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1AO | IOType.CICS_MAP | Output structure for SEND MAP containing display attributes (C=changed, P=protected, H=highlight, V=visible?) and output data fields (e.g., TRNNAMEO, CARDNUMO, AUTHDTO, MERNAMEO, ERRMSGO) for screen presentation |

## Open Questions

- ? Which specific CICS programs include and use this BMS copybook?
  - Context: Copybook defines structures but does not indicate including programs
- ? Exact meanings of BMS attribute flags (C, P, H, V) in this context?
  - Context: Standard BMS conventions assumed (Changed, Protected, Highlight, Visible?), but not explicitly documented here
