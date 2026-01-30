# COPAU01

**File**: `cpy-bms/COPAU01.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-30 19:44:31.500364

## Purpose

This copybook defines the COBOL data structures for the COPAU01 BMS map used in CICS online transactions. It provides the input area COPAU1AI with length, format/attribute, and data fields for screen elements including transaction names, titles, dates, times, card numbers, authorization details, merchant information, and error messages. The output area COPAU1AO redefines COPAU1AI with BMS attribute fields (C/P/H/V) and output data fields for screen rendering.

**Business Context**: Supports credit card authorization screens in payment processing, displaying and capturing transaction, card, authorization response, and merchant details.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1AI | IOType.CICS_MAP | Input map structure containing length (L), format/attribute (F/A), and input data (I) fields for all screen elements |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1AO | IOType.CICS_MAP | Output map structure redefining input, with changed/protected/highlighted/unprotected attributes (C/P/H/V) and output data (O) fields for screen display |

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

## Open Questions

- ? Which CICS programs include and use this copybook?
  - Context: Copybook defines map structures but does not specify including programs
