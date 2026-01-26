# COPAU01

**File**: `cpy-bms/COPAU01.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-26 17:39:58.969964

## Purpose

This COBOL copybook defines the BMS map data structures COPAU1AI and COPAU1AO for a CICS screen handling payment authorization details. COPAU1AI provides input field definitions with length (L), flag/format (F), attribute (A), and input data (I) for RECEIVE MAP. COPAU1AO redefines the storage for output with cursor (C), protected (P), highlight (H), visible (V), and output data (O) fields for SEND MAP.

**Business Context**: Payment authorization response screen in a CICS-based point-of-sale or transaction processing system, displaying transaction info, card details, auth codes, merchant data, and errors.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1AI | IOType.CICS_MAP | Input structure for BMS RECEIVE MAP containing screen data for transaction name, titles, date/time, program name, card number, auth details, merchant info, and error message |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1AO | IOType.CICS_MAP | Output structure redefining COPAU1AI for BMS SEND MAP with C/P/H/V/O fields for the same data elements |

## Business Rules

- **BR001**: BMS map fields use a standard structure with length (COMP S9(4)), flag/format (X), attribute (X), and data fields for input (COPAU1AI), redefined as C/P/H/V/O for output (COPAU1AO) to support dynamic screen formatting in CICS.
