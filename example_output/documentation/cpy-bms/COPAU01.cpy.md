# COPAU01

**File**: `cpy-bms/COPAU01.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-02-03 21:08:00.219067

## Purpose

This copybook defines the input and output data structures, COPAU1AI and COPAU1AO, for a CICS BMS map. It includes fields for transaction details, titles, dates, times, card numbers, authorization details, merchant information, and error messages. The copybook uses filler fields and redefines to structure the data for screen display.

**Business Context**: This copybook is used in a CICS environment for screen formatting and data transfer related to transaction processing, potentially involving authorization and merchant details.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1AI | IOType.CICS_MAP | Input data structure for the CICS BMS map, containing fields for transaction name, titles, current date and time, card number, authorization details, merchant information, and error messages. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1AO | IOType.CICS_MAP | Output data structure for the CICS BMS map, containing fields formatted for screen display, including transaction name, titles, current date and time, card number, authorization details, merchant information, and error messages. |

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

- ? What is the specific CICS program that uses this copybook?
  - Context: The copybook is designed for CICS BMS, but the actual program using it is unknown.
