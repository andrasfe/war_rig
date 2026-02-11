# COPAU01

**File**: `cpy-bms/COPAU01.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-02-10 17:20:55.005205

## Purpose

This copybook defines the input and output data structures, COPAU1AI and COPAU1AO, used for CICS BMS map processing. It includes fields for transaction details, titles, dates, times, card numbers, authorization details, merchant information, and error messages. The copybook uses filler fields and redefines to structure the data for screen display.

**Business Context**: UNKNOWN

## Inputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1AI | IOType.CICS_MAP | Input data structure for the CICS BMS map, containing fields to receive data from the screen. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1AO | IOType.CICS_MAP | Output data structure for the CICS BMS map, containing fields to send data to the screen. |

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

- ? What is the specific CICS application that uses this BMS map?
  - Context: The copybook defines the data structures for a BMS map, but the application using it is not specified.
