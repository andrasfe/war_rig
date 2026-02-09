# COPAU01

**File**: `cpy-bms/COPAU01.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-02-09 15:48:13.695809

## Purpose

This copybook defines the input and output data structures, COPAU1AI and COPAU1AO, used for a CICS BMS map. It contains fields related to transaction details, authorization information, merchant details, and error messages. The copybook includes definitions for both input/output fields (suffixed with 'I') and display attributes (suffixed with 'C', 'P', 'H', 'V', 'O').

**Business Context**: UNKNOWN

## Inputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1AI | IOType.CICS_MAP | Input data structure for the CICS BMS map, containing fields for transaction name, titles, date, time, card number, authorization details, merchant information, and error messages. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1AO | IOType.CICS_MAP | Output data structure for the CICS BMS map, redefining COPAU1AI and containing display attributes (color, protection, highlighting, validation) for the input fields. |

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
