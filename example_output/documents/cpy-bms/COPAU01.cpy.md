# COPAU01

**File**: `cpy-bms/COPAU01.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-28 14:55:15.224368

## Purpose

This copybook defines the input and output data structures for a CICS BMS map named COPAU1. It contains field definitions for transaction details, dates, times, card numbers, authorization information, merchant details, and error messages, used for both input (COPAU1AI) and output (COPAU1AO) layouts.

**Business Context**: UNKNOWN

## Inputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1AI | IOType.CICS_MAP | Input data structure for the COPAU1 CICS BMS map, containing fields for transaction name, titles, current date and time, card number, authorization details, merchant information, and error messages. |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| COPAU1AO | IOType.CICS_MAP | Output data structure for the COPAU1 CICS BMS map, containing fields for transaction name, titles, current date and time, card number, authorization details, merchant information, and error messages, formatted for display. |

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

- ? What is the specific purpose of each field within the COPAU1AI and COPAU1AO structures?
  - Context: The copybook defines the structure but not the specific meaning or validation rules for each field.
- ? What CICS program(s) use this BMS map?
  - Context: The copybook itself doesn't specify which programs use it.
