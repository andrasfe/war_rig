# COPAU01

**File**: `cpy-bms/COPAU01.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-03-04 03:32:49.733282

## Purpose

This copybook defines the data structure COPAU1AI and its redefinition COPAU1AO, which are used for screen input/output. It contains fields related to transaction details, titles, dates, times, card numbers, authorization details, merchant information, and error messages.

**Business Context**: This copybook is likely used in an online CICS application to format data displayed on a screen and receive input from a user, possibly related to payment authorization or transaction processing.

## Paragraphs/Procedures

### ~~COPAU1AI~~ (Dead Code)
*Record layout 'COPAU1AI' is never used by any program*

### COPAU1AO
This data structure redefines COPAU1AI to provide a different view of the same data. Instead of length and flag fields, it uses single-character fields ('C', 'P', 'H', 'V') and output fields ('O') for each data element. The 'C', 'P', 'H', and 'V' fields likely represent control characters or attributes for screen display, while the 'O' fields contain the actual output data to be displayed on the screen. This redefinition allows the program to easily format and display the data on a CICS screen. The FILLER fields are used to align the data correctly within the structure. This structure is used to populate the BMS map fields for output.

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| COPAU1AI | record_layout | 1 | Record layout 'COPAU1AI' is never used by any program |

## Open Questions

- ? What is the exact purpose of the 'C', 'P', 'H', and 'V' fields in the COPAU1AO redefinition?
  - Context: The code only shows the data structure definition, not how these fields are used.
