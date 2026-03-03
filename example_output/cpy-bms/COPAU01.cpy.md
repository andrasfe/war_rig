# COPAU01

**File**: `cpy-bms/COPAU01.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-03-03 16:50:43.690391

## Purpose

This copybook defines the data structures COPAU1AI and COPAU1AO, which are used for screen input and output, potentially related to authorization processing. COPAU1AI defines the input fields with associated length and flag fields, while COPAU1AO redefines COPAU1AI to provide a different view of the same data, using single-character fields for cursor positioning and other attributes.

**Business Context**: UNKNOWN

## Paragraphs/Procedures

### ~~COPAU1AI~~ (Dead Code)
*Record layout 'COPAU1AI' is never used by any program*

### COPAU1AO
This data structure redefines COPAU1AI to provide a different perspective on the same data, primarily for screen output formatting and control. It uses single-character fields (C, P, H, V, O) to represent cursor positioning, highlighting, and other display attributes for each corresponding field in COPAU1AI. For example, TRNNAMEC/P/H/V/O correspond to TRNNAMEL/I/F/A in COPAU1AI, allowing the program to control the display attributes of the Transaction Name field. The 'C' suffix likely represents the cursor position, 'P' the protection attribute, 'H' the highlight attribute, 'V' the validation attribute, and 'O' the output field. This redefinition enables precise control over the screen's appearance and user interaction, facilitating a user-friendly interface for authorization-related transactions.

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| COPAU1AI | record_layout | 1 | Record layout 'COPAU1AI' is never used by any program |

## Open Questions

- ? What is the specific screen layout and purpose of each field?
  - Context: The copybook defines the data structure, but the exact usage within a BMS map or CICS transaction is unclear.
- ? What is the business context for this authorization process?
  - Context: The copybook contains authorization-related fields, but the specific industry or application is unknown.
