# COPAU00

**File**: `cpy-bms/COPAU00.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-03-04 04:45:03.054289

## Purpose

This copybook defines data structures used for BMS map definitions, specifically for screen input and output fields related to transaction processing and date display. It includes definitions for field lengths, attributes, and input/output areas.

**Business Context**: Defines the layout for user interface elements in a CICS environment, facilitating data exchange between the application and the user.

## Paragraphs/Procedures

### ~~COPAU0AI~~ (Dead Code)
*Record layout 'COPAU0AI' is never used by any program*

### COPAU0AO
This paragraph redefines the COPAU0AI data structure as COPAU0AO, providing an alternate view of the same memory area. This redefinition allows accessing parts of the original fields using different names and formats. Specifically, it redefines TRNNAMEL, TRNNAMEF, and TRNNAMEI into TRNNAMEC, TRNNAMEP, TRNNAMEH, TRNNAMEV, and TRNNAMEO, and similarly for TITLE01L, TITLE01F, and TITLE01I into TITLE01C, TITLE01P, TITLE01H, TITLE01V, and TITLE01O, and CURDATEL and CURDATEF into CURDATEC. This is likely done to facilitate easier manipulation or display of the data on a BMS screen. No processing or logic is performed within this paragraph; it only provides a redefinition of the data structure. It allows different parts of the program to view the same data in different ways.

### COPAU00
[Citadel] Paragraph identified by static analysis

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| COPAU0AI | record_layout | 1 | Record layout 'COPAU0AI' is never used by any program |

## Open Questions

- ? What is the exact purpose of each field within the COPAU0AI and COPAU0AO structures?
  - Context: The copybook only defines the structure, not the specific usage of each field.
