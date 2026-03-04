# COPAU00

**File**: `cpy-bms/COPAU00.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-03-04 03:32:52.083257

## Purpose

This copybook defines the data structures COPAU0AI and COPAU0AO, which are used for screen input and output related to transaction processing. It includes fields for transaction names, titles, dates, amounts, selection options, and error messages.

**Business Context**: This copybook is likely used in online CICS applications for user interface definition and data transfer between the application and the screen.

## Paragraphs/Procedures

### ~~COPAU0AI~~ (Dead Code)
*Record layout 'COPAU0AI' is never used by any program*

### COPAU0AO
This paragraph defines the data structure COPAU0AO, which redefines COPAU0AI. It provides an alternative view of the data, breaking down some of the fields into smaller components. Specifically, it redefines TRNNAMEF into TRNNAMEC, TRNNAMEP, TRNNAMEH, TRNNAMEV, and TRNNAMEO, and TITLE01F into TITLE01C, TITLE01P, TITLE01H, TITLE01V, and TITLE01O. This redefinition allows the program to access the same data in different formats, potentially for display or processing purposes. The 'C', 'P', 'H', and 'V' suffixes might represent different attributes or parts of the original fields.

### COPAU00
[Citadel] Paragraph identified by static analysis

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| COPAU0AI | record_layout | 1 | Record layout 'COPAU0AI' is never used by any program |

## Open Questions

- ? What is the exact purpose of each field suffix (C, P, H, V, O) in the redefined data structure COPAU0AO?
  - Context: The code provides no explicit comments or documentation explaining the meaning of these suffixes.
