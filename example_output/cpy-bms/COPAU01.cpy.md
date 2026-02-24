# COPAU01

**File**: `cpy-bms/COPAU01.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-02-24 17:38:59.996763

## Purpose

This copybook defines the data structures COPAU1AI and COPAU1AO, which are used for screen input and output related to authorization processing. It includes fields for transaction name, titles, date, time, card number, authorization details, merchant information, and error messages.

**Business Context**: This copybook is used in online CICS applications to define the data structures for screen displays and data transfer related to authorization processes.

## Paragraphs/Procedures

### ~~COPAU1AI~~ (Dead Code)
*Record layout 'COPAU1AI' is never used by any program*

### COPAU1AO
This data structure redefines the COPAU1AI structure (line 181) and is used for output to the screen. It provides a different view of the same data, allowing the program to format and display the information on the screen. The structure includes fields corresponding to the input fields in COPAU1AI, but with different PICTURE clauses, primarily using 'X' to represent alphanumeric characters. These fields are named with suffixes 'C', 'P', 'H', 'V', and 'O' (e.g., TRNNAMEC, TRNNAMEP, TRNNAMEH, TRNNAMEV, TRNNAMEO) to represent control characters, presentation attributes, highlighting, validation, and output values, respectively. The structure includes redefinitions for transaction name, titles, current date and time, card number, authorization details, POS entry mode, authorization source, MCC code, card expiry, authorization type, transaction ID, authorization match, authorization fraud, merchant details, and error message. This structure is used to format the data for display on the screen, setting attributes such as color, highlighting, and validation rules.

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| COPAU1AI | record_layout | 1 | Record layout 'COPAU1AI' is never used by any program |

## Open Questions

- ? What CICS commands use this copybook?
  - Context: The copybook defines screen fields, but the specific CICS commands that utilize it are not evident from the copybook itself.
