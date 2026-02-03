# COPAU00

**File**: `cpy-bms/COPAU00.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-02-03 21:08:00.067624

## Purpose

This copybook appears to be empty. It likely serves as a placeholder or is intended to be populated with data structures or definitions in a later version. It currently contains no code or data definitions.

## Paragraphs/Procedures

### COPAU0AI/COPAU0AO
This copybook defines the screen layout for the COPAU00 application. It contains two main sections, COPAU0AI and COPAU0AO, which define the input and output fields for the screen. COPAU0AI defines the input fields with their lengths, attributes, and input/output fields, while COPAU0AO redefines COPAU0AI to define the output fields with their corresponding attributes. The fields include transaction name, titles, current date and time, account ID, customer name, customer ID, address, account status, phone number, approval/decline counts, credit/cash limits and balances, transaction details (ID, date, time, type, approval status, amount), and error messages. The fields are designed to be used with BMS macros to create the screen layout for the CICS application. The copybook does not perform any business logic or error handling itself; it only defines the structure and format of the data displayed on the screen.

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| COPAU0AI | record_layout | 1 | Record layout 'COPAU0AI' is never used by any program |

## Open Questions

- ? What is the intended purpose of this empty copybook?
  - Context: The copybook contains no code or data definitions, making it impossible to determine its purpose without external information.
