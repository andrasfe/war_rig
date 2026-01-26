# IMSFUNCS

**File**: `cpy/IMSFUNCS.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-26 02:31:39.405570

## Purpose

This COBOL copybook defines the FUNC-CODES group containing IMS DL/I function codes for database operations, such as 'GU  ' for Get Unique, 'GHU ' for Get Hold Unique, 'GN  ' for Get Next, and others including REPL, ISRT, and DLET. It also defines PARMCOUNT as a 5-digit signed binary field initialized to 4, likely indicating the number of parameters for IMS calls. These constants are used in programs performing IMS database calls to specify the function in the PCB mask.

**Business Context**: IMS hierarchical database access function codes for DL/I calls

## Open Questions

- ? In which specific programs is this IMSFUNCS copybook included?
  - Context: The copybook defines IMS function codes but does not indicate usage context or including programs.
- ? What do the specific IMS function codes correspond to in IMS DL/I documentation?
  - Context: Codes like 'GU  ', 'GHU ', etc., are standard IMS abbreviations, but exact mappings (e.g., spaces in 'GU  ') are not explained in the copybook.
