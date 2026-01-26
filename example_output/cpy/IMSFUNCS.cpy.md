# IMSFUNCS

**File**: `cpy/IMSFUNCS.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-26 14:22:23.325418

## Purpose

This COBOL copybook defines a data structure FUNC-CODES containing constants for IMS DL/I function codes used in database calls, such as 'GU  ' for Get Unique, 'GHU ' for Get Hold Unique, 'GN  ' for Get Next, and others up to 'DLET' for Delete. It also defines PARMCOUNT as a 5-digit signed binary field initialized to +4, likely representing the expected number of parameters for IMS calls. The copybook is intended for inclusion in COBOL programs performing IMS database operations.

**Business Context**: IMS hierarchical database access and manipulation in mainframe applications

## Open Questions

- ? In which specific programs or sections (e.g., WORKING-STORAGE, LINKAGE) is this copybook included?
  - Context: The copybook defines reusable constants but does not specify usage context
- ? What do the specific IMS function codes (e.g., GNP, GHNP) correspond to in IMS documentation?
  - Context: Standard IMS DL/I codes inferred (e.g., GU=Get Unique), but exact mappings for less common ones like GHNP not confirmed from file alone
