# IMSFUNCS

**File**: `cpy/IMSFUNCS.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-27 02:43:02.293366

## Purpose

The IMSFUNCS copybook defines a group of constants under 01 FUNC-CODES for IMS DL/I function codes such as 'GU  ', 'GHU ', 'GN  ', 'GHN ', 'GNP ', 'GHNP', 'REPL', 'ISRT', and 'DLET'. It also defines PARMCOUNT as a fixed value of +4, likely for the number of parameters in IMS PCB calls. These constants standardize IMS database operation calls in COBOL programs.

**Business Context**: IMS DL/I database navigation, insertion, deletion, and replacement operations in mainframe COBOL applications

## Business Rules

- **BR001**: FUNC-CODES provides standardized 4-character IMS DL/I function codes (GU, GHU, GN, GHN, GNP, GHNP, REPL, ISRT, DLET)
- **BR002**: PARMCOUNT is fixed at +4 for IMS PCB calls
