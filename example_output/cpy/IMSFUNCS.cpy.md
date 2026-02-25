# IMSFUNCS

**File**: `cpy/IMSFUNCS.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-02-25 15:31:03.414376

## Purpose

This copybook defines a data structure FUNC-CODES containing standard IMS DL/I function codes used to specify database operations in CALL statements to IMS PCBs, such as 'GU  ' for Get Unique, 'GHU ' for Get Hold Unique, 'GN  ' for Get Next, 'GHN ' for Get Hold Next, 'GNP ' for Get Next within Parent, 'GHNP' for Get Hold Next within Parent, 'REPL' for Replace, 'ISRT' for Insert, and 'DLET' for Delete (lines 18-26). It also defines PARMCOUNT as a 5-digit signed binary field initialized to +4, likely indicating the expected number of parameters in IMS DL/I calls (line 27). This structure is copied into COBOL programs to standardize IMS function code values and parameter counts for database access operations.

**Business Context**: IMS DL/I database access and manipulation in mainframe applications

## Paragraphs/Procedures

### IMSFUNCS
[Citadel] Paragraph identified by static analysis

### ~~FUNC-CODES~~ (Dead Code)
*Record layout 'FUNC-CODES' is never used by any program*

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| FUNC-CODES | record_layout | 1 | Record layout 'FUNC-CODES' is never used by any program |
