# IMSFUNCS

**File**: `cpy/IMSFUNCS.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-02-24 04:00:49.511836

## Purpose

COBOL copybook defining IMS DL/I function codes as fixed 4-byte constants and a PARMCOUNT field for IMS database call standardization. Includes codes for retrieve (GU, GHU, GN, GHN, GNP, GHNP), update (REPL), insert (ISRT), and delete (DLET) operations. Licensed under Apache License 2.0 with copyright notices.

**Business Context**: Standardizes parameters for IMS hierarchical database DL/I calls in COBOL mainframe programs.

## Paragraphs/Procedures

### IMSFUNCS
This is the top-level structure of the IMSFUNCS copybook, providing reusable data definitions for IMS DL/I operations. It includes copyright notices and Apache License 2.0 details from lines 1-16, establishing legal usage terms. The primary content is the 01 FUNC-CODES group at line 17, which holds all function code constants and PARMCOUNT. No runtime inputs are consumed, as fields are initialized with VALUE clauses at compile time. Outputs consist of these constants available for MOVE operations into IMS CALL parameters like the function code field in the PCB mask. There is no business logic, conditional decisions, or validations implemented, as it is a static definition module. No error handling mechanisms are present. It does not invoke any other paragraphs or external programs. Typically included via COPY IMSFUNCS. in WORKING-STORAGE of IMS-enabled COBOL programs to ensure consistent function code usage across the application.

### ~~FUNC-CODES~~ (Dead Code)
*Record layout 'FUNC-CODES' is never used by any program*

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| FUNC-CODES | record_layout | 1 | Record layout 'FUNC-CODES' is never used by any program |
