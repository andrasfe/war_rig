# IMSFUNCS

**File**: `cpy/IMSFUNCS.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-02-09 15:47:16.946869

## Purpose

This copybook defines a set of constants representing IMS function codes and a parameter count. These constants are used when making calls to IMS databases to specify the desired operation.

## Paragraphs/Procedures

### N/A
This copybook does not contain any paragraphs. It consists solely of data definitions for IMS function codes and a parameter count. The function codes represent various IMS database operations such as Get Unique (GU), Get Hold Unique (GHU), Get Next (GN), Get Hold Next (GHN), Get Next within Parent (GNP), Get Hold Next within Parent (GHNP), Replace (REPL), Insert (ISRT), and Delete (DLET). These codes are used as parameters when calling IMS to specify the desired database operation. The PARMCOUNT variable defines the number of parameters expected by the IMS call interface. The copybook serves as a central repository for these constants, ensuring consistency and simplifying maintenance across programs that interact with IMS databases. It is included in the WORKING-STORAGE section of COBOL programs to make these constants available for use in IMS calls. The use of this copybook promotes code reusability and reduces the risk of errors associated with hardcoding these values in multiple programs.

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| FUNC-CODES | record_layout | 1 | Record layout 'FUNC-CODES' is never used by any program |
