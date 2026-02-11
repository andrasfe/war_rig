# IMSFUNCS

**File**: `cpy/IMSFUNCS.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-02-10 17:20:13.388376

## Purpose

This copybook defines a set of constants representing IMS function codes and a parameter count used in IMS database calls. It provides symbolic names for common IMS operations, such as Get Unique (GU), Get Hold Unique (GHU), Get Next (GN), and others, making the code more readable and maintainable. It also defines a parameter count for use with IMS calls.

## Paragraphs/Procedures

### DEFINE-IMS-FUNCTIONS
This copybook does not contain any paragraphs, it only defines a set of constants. The purpose of defining these constants is to provide symbolic names for IMS function codes, such as GU, GHU, GN, GHN, GNP, GHNP, REPL, ISRT, and DLET. These codes are used when making calls to the IMS database to perform specific operations. The copybook also defines a parameter count, PARMCOUNT, which is set to +4. This parameter count is likely used to indicate the number of parameters expected by the IMS call. The constants defined in this copybook are intended to be used in other COBOL programs that interact with an IMS database. By using these symbolic names, the code becomes more readable and maintainable, as the programmer can refer to the operations by name rather than by their numeric codes. This copybook does not perform any error handling or call any other programs or paragraphs. It simply provides a set of pre-defined values for use in other parts of the application.

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| FUNC-CODES | record_layout | 1 | Record layout 'FUNC-CODES' is never used by any program |
