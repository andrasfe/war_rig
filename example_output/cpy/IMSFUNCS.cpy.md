# IMSFUNCS

**File**: `cpy/IMSFUNCS.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-02-24 17:38:22.700434

## Purpose

This copybook defines a set of constants representing IMS function codes and a parameter count. These constants are used when making calls to IMS databases.

**Business Context**: This copybook is used in programs that interact with an IMS database.

## Paragraphs/Procedures

### ~~FUNC-CODES~~ (Dead Code)
*Record layout 'FUNC-CODES' is never used by any program*

### PARMCOUNT
This data item defines a COMP-5 signed integer constant representing the number of parameters expected by an IMS call. It is initialized with the value +4. This constant is likely used to ensure that the correct number of parameters are passed when invoking IMS functions. The value is directly assigned within the copybook and is intended to be used as a read-only constant within the calling program. It does not perform any error handling or call any other programs.

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| FUNC-CODES | record_layout | 1 | Record layout 'FUNC-CODES' is never used by any program |
