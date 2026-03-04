# IMSFUNCS

**File**: `cpy/IMSFUNCS.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-03-04 04:44:16.024875

## Purpose

This copybook defines a set of constants representing IMS function codes and a parameter count. These constants are used when making calls to IMS databases.

## Paragraphs/Procedures

### ~~FUNC-CODES~~ (Dead Code)
*Record layout 'FUNC-CODES' is never used by any program*

### PARMCOUNT
This data item defines a constant that represents the number of parameters expected by the IMS call. It is defined as a signed 5-digit integer in COMP-5 format, which is a binary representation. The value is initialized to +4, indicating that the IMS call expects four parameters. This constant is likely used to validate the number of parameters passed to the IMS call, ensuring that the call is correctly formatted. This validation helps prevent errors during IMS database interaction. The data item does not perform any processing but provides a central location for defining the expected parameter count for IMS calls.

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| FUNC-CODES | record_layout | 1 | Record layout 'FUNC-CODES' is never used by any program |
