# IMSFUNCS

**File**: `cpy/IMSFUNCS.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-02-03 21:07:41.068545

## Purpose

This copybook defines a set of constants representing IMS function codes and a parameter count used in IMS DL/I calls. It provides symbolic names for commonly used IMS functions like GU, GHU, GN, GHN, GNP, GHNP, REPL, ISRT, and DLET, as well as a parameter count for use in setting up the PCB.

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

## Open Questions

- ? How are these IMS function codes used in the calling programs?
  - Context: The copybook only defines the codes, but not how they are utilized.
