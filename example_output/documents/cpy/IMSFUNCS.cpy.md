# IMSFUNCS

**File**: `cpy/IMSFUNCS.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-28 14:54:46.069511

## Purpose

This copybook defines a set of constant values representing IMS function codes and a parameter count used in IMS database interactions. These codes are used to specify the type of operation to be performed against the IMS database, such as retrieving, inserting, deleting, or replacing segments.

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

- ? What is the specific purpose of PARMCOUNT?
  - Context: The copybook defines PARMCOUNT, but its usage is unclear without seeing the programs that include this copybook.
