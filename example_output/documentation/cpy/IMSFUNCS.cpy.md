# IMSFUNCS

**File**: `cpy/IMSFUNCS.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-01-30 19:47:11.422402

## Purpose

This COBOL copybook defines the 01-level group FUNC-CODES containing multiple 05-level elementary items, each a PIC X(04) field initialized with specific 4-character values representing IMS database function codes (e.g., 'GU  ', 'GHU ', 'GN  ', 'REPL', 'ISRT', 'DLET'). It also defines PARMCOUNT as PIC S9(05) VALUE +4 COMP-5, likely specifying the parameter count for IMS calls. These structures are used in programs performing IMS database operations such as Get Unique (GU), Get Hold Unique (GHU), Get Next (GN), Replace (REPL), Insert (ISRT), and Delete (DLET).

**Business Context**: IMS hierarchical database call parameters

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
