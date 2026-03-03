# COPAU00

**File**: `cpy-bms/COPAU00.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-02-27 14:44:47.848471

## Purpose

This copybook defines the data structures COPAU0AI and COPAU0AO, which are used for screen input and output. COPAU0AI contains fields for transaction name, title, and current date, while COPAU0AO redefines COPAU0AI to provide different views of the same data.

## Paragraphs/Procedures

### ~~COPAU0AI~~ (Dead Code)
*Record layout 'COPAU0AI' is never used by any program*

### COPAU0AO
This data structure redefines the COPAU0AI structure to provide a different view of the same data. Specifically, it redefines parts of the transaction name, title, and current date fields into single-character fields (TRNNAMEC, TRNNAMEP, TRNNAMEH, TRNNAMEV, TRNNAMEO, TITLE01C, TITLE01P, TITLE01H, TITLE01V, TITLE01O, CURDATEC). This allows for easier manipulation or display of individual characters within these fields. The FILLER fields are used to skip over portions of the original COPAU0AI structure that are not being redefined. This structure does not perform any business logic or call other paragraphs; it simply provides an alternative data layout based on COPAU0AI.

### COPAU00
[Citadel] Paragraph identified by static analysis

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| COPAU0AI | record_layout | 1 | Record layout 'COPAU0AI' is never used by any program |
