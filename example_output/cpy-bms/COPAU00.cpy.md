# COPAU00

**File**: `cpy-bms/COPAU00.cpy`
**Type**: FileType.COPYBOOK
**Analyzed**: 2026-03-03 16:50:35.696087

## Purpose

This copybook defines the data structures COPAU0AI and COPAU0AO, which are used for screen input and output. COPAU0AI contains fields for transaction name, title, and current date, while COPAU0AO redefines COPAU0AI to provide character-by-character access to some of the same fields.

## Paragraphs/Procedures

### ~~COPAU0AI~~ (Dead Code)
*Record layout 'COPAU0AI' is never used by any program*

### COPAU0AO
This data structure redefines the COPAU0AI structure to allow access to individual characters within certain fields. Specifically, it provides character-by-character access to the transaction name (TRNNAMEC, TRNNAMEP, TRNNAMEH, TRNNAMEV, TRNNAMEO), title (TITLE01C, TITLE01P, TITLE01H, TITLE01V, TITLE01O), and current date (CURDATEC). This redefinition is useful for manipulating individual characters within these fields, such as for validation or formatting purposes. The filler fields ensure proper alignment with the corresponding fields in COPAU0AI. This structure is likely used in conjunction with COPAU0AI to provide more granular control over the data being displayed or processed.

### COPAU00
[Citadel] Paragraph identified by static analysis

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| COPAU0AI | record_layout | 1 | Record layout 'COPAU0AI' is never used by any program |
