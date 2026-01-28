# XAUTHFRD

**File**: `ddl/XAUTHFRD.ddl`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-28 14:52:35.778972

## Purpose

This DDL file creates a unique index named CARDDEMO.XAUTHFRD on the CARDDEMO.AUTHFRDS table, using CARD_NUM in ascending order and AUTH_TS in descending order. The index allows copies.

## Paragraphs/Procedures

### ~~XAUTHFRD~~ (Dead Code)
*Artifact 'XAUTHFRD' (index) is never referenced by any other artifact in the dependency graph*

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| XAUTHFRD | index | 1 | Artifact 'XAUTHFRD' (index) is never referenced by any other artifact in the dependency graph |

## Open Questions

- ? What is the purpose of the CARDDEMO database and AUTHFRDS table?
  - Context: The DDL script only provides the index creation statement, not the table definition or database context.
