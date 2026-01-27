# XAUTHFRD

**File**: `ddl/XAUTHFRD.ddl`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-27 23:03:40.758949

## Purpose

This DDL file creates a unique index named CARDDEMO.XAUTHFRD on the CARDDEMO.AUTHFRDS table, using the CARD_NUM in ascending order and AUTH_TS in descending order. It also specifies that the index should be copied.

## Paragraphs/Procedures

### ~~XAUTHFRD~~ (Dead Code)
*Artifact 'XAUTHFRD' (index) is never referenced by any other artifact in the dependency graph*

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| XAUTHFRD | index | 1 | Artifact 'XAUTHFRD' (index) is never referenced by any other artifact in the dependency graph |

## Open Questions

- ? What is the purpose of copying the index?
  - Context: The 'COPY YES' clause is used, but its specific function is unclear without more context.
