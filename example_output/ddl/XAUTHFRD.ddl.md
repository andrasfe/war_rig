# XAUTHFRD

**File**: `ddl/XAUTHFRD.ddl`
**Type**: FileType.OTHER
**Analyzed**: 2026-02-24 17:38:45.620168

## Purpose

This DDL file defines a unique index named CARDDEMO.XAUTHFRD on the CARDDEMO.AUTHFRDS table, using the CARD_NUM and AUTH_TS columns. The index is created in ascending order for CARD_NUM and descending order for AUTH_TS, and the COPY YES clause indicates that the index should be copied during a recovery operation.

## Paragraphs/Procedures

### ~~XAUTHFRD~~ (Dead Code)
*Artifact 'XAUTHFRD' (index) is never referenced by any other artifact in the dependency graph*

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| XAUTHFRD | index | 1 | Artifact 'XAUTHFRD' (index) is never referenced by any other artifact in the dependency graph |
