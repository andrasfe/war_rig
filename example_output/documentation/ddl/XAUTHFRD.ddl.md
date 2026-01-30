# XAUTHFRD

**File**: `ddl/XAUTHFRD.ddl`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-30 19:27:52.864503

## Purpose

This DDL script creates a unique index named CARDDEMO.XAUTHFRD on the CARDDEMO.AUTHFRDS table. The index keys on CARD_NUM in ascending order and AUTH_TS in descending order. The COPY YES option is specified to allow the index to participate in image copy operations.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| CARDDEMO.AUTHFRDS | IOType.DB2_TABLE | DB2 table on which the unique index is created |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| CARDDEMO.XAUTHFRD | IOType.OTHER | Unique DB2 index created on AUTHFRDS table |

## Business Rules

- **BR001**: Enforces uniqueness on the combination of CARD_NUM and AUTH_TS

## Paragraphs/Procedures

### ~~XAUTHFRD~~ (Dead Code)
*Artifact 'XAUTHFRD' (index) is never referenced by any other artifact in the dependency graph*

## Dead Code

The following artifacts were identified as dead code by static analysis:

| Artifact | Type | Line | Reason |
|----------|------|------|--------|
| XAUTHFRD | index | 1 | Artifact 'XAUTHFRD' (index) is never referenced by any other artifact in the dependency graph |

## Open Questions

- ? What is the business purpose of the AUTHFRDS table?
  - Context: Table name implies authorization fraud records, but no description provided in DDL
- ? No procedural paragraphs present
  - Context: This is pure DDL with no COBOL/PL/I/JCL structure
