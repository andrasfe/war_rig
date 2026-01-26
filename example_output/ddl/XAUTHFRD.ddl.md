# XAUTHFRD

**File**: `ddl/XAUTHFRD.ddl`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-26 15:13:37.100261

## Purpose

This DDL script creates a unique index named CARDDEMO.XAUTHFRD on the CARDDEMO.AUTHFRDS table. The index is defined on CARD_NUM in ascending order and AUTH_TS in descending order. It enforces uniqueness on combinations of CARD_NUM and AUTH_TS to prevent duplicate entries.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| CARDDEMO.AUTHFRDS | IOType.DB2_TABLE | DB2 table on which the unique index is created |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| CARDDEMO.XAUTHFRD | IOType.OTHER | Unique DB2 index on CARD_NUM ASC and AUTH_TS DESC with COPY YES option |

## Business Rules

- **BR001**: Enforces uniqueness constraint on the CARDDEMO.AUTHFRDS table, preventing duplicate combinations of CARD_NUM and AUTH_TS

## Open Questions

- ? What is the exact business purpose of the AUTHFRDS table and this index?
  - Context: Table and index names suggest authorization fraud records for card demo, but no descriptive comments or context in the DDL
- ? What does the COPY YES clause precisely imply in this DB2 context?
  - Context: Recognized as DB2 z/OS option related to image copies, but exact behavior not confirmed from this file alone
