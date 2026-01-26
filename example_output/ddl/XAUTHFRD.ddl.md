# XAUTHFRD

**File**: `ddl/XAUTHFRD.ddl`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-26 14:23:13.882483

## Purpose

This DDL script creates a unique index named CARDDEMO.XAUTHFRD on the CARDDEMO.AUTHFRDS table. The index is defined on CARD_NUM in ascending order and AUTH_TS in descending order, enforcing uniqueness on these column combinations. It includes the COPY YES option.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| CARDDEMO.AUTHFRDS | IOType.DB2_TABLE | Target table on which the unique index is created |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| CARDDEMO.XAUTHFRD | IOType.OTHER | Unique index enforcing no duplicate (CARD_NUM, AUTH_TS) combinations and supporting ordered access |

## Business Rules

- **BR001**: Enforce uniqueness on (CARD_NUM, AUTH_TS) combinations in the AUTHFRDS table

## Open Questions

- ? Exact purpose and implications of the COPY YES option
  - Context: Syntax is present at line 4 but specific DB2 behavior (e.g., copy utility compatibility) cannot be confirmed without external documentation
