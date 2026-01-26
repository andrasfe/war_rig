# XAUTHFRD

**File**: `ddl/XAUTHFRD.ddl`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-26 02:29:44.816538

## Purpose

This DDL script creates a unique index named CARDDEMO.XAUTHFRD on the CARDDEMO.AUTHFRDS table. The index is defined on CARD_NUM in ascending order and AUTH_TS in descending order. It includes the COPY YES option.

## Outputs

| Name | Type | Description |
|------|------|-------------|
| CARDDEMO.XAUTHFRD | IOType.OTHER | Unique index on CARDDEMO.AUTHFRDS table with keys CARD_NUM ASC and AUTH_TS DESC |

## Open Questions

- ? Business purpose of the AUTHFRDS table and index
  - Context: Names suggest authorization fraud records, but no description provided in DDL
