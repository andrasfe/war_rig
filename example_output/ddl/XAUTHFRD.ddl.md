# XAUTHFRD

**File**: `ddl/XAUTHFRD.ddl`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-27 02:39:58.128148

## Purpose

This DDL script creates a unique index named CARDDEMO.XAUTHFRD on the CARDDEMO.AUTHFRDS table. The index is defined on CARD_NUM in ascending order and AUTH_TS in descending order. It includes the COPY YES option.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| CARDDEMO.AUTHFRDS | IOType.DB2_TABLE | Target table on which the unique index is created |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| CARDDEMO.XAUTHFRD | IOType.OTHER | Unique index enforcing uniqueness on (CARD_NUM ASC, AUTH_TS DESC) |

## Business Rules

- **BR001**: Enforces uniqueness on the composite key (CARD_NUM, AUTH_TS)

## Open Questions

- ? Specific purpose of the COPY YES option
  - Context: Not self-explanatory from the DDL statement alone
