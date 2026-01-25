# XAUTHFRD

**File**: `ddl/XAUTHFRD.ddl`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-25 18:34:13.001575

## Purpose

Creates a unique index named XAUTHFRD on the CARDDEMO.AUTHFRDS table with CARD_NUM in ascending order and AUTH_TS in descending order. Includes COPY YES option for index copy storage. Enforces uniqueness on the indexed column combination.

## Business Rules

- **BR001**: Prevents insertion of duplicate rows sharing the same CARD_NUM and AUTH_TS values
