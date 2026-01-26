# XAUTHFRD

**File**: `ddl/XAUTHFRD.ddl`
**Type**: FileType.OTHER
**Analyzed**: 2026-01-26 17:39:50.104629

## Purpose

This DDL script creates a unique index named CARDDEMO.XAUTHFRD on the CARDDEMO.AUTHFRDS table. The index keys on CARD_NUM in ascending order and AUTH_TS in descending order. The COPY YES option is specified, which in DB2 context allows the index to be copied during reorganization.

**Business Context**: Supports data integrity and efficient querying of authorization records (likely fraud-related) in the CARDDEMO schema by enforcing uniqueness and optimizing access by card number and timestamp.

## Inputs

| Name | Type | Description |
|------|------|-------------|
| CARDDEMO.AUTHFRDS | IOType.DB2_TABLE | Base table on which the unique index is created |

## Outputs

| Name | Type | Description |
|------|------|-------------|
| CARDDEMO.XAUTHFRD | IOType.OTHER | Unique index enforcing data integrity and supporting ordered access |

## Business Rules

- **BR001**: Enforces uniqueness constraint preventing duplicate combinations of CARD_NUM and AUTH_TS in the AUTHFRDS table
